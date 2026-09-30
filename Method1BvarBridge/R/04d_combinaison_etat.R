# ============================================================================
# 04d_combinaison_etat.R -- PHASE 4 quater : poids de combinaison dependant de l'etat
# ============================================================================
# CE QUE LA PHASE 4 A ETABLI
#   La passerelle est battue par le BVAR en moyenne, mais elle le bat nettement
#   quand il se passe quelque chose. Mesure sur les 12 branches couvertes :
#
#     en 2020        erreur absolue moyenne : passerelle 0,075 | BVAR 0,108
#     hors 2020      la passerelle est battue sur 6 branches sur 10
#
#   Le cas le plus net est l'hebergement-restauration au deuxieme trimestre
#   2020 : realise -85,7 %, passerelle -98,0 %, BVAR -3,3 %. La passerelle a vu
#   l'effondrement parce que les arrivees et les nuitees le disaient ; le BVAR
#   ne pouvait pas le voir, puisqu'il ne regarde que le passe de la VA.
#
# LE DEFAUT QUE CELA REVELE DANS LE POIDS
#   delta est estime en minimisant l'erreur quadratique passee (phase 10 du
#   plan). Cette moyenne est dominee par les trimestres calmes, ou la passerelle
#   est mauvaise. Le poids sous-pondere donc la passerelle PRECISEMENT au moment
#   ou elle est sur le point d'avoir raison. C'est un defaut structurel du
#   critere, pas un defaut d'estimation : aucun echantillon plus long ne le
#   corrigerait.
#
# LA CORRECTION
#   Faire dependre le poids de l'ETAT signale par les indicateurs. Quand la
#   passerelle annonce un mouvement de grande amplitude au regard de la
#   volatilite habituelle de la branche, elle porte une information que le BVAR
#   n'a structurellement pas, et c'est a elle qu'il faut donner le poids.
#
#       z_T = | g_bridge,T | / sigma_{j, t<T}
#
#       delta_T = 0           si z_T >= z0   (tout a la passerelle)
#               = delta_base  sinon           (le poids habituel)
#
#   sigma est recalcule a chaque origine sur t < T, et z_T n'utilise que la
#   prevision de la passerelle, disponible avant la VA. Rien de posterieur a la
#   cible n'intervient.
#
# POURQUOI UNE REGLE SIMPLE ET NON OPTIMISEE
#   Il n'y a QU'UN SEUL episode de choc dans l'echantillon. Optimiser z0 sur
#   2020 reviendrait a ajuster un parametre sur une observation -- exactement la
#   limite 14.2 de la phase 3. Le seuil est donc fixe A PRIORI a z0 = 2, la
#   convention usuelle des deux ecarts-types, et n'est pas choisi dans les
#   donnees. La sensibilite a z0 est REPORTEE, pour montrer si le resultat tient
#   ou s'il depend du seuil -- mais elle ne sert jamais a choisir.
#
# SORTIES
#   resultats/04d_combinaison_etat.csv
#   resultats/04d_sensibilite_seuil.csv
#   resultats/04d_par_periode.csv
#   figures/04d_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("04d_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("04d_", x))

Z0            <- 2.0    # fixe A PRIORI, jamais choisi dans les donnees
MIN_OBS_DELTA <- 8L

cat("\n[1/4] Bases\n")
va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)

bvar <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_recursives.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(branche, origine, bvar = prevision, reel)

passerelles <- lire_csv(file.path(DOSSIER_RESULTATS, "04b_previsions_variantes.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::filter(specification %in% c("reference (phase 4)",
                                     "1+4. disponibilite + terme AR")) %>%
  dplyr::select(variante = specification, branche, origine, bridge = prevision)
cat(sprintf("      %d variantes de passerelle, %d branches\n",
            dplyr::n_distinct(passerelles$variante),
            dplyr::n_distinct(passerelles$branche)))

#' Ecart-type de la branche sur la seule information anterieure a la cible.
sigma_recursif <- function(b, cible) {
  h <- va$g[va$branche == b & va$date < cible]
  if (length(h) < 12L) NA_real_ else stats::sd(h)
}

# ============================================================================
# 2) COMBINAISONS
# ============================================================================
cat("\n[2/4] Combinaisons : constante et dependante de l'etat\n")

construire <- function(df, z0) {
  df %>% dplyr::arrange(variante, branche, origine) %>%
    dplyr::group_by(variante, branche) %>%
    dplyr::group_modify(function(g, cle) {
      g$delta_base <- NA_real_; g$z <- NA_real_
      g$delta_etat <- NA_real_
      g$comb_constante <- NA_real_; g$comb_etat <- NA_real_
      for (i in seq_len(nrow(g))) {
        hist <- g[seq_len(i - 1L), ] %>%
          dplyr::transmute(reel, bvar, bridge)
        pd <- poids_combinaison(hist, min_obs = MIN_OBS_DELTA)
        g$delta_base[i] <- pd$delta

        sg <- sigma_recursif(cle$branche, g$origine[i])
        zi <- if (is.na(sg) || is.na(g$bridge[i]) || sg <= 0) NA_real_
              else abs(g$bridge[i]) / sg
        g$z[i] <- zi
        # Basculement : la passerelle annonce un mouvement inhabituel.
        g$delta_etat[i] <- if (!is.na(zi) && zi >= z0) 0 else pd$delta

        melange <- function(dl) {
          if (is.na(g$bridge[i]) && is.na(g$bvar[i])) NA_real_
          else if (is.na(g$bridge[i])) g$bvar[i]
          else if (is.na(g$bvar[i])) g$bridge[i]
          else dl * g$bvar[i] + (1 - dl) * g$bridge[i]
        }
        g$comb_constante[i] <- melange(pd$delta)
        g$comb_etat[i]      <- melange(g$delta_etat[i])
      }
      g
    }) %>% dplyr::ungroup()
}

base <- passerelles %>%
  dplyr::inner_join(bvar, by = c("branche", "origine"))
comb <- construire(base, Z0)
ecrire_csv(comb, chemin_res("combinaison_etat.csv"))

n_bascule <- sum(!is.na(comb$z) & comb$z >= Z0)
cat(sprintf("      seuil z0 = %.1f | %d basculements sur %d cas (%.1f%%)\n",
            Z0, n_bascule, sum(!is.na(comb$z)),
            100 * n_bascule / sum(!is.na(comb$z))))
cat(sprintf("      dont %d en 2020\n",
            sum(!is.na(comb$z) & comb$z >= Z0 &
                  lubridate::year(comb$origine) == 2020)))

# ============================================================================
# 3) EVALUATION, PAR PERIODE
# ============================================================================
cat("\n[3/4] Evaluation\n")
# On separe 2020 du reste. Un systeme dont la valeur est concentree sur les
# ruptures ne peut pas etre juge par une moyenne sur 48 trimestres : cette
# moyenne ecrase precisement ce qui l'interesse.
evaluer <- function(df, periode_lab) {
  df %>%
    tidyr::pivot_longer(c(bvar, bridge, comb_constante, comb_etat),
                        names_to = "modele", values_to = "prevu") %>%
    dplyr::filter(!is.na(prevu), !is.na(reel)) %>%
    dplyr::group_by(variante, modele, branche) %>%
    dplyr::filter(dplyr::n() >= 4L) %>%
    dplyr::summarise(n = dplyr::n(),
                     mae = mean(abs(reel - prevu)),
                     ratio = sqrt(mean((reel - prevu)^2)) / stats::sd(reel),
                     .groups = "drop") %>%
    dplyr::group_by(variante, modele) %>%
    dplyr::summarise(branches = dplyr::n(),
                     mae_moyenne = mean(mae),
                     ratio_median = stats::median(ratio),
                     n_ratio_ok = sum(ratio < 1), .groups = "drop") %>%
    dplyr::mutate(periode = periode_lab)
}

par_periode <- dplyr::bind_rows(
  evaluer(comb, "toutes origines"),
  evaluer(comb %>% dplyr::filter(lubridate::year(origine) == 2020), "2020"),
  evaluer(comb %>% dplyr::filter(lubridate::year(origine) != 2020), "hors 2020"))
ecrire_csv(par_periode, chemin_res("par_periode.csv"))

etiquettes <- c(bvar = "BVAR seul", bridge = "Passerelle seule",
                comb_constante = "Combinaison, delta constant",
                comb_etat = "Combinaison, delta selon l'etat")
for (p in c("toutes origines", "2020", "hors 2020")) {
  cat(sprintf("\n      --- %s ---\n", p))
  print(par_periode %>% dplyr::filter(periode == p) %>%
          dplyr::arrange(variante, ratio_median) %>%
          dplyr::transmute(variante = substr(variante, 1, 20),
                           modele = etiquettes[modele],
                           `MAE` = round(mae_moyenne, 4),
                           `ratio med.` = round(ratio_median, 3),
                           `br. < 1` = n_ratio_ok), n = 12)
}

# ============================================================================
# 4) SENSIBILITE AU SEUIL -- reportee, jamais utilisee pour choisir
# ============================================================================
cat("\n[4/4] Sensibilite au seuil z0\n")
sensibilite <- purrr::map_dfr(c(1.0, 1.5, 2.0, 2.5, 3.0, Inf), function(z0) {
  cc <- construire(base, z0)
  dplyr::bind_rows(
    evaluer(cc, "toutes origines"),
    evaluer(cc %>% dplyr::filter(lubridate::year(origine) == 2020), "2020"),
    evaluer(cc %>% dplyr::filter(lubridate::year(origine) != 2020), "hors 2020")) %>%
    dplyr::filter(modele == "comb_etat") %>%
    dplyr::mutate(z0 = z0,
                  bascules = sum(!is.na(cc$z) & cc$z >= z0))
})
ecrire_csv(sensibilite, chemin_res("sensibilite_seuil.csv"))
print(sensibilite %>%
        dplyr::transmute(variante = substr(variante, 1, 20), z0, periode,
                         `MAE` = round(mae_moyenne, 4),
                         `ratio med.` = round(ratio_median, 3)) %>%
        dplyr::arrange(variante, periode, z0), n = 40)
cat("\n      z0 = Inf correspond au delta constant : aucun basculement.\n")

# --- figures -----------------------------------------------------------------
g1 <- par_periode %>%
  dplyr::mutate(modele = etiquettes[modele],
                periode = factor(periode,
                                 levels = c("2020", "hors 2020", "toutes origines"))) %>%
  ggplot2::ggplot(ggplot2::aes(periode, mae_moyenne, fill = modele)) +
  ggplot2::geom_col(position = "dodge", width = 0.7) +
  ggplot2::facet_wrap(~ variante, scales = "free_y") +
  ggplot2::labs(x = NULL, y = "erreur absolue moyenne", fill = NULL,
                title = "La valeur de la passerelle est concentree sur la rupture",
                subtitle = "une moyenne sur toutes les origines ecrase ce que le systeme sait faire")
ggplot2::ggsave(chemin_fig("mae_par_periode.png"), g1, width = 11, height = 5, dpi = 150)

g2 <- comb %>% dplyr::filter(!is.na(z)) %>%
  ggplot2::ggplot(ggplot2::aes(origine, z)) +
  ggplot2::geom_hline(yintercept = Z0, linetype = "dashed", colour = "#b03a2e") +
  ggplot2::geom_point(size = 0.7, alpha = 0.6) +
  ggplot2::facet_wrap(~ variante) +
  ggplot2::labs(x = NULL, y = "z = |prevision passerelle| / ecart-type de la branche",
                title = "Le signal d'etat, origine par origine",
                subtitle = "au-dessus du trait, le poids bascule vers la passerelle")
ggplot2::ggsave(chemin_fig("signal_etat.png"), g2, width = 10, height = 4.5, dpi = 150)

cat("\n      figures/04d_mae_par_periode.png\n      figures/04d_signal_etat.png\n")
cat("\nPhase 4 quater terminee.\n")
