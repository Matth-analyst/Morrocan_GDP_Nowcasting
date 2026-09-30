# ============================================================================
# 09b_backtest_etendu.R -- Backtest etendu a 2009 : un second episode de choc
# ============================================================================
# POURQUOI
#   Deux limites du projet n'en font qu'une, et le calcul de puissance le
#   montre. Sur l'exercice principal (48 origines, T2-2014 a T1-2026), le test
#   groupe donne, pour la combinaison contre le BVAR seul :
#
#       M1 : stat 1,74   p 0,088   significatif
#       M2 : stat 1,56   p 0,126
#       M3 : stat 1,39   p 0,173
#
#   La statistique de Diebold-Mariano croit comme sqrt(n) a effet constant. Il
#   faudrait donc n = 48 x (1,68/1,39)^2 = 71 origines pour que M3 franchisse le
#   seuil, soit 23 de plus. Reculer la premiere cible a 2009 en apporte 22 :
#
#       M1 : p passerait de 0,088 a 0,039
#       M2 : p passerait de 0,126 a 0,064
#       M3 : p passerait de 0,173 a 0,099
#
#   Et la regle de detection de la phase 3 identifie T4-2008 comme choc. Le
#   backtest etendu apporte donc a la fois la PUISSANCE qui manque et le SECOND
#   EPISODE de rupture sans lequel l'affirmation "la passerelle voit les chocs"
#   restait invérifiable.
#
# CE QUE CET EXERCICE N'EST PAS
#   Il ne remplace pas l'exercice principal, il le complete. Deux raisons.
#
#   (a) La couverture s'effondre avant 2014 : 6 branches au lieu de 12, et
#       seules trois ont une vraie profondeur -- finances et assurances,
#       construction, electricite-gaz-eau. Les resultats ne valent que pour
#       elles.
#   (b) La valeur ajoutee NOMINALE, qui fournit les poids en prix courants, ne
#       commence qu'en 2014. Aucune agregation n'est donc possible ici. Ce n'est
#       pas genant : cet exercice est branche par branche, et l'evaluation
#       branche par branche n'a besoin que de la VA reelle, disponible depuis
#       1998.
#
#   L'exercice principal reste donc T2-2014 a T1-2026, sur 16 branches, et c'est
#   lui qui produira le nowcast. Celui-ci ne sert qu'a trancher UNE question :
#   l'avantage intra-trimestriel est-il distinguable du bruit ?
#
# SORTIES
#   resultats/09b_previsions_etendu.csv
#   resultats/09b_dm_groupe.csv
#   resultats/09b_par_episode.csv
#   figures/09b_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")
source("R/fonctions/kalman.R")
source("R/fonctions/bvar.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("09b_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("09b_", x))

# T2-2008, et non 2009 : la regle de detection identifie T4-2008 comme choc, et
# une premiere cible en 2009 en ferait de l'HISTORIQUE, jamais une cible. Le
# second episode de rupture serait alors absent du backtest -- ce qui viderait
# la moitie de la justification de cet exercice. Reculer de trois trimestres
# coute une branche (4 au lieu de 5) et fait entrer T4-2008 dans l'evaluation.
PREMIERE_CIBLE <- as.Date("2008-06-30")
BASCULE        <- as.Date("2014-06-30")   # debut de l'exercice principal
SEUIL_R <- 0.15; SEUIL_P <- 0.10
MIN_OBS_SEL <- 20L; MAX_RETENUS <- 5L; MIN_OBS_DELTA <- 8L
MIN_SERIES_BRANCHE <- 3L   # une branche n'entre que si elle a de quoi choisir
SCENARIOS <- c("M0", "M1", "M2", "M3")
INCLURE_TRIMESTRIELS_M3 <- FALSE   # decision de la phase 13

cat("\n[1/6] Bases\n")
couverture <- charger_couverture(); BC <- couverture$couvertes
ind_tous <- charger_indicateurs(branches = BC)
meta <- charger_metadonnees() %>% dplyr::select(id_serie, agregation, transformation)

va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)

# --- quelles branches supportent l'anteriorite ? -----------------------------
# On tranche sur la donnee : une branche entre si elle dispose d'au moins
# MIN_SERIES_BRANCHE series exploitables DES LA PREMIERE ORIGINE. Choisir les
# branches a la main reviendrait a les selectionner sur ce qu'on espere y
# trouver.
trim_tous <- indicateurs_trimestriels(ind_tous, tolerant = TRUE)
dispo <- trim_tous %>%
  dplyr::filter(date < PREMIERE_CIBLE, !is.na(x)) %>%
  dplyr::group_by(branche, id_serie) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n >= MIN_OBS_SEL) %>%
  dplyr::count(branche, name = "series")
BRANCHES <- dispo$branche[dispo$series >= MIN_SERIES_BRANCHE]
cat(sprintf("      %d branches retenues (>= %d series des %s) :\n",
            length(BRANCHES), MIN_SERIES_BRANCHE, format(PREMIERE_CIBLE)))
print(as.data.frame(dispo %>% dplyr::filter(branche %in% BRANCHES) %>%
                      dplyr::arrange(dplyr::desc(series))), row.names = FALSE)

ind <- ind_tous %>% dplyr::filter(branche %in% BRANCHES)
longueur <- ind %>% dplyr::count(id_serie, name = "n_obs")
ind <- ind %>% dplyr::filter(id_serie %in% longueur$id_serie[longueur$n_obs >= 36L])
cat(sprintf("      %d series a traiter (contre 393 dans l'exercice principal)\n",
            dplyr::n_distinct(ind$id_serie)))

origines <- sort(unique(va$date[va$date >= PREMIERE_CIBLE]))
cat(sprintf("      %d origines, de %s a %s (dont %d avant %s)\n",
            length(origines), date_vers_trimestre(origines[1]),
            date_vers_trimestre(origines[length(origines)]),
            sum(origines < BASCULE), date_vers_trimestre(BASCULE)))

# --- les chocs, avec la regle retenue en phase 3 -----------------------------
large <- va %>% tidyr::pivot_wider(names_from = branche, values_from = g) %>%
  dplyr::arrange(date) %>%
  dplyr::filter(dplyr::if_all(dplyr::all_of(TOUTES_BRANCHES), ~ !is.na(.)))
chocs_complets <- detecter_chocs(as.matrix(large[, TOUTES_BRANCHES]), large$date,
                                 z = 4, k = 3)
cat(sprintf("      chocs sur l'echantillon complet : %s\n",
            paste(date_vers_trimestre(chocs_complets), collapse = ", ")))
chocs_dans_backtest <- chocs_complets[chocs_complets >= PREMIERE_CIBLE]
cat(sprintf("      dont dans le backtest etendu : %s\n",
            paste(date_vers_trimestre(chocs_dans_backtest), collapse = ", ")))

# Previsions BVAR. On prend celles du BVAR ETENDU (R/03d_bvar_etendu.R), qui
# remontent a T2-2008 : celles de la phase 3 s'arretent a T2-2014 et feraient
# retomber tous les tests impliquant le BVAR a 48 origines, ce qui viderait
# l'exercice de son objet. Le script 03d verifie que les deux coincident a la
# precision machine sur les origines communes.
f_bvar <- file.path(DOSSIER_RESULTATS, "03d_previsions_bvar_etendu.csv")
if (!file.exists(f_bvar)) {
  stop("03d_previsions_bvar_etendu.csv absent : executer d'abord R/03d_bvar_etendu.R",
       call. = FALSE)
}
bvar <- lire_csv(f_bvar) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(branche, origine, bvar = prevision)

# ============================================================================
# 2) UNE ORIGINE, UN SCENARIO
# ============================================================================
traiter <- function(cible, scenario) {
  n_vus <- switch(scenario, M0 = 0L, M1 = 1L, M2 = 2L, M3 = 3L)
  mois_T <- mois_du_trimestre(cible)
  mois_vus <- if (n_vus > 0L) mois_T[seq_len(n_vus)] else as.Date(character(0))
  a_prevoir <- as.Date(setdiff(mois_T, mois_vus), origin = "1970-01-01")

  info <- information_set_intra(ind, cible, scenario = scenario)
  if (!INCLURE_TRIMESTRIELS_M3) {
    info <- info %>%
      dplyr::filter(!(frequence == "trimestriel" &
                        date >= debut_trimestre(cible) & date <= fin_trimestre(cible)))
  }
  mensuel <- info %>% dplyr::filter(frequence == "mensuel")
  trimest <- info %>% dplyr::filter(frequence == "trimestriel")

  # Bord irregulier : mois manquants determines serie par serie (voir R/09).
  if (nrow(mensuel) > 0L) {
    mensuel <- mensuel %>%
      dplyr::group_by(id_serie, branche, indicateur) %>%
      dplyr::group_modify(function(g, cle) {
        manquants <- as.Date(setdiff(mois_T, g$date), origin = "1970-01-01")
        if (length(manquants) == 0L) return(dplyr::select(g, date, valeur))
        r <- prevoir_mois_manquants(g$date, g$valeur, manquants)
        if (is.null(r)) return(dplyr::select(g, date, valeur))
        dplyr::select(r, date, valeur)
      }) %>% dplyr::ungroup()
  } else {
    mensuel <- mensuel %>% dplyr::select(id_serie, branche, indicateur, date, valeur)
  }

  base_long <- dplyr::bind_rows(
    mensuel %>% dplyr::mutate(frequence = "mensuel") %>%
      dplyr::select(id_serie, branche, indicateur, frequence, date, valeur),
    trimest %>% dplyr::select(id_serie, branche, indicateur, frequence, date, valeur)) %>%
    dplyr::left_join(meta, by = "id_serie")
  if (nrow(base_long) == 0L) return(NULL)

  trim <- indicateurs_trimestriels(base_long, tolerant = TRUE)
  d <- trim %>% dplyr::group_by(id_serie) %>%
    dplyr::summarise(n = sum(!is.na(x)), .groups = "drop")
  tok <- trim %>%
    dplyr::filter(id_serie %in% d$id_serie[d$n >= MIN_OBS_SEL], !is.na(x)) %>%
    dplyr::select(id_serie, branche, date, x)

  purrr::map_dfr(BRANCHES, function(b) {
    g_b <- va %>% dplyr::filter(branche == b) %>% dplyr::select(date, g)
    x_b <- tok %>% dplyr::filter(branche == b) %>% dplyr::select(id_serie, date, x)
    if (nrow(x_b) == 0L) return(NULL)
    x_l <- x_b %>% tidyr::pivot_wider(names_from = id_serie, values_from = x)
    s <- selectionner_disponible(g_b, x_b, x_l, cible, SEUIL_R, SEUIL_P,
                                 MIN_OBS_SEL, MAX_RETENUS)
    r <- if (!is.null(s)) estimer_bridge_ar(g_b, x_l, s$ids, cible, avec_ar = TRUE)
         else NULL
    reel <- g_b$g[g_b$date == cible]
    tibble::tibble(scenario = scenario, branche = b, origine = cible,
                   n_retenus = if (is.null(s)) 0L else length(s$ids),
                   bridge = if (is.null(r)) NA_real_ else r$prevision,
                   derniere_obs = if (is.null(r)) as.Date(NA) else r$derniere_obs,
                   reel = if (length(reel) == 1L) reel else NA_real_)
  })
}

cat("\n[2/6] Execution\n")
t0 <- Sys.time()
DOSSIER_REPRISE <- file.path(DOSSIER_RESULTATS, "09b_reprise")
dir.create(DOSSIER_REPRISE, showWarnings = FALSE, recursive = TRUE)

n_coeurs <- max(1L, min(3L, parallel::detectCores() - 1L))
cl <- parallel::makeCluster(n_coeurs)
rep_travail <- getwd()
parallel::clusterExport(cl, "rep_travail", envir = environment())
parallel::clusterEvalQ(cl, {
  setwd(rep_travail)
  suppressMessages({
    source("R/00_setup.R"); source("R/fonctions/information_set.R")
    source("R/fonctions/transformations.R"); source("R/fonctions/donnees.R")
    source("R/fonctions/passerelle.R"); source("R/fonctions/kalman.R")
  })
  NULL
})
taches <- expand.grid(i = seq_along(origines), s = SCENARIOS, stringsAsFactors = FALSE)
parallel::clusterExport(cl, c("ind", "meta", "va", "BRANCHES", "origines",
                              "SCENARIOS", "INCLURE_TRIMESTRIELS_M3", "SEUIL_R",
                              "SEUIL_P", "MIN_OBS_SEL", "MAX_RETENUS", "traiter",
                              "taches", "DOSSIER_REPRISE"), envir = environment())

res <- parallel::parLapply(cl, seq_len(nrow(taches)), function(k) {
  f <- file.path(DOSSIER_REPRISE, sprintf("%s_%s.csv", taches$s[k],
                                          format(origines[taches$i[k]], "%Y%m%d")))
  if (file.exists(f)) {
    return(lire_csv(f) %>% dplyr::mutate(origine = as.Date(origine),
                                         derniere_obs = as.Date(derniere_obs)))
  }
  out <- tryCatch(traiter(origines[taches$i[k]], taches$s[k]),
                  error = function(e) NULL)
  if (!is.null(out) && nrow(out) > 0L) ecrire_csv(out, f)
  out
})
parallel::stopCluster(cl)
previsions <- dplyr::bind_rows(res)
cat(sprintf("      %s | %d lignes\n",
            format(round(difftime(Sys.time(), t0, units = "mins"), 1)),
            nrow(previsions)))
stopifnot("[ANTI-LOOK-AHEAD] une estimation a vu sa cible" =
            all(previsions$derniere_obs < previsions$origine, na.rm = TRUE))
cat("      controle : aucune estimation ne contient sa propre cible\n")

# ============================================================================
# 3) COMBINAISON
# ============================================================================
cat("\n[3/6] Combinaison avec le BVAR\n")
comb <- previsions %>%
  dplyr::left_join(bvar, by = c("branche", "origine")) %>%
  dplyr::arrange(scenario, branche, origine) %>%
  dplyr::group_by(scenario, branche) %>%
  dplyr::group_modify(function(g, cle) {
    g$delta <- NA_real_; g$combinee <- NA_real_
    for (i in seq_len(nrow(g))) {
      h <- g[seq_len(i - 1L), ] %>% dplyr::transmute(reel, bvar, bridge)
      pd <- poids_combinaison(h, min_obs = MIN_OBS_DELTA)
      g$delta[i] <- pd$delta
      g$combinee[i] <- if (is.na(g$bridge[i])) g$bvar[i]
        else if (is.na(g$bvar[i])) g$bridge[i]
        else pd$delta * g$bvar[i] + (1 - pd$delta) * g$bridge[i]
    }
    g
  }) %>% dplyr::ungroup()
ecrire_csv(comb, chemin_res("previsions_etendu.csv"))
cat(sprintf("      previsions BVAR disponibles sur %d lignes sur %d\n",
            sum(!is.na(comb$bvar)), nrow(comb)))

# ============================================================================
# 4) TEST GROUPE, SUR LES DEUX ECHANTILLONS
# ============================================================================
cat("\n[4/6] Diebold-Mariano groupe\n")
#' @param ech_ref variances de branche, calculees UNE FOIS sur l'echantillon
#'   complet. Les recalculer sur chaque sous-echantillon ferait varier la
#'   ponderation entre branches, et deux sous-echantillons donneraient des
#'   statistiques differentes sans aucune donnee supplementaire -- un artefact
#'   de normalisation qu'on prendrait pour un gain de puissance.
dm_groupe <- function(d_sc, ref, alt, ech_ref) {
  ech <- ech_ref
  s <- d_sc %>%
    dplyr::filter(!is.na(reel), !is.na(.data[[ref]]), !is.na(.data[[alt]])) %>%
    dplyr::left_join(ech, by = "branche") %>%
    dplyr::filter(is.finite(v), v > 0) %>%
    dplyr::mutate(d = ((reel - .data[[ref]])^2 - (reel - .data[[alt]])^2) / v) %>%
    dplyr::group_by(origine) %>%
    dplyr::summarise(d = mean(d), .groups = "drop") %>% dplyr::arrange(origine)
  if (nrow(s) < 10L) return(NULL)
  n <- nrow(s); dbar <- mean(s$d)
  L <- max(1L, floor(4 * (n / 100)^(2/9)))
  g0 <- stats::var(s$d) * (n - 1) / n
  gam <- vapply(seq_len(L), function(l)
    mean((s$d[-(1:l)] - dbar) * (s$d[1:(n - l)] - dbar)), numeric(1))
  v_lr <- g0 + 2 * sum((1 - seq_len(L) / (L + 1)) * gam)
  if (!is.finite(v_lr) || v_lr <= 0) return(NULL)
  stat <- dbar / sqrt(v_lr / n)
  tibble::tibble(n_origines = n, statistique = stat,
                 p_value = 2 * stats::pt(-abs(stat), df = n - 1L))
}

paires <- list(list(lab = "Combinaison contre BVAR seul", ref = "bvar", alt = "combinee"),
               list(lab = "Passerelle contre BVAR seul",  ref = "bvar", alt = "bridge"),
               list(lab = "Combinaison contre Passerelle", ref = "bridge", alt = "combinee"))
# Variances de reference, figees sur l'echantillon complet.
variances_branche <- comb %>% dplyr::group_by(branche) %>%
  dplyr::summarise(v = stats::var(reel, na.rm = TRUE), .groups = "drop")

echantillons <- list(
  list(lab = "etendu (2009-2026)", d = comb),
  list(lab = "principal (2014-2026)", d = comb %>% dplyr::filter(origine >= BASCULE)),
  list(lab = "avant 2014 seulement", d = comb %>% dplyr::filter(origine < BASCULE)))

tests <- purrr::map_dfr(echantillons, function(e) {
  purrr::map_dfr(SCENARIOS, function(sc) {
    d_sc <- e$d %>% dplyr::filter(scenario == sc)
    purrr::map_dfr(paires, function(p) {
      g <- dm_groupe(d_sc, p$ref, p$alt, variances_branche)
      if (is.null(g)) return(NULL)
      dplyr::mutate(g, echantillon = e$lab, scenario = sc, comparaison = p$lab,
                    .before = 1)
    })
  })
})
ecrire_csv(tests, chemin_res("dm_groupe.csv"))
cat("\n      --- Combinaison contre BVAR seul ---\n")
print(tests %>% dplyr::filter(comparaison == "Combinaison contre BVAR seul") %>%
        dplyr::transmute(echantillon, scenario, `n orig.` = n_origines,
                         `stat.` = round(statistique, 2),
                         `p` = round(p_value, 4),
                         significatif = ifelse(p_value < 0.10, "oui", "")), n = 20)

# ============================================================================
# 5) LES DEUX EPISODES DE CHOC
# ============================================================================
cat("\n[5/6] Comportement par episode\n")
episode <- function(d) {
  dplyr::case_when(
    lubridate::year(d) %in% c(2008, 2009) ~ "crise 2008-2009",
    lubridate::year(d) == 2020            ~ "covid 2020",
    TRUE                                  ~ "periode calme")
}
par_ep <- comb %>% dplyr::mutate(episode = episode(origine)) %>%
  tidyr::pivot_longer(c(bvar, bridge, combinee), names_to = "modele",
                      values_to = "prevu") %>%
  dplyr::filter(!is.na(prevu), !is.na(reel)) %>%
  dplyr::group_by(episode, scenario, modele) %>%
  dplyr::summarise(n = dplyr::n(), mae = mean(abs(reel - prevu)), .groups = "drop")
ecrire_csv(par_ep, chemin_res("par_episode.csv"))
cat("\n      --- erreur absolue moyenne par episode ---\n")
print(par_ep %>%
        dplyr::mutate(modele = dplyr::recode(modele, bvar = "BVAR",
                                             bridge = "Passerelle",
                                             combinee = "Combinaison")) %>%
        tidyr::pivot_wider(names_from = modele, values_from = c(mae, n)) %>%
        dplyr::transmute(episode, scenario, n = n_BVAR,
                         BVAR = round(mae_BVAR, 4),
                         Passerelle = round(mae_Passerelle, 4),
                         Combinaison = round(mae_Combinaison, 4)), n = 20)

# ============================================================================
# 6) FIGURES
# ============================================================================
cat("\n[6/6] Figures\n")
g1 <- tests %>% dplyr::filter(comparaison == "Combinaison contre BVAR seul") %>%
  ggplot2::ggplot(ggplot2::aes(scenario, p_value, colour = echantillon,
                               group = echantillon)) +
  ggplot2::geom_hline(yintercept = 0.10, linetype = "dashed", linewidth = 0.3) +
  ggplot2::geom_line(linewidth = 0.55) + ggplot2::geom_point(size = 2.2) +
  ggplot2::labs(x = NULL, y = "p-value du test groupe", colour = NULL,
                title = "Ce que la puissance change",
                subtitle = "sous le trait, l'ecart avec le BVAR est distinguable du bruit")
ggplot2::ggsave(chemin_fig("puissance.png"), g1, width = 9, height = 4.5, dpi = 150)

g2 <- par_ep %>%
  dplyr::mutate(modele = dplyr::recode(modele, bvar = "BVAR",
                                       bridge = "Passerelle", combinee = "Combinaison")) %>%
  ggplot2::ggplot(ggplot2::aes(scenario, mae, fill = modele)) +
  ggplot2::geom_col(position = "dodge", width = 0.7) +
  ggplot2::facet_wrap(~ episode, scales = "free_y") +
  ggplot2::labs(x = NULL, y = "erreur absolue moyenne", fill = NULL,
                title = "Les deux episodes de rupture, et les periodes calmes",
                subtitle = "la passerelle voit-elle 2008 comme elle a vu 2020 ?")
ggplot2::ggsave(chemin_fig("episodes.png"), g2, width = 11, height = 4.5, dpi = 150)

cat("      figures/09b_puissance.png\n      figures/09b_episodes.png\n")
cat("\nBacktest etendu termine.\n")
