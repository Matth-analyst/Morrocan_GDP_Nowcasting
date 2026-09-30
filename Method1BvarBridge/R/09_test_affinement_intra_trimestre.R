# ============================================================================
# 09_test_affinement_intra_trimestre.R -- PHASE 13 : nowcasting M0/M1/M2/M3
# ============================================================================
# CE QUE MESURE CETTE PHASE
#   Jusqu'ici la passerelle a toujours ete jugee sur un trimestre COMPLET, ce
#   qui la met dans la position la moins favorable : a ce moment-la, le BVAR
#   dispose lui aussi de toute l'information trimestrielle. Or l'interet d'une
#   passerelle n'est pas d'etre meilleure a la fin du trimestre, c'est de dire
#   quelque chose AVANT -- des le premier ou le deuxieme mois, quand le passe
#   des valeurs ajoutees n'a rien de nouveau a apporter.
#
#   Les quatre scenarios mesurent la valeur de l'information au fil du
#   trimestre :
#
#     M0   aucun mois du trimestre cible observe
#     M1   un mois observe, deux a prevoir
#     M2   deux mois observes, un a prevoir
#     M3   les trois mois observes, et les indicateurs TRIMESTRIELS disponibles
#
#   Le BVAR, lui, est IDENTIQUE dans les quatre scenarios : il ne lit que le
#   passe de la VA, qui ne change pas d'un mois a l'autre a l'interieur du
#   trimestre. Il sert donc d'etalon fixe, et l'ecart entre M0 et M3 mesure
#   exactement ce que les indicateurs mensuels apportent, et a quel moment.
#
# CE QUE LE PLAN INTERDIT, ET QUI EST RESPECTE
#   "Il ne faut donc plus calculer M1/M2 comme une simple moyenne des mois
#   observes." Sommer deux mois au lieu de trois sous-estimerait le trimestre
#   d'un tiers. Les mois manquants sont donc PREVUS, puis l'agregation se fait
#   sur trois mois -- un observe et deux prevus en M1, par exemple. La regle
#   d'agregation reste celle de la metadonnee : sum, mean ou last.
#
#   La prevision de mois utilise `prevoir_mois_manquants()`, distincte du
#   comblement de trous : ici rien ne vient apres la date estimee, le modele
#   extrapole au lieu de lisser. Les confondre reviendrait a se donner en M1
#   une precision qu'on n'a qu'en M3, donc a fabriquer le resultat cherche.
#
# SORTIES
#   resultats/09_previsions_intra.csv
#   resultats/09_comparaison_scenarios.csv
#   resultats/09_par_periode.csv
#   figures/09_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")
source("R/fonctions/kalman.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("09_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("09_", x))

PREMIERE_CIBLE <- as.Date("2014-06-30")
SEUIL_R <- 0.15; SEUIL_P <- 0.10
MIN_OBS_SEL <- 20L; MAX_RETENUS <- 5L; MIN_OBS_DELTA <- 8L
SCENARIOS <- c("M0", "M1", "M2", "M3")

# --- Les indicateurs TRIMESTRIELS du trimestre cible sont-ils admis en M3 ? ---
# La phase 14 du plan prevoit de les rendre disponibles en M3, pour "mesurer
# separement l'apport des indicateurs mensuels et des indicateurs trimestriels".
# La mesure a ete faite, et leur apport est NEGATIF. A perimetre commun, sur la
# passerelle seule en M3 :
#
#     avec indicateurs trimestriels      ratio 0,975    3,13 retenus
#     sans indicateurs trimestriels      ratio 0,934    2,88 retenus
#
# Les admettre rendait de surcroit la progression NON MONOTONE -- M3 etait moins
# bon que M2 (0,975 contre 0,962) -- alors qu'ajouter un troisieme mois OBSERVE
# ne peut pas degrader : il remplace une valeur extrapolee par une donnee.
#
# Deux corrections ont ete testees et ont ECHOUE :
#   - seuil de correlation renforce pour eux seuls : la courbe est plate de 0,15
#     a 0,45 (0,975 -> 0,969) puis chute d'un coup a l'exclusion (0,934). Meme
#     ceux qui franchissent |r| >= 0,45 nuisent encore ;
#   - selection sur la correlation PARTIELLE, qui ecarte par construction ce qui
#     est redondant : elle degrade tout (0,999 sans eux, 1,071 avec) ET en
#     retient autant (0,53 contre 0,50). Ils ne sont donc PAS redondants.
#
# Lecture qui subsiste : ils sont correles a la VA sur l'historique mais
# INSTABLES hors echantillon. Aucun critere fonde sur l'ajustement passe --
# marginal ou partiel -- ne peut le voir ; seule l'evaluation hors echantillon
# le montre.
#
# Consequence a assumer : M3 ne se distingue plus de M2 que par le troisieme
# mois observe. La comparaison "apport des mensuels contre apport des
# trimestriels" voulue par la phase 14 a bien eu lieu, mais elle a conclu a
# l'exclusion des seconds. Le drapeau reste pour pouvoir refaire le test.
INCLURE_TRIMESTRIELS_M3 <- FALSE

cat("\n[1/5] Bases\n")
couverture <- charger_couverture(); BC <- couverture$couvertes
ind  <- charger_indicateurs(branches = BC)
meta <- charger_metadonnees() %>% dplyr::select(id_serie, agregation, transformation)

va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)
origines <- sort(unique(va$date[va$date >= PREMIERE_CIBLE]))

bvar <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_recursives.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(branche, origine, bvar = prevision, reel)

# Series suffisamment longues pour etre exploitables, mesure une fois sur la
# base complete : c'est une propriete de la serie, pas de l'origine.
longueur <- ind %>% dplyr::count(id_serie, name = "n_obs")
series_utiles <- longueur$id_serie[longueur$n_obs >= 36L]
ind <- ind %>% dplyr::filter(id_serie %in% series_utiles)
cat(sprintf("      %d series retenues (>= 36 observations)\n",
            dplyr::n_distinct(ind$id_serie)))
cat(sprintf("      %d origines x %d branches x %d scenarios\n",
            length(origines), length(BC), length(SCENARIOS)))

# ============================================================================
# 2) UNE ORIGINE, UN SCENARIO
# ============================================================================
traiter <- function(cible, scenario) {
  n_mois_vus <- switch(scenario, M0 = 0L, M1 = 1L, M2 = 2L, M3 = 3L)
  mois_T     <- mois_du_trimestre(cible)
  mois_vus   <- if (n_mois_vus > 0L) mois_T[seq_len(n_mois_vus)] else as.Date(character(0))
  mois_a_prevoir <- setdiff(mois_T, mois_vus)
  mois_a_prevoir <- as.Date(mois_a_prevoir, origin = "1970-01-01")

  # --- ensemble d'information du scenario ---------------------------------
  info <- information_set_intra(ind, cible, scenario = scenario)

  # Exclusion des indicateurs trimestriels du trimestre CIBLE (voir la note sur
  # INCLURE_TRIMESTRIELS_M3). Leur historique est conserve : seule leur
  # observation en T disparait, ce qui suffit a les ecarter de la selection,
  # laquelle n'admet que des series observees a la cible. En M0/M1/M2 ils
  # etaient deja absents, donc cette ligne ne modifie que M3.
  if (!INCLURE_TRIMESTRIELS_M3) {
    info <- info %>%
      dplyr::filter(!(frequence == "trimestriel" &
                        date >= debut_trimestre(cible) &
                        date <= fin_trimestre(cible)))
  }

  mensuel <- info %>% dplyr::filter(frequence == "mensuel")
  trimest <- info %>% dplyr::filter(frequence == "trimestriel")

  # --- prevision des mois manquants du trimestre cible ---------------------
  n_prevus <- 0L
  # BORD IRREGULIER. Les series ne s'arretent pas toutes au meme mois : a une
  # origine donnee, 19 a 40 % d'entre elles n'ont pas tous les mois que le
  # scenario autorise. Ne prevoir que les mois manquants AU SENS DU SCENARIO
  # laissait donc des trous propres a chaque serie, leur trimestre etait refuse
  # par `agreger_trimestriel`, et elles disparaissaient du vivier de candidats
  # sans que rien ne le signale. Les mois a prevoir sont donc determines SERIE
  # PAR SERIE.
  if (nrow(mensuel) > 0L) {
    mensuel <- mensuel %>%
      dplyr::group_by(id_serie, branche, indicateur) %>%
      dplyr::group_modify(function(g, cle) {
        manquants <- as.Date(setdiff(mois_T, g$date), origin = "1970-01-01")
        if (length(manquants) == 0L) {
          return(dplyr::select(g, date, valeur) %>% dplyr::mutate(prevu = FALSE))
        }
        r <- prevoir_mois_manquants(g$date, g$valeur, manquants)
        if (is.null(r)) return(dplyr::select(g, date, valeur) %>%
                                 dplyr::mutate(prevu = FALSE))
        dplyr::select(r, date, valeur, prevu)
      }) %>% dplyr::ungroup()
    n_prevus <- sum(mensuel$prevu)
  } else {
    mensuel <- mensuel %>% dplyr::select(id_serie, branche, indicateur, date, valeur) %>%
      dplyr::mutate(prevu = FALSE)
  }

  base_long <- dplyr::bind_rows(
    mensuel %>% dplyr::mutate(frequence = "mensuel") %>%
      dplyr::select(id_serie, branche, indicateur, frequence, date, valeur),
    trimest %>% dplyr::select(id_serie, branche, indicateur, frequence, date, valeur)) %>%
    dplyr::left_join(meta, by = "id_serie")
  if (nrow(base_long) == 0L) return(NULL)

  # tolerant = TRUE : ici des valeurs sont FABRIQUEES (mois prevus), et une
  # prevision bornee peut rendre un agregat nul. Une serie dont la
  # transformation echoue est ecartee, elle n'interrompt pas le calcul.
  trim <- indicateurs_trimestriels(base_long, tolerant = TRUE)
  d <- trim %>% dplyr::group_by(id_serie) %>%
    dplyr::summarise(n = sum(!is.na(x)), .groups = "drop")
  tok <- trim %>%
    dplyr::filter(id_serie %in% d$id_serie[d$n >= MIN_OBS_SEL], !is.na(x)) %>%
    dplyr::select(id_serie, branche, date, x)

  purrr::map_dfr(BC, function(b) {
    g_b <- va %>% dplyr::filter(branche == b) %>% dplyr::select(date, g)
    x_b <- tok %>% dplyr::filter(branche == b) %>% dplyr::select(id_serie, date, x)
    if (nrow(x_b) == 0L) return(NULL)
    x_l <- x_b %>% tidyr::pivot_wider(names_from = id_serie, values_from = x)

    s <- selectionner_disponible(g_b, x_b, x_l, cible, SEUIL_R, SEUIL_P,
                                 MIN_OBS_SEL, MAX_RETENUS)
    r <- if (!is.null(s)) estimer_bridge_ar(g_b, x_l, s$ids, cible, avec_ar = TRUE)
         else NULL
    reel <- g_b$g[g_b$date == cible]
    tibble::tibble(
      scenario = scenario, branche = b, origine = cible,
      n_mois_prevus = n_prevus,
      n_retenus = if (is.null(s)) 0L else length(s$ids),
      bridge = if (is.null(r)) NA_real_ else r$prevision,
      derniere_obs = if (is.null(r)) as.Date(NA) else r$derniere_obs,
      reel = if (length(reel) == 1L) reel else NA_real_)
  })
}

cat("\n[2/5] Execution des quatre scenarios\n")
t0 <- Sys.time()
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
parallel::clusterExport(cl, c("ind", "meta", "va", "BC", "origines", "SCENARIOS",
                              "INCLURE_TRIMESTRIELS_M3",
                              "SEUIL_R", "SEUIL_P", "MIN_OBS_SEL", "MAX_RETENUS",
                              "traiter"), envir = environment())
taches <- expand.grid(i = seq_along(origines), s = SCENARIOS,
                      stringsAsFactors = FALSE)
parallel::clusterExport(cl, "taches", envir = environment())
# Point de reprise : chaque tache ecrit son resultat des qu'il est pret.
# Un calcul de plusieurs heures qui ne produit rien avant la fin est ingerable --
# une seule serie defaillante avait deja fait perdre trois heures. Les taches
# deja faites sont relues au lieu d'etre refaites.
DOSSIER_REPRISE <- file.path(DOSSIER_RESULTATS, "09_reprise")
dir.create(DOSSIER_REPRISE, showWarnings = FALSE, recursive = TRUE)
parallel::clusterExport(cl, "DOSSIER_REPRISE", envir = environment())

res <- parallel::parLapply(cl, seq_len(nrow(taches)), function(k) {
  f <- file.path(DOSSIER_REPRISE,
                 sprintf("%s_%s_%s.csv", taches$s[k],
                         ifelse(INCLURE_TRIMESTRIELS_M3, "avecT", "sansT"),
                         format(origines[taches$i[k]], "%Y%m%d")))
  if (file.exists(f)) return(lire_csv(f) %>% dplyr::mutate(origine = as.Date(origine),
                                                           derniere_obs = as.Date(derniere_obs)))
  out <- tryCatch(traiter(origines[taches$i[k]], taches$s[k]),
                  error = function(e) {
                    tibble::tibble(scenario = taches$s[k], branche = NA_character_,
                                   origine = origines[taches$i[k]],
                                   n_mois_prevus = NA_integer_, n_retenus = NA_integer_,
                                   bridge = NA_real_, derniere_obs = as.Date(NA),
                                   reel = NA_real_, erreur = conditionMessage(e))
                  })
  if (!is.null(out) && nrow(out) > 0L) ecrire_csv(out, f)
  out
})
parallel::stopCluster(cl)
previsions <- dplyr::bind_rows(res)

if ("erreur" %in% names(previsions)) {
  ratees <- previsions %>% dplyr::filter(!is.na(erreur))
  if (nrow(ratees) > 0L) {
    cat(sprintf("      ! %d tache(s) en echec, ecartees :\n", nrow(ratees)))
    print(as.data.frame(dplyr::count(ratees, scenario, erreur)), row.names = FALSE)
  }
  previsions <- previsions %>% dplyr::filter(is.na(erreur)) %>% dplyr::select(-erreur)
}
previsions <- previsions %>% dplyr::filter(!is.na(branche))
cat(sprintf("      %s | %d lignes\n",
            format(round(difftime(Sys.time(), t0, units = "mins"), 1)),
            nrow(previsions)))

stopifnot("[ANTI-LOOK-AHEAD] une passerelle a vu sa cible" =
            all(previsions$derniere_obs < previsions$origine, na.rm = TRUE))
cat("      controle : aucune estimation ne contient sa propre cible\n")

# ============================================================================
# 3) COMBINAISON AVEC LE BVAR
# ============================================================================
cat("\n[3/5] Combinaison\n")
comb <- previsions %>%
  dplyr::left_join(bvar %>% dplyr::select(branche, origine, bvar),
                   by = c("branche", "origine")) %>%
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
ecrire_csv(comb, chemin_res("previsions_intra.csv"))

taux <- comb %>% dplyr::group_by(scenario) %>%
  dplyr::summarise(taux = mean(!is.na(bridge)),
                   mois_prevus_median = stats::median(n_mois_prevus),
                   .groups = "drop")
cat("\n      taux de production par scenario :\n")
for (i in seq_len(nrow(taux))) {
  cat(sprintf("        %s : %.0f%%\n", taux$scenario[i], 100 * taux$taux[i]))
}

# ============================================================================
# 4) EVALUATION
# ============================================================================
cat("\n[4/5] Evaluation\n")
evaluer <- function(df, lab) {
  df %>%
    tidyr::pivot_longer(c(bvar, bridge, combinee), names_to = "modele",
                        values_to = "prevu") %>%
    dplyr::filter(!is.na(prevu), !is.na(reel)) %>%
    dplyr::group_by(scenario, modele, branche) %>%
    dplyr::filter(dplyr::n() >= 4L) %>%
    dplyr::summarise(mae = mean(abs(reel - prevu)),
                     ratio = sqrt(mean((reel - prevu)^2)) / stats::sd(reel),
                     .groups = "drop") %>%
    dplyr::group_by(scenario, modele) %>%
    dplyr::summarise(branches = dplyr::n(),
                     mae_moyenne = mean(mae),
                     ratio_median = stats::median(ratio),
                     n_ratio_ok = sum(ratio < 1), .groups = "drop") %>%
    dplyr::mutate(periode = lab)
}
par_periode <- dplyr::bind_rows(
  evaluer(comb, "toutes origines"),
  evaluer(comb %>% dplyr::filter(lubridate::year(origine) == 2020), "2020"),
  evaluer(comb %>% dplyr::filter(lubridate::year(origine) != 2020), "hors 2020"))
ecrire_csv(par_periode, chemin_res("par_periode.csv"))
ecrire_csv(par_periode %>% dplyr::filter(periode == "toutes origines"),
           chemin_res("comparaison_scenarios.csv"))

etiq <- c(bvar = "BVAR seul", bridge = "Passerelle", combinee = "Combinaison")
for (p in c("toutes origines", "2020", "hors 2020")) {
  cat(sprintf("\n      --- %s ---\n", p))
  print(par_periode %>% dplyr::filter(periode == p) %>%
          dplyr::arrange(modele, scenario) %>%
          dplyr::transmute(scenario, modele = etiq[modele],
                           `MAE` = round(mae_moyenne, 4),
                           `ratio med.` = round(ratio_median, 3),
                           `br. < 1` = n_ratio_ok), n = 20)
}

# --- PERIMETRE COMMUN --------------------------------------------------------
# Les quatre scenarios ne produisent pas sur les memes couples branche-origine
# (57 % en M0/M1/M2 contre 72 % en M3). Comparer leurs ratios bruts reviendrait
# a comparer des epreuves de difficultes differentes : M3, qui reussit plus
# souvent, affronte aussi les cas les plus durs. On restreint donc aux couples
# ou LES QUATRE scenarios ont produit une prevision.
n_sc <- dplyr::n_distinct(comb$scenario)
commun <- comb %>% dplyr::filter(!is.na(bridge)) %>%
  dplyr::count(branche, origine) %>% dplyr::filter(n == n_sc) %>%
  dplyr::select(branche, origine)
sous <- comb %>% dplyr::inner_join(commun, by = c("branche", "origine"))

perimetre_commun <- dplyr::bind_rows(
  evaluer(sous, "toutes origines"),
  evaluer(sous %>% dplyr::filter(lubridate::year(origine) == 2020), "2020"),
  evaluer(sous %>% dplyr::filter(lubridate::year(origine) != 2020), "hors 2020"))
ecrire_csv(perimetre_commun, chemin_res("perimetre_commun.csv"))
ecrire_csv(commun %>% dplyr::count(branche, name = "n_origines"),
           chemin_res("perimetre_commun_effectifs.csv"))

cat(sprintf("
      === PERIMETRE COMMUN : %d couples branche-origine ===
",
            nrow(commun)))
for (per in c("toutes origines", "2020", "hors 2020")) {
  cat(sprintf("
      --- %s ---
", per))
  print(perimetre_commun %>% dplyr::filter(periode == per) %>%
          dplyr::arrange(modele, scenario) %>%
          dplyr::transmute(scenario, modele = etiq[modele], branches,
                           `MAE` = round(mae_moyenne, 4),
                           `ratio med.` = round(ratio_median, 3),
                           `br. < 1` = n_ratio_ok), n = 20)
}

# ============================================================================
# 5) FIGURES
# ============================================================================
cat("\n[5/5] Figures\n")
g1 <- perimetre_commun %>%
  dplyr::mutate(modele = etiq[modele],
                periode = factor(periode,
                                 levels = c("2020", "hors 2020", "toutes origines"))) %>%
  ggplot2::ggplot(ggplot2::aes(scenario, ratio_median, colour = modele,
                               group = modele)) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  ggplot2::geom_line(linewidth = 0.6) + ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ periode) +
  ggplot2::labs(x = NULL, y = "ratio median", colour = NULL,
                title = "Valeur de l'information au fil du trimestre",
                subtitle = "perimetre commun aux quatre scenarios ; le BVAR est plat par construction")
ggplot2::ggsave(chemin_fig("valeur_information.png"), g1, width = 11, height = 4.5, dpi = 150)

g2 <- par_periode %>% dplyr::filter(periode == "toutes origines") %>%
  dplyr::mutate(modele = etiq[modele]) %>%
  ggplot2::ggplot(ggplot2::aes(scenario, mae_moyenne, fill = modele)) +
  ggplot2::geom_col(position = "dodge", width = 0.7) +
  ggplot2::labs(x = NULL, y = "erreur absolue moyenne", fill = NULL,
                title = "Erreur selon le moment du trimestre ou l'on se place")
ggplot2::ggsave(chemin_fig("mae_scenarios.png"), g2, width = 9, height = 4.5, dpi = 150)

cat("      figures/09_valeur_information.png\n      figures/09_mae_scenarios.png\n")
cat("\nPhase 13 terminee.\n")
