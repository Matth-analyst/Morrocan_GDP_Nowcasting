# ============================================================================
# 04c_kalman_trous.R -- PHASE 4 ter : combler les trous par filtre de Kalman
# ============================================================================
# CE QUE MESURE CE SCRIPT
#   Le refus strict des trimestres incomplets coute 2 562 trimestres sur les
#   12 branches couvertes -- 1 555 refuses directement, plus 1 007 detruits en
#   cascade parce qu'une difference a cheval sur un trou est invalide. Il
#   ramene aussi a 4 series le panneau rectangulaire exige par l'ACP et la
#   ridge, si bien que ces deux methodes n'ont jamais ete testees dans des
#   conditions correctes.
#
#   Ce script comble les trous INTERNES courts par lissage de Kalman, reestime
#   a chaque origine, puis rejoue les variantes de la phase 4 bis sur la base
#   ainsi completee. La question est simple : les trimestres recuperes
#   ameliorent-ils la prevision, ou n'ajoutent-ils que du bruit reconstitue ?
#
# VALIDATION PREALABLE (voir resultats/04c_validation_kalman.csv)
#   Sur 410 trous artificiels perces dans 60 series completes, le comblement
#   donne une erreur mediane de 0,069 ecart-type au MOIS et de 0,023 au
#   TRIMESTRE -- l'agregation moyenne l'erreur. Le Kalman ne bat l'interpolation
#   lineaire que 52 % du temps, mais il la domine en RMSE (0,49 contre 0,60) et
#   surtout sur les series saisonnieres, ou la moyenne du mois calendaire
#   s'effondre a 0,94.
#
# ANTI-LOOK-AHEAD
#   Le modele est reestime a chaque origine sur la seule information
#   disponible alors. Le lissage a l'interieur de cet echantillon est legitime :
#   estimer un mois de 2011 en utilisant 2012-2019 depuis l'origine 2020
#   n'utilise rien de posterieur a la CIBLE. Ce qui serait fautif -- lisser une
#   fois pour toutes sur l'echantillon complet -- est evite par construction.
#
# SORTIES
#   resultats/04c_comblement_bilan.csv
#   resultats/04c_previsions_kalman.csv
#   resultats/04c_comparaison_kalman.csv
#   figures/04c_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")
source("R/fonctions/kalman.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("04c_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("04c_", x))

PREMIERE_CIBLE <- as.Date("2014-06-30")
SEUIL_R <- 0.15; SEUIL_P <- 0.10
MIN_OBS_SEL <- 20L; MAX_RETENUS <- 5L; K_COMPOSANTES <- 3L
MAX_TROU <- 3L

cat("\n[1/5] Bases\n")
couverture <- charger_couverture(); BC <- couverture$couvertes
ind <- charger_indicateurs(branches = BC)
meta <- charger_metadonnees() %>%
  dplyr::select(id_serie, agregation, transformation)

va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)
origines <- sort(unique(va$date[va$date >= PREMIERE_CIBLE]))
cat(sprintf("      %d origines x %d branches\n", length(origines), length(BC)))

mensuel <- ind %>% dplyr::filter(frequence == "mensuel")
trimest <- ind %>% dplyr::filter(frequence == "trimestriel")
series_m <- unique(mensuel$id_serie)
cat(sprintf("      %d series mensuelles a traiter, %d trimestrielles inchangees\n",
            length(series_m), dplyr::n_distinct(trimest$id_serie)))

# ============================================================================
# 2) UNE ORIGINE : comblement, agregation, previsions
# ============================================================================
traiter_origine <- function(cible) {
  limite <- fin_mois(debut_trimestre(cible) %m-% months(1L))  # dernier mois < T
  n_comble <- 0L

  # --- comblement des series mensuelles, sur I_T uniquement ----------------
  m_cut <- mensuel %>% dplyr::filter(date <= fin_trimestre(cible))
  comble <- m_cut %>%
    dplyr::group_by(id_serie, branche, indicateur) %>%
    dplyr::group_modify(function(g, cle) {
      r <- combler_trous_kalman(g$date, g$valeur, max(g$date), max_trou = MAX_TROU)
      if (is.null(r)) return(dplyr::select(g, date, valeur) %>%
                               dplyr::mutate(comble = FALSE))
      dplyr::select(r, date, valeur, comble)
    }) %>% dplyr::ungroup()
  n_comble <- sum(comble$comble)

  base_long <- dplyr::bind_rows(
    comble %>% dplyr::mutate(frequence = "mensuel") %>%
      dplyr::select(id_serie, branche, indicateur, frequence, date, valeur),
    trimest %>% dplyr::filter(date <= fin_trimestre(cible)) %>%
      dplyr::select(id_serie, branche, indicateur, frequence, date, valeur)) %>%
    dplyr::left_join(meta, by = "id_serie")

  trim <- indicateurs_trimestriels(base_long)
  d <- trim %>% dplyr::group_by(id_serie) %>%
    dplyr::summarise(n = sum(!is.na(x)), .groups = "drop")
  tok <- trim %>%
    dplyr::filter(id_serie %in% d$id_serie[d$n >= MIN_OBS_SEL], !is.na(x)) %>%
    dplyr::select(id_serie, branche, date, x)

  # --- previsions, pour trois variantes ------------------------------------
  purrr::map_dfr(BC, function(b) {
    g_b <- va %>% dplyr::filter(branche == b) %>% dplyr::select(date, g)
    x_b <- tok %>% dplyr::filter(branche == b) %>% dplyr::select(id_serie, date, x)
    if (nrow(x_b) == 0L) return(NULL)
    x_l <- x_b %>% tidyr::pivot_wider(names_from = id_serie, values_from = x)
    reel <- g_b$g[g_b$date == cible]
    reel <- if (length(reel) == 1L) reel else NA_real_
    sortie <- function(lab, r) tibble::tibble(
      specification = lab, branche = b, origine = cible,
      prevision = if (is.null(r)) NA_real_ else r$prevision,
      derniere_obs = if (is.null(r)) as.Date(NA) else r$derniere_obs,
      reel = reel, n_comble = n_comble)

    # 1+4 : disponibilite + terme AR, la meilleure variante de la phase 4 bis
    s <- selectionner_disponible(g_b, x_b, x_l, cible, SEUIL_R, SEUIL_P,
                                 MIN_OBS_SEL, MAX_RETENUS)
    r14 <- if (!is.null(s)) estimer_bridge_ar(g_b, x_l, s$ids, cible, avec_ar = TRUE)
           else NULL
    # ACP
    cp <- composantes_principales(x_l, cible, k = K_COMPOSANTES)
    racp <- if (!is.null(cp)) estimer_bridge_ar(g_b, cp$donnees, cp$ids, cible,
                                                avec_ar = FALSE) else NULL
    # ridge + AR
    rrg <- estimer_bridge_ridge(g_b, x_l, cible, avec_ar = TRUE)

    dplyr::bind_rows(sortie("1+4 + Kalman", r14),
                     sortie("ACP + Kalman", racp),
                     sortie("ridge + AR + Kalman", rrg))
  })
}

cat("\n[2/5] Comblement et previsions, origine par origine\n")
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
parallel::clusterExport(cl, c("mensuel", "trimest", "meta", "va", "BC", "origines",
                              "SEUIL_R", "SEUIL_P", "MIN_OBS_SEL", "MAX_RETENUS",
                              "K_COMPOSANTES", "MAX_TROU", "traiter_origine"),
                        envir = environment())
res <- parallel::parLapply(cl, seq_along(origines),
                           function(i) traiter_origine(origines[i]))
parallel::stopCluster(cl)
previsions <- dplyr::bind_rows(res)
cat(sprintf("      %s | %d lignes\n",
            format(round(difftime(Sys.time(), t0, units = "mins"), 1)),
            nrow(previsions)))

stopifnot("[ANTI-LOOK-AHEAD] une passerelle a vu sa cible" =
            all(previsions$derniere_obs < previsions$origine, na.rm = TRUE))
cat("      controle : aucune estimation ne contient sa propre cible\n")
ecrire_csv(previsions, chemin_res("previsions_kalman.csv"))

bilan_comble <- previsions %>%
  dplyr::distinct(origine, n_comble) %>% dplyr::arrange(origine)
ecrire_csv(bilan_comble, chemin_res("comblement_bilan.csv"))
cat(sprintf("      mois combles : %d a la premiere origine, %d a la derniere\n",
            bilan_comble$n_comble[1], bilan_comble$n_comble[nrow(bilan_comble)]))

# ============================================================================
# 3) COMPARAISON AVEC ET SANS KALMAN
# ============================================================================
cat("\n[3/5] Comparaison avec et sans comblement\n")
sans <- dplyr::bind_rows(
  lire_csv(file.path(DOSSIER_RESULTATS, "04b_previsions_variantes.csv")) %>%
    dplyr::mutate(origine = as.Date(origine)) %>%
    dplyr::filter(specification %in% c("1+4. disponibilite + terme AR",
                                       "2. composantes principales")) %>%
    dplyr::select(specification, branche, origine, prevision, reel),
  lire_csv(file.path(DOSSIER_RESULTATS, "04b_ridge_corrige.csv")) %>%
    dplyr::mutate(origine = as.Date(origine)) %>%
    dplyr::filter(specification == "6+4. ridge + AR") %>%
    dplyr::select(specification, branche, origine, prevision, reel)) %>%
  dplyr::mutate(specification = dplyr::recode(specification,
    "1+4. disponibilite + terme AR" = "1+4 sans Kalman",
    "2. composantes principales"    = "ACP sans Kalman",
    "6+4. ridge + AR"               = "ridge + AR sans Kalman"))

tous <- dplyr::bind_rows(
  sans,
  previsions %>% dplyr::select(specification, branche, origine, prevision, reel))

taux <- tous %>% dplyr::group_by(specification) %>%
  dplyr::summarise(taux = mean(!is.na(prevision)),
                   n_produites = sum(!is.na(prevision)), .groups = "drop")

par_branche <- tous %>% dplyr::filter(!is.na(prevision), !is.na(reel)) %>%
  dplyr::group_by(specification, branche) %>%
  dplyr::summarise(n = dplyr::n(),
                   ratio = sqrt(mean((reel - prevision)^2)) / stats::sd(reel),
                   correlation = suppressWarnings(stats::cor(prevision, reel)),
                   .groups = "drop")
ecrire_csv(par_branche, chemin_res("evaluation_par_branche.csv"))

bilan <- par_branche %>% dplyr::group_by(specification) %>%
  dplyr::summarise(ratio_median = stats::median(ratio),
                   n_branches_ok = sum(ratio < 1),
                   correl = stats::median(correlation, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::left_join(taux, by = "specification") %>%
  dplyr::arrange(ratio_median)
ecrire_csv(bilan, chemin_res("comparaison_kalman.csv"))

cat("\n      --- toutes origines produites par chaque variante ---\n")
print(bilan %>% dplyr::transmute(specification = substr(specification, 1, 26),
                                 `taux prod.` = sprintf("%.0f%%", 100 * taux),
                                 `ratio med.` = round(ratio_median, 3),
                                 `br. < 1` = n_branches_ok,
                                 `correl.` = round(correl, 2)), n = 10)

# --- perimetre commun --------------------------------------------------------
ns <- dplyr::n_distinct(tous$specification)
commun <- tous %>% dplyr::filter(!is.na(prevision)) %>%
  dplyr::count(branche, origine) %>% dplyr::filter(n == ns) %>%
  dplyr::select(branche, origine)
bc <- tous %>% dplyr::inner_join(commun, by = c("branche", "origine")) %>%
  dplyr::group_by(specification, branche) %>%
  dplyr::summarise(ratio = sqrt(mean((reel - prevision)^2)) / stats::sd(reel),
                   correlation = suppressWarnings(stats::cor(prevision, reel)),
                   .groups = "drop") %>%
  dplyr::group_by(specification) %>%
  dplyr::summarise(ratio_median = stats::median(ratio),
                   n_branches_ok = sum(ratio < 1),
                   correl = stats::median(correlation, na.rm = TRUE),
                   .groups = "drop") %>% dplyr::arrange(ratio_median)
ecrire_csv(bc, chemin_res("comparaison_perimetre_commun.csv"))
cat(sprintf("\n      --- PERIMETRE COMMUN : %d cas ---\n", nrow(commun)))
print(bc %>% dplyr::transmute(specification = substr(specification, 1, 26),
                              `ratio med.` = round(ratio_median, 3),
                              `br. < 1` = n_branches_ok,
                              `correl.` = round(correl, 2)), n = 10)

bv <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_recursives.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::inner_join(commun, by = c("branche", "origine")) %>%
  dplyr::group_by(branche) %>%
  dplyr::summarise(ratio = sqrt(mean((reel - prevision)^2)) / stats::sd(reel),
                   .groups = "drop")
cat(sprintf("      BVAR seul, meme perimetre : ratio median %.3f | %d/%d < 1\n",
            stats::median(bv$ratio), sum(bv$ratio < 1), nrow(bv)))

# ============================================================================
# 4) FIGURES
# ============================================================================
cat("\n[4/5] Figures\n")
g1 <- bilan %>%
  dplyr::mutate(kalman = ifelse(grepl("Kalman$", specification) &
                                  !grepl("sans", specification),
                                "avec comblement", "sans comblement"),
                methode = sub(" (sans Kalman|\\+ Kalman)$", "", specification)) %>%
  ggplot2::ggplot(ggplot2::aes(methode, ratio_median, fill = kalman)) +
  ggplot2::geom_col(position = "dodge", width = 0.65) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  ggplot2::scale_fill_manual(values = c("sans comblement" = "grey70",
                                        "avec comblement" = "#1f4e79")) +
  ggplot2::labs(x = NULL, y = "ratio median", fill = NULL,
                title = "Effet du comblement des trous sur la qualite de la prevision",
                subtitle = "chaque methode, avec et sans filtre de Kalman")
ggplot2::ggsave(chemin_fig("effet_kalman.png"), g1, width = 9, height = 4.5, dpi = 150)

g2 <- bilan_comble %>%
  ggplot2::ggplot(ggplot2::aes(origine, n_comble)) +
  ggplot2::geom_step(linewidth = 0.5) +
  ggplot2::labs(x = NULL, y = "mois combles",
                title = "Nombre de mois comblés dans l'ensemble d'information",
                subtitle = "il croit avec l'origine, puisque l'historique disponible s'allonge")
ggplot2::ggsave(chemin_fig("mois_combles.png"), g2, width = 9, height = 4, dpi = 150)

cat("      figures/04c_effet_kalman.png\n      figures/04c_mois_combles.png\n")
cat("\nPhase 4 ter terminee.\n")
