# ============================================================================
# 14b_courbe_delta.R -- La courbe de perte en fonction du poids de combinaison
# ============================================================================
# Complement a la phase 23 du plan (robustesse du delta). Le script 14 compare
# CINQ REGLES au niveau des branches ; celui-ci balaie delta sur toute la
# grille [0, 1] au niveau de L'AGREGAT, qui est la grandeur que le projet
# publie, et scinde par periode.
#
# POURQUOI CE SCRIPT EXISTE
#   La justification de delta = 0,5 a d'abord repose sur une affirmation FAUSSE :
#   que le delta estime "basculerait" d'un trimestre a l'autre. La mesure, faite
#   ici, dit le contraire -- il est stable, saut median nul, ecart-type
#   intra-branche 0,12.
#
#   La vraie raison est ailleurs, et ce script la mesure : compare a une
#   CONSTANTE PLACEE A SON PROPRE NIVEAU MOYEN, le delta estime est BATTU. Sa
#   variation d'une branche et d'un trimestre a l'autre n'apporte donc aucune
#   information -- elle n'ajoute que du bruit. C'est le "forecast combination
#   puzzle" (Stock & Watson 2004 ; Smith & Wallis 2009 ; Claeskens et al. 2016)
#   observe sur nos propres donnees.
#
#   Le reste de la justification est dans la FORME de la courbe, d'ou le
#   balayage : plate autour de son minimum, et de minimum DEPENDANT DU REGIME.
#
# Sorties : resultats/14b_courbe_delta.csv      ratio et correlation par delta
#           resultats/14b_stabilite_delta.csv   ce que fait le delta estime
#           resultats/14b_estime_contre_constante.csv  la comparaison decisive
#           figures/14b_courbe_delta.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("14b_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("14b_", x))
r <- function(x) file.path(DOSSIER_RESULTATS, x)

MIN_OBS_DELTA <- 8L
GRILLE <- seq(0, 1, by = 0.05)

# ============================================================================
# 1) BASES
# ============================================================================
cat("\n[1/4] Bases\n")

intra <- lire_csv(r("09_previsions_intra.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::filter(scenario == "M3")

poids <- lire_csv(r("06_poids.csv")) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::transmute(branche,
                   origine = fin_trimestre(debut_trimestre(date) %m+% months(3)), w)

bvar_br <- lire_csv(r("03e_previsions_corrigees.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(branche, origine, bvar = prevision)

va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(reel = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(reel)) %>%
  dplyr::transmute(branche, origine = date, reel)

cat(sprintf("      %d couples branche-origine en M3\n", nrow(intra)))

# ============================================================================
# 2) LA COURBE
# ============================================================================
cat("\n[2/4] Balayage de delta sur l'agregat\n")

#' Agrege exactement comme l'etape 6 : indice de Laspeyres, seize branches
#' exigees, poids du trimestre precedent.
agreger_pour <- function(combinee_col) {
  base <- va %>%
    dplyr::inner_join(poids, by = c("branche", "origine")) %>%
    dplyr::left_join(bvar_br, by = c("branche", "origine")) %>%
    dplyr::left_join(combinee_col, by = c("branche", "origine")) %>%
    dplyr::mutate(nowcast = dplyr::coalesce(combinee, bvar))
  completes <- base %>% dplyr::group_by(origine) %>%
    dplyr::summarise(n = sum(!is.na(nowcast)), .groups = "drop") %>%
    dplyr::filter(n == length(TOUTES_BRANCHES))
  base %>% dplyr::filter(origine %in% completes$origine) %>%
    dplyr::group_by(origine) %>%
    dplyr::summarise(reel_n = log(sum(w * exp(reel))),
                     now_n  = log(sum(w * exp(nowcast))), .groups = "drop")
}

# `d` est passe par .env : `delta` est aussi un nom de colonne du fichier, et
# dans un mutate c'est la colonne qui gagnerait.
combiner <- function(d) {
  intra %>%
    dplyr::mutate(combinee = dplyr::case_when(
      is.na(bridge) & is.na(bvar) ~ NA_real_,
      is.na(bridge)               ~ bvar,
      is.na(bvar)                 ~ bridge,
      TRUE ~ .env$d * bvar + (1 - .env$d) * bridge)) %>%
    dplyr::select(branche, origine, combinee)
}

mesurer <- function(ag, lab, d) {
  if (nrow(ag) < 3L) return(NULL)
  e <- ag$reel_n - ag$now_n
  tibble::tibble(delta = d, periode = lab, n = nrow(ag),
                 RMSFE = sqrt(mean(e^2)),
                 ratio = sqrt(mean(e^2)) / stats::sd(ag$reel_n),
                 correlation = stats::cor(ag$now_n, ag$reel_n))
}

courbe <- purrr::map_dfr(GRILLE, function(d) {
  ag <- agreger_pour(combiner(d))
  an <- as.integer(format(ag$origine, "%Y"))
  dplyr::bind_rows(
    mesurer(ag,              "toutes origines", d),
    mesurer(ag[an == 2020, ], "2020",           d),
    mesurer(ag[an != 2020, ], "hors 2020",      d))
})
ecrire_csv(courbe, chemin_res("courbe_delta.csv"))

apercu <- courbe %>%
  dplyr::filter(delta %in% seq(0, 1, by = 0.1)) %>%
  dplyr::select(delta, periode, ratio) %>%
  tidyr::pivot_wider(names_from = periode, values_from = ratio)
cat("\n      --- ratio agrege selon delta ---\n")
print(as.data.frame(apercu %>% dplyr::mutate(dplyr::across(-delta, ~round(.x, 4)))),
      row.names = FALSE)

optima <- courbe %>% dplyr::group_by(periode) %>%
  dplyr::slice_min(ratio, n = 1, with_ties = FALSE) %>% dplyr::ungroup() %>%
  dplyr::select(periode, delta_optimal = delta, ratio_optimal = ratio)
cat("\n      --- minimum par periode ---\n")
print(as.data.frame(optima %>% dplyr::mutate(ratio_optimal = round(ratio_optimal, 4))),
      row.names = FALSE)

cout_05 <- courbe %>% dplyr::filter(delta == 0.5) %>%
  dplyr::select(periode, ratio_05 = ratio) %>%
  dplyr::inner_join(optima, by = "periode") %>%
  dplyr::mutate(cout = ratio_05 - ratio_optimal)
cat("\n      cout de delta = 0,5 par rapport au minimum de sa periode :\n")
for (i in seq_len(nrow(cout_05))) {
  cat(sprintf("        %-16s %+.4f point de ratio\n",
              cout_05$periode[i], cout_05$cout[i]))
}

# Largeur de la zone plate : tous les delta a moins de 0,01 point du minimum.
plat <- courbe %>% dplyr::filter(periode == "hors 2020") %>%
  dplyr::mutate(ecart = ratio - min(ratio)) %>%
  dplyr::filter(ecart <= 0.01)
cat(sprintf("\n      hors 2020, tout delta de %.2f a %.2f est a moins de 0,01 point du minimum\n",
            min(plat$delta), max(plat$delta)))

# ============================================================================
# 3) CE QUE FAIT LE DELTA ESTIME
# ============================================================================
cat("\n[3/4] Le delta estime, recalcule ici\n")

# Recalcule independamment du reglage du projet : constante = NA force
# l'estimation, meme quand DELTA_CONSTANT est fixe.
estimes <- intra %>% dplyr::arrange(branche, origine) %>%
  dplyr::group_by(branche) %>%
  dplyr::group_modify(function(g, cle) {
    g$d_est <- NA_real_; g$mode <- NA_character_
    for (i in seq_len(nrow(g))) {
      pd <- poids_combinaison(
        g[seq_len(i - 1L), ] %>% dplyr::transmute(reel, bvar, bridge),
        min_obs = MIN_OBS_DELTA, constante = NA_real_)
      g$d_est[i] <- pd$delta; g$mode[i] <- pd$mode
    }
    g
  }) %>% dplyr::ungroup()

vrais <- estimes %>% dplyr::filter(mode == "estime")
sauts <- vrais %>% dplyr::group_by(branche) %>%
  dplyr::mutate(saut = abs(d_est - dplyr::lag(d_est))) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(saut))

stab <- tibble::tibble(
  mesure = c("couples ou delta est reellement estime",
             "part des couples au mode par defaut",
             "moyenne du delta estime",
             "mediane du delta estime",
             "part en solution de coin (0 ou 1)",
             "saut median d'un trimestre au suivant",
             "saut moyen",
             "part des sauts superieurs a 0,25",
             "ecart-type intra-branche moyen"),
  valeur = c(nrow(vrais),
             mean(estimes$mode != "estime"),
             mean(vrais$d_est),
             stats::median(vrais$d_est),
             mean(vrais$d_est %in% c(0, 1)),
             stats::median(sauts$saut),
             mean(sauts$saut),
             mean(sauts$saut > 0.25),
             mean((vrais %>% dplyr::group_by(branche) %>%
                     dplyr::summarise(s = stats::sd(d_est), .groups = "drop"))$s,
                  na.rm = TRUE)))
ecrire_csv(stab, chemin_res("stabilite_delta.csv"))
print(as.data.frame(stab %>% dplyr::mutate(valeur = round(valeur, 3))), row.names = FALSE)

cat(sprintf(paste0("\n      LECTURE : le delta estime est STABLE (saut median %.3f), ",
                   "mais centre sur %.2f --\n      trop haut sur une courbe qui decroit ",
                   "avec delta. Ce n'est pas son instabilite qui le penalise,\n",
                   "      c'est sa position.\n"),
            stats::median(sauts$saut), mean(vrais$d_est)))

# --- la comparaison decisive : estime contre constante DE MEME NIVEAU --------
# Si le delta estime faisait mieux qu'une constante placee a sa moyenne, sa
# VARIATION apporterait quelque chose. Sinon, elle ne fait qu'ajouter du bruit.
ag_est <- agreger_pour(
  estimes %>% dplyr::mutate(combinee = dplyr::case_when(
    is.na(bridge) & is.na(bvar) ~ NA_real_,
    is.na(bridge)               ~ bvar,
    is.na(bvar)                 ~ bridge,
    TRUE ~ d_est * bvar + (1 - d_est) * bridge)) %>%
    dplyr::select(branche, origine, combinee))
r_est <- mesurer(ag_est, "toutes origines", NA_real_)

d_moy <- mean(estimes$d_est)
ag_cst <- agreger_pour(combiner(d_moy))
r_cst <- mesurer(ag_cst, "toutes origines", d_moy)

comp <- dplyr::bind_rows(
  r_est %>% dplyr::mutate(regle = sprintf("delta estime (moyenne %.2f)", d_moy)),
  r_cst %>% dplyr::mutate(regle = sprintf("constante au MEME niveau (%.2f)", d_moy)),
  courbe %>% dplyr::filter(delta == 0.5, periode == "toutes origines") %>%
    dplyr::mutate(regle = "constante retenue (0,50)")) %>%
  dplyr::select(regle, ratio, correlation)
ecrire_csv(comp, chemin_res("estime_contre_constante.csv"))
cat("
      --- le delta estime contre une constante de meme niveau ---
")
print(as.data.frame(comp %>% dplyr::mutate(ratio = round(ratio, 4),
                                           correlation = round(correlation, 3))),
      row.names = FALSE)
cat(sprintf(paste0("
      A niveau moyen EGAL, la constante fait %+.4f point de ratio.
",
                   "      La variation du delta estime n'apporte donc rien : elle ajoute du bruit.
"),
            r_cst$ratio - r_est$ratio))

# ============================================================================
# 4) FIGURE
# ============================================================================
cat("\n[4/4] Figure\n")

d_proj <- if (exists("DELTA_CONSTANT") && !is.na(DELTA_CONSTANT)) DELTA_CONSTANT else NA_real_

g <- ggplot2::ggplot(courbe, ggplot2::aes(delta, ratio, colour = periode)) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dotted", colour = "grey55") +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(data = optima,
                      ggplot2::aes(delta_optimal, ratio_optimal, colour = periode),
                      size = 2.4, inherit.aes = FALSE) +
  { if (!is.na(d_proj)) ggplot2::geom_vline(xintercept = d_proj, linetype = "dashed",
                                            colour = "grey35") } +
  ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  ggplot2::labs(
    title = "Ratio de l'agregat selon le poids de combinaison",
    subtitle = sprintf(paste0("Les points marquent le minimum de chaque periode ; ",
                              "la verticale, le reglage retenu (delta = %.2f)"), d_proj),
    x = "delta  (0 = passerelle seule, 1 = BVAR seul)",
    y = "ratio  (RMSFE / ecart-type du realise)", colour = NULL) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom",
                 panel.grid.minor = ggplot2::element_blank())
ggplot2::ggsave(chemin_fig("courbe_delta.png"), g, width = 7.5, height = 5, dpi = 150)
cat(sprintf("      %s\n", chemin_fig("courbe_delta.png")))

cat("\nCourbe du delta terminee.\n")
