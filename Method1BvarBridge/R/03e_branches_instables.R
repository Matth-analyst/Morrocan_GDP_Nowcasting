# ============================================================================
# 03e_branches_instables.R -- Appliquer la regle d'instabilite au backtest
# ============================================================================
# Produit les previsions BVAR CORRIGEES, ou les branches declarees instables par
# le diagnostic de variance recoivent leur moyenne recente au lieu de la
# prevision du BVAR. Voir R/fonctions/branches_instables.R pour le raisonnement
# complet et les trois corrections rejetees avant celle-ci.
#
# SORTIES
#   resultats/03e_previsions_corrigees.csv   previsions substituees
#   resultats/03e_diagnostic_instabilite.csv rapport de variance par branche
#   resultats/03e_effet_correction.csv       avant / apres, branche et agregat
#   figures/03e_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/branches_instables.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("03e_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("03e_", x))

RAPPORT_MIN <- 10
FENETRE     <- 60L

cat("\n[1/4] Bases\n")
va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)
large <- va %>% tidyr::pivot_wider(names_from = branche, values_from = g) %>%
  dplyr::arrange(date) %>%
  dplyr::filter(dplyr::if_all(dplyr::all_of(TOUTES_BRANCHES), ~ !is.na(.)))
dates_vec <- large$date
Y <- as.matrix(large[, TOUTES_BRANCHES])

previsions <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_recursives.csv")) %>%
  dplyr::mutate(origine = as.Date(origine))
cat(sprintf("      %d previsions BVAR, %d origines\n", nrow(previsions),
            dplyr::n_distinct(previsions$origine)))

# ============================================================================
# 2) LE DIAGNOSTIC
# ============================================================================
cat("\n[2/4] Diagnostic d'instabilite\n")
d_complet <- diagnostic_instabilite(Y, RAPPORT_MIN)
diag_tab <- tibble::tibble(branche = TOUTES_BRANCHES,
                           rapport_variance = d_complet$rapport,
                           instable = d_complet$instables) %>%
  dplyr::arrange(dplyr::desc(rapport_variance))
ecrire_csv(diag_tab, chemin_res("diagnostic_instabilite.csv"))
print(as.data.frame(diag_tab %>%
  dplyr::mutate(rapport_variance = round(rapport_variance, 1))), row.names = FALSE)
cat(sprintf("\n      seuil = %g | separation : %.1f contre %.1f pour la suivante\n",
            RAPPORT_MIN, diag_tab$rapport_variance[1], diag_tab$rapport_variance[2]))

# --- la regle est-elle stable au fil des origines ? --------------------------
# Une regle qui se declencherait tantot sur une branche tantot sur une autre
# serait du bruit, pas un diagnostic.
origines <- sort(unique(previsions$origine))
stabilite <- purrr::map_dfr(origines, function(cible) {
  ok <- which(dates_vec < cible)
  d <- diagnostic_instabilite(Y[ok, , drop = FALSE], RAPPORT_MIN)
  tibble::tibble(origine = cible, n = sum(d$instables),
                 branches = paste(TOUTES_BRANCHES[d$instables], collapse = ", "))
})
cat("\n      declenchements au fil des 48 origines :\n")
print(as.data.frame(dplyr::count(stabilite, n, branches, name = "origines")),
      row.names = FALSE)

# ============================================================================
# 3) CORRECTION ET EFFET
# ============================================================================
cat("\n[3/4] Correction\n")
corrigees <- corriger_branches_instables(previsions, Y, dates_vec, va,
                                         RAPPORT_MIN, FENETRE)
ecrire_csv(corrigees, chemin_res("previsions_corrigees.csv"))
cat(sprintf("      %d previsions substituees (%d branche(s) x %d origines)\n",
            sum(corrigees$instable),
            dplyr::n_distinct(corrigees$branche[corrigees$instable]),
            dplyr::n_distinct(corrigees$origine[corrigees$instable])))

evaluer <- function(d) d %>% dplyr::group_by(branche) %>%
  dplyr::summarise(ratio = sqrt(mean((reel - prevision)^2)) / stats::sd(reel),
                   correl = suppressWarnings(stats::cor(prevision, reel)),
                   .groups = "drop")
av <- evaluer(previsions) %>% dplyr::rename(ratio_avant = ratio, correl_avant = correl)
ap <- evaluer(corrigees) %>% dplyr::rename(ratio_apres = ratio, correl_apres = correl)
effet_branche <- av %>% dplyr::inner_join(ap, by = "branche") %>%
  dplyr::mutate(gain = ratio_avant - ratio_apres) %>%
  dplyr::arrange(dplyr::desc(abs(gain)))
cat("\n      --- branches les plus affectees ---\n")
print(as.data.frame(effet_branche %>% utils::head(4) %>%
  dplyr::transmute(branche, avant = round(ratio_avant, 3),
                   apres = round(ratio_apres, 3), gain = round(gain, 3))),
  row.names = FALSE)
cat(sprintf("\n      mediane : %.3f -> %.3f | branches < 1 : %d -> %d\n",
            stats::median(effet_branche$ratio_avant),
            stats::median(effet_branche$ratio_apres),
            sum(effet_branche$ratio_avant < 1), sum(effet_branche$ratio_apres < 1)))

# --- l'agregat, seule metrique qui engage le projet --------------------------
poids <- lire_csv(file.path(DOSSIER_RESULTATS, "06_poids.csv")) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::transmute(branche,
                   origine = fin_trimestre(debut_trimestre(date) %m+% months(3)), w)
agreger <- function(d, lab) {
  a <- d %>% dplyr::inner_join(poids, by = c("branche", "origine")) %>%
    dplyr::group_by(origine) %>% dplyr::filter(dplyr::n() == 16L) %>%
    dplyr::summarise(reel = log(sum(w * exp(reel))),
                     prev = log(sum(w * exp(prevision))), .groups = "drop")
  e <- a$reel - a$prev
  tibble::tibble(specification = lab, n = nrow(a), RMSFE = sqrt(mean(e^2)),
                 ratio = sqrt(mean(e^2)) / stats::sd(a$reel),
                 correlation = stats::cor(a$prev, a$reel))
}
effet_agregat <- dplyr::bind_rows(agreger(previsions, "BVAR sans correction"),
                                  agreger(corrigees,  "BVAR avec correction"))
ecrire_csv(dplyr::bind_rows(
  effet_branche %>% dplyr::mutate(niveau = "branche"),
  effet_agregat %>% dplyr::mutate(niveau = "agregat")), chemin_res("effet_correction.csv"))
cat("\n      --- agregat ---\n")
print(as.data.frame(effet_agregat %>%
  dplyr::transmute(specification, n, `RMSFE (%)` = round(100 * RMSFE, 3),
                   ratio = round(ratio, 3), `correl.` = round(correlation, 2))),
  row.names = FALSE)

# ============================================================================
# 4) FIGURES
# ============================================================================
cat("\n[4/4] Figures\n")
g1 <- diag_tab %>%
  ggplot2::ggplot(ggplot2::aes(stats::reorder(branche, rapport_variance),
                               rapport_variance, fill = instable)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::geom_hline(yintercept = RAPPORT_MIN, linetype = "dashed",
                      colour = "#b03a2e") +
  ggplot2::scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "#b03a2e"),
                             guide = "none") +
  ggplot2::scale_y_log10() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "variance du premier tiers / variance du reste (echelle log)",
                title = "Diagnostic d'instabilite par branche",
                subtitle = "le trait marque le seuil ; la separation est franche, 49 contre 3")
ggplot2::ggsave(chemin_fig("diagnostic.png"), g1, width = 9, height = 5, dpi = 150)

b_inst <- diag_tab$branche[diag_tab$instable]
if (length(b_inst) > 0L) {
  g2 <- va %>% dplyr::filter(branche %in% b_inst) %>%
    ggplot2::ggplot(ggplot2::aes(date, 100 * g)) +
    ggplot2::geom_line(linewidth = 0.45) +
    ggplot2::facet_wrap(~ branche, scales = "free_y") +
    ggplot2::labs(x = NULL, y = "croissance trimestrielle (%)",
                  title = "Les branches declarees instables",
                  subtitle = "la rupture de variance est visible a l'oeil nu")
  ggplot2::ggsave(chemin_fig("series_instables.png"), g2,
                  width = 9, height = 4, dpi = 150)
  cat("      figures/03e_diagnostic.png\n      figures/03e_series_instables.png\n")
}
cat("\nCorrection des branches instables terminee.\n")
