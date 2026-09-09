# ============================================================================
# GDPNow Maroc — 10_evaluation.R
# Evaluation du modèle : ajustement in-sample + squelette backtesting
# ============================================================================

# --------------------------------------------------------------------------
# 1. Métriques
# --------------------------------------------------------------------------

rmse <- function(actual, predicted) {
  ok <- is.finite(actual) & is.finite(predicted)
  if (!any(ok)) return(NA_real_)
  sqrt(mean((actual[ok] - predicted[ok])^2))
}

mae <- function(actual, predicted) {
  ok <- is.finite(actual) & is.finite(predicted)
  if (!any(ok)) return(NA_real_)
  mean(abs(actual[ok] - predicted[ok]))
}

smape <- function(actual, predicted) {
  ok <- is.finite(actual) & is.finite(predicted) & (abs(actual) + abs(predicted) > 0)
  if (!any(ok)) return(NA_real_)
  mean(2 * abs(predicted[ok] - actual[ok]) / (abs(actual[ok]) + abs(predicted[ok])))
}

evaluer_modele <- function(actual, predicted) {
  data.frame(RMSE = rmse(actual, predicted), MAE = mae(actual, predicted), sMAPE = smape(actual, predicted))
}

# --------------------------------------------------------------------------
# 2. Graphique nowcast vs réalisé
# --------------------------------------------------------------------------

plot_nowcast <- function(dates, actual, predicted, titre) {
  df <- data.frame(date = dates, realise = actual, ajuste = predicted)
  ggplot(df, aes(x = date)) +
    geom_line(aes(y = realise, color = "Réalisé"), linewidth = 0.8) +
    geom_line(aes(y = ajuste, color = "Ajusté DFM"), linewidth = 0.8, linetype = "dashed") +
    scale_color_manual(values = c("Réalisé" = "black", "Ajusté DFM" = "#de2d26")) +
    labs(title = titre, x = NULL, y = "Croissance VA (%, transformée)", color = NULL) +
    theme_minimal()
}

# --------------------------------------------------------------------------
# 3. Evaluation IN-SAMPLE
# --------------------------------------------------------------------------
# ATTENTION : mesure la qualité d'ajustement en échantillon plein (le
# modèle "voit" toute la période). Ce n'est PAS une évaluation en pseudo
# temps réel comme dans les papiers de référence (vintages successives).
# Section 5 ci-dessous propose le squelette pour construire ça ensuite.

evaluation <- data.frame()

for (s in names(resultats)) {

  res <- resultats[[s]]
  if (is.null(res$model)) { message("Pas de modèle pour ", s, " -- ignoré."); next }

  message("Evaluation (in-sample) : ", s)

  actual <- as.numeric(res$panel_stat[, 1])
  predicted <- res$serie_ajustee

  if (is.null(predicted) || length(predicted) != length(actual)) {
    message("  -> série ajustée indisponible pour ", s, " (voir 08_dfm_estimation.R)")
    metriques <- data.frame(RMSE = NA_real_, MAE = NA_real_, sMAPE = NA_real_)
  } else {
    metriques <- evaluer_modele(actual, predicted)

    g <- plot_nowcast(zoo::index(res$panel_stat), actual, predicted,
                       paste("Ajustement DFM in-sample —", s))
    ggsave(file.path(FIG_DIR, paste0("ajustement_", s, ".png")), g, width = 9, height = 4.5, dpi = 150)
  }

  evaluation <- rbind(evaluation, data.frame(
    secteur = s, r = res$r, p = res$p, modele = "DFM-EM (dfms)",
    RMSE = metriques$RMSE, MAE = metriques$MAE, sMAPE = metriques$sMAPE
  ))
}

write.csv(evaluation, file.path(OUT_DIR, "evaluation.csv"), row.names = FALSE)

message("\n=== Evaluation in-sample par secteur ===")
print(evaluation)

# --------------------------------------------------------------------------
# 4. Comparatif visuel des sMAPE entre secteurs
# --------------------------------------------------------------------------

g_smape <- ggplot(na.omit(evaluation), aes(x = reorder(secteur, sMAPE), y = sMAPE)) +
  geom_col(fill = "#31a354") +
  coord_flip() +
  labs(title = "sMAPE in-sample par secteur (DFM-EM)", x = NULL, y = "sMAPE") +
  theme_minimal()

ggsave(file.path(FIG_DIR, "smape_par_secteur.png"), g_smape, width = 8, height = 6, dpi = 150)

# --------------------------------------------------------------------------
# 5. TODO — pseudo temps réel (prochaine itération)
# --------------------------------------------------------------------------
# Squelette pour une vraie évaluation out-of-sample façon Chernis & Sekkel
# (2017) / Supriyatna et al. (2024) : reestimer recursivement sur des
# fenetres tronquees, comparer le nowcast produit a chaque etape a la
# realisation finale.
#
#   backtest_secteur <- function(secteur, date_debut_test, h = 1) {
#     dates_test <- ... # sequence de trimestres a tester
#     resultats_bt <- list()
#     for (t in dates_test) {
#       panel_tronque <- panels_stationnaires_filtres[[secteur]][zoo::index(...) <= t - h, ]
#       # reestimer (ou reutiliser r,p deja choisis) et repredire le nowcast
#       # pour le trimestre t, stocker dans resultats_bt
#     }
#     # comparer resultats_bt empiles a la realisation -> RMSFE, sMAPE
#     # cf. Table 4/6 du papier indonesien, Table 3 du papier canadien
#   }
# ==========================================================================
