# ============================================================================
# 08_rapport_synthese.R -- Tableau de synthese final (texte + figure recap)
# ============================================================================
source("R/00_setup.R")
nowcast <- readRDS(file.path(DOSSIER_RESULTATS, "nowcast_final.rds"))
backtest <- readRDS(file.path(DOSSIER_RESULTATS, "backtest.rds"))
bridge <- readRDS(file.path(DOSSIER_RESULTATS, "bridge_equations.rds"))
adf <- read.csv(file.path(DOSSIER_RESULTATS, "tests_stationnarite.csv"))

cat("\n\n============================================================\n")
cat("        SYNTHESE FINALE -- GDPNow-Maroc\n")
cat("============================================================\n\n")

cat(sprintf("1) STATIONNARITE : %d/16 branches stationnaires en Δlog (test ADF, seuil 5%%)\n",
            sum(adf$stationnaire_dlog, na.rm = TRUE)))

cat(sprintf("\n2) NOWCAST DU PIB (prochain trimestre) : %+.2f %%\n",
            nowcast$croissance_pib * 100))

cat(sprintf("\n3) VALIDATION HORS ECHANTILLON (%d trimestres, %s a %s) :\n",
            length(backtest$dates_test), min(backtest$dates_test), max(backtest$dates_test)))
cat(sprintf("   RMSFE modele complet : %.4f\n", backtest$rmsfe_modele))
cat(sprintf("   RMSFE repere AR(2)   : %.4f\n", backtest$rmsfe_ar2))
cat(sprintf("   Test Diebold-Mariano : p-value = %.3f (%s)\n", backtest$test_dm$p.value,
            ifelse(backtest$test_dm$p.value < 0.10, "significatif a 10%", "non significatif")))

cat("\n4) POIDS BVAR VS BRIDGE EQUATION (branches couvertes) :\n")
for (b in names(bridge)) {
  cat(sprintf("   %-28s delta(BVAR)=%.2f\n", b, bridge[[b]]$delta))
}

cat("\n============================================================\n")
cat("Toutes les figures sont dans le dossier figures/\n")
cat("Tous les resultats intermediaires (.rds) sont dans le dossier resultats/\n")
cat("============================================================\n")
