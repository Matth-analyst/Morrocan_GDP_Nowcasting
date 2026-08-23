source("R/00_setup.R")
nowcast <- readRDS(file.path(DOSSIER_RESULTATS, "nowcast_dfm.rds"))
backtest <- readRDS(file.path(DOSSIER_RESULTATS, "backtest_dfm.rds"))
dfm <- readRDS(file.path(DOSSIER_RESULTATS, "dfm_resultats.rds"))

cat("\n\n============================================================\n")
cat("        SYNTHESE FINALE -- Methode DFM\n")
cat("============================================================\n\n")
cat(sprintf("1) NOWCAST DU PIB (prochain trimestre) : %+.2f %%\n", nowcast$croissance_pib * 100))
cat(sprintf("\n2) VALIDATION HORS ECHANTILLON (%d trimestres) :\n", length(backtest$dates_test)))
cat(sprintf("   RMSFE DFM          : %.4f\n", backtest$rmsfe_modele))
cat(sprintf("   RMSFE repere AR(2) : %.4f\n", backtest$rmsfe_ar2))
cat(sprintf("   Diebold-Mariano p-value : %.3f\n", backtest$test_dm$p.value))
cat("\n3) METHODE PAR BRANCHE :\n")
for (b in names(dfm)) cat(sprintf("   %-28s %s (R2=%s, n_ind=%d)\n", b, dfm[[b]]$methode,
                                    ifelse(is.na(dfm[[b]]$r2), "NA", sprintf("%.3f", dfm[[b]]$r2)),
                                    dfm[[b]]$nb_indicateurs))
cat("\n============================================================\n")
