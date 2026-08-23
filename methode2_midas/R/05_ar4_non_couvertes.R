# ============================================================================
# 05_ar4_non_couvertes.R -- AR(4) pur pour les 9 branches sans indicateur
# ============================================================================
# Reference : Higgins (2014) -- "For dormitories investment growth, and any
# other subcomponents for which we do not have a related monthly series,
# we use an AR(4) forecast" (section 3, etape 4a ; note du tableau A5b).
# Meme principe applique ici a l'echelle de la branche entiere plutot qu'a
# une sous-composante mineure -- difference d'echelle documentee dans les
# livrables precedents (note methodologique jointe au rapport).
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))

resultats_ar4 <- list()
for (b in BRANCHES_NON_COUVERTES) {
  serie <- cibles %>% filter(branche == b) %>% arrange(date) %>% pull(dlog_va)
  serie <- serie[!is.na(serie)]
  fit <- Arima(serie, order = c(4, 0, 0))
  prev <- forecast(fit, h = 1)
  resultats_ar4[[b]] <- list(fit = fit, prevision = as.numeric(prev$mean),
                              ic80_bas = as.numeric(prev$lower[,"80%"]),
                              ic80_haut = as.numeric(prev$upper[,"80%"]))
  cat(sprintf("%-28s | AR(4) prevision=%+.4f  [IC80%%: %+.4f ; %+.4f]\n",
              b, resultats_ar4[[b]]$prevision, resultats_ar4[[b]]$ic80_bas, resultats_ar4[[b]]$ic80_haut))
}

saveRDS(resultats_ar4, file.path(DOSSIER_RESULTATS, "ar4_non_couvertes.rds"))
cat("\nModeles AR(4) sauvegardes pour les", length(resultats_ar4), "branches non couvertes.\n")
