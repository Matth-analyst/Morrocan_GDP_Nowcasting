# ============================================================================
# 02_import_indicateurs_midas.R -- Import des 37 indicateurs presélectionnés
#                                    par le critere dedie a MIDAS (test F joint
#                                    sur m1, m2, m3 -- voir criteres_midas.py)
# ============================================================================
source("R/00_setup.R")

indicateurs_midas <- read.csv("data/indicateurs_midas.csv", encoding = "UTF-8") %>%
  mutate(date = as.Date(date)) %>%
  as_tibble()

cat("Indicateurs MIDAS importés :", n_distinct(indicateurs_midas$branche, indicateurs_midas$indicateur),
    "séries,", nrow(indicateurs_midas), "observations.\n")
cat("\nPar branche :\n")
print(indicateurs_midas %>% distinct(branche, indicateur) %>% count(branche, name = "n_series"))

saveRDS(indicateurs_midas, file.path(DOSSIER_RESULTATS, "indicateurs_midas.rds"))
