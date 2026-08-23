scripts <- c("R/01_import_donnees.R","R/02_analyse_exploratoire.R",
             "R/02b_import_indicateurs_dfm.R","R/03_bvar_trimestriel.R",
             "R/04_dfm.R","R/05_ar4_non_couvertes.R","R/06_agregation_dfm.R",
             "R/07_validation_dfm.R","R/08_rapport_synthese_dfm.R")
for (s in scripts) {
  cat("\n", strrep("=", 70), "\n", "EXECUTION : ", s, "\n", strrep("=", 70), "\n", sep = "")
  source(s)
}
