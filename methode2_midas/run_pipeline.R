scripts <- c("R/01_import_donnees.R","R/02_analyse_exploratoire.R",
             "R/02b_import_indicateurs_midas.R","R/03_bvar_trimestriel.R",
             "R/04_midas.R","R/05_ar4_non_couvertes.R","R/06_agregation_midas.R",
             "R/07_validation_midas.R","R/08_rapport_synthese_midas.R")
for (s in scripts) {
  cat("\n", strrep("=", 70), "\n", "EXECUTION : ", s, "\n", strrep("=", 70), "\n", sep = "")
  source(s)
}
