# ============================================================================
# run_pipeline.R -- Execute l'ensemble du pipeline GDPNow-Maroc, dans l'ordre
# ============================================================================
# Usage : Rscript run_pipeline.R   (a lancer depuis le dossier racine
#          R_gdpnow_maroc/, la ou se trouvent les dossiers R/, data/, etc.)
# ============================================================================
scripts <- c(
  "R/01_import_donnees.R",
  "R/02_analyse_exploratoire.R",
  "R/03_bvar_trimestriel.R",
  "R/04_bridge_equations.R",
  "R/05_ar4_non_couvertes.R",
  "R/06_agregation_fisher.R",
  "R/07_validation_pseudo_temps_reel.R",
  "R/08_rapport_synthese.R"
)
for (s in scripts) {
  cat("\n", strrep("=", 70), "\n", "EXECUTION : ", s, "\n", strrep("=", 70), "\n", sep = "")
  source(s)
}
