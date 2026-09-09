# ============================================================================
# GDPNow Maroc — 00_run_all.R
# Exécution complète du pipeline
# ============================================================================
# Ordre d'exécution :
#   01_setup.R                    -> packages, chemins, paramètres
#   02_load_data.R                -> fonctions de chargement (pas d'exécution)
#   03_stationarity_diagnostic.R  -> diagnostic ADF au niveau (charge les panels bruts)
#   04_transformation.R           -> décision + application de la transformation
#   05_missing_data.R             -> diagnostic NA + panel équilibré
#   06_factor_selection.R         -> choix de r (Bai & Ng ICp2)
#   07_var_selection.R            -> choix de p (AIC/HQ/SC/FPE)
#   08_dfm_estimation.R           -> estimation DFM-EM (dfms)
#   09_nowcast.R                  -> nowcast de croissance sectorielle
#   10_evaluation.R               -> évaluation in-sample + diagnostics
#
# Placer ce script et les 10 fichiers numérotés dans le même dossier,
# avec le sous-dossier clean_output/ (livré séparément) accessible via
# DATA_DIR (défini dans 01_setup.R).
# ============================================================================

t0 <- Sys.time()

fichiers <- c(
  "01_setup.R",
  "02_load_data.R",
  "03_stationarity_diagnostic.R",
  "04_transformation.R",
  "05_missing_data.R",
  "06_factor_selection.R",
  "07_var_selection.R",
  "08_dfm_estimation.R",
  "09_nowcast.R",
  "10_evaluation.R"
)

for (f in fichiers) {
  message("\n############################################################")
  message("## Exécution : ", f)
  message("############################################################")
  t_debut <- Sys.time()
  source(f, echo = FALSE)
  message("(", f, " terminé en ", round(difftime(Sys.time(), t_debut, units = "secs"), 1), " sec)")
}

message("\n============================================================")
message(" Pipeline complet terminé en ",
        round(difftime(Sys.time(), t0, units = "mins"), 1), " minutes.")
message(" Résultats dans : ", OUT_DIR)
message(" Figures dans   : ", FIG_DIR)
message("============================================================")

message("\nFichiers clés à relire pour le rapport :")
message("  - diagnostic_stationnarite_resume_secteur.csv (diagnostic ADF au niveau)")
message("  - transformations_ADF.csv (décisions de transformation)")
message("  - verification_stationnarite_apres_transfo.csv (gain de la transformation)")
message("  - resume_valeurs_manquantes_secteur.csv (taux de NA après transformation)")
message("  - selection_p_retenu.csv (r et p retenus par secteur)")
message("  - evaluation.csv (RMSE/MAE/sMAPE in-sample par secteur)")
message("  - nowcast_croissance_sectorielle.csv (résultat final : croissance nowcastée par secteur)")
