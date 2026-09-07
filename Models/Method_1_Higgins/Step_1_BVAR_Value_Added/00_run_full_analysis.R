# ============================================================================
# run_full_analysis.R
# ----------------------------------------------------------------------------
# Script maitre -- lance, dans l'ordre, les 4 scripts de l'Etape 1
# (BVAR trimestriel) :
#
#   01_transformation_deltalog_ADF.R        -> justifie le choix du Δlog
#   02_calcul_sigma_i.R                     -> calcule sigma_i par branche
#   03_bvar_estimation_et_choix_lambda.R    -> estime le BVAR, choisit lambda
#   04_verification_stabilite_VAR.R         -> verifie la stabilite du VAR
#
# A LANCER depuis le dossier qui contient a la fois ce script et le fichier
# VA_reelle_par_branche.xlsx (les 4 scripts utilisent des chemins relatifs
# a ce dossier de travail).
#
# Chaque script se recharge les donnees necessaires depuis les fichiers
# intermediaires du precedent (resultats/*.rds) -- ce script maitre se
# contente de les executer dans le bon ordre, sans dupliquer leur logique.
# ============================================================================

scripts <- c(
  "01_transformation_deltalog_ADF.R",
  "02_calcul_sigma_i.R",
  "03_bvar_estimation_et_choix_lambda.R",
  "04_verification_stabilite_VAR.R"
)

# --- Verification prealable : tous les scripts sont-ils presents ? --------
manquants <- scripts[!file.exists(scripts)]
if (length(manquants) > 0) {
  stop("Script(s) introuvable(s) dans le dossier courant : ",
       paste(manquants, collapse = ", "),
       "\nVerifiez que run_full_analysis.R est bien lance depuis le dossier",
       " qui contient tous les scripts numerotes.")
}

if (!file.exists("VA_reelle_par_branche.xlsx")) {
  stop("Fichier de donnees introuvable : VA_reelle_par_branche.xlsx\n",
       "Placez-le dans le meme dossier que ce script avant de relancer.")
}

# --- Execution sequentielle, avec chronometrage et gestion d'erreur -------
heure_debut_globale <- Sys.time()

for (script in scripts) {
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat(sprintf("EXECUTION : %s\n", script))
  cat(strrep("=", 78), "\n\n", sep = "")

  heure_debut <- Sys.time()

  resultat <- tryCatch({
    source(script, echo = FALSE)
    TRUE
  }, error = function(e) {
    cat("\n", strrep("!", 78), "\n", sep = "")
    cat(sprintf("ERREUR dans %s :\n%s\n", script, conditionMessage(e)))
    cat(strrep("!", 78), "\n", sep = "")
    FALSE
  })

  duree <- round(difftime(Sys.time(), heure_debut, units = "secs"), 1)

  if (!resultat) {
    stop(sprintf(
      "\nL'execution s'est arretee a l'etape '%s'. Corrigez l'erreur ci-dessus",
      " avant de relancer -- les etapes suivantes dependent de ses sorties.",
      script
    ))
  }

  cat(sprintf("\n[%s termine en %s secondes]\n", script, duree))
}

duree_totale <- round(difftime(Sys.time(), heure_debut_globale, units = "secs"), 1)

cat("\n", strrep("=", 78), "\n", sep = "")
cat(sprintf("ANALYSE COMPLETE TERMINEE en %s secondes.\n", duree_totale))
cat(strrep("=", 78), "\n\n", sep = "")

cat("Fichiers produits dans resultats/ :\n")
for (f in list.files("resultats", full.names = FALSE)) cat("  -", f, "\n")

cat("\nFigures produites dans figures/ :\n")
for (f in list.files("figures", full.names = FALSE)) cat("  -", f, "\n")

cat("\nModele BVAR final disponible : resultats/bvar_modele_final.rds\n")
