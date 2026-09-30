# ============================================================================
# lancer.R -- Point d'entree unique de l'application de production
# ============================================================================
# Depuis la racine du depot :   Rscript NowcastApp/lancer.R
# Depuis le dossier :           Rscript lancer.R
# Depuis RStudio :              source("NowcastApp/lancer.R")
#
# Le script verifie d'abord que tout ce dont l'application a besoin est en
# place, puis ouvre le navigateur. Un demarrage qui echoue doit dire pourquoi,
# pas afficher une page blanche.
# ============================================================================

# --- Se placer dans le dossier de l'application -----------------------------
dossier_app <- if (file.exists("app.R")) "." else
  if (file.exists("NowcastApp/app.R")) "NowcastApp" else
    stop("Lancer depuis la racine du depot ou depuis NowcastApp/.", call. = FALSE)
setwd(dossier_app)

# --- Bibliotheques -----------------------------------------------------------
requises <- c("shiny", "bslib", "DT", "plotly", "ggplot2", "dplyr", "tidyr",
              "tibble", "writexl", "readxl", "lubridate", "purrr", "stringr")
absentes <- requises[!vapply(requises, requireNamespace, logical(1), quietly = TRUE)]
if (length(absentes)) {
  stop("Bibliotheques manquantes : ", paste(absentes, collapse = ", "),
       "\nInstaller avec : install.packages(c(",
       paste(sprintf('"%s"', absentes), collapse = ", "), "))", call. = FALSE)
}

# --- Localiser la chaine de calcul ------------------------------------------
# L'application ne contient aucun modele : elle lit les sorties de la methode 1
# et, pour recalculer, appelle sa fonction centrale. Sans elle, il n'y a rien a
# afficher.
chaine <- local({
  for (c in c(file.path("..", "Method1BvarBridge"), "..")) {
    if (dir.exists(file.path(c, "R", "fonctions")) &&
        dir.exists(file.path(c, "resultats"))) return(normalizePath(c))
  }
  for (d in list.dirs("..", recursive = FALSE)) {
    if (dir.exists(file.path(d, "R", "fonctions")) &&
        file.exists(file.path(d, "run_pipeline.R"))) return(normalizePath(d))
  }
  stop("Chaine de calcul introuvable. L'application attend, a cote d'elle, un ",
       "dossier contenant R/fonctions et resultats (Method1BvarBridge).",
       call. = FALSE)
})

# --- Sorties indispensables --------------------------------------------------
# L'application peut fonctionner sans certaines sorties, mais pas sans
# celles-ci : les poids sectoriels et l'historique des previsions, dont depend
# la production du nowcast courant.
indispensables <- file.path(chaine, "resultats",
                            c("06_poids.csv", "09_previsions_intra.csv"))
absents <- indispensables[!file.exists(indispensables)]
if (length(absents)) {
  stop("Sorties de la chaine absentes : ", paste(basename(absents), collapse = ", "),
       "\nExecuter d'abord le pipeline (run_pipeline.R dans ", basename(chaine),
       ").", call. = FALSE)
}

classeur <- file.path("..", "SourceData",
                      "GDPNow_Maroc_series_retenues_Apres_Tris_economiques.xlsx")
if (!file.exists(classeur)) {
  warning("Le classeur source est introuvable dans SourceData/ : l'affichage ",
          "fonctionnera, mais le recalcul echouera.", call. = FALSE)
}

cat("\n  Nowcast de la valeur ajoutee -- application de production\n")
cat("  Chaine lue        : ", basename(chaine), "\n", sep = "")
cat("  Classeur source   : lu, jamais modifie\n")
cat("  Productions ecrites dans NowcastApp/productions/\n\n")

shiny::runApp(".", launch.browser = TRUE, port = 4321)
