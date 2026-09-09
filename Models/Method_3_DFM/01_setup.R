# ============================================================================
# GDPNow Maroc — 01_setup.R
# Configuration générale du projet
# ============================================================================

# --------------------------------------------------------------------------
# 1. Packages
# --------------------------------------------------------------------------
# NB : "nowcasting" est archivé sur CRAN, "nowcast" est un package
# d'épidémiologie (rien à voir), "nowcastDFM" n'existe pas sur CRAN.
# On utilise "dfms" (Sebastian Krantz) : implémentation active et
# maintenue de l'estimation EM/QML (Doz-Giannone-Reichlin / Bańbura-
# Modugno) des DFM, avec gestion native des données manquantes.

packages <- c(
  "tseries", "vars", "zoo", "xts", "lubridate",
  "dplyr", "tidyr", "ggplot2", "dfms"
)

packages_manquants <- packages[!packages %in% rownames(installed.packages())]
if (length(packages_manquants) > 0) install.packages(packages_manquants)
invisible(lapply(packages, library, character.only = TRUE))

# --------------------------------------------------------------------------
# 2. Répertoires
# --------------------------------------------------------------------------

PROJECT_DIR <- path.expand("~/2A.ESBD/GDPNow_Maroc_donnees_nettoyees")
DATA_DIR    <- file.path(PROJECT_DIR, "clean_output")
OUT_DIR     <- file.path(PROJECT_DIR, "resultats_dfm")
FIG_DIR     <- file.path(OUT_DIR, "figures")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

# --------------------------------------------------------------------------
# 3. Secteurs
# --------------------------------------------------------------------------

SECTEURS <- c(
  "Agriculture", "Peche", "Industrie_transformation", "Industrie_extraction",
  "Finances_assurances", "Hebergement_restauration", "Construction",
  "Commerce", "Transports", "Electricite_gaz_eau",
  "Information_communication", "Immobilier"
)

# --------------------------------------------------------------------------
# 4. Paramètres statistiques
# --------------------------------------------------------------------------

ADF_SEUIL <- 0.05    # seuil de p-value pour conclure a la stationnarite
N_MIN_ADF <- 15       # nb minimal d'observations pour un ADF fiable

RMAX <- 15             # nombre max de facteurs candidats
PMAX <- 6             # ordre max de VAR candidat sur les facteurs

COUVERTURE_MIN_PANEL_STAT <- 0.30  # seuil de securite (etape 05) : si une
# serie deja filtree en amont (Python) tombe sous ce seuil de couverture
# APRES transformation (log-diff perd 1 obs, diff ordre2 en perd 2), on
# l'exclut du panel final avec un avertissement.

# --------------------------------------------------------------------------
# 5. Fréquences
# --------------------------------------------------------------------------

FREQ_MENSUELLE     <- 12
FREQ_TRIMESTRIELLE <- 4

# --------------------------------------------------------------------------
# 6. Vérifications de démarrage
# --------------------------------------------------------------------------

message("==============================================")
message(" GDPNow Maroc — Configuration")
message("==============================================")
message("Projet    : ", PROJECT_DIR)
message("Données   : ", DATA_DIR)
message("Résultats : ", OUT_DIR)

if (!dir.exists(DATA_DIR)) {
  stop("Le dossier DATA_DIR n'existe pas :\n", DATA_DIR)
}
message("OK : dossier de données trouvé.")
message("Nombre de secteurs : ", length(SECTEURS))

if (!requireNamespace("dfms", quietly = TRUE)) {
  stop(
    "Le package 'dfms' n'a pas pu etre installe/charge.\n",
    "Essayer manuellement : install.packages('dfms')"
  )
}
message("OK : dfms disponible (version ", as.character(packageVersion("dfms")), ")")
message("==============================================")
