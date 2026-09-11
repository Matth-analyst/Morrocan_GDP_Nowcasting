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
# /!\ Pointer DATA_DIR vers le nouveau dossier de nettoyage (celui qui
# contient désormais aussi Administration_publique, Autres_services,
# Education-sante, Services_aux_entreprises) -- renomme-le en
# "clean_output" ou ajuste le chemin ci-dessous.

PROJECT_DIR <- path.expand("~/2A.ESBD/GDPNow_Maroc_donnees_nettoyees")
DATA_DIR    <- file.path(PROJECT_DIR, "clean_output_v2")
OUT_DIR     <- file.path(PROJECT_DIR, "resultats_dfm")
FIG_DIR     <- file.path(OUT_DIR, "figures")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

# --------------------------------------------------------------------------
# 3. Secteurs
# --------------------------------------------------------------------------
# Les 12 secteurs "historiques" disposent tous d'au moins une cible +
# des indicateurs (trimestriels et/ou mensuels). Les 4 nouveaux secteurs
# n'ont QUE leur VA cible (aucun fichier __trimestriel.csv ni
# __mensuel.csv livré) -- le pipeline DFM (06/07/08) les écarte déjà
# automatiquement faute d'indicateur mensuel, et 09_nowcast.R les bascule
# alors sur un simple AR(p) sur leur Δlog(VA), comme c'était déjà fait
# pour Information_communication (qui n'a que des indicateurs
# trimestriels, volontairement exclus du DFM -- cf. 08).

SECTEURS_AVEC_INDICATEURS <- c(
  "Agriculture", "Peche", "Industrie_transformation", "Industrie_extraction",
  "Finances_assurances", "Hebergement_restauration", "Construction",
  "Commerce", "Transports", "Electricite_gaz_eau",
  "Information_communication", "Immobilier"
)

# Informatif uniquement : la bascule vers l'AR dans 09_nowcast.R est
# détectée dynamiquement (secteur sans nowcast DFM), pas via cette liste.
SECTEURS_SANS_INDICATEURS <- c(
  "Administration_publique", "Autres_services",
  "Education-sante", "Services_aux_entreprises"
)

SECTEURS <- c(SECTEURS_AVEC_INDICATEURS, SECTEURS_SANS_INDICATEURS)

# --------------------------------------------------------------------------
# 4. Paramètres statistiques
# --------------------------------------------------------------------------

ADF_SEUIL <- 0.05    # seuil de p-value pour conclure a la stationnarite
N_MIN_ADF <- 15      # nb minimal d'observations pour un ADF fiable

RMAX <- 15           # nombre max de facteurs candidats
PMAX <- 6            # ordre max de VAR candidat sur les facteurs

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
message("Nombre de secteurs total       : ", length(SECTEURS))
message("  dont avec indicateurs (DFM)  : ", length(SECTEURS_AVEC_INDICATEURS))
message("  dont cible seule (AR)        : ", length(SECTEURS_SANS_INDICATEURS),
        " (", paste(SECTEURS_SANS_INDICATEURS, collapse = ", "), ")")

if (!requireNamespace("dfms", quietly = TRUE)) {
  stop(
    "Le package 'dfms' n'a pas pu etre installe/charge.\n",
    "Essayer manuellement : install.packages('dfms')"
  )
}
message("OK : dfms disponible (version ", as.character(packageVersion("dfms")), ")")
message("==============================================")