# ============================================================================
# 00_setup.R -- Configuration generale, methode DFM
# ============================================================================
# References methodologiques :
#   Higgins, P. (2014) "GDPNow: A Model for GDP Nowcasting", FRB Atlanta WP 2014-7
#   Banbura, M., Giannone, D., Reichlin, L. (2010) "Large Bayesian VARs", JAE 25(1)
#   Litterman, R. (1986) "Forecasting with Bayesian Vector Autoregressions"
#   Diebold, F.X., Mariano, R.S. (1995) "Comparing Predictive Accuracy"
#
# METHODE : modele a facteurs dynamiques (Dynamic Factor Model), estime par
# quasi-maximum de vraisemblance via l'algorithme EM.
#
# L'IMPORT EST REPRIS A L'IDENTIQUE DE LA METHODE 1, ET C'EST DELIBERE.
# Les deux methodes doivent etre comparees sur EXACTEMENT les memes donnees :
# si l'import differait, un ecart de performance pourrait venir du traitement
# des donnees plutot que du modele, et la comparaison ne prouverait rien. Un
# controle verifie que les fichiers produits ici sont identiques, octet pour
# octet, a ceux de la methode 1.
#
# CONVENTIONS DU PROJET
#   1. Source unique des donnees : le grand classeur
#      GDPNow_Maroc_series_retenues_Apres_Tris_economiques.xlsx.
#      Il n'est jamais modifie : le pipeline y puise, il n'y ecrit pas.
#   2. Datation : toute observation est datee au DERNIER JOUR de sa periode.
#      Le classeur date au premier jour ; la conversion est faite a la lecture.
#   3. Echanges entre etapes : fichiers CSV uniquement, pas de .rds.
# ============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
})

# --- Poids de combinaison BVAR / passerelle -----------------------------------
# delta = 1 donne le BVAR seul, delta = 0 la passerelle seule.
#
# POURQUOI UNE CONSTANTE. Le delta optimise recursivement (solution fermee des
# moindres carres sur l'historique) a ete compare a quatre autres regles sur le
# protocole complet, phase 23 du plan. L'amplitude totale entre la meilleure
# regle et la pire est de 0,005 point de ratio median -- autrement dit, le
# reglage de delta ne decide de rien. Le delta estime n'est meme pas le
# meilleur : la passerelle seule le devance.
#
# C'est le resultat classique de la litterature sur la combinaison de
# previsions : les poids egaux sont difficiles a battre, parce que les poids
# estimes sur un historique court sont trop bruites pour rapporter plus que le
# bruit qu'ils ajoutent. A performance egale, la constante est preferee : un
# parametre estime de moins, donc un risque de surajustement de moins, et un
# chiffre publie qui ne depend plus d'une optimisation invisible.
#
# Mettre NA pour revenir au delta estime recursivement.
DELTA_CONSTANT <- 0.5

# --- Chemins -----------------------------------------------------------------
# Les classeurs sources sont a la RACINE DU DEPOT, dans SourceData/, en un
# seul exemplaire partage par les deux methodes. Auparavant chacune en gardait
# sa copie : trois exemplaires du meme fichier, donc trois occasions de diverger
# sans que rien ne le signale.
DOSSIER_SOURCES <- file.path("..", "SourceData")
CHEMIN_VIVIER <- file.path(DOSSIER_SOURCES,
                           "GDPNow_Maroc_series_retenues_Apres_Tris_economiques.xlsx")
CHEMIN_VA     <- file.path(DOSSIER_SOURCES, "VA_reelle_par_branche.xlsx")

DOSSIER_DATA      <- "data"
DOSSIER_RESULTATS <- "resultats"
DOSSIER_FIGURES   <- "figures"
for (d in c(DOSSIER_DATA, DOSSIER_RESULTATS, DOSSIER_FIGURES)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# --- Nomenclature des 16 branches (HCP) --------------------------------------
# L'orthographe exacte fait foi : c'est la cle de jointure entre la cible et
# les indicateurs.
TOUTES_BRANCHES <- c(
  "Agriculture",
  "Pêche",
  "Industrie d'extraction",
  "Industrie de transformation",
  "Électricité, gaz, eau",
  "Construction",
  "Commerce",
  "Transports",
  "Hébergement-restauration",
  "Information-communication",
  "Finances et assurances",
  "Immobilier",
  "Services aux entreprises",
  "Administration publique",
  "Éducation-santé",
  "Autres services"
)

# --- Correspondance feuille du classeur -> nomenclature officielle ----------
CORRESP_BRANCHES <- c(
  "Agriculture"               = "Agriculture",
  "Peche"                     = "Pêche",
  "Industrie transformation"  = "Industrie de transformation",
  "Industrie extraction"      = "Industrie d'extraction",
  "Finances assurances"       = "Finances et assurances",
  "Hebergement restauration"  = "Hébergement-restauration",
  "Construction"              = "Construction",
  "Commerce"                  = "Commerce",
  "Transports"                = "Transports",
  "Electricite gaz eau"       = "Électricité, gaz, eau",
  "Information communication" = "Information-communication",
  "Immobilier"                = "Immobilier"
)

#' Nom de dossier ASCII pour une branche (sans accent, sans apostrophe ni
#' virgule) : c'est le nom de l'onglet du classeur, espaces remplaces par des
#' underscores. Garantit des chemins portables.
dossier_branche <- function(branche) {
  feuille <- names(CORRESP_BRANCHES)[match(branche, CORRESP_BRANCHES)]
  feuille[is.na(feuille)] <- branche[is.na(feuille)]
  gsub("[^A-Za-z0-9]+", "_", feuille)
}

# --- Ecriture CSV ------------------------------------------------------------
#' Ecrit un CSV en UTF-8 avec BOM, pour que les accents s'affichent
#' correctement a l'ouverture dans Excel. Separateur virgule, decimale point
#' (format standard, relu tel quel par read.csv et par pandas).
ecrire_csv <- function(x, chemin) {
  dir.create(dirname(chemin), showWarnings = FALSE, recursive = TRUE)
  con <- file(chemin, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("﻿"), con)                 # BOM UTF-8
  txt <- utils::capture.output(
    utils::write.csv(x, row.names = FALSE, na = "", quote = TRUE)
  )
  writeBin(charToRaw(paste0(paste(txt, collapse = "\n"), "\n")), con)
  invisible(chemin)
}

#' Lecture symetrique (gere le BOM).
lire_csv <- function(chemin) {
  utils::read.csv(chemin, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE,
                  check.names = FALSE)
}

# --- Couverture des branches -------------------------------------------------
#' Partition couvertes / non couvertes, deduite en phase 1 et relue ici.
charger_couverture <- function() {
  f <- file.path(DOSSIER_DATA, "couverture_branches.csv")
  if (!file.exists(f)) {
    stop("couverture_branches.csv absent : executer d'abord R/01_import_donnees.R",
         call. = FALSE)
  }
  d <- lire_csv(f)
  list(couvertes     = d$branche[d$couverte],
       non_couvertes = d$branche[!d$couverte])
}

# --- Theme graphique commun --------------------------------------------------
theme_gdpnow <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 9.5),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )
theme_set(theme_gdpnow)

cat(sprintf("[setup] %d branches HCP | source : %s\n",
            length(TOUTES_BRANCHES), CHEMIN_VIVIER))
