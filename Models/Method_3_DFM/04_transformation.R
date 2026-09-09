# ============================================================
# 04_transformation.R
# TRANSFORMATION ET STATIONNARISATION DES SERIES
# ============================================================
#
# OBJECTIF :
#   - Rendre les séries compatibles avec le DFM
#   - Les indicateurs : transformation automatique selon ADF
#   - Les cibles VA : TOUJOURS en log-différence
#
# CHOIX METHODOLOGIQUE POUR LES CIBLES :
#
#   VA_t  -> log(VA_t) -> Δlog(VA_t)
#
#   Le DFM prédit donc :
#
#       Δlog(VA_t)
#
#   et le nowcast final sera converti en taux de croissance :
#
#       100 * (exp(Δlog(VA_t)) - 1)
#
#   Ainsi, TOUS les secteurs auront une sortie en % de
#   croissance trimestrielle.
#
# IMPORTANT :
#   Les données restent sur leur grille mensuelle.
#   Les variables trimestrielles sont présentes uniquement
#   aux mois de fin de trimestre (mars, juin, septembre, décembre).
#
# ============================================================


# ============================================================
# 0. CHARGEMENT
# ============================================================

source(file.path(PROJECT_DIR, "01_setup.R"))

library(zoo)
library(xts)
library(tseries)
library(dplyr)
library(ggplot2)


# ============================================================
# 1. VERIFICATION DES OBJETS
# ============================================================

if (!exists("panels_bruts")) {
  
  fichier <- file.path(
    OUT_DIR,
    "panels_bruts.rds"
  )
  
  if (file.exists(fichier)) {
    
    panels_bruts <- readRDS(fichier)
    
  } else {
    
    stop(
      "Objet 'panels_bruts' introuvable.\n",
      "Lance d'abord 02_load_data.R."
    )
  }
}


# ============================================================
# 2. PARAMETRES
# ============================================================

ADF_SEUIL <- 0.05
N_MIN_ADF <- 15

# Seuil utilisé pour déterminer si une série peut être log-transformée
SEUIL_POSITIF <- 0.95


# ============================================================
# 3. FONCTION ADF SECURISEE
# ============================================================

test_adf <- function(x) {
  
  x <- as.numeric(x)
  
  x <- x[is.finite(x)]
  
  if (length(x) < N_MIN_ADF) {
    return(
      list(
        pvalue = NA_real_,
        fiable = FALSE,
        n = length(x)
      )
    )
  }
  
  resultat <- tryCatch(
    tseries::adf.test(x),
    error = function(e) NULL
  )
  
  if (is.null(resultat)) {
    
    return(
      list(
        pvalue = NA_real_,
        fiable = FALSE,
        n = length(x)
      )
    )
  }
  
  list(
    pvalue = resultat$p.value,
    fiable = TRUE,
    n = length(x)
  )
}


# ============================================================
# 4. FONCTION POUR SAVOIR SI UNE SERIE EST POSITIVE
# ============================================================

est_majoritairement_positive <- function(x) {
  
  x <- as.numeric(x)
  
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    return(FALSE)
  }
  
  mean(x > 0) >= SEUIL_POSITIF
}


# ============================================================
# 5. APPLICATION DES TRANSFORMATIONS
# ============================================================
#
# Transformations possibles :
#
#   niveau
#   diff
#   log_diff
#   diff_ordre2
#   log_diff_ordre2
#
# Pour les cibles :
#
#   FORCAGE = log_diff
#
# ============================================================

appliquer_transformation <- function(x, transfo) {
  
  x <- as.numeric(x)
  
  resultat <- rep(NA_real_, length(x))
  
  
  # ----------------------------------------------------------
  # Niveau
  # ----------------------------------------------------------
  
  if (transfo == "niveau") {
    
    resultat <- x
  }
  
  
  # ----------------------------------------------------------
  # Première différence
  # ----------------------------------------------------------
  
  else if (transfo == "diff") {
    
    resultat <- c(
      NA_real_,
      diff(x)
    )
  }
  
  
  # ----------------------------------------------------------
  # Différence logarithmique
  # ----------------------------------------------------------
  
  else if (transfo == "log_diff") {
    
    if (any(
      is.finite(x) & x <= 0,
      na.rm = TRUE
    )) {
      
      warning(
        "Présence de valeurs <= 0 : log_diff impossible."
      )
      
      return(resultat)
    }
    
    lx <- log(x)
    
    resultat <- c(
      NA_real_,
      diff(lx)
    )
  }
  
  
  # ----------------------------------------------------------
  # Deuxième différence
  # ----------------------------------------------------------
  
  else if (transfo == "diff_ordre2") {
    
    d1 <- c(
      NA_real_,
      diff(x)
    )
    
    resultat <- c(
      NA_real_,
      diff(d1)
    )
  }
  
  
  # ----------------------------------------------------------
  # Deuxième différence logarithmique
  # ----------------------------------------------------------
  
  else if (transfo == "log_diff_ordre2") {
    
    if (any(
      is.finite(x) & x <= 0,
      na.rm = TRUE
    )) {
      
      warning(
        "Présence de valeurs <= 0 : log_diff_ordre2 impossible."
      )
      
      return(resultat)
    }
    
    lx <- log(x)
    
    d1 <- c(
      NA_real_,
      diff(lx)
    )
    
    resultat <- c(
      NA_real_,
      diff(d1)
    )
  }
  
  
  else {
    
    stop(
      "Transformation inconnue : ",
      transfo
    )
  }
  
  
  # ----------------------------------------------------------
  # IMPORTANT :
  # on conserve exactement la longueur originale.
  #
  # Cela permet de conserver les NA correspondant aux mois
  # où aucune observation trimestrielle n'existe.
  # ----------------------------------------------------------
  
  resultat
}

# ============================================================
# TRANSFORMATION SPECIFIQUE DE LA CIBLE TRIMESTRIELLE
# ============================================================
# La VA est stockée sur une grille mensuelle mais n'est observée
# qu'une fois par trimestre :
#
# 1998-01 : NA
# 1998-02 : NA
# 1998-03 : VA T1
# 1998-04 : NA
# 1998-05 : NA
# 1998-06 : VA T2
#
# Un diff(log(x)) classique produit alors uniquement des NA,
# car les observations trimestrielles ne sont pas consécutives.
#
# On extrait donc d'abord les observations trimestrielles,
# on calcule la croissance entre deux trimestres consécutifs,
# puis on replace les résultats aux dates trimestrielles.

appliquer_log_diff_trimestriel <- function(x) {
  
  x_num <- as.numeric(x)
  
  # Résultat initial : uniquement des NA
  resultat <- rep(NA_real_, length(x_num))
  
  # Positions où la VA est effectivement observée
  idx_valides <- which(is.finite(x_num))
  
  # Pas assez d'observations pour calculer une croissance
  if (length(idx_valides) < 2) {
    return(resultat)
  }
  
  # Extraction des seules observations trimestrielles
  valeurs <- x_num[idx_valides]
  
  # Le logarithme nécessite des valeurs strictement positives
  if (any(valeurs <= 0)) {
    warning(
      "CIBLE VA : présence de valeurs <= 0 ; log_diff impossible."
    )
    return(resultat)
  }
  
  # Croissance trimestrielle en logarithme :
  # Δlog(VA_t) = log(VA_t) - log(VA_{t-1})
  dlog <- diff(log(valeurs))
  
  # On replace chaque croissance à la date du trimestre
  # correspondant à la deuxième observation et suivantes
  resultat[idx_valides[-1]] <- dlog
  
  return(resultat)
}

# ============================================================
# 6. TRANSFORMATION AUTOMATIQUE D'UNE SERIE
# ============================================================
#
# Cette fonction est utilisée UNIQUEMENT pour les indicateurs.
#
# Les cibles passent par une fonction séparée qui impose
# log_diff.
# ============================================================

choisir_transformation_indicateur <- function(x) {
  
  x_num <- as.numeric(x)
  
  adf_niveau <- test_adf(x_num)
  
  # ----------------------------------------------------------
  # Cas 1 : série stationnaire en niveau
  # ----------------------------------------------------------
  
  if (
    isTRUE(adf_niveau$fiable) &&
    !is.na(adf_niveau$pvalue) &&
    adf_niveau$pvalue < ADF_SEUIL
  ) {
    
    return(
      list(
        transfo = "niveau",
        pval_niveau = adf_niveau$pvalue,
        motif = "Stationnaire en niveau"
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Cas 2 : série non stationnaire
  #
  # Si elle est majoritairement positive :
  #   -> log_diff
  #
  # Sinon :
  #   -> diff
  # ----------------------------------------------------------
  
  if (est_majoritairement_positive(x_num)) {
    
    transfo <- "log_diff"
    
    motif <- paste(
      "Non stationnaire ; série positive ;",
      "première différence logarithmique"
    )
    
  } else {
    
    transfo <- "diff"
    
    motif <- paste(
      "Non stationnaire ; série non positive ;",
      "première différence"
    )
  }
  
  
  # ----------------------------------------------------------
  # Vérification de la stationnarité après transformation
  # ----------------------------------------------------------
  
  x_transfo <- appliquer_transformation(
    x_num,
    transfo
  )
  
  adf_transfo <- test_adf(x_transfo)
  
  
  # ----------------------------------------------------------
  # Si toujours non stationnaire :
  # deuxième différence
  # ----------------------------------------------------------
  
  if (
    isTRUE(adf_transfo$fiable) &&
    !is.na(adf_transfo$pvalue) &&
    adf_transfo$pvalue >= ADF_SEUIL
  ) {
    
    if (transfo == "log_diff") {
      
      transfo_final <- "log_diff_ordre2"
      
    } else {
      
      transfo_final <- "diff_ordre2"
    }
    
    motif <- paste(
      motif,
      "| Première différence insuffisante ;",
      "deuxième différence utilisée"
    )
    
  } else {
    
    transfo_final <- transfo
  }
  
  
  list(
    transfo = transfo_final,
    pval_niveau = adf_niveau$pvalue,
    motif = motif
  )
}


# ============================================================
# 7. TRANSFORMATION SPECIFIQUE DES CIBLES
# ============================================================
#
# REGLE FONDAMENTALE DU PROJET :
#
#       TOUTES LES VA = log_diff
#
# Même si l'ADF considère que la VA est stationnaire en niveau,
# on impose cette transformation car l'objectif final est de
# produire un taux de croissance trimestriel pour chaque secteur.
#
# ============================================================

choisir_transformation_cible <- function(x) {
  
  x_num <- as.numeric(x)
  
  adf_niveau <- test_adf(x_num)
  
  
  # ----------------------------------------------------------
  # Vérification de la possibilité de prendre le logarithme
  # ----------------------------------------------------------
  
  valeurs_finies <- x_num[
    is.finite(x_num)
  ]
  
  proportion_positive <- if (
    length(valeurs_finies) > 0
  ) {
    
    mean(valeurs_finies > 0)
    
  } else {
    
    0
  }
  
  
  # ----------------------------------------------------------
  # Si la série est suffisamment positive :
  #       log_diff
  #
  # C'est la transformation imposée aux cibles.
  # ----------------------------------------------------------
  
  if (
    proportion_positive >= SEUIL_POSITIF
  ) {
    
    transfo <- "log_diff"
    
    motif <- paste(
      "CIBLE VA : transformation log-diff imposée",
      "pour obtenir un taux de croissance trimestriel"
    )
    
    x_transfo <- appliquer_transformation(
      x_num,
      transfo
    )
    
    adf_apres <- test_adf(x_transfo)
    
    
    return(
      list(
        transfo = transfo,
        pval_niveau = adf_niveau$pvalue,
        pval_apres = adf_apres$pvalue,
        motif = motif
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Cas exceptionnel :
  # impossibilité mathématique d'utiliser log_diff.
  #
  # On ne bascule PAS automatiquement vers diff.
  # On signale la série pour vérification.
  # ----------------------------------------------------------
  
  warning(
    "CIBLE : série insuffisamment positive pour log_diff."
  )
  
  list(
    transfo = "log_diff",
    pval_niveau = adf_niveau$pvalue,
    pval_apres = NA_real_,
    motif = paste(
      "CIBLE VA : log_diff imposé mais",
      "présence de valeurs <= 0 ; vérification nécessaire"
    )
  )
}


# ============================================================
# 8. TRAITEMENT DE TOUS LES SECTEURS
# ============================================================

panels_stationnaires <- list()

resultats_transformation <- list()

series_manuel <- list()


for (secteur in names(panels_bruts)) {
  
  cat("\n")
  cat("============================================================\n")
  cat("Secteur :", secteur, "\n")
  cat("============================================================\n")
  
  
  panel <- panels_bruts[[secteur]]
  
  
  # ----------------------------------------------------------
  # Récupération des métadonnées
  # ----------------------------------------------------------
  
  monthly_vars <- attr(
    panel,
    "monthly_vars"
  )
  
  quarterly_vars <- attr(
    panel,
    "quarterly_vars"
  )
  
  target <- attr(
    panel,
    "target"
  )
  
  
  if (is.null(monthly_vars)) {
    monthly_vars <- character(0)
  }
  
  if (is.null(quarterly_vars)) {
    quarterly_vars <- character(0)
  }
  
  
  # ----------------------------------------------------------
  # Initialisation du panel transformé
  # ----------------------------------------------------------
  
  panel_transfo <- panel
  
  
  # ----------------------------------------------------------
  # Parcours des variables
  # ----------------------------------------------------------
  
  for (nom_var in colnames(panel)) {
    
    x <- panel[, nom_var]
    
    
    # ========================================================
    # IDENTIFICATION DE LA CIBLE
    # ========================================================
    
    est_cible <- (
      !is.null(target) &&
        nom_var == target
    )
    
    
    # ========================================================
    # CAS 1 : CIBLE VA
    # ========================================================
    
    if (est_cible) {
      
      # ==========================================================
      # CIBLE : VA TRIMESTRIELLE
      # ==========================================================
      
      choix <- choisir_transformation_cible(x)
      transfo <- choix$transfo
      
      # Transformation spécifique :
      # on calcule Δlog(VA) uniquement entre deux
      # observations trimestrielles successives.
      x_transfo <- appliquer_log_diff_trimestriel(x)
      
      panel_transfo[, nom_var] <- x_transfo
      
      cat(
        "  CIBLE :", nom_var,
        "| transformation =", transfo,
        "| non-NA avant =", sum(is.finite(as.numeric(x))),
        "| non-NA après =", sum(is.finite(x_transfo)),
        "\n"
      )
      
    } else {
      
      # ==========================================================
      # INDICATEURS MENSUELS
      # ==========================================================
      
      choix <- choisir_transformation_indicateur(x)
      transfo <- choix$transfo
      
      x_transfo <- appliquer_transformation(x, transfo)
      
      panel_transfo[, nom_var] <- x_transfo
      
      # Affichage robuste de la p-value
      pval <- choix$pvalue
      
      if (is.numeric(pval) && length(pval) == 1 && is.finite(pval)) {
        pval_txt <- as.character(round(pval, 4))
      } else {
        pval_txt <- "NA"
      }
      
      cat(
        "  ", nom_var,
        "| transformation =", transfo,
        "| ADF p-value =", pval_txt,
        "\n"
      )
    }
  }
  
  
  # ==========================================================
  # CONSERVATION DES METADONNEES
  # ==========================================================
  
  attr(
    panel_transfo,
    "monthly_vars"
  ) <- monthly_vars
  
  attr(
    panel_transfo,
    "quarterly_vars"
  ) <- quarterly_vars
  
  attr(
    panel_transfo,
    "target"
  ) <- target
  
  
  # ----------------------------------------------------------
  # Stockage
  # ----------------------------------------------------------
  
  panels_stationnaires[[secteur]] <- panel_transfo
  
  
  cat(
    "Nombre de variables :",
    ncol(panel_transfo),
    "\n"
  )
}


# ============================================================
# 9. CONVERSION DES RESULTATS
# ============================================================

transformations_ADF <- do.call(
  rbind,
  resultats_transformation
)

rownames(
  transformations_ADF
) <- NULL


# ============================================================
# 10. VERIFICATION DE LA STATIONNARITE APRES TRANSFORMATION
# ============================================================

verification_stationnarite <- list()


for (secteur in names(panels_stationnaires)) {
  
  panel <- panels_stationnaires[[secteur]]
  
  for (nom_var in colnames(panel)) {
    
    x <- panel[, nom_var]
    
    adf <- test_adf(x)
    
    verification_stationnarite[[length(verification_stationnarite) + 1]] <- data.frame(
      secteur = secteur,
      nom = nom_var,
      pvalue = adf$pvalue,
      stationnaire = (
        isTRUE(adf$fiable) &&
          !is.na(adf$pvalue) &&
          adf$pvalue < ADF_SEUIL
      ),
      fiable = adf$fiable,
      n = adf$n
    )
  }
}


verification_stationnarite_apres_transfo <- do.call(
  rbind,
  verification_stationnarite
)

rownames(
  verification_stationnarite_apres_transfo
) <- NULL


# ============================================================
# RESUME DES TRANSFORMATIONS
# ============================================================

cat("\n")
cat("============================================================\n")
cat("RESUME DES TRANSFORMATIONS\n")
cat("============================================================\n")

# Construire le résumé directement à partir des panels
resume_transformations <- data.frame()

for (secteur in names(panels_stationnaires)) {
  
  panel <- panels_stationnaires[[secteur]]
  
  target <- attr(panel, "target")
  
  # Nombre total de variables
  n_total <- ncol(panel)
  
  # Nombre de variables mensuelles
  monthly_vars <- attr(panel, "monthly_vars")
  
  if (is.null(monthly_vars)) {
    n_monthly <- 0
  } else {
    n_monthly <- length(monthly_vars)
  }
  
  # Ajouter le secteur au résumé
  resume_transformations <- rbind(
    resume_transformations,
    data.frame(
      Secteur = secteur,
      Variables_total = n_total,
      Variables_mensuelles = n_monthly,
      Cible = target,
      stringsAsFactors = FALSE
    )
  )
}

print(resume_transformations, row.names = FALSE)


# ============================================================
# VERIFICATION DES CIBLES
# ============================================================

cat("\n")
cat("============================================================\n")
cat("VERIFICATION DES CIBLES\n")
cat("============================================================\n")

cibles_non_logdiff <- data.frame()

for (secteur in names(panels_stationnaires)) {
  
  panel <- panels_stationnaires[[secteur]]
  target <- attr(panel, "target")
  
  if (is.null(target)) {
    next
  }
  
  # Récupération de l'index de la cible
  idx_target <- which(names(panel) == target)
  
  if (length(idx_target) != 1) {
    cat(
      "ATTENTION :", secteur,
      "| cible introuvable :", target,
      "\n"
    )
    next
  }
  
  # Valeurs transformées de la cible
  cible <- as.numeric(panel[, idx_target])
  
  n_non_na <- sum(is.finite(cible))
  
  # La cible doit être en log-diff
  # et donc contenir des variations autour de zéro
  cat(
    secteur,
    "| cible =", target,
    "| non-NA =", n_non_na,
    "| min =", round(min(cible, na.rm = TRUE), 4),
    "| max =", round(max(cible, na.rm = TRUE), 4),
    "\n"
  )
}


# ============================================================
# VERIFICATION FINALE
# ============================================================

cat("\n")
cat("============================================================\n")
cat("VERIFICATION FINALE DES CIBLES\n")
cat("============================================================\n")

toutes_cibles_ok <- TRUE

for (secteur in names(panels_stationnaires)) {
  
  panel <- panels_stationnaires[[secteur]]
  target <- attr(panel, "target")
  
  if (is.null(target)) {
    toutes_cibles_ok <- FALSE
    next
  }
  
  idx_target <- which(names(panel) == target)
  
  if (length(idx_target) != 1) {
    toutes_cibles_ok <- FALSE
    next
  }
  
  cible <- as.numeric(panel[, idx_target])
  
  n_non_na <- sum(is.finite(cible))
  
  if (n_non_na == 0) {
    cat("ERREUR :", secteur, "| cible entièrement NA\n")
    toutes_cibles_ok <- FALSE
  } else {
    cat("OK :", secteur, "|", n_non_na, "croissances trimestrielles\n")
  }
}

if (toutes_cibles_ok) {
  
  cat("\n")
  cat("============================================================\n")
  cat("TOUTES LES CIBLES SONT CORRECTEMENT TRANSFORMEES EN ΔLOG(VA)\n")
  cat("============================================================\n")
  
} else {
  
  cat("\n")
  cat("ATTENTION : certaines cibles nécessitent encore une vérification.\n")
}


# ============================================================
# SAUVEGARDE
# ============================================================

saveRDS(
  panels_stationnaires,
  file.path(DATA_DIR, "panels_stationnaires.rds")
)

cat("\n")
cat("Panels stationnaires sauvegardés.\n")