# ============================================================
# 09_nowcast.R
# NOWCASTING SECTORIEL DU PIB MAROCAIN
# ============================================================
#
# OBJECTIF :
# Produire le nowcast de la croissance trimestrielle
# de la valeur ajoutée pour chaque secteur dont un DFM
# a été estimé -- ET, pour tout secteur qui n'a PAS de DFM
# exploitable (pas d'indicateur mensuel -- que ce soit un
# secteur "cible seule" comme Administration_publique,
# Autres_services, Education-sante, Services_aux_entreprises,
# ou un secteur comme Information_communication qui n'a que
# des indicateurs trimestriels volontairement exclus du DFM),
# un simple AR(p) est estimé sur sa cible déjà transformée.
#
# CIBLE DU DFM (et de l'AR, identique) :
#   Δlog(VA)
#
# CONVERSION FINALE :
#   croissance (%) = 100 * (exp(Δlog(VA)) - 1)
#
# IMPORTANT :
#   - Les cibles sont trimestrielles.
#   - Les panels sont organisés sur une grille mensuelle.
#   - Les NA entre deux fins de trimestre sont normaux.
#   - dfms traite la fréquence mixte grâce à quarterly.vars.
#   - predict.dfm() fournit la prévision sur l'échelle
#     transformée, ici Δlog(VA).
#   - standardized = FALSE ne reconstruit PAS le niveau
#     original de VA : il retire seulement la standardisation
#     interne de dfms.
#
# RESULTAT FINAL :
#   Le fichier CSV contient directement le taux de croissance
#   trimestriel prévu en pourcentage, DFM et AR confondus.
#
# ============================================================


# ============================================================
# 1. CHARGEMENT
# ============================================================

source(file.path(PROJECT_DIR, "01_setup.R"))

library(zoo)
library(xts)
library(dplyr)
library(ggplot2)


# ============================================================
# 2. CHARGER LES DONNEES ET LES MODELES
# ============================================================

panels_stationnaires_filtres <- readRDS(
  file.path(
    OUT_DIR,
    "panels_stationnaires_filtres.rds"
  )
)

modeles_dfm <- readRDS(
  file.path(
    OUT_DIR,
    "modeles_dfm.rds"
  )
)

cat("\n==============================================\n")
cat(" GDPNow Maroc — Nowcasting sectoriel\n")
cat("==============================================\n")

cat(
  "Panels charges :",
  length(panels_stationnaires_filtres),
  "\n"
)

cat(
  "Modeles charges :",
  length(modeles_dfm),
  "\n"
)


# ============================================================
# 3. FONCTION : PROCHAIN TRIMESTRE
# ============================================================
#
# La cible est observée sur :
#   mars / juin / septembre / décembre
#
# Exemple :
#
#   dernière date = mai 2026
#   cible = juin 2026
#
#   dernière date = juin 2026
#   cible = septembre 2026
#
#   dernière date = septembre 2026
#   cible = décembre 2026
#
#   dernière date = décembre 2026
#   cible = mars 2027
#
# ============================================================

prochain_fin_trimestre <- function(date_derniere) {
  
  annee <- as.integer(
    format(date_derniere, "%Y")
  )
  
  mois <- as.integer(
    format(date_derniere, "%m")
  )
  
  if (mois < 3) {
    
    # Janvier / février
    as.Date(
      sprintf("%d-03-01", annee)
    )
    
  } else if (mois < 6) {
    
    # Avril / mai
    as.Date(
      sprintf("%d-06-01", annee)
    )
    
  } else if (mois < 9) {
    
    # Juillet / août
    as.Date(
      sprintf("%d-09-01", annee)
    )
    
  } else if (mois < 12) {
    
    # Octobre / novembre
    as.Date(
      sprintf("%d-12-01", annee)
    )
    
  } else {
    
    # Décembre -> mars de l'année suivante
    as.Date(
      sprintf("%d-03-01", annee + 1)
    )
  }
}


# ============================================================
# 4. FONCTION : CALCULER L'HORIZON
# ============================================================

calculer_horizon <- function(
    date_derniere,
    date_cible
) {
  
  annee_diff <-
    as.integer(format(date_cible, "%Y")) -
    as.integer(format(date_derniere, "%Y"))
  
  mois_diff <-
    as.integer(format(date_cible, "%m")) -
    as.integer(format(date_derniere, "%m"))
  
  h <-
    12 * annee_diff +
    mois_diff
  
  max(1, h)
}


# ============================================================
# 5. FONCTION : CONVERSION ΔLOG -> CROISSANCE %
# ============================================================
#
# Le DFM (et l'AR) prévoit :
#
#   Δlog(VA)
#
# On convertit ensuite :
#
#   100 * (exp(Δlog(VA)) - 1)
#
# Exemple :
#
#   Δlog(VA) = 0.04237
#
#   croissance ≈ +4.33 %
#
# ============================================================

convertir_croissance <- function(x) {
  
  ifelse(
    is.na(x),
    NA_real_,
    100 * (exp(x) - 1)
  )
}


# ============================================================
# 6. FONCTION : EXTRAIRE LE NOWCAST DFM
# ============================================================

extraire_nowcast <- function(
    secteur,
    panel,
    modele,
    methode = "qml"
) {
  
  # ----------------------------------------------------------
  # Vérification du modèle
  # ----------------------------------------------------------
  
  if (is.null(modele)) {
    return(NULL)
  }
  
  if (is.null(modele$model)) {
    return(NULL)
  }
  
  model <- modele$model
  
  
  # ----------------------------------------------------------
  # Identifier automatiquement la cible
  # ----------------------------------------------------------
  
  target_name <- attr(
    panel,
    "target"
  )
  
  if (is.null(target_name)) {
    
    warning(
      "Cible absente pour : ",
      secteur
    )
    
    return(NULL)
  }
  
  
  if (!(target_name %in% colnames(panel))) {
    
    warning(
      "Cible introuvable dans le panel : ",
      target_name
    )
    
    return(NULL)
  }
  
  
  # ----------------------------------------------------------
  # Dernière date du panel
  # ----------------------------------------------------------
  
  dates <- index(panel)
  
  date_derniere <- max(
    dates,
    na.rm = TRUE
  )
  
  
  # ----------------------------------------------------------
  # Déterminer le prochain trimestre
  # ----------------------------------------------------------
  
  date_cible <- prochain_fin_trimestre(
    date_derniere
  )
  
  
  # ----------------------------------------------------------
  # Horizon
  # ----------------------------------------------------------
  
  h <- calculer_horizon(
    date_derniere,
    date_cible
  )
  
  
  # ----------------------------------------------------------
  # Prévision DFM
  # ----------------------------------------------------------
  #
  # Le modèle a été estimé avec :
  #
  #   X = indicateurs mensuels + Δlog(VA)
  #
  # et :
  #
  #   quarterly.vars = cible
  #
  # La prévision extraite est donc une prévision
  # de Δlog(VA).
  #
  # ----------------------------------------------------------
  
  fc <- tryCatch(
    
    predict(
      model,
      h = h,
      method = methode,
      standardized = FALSE
    ),
    
    error = function(e) {
      
      warning(
        "Erreur predict() pour ",
        secteur,
        " : ",
        e$message
      )
      
      return(NULL)
    }
  )
  
  
  if (is.null(fc)) {
    return(NULL)
  }
  
  
  # ----------------------------------------------------------
  # Vérifier X_fcst
  # ----------------------------------------------------------
  
  if (is.null(fc$X_fcst)) {
    
    warning(
      "X_fcst absent pour : ",
      secteur
    )
    
    return(NULL)
  }
  
  
  # ----------------------------------------------------------
  # Identifier la colonne cible
  # ----------------------------------------------------------
  
  col_target <- which(
    colnames(fc$X_fcst) == target_name
  )
  
  if (length(col_target) != 1) {
    
    warning(
      "Impossible d'identifier la cible dans X_fcst pour : ",
      secteur
    )
    
    return(NULL)
  }
  
  
  # ----------------------------------------------------------
  # Prévision Δlog(VA)
  # ----------------------------------------------------------
  
  nowcast_dlog <- as.numeric(
    fc$X_fcst[
      h,
      col_target
    ]
  )
  
  
  # ----------------------------------------------------------
  # Conversion en taux de croissance %
  # ----------------------------------------------------------
  
  nowcast_pct <- convertir_croissance(
    nowcast_dlog
  )
  
  
  # ----------------------------------------------------------
  # Retourner les deux informations
  # ----------------------------------------------------------
  
  data.frame(
    
    secteur = secteur,
    
    date_derniere_observation =
      as.Date(date_derniere),
    
    date_nowcast =
      as.Date(date_cible),
    
    trimestre_nowcast =
      paste0(
        "T",
        ceiling(
          as.integer(
            format(
              date_cible,
              "%m"
            )
          ) / 3
        ),
        " ",
        format(
          date_cible,
          "%Y"
        )
      ),
    
    horizon_mois = h,
    
    cible = target_name,
    
    nowcast_dlog = nowcast_dlog,
    
    nowcast_pct = nowcast_pct,
    
    methode = methode,
    
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 7. BOUCLE SUR LES SECTEURS AVEC DFM
# ============================================================

resultats_nowcast <- list()

secteurs_modeles <- names(modeles_dfm)

cat("\n============================================================\n")
cat("CALCUL DES NOWCASTS (DFM)\n")
cat("============================================================\n")


for (secteur in secteurs_modeles) {
  
  cat("\n------------------------------------------------------------\n")
  cat("SECTEUR :", secteur, "\n")
  cat("------------------------------------------------------------\n")
  
  
  # ----------------------------------------------------------
  # Vérifier le panel
  # ----------------------------------------------------------
  
  if (!(secteur %in% names(panels_stationnaires_filtres))) {
    
    cat(
      "Panel absent -> secteur ignoré.\n"
    )
    
    next
  }
  
  
  panel <- panels_stationnaires_filtres[[secteur]]
  
  modele <- modeles_dfm[[secteur]]
  
  
  # ----------------------------------------------------------
  # Informations
  # ----------------------------------------------------------
  
  target_name <- attr(
    panel,
    "target"
  )
  
  cat(
    "Cible :",
    target_name,
    "\n"
  )
  
  cat(
    "Dernière date :",
    as.character(
      max(index(panel))
    ),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # Calcul
  # ----------------------------------------------------------
  
  res <- extraire_nowcast(
    
    secteur = secteur,
    
    panel = panel,
    
    modele = modele,
    
    methode = "qml"
    
  )
  
  
  # ----------------------------------------------------------
  # Stockage
  # ----------------------------------------------------------
  
  if (!is.null(res)) {
    
    resultats_nowcast[[secteur]] <- res
    
    
    cat(
      "Date du nowcast :",
      as.character(
        res$date_nowcast
      ),
      "\n"
    )
    
    
    cat(
      "Trimestre :",
      res$trimestre_nowcast,
      "\n"
    )
    
    
    cat(
      "Horizon :",
      res$horizon_mois,
      "mois\n"
    )
    
    
    cat(
      "Prévision Δlog(VA) :",
      round(
        res$nowcast_dlog,
        6
      ),
      "\n"
    )
    
    
    cat(
      "Croissance prévue :",
      round(
        res$nowcast_pct,
        2
      ),
      "%\n"
    )
    
    
  } else {
    
    cat(
      "Pas de DFM exploitable -> sera basculé sur AR (voir section 7 BIS).\n"
    )
  }
}


# ============================================================
# 7 BIS. NOWCAST AR — TOUS LES SECTEURS SANS DFM EXPLOITABLE
# ============================================================
#
# Tout secteur pour lequel aucun nowcast DFM n'a été produit ci-dessus
# (parce qu'il n'a aucun indicateur mensuel exploitable -- que ce soit
# un secteur "cible seule" comme Administration_publique, Autres_services,
# Education-sante, Services_aux_entreprises, ou un secteur comme
# Information_communication qui n'a que des indicateurs trimestriels,
# volontairement exclus du DFM cf. 08) est basculé automatiquement sur un
# simple AR(p) appliqué directement à sa cible déjà transformée en
# Δlog(VA) (04_transformation.R impose cette transformation à TOUTE
# cible, DFM ou non -- donc rien à refaire ici : ni log(), ni diff()).
#
# La détection est DYNAMIQUE (pas de liste de secteurs codée en dur) :
# un secteur bascule vers l'AR dès lors qu'il ne figure pas parmi les
# nowcasts DFM déjà produits dans la boucle ci-dessus. Ça couvre donc
# automatiquement Information_communication ET les 4 nouveaux secteurs,
# et continuera de fonctionner si d'autres secteurs sans indicateur sont
# ajoutés plus tard, sans qu'il faille retoucher ce fichier.
# ============================================================

nowcast_ar_secteur <- function(secteur_ar, panels_stationnaires_filtres) {
  
  if (!(secteur_ar %in% names(panels_stationnaires_filtres))) {
    cat("\n", secteur_ar, ": panel absent -> ignoré pour l'AR.\n")
    return(NULL)
  }
  
  cat("\n============================================================\n")
  cat("NOWCAST AR —", secteur_ar, "\n")
  cat("============================================================\n")
  
  # ----------------------------------------------------------
  # 1. Récupération du panel
  # ----------------------------------------------------------
  
  panel_ar <- panels_stationnaires_filtres[[secteur_ar]]
  
  target_ar <- attr(
    panel_ar,
    "target"
  )
  
  if (is.null(target_ar) || !(target_ar %in% colnames(panel_ar))) {
    cat("Cible introuvable pour", secteur_ar, "-> ignoré.\n")
    return(NULL)
  }
  
  cat(
    "Cible :",
    target_ar,
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # 2. Récupération directe de Δlog(VA)
  # ----------------------------------------------------------
  #
  # ATTENTION :
  # la série est déjà stationnaire (04_transformation.R impose
  # log_diff à TOUTE cible, DFM ou non).
  #
  # On ne fait PAS :
  #
  #   log()
  #   diff()
  #
  # ----------------------------------------------------------
  
  serie_ar <- panel_ar[, target_ar]
  
  dates_ar <- index(serie_ar)
  
  valeurs_ar <- as.numeric(
    serie_ar
  )
  
  
  # ----------------------------------------------------------
  # 3. Garder uniquement les observations disponibles
  # ----------------------------------------------------------
  
  idx_valides <- !is.na(valeurs_ar)
  
  valeurs_ar <- valeurs_ar[
    idx_valides
  ]
  
  dates_ar <- dates_ar[
    idx_valides
  ]
  
  
  cat(
    "Nombre d'observations trimestrielles :",
    length(valeurs_ar),
    "\n"
  )
  
  if (length(valeurs_ar) < 10) {
    cat(
      "Trop peu d'observations (<10) pour estimer un AR fiable -> ignoré.\n"
    )
    return(NULL)
  }
  
  cat(
    "Dernière observation de Δlog(VA) :",
    as.character(
      max(dates_ar)
    ),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # 4. Tester AR(1) à AR(6)
  # ----------------------------------------------------------
  
  max_p <- min(
    6,
    floor(length(valeurs_ar) / 5)
  )
  max_p <- max(max_p, 1)
  
  
  resultats_ar <- data.frame(
    
    p = integer(),
    
    AIC = numeric(),
    
    BIC = numeric(),
    
    stringsAsFactors = FALSE
    
  )
  
  
  for (p in 1:max_p) {
    
    modele_test <- tryCatch(
      
      arima(
        
        valeurs_ar,
        
        order = c(
          p,
          0,
          0
        ),
        
        include.mean = TRUE,
        
        method = "ML"
        
      ),
      
      error = function(e) {
        NULL
      }
      
    )
    
    
    if (!is.null(modele_test)) {
      
      resultats_ar <- rbind(
        
        resultats_ar,
        
        data.frame(
          
          p = p,
          
          AIC = AIC(
            modele_test
          ),
          
          BIC = BIC(
            modele_test
          )
          
        )
        
      )
    }
  }
  
  
  # ----------------------------------------------------------
  # 5. Afficher les critères
  # ----------------------------------------------------------
  
  cat("\nCritères des modèles AR :\n")
  
  print(
    resultats_ar
  )
  
  
  if (nrow(resultats_ar) == 0) {
    warning(
      "Aucun modèle AR n'a pu être estimé pour ",
      secteur_ar
    )
    return(NULL)
  }
  
  
  # ----------------------------------------------------------
  # 6. Sélection du meilleur AR
  # ----------------------------------------------------------
  
  p_opt <- resultats_ar$p[
    which.min(
      resultats_ar$AIC
    )
  ]
  
  
  cat(
    "\nModèle retenu : AR(",
    p_opt,
    ")\n",
    sep = ""
  )
  
  
  # ----------------------------------------------------------
  # 7. Estimation finale
  # ----------------------------------------------------------
  
  modele_ar_final <- arima(
    
    valeurs_ar,
    
    order = c(
      p_opt,
      0,
      0
    ),
    
    include.mean = TRUE,
    
    method = "ML"
    
  )
  
  
  # ----------------------------------------------------------
  # 8. Prévision du prochain trimestre
  # ----------------------------------------------------------
  
  prediction_ar <- predict(
    
    modele_ar_final,
    
    n.ahead = 1
    
  )
  
  
  # Prévision DIRECTE de Δlog(VA)
  
  nowcast_dlog_ar <- as.numeric(
    prediction_ar$pred[1]
  )
  
  
  # ----------------------------------------------------------
  # 9. Conversion Δlog(VA) -> croissance %
  # ----------------------------------------------------------
  
  nowcast_pct_ar <- convertir_croissance(
    nowcast_dlog_ar
  )
  
  
  # ----------------------------------------------------------
  # 10. Date de dernière observation
  # ----------------------------------------------------------
  
  date_derniere_ar <- max(
    dates_ar,
    na.rm = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 11. Prochain trimestre
  # ----------------------------------------------------------
  #
  # IMPORTANT :
  # Les dates des cibles trimestrielles sont stockées comme
  # le PREMIER JOUR du trimestre :
  #
  #   2025-01-01 = T1 2025
  #   2025-04-01 = T2 2025
  #   2025-07-01 = T3 2025
  #   2025-10-01 = T4 2025
  #
  # Donc, si la dernière observation est :
  #
  #   2026-01-01 = T1 2026
  #
  # le prochain trimestre est :
  #
  #   2026-04-01 = T2 2026
  #
  # On ajoute simplement 3 mois.
  # ----------------------------------------------------------
  
  date_cible_ar <- seq(
    from = as.Date(date_derniere_ar),
    by = "3 months",
    length.out = 2
  )[2]
  
  
  # ----------------------------------------------------------
  # Horizon
  # ----------------------------------------------------------
  
  horizon_ar <- calculer_horizon(
    date_derniere_ar,
    date_cible_ar
  )
  
  
  # ----------------------------------------------------------
  # Etiquette du trimestre
  # ----------------------------------------------------------
  #
  # Ici la date correspond au PREMIER mois du trimestre :
  #
  # janvier  -> T1
  # avril    -> T2
  # juillet  -> T3
  # octobre  -> T4
  #
  # ----------------------------------------------------------
  
  mois_cible <- as.integer(
    format(
      date_cible_ar,
      "%m"
    )
  )
  
  trimestre_ar <- paste0(
    "T",
    ((mois_cible - 1) %/% 3) + 1,
    " ",
    format(
      date_cible_ar,
      "%Y"
    )
  )
  
  # ----------------------------------------------------------
  # 12. Construire le résultat
  # ----------------------------------------------------------
  
  resultat_ar <- data.frame(
    
    secteur = secteur_ar,
    
    date_derniere_observation =
      as.Date(
        date_derniere_ar
      ),
    
    date_nowcast =
      as.Date(
        date_cible_ar
      ),
    
    trimestre_nowcast =
      trimestre_ar,
    
    horizon_mois =
      horizon_ar,
    
    cible =
      target_ar,
    
    nowcast_dlog =
      nowcast_dlog_ar,
    
    nowcast_pct =
      nowcast_pct_ar,
    
    methode =
      paste0(
        "AR(",
        p_opt,
        ")"
      ),
    
    stringsAsFactors = FALSE
    
  )
  
  
  # ----------------------------------------------------------
  # 13. Affichage
  # ----------------------------------------------------------
  
  cat("\n------------------------------------------------------------\n")
  
  cat(
    "SECTEUR :",
    secteur_ar,
    "\n"
  )
  
  cat(
    "Modèle : AR(",
    p_opt,
    ")\n",
    sep = ""
  )
  
  cat(
    "Dernière observation :",
    as.character(
      date_derniere_ar
    ),
    "\n"
  )
  
  cat(
    "Trimestre prévu :",
    trimestre_ar,
    "\n"
  )
  
  cat(
    "Prévision Δlog(VA) :",
    round(
      nowcast_dlog_ar,
      6
    ),
    "\n"
  )
  
  cat(
    "Croissance prévue :",
    round(
      nowcast_pct_ar,
      2
    ),
    "%\n"
  )
  
  cat(
    "------------------------------------------------------------\n"
  )
  
  resultat_ar
}


# --------------------------------------------------------------------------
# Détection dynamique des secteurs sans DFM exploitable : tout secteur du
# panel qui n'a pas déjà un nowcast DFM dans resultats_nowcast.
# --------------------------------------------------------------------------

secteurs_a_basculer_ar <- setdiff(
  names(panels_stationnaires_filtres),
  names(resultats_nowcast)
)

if (length(secteurs_a_basculer_ar) > 0) {
  cat("\n============================================================\n")
  cat("SECTEURS BASCULES VERS UN NOWCAST AR (pas de DFM exploitable) :\n")
  cat(paste(" -", secteurs_a_basculer_ar), sep = "\n")
  cat("\n============================================================\n")
} else {
  cat("\nAucun secteur à basculer vers l'AR : tous ont un DFM exploitable.\n")
}

for (secteur_ar in secteurs_a_basculer_ar) {
  
  res_ar <- nowcast_ar_secteur(
    secteur_ar,
    panels_stationnaires_filtres
  )
  
  if (!is.null(res_ar)) {
    resultats_nowcast[[secteur_ar]] <- res_ar
  }
}


# ============================================================
# 8. CONSOLIDER LES RESULTATS
# ============================================================

if (length(resultats_nowcast) > 0) {
  
  nowcasts <- bind_rows(
    resultats_nowcast
  )
  
} else {
  
  nowcasts <- data.frame()
}


# ============================================================
# 9. AFFICHAGE
# ============================================================

cat("\n============================================================\n")
cat("RESULTATS DES NOWCASTS (DFM + AR)\n")
cat("============================================================\n")


if (nrow(nowcasts) > 0) {
  
  print(nowcasts)
  
  cat(
    "\nRépartition par méthode :\n"
  )
  
  print(table(nowcasts$methode))
  
} else {
  
  cat(
    "Aucun nowcast disponible.\n"
  )
}


# ============================================================
# 10. SAUVEGARDE CSV
# ============================================================
#
# Le CSV contient :
#
#   nowcast_dlog = résultat brut du DFM ou de l'AR
#   nowcast_pct  = croissance trimestrielle en %
#   methode      = "qml" (DFM) ou "AR(p)"
#
# ============================================================

fichier_csv <- file.path(
  OUT_DIR,
  "nowcasts_sectoriels.csv"
)

write.csv(
  nowcasts,
  fichier_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


cat(
  "\nCSV sauvegardé :",
  fichier_csv,
  "\n"
)


# ============================================================
# 11. SAUVEGARDE RDS
# ============================================================

saveRDS(
  nowcasts,
  file.path(
    OUT_DIR,
    "nowcasts_sectoriels.rds"
  )
)


# ============================================================
# 12. FIGURES
# ============================================================
#
# Les figures représentent maintenant :
#
#   historique de Δlog(VA)
#              +
#   nowcast de Δlog(VA)
#
# afin de comparer les observations et la prévision
# sur la même échelle. Fonctionne identiquement pour un
# secteur DFM ou un secteur AR (même cible transformée).
#
# ============================================================

dir.create(
  FIG_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


for (secteur in nowcasts$secteur) {
  
  panel <- panels_stationnaires_filtres[[secteur]]
  
  target_name <- attr(
    panel,
    "target"
  )
  
  
  # ----------------------------------------------------------
  # Historique de la cible transformée
  # ----------------------------------------------------------
  
  dates <- index(panel)
  
  valeurs <- as.numeric(
    panel[, target_name]
  )
  
  
  df_obs <- data.frame(
    
    date = as.Date(dates),
    
    valeur = valeurs
    
  ) %>%
    
    filter(
      !is.na(valeur)
    )
  
  
  # ----------------------------------------------------------
  # Nowcast
  # ----------------------------------------------------------
  
  nc <- nowcasts %>%
    
    filter(
      .data$secteur == secteur
    )
  
  
  if (nrow(nc) == 0) {
    next
  }
  
  
  # ----------------------------------------------------------
  # Figure
  # ----------------------------------------------------------
  
  p <- ggplot(
    
    df_obs,
    
    aes(
      x = date,
      y = valeur
    )
    
  ) +
    
    geom_line(
      linewidth = 0.8
    ) +
    
    geom_point(
      size = 1.5
    ) +
    
    geom_point(
      
      data = data.frame(
        
        date = nc$date_nowcast,
        
        valeur = nc$nowcast_dlog
        
      ),
      
      aes(
        x = date,
        y = valeur
      ),
      
      size = 3
      
    ) +
    
    labs(
      
      title =
        paste(
          "Nowcast -",
          secteur
        ),
      
      subtitle =
        paste(
          "Méthode :", nc$methode, "| Cible : Δlog(VA) |",
          nc$trimestre_nowcast,
          "| croissance prévue :",
          round(
            nc$nowcast_pct,
            2
          ),
          "%"
        ),
      
      x = "Date",
      
      y = "Δlog(VA)"
      
    ) +
    
    theme_minimal()
  
  
  fichier_fig <- file.path(
    
    FIG_DIR,
    
    paste0(
      "nowcast_",
      secteur,
      ".png"
    )
    
  )
  
  
  ggsave(
    
    filename = fichier_fig,
    
    plot = p,
    
    width = 10,
    
    height = 6,
    
    dpi = 300
    
  )
}


# ============================================================
# 13. FIN
# ============================================================

cat("\n============================================================\n")
cat("NOWCASTING TERMINE\n")
cat("============================================================\n")

cat(
  "Nombre de secteurs nowcastés :",
  nrow(nowcasts),
  "\n"
)

cat(
  "  dont via DFM :",
  sum(nowcasts$methode == "qml"),
  "\n"
)

cat(
  "  dont via AR  :",
  sum(grepl("^AR\\(", nowcasts$methode)),
  "\n"
)

cat(
  "Résultats :",
  fichier_csv,
  "\n"
)

cat(
  "Figures :",
  FIG_DIR,
  "\n"
)