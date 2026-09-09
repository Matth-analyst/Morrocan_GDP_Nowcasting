
# ============================================================
# 09_nowcast.R
# NOWCASTING SECTORIEL DU PIB MAROCAIN
# ============================================================
#
# OBJECTIF :
# Produire le nowcast de la croissance trimestrielle
# de la valeur ajoutée pour chaque secteur dont un DFM
# a été estimé.
#
# CIBLE DU DFM :
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
#   trimestriel prévu en pourcentage.
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
# Le DFM prévoit :
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
# 6. FONCTION : EXTRAIRE LE NOWCAST
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
  #
  # nowcast_dlog = prévision brute du DFM
  # nowcast_pct  = croissance trimestrielle interprétable
  #
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
# 7. BOUCLE SUR LES SECTEURS
# ============================================================

resultats_nowcast <- list()

secteurs_modeles <- names(modeles_dfm)

cat("\n============================================================\n")
cat("CALCUL DES NOWCASTS\n")
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
      "Nowcast non disponible.\n"
    )
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
cat("RESULTATS DES NOWCASTS\n")
cat("============================================================\n")


if (nrow(nowcasts) > 0) {
  
  print(nowcasts)
  
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
#   nowcast_dlog = résultat brut du DFM
#   nowcast_pct  = croissance trimestrielle en %
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
# sur la même échelle.
#
# Pour une figure destinée au rapport, on pourra ensuite
# créer une figure dédiée en croissance (%).
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
          "Cible : Δlog(VA) |",
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
  "Résultats :",
  fichier_csv,
  "\n"
)

cat(
  "Figures :",
  FIG_DIR,
  "\n"
)


