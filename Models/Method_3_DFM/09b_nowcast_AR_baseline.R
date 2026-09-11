# ============================================================
# 09b_nowcast_AR_baseline.R
# BASELINE AR — NOWCASTING SECTORIEL DU PIB MAROCAIN
# ============================================================
#
# OBJECTIF :
# ------------------------------------------------------------
# Construire un modèle de référence (BASELINE) utilisant
# EXCLUSIVEMENT des modèles autorégressifs AR(p).
#
# IMPORTANT :
# ------------------------------------------------------------
# Ce fichier est totalement indépendant des modèles DFM.
#
# Pour CHAQUE secteur :
#
#   1. récupérer uniquement la cible Δlog(VA)
#   2. supprimer les NA
#   3. tester AR(1) à AR(6)
#   4. sélectionner p selon l'AIC
#   5. estimer l'AR(p*) final
#   6. prévoir la croissance du trimestre suivant
#   7. convertir Δlog(VA) en croissance %
#
# La baseline doit être comparable au modèle principal.
#
# MODÈLE ESTIMÉ :
#
#   y_t = c + φ1 y_{t-1} + ... + φp y_{t-p} + ε_t
#
# avec :
#
#   y_t = Δlog(VA_t)
#
# CONVERSION FINALE :
#
#   croissance (%) = 100 × [exp(Δlog(VA)) - 1]
#
#
# DATE DES CIBLES :
# ------------------------------------------------------------
# Les cibles trimestrielles utilisent le PREMIER JOUR
# du trimestre :
#
#   01/01/2026 -> T1 2026
#   01/04/2026 -> T2 2026
#   01/07/2026 -> T3 2026
#   01/10/2026 -> T4 2026
#
# Exemple :
#
#   dernière observation = 01/01/2026
#   prévision            = 01/04/2026
#   trimestre prévu      = T2 2026
#
#
# SORTIES :
# ------------------------------------------------------------
# 1. ar_baseline_sectoriel.csv
# 2. ar_baseline_sectoriel.rds
# 3. tableau des critères AIC/BIC
# 4. figures de contrôle pour chaque secteur
#
# La sortie principale contient notamment :
#
#   secteur
#   date_derniere_observation
#   date_nowcast
#   trimestre_nowcast
#   p_opt
#   nowcast_dlog
#   nowcast_pct
#   methode
#
# ============================================================


# ============================================================
# 1. CHARGEMENT
# ============================================================

source(
  file.path(
    PROJECT_DIR,
    "01_setup.R"
  )
)

library(zoo)
library(xts)
library(dplyr)
library(ggplot2)


# ============================================================
# 2. CHARGER UNIQUEMENT LES PANELS
# ============================================================
#
# IMPORTANT :
# Aucun modèle DFM n'est chargé.
#
# La baseline AR utilise uniquement :
#
#   panels_stationnaires_filtres.rds
#
# ============================================================

panels_stationnaires_filtres <- readRDS(
  file.path(
    OUT_DIR,
    "panels_stationnaires_filtres.rds"
  )
)


cat("\n==============================================\n")
cat(" GDPNow Maroc — BASELINE AR\n")
cat("==============================================\n")

cat(
  "Nombre de secteurs disponibles :",
  length(panels_stationnaires_filtres),
  "\n"
)


# ============================================================
# 3. FONCTION : CONVERSION ΔLOG -> CROISSANCE %
# ============================================================
#
# Le modèle AR prévoit directement :
#
#   Δlog(VA)
#
# Conversion :
#
#   100 × [exp(Δlog(VA)) - 1]
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
# 4. FONCTION : IDENTIFIER LE TRIMESTRE SUIVANT
# ============================================================
#
# Les dates sont au PREMIER JOUR du trimestre.
#
#   Janvier -> T1
#   Avril   -> T2
#   Juillet -> T3
#   Octobre -> T4
#
# Donc :
#
#   2026-01-01 -> 2026-04-01
#   2026-04-01 -> 2026-07-01
#   2026-07-01 -> 2026-10-01
#   2026-10-01 -> 2027-01-01
#
# ============================================================

prochain_trimestre <- function(date_derniere) {
  
  seq(
    from = as.Date(date_derniere),
    by = "3 months",
    length.out = 2
  )[2]
}


# ============================================================
# 5. FONCTION : ETIQUETTE DU TRIMESTRE
# ============================================================

identifier_trimestre <- function(date) {
  
  mois <- as.integer(
    format(
      date,
      "%m"
    )
  )
  
  trimestre <- ((mois - 1) %/% 3) + 1
  
  paste0(
    "T",
    trimestre,
    " ",
    format(
      date,
      "%Y"
    )
  )
}


# ============================================================
# 6. FONCTION : ESTIMATION DE LA BASELINE AR
# ============================================================
#
# Cette fonction est appliquée indépendamment à chaque secteur.
#
# Elle ne regarde :
#
#   - ni les indicateurs mensuels
#   - ni les facteurs
#   - ni le DFM
#   - ni les modèles précédemment estimés
#
# Elle utilise uniquement :
#
#   cible = Δlog(VA)
#
# ============================================================

estimer_ar_baseline <- function(
    secteur,
    panel
) {
  
  
  cat("\n============================================================\n")
  cat("BASELINE AR —", secteur, "\n")
  cat("============================================================\n")
  
  
  # ==========================================================
  # 6.1 Identifier la cible
  # ==========================================================
  
  target_name <- attr(
    panel,
    "target"
  )
  
  
  if (
    is.null(target_name) ||
    !(target_name %in% colnames(panel))
  ) {
    
    cat(
      "Cible introuvable -> secteur ignoré.\n"
    )
    
    return(NULL)
  }
  
  
  cat(
    "Cible :",
    target_name,
    "\n"
  )
  
  
  # ==========================================================
  # 6.2 Récupérer la série Δlog(VA)
  # ==========================================================
  #
  # ATTENTION :
  #
  # La transformation a déjà été effectuée dans
  # 04_transformation.R.
  #
  # On NE REFAIT PAS :
  #
  #   log()
  #   diff()
  #
  # ==========================================================
  
  serie <- panel[, target_name]
  
  dates <- index(
    serie
  )
  
  valeurs <- as.numeric(
    serie
  )
  
  
  # ==========================================================
  # 6.3 Garder uniquement les observations disponibles
  # ==========================================================
  
  idx_valides <- is.finite(
    valeurs
  )
  
  valeurs <- valeurs[
    idx_valides
  ]
  
  dates <- dates[
    idx_valides
  ]
  
  
  n_obs <- length(
    valeurs
  )
  
  
  cat(
    "Nombre d'observations trimestrielles :",
    n_obs,
    "\n"
  )
  
  
  # ==========================================================
  # 6.4 Vérification du nombre d'observations
  # ==========================================================
  
  if (n_obs < 10) {
    
    cat(
      "Trop peu d'observations (<10) -> secteur ignoré.\n"
    )
    
    return(NULL)
  }
  
  
  date_derniere <- max(
    dates,
    na.rm = TRUE
  )
  
  
  cat(
    "Dernière observation de Δlog(VA) :",
    as.character(
      date_derniere
    ),
    "\n"
  )
  
  
  # ==========================================================
  # 6.5 Tester AR(1) à AR(6)
  # ==========================================================
  #
  # Pour éviter d'estimer un modèle trop complexe avec peu
  # d'observations :
  #
  #   p <= n / 5
  #
  # tout en autorisant au maximum AR(6).
  #
  # ==========================================================
  
  max_p <- min(
    6,
    floor(
      n_obs / 5
    )
  )
  
  max_p <- max(
    max_p,
    1
  )
  
  
  cat(
    "Ordre maximal testé : AR(",
    max_p,
    ")\n",
    sep = ""
  )
  
  
  resultats_ar <- data.frame(
    
    secteur = character(),
    
    p = integer(),
    
    AIC = numeric(),
    
    BIC = numeric(),
    
    logLik = numeric(),
    
    stringsAsFactors = FALSE
    
  )
  
  
  modeles_testes <- list()
  
  
  for (p in 1:max_p) {
    
    
    modele_test <- tryCatch(
      
      arima(
        
        valeurs,
        
        order = c(
          p,
          0,
          0
        ),
        
        include.mean = TRUE,
        
        method = "ML"
        
      ),
      
      error = function(e) {
        
        cat(
          "AR(",
          p,
          ") impossible : ",
          e$message,
          "\n",
          sep = ""
        )
        
        NULL
      }
      
    )
    
    
    if (!is.null(modele_test)) {
      
      
      resultats_ar <- rbind(
        
        resultats_ar,
        
        data.frame(
          
          secteur = secteur,
          
          p = p,
          
          AIC = AIC(
            modele_test
          ),
          
          BIC = BIC(
            modele_test
          ),
          
          logLik = as.numeric(
            logLik(
              modele_test
            )
          ),
          
          stringsAsFactors = FALSE
          
        )
        
      )
      
      
      modeles_testes[[as.character(p)]] <-
        modele_test
    }
  }
  
  
  # ==========================================================
  # 6.6 Vérifier les modèles estimés
  # ==========================================================
  
  cat("\nCritères des modèles AR :\n")
  
  print(
    resultats_ar
  )
  
  
  if (
    nrow(resultats_ar) == 0
  ) {
    
    cat(
      "Aucun modèle AR exploitable -> secteur ignoré.\n"
    )
    
    return(NULL)
  }
  
  
  # ==========================================================
  # 6.7 Sélection du modèle par AIC
  # ==========================================================
  #
  # Le critère utilisé pour sélectionner p est l'AIC.
  #
  # Le BIC est conservé dans les résultats à titre
  # informatif et pour l'analyse ultérieure.
  #
  # ==========================================================
  
  ligne_opt <- which.min(
    resultats_ar$AIC
  )
  
  p_opt <- resultats_ar$p[
    ligne_opt
  ]
  
  
  cat(
    "\nModèle retenu selon AIC : AR(",
    p_opt,
    ")\n",
    sep = ""
  )
  
  
  # ==========================================================
  # 6.8 Récupérer / ré-estimer le modèle final
  # ==========================================================
  
  modele_final <- modeles_testes[[as.character(p_opt)]]
  
  
  # Sécurité : ré-estimation si nécessaire
  
  if (is.null(modele_final)) {
    
    modele_final <- arima(
      
      valeurs,
      
      order = c(
        p_opt,
        0,
        0
      ),
      
      include.mean = TRUE,
      
      method = "ML"
      
    )
  }
  
  
  # ==========================================================
  # 6.9 Prévoir le trimestre suivant
  # ==========================================================
  
  prediction <- predict(
    
    modele_final,
    
    n.ahead = 1
    
  )
  
  
  # Prévision de Δlog(VA)
  
  nowcast_dlog <- as.numeric(
    prediction$pred[1]
  )
  
  
  # ==========================================================
  # 6.10 Conversion en croissance %
  # ==========================================================
  
  nowcast_pct <- convertir_croissance(
    nowcast_dlog
  )
  
  
  # ==========================================================
  # 6.11 Date du prochain trimestre
  # ==========================================================
  
  date_nowcast <- prochain_trimestre(
    date_derniere
  )
  
  
  trimestre_nowcast <- identifier_trimestre(
    date_nowcast
  )
  
  
  # ==========================================================
  # 6.12 Construire le résultat
  # ==========================================================
  
  resultat <- data.frame(
    
    secteur = secteur,
    
    date_derniere_observation =
      as.Date(
        date_derniere
      ),
    
    date_nowcast =
      as.Date(
        date_nowcast
      ),
    
    trimestre_nowcast =
      trimestre_nowcast,
    
    n_observations =
      n_obs,
    
    p_opt =
      p_opt,
    
    AIC =
      resultats_ar$AIC[
        ligne_opt
      ],
    
    BIC =
      resultats_ar$BIC[
        ligne_opt
      ],
    
    nowcast_dlog =
      nowcast_dlog,
    
    nowcast_pct =
      nowcast_pct,
    
    methode =
      paste0(
        "AR(",
        p_opt,
        ")"
      ),
    
    stringsAsFactors = FALSE
    
  )
  
  
  # ==========================================================
  # 6.13 Affichage
  # ==========================================================
  
  cat("\n------------------------------------------------------------\n")
  
  cat(
    "SECTEUR :",
    secteur,
    "\n"
  )
  
  cat(
    "Modèle retenu : AR(",
    p_opt,
    ")\n",
    sep = ""
  )
  
  cat(
    "Dernière observation :",
    as.character(
      date_derniere
    ),
    "\n"
  )
  
  cat(
    "Trimestre prévu :",
    trimestre_nowcast,
    "\n"
  )
  
  cat(
    "Prévision Δlog(VA) :",
    round(
      nowcast_dlog,
      6
    ),
    "\n"
  )
  
  cat(
    "Croissance prévue :",
    round(
      nowcast_pct,
      2
    ),
    "%\n"
  )
  
  cat(
    "------------------------------------------------------------\n"
  )
  
  
  # Retourner à la fois le résultat final et les critères
  
  list(
    
    resultat = resultat,
    
    criteres = resultats_ar,
    
    modele = modele_final
    
  )
}


# ============================================================
# 7. BOUCLE SUR TOUS LES SECTEURS
# ============================================================
#
# IMPORTANT :
#
# Ici on boucle sur TOUS les secteurs disponibles dans
# panels_stationnaires_filtres.
#
# Il n'y a AUCUNE distinction :
#
#   DFM
#   AR existant
#   secteur avec indicateurs
#   secteur sans indicateurs
#
# Tout le monde reçoit un AR.
#
# ============================================================

resultats_baseline <- list()

criteres_tous_secteurs <- list()

modeles_ar_baseline <- list()


cat("\n============================================================\n")
cat("ESTIMATION DE LA BASELINE AR POUR TOUS LES SECTEURS\n")
cat("============================================================\n")


for (
  secteur in names(
    panels_stationnaires_filtres
  )
) {
  
  
  panel <- panels_stationnaires_filtres[[secteur]]
  
  
  res <- estimer_ar_baseline(
    
    secteur = secteur,
    
    panel = panel
    
  )
  
  
  if (!is.null(res)) {
    
    
    resultats_baseline[[secteur]] <-
      res$resultat
    
    
    criteres_tous_secteurs[[secteur]] <-
      res$criteres
    
    
    modeles_ar_baseline[[secteur]] <-
      res$modele
  }
}


# ============================================================
# 8. CONSOLIDATION DES RESULTATS
# ============================================================

if (
  length(resultats_baseline) > 0
) {
  
  baseline_ar <- bind_rows(
    resultats_baseline
  )
  
} else {
  
  baseline_ar <- data.frame()
}


# ============================================================
# 9. CONSOLIDATION DES CRITERES
# ============================================================

if (
  length(criteres_tous_secteurs) > 0
) {
  
  criteres_ar <- bind_rows(
    criteres_tous_secteurs
  )
  
} else {
  
  criteres_ar <- data.frame()
}


# ============================================================
# 10. AFFICHAGE FINAL
# ============================================================

cat("\n\n")
cat("============================================================\n")
cat("RESULTATS FINAUX — BASELINE AR\n")
cat("============================================================\n")


if (
  nrow(baseline_ar) > 0
) {
  
  
  # Affichage compact des résultats importants
  
  print(
    baseline_ar %>%
      select(
        secteur,
        trimestre_nowcast,
        p_opt,
        nowcast_dlog,
        nowcast_pct,
        methode
      )
  )
  
  
  cat(
    "\nNombre de secteurs nowcastés par AR :",
    nrow(baseline_ar),
    "\n"
  )
  
  
} else {
  
  cat(
    "Aucun secteur n'a pu être nowcasté.\n"
  )
}


# ============================================================
# 11. VERIFICATION DU NOMBRE DE SECTEURS
# ============================================================

cat("\n============================================================\n")
cat("VERIFICATION\n")
cat("============================================================\n")

cat(
  "Secteurs disponibles :",
  length(
    panels_stationnaires_filtres
  ),
  "\n"
)

cat(
  "Secteurs AR estimés :",
  nrow(
    baseline_ar
  ),
  "\n"
)


if (
  nrow(baseline_ar) ==
  length(panels_stationnaires_filtres)
) {
  
  cat(
    "OK : un AR a été estimé pour chaque secteur.\n"
  )
  
} else {
  
  cat(
    "ATTENTION : certains secteurs n'ont pas pu être estimés.\n"
  )
  
  secteurs_absents <- setdiff(
    names(
      panels_stationnaires_filtres
    ),
    baseline_ar$secteur
  )
  
  cat(
    "Secteurs concernés :\n"
  )
  
  cat(
    paste(
      "-",
      secteurs_absents
    ),
    sep = "\n"
  )
}


# ============================================================
# 12. SAUVEGARDE DU TABLEAU PRINCIPAL
# ============================================================
#
# Fichier destiné notamment à être utilisé plus tard pour
# l'évaluation et le test de Diebold-Mariano.
#
# ============================================================

fichier_csv <- file.path(
  OUT_DIR,
  "ar_baseline_sectoriel.csv"
)


write.csv(
  
  baseline_ar,
  
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
# 13. SAUVEGARDE RDS
# ============================================================

saveRDS(
  
  baseline_ar,
  
  file.path(
    OUT_DIR,
    "ar_baseline_sectoriel.rds"
  )
)


# ============================================================
# 14. SAUVEGARDE DES CRITERES AIC / BIC
# ============================================================
#
# Ce fichier permet de vérifier après coup :
#
#   secteur | p | AIC | BIC | logLik
#
# et donc de documenter le choix de AR(p).
#
# ============================================================

fichier_criteres <- file.path(
  OUT_DIR,
  "ar_baseline_criteres.csv"
)


write.csv(
  
  criteres_ar,
  
  fichier_criteres,
  
  row.names = FALSE,
  
  fileEncoding = "UTF-8"
)


cat(
  "Critères AIC/BIC sauvegardés :",
  fichier_criteres,
  "\n"
)


# ============================================================
# 15. SAUVEGARDE DES MODELES AR
# ============================================================

saveRDS(
  
  modeles_ar_baseline,
  
  file.path(
    OUT_DIR,
    "modeles_ar_baseline.rds"
  )
)


cat(
  "Modèles AR sauvegardés :",
  file.path(
    OUT_DIR,
    "modeles_ar_baseline.rds"
  ),
  "\n"
)


# ============================================================
# 16. FIGURES DE CONTROLE
# ============================================================
#
# Pour chaque secteur :
#
#   historique Δlog(VA)
#          +
#   prévision AR
#
# La prévision est affichée au trimestre suivant la dernière
# observation disponible.
#
# ============================================================

FIG_DIR_AR <- file.path(
  OUT_DIR,
  "figures_AR_baseline"
)


dir.create(
  
  FIG_DIR_AR,
  
  recursive = TRUE,
  
  showWarnings = FALSE
  
)


if (
  nrow(baseline_ar) > 0
) {
  
  
  for (
    secteur in baseline_ar$secteur
  ) {
    
    
    panel <- panels_stationnaires_filtres[[secteur]]
    
    
    target_name <- attr(
      panel,
      "target"
    )
    
    
    if (
      is.null(target_name) ||
      !(target_name %in% colnames(panel))
    ) {
      next
    }
    
    
    # --------------------------------------------------------
    # Historique Δlog(VA)
    # --------------------------------------------------------
    
    dates <- as.Date(
      index(panel)
    )
    
    valeurs <- as.numeric(
      panel[, target_name]
    )
    
    
    df_obs <- data.frame(
      
      date = dates,
      
      valeur = valeurs
      
    ) %>%
      
      filter(
        is.finite(valeur)
      )
    
    
    # --------------------------------------------------------
    # Résultat AR
    # --------------------------------------------------------
    
    nc <- baseline_ar %>%
      
      filter(
        .data$secteur == secteur
      )
    
    
    if (
      nrow(nc) == 0
    ) {
      next
    }
    
    
    # --------------------------------------------------------
    # Figure
    # --------------------------------------------------------
    
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
            "Baseline AR -",
            secteur
          ),
        
        subtitle =
          paste(
            "Modèle :",
            nc$methode,
            "| Cible : Δlog(VA) |",
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
      
      FIG_DIR_AR,
      
      paste0(
        "AR_baseline_",
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
}


# ============================================================
# 17. TABLEAU FINAL COMPACT
# ============================================================

cat("\n============================================================\n")
cat("TABLEAU FINAL DE LA BASELINE AR\n")
cat("============================================================\n")


if (
  nrow(baseline_ar) > 0
) {
  
  
  tableau_final <- baseline_ar %>%
    
    select(
      
      secteur,
      
      date_derniere_observation,
      
      date_nowcast,
      
      trimestre_nowcast,
      
      n_observations,
      
      p_opt,
      
      AIC,
      
      BIC,
      
      nowcast_dlog,
      
      nowcast_pct,
      
      methode
      
    )
  
  
  print(
    tableau_final
  )
}


# ============================================================
# 18. FIN
# ============================================================

cat("\n============================================================\n")
cat("BASELINE AR TERMINEE\n")
cat("============================================================\n")

cat(
  "Nombre de secteurs :",
  nrow(
    baseline_ar
  ),
  "\n"
)

cat(
  "Résultats :",
  fichier_csv,
  "\n"
)

cat(
  "Critères :",
  fichier_criteres,
  "\n"
)

cat(
  "Modèles :",
  file.path(
    OUT_DIR,
    "modeles_ar_baseline.rds"
  ),
  "\n"
)

cat(
  "Figures :",
  FIG_DIR_AR,
  "\n"
)

cat("============================================================\n")