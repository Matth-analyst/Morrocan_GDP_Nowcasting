# ============================================================================
# GDPNow Maroc — 10_evaluation.R
# ============================================================================
#
# OBJECTIF
# --------
# Evaluer hors-échantillon les performances des modèles de nowcasting
# sectoriels :
#
#   1. Modèle principal :
#        - DFM pour les secteurs disposant d'indicateurs mensuels
#        - AR pour les secteurs sans indicateurs mensuels
#
#   2. Modèle benchmark :
#        - AR uniquement pour les 16 secteurs
#
# L'évaluation repose sur un BACKTEST ROLLING :
#
#   historique disponible jusqu'à t
#                ↓
#          estimation modèle
#                ↓
#        prévision de t+1
#                ↓
#      comparaison avec y(t+1)
#
# IMPORTANT
# ---------
# La cible est déjà transformée dans le pipeline :
#
#        Δlog(VA)
#
# Les erreurs sont calculées sur cette même échelle.
#
# Les taux de croissance sont ensuite calculés pour interprétation :
#
#        100 * (exp(Δlog(VA)) - 1)
#
# SPECIFICATION DFM
# -----------------
# Les nombres de facteurs r et de retards p utilisés sont ceux du modèle
# final :
#
#   selection_facteurs.rds
#   selection_retards_resume.csv
#
# Ils sont FIXES pendant le backtest.
#
# En revanche, les paramètres du DFM sont ré-estimés à chaque origine
# avec uniquement les observations disponibles à cette date.
#
# AR
# --
# A chaque origine :
#
#   - AR(1) à AR(pmax) sont testés
#   - p est sélectionné par AIC
#   - le modèle retenu est réestimé
#   - une prévision à un trimestre est produite
#
# METRIQUES
# ---------
#   RMSE / RMSFE
#   MAE
#   ME / Bias
#   Diebold-Mariano
#
# SORTIES
# -------
#   evaluation_predictions_detail.csv
#   evaluation_resume_sectoriel.csv
#   evaluation_dm.csv
#   evaluation_predictions_detail.rds
#   evaluation_resume_sectoriel.rds
#   evaluation_dm.rds
#
#   figures_evaluation/
#       RMSE_DFM_vs_AR.png
#       MAE_DFM_vs_AR.png
#       comparaison_<secteur>.png
#
# ============================================================================


# ============================================================================
# 0. INITIALISATION
# ============================================================================

if (!exists("PROJECT_DIR")) {
  
  source(
    "~/2A.ESBD/GDPNow_Maroc_donnees_nettoyees/01_setup.R"
  )
}


message(
  "\n============================================================"
)

message(
  "GDPNow Maroc — EVALUATION HORS-ECHANTILLON"
)

message(
  "============================================================"
)


# ============================================================================
# 1. PARAMETRES DU BACKTEST
# ============================================================================
#
# INITIAL_TRAIN :
# nombre minimum d'observations trimestrielles nécessaires avant de
# commencer les prévisions.
#
# Avec environ 112 observations trimestrielles, 60 donne un historique
# suffisamment long tout en conservant un nombre raisonnable de prévisions.
#
# Exemple :
#
#   60 observations d'apprentissage
#   -> prévision observation 61
#
# puis :
#
#   61 observations d'apprentissage
#   -> prévision observation 62
#
# etc.
#
# ============================================================================

INITIAL_TRAIN <- 60

# Nombre maximal de retards pour le benchmark AR.
# Cohérent avec 09b.
MAX_AR_LAG <- 6

# Horizon de prévision :
#
# h = 1 trimestre
#
HORIZON <- 1


# ============================================================================
# 2. FICHIERS D'ENTREE
# ============================================================================

fichier_panel <- file.path(
  OUT_DIR,
  "panels_stationnaires_filtres.rds"
)

fichier_modeles <- file.path(
  OUT_DIR,
  "modeles_dfm.rds"
)

fichier_r <- file.path(
  OUT_DIR,
  "selection_facteurs.rds"
)

fichier_p <- file.path(
  OUT_DIR,
  "selection_retards_resume.csv"
)


# ============================================================================
# 3. VERIFICATION DES FICHIERS
# ============================================================================

fichiers_requis <- c(
  fichier_panel,
  fichier_modeles,
  fichier_r,
  fichier_p
)

for (f in fichiers_requis) {
  
  if (!file.exists(f)) {
    
    stop(
      "\nFichier requis introuvable :\n",
      f
    )
  }
}


# ============================================================================
# 4. CHARGEMENT DES DONNEES
# ============================================================================

panels_stationnaires_filtres <-
  readRDS(
    fichier_panel
  )

modeles_dfm <-
  readRDS(
    fichier_modeles
  )

selection_facteurs <-
  readRDS(
    fichier_r
  )

selection_retards <-
  read.csv(
    fichier_p,
    stringsAsFactors = FALSE
  )


message(
  "\nPanel chargé."
)

message(
  "Modèles DFM chargés."
)

message(
  "Sélections r chargées."
)

message(
  "Sélections p chargées."
)


# ============================================================================
# 5. CONSTRUCTION DES PARAMETRES r ET p
# ============================================================================

r_final <- sapply(
  
  selection_facteurs,
  
  function(x) {
    
    if (is.null(x$r_final)) {
      return(NA_real_)
    }
    
    as.numeric(
      x$r_final
    )
  }
)

names(r_final) <-
  names(
    selection_facteurs
  )


p_final <- setNames(
  
  as.numeric(
    selection_retards$p_final
  ),
  
  selection_retards$secteur
)


# ============================================================================
# 6. SECTEURS
# ============================================================================

secteurs <-
  names(
    panels_stationnaires_filtres
  )


message(
  "\nNombre total de secteurs : ",
  length(secteurs)
)


# ============================================================================
# 7. FONCTION : IDENTIFICATION DU TARGET
# ============================================================================

get_target_name <- function(panel) {
  
  target_name <-
    attr(
      panel,
      "target"
    )
  
  if (
    is.null(target_name) ||
    length(target_name) == 0 ||
    !target_name %in% colnames(panel)
  ) {
    
    stop(
      "Cible trimestrielle introuvable dans le panel."
    )
  }
  
  target_name
}


# ============================================================================
# 8. FONCTION : RECUPERATION DES DATES
# ============================================================================
#
# Les panels issus du pipeline peuvent être stockés sous différentes formes :
#
#   - zoo
#   - xts
#   - data.frame avec index
#   - data.frame avec colonne Date
#   - data.frame avec dates dans les rownames
#
# On teste donc plusieurs possibilités dans un ordre robuste.
#
# ============================================================================

get_panel_dates <- function(panel) {
  
  
  # --------------------------------------------------------------------------
  # 8.1 Cas zoo / xts
  # --------------------------------------------------------------------------
  
  dates <- tryCatch(
    
    zoo::index(
      panel
    ),
    
    error = function(e)
      NULL
  )
  
  
  if (
    !is.null(dates) &&
    length(dates) == nrow(panel)
  ) {
    
    dates <- as.Date(
      dates
    )
    
    if (
      any(!is.na(dates))
    ) {
      
      return(
        dates
      )
    }
  }
  
  
  # --------------------------------------------------------------------------
  # 8.2 Recherche d'un attribut "index"
  # --------------------------------------------------------------------------
  
  dates <- attr(
    panel,
    "index"
  )
  
  
  if (
    !is.null(dates) &&
    length(dates) == nrow(panel)
  ) {
    
    #
    # L'index peut être numérique dans les objets zoo.
    #
    if (
      is.numeric(dates)
    ) {
      
      dates <- tryCatch(
        
        as.Date(
          dates,
          origin = "1970-01-01"
        ),
        
        error = function(e)
          NULL
      )
      
    } else {
      
      dates <- tryCatch(
        
        as.Date(
          dates
        ),
        
        error = function(e)
          NULL
      )
    }
    
    
    if (
      !is.null(dates) &&
      any(
        !is.na(dates)
      )
    ) {
      
      return(
        dates
      )
    }
  }
  
  
  # --------------------------------------------------------------------------
  # 8.3 Recherche d'une colonne de date
  # --------------------------------------------------------------------------
  
  candidats <- c(
    "date",
    "Date",
    "DATE",
    "Date_trimestre",
    "date_trimestre"
  )
  
  
  for (
    v in candidats
  ) {
    
    if (
      v %in%
      colnames(
        panel
      )
    ) {
      
      dates <- tryCatch(
        
        as.Date(
          panel[[v]]
        ),
        
        error = function(e)
          NULL
      )
      
      
      if (
        !is.null(dates) &&
        length(dates) == nrow(panel) &&
        any(
          !is.na(dates)
        )
      ) {
        
        return(
          dates
        )
      }
    }
  }
  
  
  # --------------------------------------------------------------------------
  # 8.4 Recherche dans les rownames
  # --------------------------------------------------------------------------
  
  rn <-
    rownames(
      panel
    )
  
  
  if (
    !is.null(rn) &&
    length(rn) == nrow(panel)
  ) {
    
    dates <- tryCatch(
      
      as.Date(
        rn
      ),
      
      error = function(e)
        NULL
    )
    
    
    if (
      !is.null(dates) &&
      any(
        !is.na(dates)
      )
    ) {
      
      return(
        dates
      )
    }
  }
  
  
  # --------------------------------------------------------------------------
  # 8.5 Diagnostic détaillé si aucune méthode ne fonctionne
  # --------------------------------------------------------------------------
  
  message(
    "\n--- DIAGNOSTIC STRUCTURE DU PANEL ---"
  )
  
  message(
    "Classe : ",
    paste(
      class(panel),
      collapse = ", "
    )
  )
  
  message(
    "Nombre de lignes : ",
    nrow(panel)
  )
  
  message(
    "Attributs : ",
    paste(
      names(
        attributes(panel)
      ),
      collapse = ", "
    )
  )
  
  message(
    "Colonnes : ",
    paste(
      colnames(panel),
      collapse = ", "
    )
  )
  
  message(
    "Rownames présents : ",
    !is.null(
      rownames(panel)
    )
  )
  
  
  stop(
    "Impossible d'identifier les dates du panel."
  )
}

# ============================================================================
# 9. FONCTION : PROCHAIN TRIMESTRE
# ============================================================================
#
# Convention du projet :
#
#   2026-01-01 = T1 2026
#   2026-04-01 = T2 2026
#   2026-07-01 = T3 2026
#   2026-10-01 = T4 2026
#
# ============================================================================

prochain_trimestre <- function(date) {
  
  date <-
    as.Date(date)
  
  mois <-
    as.integer(
      format(
        date,
        "%m"
      )
    )
  
  annee <-
    as.integer(
      format(
        date,
        "%Y"
      )
    )
  
  
  if (mois == 1) {
    
    return(
      as.Date(
        paste0(
          annee,
          "-04-01"
        )
      )
    )
  }
  
  
  if (mois == 4) {
    
    return(
      as.Date(
        paste0(
          annee,
          "-07-01"
        )
      )
    )
  }
  
  
  if (mois == 7) {
    
    return(
      as.Date(
        paste0(
          annee,
          "-10-01"
        )
      )
    )
  }
  
  
  if (mois == 10) {
    
    return(
      as.Date(
        paste0(
          annee + 1,
          "-01-01"
        )
      )
    )
  }
  
  
  # Sécurité si une date mensuelle est passée.
  
  date_debut_quarter <-
    as.Date(
      paste0(
        annee,
        "-",
        sprintf(
          "%02d",
          floor(
            (mois - 1) / 3
          ) * 3 + 1
        ),
        "-01"
      )
    )
  
  
  seq(
    date_debut_quarter,
    by = "quarter",
    length.out = 2
  )[2]
}


# ============================================================================
# 10. FONCTION : IDENTIFICATION DU TRIMESTRE
# ============================================================================

identifier_trimestre <- function(date) {
  
  date <-
    as.Date(date)
  
  mois <-
    as.integer(
      format(
        date,
        "%m"
      )
    )
  
  annee <-
    as.integer(
      format(
        date,
        "%Y"
      )
    )
  
  trimestre <-
    ceiling(
      mois / 3
    )
  
  paste0(
    annee,
    "T",
    trimestre
  )
}


# ============================================================================
# 11. FONCTION : SELECTION AR PAR AIC
# ============================================================================
#
# La sélection du retard est effectuée UNIQUEMENT avec les données
# disponibles avant l'origine du backtest.
#
# Cela évite une fuite d'information.
#
# ============================================================================

selection_ar_aic <- function(
    y,
    max_lag = 6
) {
  
  y <-
    as.numeric(y)
  
  y <-
    y[
      is.finite(y)
    ]
  
  
  n <-
    length(y)
  
  
  if (
    n < 15
  ) {
    
    return(
      list(
        p = NA_integer_,
        model = NULL,
        aic = NA_real_
      )
    )
  }
  
  
  p_max <-
    min(
      max_lag,
      floor(
        n / 5
      )
    )
  
  
  resultats <- list()
  
  
  for (p in 1:p_max) {
    
    modele <- tryCatch(
      
      arima(
        y,
        order = c(
          p,
          0,
          0
        ),
        include.mean = TRUE,
        method = "ML"
      ),
      
      error = function(e)
        NULL
    )
    
    
    if (
      !is.null(modele)
    ) {
      
      resultats[[as.character(p)]] <-
        modele
    }
  }
  
  
  if (
    length(resultats) == 0
  ) {
    
    return(
      list(
        p = NA_integer_,
        model = NULL,
        aic = NA_real_
      )
    )
  }
  
  
  aics <-
    sapply(
      resultats,
      AIC
    )
  
  
  p_best <-
    as.integer(
      names(
        aics
      )[
        which.min(
          aics
        )
      ]
    )
  
  
  list(
    
    p =
      p_best,
    
    model =
      resultats[[
        as.character(
          p_best
        )
      ]],
    
    aic =
      aics[
        as.character(
          p_best
        )
      ]
  )
}


# ============================================================================
# 12. FONCTION : PREVISION AR
# ============================================================================

forecast_ar <- function(
    y,
    max_lag = 6
) {
  
  selection <-
    selection_ar_aic(
      y =
        y,
      max_lag =
        max_lag
    )
  
  
  if (
    is.null(
      selection$model
    )
  ) {
    
    return(
      list(
        forecast = NA_real_,
        p = NA_integer_,
        aic = NA_real_
      )
    )
  }
  
  
  prediction <-
    tryCatch(
      
      predict(
        selection$model,
        n.ahead = 1
      )$pred[1],
      
      error = function(e)
        NA_real_
    )
  
  
  list(
    
    forecast =
      as.numeric(
        prediction
      ),
    
    p =
      selection$p,
    
    aic =
      selection$aic
  )
}


# ============================================================================
# 13. FONCTION : PREVISION DFM
# ============================================================================
#
# Cette fonction reproduit la logique du 08 :
#
#   X = variables mensuelles + cible trimestrielle
#
# Les autres variables trimestrielles sont exclues.
#
# Le modèle est réestimé avec les observations disponibles à l'origine.
#
# ============================================================================

forecast_dfm <- function(
    panel,
    target_name,
    monthly_vars,
    r,
    p,
    origin_index
) {
  
  
  # --------------------------------------------------------------------------
  # 13.1 Vérifications
  # --------------------------------------------------------------------------
  
  if (
    is.na(r) ||
    is.na(p) ||
    r < 1 ||
    p < 1
  ) {
    
    return(
      list(
        forecast = NA_real_,
        status = "invalid_r_p",
        convergence = NA,
        n_iter = NA,
        warning = NA
      )
    )
  }
  
  
  if (
    length(monthly_vars) == 0
  ) {
    
    return(
      list(
        forecast = NA_real_,
        status = "no_monthly_information",
        convergence = NA,
        n_iter = NA,
        warning = NA
      )
    )
  }
  
  
  if (
    length(monthly_vars) < r
  ) {
    
    return(
      list(
        forecast = NA_real_,
        status = "insufficient_monthly_series",
        convergence = NA,
        n_iter = NA,
        warning = NA
      )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # 13.2 Panel disponible à l'origine
  # --------------------------------------------------------------------------
  
  #
  # origin_index correspond au dernier trimestre observé au moment
  # où l'on souhaite produire la prévision.
  #
  # Pour la cible trimestrielle, toutes les observations jusqu'à origin_index
  # sont disponibles.
  #
  # Pour les indicateurs mensuels, le panel contient les observations
  # disponibles jusqu'à cette origine.
  #
  
  panel_train <-
    panel[
      1:origin_index,
      ,
      drop = FALSE
    ]
  
  
  variables_dfm <-
    unique(
      c(
        monthly_vars,
        target_name
      )
    )
  
  
  variables_dfm <-
    intersect(
      variables_dfm,
      colnames(
        panel_train
      )
    )
  
  
  if (
    !target_name %in% variables_dfm
  ) {
    
    return(
      list(
        forecast = NA_real_,
        status = "missing_target",
        convergence = NA,
        n_iter = NA,
        warning = NA
      )
    )
  }
  
  
  X <-
    as.matrix(
      panel_train[
        ,
        variables_dfm,
        drop = FALSE
      ]
    )
  
  
  # --------------------------------------------------------------------------
  # 13.3 Vérification de la cible
  # --------------------------------------------------------------------------
  
  y <-
    as.numeric(
      X[
        ,
        target_name
      ]
    )
  
  
  n_valid_y <-
    sum(
      is.finite(y)
    )
  
  
  #
  # Le DFM doit avoir suffisamment d'observations trimestrielles.
  #
  
  if (
    n_valid_y < 20
  ) {
    
    return(
      list(
        forecast = NA_real_,
        status = "insufficient_target_history",
        convergence = NA,
        n_iter = NA,
        warning = NA
      )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # 13.4 Estimation du DFM
  # --------------------------------------------------------------------------
  
  warnings_dfm <-
    character(0)
  
  
  modele <- tryCatch(
    
    withCallingHandlers(
      
      {
        
        dfms::DFM(
          
          X =
            X,
          
          r =
            r,
          
          p =
            p,
          
          quarterly.vars =
            target_name,
          
          em.method =
            "BM"
        )
      },
      
      warning = function(w) {
        
        warnings_dfm <<-
          c(
            warnings_dfm,
            conditionMessage(w)
          )
        
        invokeRestart(
          "muffleWarning"
        )
      }
    ),
    
    error = function(e) {
      
      attr(
        e,
        "dfm_error"
      ) <-
        TRUE
      
      e
    }
  )
  
  
  # --------------------------------------------------------------------------
  # 13.5 Erreur DFM
  # --------------------------------------------------------------------------
  
  if (
    inherits(
      modele,
      "error"
    )
  ) {
    
    return(
      list(
        forecast = NA_real_,
        status = "error",
        convergence = FALSE,
        n_iter = NA,
        warning = conditionMessage(
          modele
        )
      )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # 13.6 Prévision
  # --------------------------------------------------------------------------
  
  prediction <- tryCatch(
    
    {
      
      fcst <-
        predict(
          modele,
          h =
            HORIZON,
          method =
            "qml",
          standardized =
            FALSE
        )
      
      
      #
      # Dans dfms, X_fcst contient les prévisions des variables.
      #
      
      if (
        is.null(
          fcst$X_fcst
        )
      ) {
        
        stop(
          "X_fcst absent de l'objet de prévision."
        )
      }
      
      
      X_fcst <-
        fcst$X_fcst
      
      
      if (
        is.null(
          colnames(
            X_fcst
          )
        )
      ) {
        
        stop(
          "Impossible d'identifier la colonne cible dans X_fcst."
        )
      }
      
      
      if (
        !target_name %in%
        colnames(
          X_fcst
        )
      ) {
        
        stop(
          "Cible absente de X_fcst."
        )
      }
      
      
      as.numeric(
        X_fcst[
          1,
          target_name
        ]
      )
    },
    
    error = function(e) {
      
      NA_real_
    }
  )
  
  
  # --------------------------------------------------------------------------
  # 13.7 Nombre d'itérations
  # --------------------------------------------------------------------------
  
  n_iter <-
    NA
  
  champs_iter <- c(
    "n.iter",
    "n_iter",
    "iterations",
    "iter"
  )
  
  
  for (
    champ in champs_iter
  ) {
    
    if (
      !is.null(
        modele[[champ]]
      )
    ) {
      
      n_iter <-
        modele[[champ]][1]
      
      break
    }
  }
  
  
  # --------------------------------------------------------------------------
  # 13.8 Convergence
  # --------------------------------------------------------------------------
  
  convergence <-
    NA
  
  
  champs_convergence <- c(
    "converged",
    "convergence"
  )
  
  
  for (
    champ in champs_convergence
  ) {
    
    if (
      !is.null(
        modele[[champ]]
      )
    ) {
      
      valeur <-
        modele[[champ]]
      
      if (
        is.logical(
          valeur
        )
      ) {
        
        convergence <-
          valeur[1]
        
        break
      }
    }
  }
  
  
  if (
    is.na(
      convergence
    )
  ) {
    
    if (
      any(
        grepl(
          "Maximum number of iterations",
          warnings_dfm,
          ignore.case = TRUE
        )
      )
    ) {
      
      convergence <-
        FALSE
    }
  }
  
  
  # --------------------------------------------------------------------------
  # 13.9 Statut
  # --------------------------------------------------------------------------
  
  status <-
    if (
      is.na(
        prediction
      )
    )
      "forecast_error"
  else
    "estimated"
  
  
  list(
    
    forecast =
      prediction,
    
    status =
      status,
    
    convergence =
      convergence,
    
    n_iter =
      n_iter,
    
    warning =
      if (
        length(
          warnings_dfm
        ) > 0
      )
        paste(
          unique(
            warnings_dfm
          ),
          collapse = " || "
        )
    else
      NA
  )
}


# ============================================================================
# 14. FONCTION : CONVERSION DLOG -> CROISSANCE %
# ============================================================================

dlog_to_growth <- function(x) {
  
  ifelse(
    is.finite(x),
    100 * (
      exp(x) - 1
    ),
    NA_real_
  )
}


# ============================================================================
# 15. FONCTION : TEST DE DIEBOLD-MARIANO
# ============================================================================
#
# On compare :
#
#   loss_DFM = erreur_DFM²
#   loss_AR  = erreur_AR²
#
# d_t = loss_DFM - loss_AR
#
# H0 :
#
#   E[d_t] = 0
#
# Si p-value < 5 % :
#
#   différence statistiquement significative.
#
# Convention :
#
#   DM < 0 :
#       DFM possède une perte moyenne plus faible
#       -> DFM meilleur
#
#   DM > 0 :
#       AR possède une perte moyenne plus faible
#       -> AR meilleur
#
# Pour un horizon h = 1, l'autocorrélation des différences de pertes
# est généralement limitée. On utilise néanmoins une variance HAC
# simple afin de rester robuste.
#
# ============================================================================

dm_test <- function(
    e1,
    e2,
    h = 1
) {
  
  ok <-
    is.finite(e1) &
    is.finite(e2)
  
  
  e1 <-
    e1[ok]
  
  e2 <-
    e2[ok]
  
  
  n <-
    length(e1)
  
  
  if (
    n < 10
  ) {
    
    return(
      data.frame(
        n = n,
        dm_statistic = NA_real_,
        p_value = NA_real_,
        mean_loss_difference = NA_real_,
        interpretation = "Nombre d'observations insuffisant"
      )
    )
  }
  
  
  d <-
    e1^2 - e2^2
  
  
  d_bar <-
    mean(
      d
    )
  
  
  # --------------------------------------------------------------------------
  # Variance HAC
  # --------------------------------------------------------------------------
  
  #
  # Pour h=1, lag=0.
  #
  # Pour h>1, on prend h-1 retards.
  #
  
  lag_max <-
    max(
      0,
      h - 1
    )
  
  
  gamma0 <-
    mean(
      (
        d - d_bar
      )^2
    )
  
  
  long_run_var <-
    gamma0
  
  
  if (
    lag_max >= 1
  ) {
    
    for (
      j in 1:lag_max
    ) {
      
      gamma_j <-
        mean(
          (
            d[
              (j + 1):n
            ] -
              d_bar
          ) *
            (
              d[
                1:(n - j)
              ] -
                d_bar
            )
        )
      
      
      weight <-
        1 -
        j /
        (
          lag_max + 1
        )
      
      
      long_run_var <-
        long_run_var +
        2 *
        weight *
        gamma_j
    }
  }
  
  
  if (
    long_run_var <= 0 ||
    !is.finite(
      long_run_var
    )
  ) {
    
    return(
      data.frame(
        n = n,
        dm_statistic = NA_real_,
        p_value = NA_real_,
        mean_loss_difference = d_bar,
        interpretation = "Variance DM non exploitable"
      )
    )
  }
  
  
  dm_stat <-
    d_bar /
    sqrt(
      long_run_var /
        n
    )
  
  
  p_value <-
    2 *
    pnorm(
      -abs(
        dm_stat
      )
    )
  
  
  interpretation <-
    if (
      p_value < 0.05
    ) {
      
      if (
        dm_stat < 0
      )
        "DFM significativement meilleur"
      else
        "AR significativement meilleur"
      
    } else {
      
      "Pas de différence significative"
    }
  
  
  data.frame(
    
    n =
      n,
    
    dm_statistic =
      dm_stat,
    
    p_value =
      p_value,
    
    mean_loss_difference =
      d_bar,
    
    interpretation =
      interpretation
  )
}


# ============================================================================
# 16. FONCTIONS METRIQUES
# ============================================================================

calculer_metriques <- function(
    erreurs
) {
  
  erreurs <-
    erreurs[
      is.finite(
        erreurs
      )
    ]
  
  
  if (
    length(
      erreurs
    ) == 0
  ) {
    
    return(
      list(
        n = 0,
        rmse = NA_real_,
        rmsfe = NA_real_,
        mae = NA_real_,
        bias = NA_real_
      )
    )
  }
  
  
  list(
    
    n =
      length(
        erreurs
      ),
    
    rmse =
      sqrt(
        mean(
          erreurs^2
        )
      ),
    
    rmsfe =
      sqrt(
        mean(
          erreurs^2
        )
      ),
    
    mae =
      mean(
        abs(
          erreurs
        )
      ),
    
    bias =
      mean(
        erreurs
      )
  )
}


# ============================================================================
# 17. BACKTEST SECTORIEL
# ============================================================================

predictions_list <-
  list()


compteur_prediction <-
  0


for (
  s in secteurs
) {
  
  message(
    "\n============================================================"
  )
  
  message(
    "BACKTEST : ",
    s
  )
  
  message(
    "============================================================"
  )
  
  
  panel <-
    panels_stationnaires_filtres[[
      s
    ]]
  
  
  target_name <-
    get_target_name(
      panel
    )
  
  
  dates <-
    get_panel_dates(
      panel
    )
  
  
  #
  # Les observations du panel sont mensuelles.
  #
  # Nous devons donc travailler sur les dates correspondant aux cibles
  # trimestrielles.
  #
  
  y <-
    as.numeric(
      panel[
        ,
        target_name
      ]
    )
  
  
  #
  # Dates auxquelles la cible trimestrielle est observée.
  #
  
  dates_cible <-
    dates[
      is.finite(
        y
      )
    ]
  
  
  #
  # Indicateurs mensuels
  #
  
  monthly_vars <-
    attr(
      panel,
      "monthly_vars"
    )
  
  
  if (
    is.null(
      monthly_vars
    )
  ) {
    
    monthly_vars <-
      setdiff(
        colnames(
          panel
        ),
        target_name
      )
  }
  
  
  monthly_vars <-
    intersect(
      monthly_vars,
      colnames(
        panel
      )
    )
  
  
  #
  # Paramètres DFM
  #
  
  r_s <-
    if (
      s %in%
      names(
        r_final
      )
    )
      r_final[s]
  else
    NA
  
  
  p_s <-
    if (
      s %in%
      names(
        p_final
      )
    )
      p_final[s]
  else
    NA
  
  
  #
  # Le DFM est considéré disponible si le secteur possède des indicateurs
  # mensuels et une spécification r/p.
  #
  
  use_dfm <-
    length(
      monthly_vars
    ) > 0 &&
    !is.na(
      r_s
    ) &&
    !is.na(
      p_s
    )
  
  
  if (
    use_dfm
  ) {
    
    message(
      "Modèle principal : DFM"
    )
    
    message(
      "r = ",
      r_s,
      " | p = ",
      p_s
    )
    
  } else {
    
    message(
      "Modèle principal : AR"
    )
    
  }
  
  
  #
  # On identifie les lignes correspondant aux observations trimestrielles.
  #
  
  indices_cibles <-
    which(
      is.finite(
        y
      )
    )
  
  
  #
  # Nombre minimum d'observations nécessaires.
  #
  
  if (
    length(
      indices_cibles
    ) <= INITIAL_TRAIN
  ) {
    
    warning(
      s,
      " : pas assez d'observations pour le backtest."
    )
    
    next
  }
  
  
  #
  # On commence après INITIAL_TRAIN observations trimestrielles.
  #
  
  for (
    j in seq(
      INITIAL_TRAIN,
      length(
        indices_cibles
      ) - HORIZON
    )
  ) {
    
    
    # ------------------------------------------------------------------------
    # Origine
    # ------------------------------------------------------------------------
    
    origin_index <-
      indices_cibles[j]
    
    
    target_index <-
      indices_cibles[
        j + HORIZON
      ]
    
    
    origin_date <-
      dates[
        origin_index
      ]
    
    
    target_date <-
      dates[
        target_index
      ]
    
    
    #
    # Vérification :
    # la cible doit être exactement le trimestre suivant.
    #
    
    trimestre_origine <-
      identifier_trimestre(
        origin_date
      )
    
    trimestre_cible <-
      identifier_trimestre(
        target_date
      )
    
    
    #
    # Valeur réelle
    #
    
    actual_dlog <-
      y[
        target_index
      ]
    
    
    actual_pct <-
      dlog_to_growth(
        actual_dlog
      )
    
    
    # ------------------------------------------------------------------------
    # AR benchmark
    # ------------------------------------------------------------------------
    
    #
    # Pour l'AR, on utilise uniquement les cibles observées jusqu'à l'origine.
    #
    
    y_train_ar <-
      y[
        indices_cibles[
          1:j
        ]
      ]
    
    
    resultat_ar <-
      forecast_ar(
        y =
          y_train_ar,
        max_lag =
          MAX_AR_LAG
      )
    
    
    ar_dlog <-
      resultat_ar$forecast
    
    
    ar_pct <-
      dlog_to_growth(
        ar_dlog
      )
    
    
    # ------------------------------------------------------------------------
    # DFM
    # ------------------------------------------------------------------------
    
    if (
      use_dfm
    ) {
      
      resultat_dfm <-
        forecast_dfm(
          
          panel =
            panel,
          
          target_name =
            target_name,
          
          monthly_vars =
            monthly_vars,
          
          r =
            r_s,
          
          p =
            p_s,
          
          origin_index =
            origin_index
        )
      
      
      dfm_dlog <-
        resultat_dfm$forecast
      
      
      dfm_pct <-
        dlog_to_growth(
          dfm_dlog
        )
      
      dfm_status <-
        resultat_dfm$status
      
      dfm_convergence <-
        resultat_dfm$convergence
      
      dfm_n_iter <-
        resultat_dfm$n_iter
      
      dfm_warning <-
        resultat_dfm$warning
      
    } else {
      
      #
      # Pour les secteurs sans DFM :
      #
      # modèle principal = AR.
      #
      dfm_dlog <-
        ar_dlog
      
      dfm_pct <-
        ar_pct
      
      dfm_status <-
        "AR_fallback"
      
      dfm_convergence <-
        NA
      
      dfm_n_iter <-
        NA
      
      dfm_warning <-
        NA
    }
    
    
    # ------------------------------------------------------------------------
    # Erreurs
    # ------------------------------------------------------------------------
    
    erreur_dfm_dlog <-
      dfm_dlog -
      actual_dlog
    
    
    erreur_ar_dlog <-
      ar_dlog -
      actual_dlog
    
    
    erreur_dfm_pct <-
      dfm_pct -
      actual_pct
    
    
    erreur_ar_pct <-
      ar_pct -
      actual_pct
    
    
    compteur_prediction <-
      compteur_prediction + 1
    
    
    predictions_list[[
      compteur_prediction
    ]] <-
      
      data.frame(
        
        secteur =
          s,
        
        origine_date =
          origin_date,
        
        origine_trimestre =
          trimestre_origine,
        
        date_cible =
          target_date,
        
        trimestre_cible =
          trimestre_cible,
        
        horizon =
          HORIZON,
        
        actual_dlog =
          actual_dlog,
        
        actual_pct =
          actual_pct,
        
        dfm_dlog =
          dfm_dlog,
        
        dfm_pct =
          dfm_pct,
        
        ar_dlog =
          ar_dlog,
        
        ar_pct =
          ar_pct,
        
        erreur_dfm_dlog =
          erreur_dfm_dlog,
        
        erreur_ar_dlog =
          erreur_ar_dlog,
        
        erreur_dfm_pct =
          erreur_dfm_pct,
        
        erreur_ar_pct =
          erreur_ar_pct,
        
        modele_principal =
          if (
            use_dfm
          )
            "DFM"
        else
          "AR",
        
        dfm_status =
          dfm_status,
        
        dfm_convergence =
          dfm_convergence,
        
        dfm_n_iter =
          dfm_n_iter,
        
        dfm_warning =
          dfm_warning,
        
        ar_p =
          resultat_ar$p,
        
        ar_aic =
          resultat_ar$aic,
        
        r_dfm =
          if (
            use_dfm
          )
            r_s
        else
          NA,
        
        p_dfm =
          if (
            use_dfm
          )
            p_s
        else
          NA,
        
        stringsAsFactors =
          FALSE
      )
    
    
    #
    # Affichage périodique pour suivre l'avancement.
    #
    
    if (
      j == INITIAL_TRAIN ||
      j %% 10 == 0 ||
      j == length(
        indices_cibles
      ) - HORIZON
    ) {
      
      message(
        sprintf(
          "  %s -> %s | DFM = %s | AR = %s",
          trimestre_origine,
          trimestre_cible,
          ifelse(
            is.finite(
              dfm_pct
            ),
            sprintf(
              "%.2f%%",
              dfm_pct
            ),
            "NA"
          ),
          ifelse(
            is.finite(
              ar_pct
            ),
            sprintf(
              "%.2f%%",
              ar_pct
            ),
            "NA"
          )
        )
      )
    }
  }
}


# ============================================================================
# 18. ASSEMBLAGE DES PREDICTIONS
# ============================================================================

if (
  length(
    predictions_list
  ) == 0
) {
  
  stop(
    "Aucune prévision de backtest n'a été produite."
  )
}


evaluation_predictions <-
  dplyr::bind_rows(
    predictions_list
  )


message(
  "\nNombre total de prévisions produites : ",
  nrow(
    evaluation_predictions
  )
)


# ============================================================================
# 19. METRIQUES SECTORIELLES
# ============================================================================

resume_list <-
  list()


for (
  s in secteurs
) {
  
  df <-
    evaluation_predictions[
      evaluation_predictions$secteur == s,
      ,
      drop = FALSE
    ]
  
  
  if (
    nrow(df) == 0
  ) {
    
    next
  }
  
  
  #
  # Métriques DFM
  #
  
  met_dfm_dlog <-
    calculer_metriques(
      df$erreur_dfm_dlog
    )
  
  
  met_dfm_pct <-
    calculer_metriques(
      df$erreur_dfm_pct
    )
  
  
  #
  # Métriques AR
  #
  
  met_ar_dlog <-
    calculer_metriques(
      df$erreur_ar_dlog
    )
  
  
  met_ar_pct <-
    calculer_metriques(
      df$erreur_ar_pct
    )
  
  
  #
  # Gain relatif du DFM sur RMSE
  #
  
  gain_rmse <-
    if (
      is.finite(
        met_dfm_dlog$rmse
      ) &&
      is.finite(
        met_ar_dlog$rmse
      ) &&
      met_ar_dlog$rmse != 0
    ) {
      
      100 *
        (
          1 -
            met_dfm_dlog$rmse /
            met_ar_dlog$rmse
        )
      
    } else {
      
      NA_real_
    }
  
  
  #
  # Gain relatif du DFM sur MAE
  #
  
  gain_mae <-
    if (
      is.finite(
        met_dfm_dlog$mae
      ) &&
      is.finite(
        met_ar_dlog$mae
      ) &&
      met_ar_dlog$mae != 0
    ) {
      
      100 *
        (
          1 -
            met_dfm_dlog$mae /
            met_ar_dlog$mae
        )
      
    } else {
      
      NA_real_
    }
  
  
  #
  # Détermination du meilleur modèle selon RMSE
  #
  
  meilleur_modele <-
    if (
      !is.finite(
        met_dfm_dlog$rmse
      ) &&
      !is.finite(
        met_ar_dlog$rmse
      )
    ) {
      
      NA_character_
      
    } else if (
      !is.finite(
        met_dfm_dlog$rmse
      )
    ) {
      
      "AR"
      
    } else if (
      !is.finite(
        met_ar_dlog$rmse
      )
    ) {
      
      "DFM"
      
    } else if (
      met_dfm_dlog$rmse <
      met_ar_dlog$rmse
    ) {
      
      "DFM"
      
    } else if (
      met_dfm_dlog$rmse >
      met_ar_dlog$rmse
    ) {
      
      "AR"
      
    } else {
      
      "Egalité"
    }
  
  
  #
  # Le modèle principal est DFM ou AR fallback.
  #
  
  modele_principal <-
    unique(
      df$modele_principal
    )
  
  
  resume_list[[
    s
  ]] <-
    
    data.frame(
      
      secteur =
        s,
      
      modele_principal =
        paste(
          modele_principal,
          collapse = "/"
        ),
      
      n_predictions =
        nrow(
          df
        ),
      
      rmse_dfm_dlog =
        met_dfm_dlog$rmse,
      
      rmsfe_dfm_dlog =
        met_dfm_dlog$rmsfe,
      
      mae_dfm_dlog =
        met_dfm_dlog$mae,
      
      bias_dfm_dlog =
        met_dfm_dlog$bias,
      
      rmse_ar_dlog =
        met_ar_dlog$rmse,
      
      rmsfe_ar_dlog =
        met_ar_dlog$rmsfe,
      
      mae_ar_dlog =
        met_ar_dlog$mae,
      
      bias_ar_dlog =
        met_ar_dlog$bias,
      
      rmse_dfm_pct =
        met_dfm_pct$rmse,
      
      mae_dfm_pct =
        met_dfm_pct$mae,
      
      bias_dfm_pct =
        met_dfm_pct$bias,
      
      rmse_ar_pct =
        met_ar_pct$rmse,
      
      mae_ar_pct =
        met_ar_pct$mae,
      
      bias_ar_pct =
        met_ar_pct$bias,
      
      gain_rmse_dfm_pct =
        gain_rmse,
      
      gain_mae_dfm_pct =
        gain_mae,
      
      meilleur_modele_rmse =
        meilleur_modele,
      
      stringsAsFactors =
        FALSE
    )
}


evaluation_resume <-
  dplyr::bind_rows(
    resume_list
  )


# ============================================================================
# 20. TESTS DE DIEBOLD-MARIANO
# ============================================================================

dm_list <-
  list()


for (
  s in secteurs
) {
  
  df <-
    evaluation_predictions[
      evaluation_predictions$secteur == s,
      ,
      drop = FALSE
    ]
  
  
  if (
    nrow(df) == 0
  ) {
    
    next
  }
  
  
  modele_principal <-
    unique(
      df$modele_principal
    )
  
  
  #
  # Pour les secteurs AR fallback :
  #
  # DFM et AR sont exactement le même modèle.
  #
  
  if (
    all(
      modele_principal == "AR"
    )
  ) {
    
    dm_list[[
      s
    ]] <-
      
      data.frame(
        
        secteur =
          s,
        
        modele_1 =
          "AR_fallback",
        
        modele_2 =
          "AR_baseline",
        
        n =
          nrow(
            df
          ),
        
        dm_statistic =
          NA_real_,
        
        p_value =
          NA_real_,
        
        mean_loss_difference =
          0,
        
        interpretation =
          "Modèles identiques — DM non applicable",
        
        stringsAsFactors =
          FALSE
      )
    
    next
  }
  
  
  #
  # DFM vs AR
  #
  
  dm <-
    dm_test(
      
      e1 =
        df$erreur_dfm_dlog,
      
      e2 =
        df$erreur_ar_dlog,
      
      h =
        HORIZON
    )
  
  
  dm_list[[
    s
  ]] <-
    
    data.frame(
      
      secteur =
        s,
      
      modele_1 =
        "DFM",
      
      modele_2 =
        "AR_baseline",
      
      n =
        dm$n,
      
      dm_statistic =
        dm$dm_statistic,
      
      p_value =
        dm$p_value,
      
      mean_loss_difference =
        dm$mean_loss_difference,
      
      interpretation =
        dm$interpretation,
      
      stringsAsFactors =
        FALSE
    )
}


evaluation_dm <-
  dplyr::bind_rows(
    dm_list
  )


# ============================================================================
# 21. AJOUT DES RESULTATS DM AU RESUME
# ============================================================================

evaluation_resume <-
  evaluation_resume %>%
  
  dplyr::left_join(
    
    evaluation_dm %>%
      dplyr::select(
        secteur,
        dm_statistic,
        dm_p_value = p_value,
        dm_interpretation = interpretation
      ),
    
    by =
      "secteur"
  )


# ============================================================================
# 22. CONTROLE DES RESULTATS
# ============================================================================

message(
  "\n============================================================"
)

message(
  "RESUME DE L'EVALUATION"
)

message(
  "============================================================"
)

print(
  evaluation_resume
)


message(
  "\n============================================================"
)

message(
  "DIEBOLD-MARIANO"
)

message(
  "============================================================"
)

print(
  evaluation_dm
)


# ============================================================================
# 23. SAUVEGARDE DES PREDICTIONS DETAILLEES
# ============================================================================

write.csv(
  
  evaluation_predictions,
  
  file.path(
    OUT_DIR,
    "evaluation_predictions_detail.csv"
  ),
  
  row.names =
    FALSE
)


saveRDS(
  
  evaluation_predictions,
  
  file.path(
    OUT_DIR,
    "evaluation_predictions_detail.rds"
  )
)


# ============================================================================
# 24. SAUVEGARDE DU RESUME
# ============================================================================

write.csv(
  
  evaluation_resume,
  
  file.path(
    OUT_DIR,
    "evaluation_resume_sectoriel.csv"
  ),
  
  row.names =
    FALSE
)


saveRDS(
  
  evaluation_resume,
  
  file.path(
    OUT_DIR,
    "evaluation_resume_sectoriel.rds"
  )
)


# ============================================================================
# 25. SAUVEGARDE DU DM
# ============================================================================

write.csv(
  
  evaluation_dm,
  
  file.path(
    OUT_DIR,
    "evaluation_dm.csv"
  ),
  
  row.names =
    FALSE
)


saveRDS(
  
  evaluation_dm,
  
  file.path(
    OUT_DIR,
    "evaluation_dm.rds"
  )
)


# ============================================================================
# 26. CREATION DU DOSSIER FIGURES
# ============================================================================

FIG_EVAL_DIR <-
  file.path(
    OUT_DIR,
    "figures_evaluation"
  )


if (
  !dir.exists(
    FIG_EVAL_DIR
  )
) {
  
  dir.create(
    FIG_EVAL_DIR,
    recursive =
      TRUE
  )
}


# ============================================================================
# 27. FIGURE : RMSE DFM VS AR
# ============================================================================

df_plot_rmse <-
  evaluation_resume %>%
  
  dplyr::select(
    secteur,
    rmse_dfm_dlog,
    rmse_ar_dlog
  ) %>%
  
  tidyr::pivot_longer(
    cols =
      c(
        rmse_dfm_dlog,
        rmse_ar_dlog
      ),
    names_to =
      "modele",
    values_to =
      "RMSE"
  ) %>%
  
  dplyr::mutate(
    
    modele =
      dplyr::case_when(
        
        modele ==
          "rmse_dfm_dlog"
        ~
          "DFM",
        
        modele ==
          "rmse_ar_dlog"
        ~
          "AR",
        
        TRUE
        ~
          modele
      )
  )


g_rmse <-
  ggplot(
    
    df_plot_rmse,
    
    aes(
      x =
        reorder(
          secteur,
          RMSE
        ),
      y =
        RMSE,
      fill =
        modele
    )
  ) +
  
  geom_col(
    position =
      "dodge"
  ) +
  
  coord_flip() +
  
  labs(
    
    title =
      "RMSE hors-échantillon — DFM vs AR",
    
    x =
      NULL,
    
    y =
      "RMSE de Δlog(VA)",
    
    fill =
      "Modèle"
  ) +
  
  theme_minimal()


ggsave(
  
  file.path(
    FIG_EVAL_DIR,
    "RMSE_DFM_vs_AR.png"
  ),
  
  g_rmse,
  
  width =
    11,
  
  height =
    8,
  
  dpi =
    150
)


# ============================================================================
# 28. FIGURE : MAE DFM VS AR
# ============================================================================

df_plot_mae <-
  evaluation_resume %>%
  
  dplyr::select(
    secteur,
    mae_dfm_dlog,
    mae_ar_dlog
  ) %>%
  
  tidyr::pivot_longer(
    cols =
      c(
        mae_dfm_dlog,
        mae_ar_dlog
      ),
    names_to =
      "modele",
    values_to =
      "MAE"
  ) %>%
  
  dplyr::mutate(
    
    modele =
      dplyr::case_when(
        
        modele ==
          "mae_dfm_dlog"
        ~
          "DFM",
        
        modele ==
          "mae_ar_dlog"
        ~
          "AR",
        
        TRUE
        ~
          modele
      )
  )


g_mae <-
  ggplot(
    
    df_plot_mae,
    
    aes(
      x =
        reorder(
          secteur,
          MAE
        ),
      y =
        MAE,
      fill =
        modele
    )
  ) +
  
  geom_col(
    position =
      "dodge"
  ) +
  
  coord_flip() +
  
  labs(
    
    title =
      "MAE hors-échantillon — DFM vs AR",
    
    x =
      NULL,
    
    y =
      "MAE de Δlog(VA)",
    
    fill =
      "Modèle"
  ) +
  
  theme_minimal()


ggsave(
  
  file.path(
    FIG_EVAL_DIR,
    "MAE_DFM_vs_AR.png"
  ),
  
  g_mae,
  
  width =
    11,
  
  height =
    8,
  
  dpi =
    150
)


# ============================================================================
# 29. FIGURES SECTORIELLES : REALISE VS PREVISIONS
# ============================================================================

for (
  s in secteurs
) {
  
  df <-
    evaluation_predictions[
      evaluation_predictions$secteur == s,
      ,
      drop = FALSE
    ]
  
  
  if (
    nrow(df) == 0
  ) {
    
    next
  }
  
  
  df_plot <-
    df %>%
    
    dplyr::select(
      date_cible,
      actual_pct,
      dfm_pct,
      ar_pct
    ) %>%
    
    tidyr::pivot_longer(
      cols =
        c(
          actual_pct,
          dfm_pct,
          ar_pct
        ),
      names_to =
        "serie",
      values_to =
        "croissance"
    ) %>%
    
    dplyr::mutate(
      
      serie =
        dplyr::case_when(
          
          serie ==
            "actual_pct"
          ~
            "Réalisé",
          
          serie ==
            "dfm_pct"
          ~
            "DFM",
          
          serie ==
            "ar_pct"
          ~
            "AR",
          
          TRUE
          ~
            serie
        )
    )
  
  
  g <-
    ggplot(
      
      df_plot,
      
      aes(
        x =
          date_cible,
        y =
          croissance,
        linetype =
          serie
      )
    ) +
    
    geom_line(
      na.rm =
        TRUE
    ) +
    
    geom_point(
      na.rm =
        TRUE
    ) +
    
    labs(
      
      title =
        paste(
          "Backtest —",
          s
        ),
      
      subtitle =
        "Croissance trimestrielle de la VA : réalisé vs prévisions",
      
      x =
        NULL,
      
      y =
        "Croissance (%)",
      
      linetype =
        "Série"
    ) +
    
    theme_minimal()
  
  
  ggsave(
    
    file.path(
      FIG_EVAL_DIR,
      paste0(
        "comparaison_",
        s,
        ".png"
      )
    ),
    
    g,
    
    width =
      11,
    
    height =
      6,
    
    dpi =
      150
  )
}


# ============================================================================
# 30. STATISTIQUES GLOBALES
# ============================================================================

#
# ATTENTION :
#
# On ne moyenne pas directement les RMSE sectoriels.
# On calcule ici une statistique descriptive supplémentaire sur l'ensemble
# des prévisions disponibles.
#

erreurs_globales_dfm <-
  evaluation_predictions$erreur_dfm_dlog

erreurs_globales_ar <-
  evaluation_predictions$erreur_ar_dlog


metriques_globales_dfm <-
  calculer_metriques(
    erreurs_globales_dfm
  )


metriques_globales_ar <-
  calculer_metriques(
    erreurs_globales_ar
  )


message(
  "\n============================================================"
)

message(
  "PERFORMANCES GLOBALES"
)

message(
  "============================================================"
)

message(
  sprintf(
    "DFM : N = %d | RMSE = %.6f | MAE = %.6f | Bias = %.6f",
    metriques_globales_dfm$n,
    metriques_globales_dfm$rmse,
    metriques_globales_dfm$mae,
    metriques_globales_dfm$bias
  )
)

message(
  sprintf(
    "AR  : N = %d | RMSE = %.6f | MAE = %.6f | Bias = %.6f",
    metriques_globales_ar$n,
    metriques_globales_ar$rmse,
    metriques_globales_ar$mae,
    metriques_globales_ar$bias
  )
)


# ============================================================================
# 31. NOMBRE DE SECTEURS GAGNES
# ============================================================================

n_dfm_gagne <-
  sum(
    evaluation_resume$meilleur_modele_rmse ==
      "DFM",
    na.rm =
      TRUE
  )


n_ar_gagne <-
  sum(
    evaluation_resume$meilleur_modele_rmse ==
      "AR",
    na.rm =
      TRUE
  )


n_egalite <-
  sum(
    evaluation_resume$meilleur_modele_rmse ==
      "Egalité",
    na.rm =
      TRUE
  )


message(
  "\n============================================================"
)

message(
  "COMPARAISON PAR SECTEUR"
)

message(
  "============================================================"
)

message(
  "Secteurs où DFM gagne : ",
  n_dfm_gagne
)

message(
  "Secteurs où AR gagne  : ",
  n_ar_gagne
)

message(
  "Égalités              : ",
  n_egalite
)


# ============================================================================
# 32. DM SIGNIFICATIFS
# ============================================================================

dm_significatif <-
  evaluation_dm[
    is.finite(
      evaluation_dm$p_value
    ) &
      evaluation_dm$p_value < 0.05,
    ,
    drop =
      FALSE
  ]


message(
  "\n============================================================"
)

message(
  "RESULTATS DIEBOLD-MARIANO SIGNIFICATIFS"
)

message(
  "============================================================"
)


if (
  nrow(
    dm_significatif
  ) == 0
) {
  
  message(
    "Aucun secteur ne présente une différence significative au seuil de 5 %."
  )
  
} else {
  
  print(
    dm_significatif
  )
}


# ============================================================================
# 33. SAUVEGARDE DES PARAMETRES DE L'EVALUATION
# ============================================================================

parametres_evaluation <-
  list(
    
    initial_train =
      INITIAL_TRAIN,
    
    horizon =
      HORIZON,
    
    max_ar_lag =
      MAX_AR_LAG,
    
    target_transformation =
      "Delta log(VA)",
    
    growth_conversion =
      "100 * (exp(Delta log(VA)) - 1)",
    
    dfm_method =
      "dfms::DFM",
    
    dfm_em_method =
      "BM",
    
    dfm_r_source =
      "selection_facteurs.rds",
    
    dfm_p_source =
      "selection_retards_resume.csv",
    
    ar_selection =
      "AIC à chaque origine",
    
    evaluation_type =
      "Rolling-origin one-step-ahead"
  )


saveRDS(
  
  parametres_evaluation,
  
  file.path(
    OUT_DIR,
    "parametres_evaluation.rds"
  )
)


# ============================================================================
# 34. BILAN FINAL
# ============================================================================

message(
  "\n============================================================"
)

message(
  "10_evaluation.R TERMINE"
)

message(
  "============================================================"
)

message(
  "Prévisions de backtest : ",
  nrow(
    evaluation_predictions
  )
)

message(
  "Secteurs évalués       : ",
  length(
    unique(
      evaluation_predictions$secteur
    )
  )
)

message(
  "Secteurs DFM           : ",
  sum(
    evaluation_resume$modele_principal == "DFM",
    na.rm =
      TRUE
  )
)

message(
  "Secteurs AR fallback   : ",
  sum(
    evaluation_resume$modele_principal == "AR",
    na.rm =
      TRUE
  )
)

message(
  "DFM meilleur RMSE      : ",
  n_dfm_gagne
)

message(
  "AR meilleur RMSE       : ",
  n_ar_gagne
)

message(
  "Egalités               : ",
  n_egalite
)

message(
  "\nFichiers produits dans : "
)

message(
  OUT_DIR
)

message(
  "\n============================================================"
)