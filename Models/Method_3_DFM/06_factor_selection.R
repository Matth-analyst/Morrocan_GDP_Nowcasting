# ============================================================================
# GDPNow Maroc — 06_factor_selection.R
# Sélection du nombre de facteurs latents par secteur
# ============================================================================
#
# OBJECTIF
# --------
# Sélectionner le nombre de facteurs r des DFM sectoriels.
#
# SOURCE
# ------
# panels_stationnaires_filtres.rds
#
# IMPORTANT
# ---------
# Seuls les indicateurs MENSUELS entrent dans la sélection des facteurs.
# La cible trimestrielle de VA est exclue de la PCA.
#
# METHODE
# -------
# Critères d'information de Bai-Ng via dfms::ICr().
#
# On conserve :
#   - IC1
#   - IC2
#   - IC3
#   - r.star fourni par dfms
#   - variance cumulée expliquée
#
# ATTENTION
# ---------
# Si un critère atteint RMAX, cela signifie que la grille de recherche est
# probablement trop courte. On ne considère PAS automatiquement RMAX comme
# un nombre économiquement optimal de facteurs.
#
# ============================================================================


# --------------------------------------------------------------------------
# 1. Vérifications
# --------------------------------------------------------------------------

if (!exists("OUT_DIR")) {
  stop("OUT_DIR n'existe pas. Lance d'abord 01_setup.R.")
}


# --------------------------------------------------------------------------
# 2. Chargement du panel FINAL
# --------------------------------------------------------------------------

fichier_panel <- file.path(
  OUT_DIR,
  "panels_stationnaires_filtres.rds"
)

if (!file.exists(fichier_panel)) {
  stop(
    "Fichier introuvable : ",
    fichier_panel
  )
}

panels_stationnaires_filtres <- readRDS(
  fichier_panel
)

message(
  "Panel final chargé : ",
  fichier_panel
)


# --------------------------------------------------------------------------
# 3. Fonction d'équilibrage temporaire
# --------------------------------------------------------------------------
#
# Cette opération sert uniquement à la PCA / sélection de r.
# Elle ne modifie pas le panel utilisé ensuite pour le DFM.
#
# --------------------------------------------------------------------------

balance_pour_ic <- function(panel_stat) {
  
  x <- as.data.frame(panel_stat)
  
  x <- zoo::na.approx(
    x,
    na.rm = FALSE
  )
  
  x <- zoo::na.locf(
    x,
    na.rm = FALSE
  )
  
  x <- zoo::na.locf(
    x,
    fromLast = TRUE,
    na.rm = FALSE
  )
  
  x <- as.matrix(x)
  
  x <- x[
    complete.cases(x),
    ,
    drop = FALSE
  ]
  
  x
}


# --------------------------------------------------------------------------
# 4. Fonction de sélection des facteurs
# --------------------------------------------------------------------------

selection_facteurs <- function(
    panel_stat,
    secteur,
    rmax = RMAX
) {
  
  # ------------------------------------------------------------------------
  # Variables mensuelles
  # ------------------------------------------------------------------------
  
  monthly_vars <- attr(
    panel_stat,
    "monthly_vars"
  )
  
  # Si la métadonnée est absente, on essaie de la reconstruire :
  # la cible est connue et toutes les autres variables sont considérées
  # comme variables mensuelles.
  
  if (is.null(monthly_vars)) {
    
    target_name <- attr(
      panel_stat,
      "target"
    )
    
    if (!is.null(target_name)) {
      
      monthly_vars <- setdiff(
        colnames(panel_stat),
        target_name
      )
      
      message(
        "  -> monthly_vars absente : reconstruction automatique."
      )
      
    } else {
      
      warning(
        "Impossible d'identifier les variables mensuelles pour ",
        secteur
      )
      
      return(
        list(
          secteur = secteur,
          n_monthly = 0,
          n_obs = NA,
          r_ic1 = NA,
          r_ic2 = NA,
          r_ic3 = NA,
          r_dfms = NA,
          r_var60 = NA,
          r_final = NA,
          borne_ic = FALSE,
          variance = NULL,
          icr = NULL
        )
      )
    }
  }
  
  
  # ------------------------------------------------------------------------
  # Garder uniquement les variables réellement présentes
  # ------------------------------------------------------------------------
  
  monthly_vars <- intersect(
    monthly_vars,
    colnames(panel_stat)
  )
  
  
  n_monthly <- length(
    monthly_vars
  )
  
  
  # ------------------------------------------------------------------------
  # Aucun indicateur mensuel
  # ------------------------------------------------------------------------
  
  if (n_monthly == 0) {
    
    message(
      "  -> aucune variable mensuelle : secteur exclu."
    )
    
    return(
      list(
        secteur = secteur,
        n_monthly = 0,
        n_obs = NA,
        r_ic1 = NA,
        r_ic2 = NA,
        r_ic3 = NA,
        r_dfms = NA,
        r_var60 = NA,
        r_final = NA,
        borne_ic = FALSE,
        variance = NULL,
        icr = NULL
      )
    )
  }
  
  
  # ------------------------------------------------------------------------
  # Extraction des indicateurs mensuels
  # ------------------------------------------------------------------------
  
  X <- panel_stat[
    ,
    monthly_vars,
    drop = FALSE
  ]
  
  
  # ------------------------------------------------------------------------
  # Equilibrage temporaire
  # ------------------------------------------------------------------------
  
  X_bal <- balance_pour_ic(
    X
  )
  
  
  # ------------------------------------------------------------------------
  # Vérification
  # ------------------------------------------------------------------------
  
  if (
    nrow(X_bal) < 20 ||
    ncol(X_bal) < 2
  ) {
    
    message(
      "  -> données insuffisantes pour ICr."
    )
    
    return(
      list(
        secteur = secteur,
        n_monthly = n_monthly,
        n_obs = nrow(X_bal),
        r_ic1 = NA,
        r_ic2 = NA,
        r_ic3 = NA,
        r_dfms = NA,
        r_var60 = NA,
        r_final = NA,
        borne_ic = FALSE,
        variance = NULL,
        icr = NULL
      )
    )
  }
  
  
  # ------------------------------------------------------------------------
  # RMAX adapté au nombre de variables
  # ------------------------------------------------------------------------
  
  rmax_eff <- min(
    rmax,
    ncol(X_bal) - 1
  )
  
  
  # ------------------------------------------------------------------------
  # Critères Bai-Ng
  # ------------------------------------------------------------------------
  
  icr <- tryCatch(
    
    dfms::ICr(
      X_bal,
      max.r = rmax_eff
    ),
    
    error = function(e) {
      
      message(
        "  -> erreur ICr : ",
        conditionMessage(e)
      )
      
      NULL
    }
  )
  
  
  if (is.null(icr)) {
    
    return(
      list(
        secteur = secteur,
        n_monthly = n_monthly,
        n_obs = nrow(X_bal),
        r_ic1 = NA,
        r_ic2 = NA,
        r_ic3 = NA,
        r_dfms = NA,
        r_var60 = NA,
        r_final = NA,
        borne_ic = FALSE,
        variance = NULL,
        icr = NULL
      )
    )
  }
  
  
  # ------------------------------------------------------------------------
  # Extraction des critères
  # ------------------------------------------------------------------------
  
  IC_raw <- as.matrix(
    icr$IC
  )
  
  IC1 <- IC_raw[, 1]
  IC2 <- IC_raw[, 2]
  IC3 <- IC_raw[, 3]
  
  
  r_ic1 <- which.min(
    IC1
  )
  
  r_ic2 <- which.min(
    IC2
  )
  
  r_ic3 <- which.min(
    IC3
  )
  
  
  # ------------------------------------------------------------------------
  # r.star fourni directement par dfms
  # ------------------------------------------------------------------------
  
  r_star <- icr$r.star
  
  
  # Selon la version de dfms, r.star peut contenir plusieurs valeurs
  # correspondant aux trois critères.
  
  if (length(r_star) >= 3) {
    
    r_dfms <- round(
      median(
        r_star[1:3]
      )
    )
    
  } else if (length(r_star) == 1) {
    
    r_dfms <- as.numeric(
      r_star
    )
    
  } else {
    
    r_dfms <- NA
  }
  
  
  # ------------------------------------------------------------------------
  # Variance expliquée
  # ------------------------------------------------------------------------
  
  eig <- icr$eigenvalues
  
  variance_cumulee <- cumsum(
    eig
  ) / sum(
    eig
  )
  
  
  # Premier nombre de facteurs atteignant 60 %
  
  r_var60 <- which(
    variance_cumulee >= 0.60
  )[1]
  
  
  if (length(r_var60) == 0 || is.na(r_var60)) {
    
    r_var60 <- NA
  }
  
  
  # ------------------------------------------------------------------------
  # Détection du problème de borne
  # ------------------------------------------------------------------------
  
  borne_ic <- any(
    c(
      r_ic1,
      r_ic2,
      r_ic3
    ) >= rmax_eff
  )
  
  
  if (borne_ic) {
    
    message(
      "  !! Attention : au moins un critère atteint la borne RMAX = ",
      rmax_eff
    )
  }
  
  
  # ------------------------------------------------------------------------
  # Choix final provisoire
  # ------------------------------------------------------------------------
  #
  # Si les critères convergent vers une petite valeur :
  #       on utilise leur médiane.
  #
  # Si les critères atteignent la borne :
  #       on ne retient PAS automatiquement RMAX.
  #
  # On utilise alors r_dfms ou une valeur parcimonieuse fondée sur la
  # variance expliquée.
  #
  # ------------------------------------------------------------------------
  
  r_criteres <- c(
    r_ic1,
    r_ic2,
    r_ic3
  )
  
  
  if (!borne_ic) {
    
    r_final <- round(
      median(
        r_criteres
      )
    )
    
  } else {
    
    # Lorsque les IC atteignent la borne, on privilégie la structure
    # de la variance expliquée et on limite volontairement le nombre
    # de facteurs à une solution parcimonieuse.
    
    if (
      !is.na(r_var60) &&
      r_var60 <= 8
    ) {
      
      r_final <- r_var60
      
    } else if (
      !is.na(r_dfms) &&
      r_dfms < rmax_eff
    ) {
      
      r_final <- r_dfms
      
    } else {
      
      # Valeur provisoire volontairement parcimonieuse.
      # Elle devra être confirmée par l'estimation DFM et les diagnostics.
      
      r_final <- min(
        6,
        rmax_eff
      )
    }
  }
  
  
  # Protection finale
  
  r_final <- max(
    1,
    min(
      r_final,
      rmax_eff
    )
  )
  
  
  # ------------------------------------------------------------------------
  # Tableau de variance
  # ------------------------------------------------------------------------
  
  variance <- data.frame(
    
    secteur = secteur,
    
    r = seq_along(
      variance_cumulee
    ),
    
    variance_expliquee_cumulee =
      variance_cumulee * 100
  )
  
  
  # ------------------------------------------------------------------------
  # Résultat
  # ------------------------------------------------------------------------
  
  list(
    
    secteur = secteur,
    
    n_monthly = n_monthly,
    
    n_obs = nrow(X_bal),
    
    r_ic1 = r_ic1,
    
    r_ic2 = r_ic2,
    
    r_ic3 = r_ic3,
    
    r_dfms = r_dfms,
    
    r_var60 = r_var60,
    
    r_final = r_final,
    
    borne_ic = borne_ic,
    
    variance = variance,
    
    icr = icr
  )
}


# --------------------------------------------------------------------------
# 5. Application à tous les secteurs
# --------------------------------------------------------------------------

selection <- list()


for (s in names(panels_stationnaires_filtres)) {
  
  message("\n----------------------------------------")
  message("Secteur : ", s)
  message("----------------------------------------")
  
  
  selection[[s]] <- selection_facteurs(
    
    panel_stat =
      panels_stationnaires_filtres[[s]],
    
    secteur =
      s,
    
    rmax =
      RMAX
  )
  
  
  res <- selection[[s]]
  
  
  message(
    "Variables mensuelles : ",
    res$n_monthly
  )
  
  message(
    "IC1 = ", res$r_ic1,
    " | IC2 = ", res$r_ic2,
    " | IC3 = ", res$r_ic3
  )
  
  message(
    "r.dfms = ", res$r_dfms,
    " | r(60%) = ", res$r_var60,
    " | r final = ", res$r_final
  )
}


# --------------------------------------------------------------------------
# 6. Tableau détaillé
# --------------------------------------------------------------------------

selection_detail <- dplyr::bind_rows(
  
  lapply(
    
    selection,
    
    function(x) {
      
      data.frame(
        
        secteur = x$secteur,
        
        n_monthly = x$n_monthly,
        
        n_obs = x$n_obs,
        
        r_ic1 = x$r_ic1,
        
        r_ic2 = x$r_ic2,
        
        r_ic3 = x$r_ic3,
        
        r_dfms = x$r_dfms,
        
        r_var60 = x$r_var60,
        
        r_final = x$r_final,
        
        borne_ic = x$borne_ic
      )
    }
  )
)


# --------------------------------------------------------------------------
# 7. Tableau de variance
# --------------------------------------------------------------------------

variance_detail <- dplyr::bind_rows(
  
  lapply(
    
    selection,
    
    function(x) {
      
      if (
        is.null(x$variance)
      ) {
        return(NULL)
      }
      
      x$variance
    }
  )
)


# --------------------------------------------------------------------------
# 8. Tableau résumé
# --------------------------------------------------------------------------

selection_resume <- selection_detail %>%
  
  dplyr::select(
    
    secteur,
    
    n_monthly,
    
    r_ic1,
    
    r_ic2,
    
    r_ic3,
    
    r_dfms,
    
    r_var60,
    
    r_final,
    
    borne_ic
  )


# --------------------------------------------------------------------------
# 9. Sauvegardes
# --------------------------------------------------------------------------

write.csv(
  
  selection_detail,
  
  file.path(
    OUT_DIR,
    "selection_facteurs_detail.csv"
  ),
  
  row.names = FALSE
)


write.csv(
  
  selection_resume,
  
  file.path(
    OUT_DIR,
    "selection_facteurs_resume.csv"
  ),
  
  row.names = FALSE
)


write.csv(
  
  variance_detail,
  
  file.path(
    OUT_DIR,
    "facteurs_variance_expliquee.csv"
  ),
  
  row.names = FALSE
)


saveRDS(
  
  selection,
  
  file.path(
    OUT_DIR,
    "selection_facteurs.rds"
  )
)


# --------------------------------------------------------------------------
# 10. Affichage final
# --------------------------------------------------------------------------

message("\n========================================")
message("SELECTION FINALE DES FACTEURS")
message("========================================")

print(
  selection_resume
)

message(
  "\n06_factor_selection.R terminé."
)