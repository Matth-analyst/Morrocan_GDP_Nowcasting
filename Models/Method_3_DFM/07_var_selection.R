# ============================================================================
# GDPNow Maroc — 07_var_selection.R
# Sélection du nombre de retards p du VAR des facteurs
# ============================================================================
#
# OBJECTIF
# --------
# Déterminer le nombre de retards p du VAR associé aux facteurs du DFM.
#
# SOURCE
# ------
#   - panels_stationnaires_filtres.rds
#   - selection_facteurs.rds
#
# METHODE
# -------
# 1. Récupération du nombre de facteurs r choisi au 06
# 2. Extraction des indicateurs mensuels
# 3. Équilibrage temporaire des données
# 4. PCA
# 5. VARselect sur les facteurs
# 6. Comparaison AIC / HQ / SC / FPE
#
# IMPORTANT
# ---------
# Le panel original n'est jamais modifié.
#
# ============================================================================


# --------------------------------------------------------------------------
# 1. Vérifications
# --------------------------------------------------------------------------

if (!exists("OUT_DIR")) {
  stop("OUT_DIR n'existe pas. Lance d'abord 01_setup.R.")
}


# --------------------------------------------------------------------------
# 2. Fichiers nécessaires
# --------------------------------------------------------------------------

fichier_panel <- file.path(
  OUT_DIR,
  "panels_stationnaires_filtres.rds"
)

fichier_facteurs <- file.path(
  OUT_DIR,
  "selection_facteurs.rds"
)


if (!file.exists(fichier_panel)) {
  stop(
    "Panel final introuvable : ",
    fichier_panel
  )
}


if (!file.exists(fichier_facteurs)) {
  stop(
    "Résultats du 06 introuvables. Lance d'abord 06_factor_selection.R."
  )
}


# --------------------------------------------------------------------------
# 3. Chargement
# --------------------------------------------------------------------------

panels_stationnaires_filtres <- readRDS(
  fichier_panel
)

selection <- readRDS(
  fichier_facteurs
)

message("Panel final chargé.")
message("Sélection des facteurs chargée.")


# --------------------------------------------------------------------------
# 4. Equilibrage temporaire
# --------------------------------------------------------------------------

balance_pour_var <- function(panel_stat) {
  
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
  
  x[
    complete.cases(x),
    ,
    drop = FALSE
  ]
}


# --------------------------------------------------------------------------
# 5. Sélection de p pour un secteur
# --------------------------------------------------------------------------

selection_retards <- function(
    panel_stat,
    r,
    secteur,
    pmax = PMAX
) {
  
  # ------------------------------------------------------------------------
  # Vérification de r
  # ------------------------------------------------------------------------
  
  if (
    is.na(r) ||
    r < 1
  ) {
    
    message(
      "  -> r indisponible : secteur exclu."
    )
    
    return(NULL)
  }
  
  
  # ------------------------------------------------------------------------
  # Variables mensuelles
  # ------------------------------------------------------------------------
  
  monthly_vars <- attr(
    panel_stat,
    "monthly_vars"
  )
  
  
  # Si la métadonnée est absente, reconstruction à partir de la cible.
  
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
      
    } else {
      
      warning(
        "Impossible d'identifier les variables mensuelles pour ",
        secteur
      )
      
      return(NULL)
    }
  }
  
  
  monthly_vars <- intersect(
    monthly_vars,
    colnames(panel_stat)
  )
  
  
  if (
    length(monthly_vars) < r
  ) {
    
    message(
      "  -> Nombre d'indicateurs mensuels insuffisant."
    )
    
    return(NULL)
  }
  
  
  # ------------------------------------------------------------------------
  # Extraction
  # ------------------------------------------------------------------------
  
  X <- panel_stat[
    ,
    monthly_vars,
    drop = FALSE
  ]
  
  
  # ------------------------------------------------------------------------
  # Equilibrage temporaire
  # ------------------------------------------------------------------------
  
  X_bal <- balance_pour_var(
    X
  )
  
  
  if (
    nrow(X_bal) <= 20
  ) {
    
    message(
      "  -> Trop peu d'observations pour VARselect."
    )
    
    return(NULL)
  }
  
  
  # ------------------------------------------------------------------------
  # Standardisation
  # ------------------------------------------------------------------------
  
  X_scaled <- scale(
    X_bal
  )
  
  
  # ------------------------------------------------------------------------
  # PCA
  # ------------------------------------------------------------------------
  
  pca <- prcomp(
    X_scaled,
    center = FALSE,
    scale. = FALSE
  )
  
  
  # ------------------------------------------------------------------------
  # Extraction des facteurs
  # ------------------------------------------------------------------------
  
  facteurs <- pca$x[
    ,
    1:r,
    drop = FALSE
  ]
  
  
  # ------------------------------------------------------------------------
  # Nombre maximal de retards
  # ------------------------------------------------------------------------
  
  # On évite d'utiliser trop de retards lorsque le nombre de facteurs
  # est élevé et/ou que l'échantillon est relativement court.
  
  pmax_eff <- min(
    pmax,
    floor(
      nrow(facteurs) / 10
    )
  )
  
  
  if (
    pmax_eff < 1
  ) {
    
    return(NULL)
  }
  
  
  # ------------------------------------------------------------------------
  # VARselect
  # ------------------------------------------------------------------------
  
  varsel <- tryCatch(
    
    vars::VARselect(
      facteurs,
      lag.max = pmax_eff,
      type = "const"
    ),
    
    error = function(e) {
      
      message(
        "  -> Erreur VARselect : ",
        conditionMessage(e)
      )
      
      NULL
    }
  )
  
  
  if (is.null(varsel)) {
    return(NULL)
  }
  
  
  # ------------------------------------------------------------------------
  # Extraction des choix
  # ------------------------------------------------------------------------
  
  p_AIC <- unname(
    varsel$selection["AIC(n)"]
  )
  
  p_HQ <- unname(
    varsel$selection["HQ(n)"]
  )
  
  p_SC <- unname(
    varsel$selection["SC(n)"]
  )
  
  p_FPE <- unname(
    varsel$selection["FPE(n)"]
  )
  
  
  # ------------------------------------------------------------------------
  # Choix parcimonieux
  # ------------------------------------------------------------------------
  #
  # HQ et SC pénalisent davantage la complexité que AIC.
  #
  # On utilise leur médiane comme point de départ.
  #
  # Si HQ et SC donnent la même valeur, celle-ci est retenue directement.
  #
  # ------------------------------------------------------------------------
  
  if (
    !is.na(p_HQ) &&
    !is.na(p_SC)
  ) {
    
    p_final <- round(
      median(
        c(
          p_HQ,
          p_SC
        )
      )
    )
    
  } else {
    
    p_final <- p_AIC
  }
  
  
  # ------------------------------------------------------------------------
  # Protection
  # ------------------------------------------------------------------------
  
  if (
    is.na(p_final) ||
    p_final < 1
  ) {
    
    p_final <- 1
  }
  
  
  p_final <- min(
    p_final,
    pmax_eff
  )
  
  
  # ------------------------------------------------------------------------
  # Résultat
  # ------------------------------------------------------------------------
  
  list(
    
    secteur = secteur,
    
    r = r,
    
    n_monthly = length(
      monthly_vars
    ),
    
    n_obs = nrow(
      facteurs
    ),
    
    p_AIC = p_AIC,
    
    p_HQ = p_HQ,
    
    p_SC = p_SC,
    
    p_FPE = p_FPE,
    
    p_final = p_final,
    
    facteurs = facteurs,
    
    VARselect = varsel
  )
}


# --------------------------------------------------------------------------
# 6. Application aux secteurs
# --------------------------------------------------------------------------

selection_p <- list()


for (s in names(panels_stationnaires_filtres)) {
  
  message("\n========================================")
  message("Secteur : ", s)
  message("========================================")
  
  
  # ------------------------------------------------------------------------
  # r choisi au 06
  # ------------------------------------------------------------------------
  
  if (
    is.null(selection[[s]])
  ) {
    
    message(
      "  -> aucun résultat du 06."
    )
    
    next
  }
  
  
  r <- selection[[s]]$r_final
  
  
  if (
    is.na(r)
  ) {
    
    message(
      "  -> secteur exclu : aucun r disponible."
    )
    
    next
  }
  
  
  message(
    "r utilisé = ",
    r
  )
  
  
  # ------------------------------------------------------------------------
  # Sélection p
  # ------------------------------------------------------------------------
  
  selection_p[[s]] <- selection_retards(
    
    panel_stat =
      panels_stationnaires_filtres[[s]],
    
    r =
      r,
    
    secteur =
      s,
    
    pmax =
      PMAX
  )
  
  
  res <- selection_p[[s]]
  
  
  if (!is.null(res)) {
    
    message(
      "AIC = ", res$p_AIC,
      " | HQ = ", res$p_HQ,
      " | SC = ", res$p_SC,
      " | FPE = ", res$p_FPE,
      " | p final = ", res$p_final
    )
  }
}


# --------------------------------------------------------------------------
# 7. Tableau détaillé
# --------------------------------------------------------------------------

selection_p_detail <- dplyr::bind_rows(
  
  lapply(
    
    selection_p,
    
    function(x) {
      
      if (is.null(x)) {
        return(NULL)
      }
      
      data.frame(
        
        secteur = x$secteur,
        
        r = x$r,
        
        n_monthly = x$n_monthly,
        
        n_obs = x$n_obs,
        
        p_AIC = x$p_AIC,
        
        p_HQ = x$p_HQ,
        
        p_SC = x$p_SC,
        
        p_FPE = x$p_FPE,
        
        p_final = x$p_final
      )
    }
  )
)


# --------------------------------------------------------------------------
# 8. Tableau résumé
# --------------------------------------------------------------------------

selection_p_resume <- selection_p_detail %>%
  
  dplyr::select(
    
    secteur,
    
    r,
    
    p_AIC,
    
    p_HQ,
    
    p_SC,
    
    p_FPE,
    
    p_final
  )


# --------------------------------------------------------------------------
# 9. Sauvegarde
# --------------------------------------------------------------------------

write.csv(
  
  selection_p_detail,
  
  file.path(
    OUT_DIR,
    "selection_retards_detail.csv"
  ),
  
  row.names = FALSE
)


write.csv(
  
  selection_p_resume,
  
  file.path(
    OUT_DIR,
    "selection_retards_resume.csv"
  ),
  
  row.names = FALSE
)


saveRDS(
  
  selection_p,
  
  file.path(
    OUT_DIR,
    "facteurs_VAR.rds"
  )
)


# --------------------------------------------------------------------------
# 10. Affichage
# --------------------------------------------------------------------------

message("\n========================================")
message("SELECTION FINALE DES RETARDS")
message("========================================")

print(
  selection_p_resume
)

message(
  "\n07_var_selection.R terminé."
)