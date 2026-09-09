# ============================================================================
# GDPNow Maroc — 08_dfm_estimation.R
# Estimation des Dynamic Factor Models (DFM)
# ============================================================================
#
# OBJECTIF
# --------
# Estimer un DFM sectoriel pour le nowcasting de la VA trimestrielle.
#
# ARCHITECTURE
# ------------
# Pour chaque secteur :
#
#   indicateurs mensuels + VA trimestrielle
#
# Les autres variables trimestrielles sont volontairement exclues.
#
# Les valeurs manquantes sont conservées et traitées par :
#
#   dfms::DFM(..., em.method = "BM")
#
# ============================================================================
# SPECIFICATION
# -------------
# Le nombre de facteurs r est récupéré depuis :
#
#   selection_facteurs.rds
#
# Le nombre de retards p est récupéré depuis :
#
#   selection_retards_resume.csv
#
# Il n'y a donc PLUS de r/p codés manuellement dans ce script.
#
# ============================================================================
# SORTIES
# -------
#   modeles_dfm.rds
#   estimation_dfm_detail.csv
#   facteurs_dfm.rds
#   cibles_ajustees_dfm.rds
#   figures/fitted_<secteur>.png
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


# ============================================================================
# 1. CHARGEMENT DU PANEL FINAL
# ============================================================================

fichier_panel <- file.path(
  OUT_DIR,
  "panels_stationnaires_filtres.rds"
)

if (!file.exists(fichier_panel)) {
  
  stop(
    "Panel final introuvable : ",
    fichier_panel
  )
}

panels_stationnaires_filtres <-
  readRDS(fichier_panel)

message(
  "\nPanel final chargé."
)


# ============================================================================
# 2. CHARGEMENT DE LA SELECTION DES FACTEURS
# ============================================================================

fichier_r <- file.path(
  OUT_DIR,
  "selection_facteurs.rds"
)

if (!file.exists(fichier_r)) {
  
  stop(
    "Fichier de sélection des facteurs introuvable : ",
    fichier_r
  )
}

selection_facteurs <-
  readRDS(fichier_r)

message(
  "Sélection des facteurs chargée."
)


# ============================================================================
# 3. CHARGEMENT DE LA SELECTION DES RETARDS
# ============================================================================

fichier_p <- file.path(
  OUT_DIR,
  "selection_retards_resume.csv"
)

if (!file.exists(fichier_p)) {
  
  stop(
    "Fichier de sélection des retards introuvable : ",
    fichier_p
  )
}

selection_retards <-
  read.csv(
    fichier_p,
    stringsAsFactors = FALSE
  )

message(
  "Sélection des retards chargée."
)


# ============================================================================
# 4. CONSTRUCTION DES VECTEURS r ET p
# ============================================================================
#
# IMPORTANT :
# selection_facteurs.rds est une liste nommée de listes.
#
# Exemple :
#   selection_facteurs[["Peche"]]$r_final
#
# et non un data.frame contenant une colonne "r_final".
# ============================================================================


# --------------------------------------------------------------------------
# 4.1 Nombre de facteurs r
# --------------------------------------------------------------------------

r_final <- sapply(
  selection_facteurs,
  function(x) {
    
    if (is.null(x$r_final)) {
      return(NA_real_)
    }
    
    as.numeric(x$r_final)
  }
)

names(r_final) <- names(selection_facteurs)


# --------------------------------------------------------------------------
# 4.2 Nombre de retards p
# --------------------------------------------------------------------------

# selection_retards_resume.csv est normalement un data.frame,
# donc ici la structure précédente reste adaptée.

p_final <- setNames(
  as.numeric(selection_retards$p_final),
  selection_retards$secteur
)


# --------------------------------------------------------------------------
# 4.3 Vérification
# --------------------------------------------------------------------------

secteurs <- names(
  panels_stationnaires_filtres
)


message("\n=== VERIFICATION DES PARAMETRES r ET p ===")

for (s in secteurs) {
  
  r_s <- if (s %in% names(r_final)) r_final[s] else NA
  p_s <- if (s %in% names(p_final)) p_final[s] else NA
  
  message(
    sprintf(
      "%-35s | r = %-3s | p = %-3s",
      s,
      ifelse(is.na(r_s), "NA", r_s),
      ifelse(is.na(p_s), "NA", p_s)
    )
  )
}


# --------------------------------------------------------------------------
# 4.4 Secteurs manquants
# --------------------------------------------------------------------------

secteurs_manquants_r <- setdiff(
  secteurs,
  names(r_final)
)

secteurs_manquants_p <- setdiff(
  secteurs,
  names(p_final)
)


if (length(secteurs_manquants_r) > 0) {
  
  warning(
    "Secteurs absents de la sélection des facteurs : ",
    paste(
      secteurs_manquants_r,
      collapse = ", "
    )
  )
}


if (length(secteurs_manquants_p) > 0) {
  
  warning(
    "Secteurs absents de la sélection des retards : ",
    paste(
      secteurs_manquants_p,
      collapse = ", "
    )
  )
}
# ============================================================================
# 5. FONCTION D'ESTIMATION D'UN DFM SECTORIEL
# ============================================================================

estimer_dfm_secteur <- function(
    panel,
    secteur,
    r,
    p
) {
  
  message(
    "\n============================================================"
  )
  
  message(
    "SECTEUR : ",
    secteur
  )
  
  message(
    "r = ",
    r,
    " | p = ",
    p
  )
  
  
  # --------------------------------------------------------------------------
  # 5.1 Vérification des paramètres
  # --------------------------------------------------------------------------
  
  if (
    length(r) == 0 ||
    is.na(r) ||
    r < 1
  ) {
    
    message(
      "  -> r invalide : secteur exclu."
    )
    
    return(
      list(
        status = "invalid_r",
        model = NULL,
        fitted = NULL,
        factors = NULL,
        target = NA,
        monthly_vars = character(0),
        n_monthly = 0,
        n_variables_dfm = NA,
        r = NA,
        p = p,
        convergence = NA,
        n_iter = NA,
        warning = NA
      )
    )
  }
  
  
  if (
    length(p) == 0 ||
    is.na(p) ||
    p < 1
  ) {
    
    message(
      "  -> p invalide : secteur exclu."
    )
    
    return(
      list(
        status = "invalid_p",
        model = NULL,
        fitted = NULL,
        factors = NULL,
        target = NA,
        monthly_vars = character(0),
        n_monthly = 0,
        n_variables_dfm = NA,
        r = r,
        p = NA,
        convergence = NA,
        n_iter = NA,
        warning = NA
      )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # 5.2 Récupération des métadonnées
  # --------------------------------------------------------------------------
  
  target_name <-
    attr(
      panel,
      "target"
    )
  
  monthly_vars <-
    attr(
      panel,
      "monthly_vars"
    )
  
  
  # --------------------------------------------------------------------------
  # 5.3 Sécurité : reconstruction des variables mensuelles
  # --------------------------------------------------------------------------
  
  if (
    is.null(monthly_vars)
  ) {
    
    monthly_vars <-
      setdiff(
        colnames(panel),
        target_name
      )
  }
  
  
  monthly_vars <-
    intersect(
      monthly_vars,
      colnames(panel)
    )
  
  
  # --------------------------------------------------------------------------
  # 5.4 Vérification de la cible
  # --------------------------------------------------------------------------
  
  if (
    is.null(target_name) ||
    !target_name %in% colnames(panel)
  ) {
    
    message(
      "  -> Cible introuvable."
    )
    
    return(
      list(
        status = "missing_target",
        model = NULL,
        fitted = NULL,
        factors = NULL,
        target = target_name,
        monthly_vars = monthly_vars,
        n_monthly = length(monthly_vars),
        n_variables_dfm = NA,
        r = r,
        p = p,
        convergence = NA,
        n_iter = NA,
        warning = NA
      )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # 5.5 Vérification des indicateurs mensuels
  # --------------------------------------------------------------------------
  
  if (
    length(monthly_vars) == 0
  ) {
    
    message(
      "  -> AUCUN indicateur mensuel disponible."
    )
    
    message(
      "  -> DFM non estimé."
    )
    
    return(
      list(
        status = "no_monthly_information",
        model = NULL,
        fitted = NULL,
        factors = NULL,
        target = target_name,
        monthly_vars = monthly_vars,
        n_monthly = 0,
        n_variables_dfm = 1,
        r = r,
        p = p,
        convergence = NA,
        n_iter = NA,
        warning = NA
      )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # 5.6 Vérification du nombre de séries
  # --------------------------------------------------------------------------
  
  if (
    length(monthly_vars) < r
  ) {
    
    message(
      "  -> Nombre de séries mensuelles insuffisant : ",
      length(monthly_vars),
      " < r = ",
      r
    )
    
    return(
      list(
        status = "insufficient_monthly_series",
        model = NULL,
        fitted = NULL,
        factors = NULL,
        target = target_name,
        monthly_vars = monthly_vars,
        n_monthly = length(monthly_vars),
        n_variables_dfm = length(monthly_vars) + 1,
        r = r,
        p = p,
        convergence = NA,
        n_iter = NA,
        warning = NA
      )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # 5.7 Construction du panel DFM
  # --------------------------------------------------------------------------
  #
  # IMPORTANT :
  #
  #   X = indicateurs mensuels + cible trimestrielle
  #
  # Les autres variables trimestrielles sont exclues.
  #
  # --------------------------------------------------------------------------
  
  variables_dfm <-
    unique(
      c(
        monthly_vars,
        target_name
      )
    )
  
  panel_dfm <-
    panel[
      ,
      variables_dfm,
      drop = FALSE
    ]
  
  
  X <-
    as.matrix(
      panel_dfm
    )
  
  
  quarterly_vars_dfm <-
    target_name
  
  
  message(
    "  Panel DFM : ",
    nrow(X),
    " observations x ",
    ncol(X),
    " variables"
  )
  
  message(
    "  Mensuelles : ",
    length(monthly_vars)
  )
  
  message(
    "  Cible trimestrielle : ",
    target_name
  )
  
  
  # --------------------------------------------------------------------------
  # 5.8 Estimation avec capture des warnings
  # --------------------------------------------------------------------------
  
  warnings_dfm <-
    character(0)
  
  
  model <-
    tryCatch(
      
      withCallingHandlers(
        
        {
          
          dfms::DFM(
            
            X = X,
            
            r = r,
            
            p = p,
            
            quarterly.vars =
              quarterly_vars_dfm,
            
            em.method = "BM"
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
        
        message(
          "  -> ERREUR DFM : ",
          conditionMessage(e)
        )
        
        NULL
      }
    )
  
  
  # --------------------------------------------------------------------------
  # 5.9 Echec de l'estimation
  # --------------------------------------------------------------------------
  
  if (
    is.null(model)
  ) {
    
    return(
      list(
        status = "error",
        model = NULL,
        fitted = NULL,
        factors = NULL,
        target = target_name,
        monthly_vars = monthly_vars,
        n_monthly = length(monthly_vars),
        n_variables_dfm = ncol(X),
        r = r,
        p = p,
        convergence = FALSE,
        n_iter = NA,
        warning =
          if (length(warnings_dfm) > 0)
            paste(
              warnings_dfm,
              collapse = " || "
            )
        else
          NA
      )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # 5.10 Récupération des facteurs
  # --------------------------------------------------------------------------
  
  facteurs <-
    tryCatch(
      
      model$F_qml,
      
      error = function(e)
        NULL
    )
  
  
  # --------------------------------------------------------------------------
  # 5.11 Valeurs ajustées
  # --------------------------------------------------------------------------
  
  fitted_target <-
    tryCatch(
      
      fitted(
        
        model,
        
        method = "qml",
        
        orig.format = TRUE,
        
        standardized = FALSE,
        
        na.keep = TRUE
      ),
      
      error = function(e) {
        
        message(
          "  -> Impossible d'extraire les valeurs ajustées : ",
          conditionMessage(e)
        )
        
        NULL
      }
    )
  
  
  # --------------------------------------------------------------------------
  # 5.12 Recherche du nombre d'itérations
  # --------------------------------------------------------------------------
  
  n_iter <- NA
  
  champs_iter <- c(
    "n.iter",
    "n_iter",
    "iterations",
    "iter"
  )
  
  for (champ in champs_iter) {
    
    if (
      !is.null(model[[champ]])
    ) {
      
      n_iter <-
        model[[champ]][1]
      
      break
    }
  }
  
  
  # --------------------------------------------------------------------------
  # 5.13 Détermination prudente du statut de convergence
  # --------------------------------------------------------------------------
  #
  # On ne considère PAS automatiquement :
  #
  #   status == "estimated"
  #
  # comme une preuve de convergence.
  #
  # Le package affiche notamment :
  #
  #   "Converged after XX iterations"
  #
  # ou
  #
  #   "Maximum number of iterations reached."
  #
  # Si aucune information explicite n'est disponible, on conserve NA.
  #
  # --------------------------------------------------------------------------
  
  convergence <-
    NA
  
  
  # Recherche éventuelle d'un champ logique explicite
  
  champs_convergence <- c(
    "converged",
    "convergence"
  )
  
  for (champ in champs_convergence) {
    
    if (
      !is.null(model[[champ]])
    ) {
      
      valeur <-
        model[[champ]]
      
      if (
        is.logical(valeur) &&
        length(valeur) >= 1
      ) {
        
        convergence <-
          valeur[1]
        
        break
      }
    }
  }
  
  
  # Si aucune information explicite n'est disponible,
  # on utilise les warnings uniquement comme indicateur secondaire.
  
  if (
    is.na(convergence)
  ) {
    
    convergence <-
      ifelse(
        any(
          grepl(
            "Maximum number of iterations",
            warnings_dfm,
            ignore.case = TRUE
          )
        ),
        FALSE,
        NA
      )
  }
  
  
  # --------------------------------------------------------------------------
  # 5.14 Statut final
  # --------------------------------------------------------------------------
  
  status_final <-
    "estimated"
  
  
  if (
    !is.na(convergence) &&
    convergence == FALSE
  ) {
    
    status_final <-
      "estimated_max_iterations"
  }
  
  
  # --------------------------------------------------------------------------
  # 5.15 Affichage
  # --------------------------------------------------------------------------
  
  message(
    "  -> DFM estimé."
  )
  
  message(
    "  -> Convergence : ",
    convergence
  )
  
  if (
    length(warnings_dfm) > 0
  ) {
    
    message(
      "  -> Warning(s) : ",
      paste(
        unique(warnings_dfm),
        collapse = " || "
      )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # 5.16 Retour
  # --------------------------------------------------------------------------
  
  list(
    
    status =
      status_final,
    
    model =
      model,
    
    fitted =
      fitted_target,
    
    factors =
      facteurs,
    
    target =
      target_name,
    
    monthly_vars =
      monthly_vars,
    
    quarterly_vars_dfm =
      quarterly_vars_dfm,
    
    n_monthly =
      length(monthly_vars),
    
    n_variables_dfm =
      ncol(X),
    
    r =
      r,
    
    p =
      p,
    
    convergence =
      convergence,
    
    n_iter =
      n_iter,
    
    warning =
      if (
        length(warnings_dfm) > 0
      )
        paste(
          unique(warnings_dfm),
          collapse = " || "
        )
    else
      NA
  )
}


# ============================================================================
# 6. ESTIMATION SECTEUR PAR SECTEUR
# ============================================================================

modeles_dfm <-
  list()


resultats_detail <-
  list()


for (s in secteurs) {
  
  # --------------------------------------------------------------------------
  # Récupération automatique de r et p
  # --------------------------------------------------------------------------
  
  r_s <-
    if (
      s %in% names(r_final)
    )
      r_final[s]
  else
    NA
  
  
  p_s <-
    if (
      s %in% names(p_final)
    )
      p_final[s]
  else
    NA
  
  
  # --------------------------------------------------------------------------
  # Secteur sans spécification
  # --------------------------------------------------------------------------
  
  if (
    is.na(r_s) ||
    is.na(p_s)
  ) {
    
    message(
      "\n",
      s,
      " : exclu du DFM."
    )
    
    modeles_dfm[[s]] <-
      list(
        status = "excluded",
        model = NULL,
        fitted = NULL,
        factors = NULL,
        r = r_s,
        p = p_s,
        n_monthly = NA,
        n_variables_dfm = NA,
        convergence = NA,
        n_iter = NA,
        warning = NA
      )
    
    next
  }
  
  
  # --------------------------------------------------------------------------
  # Estimation
  # --------------------------------------------------------------------------
  
  resultat <-
    estimer_dfm_secteur(
      
      panel =
        panels_stationnaires_filtres[[s]],
      
      secteur =
        s,
      
      r =
        r_s,
      
      p =
        p_s
    )
  
  
  modeles_dfm[[s]] <-
    resultat
}


# ============================================================================
# 7. CONSTRUCTION DU RESUME FINAL
# ============================================================================

resultats_detail <-
  list()


for (s in names(modeles_dfm)) {
  
  resultat <-
    modeles_dfm[[s]]
  
  
  resultats_detail[[s]] <-
    data.frame(
      
      secteur =
        s,
      
      status =
        resultat$status,
      
      r =
        if (!is.null(resultat$r))
          resultat$r
      else
        NA,
      
      p =
        if (!is.null(resultat$p))
          resultat$p
      else
        NA,
      
      n_mensuelles =
        if (!is.null(resultat$n_monthly))
          resultat$n_monthly
      else
        NA,
      
      n_variables_dfm =
        if (!is.null(resultat$n_variables_dfm))
          resultat$n_variables_dfm
      else
        NA,
      
      convergence =
        if (!is.null(resultat$convergence))
          resultat$convergence
      else
        NA,
      
      n_iterations =
        if (!is.null(resultat$n_iter))
          resultat$n_iter
      else
        NA,
      
      warning =
        if (!is.null(resultat$warning))
          resultat$warning
      else
        NA,
      
      stringsAsFactors = FALSE
    )
}


resume_dfm <-
  dplyr::bind_rows(
    resultats_detail
  )


# ============================================================================
# 8. AFFICHAGE DU RESUME
# ============================================================================

message(
  "\n============================================================"
)

message(
  "RESUME DES ESTIMATIONS DFM"
)

message(
  "============================================================"
)

print(
  resume_dfm
)


# ============================================================================
# 9. SAUVEGARDE DU RESUME
# ============================================================================

write.csv(
  
  resume_dfm,
  
  file.path(
    OUT_DIR,
    "estimation_dfm_detail.csv"
  ),
  
  row.names = FALSE
)


# ============================================================================
# 10. SAUVEGARDE DES MODELES
# ============================================================================

saveRDS(
  
  modeles_dfm,
  
  file.path(
    OUT_DIR,
    "modeles_dfm.rds"
  )
)


# ============================================================================
# 11. EXTRACTION DES FACTEURS
# ============================================================================

facteurs_dfm <-
  list()


for (s in names(modeles_dfm)) {
  
  resultat <-
    modeles_dfm[[s]]
  
  
  if (
    !is.null(resultat$factors)
  ) {
    
    facteurs_dfm[[s]] <-
      resultat$factors
  }
}


saveRDS(
  
  facteurs_dfm,
  
  file.path(
    OUT_DIR,
    "facteurs_dfm.rds"
  )
)


# ============================================================================
# 12. EXTRACTION DES VALEURS AJUSTEES
# ============================================================================

cibles_ajustees <-
  list()


for (s in names(modeles_dfm)) {
  
  resultat <-
    modeles_dfm[[s]]
  
  
  if (
    !is.null(resultat$fitted)
  ) {
    
    fitted_target <-
      resultat$fitted
    
    
    cibles_ajustees[[s]] <-
      
      data.frame(
        
        date =
          zoo::index(
            fitted_target
          ),
        
        fitted =
          as.numeric(
            fitted_target
          )
      )
  }
}


saveRDS(
  
  cibles_ajustees,
  
  file.path(
    OUT_DIR,
    "cibles_ajustees_dfm.rds"
  )
)


# ============================================================================
# 13. FIGURES DES VALEURS AJUSTEES
# ============================================================================

if (
  !dir.exists(FIG_DIR)
) {
  
  dir.create(
    FIG_DIR,
    recursive = TRUE
  )
}


for (s in names(cibles_ajustees)) {
  
  df <-
    cibles_ajustees[[s]]
  
  
  if (
    nrow(df) == 0
  ) {
    
    next
  }
  
  
  g <-
    ggplot(
      
      df,
      
      aes(
        x = date,
        y = fitted
      )
      
    ) +
    
    geom_line() +
    
    labs(
      
      title =
        paste(
          "VA ajustée par le DFM —",
          s
        ),
      
      x = NULL,
      
      y = "Valeur ajustée"
      
    ) +
    
    theme_minimal()
  
  
  ggsave(
    
    file.path(
      
      FIG_DIR,
      
      paste0(
        "fitted_",
        s,
        ".png"
      )
    ),
    
    g,
    
    width = 10,
    
    height = 5,
    
    dpi = 150
  )
}


# ============================================================================
# 14. CONTROLE FINAL DE LA STRUCTURE DES MODELES
# ============================================================================

message(
  "\n=== CONTROLE FINAL DE LA STRUCTURE DES DFM ==="
)


for (s in names(modeles_dfm)) {
  
  resultat <-
    modeles_dfm[[s]]
  
  
  if (
    resultat$status %in%
    c(
      "estimated",
      "estimated_max_iterations"
    )
  ) {
    
    message(
      
      s,
      
      " : ",
      
      resultat$n_monthly,
      
      " mensuelles + 1 cible trimestrielle",
      
      " | r = ",
      
      resultat$r,
      
      " | p = ",
      
      resultat$p,
      
      " | convergence = ",
      
      resultat$convergence
      
    )
    
  } else {
    
    message(
      
      s,
      
      " : ",
      
      resultat$status
    )
  }
}


# ============================================================================
# 15. BILAN FINAL
# ============================================================================

n_estimes <-
  sum(
    resume_dfm$status %in%
      c(
        "estimated",
        "estimated_max_iterations"
      )
  )


n_convergence_ok <-
  sum(
    resume_dfm$convergence == TRUE,
    na.rm = TRUE
  )


n_max_iterations <-
  sum(
    resume_dfm$status ==
      "estimated_max_iterations"
  )


n_echecs <-
  sum(
    resume_dfm$status ==
      "error"
  )


n_sans_mensuelles <-
  sum(
    resume_dfm$status ==
      "no_monthly_information"
  )


n_exclus <-
  sum(
    resume_dfm$status ==
      "excluded"
  )


message(
  "\n============================================================"
)

message(
  "08_dfm_estimation.R terminé."
)

message(
  "Modèles estimés              : ",
  n_estimes
)

message(
  "Convergence confirmée       : ",
  n_convergence_ok
)

message(
  "Maximum d'itérations        : ",
  n_max_iterations
)

message(
  "Erreurs d'estimation        : ",
  n_echecs
)

message(
  "Sans données mensuelles     : ",
  n_sans_mensuelles
)

message(
  "Secteurs exclus             : ",
  n_exclus
)

message(
  "============================================================"
)

