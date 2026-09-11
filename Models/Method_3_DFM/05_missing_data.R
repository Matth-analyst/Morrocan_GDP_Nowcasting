# ============================================================================
# GDPNow Maroc — 05_missing_data.R
# Traitement des valeurs manquantes
# ============================================================================
#
# OBJECTIF
# --------
# Préparer les panels après transformation sans imputer artificiellement
# les données destinées à l'estimation finale du DFM.
#
# STRATEGIE
# ---------
# 1. Diagnostiquer les valeurs manquantes après transformation.
#
# 2. Exclure uniquement les séries dont la couverture devient trop faible
#    après transformation.
#
# 3. Ne PAS imputer les NA dans le panel utilisé par le DFM final :
#       dfms::DFM(..., em.method = "BM")
#    gère les observations manquantes via l'estimation EM/QML.
#
# 4. Conserver une version "équilibrée" uniquement pour les étapes PCA
#    de sélection des facteurs et des retards (06 / 07).
#
# 5. Préserver les métadonnées :
#       - monthly_vars
#       - quarterly_vars
#       - target
#
# 6. La cible trimestrielle est toujours conservée, même si son taux de
#    couverture est inférieur au seuil de sécurité.
#
# SORTIES
# -------
# diagnostic_valeurs_manquantes.csv
# resume_valeurs_manquantes_secteur.csv
# missingness_<secteur>.png
# variables_exclues_securite_post_transfo.csv
# panels_stationnaires_filtres.rds
# diagnostic_global_filtrage.csv
# diagnostic_filtrage_par_secteur.csv
# diagnostic_frequences_apres_filtrage.csv
# ============================================================================


# ============================================================================
# 0. VERIFICATION DE L'ENVIRONNEMENT
# ============================================================================

if (!exists("OUT_DIR")) {
  stop(
    "OUT_DIR n'existe pas. Lance d'abord 01_setup.R."
  )
}

if (!exists("panels_stationnaires")) {
  stop(
    "panels_stationnaires n'existe pas. Lance d'abord 04_transformation.R."
  )
}

if (!exists("COUVERTURE_MIN_PANEL_STAT")) {
  stop(
    "COUVERTURE_MIN_PANEL_STAT n'est pas défini dans 01_setup.R."
  )
}

dir.create(
  FIG_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================================
# 1. DIAGNOSTIC DES VALEURS MANQUANTES
# ============================================================================

diagnostiquer_manquants <- function(panel, secteur) {
  
  n <- nrow(panel)
  
  data.frame(
    secteur = secteur,
    variable = colnames(panel),
    n_total = n,
    
    n_manquants = apply(
      panel,
      2,
      function(x) sum(!is.finite(x))
    ),
    
    pct_manquants = round(
      100 * apply(
        panel,
        2,
        function(x) mean(!is.finite(x))
      ),
      1
    )
  )
}


diag_manquants <- dplyr::bind_rows(
  lapply(
    names(panels_stationnaires),
    function(s) {
      
      diagnostiquer_manquants(
        panels_stationnaires[[s]],
        s
      )
    }
  )
)


write.csv(
  diag_manquants,
  file.path(
    OUT_DIR,
    "diagnostic_valeurs_manquantes.csv"
  ),
  row.names = FALSE
)


# ============================================================================
# 2. RESUME PAR SECTEUR
# ============================================================================

resume_manquants_secteur <- diag_manquants %>%
  
  dplyr::group_by(secteur) %>%
  
  dplyr::summarise(
    
    n_variables = dplyr::n(),
    
    pct_manquants_moyen =
      round(mean(pct_manquants), 1),
    
    pct_manquants_max =
      round(max(pct_manquants), 1),
    
    n_variables_couverture_faible =
      sum(
        pct_manquants >
          100 * (1 - COUVERTURE_MIN_PANEL_STAT)
      ),
    
    .groups = "drop"
  )


message(
  "\n=== TAUX DE VALEURS MANQUANTES APRES TRANSFORMATION ==="
)

print(
  resume_manquants_secteur
)


write.csv(
  resume_manquants_secteur,
  file.path(
    OUT_DIR,
    "resume_valeurs_manquantes_secteur.csv"
  ),
  row.names = FALSE
)


# ============================================================================
# 3. HEATMAP DE MISSINGNESS
# ============================================================================

tracer_missingness <- function(panel, secteur) {
  
  m <- as.data.frame(
    !is.finite(panel)
  )
  
  m$date <- zoo::index(panel)
  
  m_long <- tidyr::pivot_longer(
    m,
    -date,
    names_to = "variable",
    values_to = "manquant"
  )
  
  g <- ggplot(
    m_long,
    aes(
      x = date,
      y = variable,
      fill = manquant
    )
  ) +
    
    geom_tile() +
    
    scale_fill_manual(
      values = c(
        `FALSE` = "#2c7fb8",
        `TRUE` = "#f0f0f0"
      ),
      labels = c(
        "Observé",
        "Manquant"
      )
    ) +
    
    labs(
      title = paste(
        "Carte des valeurs manquantes —",
        secteur
      ),
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    
    theme_minimal() +
    
    theme(
      axis.text.y =
        element_text(size = 6)
    )
  
  
  ggsave(
    file.path(
      FIG_DIR,
      paste0(
        "missingness_",
        secteur,
        ".png"
      )
    ),
    g,
    width = 10,
    height = max(
      4,
      0.15 * ncol(panel)
    ),
    dpi = 150,
    limitsize = FALSE
  )
}


for (s in names(panels_stationnaires)) {
  
  message(
    "Heatmap missingness : ",
    s
  )
  
  tryCatch(
    
    tracer_missingness(
      panels_stationnaires[[s]],
      s
    ),
    
    error = function(e) {
      
      message(
        "  -> échec heatmap pour ",
        s,
        " : ",
        conditionMessage(e)
      )
    }
  )
}


# ============================================================================
# 4. FILTRE DE SECURITE POST-TRANSFORMATION
# ============================================================================

panels_stationnaires_filtres <-
  list()

variables_exclues_securite <-
  list()


for (s in names(panels_stationnaires)) {
  
  panel <- panels_stationnaires[[s]]
  
  
  # --------------------------------------------------------------------------
  # 4.1 Taux de valeurs manquantes
  # --------------------------------------------------------------------------
  
  pct_na <- apply(
    panel,
    2,
    function(x) mean(!is.finite(x))
  )
  
  
  # --------------------------------------------------------------------------
  # 4.2 Filtre de couverture
  # --------------------------------------------------------------------------
  
  garder <- pct_na <= (
    1 - COUVERTURE_MIN_PANEL_STAT
  )
  
  
  # --------------------------------------------------------------------------
  # 4.3 Protection de la cible
  # --------------------------------------------------------------------------
  
  target_name <- attr(
    panel,
    "target"
  )
  
  if (
    !is.null(target_name) &&
    target_name %in% colnames(panel)
  ) {
    
    idx_target <- match(
      target_name,
      colnames(panel)
    )
    
    garder[idx_target] <- TRUE
    
  } else {
    
    warning(
      "Cible introuvable pour le secteur : ",
      s
    )
  }
  
  
  # --------------------------------------------------------------------------
  # 4.3 bis Protection des indicateurs climatiques de l'Agriculture
  # --------------------------------------------------------------------------
  #
  # Ces deux séries sont considérées comme des indicateurs stratégiques
  # pour le nowcasting de la VA agricole.
  #
  # Elles sont conservées même si leur couverture est inférieure au seuil
  # général COUVERTURE_MIN_PANEL_STAT.
  #
  # IMPORTANT :
  # Cette exception concerne uniquement l'Agriculture.
  # --------------------------------------------------------------------------
  
  if (s == "Agriculture") {
    
    variables_climatiques_agriculture <- c(
      "Moyenne des precipitations",
      "Température_moyenne"
    )
    
    
    climatiques_presentes <- intersect(
      variables_climatiques_agriculture,
      colnames(panel)
    )
    
    
    if (length(climatiques_presentes) > 0) {
      
      idx_climatiques <- match(
        climatiques_presentes,
        colnames(panel)
      )
      
      garder[idx_climatiques] <- TRUE
      
      message(
        s,
        " : protection de ",
        length(climatiques_presentes),
        " variable(s) climatique(s) : ",
        paste(
          climatiques_presentes,
          collapse = ", "
        )
      )
    }
  }
  # --------------------------------------------------------------------------
  # 4.4 Enregistrement des séries exclues
  # --------------------------------------------------------------------------
  
  if (any(!garder)) {
    
    message(
      s,
      " : exclusion de ",
      sum(!garder),
      " variable(s) sous le seuil de couverture."
    )
    
    
    variables_exclues_securite[[s]] <-
      
      data.frame(
        
        secteur = s,
        
        variable =
          colnames(panel)[!garder],
        
        pct_manquants =
          round(
            100 * pct_na[!garder],
            1
          )
      )
    
  } else {
    
    message(
      s,
      " : aucune exclusion."
    )
  }
  
  
  # --------------------------------------------------------------------------
  # 4.5 Construction du panel filtré
  # --------------------------------------------------------------------------
  
  panel_filtre <-
    
    panel[
      ,
      garder,
      drop = FALSE
    ]
  
  
  # --------------------------------------------------------------------------
  # 4.6 Mise à jour des métadonnées
  # --------------------------------------------------------------------------
  
  monthly_vars <- intersect(
    attr(panel, "monthly_vars"),
    colnames(panel_filtre)
  )
  
  
  quarterly_vars <- intersect(
    attr(panel, "quarterly_vars"),
    colnames(panel_filtre)
  )
  
  
  # La cible doit toujours rester identifiée
  # comme cible du modèle.
  
  attr(
    panel_filtre,
    "monthly_vars"
  ) <- monthly_vars
  
  
  attr(
    panel_filtre,
    "quarterly_vars"
  ) <- quarterly_vars
  
  
  attr(
    panel_filtre,
    "target"
  ) <- target_name
  
  
  # --------------------------------------------------------------------------
  # 4.7 Sauvegarde dans la liste finale
  # --------------------------------------------------------------------------
  
  panels_stationnaires_filtres[[s]] <-
    panel_filtre
}


# ============================================================================
# 5. SAUVEGARDE DES VARIABLES EXCLUES
# ============================================================================

if (
  length(variables_exclues_securite) > 0
) {
  
  write.csv(
    
    dplyr::bind_rows(
      variables_exclues_securite
    ),
    
    file.path(
      OUT_DIR,
      "variables_exclues_securite_post_transfo.csv"
    ),
    
    row.names = FALSE
  )
  
} else {
  
  message(
    "Aucune variable exclue au filtre de sécurité post-transformation."
  )
}


# ============================================================================
# 5 BIS. LISTE DES VARIABLES RETENUES PAR SECTEUR
# ============================================================================
#
# Objectif :
#   Produire la liste exhaustive des séries ayant survécu au filtre
#   de couverture post-transformation.
#
# Cette sortie permet de connaître précisément les variables finalement
# utilisées dans les panels destinés aux étapes suivantes du pipeline.
#
# Sortie :
#   variables_retenues_post_transfo_par_secteur.csv
# ============================================================================


variables_retenues_post_transfo <-
  dplyr::bind_rows(
    
    lapply(
      
      names(panels_stationnaires_filtres),
      
      function(s) {
        
        panel <-
          panels_stationnaires_filtres[[s]]
        
        
        # Métadonnées
        monthly_vars <-
          attr(
            panel,
            "monthly_vars"
          )
        
        quarterly_vars <-
          attr(
            panel,
            "quarterly_vars"
          )
        
        target_name <-
          attr(
            panel,
            "target"
          )
        
        
        # Variables conservées
        variables <-
          colnames(panel)
        
        
        # Détermination de la fréquence
        frequence <-
          ifelse(
            variables %in% monthly_vars,
            "Mensuelle",
            ifelse(
              variables %in% quarterly_vars,
              "Trimestrielle",
              "Autre"
            )
          )
        
        
        data.frame(
          
          secteur = s,
          
          variable = variables,
          
          frequence = frequence,
          
          cible = target_name,
          
          stringsAsFactors = FALSE
        )
      }
    )
  )


# Affichage
message(
  "\n=== VARIABLES RETENUES PAR SECTEUR ==="
)

print(
  variables_retenues_post_transfo,
  row.names = FALSE
)


# Sauvegarde
write.csv(
  
  variables_retenues_post_transfo,
  
  file.path(
    OUT_DIR,
    "variables_retenues_post_transfo_par_secteur.csv"
  ),
  
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ============================================================================
# 6. SAUVEGARDE DU PANEL FINAL
# ============================================================================

saveRDS(
  
  panels_stationnaires_filtres,
  
  file.path(
    OUT_DIR,
    "panels_stationnaires_filtres.rds"
  )
)


# ============================================================================
# 7. FONCTION DE BALANCEMENT POUR LA PCA
# ============================================================================
#
# IMPORTANT :
# Cette fonction n'est PAS utilisée pour le DFM final.
#
# Elle sert uniquement à obtenir temporairement un panel sans NA pour :
#   - PCA
#   - sélection de r
#   - sélection de p
#
# L'interpolation ne modifie donc pas les données utilisées dans le DFM.
# ============================================================================

balance_pour_ic <- function(panel_stat) {
  
  p <- zoo::na.approx(
    panel_stat,
    na.rm = FALSE
  )
  
  p <- zoo::na.locf(
    p,
    na.rm = FALSE
  )
  
  p <- zoo::na.locf(
    p,
    fromLast = TRUE,
    na.rm = FALSE
  )
  
  p <- p[
    complete.cases(p),
    ,
    drop = FALSE
  ]
  
  p
}


# ============================================================================
# 8. DIAGNOSTIC GLOBAL DU FILTRAGE
# ============================================================================

diagnostic_filtrage <- dplyr::bind_rows(
  
  lapply(
    
    names(panels_stationnaires),
    
    function(s) {
      
      panel_avant <-
        panels_stationnaires[[s]]
      
      panel_apres <-
        panels_stationnaires_filtres[[s]]
      
      
      n_avant <-
        ncol(panel_avant)
      
      n_apres <-
        ncol(panel_apres)
      
      n_supprimees <-
        n_avant - n_apres
      
      
      data.frame(
        
        secteur = s,
        
        n_series_avant =
          n_avant,
        
        n_series_supprimees =
          n_supprimees,
        
        n_series_restantes =
          n_apres,
        
        pct_supprimees =
          round(
            100 *
              n_supprimees /
              n_avant,
            1
          ),
        
        pct_restantes =
          round(
            100 *
              n_apres /
              n_avant,
            1
          )
      )
    }
  )
)


# ============================================================================
# 9. DIAGNOSTIC GLOBAL
# ============================================================================

n_total_avant <-
  sum(
    diagnostic_filtrage$n_series_avant
  )


n_total_supprimees <-
  sum(
    diagnostic_filtrage$n_series_supprimees
  )


n_total_restantes <-
  sum(
    diagnostic_filtrage$n_series_restantes
  )


diagnostic_global_filtrage <-
  
  data.frame(
    
    n_series_avant =
      n_total_avant,
    
    n_series_supprimees =
      n_total_supprimees,
    
    n_series_restantes =
      n_total_restantes,
    
    pct_supprimees =
      round(
        100 *
          n_total_supprimees /
          n_total_avant,
        1
      ),
    
    pct_restantes =
      round(
        100 *
          n_total_restantes /
          n_total_avant,
        1
      )
  )


message(
  "\n=== DIAGNOSTIC GLOBAL DU FILTRAGE ==="
)

print(
  diagnostic_global_filtrage
)


message(
  "\n=== DIAGNOSTIC DU FILTRAGE PAR SECTEUR ==="
)

print(
  diagnostic_filtrage
)


# ============================================================================
# 10. SAUVEGARDE DES DIAGNOSTICS
# ============================================================================

write.csv(
  
  diagnostic_global_filtrage,
  
  file.path(
    OUT_DIR,
    "diagnostic_global_filtrage.csv"
  ),
  
  row.names = FALSE
)


write.csv(
  
  diagnostic_filtrage,
  
  file.path(
    OUT_DIR,
    "diagnostic_filtrage_par_secteur.csv"
  ),
  
  row.names = FALSE
)


# ============================================================================
# 11. VERIFICATION DE COHERENCE
# ============================================================================

if (
  n_total_avant !=
  n_total_supprimees +
  n_total_restantes
) {
  
  warning(
    "Incohérence détectée dans le bilan du filtrage."
  )
  
} else {
  
  message(
    "\n✓ Vérification de cohérence : OK"
  )
}


# ============================================================================
# 12. DIAGNOSTIC DES FREQUENCES APRES FILTRAGE
# ============================================================================

diagnostic_frequences <- dplyr::bind_rows(
  
  lapply(
    
    names(panels_stationnaires_filtres),
    
    function(s) {
      
      panel <-
        panels_stationnaires_filtres[[s]]
      
      
      monthly_vars <-
        attr(
          panel,
          "monthly_vars"
        )
      
      
      quarterly_vars <-
        attr(
          panel,
          "quarterly_vars"
        )
      
      
      target_name <-
        attr(
          panel,
          "target"
        )
      
      
      data.frame(
        
        secteur = s,
        
        n_mensuelles =
          sum(
            colnames(panel) %in%
              monthly_vars
          ),
        
        n_trimestrielles =
          sum(
            colnames(panel) %in%
              quarterly_vars
          ),
        
        n_total =
          ncol(panel),
        
        cible =
          target_name
      )
    }
  )
)


message(
  "\n=== FREQUENCES DES SERIES APRES FILTRAGE ==="
)

print(
  diagnostic_frequences
)


write.csv(
  
  diagnostic_frequences,
  
  file.path(
    OUT_DIR,
    "diagnostic_frequences_apres_filtrage.csv"
  ),
  
  row.names = FALSE
)


# ============================================================================
# 13. CONTROLE FINAL DES METADONNEES
# ============================================================================

message(
  "\n=== CONTROLE FINAL DES METADONNEES ==="
)


for (s in names(panels_stationnaires_filtres)) {
  
  panel <-
    panels_stationnaires_filtres[[s]]
  
  
  monthly_vars <-
    attr(
      panel,
      "monthly_vars"
    )
  
  
  quarterly_vars <-
    attr(
      panel,
      "quarterly_vars"
    )
  
  
  target_name <-
    attr(
      panel,
      "target"
    )
  
  
  message(
    s,
    " : ",
    length(monthly_vars),
    " mensuelles | ",
    length(quarterly_vars),
    " trimestrielles | cible = ",
    target_name
  )
}


# ============================================================================
# 14. FIN
# ============================================================================

message(
  "\n============================================================"
)

message(
  "05_missing_data.R terminé."
)

message(
  "✓ Diagnostic des NA effectué."
)

message(
  "✓ Séries à couverture insuffisante exclues."
)

message(
  "✓ Cible protégée."
)

message(
  "✓ Métadonnées conservées."
)

message(
  "✓ NA conservés pour l'estimation finale du DFM."
)

message(
  "✓ Panel équilibré disponible uniquement pour PCA."
)

message(
  "============================================================"
)