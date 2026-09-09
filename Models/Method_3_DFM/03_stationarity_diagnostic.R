# ============================================================================
# GDPNow Maroc — 03_stationarity_diagnostic.R
# Diagnostic de stationnarité (ADF) sur les séries AU NIVEAU
# ============================================================================
# Objectif : avant toute decision de transformation, quantifier precisement
# combien de series sont non stationnaires au niveau, secteur par secteur
# et globalement. Ce fichier ne modifie AUCUNE donnee -- il ne fait que
# tester et rapporter. La decision de transformation est prise dans
# 04_transformation.R, sur la base de ce diagnostic.
#
# Sorties (dans OUT_DIR / FIG_DIR) :
#   - diagnostic_stationnarite_niveau.csv : 1 ligne par variable
#   - diagnostic_stationnarite_resume_secteur.csv : % non stationnaire / secteur
#   - figures/stationnarite_par_secteur.png : barplot
# ============================================================================


# ============================================================================
# GDPNow Maroc — 03_stationarity_diagnostic.R
# Diagnostic de stationnarité (ADF) sur les séries AU NIVEAU
# ============================================================================


# --------------------------------------------------------------------------
# 1. Test ADF sécurisé
# --------------------------------------------------------------------------

adf_pvalue_safe <- function(x) {
  
  x <- as.numeric(x)
  
  x <- x[is.finite(x)]
  
  if (length(x) < N_MIN_ADF) {
    return(NA_real_)
  }
  
  out <- tryCatch(
    
    suppressWarnings(
      tseries::adf.test(
        x,
        alternative = "stationary"
      )
    ),
    
    error = function(e) NULL
  )
  
  if (is.null(out)) {
    return(NA_real_)
  }
  
  out$p.value
}



# --------------------------------------------------------------------------
# 2. Diagnostic d'une série
# --------------------------------------------------------------------------

diagnostiquer_serie <- function(
    x,
    nom,
    secteur,
    frequence
) {
  
  x <- as.numeric(x)
  
  valid <- is.finite(x)
  
  n_valid <- sum(valid)
  
  
  # ------------------------------------------------------------------------
  # Pas suffisamment d'observations
  # ------------------------------------------------------------------------
  
  if (n_valid == 0) {
    
    return(
      data.frame(
        secteur = secteur,
        variable = nom,
        frequence = frequence,
        n_obs = 0,
        pct_positif = NA_real_,
        pval_ADF_niveau = NA_real_,
        conclusion = "aucune observation valide"
      )
    )
  }
  
  
  pct_positif <- mean(
    x[valid] > 0
  )
  
  
  if (n_valid < N_MIN_ADF) {
    
    return(
      data.frame(
        secteur = secteur,
        variable = nom,
        frequence = frequence,
        n_obs = n_valid,
        pct_positif = pct_positif,
        pval_ADF_niveau = NA_real_,
        conclusion = "test ADF non fiable (n < 15)"
      )
    )
  }
  
  
  # ------------------------------------------------------------------------
  # Test ADF
  # ------------------------------------------------------------------------
  
  pval <- adf_pvalue_safe(x)
  
  
  conclusion <- if (is.na(pval)) {
    
    "test ADF echoue"
    
  } else if (pval < ADF_SEUIL) {
    
    "stationnaire au niveau"
    
  } else {
    
    "NON stationnaire au niveau"
  }
  
  
  data.frame(
    secteur = secteur,
    variable = nom,
    frequence = frequence,
    n_obs = n_valid,
    pct_positif = pct_positif,
    pval_ADF_niveau = pval,
    conclusion = conclusion
  )
}



# --------------------------------------------------------------------------
# 3. Diagnostic de tous les secteurs
# --------------------------------------------------------------------------

diagnostiquer_stationnarite <- function(panels) {
  
  lignes <- list()
  
  
  for (s in names(panels)) {
    
    panel <- panels[[s]]
    
    if (is.null(panel)) {
      next
    }
    
    
    vars_m <- attr(
      panel,
      "monthly_vars"
    )
    
    vars_q <- attr(
      panel,
      "quarterly_vars"
    )
    
    
    message(
      "Diagnostic ADF : ",
      s,
      " (",
      ncol(panel),
      " variables)"
    )
    
    
    for (j in seq_len(ncol(panel))) {
      
      nom <- colnames(panel)[j]
      
      
      # --------------------------------------------------------------
      # Identification de la fréquence
      # --------------------------------------------------------------
      
      if (nom %in% vars_m) {
        
        frequence <- "mensuelle"
        
      } else if (nom %in% vars_q) {
        
        frequence <- "trimestrielle"
        
      } else {
        
        frequence <- "inconnue"
      }
      
      
      # --------------------------------------------------------------
      # Pour les séries trimestrielles :
      # on conserve uniquement les observations réellement publiées
      # --------------------------------------------------------------
      
      x <- panel[, j]
      
      
      lignes[[paste(s, j)]] <- diagnostiquer_serie(
        x = x,
        nom = nom,
        secteur = s,
        frequence = frequence
      )
    }
  }
  
  
  dplyr::bind_rows(lignes)
}



# --------------------------------------------------------------------------
# 4. Chargement des données brutes
# --------------------------------------------------------------------------

panels_bruts <- charger_tous_les_panels(
  SECTEURS
)



# --------------------------------------------------------------------------
# 5. Diagnostic
# --------------------------------------------------------------------------

diagnostic_niveau <- diagnostiquer_stationnarite(
  panels_bruts
)



# --------------------------------------------------------------------------
# 6. Sauvegarde du diagnostic détaillé
# --------------------------------------------------------------------------

write.csv(
  diagnostic_niveau,
  file.path(
    OUT_DIR,
    "diagnostic_stationnarite_niveau.csv"
  ),
  row.names = FALSE
)



# --------------------------------------------------------------------------
# 7. Résumé par secteur
# --------------------------------------------------------------------------

resume_secteur <- diagnostic_niveau %>%
  
  group_by(secteur) %>%
  
  summarise(
    
    n_variables = n(),
    
    n_stationnaires =
      sum(
        conclusion ==
          "stationnaire au niveau"
      ),
    
    n_non_stationnaires =
      sum(
        conclusion ==
          "NON stationnaire au niveau"
      ),
    
    n_tests_non_fiables =
      sum(
        conclusion %in%
          c(
            "test ADF non fiable (n < 15)",
            "test ADF echoue",
            "aucune observation valide"
          )
      ),
    
    pct_non_stationnaire =
      round(
        100 *
          n_non_stationnaires /
          n_variables,
        1
      ),
    
    .groups = "drop"
  )



# --------------------------------------------------------------------------
# 8. Sauvegarde du résumé
# --------------------------------------------------------------------------

write.csv(
  resume_secteur,
  file.path(
    OUT_DIR,
    "diagnostic_stationnarite_resume_secteur.csv"
  ),
  row.names = FALSE
)



# --------------------------------------------------------------------------
# 9. Résumé global
# --------------------------------------------------------------------------

resume_global <- diagnostic_niveau %>%
  
  summarise(
    
    n_variables_total = n(),
    
    pct_stationnaire =
      round(
        100 *
          mean(
            conclusion ==
              "stationnaire au niveau"
          ),
        1
      ),
    
    pct_non_stationnaire =
      round(
        100 *
          mean(
            conclusion ==
              "NON stationnaire au niveau"
          ),
        1
      ),
    
    pct_non_fiable =
      round(
        100 *
          mean(
            conclusion %in%
              c(
                "test ADF non fiable (n < 15)",
                "test ADF echoue",
                "aucune observation valide"
              )
          ),
        1
      )
  )



# --------------------------------------------------------------------------
# 10. Affichage
# --------------------------------------------------------------------------

message(
  "\n=== Diagnostic global de stationnarité ==="
)

print(
  resume_global
)

message(
  "\n=== Détail par secteur ==="
)

print(
  resume_secteur
)



# --------------------------------------------------------------------------
# 11. Graphique
# --------------------------------------------------------------------------

resume_long <- resume_secteur %>%
  
  select(
    secteur,
    n_stationnaires,
    n_non_stationnaires,
    n_tests_non_fiables
  ) %>%
  
  tidyr::pivot_longer(
    -secteur,
    names_to = "categorie",
    values_to = "n"
  )


g_stat <- ggplot(
  resume_long,
  aes(
    x = secteur,
    y = n,
    fill = categorie
  )
) +
  
  geom_col(
    position = "stack"
  ) +
  
  coord_flip() +
  
  labs(
    title =
      "Diagnostic ADF au niveau, par secteur",
    x = NULL,
    y = "Nombre de variables",
    fill = NULL
  ) +
  
  theme_minimal()


ggsave(
  file.path(
    FIG_DIR,
    "stationnarite_par_secteur.png"
  ),
  g_stat,
  width = 9,
  height = 6,
  dpi = 150
)


message(
  "\nDiagnostic de stationnarité terminé."
)