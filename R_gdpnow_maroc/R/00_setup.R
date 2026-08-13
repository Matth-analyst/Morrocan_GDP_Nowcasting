# ============================================================================
# 00_setup.R -- Configuration generale du pipeline GDPNow-Maroc
# ============================================================================
# Reference methodologique principale :
#   Higgins, P. (2014) "GDPNow: A Model for GDP Nowcasting", FRB Atlanta WP 2014-7
#   Banbura, M., Giannone, D., Reichlin, L. (2010) "Large Bayesian VARs",
#     Journal of Applied Econometrics, 25(1)
#   Litterman, R. (1986) "Forecasting with Bayesian Vector Autoregressions"
#   Diebold, F.X., Mariano, R.S. (1995) "Comparing Predictive Accuracy"
# ============================================================================

Sys.setlocale("LC_ALL", "C.UTF-8")

suppressMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(purrr)
  library(stringr)
  library(zoo)
  library(tseries)
  library(forecast)
})

# --- Chemins (a adapter si le classeur est ailleurs) -----------------------
CHEMIN_CLASSEUR <- "data/Series_retenues_modelisation.xlsx"
DOSSIER_FIGURES <- "figures"
DOSSIER_RESULTATS <- "resultats"
dir.create(DOSSIER_FIGURES, showWarnings = FALSE, recursive = TRUE)
dir.create(DOSSIER_RESULTATS, showWarnings = FALSE, recursive = TRUE)

# --- Nomenclature des 16 branches (HCP), par groupe -------------------------
BRANCHES_COUVERTES <- c("Pêche", "Industrie d'extraction", "Industrie de transformation",
  "Électricité, gaz, eau", "Immobilier", "Hébergement-restauration", "Finances et assurances")

BRANCHES_NON_COUVERTES <- c("Agriculture", "Construction", "Commerce", "Transports",
  "Information-communication", "Administration publique", "Éducation-santé",
  "Services aux entreprises", "Autres services")

TOUTES_BRANCHES <- c(BRANCHES_COUVERTES, BRANCHES_NON_COUVERTES)

# --- theme graphique commun --------------------------------------------------
theme_gdpnow <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 9.5),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )
theme_set(theme_gdpnow)

cat("Configuration chargee.", length(TOUTES_BRANCHES), "branches definies (",
    length(BRANCHES_COUVERTES), "couvertes,", length(BRANCHES_NON_COUVERTES), "non couvertes).\n")
