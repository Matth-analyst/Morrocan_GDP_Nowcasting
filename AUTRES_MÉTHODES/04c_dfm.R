# ============================================================================
# 04c_dfm.R -- Modele a facteurs dynamiques (DFM) pour les branches couvertes
# ============================================================================
# NOUVEAU SCRIPT -- n'affecte aucun fichier existant du pipeline. A placer
# dans R_gdpnow_maroc/R/, a lancer APRES 01_import_donnees.R.
#
# Necessite le package 'dfms' (Doz, Giannone & Reichlin, algorithme EM,
# gere nativement les donnees manquantes / ragged edges) :
#   install.packages("dfms")
#
# Reference methodologique :
#   Doz, C., Giannone, D., Reichlin, L. (2011) "A Two-Step Estimator for
#     Large Approximate Dynamic Factor Models Based on Kalman Filtering",
#     Journal of Econometrics
#   Stock, J.H., Watson, M.W. (2002) "Macroeconomic Forecasting Using
#     Diffusion Indexes"
#
# Principe : pour chaque branche couverte disposant d'au moins 2 indicateurs
# mensuels, on extrait UN facteur commun latent (r = 1) qui resume
# l'information partagee par ces indicateurs, relie ensuite a la croissance
# de la VA par une bridge equation simple.
#
# ----------------------------------------------------------------------------
# CORRECTIF (suite a l'erreur "colMeans(x, na.rm = TRUE) : 'x' doit etre
# numerique") :
#
# Le panel large est construit par pivot_wider(names_from = indicateur,
# values_from = dlog). Si un meme indicateur a PLUSIEURS lignes pour la
# MEME date (doublon dans indicateurs.rds -- ex. deux colonnes source
# portant le meme nom d'indicateur dans le classeur), pivot_wider ne peut
# pas les fusionner en une seule valeur numerique : il produit une
# "list-col" (une cellule contenant un vecteur au lieu d'un nombre). La
# colonne resultante n'est alors plus numerique, ce qui fait planter
# scale() (colMeans) des le debut du script -- pas une panne du DFM
# lui-meme, mais une donnee source dupliquee en amont.
#
# Correctif : avant le pivot, on regroupe explicitement par (indicateur,
# date) et on moyenne les eventuels doublons -- reproductible et sans
# perte d'information (les valeurs dupliquees pour une meme date/indic.
# sont presque toujours des repetitions quasi identiques dans ce type de
# classeur). On reconstruit ensuite un calendrier mensuel complet (comme
# pour 04b_midas.R) afin que le DFM recoive une grille temporelle
# reguliere -- les eventuels trous restent NA, ce que le DFM (methode EM,
# Doz-Giannone-Reichlin) gere nativement, contrairement au lm() du MIDAS.
# ============================================================================
source("R/00_setup.R")

if (!requireNamespace("dfms", quietly = TRUE)) {
  install.packages("dfms")
}
library(dfms)

cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
indicateurs <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs.rds"))

NB_FACTEURS <- 1   # un seul facteur commun par branche (panels sectoriels courts)
NB_RETARDS_VAR_FACTEUR <- 1

resultats_dfm <- list()

for (b in BRANCHES_COUVERTES) {
  
  indics_b <- indicateurs %>% filter(branche == b, frequence == "mensuel")
  nb_series <- n_distinct(indics_b$indicateur)
  if (nb_series < 2) {
    cat(sprintf("%-28s | %d indicateur(s) mensuel(s) seulement -> DFM non applicable (min. 2)\n", b, nb_series))
    next
  }
  
  # --- Deduplication (indicateur, date) : moyenne des doublons eventuels ---
  # (correctif -- voir explication en tete de script)
  n_avant <- nrow(indics_b)
  indics_b <- indics_b %>%
    group_by(indicateur, date) %>%
    summarise(valeur = mean(valeur, na.rm = TRUE), .groups = "drop")
  n_apres <- nrow(indics_b)
  if (n_avant != n_apres) {
    cat(sprintf("    (%s : %d doublons (indicateur, date) fusionnes par moyenne)\n", b, n_avant - n_apres))
  }
  
  # --- Panel mensuel large (dates x indicateurs), en taux de croissance ----
  panel_long <- indics_b %>%
    group_by(indicateur) %>%
    arrange(date) %>%
    mutate(dlog = c(NA, diff(log(pmax(valeur, 1e-6))))) %>%
    ungroup() %>%
    select(date, indicateur, dlog)
  
  panel <- panel_long %>%
    pivot_wider(names_from = indicateur, values_from = dlog, values_fn = mean) %>%
    arrange(date)
  
  # --- Reindexation sur un calendrier mensuel complet (pas de trou de date
  #     implicite dans la grille temporelle globale du panel) ---
  calendrier <- tibble(date = seq(min(panel$date), max(panel$date), by = "month"))
  panel <- calendrier %>% left_join(panel, by = "date") %>% arrange(date)
  
  mat <- as.matrix(panel[, -1, drop = FALSE])
  storage.mode(mat) <- "double"  # garantit une matrice numerique (filet de securite)
  if (nrow(mat) < 24) {
    cat(sprintf("%-28s | moins de 24 mois de donnees -> DFM non calcule\n", b))
    next
  }
  
  # Standardisation : indispensable pour le DFM, les indicateurs ont des
  # echelles tres differentes (indices, volumes, effectifs...). scale()
  # ignore automatiquement les NA (na.rm = TRUE implicite pour colMeans/sd).
  mat_std <- scale(mat)
  
  fit_dfm <- tryCatch(
    DFM(mat_std, r = NB_FACTEURS, p = NB_RETARDS_VAR_FACTEUR, max.iter = 100),
    error = function(e) { cat(sprintf("%-28s | echec DFM (%s)\n", b, conditionMessage(e))); NULL }
  )
  if (is.null(fit_dfm)) next
  
  # NOTE : le DFM (methode a espace d'etats, Doz-Giannone-Reichlin) conditionne
  # l'estimation sur les NB_RETARDS_VAR_FACTEUR premieres observations pour
  # initialiser le VAR du facteur -- F_qml peut donc avoir MOINS de lignes que
  # panel$date (typiquement panel$date moins p). On aligne en gardant les
  # dates les PLUS RECENTES (celles qui correspondent bien aux valeurs
  # estimees), plutot que de supposer une correspondance ligne a ligne stricte.
  n_facteur <- nrow(fit_dfm$F_qml)
  dates_facteur <- tail(panel$date, n_facteur)
  if (n_facteur != nrow(panel)) {
    cat(sprintf("    (%s : facteur estime sur %d mois sur %d -- %d observation(s) initiale(s) utilisee(s) pour conditionner le VAR)\n",
                b, n_facteur, nrow(panel), nrow(panel) - n_facteur))
  }
  facteur <- tibble(date = dates_facteur, facteur = as.numeric(fit_dfm$F_qml[, 1]))
  
  # --- Agregation trimestrielle du facteur (moyenne des mois du trimestre) -
  facteur_trim <- facteur %>%
    mutate(trimestre = floor_date(date, "quarter")) %>%
    group_by(trimestre) %>%
    summarise(facteur = mean(facteur, na.rm = TRUE)) %>%
    rename(date = trimestre)
  
  # --- Bridge equation : croissance VA ~ facteur commun --------------------
  cible_b <- cibles %>% filter(branche == b) %>% select(date, dlog_va)
  df <- inner_join(cible_b, facteur_trim, by = "date") %>%
    filter(!is.na(dlog_va), !is.na(facteur))
  if (nrow(df) < 12) {
    cat(sprintf("%-28s | echantillon trimestriel insuffisant pour la bridge sur facteur\n", b))
    next
  }
  fit_bridge_facteur <- lm(dlog_va ~ facteur, data = df)
  
  # --- Prevision du facteur au prochain trimestre (AR(1) mensuel, 3 mois) --
  facteur_pour_ar1 <- facteur$facteur[!is.na(facteur$facteur)]
  facteur_ar1 <- tryCatch(Arima(facteur_pour_ar1, order = c(1, 0, 0)), error = function(e) NULL)
  facteur_prevu_mensuel <- if (!is.null(facteur_ar1)) {
    as.numeric(forecast(facteur_ar1, h = 3)$mean)
  } else {
    rep(tail(facteur_pour_ar1, 1), 3)
  }
  facteur_prevu_trim <- mean(facteur_prevu_mensuel)
  
  prevision_dfm <- as.numeric(predict(fit_bridge_facteur, newdata = data.frame(facteur = facteur_prevu_trim)))
  
  resultats_dfm[[b]] <- list(
    fit_dfm = fit_dfm,
    fit_bridge = fit_bridge_facteur,
    facteur_mensuel = facteur,
    prevision_dfm = prevision_dfm,
    r2_bridge = summary(fit_bridge_facteur)$r.squared,
    nb_indicateurs = ncol(mat_std)
  )
  
  cat(sprintf("%-28s | DFM=%+.4f (r2 bridge sur facteur=%.2f, %d indicateurs mensuels)\n",
              b, prevision_dfm, resultats_dfm[[b]]$r2_bridge, ncol(mat_std)))
}

saveRDS(resultats_dfm, file.path(DOSSIER_RESULTATS, "dfm_equations.rds"))
cat("\nModeles a facteurs dynamiques sauvegardes pour", length(resultats_dfm), "branches couvertes.\n")