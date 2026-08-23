# ============================================================================
# 04b_midas.R -- MIDAS (U-MIDAS) pour les branches couvertes
# ============================================================================
# NOUVEAU SCRIPT -- n'affecte aucun fichier existant du pipeline. A placer
# dans R_gdpnow_maroc/R/, a lancer APRES 01_import_donnees.R et
# 02_analyse_exploratoire.R (qui produisent cibles_avec_dlog.rds et
# indicateurs.rds dans resultats/).
#
# Reference methodologique :
#   Ghysels, Santa-Clara & Valkanov (2004) "The MIDAS touch"
#   Foroni, Marcellino & Schumacher (2015) "Unrestricted MIDAS (U-MIDAS)",
#     Journal of the Royal Statistical Society, Series A
#
# Principe : contrairement a la bridge equation actuelle (04_bridge_equations.R)
# qui agrege chaque indicateur mensuel en trimestriel par une simple moyenne
# (perte d'information intra-trimestrielle), le U-MIDAS garde les 3 mois du
# trimestre comme 3 regresseurs distincts :
#
#   dlog_VA_t = b0 + b1*dlog_indic_(mois1) + b2*dlog_indic_(mois2)
#               + b3*dlog_indic_(mois3) + erreur
#
# On estime cette regression pour chaque indicateur mensuel d'une branche
# couverte, puis on moyenne les previsions individuelles -- meme logique
# d'agregation que celle deja utilisee pour les bridge equations.
#
# ----------------------------------------------------------------------------
# TRAITEMENT DES VALEURS MANQUANTES (ajoute suite a l'audit du classeur
# Series_retenues_modelisation.xlsx) :
#
# 01_import_donnees.R supprime les lignes ou la valeur est manquante --
# les mois absents disparaissent donc silencieusement de la serie au lieu
# d'etre marques NA. Consequence concrete pour Peche (4 indicateurs sur 6
# avec ~18-19% de mois manquants, trous internes disperses -- pas
# seulement en debut/fin de serie) et Immobilier (indicateurs trimestriels
# ANCFCC, 10/43 trimestres manquants) : un simple diff(log(valeur)) sur
# la serie compressee calculerait parfois un taux de croissance entre deux
# mois NON CONSECUTIFS (ex. octobre 2010 -> fevrier 2011) sans le savoir,
# ce qui fausse silencieusement le U-MIDAS pour ces branches.
#
# Solution : avant tout calcul de Δlog, on reconstruit un calendrier
# mensuel complet (min a max) et on interpole lineairement les trous
# COURTS (<= MAXGAP_INTERPOLATION mois consecutifs manquants) -- pratique
# standard pour des series macro infra-annuelles avec trous ponctuels.
# Les trous plus longs, et toute valeur manquante en debut/fin de serie
# (qu'on ne peut pas extrapoler de facon fiable), restent NA et sont
# naturellement exclus de la regression par le filtre already-present
# (if_all(all_of(cols_m), ~ !is.na(.))).
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
indicateurs <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs.rds"))  # donnees brutes (mensuel + trimestriel)

MAXGAP_INTERPOLATION <- 3  # nb max de mois consecutifs manquants tolere pour interpolation lineaire

#' Reconstruit un calendrier mensuel complet pour un indicateur (a partir
#' de ses dates min/max reellement observees) et interpole lineairement
#' les trous internes courts. Renvoie un tibble (date, valeur) SANS trou
#' dans la sequence de dates -- indispensable avant de calculer un Δlog
#' mois-a-mois fiable.
completer_serie_mensuelle <- function(indic_df) {
  indic_df <- indic_df %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
  calendrier <- tibble(date = seq(min(indic_df$date), max(indic_df$date), by = "month"))
  serie <- calendrier %>% left_join(indic_df %>% select(date, valeur), by = "date")
  
  n_manquants <- sum(is.na(serie$valeur))
  if (n_manquants > 0) {
    serie$valeur <- zoo::na.approx(serie$valeur, x = serie$date, na.rm = FALSE,
                                   maxgap = MAXGAP_INTERPOLATION)
  }
  serie
}

#' Met en forme un indicateur mensuel en 3 colonnes (m1, m2, m3), une par
#' mois du trimestre, alignees sur le trimestre correspondant. Travaille
#' sur la serie deja completee (sans trou de calendrier).
construire_midas_indicateur <- function(indic_df_complet) {
  indic_df_complet <- indic_df_complet %>%
    arrange(date) %>%
    mutate(dlog = c(NA, diff(log(pmax(valeur, 1e-6)))),
           trimestre = floor_date(date, "quarter"),
           mois_dans_trim = interval(trimestre, date) %/% months(1) + 1)
  
  indic_df_complet %>%
    select(trimestre, mois_dans_trim, dlog) %>%
    pivot_wider(names_from = mois_dans_trim, values_from = dlog, names_prefix = "m") %>%
    arrange(trimestre)
}

#' Ajuste un U-MIDAS pour un indicateur donne et renvoie la prevision au
#' prochain trimestre. Si un mois du trimestre a venir n'est pas encore
#' publie (pas interpolable, en bout de serie), on le complete par un
#' AR(1) sur l'indicateur lui-meme (meme repli que celui deja utilise pour
#' l'indicateur dans la bridge equation classique).
midas_un_indicateur <- function(cible_df, indic_df_brut) {
  if (nrow(indic_df_brut) < 24) return(NULL)  # trop peu d'observations brutes, meme avant completion
  
  serie_complete <- completer_serie_mensuelle(indic_df_brut)
  taux_manquant <- mean(is.na(serie_complete$valeur))
  if (taux_manquant > 0.30) return(NULL)  # trop de trous non interpolables (> 3 mois consecutifs, trop frequents)
  
  large <- construire_midas_indicateur(serie_complete)
  cols_m <- intersect(c("m1", "m2", "m3"), names(large))
  if (length(cols_m) < 2) return(NULL)  # pas assez de granularite mensuelle exploitable
  
  df <- cible_df %>%
    rename(trimestre = date) %>%
    inner_join(large, by = "trimestre") %>%
    filter(!is.na(dlog_va))
  df_complet <- df %>% filter(if_all(all_of(cols_m), ~ !is.na(.)))
  if (nrow(df_complet) < 12) return(NULL)  # au moins 12 trimestres complets pour estimer
  
  formule <- as.formula(paste("dlog_va ~", paste(cols_m, collapse = " + ")))
  fit <- lm(formule, data = df_complet)
  
  # --- Prevision : complete les mois manquants du trimestre a venir --------
  derniere <- tail(large, 1)
  serie_m_pour_ar1 <- serie_complete %>%
    mutate(dlog = c(NA, diff(log(pmax(valeur, 1e-6))))) %>% pull(dlog) %>% na.omit()
  ar1_indic <- tryCatch(Arima(serie_m_pour_ar1, order = c(1, 0, 0)), error = function(e) NULL)
  
  x_new <- list()
  for (cc in cols_m) {
    val <- derniere[[cc]]
    if (is.null(val) || is.na(val)) {
      val <- if (!is.null(ar1_indic)) as.numeric(forecast(ar1_indic, h = 1)$mean) else tail(serie_m_pour_ar1, 1)
    }
    x_new[[cc]] <- val
  }
  
  prevision <- as.numeric(predict(fit, newdata = as.data.frame(x_new)))
  list(fit = fit, prevision = prevision, r2 = summary(fit)$r.squared, n = nrow(df_complet),
       taux_manquant = taux_manquant)
}

# ----------------------------------------------------------------------------
# Boucle sur les 7 branches couvertes (comme pour les bridge equations)
# ----------------------------------------------------------------------------
resultats_midas <- list()

for (b in BRANCHES_COUVERTES) {
  cible_b <- cibles %>% filter(branche == b) %>% select(date, dlog_va)
  indics_b <- indicateurs %>% filter(branche == b, frequence == "mensuel") %>%
    distinct(indicateur) %>% pull(indicateur)
  
  if (length(indics_b) == 0) {
    cat(sprintf("%-28s | aucun indicateur mensuel disponible -> MIDAS non applicable\n", b))
    next
  }
  
  previsions_indiv <- c()
  for (ind_nom in indics_b) {
    indic_df <- indicateurs %>% filter(branche == b, indicateur == ind_nom) %>% select(date, valeur)
    res <- midas_un_indicateur(cible_b, indic_df)
    if (!is.null(res)) {
      previsions_indiv[ind_nom] <- res$prevision
      if (res$taux_manquant > 0.05) {
        cat(sprintf("    (%s : %.0f%% de mois manquants interpoles)\n", ind_nom, res$taux_manquant * 100))
      }
    }
  }
  
  if (length(previsions_indiv) == 0) {
    cat(sprintf("%-28s | echantillon insuffisant (ou trop de trous) pour tout indicateur -> MIDAS non calcule\n", b))
    next
  }
  
  resultats_midas[[b]] <- list(
    previsions_individuelles = previsions_indiv,
    prevision_midas = mean(previsions_indiv, na.rm = TRUE),
    nb_indicateurs = length(previsions_indiv)
  )
  
  cat(sprintf("%-28s | MIDAS=%+.4f (moyenne sur %d indicateur(s) mensuel(s))\n",
              b, resultats_midas[[b]]$prevision_midas, length(previsions_indiv)))
}

saveRDS(resultats_midas, file.path(DOSSIER_RESULTATS, "midas_equations.rds"))
cat("\nModeles MIDAS (U-MIDAS) sauvegardes pour", length(resultats_midas), "branches couvertes.\n")