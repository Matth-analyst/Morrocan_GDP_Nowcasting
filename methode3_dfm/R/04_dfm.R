# ============================================================================
# 04_dfm.R -- Modele a facteur dynamique (DFM), Methode 3
# ============================================================================
# Reference : Doz, Giannone & Reichlin (2011) "A Two-Step Estimator for
# Large Approximate Dynamic Factor Models" ; Stock & Watson (2002)
# "Macroeconomic Forecasting Using Diffusion Indexes" ; package R `dfms`
# (Krueger, 2023), qui implemente l'algorithme EM standard pour
# l'estimation d'un DFM approximatif avec donnees manquantes.
#
# Contrairement aux Methodes 1 et 2, le DFM n'estime pas une regression
# indicateur par indicateur : il extrait UN FACTEUR COMMUN a partir de
# TOUS les indicateurs retenus d'une branche simultanement (par
# l'algorithme EM, qui gere nativement les donnees manquantes -- pas
# besoin ici du comblement par lissage de Kalman indicateur par indicateur
# comme aux Methodes 1 et 2 : le DFM applique lui-meme un filtre/lisseur
# de Kalman a l'ensemble du systeme, exactement la meme famille de
# methode mais appliquee au niveau multivarie plutot que serie par serie).
#
# Etape finale : regression simple de la croissance trimestrielle de la
# VA sur le facteur (agrege au trimestre), pour produire la prevision --
# comparable en esprit a la bridge equation, mais sur un facteur plutot
# que sur un indicateur brut.
# ============================================================================
source("R/00_setup.R")
suppressMessages(library(dfms))

cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
indicateurs_dfm <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs_dfm.rds"))
bvar <- readRDS(file.path(DOSSIER_RESULTATS, "bvar_modele.rds"))
repli_methode1 <- readRDS(file.path(DOSSIER_RESULTATS, "bridge_equations_methode1.rds"))

BRANCHES_DFM_ELIGIBLES <- indicateurs_dfm %>% distinct(branche) %>% pull(branche)
cat("Branches avec un facteur DFM estimable (>=2 indicateurs) :",
    paste(BRANCHES_DFM_ELIGIBLES, collapse=", "), "\n")
cat("Branches en repli (bridge+BVAR de la Methode 1) :",
    paste(setdiff(BRANCHES_COUVERTES, BRANCHES_DFM_ELIGIBLES), collapse=", "), "\n\n")

#' Estime un DFM a 1 facteur pour une branche, a partir de ses indicateurs
#' mensuels retenus, et renvoie le facteur trimestrialise + la prevision
#' du facteur pour le prochain trimestre.
estimer_dfm_branche <- function(branche, indics_df) {
  noms <- unique(indics_df$indicateur)
  if (length(noms) < 2) return(NULL)

  large <- indics_df %>% select(date, indicateur, valeur) %>%
    pivot_wider(names_from = indicateur, values_from = valeur) %>% arrange(date)

  # Δlog sur calendrier mensuel complet (les trous internes sont geres
  # nativement par l'algorithme EM du DFM -- dfms accepte les NA)
  calendrier <- tibble(date = seq(min(large$date), max(large$date), by = "month"))
  large <- calendrier %>% left_join(large, by = "date")
  mat_niveau <- as.matrix(large[, noms, drop = FALSE])
  mat_dlog <- apply(log(pmax(mat_niveau, 1e-6)), 2, function(x) c(NA, diff(x)))
  rownames(mat_dlog) <- as.character(large$date)

  # standardisation (necessaire pour un DFM -- unites heterogenes)
  mat_std <- scale(mat_dlog)

  nb_facteurs <- 1
  nb_retards_facteur <- 2
  fit <- tryCatch(
    DFM(mat_std, r = nb_facteurs, p = nb_retards_facteur),
    error = function(e) { message("Echec DFM pour ", branche, " : ", conditionMessage(e)); NULL }
  )
  if (is.null(fit)) return(NULL)

  facteur <- as.numeric(fit$F_qml)  # facteur lisse (Kalman smoother), mensuel
  # dfms::DFM() retire en interne les lignes entierement vides en debut de
  # serie (fit$rm.rows) sans reindexer les dates -- on doit reproduire le
  # meme retrait pour aligner correctement le facteur sur son vrai calendrier
  dates_completes <- large$date
  if (!is.null(fit$rm.rows) && length(fit$rm.rows) > 0) {
    dates_facteur <- dates_completes[-fit$rm.rows]
  } else {
    dates_facteur <- dates_completes
  }

  df_facteur <- tibble(date = dates_facteur, facteur = facteur) %>%
    mutate(trimestre = floor_date(date, "quarter")) %>%
    group_by(trimestre) %>% summarise(facteur_q = mean(facteur, na.rm = TRUE)) %>%
    rename(date = trimestre)

  list(fit = fit, facteur_trimestriel = df_facteur, noms_indicateurs = noms,
       derniere_date_facteur = tail(dates_facteur, 1))
}

resultats_dfm <- list()

for (b in BRANCHES_DFM_ELIGIBLES) {
  indics_b <- indicateurs_dfm %>% filter(branche == b)
  cible_b <- cibles %>% filter(branche == b) %>% select(date, dlog_va)

  dfm_b <- estimer_dfm_branche(b, indics_b)
  if (is.null(dfm_b)) next

  df_reg <- inner_join(cible_b, dfm_b$facteur_trimestriel, by = "date") %>% filter(!is.na(dlog_va))
  if (nrow(df_reg) < 10) next

  fit_reg <- lm(dlog_va ~ facteur_q, data = df_reg)

  # prevision du facteur au prochain trimestre : AR(2) sur le facteur
  # trimestriel lui-meme (le facteur resume deja l'information commune,
  # un simple AR suffit pour l'extrapoler -- pas besoin de repeter le DFM)
  fit_ar_facteur <- tryCatch(Arima(dfm_b$facteur_trimestriel$facteur_q, order = c(2,0,0)),
                               error = function(e) NULL)
  facteur_prevu <- if (!is.null(fit_ar_facteur)) as.numeric(forecast(fit_ar_facteur, h=1)$mean) else tail(dfm_b$facteur_trimestriel$facteur_q, 1)

  prevision <- as.numeric(predict(fit_reg, newdata = data.frame(facteur_q = facteur_prevu)))

  resultats_dfm[[b]] <- list(prevision_finale = prevision, r2 = summary(fit_reg)$r.squared,
                               n = nrow(df_reg), nb_indicateurs = length(dfm_b$noms_indicateurs),
                               methode = "DFM")
  cat(sprintf("%-28s | DFM=%+.4f (R2=%.3f, n=%d ind., %d trimestres)\n",
              b, prevision, summary(fit_reg)$r.squared, length(dfm_b$noms_indicateurs), nrow(df_reg)))
}

for (b in setdiff(BRANCHES_COUVERTES, names(resultats_dfm))) {
  if (b %in% names(repli_methode1)) {
    resultats_dfm[[b]] <- list(prevision_finale = repli_methode1[[b]]$prevision_finale,
                                 r2 = NA, n = NA, nb_indicateurs = 0, methode = "Repli Methode 1 (bridge+BVAR)")
    cat(sprintf("%-28s | REPLI Methode 1 = %+.4f\n", b, repli_methode1[[b]]$prevision_finale))
  }
}

saveRDS(resultats_dfm, file.path(DOSSIER_RESULTATS, "dfm_resultats.rds"))
cat("\nModele DFM sauvegarde.\n")
