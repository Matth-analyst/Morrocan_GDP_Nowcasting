# ============================================================================
# 04_midas.R -- Modele MIDAS (Mixed Data Sampling) avec traitement du jagged edge
# ============================================================================
# Reference : Ghysels, Santa-Clara & Valkanov (2004) "The MIDAS Touch:
# Mixed Data Sampling Regression Models" ; Foroni, Marcellino & Schumacher
# (2015) "U-MIDAS: MIDAS regressions with unrestricted lag polynomials"
# (specification U-MIDAS retenue ici : un coefficient libre par mois, pas
# de fonction de poids parametrique -- plus simple, adaptee a un horizon
# court comme le notre, h=1 trimestre).
#
# Specification : Δlog(VA)_t = β0 + β1.m1_t + β2.m2_t + β3.m3_t + ε_t
# ou m1, m2, m3 sont les taux de croissance mensuels des 3 mois du
# trimestre t pour un indicateur donne.
#
# JAGGED EDGE : comme pour la Methode 1, chaque indicateur est reconstruit
# sur un calendrier mensuel complet par lissage de Kalman
# (stats::arima + stats::KalmanSmooth) avant de construire m1/m2/m3 --
# aucun trimestre n'est perdu a cause d'un seul mois manquant.
#
# Indicateurs utilises : les 37 series ayant passe le CRITERE DEDIE A
# MIDAS (test F joint sur m1,m2,m3 -- distinct du critere de la bridge
# equation, qui teste la correlation sur le taux de croissance agrege au
# trimestre). Voir NOTE_METHODOLOGIQUE_MIDAS.md pour la justification
# complete de cette difference.
#
# REPLI : pour toute branche couverte sans aucun indicateur eligible a
# MIDAS (ex. Industrie d'extraction, dont l'unique indicateur retenu en
# Methode 1 est trimestriel), on reutilise la prevision bridge+BVAR deja
# combinee de la Methode 1 (fichier bridge_equations_methode1.rds).
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
indicateurs_midas <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs_midas.rds"))
bvar <- readRDS(file.path(DOSSIER_RESULTATS, "bvar_modele.rds"))
repli_methode1 <- readRDS(file.path(DOSSIER_RESULTATS, "bridge_equations_methode1.rds"))

BRANCHES_MIDAS_ELIGIBLES <- indicateurs_midas %>% distinct(branche) %>% pull(branche)
cat("Branches avec au moins un indicateur eligible a MIDAS :", paste(BRANCHES_MIDAS_ELIGIBLES, collapse=", "), "\n")
cat("Branches en repli (bridge+BVAR de la Methode 1) :",
    paste(setdiff(BRANCHES_COUVERTES, BRANCHES_MIDAS_ELIGIBLES), collapse=", "), "\n\n")

#' Reconstruit un calendrier mensuel complet par lissage de Kalman
#' (identique a la fonction de la Methode 1 -- meme methode, meme code).
completer_serie_kalman <- function(indic_df) {
  indic_df <- indic_df %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
  if (nrow(indic_df) < 8) return(NULL)
  calendrier <- tibble(date = seq(min(indic_df$date), max(indic_df$date), by = "month"))
  serie <- calendrier %>% left_join(indic_df %>% select(date, valeur), by = "date")
  serie$etait_manquant <- is.na(serie$valeur)
  if (sum(serie$etait_manquant) == 0) return(serie)

  log_vals <- log(pmax(serie$valeur, 1e-6))
  ts_obj <- ts(log_vals, frequency = 12)
  p <- min(4, max(1, floor(sum(!is.na(log_vals)) / 10)))
  fit <- tryCatch(arima(ts_obj, order = c(p, 0, 0)), error = function(e) NULL)
  if (is.null(fit)) fit <- tryCatch(arima(ts_obj, order = c(1, 0, 0)), error = function(e) NULL)
  if (is.null(fit)) return(serie)
  kf <- tryCatch(KalmanSmooth(ts_obj, fit$model), error = function(e) NULL)
  if (is.null(kf)) return(serie)
  interpole <- kf$smooth[, 1]
  if ("intercept" %in% names(fit$coef)) interpole <- interpole + fit$coef["intercept"]
  serie$valeur <- ifelse(serie$etait_manquant, exp(interpole), serie$valeur)
  serie %>% select(date, valeur, etait_manquant)
}

#' Construit la matrice (trimestre, m1, m2, m3) a partir d'une serie
#' mensuelle completee, et l'aligne avec la cible.
construire_m1m2m3 <- function(serie_complete, cible_df) {
  serie_complete <- serie_complete %>% arrange(date) %>%
    mutate(dlog = c(NA, diff(log(pmax(valeur, 1e-6)))),
           trimestre = floor_date(date, "quarter"),
           position = (month(date) - 1) %% 3 + 1) %>%
    filter(!is.na(dlog))

  large <- serie_complete %>% select(trimestre, position, dlog) %>%
    pivot_wider(names_from = position, values_from = dlog, names_prefix = "m") %>%
    filter(!is.na(m1), !is.na(m2), !is.na(m3))

  inner_join(cible_df, large, by = c("date" = "trimestre")) %>% filter(!is.na(dlog_va))
}

#' Ajuste le modele MIDAS (U-MIDAS, 3 regresseurs libres) pour un
#' indicateur, et prevoit le prochain trimestre en reutilisant le modele
#' de Kalman pour extrapoler les 3 mois a venir.
midas_un_indicateur <- function(cible_df, indic_df_brut) {
  serie_complete <- completer_serie_kalman(indic_df_brut)
  if (is.null(serie_complete)) return(NULL)
  taux_comble <- mean(serie_complete$etait_manquant)

  df <- construire_m1m2m3(serie_complete, cible_df)
  if (nrow(df) < 10) return(NULL)

  # --- Contrainte d'Almon (degre 1) : au lieu de 3 coefficients libres
  # (beta1, beta2, beta3 independants -- U-MIDAS, sujet au sur-ajustement
  # diagnostique dans NOTE_METHODOLOGIQUE_MIDAS.md), on impose une
  # decroissance/croissance LINEAIRE des coefficients d'un mois a l'autre :
  #   beta_j = a + b.j  (j=1,2,3)
  # Reference : Almon, S. (1965), "The Distributed Lag Between Capital
  # Appropriations and Expenditures", Econometrica, 33(1), 178-196 --
  # methode originale des retards polynomiaux contraints, reprise ensuite
  # dans le cadre MIDAS (Ghysels, Sinko & Valkanov, 2007, "MIDAS
  # Regressions: Further Results and New Directions") pour reduire le
  # nombre de parametres libres et limiter le sur-ajustement.
  # 2 parametres (a, b) au lieu de 3 (beta1, beta2, beta3).
  df$z1 <- df$m1 + df$m2 + df$m3
  df$z2 <- 1*df$m1 + 2*df$m2 + 3*df$m3
  fit_midas <- lm(dlog_va ~ z1 + z2, data = df)

  # --- prevision des 3 prochains mois via le modele Kalman ajuste ---
  log_vals <- log(pmax(serie_complete$valeur, 1e-6))
  ts_obj <- ts(log_vals, frequency = 12)
  p <- min(4, max(1, floor(length(log_vals) / 10)))
  fit_ar <- tryCatch(Arima(ts_obj, order = c(p, 0, 0)), error = function(e) NULL)
  if (is.null(fit_ar)) fit_ar <- tryCatch(Arima(ts_obj, order = c(1, 0, 0)), error = function(e) NULL)
  if (is.null(fit_ar)) return(NULL)

  log_prevu <- as.numeric(forecast(fit_ar, h = 3)$mean)
  m_prevus <- diff(c(tail(log_vals, 1), log_prevu))  # 3 Δlog mensuels prevus

  z1_prevu <- sum(m_prevus)
  z2_prevu <- 1*m_prevus[1] + 2*m_prevus[2] + 3*m_prevus[3]
  prevision <- as.numeric(predict(fit_midas, newdata = data.frame(z1 = z1_prevu, z2 = z2_prevu)))

  list(fit = fit_midas, prevision = prevision, r2 = summary(fit_midas)$r.squared,
       n = nrow(df), taux_comble = taux_comble)
}

resultats_midas <- list()

for (b in BRANCHES_MIDAS_ELIGIBLES) {
  cible_b <- cibles %>% filter(branche == b) %>% select(date, dlog_va)
  indics_b <- indicateurs_midas %>% filter(branche == b) %>% distinct(indicateur) %>% pull(indicateur)

  previsions_indiv <- c(); taux_combles <- c()
  for (ind_nom in indics_b) {
    indic_df <- indicateurs_midas %>% filter(branche == b, indicateur == ind_nom) %>% select(date, valeur)
    res <- midas_un_indicateur(cible_b, indic_df)
    if (!is.null(res)) {
      previsions_indiv[ind_nom] <- res$prevision
      taux_combles[ind_nom] <- res$taux_comble
    }
  }
  if (length(previsions_indiv) == 0) next
  prevision_midas <- mean(previsions_indiv, na.rm = TRUE)

  resultats_midas[[b]] <- list(previsions_individuelles = previsions_indiv,
                                 prevision_finale = prevision_midas,
                                 nb_indicateurs = length(previsions_indiv),
                                 taux_combles_moyen = mean(taux_combles, na.rm = TRUE),
                                 methode = "MIDAS")
  cat(sprintf("%-28s | MIDAS=%+.4f (n=%d ind., comblement moyen=%.0f%%)\n",
              b, prevision_midas, length(previsions_indiv), mean(taux_combles, na.rm=TRUE)*100))
}

# --- Repli sur la Methode 1 pour les branches non eligibles a MIDAS ---
for (b in setdiff(BRANCHES_COUVERTES, BRANCHES_MIDAS_ELIGIBLES)) {
  if (b %in% names(repli_methode1)) {
    resultats_midas[[b]] <- list(previsions_individuelles = NA,
                                   prevision_finale = repli_methode1[[b]]$prevision_finale,
                                   nb_indicateurs = 0, taux_combles_moyen = NA,
                                   methode = "Repli Methode 1 (bridge+BVAR)")
    cat(sprintf("%-28s | REPLI Methode 1 = %+.4f (aucun indicateur eligible a MIDAS)\n",
                b, repli_methode1[[b]]$prevision_finale))
  }
}

saveRDS(resultats_midas, file.path(DOSSIER_RESULTATS, "midas_resultats.rds"))
cat("\nModele MIDAS sauvegarde.\n")
