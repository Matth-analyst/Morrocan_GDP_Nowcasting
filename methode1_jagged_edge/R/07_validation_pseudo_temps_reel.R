# ============================================================================
# 07_validation_pseudo_temps_reel.R -- Backtest RMSFE, variante JAGGED EDGE
# ============================================================================
# Meme protocole que la version precedente (troncature successive, repere
# AR(2), test de Diebold-Mariano), mais la bridge equation utilisee a chaque
# origine applique desormais le meme comblement par lissage de Kalman que
# 04_bridge_equations.R -- pas de fuite d'information : a chaque origine, le
# comblement est refait UNIQUEMENT sur les donnees disponibles jusqu'a cette
# date (le futur n'est jamais utilise pour combler un trou passe).
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
indicateurs <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs.rds"))

H_TEST <- 16
P_RETARDS <- 5
LAMBDA <- 0.15

mat_large <- cibles %>% select(branche, date, dlog_va) %>%
  pivot_wider(names_from = branche, values_from = dlog_va) %>% arrange(date) %>%
  filter(if_all(everything(), ~ !is.na(.)))
dates_vec <- mat_large$date
Y_complet <- as.matrix(mat_large[, TOUTES_BRANCHES])
Tn_complet <- nrow(Y_complet)

dernier_trim <- cibles %>% group_by(branche) %>% filter(date == max(date)) %>% ungroup() %>% select(branche, va)
poids <- (dernier_trim$va / sum(dernier_trim$va)) %>% setNames(dernier_trim$branche)

covid_dummy <- as.integer(dates_vec %in% as.Date(c("2020-04-01", "2020-07-01")))

estimer_bvar_local <- function(Y, p, lambda, covid) {
  n <- ncol(Y)
  sigma_i <- sapply(1:n, function(i) {
    fit <- tryCatch(ar.ols(Y[, i], order.max = p, aic = FALSE, demean = TRUE), error = function(e) NULL)
    if (is.null(fit)) sd(Y[, i]) else sqrt(fit$var.pred)
  })
  Tn <- nrow(Y)
  Y_reg <- Y[(p+1):Tn, , drop = FALSE]
  X_reg <- matrix(1, nrow = Tn - p, ncol = 2 + n * p)
  X_reg[, 2] <- covid[(p+1):Tn]
  for (l in 1:p) X_reg[, (3+(l-1)*n):(2+l*n)] <- Y[(p+1-l):(Tn-l), , drop = FALSE]
  Yd1 <- matrix(0, n*p, n); Xd1 <- matrix(0, n*p, 2+n*p)
  for (l in 1:p) { rows <- ((l-1)*n+1):(l*n); Xd1[rows, (3+(l-1)*n):(2+l*n)] <- diag(sigma_i*l/lambda) }
  Yd2 <- matrix(0, n, n); diag(Yd2) <- sigma_i; Xd2 <- matrix(0, n, 2+n*p)
  Yd3 <- matrix(0, 2, n); Xd3 <- matrix(0, 2, 2+n*p); Xd3[1,1] <- 1e-5; Xd3[2,2] <- 1e-5
  Y_aug <- rbind(Y_reg, Yd1, Yd2, Yd3); X_aug <- rbind(X_reg, Xd1, Xd2, Xd3)
  B <- solve(t(X_aug) %*% X_aug) %*% t(X_aug) %*% Y_aug
  x_new <- matrix(c(1, 0, as.vector(t(Y[Tn:(Tn-p+1), , drop=FALSE]))), nrow = 1)
  setNames(as.vector(x_new %*% B), colnames(Y))
}

#' Version "date_limite"-consciente du comblement par Kalman : ne voit
#' jamais les donnees posterieures a date_limite (pas de fuite d'info).
completer_serie_kalman_limitee <- function(indic_df, freq, date_limite) {
  indic_df <- indic_df %>% filter(date <= date_limite) %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
  if (nrow(indic_df) < 8) return(NULL)
  pas <- if (freq == "mensuel") "month" else "quarter"
  calendrier <- tibble(date = seq(min(indic_df$date), max(indic_df$date), by = pas))
  serie <- calendrier %>% left_join(indic_df %>% select(date, valeur), by = "date")
  serie$etait_manquant <- is.na(serie$valeur)
  if (sum(serie$etait_manquant) == 0) return(serie)

  log_vals <- log(pmax(serie$valeur, 1e-6))
  freq_num <- if (freq == "mensuel") 12 else 4
  ts_obj <- ts(log_vals, frequency = freq_num)
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

bridge_jagged_limite <- function(branche, date_limite) {
  cible_b <- cibles %>% filter(branche == !!branche, date <= date_limite) %>% select(date, dlog_va)
  indics_b <- indicateurs %>% filter(branche == !!branche) %>% distinct(indicateur, frequence)
  if (nrow(indics_b) == 0) return(NA_real_)

  prevs <- c()
  for (i in seq_len(nrow(indics_b))) {
    ind_nom <- indics_b$indicateur[i]; freq_ind <- indics_b$frequence[i]
    indic_df <- indicateurs %>% filter(branche == !!branche, indicateur == ind_nom) %>% select(date, valeur)
    serie_complete <- completer_serie_kalman_limitee(indic_df, freq_ind, date_limite)
    if (is.null(serie_complete)) next
    serie_complete <- serie_complete %>% arrange(date) %>% mutate(dlog = c(NA, diff(log(pmax(valeur, 1e-6)))))
    serie_trim <- if (freq_ind == "mensuel") {
      serie_complete %>% mutate(trimestre = floor_date(date, "quarter")) %>%
        group_by(trimestre) %>% summarise(dlog = mean(dlog, na.rm = TRUE)) %>% rename(date = trimestre)
    } else serie_complete %>% select(date, dlog)

    df <- inner_join(cible_b, serie_trim, by = "date") %>% filter(!is.na(dlog_va), !is.na(dlog))
    if (nrow(df) < 10) next
    fit <- lm(dlog_va ~ dlog, data = df)

    log_vals <- log(pmax(serie_complete$valeur, 1e-6))
    freq_num <- if (freq_ind == "mensuel") 12 else 4
    ts_obj <- ts(log_vals, frequency = freq_num)
    p <- min(4, max(1, floor(length(log_vals) / 10)))
    fit_ar <- tryCatch(Arima(ts_obj, order = c(p, 0, 0)), error = function(e) NULL)
    if (is.null(fit_ar)) fit_ar <- tryCatch(Arima(ts_obj, order = c(1, 0, 0)), error = function(e) NULL)
    h_prev <- if (freq_ind == "mensuel") 3 else 1
    if (!is.null(fit_ar)) {
      log_prevu <- as.numeric(forecast(fit_ar, h = h_prev)$mean)
      dlog_prevu <- if (freq_ind == "mensuel") mean(diff(c(tail(log_vals, 1), log_prevu))) else log_prevu[1] - tail(log_vals, 1)
    } else dlog_prevu <- tail(serie_trim$dlog, 1)

    prevs[ind_nom] <- as.numeric(predict(fit, newdata = data.frame(dlog = dlog_prevu)))
  }
  if (length(prevs) == 0) return(NA_real_)
  mean(prevs, na.rm = TRUE)
}

erreurs_modele <- c(); erreurs_ar2 <- c(); dates_test <- dates_vec[(Tn_complet - H_TEST + 1):Tn_complet]

cat("Backtest (jagged edge) en cours (", H_TEST, "origines) ...\n")
for (h in seq_along(dates_test)) {
  idx <- Tn_complet - H_TEST + h - 1
  date_limite <- dates_vec[idx]
  Y_tr <- Y_complet[1:idx, , drop = FALSE]
  prev_bvar <- estimer_bvar_local(Y_tr, P_RETARDS, LAMBDA, covid_dummy[1:idx])

  prev_branches <- c()
  for (b in colnames(Y_complet)) {
    if (b %in% BRANCHES_COUVERTES) {
      pb <- bridge_jagged_limite(b, date_limite)
      prev_branches[b] <- if (is.na(pb)) prev_bvar[b] else 0.5 * prev_bvar[b] + 0.5 * pb
    } else {
      fit_ar4 <- tryCatch(Arima(Y_tr[, b], order = c(4,0,0)), error = function(e) NULL)
      prev_branches[b] <- if (is.null(fit_ar4)) prev_bvar[b] else as.numeric(forecast(fit_ar4, h=1)$mean)
    }
  }

  prevision_pib <- sum(prev_branches[names(poids)] * poids)
  reel_pib <- sum(Y_complet[idx + 1, names(poids)] * poids)
  erreurs_modele[h] <- reel_pib - prevision_pib

  pib_total_tr <- as.vector(Y_tr %*% poids[colnames(Y_tr)])
  fit_ar2 <- Arima(pib_total_tr, order = c(2,0,0))
  erreurs_ar2[h] <- reel_pib - as.numeric(forecast(fit_ar2, h=1)$mean)

  cat(sprintf("  Origine %s -> cible %s : modele=%+.4f (reel=%+.4f) | AR(2)=%+.4f\n",
              date_limite, dates_vec[idx+1], prevision_pib, reel_pib, as.numeric(forecast(fit_ar2, h=1)$mean)))
}

rmsfe_modele <- sqrt(mean(erreurs_modele^2))
rmsfe_ar2 <- sqrt(mean(erreurs_ar2^2))
test_dm <- tryCatch(dm.test(erreurs_modele, erreurs_ar2, h = 1, power = 2), error = function(e) NULL)

cat(sprintf("\n=== RESULTATS DU BACKTEST -- JAGGED EDGE (%d trimestres) ===\n", H_TEST))
cat(sprintf("RMSFE modele complet (BVAR + bridge jagged edge + AR4) : %.4f\n", rmsfe_modele))
cat(sprintf("RMSFE repere AR(2) sur le PIB total                     : %.4f\n", rmsfe_ar2))
cat(sprintf("Gain relatif du modele complet                          : %.1f %%\n", (1 - rmsfe_modele / rmsfe_ar2) * 100))
if (!is.null(test_dm)) {
  cat(sprintf("\nTest de Diebold-Mariano : statistique=%.3f, p-value=%.3f\n", test_dm$statistic, test_dm$p.value))
}

saveRDS(list(dates_test = dates_test, erreurs_modele = erreurs_modele, erreurs_ar2 = erreurs_ar2,
             rmsfe_modele = rmsfe_modele, rmsfe_ar2 = rmsfe_ar2, test_dm = test_dm),
        file.path(DOSSIER_RESULTATS, "backtest.rds"))

df_erreurs <- tibble(date = dates_test, Modele = abs(erreurs_modele), `AR(2)` = abs(erreurs_ar2)) %>%
  pivot_longer(-date, names_to = "modele", values_to = "erreur_absolue")
p8 <- ggplot(df_erreurs, aes(date, erreur_absolue, color = modele)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  scale_color_manual(values = c("Modele" = "#2E74B5", "AR(2)" = "#C55A11")) +
  labs(title = "Erreur de prévision absolue — modèle complet (jagged edge) vs. repère AR(2)",
       subtitle = sprintf("RMSFE modèle = %.4f | RMSFE AR(2) = %.4f", rmsfe_modele, rmsfe_ar2),
       x = NULL, y = "|Erreur| (Δlog)", color = NULL)
ggsave(file.path(DOSSIER_FIGURES, "08_backtest_erreurs.png"), p8, width = 9, height = 5, dpi = 150)

cat("\nBacktest (jagged edge) termine et sauvegarde.\n")
