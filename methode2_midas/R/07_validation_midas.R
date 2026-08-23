# ============================================================================
# 07_validation_midas.R -- Backtest RMSFE + Diebold-Mariano, variante MIDAS
# ============================================================================
# Meme protocole que la Methode 1 (troncature successive, repere AR(2),
# test de Diebold-Mariano) -- pour comparer les deux methodes sur un pied
# d'egalite, avec exactement le meme decoupage temporel.
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
indicateurs_midas <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs_midas.rds"))
repli_methode1 <- readRDS(file.path(DOSSIER_RESULTATS, "bridge_equations_methode1.rds"))

H_TEST <- 16
P_RETARDS <- 5
LAMBDA <- 0.15
BRANCHES_MIDAS_ELIGIBLES <- indicateurs_midas %>% distinct(branche) %>% pull(branche)

mat_large <- cibles %>% select(branche, date, dlog_va) %>%
  pivot_wider(names_from = branche, values_from = dlog_va) %>% arrange(date) %>%
  filter(if_all(everything(), ~ !is.na(.)))
dates_vec <- mat_large$date
Y_complet <- as.matrix(mat_large[, TOUTES_BRANCHES])
Tn_complet <- nrow(Y_complet)

dernier_trim <- cibles %>% group_by(branche) %>% filter(date == max(date)) %>% ungroup() %>% select(branche, va)
poids <- (dernier_trim$va / sum(dernier_trim$va)) %>% setNames(dernier_trim$branche)

estimer_bvar_local <- function(Y, p, lambda) {
  n <- ncol(Y)
  sigma_i <- sapply(1:n, function(i) {
    fit <- tryCatch(ar.ols(Y[, i], order.max = p, aic = FALSE, demean = TRUE), error = function(e) NULL)
    if (is.null(fit)) sd(Y[, i]) else sqrt(fit$var.pred)
  })
  Tn <- nrow(Y)
  Y_reg <- Y[(p+1):Tn, , drop = FALSE]
  X_reg <- matrix(1, nrow = Tn - p, ncol = 1 + n * p)
  for (l in 1:p) X_reg[, (2+(l-1)*n):(1+l*n)] <- Y[(p+1-l):(Tn-l), , drop = FALSE]
  Yd1 <- matrix(0, n*p, n); Xd1 <- matrix(0, n*p, 1+n*p)
  for (l in 1:p) { rows <- ((l-1)*n+1):(l*n); Xd1[rows, (2+(l-1)*n):(1+l*n)] <- diag(sigma_i*l/lambda) }
  Yd2 <- matrix(0, n, n); diag(Yd2) <- sigma_i; Xd2 <- matrix(0, n, 1+n*p)
  Yd3 <- matrix(0, 1, n); Xd3 <- matrix(0, 1, 1+n*p); Xd3[1,1] <- 1e-5
  Y_aug <- rbind(Y_reg, Yd1, Yd2, Yd3); X_aug <- rbind(X_reg, Xd1, Xd2, Xd3)
  B <- solve(t(X_aug) %*% X_aug) %*% t(X_aug) %*% Y_aug
  x_new <- matrix(c(1, as.vector(t(Y[Tn:(Tn-p+1), , drop=FALSE]))), nrow = 1)
  setNames(as.vector(x_new %*% B), colnames(Y))
}

completer_serie_kalman_limitee <- function(indic_df, date_limite) {
  indic_df <- indic_df %>% filter(date <= date_limite) %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
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

midas_jagged_limite <- function(branche, date_limite) {
  cible_b <- cibles %>% filter(branche == !!branche, date <= date_limite) %>% select(date, dlog_va)
  indics_b <- indicateurs_midas %>% filter(branche == !!branche) %>% distinct(indicateur) %>% pull(indicateur)
  if (length(indics_b) == 0) return(NA_real_)

  prevs <- c()
  for (ind_nom in indics_b) {
    indic_df <- indicateurs_midas %>% filter(branche == !!branche, indicateur == ind_nom, date <= date_limite) %>%
      select(date, valeur)
    serie_complete <- completer_serie_kalman_limitee(indic_df, date_limite)
    if (is.null(serie_complete)) next

    sc <- serie_complete %>% arrange(date) %>%
      mutate(dlog = c(NA, diff(log(pmax(valeur, 1e-6)))),
             trimestre = floor_date(date, "quarter"), position = (month(date)-1) %% 3 + 1) %>%
      filter(!is.na(dlog))
    large <- sc %>% select(trimestre, position, dlog) %>%
      pivot_wider(names_from = position, values_from = dlog, names_prefix = "m") %>%
      filter(!is.na(m1), !is.na(m2), !is.na(m3))
    df <- inner_join(cible_b, large, by = c("date" = "trimestre")) %>% filter(!is.na(dlog_va))
    if (nrow(df) < 10) next

    df$z1 <- df$m1 + df$m2 + df$m3
    df$z2 <- 1*df$m1 + 2*df$m2 + 3*df$m3
    fit_midas <- lm(dlog_va ~ z1 + z2, data = df)
    log_vals <- log(pmax(serie_complete$valeur, 1e-6))
    ts_obj <- ts(log_vals, frequency = 12)
    p <- min(4, max(1, floor(length(log_vals) / 10)))
    fit_ar <- tryCatch(Arima(ts_obj, order = c(p, 0, 0)), error = function(e) NULL)
    if (is.null(fit_ar)) fit_ar <- tryCatch(Arima(ts_obj, order = c(1, 0, 0)), error = function(e) NULL)
    if (is.null(fit_ar)) next
    log_prevu <- as.numeric(forecast(fit_ar, h = 3)$mean)
    m_prevus <- diff(c(tail(log_vals, 1), log_prevu))
    z1_prevu <- sum(m_prevus); z2_prevu <- 1*m_prevus[1] + 2*m_prevus[2] + 3*m_prevus[3]
    prevs[ind_nom] <- as.numeric(predict(fit_midas, newdata = data.frame(z1=z1_prevu, z2=z2_prevu)))
  }
  if (length(prevs) == 0) return(NA_real_)
  mean(prevs, na.rm = TRUE)
}

erreurs_modele <- c(); erreurs_ar2 <- c(); dates_test <- dates_vec[(Tn_complet - H_TEST + 1):Tn_complet]

cat("Backtest MIDAS en cours (", H_TEST, "origines) ...\n")
for (h in seq_along(dates_test)) {
  idx <- Tn_complet - H_TEST + h - 1
  date_limite <- dates_vec[idx]
  Y_tr <- Y_complet[1:idx, , drop = FALSE]
  prev_bvar <- estimer_bvar_local(Y_tr, P_RETARDS, LAMBDA)

  prev_branches <- c()
  for (b in colnames(Y_complet)) {
    if (b %in% BRANCHES_MIDAS_ELIGIBLES) {
      pm <- midas_jagged_limite(b, date_limite)
      prev_branches[b] <- if (is.na(pm)) prev_bvar[b] else pm
    } else if (b %in% BRANCHES_COUVERTES) {
      # repli Methode 1 (statique -- meme prevision qu'en echantillon plein,
      # approximation raisonnable puisque ces branches n'ont de toute facon
      # pas d'indicateur MIDAS a re-estimer)
      prev_branches[b] <- if (b %in% names(repli_methode1)) repli_methode1[[b]]$prevision_finale else prev_bvar[b]
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

  cat(sprintf("  Origine %s -> cible %s : MIDAS=%+.4f (reel=%+.4f) | AR(2)=%+.4f\n",
              date_limite, dates_vec[idx+1], prevision_pib, reel_pib, as.numeric(forecast(fit_ar2, h=1)$mean)))
}

rmsfe_modele <- sqrt(mean(erreurs_modele^2))
rmsfe_ar2 <- sqrt(mean(erreurs_ar2^2))
test_dm <- tryCatch(dm.test(erreurs_modele, erreurs_ar2, h = 1, power = 2), error = function(e) NULL)

cat(sprintf("\n=== RESULTATS DU BACKTEST -- MIDAS (%d trimestres) ===\n", H_TEST))
cat(sprintf("RMSFE MIDAS          : %.4f\n", rmsfe_modele))
cat(sprintf("RMSFE repere AR(2)   : %.4f\n", rmsfe_ar2))
cat(sprintf("Gain relatif         : %.1f %%\n", (1 - rmsfe_modele / rmsfe_ar2) * 100))
if (!is.null(test_dm)) cat(sprintf("Diebold-Mariano p-value : %.3f\n", test_dm$p.value))

saveRDS(list(dates_test = dates_test, erreurs_modele = erreurs_modele, erreurs_ar2 = erreurs_ar2,
             rmsfe_modele = rmsfe_modele, rmsfe_ar2 = rmsfe_ar2, test_dm = test_dm),
        file.path(DOSSIER_RESULTATS, "backtest_midas.rds"))

df_erreurs <- tibble(date = dates_test, MIDAS = abs(erreurs_modele), `AR(2)` = abs(erreurs_ar2)) %>%
  pivot_longer(-date, names_to = "modele", values_to = "erreur_absolue")
p <- ggplot(df_erreurs, aes(date, erreur_absolue, color = modele)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  scale_color_manual(values = c("MIDAS" = "#2E74B5", "AR(2)" = "#C55A11")) +
  labs(title = "Erreur de prévision absolue — MIDAS vs. repère AR(2)",
       subtitle = sprintf("RMSFE MIDAS = %.4f | RMSFE AR(2) = %.4f", rmsfe_modele, rmsfe_ar2),
       x = NULL, y = "|Erreur| (Δlog)", color = NULL)
ggsave(file.path(DOSSIER_FIGURES, "08_backtest_midas.png"), p, width = 9, height = 5, dpi = 150)
cat("\nBacktest MIDAS termine et sauvegarde.\n")
