# ============================================================================
# 07_validation_dfm.R -- Backtest RMSFE + Diebold-Mariano, variante DFM
# ============================================================================
source("R/00_setup.R")
suppressMessages(library(dfms))
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
indicateurs_dfm <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs_dfm.rds"))
repli_methode1 <- readRDS(file.path(DOSSIER_RESULTATS, "bridge_equations_methode1.rds"))

H_TEST <- 16
P_RETARDS <- 5
LAMBDA <- 0.15
BRANCHES_DFM_ELIGIBLES <- indicateurs_dfm %>% distinct(branche) %>% pull(branche)

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

dfm_limite <- function(branche, indics_df, date_limite) {
  indics_df <- indics_df %>% filter(date <= date_limite)
  noms <- unique(indics_df$indicateur)
  if (length(noms) < 2) return(NA_real_)

  large <- indics_df %>% select(date, indicateur, valeur) %>%
    pivot_wider(names_from = indicateur, values_from = valeur) %>% arrange(date)
  calendrier <- tibble(date = seq(min(large$date), max(large$date), by = "month"))
  large <- calendrier %>% left_join(large, by = "date")
  mat_niveau <- as.matrix(large[, noms, drop = FALSE])
  mat_dlog <- apply(log(pmax(mat_niveau, 1e-6)), 2, function(x) c(NA, diff(x)))
  rownames(mat_dlog) <- as.character(large$date)
  mat_std <- scale(mat_dlog)

  fit <- tryCatch(DFM(mat_std, r = 1, p = 2), error = function(e) NULL)
  if (is.null(fit)) return(NA_real_)

  facteur <- as.numeric(fit$F_qml)
  dates_completes <- large$date
  if (!is.null(fit$rm.rows) && length(fit$rm.rows) > 0) {
    dates_facteur <- dates_completes[-fit$rm.rows]
  } else {
    dates_facteur <- dates_completes
  }
  df_facteur <- tibble(date = dates_facteur, facteur = facteur) %>%
    mutate(trimestre = floor_date(date, "quarter")) %>%
    group_by(trimestre) %>% summarise(facteur_q = mean(facteur, na.rm = TRUE)) %>% rename(date = trimestre)

  cible_b <- cibles %>% filter(branche == !!branche, date <= date_limite) %>% select(date, dlog_va)
  df_reg <- inner_join(cible_b, df_facteur, by = "date") %>% filter(!is.na(dlog_va))
  if (nrow(df_reg) < 10) return(NA_real_)
  fit_reg <- lm(dlog_va ~ facteur_q, data = df_reg)

  fit_ar_facteur <- tryCatch(Arima(df_facteur$facteur_q, order = c(2,0,0)), error = function(e) NULL)
  facteur_prevu <- if (!is.null(fit_ar_facteur)) as.numeric(forecast(fit_ar_facteur, h=1)$mean) else tail(df_facteur$facteur_q, 1)
  as.numeric(predict(fit_reg, newdata = data.frame(facteur_q = facteur_prevu)))
}

erreurs_modele <- c(); erreurs_ar2 <- c(); dates_test <- dates_vec[(Tn_complet - H_TEST + 1):Tn_complet]

cat("Backtest DFM en cours (", H_TEST, "origines -- peut prendre quelques minutes) ...\n")
for (h in seq_along(dates_test)) {
  idx <- Tn_complet - H_TEST + h - 1
  date_limite <- dates_vec[idx]
  Y_tr <- Y_complet[1:idx, , drop = FALSE]
  prev_bvar <- estimer_bvar_local(Y_tr, P_RETARDS, LAMBDA)

  prev_branches <- c()
  for (b in colnames(Y_complet)) {
    if (b %in% BRANCHES_DFM_ELIGIBLES) {
      indics_b <- indicateurs_dfm %>% filter(branche == b)
      pd <- dfm_limite(b, indics_b, date_limite)
      prev_branches[b] <- if (is.na(pd)) prev_bvar[b] else pd
    } else if (b %in% BRANCHES_COUVERTES) {
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

  cat(sprintf("  Origine %s -> cible %s : DFM=%+.4f (reel=%+.4f) | AR(2)=%+.4f\n",
              date_limite, dates_vec[idx+1], prevision_pib, reel_pib, as.numeric(forecast(fit_ar2, h=1)$mean)))
}

rmsfe_modele <- sqrt(mean(erreurs_modele^2))
rmsfe_ar2 <- sqrt(mean(erreurs_ar2^2))
test_dm <- tryCatch(dm.test(erreurs_modele, erreurs_ar2, h = 1, power = 2), error = function(e) NULL)

cat(sprintf("\n=== RESULTATS DU BACKTEST -- DFM (%d trimestres) ===\n", H_TEST))
cat(sprintf("RMSFE DFM           : %.4f\n", rmsfe_modele))
cat(sprintf("RMSFE repere AR(2)  : %.4f\n", rmsfe_ar2))
cat(sprintf("Gain relatif        : %.1f %%\n", (1 - rmsfe_modele / rmsfe_ar2) * 100))
if (!is.null(test_dm)) cat(sprintf("Diebold-Mariano p-value : %.3f\n", test_dm$p.value))

saveRDS(list(dates_test = dates_test, erreurs_modele = erreurs_modele, erreurs_ar2 = erreurs_ar2,
             rmsfe_modele = rmsfe_modele, rmsfe_ar2 = rmsfe_ar2, test_dm = test_dm),
        file.path(DOSSIER_RESULTATS, "backtest_dfm.rds"))

df_erreurs <- tibble(date = dates_test, DFM = abs(erreurs_modele), `AR(2)` = abs(erreurs_ar2)) %>%
  pivot_longer(-date, names_to = "modele", values_to = "erreur_absolue")
p <- ggplot(df_erreurs, aes(date, erreur_absolue, color = modele)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  scale_color_manual(values = c("DFM" = "#2E74B5", "AR(2)" = "#C55A11")) +
  labs(title = "Erreur de prévision absolue : DFM vs. repère AR(2)",
       subtitle = sprintf("RMSFE DFM = %.4f | RMSFE AR(2) = %.4f", rmsfe_modele, rmsfe_ar2),
       x = NULL, y = "|Erreur| (Δlog)", color = NULL)
ggsave(file.path(DOSSIER_FIGURES, "08_backtest_dfm.png"), p, width = 9, height = 5, dpi = 150)
cat("\nBacktest DFM termine et sauvegarde.\n")
