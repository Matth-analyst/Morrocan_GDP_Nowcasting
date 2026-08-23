# ============================================================================
# 07c_validation_dfm.R -- Backtest pseudo temps reel, variante DFM
# ============================================================================
# NOUVEAU SCRIPT. Meme protocole que 07b_validation_midas.R, avec le DFM
# reestime (EM, dfms::DFM) a chaque origine pour les branches couvertes
# disposant d'au moins 2 indicateurs mensuels ; repli sur bridge+BVAR
# simplifie sinon. Necessite le package 'dfms'.
# ============================================================================
source("R/00_setup.R")
if (!requireNamespace("dfms", quietly = TRUE)) install.packages("dfms")
library(dfms)

cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
indicateurs <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs.rds"))
indic_trim <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs_trimestriels.rds"))

H_TEST <- 8
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

bridge_simplifie <- function(cible_branche_df, indics_branche_df, date_limite) {
  cible_tr <- cible_branche_df %>% filter(date <= date_limite, !is.na(dlog_va))
  if (nrow(cible_tr) < 15) return(NA_real_)
  indics <- unique(indics_branche_df$indicateur)
  prevs <- c()
  for (ind_nom in indics) {
    ind_df <- indics_branche_df %>% filter(indicateur == ind_nom, date <= date_limite) %>% select(date, dlog)
    df <- inner_join(cible_tr, ind_df, by = "date") %>% filter(!is.na(dlog))
    if (nrow(df) < 10) next
    fit <- lm(dlog_va ~ dlog, data = df)
    ar1 <- tryCatch(Arima(ind_df$dlog[!is.na(ind_df$dlog)], order = c(1,0,0)), error = function(e) NULL)
    dlog_prevu <- if (!is.null(ar1)) as.numeric(forecast(ar1, h=1)$mean) else tail(ind_df$dlog, 1)
    prevs[ind_nom] <- as.numeric(predict(fit, newdata = data.frame(dlog = dlog_prevu)))
  }
  if (length(prevs) == 0) return(NA_real_)
  mean(prevs, na.rm = TRUE)
}

# --- DFM simplifie, contraint a date <= date_limite (reprend 04c_dfm.R) ---
dfm_simplifie <- function(branche, date_limite) {
  indics_b <- indicateurs %>% filter(branche == !!branche, frequence == "mensuel", date <= date_limite)
  if (n_distinct(indics_b$indicateur) < 2) return(NA_real_)

  indics_b <- indics_b %>% group_by(indicateur, date) %>% summarise(valeur = mean(valeur, na.rm = TRUE), .groups = "drop")
  panel <- indics_b %>% group_by(indicateur) %>% arrange(date) %>%
    mutate(dlog = c(NA, diff(log(pmax(valeur, 1e-6))))) %>% ungroup() %>%
    select(date, indicateur, dlog) %>%
    pivot_wider(names_from = indicateur, values_from = dlog, values_fn = mean) %>% arrange(date)
  if (nrow(panel) < 24) return(NA_real_)
  calendrier <- tibble(date = seq(min(panel$date), max(panel$date), by = "month"))
  panel <- calendrier %>% left_join(panel, by = "date") %>% arrange(date)

  mat <- as.matrix(panel[, -1, drop = FALSE]); storage.mode(mat) <- "double"
  mat_std <- scale(mat)
  fit_dfm <- tryCatch(DFM(mat_std, r = 1, p = 1, max.iter = 100), error = function(e) NULL)
  if (is.null(fit_dfm)) return(NA_real_)

  n_f <- nrow(fit_dfm$F_qml)
  dates_f <- tail(panel$date, n_f)
  facteur <- tibble(date = dates_f, facteur = as.numeric(fit_dfm$F_qml[, 1]))
  facteur_trim <- facteur %>% mutate(trimestre = floor_date(date, "quarter")) %>%
    group_by(trimestre) %>% summarise(facteur = mean(facteur, na.rm = TRUE)) %>% rename(date = trimestre)

  cible_b <- cibles %>% filter(branche == !!branche, date <= date_limite) %>% select(date, dlog_va)
  df <- inner_join(cible_b, facteur_trim, by = "date") %>% filter(!is.na(dlog_va), !is.na(facteur))
  if (nrow(df) < 12) return(NA_real_)
  fit_bridge <- lm(dlog_va ~ facteur, data = df)

  facteur_ar1 <- tryCatch(Arima(facteur$facteur[!is.na(facteur$facteur)], order = c(1,0,0)), error = function(e) NULL)
  facteur_prevu <- if (!is.null(facteur_ar1)) mean(as.numeric(forecast(facteur_ar1, h=3)$mean)) else tail(facteur$facteur, 1)
  as.numeric(predict(fit_bridge, newdata = data.frame(facteur = facteur_prevu)))
}

# --- Boucle de troncature successive -----------------------------------
erreurs_dfm <- c(); erreurs_ar2 <- c()
H_TEST_local <- H_TEST
mat_large_dates <- dates_vec
Tn <- Tn_complet
dates_test <- dates_vec[(Tn - H_TEST_local + 1):Tn]

cat("Backtest DFM en cours (", H_TEST_local, "origines) ...\n")
for (h in seq_along(dates_test)) {
  idx <- Tn - H_TEST_local + h - 1
  date_limite <- dates_vec[idx]
  Y_tr <- Y_complet[1:idx, , drop = FALSE]
  prev_bvar <- estimer_bvar_local(Y_tr, P_RETARDS, LAMBDA)

  prev_branches <- c()
  for (b in TOUTES_BRANCHES) {
    if (b %in% BRANCHES_COUVERTES) {
      pd <- tryCatch(dfm_simplifie(b, date_limite), error = function(e) NA_real_)
      if (!is.na(pd)) {
        prev_branches[b] <- pd
      } else {
        cible_b <- cibles %>% filter(branche == b) %>% select(date, dlog_va)
        indics_b <- indic_trim %>% filter(branche == b)
        pb <- bridge_simplifie(cible_b, indics_b, date_limite)
        prev_branches[b] <- if (is.na(pb)) prev_bvar[b] else 0.5 * prev_bvar[b] + 0.5 * pb
      }
    } else {
      serie_tr <- Y_tr[, b]
      fit_ar4 <- tryCatch(Arima(serie_tr, order = c(4,0,0)), error = function(e) NULL)
      prev_branches[b] <- if (is.null(fit_ar4)) prev_bvar[b] else as.numeric(forecast(fit_ar4, h=1)$mean)
    }
  }

  prevision_pib <- sum(prev_branches[names(poids)] * poids)
  reel_pib <- sum(Y_complet[idx + 1, names(poids)] * poids)
  erreurs_dfm[h] <- reel_pib - prevision_pib

  pib_total_tr <- as.vector(Y_tr %*% poids[colnames(Y_tr)])
  fit_ar2 <- Arima(pib_total_tr, order = c(2,0,0))
  erreurs_ar2[h] <- reel_pib - as.numeric(forecast(fit_ar2, h=1)$mean)

  cat(sprintf("  Origine %s : PIB modele DFM=%+.4f (reel=%+.4f)\n", date_limite, prevision_pib, reel_pib))
}

rmsfe_dfm <- sqrt(mean(erreurs_dfm^2))
rmsfe_ar2 <- sqrt(mean(erreurs_ar2^2))
test_dm <- tryCatch(dm.test(erreurs_dfm, erreurs_ar2, h = 1, power = 2), error = function(e) NULL)

cat(sprintf("\n=== BACKTEST DFM (%d trimestres) ===\n", H_TEST_local))
cat(sprintf("RMSFE variante DFM  : %.4f\n", rmsfe_dfm))
cat(sprintf("RMSFE repere AR(2)  : %.4f\n", rmsfe_ar2))
if (!is.null(test_dm)) cat(sprintf("Diebold-Mariano : p-value = %.3f\n", test_dm$p.value))

saveRDS(list(dates_test = dates_test, erreurs_modele = erreurs_dfm, erreurs_ar2 = erreurs_ar2,
             rmsfe_modele = rmsfe_dfm, rmsfe_ar2 = rmsfe_ar2, test_dm = test_dm),
        file.path(DOSSIER_RESULTATS, "backtest_dfm.rds"))
cat("\nBacktest DFM termine et sauvegarde.\n")
