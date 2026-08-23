# ============================================================================
# 07b_validation_midas.R -- Backtest pseudo temps reel, variante MIDAS
# ============================================================================
# NOUVEAU SCRIPT. Meme protocole que 07_validation_pseudo_temps_reel.R
# (troncature successive de l'echantillon, RMSFE, test de Diebold-Mariano
# contre le meme repere AR(2) sur le PIB total). A chaque origine, le MIDAS
# est REESTIME sur les donnees tronquees (pas de fuite d'information) pour
# les branches couvertes ou il est calculable ; repli sur bridge+BVAR
# simplifie (identique au script original) sinon.
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
indicateurs <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs.rds"))
indic_trim <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs_trimestriels.rds"))

H_TEST <- 8
P_RETARDS <- 5
LAMBDA <- 0.15
MAXGAP_INTERPOLATION <- 3

mat_large <- cibles %>% select(branche, date, dlog_va) %>%
  pivot_wider(names_from = branche, values_from = dlog_va) %>% arrange(date) %>%
  filter(if_all(everything(), ~ !is.na(.)))
dates_vec <- mat_large$date
Y_complet <- as.matrix(mat_large[, TOUTES_BRANCHES])
Tn_complet <- nrow(Y_complet)

dernier_trim <- cibles %>% group_by(branche) %>% filter(date == max(date)) %>% ungroup() %>% select(branche, va)
poids <- (dernier_trim$va / sum(dernier_trim$va)) %>% setNames(dernier_trim$branche)

# --- Fonctions reprises telles quelles de 07_validation_pseudo_temps_reel.R -
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

# --- Fonctions MIDAS, reprises de 04b_midas.R, contraintes a date <= date_limite
completer_serie_mensuelle <- function(indic_df) {
  indic_df <- indic_df %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
  calendrier <- tibble(date = seq(min(indic_df$date), max(indic_df$date), by = "month"))
  serie <- calendrier %>% left_join(indic_df %>% select(date, valeur), by = "date")
  if (any(is.na(serie$valeur))) {
    serie$valeur <- zoo::na.approx(serie$valeur, x = serie$date, na.rm = FALSE, maxgap = MAXGAP_INTERPOLATION)
  }
  serie
}
construire_midas_indicateur <- function(indic_df_complet) {
  indic_df_complet %>% arrange(date) %>%
    mutate(dlog = c(NA, diff(log(pmax(valeur, 1e-6)))),
           trimestre = floor_date(date, "quarter"),
           mois_dans_trim = interval(trimestre, date) %/% months(1) + 1) %>%
    select(trimestre, mois_dans_trim, dlog) %>%
    pivot_wider(names_from = mois_dans_trim, values_from = dlog, names_prefix = "m") %>%
    arrange(trimestre)
}
midas_simplifie_un_indicateur <- function(cible_df, indic_df_brut, date_limite) {
  indic_df_brut <- indic_df_brut %>% filter(date <= date_limite)
  cible_df <- cible_df %>% filter(date <= date_limite)
  if (nrow(indic_df_brut) < 24) return(NA_real_)
  serie_complete <- completer_serie_mensuelle(indic_df_brut)
  if (mean(is.na(serie_complete$valeur)) > 0.30) return(NA_real_)
  large <- construire_midas_indicateur(serie_complete)
  cols_m <- intersect(c("m1", "m2", "m3"), names(large))
  if (length(cols_m) < 2) return(NA_real_)
  df <- cible_df %>% rename(trimestre = date) %>% inner_join(large, by = "trimestre") %>% filter(!is.na(dlog_va))
  df_complet <- df %>% filter(if_all(all_of(cols_m), ~ !is.na(.)))
  if (nrow(df_complet) < 12) return(NA_real_)
  formule <- as.formula(paste("dlog_va ~", paste(cols_m, collapse = " + ")))
  fit <- lm(formule, data = df_complet)
  derniere <- tail(large, 1)
  serie_m_ar1 <- serie_complete %>% mutate(dlog = c(NA, diff(log(pmax(valeur, 1e-6))))) %>% pull(dlog) %>% na.omit()
  ar1 <- tryCatch(Arima(serie_m_ar1, order = c(1,0,0)), error = function(e) NULL)
  x_new <- list()
  for (cc in cols_m) {
    val <- derniere[[cc]]
    if (is.null(val) || is.na(val)) val <- if (!is.null(ar1)) as.numeric(forecast(ar1, h=1)$mean) else tail(serie_m_ar1, 1)
    x_new[[cc]] <- val
  }
  as.numeric(predict(fit, newdata = as.data.frame(x_new)))
}
midas_simplifie <- function(branche, date_limite) {
  cible_b <- cibles %>% filter(branche == !!branche) %>% select(date, dlog_va)
  indics_b <- indicateurs %>% filter(branche == !!branche, frequence == "mensuel") %>% distinct(indicateur) %>% pull(indicateur)
  if (length(indics_b) == 0) return(NA_real_)
  prevs <- c()
  for (ind_nom in indics_b) {
    indic_df <- indicateurs %>% filter(branche == !!branche, indicateur == ind_nom) %>% select(date, valeur)
    p <- midas_simplifie_un_indicateur(cible_b, indic_df, date_limite)
    if (!is.na(p)) prevs[ind_nom] <- p
  }
  if (length(prevs) == 0) return(NA_real_)
  mean(prevs, na.rm = TRUE)
}

# --- Boucle de troncature successive -----------------------------------
erreurs_midas <- c(); erreurs_ar2 <- c()
dates_test <- dates_vec[(Tn_complet - H_TEST + 1):Tn_complet]

cat("Backtest MIDAS en cours (", H_TEST, "origines) ...\n")
for (h in seq_along(dates_test)) {
  idx <- Tn_complet - H_TEST + h - 1
  date_limite <- dates_vec[idx]
  Y_tr <- Y_complet[1:idx, , drop = FALSE]
  prev_bvar <- estimer_bvar_local(Y_tr, P_RETARDS, LAMBDA)

  prev_branches <- c()
  for (b in TOUTES_BRANCHES) {
    if (b %in% BRANCHES_COUVERTES) {
      pm <- midas_simplifie(b, date_limite)
      if (!is.na(pm)) {
        prev_branches[b] <- pm
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
  erreurs_midas[h] <- reel_pib - prevision_pib

  pib_total_tr <- as.vector(Y_tr %*% poids[colnames(Y_tr)])
  fit_ar2 <- Arima(pib_total_tr, order = c(2,0,0))
  erreurs_ar2[h] <- reel_pib - as.numeric(forecast(fit_ar2, h=1)$mean)

  cat(sprintf("  Origine %s : PIB modele MIDAS=%+.4f (reel=%+.4f) | AR(2)=%+.4f\n",
              date_limite, prevision_pib, reel_pib, as.numeric(forecast(fit_ar2, h=1)$mean)))
}

rmsfe_midas <- sqrt(mean(erreurs_midas^2))
rmsfe_ar2 <- sqrt(mean(erreurs_ar2^2))
test_dm <- tryCatch(dm.test(erreurs_midas, erreurs_ar2, h = 1, power = 2), error = function(e) NULL)

cat(sprintf("\n=== BACKTEST MIDAS (%d trimestres) ===\n", H_TEST))
cat(sprintf("RMSFE variante MIDAS : %.4f\n", rmsfe_midas))
cat(sprintf("RMSFE repere AR(2)   : %.4f\n", rmsfe_ar2))
if (!is.null(test_dm)) cat(sprintf("Diebold-Mariano : p-value = %.3f\n", test_dm$p.value))

saveRDS(list(dates_test = dates_test, erreurs_modele = erreurs_midas, erreurs_ar2 = erreurs_ar2,
             rmsfe_modele = rmsfe_midas, rmsfe_ar2 = rmsfe_ar2, test_dm = test_dm),
        file.path(DOSSIER_RESULTATS, "backtest_midas.rds"))
cat("\nBacktest MIDAS termine et sauvegarde.\n")
