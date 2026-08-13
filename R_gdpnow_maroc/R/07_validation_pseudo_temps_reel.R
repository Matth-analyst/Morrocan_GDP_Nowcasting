# ============================================================================
# 07_validation_pseudo_temps_reel.R -- Backtest RMSFE + test de Diebold-Mariano
# ============================================================================
# Reference : Higgins (2014), section 4 "Horse Races" -- algorithme de
# troncature successive de l'echantillon, comparaison a des modeles de
# repere via la RMSFE (root mean square forecast error) et test de
# Diebold & Mariano (1995) pour juger si l'ecart de precision est
# statistiquement significatif.
#
# Repere de comparaison : AR(2) sur la croissance du PIB total agrege
# (modele 1 du tableau 5 de Higgins, 2014).
#
# Fenetre de test : les H_TEST derniers trimestres disponibles.
# A chaque origine, le BVAR (16 branches) et les bridge equations sont
# RE-ESTIMES sur les donnees tronquees (pas de fuite d'information), une
# prevision a un pas est produite, comparee a la valeur reellement
# observee ensuite.
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
indic_trim <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs_trimestriels.rds"))

H_TEST <- 8      # nombre de trimestres reserves au test hors echantillon
P_RETARDS <- 5
LAMBDA <- 0.15

mat_large <- cibles %>%
  select(branche, date, dlog_va) %>%
  pivot_wider(names_from = branche, values_from = dlog_va) %>%
  arrange(date) %>%
  filter(if_all(everything(), ~ !is.na(.)))
dates_vec <- mat_large$date
Y_complet <- as.matrix(mat_large[, TOUTES_BRANCHES])
Tn_complet <- nrow(Y_complet)

# --- Poids de ponderation (parts de VA, dernier trimestre -- meme limite
#     documentee dans 06_agregation_fisher.R) ---
dernier_trim <- cibles %>% group_by(branche) %>% filter(date == max(date)) %>% ungroup() %>% select(branche, va)
poids <- (dernier_trim$va / sum(dernier_trim$va)) %>% setNames(dernier_trim$branche)

# --- Fonctions reprises des scripts 03-05 (factorisees pour le backtest) --
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

# --- Boucle de troncature successive (pseudo temps reel) -------------------
erreurs_modele_complet <- c()
erreurs_ar2_benchmark <- c()
dates_test <- dates_vec[(Tn_complet - H_TEST + 1):Tn_complet]

cat("Backtest en cours (", H_TEST, "origines) ...\n")
for (h in seq_along(dates_test)) {
  idx_limite <- Tn_complet - H_TEST + h - 1   # dernier point inclus dans l'estimation
  date_limite <- dates_vec[idx_limite]
  date_cible <- dates_vec[idx_limite + 1]
  Y_tr <- Y_complet[1:idx_limite, , drop = FALSE]

  prev_bvar <- estimer_bvar_local(Y_tr, P_RETARDS, LAMBDA)

  prev_branches <- c()
  for (b in TOUTES_BRANCHES) {
    if (b %in% BRANCHES_COUVERTES) {
      cible_b <- cibles %>% filter(branche == b) %>% select(date, dlog_va)
      indics_b <- indic_trim %>% filter(branche == b)
      pb <- bridge_simplifie(cible_b, indics_b, date_limite)
      prev_branches[b] <- if (is.na(pb)) prev_bvar[b] else 0.5 * prev_bvar[b] + 0.5 * pb
    } else {
      serie_tr <- Y_tr[, b]
      fit_ar4 <- tryCatch(Arima(serie_tr, order = c(4,0,0)), error = function(e) NULL)
      prev_branches[b] <- if (is.null(fit_ar4)) prev_bvar[b] else as.numeric(forecast(fit_ar4, h=1)$mean)
    }
  }

  prevision_pib <- sum(prev_branches[names(poids)] * poids)
  reel_pib <- sum(Y_complet[idx_limite + 1, names(poids)] * poids)
  erreurs_modele_complet[h] <- reel_pib - prevision_pib

  # --- Repere AR(2) sur la croissance du PIB total (reconstitue) ---------
  pib_total_tr <- as.vector(Y_tr %*% poids[colnames(Y_tr)])
  fit_ar2 <- Arima(pib_total_tr, order = c(2,0,0))
  prev_ar2 <- as.numeric(forecast(fit_ar2, h = 1)$mean)
  erreurs_ar2_benchmark[h] <- reel_pib - prev_ar2

  cat(sprintf("  Origine %s -> cible %s : modele=%+.4f (reel=%+.4f) | AR(2)=%+.4f\n",
              date_limite, date_cible, prevision_pib, reel_pib, prev_ar2))
}

rmsfe_modele <- sqrt(mean(erreurs_modele_complet^2))
rmsfe_ar2 <- sqrt(mean(erreurs_ar2_benchmark^2))

cat(sprintf("\n=== RESULTATS DU BACKTEST (%d trimestres) ===\n", H_TEST))
cat(sprintf("RMSFE modele complet (BVAR + bridge + AR4)  : %.4f\n", rmsfe_modele))
cat(sprintf("RMSFE repere AR(2) sur le PIB total          : %.4f\n", rmsfe_ar2))
cat(sprintf("Gain relatif du modele complet                : %.1f %%\n",
            (1 - rmsfe_modele / rmsfe_ar2) * 100))

test_dm <- tryCatch(
  dm.test(erreurs_modele_complet, erreurs_ar2_benchmark, h = 1, power = 2),
  error = function(e) NULL
)
if (!is.null(test_dm)) {
  cat(sprintf("\nTest de Diebold-Mariano (H0 : precision egale) : statistique=%.3f, p-value=%.3f\n",
              test_dm$statistic, test_dm$p.value))
  if (test_dm$p.value < 0.10) {
    cat("=> Difference de precision significative au seuil de 10%.\n")
  } else {
    cat("=> Difference de precision NON significative (echantillon de test trop court : ",
        H_TEST, " trimestres seulement -- a interpreter avec prudence).\n", sep = "")
  }
}

resultats_backtest <- list(dates_test = dates_test, erreurs_modele = erreurs_modele_complet,
                            erreurs_ar2 = erreurs_ar2_benchmark, rmsfe_modele = rmsfe_modele,
                            rmsfe_ar2 = rmsfe_ar2, test_dm = test_dm)
saveRDS(resultats_backtest, file.path(DOSSIER_RESULTATS, "backtest.rds"))

# --- Figure : erreurs de prevision, modele vs repere -----------------------
df_erreurs <- tibble(date = dates_test, Modele = abs(erreurs_modele_complet), `AR(2)` = abs(erreurs_ar2_benchmark)) %>%
  pivot_longer(-date, names_to = "modele", values_to = "erreur_absolue")
p8 <- ggplot(df_erreurs, aes(date, erreur_absolue, color = modele)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  scale_color_manual(values = c("Modele" = "#2E74B5", "AR(2)" = "#C55A11")) +
  labs(title = "Erreur de prévision absolue — modèle complet vs. repère AR(2)",
       subtitle = sprintf("RMSFE modèle = %.4f | RMSFE AR(2) = %.4f", rmsfe_modele, rmsfe_ar2),
       x = NULL, y = "|Erreur| (Δlog)", color = NULL)
ggsave(file.path(DOSSIER_FIGURES, "08_backtest_erreurs.png"), p8, width = 9, height = 5, dpi = 150)

cat("\nBacktest termine et sauvegarde.\n")
