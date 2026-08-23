# ============================================================================
# 09_test_affinement_intra_trimestre.R -- Le modele s'ameliore-t-il au fil
#                                          du trimestre ? (coeur meme de la
#                                          promesse de GDPNow)
# ============================================================================
# Tout ce qu'on a teste jusqu'ici repond a la question "le modele prevoit-il
# mieux qu'un AR(2), a un instant fige (juste avant le trimestre) ?". Mais
# l'interet reel de GDPNow n'est pas la, il est dans le fait que la
# prevision s'affine A MESURE que les indicateurs mensuels du trimestre
# arrivent -- jamais mesure jusqu'ici.
#
# Protocole : pour chacun des 16 trimestres deja testes, on recalcule la
# prevision du PIB a 3 "etages" d'information differents :
#   - Etage 0 (M0) : aucune donnee du trimestre cible disponible -- c'est
#     exactement le backtest deja fait (07_validation_pseudo_temps_reel.R)
#   - Etage 1 (M1) : le 1er mois du trimestre cible est desormais disponible
#     pour les indicateurs (mais jamais pour la cible VA elle-meme, qui
#     n'est publiee qu'en fin de trimestre suivant -- contrainte de temps
#     reel respectee)
#   - Etage 2 (M2) : les 2 premiers mois du trimestre cible sont disponibles
#
# Le BVAR et les branches AR(4) restent IDENTIQUES aux 3 etages (leur
# information -- les VA passees -- ne change jamais en cours de trimestre,
# c'est la ou la contrainte reelle du "jagged edge" est la plus dure). Seule
# la partie bridge equation (indicateurs mensuels) evolue -- c'est
# precisement le mecanisme que GDPNow est cense apporter.
#
# Si le RMSFE diminue de M0 a M2, c'est une validation reelle du concept,
# independamment du fait que le modele batte ou non l'AR(2) a horizon fixe.
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
indicateurs <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs.rds"))
bvar_complet <- readRDS(file.path(DOSSIER_RESULTATS, "bvar_modele.rds"))
bridge_complet <- readRDS(file.path(DOSSIER_RESULTATS, "bridge_equations.rds"))

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

# --- BVAR (reutilise du script 07, inchange) --------------------------------
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

#' Prevision bridge pour une branche, a une date limite d'INDICATEURS donnee
#' (peut inclure 1 ou 2 mois du trimestre cible), mais toujours combinee
#' avec la prevision BVAR calculee sur les VA passees uniquement (figee).
bridge_a_date <- function(branche, date_limite_indicateurs, prevision_bvar_branche, delta_branche) {
  cible_b <- cibles %>% filter(branche == !!branche) %>% select(date, dlog_va)
  indics_b <- indicateurs %>% filter(branche == !!branche) %>% distinct(indicateur)
  if (nrow(indics_b) == 0) return(NA_real_)

  prevs <- c()
  for (ind_nom in indics_b$indicateur) {
    indic_df <- indicateurs %>% filter(branche == !!branche, indicateur == ind_nom) %>% select(date, valeur)
    serie_complete <- completer_serie_kalman_limitee(indic_df, date_limite_indicateurs)
    if (is.null(serie_complete)) next
    serie_complete <- serie_complete %>% arrange(date) %>% mutate(dlog = c(NA, diff(log(pmax(valeur, 1e-6)))))
    serie_trim <- serie_complete %>% mutate(trimestre = floor_date(date, "quarter")) %>%
      group_by(trimestre) %>% summarise(dlog = mean(dlog, na.rm = TRUE)) %>% rename(date = trimestre)
    df <- inner_join(cible_b, serie_trim, by = "date") %>% filter(!is.na(dlog_va), !is.na(dlog))
    if (nrow(df) < 10) next
    fit <- lm(dlog_va ~ dlog, data = df)
    # le dernier point de serie_trim EST la prevision du trimestre cible des
    # que >=1 mois du trimestre est disponible (moyenne partielle, comme le
    # ferait un vrai bridge equation en temps reel)
    dlog_prevu <- tail(serie_trim$dlog, 1)
    prevs[ind_nom] <- as.numeric(predict(fit, newdata = data.frame(dlog = dlog_prevu)))
  }
  if (length(prevs) == 0) return(NA_real_)
  prevision_bridge <- mean(prevs, na.rm = TRUE)
  delta_branche * prevision_bvar_branche + (1 - delta_branche) * prevision_bridge
}

resultats_etages <- list(M0 = c(), M1 = c(), M2 = c())
dates_test <- dates_vec[(Tn_complet - H_TEST + 1):Tn_complet]

cat("Test d'affinement intra-trimestriel en cours (", H_TEST, "trimestres x 3 etages) ...\n")
for (h in seq_along(dates_test)) {
  idx <- Tn_complet - H_TEST + h - 1
  date_limite_cible <- dates_vec[idx]          # fin du trimestre precedent (T-1)
  date_trimestre_cible <- dates_vec[idx + 1]   # le trimestre qu'on cherche a prevoir
  Y_tr <- Y_complet[1:idx, , drop = FALSE]

  prev_bvar <- estimer_bvar_local(Y_tr, P_RETARDS, LAMBDA, covid_dummy[1:idx])

  # --- delta par branche : reutilise du modele complet (fige, comme discute) ---
  deltas <- sapply(bridge_complet, `[[`, "delta")

  for (etage in c("M0", "M1", "M2")) {
    n_mois <- switch(etage, M0 = 0, M1 = 1, M2 = 2)
    date_limite_indic <- date_trimestre_cible %m+% months(n_mois) %m+% months(1) - 1  # fin du n-ieme mois du trimestre cible

    prev_branches <- c()
    for (b in colnames(Y_complet)) {
      if (b %in% BRANCHES_COUVERTES) {
        pb <- bridge_a_date(b, date_limite_indic, prev_bvar[b], deltas[b])
        prev_branches[b] <- if (is.na(pb)) prev_bvar[b] else pb
      } else {
        fit_ar4 <- tryCatch(Arima(Y_tr[, b], order = c(4,0,0)), error = function(e) NULL)
        prev_branches[b] <- if (is.null(fit_ar4)) prev_bvar[b] else as.numeric(forecast(fit_ar4, h=1)$mean)
      }
    }
    prevision_pib <- sum(prev_branches[names(poids)] * poids)
    reel_pib <- sum(Y_complet[idx + 1, names(poids)] * poids)
    resultats_etages[[etage]][h] <- reel_pib - prevision_pib
  }
  cat(sprintf("  Trimestre %s : erreur M0=%+.4f | M1=%+.4f | M2=%+.4f\n",
              date_trimestre_cible, resultats_etages$M0[h], resultats_etages$M1[h], resultats_etages$M2[h]))
}

rmsfe_M0 <- sqrt(mean(resultats_etages$M0^2))
rmsfe_M1 <- sqrt(mean(resultats_etages$M1^2))
rmsfe_M2 <- sqrt(mean(resultats_etages$M2^2))

cat("\n=== RESULTAT : le RMSFE diminue-t-il au fil du trimestre ? ===\n")
cat(sprintf("Etage M0 (avant le trimestre, aucune donnee du trimestre cible) : RMSFE = %.4f\n", rmsfe_M0))
cat(sprintf("Etage M1 (1 mois du trimestre cible disponible)                : RMSFE = %.4f\n", rmsfe_M1))
cat(sprintf("Etage M2 (2 mois du trimestre cible disponibles)               : RMSFE = %.4f\n", rmsfe_M2))
cat(sprintf("\nAmelioration M0->M1 : %.1f %%\n", (1 - rmsfe_M1/rmsfe_M0)*100))
cat(sprintf("Amelioration M0->M2 : %.1f %%\n", (1 - rmsfe_M2/rmsfe_M0)*100))

test_M0_M2 <- tryCatch(dm.test(resultats_etages$M0, resultats_etages$M2, h=1, power=2), error=function(e) NULL)
if (!is.null(test_M0_M2)) {
  cat(sprintf("\nTest de Diebold-Mariano (M0 vs M2) : p-value = %.3f\n", test_M0_M2$p.value))
}

saveRDS(list(dates_test = dates_test, resultats_etages = resultats_etages,
             rmsfe_M0 = rmsfe_M0, rmsfe_M1 = rmsfe_M1, rmsfe_M2 = rmsfe_M2,
             test_M0_M2 = test_M0_M2),
        file.path(DOSSIER_RESULTATS, "test_affinement_intra_trimestre.rds"))

df_fig <- tibble(
  etage = factor(c("Avant le trimestre (M0)", "1 mois disponible (M1)", "2 mois disponibles (M2)"),
                  levels = c("Avant le trimestre (M0)", "1 mois disponible (M1)", "2 mois disponibles (M2)")),
  RMSFE = c(rmsfe_M0, rmsfe_M1, rmsfe_M2)
)
p <- ggplot(df_fig, aes(etage, RMSFE, group = 1)) +
  geom_line(color = "#2E74B5", linewidth = 1) + geom_point(size = 3, color = "#2E74B5") +
  geom_hline(yintercept = rmsfe_M0, linetype = "dashed", color = "grey60") +
  labs(title = "Le nowcast s'améliore-t-il au fil du trimestre ?",
       subtitle = "RMSFE de la Méthode 1, selon la quantité d'information disponible dans le trimestre cible",
       x = NULL, y = "RMSFE") +
  theme_minimal(base_size = 12)
ggsave(file.path(DOSSIER_FIGURES, "09_affinement_intra_trimestre.png"), p, width = 8, height = 5.5, dpi = 150)

cat("\nTest termine et sauvegarde.\n")
