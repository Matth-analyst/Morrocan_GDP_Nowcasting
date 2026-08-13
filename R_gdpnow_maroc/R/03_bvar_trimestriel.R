# ============================================================================
# 03_bvar_trimestriel.R -- BVAR trimestriel a prior Minnesota (16 branches)
# ============================================================================
# Reference : Litterman (1986) ; Banbura, Giannone, Reichlin (2010) "Large
# Bayesian VARs" ; methode des "observations fictives" (dummy observations)
# reprise telle que decrite dans l'annexe du papier GDPNow (Higgins, 2014,
# section "Implementing the BVARs").
#
# Choix de specification :
#   - Variables : taux de croissance trimestriels (Δlog) des 16 VA de
#     branche -- deja stationnaires (cf. 02_analyse_exploratoire.R, tests
#     ADF : 16/16 stationnaires en Δlog contre 1/16 en niveau).
#   - Comme les variables sont deja des taux de croissance (pas des
#     log-niveaux), le prior de retour a la moyenne (delta_i = 0) est
#     utilise pour toutes les variables -- pas le prior de marche
#     aleatoire (delta_i = 1) qui s'applique aux log-niveaux dans le
#     papier original.
#   - p = 5 retards (identique au BVAR trimestriel de GDPNow, Table A1).
#   - lambda = 0.15 (identique au choix de Higgins pour son BVAR
#     trimestriel des composantes de quantite -- meme nature de variable).
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))

P_RETARDS <- 5
LAMBDA <- 0.15

# --- Construction de la matrice Y (T x n), une colonne par branche ---------
mat_large <- cibles %>%
  select(branche, date, dlog_va) %>%
  pivot_wider(names_from = branche, values_from = dlog_va) %>%
  arrange(date) %>%
  filter(if_all(everything(), ~ !is.na(.)))

dates_vec <- mat_large$date
Y <- as.matrix(mat_large[, TOUTES_BRANCHES])
Tn <- nrow(Y); n <- ncol(Y)
cat(sprintf("Matrice Y : %d observations x %d branches (%s a %s)\n",
            Tn, n, dates_vec[1], dates_vec[Tn]))

# --- Echelle de chaque serie (sigma_i), via un AR(p) univarie --------------
# Sert a rendre le prior comparable entre variables d'echelles différentes
# (Banbura, Giannone, Reichlin 2010, section 2.1)
sigma_i <- sapply(1:n, function(i) {
  fit <- tryCatch(ar.ols(Y[, i], order.max = P_RETARDS, aic = FALSE, demean = TRUE),
                   error = function(e) NULL)
  if (is.null(fit)) return(sd(Y[, i]))
  sqrt(fit$var.pred)
})
names(sigma_i) <- colnames(Y)

# --- Construction des observations reelles (X, Y) pour un VAR(p) ----------
construire_XY <- function(Y, p) {
  Tn <- nrow(Y); n <- ncol(Y)
  Y_reg <- Y[(p+1):Tn, , drop = FALSE]
  X_reg <- matrix(1, nrow = Tn - p, ncol = 1 + n * p)  # colonne de constante
  for (l in 1:p) {
    X_reg[, (2 + (l-1)*n):(1 + l*n)] <- Y[(p+1-l):(Tn-l), , drop = FALSE]
  }
  list(Y = Y_reg, X = X_reg)
}

# --- Observations fictives du prior Minnesota (delta_i = 0 pour tous) -----
# Bloc 1 : prior sur les coefficients (retour a la moyenne, retards distants
#          moins contraints -- facteur l^1)
# Bloc 2 : prior sur la matrice de covariance des residus
# Bloc 3 : prior lache sur la constante (grande variance = peu informatif)
construire_dummies <- function(sigma_i, n, p, lambda) {
  Yd1 <- matrix(0, nrow = n * p, ncol = n)  # delta_i = 0 => cible nulle sur le 1er retard
  Xd1 <- matrix(0, nrow = n * p, ncol = 1 + n * p)
  for (l in 1:p) {
    rows <- ((l-1)*n + 1):(l*n)
    Xd1[rows, (2 + (l-1)*n):(1 + l*n)] <- diag(sigma_i * l / lambda)
  }
  Yd2 <- matrix(0, nrow = n, ncol = n); diag(Yd2) <- sigma_i
  Xd2 <- matrix(0, nrow = n, ncol = 1 + n * p)

  eps <- 1e-5
  Yd3 <- matrix(0, nrow = 1, ncol = n)
  Xd3 <- matrix(0, nrow = 1, ncol = 1 + n * p); Xd3[1, 1] <- eps

  list(Y = rbind(Yd1, Yd2, Yd3), X = rbind(Xd1, Xd2, Xd3))
}

estimer_bvar <- function(Y, p, lambda, sigma_i) {
  n <- ncol(Y)
  reel <- construire_XY(Y, p)
  dum <- construire_dummies(sigma_i, n, p, lambda)
  Y_aug <- rbind(reel$Y, dum$Y)
  X_aug <- rbind(reel$X, dum$X)
  B <- solve(t(X_aug) %*% X_aug) %*% t(X_aug) %*% Y_aug  # (1+np) x n
  residus <- reel$Y - reel$X %*% B
  list(B = B, residus = residus, X_dernier = reel$X[nrow(reel$X), ])
}

modele <- estimer_bvar(Y, P_RETARDS, LAMBDA, sigma_i)

# --- Prevision a un pas (prochain trimestre, pour les 16 branches) --------
# Construit le vecteur explicatif pour T+1 a partir des p derniers Y observes
construire_X_prevision <- function(Y, p) {
  Tn <- nrow(Y); n <- ncol(Y)
  x <- c(1, as.vector(t(Y[Tn:(Tn-p+1), , drop = FALSE])))
  matrix(x, nrow = 1)
}
X_new <- construire_X_prevision(Y, P_RETARDS)
prevision_bvar <- as.vector(X_new %*% modele$B)
names(prevision_bvar) <- colnames(Y)

prochain_trimestre <- dates_vec[Tn] %m+% months(3)
cat(sprintf("\n--- Prevision BVAR pour %s (Δlog, croissance trimestrielle) ---\n",
            format(prochain_trimestre, "%Y-%m")))
print(round(sort(prevision_bvar, decreasing = TRUE), 4))

saveRDS(list(modele = modele, sigma_i = sigma_i, Y = Y, dates = dates_vec,
             p = P_RETARDS, lambda = LAMBDA, prevision = prevision_bvar,
             date_prevision = prochain_trimestre),
        file.path(DOSSIER_RESULTATS, "bvar_modele.rds"))

# --- Figure : previsions BVAR (prochain trimestre) par branche ------------
df_prev <- tibble(branche = names(prevision_bvar), prevision = prevision_bvar) %>%
  mutate(branche = factor(branche, levels = branche[order(prevision)]))
p5 <- ggplot(df_prev, aes(branche, prevision, fill = prevision > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(`TRUE` = "#2E74B5", `FALSE` = "#C55A11"), guide = "none") +
  labs(title = sprintf("Prévision BVAR de croissance trimestrielle — %s",
                        format(prochain_trimestre, "%Y T%q") %>% str_replace("%q", "")),
       subtitle = "Modèle autorégressif bayésien à prior Minnesota, 16 branches, 5 retards",
       x = NULL, y = "Δlog prévu")
ggsave(file.path(DOSSIER_FIGURES, "05_previsions_bvar.png"), p5, width = 8, height = 7, dpi = 150)

cat("\nModele BVAR sauvegarde.\n")
