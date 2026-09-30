# ============================================================================
# tests/test_dfm.R -- L'algorithme EM retrouve-t-il ce qu'il doit retrouver ?
# ============================================================================
# On simule un modele a facteurs dynamiques dont on connait tout : facteurs,
# chargements, variances, dynamique. On lui donne ensuite un panel troue, avec
# une cible trimestrielle observee un mois sur trois, et on verifie qu'il
# reconstitue ce qui est reconstituable.
#
# Une precaution de lecture : les facteurs d'un DFM ne sont identifies qu'a une
# ROTATION pres. Comparer F estime a F vrai coefficient par coefficient n'a
# donc aucun sens. On compare ce qui est invariant par rotation : l'espace
# engendre par les facteurs, et surtout la PREVISION de la cible, qui est la
# seule chose dont le projet a besoin.
# ============================================================================

source("R/fonctions/dfm.R")

set.seed(20260915)
ok <- TRUE
verifier <- function(nom, condition, detail = "") {
  ok <<- ok && isTRUE(condition)
  cat(sprintf("  [%s] %-56s %s\n", if (isTRUE(condition)) "OK" else "ECHEC",
              nom, detail))
}

# ------------------------------------------------------------- simulation
n_t <- 240; r_vrai <- 2; n_mens <- 15; p_vrai <- 1
A <- matrix(c(0.6, 0.15, -0.1, 0.45), r_vrai, r_vrai)
Q <- diag(c(1, 0.5))
F_vrai <- matrix(0, n_t, r_vrai)
for (t in 2:n_t) {
  F_vrai[t, ] <- as.vector(A %*% F_vrai[t - 1L, ]) + as.vector(chol(Q) %*% rnorm(r_vrai))
}
lam_m <- matrix(rnorm(n_mens * r_vrai), n_mens, r_vrai)
sig_m <- runif(n_mens, 0.2, 0.7)
Xm <- F_vrai %*% t(lam_m) + matrix(rnorm(n_t * n_mens), n_t, n_mens) %*% diag(sqrt(sig_m))

# La cible : croissance MENSUELLE latente, puis agregation de Mariano-Murasawa
lam_c <- c(0.9, -0.4)
g_mens <- as.vector(F_vrai %*% lam_c) + rnorm(n_t, 0, 0.3)
g_trim <- rep(NA_real_, n_t)
for (t in 5:n_t) {
  g_trim[t] <- sum(POIDS_MM * g_mens[t:(t - 4L)])
}
# observee seulement a la fin de chaque trimestre
mois_fin <- seq(6L, n_t, by = 3L)
cible <- rep(NA_real_, n_t); cible[mois_fin] <- g_trim[mois_fin]

X <- cbind(Xm, cible)
i_cible <- ncol(X)
cat(sprintf("\nPanel simule : %d mois, %d series mensuelles, 1 cible trimestrielle\n",
            n_t, n_mens))
cat(sprintf("taux de valeurs manquantes : %.0f %%\n", 100 * mean(is.na(X))))

# --------------------------------------------------- 1. convergence de l'EM
cat("\n=== 1. L'algorithme converge-t-il ? ===\n")
mod <- estimer_dfm(X, r = 2L, p = 1L, i_cible = i_cible, max_iter = 200L)
verifier("convergence atteinte", mod$converge,
         sprintf("%d iterations", mod$iterations))
verifier("la log-vraisemblance croit a chaque iteration",
         all(diff(mod$journal) > -1e-6),
         sprintf("%.1f -> %.1f", mod$journal[1], mod$journal[length(mod$journal)]))

# ------------------------------------------- 2. l'espace des facteurs
cat("\n=== 2. L'espace engendre par les facteurs est-il retrouve ? ===\n")
# R^2 de la regression de chaque facteur vrai sur les facteurs estimes.
r2 <- sapply(1:r_vrai, function(k)
  summary(stats::lm(F_vrai[, k] ~ mod$facteurs))$r.squared)
verifier("R2 de chaque facteur vrai sur les facteurs estimes > 0,90",
         all(r2 > 0.90), sprintf("R2 = %s", paste(round(r2, 3), collapse = " ; ")))

# ------------------------------------------- 3. la composante commune
cat("\n=== 3. La composante commune est-elle retrouvee ? ===\n")
# ATTENTION AU CHOIX DE L INVARIANT. L algorithme peut renvoyer n importe
# quelle transformation inversible des facteurs : F* = M F et Lambda* = Lambda
# M^{-1}. La matrice Lambda Lambda' n est donc PAS invariante (elle ne l est
# que par rotation orthogonale) : la comparer reviendrait a mesurer
# l arbitraire de la normalisation, non la qualite de l estimation.
#
# Ce qui est reellement invariant, c est la COMPOSANTE COMMUNE
# chi_t = Lambda F_t, puisque Lambda* F* = Lambda M^{-1} M F = Lambda F.
chi_vrai <- F_vrai %*% t(lam_m)
chi_est  <- mod$facteurs %*% t(mod$Lambda[1:n_mens, , drop = FALSE])
c_load <- stats::cor(as.vector(chi_vrai), as.vector(chi_est))
r2_chi <- 1 - sum((chi_vrai - chi_est)^2) / sum(scale(chi_vrai, scale = FALSE)^2)
verifier("correlation des composantes communes > 0,95",
         c_load > 0.95, sprintf("r = %.4f", c_load))
verifier("part de variance de la composante commune expliquee > 0,90",
         r2_chi > 0.90, sprintf("R2 = %.4f", r2_chi))

# ------------------------------------------- 4. la prevision de la cible
cat("\n=== 4. Ce qui compte vraiment : la prevision de la cible ===\n")
# On refait la prevision en temps reel sur les 30 derniers trimestres.
cibles_test <- mois_fin[mois_fin > n_t - 90L]
prev <- reel <- numeric(0)
for (tc in cibles_test) {
  Xtr <- X[1:tc, , drop = FALSE]
  Xtr[tc, i_cible] <- NA        # la cible du trimestre vise n'est pas connue
  m2 <- estimer_dfm(Xtr, r = 2L, p = 1L, i_cible = i_cible, max_iter = 60L)
  prev <- c(prev, prevoir_cible(m2, Xtr, h = 0L))
  reel <- c(reel, X[tc, i_cible])
}
rat <- sqrt(mean((reel - prev)^2)) / stats::sd(reel)
cr  <- stats::cor(prev, reel)
verifier("ratio d'erreur nettement sous 1", rat < 0.75,
         sprintf("ratio = %.3f sur %d trimestres", rat, length(reel)))
verifier("correlation prevu / realise > 0,70", cr > 0.70,
         sprintf("r = %.3f", cr))

# ------------------------------------------- 5. l'agregation temporelle sert-elle ?
cat("\n=== 5. Les poids d'agregation apportent-ils quelque chose ? ===\n")
# Variante naive : la cible charge le seul facteur courant.
POIDS_SAUVE <- POIDS_MM
POIDS_MM <<- c(1, 0, 0, 0, 0)
prev_n <- numeric(0)
for (tc in cibles_test) {
  Xtr <- X[1:tc, , drop = FALSE]; Xtr[tc, i_cible] <- NA
  m2 <- estimer_dfm(Xtr, r = 2L, p = 1L, i_cible = i_cible, max_iter = 60L)
  prev_n <- c(prev_n, prevoir_cible(m2, Xtr, h = 0L))
}
POIDS_MM <<- POIDS_SAUVE
rat_n <- sqrt(mean((reel - prev_n)^2)) / stats::sd(reel)
verifier("l'agregation correcte bat la convention naive", rat < rat_n,
         sprintf("%.3f contre %.3f, soit %+.0f %%",
                 rat, rat_n, 100 * (rat_n - rat) / rat_n))

# ------------------------------------------- 6. anti-anteriorite
cat("\n=== 6. La prevision lit-elle le futur ? ===\n")
tc <- cibles_test[10]
Xa <- X[1:tc, , drop = FALSE]; Xa[tc, i_cible] <- NA
ma <- estimer_dfm(Xa, r = 2L, p = 1L, i_cible = i_cible, max_iter = 60L)
p_a <- prevoir_cible(ma, Xa, h = 0L)
Xb <- X; Xb[(tc + 1L):n_t, ] <- Xb[(tc + 1L):n_t, ] * 3 + 5
Xb <- Xb[1:tc, , drop = FALSE]; Xb[tc, i_cible] <- NA
mb <- estimer_dfm(Xb, r = 2L, p = 1L, i_cible = i_cible, max_iter = 60L)
p_b <- prevoir_cible(mb, Xb, h = 0L)
verifier("une perturbation du futur ne change rien a la prevision",
         abs(p_a - p_b) < 1e-10, sprintf("ecart %.1e", abs(p_a - p_b)))

cat("\n", strrep("-", 76), "\n", sep = "")
if (ok) cat("Tous les controles passent : l'estimateur DFM est valide.\n") else
  stop("Au moins un controle a echoue.", call. = FALSE)
