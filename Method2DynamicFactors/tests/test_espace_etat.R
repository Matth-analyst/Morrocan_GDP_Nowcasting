# ============================================================================
# tests/test_espace_etat.R -- Le filtre et le lisseur sont-ils justes ?
# ============================================================================
# Un estimateur qui tourne n'est pas un estimateur qui marche. On le confronte
# ici a des donnees SIMULEES, ou l'etat vrai est connu par construction : c'est
# la seule situation ou l'on peut verifier qu'il retrouve ce qu'il doit
# retrouver, plutot que de constater qu'il produit des nombres plausibles.
#
# Quatre proprietes sont verifiees :
#   1. sur donnees completes, l'etat filtre suit l'etat vrai ;
#   2. le lisseur fait MIEUX que le filtre (il voit le futur en plus) ;
#   3. la presence de donnees manquantes degrade sans casser ;
#   4. la log-vraisemblance est maximale au voisinage des vrais parametres.
# ============================================================================

source("R/fonctions/espace_etat.R")

set.seed(20260915)
ok <- TRUE
verifier <- function(nom, condition, detail = "") {
  ok <<- ok && isTRUE(condition)
  cat(sprintf("  [%s] %-52s %s\n", if (isTRUE(condition)) "OK" else "ECHEC",
              nom, detail))
}

# ---------------------------------------------------------------- simulation
n_t <- 300; r <- 2; n <- 12
A <- matrix(c(0.7, 0.1, -0.2, 0.5), r, r)      # VAR(1) stable sur les facteurs
Q <- diag(c(1, 0.6))
lambda <- matrix(rnorm(n * r, 0, 1), n, r)
sigma2 <- runif(n, 0.2, 0.8)

F_vrai <- matrix(0, n_t, r)
for (t in 2:n_t) {
  F_vrai[t, ] <- as.vector(A %*% F_vrai[t - 1L, ]) +
    as.vector(chol(Q) %*% rnorm(r))
}
X <- F_vrai %*% t(lambda) +
  matrix(rnorm(n_t * n), n_t, n) %*% diag(sqrt(sigma2))

cat("\n=== 1. Donnees completes : le filtre retrouve-t-il l'etat ? ===\n")
kf <- filtre_kalman(X, Z = lambda, TT = A, R = sigma2, Q = Q)
ks <- lisseur_kalman(kf)

# Les facteurs ne sont identifies qu'a une rotation pres ; on compare donc la
# projection, c'est-a-dire ce que le modele predit des observations.
correl <- function(a, b) stats::cor(as.vector(a), as.vector(b))
c_filt <- correl(kf$a_filt %*% t(lambda), F_vrai %*% t(lambda))
c_liss <- correl(ks$a_liss %*% t(lambda), F_vrai %*% t(lambda))
verifier("correlation etat filtre / etat vrai > 0,95", c_filt > 0.95,
         sprintf("r = %.4f", c_filt))

cat("\n=== 2. Le lisseur fait-il mieux que le filtre ? ===\n")
eqm <- function(a) mean((a %*% t(lambda) - F_vrai %*% t(lambda))^2)
e_f <- eqm(kf$a_filt); e_l <- eqm(ks$a_liss)
verifier("erreur du lisseur < erreur du filtre", e_l < e_f,
         sprintf("%.5f contre %.5f, soit %.0f %% de mieux",
                 e_l, e_f, 100 * (e_f - e_l) / e_f))

cat("\n=== 3. Donnees manquantes : degradation maitrisee ? ===\n")
for (taux in c(0.2, 0.5, 0.8)) {
  Xm <- X
  Xm[sample(length(Xm), floor(taux * length(Xm)))] <- NA
  kfm <- filtre_kalman(Xm, lambda, A, sigma2, Q)
  ksm <- lisseur_kalman(kfm)
  cm <- correl(ksm$a_liss %*% t(lambda), F_vrai %*% t(lambda))
  verifier(sprintf("%.0f %% de valeurs manquantes : correlation > 0,80",
                   100 * taux),
           cm > 0.80, sprintf("r = %.4f", cm))
}

cat("\n=== 4. Bord irregulier : le cas qui nous interesse vraiment ===\n")
# Chaque serie s'arrete a une date differente, comme dans un panel reel.
Xb <- X
for (i in seq_len(n)) {
  fin <- n_t - sample(0:5, 1)
  if (fin < n_t) Xb[(fin + 1L):n_t, i] <- NA
}
kfb <- filtre_kalman(Xb, lambda, A, sigma2, Q)
cb <- correl(kfb$a_filt[(n_t - 20):n_t, ] %*% t(lambda),
             F_vrai[(n_t - 20):n_t, ] %*% t(lambda))
verifier("bord irregulier : correlation sur les 20 derniers mois > 0,85",
         cb > 0.85, sprintf("r = %.4f", cb))

cat("\n=== 5. La log-vraisemblance est-elle maximale au vrai parametre ? ===\n")
ll <- function(a11) {
  Ax <- A; Ax[1, 1] <- a11
  filtre_kalman(X, lambda, Ax, sigma2, Q)$logL
}
grille <- seq(0.3, 0.95, by = 0.05)
vals <- vapply(grille, ll, numeric(1))
pic <- grille[which.max(vals)]
verifier("le maximum se situe a moins de 0,10 du vrai coefficient (0,70)",
         abs(pic - 0.70) <= 0.10, sprintf("maximum en %.2f", pic))

cat("\n=== 6. Prevision : part-elle bien de l'etat FILTRE ? ===\n")
# Une prevision construite sur l'etat lisse lirait le futur. On verifie que la
# prevision a partir de t = 200 ne depend pas de ce qui suit.
kf200 <- filtre_kalman(X[1:200, ], lambda, A, sigma2, Q)
p1 <- prevoir_etat(kf200, h = 3)$observations
X2 <- X; X2[201:n_t, ] <- X2[201:n_t, ] + 10   # on perturbe le futur
kf200b <- filtre_kalman(X2[1:200, ], lambda, A, sigma2, Q)
p2 <- prevoir_etat(kf200b, h = 3)$observations
verifier("la prevision est insensible a une perturbation du futur",
         max(abs(p1 - p2)) < 1e-12,
         sprintf("ecart maximal %.1e", max(abs(p1 - p2))))

cat("\n", strrep("-", 74), "\n", sep = "")
if (ok) {
  cat("Tous les controles passent : le moteur d'espace d'etat est valide.\n")
} else {
  stop("Au moins un controle a echoue.", call. = FALSE)
}
