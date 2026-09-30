# ============================================================================
# fonctions/espace_etat.R -- Filtre et lisseur de Kalman, donnees manquantes
# ============================================================================
# Le modele a facteurs dynamiques s'ecrit sous forme espace d'etat :
#
#     x_t = Z alpha_t + e_t          e_t ~ N(0, R),  R diagonale
#     alpha_t = T alpha_{t-1} + u_t  u_t ~ N(0, Q)
#
# ou alpha_t empile les facteurs courants et leurs retards (forme compagnon).
#
# DEUX EXIGENCES QUI COMMANDENT TOUT LE FICHIER
#
#   1. Les donnees manquantes ne sont pas imputees. A chaque date, seules les
#      lignes effectivement observees entrent dans la mise a jour. C'est ce qui
#      permet au modele de digerer un panel a bord irregulier sans reconstituer
#      artificiellement ce qui n'a pas ete publie.
#
#   2. Le FILTRE et le LISSEUR ne servent pas au meme usage, et les confondre
#      introduirait une anteriorite invisible :
#        - le filtre calcule E[alpha_t | x_1..x_t]     : passe seulement ;
#        - le lisseur calcule E[alpha_t | x_1..x_T]    : passe ET futur.
#      Le lisseur sert a l'ESTIMATION (etape E de l'algorithme EM, ou tout
#      l'echantillon est legitimement disponible) ; le filtre sert a la
#      PREVISION, ou utiliser le lisseur reviendrait a lire le futur.
#
# Aucun package d'espace d'etat n'est employe : tout est ecrit ici, comme le
# reste du projet, de facon que chaque etape soit verifiable.
# ============================================================================

#' Filtre de Kalman avec donnees manquantes.
#'
#' @param X matrice T x n des observations (NA autorises).
#' @param Z matrice n x m des coefficients d'observation.
#' @param TT matrice m x m de transition.
#' @param R vecteur de longueur n : variances idiosyncratiques (diagonale).
#' @param Q matrice m x m de covariance des chocs d'etat.
#' @param a1 vecteur m : etat initial ; P1 sa covariance.
#' @return liste des etats predits et filtres, de leurs covariances, et de la
#'   log-vraisemblance.
filtre_kalman <- function(X, Z, TT, R, Q, a1 = NULL, P1 = NULL) {
  X <- as.matrix(X)
  n_t <- nrow(X); n <- ncol(X); m <- nrow(TT)
  stopifnot(nrow(Z) == n, ncol(Z) == m, length(R) == n,
            all(dim(Q) == c(m, m)))

  if (is.null(a1)) a1 <- rep(0, m)
  if (is.null(P1)) {
    # Covariance non conditionnelle de l'etat. L'equation de Lyapunov
    #     P = T P T' + Q      donne      vec(P) = (I - T (x) T)^{-1} vec(Q)
    # ou (x) est le produit de Kronecker. Oublier le vec(Q) au second membre
    # renvoie une matrice de la bonne taille mais sans aucun rapport : le
    # filtre converge quand meme sur echantillon long, ce qui rend l'erreur
    # invisible -- d'ou le controle sur echantillon court dans les tests.
    P1 <- tryCatch({
      stable <- max(Mod(eigen(TT, only.values = TRUE)$values)) < 1 - 1e-8
      if (!stable) stop("systeme non stable")
      matrix(solve(diag(m * m) - kronecker(TT, TT)) %*% as.vector(Q), m, m)
    }, error = function(e) diag(m) * 1e4)
    if (any(!is.finite(P1)) || any(diag(P1) <= 0)) P1 <- diag(m) * 1e4
    P1 <- (P1 + t(P1)) / 2
  }

  a_pred <- matrix(0, n_t, m); P_pred <- array(0, c(m, m, n_t))
  a_filt <- matrix(0, n_t, m); P_filt <- array(0, c(m, m, n_t))
  logL <- 0

  a <- a1; P <- P1
  for (t in seq_len(n_t)) {
    a_pred[t, ] <- a; P_pred[, , t] <- P

    obs <- which(!is.na(X[t, ]))
    if (length(obs) > 0L) {
      Zt <- Z[obs, , drop = FALSE]
      Rt <- R[obs]
      v  <- X[t, obs] - as.vector(Zt %*% a)          # innovation
      F  <- Zt %*% P %*% t(Zt) + diag(Rt, nrow = length(obs))
      F  <- (F + t(F)) / 2                            # symetrisation
      Fi <- tryCatch(solve(F), error = function(e) MASS_ginv(F))
      K  <- P %*% t(Zt) %*% Fi                        # gain de Kalman
      a  <- a + as.vector(K %*% v)
      P  <- P - K %*% Zt %*% P
      P  <- (P + t(P)) / 2

      dF <- determinant(F, logarithm = TRUE)
      if (is.finite(dF$modulus)) {
        logL <- logL - 0.5 * (length(obs) * log(2 * pi) +
                                as.numeric(dF$modulus) +
                                as.numeric(t(v) %*% Fi %*% v))
      }
    }
    a_filt[t, ] <- a; P_filt[, , t] <- P

    a <- as.vector(TT %*% a)
    P <- TT %*% P %*% t(TT) + Q
    P <- (P + t(P)) / 2
  }

  list(a_pred = a_pred, P_pred = P_pred,
       a_filt = a_filt, P_filt = P_filt, logL = logL,
       Z = Z, TT = TT, R = R, Q = Q)
}

#' Inverse generalisee, sans dependre du package MASS.
MASS_ginv <- function(M, tol = sqrt(.Machine$double.eps)) {
  s <- svd(M)
  positifs <- s$d > max(tol * s$d[1], 0)
  if (!any(positifs)) return(matrix(0, ncol(M), nrow(M)))
  s$v[, positifs, drop = FALSE] %*%
    ((1 / s$d[positifs]) * t(s$u[, positifs, drop = FALSE]))
}

#' Lisseur de Kalman (recursion de Rauch-Tung-Striebel), avec les covariances
#' croisees dont l'etape M de l'algorithme EM a besoin.
#'
#' @param kf sortie de `filtre_kalman`.
#' @return liste : etats lisses, covariances lisses, covariances croisees
#'   d'ordre 1, et les moments E[alpha_t alpha_t'] et E[alpha_t alpha_{t-1}'].
lisseur_kalman <- function(kf) {
  a_pred <- kf$a_pred; P_pred <- kf$P_pred
  a_filt <- kf$a_filt; P_filt <- kf$P_filt
  TT <- kf$TT
  n_t <- nrow(a_filt); m <- ncol(a_filt)

  a_liss <- matrix(0, n_t, m); P_liss <- array(0, c(m, m, n_t))
  P_croise <- array(0, c(m, m, n_t))   # Cov(alpha_t, alpha_{t-1} | tout)
  J <- array(0, c(m, m, n_t))

  a_liss[n_t, ] <- a_filt[n_t, ]; P_liss[, , n_t] <- P_filt[, , n_t]

  for (t in seq(n_t - 1L, 1L)) {
    Pp <- P_pred[, , t + 1L]
    Ppi <- tryCatch(solve(Pp), error = function(e) MASS_ginv(Pp))
    Jt <- P_filt[, , t] %*% t(TT) %*% Ppi
    J[, , t] <- Jt
    a_liss[t, ] <- a_filt[t, ] +
      as.vector(Jt %*% (a_liss[t + 1L, ] - a_pred[t + 1L, ]))
    P_liss[, , t] <- P_filt[, , t] +
      Jt %*% (P_liss[, , t + 1L] - Pp) %*% t(Jt)
    P_liss[, , t] <- (P_liss[, , t] + t(P_liss[, , t])) / 2
  }

  # Covariances croisees, recursion de de Jong et Mackinnon.
  for (t in seq(n_t, 2L)) {
    P_croise[, , t] <- P_liss[, , t] %*% t(J[, , t - 1L])
  }

  # Moments non centres, ceux qu'utilise l'etape M.
  M2 <- array(0, c(m, m, n_t)); M2c <- array(0, c(m, m, n_t))
  for (t in seq_len(n_t)) {
    M2[, , t] <- P_liss[, , t] + tcrossprod(a_liss[t, ])
    if (t >= 2L) {
      M2c[, , t] <- P_croise[, , t] + tcrossprod(a_liss[t, ], a_liss[t - 1L, ])
    }
  }

  list(a_liss = a_liss, P_liss = P_liss, P_croise = P_croise,
       M2 = M2, M2c = M2c)
}

#' Prevision a h pas, a partir du dernier etat FILTRE.
#'
#' On part deliberement de l'etat filtre et non de l'etat lisse : a la date de
#' prevision, le futur n'est pas disponible.
#'
#' @param kf sortie de `filtre_kalman`.
#' @param h horizon, en periodes du modele (ici des mois).
#' @return liste : etats prevus (h x m) et observations prevues (h x n).
prevoir_etat <- function(kf, h = 1L) {
  m <- ncol(kf$a_filt)
  a <- kf$a_filt[nrow(kf$a_filt), ]
  P <- kf$P_filt[, , dim(kf$P_filt)[3]]
  A <- matrix(0, h, m); Xh <- matrix(0, h, nrow(kf$Z))
  Pv <- array(0, c(m, m, h))
  for (i in seq_len(h)) {
    a <- as.vector(kf$TT %*% a)
    P <- kf$TT %*% P %*% t(kf$TT) + kf$Q
    A[i, ] <- a; Pv[, , i] <- P
    Xh[i, ] <- as.vector(kf$Z %*% a)
  }
  list(etats = A, P = Pv, observations = Xh)
}
