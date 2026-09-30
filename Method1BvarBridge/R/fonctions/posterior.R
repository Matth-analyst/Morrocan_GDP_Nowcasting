# ============================================================================
# fonctions/posterior.R -- Distribution predictive du BVAR
# ============================================================================
# LA LIMITE QUE CECI LEVE
#   Jusqu'ici le BVAR ne produisait qu'une estimation PONCTUELLE -- la moyenne a
#   posteriori des coefficients. Le nowcast etait annonce sans intervalle, ou
#   assorti du seul RMSFE du backtest : une statistique retrospective, moyenne
#   sur 48 trimestres, qui ne dit rien de l'incertitude propre AU TRIMESTRE que
#   l'on prevoit. Un trimestre ou les indicateurs divergent et un trimestre
#   tranquille recevaient la meme barre d'erreur.
#
# POURQUOI C'EST FACILE ICI, ET CE N'EST PAS UN HASARD
#   Le prior de Minnesota impose par observations fictives est CONJUGUE
#   normal-inverse-Wishart. La loi a posteriori a donc une forme close :
#
#       Sigma            ~  IW( S* , nu )                    nu = T* - k
#       B | Sigma        ~  MN( B_chapeau , Sigma , (X*'X*)^{-1} )
#
#   On tire donc DIRECTEMENT, sans echantillonneur de Gibbs, sans chaine de
#   Markov -- donc sans periode de chauffe, sans diagnostic de convergence et
#   sans autocorrelation des tirages. Chaque tirage est independant.
#
#   C'est la meme propriete de conjugaison qui donnait la vraisemblance
#   marginale en forme close (phase 3, section 8). Elle se paie ailleurs : avec
#   theta different de 1 le prior differe d'une equation a l'autre, la structure
#   est perdue, et cette fonction refuse de s'executer.
#
# DEUX SOURCES D'INCERTITUDE, ET IL FAUT LES DISTINGUER
#   (a) l'incertitude PARAMETRIQUE : on ne connait ni B ni Sigma ;
#   (b) l'incertitude de CHOC : meme en connaissant B et Sigma exactement, le
#       trimestre a venir porte un alea e_T de loi N(0, Sigma).
#
#   La distribution PREDICTIVE cumule les deux. La fonction renvoie les deux
#   separement, parce que leur part respective est informative : si l'alea
#   domine, aucun raffinement du modele ne reduira l'intervalle.
#
# CE QUE CET INTERVALLE NE COUVRE PAS
#   L'incertitude sur les HYPERPARAMETRES. p, lambda et d sont choisis a chaque
#   origine puis traites comme connus ; la regle de detection des chocs et la
#   fenetre sigma de meme. Un traitement complet integrerait aussi sur eux, ce
#   qui elargirait l'intervalle. Celui calcule ici est donc une BORNE BASSE de
#   l'incertitude, et la verification de calibration le montrera.
# ============================================================================

#' Tire une matrice de loi Wishart W(V, nu) par la decomposition de Bartlett.
#'
#' On evite `MCMCpack` ou `rWishart` pour ne pas ajouter de dependance : la
#' decomposition de Bartlett est exacte et tient en dix lignes.
rwishart <- function(nu, V) {
  d <- nrow(V)
  L <- chol(V)                       # V = L'L, L triangulaire superieure
  A <- matrix(0, d, d)
  diag(A) <- sqrt(stats::rchisq(d, df = nu - seq_len(d) + 1))
  if (d > 1L) {
    A[lower.tri(A)] <- stats::rnorm(d * (d - 1) / 2)
  }
  LA <- t(L) %*% A
  LA %*% t(LA)
}

#' Tirages de la loi predictive du BVAR pour un vecteur de regresseurs donne.
#'
#' @param modele sortie de `estimer_bvar` (cas conjugue uniquement).
#' @param x_futur vecteur de regresseurs de la cible, longueur k.
#' @param n_tirages nombre de tirages independants.
#' @param avec_choc si TRUE (defaut), ajoute l'alea e_T ~ N(0, Sigma) : on
#'   obtient la loi PREDICTIVE. Si FALSE, seule l'incertitude parametrique est
#'   propagee -- utile pour mesurer la part de chacune.
#' @return matrice n_tirages x n_branches.
tirer_predictive <- function(modele, x_futur, n_tirages = 2000L,
                             avec_choc = TRUE) {
  post <- modele$posterior
  if (is.null(post)) {
    stop("Loi a posteriori indisponible : le prior n'est pas conjugue (theta != 1).",
         call. = FALSE)
  }
  n <- modele$n; k <- length(x_futur)
  stopifnot("dimension de x_futur incompatible" = k == nrow(modele$B))
  if (post$nu <= n + 1) {
    stop("Degres de liberte insuffisants pour la loi inverse-Wishart.", call. = FALSE)
  }

  S_inv <- tryCatch(solve(post$S), error = function(e) MASS::ginv(post$S))
  S_inv <- (S_inv + t(S_inv)) / 2
  # racine de (X*'X*)^{-1}, pour le tirage matriciel normal
  XtXi <- (post$XtX_inv + t(post$XtX_inv)) / 2
  R <- tryCatch(chol(XtXi), error = function(e) {
    ed <- eigen(XtXi, symmetric = TRUE)
    d <- pmax(ed$values, 0)
    t(ed$vectors %*% diag(sqrt(d), length(d)))
  })

  out <- matrix(NA_real_, n_tirages, n)
  for (i in seq_len(n_tirages)) {
    # Sigma ~ IW(S, nu)  <=>  Sigma^{-1} ~ W(S^{-1}, nu)
    W <- rwishart(post$nu, S_inv)
    Sigma <- tryCatch(solve(W), error = function(e) MASS::ginv(W))
    Sigma <- (Sigma + t(Sigma)) / 2
    Cs <- tryCatch(chol(Sigma), error = function(e) {
      ed <- eigen(Sigma, symmetric = TRUE)
      t(ed$vectors %*% diag(sqrt(pmax(ed$values, 0)), n))
    })
    # B | Sigma ~ MN(B_chapeau, Sigma, (X*'X*)^{-1})
    Z <- matrix(stats::rnorm(k * n), k, n)
    B_tire <- modele$B + t(R) %*% Z %*% Cs
    mu <- as.numeric(crossprod(x_futur, B_tire))
    out[i, ] <- if (avec_choc) {
      mu + as.numeric(stats::rnorm(n) %*% Cs)
    } else mu
  }
  colnames(out) <- colnames(modele$B)
  out
}

#' Resume d'un jeu de tirages : mediane et intervalle de credibilite.
resumer_tirages <- function(tirages, niveaux = c(0.10, 0.90)) {
  tibble::tibble(
    branche = colnames(tirages),
    mediane = apply(tirages, 2, stats::median),
    borne_basse = apply(tirages, 2, stats::quantile, probs = niveaux[1]),
    borne_haute = apply(tirages, 2, stats::quantile, probs = niveaux[2]),
    ecart_type = apply(tirages, 2, stats::sd))
}

#' Agrege des tirages de branche en tirages d'agregat, avec les poids en prix
#' courants et la formule de Laspeyres retenue a l'etape 6.
#'
#' L'agregation se fait TIRAGE PAR TIRAGE, et non sur les resumes : agreger des
#' quantiles de branche donnerait un intervalle faux, puisque les erreurs de
#' branche ne sont ni independantes ni parfaitement correlees. La matrice Sigma
#' porte precisement cette correlation, et le tirage la propage correctement.
agreger_tirages <- function(tirages, poids) {
  ordre <- match(colnames(tirages), poids$branche)
  stopifnot("poids manquants pour certaines branches" = !anyNA(ordre))
  w <- poids$w[ordre]
  apply(tirages, 1, function(g) log(sum(w * exp(g))))
}
