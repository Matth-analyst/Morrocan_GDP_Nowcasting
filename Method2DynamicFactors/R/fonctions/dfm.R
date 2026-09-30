# ============================================================================
# fonctions/dfm.R -- Modele a facteurs dynamiques, estime par EM/QML
# ============================================================================
# LE MODELE
#
#   x_t = Lambda F_t + e_t        e_t ~ N(0, R), R diagonale
#   F_t = A_1 F_{t-1} + ... + A_p F_{t-p} + u_t     u_t ~ N(0, Q)
#
# x_t est le panel MENSUEL standardise, F_t un petit nombre de facteurs
# latents communs, e_t la composante idiosyncratique propre a chaque serie.
#
# LA FREQUENCE MIXTE, ET POURQUOI ELLE N'EST PAS UN DETAIL
#
# La cible est trimestrielle, les facteurs sont mensuels. La convention
# naive -- poser la valeur trimestrielle sur un mois du trimestre et laisser
# les deux autres manquants -- revient a affirmer que la croissance du
# trimestre est engendree par le facteur d'UN SEUL mois. C'est faux : elle
# resulte des trois mois, et des trois precedents par l'effet de base.
#
# Le traitement correct (Mariano et Murasawa, 2003) part du niveau. Si y_t est
# le log-niveau mensuel latent, le log-niveau trimestriel vaut approximativement
# la moyenne des trois mois, et la croissance trimestrielle s'ecrit alors
#
#   g^Q_t = (1/3)( g_t + 2 g_{t-1} + 3 g_{t-2} + 2 g_{t-3} + g_{t-4} )
#
# ou g_s est la croissance MENSUELLE latente. Les poids (1,2,3,2,1)/3 ne sont
# pas un lissage arbitraire : ils tombent de l'identite comptable entre niveaux
# et taux de croissance. Ils imposent que l'etat contienne cinq retards du
# facteur, d'ou la dimension de la forme compagnon ci-dessous.
#
# CE QUI EST ESTIME, ET COMMENT
#
# L'algorithme EM alterne deux etapes jusqu'a stabilisation de la
# log-vraisemblance :
#   etape E : le filtre puis le lisseur de Kalman calculent les moments des
#             facteurs, en n'utilisant a chaque date que les series
#             effectivement observees -- aucune imputation ;
#   etape M : Lambda, R, A et Q sont reestimes en forme close a partir de ces
#             moments, serie par serie pour Lambda et R, de sorte qu'une serie
#             trouee contribue sur ses seules dates observees.
#
# LA COMPOSANTE IDIOSYNCRATIQUE DE LA CIBLE
#
# Le modele de base suppose e_t independant dans le temps. Le diagnostic mene
# sur les donnees marocaines montre que cette hypothese est couteuse : sur
# plusieurs branches, la cible est idiosyncratique a plus de 95 %, autrement
# dit tout ce qui la meut se trouve dans un terme que le modele traite comme
# du bruit blanc et qu'il ne peut donc pas prevoir.
#
# On autorise donc, pour la SEULE cible, une dynamique autoregressive :
#
#   e_cible,t = rho e_cible,t-1 + eps_t
#
# Elle est obtenue en augmentant l'etat d'une composante, ce qui coute un
# etat sur quinze plutot que n etats sur n. Le modele retrouve alors, quand
# les facteurs n'apportent rien, l'equivalent d'une prevision autoregressive
# sur la cible -- et quand ils apportent, il combine les deux.
# ============================================================================

source("R/fonctions/espace_etat.R")

#' Poids d'agregation temporelle de Mariano-Murasawa pour un taux de croissance.
POIDS_MM <- c(1, 2, 3, 2, 1) / 3

#' Construit la matrice de transition en forme compagnon.
#'
#' @param A liste des p matrices r x r, ou une matrice r x (r p).
#' @param s nombre de blocs de retard conserves dans l'etat (>= p).
compagnon <- function(A, r, p, s) {
  stopifnot(s >= p)
  TT <- matrix(0, r * s, r * s)
  Am <- if (is.list(A)) do.call(cbind, A) else A
  TT[1:r, 1:(r * p)] <- Am
  if (s > 1L) TT[(r + 1L):(r * s), 1:(r * (s - 1L))] <- diag(r * (s - 1L))
  TT
}

#' Matrice d'observation du panel.
#'
#' Toute serie TRIMESTRIELLE -- la cible comme les indicateurs -- charge la
#' combinaison ponderee des cinq retards du facteur, et non le seul facteur
#' courant. Les indicateurs trimestriels n'ont donc aucune raison d'etre
#' exclus du modele : le formalisme espace d'etat les accueille exactement
#' comme la cible.
#'
#' @param Lambda matrice n x r des chargements.
#' @param i_trim indices des lignes trimestrielles (vecteur, eventuellement
#'   vide).
#' @param r,s dimensions de l'etat.
matrice_observation <- function(Lambda, i_trim, r, s, i_idio = NA_integer_) {
  n <- nrow(Lambda)
  m <- r * s + (if (is.na(i_idio)) 0L else 1L)
  Z <- matrix(0, n, m)
  Z[, 1:r] <- Lambda                    # les mensuelles chargent F_t seul
  i_trim <- i_trim[!is.na(i_trim)]
  for (i in i_trim) {
    Z[i, 1:(r * s)] <- 0
    for (k in seq_along(POIDS_MM)) {
      if (k <= s) Z[i, ((k - 1L) * r + 1L):(k * r)] <- POIDS_MM[k] * Lambda[i, ]
    }
  }
  # La composante idiosyncratique de la cible entre dans l'etat : son
  # coefficient d'observation vaut 1, et la variance de mesure correspondante
  # sera rendue negligeable.
  if (!is.na(i_idio)) Z[i_idio, m] <- 1
  Z
}

#' Selection G telle que G alpha_t soit le regresseur effectif d'une serie.
#'
#' Pour une mensuelle, c'est F_t ; pour la cible, la combinaison ponderee des
#' cinq retards. Cette fonction evite de dupliquer la logique entre la
#' construction de Z et l'etape M.
selection_regresseur <- function(cible, r, s, m_tot = r * s) {
  G <- matrix(0, r, m_tot)
  if (!cible) {
    G[, 1:r] <- diag(r)
  } else {
    for (k in seq_along(POIDS_MM)) {
      if (k <= s) G[, ((k - 1L) * r + 1L):(k * r)] <- POIDS_MM[k] * diag(r)
    }
  }
  G
}

#' Initialisation par composantes principales.
#'
#' Un panel complet est necessaire pour l'ACP ; il est obtenu par interpolation
#' puis remplissage des bords. CE PANEL NE SERT QU'ICI : l'estimation finale
#' n'en voit jamais la couleur, elle travaille sur les donnees trouees.
initialiser_acp <- function(X, r, p, i_cible) {
  n_t <- nrow(X); n <- ncol(X)
  Xb <- X
  for (j in seq_len(n)) {
    v <- Xb[, j]
    if (all(is.na(v))) { Xb[, j] <- 0; next }
    obs <- which(!is.na(v))
    if (length(obs) >= 2L) {
      v <- stats::approx(obs, v[obs], xout = seq_len(n_t), rule = 2)$y
    } else {
      v[is.na(v)] <- v[obs[1]]
    }
    Xb[, j] <- v
  }
  Xb[!is.finite(Xb)] <- 0

  e <- eigen(stats::cov(Xb), symmetric = TRUE)
  r <- min(r, sum(e$values > 1e-10), n)
  V <- e$vectors[, seq_len(r), drop = FALSE]
  F0 <- scale(Xb, scale = FALSE) %*% V
  Lambda <- matrix(0, n, r)
  for (j in seq_len(n)) {
    Lambda[j, ] <- stats::coef(stats::lm(Xb[, j] ~ F0 - 1))
  }

  # VAR(p) sur les facteurs initiaux.
  A <- matrix(0, r, r * p)
  if (n_t > p + 5L) {
    Y <- F0[(p + 1L):n_t, , drop = FALSE]
    Xr <- do.call(cbind, lapply(seq_len(p),
                                function(k) F0[(p + 1L - k):(n_t - k), , drop = FALSE]))
    A <- t(tryCatch(solve(crossprod(Xr), crossprod(Xr, Y)),
                    error = function(e) MASS_ginv(crossprod(Xr)) %*% crossprod(Xr, Y)))
    U <- Y - Xr %*% t(A)
    Q <- crossprod(U) / nrow(U)
  } else {
    Q <- diag(r)
  }
  R <- apply(Xb - F0 %*% t(Lambda), 2, stats::var)
  R[!is.finite(R) | R <= 1e-8] <- 1e-4

  list(Lambda = Lambda, A = A, Q = as.matrix(Q), R = R, r = r)
}

#' Estimation du modele a facteurs dynamiques par EM / quasi-maximum de
#' vraisemblance.
#'
#' @param X matrice T x n du panel mensuel standardise, NA autorises.
#' @param r nombre de facteurs ; p ordre du VAR sur les facteurs.
#' @param i_cible indice de colonne de la cible trimestrielle (NA si absente).
#' @param max_iter,tol criteres d'arret.
#' @return liste des parametres estimes, de l'etat lisse et du journal de
#'   convergence.
estimer_dfm <- function(X, r = 2L, p = 1L, i_cible = NA_integer_,
                        i_trim = integer(0), max_iter = 200L, tol = 1e-5,
                        init = NULL, idio_ar = FALSE) {
  # DEMARRAGE A CHAUD. Dans un backtest recursif, le panel de l'origine T ne
  # differe de celui de T-1 que par trois mois. Repartir des parametres de
  # l'origine precedente divise le nombre d'iterations sans rien changer au
  # point de convergence : l'algorithme EM converge vers le meme maximum local
  # quel que soit un point de depart voisin.
  #
  # Ce n'est PAS une fuite d'information : les parametres de T-1 sont estimes
  # sur des donnees anterieures a T-1, donc incluses dans l'information
  # disponible en T.
  # i_cible : la cible ; i_trim : TOUTES les lignes trimestrielles, cible
  # comprise. On tolere qu'on ne passe que i_cible, par commodite.
  i_trim <- unique(c(i_trim, i_cible))
  i_trim <- i_trim[!is.na(i_trim)]
  X <- as.matrix(X)
  n_t <- nrow(X); n <- ncol(X)
  s <- max(p, length(POIDS_MM))              # l'agregation exige 5 retards
  idio_ar <- isTRUE(idio_ar) && !is.na(i_cible)
  i_idio <- if (idio_ar) i_cible else NA_integer_
  m_tot <- r * s + (if (idio_ar) 1L else 0L)
  rho <- 0.2; s2_u <- 0.5                    # depart neutre

  reprise <- !is.null(init) && is.matrix(init$Lambda) &&
    nrow(init$Lambda) == n && ncol(init$Lambda) == r &&
    all(dim(init$A) == c(r, r * p))
  if (reprise) {
    Lambda <- init$Lambda; A <- init$A; Q <- init$Q; R <- init$R
  } else {
    ini <- initialiser_acp(X, r, p, i_cible)
    r <- ini$r
    Lambda <- ini$Lambda; A <- ini$A; Q <- ini$Q; R <- ini$R
  }

  # Selections G, une par serie : F_t pour les mensuelles, la combinaison
  # ponderee pour la cible.
  G <- lapply(seq_len(n), function(j)
    selection_regresseur(j %in% i_trim, r, s, m_tot))
  # Selection de la composante idiosyncratique, quand elle est dans l'etat.
  h_idio <- if (idio_ar) { v <- numeric(m_tot); v[m_tot] <- 1; v } else NULL

  obs_par_serie <- lapply(seq_len(n), function(j) which(!is.na(X[, j])))

  logL_prec <- -Inf; journal <- numeric(0); converge <- FALSE
  for (iter in seq_len(max_iter)) {
    # ------------------------------------------------------------- etape E
    Z  <- matrice_observation(Lambda, i_trim, r, s, i_idio)
    TT <- matrix(0, m_tot, m_tot)
    TT[1:(r * s), 1:(r * s)] <- compagnon(A, r, p, s)
    Qf <- matrix(0, m_tot, m_tot); Qf[1:r, 1:r] <- Q
    Rv <- R
    if (idio_ar) {
      TT[m_tot, m_tot] <- rho
      Qf[m_tot, m_tot] <- s2_u
      Rv[i_cible] <- 1e-8   # le bruit de la cible est passe dans l'etat
    }

    kf <- filtre_kalman(X, Z, TT, Rv, Qf)
    ks <- lisseur_kalman(kf)
    journal <- c(journal, kf$logL)

    if (is.finite(logL_prec) &&
        abs(kf$logL - logL_prec) < tol * (abs(logL_prec) + tol)) {
      converge <- TRUE; break
    }
    logL_prec <- kf$logL

    # ------------------------------------------------------------- etape M
    # Lambda et R, serie par serie : une serie trouee ne contribue que sur
    # ses dates observees, sans qu'aucune valeur ne soit inventee.
    for (j in seq_len(n)) {
      tj <- obs_par_serie[[j]]
      if (length(tj) < 5L) { R[j] <- max(R[j], 1e-4); next }
      Gj <- G[[j]]
      # Quand la composante idiosyncratique de la cible est dans l'etat, la
      # part expliquee par les facteurs porte sur x - u, et non sur x : sans
      # cette correction, les chargements absorberaient aussi l'idiosyncratique
      # et le modele compterait deux fois la meme variation.
      corrige_idio <- idio_ar && j == i_cible
      S_ff <- matrix(0, r, r); S_xf <- numeric(r); S_xx <- 0
      for (t in tj) {
        Eg  <- as.vector(Gj %*% ks$a_liss[t, ])
        Egg <- Gj %*% ks$M2[, , t] %*% t(Gj)
        S_ff <- S_ff + Egg
        if (corrige_idio) {
          Egu <- as.vector(Gj %*% ks$M2[, , t] %*% h_idio)
          S_xf <- S_xf + X[t, j] * Eg - Egu
        } else {
          S_xf <- S_xf + X[t, j] * Eg
        }
        S_xx <- S_xx + X[t, j]^2
      }
      lam <- tryCatch(solve(S_ff, S_xf),
                      error = function(e) as.vector(MASS_ginv(S_ff) %*% S_xf))
      Lambda[j, ] <- lam
      res <- (S_xx - 2 * sum(lam * S_xf) + as.numeric(t(lam) %*% S_ff %*% lam))
      R[j] <- if (idio_ar && j == i_cible) 1e-8 else max(res / length(tj), 1e-6)
    }

    # A et Q : VAR sur les facteurs, a partir des moments lisses.
    idxF <- 1:r; idxL <- 1:(r * p)
    S00 <- matrix(0, r * p, r * p); S10 <- matrix(0, r, r * p)
    S11 <- matrix(0, r, r)
    for (t in 2:n_t) {
      S00 <- S00 + ks$M2[idxL, idxL, t - 1L]
      S10 <- S10 + ks$M2c[idxF, idxL, t]
      S11 <- S11 + ks$M2[idxF, idxF, t]
    }
    A <- tryCatch(S10 %*% solve(S00),
                  error = function(e) S10 %*% MASS_ginv(S00))
    Q <- (S11 - A %*% t(S10)) / (n_t - 1L)
    Q <- (Q + t(Q)) / 2
    if (any(diag(Q) <= 0)) diag(Q) <- pmax(diag(Q), 1e-6)

    # Dynamique de la composante idiosyncratique de la cible.
    if (idio_ar) {
      s11 <- sum(ks$M2[m_tot, m_tot, 2:n_t])
      s00 <- sum(ks$M2[m_tot, m_tot, 1:(n_t - 1L)])
      s10 <- sum(ks$M2c[m_tot, m_tot, 2:n_t])
      rho <- if (s00 > 1e-10) max(-0.95, min(0.95, s10 / s00)) else 0
      s2_u <- max((s11 - rho * s10) / (n_t - 1L), 1e-6)
    }

    # Garde-fou de stabilite : un VAR explosif ferait diverger la prevision.
    ray <- max(Mod(eigen(compagnon(A, r, p, p), only.values = TRUE)$values))
    if (ray >= 0.999) A <- A * (0.98 / ray)
  }

  Z  <- matrice_observation(Lambda, i_trim, r, s, i_idio)
  TT <- matrix(0, m_tot, m_tot)
  TT[1:(r * s), 1:(r * s)] <- compagnon(A, r, p, s)
  Qf <- matrix(0, m_tot, m_tot); Qf[1:r, 1:r] <- Q
  Rv <- R
  if (idio_ar) { TT[m_tot, m_tot] <- rho; Qf[m_tot, m_tot] <- s2_u
                 Rv[i_cible] <- 1e-8 }
  kf <- filtre_kalman(X, Z, TT, Rv, Qf)
  ks <- lisseur_kalman(kf)

  list(Lambda = Lambda, A = A, Q = Q, R = Rv, Z = Z, TT = TT, Qf = Qf,
       idio_ar = idio_ar, rho = if (idio_ar) rho else NA_real_,
       s2_u = if (idio_ar) s2_u else NA_real_,
       r = r, p = p, s = s, i_cible = i_cible, i_trim = i_trim,
       facteurs = ks$a_liss[, 1:r, drop = FALSE],
       facteurs_filtres = kf$a_filt[, 1:r, drop = FALSE],
       kf = kf, logL = kf$logL, journal = journal, demarrage_chaud = reprise,
       iterations = length(journal), converge = converge)
}

#' Prevision de la cible trimestrielle a une date donnee.
#'
#' Le panel est suppose deja tronque a l'ensemble d'information : la fonction
#' filtre ce qu'elle recoit, projette l'etat jusqu'au mois demande, et applique
#' la ligne d'observation de la cible -- laquelle porte deja les poids
#' d'agregation temporelle.
#'
#' @param mod sortie de `estimer_dfm`.
#' @param X panel tronque (les memes colonnes, dans le meme ordre).
#' @param h nombre de mois a projeter au-dela de la derniere ligne de X.
prevoir_cible <- function(mod, X, h) {
  stopifnot(!is.na(mod$i_cible), h >= 0L)
  kf <- filtre_kalman(as.matrix(X), mod$Z, mod$TT, mod$R, mod$Qf)
  if (h == 0L) {
    a <- kf$a_filt[nrow(kf$a_filt), ]
  } else {
    a <- prevoir_etat(kf, h = h)$etats[h, ]
  }
  as.numeric(mod$Z[mod$i_cible, ] %*% a)
}
