# ============================================================================
# fonctions/bvar.R -- BVAR a prior de Minnesota, par systeme augmente
# ============================================================================
# Plan de correction : section 8 ("Phase 8 -- BVAR trimestriel") et section 32
# ("Etape 3").
#
# References :
#   Litterman, R. (1986) "Forecasting with Bayesian Vector Autoregressions"
#   Banbura, M., Giannone, D., Reichlin, L. (2010) "Large Bayesian VARs", JAE 25(1)
#   Higgins, P. (2014) "GDPNow", FRB Atlanta WP 2014-7, annexe
#
# PRECISION METHODOLOGIQUE (exigee par la section 8 du plan)
#   L'implementation ci-dessous est une estimation a prior de Minnesota PAR
#   SYSTEME AUGMENTE : le prior est impose en empilant des observations
#   fictives sous les observations reelles, puis en resolvant le systeme par
#   moindres carres. L'estimateur obtenu est la moyenne a posteriori sous
#   prior normal-diffus.
#
#   Ce n'est PAS une simulation complete de la distribution a posteriori. Il
#   n'y a donc ni intervalle de credibilite, ni quantification de
#   l'incertitude parametrique. Le resultat doit etre presente comme tel dans
#   le memoire, et non comme "un BVAR estime par methodes bayesiennes" au sens
#   d'un echantillonnage de Gibbs.
#
# SPECIFICATION
#   Variables : taux de croissance trimestriels (Δlog) des 16 VA de branche.
#   Comme ce sont deja des taux de croissance et non des log-niveaux, le prior
#   est centre sur un RETOUR A LA MOYENNE (delta_i = 0) et non sur une marche
#   aleatoire (delta_i = 1) comme dans la formulation originale de Litterman.
#   Ce choix est justifie par les tests de stationnarite de la phase 2 :
#   16 branches sur 16 sont stationnaires en Δlog, 1 seule en niveau.
#
# ANTI-LOOK-AHEAD
#   Toutes les fonctions de ce fichier ne recoivent QUE la matrice
#   d'entrainement. Aucune ne connait la date cible ni les observations
#   posterieures. Les echelles sigma_i, en particulier, sont recalculees a
#   chaque appel sur les donnees fournies -- jamais sur l'echantillon complet.
#   C'est imperatif ici : la phase 2 a montre une rupture de variance en 2014
#   (artefact de retropolation) sur 12 branches sur 16, avec un facteur 9 pour
#   l'administration publique. Un sigma_i plein echantillon serait faux pour
#   les deux regimes a la fois.
# ============================================================================

#' Echelle propre de chaque variable, pour rendre le prior comparable entre
#' equations d'ampleurs differentes (Banbura et al. 2010, section 2.1).
#'
#'   sigma_i = ecart-type residuel de l'autoregression univariee
#'             g_{i,t} = c_i + somme_{l=1..p} phi_{i,l} g_{i,t-l}
#'                            + somme_k gamma_{i,k} D_{k,t} + e_{i,t}
#'
#' LES INDICATRICES DE CHOC DOIVENT FIGURER DANS CETTE REGRESSION.
#'
#' Sans elles, sigma_i est calcule sur une serie dont 2020 fait partie, alors
#' que le BVAR declare justement ces trimestres aberrants et les neutralise par
#' des indicatrices. Le prior serait donc calibre sur une volatilite que le
#' modele lui-meme considere comme non representative. L'effet mesure sur cette
#' base est massif :
#'
#'     branche                     sigma avec 2020   sigma hors 2020   inflation
#'     Hebergement-restauration          9,82 %            4,05 %         +142 %
#'     Services aux entreprises          3,40 %            1,51 %         +125 %
#'     Transports                        5,68 %            2,75 %         +107 %
#'
#' Comme sd(B) = (lambda/l) * (sigma_j/sigma_i), un sigma_i gonfle de moitie
#' relache d'autant le prior sur les equations expliquees par cette variable --
#' exactement la ou l'echantillon est le plus pollue.
#'
#' @param Y matrice T x n des donnees d'entrainement.
#' @param p ordre de l'autoregression univariee.
#' @param exo matrice T x k des indicatrices, ou NULL.
echelles_variables <- function(Y, p, exo = NULL) {
  Tn <- nrow(Y)
  exo <- filtrer_indicatrices_utiles(exo, p)
  if (Tn <= p + 2L) {
    return(setNames(apply(Y, 2, stats::sd), colnames(Y)))
  }

  # construction des retards, commune a toutes les variables
  idx <- (p + 1L):Tn
  base <- matrix(1, nrow = length(idx), ncol = 1L)
  if (!is.null(exo)) base <- cbind(base, exo[idx, , drop = FALSE])

  out <- vapply(seq_len(ncol(Y)), function(i) {
    x <- Y[, i]
    retards <- vapply(seq_len(p), function(l) x[idx - l], numeric(length(idx)))
    X <- cbind(base, retards)
    y <- x[idx]
    ok <- stats::complete.cases(X, y)
    if (sum(ok) < ncol(X) + 2L) return(stats::sd(x, na.rm = TRUE))
    fit <- tryCatch(stats::lm.fit(X[ok, , drop = FALSE], y[ok]),
                    error = function(e) NULL)
    if (is.null(fit)) return(stats::sd(x, na.rm = TRUE))
    ddl <- sum(ok) - ncol(X)
    if (ddl < 1L) return(stats::sd(x, na.rm = TRUE))
    s <- sqrt(sum(fit$residuals^2) / ddl)
    if (!is.finite(s) || s <= 0) return(stats::sd(x, na.rm = TRUE))
    s
  }, numeric(1))
  names(out) <- colnames(Y)
  out
}


#' Ecarte les indicatrices qui seraient des colonnes de zeros dans le systeme.
#'
#' Un VAR(p) n'explique pas ses p premieres observations : elles ne servent que
#' de retards. Une indicatrice posee sur l'une d'elles survit au filtre de
#' construire_indicatrices() -- qui regarde l'echantillon complet -- mais
#' devient une colonne nulle dans la regression, et rend le systeme singulier.
#'
#' Le cas se produit reellement : la regle de detection identifie T2-1998, qui
#' est la toute premiere observation de la serie des taux de croissance.
filtrer_indicatrices_utiles <- function(exo, p) {
  if (is.null(exo)) return(NULL)
  Tn <- nrow(exo)
  if (Tn <= p) return(NULL)
  garde <- colSums(abs(exo[(p + 1L):Tn, , drop = FALSE])) > 0
  if (!any(garde)) return(NULL)
  exo[, garde, drop = FALSE]
}

#' Observations reelles d'un VAR(p) avec regresseurs exogenes.
#'
#' @param Y matrice T x n.
#' @param p nombre de retards.
#' @param exo matrice T x k des exogenes (hors constante), ou NULL.
#' @return liste (Y, X) ou X = [constante | exo | retards 1..p].
construire_systeme <- function(Y, p, exo = NULL) {
  Tn <- nrow(Y); n <- ncol(Y)
  if (Tn <= p) stop("[BVAR] echantillon trop court : ", Tn, " obs. pour p = ", p,
                    call. = FALSE)
  k <- if (is.null(exo)) 0L else ncol(exo)

  Y_reg <- Y[(p + 1L):Tn, , drop = FALSE]
  X_reg <- matrix(0, nrow = Tn - p, ncol = 1L + k + n * p)
  X_reg[, 1L] <- 1                                    # constante
  if (k > 0L) X_reg[, 1L + seq_len(k)] <- exo[(p + 1L):Tn, , drop = FALSE]
  for (l in seq_len(p)) {
    col <- 1L + k + (l - 1L) * n + seq_len(n)
    X_reg[, col] <- Y[(p + 1L - l):(Tn - l), , drop = FALSE]
  }
  colnames(X_reg) <- c("const",
                       if (k > 0L) colnames(exo) else character(0),
                       paste0(rep(colnames(Y), p), "_L", rep(seq_len(p), each = n)))
  list(Y = Y_reg, X = X_reg, n = n, p = p, k = k)
}

#' Observations fictives portant le prior de Minnesota.
#'
#' Bloc 1 : prior sur les coefficients de retard. Cible nulle (delta_i = 0,
#'          retour a la moyenne) ; la contrainte se resserre avec le retard via
#'          le facteur l, de sorte que les retards lointains sont plus
#'          fortement pousses vers zero.
#' Bloc 2 : prior sur la matrice de covariance des residus.
#' Bloc 3 : prior tres lache sur la constante et sur les exogenes -- leur
#'          valeur n'est pas contrainte a priori, seule leur presence l'est.
#'
#' @param sigma vecteur des echelles, longueur n.
#' @param lambda hyperparametre de serrage global. Plus il est petit, plus le
#'   prior domine ; quand il tend vers l'infini, on retrouve les MCO.
construire_dummies_minnesota <- function(sigma, n, p, lambda, k = 0L,
                                         eps_exo = 1e-5, d = 1, theta = 1,
                                         equation = NULL) {
  ncols <- 1L + k + n * p

  Yd1 <- matrix(0, nrow = n * p, ncol = n)
  Xd1 <- matrix(0, nrow = n * p, ncol = ncols)
  for (l in seq_len(p)) {
    lignes <- (l - 1L) * n + seq_len(n)
    cols   <- 1L + k + (l - 1L) * n + seq_len(n)
    # serrage de base : sigma_i * l^d / lambda
    serrage <- sigma * l^d / lambda
    # serrage croise : les retards des AUTRES variables sont resserres d'un
    # facteur supplementaire 1/theta. theta = 1 retablit le prior symetrique.
    if (!is.null(equation) && theta != 1) {
      croise <- seq_len(n) != equation
      serrage[croise] <- serrage[croise] / theta
    }
    Xd1[lignes, cols] <- diag(serrage, nrow = n)
  }

  Yd2 <- diag(sigma, nrow = n)
  Xd2 <- matrix(0, nrow = n, ncol = ncols)

  n_exo <- 1L + k
  Yd3 <- matrix(0, nrow = n_exo, ncol = n)
  Xd3 <- matrix(0, nrow = n_exo, ncol = ncols)
  Xd3[seq_len(n_exo), seq_len(n_exo)] <- diag(eps_exo, nrow = n_exo)

  list(Y = rbind(Yd1, Yd2, Yd3), X = rbind(Xd1, Xd2, Xd3))
}

#' Poids a decroissance geometrique sur les observations reelles.
#'
#'   w(t) = rho^(T - t)      rho dans ]0, 1]
#'
#' rho = 1 redonne l'estimation non ponderee. Plus rho est petit, plus les
#' observations anciennes comptent peu.
#'
#' POURQUOI PONDERER PLUTOT QUE TRONQUER
#'   La phase 2 a montre une rupture de variance en 2014 sur 12 branches sur 16.
#'   La reponse naturelle -- une fenetre glissante -- s'est revelee la PIRE des
#'   specifications testees : dans un systeme qui compte deja plus de parametres
#'   que d'observations, jeter des donnees coute bien plus que ce que
#'   l'homogeneite rapporte.
#'
#'   Une ponderation geometrique conserve toutes les observations en reduisant
#'   progressivement le poids des anciennes. Elle repond donc a l'heterogeneite
#'   sans payer le prix de la troncature. Techniquement, il s'agit de moindres
#'   carres ponderes : chaque ligne REELLE du systeme est multipliee par la
#'   racine de son poids, les observations FICTIVES du prior restant a poids 1
#'   -- le prior ne doit pas etre affaibli parce que l'echantillon vieillit.
poids_geometriques <- function(Tn, rho = 1) {
  if (rho >= 1) return(rep(1, Tn))
  rho^((Tn - 1L):0L)
}

#' Estime un BVAR sur les donnees fournies. Ne connait rien d'autre.
#'
#' @param Y matrice T x n d'entrainement.
#' @param p retards ; lambda serrage ; exo matrice T x k ou NULL.
#' @return liste : B (coefficients), residus, sigma, systeme, n, p, k.
estimer_bvar <- function(Y, p = 5L, lambda = 0.15, exo = NULL, sigma = NULL,
                         d = 1, theta = 1, rho = 1, fenetre_sigma = Inf) {
  n <- ncol(Y)
  exo <- filtrer_indicatrices_utiles(exo, p)

  # --- echelle du prior : fenetre eventuellement plus courte ---------------
  # Le prior porte sur une ECHELLE, qui doit refleter le regime courant ; les
  # coefficients, eux, profitent de tout l'historique. Dissocier les deux
  # echantillons repond a la rupture de variance de 2014 sans rien jeter.
  if (is.null(sigma)) {
    if (is.finite(fenetre_sigma) && nrow(Y) > fenetre_sigma) {
      k_s <- (nrow(Y) - fenetre_sigma + 1L):nrow(Y)
      sigma <- echelles_variables(Y[k_s, , drop = FALSE], p,
                                  if (is.null(exo)) NULL else exo[k_s, , drop = FALSE])
    } else {
      sigma <- echelles_variables(Y, p, exo)
    }
  }

  sys <- construire_systeme(Y, p, exo)
  w   <- poids_geometriques(nrow(sys$Y), rho)
  rw  <- sqrt(w)

  X_reel <- sys$X * rw           # moindres carres ponderes
  Y_reel <- sys$Y * rw

  if (theta == 1) {
    # Prior symetrique : une seule resolution multivariee. C'est le cas
    # conjugue, celui qui donne la vraisemblance marginale en forme close.
    dum <- construire_dummies_minnesota(sigma, n, p, lambda, k = sys$k,
                                        d = d, theta = 1)
    X_aug <- rbind(X_reel, dum$X)
    Y_aug <- rbind(Y_reel, dum$Y)
    XtX <- crossprod(X_aug)
    B <- tryCatch(solve(XtX, crossprod(X_aug, Y_aug)),
                  error = function(e) qr.solve(X_aug, Y_aug, tol = 1e-12))
    # --- de quoi TIRER dans la loi a posteriori ------------------------------
    # Le prior de Minnesota par observations fictives est conjugue
    # normal-inverse-Wishart : la loi a posteriori est connue en forme close, et
    # l'on peut y tirer directement -- sans echantillonneur de Gibbs, sans
    # chaine de Markov, donc sans diagnostic de convergence.
    #
    #     Sigma           ~ IW( S_post , nu )
    #     B | Sigma       ~ MN( B_chapeau , Sigma , (X*'X*)^{-1} )
    #
    # On conserve donc S_post, nu et l'inverse de X*'X*. Ces quantites
    # N'EXISTENT QUE DANS LE CAS CONJUGUE : avec theta different de 1 le prior
    # differe d'une equation a l'autre et la structure est perdue.
    S_post <- crossprod(Y_aug) - crossprod(Y_aug, X_aug) %*% B
    S_post <- (S_post + t(S_post)) / 2      # symetrisation, erreurs d'arrondi
    posterior <- list(S = S_post,
                      nu = nrow(Y_aug) - sys$k,
                      XtX_inv = tryCatch(solve(XtX),
                                         error = function(e) MASS::ginv(XtX)))
  } else {
    posterior <- NULL
    # Prior ASYMETRIQUE : le serrage des retards croises differe de celui du
    # propre retard. Le bloc d'observations fictives n'est alors plus le meme
    # d'une equation a l'autre, et le systeme se resout EQUATION PAR EQUATION.
    #
    # Consequence a assumer : la structure normale-inverse-Wishart est perdue,
    # donc la vraisemblance marginale n'a plus de forme close. theta doit etre
    # choisi hors echantillon, pas par maximisation de la vraisemblance.
    B <- matrix(0, nrow = ncol(sys$X), ncol = n)
    for (j in seq_len(n)) {
      dum <- construire_dummies_minnesota(sigma, n, p, lambda, k = sys$k,
                                          d = d, theta = theta, equation = j)
      X_aug <- rbind(X_reel, dum$X)
      y_aug <- c(Y_reel[, j], dum$Y[, j])
      B[, j] <- tryCatch(qr.solve(X_aug, y_aug, tol = 1e-12),
                         error = function(e) rep(NA_real_, ncol(X_aug)))
    }
  }
  rownames(B) <- colnames(sys$X)
  colnames(B) <- colnames(Y)

  list(B = B, residus = sys$Y - sys$X %*% B, sigma = sigma,
       n = n, p = p, k = sys$k, lambda = lambda, d = d, theta = theta, rho = rho,
       poids = w, X_noms = colnames(sys$X), n_obs = nrow(sys$Y),
       posterior = posterior)
}

#' Prevision a un pas. `exo_futur` est le vecteur des exogenes a la date cible.
#'
#' Les indicatrices de choc valent 0 en prevision : elles servent a empecher
#' les trimestres aberrants de contaminer les COEFFICIENTS, pas a annoncer un
#' choc. Un modele qui poserait l'indicatrice a 1 pour prevoir 2020 T2
#' utiliserait la connaissance de l'ampleur du choc, qu'il est cense estimer.
prevoir_bvar <- function(modele, Y, exo_futur = NULL) {
  p <- modele$p; n <- modele$n; Tn <- nrow(Y)
  k <- modele$k
  if (is.null(exo_futur)) exo_futur <- numeric(k)
  stopifnot(length(exo_futur) == k)

  x <- c(1, exo_futur, as.vector(t(Y[Tn:(Tn - p + 1L), , drop = FALSE])))
  setNames(as.vector(matrix(x, nrow = 1L) %*% modele$B), colnames(Y))
}

#' Indicatrices de choc, restreintes aux dates presentes dans l'echantillon.
#'
#' Une colonne entierement nulle rendrait le systeme singulier : aux origines
#' anterieures a 2020, les indicatrices COVID n'ont encore aucune observation
#' non nulle et doivent simplement ne pas exister. Cette fonction les retire.
#'
#' @param dates vecteur de dates de l'echantillon (fin de trimestre).
#' @param dates_choc dates auxquelles une indicatrice doit valoir 1.
construire_indicatrices <- function(dates, dates_choc) {
  if (length(dates_choc) == 0L) return(NULL)
  M <- vapply(dates_choc, function(d) as.numeric(dates == as.Date(d)),
              numeric(length(dates)))
  M <- matrix(M, nrow = length(dates))
  colnames(M) <- paste0("choc_", date_vers_trimestre(as.Date(dates_choc)))
  garde <- colSums(M) > 0
  if (!any(garde)) return(NULL)
  M[, garde, drop = FALSE]
}

# ----------------------------------------------------------------------------
# Estimation recursive
# ----------------------------------------------------------------------------

#' Prevision BVAR du trimestre cible, estimee UNIQUEMENT sur l'information
#' anterieure. C'est la fonction que consommera le backtest de la phase 7.
#'
#' Pour prevoir T : Y_1, ..., Y_{T-1} -> BVAR_T -> Y_T chapeau.
#' Le controle de la section 30 est applique explicitement.
#'
#' @param mat matrice (T x n) de TOUTE la serie, avec attribut de dates.
#' @param dates vecteur de dates correspondant aux lignes de `mat`.
#' @param target_date date de fin du trimestre cible.
#' @param dates_choc dates des indicatrices de choc.
#' @param selection "fixe" conserve p et lambda tels quels ; "ml" les choisit
#'   par maximisation de la vraisemblance marginale ; "bgr" par le critere
#'   d'ajustement de Banbura, Giannone & Reichlin.
#'
#'   POINT CRUCIAL : quand la selection est active, elle n'utilise QUE
#'   l'echantillon d'entrainement de cette origine. Choisir (p, lambda) une
#'   fois sur l'echantillon complet serait exactement le look-ahead que tout le
#'   projet combat -- et d'autant plus tentant que l'optimum est instable dans
#'   le temps, puisque les echelles sigma_i le sont.
#' @param regle_choc liste(z, k) : si fournie, les trimestres de choc sont
#'   DETECTES sur l'echantillon d'entrainement au lieu d'etre imposes par
#'   `dates_choc`. Voir detecter_chocs() pour la justification : une liste de
#'   dates codee en dur n'est pas testable, une regle l'est.
prevision_bvar_recursive <- function(mat, dates, target_date, p = 5L,
                                     lambda = 0.15, dates_choc = NULL,
                                     fenetre = Inf,
                                     selection = c("fixe", "ml", "bgr"),
                                     p_grille = 1:5,
                                     lambda_grille = exp(seq(log(0.01), log(2),
                                                             length.out = 15)),
                                     ref = NULL, regle_choc = NULL,
                                     d_grille = c(0.5, 1, 1.5, 2),
                                     theta = 1, rho = 1, fenetre_sigma = Inf) {
  selection <- match.arg(selection)
  target_date <- as.Date(target_date)
  dispo <- dates < target_date                       # strictement avant la cible
  besoin <- max(p, if (selection == "fixe") p else max(p_grille)) + 10L
  if (sum(dispo) < besoin) return(NULL)

  idx <- which(dispo)
  if (is.finite(fenetre) && length(idx) > fenetre) {
    idx <- utils::tail(idx, fenetre)                 # fenetre glissante
  }

  Y_tr <- mat[idx, , drop = FALSE]
  d_tr <- dates[idx]

  # Controle 1 de la section 30 : l'echantillon d'estimation s'arrete
  # strictement avant la cible.
  if (max(d_tr) >= target_date) {
    stop("[ANTI-LOOK-AHEAD] BVAR : donnee du ", max(d_tr),
         " >= cible ", target_date, call. = FALSE)
  }

  # Detection des chocs sur le SEUL echantillon d'entrainement, si une regle
  # est fournie. Reperer un point aberrant a l'interieur de l'information deja
  # disponible n'est pas du look-ahead : c'est du nettoyage.
  if (!is.null(regle_choc)) {
    dates_choc <- detecter_chocs(Y_tr, d_tr, z = regle_choc$z, k = regle_choc$k)
  }
  exo <- construire_indicatrices(d_tr, dates_choc)

  if (selection != "fixe") {
    choix <- choisir_hyperparametres(Y_tr, exo, methode = selection,
                                     p_grille = p_grille,
                                     d_grille = d_grille,
                                     ref = ref, p_fixe = p, lambda_fixe = lambda)
    p <- choix$p; lambda <- choix$lambda; d_ret <- choix$d
  } else d_ret <- 1

  mod <- estimer_bvar(Y_tr, p = p, lambda = lambda, exo = exo, d = d_ret,
                      theta = theta, rho = rho, fenetre_sigma = fenetre_sigma)
  prev <- prevoir_bvar(mod, Y_tr, exo_futur = rep(0, mod$k))

  list(prevision = prev, modele = mod, n_obs = nrow(Y_tr),
       derniere_obs = max(d_tr), origine = target_date,
       p = p, lambda = lambda, d = d_ret, theta = theta, rho = rho,
       fenetre_sigma = fenetre_sigma, selection = selection,
       n_chocs = length(dates_choc), dates_choc = dates_choc)
}

# ============================================================================
# CHOIX DES HYPERPARAMETRES (p, lambda)
# ============================================================================
# Jusqu'ici p = 5 et lambda = 0,15 etaient repris de Higgins (2014), sans
# justification propre a cet echantillon. Deux methodes de la litterature
# permettent de les choisir, et toutes deux sont implementees ici pour etre
# departagees hors echantillon.
#
# COMPARABILITE ENTRE ORDRES DE RETARD
#   Un VAR(p) estime sur T observations n'en utilise que T - p. Comparer des
#   vraisemblances calculees sur des echantillons de tailles differentes n'a
#   aucun sens. Toutes les fonctions ci-dessous prennent donc un argument
#   `p_max` et n'utilisent QUE les observations (p_max + 1) ... T, quel que
#   soit le p teste : l'echantillon effectif est identique pour tous les
#   candidats.
# ============================================================================

#' Tronque l'echantillon pour que tous les ordres de retard soient compares sur
#' les memes observations expliquees.
aligner_echantillon <- function(Y, exo, p, p_max) {
  Tn <- nrow(Y)
  if (Tn <= p_max + 5L) return(NULL)
  debut <- p_max - p + 1L
  list(Y = Y[debut:Tn, , drop = FALSE],
       exo = if (is.null(exo)) NULL else exo[debut:Tn, , drop = FALSE])
}

#' Log-vraisemblance marginale du BVAR a prior normal-inverse-Wishart impose par
#' observations fictives (Giannone, Lenza & Primiceri 2015 ; annexe de Banbura,
#' Giannone & Reichlin 2010).
#'
#' Soit (Y, X) les observations reelles, (Yd, Xd) les observations fictives du
#' prior, et Y* = [Y ; Yd], X* = [X ; Xd] le systeme augmente. On note
#'   T   nombre d'observations reelles      Td  nombre d'observations fictives
#'   k   nombre de regresseurs              n   nombre de variables
#'   Sd  = Yd'Yd - Yd'Xd (Xd'Xd)^-1 Xd'Yd   (matrice d'echelle a priori)
#'   S*  = Y*'Y* - Y*'X* (X*'X*)^-1 X*'Y*   (matrice d'echelle a posteriori)
#'
#' Alors
#'
#'   log p(Y | lambda, p) =
#'       - (n T / 2) log(pi)
#'       + somme_{i=1..n} [ lgamma((T + Td - k + 1 - i)/2)
#'                          - lgamma((Td - k + 1 - i)/2) ]
#'       - (n/2) [ log|X*'X*| - log|Xd'Xd| ]
#'       - ((T + Td - k)/2) log|S*| + ((Td - k)/2) log|Sd|
#'
#' Maximiser cette quantite en (p, lambda) revient a retenir les
#' hyperparametres les plus vraisemblables au vu des seules donnees fournies --
#' donc, dans le backtest, au vu de la seule information disponible a l'origine.
log_vraisemblance_marginale <- function(Y, p, lambda, exo = NULL, sigma = NULL,
                                        d = 1) {
  n <- ncol(Y)
  exo <- filtrer_indicatrices_utiles(exo, p)
  if (is.null(sigma)) sigma <- echelles_variables(Y, p, exo)
  sys <- tryCatch(construire_systeme(Y, p, exo), error = function(e) NULL)
  if (is.null(sys)) return(NA_real_)
  dum <- construire_dummies_minnesota(sigma, n, p, lambda, k = sys$k, d = d)

  Td <- nrow(dum$Y); Treel <- nrow(sys$Y); k <- ncol(sys$X)
  if (Td - k + 1L - n <= 0L) return(NA_real_)   # prior impropre

  Xd <- dum$X; Yd <- dum$Y
  Xs <- rbind(sys$X, Xd); Ys <- rbind(sys$Y, Yd)

  ld <- function(M) {
    r <- determinant(M, logarithm = TRUE)
    if (r$sign <= 0) return(NA_real_)
    as.numeric(r$modulus)
  }
  XdXd <- crossprod(Xd); XsXs <- crossprod(Xs)
  l_XdXd <- ld(XdXd); l_XsXs <- ld(XsXs)
  if (is.na(l_XdXd) || is.na(l_XsXs)) return(NA_real_)

  Sd <- tryCatch(crossprod(Yd) - crossprod(Yd, Xd) %*% solve(XdXd, crossprod(Xd, Yd)),
                 error = function(e) NULL)
  Ss <- tryCatch(crossprod(Ys) - crossprod(Ys, Xs) %*% solve(XsXs, crossprod(Xs, Ys)),
                 error = function(e) NULL)
  if (is.null(Sd) || is.null(Ss)) return(NA_real_)
  l_Sd <- ld(Sd); l_Ss <- ld(Ss)
  if (is.na(l_Sd) || is.na(l_Ss)) return(NA_real_)

  i <- seq_len(n)
  - (n * Treel / 2) * log(pi) +
    sum(lgamma((Treel + Td - k + 1L - i) / 2) - lgamma((Td - k + 1L - i) / 2)) -
    (n / 2) * (l_XsXs - l_XdXd) -
    ((Treel + Td - k) / 2) * l_Ss + ((Td - k) / 2) * l_Sd
}

#' MSE en echantillon d'un VAR(p) non contraint, estime par MCO sur les seules
#' variables de reference. C'est le denominateur du critere BGR.
mse_petit_var_mco <- function(Y, p, exo, ref) {
  if (is.character(ref)) ref <- match(ref, colnames(Y))
  Yr <- Y[, ref, drop = FALSE]
  sys <- tryCatch(construire_systeme(Yr, p, exo), error = function(e) NULL)
  if (is.null(sys)) return(rep(NA_real_, length(ref)))
  if (nrow(sys$X) <= ncol(sys$X) + 1L) return(rep(NA_real_, length(ref)))
  B <- tryCatch(qr.solve(sys$X, sys$Y), error = function(e) NULL)
  if (is.null(B)) return(rep(NA_real_, length(ref)))
  colMeans((sys$Y - sys$X %*% B)^2)
}

#' Critere d'ajustement de Banbura, Giannone & Reichlin (2010, section 3.1).
#'
#' Principe : un grand BVAR contraint doit ajuster un petit ensemble de
#' variables de reference AUSSI BIEN, mais pas mieux, qu'un petit VAR non
#' contraint estime par moindres carres sur ces seules variables. Un ajustement
#' superieur signale un prior trop lache, donc du sur-ajustement.
#'
#'   Fit(lambda) = (1/|R|) somme_{i dans R}  MSE_i(lambda) / MSE_i(petit VAR MCO)
#'
#' et l'on retient le lambda qui amene Fit(lambda) au plus pres de 1.
critere_ajustement_bgr <- function(Y, p, lambda, exo = NULL, ref, sigma = NULL,
                                   mse_reference = NULL, d = 1) {
  exo <- filtrer_indicatrices_utiles(exo, p)
  if (is.character(ref)) ref <- match(ref, colnames(Y))
  mod <- tryCatch(estimer_bvar(Y, p = p, lambda = lambda, exo = exo,
                               sigma = sigma, d = d),
                  error = function(e) NULL)
  if (is.null(mod)) return(NA_real_)
  mse_bvar <- colMeans(mod$residus^2)[ref]
  if (is.null(mse_reference)) mse_reference <- mse_petit_var_mco(Y, p, exo, ref)
  if (any(!is.finite(mse_reference)) || any(mse_reference <= 0)) return(NA_real_)
  mean(mse_bvar / mse_reference)
}

#' Choix conjoint de (p, lambda) sur les SEULES donnees fournies.
#'
#' @param methode "ml" pour la vraisemblance marginale (Giannone, Lenza &
#'   Primiceri 2015), "bgr" pour le critere d'ajustement (Banbura, Giannone &
#'   Reichlin 2010), "fixe" pour conserver p_fixe et lambda_fixe.
#' @return liste (p, lambda, critere, table) ; `table` conserve tous les couples
#'   evalues, pour la tracabilite.
choisir_hyperparametres <- function(Y, exo = NULL,
                                    methode = c("ml", "bgr", "fixe"),
                                    p_grille = 1:5,
                                    d_grille = c(0.5, 1, 1.5, 2),
                                    lambda_bornes = c(0.01, 2),
                                    lambda_grille = NULL,
                                    ref = NULL, p_fixe = 5L, lambda_fixe = 0.15,
                                    d_fixe = 1) {
  methode <- match.arg(methode)
  if (methode == "fixe") {
    return(list(p = p_fixe, lambda = lambda_fixe, d = d_fixe,
                critere = NA_real_, table = NULL))
  }

  p_max <- max(p_grille)
  if (is.null(ref)) ref <- utils::head(colnames(Y), 3L)

  lignes <- list()
  for (p in p_grille) {
    al <- aligner_echantillon(Y, exo, p, p_max)
    if (is.null(al)) next
    # l'alignement peut rendre nulle une indicatrice qui ne l'etait pas : une
    # observation flaguee peut tomber dans les p premieres lignes, qui ne sont
    # jamais expliquees. On refiltre donc apres troncature.
    al$exo <- filtrer_indicatrices_utiles(al$exo, p)
    sig <- echelles_variables(al$Y, p, al$exo)
    mse_ref <- if (methode == "bgr") mse_petit_var_mco(al$Y, p, al$exo, ref) else NULL

    for (dd in d_grille) {
      # Le critere est evalue sur lambda par OPTIMISATION CONTINUE, et non sur
      # une grille : la surface est lisse et unimodale en log(lambda), donc une
      # recherche unidimensionnelle y suffit et evite d'attribuer au pas de
      # grille un ecart qui n'en est pas un.
      objectif <- function(ll) {
        lam <- exp(ll)
        if (methode == "ml") {
          v <- log_vraisemblance_marginale(al$Y, p, lam, al$exo, sigma = sig, d = dd)
          if (!is.finite(v)) return(-1e12)   # borne finie : optimize refuse -Inf
          v
        } else {
          f <- critere_ajustement_bgr(al$Y, p, lam, al$exo, ref, sigma = sig,
                                      mse_reference = mse_ref, d = dd)
          if (!is.finite(f)) return(-1e12)   # borne finie : optimize refuse -Inf
          -abs(f - 1)          # on maximise, donc on minimise l'ecart a 1
        }
      }
      opt <- tryCatch(stats::optimize(objectif, interval = log(lambda_bornes),
                                      maximum = TRUE, tol = 1e-4),
                      error = function(e) NULL)
      if (is.null(opt) || !is.finite(opt$objective) || opt$objective <= -1e11) next
      lignes[[length(lignes) + 1L]] <- data.frame(
        p = p, d = dd, lambda = exp(opt$maximum), valeur = opt$objective)
    }
  }
  if (length(lignes) == 0L) {
    return(list(p = p_fixe, lambda = lambda_fixe, d = d_fixe,
                critere = NA_real_, table = NULL))
  }
  tab <- do.call(rbind, lignes)
  tab <- tab[is.finite(tab$valeur), , drop = FALSE]
  if (nrow(tab) == 0L) {
    return(list(p = p_fixe, lambda = lambda_fixe, d = d_fixe,
                critere = NA_real_, table = NULL))
  }

  meilleur <- which.max(tab$valeur)      # les deux criteres sont maximises
  list(p = tab$p[meilleur], lambda = tab$lambda[meilleur], d = tab$d[meilleur],
       critere = tab$valeur[meilleur], table = tab)
}

# ============================================================================
# DETECTION DES TRIMESTRES DE CHOC
# ============================================================================
# La version precedente codait en dur DATES_CHOC = 2020 T1, T2, T3. Ce choix
# etait indefendable empiriquement : avec un seul episode dans l'echantillon,
# aucun exercice hors echantillon ne peut departager deux listes de dates.
#
# Une REGLE, elle, se teste. Appliquee a cet echantillon, elle se declenche sur
# plusieurs episodes temporellement separes -- 1998, 2000-2001, 2003, la crise
# financiere de 2008-2009, 2010, puis 2019-2021 -- et ses parametres deviennent
# comparables hors echantillon sur plusieurs episodes au lieu d'un.
#
# C'est aussi plus honnete en temps reel : un previsionniste de 2019 n'avait
# aucune indicatrice 2020 codee en dur, mais il pouvait avoir une regle.
#
# ROBUSTESSE DE L'ECHELLE
#   Les moments sont estimes par la MEDIANE et l'ECART ABSOLU MEDIAN, non par
#   la moyenne et l'ecart-type. Avec ces derniers, un choc majeur gonfle
#   l'echelle et masque les chocs voisins : le rebond de 2020 T3 deviendrait
#   invisible apres l'effondrement de T2. La MAD, elle, ne bouge pratiquement
#   pas. Le facteur 1,4826 applique par stats::mad la rend comparable a un
#   ecart-type sous hypothese gaussienne.
#
# ANTI-LOOK-AHEAD
#   La fonction ne recoit que l'echantillon d'entrainement de son origine.
#   Reperer un point aberrant A L'INTERIEUR de cet echantillon n'est pas du
#   look-ahead : c'est du nettoyage de l'information deja disponible.

#' Trimestres ou au moins `k` branches s'ecartent de plus de `z` echelles
#' robustes de leur mediane.
#'
#' @param Y matrice T x n de l'echantillon d'entrainement.
#' @param dates dates correspondant aux lignes de Y.
#' @param z seuil en nombre d'ecarts absolus medians.
#' @param k nombre minimal de branches devant depasser le seuil.
#' @param min_obs taille minimale pour estimer les moments.
#' @param saisonnier si TRUE, les moments sont estimes SEPAREMENT pour chaque
#'   trimestre calendaire.
#' @return vecteur de dates, eventuellement vide.
#'
#' POURQUOI LA COMPARAISON DOIT ETRE SAISONNIERE
#'   Les valeurs ajoutees ne sont pas corrigees des variations saisonnieres.
#'   Une regle qui compare chaque trimestre a la mediane de TOUS les trimestres
#'   flague donc les premiers trimestres en serie -- sur cet echantillon,
#'   T1-2004, T1-2005, T1-2006, T1-2007, T1-2008, T1-2010, T1-2011, T1-2017...
#'   Ce n'est pas une suite de chocs, c'est la saisonnalite de l'agriculture et
#'   de la peche.
#'
#'   Chaque trimestre est donc compare aux trimestres de MEME RANG CALENDAIRE.
detecter_chocs <- function(Y, dates, z = 3, k = 2L, min_obs = 20L,
                           saisonnier = TRUE) {
  if (is.na(z) || is.na(k) || nrow(Y) < min_obs) return(as.Date(character(0)))
  dates <- as.Date(dates)
  groupe <- if (saisonnier) lubridate::quarter(dates) else rep(1L, length(dates))

  depasse <- matrix(FALSE, nrow = nrow(Y), ncol = ncol(Y))
  for (g in unique(groupe)) {
    lignes <- which(groupe == g)
    if (length(lignes) < 8L) next          # trop peu pour une echelle robuste
    bloc <- Y[lignes, , drop = FALSE]
    centre  <- apply(bloc, 2, stats::median, na.rm = TRUE)
    echelle <- apply(bloc, 2, stats::mad, na.rm = TRUE)
    faible  <- !is.finite(echelle) | echelle <= 0
    if (any(faible)) {
      echelle[faible] <- apply(bloc[, faible, drop = FALSE], 2, stats::sd, na.rm = TRUE)
    }
    if (any(!is.finite(echelle) | echelle <= 0)) next
    depasse[lignes, ] <- sweep(abs(sweep(bloc, 2, centre, "-")), 2, z * echelle, ">")
  }
  as.Date(dates[rowSums(depasse, na.rm = TRUE) >= k])
}
