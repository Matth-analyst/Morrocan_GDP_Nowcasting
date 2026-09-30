# ============================================================================
# 05_ar4_non_couvertes.R -- ETAPE 5 : les branches sans indicateurs
# ============================================================================
# Plan de correction : etape 5, et phase 9 ("Branches non couvertes : AR(4)").
#
# LE PROBLEME
#   Quatre branches sur seize n'ont AUCUN indicateur dans le vivier : services
#   aux entreprises, administration publique, education-sante, autres services.
#   Aucune equation de passerelle n'est donc possible, et le plan prevoit pour
#   elles un AR(4) :
#
#       g_{j,t} = c_j + phi_{j1} g_{j,t-1} + ... + phi_{j4} g_{j,t-4} + eps_{j,t}
#
#   reestime a CHAQUE origine du backtest.
#
# UN POINT QUE LE PLAN NE DIT PAS, ET QUI CHANGE LA QUESTION
#   Le BVAR de la phase 3 porte sur les SEIZE branches, celles-ci comprises. Il
#   produit donc deja une prevision pour chacune d'elles. L'AR(4) n'est pas un
#   modele de remplacement faute de mieux : c'est un CONCURRENT, et la question
#   n'est pas "que faire de ces branches" mais "l'AR(4) fait-il mieux que le
#   BVAR sur elles".
#
#   La reponse n'est pas acquise d'avance. Sur ces quatre branches le BVAR donne
#   des ratios de 0,941, 0,954 et 0,968 -- mais 1,262 sur l'administration
#   publique, son PIRE resultat des seize branches. Il y a donc au moins une
#   branche ou un modele plus simple a des chances.
#
# CE QUE LE SCRIPT COMPARE
#   Deux etalons naifs, le modele du plan, et quatre variantes de robustesse que
#   le plan demande explicitement pour les branches saisonnieres :
#
#     moyenne recursive          l'etalon minimal : prevoir la moyenne passee
#     marche aleatoire           g_T = g_{T-1}
#     AR(4)                      le modele du plan
#     AR(p), p par BIC           l'ordre est-il justifiable dans les donnees ?
#     AR(4) + indicatrices choc  meme regle de detection qu'en phase 3
#     AR(4) + indicatrices saison
#     SARIMA(1,0,0)(1,0,0)[4]
#     BVAR                       relu de la phase 3
#
# SORTIES
#   resultats/05_previsions_ar.csv
#   resultats/05_evaluation.csv
#   resultats/05_par_periode.csv
#   resultats/05_ordre_retenu.csv
#   figures/05_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/bvar.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("05_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("05_", x))

PREMIERE_CIBLE <- as.Date("2014-06-30")
P_AR           <- 4L            # l'ordre du plan
P_GRILLE_BIC   <- 1:6
REGLE_Z <- 4; REGLE_K <- 3      # regle de detection retenue en phase 3

cat("\n[1/5] Bases\n")
couverture <- charger_couverture()
NON_COUVERTES <- couverture$non_couvertes
cat(sprintf("      %d branches sans indicateur : %s\n",
            length(NON_COUVERTES), paste(NON_COUVERTES, collapse = ", ")))

va <- charger_va() %>%
  dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)

# Matrice des 16 branches : sert uniquement a la detection des chocs, qui est
# multivariee (un trimestre est un choc si K branches decrochent ensemble).
large <- va %>% tidyr::pivot_wider(names_from = branche, values_from = g) %>%
  dplyr::arrange(date) %>%
  dplyr::filter(dplyr::if_all(dplyr::all_of(TOUTES_BRANCHES), ~ !is.na(.)))
dates_vec <- large$date
Y <- as.matrix(large[, TOUTES_BRANCHES])

origines <- sort(unique(va$date[va$date >= PREMIERE_CIBLE]))
cat(sprintf("      %d origines, de %s a %s\n", length(origines),
            date_vers_trimestre(origines[1]),
            date_vers_trimestre(origines[length(origines)])))

bvar <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_recursives.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::filter(branche %in% NON_COUVERTES) %>%
  dplyr::select(branche, origine, prevision, reel)
cat(sprintf("      %d previsions BVAR relues pour ces branches\n", nrow(bvar)))

# ============================================================================
# 2) LES MODELES
# ============================================================================
#' Construit la matrice de retards d'une serie univariee.
retards <- function(g, p) {
  n <- length(g)
  if (n <= p) return(NULL)
  X <- sapply(seq_len(p), function(l) g[(p + 1 - l):(n - l)])
  if (p == 1L) X <- matrix(X, ncol = 1L)
  list(y = g[(p + 1):n], X = X, dernier = g[(n - p + 1):n])
}

#' AR(p) estime par moindres carres, avec exogenes optionnelles.
#'
#' `exo_train` et `exo_cible` permettent d'ajouter des indicatrices -- de choc
#' ou de saison. En prevision les indicatrices de CHOC valent zero, comme en
#' phase 3 : elles nettoient l'estimation, elles n'annoncent rien.
ajuster_ar <- function(g, p, exo_train = NULL, exo_cible = NULL) {
  r <- retards(g, p)
  if (is.null(r)) return(NULL)
  X <- cbind(1, r$X)
  if (!is.null(exo_train)) {
    e <- exo_train[(p + 1):length(g), , drop = FALSE]
    # une colonne constante sur l'echantillon de regression rend X singuliere
    utiles <- apply(e, 2, function(cc) length(unique(cc)) > 1L)
    if (any(utiles)) X <- cbind(X, e[, utiles, drop = FALSE])
    else exo_cible <- NULL
  }
  if (nrow(X) < ncol(X) + 5L) return(NULL)
  bb <- tryCatch(qr.solve(X, r$y), error = function(e) NULL)
  if (is.null(bb)) return(NULL)
  # prevision : constante + retards les plus recents, indicatrices a zero
  x_new <- c(1, rev(r$dernier))
  if (length(bb) > length(x_new)) x_new <- c(x_new, rep(0, length(bb) - length(x_new)))
  list(prevision = sum(bb * x_new[seq_along(bb)]),
       n_obs = nrow(X), k = ncol(X),
       sigma2 = sum((r$y - X %*% bb)^2) / max(1, nrow(X) - ncol(X)))
}

#' Ordre choisi par critere bayesien d'information, sur echantillon ALIGNE.
#'
#' Comparer des ordres sur des echantillons de tailles differentes n'a pas de
#' sens -- le plus petit p aurait mecaniquement plus d'observations. Tous les
#' candidats sont donc evalues sur les memes observations, celles qu'autorise
#' le p maximal de la grille.
choisir_ordre <- function(g, grille) {
  p_max <- max(grille)
  meilleur <- NULL; bic_min <- Inf
  for (p in grille) {
    r <- retards(g, p_max)          # echantillon commun
    if (is.null(r)) next
    n <- length(r$y)
    rp <- retards(g, p)
    if (is.null(rp)) next
    # on tronque la matrice du modele p a l'echantillon commun
    dec <- length(rp$y) - n
    if (dec < 0L) next
    X <- cbind(1, rp$X[(dec + 1):length(rp$y), , drop = FALSE])
    y <- rp$y[(dec + 1):length(rp$y)]
    if (nrow(X) < ncol(X) + 5L) next
    bb <- tryCatch(qr.solve(X, y), error = function(e) NULL)
    if (is.null(bb)) next
    rss <- sum((y - X %*% bb)^2)
    bic <- n * log(rss / n) + ncol(X) * log(n)
    if (is.finite(bic) && bic < bic_min) { bic_min <- bic; meilleur <- p }
  }
  if (is.null(meilleur)) P_AR else meilleur
}

#' Indicatrices saisonnieres (trois colonnes, T1 en reference).
indicatrices_saison <- function(dates) {
  tri <- lubridate::quarter(dates)
  cbind(T2 = as.numeric(tri == 2), T3 = as.numeric(tri == 3),
        T4 = as.numeric(tri == 4))
}

cat("\n[2/5] Estimation recursive\n")
lignes <- list()
ordres <- list()

for (i_o in seq_along(origines)) {
  cible <- origines[i_o]

  # --- chocs detectes sur la seule information anterieure, regle de la phase 3
  ok <- dates_vec < cible
  chocs <- detecter_chocs(Y[ok, , drop = FALSE], dates_vec[ok],
                          z = REGLE_Z, k = REGLE_K)

  for (b in NON_COUVERTES) {
    h <- va %>% dplyr::filter(branche == b, date < cible) %>% dplyr::arrange(date)
    if (nrow(h) < 24L) next
    g <- h$g; d <- h$date
    reel <- va$g[va$branche == b & va$date == cible]
    reel <- if (length(reel) == 1L) reel else NA_real_

    exo_choc <- matrix(as.numeric(d %in% chocs), ncol = 1L,
                       dimnames = list(NULL, "choc"))
    exo_sais <- indicatrices_saison(d)

    p_bic <- choisir_ordre(g, P_GRILLE_BIC)
    ordres[[length(ordres) + 1L]] <- tibble::tibble(
      branche = b, origine = cible, p_bic = p_bic)

    m_ar4   <- ajuster_ar(g, P_AR)
    m_arbic <- ajuster_ar(g, p_bic)
    m_choc  <- ajuster_ar(g, P_AR, exo_choc)
    m_sais  <- ajuster_ar(g, P_AR, exo_sais)

    # SARIMA : composante saisonniere d'ordre 4 sur donnees trimestrielles
    m_sar <- tryCatch({
      fit <- stats::arima(stats::ts(g, frequency = 4L), order = c(1, 0, 0),
                          seasonal = list(order = c(1, 0, 0), period = 4L),
                          method = "ML")
      as.numeric(stats::predict(fit, n.ahead = 1L)$pred[1])
    }, error = function(e) NA_real_)

    lignes[[length(lignes) + 1L]] <- tibble::tibble(
      branche = b, origine = cible, reel = reel, derniere_obs = max(d),
      `moyenne recursive` = mean(g),
      `marche aleatoire`  = g[length(g)],
      `AR(4)`             = if (is.null(m_ar4))   NA_real_ else m_ar4$prevision,
      `AR(p) par BIC`     = if (is.null(m_arbic)) NA_real_ else m_arbic$prevision,
      `AR(4) + choc`      = if (is.null(m_choc))  NA_real_ else m_choc$prevision,
      `AR(4) + saison`    = if (is.null(m_sais))  NA_real_ else m_sais$prevision,
      `SARIMA`            = m_sar)
  }
}

previsions <- dplyr::bind_rows(lignes)
ordres_df  <- dplyr::bind_rows(ordres)

stopifnot("[ANTI-LOOK-AHEAD] une estimation a vu sa cible" =
            all(previsions$derniere_obs < previsions$origine))
cat("      controle : aucune estimation ne contient sa propre cible\n")
cat(sprintf("      %d lignes (%d origines x %d branches)\n", nrow(previsions),
            dplyr::n_distinct(previsions$origine),
            dplyr::n_distinct(previsions$branche)))

long <- previsions %>%
  tidyr::pivot_longer(c(`moyenne recursive`, `marche aleatoire`, `AR(4)`,
                        `AR(p) par BIC`, `AR(4) + choc`, `AR(4) + saison`,
                        `SARIMA`),
                      names_to = "modele", values_to = "prevision") %>%
  dplyr::select(branche, origine, modele, prevision, reel) %>%
  dplyr::bind_rows(bvar %>% dplyr::mutate(modele = "BVAR (phase 3)") %>%
                     dplyr::select(branche, origine, modele, prevision, reel))
ecrire_csv(long, chemin_res("previsions_ar.csv"))
ecrire_csv(ordres_df, chemin_res("ordre_retenu.csv"))

cat(sprintf("      ordre choisi par BIC : median %d, etendue %d-%d\n",
            stats::median(ordres_df$p_bic), min(ordres_df$p_bic),
            max(ordres_df$p_bic)))

# ============================================================================
# 3) EVALUATION
# ============================================================================
cat("\n[3/5] Evaluation\n")
evaluer <- function(df, lab) {
  df %>% dplyr::filter(!is.na(prevision), !is.na(reel)) %>%
    dplyr::group_by(modele, branche) %>%
    dplyr::filter(dplyr::n() >= 4L) %>%
    dplyr::summarise(n = dplyr::n(),
                     mae = mean(abs(reel - prevision)),
                     ratio = sqrt(mean((reel - prevision)^2)) / stats::sd(reel),
                     correlation = suppressWarnings(stats::cor(prevision, reel)),
                     .groups = "drop") %>%
    dplyr::mutate(periode = lab)
}
par_branche <- dplyr::bind_rows(
  evaluer(long, "toutes origines"),
  evaluer(long %>% dplyr::filter(lubridate::year(origine) == 2020), "2020"),
  evaluer(long %>% dplyr::filter(lubridate::year(origine) != 2020), "hors 2020"))
ecrire_csv(par_branche, chemin_res("evaluation.csv"))

bilan <- par_branche %>% dplyr::group_by(modele, periode) %>%
  dplyr::summarise(branches = dplyr::n(),
                   mae_moyenne = mean(mae),
                   ratio_median = stats::median(ratio),
                   n_ok = sum(ratio < 1),
                   correl = stats::median(correlation, na.rm = TRUE),
                   .groups = "drop")
ecrire_csv(bilan, chemin_res("par_periode.csv"))

for (p in c("toutes origines", "2020", "hors 2020")) {
  cat(sprintf("\n      --- %s ---\n", p))
  print(bilan %>% dplyr::filter(periode == p) %>% dplyr::arrange(ratio_median) %>%
          dplyr::transmute(modele, `MAE` = round(mae_moyenne, 4),
                           `ratio med.` = round(ratio_median, 3),
                           `br. < 1` = sprintf("%d/%d", n_ok, branches),
                           `correl.` = round(correl, 2)), n = 10)
}

cat("\n      --- ratio par branche, toutes origines ---\n")
tab <- par_branche %>% dplyr::filter(periode == "toutes origines") %>%
  dplyr::select(branche, modele, ratio) %>%
  tidyr::pivot_wider(names_from = modele, values_from = ratio) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 3)))
print(as.data.frame(tab))

meilleur <- par_branche %>% dplyr::filter(periode == "toutes origines") %>%
  dplyr::group_by(branche) %>% dplyr::slice_min(ratio, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>% dplyr::select(branche, modele, ratio)
ecrire_csv(meilleur, chemin_res("meilleur_par_branche.csv"))
cat("\n      --- meilleur modele par branche ---\n")
print(as.data.frame(meilleur %>% dplyr::mutate(ratio = round(ratio, 3))))

# ============================================================================
# 4) FIGURES
# ============================================================================
cat("\n[4/5] Figures\n")
g1 <- par_branche %>% dplyr::filter(periode == "toutes origines") %>%
  ggplot2::ggplot(ggplot2::aes(stats::reorder(modele, -ratio, stats::median),
                               ratio)) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  ggplot2::geom_point(ggplot2::aes(colour = branche), size = 2.2) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "RMSFE / ecart-type de la branche", colour = NULL,
                title = "Les quatre branches sans indicateurs",
                subtitle = "a gauche du trait, le modele fait mieux que predire la moyenne")
ggplot2::ggsave(chemin_fig("comparaison_modeles.png"), g1,
                width = 9.5, height = 5, dpi = 150)

g2 <- long %>% dplyr::filter(modele %in% c("AR(4)", "BVAR (phase 3)")) %>%
  ggplot2::ggplot(ggplot2::aes(origine)) +
  ggplot2::geom_line(ggplot2::aes(y = reel), linewidth = 0.45) +
  ggplot2::geom_line(ggplot2::aes(y = prevision, colour = modele), linewidth = 0.45) +
  ggplot2::facet_wrap(~ branche, scales = "free_y", ncol = 2) +
  ggplot2::labs(x = NULL, y = "croissance trimestrielle", colour = NULL,
                title = "Realise et prevu, AR(4) contre BVAR",
                subtitle = "les quatre branches non couvertes par le vivier")
ggplot2::ggsave(chemin_fig("previsions_par_branche.png"), g2,
                width = 10, height = 6, dpi = 150)

g3 <- ordres_df %>%
  ggplot2::ggplot(ggplot2::aes(origine, p_bic)) +
  ggplot2::geom_step(linewidth = 0.45) +
  ggplot2::geom_hline(yintercept = P_AR, linetype = "dashed", colour = "#b03a2e") +
  ggplot2::facet_wrap(~ branche, ncol = 2) +
  ggplot2::labs(x = NULL, y = "ordre retenu",
                title = "Ordre autoregressif choisi par BIC a chaque origine",
                subtitle = "le trait rouge marque l'ordre 4 impose par le plan")
ggplot2::ggsave(chemin_fig("ordre_bic.png"), g3, width = 10, height = 5, dpi = 150)

cat("      figures/05_comparaison_modeles.png\n")
cat("      figures/05_previsions_par_branche.png\n")
cat("      figures/05_ordre_bic.png\n")
cat("\n[5/5] Etape 5 terminee.\n")
