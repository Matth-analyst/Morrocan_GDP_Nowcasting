# ============================================================================
# 12c_intervalle_combinaison.R -- Un intervalle autour du chiffre publie
# ============================================================================
# LE TROU QUE CECI COMBLE
#   Le script 12 produit une loi predictive pour le BVAR. Mais le nowcast
#   PUBLIE est la COMBINAISON BVAR + passerelle. Le chiffre qui compte etait
#   donc le seul sans bornes : +2,20 % sans intervalle, face a +1,04 %
#   [-0,35 ; +2,49] pour une composante qu'on ne publie pas.
#
# POURQUOI ON NE PROPAGE PAS ANALYTIQUEMENT
#   La tentation serait d'ecrire, branche par branche,
#
#       V_comb = delta^2 V_bvar + (1-delta)^2 V_pass + 2 delta (1-delta) rho ...
#
#   puis d'agreger. Deux obstacles, dont le second est redhibitoire.
#
#   (a) La correlation rho entre les erreurs des deux composantes est estimable
#       sur l'historique, donc celui-la se traite.
#   (b) L'agregation exige la COVARIANCE CROISEE DES BRANCHES pour la
#       passerelle. Le BVAR la fournit -- c'est sa matrice Sigma -- mais les
#       passerelles sont seize regressions INDEPENDANTES, estimees separement,
#       sur des indicateurs differents. Leur covariance croisee n'est pas
#       identifiee par le modele. La supposer nulle sous-estimerait gravement
#       l'intervalle agrege, puisque les branches se trompent ensemble lors des
#       ruptures -- c'est precisement ce que 2020 a montre.
#
#   On construit donc l'intervalle DIRECTEMENT au niveau de l'agregat, ou la
#   covariance croisee n'a plus a etre modelisee : elle est deja dans l'erreur
#   agregee observee.
#
# LA METHODE
#   Prediction conforme sur les erreurs de la COMBINAISON, comme au script 12b
#   mais appliquee au bon objet. Deux variantes, qui different par l'echelle :
#
#     echelle variable   z_t = (realise_t - combinaison_t) / sd_bvar_t
#                        l'intervalle se resserre quand le BVAR est confiant
#     echelle constante  z_t = realise_t - combinaison_t
#                        largeur identique a toutes les origines
#
#   La premiere n'a de sens que si l'incertitude du BVAR renseigne sur celle de
#   la combinaison. Rien ne le garantit : on compare donc les deux sur leur
#   couverture, et c'est la mesure qui tranche.
#
#   Tout est recursif : le quantile de l'origine T ne connait que les erreurs
#   anterieures a T.
#
# SORTIES
#   resultats/12c_intervalle_combinaison.csv
#   resultats/12c_couverture.csv
#   resultats/12c_nowcast_publie.csv
#   figures/12c_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("12c_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("12c_", x))

MIN_SCORES <- 12L
NIVEAUX    <- c(0.50, 0.80, 0.90)

cat("\n[1/4] Bases\n")
ag <- lire_csv(file.path(DOSSIER_RESULTATS, "06_agregat.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(origine, trimestre, reel = reel_niveau,
                combinaison = nowcast_niveau, bvar = bvar_niveau)
pred <- lire_csv(file.path(DOSSIER_RESULTATS, "12_predictive_agregat.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(origine, sd_bvar = sd_predictive)
base <- ag %>% dplyr::inner_join(pred, by = "origine") %>% dplyr::arrange(origine)
cat(sprintf("      %d origines, de %s a %s\n", nrow(base),
            base$trimestre[1], base$trimestre[nrow(base)]))
cat(sprintf("      RMSFE combinaison %.3f pt | BVAR %.3f pt\n",
            100 * sqrt(mean((base$reel - base$combinaison)^2)),
            100 * sqrt(mean((base$reel - base$bvar)^2))))

# ============================================================================
# 2) INTERVALLES CONFORMES
# ============================================================================
cat("\n[2/4] Construction des intervalles\n")
base$err <- base$reel - base$combinaison
base$z_var <- base$err / base$sd_bvar

construire <- function(base, niveau, echelle = c("variable", "constante")) {
  echelle <- match.arg(echelle)
  alpha <- 1 - niveau; n <- nrow(base)
  bas <- rep(NA_real_, n); haut <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    scores <- if (echelle == "variable") base$z_var[seq_len(i - 1L)]
              else base$err[seq_len(i - 1L)]
    scores <- scores[is.finite(scores)]
    ech_i <- if (echelle == "variable") base$sd_bvar[i] else 1
    if (length(scores) < MIN_SCORES) {
      # pas assez d'historique : on retombe sur l'ecart-type du BVAR et une loi
      # normale, ce qui est l'information disponible a ce stade
      q_b <- stats::qnorm(alpha / 2) * base$sd_bvar[i]
      q_h <- stats::qnorm(1 - alpha / 2) * base$sd_bvar[i]
    } else {
      # quantiles ASYMETRIQUES : les erreurs de rupture sont massivement
      # negatives, elargir vers le haut serait gratuit
      q_b <- unname(stats::quantile(scores, alpha / 2)) * ech_i
      q_h <- unname(stats::quantile(scores, 1 - alpha / 2)) * ech_i
    }
    bas[i]  <- base$combinaison[i] + q_b
    haut[i] <- base$combinaison[i] + q_h
  }
  tibble::tibble(origine = base$origine, trimestre = base$trimestre,
                 niveau = niveau, echelle = echelle,
                 combinaison = base$combinaison, reel = base$reel,
                 bas = bas, haut = haut)
}

intervalles <- purrr::map_dfr(NIVEAUX, function(nv) {
  dplyr::bind_rows(construire(base, nv, "variable"),
                   construire(base, nv, "constante"))
})
ecrire_csv(intervalles, chemin_res("intervalle_combinaison.csv"))

# ============================================================================
# 3) COUVERTURE
# ============================================================================
cat("\n[3/4] Couverture\n")
couv <- intervalles %>%
  dplyr::group_by(niveau, echelle) %>%
  dplyr::summarise(n = dplyr::n(),
                   couverture = mean(reel >= bas & reel <= haut),
                   largeur = mean(haut - bas), .groups = "drop") %>%
  dplyr::mutate(ecart = couverture - niveau) %>%
  dplyr::arrange(niveau, echelle)
ecrire_csv(couv, chemin_res("couverture.csv"))
print(as.data.frame(couv %>% dplyr::transmute(
  `niveau` = sprintf("%.0f %%", 100 * niveau), echelle,
  `couverture` = sprintf("%.0f %%", 100 * couverture),
  `ecart` = sprintf("%+.0f pt", 100 * ecart),
  `largeur moyenne` = sprintf("%.2f pt", 100 * largeur))), row.names = FALSE)

cat("\n      --- avant et apres 2020 ---\n")
print(as.data.frame(intervalles %>% dplyr::filter(niveau == 0.80) %>%
  dplyr::mutate(periode = ifelse(origine < as.Date("2020-01-01"),
                                 "avant 2020", "2020 et apres")) %>%
  dplyr::group_by(echelle, periode) %>%
  dplyr::summarise(n = dplyr::n(),
                   couverture = sprintf("%.0f %%", 100 * mean(reel >= bas & reel <= haut)),
                   .groups = "drop")), row.names = FALSE)

meilleure <- couv %>% dplyr::filter(niveau == 0.80) %>%
  dplyr::slice_min(abs(ecart), n = 1, with_ties = FALSE)
cat(sprintf("\n      echelle retenue a 80 %% : %s (ecart %+.0f pt, largeur %.2f pt)\n",
            meilleure$echelle, 100 * meilleure$ecart, 100 * meilleure$largeur))

# ============================================================================
# 4) LE NOWCAST PUBLIE
# ============================================================================
cat("\n[4/4] Intervalle autour du nowcast courant\n")
f_nc <- file.path(DOSSIER_RESULTATS, "08_nowcast_courant_agregat.csv")
f_iv <- file.path(DOSSIER_RESULTATS, "12_nowcast_intervalle.csv")
if (file.exists(f_nc) && file.exists(f_iv)) {
  nc <- lire_csv(f_nc); iv <- lire_csv(f_iv)
  point <- nc$nowcast[1]
  # ecart-type predictif du BVAR pour la cible courante, deduit de son
  # intervalle a 80 % : (q90 - q10) / (2 * 1,2816)
  sd_courant <- ((iv$q90 - iv$q10) / 100) / (2 * stats::qnorm(0.90))
  ech <- meilleure$echelle

  publie <- purrr::map_dfr(NIVEAUX, function(nv) {
    alpha <- 1 - nv
    scores <- if (ech == "variable") base$z_var else base$err
    ech_i <- if (ech == "variable") sd_courant else 1
    q_b <- unname(stats::quantile(scores, alpha / 2)) * ech_i
    q_h <- unname(stats::quantile(scores, 1 - alpha / 2)) * ech_i
    tibble::tibble(trimestre = nc$trimestre[1], niveau = nv, echelle = ech,
                   nowcast_pct = 100 * point,
                   bas_pct = 100 * (point + q_b),
                   haut_pct = 100 * (point + q_h))
  })
  ecrire_csv(publie, chemin_res("nowcast_publie.csv"))
  cat(sprintf("\n      %s : %+.2f %%\n", publie$trimestre[1], publie$nowcast_pct[1]))
  for (i in seq_len(nrow(publie))) {
    cat(sprintf("        a %.0f %% : [%+.2f ; %+.2f]\n",
                100 * publie$niveau[i], publie$bas_pct[i], publie$haut_pct[i]))
  }
  cat("\n      couverture OBSERVEE de ces bornes sur le backtest :\n")
  for (i in seq_len(nrow(couv %>% dplyr::filter(echelle == ech)))) {
    l <- couv %>% dplyr::filter(echelle == ech) %>% dplyr::slice(i)
    cat(sprintf("        annonce %.0f %% -> observe %.0f %%\n",
                100 * l$niveau, 100 * l$couverture))
  }
} else {
  cat("      nowcast courant ou intervalle BVAR absent.\n")
}

# --- figures -----------------------------------------------------------------
g1 <- intervalles %>% dplyr::filter(niveau == 0.80) %>%
  ggplot2::ggplot(ggplot2::aes(origine)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = 100 * bas, ymax = 100 * haut),
                       fill = "grey78") +
  ggplot2::geom_line(ggplot2::aes(y = 100 * combinaison), colour = "#1f4e79",
                     linewidth = 0.5) +
  ggplot2::geom_point(ggplot2::aes(y = 100 * reel), size = 1.1) +
  ggplot2::facet_wrap(~ echelle) +
  ggplot2::labs(x = NULL, y = "croissance trimestrielle (%)",
                title = "Intervalle a 80 % autour du nowcast publie",
                subtitle = "en bleu la combinaison, en points le realise")
ggplot2::ggsave(chemin_fig("intervalle.png"), g1, width = 11, height = 4.5, dpi = 150)

g2 <- couv %>%
  ggplot2::ggplot(ggplot2::aes(100 * niveau, 100 * couverture, colour = echelle)) +
  ggplot2::geom_abline(linetype = "dashed", linewidth = 0.3) +
  ggplot2::geom_line(linewidth = 0.55) + ggplot2::geom_point(size = 2.2) +
  ggplot2::labs(x = "niveau annonce (%)", y = "couverture observee (%)",
                colour = NULL,
                title = "Les intervalles tiennent-ils leur promesse ?",
                subtitle = "sur la diagonale, la couverture annoncee est la couverture reelle")
ggplot2::ggsave(chemin_fig("couverture.png"), g2, width = 7, height = 4.5, dpi = 150)

cat("\n      figures/12c_intervalle.png\n      figures/12c_couverture.png\n")
cat("\nIntervalle de la combinaison termine.\n")
