# ============================================================================
# 12b_recalibration.R -- Corriger la calibration des intervalles
# ============================================================================
# LE CONSTAT
#   La distribution predictive du BVAR est mal calibree, et systematiquement
#   trop etroite :
#
#       niveau 50 %  ->  couverture reelle 54 %   (+4 pt)
#       niveau 80 %  ->                    71 %   (-9 pt)
#       niveau 90 %  ->                    79 %  (-11 pt)
#
#   Quatorze trimestres sur 48 sortent de l'intervalle a 80 %, et ce sont les
#   trimestres de rupture : T2-2020 realise -11,01 % contre un intervalle
#   [-1,26 ; +1,95]. Le modele est le plus confiant precisement la ou il se
#   trompe le plus.
#
#   La cause est identifiee : l'intervalle propage l'incertitude sur B et Sigma,
#   mais traite les HYPERPARAMETRES comme connus -- p, lambda, d, la regle de
#   detection des chocs, la fenetre sigma. C'est une borne basse par
#   construction.
#
# CE QU'ON NE FAIT PAS
#   Multiplier les bornes par un facteur choisi pour que la couverture tombe
#   juste SUR L'ECHANTILLON D'EVALUATION. Ce serait calibrer sur le test, et la
#   couverture annoncee ne vaudrait rien -- exactement l'erreur que la phase 3 a
#   documentee avec la selection conjointe.
#
# CE QU'ON FAIT : LA RECALIBRATION CONFORME
#   On laisse les ERREURS PASSEES du modele fixer la largeur. Pour chaque
#   origine T, on forme le score standardise des origines anterieures
#
#       z_t = ( realise_t - mediane_t ) / ecart_type_predictif_t ,   t < T
#
#   et l'on prend le quantile empirique de |z| au niveau voulu :
#
#       intervalle_T = mediane_T  +/-  q_{1-alpha}( |z|_{t<T} ) * ecart_type_T
#
#   Sous une hypothese d'echangeabilite des scores, cette construction a une
#   couverture garantie -- c'est le principe de la prediction conforme (Vovk,
#   Gammerman & Shafer). Elle est DISTRIBUTION-FREE : on ne suppose rien sur la
#   forme de la loi predictive, seulement que les erreurs passees renseignent
#   sur les erreurs futures.
#
#   Tout est calcule sur t < T uniquement : la largeur de l'intervalle de 2020
#   ne doit rien a ce qui s'est passe en 2020.
#
# UNE VARIANTE ASYMETRIQUE, ET POURQUOI
#   Les erreurs de 2020 sont massivement NEGATIVES : le modele n'a pas vu
#   l'effondrement. Un intervalle symetrique en |z| s'elargit alors des deux
#   cotes, ce qui est inutile vers le haut. On teste donc aussi une version qui
#   prend les quantiles des z signes separement.
#
# LA LIMITE QUI SUBSISTE, ET ELLE EST SERIEUSE
#   L'echangeabilite est justement ce que 2020 viole. Avant 2020 les scores ne
#   contiennent aucun evenement comparable, donc la recalibration ne peut pas
#   anticiper l'ampleur du choc -- elle ne fait que s'y adapter APRES. On
#   mesurera donc la couverture separement avant et apres.
#
# SORTIES
#   resultats/12b_intervalles_recalibres.csv
#   resultats/12b_calibration_comparee.csv
#   figures/12b_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("12b_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("12b_", x))

MIN_SCORES <- 12L    # en deca, on retombe sur le quantile gaussien
NIVEAUX    <- c(0.50, 0.80, 0.90)

cat("\n[1/4] Lecture de la distribution predictive\n")
f <- file.path(DOSSIER_RESULTATS, "12_predictive_agregat.csv")
if (!file.exists(f)) {
  stop("12_predictive_agregat.csv absent : executer R/12_incertitude_parametrique.R",
       call. = FALSE)
}
pred <- lire_csv(f) %>% dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::arrange(origine)
cat(sprintf("      %d origines, de %s a %s\n", nrow(pred),
            pred$trimestre[1], pred$trimestre[nrow(pred)]))

# ============================================================================
# 2) RECALIBRATION, RECURSIVE
# ============================================================================
cat("\n[2/4] Recalibration conforme\n")
pred$z <- (pred$reel - pred$mediane) / pred$sd_predictive

recalibrer <- function(pred, niveau, asymetrique = FALSE) {
  alpha <- 1 - niveau
  n <- nrow(pred)
  bas <- rep(NA_real_, n); haut <- rep(NA_real_, n); facteur <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    z_passes <- pred$z[seq_len(i - 1L)]
    z_passes <- z_passes[is.finite(z_passes)]
    if (length(z_passes) < MIN_SCORES) {
      # pas assez d'historique : on garde le quantile gaussien, qui est ce que
      # la loi predictive donnait deja
      q_bas <- stats::qnorm(alpha / 2); q_haut <- stats::qnorm(1 - alpha / 2)
    } else if (asymetrique) {
      q_bas  <- unname(stats::quantile(z_passes, alpha / 2))
      q_haut <- unname(stats::quantile(z_passes, 1 - alpha / 2))
    } else {
      q <- unname(stats::quantile(abs(z_passes), niveau))
      q_bas <- -q; q_haut <- q
    }
    bas[i]  <- pred$mediane[i] + q_bas  * pred$sd_predictive[i]
    haut[i] <- pred$mediane[i] + q_haut * pred$sd_predictive[i]
    facteur[i] <- (q_haut - q_bas) / (stats::qnorm(1 - alpha / 2) -
                                        stats::qnorm(alpha / 2))
  }
  tibble::tibble(origine = pred$origine, trimestre = pred$trimestre,
                 niveau = niveau, methode = ifelse(asymetrique,
                                                   "conforme asymetrique",
                                                   "conforme symetrique"),
                 bas = bas, haut = haut, facteur_elargissement = facteur)
}

intervalles <- purrr::map_dfr(NIVEAUX, function(nv) {
  dplyr::bind_rows(recalibrer(pred, nv, FALSE), recalibrer(pred, nv, TRUE))
}) %>% dplyr::left_join(pred %>% dplyr::select(origine, reel, mediane),
                        by = "origine")
ecrire_csv(intervalles, chemin_res("intervalles_recalibres.csv"))

cat("      facteur d'elargissement median, par niveau :\n")
print(as.data.frame(intervalles %>% dplyr::group_by(niveau, methode) %>%
  dplyr::summarise(facteur = round(stats::median(facteur_elargissement, na.rm = TRUE), 2),
                   .groups = "drop")), row.names = FALSE)

# ============================================================================
# 3) LA COUVERTURE EST-ELLE CORRIGEE ?
# ============================================================================
cat("\n[3/4] Couverture\n")
# La couverture est evaluee hors echantillon par construction : le quantile de
# l'origine T ne connait que les scores anterieurs a T.
couv_brute <- purrr::map_dfr(NIVEAUX, function(nv) {
  a <- (1 - nv) / 2
  q_b <- switch(as.character(nv), "0.5" = pred$q25, "0.8" = pred$q10, "0.9" = pred$q05)
  q_h <- switch(as.character(nv), "0.5" = pred$q75, "0.8" = pred$q90, "0.9" = pred$q95)
  tibble::tibble(niveau = nv, methode = "predictive brute",
                 couverture = mean(pred$reel >= q_b & pred$reel <= q_h),
                 largeur = mean(q_h - q_b))
})
couv_recal <- intervalles %>%
  dplyr::group_by(niveau, methode) %>%
  dplyr::summarise(couverture = mean(reel >= bas & reel <= haut),
                   largeur = mean(haut - bas), .groups = "drop")
comparaison <- dplyr::bind_rows(couv_brute, couv_recal) %>%
  dplyr::mutate(ecart = couverture - niveau) %>%
  dplyr::arrange(niveau, methode)
ecrire_csv(comparaison, chemin_res("calibration_comparee.csv"))
print(as.data.frame(comparaison %>% dplyr::transmute(
  `niveau` = sprintf("%.0f %%", 100 * niveau), methode,
  `couverture` = sprintf("%.0f %%", 100 * couverture),
  `ecart` = sprintf("%+.0f pt", 100 * ecart),
  `largeur` = sprintf("%.2f pt", 100 * largeur))), row.names = FALSE)

# --- avant et apres 2020 -----------------------------------------------------
# L'echangeabilite des scores est precisement ce que 2020 viole. La
# recalibration ne peut pas ANTICIPER un evenement sans precedent ; elle ne
# fait que s'y adapter ensuite. Il faut donc regarder les deux periodes.
cat("\n      --- la recalibration peut-elle anticiper une rupture ? ---\n")
par_periode <- intervalles %>%
  dplyr::mutate(periode = ifelse(origine < as.Date("2020-01-01"),
                                 "avant 2020", "2020 et apres")) %>%
  dplyr::group_by(niveau, methode, periode) %>%
  dplyr::summarise(n = dplyr::n(), couverture = mean(reel >= bas & reel <= haut),
                   .groups = "drop") %>%
  dplyr::filter(niveau == 0.80)
print(as.data.frame(par_periode %>% dplyr::transmute(
  methode, periode, n, `couverture a 80 %` = sprintf("%.0f %%", 100 * couverture))),
  row.names = FALSE)

# ============================================================================
# 4) FIGURES
# ============================================================================
cat("\n[4/4] Figures\n")
g1 <- intervalles %>% dplyr::filter(niveau == 0.80) %>%
  ggplot2::ggplot(ggplot2::aes(origine)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = 100 * bas, ymax = 100 * haut),
                       fill = "grey78") +
  ggplot2::geom_line(ggplot2::aes(y = 100 * mediane), colour = "#1f4e79",
                     linewidth = 0.5) +
  ggplot2::geom_point(ggplot2::aes(y = 100 * reel), size = 1.1) +
  ggplot2::facet_wrap(~ methode) +
  ggplot2::labs(x = NULL, y = "croissance trimestrielle (%)",
                title = "Intervalles recalibres a 80 %",
                subtitle = "la largeur est fixee par les erreurs passees du modele, recursivement")
ggplot2::ggsave(chemin_fig("intervalles.png"), g1, width = 11, height = 4.5, dpi = 150)

g2 <- intervalles %>% dplyr::filter(niveau == 0.80) %>%
  ggplot2::ggplot(ggplot2::aes(origine, facteur_elargissement, colour = methode)) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  ggplot2::geom_step(linewidth = 0.55) +
  ggplot2::labs(x = NULL, y = "largeur / largeur gaussienne", colour = NULL,
                title = "De combien l'intervalle est-il elargi ?",
                subtitle = "le saut de 2021 est la reaction aux erreurs de 2020")
ggplot2::ggsave(chemin_fig("facteur.png"), g2, width = 9.5, height = 4.2, dpi = 150)

cat("      figures/12b_intervalles.png\n      figures/12b_facteur.png\n")
cat("\nRecalibration terminee.\n")
