# ============================================================================
# 12_incertitude_parametrique.R -- Distribution predictive et calibration
# ============================================================================
# CE QUE CE SCRIPT LEVE
#   La limite "le BVAR ne fournit pas d'incertitude parametrique", ouverte
#   depuis la phase 3 et reportee dans les trois rapports. Le modele ne donnait
#   qu'une estimation ponctuelle ; le nowcast etait annonce avec, pour toute
#   mesure d'incertitude, le RMSFE du backtest -- une moyenne retrospective sur
#   48 trimestres, identique pour un trimestre calme et pour un trimestre ou le
#   modele est en terrain inconnu.
#
# CE QU'IL PRODUIT
#   (1) la distribution predictive de chaque branche et de l'agregat, a chaque
#       origine du backtest ;
#   (2) une VERIFICATION DE CALIBRATION : le realise tombe-t-il dans
#       l'intervalle a 80 % dans 80 % des cas ? C'est le seul test qui dise si
#       l'intervalle vaut quelque chose ;
#   (3) la decomposition de l'incertitude entre part parametrique et alea de
#       choc, qui dit s'il y a quelque chose a gagner a mieux estimer ;
#   (4) l'intervalle autour du nowcast courant.
#
# CE QUE L'INTERVALLE NE COUVRE PAS, ET IL FAUT S'Y ATTENDRE
#   L'incertitude sur les HYPERPARAMETRES : p, lambda, d, la regle de detection
#   des chocs et la fenetre sigma sont choisis a chaque origine puis traites
#   comme connus. L'intervalle calcule ici est donc une BORNE BASSE, et la
#   verification de calibration doit le montrer -- une couverture inferieure a
#   80 % serait le symptome attendu, pas une surprise.
#
#   Il ne couvre pas non plus la passerelle : la loi a posteriori est celle du
#   BVAR. L'intervalle publie porte donc sur la composante BVAR de l'agregat.
#
# SORTIES
#   resultats/12_predictive_agregat.csv
#   resultats/12_calibration.csv
#   resultats/12_decomposition.csv
#   resultats/12_nowcast_intervalle.csv
#   figures/12_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/bvar.R")
source("R/fonctions/posterior.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("12_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("12_", x))

set.seed(20260914)
N_TIRAGES      <- 4000L
PREMIERE_CIBLE <- as.Date("2014-06-30")
NIVEAUX        <- c(0.50, 0.80, 0.90)

cat("\n[1/5] Bases\n")
va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)
large <- va %>% tidyr::pivot_wider(names_from = branche, values_from = g) %>%
  dplyr::arrange(date) %>%
  dplyr::filter(dplyr::if_all(dplyr::all_of(TOUTES_BRANCHES), ~ !is.na(.)))
dates_vec <- large$date
Y <- as.matrix(large[, TOUTES_BRANCHES])
origines <- dates_vec[dates_vec >= PREMIERE_CIBLE]

poids <- lire_csv(file.path(DOSSIER_RESULTATS, "06_poids.csv")) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::transmute(branche,
                   origine = fin_trimestre(debut_trimestre(date) %m+% months(3)), w)
hyper <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_recursives.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::distinct(origine, p, lambda, d)
cat(sprintf("      %d origines | %d tirages par origine\n", length(origines), N_TIRAGES))

# ============================================================================
# 2) TIRAGES A CHAQUE ORIGINE
# ============================================================================
cat("\n[2/5] Tirages\n")

#' Reconstruit le vecteur de regresseurs de la cible, dans l'ordre des colonnes
#' du modele : constante, indicatrices de choc (nulles en prevision), retards.
vecteur_cible <- function(modele, Y_hist, exo_largeur) {
  p <- modele$p
  n_hist <- nrow(Y_hist)
  retards <- as.numeric(t(Y_hist[n_hist:(n_hist - p + 1), , drop = FALSE]))
  c(1, rep(0, exo_largeur), retards)
}

t0 <- Sys.time()
resultats <- purrr::map_dfr(seq_along(origines), function(i) {
  cible <- origines[i]
  ok <- which(dates_vec < cible)
  Y_h <- Y[ok, , drop = FALSE]
  h <- hyper %>% dplyr::filter(origine == cible)
  if (nrow(h) != 1L) return(NULL)

  chocs <- detecter_chocs(Y_h, dates_vec[ok], z = 4, k = 3)
  exo <- construire_indicatrices(dates_vec[ok], chocs)
  exo <- filtrer_indicatrices_utiles(exo, h$p)
  sig <- echelles_variables(utils::tail(Y_h, 60), h$p, utils::tail(exo, 60))

  m <- tryCatch(estimer_bvar(Y_h, p = h$p, lambda = h$lambda, exo = exo,
                             sigma = sig, d = h$d),
                error = function(e) NULL)
  if (is.null(m) || is.null(m$posterior)) return(NULL)

  x_f <- vecteur_cible(m, Y_h, if (is.null(exo)) 0L else ncol(exo))
  if (length(x_f) != nrow(m$B)) return(NULL)

  tir  <- tirer_predictive(m, x_f, N_TIRAGES, avec_choc = TRUE)
  tirp <- tirer_predictive(m, x_f, N_TIRAGES, avec_choc = FALSE)

  w <- poids %>% dplyr::filter(origine == cible)
  if (nrow(w) != ncol(Y)) return(NULL)
  ag  <- agreger_tirages(tir,  w)
  agp <- agreger_tirages(tirp, w)
  reel_ag <- log(sum(w$w[match(colnames(Y), w$branche)] *
                       exp(Y[dates_vec == cible, ])))

  tibble::tibble(
    origine = cible, trimestre = date_vers_trimestre(cible),
    reel = reel_ag, mediane = stats::median(ag),
    sd_predictive = stats::sd(ag), sd_parametrique = stats::sd(agp),
    q05 = stats::quantile(ag, 0.05), q10 = stats::quantile(ag, 0.10),
    q25 = stats::quantile(ag, 0.25), q75 = stats::quantile(ag, 0.75),
    q90 = stats::quantile(ag, 0.90), q95 = stats::quantile(ag, 0.95))
})
cat(sprintf("      %s | %d origines traitees\n",
            format(round(difftime(Sys.time(), t0, units = "mins"), 2)),
            nrow(resultats)))
ecrire_csv(resultats, chemin_res("predictive_agregat.csv"))

# ============================================================================
# 3) CALIBRATION
# ============================================================================
cat("\n[3/5] Calibration\n")
# Un intervalle a 80 % qui ne contient le realise que 55 % du temps est un
# intervalle faux. C'est le seul test qui dise si ces bornes valent quelque
# chose -- et il porte sur l'ensemble du dispositif, pas seulement sur la loi a
# posteriori.
calibration <- tibble::tibble(
  niveau = c(0.50, 0.80, 0.90),
  couverture = c(
    mean(resultats$reel >= resultats$q25 & resultats$reel <= resultats$q75),
    mean(resultats$reel >= resultats$q10 & resultats$reel <= resultats$q90),
    mean(resultats$reel >= resultats$q05 & resultats$reel <= resultats$q95)),
  n = nrow(resultats)) %>%
  dplyr::mutate(ecart = couverture - niveau,
                largeur_moyenne = c(
                  mean(resultats$q75 - resultats$q25),
                  mean(resultats$q90 - resultats$q10),
                  mean(resultats$q95 - resultats$q05)))
ecrire_csv(calibration, chemin_res("calibration.csv"))
print(as.data.frame(calibration %>% dplyr::transmute(
  `niveau nominal` = sprintf("%.0f %%", 100 * niveau),
  `couverture reelle` = sprintf("%.0f %%", 100 * couverture),
  `ecart` = sprintf("%+.0f pt", 100 * ecart),
  `largeur moyenne` = sprintf("%.2f pt", 100 * largeur_moyenne))),
  row.names = FALSE)

hors <- resultats %>% dplyr::filter(reel < q10 | reel > q90) %>%
  dplyr::arrange(dplyr::desc(abs(reel - mediane)))
cat(sprintf("\n      %d trimestres hors de l'intervalle a 80 %% :\n", nrow(hors)))
print(as.data.frame(hors %>% utils::head(6) %>% dplyr::transmute(
  trimestre, realise = round(100 * reel, 2), mediane = round(100 * mediane, 2),
  bas = round(100 * q10, 2), haut = round(100 * q90, 2))), row.names = FALSE)

# ============================================================================
# 4) DECOMPOSITION DE L'INCERTITUDE
# ============================================================================
cat("\n[4/5] D'ou vient l'incertitude\n")
decomp <- resultats %>%
  dplyr::summarise(
    sd_predictive = mean(sd_predictive),
    sd_parametrique = mean(sd_parametrique),
    part_parametrique = mean(sd_parametrique^2 / sd_predictive^2))
ecrire_csv(decomp, chemin_res("decomposition.csv"))
cat(sprintf("      ecart-type predictif moyen   : %.2f point\n", 100 * decomp$sd_predictive))
cat(sprintf("      dont part parametrique       : %.2f point (%.0f %% de la variance)\n",
            100 * decomp$sd_parametrique, 100 * decomp$part_parametrique))
cat(sprintf("      le reste est l'alea de choc  : %.0f %% de la variance\n",
            100 * (1 - decomp$part_parametrique)))
cat("\n      lecture : si l'alea domine, mieux estimer ne resserrera pas l'intervalle.\n")

# ============================================================================
# 5) LE NOWCAST COURANT
# ============================================================================
cat("\n[5/5] Intervalle autour du nowcast courant\n")
derniere <- max(dates_vec)
CIBLE <- fin_trimestre(debut_trimestre(derniere) %m+% months(3))
ok <- which(dates_vec <= derniere)
Y_h <- Y[ok, , drop = FALSE]
h <- hyper %>% dplyr::slice_max(origine, n = 1)

chocs <- detecter_chocs(Y_h, dates_vec[ok], z = 4, k = 3)
exo <- filtrer_indicatrices_utiles(construire_indicatrices(dates_vec[ok], chocs), h$p)
sig <- echelles_variables(utils::tail(Y_h, 60), h$p, utils::tail(exo, 60))
m <- estimer_bvar(Y_h, p = h$p, lambda = h$lambda, exo = exo, sigma = sig, d = h$d)
x_f <- vecteur_cible(m, Y_h, if (is.null(exo)) 0L else ncol(exo))
tir <- tirer_predictive(m, x_f, N_TIRAGES)

w_courant <- poids %>% dplyr::filter(origine == CIBLE)
if (nrow(w_courant) == ncol(Y)) {
  ag <- agreger_tirages(tir, w_courant)
  interv <- tibble::tibble(
    trimestre = date_vers_trimestre(CIBLE),
    composante = "BVAR agrege",
    mediane = 100 * stats::median(ag),
    q10 = 100 * stats::quantile(ag, 0.10), q90 = 100 * stats::quantile(ag, 0.90),
    q05 = 100 * stats::quantile(ag, 0.05), q95 = 100 * stats::quantile(ag, 0.95))
  ecrire_csv(interv, chemin_res("nowcast_intervalle.csv"))
  cat(sprintf("      %s, composante BVAR : %+.2f %%\n",
              interv$trimestre, interv$mediane))
  cat(sprintf("        intervalle a 80 %% : [%+.2f ; %+.2f]\n", interv$q10, interv$q90))
  cat(sprintf("        intervalle a 90 %% : [%+.2f ; %+.2f]\n", interv$q05, interv$q95))
  cat("\n      NB : la loi a posteriori est celle du BVAR. L'intervalle ne couvre\n")
  cat("      donc pas la passerelle, dont l'incertitude n'est pas modelisee ici.\n")
} else {
  cat("      poids indisponibles pour la cible courante.\n")
}

# --- figures -----------------------------------------------------------------
g1 <- resultats %>%
  ggplot2::ggplot(ggplot2::aes(origine)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = 100 * q05, ymax = 100 * q95),
                       fill = "grey85") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = 100 * q10, ymax = 100 * q90),
                       fill = "grey70") +
  ggplot2::geom_line(ggplot2::aes(y = 100 * mediane), colour = "#1f4e79",
                     linewidth = 0.5) +
  ggplot2::geom_point(ggplot2::aes(y = 100 * reel), size = 1.1) +
  ggplot2::labs(x = NULL, y = "croissance trimestrielle (%)",
                title = "Distribution predictive de l'agregat",
                subtitle = "bandes a 80 % et 90 %, mediane en bleu, realise en points")
ggplot2::ggsave(chemin_fig("predictive.png"), g1, width = 10, height = 4.5, dpi = 150)

g2 <- resultats %>%
  dplyr::mutate(z = (reel - mediane) / sd_predictive) %>%
  ggplot2::ggplot(ggplot2::aes(sample = z)) +
  ggplot2::stat_qq(size = 1.2) + ggplot2::stat_qq_line(linetype = "dashed") +
  ggplot2::labs(x = "quantiles theoriques", y = "erreur standardisee",
                title = "Les erreurs sont-elles de la taille annoncee ?",
                subtitle = "si l'intervalle est bien calibre, les points suivent la diagonale")
ggplot2::ggsave(chemin_fig("calibration.png"), g2, width = 6.5, height = 4.5, dpi = 150)

cat("\n      figures/12_predictive.png\n      figures/12_calibration.png\n")
cat("\nIncertitude parametrique terminee.\n")
