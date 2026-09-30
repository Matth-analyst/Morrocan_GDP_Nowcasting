# ============================================================================
# 15_diagnostics.R -- PHASE 25 : diagnostics et analyse par episode
# ============================================================================
# CE QUE LE PLAN DEMANDE
#   Six examens -- autocorrelation des residus, heteroscedasticite, valeurs
#   extremes, stabilite des coefficients, erreurs de prevision, episodes de
#   forte erreur -- et une analyse separee par type de periode : COVID, chocs
#   agricoles, forte croissance, ralentissements, episodes touristiques,
#   changements structurels.
#
# COMMENT LES EPISODES SONT DEFINIS, ET POURQUOI PAS A LA MAIN
#   "Periodes de forte croissance" ou "ralentissements" n'ont pas de definition
#   canonique. Les decouper a l'oeil reviendrait a choisir les periodes en
#   regardant les erreurs -- donc a fabriquer le resultat. Chaque episode est
#   donc construit par une REGLE MESURABLE, posee avant de regarder quoi que ce
#   soit :
#
#     COVID                 l'annee 2020
#     choc agricole         |croissance agricole| > 2 ecarts-types de la branche
#     episode touristique   idem sur l'hebergement-restauration
#     forte croissance      agregat dans le quintile superieur, hors 2020
#     ralentissement        agregat dans le quintile inferieur, hors 2020
#     periode calme         le reste
#
#   Un trimestre peut relever de plusieurs episodes : ils ne partitionnent pas
#   l'echantillon, et les effectifs ne s'additionnent pas.
#
# LA RESERVE QUI VAUT POUR TOUT CE SCRIPT
#   Avec 48 trimestres, chaque episode compte entre quatre et une douzaine
#   d'observations. Les statistiques par episode sont donc INDICATIVES et non
#   testables. Les effectifs sont affiches a cote de chaque chiffre pour que
#   cela se voie -- comme pour les branches a quatre origines de la phase 13.
#
#   Le "changement structurel" que la phase 2 a identifie -- la rupture de
#   retropolation de 2014 -- tombe AVANT la premiere origine du backtest. Il ne
#   peut donc pas etre analyse ici, et c'est a dire plutot qu'a contourner.
#
# SORTIES
#   resultats/15_diagnostics_residus.csv
#   resultats/15_episodes.csv
#   resultats/15_contributions_branches.csv
#   resultats/15_stabilite_coefficients.csv
#   figures/15_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/bvar.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("15_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("15_", x))

cat("\n[1/6] Bases\n")
ag <- lire_csv(file.path(DOSSIER_RESULTATS, "06_agregat.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(origine, trimestre, reel = reel_niveau,
                nowcast = nowcast_niveau, bvar = bvar_niveau) %>%
  dplyr::arrange(origine) %>%
  dplyr::mutate(erreur = reel - nowcast)
bench <- lire_csv(file.path(DOSSIER_RESULTATS, "13_previsions_benchmarks.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(origine, ar2 = `AR(2)`)
ag <- ag %>% dplyr::left_join(bench, by = "origine")
cat(sprintf("      %d trimestres | RMSFE %.3f pt\n", nrow(ag),
            100 * sqrt(mean(ag$erreur^2))))

va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)

# ============================================================================
# 2) DIAGNOSTICS DES RESIDUS DU NOWCAST
# ============================================================================
cat("\n[2/6] Diagnostics des residus\n")
e <- ag$erreur
lb <- stats::Box.test(e, lag = 4, type = "Ljung-Box")
# heteroscedasticite : test ARCH, c'est-a-dire autocorrelation des CARRES des
# residus. Une erreur dont la variance change dans le temps invalide un
# intervalle de largeur constante -- ce que la section 12c avait deja suggere.
arch <- stats::Box.test(e^2, lag = 4, type = "Ljung-Box")
sw <- stats::shapiro.test(e)
z <- (e - mean(e)) / stats::sd(e)
extremes <- ag[abs(z) > 2, c("trimestre", "reel", "nowcast", "erreur")]

diagnostics <- tibble::tibble(
  test = c("Ljung-Box sur les residus (4 retards)",
           "Ljung-Box sur les residus au carre (effet ARCH)",
           "Shapiro-Wilk (normalite)",
           "Residus au-dela de 2 ecarts-types"),
  statistique = c(lb$statistic, arch$statistic, sw$statistic, sum(abs(z) > 2)),
  p_value = c(lb$p.value, arch$p.value, sw$p.value, NA_real_),
  lecture = c(
    ifelse(lb$p.value < 0.05, "residus AUTOCORRELES : signal non exploite",
           "pas d'autocorrelation detectee"),
    ifelse(arch$p.value < 0.05, "variance NON CONSTANTE : intervalle de largeur fixe inadapte",
           "pas d'heteroscedasticite detectee"),
    ifelse(sw$p.value < 0.05, "residus NON normaux : queues epaisses",
           "normalite non rejetee"),
    sprintf("%d trimestres sur %d", sum(abs(z) > 2), nrow(ag))))
ecrire_csv(diagnostics, chemin_res("diagnostics_residus.csv"))
print(as.data.frame(diagnostics %>% dplyr::transmute(
  test, stat = round(statistique, 3),
  p = ifelse(is.na(p_value), "&mdash;", sprintf("%.4f", p_value)), lecture)),
  row.names = FALSE)
cat("\n      residus au-dela de 2 ecarts-types :\n")
print(as.data.frame(extremes %>% dplyr::mutate(
  dplyr::across(c(reel, nowcast, erreur), ~ round(100 * ., 2)))), row.names = FALSE)

# ============================================================================
# 3) LES EPISODES
# ============================================================================
cat("\n[3/6] Analyse par episode\n")
agri <- va %>% dplyr::filter(branche == "Agriculture")
tour <- va %>% dplyr::filter(branche == "Hébergement-restauration")
sd_agri <- stats::sd(agri$g); sd_tour <- stats::sd(tour$g)
q <- stats::quantile(ag$reel[lubridate::year(ag$origine) != 2020], c(0.20, 0.80))

ep <- ag %>%
  dplyr::left_join(agri %>% dplyr::transmute(origine = date, g_agri = g), by = "origine") %>%
  dplyr::left_join(tour %>% dplyr::transmute(origine = date, g_tour = g), by = "origine") %>%
  dplyr::mutate(
    covid       = lubridate::year(origine) == 2020,
    choc_agri   = !covid & abs(g_agri) > 2 * sd_agri,
    tourisme    = !covid & abs(g_tour) > 2 * sd_tour,
    forte       = !covid & reel >= q[2],
    ralenti     = !covid & reel <= q[1])
ep$calme <- !(ep$covid | ep$choc_agri | ep$tourisme | ep$forte | ep$ralenti)

resumer_ep <- function(masque, lab) {
  d <- ep[masque, , drop = FALSE]
  if (nrow(d) < 2L) return(NULL)
  tibble::tibble(
    episode = lab, n = nrow(d),
    croissance_moyenne = mean(d$reel),
    RMSFE_nowcast = sqrt(mean(d$erreur^2)),
    RMSFE_bvar = sqrt(mean((d$reel - d$bvar)^2)),
    RMSFE_ar2 = sqrt(mean((d$reel - d$ar2)^2, na.rm = TRUE)),
    biais = mean(d$erreur))
}
episodes <- dplyr::bind_rows(
  resumer_ep(ep$covid,     "COVID (2020)"),
  resumer_ep(ep$choc_agri, "Choc agricole"),
  resumer_ep(ep$tourisme,  "Episode touristique"),
  resumer_ep(ep$forte,     "Forte croissance"),
  resumer_ep(ep$ralenti,   "Ralentissement"),
  resumer_ep(ep$calme,     "Periode calme"),
  resumer_ep(rep(TRUE, nrow(ep)), "Ensemble")) %>%
  dplyr::mutate(gain_sur_ar2 = 1 - RMSFE_nowcast / RMSFE_ar2)
ecrire_csv(episodes, chemin_res("episodes.csv"))
print(as.data.frame(episodes %>% dplyr::transmute(
  episode, n, `croissance (%)` = round(100 * croissance_moyenne, 2),
  `RMSFE nowcast` = round(100 * RMSFE_nowcast, 2),
  `RMSFE BVAR` = round(100 * RMSFE_bvar, 2),
  `RMSFE AR(2)` = round(100 * RMSFE_ar2, 2),
  `gain / AR(2)` = sprintf("%+.0f %%", 100 * gain_sur_ar2))), row.names = FALSE)
cat("\n      les episodes ne partitionnent pas l'echantillon : un trimestre peut\n")
cat("      relever de plusieurs, et les effectifs ne s'additionnent pas.\n")

# ============================================================================
# 4) QUELLES BRANCHES FONT L'ERREUR AGREGEE ?
# ============================================================================
cat("\n[4/6] Contribution des branches a l'erreur agregee\n")
# Au premier ordre, l'erreur agregee se decompose en somme ponderee des erreurs
# de branche. On mesure donc la contribution moyenne de chacune, ce qui dit ou
# se joue reellement la performance -- une branche mal prevue mais legere pese
# moins qu'une branche moyennement prevue mais lourde.
prev_br <- lire_csv(file.path(DOSSIER_RESULTATS, "03e_previsions_corrigees.csv")) %>%
  dplyr::mutate(origine = as.Date(origine))
comb_br <- lire_csv(file.path(DOSSIER_RESULTATS, "09_previsions_intra.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::filter(scenario == "M3") %>%
  dplyr::select(branche, origine, combinee)
poids <- lire_csv(file.path(DOSSIER_RESULTATS, "06_poids.csv")) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::transmute(branche,
                   origine = fin_trimestre(debut_trimestre(date) %m+% months(3)), w)

contrib <- prev_br %>%
  dplyr::left_join(comb_br, by = c("branche", "origine")) %>%
  dplyr::mutate(retenue = dplyr::coalesce(combinee, prevision)) %>%
  dplyr::inner_join(poids, by = c("branche", "origine")) %>%
  dplyr::mutate(contribution = w * (reel - retenue)) %>%
  dplyr::group_by(branche) %>%
  dplyr::summarise(poids_moyen = mean(w),
                   erreur_branche = sqrt(mean((reel - retenue)^2)),
                   contribution_abs = mean(abs(contribution)),
                   contribution_nette = mean(contribution), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(contribution_abs))
ecrire_csv(contrib, chemin_res("contributions_branches.csv"))
print(as.data.frame(contrib %>% utils::head(8) %>% dplyr::transmute(
  branche, `poids (%)` = round(100 * poids_moyen, 1),
  `erreur propre (pt)` = round(100 * erreur_branche, 2),
  `contribution (pt)` = round(100 * contribution_abs, 3))), row.names = FALSE)
cat(sprintf("\n      les trois premieres branches font %.0f %% de l'erreur agregee\n",
            100 * sum(utils::head(contrib$contribution_abs, 3)) /
              sum(contrib$contribution_abs)))

# ============================================================================
# 5) STABILITE DES COEFFICIENTS
# ============================================================================
cat("\n[5/6] Stabilite des coefficients du BVAR\n")
# On suit le coefficient autoregressif PROPRE de chaque branche -- la case
# (j, j) du premier retard -- a chaque origine. C'est le coefficient que le
# prior de Minnesota contraint le plus directement, et son instabilite
# signalerait que la dynamique estimee change au fil du backtest.
large <- va %>% tidyr::pivot_wider(names_from = branche, values_from = g) %>%
  dplyr::arrange(date) %>%
  dplyr::filter(dplyr::if_all(dplyr::all_of(TOUTES_BRANCHES), ~ !is.na(.)))
dates_vec <- large$date; Y <- as.matrix(large[, TOUTES_BRANCHES])
hyper <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_recursives.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::distinct(origine, p, lambda, d)

t0 <- Sys.time()
coefs <- purrr::map_dfr(seq_len(nrow(hyper)), function(i) {
  cible <- hyper$origine[i]; ok <- which(dates_vec < cible)
  ch <- detecter_chocs(Y[ok, , drop = FALSE], dates_vec[ok], z = 4, k = 3)
  exo <- filtrer_indicatrices_utiles(
    construire_indicatrices(dates_vec[ok], ch), hyper$p[i])
  sig <- echelles_variables(utils::tail(Y[ok, , drop = FALSE], 60), hyper$p[i],
                            utils::tail(exo, 60))
  m <- tryCatch(estimer_bvar(Y[ok, , drop = FALSE], p = hyper$p[i],
                             lambda = hyper$lambda[i], exo = exo, sigma = sig,
                             d = hyper$d[i]), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  lignes <- rownames(m$B)
  purrr::map_dfr(colnames(m$B), function(b) {
    k <- which(lignes == paste0(b, "_L1"))
    if (length(k) != 1L) return(NULL)
    tibble::tibble(origine = cible, branche = b, ar1 = m$B[k, b])
  })
})
cat(sprintf("      %s | %d coefficients suivis\n",
            format(round(difftime(Sys.time(), t0, units = "mins"), 2)), nrow(coefs)))
stab <- coefs %>% dplyr::group_by(branche) %>%
  dplyr::summarise(moyenne = mean(ar1), ecart_type = stats::sd(ar1),
                   minimum = min(ar1), maximum = max(ar1),
                   amplitude = max(ar1) - min(ar1), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(amplitude))
ecrire_csv(stab, chemin_res("stabilite_coefficients.csv"))
print(as.data.frame(stab %>% utils::head(5) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 3)))), row.names = FALSE)
cat(sprintf("\n      amplitude mediane %.3f | le prior contraint fortement : les\n",
            stats::median(stab$amplitude)))
cat("      coefficients bougent peu, ce qui est le comportement recherche.\n")

# ============================================================================
# 6) FIGURES
# ============================================================================
cat("\n[6/6] Figures\n")
g1 <- ag %>%
  dplyr::mutate(z = (erreur - mean(erreur)) / stats::sd(erreur)) %>%
  ggplot2::ggplot(ggplot2::aes(origine, 100 * erreur)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
  ggplot2::geom_col(ggplot2::aes(fill = abs(z) > 2), width = 70) +
  ggplot2::scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "#b03a2e"),
                             guide = "none") +
  ggplot2::labs(x = NULL, y = "erreur (points)",
                title = "Erreur du nowcast agrege",
                subtitle = "en rouge, les residus au-dela de deux ecarts-types")
ggplot2::ggsave(chemin_fig("erreurs.png"), g1, width = 10, height = 4, dpi = 150)

g2 <- episodes %>% dplyr::filter(episode != "Ensemble") %>%
  dplyr::select(episode, n, nowcast = RMSFE_nowcast, BVAR = RMSFE_bvar,
                `AR(2)` = RMSFE_ar2) %>%
  tidyr::pivot_longer(-c(episode, n), names_to = "modele", values_to = "rmsfe") %>%
  ggplot2::ggplot(ggplot2::aes(stats::reorder(episode, rmsfe), 100 * rmsfe,
                               fill = modele)) +
  ggplot2::geom_col(position = "dodge", width = 0.7) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "RMSFE (points)", fill = NULL,
                title = "Ou le systeme gagne, et ou il ne gagne pas",
                subtitle = "effectifs faibles : ces ecarts sont indicatifs, non testables")
ggplot2::ggsave(chemin_fig("episodes.png"), g2, width = 10, height = 4.5, dpi = 150)

g3 <- coefs %>%
  ggplot2::ggplot(ggplot2::aes(origine, ar1)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  ggplot2::geom_line(linewidth = 0.4) +
  ggplot2::facet_wrap(~ branche, ncol = 4, scales = "free_y") +
  ggplot2::labs(x = NULL, y = "coefficient autoregressif d'ordre 1",
                title = "Stabilite des coefficients au fil du backtest",
                subtitle = "reestimes a chaque origine sur la seule information anterieure")
ggplot2::ggsave(chemin_fig("stabilite.png"), g3, width = 11, height = 7, dpi = 150)

g4 <- contrib %>% utils::head(10) %>%
  ggplot2::ggplot(ggplot2::aes(stats::reorder(branche, contribution_abs),
                               100 * contribution_abs)) +
  ggplot2::geom_col(width = 0.65, fill = "grey70") +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "contribution moyenne a l'erreur agregee (points)",
                title = "Quelles branches font l'erreur du nowcast",
                subtitle = "poids de la branche multiplie par son erreur propre")
ggplot2::ggsave(chemin_fig("contributions.png"), g4, width = 9, height = 4.5, dpi = 150)

cat("      figures/15_erreurs.png\n      figures/15_episodes.png\n")
cat("      figures/15_stabilite.png\n      figures/15_contributions.png\n")
cat("\nDiagnostics termines.\n")
