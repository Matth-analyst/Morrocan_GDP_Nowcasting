# ============================================================================
# 11_sensibilite_delais.R -- Ce que coutent les delais de publication
# ============================================================================
# L'HYPOTHESE QUI TRAVERSE TOUT LE PROJET, ET QUI EST FAUSSE
#   La convention de datation du projet place chaque observation au DERNIER JOUR
#   de sa periode, et `information_set_intra()` admet une observation des que sa
#   date est atteinte. Le code suppose donc qu'un indicateur du mois d'avril est
#   connu le 30 avril.
#
#   En pratique un indicateur mensuel parait avec quatre a huit semaines de
#   delai. Les regles anti-look-ahead du plan portent sur les DATES
#   D'OBSERVATION, jamais sur les DATES DE PUBLICATION -- ce sont deux choses
#   differentes, et le plan ne traite que la premiere.
#
# DEUX RETARDS, DE NATURES DIFFERENTES
#
#   (1) LE RETARD DES INDICATEURS. Il ne demande AUCUN recalcul : c'est une pure
#       reetiquette. Si un indicateur du mois m parait fin m+L, alors a la fin du
#       mois m du trimestre on connait k = max(0, m - L) mois -- c'est-a-dire
#       exactement notre scenario M_k. Le tableau de la section 1 fait la
#       correspondance.
#
#   (2) LE RETARD DE LA VALEUR AJOUTEE. Celui-la est autrement plus gênant, et
#       il n'avait jamais ete souleve. Le BVAR et le terme autoregressif de la
#       passerelle utilisent la VA du trimestre T-1. Or les comptes trimestriels
#       paraissent eux aussi avec un delai : si ce delai depasse un trimestre, on
#       ne connait a la fin de T que la VA de T-2, et non celle de T-1.
#
#       Ce volet exige un vrai recalcul : l'ensemble d'information du BVAR
#       change. C'est l'objet principal de ce script.
#
# CE QUE CE SCRIPT NE PEUT PAS FAIRE
#   Etablir le VRAI calendrier. Le vivier ne documente aucune date de
#   publication, ni pour les indicateurs ni pour les comptes. On ne mesure donc
#   pas l'erreur reelle : on BORNE ce qu'elle coute sous des hypotheses de
#   retard explicites. C'est une analyse de sensibilite, pas une correction.
#
# SORTIES
#   resultats/11_calendrier_indicateurs.csv
#   resultats/11_retard_va.csv
#   figures/11_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/bvar.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("11_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("11_", x))

PREMIERE_CIBLE <- as.Date("2014-06-30")

# ============================================================================
# 1) LE RETARD DES INDICATEURS : une reetiquette du calendrier
# ============================================================================
cat("\n[1/4] Retard des indicateurs : correspondance calendaire\n")
intra <- lire_csv(file.path(DOSSIER_RESULTATS, "09_perimetre_commun.csv")) %>%
  dplyr::filter(periode == "toutes origines") %>%
  dplyr::select(scenario, modele, ratio_median)
perf <- function(sc, mo) {
  v <- intra$ratio_median[intra$scenario == sc & intra$modele == mo]
  if (length(v) == 1L) v else NA_real_
}

# Pour un retard L et une position calendaire m (mois ecoules depuis le debut du
# trimestre cible), le nombre de mois CONNUS vaut max(0, m - L).
calendrier <- purrr::map_dfr(0:2, function(L) {
  purrr::map_dfr(1:4, function(m) {
    k <- max(0L, m - L)
    if (k > 3L) return(NULL)
    sc <- paste0("M", k)
    tibble::tibble(
      retard_mois = L,
      position = if (m <= 3L) sprintf("fin du mois %d du trimestre", m)
                 else sprintf("%d mois apres la fin du trimestre", m - 3L),
      scenario_effectif = sc,
      ratio_combinaison = perf(sc, "combinee"),
      ratio_bvar = perf(sc, "bvar"))
  })
})
ecrire_csv(calendrier, chemin_res("calendrier_indicateurs.csv"))
for (L in 0:2) {
  cat(sprintf("\n      --- retard de %d mois ---\n", L))
  print(as.data.frame(calendrier %>% dplyr::filter(retard_mois == L) %>%
    dplyr::transmute(position, scenario = scenario_effectif,
                     combinaison = round(ratio_combinaison, 3),
                     BVAR = round(ratio_bvar, 3))), row.names = FALSE)
}
cat("\n      lecture : a la FIN DU TRIMESTRE (mois 3), le meilleur disponible est\n")
for (L in 0:2) {
  l <- calendrier %>% dplyr::filter(retard_mois == L, grepl("mois 3", position))
  cat(sprintf("        retard %d mois -> %s, ratio %.3f\n", L,
              l$scenario_effectif, l$ratio_combinaison))
}

# ============================================================================
# 2) LE RETARD DE LA VALEUR AJOUTEE : un vrai recalcul
# ============================================================================
cat("\n[2/4] Retard de la valeur ajoutee : re-estimation du BVAR\n")
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

#' Passe recursive avec un retard de `retard` trimestres sur la VA.
#'
#' Le mecanisme est simple : on avance artificiellement la "cible" vue par
#' `prevision_bvar_recursive`, qui n'utilise que les dates strictement
#' anterieures. Avec retard = 1, l'estimation s'arrete a T-2 au lieu de T-1,
#' mais la prevision reste evaluee contre le realise de T.
passe_retard <- function(retard) {
  purrr::map_dfr(seq_along(origines), function(i) {
    cible <- origines[i]
    borne <- cible
    if (retard > 0L) {
      borne <- fin_trimestre(debut_trimestre(cible) %m-% months(3L * retard))
    }
    ok <- which(dates_vec < borne)
    if (length(ok) < 40L) return(NULL)
    ch <- detecter_chocs(Y[ok, , drop = FALSE], dates_vec[ok], z = 4, k = 3)
    r <- prevision_bvar_recursive(
      Y, dates_vec, borne, p = 5L, lambda = 0.15, dates_choc = ch,
      fenetre = Inf, selection = "bgr", p_grille = 1:5,
      ref = c("Industrie de transformation", "Commerce", "Agriculture"),
      regle_choc = NULL, d_grille = c(0.5, 1, 1.5, 2),
      theta = 1, rho = 1, fenetre_sigma = 60)
    if (is.null(r)) return(NULL)
    # La prevision porte sur `borne` ; avec retard elle sert de prevision pour
    # `cible`, ce qui revient a prevoir a horizon retard+1. C'est exactement la
    # situation d'un praticien qui n'a pas encore les comptes de T-1.
    tibble::tibble(origine = cible, retard = retard,
                   branche = names(r$prevision),
                   prevision = as.numeric(r$prevision),
                   reel = as.numeric(Y[dates_vec == cible, ]),
                   derniere_obs = r$derniere_obs)
  })
}

resultats <- purrr::map_dfr(0:1, function(L) {
  t0 <- Sys.time()
  p <- passe_retard(L)
  cat(sprintf("      retard %d trimestre(s) : %d previsions | %s\n", L, nrow(p),
              format(round(difftime(Sys.time(), t0, units = "mins"), 2))))
  stopifnot("[ANTI-LOOK-AHEAD]" = all(p$derniere_obs < p$origine))
  p
})

poids <- lire_csv(file.path(DOSSIER_RESULTATS, "06_poids.csv")) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::transmute(branche,
                   origine = fin_trimestre(debut_trimestre(date) %m+% months(3)), w)

bilan <- resultats %>%
  dplyr::group_by(retard, branche) %>%
  dplyr::summarise(ratio = sqrt(mean((reel - prevision)^2)) / stats::sd(reel),
                   .groups = "drop") %>%
  dplyr::group_by(retard) %>%
  dplyr::summarise(ratio_median = stats::median(ratio),
                   n_ok = sum(ratio < 1), .groups = "drop")

agregat <- resultats %>%
  dplyr::inner_join(poids, by = c("branche", "origine")) %>%
  dplyr::group_by(retard, origine) %>%
  dplyr::filter(dplyr::n() == 16L) %>%
  dplyr::summarise(reel = log(sum(w * exp(reel))),
                   prev = log(sum(w * exp(prevision))), .groups = "drop") %>%
  dplyr::group_by(retard) %>%
  dplyr::summarise(n = dplyr::n(),
                   RMSFE = sqrt(mean((reel - prev)^2)),
                   ratio = sqrt(mean((reel - prev)^2)) / stats::sd(reel),
                   correlation = stats::cor(prev, reel), .groups = "drop")

tab <- bilan %>% dplyr::inner_join(agregat, by = "retard", suffix = c("_br", "_ag"))
ecrire_csv(tab, chemin_res("retard_va.csv"))
cat("\n      --- effet du retard de publication des comptes ---\n")
print(as.data.frame(tab %>% dplyr::transmute(
  `retard (trimestres)` = retard,
  `ratio median branches` = round(ratio_median, 3),
  `branches < 1` = n_ok,
  `RMSFE agregat (%)` = round(100 * RMSFE, 3),
  `ratio agregat` = round(ratio, 3),
  `correl. agregat` = round(correlation, 2))), row.names = FALSE)

# ============================================================================
# 3) LE CAS REALISTE
# ============================================================================
cat("\n[3/4] Le cas realiste\n")
r0 <- tab$ratio[tab$retard == 0]; r1 <- tab$ratio[tab$retard == 1]
c0 <- tab$correlation[tab$retard == 0]; c1 <- tab$correlation[tab$retard == 1]
cat(sprintf("      agregat sans retard  : ratio %.3f | correl %+.2f\n", r0, c0))
cat(sprintf("      agregat avec retard  : ratio %.3f | correl %+.2f\n", r1, c1))
cat(sprintf("      cout du retard       : %+.3f de ratio, %+.2f de correlation\n",
            r1 - r0, c1 - c0))
combi_m3 <- perf("M3", "combinee"); combi_m2 <- perf("M2", "combinee")
cat(sprintf("\n      cote indicateurs, a la fin du trimestre :\n"))
cat(sprintf("        sans retard  -> M3, ratio %.3f\n", combi_m3))
cat(sprintf("        retard 1 mois-> M2, ratio %.3f  (%+.3f)\n",
            combi_m2, combi_m2 - combi_m3))

# ============================================================================
# 4) FIGURES
# ============================================================================
cat("\n[4/4] Figures\n")
g1 <- calendrier %>%
  dplyr::mutate(retard = factor(sprintf("retard %d mois", retard_mois))) %>%
  ggplot2::ggplot(ggplot2::aes(position, ratio_combinaison, colour = retard,
                               group = retard)) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  ggplot2::geom_line(linewidth = 0.55) + ggplot2::geom_point(size = 2) +
  ggplot2::labs(x = NULL, y = "ratio de la combinaison", colour = NULL,
                title = "Ce que le nowcast vaut REELLEMENT a chaque date du calendrier",
                subtitle = "selon le retard de publication suppose des indicateurs") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
ggplot2::ggsave(chemin_fig("calendrier.png"), g1, width = 10, height = 4.5, dpi = 150)

g2 <- resultats %>%
  dplyr::inner_join(poids, by = c("branche", "origine")) %>%
  dplyr::group_by(retard, origine) %>% dplyr::filter(dplyr::n() == 16L) %>%
  dplyr::summarise(reel = log(sum(w * exp(reel))),
                   prev = log(sum(w * exp(prevision))), .groups = "drop") %>%
  dplyr::mutate(retard = ifelse(retard == 0, "comptes de T-1 connus",
                                "comptes de T-2 seulement")) %>%
  ggplot2::ggplot(ggplot2::aes(origine)) +
  ggplot2::geom_line(ggplot2::aes(y = 100 * reel), linewidth = 0.5) +
  ggplot2::geom_line(ggplot2::aes(y = 100 * prev), colour = "#b03a2e",
                     linewidth = 0.5) +
  ggplot2::facet_wrap(~ retard) +
  ggplot2::labs(x = NULL, y = "croissance trimestrielle (%)",
                title = "Le BVAR agrege, avec et sans retard sur les comptes",
                subtitle = "en noir le realise, en rouge la prevision")
ggplot2::ggsave(chemin_fig("retard_va.png"), g2, width = 11, height = 4.2, dpi = 150)

cat("      figures/11_calendrier.png\n      figures/11_retard_va.png\n")
cat("\nAnalyse de sensibilite terminee.\n")
