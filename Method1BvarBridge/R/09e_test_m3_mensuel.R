# ============================================================================
# Experience decisive sur l'anomalie M2 > M3
# ============================================================================
# CONSTAT
#   A perimetre commun, M3 est systematiquement moins bon que M2 : ratio de la
#   passerelle 0,975 contre 0,962, combinaison 0,944 contre 0,928. Ajouter le
#   troisieme mois OBSERVE ne devrait pas degrader : il remplace une valeur
#   extrapolee par une donnee.
#
# HYPOTHESE
#   Ce n'est pas le troisieme mois qui nuit, ce sont les indicateurs
#   TRIMESTRIELS, qui n'apparaissent qu'en M3 (convention de la phase 14 du
#   plan). Ils entrent dans la selection et evincent des indicateurs mensuels
#   plus informatifs. Indice a l'appui : M3 retient plus d'indicateurs que M2
#   (mediane 5 contre 4), et dans 70 des 72 cas ou la selection differe, c'est
#   M3 qui en ajoute.
#
# TEST
#   On rejoue M3 en RETIRANT les indicateurs trimestriels du trimestre cible,
#   tout en gardant les trois mois observes. Trois issues possibles :
#     - M3 mensuel >= M2  : l'hypothese est confirmee, ce sont bien les
#       indicateurs trimestriels qui degradent ;
#     - M3 mensuel ~ M3   : l'hypothese est refutee, la degradation vient
#       d'ailleurs ;
#     - M3 mensuel < M2   : quelque chose d'autre se joue, a chercher.
#
#   Aucune prevision de mois n'est necessaire ici -- les trois mois sont
#   observes -- donc le calcul est rapide.
# ============================================================================
source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")
source("R/fonctions/kalman.R")

SEUIL_R <- 0.15; SEUIL_P <- 0.10; MIN_OBS_SEL <- 20L; MAX_RETENUS <- 5L

couverture <- charger_couverture(); BC <- couverture$couvertes
ind  <- charger_indicateurs(branches = BC)
meta <- charger_metadonnees() %>% dplyr::select(id_serie, agregation, transformation)
longueur <- ind %>% dplyr::count(id_serie, name = "n_obs")
ind <- ind %>% dplyr::filter(id_serie %in% longueur$id_serie[longueur$n_obs >= 36L])

va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)
origines <- sort(unique(va$date[va$date >= as.Date("2014-06-30")]))

traiter_m3 <- function(cible, avec_trimestriels) {
  info <- information_set_intra(ind, cible, scenario = "M3")
  # LA SEULE DIFFERENCE ENTRE LES DEUX VARIANTES : l'indicateur trimestriel du
  # trimestre cible. Son historique reste dans les deux cas ; seule son
  # observation EN T est retiree quand avec_trimestriels vaut FALSE.
  if (!avec_trimestriels) {
    info <- info %>%
      dplyr::filter(!(frequence == "trimestriel" &
                        date >= debut_trimestre(cible) & date <= fin_trimestre(cible)))
  }

  base_long <- info %>%
    dplyr::select(id_serie, branche, indicateur, frequence, date, valeur) %>%
    dplyr::left_join(meta, by = "id_serie")
  trim <- indicateurs_trimestriels(base_long, tolerant = TRUE)
  d <- trim %>% dplyr::group_by(id_serie) %>%
    dplyr::summarise(n = sum(!is.na(x)), .groups = "drop")
  tok <- trim %>%
    dplyr::filter(id_serie %in% d$id_serie[d$n >= MIN_OBS_SEL], !is.na(x)) %>%
    dplyr::select(id_serie, branche, date, x)

  purrr::map_dfr(BC, function(b) {
    g_b <- va %>% dplyr::filter(branche == b) %>% dplyr::select(date, g)
    x_b <- tok %>% dplyr::filter(branche == b) %>% dplyr::select(id_serie, date, x)
    if (nrow(x_b) == 0L) return(NULL)
    x_l <- x_b %>% tidyr::pivot_wider(names_from = id_serie, values_from = x)
    s <- selectionner_disponible(g_b, x_b, x_l, cible, SEUIL_R, SEUIL_P,
                                 MIN_OBS_SEL, MAX_RETENUS)
    r <- if (!is.null(s)) estimer_bridge_ar(g_b, x_l, s$ids, cible, avec_ar = TRUE) else NULL
    reel <- g_b$g[g_b$date == cible]
    tibble::tibble(scenario = if (avec_trimestriels) "M3 avec trimestriels"
                              else "M3 mensuel seul",
                   branche = b, origine = cible,
                   n_retenus = if (is.null(s)) 0L else length(s$ids),
                   bridge = if (is.null(r)) NA_real_ else r$prevision,
                   derniere_obs = if (is.null(r)) as.Date(NA) else r$derniere_obs,
                   reel = if (length(reel) == 1L) reel else NA_real_)
  })
}

# Les DEUX variantes sont recalculees ici, et non lues ailleurs : la valeur de
# INCLURE_TRIMESTRIELS_M3 dans R/09 ne doit pas decider de ce que ce test peut
# montrer. Sans cela, une fois la correction adoptee en amont, le tableau ne
# documente plus l'anomalie qui l'a justifiee.
cat("execution de M3 SANS indicateurs trimestriels...\n")
m3m <- purrr::map_dfr(seq_along(origines),
                      function(i) traiter_m3(origines[i], avec_trimestriels = FALSE))
cat("execution de M3 AVEC indicateurs trimestriels...\n")
m3a <- purrr::map_dfr(seq_along(origines),
                      function(i) traiter_m3(origines[i], avec_trimestriels = TRUE))
m3m <- dplyr::bind_rows(m3m, m3a)
stopifnot(all(m3m$derniere_obs < m3m$origine, na.rm = TRUE))
cat("controle anti-look-ahead : OK |", sum(!is.na(m3m$bridge)), "previsions\n")

ancien <- lire_csv(file.path(DOSSIER_RESULTATS, "09_previsions_intra.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(scenario, branche, origine, n_retenus, bridge, reel)
tous <- dplyr::bind_rows(ancien, m3m %>% dplyr::select(-derniere_obs))

# perimetre commun a TOUS les scenarios
n_sc <- dplyr::n_distinct(tous$scenario)
commun <- tous %>% dplyr::filter(!is.na(bridge)) %>%
  dplyr::count(branche, origine) %>% dplyr::filter(n == n_sc) %>%
  dplyr::select(branche, origine)
cat(sprintf("perimetre commun aux %d scenarios : %d couples\n", n_sc, nrow(commun)))

bilan <- tous %>% dplyr::inner_join(commun, by = c("branche", "origine")) %>%
  dplyr::group_by(scenario, branche) %>%
  dplyr::filter(dplyr::n() >= 4L) %>%
  dplyr::summarise(ratio = sqrt(mean((reel - bridge)^2)) / stats::sd(reel),
                   n_ret = mean(n_retenus), .groups = "drop") %>%
  dplyr::group_by(scenario) %>%
  dplyr::summarise(branches = dplyr::n(),
                   ratio_median = round(stats::median(ratio), 3),
                   br_ok = sum(ratio < 1),
                   n_retenus_moyen = round(mean(n_ret), 2), .groups = "drop") %>%
  dplyr::arrange(ratio_median)
cat("\n=== PASSERELLE SEULE, perimetre commun a tous les scenarios ===\n")
print(as.data.frame(bilan))
ecrire_csv(bilan, file.path(DOSSIER_RESULTATS, "09_test_m3_mensuel.csv"))

cat("\n=== par branche : M2, M3, M3 mensuel seul ===\n")
pb <- tous %>% dplyr::inner_join(commun, by = c("branche", "origine")) %>%
  dplyr::filter(scenario %in% c("M2", "M3 avec trimestriels", "M3 mensuel seul")) %>%
  dplyr::group_by(branche, scenario) %>%
  dplyr::summarise(ratio = round(sqrt(mean((reel - bridge)^2)) / stats::sd(reel), 3),
                   .groups = "drop") %>%
  tidyr::pivot_wider(names_from = scenario, values_from = ratio)
print(as.data.frame(pb %>%
  dplyr::arrange(dplyr::desc(`M3 avec trimestriels` - M2))))
