# ============================================================================
# Seuil de correlation renforce pour les indicateurs TRIMESTRIELS en M3
# ============================================================================
# CE QUI EST ETABLI
#   En M3, les indicateurs trimestriels du trimestre cible deviennent
#   disponibles (convention de la phase 14 du plan). Ils entrent alors dans la
#   selection, occupent des places sous le plafond de cinq regresseurs, et
#   evincent des indicateurs mensuels plus informatifs. Mesure :
#
#     M3 avec trimestriels      ratio 0,975   3,13 indicateurs retenus
#     M3 sans trimestriels      ratio 0,934   2,88 indicateurs retenus
#
#   Les retirer purement et simplement ameliore donc la prevision. Mais c'est
#   une solution brutale : elle jette aussi les indicateurs trimestriels qui
#   seraient reellement informatifs.
#
# CE QUE CE SCRIPT TESTE
#   Un seuil de correlation DIFFERENCIE : |r| >= 0,15 pour un indicateur issu
#   de series mensuelles, |r| >= s pour un indicateur nativement trimestriel.
#   On balaye s, avec deux bornes qui servent de controle :
#
#     s = 0,15   identique au seuil mensuel  -> doit redonner M3 (0,975)
#     s = Inf    aucun trimestriel admis     -> doit redonner M3 mensuel (0,934)
#
#   Si un s intermediaire fait MIEUX que 0,934, alors les indicateurs
#   trimestriels portent bien de l'information et il suffisait de leur demander
#   davantage. Si la courbe est monotone decroissante jusqu'a Inf, ils
#   n'apportent rien et la bonne regle est de les exclure de la selection en M3.
#
# ORGANISATION DU CALCUL
#   L'agregation des 393 series est le poste couteux, et elle ne depend PAS du
#   seuil. Elle est donc faite une seule fois par origine, et les seuils sont
#   balayes ensuite sur la base agregee. Sans cette precaution le test couterait
#   une heure au lieu de dix minutes.
# ============================================================================
source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")
source("R/fonctions/kalman.R")

SEUIL_R <- 0.15; SEUIL_P <- 0.10; MIN_OBS_SEL <- 20L; MAX_RETENUS <- 5L
SEUILS_TRIM <- c(0.15, 0.25, 0.30, 0.35, 0.45, Inf)

couverture <- charger_couverture(); BC <- couverture$couvertes
ind  <- charger_indicateurs(branches = BC)
meta <- charger_metadonnees() %>% dplyr::select(id_serie, agregation, transformation)
longueur <- ind %>% dplyr::count(id_serie, name = "n_obs")
ind <- ind %>% dplyr::filter(id_serie %in% longueur$id_serie[longueur$n_obs >= 36L])
# quelles series sont NATIVEMENT trimestrielles
freq_serie <- ind %>% dplyr::distinct(id_serie, frequence) %>%
  { stats::setNames(.$frequence, .$id_serie) }

va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)
origines <- sort(unique(va$date[va$date >= as.Date("2014-06-30")]))

#' Selection avec seuil differencie, a partir de statistiques deja calculees.
#' Reprend la logique gloutonne de `selectionner_disponible` : on descend du
#' plus correle au moins correle, et on n'ajoute une serie que si elle laisse
#' assez de lignes d'entrainement conjointes.
choisir <- function(stats_sel, base, seuil_trim) {
  elig <- stats_sel %>%
    dplyr::filter(dplyr::if_else(est_trimestriel,
                                 abs(correlation) >= seuil_trim,
                                 abs(correlation) >= SEUIL_R)) %>%
    dplyr::arrange(dplyr::desc(abs(correlation)))
  retenus <- character(0)
  for (k in elig$id_serie) {
    essai <- c(retenus, k)
    if (sum(stats::complete.cases(base[, essai, drop = FALSE])) >= length(essai) + 6L) {
      retenus <- essai
    }
    if (length(retenus) >= MAX_RETENUS) break
  }
  retenus
}

traiter <- function(cible) {
  info <- information_set_intra(ind, cible, scenario = "M3")
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

    ligne_cible <- x_l %>% dplyr::filter(date == cible)
    if (nrow(ligne_cible) != 1L) return(NULL)
    observables <- setdiff(names(ligne_cible)[!is.na(unlist(ligne_cible[1, ]))], "date")
    if (length(observables) == 0L) return(NULL)

    gg <- g_b %>% dplyr::filter(date < cible)
    stats_sel <- x_b %>%
      dplyr::filter(id_serie %in% observables, date < cible) %>%
      dplyr::inner_join(gg, by = "date") %>%
      dplyr::filter(!is.na(x), !is.na(g)) %>%
      dplyr::group_by(id_serie) %>%
      dplyr::summarise(
        n = dplyr::n(),
        correlation = if (dplyr::n() >= 4L && stats::sd(x) > 0 && stats::sd(g) > 0)
          suppressWarnings(stats::cor(x, g)) else NA_real_,
        p_value = if (dplyr::n() >= 4L && stats::sd(x) > 0 && stats::sd(g) > 0)
          suppressWarnings(stats::cor.test(x, g)$p.value) else NA_real_,
        .groups = "drop") %>%
      dplyr::filter(n >= MIN_OBS_SEL, !is.na(correlation), p_value < SEUIL_P) %>%
      dplyr::mutate(est_trimestriel = freq_serie[id_serie] == "trimestriel")
    if (nrow(stats_sel) == 0L) return(NULL)

    base <- g_b %>% dplyr::inner_join(x_l, by = "date") %>%
      dplyr::filter(date < cible, !is.na(g))
    reel <- g_b$g[g_b$date == cible]
    reel <- if (length(reel) == 1L) reel else NA_real_

    purrr::map_dfr(SEUILS_TRIM, function(st) {
      ids <- choisir(stats_sel, base, st)
      r <- if (length(ids) > 0L)
        estimer_bridge_ar(g_b, x_l, ids, cible, avec_ar = TRUE) else NULL
      tibble::tibble(
        seuil_trim = st, branche = b, origine = cible,
        n_retenus = length(ids),
        n_trim_retenus = sum(freq_serie[ids] == "trimestriel", na.rm = TRUE),
        bridge = if (is.null(r)) NA_real_ else r$prevision,
        derniere_obs = if (is.null(r)) as.Date(NA) else r$derniere_obs,
        reel = reel)
    })
  })
}

cat("balayage du seuil trimestriel sur", length(origines), "origines...\n")
res <- purrr::map_dfr(seq_along(origines), function(i) traiter(origines[i]))
stopifnot(all(res$derniere_obs < res$origine, na.rm = TRUE))
cat("controle anti-look-ahead : OK\n")
ecrire_csv(res, file.path(DOSSIER_RESULTATS, "09_seuil_trimestriel_brut.csv"))

# perimetre commun a TOUS les seuils
n_s <- dplyr::n_distinct(res$seuil_trim)
commun <- res %>% dplyr::filter(!is.na(bridge)) %>%
  dplyr::count(branche, origine) %>% dplyr::filter(n == n_s) %>%
  dplyr::select(branche, origine)
cat(sprintf("perimetre commun : %d couples\n", nrow(commun)))

bilan <- res %>% dplyr::inner_join(commun, by = c("branche", "origine")) %>%
  dplyr::group_by(seuil_trim, branche) %>%
  dplyr::filter(dplyr::n() >= 4L) %>%
  dplyr::summarise(ratio = sqrt(mean((reel - bridge)^2)) / stats::sd(reel),
                   n_ret = mean(n_retenus), n_trim = mean(n_trim_retenus),
                   .groups = "drop") %>%
  dplyr::group_by(seuil_trim) %>%
  dplyr::summarise(branches = dplyr::n(),
                   ratio_median = round(stats::median(ratio), 3),
                   br_ok = sum(ratio < 1),
                   retenus_moyen = round(mean(n_ret), 2),
                   dont_trimestriels = round(mean(n_trim), 2), .groups = "drop")
cat("\n=== SEUIL TRIMESTRIEL : passerelle seule en M3, perimetre commun ===\n")
print(as.data.frame(bilan))
ecrire_csv(bilan, file.path(DOSSIER_RESULTATS, "09_seuil_trimestriel.csv"))
