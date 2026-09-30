# ============================================================================
# Selection CONDITIONNELLE contre selection MARGINALE, en M3
# ============================================================================
# CE QUI EST ETABLI
#   En M3, les indicateurs trimestriels degradent la prevision (ratio 0,975
#   contre 0,934 sans eux), et durcir leur seuil de correlation n'y change rien :
#   la courbe est plate de 0,15 a 0,45 puis chute d'un coup a l'exclusion.
#   Meme ceux qui franchissent |r| >= 0,45 nuisent encore.
#
#   L'explication n'est donc pas la faiblesse de leur correlation mais leur
#   REDONDANCE : ils mesurent en moins fin ce que les mensuels de la meme
#   branche mesurent deja. Une correlation forte avec la VA ne les rend pas
#   utiles, elle les rend SELECTIONNABLES -- donc nuisibles, puisqu'ils prennent
#   une place sous le plafond de cinq regresseurs sans rien ajouter.
#
# LE DEFAUT DU CRITERE, ET SA CORRECTION
#   Le critere actuel classe sur la correlation MARGINALE cor(x, g). Deux series
#   quasi identiques ont la meme correlation marginale elevee, et sont retenues
#   toutes les deux. Le critere ne peut structurellement pas voir la redondance.
#
#   La selection CONDITIONNELLE classe sur la correlation PARTIELLE : la
#   correlation entre ce qui reste inexplique de g et ce qui reste inexplique du
#   candidat, une fois retire l'effet des regresseurs DEJA retenus.
#
#       r_partiel(x_k | Z) = cor( g - P_Z g , x_k - P_Z x_k )
#
#   ou P_Z est la projection orthogonale sur Z = [1, g_{T-1}, deja retenus].
#   Un indicateur redondant a une correlation partielle proche de zero : il
#   n'apporte rien de neuf, et il est ecarte. C'est la selection pas a pas
#   ascendante classique, appliquee ici sous contrainte de disponibilite.
#
#   Le terme autoregressif entre dans Z DES LE DEPART : il est toujours dans
#   l'equation finale, donc un indicateur ne doit etre juge que sur ce qu'il
#   ajoute A LUI. Le critere marginal, lui, recompensait des indicateurs qui ne
#   faisaient que repeter la persistance de la branche.
#
# CE QUE LE TEST COMPARE, en M3 et a perimetre commun
#   marginal   + trimestriels     (reference actuelle, 0,975)
#   marginal   sans trimestriels  (meilleure solution connue, 0,934)
#   conditionnel + trimestriels
#   conditionnel sans trimestriels
#
#   Si le conditionnel avec trimestriels rejoint ou depasse 0,934, le critere
#   corrige le probleme SANS jeter d'information -- ce serait la bonne solution.
# ============================================================================
source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")
source("R/fonctions/kalman.R")

SEUIL_R <- 0.15; SEUIL_P <- 0.10; MIN_OBS_SEL <- 20L
MAX_RETENUS <- 5L; MARGE_DDL <- 6L

couverture <- charger_couverture(); BC <- couverture$couvertes
ind  <- charger_indicateurs(branches = BC)
meta <- charger_metadonnees() %>% dplyr::select(id_serie, agregation, transformation)
longueur <- ind %>% dplyr::count(id_serie, name = "n_obs")
ind <- ind %>% dplyr::filter(id_serie %in% longueur$id_serie[longueur$n_obs >= 36L])
freq_serie <- ind %>% dplyr::distinct(id_serie, frequence) %>%
  { stats::setNames(.$frequence, .$id_serie) }

va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)
origines <- sort(unique(va$date[va$date >= as.Date("2014-06-30")]))

#' Residu de v apres projection sur les colonnes de Z (constante comprise).
residu <- function(v, Z) {
  qrz <- qr(Z)
  as.numeric(v - Z %*% qr.coef(qrz, v))
}

#' Selection pas a pas ascendante sur la correlation PARTIELLE.
#'
#' A chaque etape : on retire des candidats et de la cible l'effet des
#' regresseurs deja retenus, et l'on ajoute celui dont la correlation partielle
#' est la plus forte, s'il est significatif et s'il laisse assez de lignes
#' conjointes. On s'arrete quand plus rien n'est significatif.
selection_conditionnelle <- function(donnees, candidats, max_retenus = MAX_RETENUS) {
  retenus <- character(0)
  restants <- candidats
  for (pas in seq_len(max_retenus)) {
    if (length(restants) == 0L) break
    meilleur <- NULL; meilleur_r <- 0
    for (k in restants) {
      cols <- c(retenus, k)
      ok <- stats::complete.cases(donnees[, c("g", "g_ret", cols), drop = FALSE])
      n <- sum(ok)
      if (n < length(cols) + MARGE_DDL + 2L) next
      sous <- donnees[ok, , drop = FALSE]
      Z <- cbind(1, sous$g_ret)
      if (length(retenus) > 0L) Z <- cbind(Z, as.matrix(sous[, retenus, drop = FALSE]))
      ry <- residu(sous$g, Z)
      rx <- residu(sous[[k]], Z)
      if (stats::sd(ry) <= 0 || stats::sd(rx) <= 0) next
      r <- suppressWarnings(stats::cor(ry, rx))
      if (is.na(r)) next
      # test de la correlation partielle : ddl = n - (colonnes de Z) - 1
      ddl <- n - ncol(Z) - 1L
      if (ddl < 5L) next
      tstat <- abs(r) * sqrt(ddl / max(1e-12, 1 - r^2))
      p <- 2 * stats::pt(-abs(tstat), df = ddl)
      if (abs(r) >= SEUIL_R && p < SEUIL_P && abs(r) > meilleur_r) {
        meilleur_r <- abs(r); meilleur <- k
      }
    }
    if (is.null(meilleur)) break
    retenus <- c(retenus, meilleur)
    restants <- setdiff(restants, meilleur)
  }
  retenus
}

#' Selection marginale, telle qu'utilisee jusqu'ici (pour comparaison).
selection_marginale <- function(donnees, candidats, stats_sel) {
  elig <- stats_sel %>% dplyr::filter(id_serie %in% candidats) %>%
    dplyr::arrange(dplyr::desc(abs(correlation)))
  retenus <- character(0)
  for (k in elig$id_serie) {
    essai <- c(retenus, k)
    if (sum(stats::complete.cases(donnees[, essai, drop = FALSE])) >=
        length(essai) + MARGE_DDL) retenus <- essai
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
    g_b <- va %>% dplyr::filter(branche == b) %>% dplyr::arrange(date) %>%
      dplyr::mutate(g_ret = dplyr::lag(g)) %>% dplyr::select(date, g, g_ret)
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
      dplyr::filter(n >= MIN_OBS_SEL, !is.na(correlation),
                    abs(correlation) >= SEUIL_R, p_value < SEUIL_P)

    donnees <- g_b %>% dplyr::inner_join(x_l, by = "date") %>%
      dplyr::filter(date < cible, !is.na(g))
    # candidats : observes en T, longueur suffisante. Pour le conditionnel on ne
    # preselectionne PAS sur la correlation marginale -- ce serait reintroduire
    # le critere qu'on cherche a remplacer.
    cand_tous <- intersect(observables, names(donnees))
    cand_tous <- cand_tous[vapply(cand_tous,
      function(k) sum(!is.na(donnees[[k]]) & !is.na(donnees$g)) >= MIN_OBS_SEL,
      logical(1))]
    cand_mens <- cand_tous[freq_serie[cand_tous] == "mensuel"]

    reel <- g_b$g[g_b$date == cible]
    reel <- if (length(reel) == 1L) reel else NA_real_

    faire <- function(lab, ids) {
      r <- if (length(ids) > 0L)
        estimer_bridge_ar(g_b %>% dplyr::select(date, g), x_l, ids, cible,
                          avec_ar = TRUE) else NULL
      tibble::tibble(methode = lab, branche = b, origine = cible,
                     n_retenus = length(ids),
                     n_trim = sum(freq_serie[ids] == "trimestriel", na.rm = TRUE),
                     bridge = if (is.null(r)) NA_real_ else r$prevision,
                     derniere_obs = if (is.null(r)) as.Date(NA) else r$derniere_obs,
                     reel = reel)
    }
    dplyr::bind_rows(
      faire("marginal + trimestriels",
            selection_marginale(donnees, cand_tous, stats_sel)),
      faire("marginal sans trimestriels",
            selection_marginale(donnees, cand_mens, stats_sel)),
      faire("conditionnel + trimestriels",
            selection_conditionnelle(donnees, cand_tous)),
      faire("conditionnel sans trimestriels",
            selection_conditionnelle(donnees, cand_mens)))
  })
}

cat("selection conditionnelle contre marginale, sur", length(origines), "origines...\n")
res <- purrr::map_dfr(seq_along(origines), function(i) traiter(origines[i]))
stopifnot(all(res$derniere_obs < res$origine, na.rm = TRUE))
cat("controle anti-look-ahead : OK\n")
ecrire_csv(res, file.path(DOSSIER_RESULTATS, "09_selection_conditionnelle_brut.csv"))

n_m <- dplyr::n_distinct(res$methode)
commun <- res %>% dplyr::filter(!is.na(bridge)) %>%
  dplyr::count(branche, origine) %>% dplyr::filter(n == n_m) %>%
  dplyr::select(branche, origine)
cat(sprintf("perimetre commun : %d couples\n", nrow(commun)))

bilan <- res %>% dplyr::inner_join(commun, by = c("branche", "origine")) %>%
  dplyr::group_by(methode, branche) %>%
  dplyr::filter(dplyr::n() >= 4L) %>%
  dplyr::summarise(ratio = sqrt(mean((reel - bridge)^2)) / stats::sd(reel),
                   correlation = suppressWarnings(stats::cor(bridge, reel)),
                   n_ret = mean(n_retenus), n_tr = mean(n_trim), .groups = "drop") %>%
  dplyr::group_by(methode) %>%
  dplyr::summarise(branches = dplyr::n(),
                   ratio_median = round(stats::median(ratio), 3),
                   br_ok = sum(ratio < 1),
                   correl = round(stats::median(correlation, na.rm = TRUE), 2),
                   retenus = round(mean(n_ret), 2),
                   dont_trim = round(mean(n_tr), 2), .groups = "drop") %>%
  dplyr::arrange(ratio_median)
cat("\n=== M3, passerelle seule, perimetre commun ===\n")
print(as.data.frame(bilan))
ecrire_csv(bilan, file.path(DOSSIER_RESULTATS, "09_selection_conditionnelle.csv"))

cat("\n=== par branche ===\n")
pb <- res %>% dplyr::inner_join(commun, by = c("branche", "origine")) %>%
  dplyr::group_by(branche, methode) %>%
  dplyr::summarise(ratio = round(sqrt(mean((reel - bridge)^2)) / stats::sd(reel), 3),
                   .groups = "drop") %>%
  tidyr::pivot_wider(names_from = methode, values_from = ratio)
print(as.data.frame(pb))
