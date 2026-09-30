# ============================================================================
# 10_incertitude.R -- Les ecarts annonces sont-ils distinguables du bruit ?
# ============================================================================
# LA LACUNE QUE CE SCRIPT COMBLE
#   Tout ce qui a ete produit jusqu'ici -- phases 3, 4, 13, etape 5 -- est
#   rapporte en ESTIMATIONS PONCTUELLES. On annonce que la combinaison fait
#   0,895 contre 0,978 pour le BVAR, sans jamais dire si cet ecart est
#   distinguable du hasard d'echantillonnage. C'est la faiblesse la plus
#   serieuse du projet, et le plan l'avait prevue (phase 18).
#
#   Elle est d'autant plus genante que les effectifs sont tres inegaux : trois
#   branches disposent des 48 origines, l'Agriculture de 4. Un ratio calcule
#   sur 4 points n'a aucune precision, et la mediane sur les branches en herite
#   sans que cela se voie.
#
# TROIS INSTRUMENTS, ET CE QU'ILS DISENT CHACUN
#
#   (1) DIEBOLD-MARIANO PAR BRANCHE
#       Pour deux previsions concurrentes d'erreurs e1 et e2, on forme la
#       difference de perte quadratique
#
#           d_t = e1_t^2 - e2_t^2
#
#       et l'on teste H0 : E[d_t] = 0. La statistique est la moyenne de d
#       rapportee a son ecart-type de long terme :
#
#           DM = mean(d) / sqrt( V(mean(d)) )
#
#       On utilise `forecast::dm.test`, qui applique la correction de
#       Harvey, Leybourne & Newbold (1997) pour petits echantillons -- une
#       necessite ici, ou n descend a 4 : la statistique est multipliee par
#       sqrt( (n + 1 - 2h + h(h-1)/n) / n ) et comparee a une loi de Student a
#       n-1 degres de liberte, non a une normale.
#
#   (2) TEST GROUPE SUR LES BRANCHES
#       Une branche seule offre trop peu d'observations. Mais les branches ne
#       sont pas independantes -- elles subissent les memes chocs au meme
#       trimestre --, donc on ne peut pas simplement empiler leurs
#       observations : cela surestimerait la precision.
#       On agrege donc D'ABORD sur les branches, a chaque origine, apres avoir
#       rendu les pertes comparables en les divisant par la variance propre de
#       la branche :
#
#           d~_t = (1/B) * somme_b [ (e1_{b,t}^2 - e2_{b,t}^2) / var_b ]
#
#       La correlation entre branches est ainsi absorbee dans la moyenne, et il
#       reste UNE serie temporelle sur laquelle le test est licite.
#
#   (3) BOOTSTRAP PAR BLOCS SUR LE RATIO
#       Le ratio median n'a pas de loi connue : c'est une mediane sur les
#       branches de rapports d'ecarts quadratiques. On le reechantillonne par
#       BLOCS d'origines consecutives, ce qui preserve la dependance temporelle
#       qu'un tirage independant detruirait, et l'on lit les quantiles.
#
# SORTIES
#   resultats/10_dm_par_branche.csv
#   resultats/10_dm_groupe.csv
#   resultats/10_intervalles_ratio.csv
#   figures/10_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
suppressPackageStartupMessages(library(forecast))

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("10_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("10_", x))

SEUIL     <- 0.10     # seuil de signification retenu, comme en phase 3
N_BOOT    <- 2000L
BLOC      <- 4L       # longueur de bloc : un an
set.seed(20260913)

cat("\n[1/5] Lecture des previsions\n")
intra <- lire_csv(file.path(DOSSIER_RESULTATS, "09_previsions_intra.csv")) %>%
  dplyr::mutate(origine = as.Date(origine))
n_sc <- dplyr::n_distinct(intra$scenario)
commun <- intra %>% dplyr::filter(!is.na(bridge)) %>%
  dplyr::count(branche, origine) %>% dplyr::filter(n == n_sc) %>%
  dplyr::select(branche, origine)
intra <- intra %>% dplyr::inner_join(commun, by = c("branche", "origine"))
cat(sprintf("      phase 13 : %d couples branche-origine, %d branches\n",
            nrow(commun), dplyr::n_distinct(commun$branche)))

effectifs <- commun %>% dplyr::count(branche, name = "n_origines") %>%
  dplyr::arrange(n_origines)
cat("      effectifs par branche : ")
cat(paste(sprintf("%s=%d", substr(effectifs$branche, 1, 12), effectifs$n_origines),
          collapse = ", "), "\n")

# ============================================================================
# 2) DIEBOLD-MARIANO PAR BRANCHE
# ============================================================================
cat("\n[2/5] Diebold-Mariano par branche\n")

#' @param a,b vecteurs d'erreurs alignes (a = reference, b = alternative).
dm_une <- function(a, b) {
  if (length(a) < 5L) {
    return(list(p = NA_real_, motif = sprintf("n = %d, trop court", length(a))))
  }
  t <- tryCatch(forecast::dm.test(a, b, h = 1L, power = 2),
                error = function(e) NULL)
  if (is.null(t)) return(list(p = NA_real_, motif = "test impossible"))
  list(p = unname(t$p.value), motif = NA_character_)
}

paires <- list(
  list(lab = "Combinaison contre BVAR seul",  ref = "bvar", alt = "combinee"),
  list(lab = "Passerelle contre BVAR seul",   ref = "bvar", alt = "bridge"),
  list(lab = "Combinaison contre Passerelle", ref = "bridge", alt = "combinee"))

dm_branche <- purrr::map_dfr(unique(intra$scenario), function(sc) {
  d_sc <- intra %>% dplyr::filter(scenario == sc)
  purrr::map_dfr(paires, function(p) {
    purrr::map_dfr(unique(d_sc$branche), function(b) {
      s <- d_sc %>% dplyr::filter(branche == b) %>% dplyr::arrange(origine) %>%
        dplyr::filter(!is.na(reel), !is.na(.data[[p$ref]]), !is.na(.data[[p$alt]]))
      if (nrow(s) == 0L) return(NULL)
      ea <- s$reel - s[[p$ref]]
      eb <- s$reel - s[[p$alt]]
      r <- dm_une(ea, eb)
      tibble::tibble(scenario = sc, comparaison = p$lab, branche = b,
                     n = nrow(s),
                     RMSFE_ref = sqrt(mean(ea^2)),
                     RMSFE_alt = sqrt(mean(eb^2)),
                     p_value = r$p, motif = r$motif)
    })
  })
}) %>%
  dplyr::mutate(
    significatif = !is.na(p_value) & p_value < SEUIL,
    verdict = dplyr::case_when(
      is.na(p_value)                        ~ "indecidable",
      !significatif                         ~ "non significatif",
      RMSFE_alt < RMSFE_ref                 ~ "alternative meilleure",
      TRUE                                  ~ "reference meilleure"))
ecrire_csv(dm_branche, chemin_res("dm_par_branche.csv"))

cat("\n      --- Combinaison contre BVAR seul, par scenario ---\n")
print(dm_branche %>%
        dplyr::filter(comparaison == "Combinaison contre BVAR seul") %>%
        dplyr::count(scenario, verdict) %>%
        tidyr::pivot_wider(names_from = verdict, values_from = n, values_fill = 0),
      n = 10)
cat(sprintf("\n      %d test(s) indecidables faute d'observations (n < 5)\n",
            sum(is.na(dm_branche$p_value))))

# ============================================================================
# 3) TEST GROUPE
# ============================================================================
cat("\n[3/5] Test groupe sur l'ensemble des branches\n")

#' Agrege les differences de perte sur les branches, a chaque origine, apres
#' mise a l'echelle par la variance propre de chaque branche. Renvoie UNE serie
#' temporelle, sur laquelle le test de Diebold-Mariano est licite.
dm_groupe <- function(d_sc, ref, alt) {
  ech <- d_sc %>% dplyr::group_by(branche) %>%
    dplyr::summarise(v = stats::var(reel, na.rm = TRUE), .groups = "drop")
  s <- d_sc %>%
    dplyr::filter(!is.na(reel), !is.na(.data[[ref]]), !is.na(.data[[alt]])) %>%
    dplyr::left_join(ech, by = "branche") %>%
    dplyr::filter(is.finite(v), v > 0) %>%
    dplyr::mutate(d = ((reel - .data[[ref]])^2 - (reel - .data[[alt]])^2) / v) %>%
    dplyr::group_by(origine) %>%
    dplyr::summarise(d = mean(d), .groups = "drop") %>%
    dplyr::arrange(origine)
  if (nrow(s) < 8L) return(NULL)
  # DM sur la serie agregee : on teste E[d] = 0 avec variance de long terme
  n <- nrow(s); dbar <- mean(s$d)
  # Newey-West, troncature usuelle
  L <- max(1L, floor(4 * (n / 100)^(2/9)))
  g0 <- stats::var(s$d) * (n - 1) / n
  gamma <- vapply(seq_len(L), function(l)
    mean((s$d[-(1:l)] - dbar) * (s$d[1:(n - l)] - dbar)), numeric(1))
  v_lr <- g0 + 2 * sum((1 - seq_len(L) / (L + 1)) * gamma)
  if (!is.finite(v_lr) || v_lr <= 0) return(NULL)
  stat <- dbar / sqrt(v_lr / n)
  list(n = n, d_moyen = dbar, stat = stat,
       p = 2 * stats::pt(-abs(stat), df = n - 1L))
}

groupe <- purrr::map_dfr(unique(intra$scenario), function(sc) {
  d_sc <- intra %>% dplyr::filter(scenario == sc)
  purrr::map_dfr(paires, function(p) {
    g <- dm_groupe(d_sc, p$ref, p$alt)
    if (is.null(g)) return(NULL)
    tibble::tibble(scenario = sc, comparaison = p$lab, n_origines = g$n,
                   perte_moyenne = g$d_moyen, statistique = g$stat,
                   p_value = g$p,
                   verdict = dplyr::case_when(
                     g$p >= SEUIL      ~ "non significatif",
                     g$d_moyen > 0     ~ "alternative meilleure",
                     TRUE              ~ "reference meilleure"))
  })
})
ecrire_csv(groupe, chemin_res("dm_groupe.csv"))
cat("\n      --- test groupe ---\n")
print(groupe %>% dplyr::transmute(scenario, comparaison = substr(comparaison, 1, 30),
                                  `n orig.` = n_origines,
                                  `stat.` = round(statistique, 2),
                                  `p` = round(p_value, 4), verdict), n = 20)

# ============================================================================
# 4) BOOTSTRAP PAR BLOCS SUR LE RATIO MEDIAN
# ============================================================================
cat("\n[4/5] Bootstrap par blocs\n")

#' Ratio median sur les branches, pour un jeu d'origines donne.
ratio_median <- function(d_sc, colonne, origines_tirees) {
  s <- d_sc %>% dplyr::filter(origine %in% origines_tirees,
                              !is.na(reel), !is.na(.data[[colonne]]))
  if (nrow(s) == 0L) return(NA_real_)
  r <- s %>% dplyr::group_by(branche) %>%
    dplyr::filter(dplyr::n() >= 4L) %>%
    dplyr::summarise(ratio = sqrt(mean((reel - .data[[colonne]])^2)) /
                       stats::sd(reel), .groups = "drop")
  if (nrow(r) == 0L) return(NA_real_)
  stats::median(r$ratio)
}

#' Tirage par blocs d'origines consecutives (bootstrap mobile).
tirer_blocs <- function(origines, bloc, n_cible) {
  n <- length(origines)
  departs <- sample.int(n - bloc + 1L, size = ceiling(n_cible / bloc), replace = TRUE)
  idx <- unlist(lapply(departs, function(s) s:(s + bloc - 1L)))
  origines[utils::head(idx, n_cible)]
}

boot <- purrr::map_dfr(unique(intra$scenario), function(sc) {
  d_sc <- intra %>% dplyr::filter(scenario == sc)
  orig <- sort(unique(d_sc$origine))
  purrr::map_dfr(c("bridge", "combinee", "bvar"), function(col) {
    obs <- ratio_median(d_sc, col, orig)
    rep <- vapply(seq_len(N_BOOT), function(i) {
      ratio_median(d_sc, col, tirer_blocs(orig, BLOC, length(orig)))
    }, numeric(1))
    rep <- rep[is.finite(rep)]
    tibble::tibble(scenario = sc,
                   modele = dplyr::recode(col, bridge = "Passerelle",
                                          combinee = "Combinaison",
                                          bvar = "BVAR seul"),
                   ratio = obs,
                   borne_basse = unname(stats::quantile(rep, 0.05)),
                   borne_haute = unname(stats::quantile(rep, 0.95)),
                   n_replications = length(rep))
  })
})
ecrire_csv(boot, chemin_res("intervalles_ratio.csv"))
cat("\n      --- ratio median et intervalle a 90 % (bootstrap par blocs) ---\n")
print(boot %>% dplyr::transmute(scenario, modele,
                                ratio = round(ratio, 3),
                                `IC 90 %` = sprintf("[%.3f ; %.3f]",
                                                    borne_basse, borne_haute)), n = 20)

# ============================================================================
# 5) FIGURES
# ============================================================================
cat("\n[5/5] Figures\n")
g1 <- boot %>%
  ggplot2::ggplot(ggplot2::aes(scenario, ratio, colour = modele, group = modele)) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = borne_basse, ymax = borne_haute),
                         width = 0.12, position = ggplot2::position_dodge(0.35)) +
  ggplot2::geom_point(size = 2.2, position = ggplot2::position_dodge(0.35)) +
  ggplot2::labs(x = NULL, y = "ratio median", colour = NULL,
                title = "Le ratio, avec son incertitude",
                subtitle = "intervalle a 90 % par bootstrap par blocs de quatre trimestres")
ggplot2::ggsave(chemin_fig("intervalles_ratio.png"), g1,
                width = 9, height = 4.8, dpi = 150)

g2 <- dm_branche %>%
  dplyr::filter(comparaison == "Combinaison contre BVAR seul", !is.na(p_value)) %>%
  ggplot2::ggplot(ggplot2::aes(stats::reorder(branche, p_value), p_value,
                               colour = verdict)) +
  ggplot2::geom_hline(yintercept = SEUIL, linetype = "dashed", linewidth = 0.3) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ scenario, nrow = 1) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "p-value du test de Diebold-Mariano", colour = NULL,
                title = "Combinaison contre BVAR seul, branche par branche",
                subtitle = "sous le trait, l'ecart est distinguable du bruit au seuil de 10 %")
ggplot2::ggsave(chemin_fig("dm_par_branche.png"), g2,
                width = 11, height = 4.5, dpi = 150)

cat("      figures/10_intervalles_ratio.png\n      figures/10_dm_par_branche.png\n")
cat("\nTests d'incertitude termines.\n")
