# ============================================================================
# 04b_variantes_passerelle.R -- PHASE 4 bis : pistes d'amelioration
# ============================================================================
# POURQUOI CE SCRIPT
#   La passerelle de la phase 4 ne produit que 308 previsions sur 576, et se
#   fait battre par le BVAR. Le diagnostic a montre que la perte n'est PAS un
#   effet du jagged edge : un indicateur existe au trimestre cible dans 576 cas
#   sur 576. Elle vient de la procedure elle-meme.
#
#     103 cas : le set des 5 retenus n'est pas complet au trimestre cible,
#               alors qu'un SOUS-ENSEMBLE l'est ;
#      62 cas : aucun des 5 retenus n'est observe au trimestre cible, alors que
#               d'autres series de la branche le sont ;
#     103 cas : aucun indicateur ne franchit le seuil de correlation.
#
#   Les deux premiers motifs, soit 165 cas, tiennent a ce que la selection
#   classe par correlation SEULE et que l'estimation supprime ensuite par liste
#   sur cinq series a la fois. Ce sont des defauts d'algorithme, pas de donnees.
#
# LES CINQ PISTES
#   (1) disponibilite : ne retenir que des series observees en T, et construire
#       le set de facon gloutonne sous contrainte de lignes conjointes ;
#   (2) ACP : remplacer la selection dure par une reduction de dimension, qui
#       garde TOUTES les series au lieu d'en jeter 80 pour en garder 5 ;
#   (3) retards : tester aussi la correlation avec X_{T-1}, invisible pour un
#       critere purement contemporain ;
#   (4) terme AR : ajouter g_{j,T-1} a la passerelle, qui ignore aujourd'hui
#       toute la persistance que le BVAR exploite de son cote ;
#   (5) fonds commun : autoriser une branche pauvre en series -- Transports en
#       a 7, Agriculture 5 -- a puiser dans les series des autres branches.
#
# PROTOCOLE
#   Les variantes sont departagees HORS ECHANTILLON sur les 48 origines, avec
#   les memes metriques que la phase 3 (ratio RMSFE / ecart-type, correlation
#   prevu-realise), auxquelles s'ajoute le TAUX DE PRODUCTION : une variante
#   qui previendrait mieux mais deux fois moins souvent ne serait pas meilleure.
#
# SORTIES
#   resultats/04b_comparaison_variantes.csv
#   resultats/04b_evaluation_par_branche.csv
#   resultats/04b_previsions_variantes.csv
#   figures/04b_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("04b_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("04b_", x))

PREMIERE_CIBLE <- as.Date("2014-06-30")
SEUIL_R        <- 0.15
SEUIL_P        <- 0.10
SEUIL_R_POOL   <- 0.30   # exigence renforcee pour une serie d'une AUTRE branche
MIN_OBS_SEL    <- 20L
MAX_RETENUS    <- 5L
MIN_OBS_DELTA  <- 8L
K_COMPOSANTES  <- 3L

# ============================================================================
# 1) BASES
# ============================================================================
cat("\n[1/5] Bases\n")
couverture <- charger_couverture()
BC <- couverture$couvertes

ind  <- charger_indicateurs(branches = BC)
trim <- indicateurs_trimestriels(ind)

diagnostic <- trim %>%
  dplyr::group_by(id_serie, branche) %>%
  dplyr::summarise(n = sum(!is.na(x)), .groups = "drop")
eligibles <- diagnostic$id_serie[diagnostic$n >= MIN_OBS_SEL]
tok <- trim %>% dplyr::filter(id_serie %in% eligibles, !is.na(x)) %>%
  dplyr::select(id_serie, branche, date, x)
cat(sprintf("      %d series eligibles sur %d\n", length(eligibles), nrow(diagnostic)))

va <- charger_va() %>%
  dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)
origines <- sort(unique(va$date[va$date >= PREMIERE_CIBLE]))
cat(sprintf("      %d origines x %d branches = %d cas\n",
            length(origines), length(BC), length(origines) * length(BC)))

bvar <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_recursives.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(branche, origine, bvar = prevision)

# ============================================================================
# 2) UNE PASSE POUR UNE VARIANTE
# ============================================================================
#' @param dispo   selection consciente de la disponibilite (piste 1)
#' @param acp     composantes principales au lieu de la selection (piste 2)
#' @param retards inclure la version retardee d'un trimestre (piste 3)
#' @param ar      ajouter g_{T-1} a la passerelle (piste 4)
#' @param pool    autoriser les series des autres branches (piste 5)
#' @param ridge   contraction de tous les indicateurs disponibles (piste 6)
passe_variante <- function(etiquette, dispo = TRUE, acp = FALSE,
                           retards = FALSE, ar = FALSE, pool = FALSE,
                           ridge = FALSE) {
  lignes <- list()
  for (b in BC) {
    g_b <- va %>% dplyr::filter(branche == b) %>% dplyr::select(date, g)

    # --- candidates de la branche, plus le fonds commun si demande ----------
    x_b <- tok %>% dplyr::filter(branche == b) %>%
      dplyr::select(id_serie, date, x) %>% dplyr::mutate(origine_serie = "propre")
    if (pool) {
      autres <- tok %>% dplyr::filter(branche != b) %>%
        dplyr::transmute(id_serie = paste0(id_serie, " [pool]"), date, x,
                         origine_serie = "pool")
      x_b <- dplyr::bind_rows(x_b, autres)
    }
    # --- versions retardees, en plus des contemporaines --------------------
    if (retards) {
      x_b <- dplyr::bind_rows(
        x_b,
        x_b %>% dplyr::filter(origine_serie == "propre") %>%
          retarder_indicateurs(1L) %>%
          dplyr::mutate(id_serie = paste0(id_serie, " [L1]")))
    }
    if (nrow(x_b) == 0L) next

    x_long  <- x_b %>% dplyr::select(id_serie, date, x)
    x_large <- x_long %>% tidyr::pivot_wider(names_from = id_serie, values_from = x)
    est_pool <- stats::setNames(x_b$origine_serie[!duplicated(x_b$id_serie)],
                                x_b$id_serie[!duplicated(x_b$id_serie)])

    for (i_o in seq_along(origines)) {
      cible <- origines[i_o]
      ids <- character(0); donnees <- x_large; n_cand <- NA_integer_

      if (ridge) {
        # La ridge ne selectionne pas : elle prend tout ce qui est disponible
        # au trimestre cible et contracte. On court-circuite donc le bloc de
        # selection et on passe directement a l'estimation.
        br <- estimer_bridge_ridge(g_b, x_large, cible, avec_ar = ar)
        reel <- g_b$g[g_b$date == cible]
        lignes[[length(lignes) + 1L]] <- tibble::tibble(
          specification = etiquette, branche = b, origine = cible,
          n_candidats = if (is.null(br)) NA_integer_ else br$n_indicateurs,
          n_retenus = if (is.null(br)) 0L else round(br$ddl_effectifs),
          prevision = if (is.null(br)) NA_real_ else br$prevision,
          derniere_obs = if (is.null(br)) as.Date(NA) else br$derniere_obs,
          reel = if (length(reel) == 1L) reel else NA_real_)
        next
      }

      if (acp) {
        cp <- composantes_principales(x_large, cible, k = K_COMPOSANTES)
        if (!is.null(cp)) { ids <- cp$ids; donnees <- cp$donnees; n_cand <- cp$n_series }
      } else if (dispo) {
        s <- selectionner_disponible(g_b, x_long, x_large, cible,
                                     seuil_r = SEUIL_R, seuil_p = SEUIL_P,
                                     min_obs = MIN_OBS_SEL, max_retenus = MAX_RETENUS)
        if (!is.null(s)) {
          # exigence renforcee pour les series venues d'une autre branche
          garde <- vapply(s$ids, function(k)
            identical(est_pool[[k]], "propre") ||
              abs(s$table$correlation[s$table$id_serie == k]) >= SEUIL_R_POOL,
            logical(1))
          ids <- s$ids[garde]; n_cand <- nrow(s$table)
        }
      } else {
        s <- selectionner_indicateurs(g_b, x_long, cible, SEUIL_R, SEUIL_P,
                                      MIN_OBS_SEL, MAX_RETENUS)
        if (!is.null(s)) { ids <- s$id_serie[s$retenu]; n_cand <- nrow(s) }
      }

      br <- if (length(ids) > 0L) {
        estimer_bridge_ar(g_b, donnees, ids, cible, avec_ar = ar)
      } else NULL

      reel <- g_b$g[g_b$date == cible]
      lignes[[length(lignes) + 1L]] <- tibble::tibble(
        specification = etiquette, branche = b, origine = cible,
        n_candidats = n_cand, n_retenus = length(ids),
        prevision = if (is.null(br)) NA_real_ else br$prevision,
        derniere_obs = if (is.null(br)) as.Date(NA) else br$derniere_obs,
        reel = if (length(reel) == 1L) reel else NA_real_)
    }
  }
  dplyr::bind_rows(lignes)
}

# ============================================================================
# 3) EXECUTION DES VARIANTES
# ============================================================================
cat("\n[2/5] Execution des variantes\n")
VARIANTES <- list(
  list(lab = "reference (phase 4)",              dispo = FALSE),
  list(lab = "1. disponibilite",                 dispo = TRUE),
  list(lab = "1+3. disponibilite + retards",     dispo = TRUE, retards = TRUE),
  list(lab = "1+4. disponibilite + terme AR",    dispo = TRUE, ar = TRUE),
  list(lab = "1+5. disponibilite + fonds commun", dispo = TRUE, pool = TRUE),
  list(lab = "2. composantes principales",       acp = TRUE),
  list(lab = "2+4. ACP + terme AR",              acp = TRUE, ar = TRUE),
  list(lab = "1+3+4+5. toutes sauf ACP",         dispo = TRUE, retards = TRUE,
       ar = TRUE, pool = TRUE),
  list(lab = "2+4+5. ACP + AR + fonds commun",   acp = TRUE, ar = TRUE, pool = TRUE),
  # --- piste 6 : contraction plutot que selection ---------------------------
  list(lab = "6. ridge (sans AR)",                ridge = TRUE, ar = FALSE),
  list(lab = "6+4. ridge + terme AR",             ridge = TRUE, ar = TRUE),
  list(lab = "6+4+5. ridge + AR + fonds commun",  ridge = TRUE, ar = TRUE, pool = TRUE),
  list(lab = "6+3+4+5. ridge + tout",             ridge = TRUE, ar = TRUE,
       pool = TRUE, retards = TRUE))

t0 <- Sys.time()
tous <- purrr::map_dfr(VARIANTES, function(v) {
  cat(sprintf("        %-38s", v$lab)); utils::flush.console()
  r <- passe_variante(v$lab,
                      dispo   = isTRUE(v$dispo),
                      acp     = isTRUE(v$acp),
                      retards = isTRUE(v$retards),
                      ar      = isTRUE(v$ar),
                      pool    = isTRUE(v$pool),
                      ridge   = isTRUE(v$ridge))
  cat(sprintf(" %3d/%d previsions\n", sum(!is.na(r$prevision)), nrow(r)))
  r
})
cat(sprintf("      %s\n", format(round(difftime(Sys.time(), t0, units = "mins"), 1))))

stopifnot("[ANTI-LOOK-AHEAD] une passerelle a vu sa cible" =
            all(tous$derniere_obs < tous$origine, na.rm = TRUE))
cat("      controle : aucune passerelle n'est estimee avec sa propre cible\n")
ecrire_csv(tous, chemin_res("previsions_variantes.csv"))

# ============================================================================
# 4) COMBINAISON ET EVALUATION
# ============================================================================
cat("\n[3/5] Combinaison avec le BVAR et evaluation\n")

combiner <- function(df) {
  df %>% dplyr::left_join(bvar, by = c("branche", "origine")) %>%
    dplyr::arrange(specification, branche, origine) %>%
    dplyr::group_by(specification, branche) %>%
    dplyr::group_modify(function(g, cle) {
      g$delta <- NA_real_; g$combinee <- NA_real_
      for (i in seq_len(nrow(g))) {
        hist <- g[seq_len(i - 1L), ] %>%
          dplyr::transmute(reel, bvar, bridge = prevision)
        pd <- poids_combinaison(hist, min_obs = MIN_OBS_DELTA)
        g$delta[i] <- pd$delta
        g$combinee[i] <- if (is.na(g$prevision[i])) g$bvar[i]
          else if (is.na(g$bvar[i])) g$prevision[i]
          else pd$delta * g$bvar[i] + (1 - pd$delta) * g$prevision[i]
      }
      g
    }) %>% dplyr::ungroup()
}
comb <- combiner(tous)

evaluer <- function(df, colonne) {
  df %>% dplyr::filter(!is.na(.data[[colonne]]), !is.na(reel)) %>%
    dplyr::group_by(specification, branche) %>%
    dplyr::summarise(n = dplyr::n(),
                     ratio = sqrt(mean((reel - .data[[colonne]])^2)) / stats::sd(reel),
                     correlation = suppressWarnings(stats::cor(.data[[colonne]], reel)),
                     .groups = "drop") %>%
    dplyr::mutate(cible = colonne)
}
eval_branche <- dplyr::bind_rows(evaluer(comb, "prevision"), evaluer(comb, "combinee"))
ecrire_csv(eval_branche, chemin_res("evaluation_par_branche.csv"))

taux <- tous %>% dplyr::group_by(specification) %>%
  dplyr::summarise(taux_production = mean(!is.na(prevision)),
                   n_produites = sum(!is.na(prevision)),
                   n_retenus_median = stats::median(n_retenus), .groups = "drop")

bilan <- eval_branche %>%
  dplyr::group_by(specification, cible) %>%
  dplyr::summarise(ratio_median = stats::median(ratio),
                   n_branches_ok = sum(ratio < 1),
                   correl_mediane = stats::median(correlation, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::left_join(taux, by = "specification")
ecrire_csv(bilan, chemin_res("comparaison_variantes.csv"))

afficher <- function(quoi, titre) {
  cat(sprintf("\n      --- %s ---\n", titre))
  print(bilan %>% dplyr::filter(cible == quoi) %>%
          dplyr::arrange(ratio_median) %>%
          dplyr::transmute(specification = substr(specification, 1, 34),
                           `taux prod.` = sprintf("%.0f%%", 100 * taux_production),
                           `ratio med.` = round(ratio_median, 3),
                           `br. < 1` = n_branches_ok,
                           `correl.` = round(correl_mediane, 2)), n = 12)
}
afficher("prevision", "passerelle seule")
afficher("combinee",  "combinaison BVAR + passerelle")

ref_bvar <- lire_csv(file.path(DOSSIER_RESULTATS, "03_diagnostics_branches.csv")) %>%
  dplyr::filter(branche %in% BC)
cat(sprintf("\n      pour memoire, BVAR seul sur ces 12 branches : ratio median %.3f | %d/12 < 1\n",
            stats::median(ref_bvar$ratio), sum(ref_bvar$ratio < 1)))

# --- Comparaison a PERIMETRE COMMUN ------------------------------------------
# Les variantes ne produisent pas sur les memes trimestres : la reference ne
# reussit que sur les cas les plus faciles. Comparer leurs ratios bruts
# reviendrait a comparer des epreuves de difficultes differentes. On refait donc
# le classement sur les seules origines-branches ou TOUTES les variantes ont
# produit une prevision.
n_spec <- dplyr::n_distinct(tous$specification)
commun <- tous %>% dplyr::filter(!is.na(prevision)) %>%
  dplyr::count(branche, origine) %>% dplyr::filter(n == n_spec) %>%
  dplyr::select(branche, origine)

bilan_commun <- tous %>%
  dplyr::inner_join(commun, by = c("branche", "origine")) %>%
  dplyr::group_by(specification, branche) %>%
  dplyr::summarise(ratio = sqrt(mean((reel - prevision)^2)) / stats::sd(reel),
                   correlation = suppressWarnings(stats::cor(prevision, reel)),
                   .groups = "drop") %>%
  dplyr::group_by(specification) %>%
  dplyr::summarise(ratio_median = stats::median(ratio),
                   n_branches_ok = sum(ratio < 1),
                   correl_mediane = stats::median(correlation, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::arrange(ratio_median)
ecrire_csv(bilan_commun, chemin_res("comparaison_perimetre_commun.csv"))

cat(sprintf("\n      --- PERIMETRE COMMUN : %d cas ou toutes les variantes produisent ---\n",
            nrow(commun)))
print(bilan_commun %>%
        dplyr::transmute(specification = substr(specification, 1, 34),
                         `ratio med.` = round(ratio_median, 3),
                         `br. < 1` = n_branches_ok,
                         `correl.` = round(correl_mediane, 2)), n = 15)

bv_commun <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_recursives.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::inner_join(commun, by = c("branche", "origine")) %>%
  dplyr::group_by(branche) %>%
  dplyr::summarise(ratio = sqrt(mean((reel - prevision)^2)) / stats::sd(reel),
                   .groups = "drop")
cat(sprintf("      BVAR seul sur le MEME perimetre : ratio median %.3f | %d/%d < 1\n",
            stats::median(bv_commun$ratio), sum(bv_commun$ratio < 1), nrow(bv_commun)))

# ============================================================================
# 5) FIGURES
# ============================================================================
cat("\n[4/5] Figures\n")

g1 <- bilan %>%
  dplyr::mutate(cible = dplyr::recode(cible, prevision = "Passerelle seule",
                                      combinee = "Combinaison BVAR + passerelle")) %>%
  ggplot2::ggplot(ggplot2::aes(taux_production, ratio_median,
                               label = substr(specification, 1, 18))) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_text(size = 2.6, hjust = -0.08, check_overlap = TRUE) +
  ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0.4, 1.25)) +
  ggplot2::facet_wrap(~ cible) +
  ggplot2::labs(x = "taux de production", y = "ratio median",
                title = "Les deux dimensions du probleme",
                subtitle = "une variante n'est meilleure que si elle previent mieux ET plus souvent")
ggplot2::ggsave(chemin_fig("production_vs_qualite.png"), g1,
                width = 11, height = 5, dpi = 150)

g2 <- eval_branche %>% dplyr::filter(cible == "prevision") %>%
  ggplot2::ggplot(ggplot2::aes(stats::reorder(specification, -ratio, stats::median),
                               ratio)) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  ggplot2::geom_boxplot(outlier.size = 0.7, linewidth = 0.3) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "RMSFE / ecart-type de la branche",
                title = "Distribution du ratio par variante, sur les 12 branches",
                subtitle = "passerelle seule")
ggplot2::ggsave(chemin_fig("ratio_par_variante.png"), g2,
                width = 9.5, height = 5.5, dpi = 150)

g3 <- tous %>% dplyr::group_by(specification, branche) %>%
  dplyr::summarise(taux = mean(!is.na(prevision)), .groups = "drop") %>%
  ggplot2::ggplot(ggplot2::aes(branche, specification, fill = taux)) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
  ggplot2::scale_fill_gradient(low = "grey92", high = "#1f4e79",
                               labels = scales::percent) +
  ggplot2::labs(x = NULL, y = NULL, fill = "taux",
                title = "Taux de production par branche et par variante",
                subtitle = "les branches pauvres en indicateurs sont les plus sensibles au choix de la variante") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
ggplot2::ggsave(chemin_fig("taux_par_branche.png"), g3,
                width = 11, height = 5.5, dpi = 150)

cat("      figures/04b_production_vs_qualite.png\n")
cat("      figures/04b_ratio_par_variante.png\n")
cat("      figures/04b_taux_par_branche.png\n")
cat("\nPhase 4 bis terminee.\n")
