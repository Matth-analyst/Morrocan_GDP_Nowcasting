# ============================================================================
# 04_bridge_equations.R -- PHASE 4 : equations de passerelle
# ============================================================================
# Plan de correction : etape 4 -- "selection recursive + agregation correcte +
# bridge recursif + delta recursif", soit les phases 5, 6, 7 et 10 du plan.
#
# CE QUE FAISAIT LA VERSION 1
#   - `mean(dlog)` applique uniformement a tous les indicateurs, quelle que
#     soit leur nature economique ;
#   - une selection d'indicateurs figee, calculee une fois sur tout
#     l'echantillon puis reutilisee pour prevoir le passe ;
#   - des coefficients de passerelle estimes de meme sur tout l'echantillon ;
#   - un poids de combinaison delta unique, optimise sur toute la periode.
#   Les quatre relevent du meme defaut : de l'information posterieure a la
#   cible entre dans la prevision de cette cible.
#
# CE QUE FAIT LA PHASE 4
#   1. Agregation mensuel -> trimestriel selon la nature de l'indicateur
#      (sum / mean / last), avec REFUS des trimestres incomplets.
#   2. Selection des indicateurs recalculee a chaque origine sur la seule
#      information anterieure.
#   3. Passerelle estimee a chaque origine sur t < T.
#   4. Poids de combinaison avec le BVAR estime a chaque origine sur t < T.
#
# PERIMETRE
#   12 branches couvertes par le vivier. Les 4 branches non couvertes
#   (services aux entreprises, administration publique, education-sante, autres
#   services) relevent de l'AR(4) de l'etape 5.
#
# SORTIES
#   resultats/04_indicateurs_trimestriels_diagnostic.csv
#   resultats/04_agregation_pertes.csv
#   resultats/04_selection_par_origine.csv
#   resultats/04_previsions_bridge.csv
#   resultats/04_previsions_combinees.csv
#   resultats/04_evaluation_branches.csv
#   resultats/04_poids_delta.csv
#   figures/04_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("04_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("04_", x))

# --- Parametres de la phase --------------------------------------------------
PREMIERE_CIBLE <- as.Date("2014-06-30")   # identique a la phase 3
SEUIL_R        <- 0.15                    # |correlation| minimale (plan, phase 5)
SEUIL_P        <- 0.10                    # p-value maximale   (plan, phase 5)
MIN_OBS_SEL    <- 20L                     # trimestres apparies exiges
MAX_RETENUS    <- 5L                      # regresseurs au plus dans la passerelle
MIN_OBS_DELTA  <- 8L                      # trimestres exiges pour estimer delta

# ============================================================================
# 1) BASE TRIMESTRIELLE DES INDICATEURS
# ============================================================================
cat("\n[1/6] Agregation des indicateurs en trimestriel\n")

couverture <- charger_couverture()
BRANCHES_COUVERTES <- couverture$couvertes
cat(sprintf("      %d branches couvertes, %d non couvertes (AR(4), etape 5)\n",
            length(BRANCHES_COUVERTES), length(couverture$non_couvertes)))

ind <- charger_indicateurs(branches = BRANCHES_COUVERTES)
cat(sprintf("      %d series lues (%d mensuelles, %d trimestrielles)\n",
            dplyr::n_distinct(ind$id_serie),
            dplyr::n_distinct(ind$id_serie[ind$frequence == "mensuel"]),
            dplyr::n_distinct(ind$id_serie[ind$frequence == "trimestriel"])))

trim <- indicateurs_trimestriels(ind)

# --- Ce que coute le refus des trimestres incomplets -------------------------
# On documente la perte plutot que de la subir en silence : un trimestre dont
# un mois manque est mis a NA (voir `agreger_trimestriel`). La question n'est
# pas de savoir si la regle est couteuse -- elle l'est -- mais si elle laisse
# assez de matiere a chaque branche.
pertes <- trim %>%
  dplyr::filter(frequence == "mensuel") %>%
  dplyr::group_by(id_serie, branche) %>%
  dplyr::arrange(date, .by_group = TRUE) %>%
  dplyr::summarise(
    trimestres = dplyr::n(),
    refuses    = sum(n_mois < 3L),
    bord       = sum(n_mois < 3L & (date == min(date) | date == max(date))),
    .groups = "drop") %>%
  dplyr::mutate(interne = refuses - bord)
ecrire_csv(pertes, chemin_res("agregation_pertes.csv"))
cat(sprintf("      trimestres refuses : %d, dont %d trous internes (%d series touchees)\n",
            sum(pertes$refuses), sum(pertes$interne), sum(pertes$refuses > 0)))

# --- Diagnostic d'exploitabilite ---------------------------------------------
diagnostic <- trim %>%
  dplyr::group_by(id_serie, branche, indicateur, frequence, agregation,
                  transformation) %>%
  dplyr::summarise(
    n_trimestres = sum(!is.na(x)),
    debut = suppressWarnings(min(date[!is.na(x)])),
    fin   = suppressWarnings(max(date[!is.na(x)])),
    .groups = "drop") %>%
  dplyr::mutate(eligible = n_trimestres >= MIN_OBS_SEL)
ecrire_csv(diagnostic, chemin_res("indicateurs_trimestriels_diagnostic.csv"))
cat(sprintf("      %d series sur %d atteignent %d trimestres exploitables\n",
            sum(diagnostic$eligible), nrow(diagnostic), MIN_OBS_SEL))

# ============================================================================
# 2) CIBLE ET ORIGINES
# ============================================================================
cat("\n[2/6] Cible et origines\n")
va <- charger_va() %>%
  dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)

origines <- sort(unique(va$date[va$date >= PREMIERE_CIBLE]))
cat(sprintf("      %d origines, de %s a %s\n", length(origines),
            date_vers_trimestre(origines[1]),
            date_vers_trimestre(origines[length(origines)])))

# Les previsions BVAR de la phase 3, pour la combinaison de l'etape 4.
bvar <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_recursives.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(branche, origine, bvar = prevision, reel)
cat(sprintf("      %d previsions BVAR relues (phase 3)\n", nrow(bvar)))

# ============================================================================
# 3) SELECTION ET PASSERELLE, RECURSIVES
# ============================================================================
cat("\n[3/6] Selection et passerelle recursives\n")

series_eligibles <- diagnostic$id_serie[diagnostic$eligible]
trim_ok <- trim %>% dplyr::filter(id_serie %in% series_eligibles, !is.na(x))

selections <- list(); previsions <- list()

for (b in BRANCHES_COUVERTES) {
  g_b <- va %>% dplyr::filter(branche == b) %>% dplyr::select(date, g)
  x_b <- trim_ok %>% dplyr::filter(branche == b) %>% dplyr::select(id_serie, date, x)
  if (nrow(x_b) == 0L) next

  # format large une fois pour toutes : les colonnes sont les id_serie
  x_large <- x_b %>% tidyr::pivot_wider(names_from = id_serie, values_from = x)

  for (i_o in seq_along(origines)) {
    cible <- origines[i_o]
    sel <- selectionner_indicateurs(g_b, x_b, cible, seuil_r = SEUIL_R,
                                    seuil_p = SEUIL_P, min_obs = MIN_OBS_SEL,
                                    max_retenus = MAX_RETENUS)
    if (is.null(sel)) next
    selections[[length(selections) + 1L]] <- sel %>%
      dplyr::mutate(branche = b, origine = cible, .before = 1)

    ids <- sel$id_serie[sel$retenu]
    br <- estimer_bridge(g_b, x_large, ids, cible)
    reel <- g_b$g[g_b$date == cible]
    previsions[[length(previsions) + 1L]] <- tibble::tibble(
      branche = b, origine = cible,
      n_candidats = nrow(sel), n_retenus = length(ids),
      prevision = if (is.null(br)) NA_real_ else br$prevision,
      n_obs = if (is.null(br)) NA_integer_ else br$n_obs,
      r2_ajuste = if (is.null(br)) NA_real_ else br$r2_ajuste,
      derniere_obs = if (is.null(br)) as.Date(NA) else br$derniere_obs,
      reel = if (length(reel) == 1L) reel else NA_real_)
  }
  cat(sprintf("      %-30s %d origines traitees\n", substr(b, 1, 30), length(origines)))
}

selection_log <- dplyr::bind_rows(selections)
bridge <- dplyr::bind_rows(previsions)

# --- Controles anti-look-ahead -----------------------------------------------
stopifnot(
  "[ANTI-LOOK-AHEAD] une passerelle a vu sa cible" =
    all(bridge$derniere_obs < bridge$origine, na.rm = TRUE))
cat("      controle : aucune passerelle n'est estimee avec sa propre cible\n")

ecrire_csv(selection_log, chemin_res("selection_par_origine.csv"))
ecrire_csv(bridge, chemin_res("previsions_bridge.csv"))

n_ok <- sum(!is.na(bridge$prevision))
cat(sprintf("      %d previsions produites sur %d tentatives (%.0f%%)\n",
            n_ok, nrow(bridge), 100 * n_ok / nrow(bridge)))
cat(sprintf("      indicateurs retenus par origine : mediane %d, etendue %d-%d\n",
            stats::median(bridge$n_retenus), min(bridge$n_retenus),
            max(bridge$n_retenus)))

# --- Stabilite de la selection ------------------------------------------------
# La liste retenue varie dans le temps : c'est le principe meme de la solution B
# du plan. On mesure cette variation plutot que de la supposer negligeable.
stabilite <- selection_log %>%
  dplyr::filter(retenu) %>%
  dplyr::group_by(branche, id_serie) %>%
  dplyr::summarise(origines_retenu = dplyr::n(), .groups = "drop") %>%
  dplyr::group_by(branche) %>%
  dplyr::summarise(
    series_utilisees = dplyr::n(),
    part_permanentes = mean(origines_retenu == length(origines)),
    .groups = "drop")
cat(sprintf("      %d series distinctes utilisees au total, dont %.0f%% presentes a toutes les origines\n",
            sum(stabilite$series_utilisees),
            100 * stats::weighted.mean(stabilite$part_permanentes,
                                       stabilite$series_utilisees)))

# ============================================================================
# 4) COMBINAISON BVAR + PASSERELLE
# ============================================================================
cat("\n[4/6] Poids de combinaison recursif\n")

base <- bridge %>%
  dplyr::select(branche, origine, bridge = prevision, reel) %>%
  dplyr::left_join(bvar %>% dplyr::select(branche, origine, bvar),
                   by = c("branche", "origine")) %>%
  dplyr::arrange(branche, origine)

lignes <- list()
for (b in unique(base$branche)) {
  sous <- base %>% dplyr::filter(branche == b)
  for (i in seq_len(nrow(sous))) {
    cible <- sous$origine[i]
    # L'historique du poids n'utilise que des origines STRICTEMENT anterieures.
    hist <- sous %>% dplyr::filter(origine < cible)
    pd <- poids_combinaison(hist, min_obs = MIN_OBS_DELTA)
    combinee <- if (is.na(sous$bridge[i]) && is.na(sous$bvar[i])) {
      NA_real_
    } else if (is.na(sous$bridge[i])) {
      sous$bvar[i]              # sans passerelle, le BVAR seul
    } else if (is.na(sous$bvar[i])) {
      sous$bridge[i]
    } else {
      pd$delta * sous$bvar[i] + (1 - pd$delta) * sous$bridge[i]
    }
    lignes[[length(lignes) + 1L]] <- tibble::tibble(
      branche = b, origine = cible, delta = pd$delta, delta_mode = pd$mode,
      delta_n_obs = pd$n_obs, bvar = sous$bvar[i], bridge = sous$bridge[i],
      combinee = combinee, reel = sous$reel[i])
  }
}
combine <- dplyr::bind_rows(lignes)
ecrire_csv(combine, chemin_res("previsions_combinees.csv"))
ecrire_csv(combine %>% dplyr::select(branche, origine, delta, delta_mode, delta_n_obs),
           chemin_res("poids_delta.csv"))

cat(sprintf("      delta median %.2f | %d origines a delta estime, %d au defaut\n",
            stats::median(combine$delta, na.rm = TRUE),
            sum(combine$delta_mode == "estime"),
            sum(combine$delta_mode != "estime")))

# ============================================================================
# 5) EVALUATION
# ============================================================================
cat("\n[5/6] Evaluation branche par branche\n")
# Meme metrique que la phase 3, pour que les chiffres soient comparables :
# le ratio RMSFE / ecart-type de la serie, et la correlation prevu / realise.
evaluer <- function(df, colonne) {
  df %>%
    dplyr::filter(!is.na(.data[[colonne]]), !is.na(reel)) %>%
    dplyr::group_by(branche) %>%
    dplyr::summarise(
      n = dplyr::n(),
      RMSFE = sqrt(mean((reel - .data[[colonne]])^2)),
      MAE   = mean(abs(reel - .data[[colonne]])),
      biais = mean(reel - .data[[colonne]]),
      sd_reel = stats::sd(reel),
      ratio = sqrt(mean((reel - .data[[colonne]])^2)) / stats::sd(reel),
      correlation = suppressWarnings(stats::cor(.data[[colonne]], reel)),
      .groups = "drop") %>%
    dplyr::mutate(modele = colonne, .before = 1)
}

evaluation <- dplyr::bind_rows(
  evaluer(combine, "bvar"), evaluer(combine, "bridge"), evaluer(combine, "combinee"))
ecrire_csv(evaluation, chemin_res("evaluation_branches.csv"))

bilan <- evaluation %>%
  dplyr::group_by(modele) %>%
  dplyr::summarise(ratio_median = stats::median(ratio),
                   n_branches_ok = sum(ratio < 1),
                   correl_mediane = stats::median(correlation, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::arrange(ratio_median)
cat("\n      --- performance sur les 12 branches couvertes ---\n")
print(bilan %>% dplyr::transmute(modele,
                                 `ratio median` = round(ratio_median, 3),
                                 `branches < 1` = n_branches_ok,
                                 `correl. med.` = round(correl_mediane, 2)), n = 5)

# ============================================================================
# 6) FIGURES
# ============================================================================
cat("\n[6/6] Figures\n")

g1 <- evaluation %>%
  dplyr::mutate(modele = dplyr::recode(modele, bvar = "BVAR seul",
                                       bridge = "Passerelle seule",
                                       combinee = "Combinaison")) %>%
  ggplot2::ggplot(ggplot2::aes(stats::reorder(branche, ratio), ratio,
                               colour = modele, shape = modele)) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  ggplot2::geom_point(size = 2) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "RMSFE / ecart-type de la branche",
                colour = NULL, shape = NULL,
                title = "Qualite de la prevision par branche et par modele",
                subtitle = "a gauche du trait, le modele fait mieux que predire la moyenne")
ggplot2::ggsave(chemin_fig("qualite_par_modele.png"), g1, width = 9, height = 5, dpi = 150)

g2 <- combine %>%
  ggplot2::ggplot(ggplot2::aes(origine, delta)) +
  ggplot2::geom_step(linewidth = 0.4) +
  ggplot2::facet_wrap(~ branche, ncol = 4) +
  ggplot2::ylim(0, 1) +
  ggplot2::labs(x = NULL, y = "delta (poids du BVAR)",
                title = "Poids de combinaison estime a chaque origine",
                subtitle = "delta = 1 : tout le poids au BVAR ; delta = 0 : tout a la passerelle")
ggplot2::ggsave(chemin_fig("poids_delta.png"), g2, width = 11, height = 7, dpi = 150)

g3 <- bridge %>%
  ggplot2::ggplot(ggplot2::aes(origine, n_retenus)) +
  ggplot2::geom_step(linewidth = 0.4) +
  ggplot2::facet_wrap(~ branche, ncol = 4) +
  ggplot2::labs(x = NULL, y = "indicateurs retenus",
                title = "Nombre d'indicateurs selectionnes a chaque origine",
                subtitle = "la liste est recalculee sur la seule information anterieure a la cible")
ggplot2::ggsave(chemin_fig("nombre_indicateurs.png"), g3, width = 11, height = 7, dpi = 150)

g4 <- combine %>%
  dplyr::select(branche, origine, reel, BVAR = bvar, Passerelle = bridge,
                Combinaison = combinee) %>%
  tidyr::pivot_longer(c(BVAR, Passerelle, Combinaison),
                      names_to = "modele", values_to = "prevu") %>%
  dplyr::filter(!is.na(prevu)) %>%
  ggplot2::ggplot(ggplot2::aes(prevu, reel)) +
  ggplot2::geom_abline(linetype = "dashed", linewidth = 0.3) +
  ggplot2::geom_point(size = 0.7, alpha = 0.5) +
  ggplot2::facet_wrap(~ modele) +
  ggplot2::labs(x = "prevu", y = "realise",
                title = "Prevu contre realise, les trois modeles",
                subtitle = "un nuage etire le long de la diagonale signale un modele qui suit")
ggplot2::ggsave(chemin_fig("prevu_realise.png"), g4, width = 10, height = 4.2, dpi = 150)

for (f in c("qualite_par_modele", "poids_delta", "nombre_indicateurs", "prevu_realise")) {
  cat(sprintf("      figures/04_%s.png\n", f))
}
cat("\nPhase 4 terminee.\n")
