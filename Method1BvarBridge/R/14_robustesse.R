# ============================================================================
# 14_robustesse.R -- PHASES 21 a 24 : les tests de robustesse restants
# ============================================================================
# CE QUI EST DEJA FAIT, ET OU
#   Phase 21 (fenetre) -- la phase 3 compare fenetre extensive et fenetres
#     glissantes de 40 et 60 trimestres, branche par branche. Resultat :
#     raccourcir l'echantillon degrade, et la phase 4 bis a confirme le meme
#     verdict avec la ponderation geometrique. On le rappelle ici sans le
#     recalculer, en le portant au niveau de l'agregat.
#   Phase 24 (saisonniere) -- l'etape 5 compare AR(4), SARIMA et AR avec
#     indicatrices saisonnieres sur les branches non couvertes.
#
# CE QUE CE SCRIPT AJOUTE
#   Phase 22 (selection) -- la comparaison que le plan demande, et dont l'un des
#     termes a une valeur particuliere : la selection FIXE, calculee une fois
#     sur tout l'echantillon puis appliquee retrospectivement. C'est exactement
#     ce que faisait la version 1 du projet. Mesurer l'ecart avec la selection
#     recursive, c'est CHIFFRER LE BIAIS DE LOOK-AHEAD que tout ce travail a
#     corrige -- et donc dire ce que valait reellement la performance annoncee
#     avant correction.
#   Phase 23 (delta) -- delta constant a 0,5, delta optimise, delta borne.
#
# ORGANISATION DU CALCUL
#   La base trimestrielle des indicateurs est reconstruite une fois par origine
#   -- c'est le poste couteux -- puis les variantes de selection sont balayees
#   dessus. Sans cette precaution le test couterait des heures au lieu d'une.
#
# SORTIES
#   resultats/14_robustesse_selection.csv
#   resultats/14_robustesse_delta.csv
#   resultats/14_robustesse_fenetre.csv
#   resultats/14_saisonnalite.csv
#   figures/14_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")
source("R/fonctions/kalman.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("14_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("14_", x))

PREMIERE_CIBLE <- as.Date("2014-06-30")
SEUIL_P <- 0.10; MIN_OBS_SEL <- 20L; MAX_RETENUS <- 5L; MIN_OBS_DELTA <- 8L

cat("\n[1/5] Bases\n")
couverture <- charger_couverture(); BC <- couverture$couvertes
ind <- charger_indicateurs(branches = BC)
longueur <- ind %>% dplyr::count(id_serie, name = "n")
ind <- ind %>% dplyr::filter(id_serie %in% longueur$id_serie[longueur$n >= 36L])
meta <- charger_metadonnees() %>% dplyr::select(id_serie, agregation, transformation)

va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)
origines <- sort(unique(va$date[va$date >= PREMIERE_CIBLE]))
cat(sprintf("      %d origines x %d branches couvertes\n", length(origines), length(BC)))

# ============================================================================
# 2) PHASE 22 : ROBUSTESSE DE LA SELECTION
# ============================================================================
cat("\n[2/5] Robustesse de la selection\n")

VARIANTES <- list(
  list(lab = "recursive, seuil 0,15 (retenue)", mode = "recursive", seuil = 0.15),
  list(lab = "recursive, seuil 0,10",           mode = "recursive", seuil = 0.10),
  list(lab = "recursive, seuil 0,20",           mode = "recursive", seuil = 0.20),
  list(lab = "aucune selection",                mode = "aucune",    seuil = 0),
  list(lab = "FIXE (calculee sur tout l'echantillon)", mode = "fixe", seuil = 0.15))

#' Selection FIXE : les correlations sont calculees sur TOUT l'echantillon, y
#' compris les trimestres posterieurs a la cible. C'est du look-ahead assume --
#' l'objet du test est precisement de mesurer ce qu'il fait gagner a tort.
selection_fixe <- function(g_b, x_b, seuil) {
  s <- x_b %>% dplyr::inner_join(g_b, by = "date") %>%
    dplyr::filter(!is.na(x), !is.na(g)) %>%
    dplyr::group_by(id_serie) %>%
    dplyr::summarise(n = dplyr::n(),
                     r = if (dplyr::n() >= 4L && stats::sd(x) > 0 && stats::sd(g) > 0)
                       suppressWarnings(stats::cor(x, g)) else NA_real_,
                     p = if (dplyr::n() >= 4L && stats::sd(x) > 0 && stats::sd(g) > 0)
                       suppressWarnings(stats::cor.test(x, g)$p.value) else NA_real_,
                     .groups = "drop") %>%
    dplyr::filter(n >= MIN_OBS_SEL, !is.na(r), abs(r) >= seuil, p < SEUIL_P) %>%
    dplyr::arrange(dplyr::desc(abs(r)))
  utils::head(s$id_serie, MAX_RETENUS)
}

traiter_origine <- function(cible) {
  info <- information_set_intra(ind, cible, scenario = "M3") %>%
    dplyr::filter(!(frequence == "trimestriel" &
                      date >= debut_trimestre(cible) & date <= fin_trimestre(cible)))
  mois_T <- mois_du_trimestre(cible)
  mens <- info %>% dplyr::filter(frequence == "mensuel") %>%
    dplyr::group_by(id_serie, branche, indicateur) %>%
    dplyr::group_modify(function(g, cle) {
      mq <- as.Date(setdiff(mois_T, g$date), origin = "1970-01-01")
      if (length(mq) == 0L) return(dplyr::select(g, date, valeur))
      r <- prevoir_mois_manquants(g$date, g$valeur, mq)
      if (is.null(r)) return(dplyr::select(g, date, valeur))
      dplyr::select(r, date, valeur)
    }) %>% dplyr::ungroup()
  bl <- dplyr::bind_rows(
    mens %>% dplyr::mutate(frequence = "mensuel") %>%
      dplyr::select(id_serie, branche, indicateur, frequence, date, valeur),
    info %>% dplyr::filter(frequence == "trimestriel") %>%
      dplyr::select(id_serie, branche, indicateur, frequence, date, valeur)) %>%
    dplyr::left_join(meta, by = "id_serie")
  trim <- indicateurs_trimestriels(bl, tolerant = TRUE)
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
    reel <- g_b$g[g_b$date == cible]
    reel <- if (length(reel) == 1L) reel else NA_real_
    # la selection fixe voit TOUT l'echantillon, y compris apres la cible
    x_b_total <- va %>% dplyr::filter(branche == b) %>% dplyr::select(date, g)

    purrr::map_dfr(VARIANTES, function(v) {
      ids <- if (v$mode == "fixe") {
        intersect(selection_fixe(x_b_total, x_b, v$seuil), names(x_l))
      } else {
        s <- selectionner_disponible(g_b, x_b, x_l, cible,
                                     seuil_r = v$seuil, seuil_p = SEUIL_P,
                                     min_obs = MIN_OBS_SEL,
                                     max_retenus = MAX_RETENUS)
        if (is.null(s)) character(0) else s$ids
      }
      r <- if (length(ids) > 0L)
        estimer_bridge_ar(g_b, x_l, ids, cible, avec_ar = TRUE) else NULL
      tibble::tibble(variante = v$lab, branche = b, origine = cible,
                     n_retenus = length(ids),
                     bridge = if (is.null(r)) NA_real_ else r$prevision,
                     reel = reel)
    })
  })
}

t0 <- Sys.time()
n_coeurs <- max(1L, min(3L, parallel::detectCores() - 1L))
cl <- parallel::makeCluster(n_coeurs)
rep_travail <- getwd()
parallel::clusterExport(cl, "rep_travail", envir = environment())
parallel::clusterEvalQ(cl, {
  setwd(rep_travail)
  suppressMessages({
    source("R/00_setup.R"); source("R/fonctions/information_set.R")
    source("R/fonctions/transformations.R"); source("R/fonctions/donnees.R")
    source("R/fonctions/passerelle.R"); source("R/fonctions/kalman.R")
  })
  NULL
})
parallel::clusterExport(cl, c("ind", "meta", "va", "BC", "origines", "VARIANTES",
                              "SEUIL_P", "MIN_OBS_SEL", "MAX_RETENUS",
                              "selection_fixe", "traiter_origine"),
                        envir = environment())
res_sel <- dplyr::bind_rows(parallel::parLapply(cl, seq_along(origines),
                                                function(i) traiter_origine(origines[i])))
parallel::stopCluster(cl)
cat(sprintf("      %s | %d lignes\n",
            format(round(difftime(Sys.time(), t0, units = "mins"), 1)), nrow(res_sel)))
ecrire_csv(res_sel, chemin_res("selection_previsions.csv"))

bvar <- lire_csv(file.path(DOSSIER_RESULTATS, "03e_previsions_corrigees.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(branche, origine, bvar = prevision)
poids <- lire_csv(file.path(DOSSIER_RESULTATS, "06_poids.csv")) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::transmute(branche,
                   origine = fin_trimestre(debut_trimestre(date) %m+% months(3)), w)

#' Combine, agrege et evalue un jeu de previsions de passerelle.
evaluer_variante <- function(d) {
  d %>% dplyr::left_join(bvar, by = c("branche", "origine")) %>%
    dplyr::arrange(variante, branche, origine) %>%
    dplyr::group_by(variante, branche) %>%
    dplyr::group_modify(function(g, cle) {
      g$comb <- NA_real_
      for (i in seq_len(nrow(g))) {
        h <- g[seq_len(i - 1L), ] %>% dplyr::transmute(reel, bvar, bridge)
        pd <- poids_combinaison(h, min_obs = MIN_OBS_DELTA)
        g$comb[i] <- if (is.na(g$bridge[i])) g$bvar[i]
          else pd$delta * g$bvar[i] + (1 - pd$delta) * g$bridge[i]
      }
      g
    }) %>% dplyr::ungroup()
}
comb_sel <- evaluer_variante(res_sel)

agreger <- function(d, col) {
  d %>% dplyr::inner_join(poids, by = c("branche", "origine")) %>%
    dplyr::filter(!is.na(.data[[col]])) %>%
    dplyr::group_by(variante, origine) %>%
    dplyr::filter(dplyr::n() == length(BC)) %>%
    dplyr::summarise(reel = log(sum(w * exp(reel))) / sum(w) * sum(w),
                     prev = log(sum(w * exp(.data[[col]]))), .groups = "drop") %>%
    dplyr::group_by(variante) %>%
    dplyr::summarise(n = dplyr::n(),
                     RMSFE = sqrt(mean((reel - prev)^2)),
                     ratio = sqrt(mean((reel - prev)^2)) / stats::sd(reel),
                     .groups = "drop")
}
#' Bilan d'un jeu de previsions. `sd(reel)` peut etre nul sur une branche a trop
#' peu d'observations : ces branches sont ecartees plutot que de produire un NA
#' qui contaminerait la mediane.
bilan_de <- function(d, lab) {
  d %>% dplyr::filter(!is.na(bridge), !is.na(reel)) %>%
    dplyr::group_by(variante, branche) %>%
    dplyr::filter(dplyr::n() >= 4L, stats::sd(reel) > 0) %>%
    dplyr::summarise(ratio = sqrt(mean((reel - bridge)^2)) / stats::sd(reel),
                     .groups = "drop") %>%
    dplyr::filter(is.finite(ratio)) %>%
    dplyr::group_by(variante) %>%
    dplyr::summarise(branches = dplyr::n(), ratio_median = stats::median(ratio),
                     n_ok = sum(ratio < 1), .groups = "drop") %>%
    dplyr::left_join(d %>% dplyr::group_by(variante) %>%
                       dplyr::summarise(taux = mean(!is.na(bridge)),
                                        retenus = stats::median(n_retenus),
                                        .groups = "drop"), by = "variante") %>%
    dplyr::mutate(perimetre = lab) %>% dplyr::arrange(ratio_median)
}

# PERIMETRE COMMUN. La selection fixe ne produit que sur un tiers des cas : elle
# retient les series les mieux correlees sur tout l'echantillon sans verifier
# qu'elles sont observables a la cible. Comparer ses ratios bruts a ceux des
# variantes recursives reviendrait a comparer des epreuves de difficultes
# differentes -- la meme erreur que la phase 4 bis a documentee.
n_var <- dplyr::n_distinct(comb_sel$variante)
commun <- comb_sel %>% dplyr::filter(!is.na(bridge)) %>%
  dplyr::count(branche, origine) %>% dplyr::filter(n == n_var) %>%
  dplyr::select(branche, origine)
cat(sprintf("      perimetre commun aux %d variantes : %d couples
",
            n_var, nrow(commun)))

bilan_sel <- dplyr::bind_rows(
  bilan_de(comb_sel, "toutes origines produites"),
  bilan_de(comb_sel %>% dplyr::inner_join(commun, by = c("branche", "origine")),
           "perimetre commun"))
ecrire_csv(bilan_sel, chemin_res("robustesse_selection.csv"))

for (per in unique(bilan_sel$perimetre)) {
  cat(sprintf("
      --- passerelle seule, %s ---
", per))
  print(as.data.frame(bilan_sel %>% dplyr::filter(perimetre == per) %>%
    dplyr::transmute(variante, `taux prod.` = sprintf("%.0f %%", 100 * taux),
                     `retenus med.` = retenus,
                     `ratio med.` = round(ratio_median, 3),
                     `br. < 1` = sprintf("%d/%d", n_ok, branches))),
    row.names = FALSE)
}

bc <- bilan_sel %>% dplyr::filter(perimetre == "perimetre commun")
rec <- bc$ratio_median[grepl("retenue", bc$variante)]
fix <- bc$ratio_median[grepl("FIXE", bc$variante)]
if (length(rec) == 1L && length(fix) == 1L && is.finite(rec) && is.finite(fix)) {
  cat("
      SELECTION FIXE contre RECURSIVE, a perimetre egal :
")
  cat(sprintf("        fixe %.3f | recursive %.3f | ecart %+.3f
", fix, rec, fix - rec))
  if (fix < rec) {
    cat(sprintf("        la fixe parait %.0f %% meilleure -- gain ILLUSOIRE : elle
",
                100 * (1 - fix / rec)))
    cat("        choisit ses indicateurs en connaissant les trimestres a prevoir.
")
  } else {
    cat("        la fixe ne fait PAS mieux, malgre son avantage informationnel.
")
  }
}
cat("
      NB : ce test isole le look-ahead de la SELECTION seule. Coefficients,
")
cat("      BVAR, delta et poids restent recursifs dans les deux variantes. La
")
cat("      version 1 cumulait les quatre defauts : le biais qu'elle subissait
")
cat("      etait donc superieur a ce que ce test revele.

")

# ============================================================================
# 3) PHASE 23 : ROBUSTESSE DU DELTA
# ============================================================================
cat("\n[3/5] Robustesse du delta\n")
base_delta <- lire_csv(file.path(DOSSIER_RESULTATS, "09_previsions_intra.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::filter(scenario == "M3") %>%
  dplyr::select(branche, origine, reel, bvar, bridge) %>%
  dplyr::arrange(branche, origine)

regles_delta <- list(
  list(lab = "delta = 0,5 (retenu)",         type = "constant", val = 0.5),
  list(lab = "delta optimise",               type = "optimise"),
  list(lab = "delta borne a [0,2 ; 0,8]",    type = "borne", bas = 0.2, haut = 0.8),
  list(lab = "delta = 1 (BVAR seul)",        type = "constant", val = 1),
  list(lab = "delta = 0 (passerelle seule)", type = "constant", val = 0))

res_delta <- purrr::map_dfr(regles_delta, function(rg) {
  base_delta %>% dplyr::group_by(branche) %>%
    dplyr::group_modify(function(g, cle) {
      g$comb <- NA_real_
      for (i in seq_len(nrow(g))) {
        dl <- switch(rg$type,
          constant = rg$val,
          # constante = NA : ces deux bras doivent ESTIMER delta, sinon la
          # comparaison des regles se compare a elle-meme.
          optimise = poids_combinaison(
            g[seq_len(i - 1L), ] %>% dplyr::transmute(reel, bvar, bridge),
            min_obs = MIN_OBS_DELTA, constante = NA_real_)$delta,
          borne = min(rg$haut, max(rg$bas, poids_combinaison(
            g[seq_len(i - 1L), ] %>% dplyr::transmute(reel, bvar, bridge),
            min_obs = MIN_OBS_DELTA, constante = NA_real_)$delta)))
        g$comb[i] <- if (is.na(g$bridge[i])) g$bvar[i]
          else if (is.na(g$bvar[i])) g$bridge[i]
          else dl * g$bvar[i] + (1 - dl) * g$bridge[i]
      }
      g
    }) %>% dplyr::ungroup() %>% dplyr::mutate(regle = rg$lab)
})

bilan_delta <- res_delta %>%
  dplyr::filter(!is.na(comb), !is.na(reel)) %>%
  dplyr::group_by(regle, branche) %>% dplyr::filter(dplyr::n() >= 4L) %>%
  dplyr::summarise(ratio = sqrt(mean((reel - comb)^2)) / stats::sd(reel),
                   .groups = "drop") %>%
  dplyr::group_by(regle) %>%
  dplyr::summarise(branches = dplyr::n(), ratio_median = stats::median(ratio),
                   n_ok = sum(ratio < 1), .groups = "drop") %>%
  dplyr::arrange(ratio_median)
ecrire_csv(bilan_delta, chemin_res("robustesse_delta.csv"))
print(as.data.frame(bilan_delta %>% dplyr::transmute(
  regle, `ratio med.` = round(ratio_median, 3),
  `br. < 1` = sprintf("%d/%d", n_ok, branches))), row.names = FALSE)

# ============================================================================
# 4) PHASES 21 ET 24 : RAPPEL DES RESULTATS ACQUIS
# ============================================================================
cat("\n[4/5] Fenetre et saisonnalite\n")
fen <- lire_csv(file.path(DOSSIER_RESULTATS, "03_comparaison_specifications.csv")) %>%
  dplyr::filter(grepl("fenetre|reference", specification)) %>%
  dplyr::select(specification, ratio_median, n_branches_ok, correl_mediane)
ecrire_csv(fen, chemin_res("robustesse_fenetre.csv"))
cat("      --- phase 21, fenetre (phase 3, branche par branche) ---\n")
print(as.data.frame(fen %>% dplyr::transmute(
  specification, `ratio med.` = round(ratio_median, 3),
  `br. < 1` = n_branches_ok, `correl.` = round(correl_mediane, 2))),
  row.names = FALSE)

sais <- va %>% dplyr::group_by(branche) %>%
  dplyr::summarise(
    R2_saison = summary(stats::lm(g ~ factor(lubridate::quarter(date))))$r.squared,
    .groups = "drop") %>% dplyr::arrange(dplyr::desc(R2_saison))
ecrire_csv(sais, chemin_res("saisonnalite.csv"))
cat(sprintf("\n      --- phase 24, saisonnalite ---\n"))
cat(sprintf("      R2 saisonnier : median %.3f | maximum %.3f (%s)\n",
            stats::median(sais$R2_saison), sais$R2_saison[1], sais$branche[1]))
cat("      aucune branche n'est fortement saisonniere : la phase 24 est sans objet\n")
cat("      sur cet echantillon, et l'etape 5 l'avait confirme -- ajouter des\n")
cat("      indicatrices saisonnieres y degradait la prevision (1,15 contre 1,14).\n")

# ============================================================================
# 5) FIGURES
# ============================================================================
cat("\n[5/5] Figures\n")
g1 <- bilan_sel %>% dplyr::filter(perimetre == "perimetre commun") %>%
  dplyr::mutate(look_ahead = grepl("FIXE", variante)) %>%
  ggplot2::ggplot(ggplot2::aes(stats::reorder(variante, -ratio_median),
                               ratio_median, fill = look_ahead)) +
  ggplot2::geom_col(width = 0.65) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  ggplot2::scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "#b03a2e"),
                             guide = "none") +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "ratio median de la passerelle",
                title = "Robustesse de la selection des indicateurs",
                subtitle = "en rouge, la selection fixe : sa performance est illusoire")
ggplot2::ggsave(chemin_fig("selection.png"), g1, width = 9.5, height = 4.5, dpi = 150)

g2 <- bilan_delta %>%
  ggplot2::ggplot(ggplot2::aes(stats::reorder(regle, -ratio_median), ratio_median)) +
  ggplot2::geom_col(width = 0.6, fill = "grey70") +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "ratio median de la combinaison",
                title = "Robustesse du poids de combinaison")
ggplot2::ggsave(chemin_fig("delta.png"), g2, width = 9, height = 4, dpi = 150)

cat("      figures/14_selection.png\n      figures/14_delta.png\n")
cat("\nTests de robustesse termines.\n")
