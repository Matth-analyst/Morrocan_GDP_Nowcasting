# ============================================================================
# 03c_selection_conjointe.R -- Limite 17.3 : selection JOINTE des hyperparametres
# ============================================================================
# CE QUE FAIT LA PHASE 3
#   Elle choisit les hyperparametres SEQUENTIELLEMENT : d'abord le mode de
#   choix de (p, lambda, d), puis la regle de detection des chocs conditionnee
#   sur ce mode, puis le levier structurel conditionne sur les deux premiers.
#   Soit 3 + 6 + 7 = 16 passes recursives.
#
# CE QUE CE SCRIPT TESTE
#   La selection JOINTE : les 3 x 6 x 7 = 126 combinaisons sont evaluees sans
#   conditionnement. Une selection sequentielle n'est optimale que si les trois
#   axes n'interagissent pas. Rien ne le garantit -- en particulier la fenetre
#   sigma et la regle de choc agissent toutes deux sur la MEME quantite,
#   l'echelle du prior : la regle neutralise les trimestres extremes par des
#   indicatrices, la fenetre les fait sortir de l'echantillon. Il y a donc de
#   bonnes raisons a priori de croire a une interaction entre ces deux axes.
#
# LE PIEGE, ET COMMENT ON L'EVITE
#   Comparer 126 candidats sur les 48 memes origines qui servent ensuite a
#   annoncer la performance, c'est selectionner sur l'echantillon de test. Le
#   maximum de 126 tirages bruites est mecaniquement plus haut que celui de 16,
#   MEME SI TOUS LES CANDIDATS SE VALENT EXACTEMENT. Un gain apparent de la
#   selection jointe ne prouverait donc rien par lui-meme.
#
#   Le script produit pour cette raison DEUX resultats distincts :
#
#     (A) Grille complete sur les 48 origines. Repond a : "le chemin
#         sequentiel a-t-il manque une combinaison ?" Question descriptive,
#         legitime telle quelle.
#
#     (B) Protocole validation / test. Les origines sont coupees en deux.
#         Chaque procedure -- jointe (126 candidats) et sequentielle (16) --
#         choisit sa specification sur la SEULE moitie de validation, puis est
#         evaluee sur la moitie de test, jamais vue. Repond a la vraie
#         question : "elargir l'ensemble de candidats ameliore-t-il la
#         prevision, ou ne fait-il qu'ajuster l'ensemble de selection ?"
#
# SORTIES
#   resultats/03c_grille_conjointe.csv    les 126 combinaisons, 48 origines
#   resultats/03c_interactions.csv        separabilite des trois axes
#   resultats/03c_validation_test.csv     protocole (B)
#   figures/03c_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/bvar.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("03c_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("03c_", x))

P_RETARDS      <- 5L
LAMBDA         <- 0.15
DATES_CHOC     <- as.Date(c("2020-03-31", "2020-06-30", "2020-09-30"))
PREMIERE_CIBLE <- as.Date("2014-06-30")
P_GRILLE       <- 1:5
D_GRILLE       <- c(0.5, 1, 1.5, 2)
BRANCHES_REFERENCE <- c("Industrie de transformation", "Commerce", "Agriculture")
TOLERANCE_RATIO    <- 0.01

cat("\n[1/5] Construction de la matrice des taux de croissance\n")
va <- charger_va() %>%
  dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup()
large <- va %>%
  dplyr::select(branche, date, g) %>%
  tidyr::pivot_wider(names_from = branche, values_from = g) %>%
  dplyr::arrange(date) %>%
  dplyr::filter(dplyr::if_all(dplyr::all_of(TOUTES_BRANCHES), ~ !is.na(.)))
dates_vec <- large$date
Y  <- as.matrix(large[, TOUTES_BRANCHES])
origines <- dates_vec[dates_vec >= PREMIERE_CIBLE]
cat(sprintf("      %d origines, de %s a %s\n", length(origines),
            date_vers_trimestre(origines[1]),
            date_vers_trimestre(origines[length(origines)])))

# --- Les trois axes, repris a l'identique de la phase 3 ----------------------
MODES <- list(
  list(id = "fixe", lab = "hyperparametres fixes (Higgins)"),
  list(id = "ml",   lab = "vraisemblance marginale"),
  list(id = "bgr",  lab = "critere d'ajustement"))

REGLES <- list(
  list(lab = "liste codee en dur", choc = DATES_CHOC, regle = NULL),
  list(lab = "aucune indicatrice", choc = NULL,       regle = NULL),
  list(lab = "regle z=4, k=3", choc = NULL, regle = list(z = 4, k = 3)),
  list(lab = "regle z=5, k=2", choc = NULL, regle = list(z = 5, k = 2)),
  list(lab = "regle z=5, k=3", choc = NULL, regle = list(z = 5, k = 3)),
  list(lab = "regle z=6, k=2", choc = NULL, regle = list(z = 6, k = 2)))

LEVIERS <- list(
  list(lab = "aucun levier",            th = 1,    rh = 1,    fs = Inf),
  list(lab = "theta = 0.50",            th = 0.50, rh = 1,    fs = Inf),
  list(lab = "theta = 0.25",            th = 0.25, rh = 1,    fs = Inf),
  list(lab = "rho = 0.99",              th = 1,    rh = 0.99, fs = Inf),
  list(lab = "rho = 0.97",              th = 1,    rh = 0.97, fs = Inf),
  list(lab = "sigma sur 40 trimestres", th = 1,    rh = 1,    fs = 40),
  list(lab = "sigma sur 60 trimestres", th = 1,    rh = 1,    fs = 60))

# La combinaison que la phase 3 a retenue par le chemin sequentiel.
SEQ_MODE   <- "critere d'ajustement"
SEQ_REGLE  <- "regle z=4, k=3"
SEQ_LEVIER <- "sigma sur 60 trimestres"

grille <- expand.grid(im = seq_along(MODES), ir = seq_along(REGLES),
                      il = seq_along(LEVIERS))
cat(sprintf("      grille conjointe : %d modes x %d regles x %d leviers = %d combinaisons\n",
            length(MODES), length(REGLES), length(LEVIERS), nrow(grille)))

# ============================================================================
# 2) EXECUTION DE LA GRILLE
# ============================================================================
cat("\n[2/5] Execution des passes recursives (parallelise)\n")

passe <- function(im, ir, il) {
  md <- MODES[[im]]; rg <- REGLES[[ir]]; lv <- LEVIERS[[il]]
  purrr::map_dfr(origines, function(cible) {
    r <- prevision_bvar_recursive(
      Y, dates_vec, cible, p = P_RETARDS, lambda = LAMBDA,
      dates_choc = rg$choc, fenetre = Inf, selection = md$id,
      p_grille = P_GRILLE, ref = BRANCHES_REFERENCE, regle_choc = rg$regle,
      d_grille = D_GRILLE, theta = lv$th, rho = lv$rh, fenetre_sigma = lv$fs)
    if (is.null(r)) return(NULL)
    tibble::tibble(
      mode = md$lab, regle = rg$lab, levier = lv$lab,
      origine = cible, derniere_obs = r$derniere_obs,
      branche = names(r$prevision),
      prevision = as.numeric(r$prevision),
      reel = as.numeric(Y[dates_vec == cible, ]))
  })
}

t0 <- Sys.time()
n_coeurs <- max(1L, min(3L, parallel::detectCores() - 1L))
cat(sprintf("      %d coeurs\n", n_coeurs))
cl <- parallel::makeCluster(n_coeurs)
rep_travail <- getwd()
parallel::clusterExport(cl, "rep_travail", envir = environment())
parallel::clusterEvalQ(cl, {
  setwd(rep_travail)
  suppressMessages({
    source("R/00_setup.R"); source("R/fonctions/information_set.R")
    source("R/fonctions/transformations.R"); source("R/fonctions/donnees.R")
    source("R/fonctions/bvar.R")
  })
  NULL
})
parallel::clusterExport(cl, c("Y", "dates_vec", "origines", "MODES", "REGLES",
                              "LEVIERS", "P_RETARDS", "LAMBDA", "P_GRILLE",
                              "D_GRILLE", "BRANCHES_REFERENCE", "passe", "grille"),
                        envir = environment())

resultats_bruts <- parallel::parLapply(cl, seq_len(nrow(grille)), function(i) {
  passe(grille$im[i], grille$ir[i], grille$il[i])
})
parallel::stopCluster(cl)

previsions <- dplyr::bind_rows(resultats_bruts)
cat(sprintf("      %s | %s previsions\n",
            format(round(difftime(Sys.time(), t0, units = "mins"), 1)),
            format(nrow(previsions), big.mark = " ")))

stopifnot("[ANTI-LOOK-AHEAD] une estimation a vu sa cible" =
            all(previsions$derniere_obs < previsions$origine))
cat("      controle : aucune estimation ne contient sa propre cible\n")

previsions <- previsions %>% dplyr::mutate(erreur = reel - prevision)

# ============================================================================
# 3) BILAN SUR LES 48 ORIGINES  (resultat A)
# ============================================================================
cat("\n[3/5] Bilan de la grille conjointe sur toutes les origines\n")

#' Ratio et correlation par branche, puis mediane sur les 16 branches.
bilan <- function(df) {
  df %>%
    dplyr::group_by(mode, regle, levier, branche) %>%
    dplyr::summarise(ratio = sqrt(mean(erreur^2)) / stats::sd(reel),
                     correlation = suppressWarnings(stats::cor(prevision, reel)),
                     .groups = "drop") %>%
    dplyr::group_by(mode, regle, levier) %>%
    dplyr::summarise(ratio_median   = stats::median(ratio),
                     n_branches_ok  = sum(ratio < 1),
                     correl_mediane = stats::median(correlation, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::arrange(ratio_median)
}

#' Regle de departage de la phase 3, appliquee a l'identique.
departager <- function(b) {
  seuil <- min(b$ratio_median) + TOLERANCE_RATIO
  b %>% dplyr::filter(ratio_median <= seuil) %>%
    dplyr::arrange(dplyr::desc(correl_mediane)) %>% dplyr::slice(1)
}

bilan_complet <- bilan(previsions)
ecrire_csv(bilan_complet, chemin_res("grille_conjointe.csv"))

gagnant <- departager(bilan_complet)
rang_de <- function(b, m, r, l) which(b$mode == m & b$regle == r & b$levier == l)
sequentiel <- bilan_complet %>%
  dplyr::filter(mode == SEQ_MODE, regle == SEQ_REGLE, levier == SEQ_LEVIER)
rang_seq <- rang_de(bilan_complet, SEQ_MODE, SEQ_REGLE, SEQ_LEVIER)

cat("\n      --- les huit meilleures combinaisons sur 126 ---\n")
print(bilan_complet %>% utils::head(8) %>%
        dplyr::transmute(mode = substr(mode, 1, 22), regle = substr(regle, 1, 18),
                         levier = substr(levier, 1, 22),
                         `ratio med.` = round(ratio_median, 3),
                         `br. < 1` = n_branches_ok,
                         `correl.` = round(correl_mediane, 2)), n = 8)

cat("\n      combinaison retenue par la selection SEQUENTIELLE (phase 3) :\n")
cat(sprintf("        %s | %s | %s\n", SEQ_MODE, SEQ_REGLE, SEQ_LEVIER))
cat(sprintf("        ratio %.3f | %d branches < 1 | correl %.2f | rang %d sur %d\n",
            sequentiel$ratio_median, sequentiel$n_branches_ok,
            sequentiel$correl_mediane, rang_seq, nrow(bilan_complet)))
cat("      combinaison retenue par la selection CONJOINTE :\n")
cat(sprintf("        %s | %s | %s\n", gagnant$mode, gagnant$regle, gagnant$levier))
cat(sprintf("        ratio %.3f | %d branches < 1 | correl %.2f\n",
            gagnant$ratio_median, gagnant$n_branches_ok, gagnant$correl_mediane))

# --- Les axes sont-ils separables ? ------------------------------------------
# Si les axes ne s'influencaient pas, le meilleur levier serait le meme quels
# que soient le mode et la regle. On compte donc, pour chaque axe, combien de
# fois chaque niveau l'emporte quand on fait varier les deux autres. Un axe
# separable donne un seul gagnant partout ; un axe qui interagit repartit ses
# victoires entre plusieurs niveaux.
interactions <- dplyr::bind_rows(
  bilan_complet %>% dplyr::group_by(mode, regle) %>%
    dplyr::slice_min(ratio_median, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>% dplyr::count(axe = "levier optimal", valeur = levier),
  bilan_complet %>% dplyr::group_by(mode, levier) %>%
    dplyr::slice_min(ratio_median, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>% dplyr::count(axe = "regle optimale", valeur = regle),
  bilan_complet %>% dplyr::group_by(regle, levier) %>%
    dplyr::slice_min(ratio_median, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>% dplyr::count(axe = "mode optimal", valeur = mode)) %>%
  dplyr::arrange(axe, dplyr::desc(n))
ecrire_csv(interactions, chemin_res("interactions.csv"))

cat("\n      --- separabilite : combien de fois chaque niveau gagne ---\n")
print(interactions, n = 40)

# ============================================================================
# 4) PROTOCOLE VALIDATION / TEST  (resultat B)
# ============================================================================
cat("\n[4/5] Protocole validation / test\n")
# Les origines sont coupees chronologiquement en deux moities. La premiere sert
# a CHOISIR, la seconde a MESURER. La coupure chronologique reproduit la
# situation reelle d'un praticien : il calibre sur ce qu'il a, et subit la
# suite. Elle a un cout -- la moitie de test contient 2020, la moitie de
# validation non -- mais toute autre coupure ferait fuir de l'information
# posterieure dans le choix de la specification.
coupure <- origines[floor(length(origines) / 2)]
valid <- previsions %>% dplyr::filter(origine <= coupure)
test  <- previsions %>% dplyr::filter(origine >  coupure)
cat(sprintf("      validation : %d origines (%s -> %s)\n",
            dplyr::n_distinct(valid$origine),
            date_vers_trimestre(min(valid$origine)),
            date_vers_trimestre(max(valid$origine))))
cat(sprintf("      test       : %d origines (%s -> %s)\n",
            dplyr::n_distinct(test$origine),
            date_vers_trimestre(min(test$origine)),
            date_vers_trimestre(max(test$origine))))

bilan_valid <- bilan(valid)
bilan_test  <- bilan(test)

# --- procedure CONJOINTE : un seul argmax sur les 126 -----------------------
sel_conj <- departager(bilan_valid)

# --- procedure SEQUENTIELLE : le chemin de la phase 3, refait sur validation -
# Etape 1 : le mode, a regle et levier de reference.
e1 <- bilan_valid %>%
  dplyr::filter(regle == "liste codee en dur", levier == "aucun levier") %>%
  departager()
# Etape 2 : la regle, conditionnee sur le mode retenu.
e2 <- bilan_valid %>%
  dplyr::filter(mode == e1$mode, levier == "aucun levier") %>% departager()
# Etape 3 : le levier, conditionne sur les deux precedents.
sel_seq <- bilan_valid %>%
  dplyr::filter(mode == e1$mode, regle == e2$regle) %>% departager()

perf_test <- function(s) {
  bilan_test %>% dplyr::filter(mode == s$mode, regle == s$regle,
                               levier == s$levier)
}
pt_conj <- perf_test(sel_conj)
pt_seq  <- perf_test(sel_seq)

vt <- dplyr::bind_rows(
  dplyr::mutate(sel_seq,  procedure = "sequentielle (16 candidats)", phase = "validation"),
  dplyr::mutate(sel_conj, procedure = "conjointe (126 candidats)",   phase = "validation"),
  dplyr::mutate(pt_seq,   procedure = "sequentielle (16 candidats)", phase = "test"),
  dplyr::mutate(pt_conj,  procedure = "conjointe (126 candidats)",   phase = "test")) %>%
  dplyr::select(procedure, phase, mode, regle, levier, ratio_median,
                n_branches_ok, correl_mediane)
ecrire_csv(vt, chemin_res("validation_test.csv"))

cat("\n      --- specification choisie sur la VALIDATION ---\n")
cat(sprintf("        sequentielle : %s | %s | %s\n",
            sel_seq$mode, sel_seq$regle, sel_seq$levier))
cat(sprintf("        conjointe    : %s | %s | %s\n",
            sel_conj$mode, sel_conj$regle, sel_conj$levier))
cat("\n      --- performance sur la moitie de TEST, jamais vue ---\n")
print(vt %>% dplyr::filter(phase == "test") %>%
        dplyr::transmute(procedure, `ratio med.` = round(ratio_median, 3),
                         `br. < 1` = n_branches_ok,
                         `correl.` = round(correl_mediane, 2)), n = 4)

# Le meilleur possible sur le test : borne inatteignable en pratique, mais elle
# dit combien la selection laisse sur la table.
oracle <- bilan_test %>% dplyr::slice_min(ratio_median, n = 1, with_ties = FALSE)
cat(sprintf("\n      oracle sur le test (inatteignable) : ratio %.3f | correl %.2f\n",
            oracle$ratio_median, oracle$correl_mediane))
cat(sprintf("      ecart conjointe - sequentielle sur le test : ratio %+.3f | correl %+.2f\n",
            pt_conj$ratio_median - pt_seq$ratio_median,
            pt_conj$correl_mediane - pt_seq$correl_mediane))

# ============================================================================
# 5) FIGURES
# ============================================================================
cat("\n[5/5] Figures\n")

g1 <- bilan_complet %>%
  dplyr::mutate(rang = dplyr::row_number(),
                surligne = dplyr::case_when(
                  rang == rang_seq ~ "chemin sequentiel (phase 3)",
                  rang == 1        ~ "meilleure combinaison",
                  TRUE             ~ "autres combinaisons")) %>%
  ggplot2::ggplot(ggplot2::aes(rang, ratio_median, colour = surligne,
                               size = surligne)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  ggplot2::scale_colour_manual(values = c(
    "autres combinaisons" = "grey72",
    "chemin sequentiel (phase 3)" = "#b03a2e",
    "meilleure combinaison" = "#1f4e79")) +
  ggplot2::scale_size_manual(values = c(
    "autres combinaisons" = 1.2, "chemin sequentiel (phase 3)" = 3,
    "meilleure combinaison" = 3), guide = "none") +
  ggplot2::labs(x = "rang de la combinaison", y = "ratio median", colour = NULL,
                title = "Les 126 combinaisons, classees",
                subtitle = "ou se situe le chemin sequentiel de la phase 3")
ggplot2::ggsave(chemin_fig("classement_combinaisons.png"), g1,
                width = 9, height = 5, dpi = 150)

g2 <- bilan_complet %>%
  ggplot2::ggplot(ggplot2::aes(levier, ratio_median)) +
  ggplot2::geom_boxplot(outlier.size = 0.6, linewidth = 0.3) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  ggplot2::facet_wrap(~ mode) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "ratio median",
                title = "Interaction entre le levier structurel et le mode de choix",
                subtitle = "chaque boite resume les six regles de detection")
ggplot2::ggsave(chemin_fig("interaction_levier_mode.png"), g2,
                width = 10, height = 4.5, dpi = 150)

g3 <- vt %>%
  ggplot2::ggplot(ggplot2::aes(phase, ratio_median, group = procedure,
                               colour = procedure)) +
  ggplot2::geom_line(linewidth = 0.6) + ggplot2::geom_point(size = 2.5) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  ggplot2::labs(x = NULL, y = "ratio median", colour = NULL,
                title = "L'avantage de la selection jointe survit-il hors de son echantillon de choix ?",
                subtitle = "un ecart qui se resserre entre validation et test est un ecart surajuste")
ggplot2::ggsave(chemin_fig("validation_test.png"), g3,
                width = 8.5, height = 4.5, dpi = 150)

cat("      figures/03c_classement_combinaisons.png\n")
cat("      figures/03c_interaction_levier_mode.png\n")
cat("      figures/03c_validation_test.png\n")
cat("\nSelection conjointe terminee.\n")
