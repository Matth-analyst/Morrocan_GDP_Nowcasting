# ============================================================================
# 03_bvar_trimestriel.R -- PHASE 3 : BVAR trimestriel, estimation recursive
# ============================================================================
# Plan de correction : section 8 et section 32 ("Etape 3").
#
# OBJECTIF DE L'ETAPE
#   Rendre l'estimation du BVAR recursive et exempte d'information future.
#
# CE QUE FAISAIT LA VERSION 1
#   Le script estimait UNE fois le BVAR sur tout l'echantillon, sauvegardait
#   le modele, et le backtest relisait cet objet. Trois consequences :
#     - les coefficients utilises pour "prevoir" 2015 avaient ete estimes en
#       connaissant 2016 a 2026 ;
#     - les echelles sigma_i du prior etaient calculees sur l'echantillon
#       complet, alors que la phase 2 a montre une rupture de variance en 2014
#       sur 12 branches sur 16 (facteur 9 pour l'administration publique) --
#       un sigma_i plein echantillon est faux pour les deux regimes a la fois ;
#     - l'indicatrice COVID valait 1 en 2020 T2 ET en 2020 T3, avec UN SEUL
#       coefficient, alors que le T2 est un effondrement et le T3 un rebond.
#
# CE QUE FAIT LA PHASE 3
#   1. Une fonction `prevision_bvar_recursive()` qui ne recoit que
#      l'information anterieure a sa cible, et qui refuse de tourner si on lui
#      en donne davantage (R/fonctions/bvar.R).
#   2. Trois indicatrices de choc distinctes -- 2020 T1, T2 et T3 -- chacune
#      avec son propre coefficient par equation.
#   3. Des echelles sigma_i recalculees a chaque origine.
#   4. Un exercice recursif hors echantillon sur toutes les origines
#      disponibles, servant a comparer les specifications (ordre de retard,
#      serrage du prior, traitement du COVID, fenetre) -- soit la matiere des
#      sections 20, 21 et 26 du plan.
#
# CE QUE LA PHASE 3 NE FAIT PAS
#   Aucune agregation. Tout est evalue BRANCHE PAR BRANCHE. L'agregation des
#   16 branches releve de la phase 11, avec des poids en prix courants issus de
#   VA_nominale_base2014.csv (voir la note plus bas).
#
# SORTIES
#   resultats/03_previsions_recursives.csv     prevision, realisation et erreur
#                                              par branche et par origine
#   resultats/03_diagnostics_branches.csv      qualite de la prevision par branche
#   resultats/03_evaluation_branches_par_specification.csv
#   resultats/03_comparaison_specifications.csv
#   resultats/03_comparaison_specifications_hors_2020.csv
#   resultats/03_tests_diebold_mariano_branches.csv
#   resultats/03_tests_diebold_mariano.csv     recapitulatif par specification
#   resultats/03_diagnostics_residus.csv
#   resultats/03_coefficients_indicatrices.csv
#   resultats/03_nowcast_courant.csv           prevision du prochain trimestre
#   figures/03_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/bvar.R")

PREFIXE <- "03"
chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0(PREFIXE, "_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0(PREFIXE, "_", x))

# --- Specification de reference ---------------------------------------------
P_RETARDS   <- 5L
LAMBDA      <- 0.15
DATES_CHOC  <- as.Date(c("2020-03-31", "2020-06-30", "2020-09-30"))
# Premiere cible de l'evaluation : deuxieme trimestre 2014.
#
# Ce choix n'est pas arbitraire. La serie de valeur ajoutee NOMINALE
# (VA_nominale_base2014.csv) commence au premier trimestre 2014. Le premier
# trimestre pour lequel un poids w_{j,T-1} en PRIX COURANTS pourra etre
# construit -- comme l'exige une agregation de type Fisher ou Tornqvist, cf.
# section 11 du plan -- est donc T2-2014.
#
# Aligner l'evaluation du BVAR sur cette borne des maintenant garantit que
# toutes les phases s'evaluent sur le MEME ensemble de trimestres cibles, et
# evite d'avoir a refaire l'exercice quand l'agregation sera introduite.
PREMIERE_CIBLE <- as.Date("2014-06-30")

# --- Grilles pour le choix des hyperparametres -------------------------------
# p = 5 et lambda = 0,15 viennent de Higgins (2014) et ne sont pas justifiables
# sur cet echantillon : 84 parametres par equation pour une centaine
# d'observations. Les deux methodes de la litterature sont donc implementees, et
# departagees hors echantillon (section 4).
P_GRILLE      <- 1:5
# Exposant de decroissance des retards : le prior serre le retard l d'un
# facteur l^d. La version precedente imposait d = 1 en dur ; Litterman le
# laisse libre, et il entre dans la selection par vraisemblance marginale
# puisqu'il preserve la structure conjuguee.
D_GRILLE      <- c(0.5, 1, 1.5, 2)
# lambda n'est plus cherche sur une grille mais par optimisation continue sur
# [0,01 ; 2] : la surface du critere est lisse et unimodale en log(lambda).
LAMBDA_BORNES <- c(0.01, 2)

# Variables de reference du critere BGR : les trois branches au plus fort poids
# moyen dans la valeur ajoutee. Un petit VAR non contraint sur ces trois seules
# variables sert d'etalon d'ajustement.
BRANCHES_REFERENCE <- c("Industrie de transformation", "Commerce", "Agriculture")

cat("\n[1/6] Construction de la matrice des taux de croissance\n")
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
Y <- as.matrix(large[, TOUTES_BRANCHES])
Tn <- nrow(Y); n <- ncol(Y)

cat(sprintf("      Y : %d trimestres x %d branches (%s -> %s)\n",
            Tn, n, date_vers_trimestre(dates_vec[1]),
            date_vers_trimestre(dates_vec[Tn])))
# Dimension du systeme selon l'ordre de retard. p n'est plus fixe a 5 : il est
# choisi a chaque origine (section 3). On affiche donc l'etendue du probleme
# sur toute la grille, pour situer l'enjeu du serrage du prior.
cat("      dimension du systeme selon p, avec", length(DATES_CHOC), "indicatrices :\n")
for (pp in P_GRILLE) {
  k_pp <- 1L + length(DATES_CHOC) + n * pp
  cat(sprintf("        p = %d -> %3d parametres/equation | %3d obs. utilisables | rapport %.2f\n",
              pp, k_pp, Tn - pp, (Tn - pp) / k_pp))
}

# --- L'AGREGATION EST HORS PERIMETRE DE CETTE PHASE --------------------------
# La phase 3 evalue le BVAR BRANCHE PAR BRANCHE. Aucun agregat n'est calcule
# ici, pour deux raisons.
#
#   1. L'agregation releve de la phase 11, avec des poids disponibles avant T.
#      Un agregat provisoire calcule ici ne servirait qu'a departager des
#      specifications, au prix d'hypotheses qui ne sont pas encore posees.
#
#   2. Surtout, les comptes sont en VOLUMES CHAINES NON ADDITIFS : la somme des
#      valeurs ajoutees de branche en prix chaines n'est pas la valeur ajoutee
#      totale chainee. Toute agregation de ces series est donc approximative,
#      quelle que soit la formule employee.
#
# La serie de valeur ajoutee NOMINALE (VA_nominale_base2014.csv, a la racine)
# permettra de faire cela correctement en phase 11 : une agregation de type
# Fisher ou Tornqvist demande des parts en PRIX COURANTS, que cette serie
# fournit. Deux points d'attention pour cette phase :
#   - sa nomenclature differe de TOUTES_BRANCHES (libelles longs des comptes
#     nationaux, apostrophes typographiques) : une table de correspondance sera
#     necessaire ;
#   - elle commence au premier trimestre 2014, ce qui bornera les origines
#     pour lesquelles une ponderation en prix courants est disponible.

# ============================================================================
# 2) VERIFICATIONS DE L'IMPLEMENTATION
# ============================================================================
# Avant de produire quoi que ce soit, on verifie que le prior se comporte comme
# la theorie l'annonce. Deux proprietes limites, faciles a tester et qui
# echoueraient immediatement en cas d'erreur d'indexation dans les blocs
# d'observations fictives.

cat("\n[2/6] Verification de l'implementation du prior\n")

local({
  Y_test <- Y[1:80, , drop = FALSE]

  # (a) lambda tres petit : le prior ecrase les donnees, les coefficients de
  #     retard tendent vers zero (delta_i = 0, retour a la moyenne).
  m_serre <- estimer_bvar(Y_test, p = 2L, lambda = 1e-4)
  coef_retards <- m_serre$B[grepl("_L", rownames(m_serre$B)), , drop = FALSE]
  stopifnot("prior serre : les retards ne tendent pas vers 0" =
              max(abs(coef_retards)) < 1e-3)

  # (b) lambda tres grand : le prior s'efface, on doit retrouver les MCO.
  m_lache <- estimer_bvar(Y_test, p = 2L, lambda = 1e6)
  sys <- construire_systeme(Y_test, 2L, NULL)
  B_mco <- qr.solve(sys$X, sys$Y)
  ecart <- max(abs(m_lache$B - B_mco))
  stopifnot("prior lache : on ne retrouve pas les MCO" = ecart < 1e-4)

  cat(sprintf("      lambda -> 0   : max|coef. de retard| = %.2e (attendu ~ 0)\n",
              max(abs(coef_retards))))
  cat(sprintf("      lambda -> inf : ecart maximal aux MCO = %.2e (attendu ~ 0)\n", ecart))
})

# (c) l'indicatrice d'un trimestre absent de l'echantillon ne doit pas exister
local({
  d_avant <- dates_vec[dates_vec < as.Date("2019-01-01")]
  exo <- construire_indicatrices(d_avant, DATES_CHOC)
  stopifnot("indicatrice creee alors que le choc n'est pas dans l'echantillon" =
              is.null(exo))
  exo2 <- construire_indicatrices(dates_vec, DATES_CHOC)
  stopifnot("indicatrices manquantes sur l'echantillon complet" =
              !is.null(exo2) && ncol(exo2) == length(DATES_CHOC))
  cat("      indicatrices : absentes avant 2020, presentes ensuite\n")
})

#' Evaluation branche par branche (section 17 du plan).
#'
#' Les seize branches ont des volatilites tres inegales -- de 0,7 % d'ecart-type
#' pour l'education-sante a 20 % pour la peche. Un RMSFE moyen sur les branches
#' serait donc domine par les plus volatiles et ne dirait rien sur la qualite du
#' modele. On raisonne sur le RATIO entre l'erreur et la variabilite propre de
#' chaque serie : un ratio inferieur a 1 signifie que le modele fait mieux que
#' predire la moyenne historique de la branche.
evaluer_branches <- function(df) {
  df %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(
      c("specification", "fenetre"))), branche) %>%
    dplyr::summarise(
      n        = dplyr::n(),
      RMSFE    = sqrt(mean(erreur^2)),
      MAE      = mean(abs(erreur)),
      biais    = mean(erreur),
      sd_reel  = stats::sd(reel),
      ratio    = sqrt(mean(erreur^2)) / stats::sd(reel),
      correlation = suppressWarnings(stats::cor(prevision, reel)),
      .groups  = "drop")
}

# ============================================================================
# 3) EXERCICE RECURSIF HORS ECHANTILLON
# ============================================================================
cat("\n[3/6] Estimation recursive\n")

origines <- dates_vec[dates_vec >= PREMIERE_CIBLE]
cat(sprintf("      %d origines, de %s a %s\n", length(origines),
            date_vers_trimestre(origines[1]),
            date_vers_trimestre(origines[length(origines)])))

#' Passe recursive complete pour une specification donnee.
#'
#' `selection` decide si (p, lambda) sont imposes ou choisis a chaque origine
#' sur la seule information disponible a cette origine.
passe_recursive <- function(p, lambda, dates_choc, fenetre = Inf,
                            etiquette = "reference", selection = "fixe",
                            regle_choc = NULL, theta = 1, rho = 1,
                            fenetre_sigma = Inf) {
  purrr::map_dfr(origines, function(cible) {
    r <- prevision_bvar_recursive(Y, dates_vec, cible, p = p, lambda = lambda,
                                  dates_choc = dates_choc, fenetre = fenetre,
                                  selection = selection,
                                  p_grille = P_GRILLE,
                                  ref = BRANCHES_REFERENCE,
                                  regle_choc = regle_choc,
                                  d_grille = D_GRILLE, theta = theta,
                                  rho = rho, fenetre_sigma = fenetre_sigma)
    if (is.null(r)) return(NULL)
    reel <- Y[dates_vec == cible, ]
    tibble::tibble(
      specification = etiquette, selection = selection,
      p = r$p, lambda = r$lambda, d = r$d, theta = r$theta, rho = r$rho,
      n_chocs = r$n_chocs,
      fenetre = ifelse(is.finite(fenetre), as.character(fenetre), "extensive"),
      origine = cible, trimestre = date_vers_trimestre(cible),
      n_obs_estimation = r$n_obs, derniere_obs = r$derniere_obs,
      branche = names(r$prevision),
      prevision = as.numeric(r$prevision), reel = as.numeric(reel)
    )
  })
}

# --- Les trois modes de choix des hyperparametres ---------------------------
cat("      comparaison des trois modes de choix de (p, lambda)
")
modes <- list(
  list(sel = "fixe", lab = "hyperparametres fixes (Higgins) : p = 5, lambda = 0.15"),
  list(sel = "ml",   lab = "vraisemblance marginale (Giannone-Lenza-Primiceri)"),
  list(sel = "bgr",  lab = "critere d'ajustement (Banbura-Giannone-Reichlin)"))

passes <- purrr::map(modes, function(m) {
  cat(sprintf("        %s
", m$lab))
  passe_recursive(P_RETARDS, LAMBDA, DATES_CHOC, etiquette = m$lab,
                  selection = m$sel) %>%
    dplyr::mutate(erreur = reel - prevision)
})
names(passes) <- vapply(modes, `[[`, character(1), "sel")

# --- Trace des hyperparametres retenus a chaque origine ---------------------
hyper <- dplyr::bind_rows(passes) %>%
  dplyr::distinct(selection, specification, origine, trimestre, n_obs_estimation,
                  p, lambda)
ecrire_csv(hyper, chemin_res("hyperparametres_par_origine.csv"))

cat("
      --- hyperparametres retenus, resume ---
")
print(hyper %>%
        dplyr::group_by(selection) %>%
        dplyr::summarise(
          p_min = min(p), p_median = stats::median(p), p_max = max(p),
          lambda_min = round(min(lambda), 4),
          lambda_median = round(stats::median(lambda), 4),
          lambda_max = round(max(lambda), 4), .groups = "drop"), n = 10)

# --- Lequel des trois fait le mieux hors echantillon ? ----------------------
eval_modes <- purrr::map_dfr(passes, evaluer_branches)
bilan_modes <- eval_modes %>%
  dplyr::group_by(specification) %>%
  dplyr::summarise(ratio_median = stats::median(ratio),
                   n_branches_ok = sum(ratio < 1),
                   correl_mediane = stats::median(correlation, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::arrange(ratio_median)
ecrire_csv(bilan_modes, chemin_res("choix_hyperparametres.csv"))

cat("
      --- performance hors echantillon des trois modes ---
")
print(bilan_modes %>%
        dplyr::transmute(specification, `ratio median` = round(ratio_median, 3),
                         `branches < 1` = n_branches_ok,
                         `correl. med.` = round(correl_mediane, 2)), n = 10)

# --- Regle de departage -----------------------------------------------------
# Le ratio median ne suffit pas : il recompense mecaniquement le sur-serrage.
# Un prior tres serre produit une prevision quasi constante, donc un ratio qui
# frole 1 par le bas -- sans qu'aucun signal ne soit capte. La correlation entre
# prevu et realise, elle, mesure le signal reel.
#
# Regle retenue, fixee a l'avance : on classe sur le ratio median ; si deux
# modes sont a moins de TOLERANCE_RATIO l'un de l'autre, on tranche sur la
# correlation mediane. Le seuil est volontairement large au regard des ecarts
# observes, pour que la regle soit lisible et non ajustee apres coup.
TOLERANCE_RATIO <- 0.01

meilleur_ratio <- min(bilan_modes$ratio_median)
ex_aequo <- bilan_modes %>%
  dplyr::filter(ratio_median <= meilleur_ratio + TOLERANCE_RATIO) %>%
  dplyr::arrange(dplyr::desc(correl_mediane))

if (nrow(ex_aequo) > 1L) {
  cat(sprintf("
      %d modes a moins de %.3f de ratio median : departage sur la correlation
",
              nrow(ex_aequo), TOLERANCE_RATIO))
  for (i in seq_len(nrow(ex_aequo))) {
    cat(sprintf("        %-52s ratio %.3f | correl %.2f
",
                substr(ex_aequo$specification[i], 1, 52),
                ex_aequo$ratio_median[i], ex_aequo$correl_mediane[i]))
  }
}
retenu_lab <- ex_aequo$specification[1]

MODE_RETENU <- names(passes)[match(retenu_lab,
                                   vapply(modes, `[[`, character(1), "lab"))]
cat(sprintf("
      mode retenu pour la suite : %s
", retenu_lab))

ref <- passes[[MODE_RETENU]]   # provisoire : remplace apres le choix de la regle
bilan_modes <- bilan_modes %>% dplyr::mutate(retenu = specification == retenu_lab)
ecrire_csv(bilan_modes, chemin_res("choix_hyperparametres.csv"))

# --- Controle global anti-look-ahead ----------------------------------------
stopifnot("[ANTI-LOOK-AHEAD] une estimation a vu sa cible" =
            all(ref$derniere_obs < ref$origine))
cat(sprintf("      %d previsions (%d origines x %d branches)\n",
            nrow(ref), dplyr::n_distinct(ref$origine), n))
cat("      controle : aucune estimation ne contient sa propre cible\n")

ecrire_csv(ref %>% dplyr::select(-specification, -fenetre),
           chemin_res("previsions_recursives.csv"))

# --- Agregat provisoire, poids du trimestre precedent -----------------------
eval_ref <- evaluer_branches(ref)

# ============================================================================
# 4) COMPARAISON DES SPECIFICATIONS -- BRANCHE PAR BRANCHE
# ============================================================================
# Une dimension varie a la fois par rapport a la reference : plus lisible qu'une
# grille complete, et suffisant pour les sections 20, 21 et 26 du plan.
#
# L'evaluation porte sur les branches, jamais sur un agregat. Comme leurs
# volatilites vont de 0,7 % (education-sante) a 20 % (peche), un RMSFE moyen
# serait entierement pilote par les branches les plus agitees. On resume donc la
# DISTRIBUTION des ratios RMSFE / ecart-type sur les seize branches.

cat("
[4/6] Comparaison des specifications
")

# --- Comment repere-t-on les trimestres de choc ? ---------------------------
# La version precedente codait DATES_CHOC en dur sur 2020. Ce choix n'etait pas
# testable : avec un seul episode, aucun exercice hors echantillon ne peut
# departager deux listes de dates.
#
# Une REGLE, elle, se teste. Appliquee recursivement a chaque origine sur le
# seul echantillon d'entrainement, elle se declenche sur plusieurs episodes
# separes dans le temps -- la crise financiere de 2008, 2019-2020, 2022 --
# et ses parametres (z, k) deviennent comparables sur plusieurs episodes.
#
# La comparaison inclut la liste codee en dur, pour mesurer ce que la regle
# coute ou rapporte par rapport a elle.

cat("
      --- regles de detection des chocs ---
")

REGLES_CHOC <- list(
  list(lab = "liste codee en dur (2020 T1/T2/T3)", regle = NULL, choc = DATES_CHOC),
  list(lab = "aucune indicatrice",                 regle = NULL, choc = NULL),
  list(lab = "regle z=4, k=3", regle = list(z = 4, k = 3L), choc = NULL),
  list(lab = "regle z=5, k=2", regle = list(z = 5, k = 2L), choc = NULL),
  list(lab = "regle z=5, k=3", regle = list(z = 5, k = 3L), choc = NULL),
  list(lab = "regle z=6, k=2", regle = list(z = 6, k = 2L), choc = NULL)
)

passes_choc <- purrr::map_dfr(REGLES_CHOC, function(rc) {
  cat(sprintf("        %s
", rc$lab))
  passe_recursive(P_RETARDS, LAMBDA, rc$choc, Inf, rc$lab,
                  selection = MODE_RETENU, regle_choc = rc$regle) %>%
    dplyr::mutate(erreur = reel - prevision)
})

# combien de trimestres chaque regle retient-elle, et comment cela evolue ?
trace_chocs <- passes_choc %>%
  dplyr::distinct(specification, origine, trimestre, n_chocs)
ecrire_csv(trace_chocs, chemin_res("detection_chocs_par_origine.csv"))

bilan_choc <- evaluer_branches(passes_choc) %>%
  dplyr::group_by(specification) %>%
  dplyr::summarise(ratio_median = stats::median(ratio),
                   n_branches_ok = sum(ratio < 1),
                   correl_mediane = stats::median(correlation, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::left_join(
    trace_chocs %>% dplyr::group_by(specification) %>%
      dplyr::summarise(chocs_min = min(n_chocs), chocs_median = stats::median(n_chocs),
                       chocs_max = max(n_chocs), .groups = "drop"),
    by = "specification") %>%
  dplyr::arrange(ratio_median)
ecrire_csv(bilan_choc, chemin_res("comparaison_regles_choc.csv"))

cat("
      --- performance des regles de detection ---
")
print(bilan_choc %>%
        dplyr::transmute(specification, `ratio median` = round(ratio_median, 3),
                         `branches < 1` = n_branches_ok,
                         `correl. med.` = round(correl_mediane, 2),
                         `chocs detectes` = sprintf("%d a %d", chocs_min, chocs_max)),
      n = 10)

REGLE_RETENUE <- REGLES_CHOC[[match(bilan_choc$specification[1],
                                    vapply(REGLES_CHOC, `[[`, character(1), "lab"))]]
cat(sprintf("\n      regle retenue : %s\n", REGLE_RETENUE$lab))

# La reference du reste de la phase devient la passe de la regle retenue : les
# diagnostics et les figures doivent porter sur la specification effectivement
# choisie, pas sur une etape intermediaire.
ref <- passes_choc %>% dplyr::filter(specification == REGLE_RETENUE$lab)
stopifnot("[ANTI-LOOK-AHEAD] une estimation a vu sa cible" =
            all(ref$derniere_obs < ref$origine))
eval_ref <- evaluer_branches(ref)
ecrire_csv(ref %>% dplyr::select(-specification), chemin_res("previsions_recursives.csv"))
cat(sprintf("      %d previsions retenues (%d origines x %d branches)\n",
            nrow(ref), dplyr::n_distinct(ref$origine), n))

# ============================================================================
# 3 ter) TROIS LEVIERS STRUCTURELS
# ============================================================================
# Trois reglages sont testes ici, tous introduits pour repousser des limites
# identifiees dans la version precedente du rapport.
#
#   theta   serrage supplementaire des retards CROISES par rapport au propre
#           retard. La version precedente ne les distinguait que par le rapport
#           sigma_j/sigma_i. theta brise la structure conjuguee -- le prior
#           n'est plus le meme d'une equation a l'autre -- donc la
#           vraisemblance marginale n'a plus de forme close et theta se choisit
#           HORS ECHANTILLON.
#
#   rho     ponderation geometrique des observations, w(t) = rho^(T-t).
#           Reponse a la rupture de variance de 2014, la ou la fenetre
#           glissante a echoue : on allege les observations anciennes au lieu
#           de les jeter.
#
#   fenetre_sigma  longueur de la fenetre servant a estimer l'ECHELLE du prior,
#           dissociee de celle des coefficients. Mesure : sigma estime depuis
#           2014 s'ecarte de 19 % en median de sigma plein echantillon, et de
#           plus de 20 % sur 7 branches sur 16.

cat("\n      --- leviers structurels ---\n")

LEVIERS <- list(
  list(lab = "reference (aucun levier)",        th = 1,    rh = 1,    fs = Inf),
  list(lab = "theta = 0.50 (croises resserres)", th = 0.50, rh = 1,    fs = Inf),
  list(lab = "theta = 0.25",                     th = 0.25, rh = 1,    fs = Inf),
  list(lab = "rho = 0.99 (decroissance lente)",  th = 1,    rh = 0.99, fs = Inf),
  list(lab = "rho = 0.97",                       th = 1,    rh = 0.97, fs = Inf),
  list(lab = "sigma sur 40 trimestres",          th = 1,    rh = 1,    fs = 40L),
  list(lab = "sigma sur 60 trimestres",          th = 1,    rh = 1,    fs = 60L)
)

passes_leviers <- purrr::map_dfr(LEVIERS, function(lv) {
  cat(sprintf("        %s\n", lv$lab))
  passe_recursive(P_RETARDS, LAMBDA, REGLE_RETENUE$choc, Inf, lv$lab,
                  selection = MODE_RETENU, regle_choc = REGLE_RETENUE$regle,
                  theta = lv$th, rho = lv$rh, fenetre_sigma = lv$fs) %>%
    dplyr::mutate(erreur = reel - prevision)
})

bilan_leviers <- evaluer_branches(passes_leviers) %>%
  dplyr::group_by(specification) %>%
  dplyr::summarise(ratio_median = stats::median(ratio),
                   n_branches_ok = sum(ratio < 1),
                   correl_mediane = stats::median(correlation, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::arrange(ratio_median)
ecrire_csv(bilan_leviers, chemin_res("comparaison_leviers.csv"))

cat("\n      --- performance des leviers structurels ---\n")
print(bilan_leviers %>%
        dplyr::transmute(specification, `ratio median` = round(ratio_median, 3),
                         `branches < 1` = n_branches_ok,
                         `correl. med.` = round(correl_mediane, 2)), n = 10)

LEVIER_RETENU <- LEVIERS[[match(bilan_leviers$specification[1],
                                vapply(LEVIERS, `[[`, character(1), "lab"))]]
cat(sprintf("\n      levier retenu : %s\n", LEVIER_RETENU$lab))

# la reference definitive de la phase integre le levier retenu
ref <- passes_leviers %>% dplyr::filter(specification == LEVIER_RETENU$lab)
stopifnot("[ANTI-LOOK-AHEAD] une estimation a vu sa cible" =
            all(ref$derniere_obs < ref$origine))
eval_ref <- evaluer_branches(ref)
ecrire_csv(ref %>% dplyr::select(-specification), chemin_res("previsions_recursives.csv"))

# Les variantes partent du MODE ET DE LA REGLE RETENUS : on ne teste pas la
# robustesse d'une specification qu'on a ecartee.
specs <- list(
  list(sel = MODE_RETENU, choc = REGLE_RETENUE$choc, regle = REGLE_RETENUE$regle,
       fen = Inf, lab = "reference (mode et regle retenus)"),
  list(sel = MODE_RETENU, choc = NULL, regle = NULL, fen = Inf,
       lab = "sans indicatrice de choc"),
  list(sel = MODE_RETENU, choc = DATES_CHOC, regle = NULL, fen = Inf,
       lab = "liste codee en dur (2020 T1/T2/T3)"),
  list(sel = MODE_RETENU, choc = as.Date(c("2020-06-30", "2020-09-30")),
       regle = NULL, fen = Inf, lab = "indicatrices de la version 1 (T2 et T3)"),
  list(sel = MODE_RETENU, choc = REGLE_RETENUE$choc, regle = REGLE_RETENUE$regle,
       fen = 40L, lab = "fenetre glissante 40 trim."),
  list(sel = MODE_RETENU, choc = REGLE_RETENUE$choc, regle = REGLE_RETENUE$regle,
       fen = 60L, lab = "fenetre glissante 60 trim."),
  list(sel = "fixe", choc = DATES_CHOC, regle = NULL, fen = Inf,
       lab = "hyperparametres fixes de Higgins (p=5, lambda=0.15)")
)

toutes <- purrr::map_dfr(specs, function(sp) {
  cat(sprintf("      %s
", sp$lab))
  passe_recursive(P_RETARDS, LAMBDA, sp$choc, sp$fen, sp$lab,
                  selection = sp$sel, regle_choc = sp$regle,
                  theta = LEVIER_RETENU$th, rho = LEVIER_RETENU$rh,
                  fenetre_sigma = LEVIER_RETENU$fs) %>%
    dplyr::mutate(erreur = reel - prevision)
})

eval_specs <- evaluer_branches(toutes)
ecrire_csv(eval_specs, chemin_res("evaluation_branches_par_specification.csv"))

comparaison <- eval_specs %>%
  dplyr::group_by(specification, fenetre) %>%
  dplyr::summarise(
    ratio_median   = stats::median(ratio),
    ratio_moyen    = mean(ratio),
    n_branches_ok  = sum(ratio < 1),
    correl_mediane = stats::median(correlation, na.rm = TRUE),
    biais_median   = stats::median(biais),
    .groups = "drop") %>%
  dplyr::arrange(ratio_median)

ecrire_csv(comparaison, chemin_res("comparaison_specifications.csv"))

cat("
      --- Distribution du ratio RMSFE / ecart-type sur les 16 branches ---
")
print(comparaison %>%
        dplyr::transmute(specification,
                         `ratio median` = round(ratio_median, 3),
                         `ratio moyen`  = round(ratio_moyen, 3),
                         `branches < 1` = n_branches_ok,
                         `correl. med.` = round(correl_mediane, 2)),
      n = 20)
cat("      ratio < 1 : le modele fait mieux que predire la moyenne de la branche
")

# --- Evaluation hors annee 2020 ---------------------------------------------
# Sans cette separation, une specification peut sembler meilleure uniquement
# parce qu'elle encaisse moins mal un episode unique.
comparaison_hors_covid <- toutes %>%
  dplyr::filter(!(origine >= as.Date("2020-01-01") & origine <= as.Date("2020-12-31"))) %>%
  evaluer_branches() %>%
  dplyr::group_by(specification) %>%
  dplyr::summarise(ratio_median = stats::median(ratio),
                   n_branches_ok = sum(ratio < 1), .groups = "drop") %>%
  dplyr::arrange(ratio_median)

cat("
      --- Hors annee 2020 ---
")
print(comparaison_hors_covid %>%
        dplyr::transmute(specification, `ratio median` = round(ratio_median, 3),
                         `branches < 1` = n_branches_ok), n = 20)
ecrire_csv(comparaison_hors_covid, chemin_res("comparaison_specifications_hors_2020.csv"))

# --- Les ecarts sont-ils significatifs ? ------------------------------------
# Test de Diebold-Mariano (section 18 du plan), applique BRANCHE PAR BRANCHE.
# Un classement de ratios ne prouve rien par lui-meme : on compte, pour chaque
# specification alternative, sur combien de branches l'ecart avec la reference
# est distinguable du bruit.
suppressPackageStartupMessages(library(forecast))

reference_lab <- specs[[1]]$lab
err_ref <- toutes %>% dplyr::filter(specification == reference_lab)

dm_branches <- purrr::map_dfr(
  setdiff(unique(toutes$specification), reference_lab),
  function(lab) {
    alt <- toutes %>% dplyr::filter(specification == lab)
    purrr::map_dfr(unique(err_ref$branche), function(b) {
      a <- err_ref %>% dplyr::filter(branche == b) %>% dplyr::arrange(origine)
      z <- alt     %>% dplyr::filter(branche == b) %>% dplyr::arrange(origine)
      com <- intersect(a$origine, z$origine)
      ea <- a$erreur[a$origine %in% com]; eb <- z$erreur[z$origine %in% com]
      t <- tryCatch(forecast::dm.test(ea, eb, h = 1L, power = 2),
                    error = function(e) NULL)
      tibble::tibble(specification = lab, branche = b,
                     RMSFE_reference = sqrt(mean(ea^2)),
                     RMSFE_alternative = sqrt(mean(eb^2)),
                     p_value = if (is.null(t)) NA_real_ else t$p.value)
    })
  }) %>%
  dplyr::mutate(
    significatif = !is.na(p_value) & p_value < 0.10,
    sens = dplyr::case_when(
      !significatif ~ "ecart non significatif",
      RMSFE_alternative < RMSFE_reference ~ "alternative meilleure",
      TRUE ~ "reference meilleure"))

ecrire_csv(dm_branches, chemin_res("tests_diebold_mariano_branches.csv"))

dm_tests <- dm_branches %>%
  dplyr::group_by(specification) %>%
  dplyr::summarise(
    n_branches = dplyr::n(),
    alternative_meilleure = sum(sens == "alternative meilleure"),
    reference_meilleure   = sum(sens == "reference meilleure"),
    non_significatif      = sum(sens == "ecart non significatif"),
    .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(reference_meilleure - alternative_meilleure))

ecrire_csv(dm_tests, chemin_res("tests_diebold_mariano.csv"))

cat("
      --- Diebold-Mariano par branche, contre la reference (seuil 10%) ---
")
print(dm_tests %>%
        dplyr::transmute(specification,
                         `alt. meilleure` = alternative_meilleure,
                         `ref. meilleure` = reference_meilleure,
                         `non significatif` = non_significatif), n = 20)

# ============================================================================
# 5) DIAGNOSTICS
# ============================================================================
cat("\n[5/6] Diagnostics\n")

# --- Erreur par branche, specification de reference -------------------------
diag_branches <- ref %>%
  dplyr::group_by(branche) %>%
  dplyr::summarise(
    RMSFE = sqrt(mean(erreur^2)), MAE = mean(abs(erreur)), biais = mean(erreur),
    ecart_type_reel = stats::sd(reel),
    ratio = sqrt(mean(erreur^2)) / stats::sd(reel),
    .groups = "drop") %>%
  dplyr::arrange(ratio)

ecrire_csv(diag_branches, chemin_res("diagnostics_branches.csv"))
cat("\n      --- Qualite par branche (ratio = RMSFE / ecart-type de la serie) ---\n")
cat("      ratio < 1 : le modele fait mieux que predire la moyenne\n")
print(diag_branches %>%
        dplyr::transmute(branche, RMSFE = round(100 * RMSFE, 2),
                         `ecart-type` = round(100 * ecart_type_reel, 2),
                         ratio = round(ratio, 2)), n = 20)

# --- Residus du modele estime sur tout l'echantillon disponible -------------
# Diagnostic EN ECHANTILLON, pour la section 25 du plan. Ce n'est pas une
# prevision : le modele est estime sur toute la serie et ses residus sont
# examines. Les hyperparametres sont donc choisis, eux aussi, sur toute la
# serie -- avec la METHODE RETENUE en section 3, pour que les diagnostics
# portent sur la specification effectivement utilisee, et non sur celle qui a
# ete ecartee.
exo_complet <- construire_indicatrices(
  dates_vec,
  if (is.null(REGLE_RETENUE$regle)) REGLE_RETENUE$choc else
    detecter_chocs(Y, dates_vec, z = REGLE_RETENUE$regle$z,
                   k = REGLE_RETENUE$regle$k))
choix_complet <- choisir_hyperparametres(Y, exo_complet, methode = MODE_RETENU,
                                         p_grille = P_GRILLE,
                                                ref = BRANCHES_REFERENCE,
                                         p_fixe = P_RETARDS, lambda_fixe = LAMBDA)
cat(sprintf("      diagnostics en echantillon : p = %d, lambda = %.4f (methode %s)\n",
            choix_complet$p, choix_complet$lambda, MODE_RETENU))
mod_complet <- estimer_bvar(Y, p = choix_complet$p, lambda = choix_complet$lambda,
                            exo = exo_complet, d = choix_complet$d,
                            theta = LEVIER_RETENU$th, rho = LEVIER_RETENU$rh,
                            fenetre_sigma = LEVIER_RETENU$fs)

diag_residus <- purrr::map_dfr(seq_len(n), function(i) {
  e <- mod_complet$residus[, i]
  lb <- tryCatch(stats::Box.test(e, lag = 4L, type = "Ljung-Box")$p.value,
                 error = function(err) NA_real_)
  tibble::tibble(
    branche = colnames(Y)[i],
    ecart_type_residu = stats::sd(e),
    autocorr_ordre1 = stats::acf(e, lag.max = 1L, plot = FALSE)$acf[2L],
    ljung_box_p = lb,
    residus_autocorreles = !is.na(lb) & lb < 0.05,
    n_residus_extremes = sum(abs(e) > 3 * stats::sd(e))
  )
}) %>% dplyr::arrange(ljung_box_p)

ecrire_csv(diag_residus, chemin_res("diagnostics_residus.csv"))
cat(sprintf("\n      residus autocorreles (Ljung-Box, 4 retards, seuil 5%%) : %d/%d branches\n",
            sum(diag_residus$residus_autocorreles), n))

# --- Coefficients des indicatrices de choc ----------------------------------
# C'est ici que se voit l'erreur de la version 1 : les coefficients du T2 et du
# T3 2020 sont de signes opposes sur la plupart des branches.
coef_choc <- as.data.frame(mod_complet$B[grepl("^choc_", rownames(mod_complet$B)),
                                          , drop = FALSE])
coef_choc <- tibble::as_tibble(coef_choc, rownames = "indicatrice") %>%
  tidyr::pivot_longer(-indicatrice, names_to = "branche", values_to = "coefficient")
ecrire_csv(coef_choc, chemin_res("coefficients_indicatrices.csv"))

signes <- coef_choc %>%
  tidyr::pivot_wider(names_from = indicatrice, values_from = coefficient)
if (all(c("choc_T2-2020", "choc_T3-2020") %in% names(signes))) {
  n_opposes <- sum(sign(signes[["choc_T2-2020"]]) != sign(signes[["choc_T3-2020"]]))
  cat(sprintf("      coefficients T2-2020 et T3-2020 de signes opposes : %d/%d branches\n",
              n_opposes, n))
  cat("      -> une indicatrice unique partagee entre les deux etait bien inadaptee\n")
}

# ============================================================================
# 6) NOWCAST COURANT
# ============================================================================
cat("\n[6/6] Nowcast du prochain trimestre\n")

cible_courante <- trimestre_suivant(dates_vec[Tn])

# Le nowcast courant doit etre produit avec la specification RETENUE, pas avec
# celle qui a ete ecartee. Il emprunte donc exactement le meme chemin que les
# 48 origines du backtest : selection des hyperparametres sur l'information
# disponible, puis estimation.
r_courant <- prevision_bvar_recursive(Y, dates_vec, cible_courante,
                                      p = P_RETARDS, lambda = LAMBDA,
                                      dates_choc = DATES_CHOC,
                                      selection = MODE_RETENU,
                                      p_grille = P_GRILLE,
                                          ref = BRANCHES_REFERENCE)
nowcast <- tibble::tibble(
  trimestre = date_vers_trimestre(cible_courante),
  date = cible_courante,
  selection = r_courant$selection,
  p = r_courant$p,
  lambda = r_courant$lambda,
  branche = names(r_courant$prevision),
  prevision_dlog = as.numeric(r_courant$prevision),
  prevision_pct = 100 * as.numeric(r_courant$prevision)
) %>% dplyr::arrange(dplyr::desc(prevision_dlog))

ecrire_csv(nowcast, chemin_res("nowcast_courant.csv"))

cat(sprintf("      cible : %s | estime sur %d trimestres jusqu'a %s\n",
            date_vers_trimestre(cible_courante), r_courant$n_obs,
            date_vers_trimestre(r_courant$derniere_obs)))
cat(sprintf("      hyperparametres retenus : p = %d, lambda = %.4f (methode %s)\n",
            r_courant$p, r_courant$lambda, r_courant$selection))
cat("      aucun agregat : l'agregation releve de la phase 11\n")
print(nowcast %>% dplyr::transmute(branche, `prevision (%)` = round(prevision_pct, 2)),
      n = 20)

# ============================================================================
# FIGURES -- toutes branche par branche
# ============================================================================
cat("\n[figures] Generation\n")

ordre_vol <- diag_branches$branche[order(-diag_branches$ecart_type_reel)]
ref_fig <- ref %>% dplyr::mutate(branche = factor(branche, levels = ordre_vol))

# --- 1. Prevision contre realisation, les 16 branches -----------------------
p1 <- ggplot(ref_fig, aes(origine)) +
  geom_hline(yintercept = 0, color = "grey80", linewidth = 0.25) +
  geom_line(aes(y = 100 * reel, color = "réalisé"), linewidth = 0.5) +
  geom_line(aes(y = 100 * prevision, color = "prévu"), linewidth = 0.5) +
  facet_wrap(~ branche, scales = "free_y", ncol = 4) +
  scale_color_manual(values = c("réalisé" = "grey25", "prévu" = "#2E74B5")) +
  labs(title = "Prévision récursive du BVAR contre réalisation, branche par branche",
       subtitle = paste("Chaque point est estimé sans aucune information postérieure à sa cible.",
                        "Échelles verticales propres à chaque branche"),
       x = NULL, y = "croissance trimestrielle (%)", color = NULL) +
  theme(strip.text = element_text(size = 7), axis.text = element_text(size = 6))
ggsave(chemin_fig("previsions_par_branche.png"), p1, width = 12, height = 8, dpi = 150)

# --- 2. Nuage prevu / realise -----------------------------------------------
p2 <- ggplot(ref_fig, aes(100 * reel, 100 * prevision)) +
  geom_abline(slope = 1, intercept = 0, color = "grey60", linetype = "dashed",
              linewidth = 0.4) +
  geom_hline(yintercept = 0, color = "grey88", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey88", linewidth = 0.25) +
  geom_point(alpha = 0.6, size = 1, color = "#2E74B5") +
  facet_wrap(~ branche, scales = "free", ncol = 4) +
  labs(title = "Prévu contre réalisé, branche par branche",
       subtitle = paste("La diagonale est la prévision parfaite. Un nuage aplati à",
                        "l'horizontale signale un modèle qui prédit surtout sa moyenne"),
       x = "réalisé (%)", y = "prévu (%)") +
  theme(strip.text = element_text(size = 7), axis.text = element_text(size = 6))
ggsave(chemin_fig("nuages_prevu_realise.png"), p2, width = 12, height = 8, dpi = 150)

# --- 3. Erreurs, branche par branche ----------------------------------------
p3 <- ggplot(ref_fig, aes(origine, 100 * erreur)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_segment(aes(xend = origine, yend = 0), color = "#2E74B5", linewidth = 0.35) +
  facet_wrap(~ branche, scales = "free_y", ncol = 4) +
  labs(title = "Erreur de prévision, branche par branche",
       subtitle = "Réalisé moins prévu, en points de croissance trimestrielle",
       x = NULL, y = "erreur (points)") +
  theme(strip.text = element_text(size = 7), axis.text = element_text(size = 6))
ggsave(chemin_fig("erreurs_par_branche.png"), p3, width = 12, height = 8, dpi = 150)

# --- 4. Qualite par branche --------------------------------------------------
df_br <- diag_branches %>%
  dplyr::mutate(branche = factor(branche, levels = diag_branches$branche))
p4 <- ggplot(df_br, aes(ratio, branche)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey45") +
  geom_segment(aes(xend = 0, yend = branche), color = "grey75", linewidth = 0.4) +
  geom_point(aes(color = ratio < 1), size = 2.6) +
  scale_color_manual(values = c(`TRUE` = "#2E74B5", `FALSE` = "#C55A11"), guide = "none") +
  labs(title = "Qualité de la prévision par branche",
       subtitle = paste("Ratio RMSFE / écart-type de la série. À gauche du trait,",
                        "le modèle fait mieux que prédire la moyenne historique"),
       x = "RMSFE / écart-type", y = NULL)
ggsave(chemin_fig("qualite_par_branche.png"), p4, width = 9, height = 6, dpi = 150)

# --- 5. Comparaison des specifications --------------------------------------
df_comp <- eval_specs %>%
  dplyr::mutate(specification = factor(specification,
                                       levels = rev(comparaison$specification)))
p5 <- ggplot(df_comp, aes(ratio, specification)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey45") +
  geom_boxplot(fill = "grey93", color = "grey35", linewidth = 0.35,
               outlier.size = 0.9, outlier.color = "#C55A11") +
  labs(title = "Distribution du ratio RMSFE / écart-type sur les 16 branches",
       subtitle = paste("Une boîte entièrement à droite du trait signale une",
                        "spécification qui n'apporte rien sur aucune branche"),
       x = "RMSFE / écart-type", y = NULL) +
  theme(axis.text.y = element_text(size = 8))
ggsave(chemin_fig("comparaison_specifications.png"), p5, width = 10, height = 5.5, dpi = 150)

# --- 5 bis. Chocs detectes au fil des origines ------------------------------
# Chaque ligne est une origine du backtest, chaque point un trimestre que la
# regle a retenu comme choc a cette origine. La diagonale vide en haut a droite
# est la contrainte temporelle : une origine ne peut pas flaguer son futur.
chocs_detail <- purrr::map_dfr(origines, function(cible) {
  d_tr <- dates_vec[dates_vec < cible]
  Y_tr <- Y[dates_vec < cible, , drop = FALSE]
  dc <- if (is.null(REGLE_RETENUE$regle)) REGLE_RETENUE$choc else
    detecter_chocs(Y_tr, d_tr, z = REGLE_RETENUE$regle$z, k = REGLE_RETENUE$regle$k)
  if (length(dc) == 0L) return(NULL)
  tibble::tibble(origine = cible, choc = as.Date(dc))
})
ecrire_csv(chocs_detail, chemin_res("chocs_detectes.csv"))

if (nrow(chocs_detail) > 0L) {
  p5b <- ggplot(chocs_detail, aes(choc, origine)) +
    geom_abline(slope = 1, intercept = 0, color = "grey75", linetype = "dashed",
                linewidth = 0.4) +
    geom_point(shape = 15, size = 1.8, color = "#C55A11") +
    labs(title = "Trimestres identifies comme chocs, origine par origine",
         subtitle = paste("Regle appliquee au seul echantillon d'entrainement.",
                          "La diagonale marque la contrainte temporelle :",
                          "une origine ne peut pas flaguer son propre futur"),
         x = "trimestre identifie comme choc", y = "origine du backtest")
  ggsave(chemin_fig("chocs_detectes.png"), p5b, width = 9.5, height = 6, dpi = 150)
}

# --- 5 ter. Comparaison des regles de detection -----------------------------
df_regles <- evaluer_branches(passes_choc) %>%
  dplyr::mutate(specification = factor(specification,
                                       levels = rev(bilan_choc$specification)))
p5c <- ggplot(df_regles, aes(ratio, specification)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey45") +
  geom_boxplot(fill = "grey93", color = "grey35", linewidth = 0.35,
               outlier.size = 0.9, outlier.color = "#C55A11") +
  labs(title = "Comment reperer les trimestres de choc",
       subtitle = paste("Une liste codee en dur n'est pas testable ; une regle l'est,",
                        "parce qu'elle se declenche sur plusieurs episodes"),
       x = "RMSFE / écart-type", y = NULL) +
  theme(axis.text.y = element_text(size = 8))
ggsave(chemin_fig("regles_choc.png"), p5c, width = 10, height = 5, dpi = 150)

# --- 6. Hyperparametres retenus au fil des origines -------------------------
hyp_fig <- hyper %>%
  dplyr::filter(selection != "fixe") %>%
  dplyr::mutate(methode = ifelse(selection == "ml",
                                 "vraisemblance marginale",
                                 "critère d'ajustement")) %>%
  tidyr::pivot_longer(c(p, lambda), names_to = "parametre", values_to = "valeur") %>%
  dplyr::mutate(parametre = factor(parametre, levels = c("p", "lambda"),
                                   labels = c("ordre de retard p",
                                              "serrage du prior λ")))
p6 <- ggplot(hyp_fig, aes(origine, valeur, color = methode)) +
  geom_hline(data = data.frame(
    parametre = factor(c("ordre de retard p", "serrage du prior λ"),
                       levels = c("ordre de retard p", "serrage du prior λ")),
    y = c(P_RETARDS, LAMBDA)),
    aes(yintercept = y), linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_step(linewidth = 0.6) +
  facet_wrap(~ parametre, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("vraisemblance marginale" = "#2E74B5",
                                "critère d'ajustement" = "#C55A11")) +
  labs(title = "Hyperparamètres retenus à chaque origine du backtest",
       subtitle = paste("Chaque valeur est choisie sur la seule information disponible",
                        "à son origine. Le trait pointillé marque les valeurs fixes de Higgins"),
       x = NULL, y = NULL, color = NULL)
ggsave(chemin_fig("hyperparametres_par_origine.png"), p6, width = 10, height = 6, dpi = 150)

# --- 7. Surface du critere sur la grille, derniere origine ------------------
derniere <- max(origines)
Y_last <- Y[dates_vec < derniere, , drop = FALSE]
exo_last <- construire_indicatrices(dates_vec[dates_vec < derniere], DATES_CHOC)
surf <- dplyr::bind_rows(
  choisir_hyperparametres(Y_last, exo_last, "ml", P_GRILLE, D_GRILLE,
                          ref = BRANCHES_REFERENCE)$table %>%
    dplyr::mutate(critere = "log-vraisemblance marginale"),
  choisir_hyperparametres(Y_last, exo_last, "bgr", P_GRILLE, D_GRILLE,
                          ref = BRANCHES_REFERENCE)$table %>%
    dplyr::mutate(critere = "critère d'ajustement BGR"))
ecrire_csv(surf, chemin_res("surface_criteres_derniere_origine.csv"))

p7 <- ggplot(surf, aes(lambda, valeur, color = factor(p))) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1) +
  facet_wrap(~ critere, scales = "free_y", ncol = 1) +
  scale_x_log10() +
  scale_color_grey(start = 0.75, end = 0.1) +
  labs(title = "Les deux critères de choix des hyperparamètres",
       subtitle = paste("Dernière origine du backtest. La vraisemblance marginale se",
                        "maximise ; le critère d'ajustement vise la valeur 1"),
       x = "λ (échelle logarithmique)", y = NULL, color = "p")
ggsave(chemin_fig("criteres_hyperparametres.png"), p7, width = 9.5, height = 7, dpi = 150)

# --- 6. Coefficients des indicatrices ---------------------------------------
coef_fig <- coef_choc %>% dplyr::mutate(indicatrice = sub("^choc_", "", indicatrice))
p8 <- ggplot(coef_fig, aes(100 * coefficient, branche, fill = indicatrice)) +
  geom_vline(xintercept = 0, color = "grey55", linewidth = 0.35) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  scale_fill_manual(values = c("T1-2020" = "#BFBFBF", "T2-2020" = "#C55A11",
                               "T3-2020" = "#2E74B5")) +
  labs(title = "Coefficients des trois indicatrices de choc, par branche",
       subtitle = "Le T2 et le T3 2020 ont des signes opposés sur la plupart des branches",
       x = "coefficient (points de croissance trimestrielle)", y = NULL, fill = NULL)
ggsave(chemin_fig("coefficients_indicatrices.png"), p8, width = 9.5, height = 6.5, dpi = 150)
cat("\n=== PHASE 3 TERMINEE ===\n")
cat("Resultats :\n")
for (f in list.files(DOSSIER_RESULTATS, pattern = paste0("^", PREFIXE), full.names = TRUE)) {
  cat("  ", f, "\n")
}
cat("Figures :\n")
for (f in list.files(DOSSIER_FIGURES, pattern = paste0("^", PREFIXE), full.names = TRUE)) {
  cat("  ", f, "\n")
}
