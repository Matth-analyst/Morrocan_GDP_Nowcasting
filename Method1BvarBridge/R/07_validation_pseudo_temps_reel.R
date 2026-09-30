# ============================================================================
# 07_validation_pseudo_temps_reel.R -- ETAPE 7 : orchestrer et valider
# ============================================================================
# Plan de correction : etape 7, sections 29 (fonction centrale), 30 (controles
# anti-look-ahead) et 31 (structure des resultats).
#
# LE PROBLEME QUE CETTE ETAPE TRAITE
#   La logique temporelle du projet est aujourd'hui REPARTIE sur six scripts :
#   la phase 3 a sa recursion, la phase 4 la sienne, la phase 13 encore une
#   autre, l'etape 6 sa regle de poids. Chacune est correcte prise isolement.
#   Mais rien ne garantit qu'elles s'accordent, et le plan demande pour cette
#   raison "une logique temporelle UNIQUE et TESTABLE" (section 29).
#
#   Ce n'est pas une exigence de style. Un defaut reel a ete cause exactement
#   par cette dispersion : la borne de T2-2014, qui vient de la disponibilite
#   des POIDS en prix courants, avait ete transposee au BVAR -- qui n'en a pas
#   besoin -- simplement par alignement d'une phase sur l'autre. Elle y est
#   restee jusqu'a bloquer le backtest etendu, sans que personne la
#   reinterroge.
#
# CE QUE FAIT CE SCRIPT
#   (1) Une fonction `backtest_nowcast(cible)` qui, pour UN trimestre cible et
#       a partir des donnees brutes, refait toute la chaine : ensemble
#       d'information, selection, passerelle, BVAR, AR, poids, agregation.
#       Elle n'appelle aucun resultat intermediaire des autres phases.
#
#   (2) Un TEST DE NON-REGRESSION : cette fonction est executee sur un
#       echantillon d'origines, et ses sorties sont confrontees a celles du
#       pipeline phase par phase. C'est le coeur de l'etape. Deux chemins de
#       calcul independants qui donnent le meme nombre valident la logique
#       temporelle ; un ecart signale une incoherence entre phases, et c'est
#       precisement ce qu'aucun controle interne a une phase ne peut voir.
#
#   (3) Les CINQ CONTROLES anti-look-ahead de la section 30, appliques
#       systematiquement a toutes les sorties du pipeline.
#
#   (4) La table de resultats de la section 31, un enregistrement par
#       trimestre cible.
#
# SORTIES
#   resultats/07_controles_antilookahead.csv
#   resultats/07_non_regression.csv
#   resultats/07_backtest_complet.csv
#   resultats/07_backtest_intra.csv
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")
source("R/fonctions/kalman.R")
source("R/fonctions/bvar.R")
source("R/fonctions/branches_instables.R")
source("R/fonctions/nowcast.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("07_", x))

# --- Parametres ---------------------------------------------------------------
# Une SEULE definition, celle de R/fonctions/nowcast.R. En maintenir une copie
# ici reintroduirait exactement la duplication que la fonction centrale doit
# supprimer : les deux listes divergeraient au premier parametre ajoute, et le
# test de non-regression comparerait alors deux specifications differentes en
# croyant comparer deux implementations.
PARAMS <- parametres_nowcast()

cat("\n[1/5] Chargement des donnees brutes\n")
couverture <- charger_couverture()
BC <- couverture$couvertes; NC <- couverture$non_couvertes
ind_brut <- charger_indicateurs(branches = BC)
meta <- charger_metadonnees() %>% dplyr::select(id_serie, agregation, transformation)
longueur <- ind_brut %>% dplyr::count(id_serie, name = "n")
ind_brut <- ind_brut %>% dplyr::filter(id_serie %in% longueur$id_serie[longueur$n >= 36L])

va_niveaux <- charger_va()
va <- va_niveaux %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)
large <- va %>% tidyr::pivot_wider(names_from = branche, values_from = g) %>%
  dplyr::arrange(date) %>%
  dplyr::filter(dplyr::if_all(dplyr::all_of(TOUTES_BRANCHES), ~ !is.na(.)))
DATES_VEC <- large$date
Y <- as.matrix(large[, TOUTES_BRANCHES])

poids_bruts <- lire_csv(file.path(DOSSIER_RESULTATS, "06_poids.csv")) %>%
  dplyr::mutate(date = as.Date(date))
origines <- sort(unique(va$date[va$date >= PARAMS$premiere_cible]))
cat(sprintf("      %d origines, %d branches couvertes, %d non couvertes\n",
            length(origines), length(BC), length(NC)))

# ============================================================================
# 2) LA FONCTION CENTRALE  (section 29 du plan)
# ============================================================================
# La fonction `backtest_nowcast()` vit desormais dans R/fonctions/nowcast.R :
# elle est appelee ici pour la valider, et par R/08_rapport_synthese.R pour
# produire le nowcast courant. Une seule definition, deux usages.
cat("      fonction centrale : R/fonctions/nowcast.R
")
CONTEXTE <- preparer_contexte()

# ============================================================================
# 3) TEST DE NON-REGRESSION
# ============================================================================
cat("\n[2/5] Test de non-regression contre le pipeline\n")
# Huit origines reparties sur la periode, dont 2020 : assez pour detecter une
# divergence, assez peu pour rester rapide.
ECHANTILLON <- origines[round(seq(1, length(origines), length.out = 8))]
cat(sprintf("      %d origines testees : %s\n", length(ECHANTILLON),
            paste(date_vers_trimestre(ECHANTILLON), collapse = ", ")))

hist_complet <- lire_csv(file.path(DOSSIER_RESULTATS, "09_previsions_intra.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::filter(scenario == "M3") %>%
  dplyr::select(branche, origine, reel, bvar, bridge)

t0 <- Sys.time()
refaits <- purrr::map(ECHANTILLON, function(cc) {
  backtest_nowcast(cc, scenario = "M3", historique = hist_complet,
                   contexte = CONTEXTE, params = PARAMS)
})
cat(sprintf("      %s\n", format(round(difftime(Sys.time(), t0, units = "mins"), 2))))

refaits_br <- dplyr::bind_rows(lapply(refaits, function(x) if (is.null(x)) NULL else x$branches))
pipeline_br <- hist_complet %>%
  dplyr::filter(origine %in% ECHANTILLON) %>%
  dplyr::select(branche, origine, bvar_pipeline = bvar, bridge_pipeline = bridge)

controle <- refaits_br %>%
  dplyr::select(branche, origine, bvar, bridge) %>%
  dplyr::inner_join(pipeline_br, by = c("branche", "origine")) %>%
  dplyr::mutate(ecart_bvar = abs(bvar - bvar_pipeline),
                ecart_bridge = abs(bridge - bridge_pipeline))
ecrire_csv(controle, chemin_res("non_regression.csv"))

e_bvar <- max(controle$ecart_bvar, na.rm = TRUE)
e_br   <- suppressWarnings(max(controle$ecart_bridge, na.rm = TRUE))
cat(sprintf("      BVAR       : %d comparaisons, ecart maximal %.2e\n",
            sum(!is.na(controle$ecart_bvar)), e_bvar))
cat(sprintf("      Passerelle : %d comparaisons, ecart maximal %.2e\n",
            sum(!is.na(controle$ecart_bridge)), e_br))
if (e_bvar > 1e-8 || (is.finite(e_br) && e_br > 1e-8)) {
  pires <- controle %>% dplyr::arrange(dplyr::desc(pmax(ecart_bvar, ecart_bridge,
                                                        na.rm = TRUE))) %>%
    utils::head(5)
  print(as.data.frame(pires))
  stop("[NON-REGRESSION] la fonction centrale ne reproduit pas le pipeline.",
       call. = FALSE)
}
cat("      la fonction centrale reproduit le pipeline a la precision machine\n")

# ============================================================================
# 4) LES CINQ CONTROLES, SUR TOUTES LES SORTIES
# ============================================================================
cat("\n[3/5] Controles anti-look-ahead sur l'ensemble du pipeline\n")
verifs <- list()
ajouter_verif <- function(num, objet, description, resultat, detail = "") {
  verifs[[length(verifs) + 1L]] <<- tibble::tibble(
    controle = num, objet = objet, description = description,
    resultat = ifelse(resultat, "OK", "ECHEC"), detail = detail)
}

p3 <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_recursives.csv")) %>%
  dplyr::mutate(origine = as.Date(origine), derniere_obs = as.Date(derniere_obs))
ajouter_verif("C1", "phase 3 -- BVAR", "echantillon d'estimation anterieur a la cible",
              all(p3$derniere_obs < p3$origine),
              sprintf("%d previsions", nrow(p3)))

p4 <- lire_csv(file.path(DOSSIER_RESULTATS, "04_previsions_bridge.csv")) %>%
  dplyr::mutate(origine = as.Date(origine), derniere_obs = as.Date(derniere_obs))
ajouter_verif("C1", "phase 4 -- passerelle", "echantillon d'estimation anterieur a la cible",
              all(p4$derniere_obs < p4$origine, na.rm = TRUE),
              sprintf("%d previsions", sum(!is.na(p4$prevision))))

p13 <- lire_csv(file.path(DOSSIER_RESULTATS, "09_previsions_intra.csv")) %>%
  dplyr::mutate(origine = as.Date(origine), derniere_obs = as.Date(derniere_obs))
ajouter_verif("C1", "phase 13 -- intra-trimestriel",
              "echantillon d'estimation anterieur a la cible",
              all(p13$derniere_obs < p13$origine, na.rm = TRUE),
              sprintf("%d previsions", sum(!is.na(p13$bridge))))

# C2 : l'ensemble d'information de chaque scenario respecte sa borne.
bornes_ok <- purrr::map_lgl(c("M0", "M1", "M2", "M3"), function(sc) {
  i <- information_set_intra(ind_brut, as.Date("2019-06-30"), scenario = sc)
  n_attendu <- switch(sc, M0 = 0L, M1 = 1L, M2 = 2L, M3 = 3L)
  mois <- mois_du_trimestre(as.Date("2019-06-30"))
  vus <- sum(unique(i$date[i$frequence == "mensuel"]) %in% mois)
  vus == n_attendu
})
ajouter_verif("C2", "information_set_intra", "nombre de mois du trimestre cible observes",
              all(bornes_ok), "M0=0, M1=1, M2=2, M3=3 mois")

# C3 : la selection varie dans le temps -- une liste figee signalerait qu'elle
# a ete calculee une fois pour toutes.
sel <- lire_csv(file.path(DOSSIER_RESULTATS, "04_selection_par_origine.csv")) %>%
  dplyr::mutate(origine = as.Date(origine))
listes <- sel %>% dplyr::filter(retenu) %>%
  dplyr::group_by(branche, origine) %>%
  dplyr::summarise(liste = paste(sort(id_serie), collapse = "|"), .groups = "drop") %>%
  dplyr::group_by(branche) %>%
  dplyr::summarise(n_listes = dplyr::n_distinct(liste), .groups = "drop")
ajouter_verif("C3", "selection recursive", "la liste d'indicateurs varie selon l'origine",
              all(listes$n_listes > 1L),
              sprintf("%d listes distinctes en mediane par branche",
                      stats::median(listes$n_listes)))

# C4 : les poids proviennent du trimestre precedent.
ag6 <- lire_csv(file.path(DOSSIER_RESULTATS, "06_agregat.csv")) %>%
  dplyr::mutate(origine = as.Date(origine))
poids_utilises <- lire_csv(chemin_res("non_regression.csv"))
ag_refait <- dplyr::bind_rows(lapply(refaits, function(x) if (is.null(x)) NULL else x$agregat))
ajouter_verif("C4", "etape 6 -- poids", "poids issus d'un trimestre anterieur a la cible",
              all(ag_refait$trimestre_poids < ag_refait$origine),
              sprintf("%d origines verifiees", nrow(ag_refait)))

# C5 : delta n'utilise aucune information posterieure a la cible -- soit parce
# qu'il est estime sur un historique strictement anterieur, soit parce qu'il est
# constant (voir DELTA_CONSTANT dans R/00_setup.R).
d4 <- lire_csv(file.path(DOSSIER_RESULTATS, "04_poids_delta.csv")) %>%
  dplyr::mutate(origine = as.Date(origine))
premiere <- d4 %>% dplyr::group_by(branche) %>%
  dplyr::slice_min(origine, n = 1, with_ties = FALSE) %>% dplyr::ungroup()
# Le controle depend du regime : un delta CONSTANT ne peut pas regarder le
# futur, puisqu'il ne regarde rien. Ce qu'il faut verifier alors, c'est qu'il
# vaut bien la constante partout -- donc qu'aucune estimation ne s'est glissee.
delta_fixe <- exists("DELTA_CONSTANT") && length(DELTA_CONSTANT) == 1L &&
  !is.na(DELTA_CONSTANT)
if (delta_fixe) {
  ajouter_verif("C5", "poids de combinaison",
                sprintf("delta constant a %.2f : aucune estimation, donc aucun futur lu",
                        DELTA_CONSTANT),
                all(d4$delta == DELTA_CONSTANT) &&
                  all(d4$delta_mode == "constant"),
                sprintf("%d valeurs", nrow(d4)))
} else {
  ajouter_verif("C5", "poids de combinaison",
                "delta au defaut a la premiere origine, faute d'historique",
                all(premiere$delta_mode == "defaut"),
                sprintf("%d branches", nrow(premiere)))
}
ajouter_verif("C5", "poids de combinaison", "delta toujours dans [0, 1]",
              all(d4$delta >= 0 & d4$delta <= 1), sprintf("%d valeurs", nrow(d4)))

controles <- dplyr::bind_rows(verifs)
ecrire_csv(controles, chemin_res("controles_antilookahead.csv"))
print(as.data.frame(controles %>% dplyr::select(controle, objet, resultat, detail)),
      row.names = FALSE)
if (any(controles$resultat == "ECHEC")) {
  stop("[ANTI-LOOK-AHEAD] un controle a echoue : voir le tableau ci-dessus.",
       call. = FALSE)
}
cat("\n      les cinq controles de la section 30 passent\n")

# ============================================================================
# 5) LA TABLE DE RESULTATS  (section 31 du plan)
# ============================================================================
cat("\n[4/5] Table de resultats\n")
intra <- lire_csv(file.path(DOSSIER_RESULTATS, "09_previsions_intra.csv")) %>%
  dplyr::mutate(origine = as.Date(origine))

backtest <- ag6 %>%
  dplyr::transmute(trimestre_cible = trimestre, origine,
                   va_totale_reelle = reel_niveau,
                   nowcast_combo = nowcast_niveau,
                   nowcast_bvar  = bvar_niveau,
                   erreur_combo  = reel_niveau - nowcast_niveau,
                   erreur_bvar   = reel_niveau - bvar_niveau)
ecrire_csv(backtest, chemin_res("backtest_complet.csv"))
cat(sprintf("      backtest agrege : %d trimestres\n", nrow(backtest)))

intra_large <- intra %>%
  dplyr::select(branche, origine, scenario, combinee, reel) %>%
  tidyr::pivot_wider(names_from = scenario, values_from = combinee,
                     names_prefix = "nowcast_") %>%
  dplyr::mutate(erreur_M0 = reel - nowcast_M0, erreur_M1 = reel - nowcast_M1,
                erreur_M2 = reel - nowcast_M2, erreur_M3 = reel - nowcast_M3,
                trimestre_cible = date_vers_trimestre(origine), .before = 1)
ecrire_csv(intra_large, chemin_res("backtest_intra.csv"))
cat(sprintf("      backtest intra-trimestriel : %d lignes (branche x origine)\n",
            nrow(intra_large)))

cat("\n[5/5] Validation terminee.\n")
