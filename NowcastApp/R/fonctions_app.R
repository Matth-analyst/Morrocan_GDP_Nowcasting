# ============================================================================
# fonctions_app.R -- Tout ce que l'interface appelle, rien qui s'affiche
# ============================================================================
# L'application ne reimplemente aucun calcul. Elle lit les sorties de la
# chaine, et quand on le lui demande elle appelle backtest_nowcast(), la
# fonction centrale validee contre le pipeline phase par phase : le chiffre
# publie emprunte donc exactement le chemin de code qui a ete evalue.
#
# REGLE D'ECRITURE. L'application n'ecrit que dans son propre dossier. Le grand
# classeur est la source et n'est jamais modifie ; les observations saisies dans
# l'interface vont dans donnees_ajoutees/ et sont fusionnees a la lecture ; les
# nowcasts produits vont dans productions/, horodates. Aucun fichier de la
# chaine n'est touche.
#
# Datation : toute observation est datee au DERNIER JOUR de sa periode.
# Echanges : CSV uniquement, pas de .rds.
# ============================================================================

# ----------------------------------------------------------------------------
# PALETTE ET CONSTANTES
# ----------------------------------------------------------------------------
COULEUR_PRIMAIRE   <- "#2E74B5"
COULEUR_SECONDAIRE <- "#7F7F7F"
COULEUR_ACCENT     <- "#C55A11"
COULEUR_OK         <- "#4C8C4A"
COULEUR_ALERTE     <- "#C00000"

LIBELLE_SCENARIO <- c(
  M0 = "aucun mois du trimestre observé",
  M1 = "un mois du trimestre observé",
  M2 = "deux mois du trimestre observés",
  M3 = "les trois mois du trimestre observés")

# ----------------------------------------------------------------------------
# DEUX RACINES, ET IL FAUT LES DISTINGUER
# ----------------------------------------------------------------------------
# L'application vit a la racine du depot, a cote de la chaine et non dedans.
# Deux dossiers entrent donc en jeu, et les confondre serait la source d'erreur
# la plus naturelle ici :
#
#   RACINE_APP     ou vit l'application. Elle y ECRIT, et seulement la : ses
#                  productions horodatees, son journal, les observations saisies
#                  dans l'interface.
#   RACINE_CHAINE  ou vit la methode 1. L'application y LIT : les sorties du
#                  pipeline, les donnees par branche, les figures, les rapports.
#                  Elle n'y ecrit jamais.
#
# Cette separation est ce qui permet de dire, sans avoir a le verifier a chaque
# fois, qu'une execution de l'application ne peut pas alterer la chaine.

# On verifie plutot que l'on ne suppose : le dossier de l'application est celui
# qui contient app.R.
RACINE_APP <- local({
  for (candidat in c(".", "..", "../..")) {
    p <- normalizePath(candidat, mustWork = FALSE)
    if (file.exists(file.path(p, "app.R")) &&
        dir.exists(file.path(p, "R"))) return(p)
  }
  stop("Dossier de l'application introuvable : lancer par NowcastApp/lancer.R.",
       call. = FALSE)
})

# La chaine est cherchee a cote, puis au-dessus. Le nom du dossier n'est pas
# code en dur : on reconnait la chaine a ce qu'elle contient, de sorte qu'un
# renommage ne casse rien en silence.
RACINE_CHAINE <- local({
  candidats <- c(file.path(RACINE_APP, "..", "Method1BvarBridge"),
                 file.path(RACINE_APP, ".."),
                 file.path(RACINE_APP, "..", ".."))
  for (c in candidats) {
    p <- normalizePath(c, mustWork = FALSE)
    if (dir.exists(file.path(p, "R", "fonctions")) &&
        dir.exists(file.path(p, "resultats"))) return(p)
  }
  # Dernier recours : parcourir les dossiers voisins.
  parent <- normalizePath(file.path(RACINE_APP, ".."), mustWork = FALSE)
  for (d in list.dirs(parent, recursive = FALSE)) {
    if (dir.exists(file.path(d, "R", "fonctions")) &&
        dir.exists(file.path(d, "resultats")) &&
        file.exists(file.path(d, "run_pipeline.R"))) return(normalizePath(d))
  }
  stop("Chaine de calcul introuvable : l'application attend un dossier voisin ",
       "contenant R/fonctions et resultats.", call. = FALSE)
})

# Conserve pour les appels qui doivent s'executer DANS la chaine.
RACINE <- RACINE_CHAINE

res     <- function(...) file.path(RACINE_CHAINE, "resultats", ...)
dat     <- function(...) file.path(RACINE_CHAINE, "data", ...)
DOSSIER_SORTIES_APP <- file.path(RACINE_APP, "productions")
DOSSIER_AJOUTS      <- file.path(RACINE_APP, "donnees_ajoutees")
CHEMIN_AJOUTS_VA    <- file.path(DOSSIER_AJOUTS, "ajouts_va.csv")
CHEMIN_AJOUTS_IND   <- file.path(DOSSIER_AJOUTS, "ajouts_indicateurs.csv")
CHEMIN_AJOUTS_NOM   <- file.path(DOSSIER_AJOUTS, "ajouts_va_nominale.csv")
CHEMIN_COMPARAISON  <- file.path(DOSSIER_SORTIES_APP, "derniere_comparaison.csv")
CHEMIN_COMPARAISON_DETAIL <- file.path(DOSSIER_SORTIES_APP,
                                       "derniere_comparaison_branches.csv")
CHEMIN_CONTROLES    <- file.path(DOSSIER_SORTIES_APP, "derniers_controles.csv")
CHEMIN_STATUT       <- file.path(DOSSIER_SORTIES_APP, "tache_statut.csv")
CHEMIN_TACHE_LOG    <- file.path(DOSSIER_SORTIES_APP, "tache_journal.txt")
CHEMIN_PERTURBATION <- file.path(DOSSIER_SORTIES_APP, "controle_perturbation.csv")
CHEMIN_JOURNAL      <- file.path(DOSSIER_SORTIES_APP, "journal_executions.csv")
for (d in c(DOSSIER_SORTIES_APP, DOSSIER_AJOUTS)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ----------------------------------------------------------------------------
# LECTURE ET ECRITURE CSV, symetriques de celles de la chaine
# ----------------------------------------------------------------------------
lire <- function(chemin) {
  if (!file.exists(chemin)) return(NULL)
  utils::read.csv(chemin, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE,
                  check.names = FALSE)
}

ecrire <- function(x, chemin) {
  dir.create(dirname(chemin), showWarnings = FALSE, recursive = TRUE)
  con <- file(chemin, open = "wb"); on.exit(close(con), add = TRUE)
  writeBin(charToRaw("﻿"), con)
  txt <- utils::capture.output(
    utils::write.csv(x, row.names = FALSE, na = "", quote = TRUE))
  writeBin(charToRaw(paste0(paste(txt, collapse = "\n"), "\n")), con)
  invisible(chemin)
}

# ----------------------------------------------------------------------------
# LIBELLES DE DATE
# ----------------------------------------------------------------------------
lbl_trimestre <- function(d) {
  d <- as.Date(d)
  sprintf("T%d-%d", (as.integer(format(d, "%m")) - 1L) %/% 3L + 1L,
          as.integer(format(d, "%Y")))
}
# Vectorisee : elle sert aussi bien sur une date isolee que sur une colonne
# entiere. Toute observation est datee au DERNIER JOUR de sa periode.
fin_de_trimestre <- function(d) {
  d <- as.Date(d)
  m <- ((as.integer(format(d, "%m")) - 1L) %/% 3L + 1L) * 3L
  debut_suivant <- as.Date(ifelse(m == 12L,
    sprintf("%d-01-01", as.integer(format(d, "%Y")) + 1L),
    sprintf("%s-%02d-01", format(d, "%Y"), m + 1L)))
  debut_suivant - 1L
}

#' Trimestre suivant, en fin de trimestre.
#'
#' On ne part JAMAIS d'une fin de mois pour ajouter des mois : seq() sur le
#' 31 mars deborde au 1er juillet, puis derive. Le calcul passe donc par le
#' PREMIER jour du trimestre, ou l'arithmetique est exacte, et ne revient a la
#' fin qu'ensuite.
trimestre_suivant <- function(d, n = 1L) {
  d <- as.Date(d)
  q <- (as.integer(format(d, "%m")) - 1L) %/% 3L + 1L
  debut <- as.Date(sprintf("%s-%02d-01", format(d, "%Y"), 3L * (q - 1L) + 1L))
  fin_de_trimestre(seq(debut, by = "3 months", length.out = n + 1L)[n + 1L])
}

#' Dernier jour du mois, meme convention.
fin_de_mois <- function(d) {
  d <- as.Date(d)
  m <- as.integer(format(d, "%m"))
  debut_suivant <- as.Date(ifelse(m == 12L,
    sprintf("%d-01-01", as.integer(format(d, "%Y")) + 1L),
    sprintf("%s-%02d-01", format(d, "%Y"), m + 1L)))
  debut_suivant - 1L
}

# ============================================================================
# LES SORTIES DE LA CHAINE
# ============================================================================
# Chargees une seule fois par processus R, pas par session : ce sont des
# fichiers, ils ne changent pas sous les pieds de l'utilisateur. Un fichier
# absent donne NULL, et l'onglet concerne le dit au lieu de faire tomber
# l'application entiere -- un tableau de bord de production qui plante a
# l'ouverture parce qu'une phase n'a pas tourne est inutilisable.
FICHIERS_CHAINE <- c(
  nowcast_agregat   = "08_nowcast_courant_agregat.csv",
  nowcast_branches  = "08_nowcast_courant_branches.csv",
  publie            = "12c_nowcast_publie.csv",
  couverture_int    = "12c_couverture.csv",
  calibration       = "12_calibration.csv",
  decomposition     = "12_decomposition.csv",
  intervalle_comp   = "12_nowcast_intervalle.csv",
  predictive        = "12_predictive_agregat.csv",
  agregat           = "06_agregat.csv",
  eval_agregat      = "06_evaluation_agregat.csv",
  poids             = "06_poids.csv",
  ecart_formules    = "06_ecart_formules.csv",
  benchmarks        = "13_evaluation.csv",
  dm_etalons        = "13_dm_contre_systeme.csv",
  episodes          = "15_episodes.csv",
  contributions     = "15_contributions_branches.csv",
  controles         = "07_controles_antilookahead.csv",
  intra             = "09_previsions_intra.csv",
  scenarios         = "09_comparaison_scenarios.csv",
  eval_branches     = "04_evaluation_branches.csv",
  selection_origine = "04_selection_par_origine.csv",
  poids_delta       = "04_poids_delta.csv",
  hyperparametres   = "03_hyperparametres_par_origine.csv",
  dm_bvar           = "03_tests_diebold_mariano.csv",
  instabilite       = "03e_diagnostic_instabilite.csv",
  chocs             = "03_chocs_detectes.csv",
  eval_ar           = "05_evaluation.csv",
  ordre_ar          = "05_ordre_retenu.csv",
  courbe_delta      = "14b_courbe_delta.csv",
  robustesse_delta  = "14_robustesse_delta.csv",
  robustesse_fen    = "14_robustesse_fenetre.csv",
  direct_indirect   = "16_comparaison.csv",
  calendrier        = "11_calendrier_indicateurs.csv",
  retard_va         = "11_retard_va.csv",
  stationnarite     = "02_tests_stationnarite.csv",
  descriptives      = "02_stats_descriptives_va.csv")

charger_sorties <- function() {
  out <- lapply(FICHIERS_CHAINE, function(f) lire(res(f)))
  names(out) <- names(FICHIERS_CHAINE)
  out$manquants <- names(FICHIERS_CHAINE)[vapply(out, is.null, logical(1))]
  out
}

# ============================================================================
# LES DONNEES SOURCES, POUR L'EXPLORATION
# ============================================================================
# Lues une fois par processus. Ce sont les memes CSV que ceux dont se sert la
# chaine : l'application n'a pas sa propre copie des donnees, ce qui interdit
# qu'elle affiche autre chose que ce qui est modelise.
charger_donnees_base <- function() {
  va_large <- lire(dat("VA_branches.csv"))
  meta <- lire(dat("metadonnees_indicateurs.csv"))
  couv <- lire(dat("couverture_branches.csv"))
  sommaire <- lire(dat("sommaire_vivier.csv"))
  branches <- couv$branche

  va <- va_large %>%
    dplyr::mutate(date = as.Date(date)) %>%
    tidyr::pivot_longer(dplyr::all_of(branches), names_to = "branche",
                        values_to = "va") %>%
    dplyr::filter(!is.na(va)) %>%
    dplyr::arrange(branche, date) %>%
    dplyr::group_by(branche) %>%
    dplyr::mutate(dlog = c(NA_real_, diff(log(va)))) %>%
    dplyr::ungroup()

  # Les indicateurs, longs, toutes branches couvertes confondues.
  lignes <- list()
  for (i in seq_len(nrow(couv))) {
    if (!isTRUE(couv$couverte[i])) next
    dos <- couv$dossier[i]
    for (f in c("mensuel", "trimestriel")) {
      chemin <- dat(dos, sprintf("%s_%s.csv", dos, f))
      if (!file.exists(chemin)) next
      large <- lire(chemin)
      if (is.null(large) || ncol(large) <= 2L) next
      lignes[[length(lignes) + 1L]] <- large %>%
        dplyr::mutate(date = as.Date(date)) %>%
        tidyr::pivot_longer(-c(date, periode), names_to = "indicateur",
                            values_to = "valeur") %>%
        dplyr::filter(!is.na(valeur)) %>%
        dplyr::mutate(branche = couv$branche[i], frequence = f)
    }
  }
  ind <- dplyr::bind_rows(lignes) %>%
    dplyr::mutate(id_serie = paste(branche, indicateur, sep = " :: "))

  list(va = va, indicateurs = ind, meta = meta, couverture = couv,
       sommaire = sommaire,
       branches = branches,
       couvertes = couv$branche[couv$couverte],
       non_couvertes = couv$branche[!couv$couverte])
}

# ============================================================================
# LES OBSERVATIONS AJOUTEES DEPUIS L'INTERFACE
# ============================================================================
# Le classeur source n'est jamais modifie. Une observation saisie ici est
# rangee dans un fichier a part, conservee d'une session a l'autre, et
# fusionnee aux donnees au moment du recalcul. On peut donc toujours revenir a
# l'etat d'origine en vidant ce seul fichier.
lire_ajouts_va <- function() {
  a <- lire(CHEMIN_AJOUTS_VA)
  if (is.null(a) || nrow(a) == 0L) return(NULL)
  a %>% dplyr::mutate(date = as.Date(date))
}
lire_ajouts_ind <- function() {
  a <- lire(CHEMIN_AJOUTS_IND)
  if (is.null(a) || nrow(a) == 0L) return(NULL)
  a %>% dplyr::mutate(date = as.Date(date))
}

lire_ajouts_nominale <- function() {
  a <- lire(CHEMIN_AJOUTS_NOM)
  if (is.null(a) || nrow(a) == 0L) return(NULL)
  a %>% dplyr::mutate(date = as.Date(date))
}

#' Valeur ajoutee NOMINALE d'une branche pour un trimestre.
#'
#' Les poids d'agregation ne sont rien d'autre que la part de chaque branche
#' dans la valeur ajoutee nominale du trimestre. Ils ne peuvent donc etre
#' calcules que lorsque les SEIZE branches sont renseignees : une part calculee
#' sur quinze branches serait fausse pour toutes.
ajouter_observation_nominale <- function(branche, date, va_nominale) {
  nouvelle <- tibble::tibble(branche = branche, date = as.Date(date),
                             va_nominale = va_nominale,
                             saisi_le = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ancien <- lire_ajouts_nominale()
  if (!is.null(ancien)) {
    ancien <- ancien %>% dplyr::filter(!(branche == !!branche &
                                           date == as.Date(!!date)))
  }
  ecrire(dplyr::bind_rows(ancien, nouvelle), CHEMIN_AJOUTS_NOM)
}

#' Poids deduits des saisies nominales, pour les seuls trimestres complets.
poids_ajoutes <- function(branches_attendues) {
  a <- lire_ajouts_nominale()
  if (is.null(a)) return(NULL)
  complets <- a %>% dplyr::filter(branche %in% branches_attendues) %>%
    dplyr::count(date) %>%
    dplyr::filter(n == length(branches_attendues)) %>% dplyr::pull(date)
  if (!length(complets)) return(NULL)
  a %>% dplyr::filter(date %in% complets) %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(w = va_nominale / sum(va_nominale)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, branche, va_nominale, w)
}

ajouter_observation_va <- function(branche, date, va) {
  nouvelle <- tibble::tibble(branche = branche, date = as.Date(date), va = va,
                             saisi_le = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ancien <- lire_ajouts_va()
  # Une meme periode saisie deux fois : la derniere saisie remplace la
  # precedente, sinon la fusion produirait deux valeurs pour une meme date.
  if (!is.null(ancien)) {
    ancien <- ancien %>% dplyr::filter(!(branche == !!branche &
                                           date == as.Date(!!date)))
  }
  ecrire(dplyr::bind_rows(ancien, nouvelle), CHEMIN_AJOUTS_VA)
}

ajouter_observation_ind <- function(id_serie, branche, indicateur, frequence,
                                    date, valeur) {
  nouvelle <- tibble::tibble(id_serie = id_serie, branche = branche,
                             indicateur = indicateur, frequence = frequence,
                             date = as.Date(date), valeur = valeur,
                             saisi_le = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ancien <- lire_ajouts_ind()
  if (!is.null(ancien)) {
    ancien <- ancien %>% dplyr::filter(!(id_serie == !!id_serie &
                                           date == as.Date(!!date)))
  }
  ecrire(dplyr::bind_rows(ancien, nouvelle), CHEMIN_AJOUTS_IND)
}

supprimer_ajouts <- function() {
  for (f in c(CHEMIN_AJOUTS_VA, CHEMIN_AJOUTS_IND, CHEMIN_AJOUTS_NOM)) {
    if (file.exists(f)) unlink(f)
  }
}

# ============================================================================
# LA CHAINE DE CALCUL
# ============================================================================
# Chargee a la demande, une seule fois, et depuis la racine : les fichiers
# sources de la chaine se referencent entre eux par des chemins relatifs.
CHAINE_CHARGEE <- FALSE

charger_chaine <- function() {
  if (CHAINE_CHARGEE) return(invisible(TRUE))
  ancien <- setwd(RACINE); on.exit(setwd(ancien), add = TRUE)
  source("R/00_setup.R", local = FALSE)
  for (f in c("information_set", "transformations", "donnees", "passerelle",
              "kalman", "bvar", "branches_instables", "nowcast")) {
    source(file.path("R", "fonctions", paste0(f, ".R")), local = FALSE)
  }
  CHAINE_CHARGEE <<- TRUE
  invisible(TRUE)
}

#' Contexte de calcul, augmente des observations saisies dans l'interface.
#'
#' `preparer_contexte()` construit le contexte a partir des seuls fichiers de
#' la chaine. Les observations ajoutees sont injectees ensuite, aux memes
#' endroits : les indicateurs bruts d'une part, la valeur ajoutee d'autre part,
#' cette derniere entrainant la reconstruction de la matrice des branches et de
#' ses taux de croissance. Rien n'est ajoute a mi-parcours : le contexte reste
#' l'unique porte d'entree des donnees.
preparer_contexte_augmente <- function() {
  contexte <- preparer_contexte()

  aj_i <- lire_ajouts_ind()
  if (!is.null(aj_i) && nrow(aj_i) > 0L) {
    modele <- contexte$ind_brut
    supplement <- aj_i %>%
      dplyr::transmute(id_serie, branche, indicateur, frequence,
                       date = as.Date(date), periode = NA_character_,
                       valeur) %>%
      # Les attributs de traitement (agregation, transformation) sont ceux de
      # la serie d'origine : une nouvelle observation ne change pas la nature
      # de la serie a laquelle elle appartient.
      dplyr::left_join(
        modele %>% dplyr::distinct(id_serie, unite, agregation, transformation,
                                   role, retenu),
        by = "id_serie") %>%
      dplyr::filter(!is.na(agregation))
    if (nrow(supplement) > 0L) {
      colonnes <- names(modele)
      contexte$ind_brut <- dplyr::bind_rows(
        modele %>% dplyr::anti_join(supplement %>% dplyr::select(id_serie, date),
                                    by = c("id_serie", "date")),
        supplement[, colonnes, drop = FALSE]) %>%
        dplyr::arrange(branche, frequence, indicateur, date)
    }
  }

  aj_v <- lire_ajouts_va()
  if (!is.null(aj_v) && nrow(aj_v) > 0L) {
    ancien <- setwd(RACINE); on.exit(setwd(ancien), add = TRUE)
    brut <- charger_va() %>%
      dplyr::anti_join(aj_v %>% dplyr::select(branche, date),
                       by = c("branche", "date")) %>%
      dplyr::bind_rows(aj_v %>% dplyr::select(branche, date, va)) %>%
      dplyr::arrange(branche, date)
    va <- brut %>% dplyr::group_by(branche) %>%
      dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
      dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
      dplyr::select(branche, date, g)
    large <- va %>% tidyr::pivot_wider(names_from = branche, values_from = g) %>%
      dplyr::arrange(date) %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(TOUTES_BRANCHES), ~ !is.na(.)))
    contexte$va <- va
    contexte$Y <- as.matrix(large[, TOUTES_BRANCHES])
    contexte$dates_vec <- large$date
  }

  # Les poids saisis completent ceux de la chaine. Ils remplacent un trimestre
  # deja present plutot que de s'y ajouter : deux jeux de poids pour une meme
  # date feraient echouer l'agregation, qui exige exactement seize lignes.
  pa <- poids_ajoutes(TOUTES_BRANCHES)
  if (!is.null(pa)) {
    contexte$poids <- dplyr::bind_rows(
      contexte$poids %>% dplyr::filter(!(date %in% pa$date)),
      pa[, intersect(names(contexte$poids), names(pa)), drop = FALSE]) %>%
      dplyr::arrange(date, branche)
  }
  contexte
}

#' Ce qu'il faut encore saisir pour viser le trimestre suivant.
#'
#' Publier un trimestre de valeur ajoutee deplace la cible d'un cran. Deux
#' choses deviennent alors necessaires, et l'utilisateur doit savoir laquelle
#' lui manque : la VA en VOLUME du trimestre publie, pour les seize branches,
#' qui devient le dernier point observe ; et la VA NOMINALE du meme trimestre,
#' pour les seize branches, dont se deduisent les poids d'agregation.
etat_trimestre <- function(base, sorties) {
  dernier_classeur <- max(base$va$date)
  aj <- lire_ajouts_va()
  dernier <- if (is.null(aj)) dernier_classeur else max(dernier_classeur, max(aj$date))
  cible <- trimestre_suivant(dernier)
  trimestre_poids <- dernier

  n_vol <- length(unique(base$va$branche[base$va$date == trimestre_poids]))
  if (!is.null(aj)) {
    n_vol <- length(unique(c(base$va$branche[base$va$date == trimestre_poids],
                             aj$branche[aj$date == trimestre_poids])))
  }
  poids_chaine <- if (is.null(sorties$poids)) character(0) else
    sorties$poids$branche[as.Date(sorties$poids$date) == trimestre_poids]
  nom <- lire_ajouts_nominale()
  poids_saisis <- if (is.null(nom)) character(0) else
    nom$branche[nom$date == trimestre_poids]
  n_poids <- length(unique(c(poids_chaine, poids_saisis)))

  n_b <- length(base$branches)
  list(cible = cible, trimestre_poids = trimestre_poids,
       n_branches = n_b, n_volume = n_vol, n_poids = n_poids,
       pret = n_vol == n_b && n_poids == n_b,
       manque_volume = setdiff(base$branches,
         unique(c(base$va$branche[base$va$date == trimestre_poids],
                  if (is.null(aj)) character(0) else aj$branche[aj$date == trimestre_poids]))),
       manque_poids = setdiff(base$branches, unique(c(poids_chaine, poids_saisis))))
}

#' Produit le nowcast du trimestre courant.
#'
#' Le trimestre vise et le scenario ne sont pas choisis, ils sont CONSTATES :
#' la cible est le trimestre qui suit la derniere valeur ajoutee disponible, et
#' le scenario est le nombre de mois de ce trimestre deja presents dans les
#' indicateurs. Offrir un reglage reviendrait a permettre de fabriquer un
#' chiffre en supposant plus d'information qu'il n'y en a.
produire_nowcast <- function(avancer = function(part, message) invisible(NULL)) {
  avancer(0.05, "chargement de la chaîne de calcul")
  charger_chaine()
  ancien <- setwd(RACINE); on.exit(setwd(ancien), add = TRUE)

  avancer(0.15, "lecture des données et construction du contexte")
  contexte <- preparer_contexte_augmente()

  derniere_va <- max(contexte$va$date)
  cible <- fin_trimestre(debut_trimestre(derniere_va) %m+% months(3))
  mois_cible <- mois_du_trimestre(cible)
  mois_vus <- sum(mois_cible %in% unique(contexte$ind_brut$date))
  scenario <- c("M0", "M1", "M2", "M3")[mois_vus + 1L]

  avancer(0.30, sprintf("cible %s, scénario %s : estimation en cours",
                        date_vers_trimestre(cible), scenario))

  historique <- lire(res("09_previsions_intra.csv"))
  if (is.null(historique)) {
    stop("09_previsions_intra.csv absent : le poids de combinaison ne peut pas ",
         "être établi sur l'historique.", call. = FALSE)
  }
  historique <- historique %>%
    dplyr::mutate(origine = as.Date(origine)) %>%
    dplyr::filter(.data$scenario == "M3") %>%
    dplyr::select(branche, origine, reel, bvar, bridge)

  t0 <- Sys.time()
  nc <- backtest_nowcast(cible, scenario = scenario, historique = historique,
                         contexte = contexte)
  duree <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (is.null(nc)) {
    stop("Le nowcast n'a pas pu être produit : il manque vraisemblablement les ",
         "poids sectoriels du trimestre précédant la cible.", call. = FALSE)
  }

  avancer(0.90, "mise en forme et archivage")
  branches <- nc$branches %>%
    dplyr::transmute(branche, w,
                     bvar_pct = 100 * bvar, bridge_pct = 100 * bridge,
                     delta, nowcast_pct = 100 * nowcast,
                     source = ifelse(is.na(bridge), "BVAR seul", "combinaison"),
                     n_retenus, indicateurs,
                     ecart_type_pct = 100 * sigma, z_amplitude, signale,
                     instable)
  agregat <- nc$agregat %>% dplyr::mutate(nowcast_pct = 100 * nowcast)

  # Archivage horodate : chaque execution laisse une trace complete, ce qui
  # permet de reconstituer apres coup le chiffre annonce a une date donnee.
  # C'est la condition pour qu'un nowcast diffuse soit defendable.
  # Les controles d'abord, l'ecriture ensuite. Un chiffre refuse ne doit avoir
  # ete ni archive ni affiche : verifier apres coup reviendrait a publier puis a
  # se raviser.
  controles <- controler_production(contexte, cible, scenario, branches, agregat)
  ecrire(controles, CHEMIN_CONTROLES)
  echecs <- sum(controles$resultat == "ECHEC")
  if (echecs > 0L) {
    stop("Contrôle d'antériorité en échec sur ce calcul (", echecs,
         ") : rien n'a été enregistré, le chiffre ne doit pas être diffusé. ",
         "Détail dans l'onglet Validation.", call. = FALSE)
  }

  # Avant d'ecraser l'affichage, on garde la production precedente : c'est la
  # seule facon de dire ensuite ce que l'ajout a change, et par quelle branche.
  precedent <- nowcast_courant(charger_sorties())

  cachet <- format(Sys.time(), "%Y%m%d_%H%M%S")
  ecrire(agregat, file.path(DOSSIER_SORTIES_APP, sprintf("%s_agregat.csv", cachet)))
  ecrire(branches, file.path(DOSSIER_SORTIES_APP, sprintf("%s_branches.csv", cachet)))
  ecrire(agregat, file.path(DOSSIER_SORTIES_APP, "dernier_agregat.csv"))
  ecrire(branches, file.path(DOSSIER_SORTIES_APP, "dernier_branches.csv"))

  aj_v <- lire_ajouts_va(); aj_i <- lire_ajouts_ind()
  ligne <- tibble::tibble(
    horodatage = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    trimestre = agregat$trimestre[1], scenario = agregat$scenario[1],
    nowcast_pct = agregat$nowcast_pct[1], bvar_pct = 100 * agregat$bvar[1],
    p_bvar = agregat$p_bvar[1], lambda_bvar = agregat$lambda_bvar[1],
    n_branches_passerelle = agregat$n_branches_passerelle[1],
    n_signalees = agregat$n_signalees[1], n_instables = agregat$n_instables[1],
    obs_va_ajoutees = if (is.null(aj_v)) 0L else nrow(aj_v),
    obs_ind_ajoutees = if (is.null(aj_i)) 0L else nrow(aj_i),
    duree_secondes = round(duree, 1), cachet = cachet)
  j <- lire(CHEMIN_JOURNAL)
  ecrire(if (is.null(j)) ligne else dplyr::bind_rows(j, ligne), CHEMIN_JOURNAL)

  comparaison <- comparer_productions(precedent, list(agregat = agregat,
                                                     branches = branches))
  # Une comparaison perimee vaut moins que pas de comparaison : si la nouvelle
  # production ne se compare a rien, on efface l'ancienne plutot que de la
  # laisser passer pour la derniere.
  if (!is.null(comparaison)) {
    ecrire(comparaison$resume, CHEMIN_COMPARAISON)
    ecrire(comparaison$detail, CHEMIN_COMPARAISON_DETAIL)
  } else {
    for (f in c(CHEMIN_COMPARAISON, CHEMIN_COMPARAISON_DETAIL)) {
      if (file.exists(f)) unlink(f)
    }
  }

  avancer(1, "terminé")
  list(agregat = agregat, branches = branches, duree = duree, cachet = cachet,
       comparaison = comparaison, controles = controles)
}

#' Les controles d'anteriorite du recalcul lui-meme.
#'
#' `backtest_nowcast()` execute ses propres controles bloquants a chaque appel,
#' mais ils passent en silence : si tout va bien, on n'apprend rien. Ceux-ci
#' MESURENT, apres coup, sur le calcul qui vient d'avoir lieu. La distinction
#' compte : l'onglet de validation montre les controles du protocole, etablis
#' sur quarante-huit origines lors de la derniere execution complete ; ceux-ci
#' portent sur LE chiffre qu'on s'apprete a diffuser.
#'
#' Aucun controle n'est declare "OK" par defaut. Quand une verification est sans
#' objet -- parce qu'il n'existe aucune donnee posterieure a la cible, par
#' exemple -- elle est marquee comme telle, et non comme reussie : annoncer une
#' garantie qu'on n'a pas exercee serait pire que de n'en annoncer aucune.
controler_production <- function(contexte, cible, scenario, branches, agregat) {
  lignes <- list()
  ajouter <- function(code, objet, description, resultat, detail) {
    lignes[[length(lignes) + 1L]] <<- tibble::tibble(
      controle = code, objet = objet, description = description,
      resultat = resultat, detail = detail)
  }
  fin_cible <- fin_trimestre(cible)

  # A1 -- rien de posterieur a la fin du trimestre vise n'entre dans le calcul.
  info <- information_set_intra(contexte$ind_brut, cible, scenario = scenario)
  n_apres <- sum(contexte$ind_brut$date > fin_cible)
  ajouter("A1", "ensemble d'information",
          "aucune observation postérieure à la fin du trimestre visé",
          if (nrow(info) == 0L) "SANS OBJET"
          else if (max(info$date) <= fin_cible) "OK" else "ECHEC",
          sprintf("dernière observation retenue : %s | %d observation(s) écartée(s) car postérieure(s)",
                  format(max(info$date)), n_apres))

  # A2 -- la valeur ajoutee du trimestre vise n'existe pas dans les donnees.
  n_cible <- sum(contexte$va$date >= cible)
  ajouter("A2", "cible du trimestre visé",
          "la valeur ajoutée du trimestre visé n'est pas dans l'échantillon",
          if (n_cible == 0L) "OK" else "ECHEC",
          sprintf("dernière valeur ajoutée connue : %s | %d observation(s) au trimestre visé ou après",
                  date_vers_trimestre(max(contexte$va$date)), n_cible))

  # A3 -- les poids datent d'un trimestre anterieur a la cible.
  tp <- as.Date(agregat$trimestre_poids[1])
  ajouter("A3", "poids d'agrégation",
          "les poids proviennent d'un trimestre antérieur à la cible",
          if (tp < cible) "OK" else "ECHEC",
          sprintf("poids du %s appliqués à la cible %s",
                  date_vers_trimestre(tp), date_vers_trimestre(cible)))

  # A4 -- les poids couvrent les seize branches et somment a un.
  w <- branches$w
  ajouter("A4", "complétude des poids",
          "seize branches pondérées, de somme un",
          if (length(w) == length(TOUTES_BRANCHES) && abs(sum(w) - 1) < 1e-10)
            "OK" else "ECHEC",
          sprintf("%d branches | somme = %.12f", length(w), sum(w)))

  # A5 -- le poids de combinaison n'est pas estime, donc ne lit aucun futur.
  d <- unique(branches$delta)
  constant <- exists("DELTA_CONSTANT") && length(DELTA_CONSTANT) == 1L &&
    !is.na(DELTA_CONSTANT)
  ajouter("A5", "poids de combinaison",
          if (constant) "delta constant : aucune estimation, donc aucun futur lu"
          else "delta estimé sur les seules origines antérieures",
          if (constant && length(d) == 1L && abs(d - DELTA_CONSTANT) < 1e-12)
            "OK" else if (constant) "ECHEC" else "SANS OBJET",
          if (constant) sprintf("delta = %.2f sur les %d branches", d[1], nrow(branches))
          else "delta estimé : contrôle assuré à l'intérieur du calcul")

  # A6 -- le controle decisif : perturber le futur ne doit rien changer.
  # Il n'a de sens que s'il EXISTE des donnees posterieures a la cible. En
  # production la cible est le dernier trimestre, donc il n'y en a le plus
  # souvent aucune : on le dit, au lieu de compter une reussite gratuite.
  ajouter("A6", "perturbation du futur",
          "une altération des données postérieures à la cible ne change rien",
          if (n_apres == 0L) "SANS OBJET" else "À EXÉCUTER",
          if (n_apres == 0L)
            "aucune donnée postérieure au trimestre visé n'existe : il n'y a rien à perturber"
          else sprintf("%d observation(s) postérieure(s) : contrôle exécutable à la demande",
                       n_apres))

  dplyr::bind_rows(lignes) %>%
    dplyr::mutate(trimestre = date_vers_trimestre(cible), scenario = scenario,
                  .before = 1)
}

#' Ce que la nouvelle production change, branche par branche.
#'
#' Un chiffre qui bouge sans qu'on sache pourquoi n'est pas exploitable. La
#' comparaison est faite sur le meme trimestre : comparer deux trimestres
#' differents ne dirait rien de l'effet d'une saisie.
comparer_productions <- function(avant, apres) {
  if (is.null(avant) || is.null(apres)) return(NULL)
  if (!identical(avant$agregat$trimestre[1], apres$agregat$trimestre[1])) return(NULL)
  b <- avant$branches %>%
    dplyr::select(branche, w_avant = w, nowcast_avant = nowcast_pct,
                  bvar_avant = bvar_pct, bridge_avant = bridge_pct,
                  source_avant = source) %>%
    dplyr::full_join(
      apres$branches %>%
        dplyr::select(branche, w_apres = w, nowcast_apres = nowcast_pct,
                      bvar_apres = bvar_pct, bridge_apres = bridge_pct,
                      source_apres = source),
      by = "branche") %>%
    dplyr::mutate(
      ecart_pct = nowcast_apres - nowcast_avant,
      # Ce que la branche apporte au deplacement de l'agregat : l'ecart de sa
      # prevision, pondere par son poids.
      effet_agregat_pt = w_apres * nowcast_apres - w_avant * nowcast_avant,
      traitement_change = source_avant != source_apres)
  list(
    resume = tibble::tibble(
      trimestre = apres$agregat$trimestre[1],
      scenario_avant = avant$agregat$scenario[1],
      scenario_apres = apres$agregat$scenario[1],
      agregat_avant = avant$agregat$nowcast_pct[1],
      agregat_apres = apres$agregat$nowcast_pct[1],
      ecart_agregat = apres$agregat$nowcast_pct[1] - avant$agregat$nowcast_pct[1]),
    detail = b)
}

#' Le controle de plausibilite d'une saisie, AVANT enregistrement.
#'
#' Le systeme signale deja une amplitude anormale, mais apres coup, une fois la
#' valeur entree dans le calcul. Mieux vaut le dire au moment de la frappe :
#' une faute de saisie se corrige, elle ne se justifie pas.
#'
#' LE CRITERE. Comparer la valeur saisie a l'amplitude historique de la serie
#' serait un generateur de fausses alertes : une serie qui croit bat son propre
#' maximum a chaque periode, et la VA en volume le fait presque toujours. On
#' juge donc la VARIATION qu'implique la saisie par rapport a la derniere
#' observation, rapportee a la dispersion des variations passees -- c'est-a-dire
#' la grandeur qui est effectivement stable dans le temps.
#'
#' La dispersion est mesuree par l'ecart absolu median plutot que par
#' l'ecart-type : une serie qui contient deja 2020 verrait son ecart-type gonfle
#' au point de ne plus rien detecter.
verifier_saisie <- function(valeurs, valeur, dates = NULL, date = NULL) {
  avis <- list()
  if (!is.null(dates) && !is.null(date) && !is.na(date) &&
      as.Date(date) %in% as.Date(dates)) {
    ancienne <- valeurs[as.Date(dates) == as.Date(date)][1]
    avis[[length(avis) + 1L]] <- list(niveau = "info", texte = sprintf(
      "Cette période est déjà observée (valeur actuelle : %s). La saisie la remplacera.",
      format(signif(ancienne, 6), big.mark = " ")))
  }
  if (!is.finite(valeur) || is.null(dates) || is.null(date) || is.na(date)) {
    return(avis)
  }
  o <- order(as.Date(dates))
  v <- valeurs[o]; d <- as.Date(dates)[o]
  garder <- is.finite(v) & d != as.Date(date)
  v <- v[garder]; d <- d[garder]
  if (length(v) < 12L) return(avis)

  anterieures <- d < as.Date(date)
  if (!any(anterieures)) return(avis)
  derniere <- v[max(which(anterieures))]

  # Variation relative quand la serie est strictement positive -- c'est le cas
  # des niveaux de valeur ajoutee et de la plupart des indicateurs ; variation
  # absolue sinon, faute de quoi le rapport n'aurait pas de sens.
  relatif <- all(v > 0) && derniere > 0 && valeur > 0
  variations <- if (relatif) diff(log(v)) else diff(v)
  saisie <- if (relatif) log(valeur / derniere) else valeur - derniere
  ecart <- stats::mad(variations)
  if (!is.finite(ecart) || ecart <= 0) return(avis)
  z <- abs(saisie - stats::median(variations)) / ecart

  libelle <- if (relatif) sprintf("%+.1f %%", 100 * (valeur / derniere - 1))
             else sprintf("%+.4g", saisie)
  if (z > 10) {
    avis[[length(avis) + 1L]] <- list(niveau = "danger", texte = sprintf(
      "La saisie implique une variation de %s par rapport à la dernière observation, soit %.0f fois la dispersion habituelle de cette série. Une faute de frappe est plus probable qu'une telle observation.",
      libelle, z))
  } else if (z > 5) {
    avis[[length(avis) + 1L]] <- list(niveau = "attention", texte = sprintf(
      "La saisie implique une variation de %s, inhabituelle pour cette série (%.0f fois sa dispersion courante). À vérifier avant enregistrement.",
      libelle, z))
  }
  avis
}

#' LE CONTROLE DECISIF : perturber le futur ne doit rien changer.
#'
#' Les controles A1 a A5 verifient la FORME du calcul. Celui-ci en verifie le
#' COMPORTEMENT : on saccage les donnees posterieures a la cible -- signe
#' inverse, facteur cinq, decalage de vingt -- on refait entierement le nowcast,
#' et l on verifie qu il n a pas bouge d un iota. Aucune relecture de code ne
#' donne cette garantie-la.
#'
#' Il coute deux nowcasts complets, soit environ deux minutes et demie : c est
#' pourquoi il se lance a la demande, et non a chaque production.
controle_perturbation <- function(avancer = function(part, message) invisible(NULL)) {
  ancien <- setwd(RACINE); on.exit(setwd(ancien), add = TRUE)
  avancer(0.05, "construction du contexte")
  contexte <- preparer_contexte_augmente()

  derniere_va <- max(contexte$va$date)
  cible <- fin_trimestre(debut_trimestre(derniere_va) %m+% months(3))
  mois_cible <- mois_du_trimestre(cible)
  mois_vus <- sum(mois_cible %in% unique(contexte$ind_brut$date))
  scenario <- c("M0", "M1", "M2", "M3")[mois_vus + 1L]
  fin_cible <- fin_trimestre(cible)
  apres <- contexte$ind_brut$date > fin_cible

  resultat_vide <- function(res, detail) tibble::tibble(
    trimestre = date_vers_trimestre(cible), scenario = scenario,
    n_perturbees = sum(apres), ecart = NA_real_, resultat = res, detail = detail,
    horodatage = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  if (!any(apres)) {
    return(resultat_vide("SANS OBJET",
      "aucune donnee posterieure au trimestre vise : il n y a rien a perturber"))
  }

  historique <- lire(res("09_previsions_intra.csv")) %>%
    dplyr::mutate(origine = as.Date(origine)) %>%
    dplyr::filter(.data$scenario == "M3") %>%
    dplyr::select(branche, origine, reel, bvar, bridge)

  avancer(0.15, "nowcast de reference")
  a <- backtest_nowcast(cible, scenario = scenario, historique = historique,
                        contexte = contexte)
  avancer(0.55, sprintf("perturbation de %d observation(s) posterieures", sum(apres)))
  perturbe <- contexte
  perturbe$ind_brut$valeur[apres] <- -5 * perturbe$ind_brut$valeur[apres] + 20
  b <- backtest_nowcast(cible, scenario = scenario, historique = historique,
                        contexte = perturbe)
  avancer(0.95, "comparaison")

  if (is.null(a) || is.null(b)) {
    return(resultat_vide("INDECIDABLE", "l un des deux calculs n a pas abouti"))
  }
  ecart <- abs(a$agregat$nowcast[1] - b$agregat$nowcast[1])
  tibble::tibble(
    trimestre = date_vers_trimestre(cible), scenario = scenario,
    n_perturbees = sum(apres), ecart = ecart,
    resultat = if (ecart < 1e-12) "OK" else "ECHEC",
    detail = sprintf("%d observation(s) posterieures saccagees | ecart sur l agregat : %.2e",
                     sum(apres), ecart),
    horodatage = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}

# ============================================================================
# LANCEMENT DU CALCUL DANS UN PROCESSUS SEPARE
# ============================================================================
lire_statut <- function() {
  st <- lire(CHEMIN_STATUT)
  if (is.null(st) || nrow(st) == 0L) return(NULL)
  st[nrow(st), , drop = FALSE]
}

#' Une tache est-elle reellement en cours ?
#'
#' Le fichier d'etat seul ne suffit pas : si le processus a ete tue, l'etat
#' resterait "en_cours" indefiniment et le bouton demeurerait grise. On le
#' considere donc comme abandonne passe un delai large au regard de la duree
#' observee du calcul.
DELAI_ABANDON <- 900   # secondes

tache_en_cours <- function() {
  st <- lire_statut()
  if (is.null(st) || !identical(st$etat[1], "en_cours")) return(FALSE)
  age <- as.numeric(difftime(Sys.time(), as.POSIXct(st$horodatage[1]),
                             units = "secs"))
  is.finite(age) && age < DELAI_ABANDON
}

#' Lance le recalcul, detache, et rend la main immediatement.
lancer_production <- function(mode = c("nowcast", "perturbation")) {
  mode <- match.arg(mode)
  if (tache_en_cours()) return(invisible(FALSE))
  ecrire(tibble::tibble(
    etat = "en_cours", part = 0,
    message = if (mode == "nowcast") "lancement du processus de calcul"
              else "lancement du controle par perturbation",
    mode = mode, horodatage = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    pid = NA_integer_), CHEMIN_STATUT)
  rscript <- file.path(R.home("bin"), "Rscript")
  system2(rscript,
          c("--vanilla", shQuote(file.path(RACINE_APP, "tache", "production.R")),
            shQuote(RACINE_APP), mode),
          wait = FALSE, stdout = CHEMIN_TACHE_LOG, stderr = CHEMIN_TACHE_LOG)
  invisible(TRUE)
}

#' Le nowcast a afficher.
#'
#' Deux sources coexistent, puisque l'application n'ecrase jamais les fichiers
#' de la chaine : celui que l'application a produit, et celui de la chaine. Le
#' plus RECENT fait foi, et non celui de l'application -- sans quoi une
#' execution complete du pipeline, qui remet tout a jour, resterait invisible
#' derriere un chiffre plus ancien produit ici. La provenance et l'horodatage
#' sont affiches en permanence.
nowcast_courant <- function(sorties) {
  f_app <- file.path(DOSSIER_SORTIES_APP, "dernier_agregat.csv")
  f_chaine <- res("08_nowcast_courant_agregat.csv")

  cote_app <- NULL
  ag <- lire(f_app); br <- lire(file.path(DOSSIER_SORTIES_APP, "dernier_branches.csv"))
  if (!is.null(ag) && !is.null(br)) {
    j <- lire(CHEMIN_JOURNAL)
    cote_app <- list(agregat = ag, branches = br, origine_calcul = "application",
                     quand = file.info(f_app)$mtime,
                     horodatage = if (is.null(j)) format(file.info(f_app)$mtime,
                                                         "%Y-%m-%d %H:%M:%S")
                                  else j$horodatage[nrow(j)])
  }
  cote_chaine <- NULL
  if (!is.null(sorties$nowcast_agregat) && file.exists(f_chaine)) {
    cote_chaine <- list(agregat = sorties$nowcast_agregat,
                        branches = sorties$nowcast_branches,
                        origine_calcul = "chaîne",
                        quand = file.info(f_chaine)$mtime,
                        horodatage = format(file.info(f_chaine)$mtime,
                                            "%Y-%m-%d %H:%M:%S"))
  }
  if (is.null(cote_app)) return(cote_chaine)
  if (is.null(cote_chaine)) return(cote_app)
  if (cote_chaine$quand > cote_app$quand) cote_chaine else cote_app
}

#' Ce qui manque pour que l'application soit complete.
#'
#' Un tableau de bord de production ne doit pas laisser l'utilisateur deduire
#' d'une succession d'onglets vides qu'une phase n'a pas tourne : il doit le
#' dire une fois, clairement.
diagnostic_sorties <- function(sorties, c0) {
  manquants <- sorties$manquants
  f_chaine <- res("08_nowcast_courant_agregat.csv")
  # Les mesures de qualite -- etalons, intervalles, controles -- decrivent la
  # derniere execution COMPLETE du protocole, pas le dernier recalcul. Quand
  # les deux divergent, il faut le dire.
  recalcul_posterieur <- !is.null(c0) && identical(c0$origine_calcul, "application") &&
    file.exists(f_chaine) && c0$quand > file.info(f_chaine)$mtime
  list(manquants = manquants,
       date_protocole = if (file.exists(f_chaine))
         format(file.info(f_chaine)$mtime, "%d/%m/%Y %H:%M") else NA_character_,
       recalcul_posterieur = recalcul_posterieur)
}

# ============================================================================
# L'INTERVALLE AUTOUR DU CHIFFRE AFFICHE
# ============================================================================
# L'etape 12c calibre l'intervalle par prediction conforme : les bornes valent
# `point + q`, ou q est un quantile de la distribution des erreurs passees du
# systeme, et `point` le nowcast pour lequel elle a ete produite.
#
# Ce sont donc les ECARTS q qui sont calibres, pas les bornes elles-memes. Si
# l'application recalcule le nowcast -- parce qu'une observation a ete ajoutee,
# par exemple -- le point se deplace mais les ecarts restent valides : ils
# decrivent la dispersion des erreurs du systeme, laquelle ne depend pas du
# chiffre du trimestre. Il faut donc RECENTRER l'intervalle sur le chiffre
# affiche, sans quoi le tableau de bord montrerait un point et un intervalle
# construits autour de deux valeurs differentes.
#
# La largeur, elle, n'est pas mise a jour : elle vaudra jusqu'a ce que l'etape
# 12c soit rejouee sur un protocole complet. C'est dit a l'ecran.
intervalle_courant <- function(sorties, c0) {
  p <- sorties$publie
  if (is.null(p) || is.null(c0)) return(NULL)
  p <- p %>% dplyr::filter(echelle == "constante") %>% dplyr::arrange(niveau)
  if (!nrow(p)) return(NULL)
  point <- c0$agregat$nowcast_pct[1]
  p %>% dplyr::mutate(
    ecart_bas = bas_pct - nowcast_pct,
    ecart_haut = haut_pct - nowcast_pct,
    bas_pct = point + ecart_bas,
    haut_pct = point + ecart_haut,
    recentre = abs(point - nowcast_pct) > 1e-9,
    nowcast_pct = point)
}

# ============================================================================
# TABLEAUX DE SOURCES
# ============================================================================
tableau_sources_va <- function(base) {
  base$va %>% dplyr::group_by(branche) %>%
    dplyr::summarise(debut = min(date), fin = max(date), n = dplyr::n(),
                     .groups = "drop") %>%
    dplyr::left_join(base$couverture %>% dplyr::select(branche, couverte),
                     by = "branche") %>%
    dplyr::arrange(dplyr::desc(couverte), branche)
}

tableau_sources_indicateurs <- function(base) {
  base$meta %>%
    dplyr::transmute(id_serie, branche, indicateur, frequence, retenu,
                     agregation, transformation, role, unite,
                     debut = as.Date(date_debut), fin = as.Date(date_fin),
                     n_obs, taux_manquant, motif_exclusion) %>%
    dplyr::arrange(branche, indicateur)
}

# ============================================================================
# HABILLAGE DES TABLEAUX
# ============================================================================
#' Un effectif n'est pas une mesure : arrondir "2" en "2,00" laisse croire a une
#' precision qui n'existe pas. Seules les colonnes reellement fractionnaires
#' recoivent des decimales.
tbl <- function(x, digits = 3, pageLength = 16, filtre = "none", ...) {
  fractionnaire <- function(v) is.numeric(v) &&
    any(abs(v - round(v)) > 1e-9, na.rm = TRUE)
  cols <- names(x)[vapply(x, fractionnaire, logical(1))]
  d <- DT::datatable(
    x, rownames = FALSE, filter = filtre, class = "compact stripe hover",
    options = list(pageLength = pageLength, dom = "tip", scrollX = TRUE,
                   language = list(search = "Filtrer :",
                                   emptyTable = "Aucune ligne",
                                   info = "_START_ à _END_ sur _TOTAL_",
                                   infoEmpty = "aucune ligne",
                                   zeroRecords = "Aucune ligne correspondante",
                                   paginate = list(previous = "Précédent",
                                                   `next` = "Suivant"))),
    ...)
  if (length(cols)) d <- DT::formatRound(d, cols, digits)
  d
}
