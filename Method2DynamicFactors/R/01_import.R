# ============================================================================
# 01_import_donnees.R -- PHASE 1 : importation et structuration des donnees
# ============================================================================
# Plan de correction : section 1 ("Phase 1") et section 32 ("Etape 1").
#
# SOURCE UNIQUE
#   GDPNow_Maroc_series_retenues_Apres_Tris_economiques.xlsx
#   Vivier APRES tri economique, AVANT tout tri statistique. Le classeur n'est
#   JAMAIS modifie : le pipeline y puise, il n'y ecrit pas.
#
#   Mise en page de chaque feuille de branche :
#     ligne 1  marqueurs de bloc : "CIBLE", "TRIMESTRIEL (n)", "MENSUEL (n)"
#     ligne 3  en-tetes ; chaque bloc commence par sa propre colonne "Date"
#     ligne 4+ donnees
#   Les colonnes de dates ne sont pas contigues : un mois absent est une ligne
#   absente, pas un NA. C'est le jagged edge reel, il est preserve tel quel.
#
#   VA_reelle_par_branche.xlsx fournit la cible : VA trimestrielle des 16
#   branches, base 2014, prix chaines, Mdh.
#
# CONVERSION DE DATATION
#   Le classeur date au PREMIER jour de la periode. Le pipeline travaille au
#   DERNIER jour :
#       mensuel     2024-01-01 -> 2024-01-31
#       trimestriel 2024-01-01 -> 2024-03-31
#   Une periode n'etant observee qu'a sa fin, la date porte alors elle-meme le
#   moment ou l'observation entre dans l'ensemble d'information : la
#   comparaison `date <= borne` suffit, sans colonne auxiliaire.
#
# POURQUOI CE VIVIER ET PAS LES SERIES DEJA RETENUES
#   Le classeur des series "retenues" avait ete filtre sur des correlations et
#   des p-values calculees sur l'echantillon COMPLET. Cette selection est un
#   look-ahead situe en amont du code : elle interdit la selection recursive
#   exigee par la section 5 du plan. La selection statistique est donc
#   entierement reportee en phase 5, ou elle est recalculee a chaque trimestre
#   cible sur I_(T-1).
#
# SORTIES -- CSV uniquement, data/ organise par branche
#   data/VA_branches.csv                     les 16 VA trimestrielles
#   data/metadonnees_indicateurs.csv         table EDITABLE : agregation et
#                                            transformation de chaque serie
#   data/couverture_branches.csv             partition couvertes/non couvertes
#   data/sommaire_vivier.csv                 recapitulatif par branche
#   data/<Branche>/<Branche>_mensuel.csv     indicateurs mensuels de la branche
#   data/<Branche>/<Branche>_trimestriel.csv indicateurs trimestriels
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")

# ============================================================================
# 1) CIBLE -- valeur ajoutee trimestrielle des 16 branches
# ============================================================================
# Mise en page : ligne 1 titre, ligne 2 vide, ligne 3 en-tetes ("Trimestre"
# puis les 16 branches), donnees a partir de la ligne 4, libelles "T1-1998".

lire_cibles <- function(chemin) {
  raw <- as.data.frame(readxl::read_excel(chemin, col_names = FALSE,
                                          .name_repair = "minimal"))
  entetes <- as.character(unlist(raw[3, ]))
  corps   <- raw[4:nrow(raw), , drop = FALSE]
  names(corps) <- entetes

  manquantes <- setdiff(TOUTES_BRANCHES, entetes[-1])
  if (length(manquantes) > 0L) {
    stop("Branches absentes de ", basename(chemin), " : ",
         paste(manquantes, collapse = ", "),
         "\n(verifier l'orthographe dans TOUTES_BRANCHES, R/00_setup.R)",
         call. = FALSE)
  }
  inattendues <- setdiff(entetes[-1], TOUTES_BRANCHES)
  if (length(inattendues) > 0L) {
    warning("Colonnes ignorees dans ", basename(chemin), " : ",
            paste(inattendues, collapse = ", "), call. = FALSE)
  }

  corps %>%
    dplyr::rename(trimestre = 1) %>%
    tidyr::pivot_longer(dplyr::all_of(TOUTES_BRANCHES),
                        names_to = "branche", values_to = "va") %>%
    dplyr::mutate(
      va   = suppressWarnings(as.numeric(va)),
      date = trimestre_vers_date(trimestre)   # -> fin de trimestre
    ) %>%
    dplyr::filter(!is.na(date), !is.na(va)) %>%
    dplyr::transmute(branche, trimestre, date, frequence = "trimestriel",
                     unite = "Mdh", va) %>%
    dplyr::arrange(branche, date)
}

cat("\n[1/6] Cible   :", CHEMIN_VA, "\n")
cibles <- lire_cibles(CHEMIN_VA)
verifier_datation(cibles, "cible VA")

# ============================================================================
# 2) INDICATEURS -- lecture du grand classeur
# ============================================================================

#' Numero de serie Excel -> Date (origine Windows, corrigee du bug de 1900)
serie_excel_vers_date <- function(x) {
  as.Date(suppressWarnings(as.numeric(x)), origin = "1899-12-30")
}

#' Frequence REELLE d'une serie, deduite de l'espacement de ses dates.
#'
#' Le marqueur de bloc du classeur n'est pas fiable : deux anomalies reelles
#' ont ete constatees.
#'   - "Production des derivees de phosphates (Jan 2018)" est MENSUELLE mais
#'     rangee dans le bloc TRIMESTRIEL d'Industrie extraction : ses 58
#'     observations sont empilees sous la colonne Date du bloc, dont la fin
#'     contient un calendrier mensuel.
#'   - "Industries", "Industries (2)", "Industries (3)" et "Industries
#'     metallurgiques..." sont bien trimestrielles (ecart 91 jours) mais datees
#'     au TROISIEME mois du trimestre (2006-12-01 = T4-2006).
#'
#' On tranche sur la donnee, et tout desaccord avec le marqueur est signale.
#'
#' On raisonne sur le PLUS PETIT ecart (10e centile, robuste a une date
#' aberrante isolee), jamais sur l'ecart median : les trous d'une serie
#' mensuelle allongent ses ecarts et la feraient passer pour trimestrielle.
#' Une serie trimestrielle n'a jamais d'ecart inferieur a ~89 jours.
deduire_frequence <- function(dates) {
  d <- sort(unique(dates[!is.na(dates)]))
  if (length(d) < 4L) return(NA_character_)
  petit <- stats::quantile(as.numeric(diff(d)), 0.10, type = 1L, names = FALSE)
  if (petit <= 45)  return("mensuel")
  if (petit <= 135) return("trimestriel")
  NA_character_
}

#' Lit une feuille de branche et renvoie ses indicateurs au format long.
#' Balayage colonne par colonne : un marqueur ouvre un bloc et fixe la
#' frequence declaree ; la premiere colonne "Date" du bloc sert de calendrier
#' a toutes les colonnes de valeurs qui suivent ; le bloc "CIBLE" est ignore
#' (la VA est lue depuis son propre classeur, qui fait foi).
lire_feuille_vivier <- function(chemin, feuille) {
  raw <- as.data.frame(readxl::read_excel(chemin, sheet = feuille,
                                          col_names = FALSE,
                                          .name_repair = "minimal"))
  if (nrow(raw) < 4L) return(NULL)

  marqueurs <- as.character(unlist(raw[1, ]))
  entetes   <- as.character(unlist(raw[3, ]))
  corps     <- raw[4:nrow(raw), , drop = FALSE]
  branche   <- unname(CORRESP_BRANCHES[feuille])

  freq_bloc <- NA_character_
  col_date  <- NA_integer_
  sorties   <- list()

  for (j in seq_along(entetes)) {
    mk <- marqueurs[j]
    if (!is.na(mk) && nzchar(mk)) {
      nouvelle <- if (grepl("^MENSUEL", mk))          "mensuel"
                  else if (grepl("^TRIMESTRIEL", mk)) "trimestriel"
                  else if (grepl("^CIBLE", mk))       "cible"
                  else                                 NA_character_
      if (!is.na(nouvelle)) {
        freq_bloc <- nouvelle
        col_date  <- NA_integer_      # chaque bloc a sa propre colonne Date
      }
    }

    et <- entetes[j]
    if (is.na(et) || !nzchar(trimws(et))) next
    if (identical(trimws(et), "Date")) { col_date <- j; next }
    if (is.na(freq_bloc) || freq_bloc == "cible") next
    if (is.na(col_date)) {
      warning(sprintf("[%s] colonne %d ('%s') sans colonne Date : ignoree",
                      feuille, j, et), call. = FALSE)
      next
    }

    valeurs <- suppressWarnings(as.numeric(corps[[j]]))
    if (all(is.na(valeurs))) next

    bloc <- tibble::tibble(
      branche        = branche,
      indicateur     = stringr::str_squish(et),
      frequence_bloc = freq_bloc,
      date_source    = serie_excel_vers_date(corps[[col_date]]),
      valeur         = valeurs
    ) %>%
      dplyr::filter(!is.na(date_source), !is.na(valeur))

    if (nrow(bloc) == 0L) next
    bloc$frequence <- dplyr::coalesce(deduire_frequence(bloc$date_source), freq_bloc)
    sorties[[length(sorties) + 1L]] <- bloc
  }

  if (length(sorties) == 0L) return(NULL)
  dplyr::bind_rows(sorties)
}

cat("[2/6] Vivier  :", CHEMIN_VIVIER, "\n")
feuilles_attendues <- names(CORRESP_BRANCHES)
absentes <- setdiff(feuilles_attendues, readxl::excel_sheets(CHEMIN_VIVIER))
if (length(absentes) > 0L) {
  stop("Feuilles absentes du classeur : ", paste(absentes, collapse = ", "),
       call. = FALSE)
}
indicateurs <- purrr::map_dfr(feuilles_attendues,
                              ~ lire_feuille_vivier(CHEMIN_VIVIER, .x))

# --- Reclassements de frequence ---------------------------------------------
desaccords <- indicateurs %>%
  dplyr::filter(frequence != frequence_bloc) %>%
  dplyr::group_by(branche, indicateur, frequence_bloc, frequence) %>%
  dplyr::summarise(n_obs = dplyr::n(), debut = min(date_source),
                   fin = max(date_source), .groups = "drop")
if (nrow(desaccords) > 0L) {
  cat("\n  ! Series reclassees (le bloc du classeur contredit l'espacement reel) :\n")
  for (i in seq_len(nrow(desaccords))) {
    cat(sprintf("    %-24s %-44s %s -> %s (%d obs., %s a %s)\n",
                desaccords$branche[i], substr(desaccords$indicateur[i], 1, 44),
                desaccords$frequence_bloc[i], desaccords$frequence[i],
                desaccords$n_obs[i], desaccords$debut[i], desaccords$fin[i]))
  }
  cat("\n")
}

# --- Conversion : premier jour -> DERNIER jour de la periode ----------------
indicateurs <- indicateurs %>%
  dplyr::mutate(
    date     = fin_periode(date_source, frequence),
    periode  = dplyr::if_else(frequence == "mensuel",
                              format(date, "%Y-%m"),
                              date_vers_trimestre(date)),
    id_serie = paste(branche, indicateur, sep = " :: ")
  ) %>%
  dplyr::arrange(branche, frequence, indicateur, date)

verifier_datation(indicateurs, "vivier")

# ============================================================================
# 2 bis) NETTOYAGE DES ANOMALIES DE NIVEAU
# ============================================================================
# Deux traitements, appliques avant le calcul des metadonnees pour que la regle
# de transformation soit deduite de la serie effectivement utilisee.

# --- (a) Valeur negative isolee dans une serie autrement positive -----------
# "Vente de ciment (1000tonnes)" vaut -0,02 en septembre 2000, entre 1375 et 700
# les mois voisins : une vente de ciment negative est physiquement impossible,
# c'est une cellule corrompue. La regle ne vise que ce cas de figure -- une
# unique valeur negative dans une serie dont tout le reste est positif ou nul --
# et laisse intactes les series legitimement negatives (energie absorbee par le
# pompage des STEP, auxiliaires de centrales : negatives sur toute leur
# longueur).
anomalies <- indicateurs %>%
  dplyr::group_by(id_serie, branche, indicateur) %>%
  dplyr::summarise(n = dplyr::n(), n_neg = sum(valeur < 0), .groups = "drop") %>%
  dplyr::filter(n_neg == 1L, n >= 20L)

if (nrow(anomalies) > 0L) {
  cat("\n  ! Valeur negative isolee, traitee comme donnee erronee :\n")
  for (i in seq_len(nrow(anomalies))) {
    ligne <- indicateurs %>%
      dplyr::filter(id_serie == anomalies$id_serie[i], valeur < 0)
    cat(sprintf("    %-24s %-42s %s = %s -> retiree\n",
                anomalies$branche[i], substr(anomalies$indicateur[i], 1, 42),
                ligne$periode[1], format(ligne$valeur[1])))
  }
  indicateurs <- indicateurs %>%
    dplyr::filter(!(id_serie %in% anomalies$id_serie & valeur < 0))
}

# --- (b) Zeros de tete : la serie n'a pas encore commence -------------------
# Une eolienne qui produit 0 GWh avant sa mise en service n'est pas une
# observation d'activite nulle : la serie n'existe pas encore. Garder ces zeros
# produit, au moment du demarrage, un saut de 0 a la pleine production --
# une valeur aberrante qui ne renseigne sur rien, et qui interdit dlog pour
# toute la serie.
#
# On retire donc la sequence initiale de zeros. Les zeros SITUES A L'INTERIEUR
# de la serie sont conserves : ce sont de vrais arrets de production ou des mois
# sans debarquement, et ils portent de l'information.
#
# Effet de bord voulu : une serie dont tous les zeros etaient en tete redevient
# strictement positive, donc eligible a dlog -- la regle de transformation
# s'ajuste d'elle-meme, sans intervention.
zeros_tete <- indicateurs %>%
  dplyr::arrange(id_serie, date) %>%
  dplyr::group_by(id_serie, branche, indicateur) %>%
  dplyr::summarise(
    n_tete = { r <- rle(valeur == 0); if (isTRUE(r$values[1])) r$lengths[1] else 0L },
    n_zero = sum(valeur == 0), n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n_tete > 0L)

if (nrow(zeros_tete) > 0L) {
  cat(sprintf("\n  ! %d serie(s) commencent par des zeros (mise en service tardive) :\n",
              nrow(zeros_tete)))
  for (i in seq_len(nrow(zeros_tete))) {
    cat(sprintf("    %-24s %-42s %d zero(s) de tete sur %d obs.%s\n",
                zeros_tete$branche[i], substr(zeros_tete$indicateur[i], 1, 42),
                zeros_tete$n_tete[i], zeros_tete$n[i],
                ifelse(zeros_tete$n_tete[i] == zeros_tete$n_zero[i],
                       " -> serie desormais strictement positive", "")))
  }
  indicateurs <- indicateurs %>%
    dplyr::arrange(id_serie, date) %>%
    dplyr::group_by(id_serie) %>%
    dplyr::filter(dplyr::row_number() > { r <- rle(valeur == 0)
                                          if (isTRUE(r$values[1])) r$lengths[1] else 0L }) %>%
    dplyr::ungroup()
  cat("\n")
}

# --- (c) Series cumulees depuis le debut de l'annee -------------------------
# Une partie du vivier n'est pas publiee en flux mensuel mais en CUMUL depuis
# janvier : la valeur de mars est le total janvier + fevrier + mars, et la
# serie repart de zero chaque janvier. "TOTAL EXPORTATIONS" vaut 22,7 Mdh en
# janvier 2018, 275,4 en decembre, puis 24,4 en janvier 2019.
#
# Laisser ces series telles quelles casse tout ce qui vient apres :
#   - l'agregation "sum" sur un trimestre triple-compte les mois ;
#   - dlog fabrique un effondrement artificiel a chaque debut d'annee ;
#   - la correlation avec la VA mesure alors la forme du cumul, pas l'activite.
# Le defaut serait SILENCIEUX : les series gardent une allure plausible.
#
# DETECTION. On ne se fie pas au libelle, qui ne dit rien. Une serie cumulee
# BAISSE a la premiere periode de l'annee civile -- retour a une seule periode
# apres le maximum de decembre -- et ne baisse pratiquement jamais ailleurs,
# puisqu'elle additionne des quantites positives. Le contraste est le critere :
#
#     part de baisses a la 1re periode  >= 0,80
#     part de baisses aux autres        <= 0,35
#
# Sur ce vivier la separation est franche, sans zone grise : la mediane du
# premier taux vaut 1,00 et celle du second 0,01. Les series de temperature, de
# precipitations et les soldes d'opinion, qui montent et descendent toute
# l'annee, ne sont pas retenues -- c'est le controle negatif attendu.
#
# CORRECTION. flux(t) = cumul(t) - cumul(t-1), sauf a la premiere periode de
# l'annee ou flux = cumul. Une periode dont la precedente manque ne peut pas
# etre decumulee et devient NA : mieux vaut un trou declare qu'un flux faux.
premiere_periode_annee <- function(d, frequence) {
  if (frequence == "mensuel") lubridate::month(d) == 1L
  else lubridate::quarter(d) == 1L
}

detection_cumul <- indicateurs %>%
  dplyr::arrange(id_serie, date) %>%
  dplyr::group_by(id_serie, branche, indicateur, frequence) %>%
  dplyr::mutate(
    .debut  = premiere_periode_annee(date, dplyr::first(frequence)),
    .prec   = dplyr::lag(valeur),
    .contig = as.integer(round(as.numeric(date - dplyr::lag(date)) /
                               ifelse(dplyr::first(frequence) == "mensuel", 30.4, 91.3)))
  ) %>%
  dplyr::filter(!is.na(.prec), .contig == 1L) %>%
  dplyr::summarise(
    n_debut       = sum(.debut),
    baisses_debut = sum(.debut & valeur < .prec),
    n_autres      = sum(!.debut),
    baisses_autres = sum(!.debut & valeur < .prec),
    .groups = "drop") %>%
  dplyr::filter(n_debut >= 4L, n_autres >= 12L) %>%
  dplyr::mutate(tx_debut  = baisses_debut / n_debut,
                tx_autres = baisses_autres / n_autres,
                suspect   = tx_debut >= 0.80 & tx_autres <= 0.35)

# GARDE-FOU. Le contraste "baisse en debut d'annee, jamais ailleurs" a un mode
# de faux positif : une serie TRIMESTRIELLE fortement saisonniere dont le T1 est
# le creux baisse a chaque T1 (tx_debut = 1) et une fois sur trois ailleurs
# (tx_autres ~ 0,33), ce qui franchit le seuil sans etre un cumul.
#
# La verification est fournie par la correction elle-meme : decumuler un vrai
# cumul de quantites positives donne des flux positifs, puisque le cumul est
# croissant entre deux remises a zero. Si le decumul produit beaucoup de flux
# NEGATIFS, c'est que la serie n'etait pas un cumul. On applique donc la
# transformation a blanc et on ne confirme que si elle se tient.
#
# Le seuil de 5 % laisse passer les revisions ponctuelles -- une correction a la
# baisse du cumul publie donne un flux negatif isole, ce qui est normal -- tout
# en ecartant les series ou le signe alterne structurellement.
essai <- indicateurs %>%
  dplyr::filter(id_serie %in% detection_cumul$id_serie[detection_cumul$suspect]) %>%
  dplyr::arrange(id_serie, date) %>%
  dplyr::group_by(id_serie) %>%
  dplyr::mutate(
    .debut  = premiere_periode_annee(date, frequence[1]),
    .prec   = dplyr::lag(valeur),
    .contig = as.integer(round(as.numeric(date - dplyr::lag(date)) /
                               ifelse(frequence[1] == "mensuel", 30.4, 91.3))),
    .flux   = dplyr::case_when(.debut ~ valeur,
                               is.na(.prec) | .contig != 1L ~ NA_real_,
                               TRUE ~ valeur - .prec)) %>%
  dplyr::summarise(part_neg = mean(.flux < 0, na.rm = TRUE), .groups = "drop")

detection_cumul <- detection_cumul %>%
  dplyr::left_join(essai, by = "id_serie") %>%
  dplyr::mutate(cumul = suspect & !is.na(part_neg) & part_neg <= 0.05)

rejetees <- detection_cumul %>% dplyr::filter(suspect, !cumul)
if (nrow(rejetees) > 0L) {
  cat(sprintf("\n  ! %d serie(s) ecartees par le garde-fou : le decumul y produirait\n",
              nrow(rejetees)))
  cat("    trop de flux negatifs, donc la serie n'est pas un cumul (saisonnalite forte) :\n")
  recap_rej <- rejetees %>% dplyr::count(branche, frequence, name = "n_series")
  for (i in seq_len(nrow(recap_rej))) {
    cat(sprintf("    %-30s %-12s %3d serie(s)\n",
                recap_rej$branche[i], recap_rej$frequence[i], recap_rej$n_series[i]))
  }
}

series_cumulees <- detection_cumul %>% dplyr::filter(cumul)

if (nrow(series_cumulees) > 0L) {
  cat(sprintf("\n  ! %d serie(s) publiees en CUMUL depuis le debut de l'annee, decumulees :\n",
              nrow(series_cumulees)))
  recap <- series_cumulees %>% dplyr::count(branche, frequence, name = "n_series")
  for (i in seq_len(nrow(recap))) {
    cat(sprintf("    %-30s %-12s %3d serie(s)\n",
                recap$branche[i], recap$frequence[i], recap$n_series[i]))
  }
  cat(sprintf("    contraste median : %.2f de baisses en debut d'annee contre %.2f ailleurs\n",
              stats::median(series_cumulees$tx_debut),
              stats::median(series_cumulees$tx_autres)))

  indicateurs <- indicateurs %>%
    dplyr::arrange(id_serie, date) %>%
    dplyr::group_by(id_serie) %>%
    dplyr::mutate(
      .cumulee = id_serie[1] %in% series_cumulees$id_serie,
      .debut   = premiere_periode_annee(date, frequence[1]),
      .prec    = dplyr::lag(valeur),
      .contig  = as.integer(round(as.numeric(date - dplyr::lag(date)) /
                                  ifelse(frequence[1] == "mensuel", 30.4, 91.3))),
      valeur   = dplyr::case_when(
        !.cumulee                      ~ valeur,
        .debut                         ~ valeur,
        is.na(.prec) | .contig != 1L   ~ NA_real_,
        TRUE                           ~ valeur - .prec)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.cumulee, -.debut, -.prec, -.contig) %>%
    dplyr::filter(!is.na(valeur))

  negatifs <- indicateurs %>%
    dplyr::filter(id_serie %in% series_cumulees$id_serie) %>%
    dplyr::group_by(id_serie) %>%
    dplyr::summarise(part_neg = mean(valeur < 0), .groups = "drop")
  cat(sprintf("    controle : %.1f%% de flux negatifs apres decumul, %d serie(s) au-dela de 5%%\n\n",
              100 * mean(indicateurs$valeur[indicateurs$id_serie %in%
                           series_cumulees$id_serie] < 0),
              sum(negatifs$part_neg > 0.05)))
} else {
  cat("\n  Aucune serie cumulee detectee.\n\n")
}

# --- Controle des doublons (id_serie, date) ---------------------------------
# Un doublon signale soit une repetition dans le classeur, soit une frequence
# mal deduite (plusieurs mois ecrases sur le meme trimestre). Dans les deux
# cas il faut le voir : une deduplication silencieuse avait masque le cas des
# phosphates.
doublons <- indicateurs %>%
  dplyr::count(id_serie, date, name = "n") %>%
  dplyr::filter(n > 1L)
if (nrow(doublons) > 0L) {
  cat("\n  ! Doublons (serie, date) :\n")
  print(as.data.frame(dplyr::count(doublons, id_serie, name = "n_dates")),
        row.names = FALSE)
  stop(nrow(doublons), " couple(s) (serie, date) en double : verifier la ",
       "frequence deduite et la mise en page du classeur.", call. = FALSE)
}

# ============================================================================
# 3) METADONNEES -- regles d'agregation et de transformation (sections 2 et 6)
# ============================================================================
# Deux decisions distinctes, que la version precedente confondait en appliquant
# mean(dlog) a tout le monde :
#
#   agregation      3 mois -> 1 trimestre
#     sum   flux cumulable (debarquements, trafic, arrivees, recettes,
#           exportations, ventes de ciment, production d'energie, precipitations)
#     mean  niveau ou stock moyen, indice, prix, taux, solde d'opinion
#     last  stock defini en fin de periode (parc d'abonnes, noms de domaine,
#           agregats monetaires)
#
#   transformation  mise en forme stationnaire
#     dlog    serie de niveau strictement positive
#     diff    serie pouvant etre nulle ou negative (soldes d'opinion,
#             temperature, precipitations)
#     niveau  serie DEJA en variation ou en taux (IPAI "var. trim. %", taux de
#             penetration, parts de marche) : la differencier a nouveau serait
#             une erreur
#
# Table pre-remplie par heuristique, puis RELUE a chaque execution si elle
# existe : les corrections manuelles ne sont jamais ecrasees.

deviner_agregation <- function(nom, branche) {
  n <- tolower(nom)
  # l'ordre compte : "Parts de marche ... Parc professionnel" est une part,
  # "Parc Eolien de Tarfaya" est une production -- ni l'un ni l'autre n'est un
  # stock d'abonnes.
  if (grepl("parts? de marché", n))                                      return("mean")
  if (grepl("parc [eé]olien", n))                                        return("sum")
  if (grepl("parc |parc$|noms? de domaine|domaine \\.ma|taux de pénétration", n)) return("last")
  if (grepl("masse monétaire|avoirs officiels|dépôts à vue", n))          return("last")
  if (grepl(paste("export|import|cabotage|trafic|arriv[eé]|nuit[eé]e|recette|vente",
                  "prime|prestation|production|[eé]nergie|consommation",
                  "d[eé]barquement|pr[eé]cipitation|turbinage|gwh|tonnes", sep = "|"), n))
    return("sum")
  if (grepl(paste("cr[eé]dit|comptes d[eé]biteurs|cr[eé]ance|encours|ipi|ipm|ipai",
                  "prix|taux|indice|[eé]volution|niveau des|part|arpm",
                  "temp[eé]rature|solde", sep = "|"), n))
    return("mean")
  if (identical(branche, "Pêche")) return("sum")   # ports : quantites debarquees
  "mean"
}

deviner_transformation <- function(nom, valeurs) {
  n <- tolower(nom)
  if (grepl("var\\. trim|variation|taux de pénétration|parts? de marché|^ipai", n))
    return("niveau")
  # Variables climatiques : ce sont des niveaux bornes, sans tendance, a
  # moyenne stable -- ils n'ont pas a etre transformes.
  #   Pluviometrie : le Δlog n'est pas un taux de croissance economique mais le
  #     rapport de deux tirages meteo (ecart-type mensuel de 1.0, extremes a
  #     +3.3 et -2.2 sur cette base). Ce qui compte pour la production agricole
  #     est la quantite d'eau recue, pas sa variation d'un trimestre a l'autre.
  #   Temperature : la serie est quasi purement saisonniere (12.2 C en janvier,
  #     28.4 C en juillet). La differencier ne retire pas la saisonnalite : elle
  #     en produit la derivee, un cycle de meme periode decale d'un quart de
  #     periode, dont les extremes (+-7 C) sont du calendrier et non du climat.
  # Dans les deux cas, la forme reellement informative est l'anomalie
  # saisonniere -- ecart a la moyenne de la meme periode calendaire --, mais
  # cette moyenne doit etre calculee recursivement (phase 24).
  if (grepl("pr[eé]cipitation|pluviom[eé]tr|pluie|temp[eé]rature", n)) return("niveau")

  # Soldes d'opinion des enquetes de conjoncture (HCP) : "Evolution de la
  # production par rapport au mois precedent", "Niveau des carnets de
  # commandes", "Evolution des ventes au cours des trois prochains mois"...
  # Ces series MESURENT DEJA UN CHANGEMENT : un solde est la difference entre
  # la part des entreprises declarant une hausse et celle declarant une baisse.
  # Les differencier revient a calculer l'acceleration de l'opinion, ce qui n'a
  # pas de contrepartie economique -- meme faute que de differencier un IPAI
  # deja exprime en "var. trim. %".
  #
  # Le test sur la plage de valeurs est indispensable : le mot "solde" attrape
  # aussi "- Solde des echanges d'energie (Espagne-Algerie)", qui est un flux
  # net en GWh allant de -928 a +5896, et non un solde d'opinion. Un solde
  # d'enquete s'exprime en points de pourcentage et reste au voisinage de
  # [-100, +100].
  est_solde_opinion <- grepl("[eé]volution|niveau des|solde|opinion", n) &&
    all(abs(valeurs) <= 150, na.rm = TRUE)
  if (est_solde_opinion) return("niveau")

  if (any(valeurs <= 0, na.rm = TRUE)) return("diff")
  "dlog"
}

deviner_unite <- function(nom) {
  if (grepl("\\(MDH\\)|\\(Mdh\\)|millions de dirhams", nom, ignore.case = TRUE)) return("MDH")
  if (grepl("GWh", nom, ignore.case = TRUE))                                    return("GWh")
  if (grepl("tonnes", nom, ignore.case = TRUE))                                 return("tonnes")
  if (grepl("USD", nom))                                                        return("USD")
  if (grepl("en milliers", nom, ignore.case = TRUE))                            return("milliers")
  if (grepl("%|var\\. trim", nom))                                              return("pourcentage")
  if (grepl("^IPI|^IPM|^IPAI", nom))                                            return("indice")
  NA_character_
}

#' Nombre de periodes theoriques entre deux dates (bornes incluses) : mesure le
#' taux de trous sans supposer un calendrier contigu.
nb_periodes <- function(d1, d2, frequence) {
  n <- lubridate::interval(debut_mois(d1), d2) %/% months(1) + 1L
  dplyr::if_else(frequence == "mensuel", as.integer(n), as.integer(ceiling(n / 3)))
}

#' Role d'une serie dans une eventuelle arborescence agregat / composantes.
#'
#' Le vivier melange des totaux et leurs postes : "Energie nette appelee" et
#' ses sources de production, "Total (2)" et les regions de vente de ciment,
#' "TOTAL EXPORTATIONS" et ses lignes. Retenir un total ET ses composantes dans
#' la meme bridge equation revient a compter deux fois la meme information.
#'
#' La detection porte sur des marqueurs explicites du libelle et ne pretend pas
#' reconstituer l'arborescence complete : elle signale les agregats surs, a
#' charge pour la phase 5 d'en tenir compte. La colonne est editable comme les
#' autres.
deviner_role <- function(nom) {
  n <- trimws(nom)
  if (grepl("^(I{1,3}|IV)-", n))                        return("agregat")
  if (grepl("^(- )?(Total|TOTAL)", n))                  return("agregat")
  if (grepl("^Energie nette appelee|^Energie nette appelée", n)) return("agregat")
  if (grepl("^- Production Totale", n))                 return("agregat")
  if (grepl("^Parc téléphonie mobile global|^Parc Internet global", n)) return("agregat")
  "simple"
}

#' Signature d'une serie : ses couples (date, valeur). Deux series de meme
#' signature sont strictement identiques.
signature_serie <- function(dates, valeurs) {
  o <- order(dates)
  paste(format(dates[o]), formatC(valeurs[o], format = "g", digits = 15),
        collapse = ";")
}

metadonnees <- indicateurs %>%
  dplyr::group_by(id_serie, branche, indicateur, frequence, frequence_bloc) %>%
  dplyr::summarise(
    date_debut = min(date), date_fin = max(date), n_obs = dplyr::n(),
    valeur_min = min(valeur, na.rm = TRUE), valeur_max = max(valeur, na.rm = TRUE),
    n_valeurs_distinctes = dplyr::n_distinct(valeur),
    .signature = signature_serie(date, valeur),
    .valeurs = list(valeur), .groups = "drop"
  ) %>%
  dplyr::mutate(
    n_attendu      = nb_periodes(date_debut, date_fin, frequence),
    taux_manquant  = round(pmax(0, 1 - n_obs / pmax(n_attendu, 1L)), 4),
    unite          = purrr::map_chr(indicateur, deviner_unite),
    agregation     = purrr::map2_chr(indicateur, branche, deviner_agregation),
    transformation = purrr::map2_chr(indicateur, .valeurs, deviner_transformation),
    role           = purrr::map_chr(indicateur, deviner_role),
    # Trace du traitement (c) : la serie etait publiee en cumul annuel et a ete
    # ramenee en flux. `agregation` et `transformation` ci-dessus sont donc
    # deduites de la serie DECUMULEE, ce qui est le comportement voulu.
    cumul_corrige = id_serie %in% series_cumulees$id_serie
  ) %>%
  dplyr::select(-.valeurs) %>%
  dplyr::arrange(branche, frequence, indicateur)

# --- Doublons stricts --------------------------------------------------------
# Deux series peuvent porter exactement les memes valeurs aux memes dates.
# La distinction qui compte n'est pas le libelle mais la BRANCHE :
#
#   INTRA-BRANCHE  deux candidats parfaitement colineaires pour la meme bridge
#                  equation. Ils passeraient ensemble n'importe quel critere de
#                  selection et compteraient double dans une prevision moyennee.
#                  -> on n'en garde qu'un, l'autre est ecarte.
#
#   INTER-BRANCHES un agregat legitimement partage. Bank Al-Maghrib publie par
#                  exemple "Agriculture et peche" en un seul poste, que le
#                  classeur reprend dans les deux branches : c'est bien le
#                  credit pertinent pour chacune.
#                  -> les deux sont conservees, le partage est documente, pour
#                     que les diagnostics de la section 25 puissent verifier que
#                     les previsions des deux branches ne deviennent pas
#                     artificiellement correlees.
#
# La serie conservee est choisie par une regle deterministe, donc reproductible :
# d'abord celles dont le libelle ne commence pas par une ponctuation (heritage
# de la mise en page du classeur : ". Centrale de Tahaddart", "- Clients
# Directs"), puis l'ordre alphabetique. A information identique, on garde le
# libelle le plus lisible.

metadonnees <- metadonnees %>%
  dplyr::group_by(branche, .signature) %>%
  dplyr::mutate(
    .cle_garde   = paste0(ifelse(grepl("^[[:punct:]]", indicateur), "1", "0"),
                          indicateur),
    .garde       = indicateur[which.min(rank(.cle_garde))],
    doublon_de   = ifelse(dplyr::n() > 1L & indicateur != .garde,
                          .garde, NA_character_)
  ) %>%
  dplyr::group_by(.signature) %>%
  dplyr::mutate(
    .n_branches  = dplyr::n_distinct(branche),
    partage_avec = ifelse(.n_branches > 1L,
                          vapply(seq_len(dplyr::n()), function(i)
                            paste(sort(unique(branche[-i])), collapse = " ; "),
                            character(1)),
                          NA_character_)
  ) %>%
  dplyr::ungroup()

# --- Verdict automatique de validite -----------------------------------------
# `valide` est RECALCULE a chaque execution : c'est un diagnostic, pas un choix.
# Pour ecarter une serie a la main, utiliser la colonne `exclure_manuel`, qui
# elle est preservee d'une execution a l'autre.
metadonnees <- metadonnees %>%
  dplyr::mutate(
    motif_exclusion = dplyr::case_when(
      n_valeurs_distinctes <= 1L ~ "serie constante : variance nulle",
      !is.na(doublon_de)         ~ paste0("doublon strict de : ", doublon_de),
      TRUE                       ~ NA_character_
    ),
    valide          = is.na(motif_exclusion),
    exclure_manuel  = FALSE,
    commentaire     = dplyr::case_when(
      !is.na(partage_avec) ~ paste0("Serie identique a celle de : ", partage_avec,
                                    ". Agregat partage, conserve dans les deux",
                                    " branches ; a surveiller dans les diagnostics."),
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(-.signature, -.cle_garde, -.garde, -.n_branches)

# --- Fusion avec les choix manuels ------------------------------------------
# Deux categories de colonnes, aux regles opposees :
#
#   REPRISES DU FICHIER   agregation, transformation, unite, role, commentaire,
#                         exclure_manuel. Ce sont des CHOIX : une correction a
#                         la main ne doit jamais etre ecrasee par l'heuristique.
#
#   TOUJOURS RECALCULEES  valide, motif_exclusion, doublon_de, partage_avec et
#                         toutes les statistiques de couverture. Ce sont des
#                         DIAGNOSTICS : les figer reviendrait a conserver le
#                         verdict d'une version anterieure du classeur.
#
# D'ou la colonne `exclure_manuel`, distincte de `valide` : elle permet
# d'ecarter une serie a la main sans entrer en conflit avec la detection
# automatique, et sans la desactiver.

CHEMIN_META <- file.path(DOSSIER_DATA, "metadonnees_indicateurs.csv")
if (file.exists(CHEMIN_META)) {
  ancien <- lire_csv(CHEMIN_META)
  if ("id_serie" %in% names(ancien)) {
    manuels <- ancien %>%
      dplyr::select(dplyr::any_of(c("id_serie", "agregation", "transformation",
                                    "unite", "role", "commentaire",
                                    "exclure_manuel"))) %>%
      dplyr::rename_with(~ paste0(.x, "_m"), -id_serie)
    metadonnees <- metadonnees %>%
      dplyr::left_join(manuels, by = "id_serie") %>%
      dplyr::mutate(
        agregation     = dplyr::coalesce(agregation_m, agregation),
        transformation = dplyr::coalesce(transformation_m, transformation),
        unite          = dplyr::coalesce(unite_m, unite),
        role           = dplyr::coalesce(role_m, role),
        commentaire    = dplyr::coalesce(commentaire_m, commentaire),
        exclure_manuel = dplyr::coalesce(exclure_manuel_m, exclure_manuel)
      ) %>%
      dplyr::select(-dplyr::ends_with("_m"))
    cat(sprintf("[3/6] Metadonnees : choix manuels repris depuis %s\n", CHEMIN_META))
  }
} else {
  cat("[3/6] Metadonnees : table creee par heuristique\n")
}

# flag final consomme par le reste du pipeline
metadonnees <- metadonnees %>%
  dplyr::mutate(retenu = valide & !exclure_manuel) %>%
  dplyr::relocate(retenu, valide, motif_exclusion, exclure_manuel, .after = frequence)

indicateurs <- indicateurs %>%
  dplyr::left_join(metadonnees %>%
                     dplyr::select(id_serie, unite, agregation, transformation,
                                   role, valide, retenu),
                   by = "id_serie")

# --- Compte rendu des exclusions --------------------------------------------
exclues <- metadonnees %>% dplyr::filter(!retenu)
if (nrow(exclues) > 0L) {
  cat(sprintf("\n  ! %d serie(s) ecartee(s) du vivier :\n", nrow(exclues)))
  for (i in seq_len(nrow(exclues))) {
    cat(sprintf("    %-26s %-46s %s\n", exclues$branche[i],
                substr(exclues$indicateur[i], 1, 46),
                ifelse(is.na(exclues$motif_exclusion[i]),
                       "exclusion manuelle", exclues$motif_exclusion[i])))
  }
}
partages <- metadonnees %>% dplyr::filter(!is.na(partage_avec), retenu)
if (nrow(partages) > 0L) {
  cat(sprintf("\n  i %d serie(s) identiques entre branches, conservees des deux cotes :\n",
              nrow(partages)))
  for (i in seq_len(nrow(partages))) {
    cat(sprintf("    %-26s %-46s = %s\n", partages$branche[i],
                substr(partages$indicateur[i], 1, 46), partages$partage_avec[i]))
  }
  cat("\n")
}

# ============================================================================
# 4) CONTROLES (sections 1 et 30)
# ============================================================================
couverture <- tibble::tibble(branche = TOUTES_BRANCHES) %>%
  dplyr::mutate(
    couverte = branche %in% unique(indicateurs$branche[indicateurs$retenu]),
    dossier  = dossier_branche(branche)
  )

stopifnot(
  "cible : 16 branches attendues"           = dplyr::n_distinct(cibles$branche) == 16L,
  "cible : trimestres dupliques"            = !any(duplicated(cibles[c("branche", "date")])),
  "indicateurs : frequence inconnue"        = all(indicateurs$frequence %in% c("mensuel", "trimestriel")),
  "indicateurs : branche hors nomenclature" = all(indicateurs$branche %in% TOUTES_BRANCHES),
  "agregation inconnue"                     = all(metadonnees$agregation %in% c("sum", "mean", "last")),
  "transformation inconnue"                 = all(metadonnees$transformation %in% c("dlog", "diff", "niveau"))
)

local({
  cible_test <- as.Date("2019-04-01")        # T2-2019
  fin_test   <- fin_trimestre(cible_test)    # 2019-06-30

  i3 <- information_set(indicateurs, fin_test)
  stopifnot("I_T laisse passer du futur" = max(i3$date) <= fin_test)

  m0 <- information_set_intra(indicateurs, cible_test, "M0")
  stopifnot("M0 voit le trimestre cible" = max(m0$date) < debut_trimestre(cible_test))

  m1 <- information_set_intra(indicateurs, cible_test, "M1")
  stopifnot("M1 voit plus d'un mois" = max(m1$date) <= as.Date("2019-04-30"))

  m2 <- information_set_intra(indicateurs, cible_test, "M2")
  stopifnot("M2 voit plus de deux mois" = max(m2$date) <= as.Date("2019-05-31"))
  stopifnot("M2 voit un indicateur trimestriel de T" =
              !any(m2$frequence == "trimestriel" & m2$date == fin_test))

  m3 <- information_set_intra(indicateurs, cible_test, "M3")
  stopifnot("M3 ne voit pas les indicateurs trimestriels de T" =
              any(m3$frequence == "trimestriel" & m3$date == fin_test))
  stopifnot("scenarios non emboites" =
              nrow(m0) <= nrow(m1) && nrow(m1) <= nrow(m2) && nrow(m2) <= nrow(m3))

  cat(sprintf("[4/6] Controles temporels OK (T2-2019 : M0=%d, M1=%d, M2=%d, M3=%d obs.)\n",
              nrow(m0), nrow(m1), nrow(m2), nrow(m3)))
})

# ============================================================================
# 5) ECRITURE DES CSV
# ============================================================================
# Un dossier par branche, deux fichiers par branche (mensuel / trimestriel),
# en format large : colonne `date` (dernier jour de la periode), colonne
# `periode` (libelle lisible), puis une colonne par indicateur. Les cellules
# vides sont les vrais trous des series -- le jagged edge --, pas des erreurs.

ecrire_branche <- function(b, freq) {
  d <- indicateurs %>% dplyr::filter(branche == b, frequence == freq, retenu)
  if (nrow(d) == 0L) return(NULL)
  large <- d %>%
    dplyr::select(date, periode, indicateur, valeur) %>%
    tidyr::pivot_wider(names_from = indicateur, values_from = valeur) %>%
    dplyr::arrange(date)
  chemin <- file.path(DOSSIER_DATA, dossier_branche(b),
                      sprintf("%s_%s.csv", dossier_branche(b), freq))
  ecrire_csv(large, chemin)
  tibble::tibble(branche = b, frequence = freq, fichier = chemin,
                 n_series = ncol(large) - 2L, n_periodes = nrow(large))
}

cat("[5/6] Ecriture des CSV par branche\n")
fichiers <- purrr::map_dfr(unique(indicateurs$branche), function(b) {
  dplyr::bind_rows(ecrire_branche(b, "mensuel"), ecrire_branche(b, "trimestriel"))
})

# --- Cible, a part -----------------------------------------------------------
va_large <- cibles %>%
  dplyr::select(date, trimestre, branche, va) %>%
  tidyr::pivot_wider(names_from = branche, values_from = va) %>%
  dplyr::select(date, trimestre, dplyr::all_of(TOUTES_BRANCHES)) %>%
  dplyr::arrange(date)
ecrire_csv(va_large, file.path(DOSSIER_DATA, "VA_branches.csv"))

# --- Tables de reference -----------------------------------------------------
ecrire_csv(metadonnees, CHEMIN_META)
ecrire_csv(couverture, file.path(DOSSIER_DATA, "couverture_branches.csv"))

sommaire <- indicateurs %>%
  dplyr::filter(retenu) %>%
  dplyr::group_by(branche) %>%
  dplyr::summarise(
    dossier     = dossier_branche(dplyr::first(branche)),
    mensuel     = dplyr::n_distinct(id_serie[frequence == "mensuel"]),
    trimestriel = dplyr::n_distinct(id_serie[frequence == "trimestriel"]),
    total       = dplyr::n_distinct(id_serie),
    debut       = min(date), fin = max(date), n_obs = dplyr::n(),
    .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(total))
ecrire_csv(sommaire, file.path(DOSSIER_DATA, "sommaire_vivier.csv"))

# ============================================================================
# 6) RECAPITULATIF
# ============================================================================
cat("\n=== PHASE 1 : BASE CONSTRUITE ===\n\n")
cat(sprintf("Cible       : %d branches | %d obs. | %s -> %s\n",
            dplyr::n_distinct(cibles$branche), nrow(cibles),
            cibles$trimestre[which.min(cibles$date)],
            cibles$trimestre[which.max(cibles$date)]))
ind_ok <- indicateurs %>% dplyr::filter(retenu)
cat(sprintf("Indicateurs : %d series lues, %d retenues, %d ecartees\n",
            dplyr::n_distinct(indicateurs$id_serie),
            dplyr::n_distinct(ind_ok$id_serie),
            dplyr::n_distinct(indicateurs$id_serie) - dplyr::n_distinct(ind_ok$id_serie)))
cat(sprintf("              %d mensuelles, %d trimestrielles | %d obs. retenues\n",
            dplyr::n_distinct(ind_ok$id_serie[ind_ok$frequence == "mensuel"]),
            dplyr::n_distinct(ind_ok$id_serie[ind_ok$frequence == "trimestriel"]),
            nrow(ind_ok)))
cat(sprintf("Couverture  : %d branches couvertes, %d en AR(4) pur\n",
            sum(couverture$couverte), sum(!couverture$couverte)))
cat(sprintf("              AR(4) pur : %s\n",
            paste(couverture$branche[!couverture$couverte], collapse = ", ")))
cat(sprintf("Datation    : dernier jour de la periode (cible %s ... %s)\n",
            min(cibles$date), max(indicateurs$date)))

cat("\n--- Vivier par branche ---\n")
print(sommaire, n = 20)

cat("\n--- Regles retenues (a relire dans", CHEMIN_META, ") ---\n")
print(metadonnees %>% dplyr::count(agregation, transformation, name = "n_series"), n = 20)

cat(sprintf("\n[6/6] %d fichiers ecrits dans %s/ :\n", nrow(fichiers) + 4L, DOSSIER_DATA))
cat("   VA_branches.csv | metadonnees_indicateurs.csv | couverture_branches.csv | sommaire_vivier.csv\n")
for (b in unique(fichiers$branche)) {
  f <- fichiers[fichiers$branche == b, ]
  cat(sprintf("   %-26s %s\n", dossier_branche(b),
              paste(sprintf("%s (%d series x %d periodes)",
                            basename(f$fichier), f$n_series, f$n_periodes),
                    collapse = " | ")))
}
cat("\n   metadonnees_indicateurs.csv <- A RELIRE ET CORRIGER A LA MAIN\n")
