# ============================================================================
# fonctions/information_set.R -- Logique temporelle centrale du projet
# ============================================================================
# Plan de correction : sections 0, 28 et 30.
#
# REGLE ABSOLUE : pour prevoir le trimestre T, aucune information datant de
# T+1 ou apres ne peut entrer dans le modele. Toute la logique temporelle du
# pipeline passe par ce fichier : aucun script de modelisation ne doit filtrer
# les dates "a la main".
#
# CONVENTION DE DATATION DU PROJET
#   Toute observation est datee au DERNIER JOUR de sa periode :
#       mensuel      2024-01-31  ->  mois de janvier 2024
#       trimestriel  2024-03-31  ->  T1-2024
#
#   Le classeur source date au PREMIER jour ; la conversion est faite une fois
#   pour toutes a la lecture (phase 1). L'interet est direct : une periode
#   n'est observee qu'a sa fin, donc la date porte desormais elle-meme le
#   moment ou l'observation entre dans l'ensemble d'information. La
#   comparaison `date <= borne` suffit, sans colonne auxiliaire.
#
# CONVENTION D'INFORMATION (section 0)
#   - toutes les donnees des trimestres precedents sont disponibles ;
#   - les trois observations mensuelles du trimestre T sont disponibles ;
#   - les indicateurs trimestriels de T sont disponibles ;
#   - aucune observation de T+1 ou posterieure n'est utilisee.
#   Ce n'est PAS un pseudo-temps reel fonde sur les vintages de publication
#   (indisponibles) : c'est un nowcasting fonde sur la date d'observation.
# ============================================================================

# ----------------------------------------------------------------------------
# 1) Bornes de periode
# ----------------------------------------------------------------------------

#' Premier / dernier jour du mois contenant d
debut_mois <- function(d) lubridate::floor_date(as.Date(d), "month")
fin_mois   <- function(d) lubridate::ceiling_date(debut_mois(d), "month") - 1L

#' Premier / dernier jour du trimestre contenant d
#'   2024-01-15 -> debut 2024-01-01, fin 2024-03-31
debut_trimestre <- function(d) lubridate::floor_date(as.Date(d), "quarter")
fin_trimestre   <- function(d) lubridate::ceiling_date(debut_trimestre(d), "quarter") - 1L

#' Fin de periode selon la frequence. Vectorise sur d ET sur frequence.
#' C'est la fonction qui applique la convention du projet a la lecture.
fin_periode <- function(d, frequence) {
  d <- as.Date(d)
  out <- fin_trimestre(d)
  est_mensuel <- frequence == "mensuel"
  out[est_mensuel] <- fin_mois(d[est_mensuel])
  out
}

# ----------------------------------------------------------------------------
# 2) Conversions trimestre <-> date
# ----------------------------------------------------------------------------

#' "T1-2014" -> Date de FIN du trimestre (2014-03-31). Vectorise.
trimestre_vers_date <- function(x) {
  m <- stringr::str_match(as.character(x), "^T([1-4])[-/ ]?(\\d{4})$")
  out <- rep(as.Date(NA), length(x))
  ok <- !is.na(m[, 1L])
  if (any(ok)) {
    t <- as.integer(m[ok, 2L]); y <- as.integer(m[ok, 3L])
    out[ok] <- fin_trimestre(as.Date(sprintf("%d-%02d-01", y, (t - 1L) * 3L + 1L)))
  }
  out
}

#' Date quelconque -> libelle "T1-2014"
date_vers_trimestre <- function(d) {
  d <- as.Date(d)
  ifelse(is.na(d), NA_character_,
         sprintf("T%d-%d", lubridate::quarter(d), lubridate::year(d)))
}

#' Trimestre precedent / suivant, exprimes en FIN de trimestre
trimestre_precedent <- function(d) fin_trimestre(debut_trimestre(d) %m-% months(3))
trimestre_suivant   <- function(d) fin_trimestre(debut_trimestre(d) %m+% months(3))

#' Les 3 mois du trimestre de d, en FIN de mois
mois_du_trimestre <- function(d) fin_mois(debut_trimestre(d) %m+% months(0:2))

# ----------------------------------------------------------------------------
# 3) Verification de la convention de datation
# ----------------------------------------------------------------------------

#' Controle : toute observation est bien datee au DERNIER jour de sa periode.
#' Appele apres la lecture -- si la convention est rompue, le pipeline doit
#' s'arreter, pas deviner.
verifier_datation <- function(data, nom = "donnees", colonne = "date") {
  stopifnot(all(c(colonne, "frequence") %in% names(data)))
  d <- as.Date(data[[colonne]])
  attendu <- fin_periode(d, data$frequence)
  mauvais <- which(!is.na(d) & d != attendu)
  if (length(mauvais) > 0L) {
    ex <- utils::head(unique(sprintf("%s %s (attendu %s)", data$frequence[mauvais],
                                     d[mauvais], attendu[mauvais])), 5L)
    stop(sprintf("[DATATION] %s : %d observation(s) ne sont pas au dernier jour de leur periode -- %s",
                 nom, length(mauvais), paste(ex, collapse = " ; ")), call. = FALSE)
  }
  invisible(TRUE)
}

# ----------------------------------------------------------------------------
# 4) Ensemble d'information
# ----------------------------------------------------------------------------

#' Ensemble d'information I_T : ne conserve que ce qui est OBSERVE au plus tard
#' a `target_date`. Les dates etant en fin de periode, la comparaison est
#' directe.
#'
#' @param data data.frame comportant une colonne `date`.
#' @param target_date borne haute incluse. Typiquement fin_trimestre(T).
information_set <- function(data, target_date) {
  target_date <- as.Date(target_date)
  d <- as.Date(data$date)
  out <- data[!is.na(d) & d <= target_date, , drop = FALSE]
  verifier_information_set(out, target_date)
  out
}

#' Ensemble d'information intra-trimestriel (sections 13 et 14) :
#'   M0 -> 0 mois du trimestre cible observe, indicateur trimestriel indisponible
#'   M1 -> 1 mois observe,                    indicateur trimestriel indisponible
#'   M2 -> 2 mois observes,                   indicateur trimestriel indisponible
#'   M3 -> 3 mois observes,                   indicateur trimestriel DISPONIBLE
#'
#' Les observations non encore disponibles ne sont pas "supprimees du
#' probleme" : elles devront etre PREVUES en aval (section 13). Cette fonction
#' definit seulement ce qui est observe.
information_set_intra <- function(data, target_quarter,
                                  scenario = c("M3", "M2", "M1", "M0")) {
  scenario <- match.arg(scenario)
  stopifnot("frequence" %in% names(data))
  n_mois <- switch(scenario, M0 = 0L, M1 = 1L, M2 = 2L, M3 = 3L)

  debut <- debut_trimestre(target_quarter)
  fin   <- fin_trimestre(target_quarter)
  d     <- as.Date(data$date)

  # 1) tout ce qui precede strictement le trimestre cible
  avant <- !is.na(d) & d < debut

  # 2) les n_mois premiers mois du trimestre cible, series mensuelles
  dans_mois <- if (n_mois == 0L) {
    rep(FALSE, nrow(data))
  } else {
    limite <- fin_mois(debut %m+% months(n_mois - 1L))
    !is.na(d) & data$frequence == "mensuel" & d >= debut & d <= limite
  }

  # 3) l'indicateur trimestriel du trimestre cible, uniquement en M3
  dans_trim <- if (scenario == "M3") {
    !is.na(d) & data$frequence == "trimestriel" & d >= debut & d <= fin
  } else {
    rep(FALSE, nrow(data))
  }

  out <- data[avant | dans_mois | dans_trim, , drop = FALSE]
  verifier_information_set(out, fin)
  out
}

# ----------------------------------------------------------------------------
# 5) Controles anti-look-ahead (section 30)
# ----------------------------------------------------------------------------

#' Controle 2 -- aucune donnee utilisee pour la PREVISION n'est posterieure a
#' l'origine autorisee.
verifier_information_set <- function(data, target_date, nom = "information_set") {
  if (nrow(data) == 0L) return(invisible(TRUE))
  pire <- suppressWarnings(max(as.Date(data$date), na.rm = TRUE))
  if (is.finite(pire) && pire > as.Date(target_date)) {
    stop(sprintf("[ANTI-LOOK-AHEAD] %s : observation du %s > borne autorisee %s",
                 nom, pire, as.Date(target_date)), call. = FALSE)
  }
  invisible(TRUE)
}

#' Controle 1 -- l'echantillon d'ESTIMATION s'arrete strictement AVANT la cible.
verifier_echantillon_estimation <- function(training_data, target_date,
                                            nom = "estimation") {
  if (nrow(training_data) == 0L) return(invisible(TRUE))
  pire <- suppressWarnings(max(as.Date(training_data$date), na.rm = TRUE))
  if (is.finite(pire) && pire >= as.Date(target_date)) {
    stop(sprintf("[ANTI-LOOK-AHEAD] %s : observation du %s >= cible %s",
                 nom, pire, as.Date(target_date)), call. = FALSE)
  }
  invisible(TRUE)
}

#' Controle 4 -- les poids sectoriels proviennent d'une periode anterieure a T.
verifier_poids <- function(date_poids, target_quarter, nom = "poids") {
  if (as.Date(date_poids) >= debut_trimestre(target_quarter)) {
    stop(sprintf("[ANTI-LOOK-AHEAD] %s : date %s >= trimestre cible %s",
                 nom, as.Date(date_poids), debut_trimestre(target_quarter)),
         call. = FALSE)
  }
  invisible(TRUE)
}
