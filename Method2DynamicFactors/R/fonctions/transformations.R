# ============================================================================
# fonctions/transformations.R -- Mise en forme stationnaire des series
# ============================================================================
# Plan de correction : section 2 ("Phase 2") et section 32 ("Etape 2").
#
# PRINCIPE
#   Une transformation est utilisable dans un backtest si, appliquee a
#   l'origine T, elle donne exactement le meme resultat que la transformation
#   plein echantillon tronquee en T. Formellement :
#
#       f( X_{<=T} )  ==  f( X )|_{<=T}
#
#   C'est vrai des operations LOCALES, qui ne regardent que t et t-1 :
#     dlog   g_t = log(X_t) - log(X_{t-1})
#     diff   g_t = X_t - X_{t-1}
#     niveau g_t = X_t
#
#   C'est FAUX de toute operation qui calibre un parametre sur l'echantillon :
#     moyennes, ecarts-types, standardisation, lissage, filtres, imputations.
#   Ces operations doivent etre recalculees a chaque origine -- d'ou
#   standardiser_recursif() plus bas, et l'interdiction d'un lissage
#   bidirectionnel dans le backtest (section 4 du plan).
#
# CONVENTION D'ECHELLE
#   Δlog brut (pas 100·Δlog). Une seule convention, tenue partout : les
#   resultats se lisent en points de log, convertis en pourcentage a
#   l'affichage uniquement.
# ============================================================================

#' Transforme un vecteur de valeurs ordonnees par date.
#'
#' @param valeurs vecteur numerique, ordonne par date croissante.
#' @param transformation "dlog", "diff" ou "niveau".
#' @param dates dates correspondantes (sert a controler l'ordre et a reperer
#'   les trous : une difference calculee par-dessus un trou n'a pas le meme
#'   sens qu'une difference d'une periode a l'autre).
#' @param frequence "mensuel" ou "trimestriel", pour mesurer l'ecart attendu.
#' @param marquer_sauts si TRUE, les differences calculees par-dessus un trou
#'   sont mises a NA plutot que d'etre presentees comme des variations d'une
#'   periode. Par defaut TRUE : c'est le traitement honnete du jagged edge, le
#'   comblement des trous est un choix de modelisation qui releve de la
#'   phase 4, pas de la transformation.
#'
#' @return vecteur de meme longueur, premiere valeur NA pour dlog et diff.
transformer_serie <- function(valeurs, transformation,
                              dates = NULL, frequence = NULL,
                              marquer_sauts = TRUE) {
  transformation <- match.arg(transformation, c("dlog", "diff", "niveau"))
  n <- length(valeurs)
  if (n == 0L) return(numeric(0))

  if (transformation == "niveau") return(as.numeric(valeurs))

  if (transformation == "dlog") {
    if (any(valeurs <= 0, na.rm = TRUE)) {
      stop("[TRANSFORMATION] dlog demande sur une serie comportant des valeurs ",
           "negatives ou nulles : utiliser 'diff'.", call. = FALSE)
    }
    out <- c(NA_real_, diff(log(valeurs)))
  } else {
    out <- c(NA_real_, diff(as.numeric(valeurs)))
  }

  # --- trous : une difference qui enjambe un trou n'est pas une variation
  #     d'une periode a l'autre, on ne la presente pas comme telle.
  if (marquer_sauts && !is.null(dates) && !is.null(frequence) && n > 1L) {
    d <- as.Date(dates)
    pas_attendu <- if (frequence[1] == "mensuel") 1L else 3L
    ecart_mois <- c(NA_integer_,
                    lubridate::interval(utils::head(d, -1L), utils::tail(d, -1L)) %/% months(1))
    # tolerance d'un jour de calendrier : les fins de mois donnent 0 ou 1 mois
    # selon la longueur des mois, on arrondit sur l'ecart en jours.
    ecart_jours <- c(NA_real_, as.numeric(diff(d)))
    pas_reel <- round(ecart_jours / 30.44)
    out[!is.na(pas_reel) & pas_reel > pas_attendu] <- NA_real_
  }

  out
}

#' Applique a une base longue les regles de transformation portees par les
#' metadonnees. La base doit contenir : id_serie, date, valeur, frequence,
#' et une colonne `transformation`.
#'
#' Operation purement locale : le resultat pour une date t ne depend que de t
#' et de la periode precedente. Aucun parametre n'est estime sur l'echantillon,
#' donc aucun look-ahead possible -- ce que verifie
#' verifier_transformation_locale().
appliquer_transformations <- function(data, marquer_sauts = TRUE) {
  stopifnot(all(c("id_serie", "date", "valeur", "frequence", "transformation")
                %in% names(data)))
  data %>%
    dplyr::arrange(id_serie, date) %>%
    dplyr::group_by(id_serie) %>%
    dplyr::mutate(
      valeur_t = transformer_serie(valeur, dplyr::first(transformation),
                                   dates = date, frequence = frequence,
                                   marquer_sauts = marquer_sauts)
    ) %>%
    dplyr::ungroup()
}

#' Standardisation RECURSIVE : moyenne et ecart-type calcules uniquement sur
#' l'information disponible a chaque date.
#'
#'   z_t = (x_t - mean(x_{s<=t})) / sd(x_{s<=t})
#'
#' A ne pas confondre avec scale(x), qui utilise la moyenne et l'ecart-type de
#' TOUTE la serie, futur compris, et qui est donc interdit dans le backtest.
#' Fournie ici parce que la phase 3 (BVAR) et la phase 5 (selection) peuvent en
#' avoir besoin ; non utilisee par defaut.
#'
#' @param min_obs nombre minimal d'observations avant de produire une valeur.
standardiser_recursif <- function(x, min_obs = 8L) {
  n <- length(x)
  out <- rep(NA_real_, n)
  for (t in seq_len(n)) {
    passe <- x[seq_len(t)]
    passe <- passe[!is.na(passe)]
    if (length(passe) < min_obs) next
    s <- stats::sd(passe)
    if (is.na(s) || s == 0) next
    out[t] <- (x[t] - mean(passe)) / s
  }
  out
}

#' Controle de non-look-ahead d'une transformation (section 30, controle 3).
#'
#' Verifie sur une serie que transformer puis tronquer donne le meme resultat
#' que tronquer puis transformer. Renvoie TRUE si la transformation est locale.
#' Sert de test de non-regression : si quelqu'un ajoute un lissage ou une
#' standardisation plein echantillon dans la chaine, ce controle echoue.
verifier_transformation_locale <- function(valeurs, dates, transformation,
                                           frequence, origines = NULL) {
  plein <- transformer_serie(valeurs, transformation, dates, frequence)
  d <- as.Date(dates)
  if (is.null(origines)) {
    idx <- unique(round(seq(max(5L, length(d) %/% 4L), length(d), length.out = 5L)))
    origines <- d[idx]
  }
  for (o in as.Date(origines)) {
    k <- d <= o
    if (sum(k) < 3L) next
    tronque <- transformer_serie(valeurs[k], transformation, d[k], frequence)
    reference <- plein[k]
    if (!isTRUE(all.equal(tronque, reference, tolerance = 1e-12))) return(FALSE)
  }
  TRUE
}
