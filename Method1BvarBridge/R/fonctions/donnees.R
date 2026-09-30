# ============================================================================
# fonctions/donnees.R -- Relecture de la base CSV produite en phase 1
# ============================================================================
# La phase 1 ecrit data/ en format large, un dossier et deux fichiers par
# branche. Les phases suivantes travaillent en format long. Ces fonctions font
# la conversion, une fois pour toutes, pour que personne n'aille relire le
# classeur Excel ni reconstruire les chemins a la main.
#
# Rappel de convention : toutes les dates sont au DERNIER JOUR de leur periode.
# ============================================================================

#' Cible : VA trimestrielle des 16 branches, format long.
#' @return branche, trimestre, date (fin de trimestre), va
charger_va <- function() {
  f <- file.path(DOSSIER_DATA, "VA_branches.csv")
  if (!file.exists(f)) {
    stop("VA_branches.csv absent : executer d'abord R/01_import_donnees.R",
         call. = FALSE)
  }
  lire_csv(f) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    tidyr::pivot_longer(dplyr::all_of(TOUTES_BRANCHES),
                        names_to = "branche", values_to = "va") %>%
    dplyr::filter(!is.na(va)) %>%
    dplyr::arrange(branche, date)
}

#' Metadonnees des indicateurs (regles d'agregation et de transformation).
charger_metadonnees <- function() {
  f <- file.path(DOSSIER_DATA, "metadonnees_indicateurs.csv")
  if (!file.exists(f)) {
    stop("metadonnees_indicateurs.csv absent : executer d'abord R/01_import_donnees.R",
         call. = FALSE)
  }
  lire_csv(f) %>%
    dplyr::mutate(dplyr::across(c(date_debut, date_fin), as.Date))
}

#' Indicateurs : relit tous les CSV de branche et renvoie une base longue.
#'
#' @param branches sous-ensemble de branches (defaut : toutes les couvertes).
#' @param frequences "mensuel", "trimestriel" ou les deux.
#' @param valides_seulement si TRUE, ecarte les series dont `retenu` est FALSE
#'   dans les metadonnees -- doublons stricts intra-branche, series constantes,
#'   et exclusions manuelles. Les CSV de branche ne contiennent deja que les
#'   series retenues ; ce filtre est une securite si la table evolue.
#' @return id_serie, branche, indicateur, frequence, date, periode, valeur,
#'   unite, agregation, transformation, role
charger_indicateurs <- function(branches = NULL,
                                frequences = c("mensuel", "trimestriel"),
                                valides_seulement = TRUE) {
  meta <- charger_metadonnees()
  if (is.null(branches)) branches <- sort(unique(meta$branche))

  lignes <- list()
  for (b in branches) {
    dos <- dossier_branche(b)
    for (f in frequences) {
      chemin <- file.path(DOSSIER_DATA, dos, sprintf("%s_%s.csv", dos, f))
      if (!file.exists(chemin)) next
      large <- lire_csv(chemin)
      if (ncol(large) <= 2L) next
      lignes[[length(lignes) + 1L]] <- large %>%
        dplyr::mutate(date = as.Date(date)) %>%
        tidyr::pivot_longer(-c(date, periode),
                            names_to = "indicateur", values_to = "valeur") %>%
        dplyr::filter(!is.na(valeur)) %>%
        dplyr::mutate(branche = b, frequence = f)
    }
  }
  if (length(lignes) == 0L) {
    stop("Aucun CSV d'indicateur trouve dans ", DOSSIER_DATA, call. = FALSE)
  }

  out <- dplyr::bind_rows(lignes) %>%
    dplyr::mutate(id_serie = paste(branche, indicateur, sep = " :: ")) %>%
    dplyr::left_join(
      meta %>% dplyr::select(id_serie, unite, agregation, transformation, role, retenu),
      by = "id_serie"
    ) %>%
    dplyr::select(id_serie, branche, indicateur, frequence, date, periode,
                  valeur, unite, agregation, transformation, role, retenu) %>%
    dplyr::arrange(branche, frequence, indicateur, date)

  if (valides_seulement) out <- out %>% dplyr::filter(retenu)
  out
}

#' Partition couvertes / non couvertes, telle que deduite en phase 1.
charger_couverture_branches <- function() charger_couverture()
