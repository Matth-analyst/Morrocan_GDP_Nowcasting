# ============================================================================
# 01_import_donnees.R -- Lecture du classeur et mise en forme "tidy"
# ============================================================================
# Chaque feuille de branche suit une mise en page fixe :
#   ligne 1 : titre ; ligne 2 : source de la cible ; ligne 3 : en-tetes de
#   section ; ligne 4 : sous-en-tetes (unite de date / source de chaque
#   indicateur) ; ligne 5 : vide ; donnees a partir de la ligne 6.
#   Colonnes A-B : cible (trimestre, VA). A partir de la colonne D : blocs
#   d'indicateurs mensuels puis trimestriels, chacun avec sa propre colonne
#   de date directement a sa gauche.
# ============================================================================

source("R/00_setup.R")

# --- Utilitaires de dates ---------------------------------------------------
parse_trimestre <- function(x) {
  # "T1-2014" -> Date du premier jour du trimestre
  m <- str_match(x, "^T([1-4])-(\\d{4})$")
  if (is.na(m[1,1])) return(as.Date(NA))
  t <- as.integer(m[1,2]); y <- as.integer(m[1,3])
  as.Date(sprintf("%d-%02d-01", y, (t - 1) * 3 + 1))
}
parse_mois <- function(x) {
  # "2010-09" -> Date
  d <- suppressWarnings(as.Date(paste0(x, "-01"), format = "%Y-%m-%d"))
  d
}

#' Lit une feuille de branche et renvoie une liste (cible, indicateurs)
lire_feuille_branche <- function(chemin, nom_feuille) {
  raw <- read_excel(chemin, sheet = nom_feuille, col_names = FALSE)
  raw <- as.data.frame(raw)

  source_cible <- raw[2, 1]

  # --- Cible (colonnes 1-2, donnees a partir de la ligne 6) ---
  cible <- tibble(
    trimestre_lbl = as.character(raw[6:nrow(raw), 1]),
    va = suppressWarnings(as.numeric(raw[6:nrow(raw), 2]))
  ) %>%
    filter(!is.na(trimestre_lbl), !is.na(va)) %>%
    mutate(date = map_vec(trimestre_lbl, parse_trimestre)) %>%
    filter(!is.na(date)) %>%
    arrange(date) %>%
    transmute(branche = nom_feuille, date, va)

  resultat <- list(cible = cible, source_cible = source_cible, indicateurs = NULL)

  ncols <- ncol(raw)
  if (ncols < 4) return(resultat)  # branche non couverte : pas d'indicateur

  # --- Reperer les colonnes de date (libelle "Mois" ou "Trimestre" en ligne 4)
  indicateurs_liste <- list()
  col_date_courante <- NULL
  type_date_courante <- NULL

  for (c in 4:ncols) {
    lbl_ligne4 <- as.character(raw[4, c])
    lbl_ligne3 <- as.character(raw[3, c])

    if (!is.na(lbl_ligne4) && lbl_ligne4 %in% c("Mois", "Trimestre")) {
      col_date_courante <- c
      type_date_courante <- if (lbl_ligne4 == "Mois") "mensuel" else "trimestriel"
      next
    }

    if (!is.na(lbl_ligne3) && str_detect(lbl_ligne3, "^Indicateurs")) next  # titre de section

    if (is.null(col_date_courante)) next
    if (is.na(lbl_ligne3) || lbl_ligne3 == "") next

    valeurs <- suppressWarnings(as.numeric(raw[6:nrow(raw), c]))
    dates_lbl <- as.character(raw[6:nrow(raw), col_date_courante])
    if (all(is.na(valeurs))) next

    dates <- if (type_date_courante == "mensuel") {
      map_vec(dates_lbl, parse_mois)
    } else {
      map_vec(dates_lbl, parse_trimestre)
    }

    nom_complet <- lbl_ligne3
    r_extrait <- str_match(nom_complet, "\\(r=([-+]?[0-9.]+)\\)")[1,2]
    nom_propre <- str_trim(str_remove(nom_complet, "\\s*\\(r=[-+]?[0-9.]+\\)"))
    signe_atypique <- str_detect(nom_propre, "\\[signe atypique\\]")
    nom_propre <- str_trim(str_remove(nom_propre, "\\s*\\[signe atypique\\]"))
    source_ind <- as.character(raw[4, c])

    df_ind <- tibble(date = dates, valeur = valeurs) %>%
      filter(!is.na(date), !is.na(valeur)) %>%
      arrange(date) %>%
      transmute(branche = nom_feuille, indicateur = nom_propre, source = source_ind,
                frequence = type_date_courante,
                r_declare = as.numeric(r_extrait), signe_atypique = signe_atypique,
                date, valeur)

    indicateurs_liste[[length(indicateurs_liste) + 1]] <- df_ind
  }

  resultat$indicateurs <- if (length(indicateurs_liste) > 0) bind_rows(indicateurs_liste) else NULL
  resultat
}

# --- Lecture de toutes les branches -----------------------------------------
cat("Lecture du classeur :", CHEMIN_CLASSEUR, "\n")
donnees_brutes <- map(TOUTES_BRANCHES, ~ lire_feuille_branche(CHEMIN_CLASSEUR, .x))
names(donnees_brutes) <- TOUTES_BRANCHES

cibles <- map_dfr(donnees_brutes, "cible")
indicateurs <- map_dfr(donnees_brutes, "indicateurs")

cat("\nCibles importees :", n_distinct(cibles$branche), "branches,",
    nrow(cibles), "observations au total.\n")
cat("Indicateurs importes :", n_distinct(indicateurs$branche, indicateurs$indicateur),
    "series,", nrow(indicateurs), "observations au total.\n")

saveRDS(cibles, file.path(DOSSIER_RESULTATS, "cibles.rds"))
saveRDS(indicateurs, file.path(DOSSIER_RESULTATS, "indicateurs.rds"))

cat("\nRecapitulatif par branche :\n")
print(cibles %>% group_by(branche) %>%
        summarise(debut = min(date), fin = max(date), n = n()) %>%
        arrange(match(branche, TOUTES_BRANCHES)), n = 20)
