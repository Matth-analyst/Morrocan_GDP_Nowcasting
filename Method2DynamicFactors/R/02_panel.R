# ============================================================================
# 02_panel.R -- Le panel mensuel a frequence mixte, branche par branche
# ============================================================================
# Le modele a facteurs travaille sur une grille MENSUELLE. Il faut donc y
# ranger trois objets de natures differentes :
#
#   - les indicateurs mensuels, a leur mois ;
#   - les indicateurs trimestriels, au DERNIER mois de leur trimestre ;
#   - la cible trimestrielle, au dernier mois de son trimestre egalement.
#
# POURQUOI LE DERNIER MOIS, ET NON LE PREMIER
#   La convention du projet veut qu'une observation soit datee de la fin de sa
#   periode : c'est la date a laquelle elle entre dans l'ensemble
#   d'information. Poser une valeur trimestrielle au premier mois du trimestre
#   reviendrait a pretendre la connaitre deux mois trop tot -- une anteriorite
#   de deux mois, silencieuse, sur toutes les series trimestrielles a la fois.
#
#   C'est aussi la seule convention compatible avec les poids d'agregation
#   temporelle : la croissance du trimestre qui s'acheve en t depend des
#   croissances mensuelles de t a t-4, et non de t+2 a t-2.
#
# CE QUI N'EST PAS FAIT ICI
#   Aucune standardisation. Centrer et reduire suppose une moyenne et un
#   ecart-type, qui doivent etre calcules sur la seule information anterieure a
#   chaque origine. Les faire ici, une fois pour toutes, introduirait une
#   anteriorite. La standardisation est donc reportee a l'estimation.
#
#   Aucun filtre fonde sur la cible. Un modele a facteurs extrait une dynamique
#   commune DU PANEL ; choisir les series d'apres leur lien avec la cible
#   reviendrait a decider a l'avance ce que les facteurs doivent contenir, et a
#   le decider en regardant toute la periode.
#
# SORTIES
#   data/panel/<Branche>_panel.csv   le panel mensuel, colonnes = series
#   data/panel_manifeste.csv         nature et role de chaque colonne
#   resultats/02_panel_bilan.csv     volumetrie et taux d'observation
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")

DOSSIER_PANEL <- file.path(DOSSIER_DATA, "panel")
dir.create(DOSSIER_PANEL, showWarnings = FALSE, recursive = TRUE)
chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("02_", x))

# Filtres de qualite, calcules sans jamais regarder la cible.
MIN_OBS_SERIE <- 12L     # observations apres transformation
MIN_VAR       <- 1e-10   # variance non nulle

cat("\n[1/4] Lecture\n")
couv <- charger_couverture()
meta <- charger_metadonnees()
va   <- charger_va()
ind  <- charger_indicateurs(branches = couv$couvertes)
cat(sprintf("      %d branches couvertes | %d series | %d branches sans indicateur\n",
            length(couv$couvertes), dplyr::n_distinct(ind$id_serie),
            length(couv$non_couvertes)))

# ============================================================================
# 2) LA CIBLE, EN CROISSANCE TRIMESTRIELLE
# ============================================================================
cat("\n[2/4] Cible\n")
cible <- va %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(g)) %>%
  dplyr::transmute(branche, date, valeur = g)
cat(sprintf("      %d trimestres de croissance, de %s a %s\n",
            dplyr::n_distinct(cible$date),
            date_vers_trimestre(min(cible$date)),
            date_vers_trimestre(max(cible$date))))

# ============================================================================
# 3) CONSTRUCTION DU PANEL, BRANCHE PAR BRANCHE
# ============================================================================
cat("\n[3/4] Panels mensuels\n")

#' Ramene une date au dernier jour de son mois.
fin_mois <- function(d) {
  d <- as.Date(d)
  lubridate::ceiling_date(d, "month") - 1L
}

bilan <- list(); manifeste <- list()

for (b in couv$couvertes) {
  ind_b <- ind %>% dplyr::filter(branche == b)
  if (nrow(ind_b) == 0L) next

  # --- transformation de chaque serie, selon sa regle economique -----------
  # `charger_indicateurs` porte deja la regle de transformation : la joindre a
  # nouveau creerait des colonnes en double.
  ind_b <- ind_b %>%
    dplyr::arrange(id_serie, date) %>%
    dplyr::group_by(id_serie) %>%
    dplyr::mutate(x = transformer_serie(valeur, transformation[1], date,
                                        frequence[1])) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(x))

  # --- filtres de qualite, sans la cible ----------------------------------
  qual <- ind_b %>% dplyr::group_by(id_serie) %>%
    dplyr::summarise(n = dplyr::n(), v = stats::var(x), .groups = "drop") %>%
    dplyr::filter(n >= MIN_OBS_SERIE, is.finite(v), v > MIN_VAR)
  ind_b <- ind_b %>% dplyr::filter(id_serie %in% qual$id_serie)
  if (nrow(ind_b) == 0L) next

  # --- grille mensuelle ----------------------------------------------------
  cible_b <- cible %>% dplyr::filter(branche == b)
  debut <- min(c(ind_b$date, cible_b$date))
  fin   <- max(c(ind_b$date, cible_b$date))
  # PIEGE : seq(as.Date("1998-01-31"), by = "month") ne donne PAS des fins de
  # mois. R ajoute un mois au quantieme, et le 31 fevrier n'existant pas, la
  # suite deborde sur le 3 mars puis derive. La grille doit donc etre engendree
  # a partir des DEBUTS de mois, puis ramenee a la fin de chaque mois.
  grille <- fin_mois(seq(lubridate::floor_date(debut, "month"),
                         lubridate::floor_date(fin, "month"), by = "month"))

  # Les dates sont deja en fin de periode : une trimestrielle datee du
  # 31 mars tombe naturellement sur le mois de mars.
  large <- tibble::tibble(date = grille)

  for (s in unique(ind_b$id_serie)) {
    v <- ind_b %>% dplyr::filter(id_serie == s) %>%
      dplyr::transmute(date = fin_mois(date), !!s := x)
    large <- large %>% dplyr::left_join(v, by = "date")
  }
  large <- large %>%
    dplyr::left_join(cible_b %>% dplyr::transmute(date = fin_mois(date),
                                                  CIBLE = valeur), by = "date")

  ecrire_csv(large, file.path(DOSSIER_PANEL, sprintf("%s_panel.csv",
                                                     dossier_branche(b))))

  freqs <- ind_b %>% dplyr::distinct(id_serie, frequence)
  manifeste[[length(manifeste) + 1L]] <- tibble::tibble(
    branche = b,
    colonne = c(freqs$id_serie, "CIBLE"),
    role = c(rep("indicateur", nrow(freqs)), "cible"),
    frequence = c(freqs$frequence, "trimestriel"))

  n_obs <- colSums(!is.na(large[, -1, drop = FALSE]))
  bilan[[length(bilan) + 1L]] <- tibble::tibble(
    branche = b, mois = nrow(large),
    series = ncol(large) - 2L,
    mensuelles = sum(freqs$frequence == "mensuel"),
    trimestrielles = sum(freqs$frequence == "trimestriel"),
    taux_observation = mean(!is.na(as.matrix(large[, -1, drop = FALSE]))),
    obs_cible = sum(!is.na(large$CIBLE)))
  cat(sprintf("      %-28s %3d mois | %3d series (%3d mens. + %2d trim.) | %.0f %% observe\n",
              b, nrow(large), ncol(large) - 2L,
              sum(freqs$frequence == "mensuel"),
              sum(freqs$frequence == "trimestriel"),
              100 * mean(!is.na(as.matrix(large[, -1, drop = FALSE])))))
}

manifeste <- dplyr::bind_rows(manifeste)
bilan <- dplyr::bind_rows(bilan)
ecrire_csv(manifeste, file.path(DOSSIER_DATA, "panel_manifeste.csv"))
ecrire_csv(bilan, chemin_res("panel_bilan.csv"))

# ============================================================================
# 4) CONTROLES
# ============================================================================
cat("\n[4/4] Controles\n")

# C1 : la cible n'est observee qu'aux fins de trimestre.
mois_cible <- manifeste %>% dplyr::filter(role == "cible") %>% nrow()
ok1 <- TRUE
for (b in unique(bilan$branche)) {
  pan <- lire_csv(file.path(DOSSIER_PANEL, sprintf("%s_panel.csv", dossier_branche(b)))) %>%
    dplyr::mutate(date = as.Date(date))
  m <- as.integer(format(pan$date[!is.na(pan$CIBLE)], "%m"))
  ok1 <- ok1 && all(m %in% c(3L, 6L, 9L, 12L))
}
cat(sprintf("      [%s] la cible n'apparait qu'aux mois 3, 6, 9 et 12\n",
            if (ok1) "OK" else "ECHEC"))

# C2 bis : la grille est bien mensuelle et sans trou.
ok_grille <- TRUE
for (b in unique(bilan$branche)) {
  pan <- lire_csv(file.path(DOSSIER_PANEL, sprintf("%s_panel.csv", dossier_branche(b)))) %>%
    dplyr::mutate(date = as.Date(date))
  ecarts <- as.integer(diff(pan$date))
  ok_grille <- ok_grille && all(ecarts >= 28L & ecarts <= 31L) &&
    all(pan$date == fin_mois(pan$date))
}
cat(sprintf("      [%s] la grille avance d'un mois exactement, sur des fins de mois
",
            if (ok_grille) "OK" else "ECHEC"))

# C3 : aucune observation n'est perdue a la pose sur la grille.
perdu <- 0L; total <- 0L
for (b in unique(bilan$branche)) {
  pan <- lire_csv(file.path(DOSSIER_PANEL, sprintf("%s_panel.csv", dossier_branche(b))))
  ib <- ind %>% dplyr::filter(branche == b) %>%
    dplyr::arrange(id_serie, date) %>% dplyr::group_by(id_serie) %>%
    dplyr::mutate(x = transformer_serie(valeur, transformation[1], date,
                                        frequence[1])) %>%
    dplyr::ungroup() %>% dplyr::filter(!is.na(x)) %>%
    dplyr::filter(id_serie %in% names(pan))
  attendu <- ib %>% dplyr::count(id_serie, name = "n")
  obtenu <- colSums(!is.na(pan[, intersect(names(pan), attendu$id_serie), drop = FALSE]))
  attendu <- attendu[match(names(obtenu), attendu$id_serie), ]
  perdu <- perdu + sum(pmax(attendu$n - obtenu, 0L)); total <- total + sum(attendu$n)
}
cat(sprintf("      [%s] observations perdues a la pose : %d sur %d (%.2f %%)
",
            if (perdu == 0L) "OK" else "ECHEC", perdu, total,
            100 * perdu / max(total, 1L)))

# C2 : aucune serie n'est constante apres transformation.
ok2 <- TRUE
for (b in unique(bilan$branche)) {
  pan <- lire_csv(file.path(DOSSIER_PANEL, sprintf("%s_panel.csv", dossier_branche(b))))
  v <- vapply(pan[, -1, drop = FALSE], function(x) stats::var(x, na.rm = TRUE), numeric(1))
  ok2 <- ok2 && all(is.na(v) | v > MIN_VAR)
}
cat(sprintf("      [%s] aucune serie constante dans les panels\n",
            if (ok2) "OK" else "ECHEC"))

# C3 : le taux d'observation d'une trimestrielle avoisine un tiers.
tx <- bilan %>% dplyr::filter(trimestrielles > 0L)
cat(sprintf("      [info] taux d'observation global : de %.0f %% a %.0f %% selon la branche\n",
            100 * min(bilan$taux_observation), 100 * max(bilan$taux_observation)))
cat("      Ce taux n'est PAS un indicateur de lacune : une serie trimestrielle\n")
cat("      posee sur une grille mensuelle est observee un mois sur trois par\n")
cat("      construction, sans qu'aucune donnee ne manque.\n")

stopifnot("La cible doit tomber en fin de trimestre" = ok1,
          "Aucune serie constante ne doit subsister" = ok2,
          "La grille doit etre mensuelle et sur des fins de mois" = ok_grille,
          "Aucune observation ne doit etre perdue a la pose" = perdu == 0L)

cat(sprintf("\n      %d branches | %d series au total | %d colonnes de cible\n",
            nrow(bilan), sum(bilan$series), mois_cible))
cat("\nPanels mensuels construits.\n")
