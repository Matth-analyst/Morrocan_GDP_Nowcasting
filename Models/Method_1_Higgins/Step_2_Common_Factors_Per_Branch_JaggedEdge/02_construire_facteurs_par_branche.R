# ==============================================================================
# 02_construire_facteurs_par_branche.R
#
# Etape 2b (GDPNow-Maroc) -- Construction d'un facteur commun latent par
# branche, a partir des series retenues (CIBLE exclue), par l'algorithme EM
# de Stock & Watson (2002) -- meme methode que celle deja appliquee et
# validee pour Agriculture dans ce travail.
#
# ENTREE : les CSV produits par 01_extraire_series_retenues_vers_csv.py,
#          dans ./csv/<branche>_trimestriel.csv et ./csv/<branche>_mensuel.csv
# SORTIE : ./resultats/<branche>_facteur.csv (le facteur, une valeur par
#          trimestre) et ./resultats/<branche>_charges.csv (le poids de
#          chaque serie dans le facteur), ./figures/<branche>_facteur.png
#          (visualisation), ./resultats/recapitulatif.csv (bilan global)
#
# METHODE, dans l'ordre exact (rappel) :
#   1. Constituer le panel (series retenues, CIBLE exclue)
#   2. Delta-log de chaque serie
#   3. Standardisation individuelle (moyenne 0, ecart-type 1)
#   4. Panel complet avec trous (union des dates)
#   5. Initialisation des valeurs manquantes a 0
#   6. Extraction du facteur par SVD (1ere composante principale)
#   7. Re-estimation des valeurs manquantes par projection sur le facteur
#   8. Iteration 6-7 jusqu'a convergence (tolerance 1e-6)
#   9. Sortie : le facteur F_t, une valeur par trimestre
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(stringr)
})

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
DOSSIER_CSV <- "csv_avec_series_deflatees"
DOSSIER_RESULTATS <- "resultats"
DOSSIER_FIGURES <- "figures"
TOLERANCE_CONVERGENCE <- 1e-6
MAX_ITERATIONS <- 300

dir.create(DOSSIER_RESULTATS, showWarnings = FALSE)
dir.create(DOSSIER_FIGURES, showWarnings = FALSE)

# Liste des branches disposant d'au moins un indicateur (cf. Phase 2).
# Le nom doit correspondre exactement au prefixe des fichiers CSV.
BRANCHES <- c(
  "Agriculture", "Peche", "Industrie_transformation", "Industrie_extraction",
  "Finances_assurances", "Hebergement_restauration", "Construction",
  "Commerce", "Transports", "Electricite_gaz_eau",
  "Information_communication", "Immobilier"
)

# ------------------------------------------------------------------------------
# Etape 1-2 : charger le panel d'une branche (CIBLE exclue), Delta-log
# ------------------------------------------------------------------------------
charger_panel_branche <- function(nom_branche) {
  chemin_trim <- file.path(DOSSIER_CSV, paste0(nom_branche, "_trimestriel.csv"))
  chemin_mens <- file.path(DOSSIER_CSV, paste0(nom_branche, "_mensuel.csv"))

  liste_series <- list()

  # --- Bloc trimestriel : la 1ere colonne (hors Date) est toujours la CIBLE,
  #     exclue ici -- seules les colonnes suivantes (ancres/selection trim.)
  #     entrent dans le panel du facteur.
  if (file.exists(chemin_trim)) {
    df_trim <- read_csv(chemin_trim, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
    df_trim$Date <- as.Date(df_trim$Date)
    cols <- names(df_trim)[-1]  # retire "Date"
    if (length(cols) > 1) {     # retire la CIBLE (1ere colonne de donnees)
      cols_sans_cible <- cols[-1]
      for (col in cols_sans_cible) {
        s <- df_trim %>% select(Date, valeur = all_of(col)) %>% filter(!is.na(valeur))
        if (nrow(s) >= 8) liste_series[[col]] <- s
      }
    }
  }

  # --- Bloc mensuel : AUCUNE cible ici (la cible n'existe qu'en trimestriel)
  #     -- on trimestrialise chaque serie (moyenne des 3 mois du trimestre)
  #     avant de l'ajouter au panel, pour rester a une frequence commune,
  #     coherente avec la methode deja appliquee (precipitations/temperature
  #     pour Agriculture).
  if (file.exists(chemin_mens)) {
    df_mens <- read_csv(chemin_mens, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
    df_mens$Date <- as.Date(df_mens$Date)
    cols <- names(df_mens)[-1]
    for (col in cols) {
      s <- df_mens %>%
        select(Date, valeur = all_of(col)) %>%
        filter(!is.na(valeur)) %>%
        mutate(Trimestre = floor_date(Date, unit = "quarter")) %>%
        group_by(Trimestre) %>%
        summarise(valeur = mean(valeur, na.rm = TRUE), .groups = "drop") %>%
        rename(Date = Trimestre)
      if (nrow(s) >= 8) liste_series[[col]] <- s
    }
  }

  if (length(liste_series) == 0) return(NULL)

  # --- Delta-log de chaque serie (Etape 2) ------------------------------------
  liste_ld <- lapply(liste_series, function(s) {
    s <- s %>% arrange(Date) %>% filter(valeur > 0)
    if (nrow(s) < 8) return(NULL)
    s$delta_log <- c(NA, diff(log(s$valeur)))
    s %>% select(Date, delta_log) %>% filter(!is.na(delta_log))
  })
  liste_ld <- liste_ld[!sapply(liste_ld, is.null)]
  if (length(liste_ld) == 0) return(NULL)

  liste_ld
}

# ------------------------------------------------------------------------------
# Etapes 3-9 : standardisation, panel, algorithme EM (facteur par SVD)
# ------------------------------------------------------------------------------
construire_facteur_EM <- function(liste_ld, nom_branche) {
  noms_series <- names(liste_ld)

  # --- Etape 3 : standardiser chaque serie individuellement -------------------
  liste_std <- lapply(liste_ld, function(s) {
    m <- mean(s$delta_log, na.rm = TRUE)
    sd_ <- sd(s$delta_log, na.rm = TRUE)
    if (is.na(sd_) || sd_ == 0) return(NULL)
    s$standardise <- (s$delta_log - m) / sd_
    s %>% select(Date, standardise)
  })
  liste_std <- liste_std[!sapply(liste_std, is.null)]
  noms_series <- names(liste_std)
  if (length(liste_std) < 2) {
    warning(paste(nom_branche, ": moins de 2 series exploitables, facteur non calcule"))
    return(NULL)
  }

  # --- Etape 4 : panel complet (union des dates), avec trous ------------------
  toutes_dates <- sort(unique(do.call(c, lapply(liste_std, function(s) s$Date))))
  panel <- matrix(NA_real_, nrow = length(toutes_dates), ncol = length(liste_std))
  colnames(panel) <- noms_series
  rownames(panel) <- as.character(toutes_dates)
  for (nom in noms_series) {
    s <- liste_std[[nom]]
    idx <- match(s$Date, toutes_dates)
    panel[idx, nom] <- s$standardise
  }

  n_manquants <- sum(is.na(panel))
  cat(sprintf("  [%s] Panel : %d trimestres x %d series, %d valeurs manquantes (%.0f%%)\n",
              nom_branche, nrow(panel), ncol(panel), n_manquants,
              100 * n_manquants / (nrow(panel) * ncol(panel))))

  # --- Etape 5 : initialisation des valeurs manquantes a 0 --------------------
  panel_rempli <- panel
  panel_rempli[is.na(panel_rempli)] <- 0

  # --- Etapes 6-8 : algorithme EM iteratif ------------------------------------
  facteur_precedent <- NULL
  charges <- NULL
  facteur <- NULL
  n_iter_reelles <- 0

  for (iteration in 1:MAX_ITERATIONS) {
    n_iter_reelles <- iteration
    X_centre <- scale(panel_rempli, center = TRUE, scale = FALSE)
    svd_res <- svd(X_centre)
    facteur <- svd_res$u[, 1] * svd_res$d[1]
    charges <- svd_res$v[, 1]

    # Normaliser le signe : facteur positivement correle a la 1ere serie
    # (convention arbitraire mais systematique, pour la reproductibilite)
    if (charges[1] < 0) {
      facteur <- -facteur
      charges <- -charges
    }

    if (!is.null(facteur_precedent)) {
      delta <- max(abs(facteur - facteur_precedent))
      if (delta < TOLERANCE_CONVERGENCE) break
    }
    facteur_precedent <- facteur

    # Etape 7 : re-estimer les valeurs manquantes par projection sur le facteur
    for (j in seq_along(noms_series)) {
      manquants <- is.na(panel[, j])
      if (any(manquants)) {
        panel_rempli[manquants, j] <- charges[j] * facteur[manquants]
      }
    }
  }

  cat(sprintf("  [%s] Convergence en %d iteration(s)\n", nom_branche, n_iter_reelles))
  if (n_iter_reelles >= MAX_ITERATIONS) {
    warning(sprintf("[%s] N'A PAS CONVERGE apres %d iterations -- resultat a interpreter avec prudence",
                     nom_branche, MAX_ITERATIONS))
  }

  list(
    facteur = data.frame(Date = toutes_dates, facteur = facteur),
    charges = data.frame(serie = noms_series, charge = charges),
    n_series = length(noms_series),
    n_trimestres = length(toutes_dates),
    n_iterations = n_iter_reelles
  )
}

# ------------------------------------------------------------------------------
# Figure diagnostique : evolution du facteur + charges (barplot)
# ------------------------------------------------------------------------------
tracer_figure_facteur <- function(resultat, nom_branche) {
  p1 <- ggplot(resultat$facteur, aes(x = Date, y = facteur)) +
    geom_line(color = "#1F4E78", linewidth = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(title = paste0("Facteur commun \u2014 ", gsub("_", " ", nom_branche)),
         subtitle = sprintf("%d series, %d trimestres, convergence en %d iteration(s)",
                             resultat$n_series, resultat$n_trimestres, resultat$n_iterations),
         x = NULL, y = "Facteur (standardise)") +
    theme_minimal(base_size = 11)

  charges_tri <- resultat$charges %>% arrange(desc(abs(charge)))
  p2 <- ggplot(charges_tri, aes(x = reorder(str_trunc(serie, 35), charge), y = charge)) +
    geom_col(fill = "#BF9000") +
    coord_flip() +
    labs(title = "Charges (loadings) sur le facteur", x = NULL, y = "Charge") +
    theme_minimal(base_size = 9)

  chemin <- file.path(DOSSIER_FIGURES, paste0(nom_branche, "_facteur.png"))
  png(chemin, width = 2400, height = 1400, res = 200)
  gridExtra::grid.arrange(p1, p2, ncol = 2, widths = c(1.3, 1))
  dev.off()
  chemin
}

# ------------------------------------------------------------------------------
# Boucle principale : une extraction de facteur par branche
# ------------------------------------------------------------------------------
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra", repos = "https://cloud.r-project.org")
}

recapitulatif <- data.frame()

for (branche in BRANCHES) {
  cat(sprintf("\n=== %s ===\n", branche))
  panel_liste <- charger_panel_branche(branche)

  if (is.null(panel_liste) || length(panel_liste) < 2) {
    cat(sprintf("  [%s] Pas assez de series exploitables -- facteur non construit\n", branche))
    recapitulatif <- rbind(recapitulatif, data.frame(
      branche = branche, n_series = length(panel_liste), n_trimestres = NA,
      n_iterations = NA, statut = "NON CONSTRUIT (< 2 series)"
    ))
    next
  }

  resultat <- construire_facteur_EM(panel_liste, branche)
  if (is.null(resultat)) {
    recapitulatif <- rbind(recapitulatif, data.frame(
      branche = branche, n_series = length(panel_liste), n_trimestres = NA,
      n_iterations = NA, statut = "ECHEC EM"
    ))
    next
  }

  # --- Sauvegarde des resultats ------------------------------------------------
  write_csv(resultat$facteur, file.path(DOSSIER_RESULTATS, paste0(branche, "_facteur.csv")))
  write_csv(resultat$charges, file.path(DOSSIER_RESULTATS, paste0(branche, "_charges.csv")))
  chemin_fig <- tracer_figure_facteur(resultat, branche)
  cat(sprintf("  [%s] Sauvegarde : resultats/%s_facteur.csv, resultats/%s_charges.csv, %s\n",
              branche, branche, branche, chemin_fig))

  recapitulatif <- rbind(recapitulatif, data.frame(
    branche = branche, n_series = resultat$n_series, n_trimestres = resultat$n_trimestres,
    n_iterations = resultat$n_iterations, statut = "OK"
  ))
}

write_csv(recapitulatif, file.path(DOSSIER_RESULTATS, "recapitulatif.csv"))
cat("\n=== RECAPITULATIF ===\n")
print(recapitulatif)
cat(sprintf("\nTermine. Resultats dans '%s/', figures dans '%s/'.\n",
            DOSSIER_RESULTATS, DOSSIER_FIGURES))
