# ==============================================================================
# 04_construire_bridge_equations.R
#
# Etape 4 (GDPNow-Maroc, Higgins 2014) -- Pour chaque branche, relie enfin
# les indicateurs retenus a la cible (VA) :
#
#   Equations INDIVIDUELLES (une par indicateur) :
#     Delta-log(VA_t) = beta_0 + beta_1 * Delta-log(indicateur_t) + eps_t
#
#   Equation COMBINEE (une par branche, tous ses indicateurs comme
#   candidats, selection pas-a-pas par AIC pour eviter le surajustement) :
#     Delta-log(VA_t) = beta_0 + sum_i beta_i * Delta-log(indicateur_i,t) + eps_t
#
# Note methodologique : l'estimation utilise les donnees HISTORIQUES
# completes (pas de jagged edge sur des donnees deja publiees) -- le
# mecanisme de comblement (script 03) n'intervient qu'au moment de
# PRODUIRE un nowcast en temps reel, pas ici, a l'estimation.
#
# ENTREE : csv/<branche>_trimestriel.csv, csv/<branche>_mensuel.csv
# SORTIE :
#   - resultats/<branche>_individuelles.csv (une ligne par indicateur :
#     beta1, R2, p-value, AIC)
#   - resultats/<branche>_combinee_coefficients.csv (modele combine retenu)
#   - resultats/recapitulatif_bridge.csv (bilan de toutes les branches)
#   - figures/<branche>_combinee_ajustement.png (VA observee vs ajustee)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(stringr)
})

DOSSIER_CSV <- "csv"
DOSSIER_RESULTATS <- "resultats"
DOSSIER_FIGURES <- "figures"
DATE_DEBUT_TRAIN <- as.Date("2010-01-01")
DATE_FIN_TRAIN <- as.Date("2021-01-01")
dir.create(DOSSIER_RESULTATS, showWarnings = FALSE)
dir.create(DOSSIER_FIGURES, showWarnings = FALSE)

BRANCHES <- c(
  "Agriculture", "Peche", "Industrie_transformation", "Industrie_extraction",
  "Finances_assurances", "Hebergement_restauration", "Construction",
  "Commerce", "Transports", "Electricite_gaz_eau",
  "Information_communication", "Immobilier"
)

# ------------------------------------------------------------------------------
# Chargement : cible (VA) + tous les indicateurs retenus, a frequence
# trimestrielle (les mensuels sont trimestrialises -- moyenne des 3 mois,
# meme formule que dans les scripts 02 et 03)
# ------------------------------------------------------------------------------
charger_branche <- function(nom_branche) {
  chemin_trim <- file.path(DOSSIER_CSV, paste0(nom_branche, "_trimestriel.csv"))
  chemin_mens <- file.path(DOSSIER_CSV, paste0(nom_branche, "_mensuel.csv"))

  cible <- NULL
  indicateurs <- list()

  if (file.exists(chemin_trim)) {
    df <- read_csv(chemin_trim, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
    df$Date <- as.Date(df$Date)
    cols <- names(df)[-1]
    if (length(cols) >= 1) {
      cible <- df %>% select(Date, valeur = all_of(cols[1])) %>% filter(!is.na(valeur))
    }
    if (length(cols) > 1) {
      for (col in cols[-1]) {
        s <- df %>% select(Date, valeur = all_of(col)) %>% filter(!is.na(valeur))
        if (nrow(s) >= 12) indicateurs[[col]] <- s
      }
    }
  }

  if (file.exists(chemin_mens)) {
    df <- read_csv(chemin_mens, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
    df$Date <- as.Date(df$Date)
    for (col in names(df)[-1]) {
      s <- df %>%
        select(Date, valeur = all_of(col)) %>%
        filter(!is.na(valeur)) %>%
        mutate(Trimestre = floor_date(Date, unit = "quarter")) %>%
        group_by(Trimestre) %>%
        summarise(valeur = mean(valeur, na.rm = TRUE), .groups = "drop") %>%
        rename(Date = Trimestre)
      if (nrow(s) >= 12) indicateurs[[col]] <- s
    }
  }

  list(cible = cible, indicateurs = indicateurs)
}

# ------------------------------------------------------------------------------
# Delta-log, avec detection des series DEJA en variation (meme regle que
# le script 03 -- cas de l'IPAI reconstruit par Denton)
# ------------------------------------------------------------------------------
deltalog_serie <- function(df, nom_serie) {
  deja_variation <- grepl("var\\. trim|variation", nom_serie, ignore.case = TRUE)
  df <- df %>% arrange(Date)
  if (deja_variation) {
    df$delta_log <- df$valeur / 100
  } else {
    df <- df %>% filter(valeur > 0)
    df$delta_log <- c(NA, diff(log(df$valeur)))
  }
  df %>% filter(!is.na(delta_log)) %>% select(Date, delta_log)
}

# ------------------------------------------------------------------------------
# Bridge equation INDIVIDUELLE : VA ~ 1 indicateur
# ------------------------------------------------------------------------------
estimer_bridge_individuelle <- function(cible_ld, indic_ld, nom_indic) {
  # RESTRICTION AU TRAIN (2010-2021) -- coherence obligatoire avec le reste
  # de ce travail (Phase 2, Niveau 2, deflation, facteurs) : jamais utiliser
  # de donnees hors de cette fenetre pour estimer quoi que ce soit.
  cible_ld <- cible_ld %>% filter(Date >= DATE_DEBUT_TRAIN, Date <= DATE_FIN_TRAIN)
  indic_ld <- indic_ld %>% filter(Date >= DATE_DEBUT_TRAIN, Date <= DATE_FIN_TRAIN)
  fusion <- inner_join(cible_ld, indic_ld, by = "Date", suffix = c("_va", "_ind"))
  if (nrow(fusion) < 12) return(NULL)

  modele <- lm(delta_log_va ~ delta_log_ind, data = fusion)
  s <- summary(modele)
  data.frame(
    indicateur = nom_indic,
    beta0 = coef(modele)[1],
    beta1 = coef(modele)[2],
    p_value = s$coefficients[2, 4],
    R2 = s$r.squared,
    R2_ajuste = s$adj.r.squared,
    AIC = AIC(modele),
    n_obs = nrow(fusion)
  )
}

# ------------------------------------------------------------------------------
# Bridge equation COMBINEE : VA ~ tous les indicateurs de la branche,
# selection pas-a-pas (stepwise) par AIC dans les deux sens
# ------------------------------------------------------------------------------
estimer_bridge_combinee <- function(cible_ld, liste_indic_ld) {
  # RESTRICTION AU TRAIN (2010-2021) -- meme regle que la fonction individuelle
  cible_ld <- cible_ld %>% filter(Date >= DATE_DEBUT_TRAIN, Date <= DATE_FIN_TRAIN)
  liste_indic_ld <- lapply(liste_indic_ld, function(s) s %>% filter(Date >= DATE_DEBUT_TRAIN, Date <= DATE_FIN_TRAIN))

  # Fusionner la cible avec TOUS les indicateurs disponibles (sur la date)
  df <- cible_ld %>% rename(VA = delta_log)
  for (nom in names(liste_indic_ld)) {
    nom_col <- str_trunc(str_replace_all(nom, "[^A-Za-z0-9]+", "_"), 25)
    base <- nom_col; i <- 2
    while (nom_col %in% names(df)) { nom_col <- paste0(base, "_", i); i <- i + 1 }
    df <- left_join(df, liste_indic_ld[[nom]] %>% rename(!!nom_col := delta_log), by = "Date")
  }

  # Ne garder que les lignes ou la cible ET au moins 1 indicateur existent ;
  # le modele complet (avec toutes les colonnes) exige des lignes completes.
  # Seuil PROPORTIONNE au nombre de regresseurs (pas un seuil fixe) : au
  # moins 5 degres de liberte residuels au-dela du nombre de parametres
  # (nombre d'indicateurs + 1 pour la constante), sinon le modele complet
  # (avant meme la selection AIC) est deja surajuste par construction.
  df_complet <- df[complete.cases(df), ]
  n_indicateurs_candidats <- ncol(df_complet) - 2  # -Date -VA
  seuil_min <- n_indicateurs_candidats + 1 + 5
  if (nrow(df_complet) < seuil_min || ncol(df_complet) < 3) return(NULL)

  noms_indicateurs <- setdiff(names(df_complet), c("Date", "VA"))
  formule_max <- as.formula(paste("VA ~", paste(noms_indicateurs, collapse = " + ")))
  modele_max <- lm(formule_max, data = df_complet %>% select(-Date))
  modele_min <- lm(VA ~ 1, data = df_complet %>% select(-Date))

  modele_final <- tryCatch(
    step(modele_min, scope = list(lower = modele_min, upper = modele_max),
         direction = "both", trace = 0),
    error = function(e) modele_max
  )

  list(modele = modele_final, donnees = df_complet, n_obs = nrow(df_complet))
}

# ------------------------------------------------------------------------------
# Figure : VA observee vs ajustee (modele combine)
# ------------------------------------------------------------------------------
tracer_figure_bridge <- function(donnees, modele, nom_branche, chemin) {
  donnees$Ajuste <- predict(modele, newdata = donnees)
  df_long <- donnees %>%
    select(Date, VA, Ajuste) %>%
    rename(Observe = VA) %>%
    pivot_longer(cols = c(Observe, Ajuste), names_to = "type", values_to = "valeur")

  r2 <- summary(modele)$r.squared
  p <- ggplot(df_long, aes(x = Date, y = valeur, color = type)) +
    geom_line(linewidth = 0.6) +
    scale_color_manual(values = c("Observe" = "black", "Ajuste" = "#1F4E78")) +
    labs(title = paste0("Bridge equation combin\u00e9e \u2014 ", gsub("_", " ", nom_branche)),
         subtitle = sprintf("R\u00b2 = %.3f, n = %d", r2, nrow(donnees)),
         x = NULL, y = "Delta-log(VA)", color = NULL) +
    theme_minimal(base_size = 11)
  ggsave(chemin, p, width = 8, height = 4, dpi = 150)
}

# ------------------------------------------------------------------------------
# Boucle principale
# ------------------------------------------------------------------------------
recapitulatif <- data.frame()

for (branche in BRANCHES) {
  cat(sprintf("\n=== %s ===\n", branche))
  donnees <- charger_branche(branche)
  if (is.null(donnees$cible) || length(donnees$indicateurs) == 0) {
    cat(sprintf("  [%s] Pas de cible ou aucun indicateur -- ignoree (branche sans donnees)\n", branche))
    next
  }

  cible_ld <- deltalog_serie(donnees$cible, "VA")

  # --- Equations individuelles ------------------------------------------------
  resultats_indiv <- list()
  liste_indic_ld <- list()
  for (nom in names(donnees$indicateurs)) {
    indic_ld <- deltalog_serie(donnees$indicateurs[[nom]], nom)
    liste_indic_ld[[nom]] <- indic_ld
    r <- estimer_bridge_individuelle(cible_ld, indic_ld, nom)
    if (!is.null(r)) resultats_indiv[[nom]] <- r
  }

  if (length(resultats_indiv) > 0) {
    df_indiv <- do.call(rbind, resultats_indiv)
    write_csv(df_indiv, file.path(DOSSIER_RESULTATS, paste0(branche, "_individuelles.csv")))
    df_indiv_tri <- df_indiv %>% arrange(desc(R2))
    for (i in 1:min(3, nrow(df_indiv_tri))) {
      cat(sprintf("  [indiv] %-40s beta1=%.3f R2=%.3f p=%.4f\n",
                  str_trunc(df_indiv_tri$indicateur[i], 40),
                  df_indiv_tri$beta1[i], df_indiv_tri$R2[i], df_indiv_tri$p_value[i]))
    }
  }

  # --- Equation combinee -------------------------------------------------------
  combinee <- tryCatch(estimer_bridge_combinee(cible_ld, liste_indic_ld),
                         error = function(e) { cat(sprintf("  [combinee] erreur: %s\n", e$message)); NULL })

  statut <- "OK"
  r2_combinee <- NA; n_regresseurs <- NA; n_obs_combinee <- NA

  if (!is.null(combinee)) {
    modele <- combinee$modele
    s <- summary(modele)
    r2_combinee <- s$r.squared
    n_regresseurs <- length(coef(modele)) - 1
    n_obs_combinee <- combinee$n_obs

    coefs_df <- as.data.frame(s$coefficients)
    coefs_df$variable <- rownames(coefs_df)
    write_csv(coefs_df, file.path(DOSSIER_RESULTATS, paste0(branche, "_combinee_coefficients.csv")))

    if (n_regresseurs > 0) {
      tracer_figure_bridge(combinee$donnees, modele, branche,
                             file.path(DOSSIER_FIGURES, paste0(branche, "_combinee_ajustement.png")))
    }
    cat(sprintf("  [combinee] %d regresseur(s) retenu(s), R2=%.3f, n=%d\n",
                n_regresseurs, r2_combinee, n_obs_combinee))
  } else {
    statut <- "ECHEC (donnees insuffisantes)"
    cat(sprintf("  [combinee] non estimee -- donnees insuffisantes\n"))
  }

  recapitulatif <- rbind(recapitulatif, data.frame(
    branche = branche, n_indicateurs = length(donnees$indicateurs),
    n_equations_individuelles = length(resultats_indiv),
    n_regresseurs_combinee = n_regresseurs, R2_combinee = r2_combinee,
    n_obs_combinee = n_obs_combinee, statut = statut
  ))
}

write_csv(recapitulatif, file.path(DOSSIER_RESULTATS, "recapitulatif_bridge.csv"))
cat("\n=== RECAPITULATIF ===\n")
print(recapitulatif)
cat(sprintf("\nTermine. Resultats dans '%s/', figures dans '%s/'.\n", DOSSIER_RESULTATS, DOSSIER_FIGURES))
