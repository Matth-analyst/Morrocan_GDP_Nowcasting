# ==============================================================================
# 03_combler_jagged_edge.R
#
# Etape 3 (GDPNow-Maroc) -- Pour chaque serie retenue de chaque branche,
# estimer une autoregression augmentee du facteur commun de sa branche
# (Higgins 2014, equation 3 / Stock & Watson 2002), avec q (retards propres)
# et r (retards du facteur) selectionnes par AIC -- exactement comme Higgins
# choisit q et r a son Etape 3.
#
#   Delta-log(y_i,t) = alpha_i + sum_{k=1}^{q} gamma_k * Delta-log(y_i,t-k)
#                              + sum_{j=0}^{r} beta_j  * F_t-j
#
# ENTREE :
#   - csv/<branche>_trimestriel.csv, csv/<branche>_mensuel.csv (Etape "01")
#   - resultats/<branche>_facteur.csv (Etape "02")
# SORTIE :
#   - resultats/<branche>_<serie>_coefficients.csv (alpha, gamma_k, beta_j retenus)
#   - resultats/<branche>_<serie>_previsions.csv (valeurs ajustees + prevision
#     du dernier trimestre, utilisable pour combler un trou de publication)
#   - resultats/recapitulatif_jagged_edge.csv (AIC retenu, q*, r*, par serie)
#   - figures/<branche>_<serie>_ajustement.png (serie observee vs ajustee)
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
Q_MAX <- 4   # nombre max de retards propres testes (0 a Q_MAX)
R_MAX <- 4   # nombre max de retards du facteur testes (0 a R_MAX)

dir.create(DOSSIER_RESULTATS, showWarnings = FALSE)
dir.create(DOSSIER_FIGURES, showWarnings = FALSE)

BRANCHES <- c(
  "Agriculture", "Peche", "Industrie_transformation", "Industrie_extraction",
  "Finances_assurances", "Hebergement_restauration", "Construction",
  "Commerce", "Transports", "Electricite_gaz_eau",
  "Information_communication", "Immobilier"
)

# ------------------------------------------------------------------------------
# Chargement du facteur d'une branche (produit par 02_construire_facteurs...)
# ------------------------------------------------------------------------------f
charger_facteur <- function(nom_branche) {
  chemin <- file.path(DOSSIER_RESULTATS, paste0(nom_branche, "_facteur.csv"))
  if (!file.exists(chemin)) return(NULL)
  f <- read_csv(chemin, show_col_types = FALSE)
  f$Date <- as.Date(f$Date)
  f
}

# ------------------------------------------------------------------------------
# Chargement de toutes les series retenues d'une branche (CIBLE exclue),
# a frequence trimestrielle (les series mensuelles sont trimestrialisees --
# meme formule que pour la construction du facteur, equation 1 du rapport)
# ------------------------------------------------------------------------------
charger_series_branche <- function(nom_branche) {
  chemin_trim <- file.path(DOSSIER_CSV, paste0(nom_branche, "_trimestriel.csv"))
  chemin_mens <- file.path(DOSSIER_CSV, paste0(nom_branche, "_mensuel.csv"))

  liste_series <- list()

  if (file.exists(chemin_trim)) {
    df <- read_csv(chemin_trim, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
    df$Date <- as.Date(df$Date)
    cols <- names(df)[-1]
    if (length(cols) > 1) {
      for (col in cols[-1]) {  # -1 retire la CIBLE (1ere colonne de donnees)
        s <- df %>% select(Date, valeur = all_of(col)) %>% filter(!is.na(valeur))
        if (nrow(s) >= 12) liste_series[[col]] <- s
      }
    }
  }

  if (file.exists(chemin_mens)) {
    df <- read_csv(chemin_mens, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
    df$Date <- as.Date(df$Date)
    cols <- names(df)[-1]
    for (col in cols) {
      s <- df %>%
        select(Date, valeur = all_of(col)) %>%
        filter(!is.na(valeur)) %>%
        mutate(Trimestre = floor_date(Date, unit = "quarter")) %>%
        group_by(Trimestre) %>%
        summarise(valeur = mean(valeur, na.rm = TRUE), .groups = "drop") %>%
        rename(Date = Trimestre)
      if (nrow(s) >= 12) liste_series[[col]] <- s
    }
  }

  liste_series
}

# ------------------------------------------------------------------------------
# Construction des retards (lags) -- utilitaire
# ------------------------------------------------------------------------------
decaler <- function(x, k) {
  n <- length(x)
  if (k == 0) return(x)
  c(rep(NA, k), x[1:(n - k)])
}

# ------------------------------------------------------------------------------
# Pour UNE serie : Delta-log, fusion avec le facteur, selection AIC de (q,r),
# estimation finale, prevision du dernier trimestre disponible
# ------------------------------------------------------------------------------
estimer_serie <- function(serie_df, facteur_df, nom_serie, nom_branche) {
  # Detecter les series DEJA exprimees en variation (ex. IPAI, reconstruites
  # par Denton avec des variation_trimestrielle en %) -- pour elles, on ne
  # doit PAS refaire un Delta-log (elles ne sont pas des niveaux positifs,
  # mais deja des taux de croissance, potentiellement negatifs). On les
  # detecte par leur nom (motif "var. trim" ou "variation"), et on les
  # ramene simplement d'une echelle en points de pourcentage (-7.8) a une
  # fraction decimale (-0.078), pour rester sur la meme echelle que les
  # Delta-log calcules pour toutes les autres series.
  deja_variation <- grepl("var\\. trim|variation", nom_serie, ignore.case = TRUE)

  serie_df <- serie_df %>% arrange(Date)
  if (deja_variation) {
    serie_df$delta_log <- serie_df$valeur / 100
  } else {
    serie_df <- serie_df %>% filter(valeur > 0)
    serie_df$delta_log <- c(NA, diff(log(serie_df$valeur)))
  }
  if (nrow(serie_df) < Q_MAX + R_MAX + 10) return(NULL)  # pas assez de points

  serie_df <- serie_df %>% filter(!is.na(delta_log)) %>% select(Date, delta_log)

  # Fusion avec le facteur sur la date (inner join -- seuls les trimestres
  # ou les deux existent sont utilisables pour l'estimation)
  fusion <- inner_join(serie_df, facteur_df, by = "Date") %>% arrange(Date)
  if (nrow(fusion) < Q_MAX + R_MAX + 10) return(NULL)

  y <- fusion$delta_log
  f <- fusion$facteur
  n <- length(y)

  # --- Construire toutes les colonnes de retards possibles (0..Q_MAX pour y,
  #     0..R_MAX pour f), une fois pour toutes -----------------------------
  X_y <- sapply(1:Q_MAX, function(k) decaler(y, k))
  colnames(X_y) <- paste0("y_lag", 1:Q_MAX)
  X_f <- sapply(0:R_MAX, function(j) decaler(f, j))
  colnames(X_f) <- paste0("f_lag", 0:R_MAX)

  # --- Recherche en grille : pour chaque (q,r), estimer le modele et
  #     calculer son AIC, sur le MEME echantillon (les lignes completes
  #     pour le plus grand modele possible, Q_MAX/R_MAX), pour que les AIC
  #     soient comparables entre eux (meme n a chaque fois) --------------
  ligne_complete <- complete.cases(cbind(y, X_y, X_f))
  y_sub <- y[ligne_complete]
  X_y_sub <- X_y[ligne_complete, , drop = FALSE]
  X_f_sub <- X_f[ligne_complete, , drop = FALSE]

  if (length(y_sub) < 10) return(NULL)

  meilleur_aic <- Inf
  meilleur_q <- 0; meilleur_r <- 0
  tableau_aic <- data.frame()

  for (q in 0:Q_MAX) {
    for (r in 0:R_MAX) {
      cols_X <- c(if (q > 0) paste0("y_lag", 1:q) else NULL,
                  paste0("f_lag", 0:r))
      X_modele <- cbind(X_y_sub[, if (q > 0) 1:q else 0, drop = FALSE],
                          X_f_sub[, 1:(r + 1), drop = FALSE])
      if (q == 0) X_modele <- X_f_sub[, 1:(r + 1), drop = FALSE]
      df_modele <- data.frame(y = y_sub, X_modele)
      modele <- tryCatch(lm(y ~ ., data = df_modele), error = function(e) NULL)
      if (is.null(modele)) next
      aic_val <- AIC(modele)
      tableau_aic <- rbind(tableau_aic, data.frame(q = q, r = r, AIC = aic_val))
      if (aic_val < meilleur_aic) {
        meilleur_aic <- aic_val
        meilleur_q <- q
        meilleur_r <- r
      }
    }
  }

  if (nrow(tableau_aic) == 0) return(NULL)

  # --- Reestimer le modele final avec (q*, r*), sur toutes les lignes
  #     disponibles pour CE modele precis (pas seulement l'echantillon
  #     commun a Q_MAX/R_MAX, pour ne pas perdre de donnees inutilement) --
  q <- meilleur_q; r <- meilleur_r
  X_y_final <- if (q > 0) sapply(1:q, function(k) decaler(y, k)) else NULL
  X_f_final <- sapply(0:r, function(j) decaler(f, j))
  if (r == 0) X_f_final <- matrix(X_f_final, ncol = 1)
  if (q > 0 && is.null(dim(X_y_final))) X_y_final <- matrix(X_y_final, ncol = q)

  X_complet <- if (q > 0) cbind(X_y_final, X_f_final) else X_f_final
  noms_col <- c(if (q > 0) paste0("y_lag", 1:q) else NULL, paste0("f_lag", 0:r))
  colnames(X_complet) <- noms_col

  df_final <- data.frame(Date = fusion$Date, y = y, X_complet)
  df_final_complet <- df_final[complete.cases(df_final), ]

  modele_final <- lm(y ~ ., data = df_final_complet %>% select(-Date))

  # --- Valeurs ajustees (in-sample) -------------------------------------------
  df_final_complet$ajuste <- predict(modele_final)

  # --- Prevision du prochain trimestre (si le facteur est disponible
  #     au-dela de la derniere observation de la serie -- exactement le
  #     cas d'usage du jagged edge : la serie manque, le facteur, lui,
  #     peut deja integrer de l'information plus recente) -----------------
  derniere_date_serie <- max(df_final_complet$Date)
  prochain_trimestre <- derniere_date_serie %m+% months(3)
  facteur_futur <- facteur_df %>% filter(Date >= derniere_date_serie) %>% arrange(Date)

  prevision_disponible <- FALSE
  valeur_prevue <- NA
  if (nrow(facteur_futur) >= r + 1) {
    # construire le vecteur de predicteurs pour la prevision hors-echantillon
    y_hist <- df_final_complet$y
    f_hist <- c(facteur_df$facteur[facteur_df$Date <= derniere_date_serie], facteur_futur$facteur)
    nouvelle_ligne <- data.frame(matrix(NA, nrow = 1, ncol = length(noms_col)))
    colnames(nouvelle_ligne) <- noms_col
    if (q > 0) {
      for (k in 1:q) {
        idx <- length(y_hist) - k + 1
        nouvelle_ligne[[paste0("y_lag", k)]] <- if (idx >= 1) y_hist[idx] else NA
      }
    }
    for (j in 0:r) {
      idx <- length(f_hist) - j
      nouvelle_ligne[[paste0("f_lag", j)]] <- if (idx >= 1) f_hist[idx] else NA
    }
    if (!any(is.na(nouvelle_ligne))) {
      valeur_prevue <- predict(modele_final, newdata = nouvelle_ligne)
      prevision_disponible <- TRUE
    }
  }

  list(
    nom_serie = nom_serie, nom_branche = nom_branche,
    q = q, r = r, aic = meilleur_aic, n_obs = nrow(df_final_complet),
    modele = modele_final, tableau_aic = tableau_aic,
    ajustement = df_final_complet %>% select(Date, y, ajuste),
    prochain_trimestre = prochain_trimestre,
    valeur_prevue = valeur_prevue,
    prevision_disponible = prevision_disponible
  )
}

# ------------------------------------------------------------------------------
# Figure : serie observee vs ajustee (+ prevision si disponible)
# ------------------------------------------------------------------------------
tracer_figure_ajustement <- function(resultat, chemin) {
  df <- resultat$ajustement %>%
    rename(Observe = y, Ajuste = ajuste) %>%
    pivot_longer(cols = c(Observe, Ajuste), names_to = "type", values_to = "valeur")

  p <- ggplot(df, aes(x = Date, y = valeur, color = type)) +
    geom_line(linewidth = 0.6) +
    scale_color_manual(values = c("Observe" = "black", "Ajuste" = "#C55A11")) +
    labs(title = str_trunc(paste0(resultat$nom_branche, " \u2014 ", resultat$nom_serie), 70),
         subtitle = sprintf("q=%d, r=%d (AIC=%.1f, n=%d)%s",
                             resultat$q, resultat$r, resultat$aic, resultat$n_obs,
                             if (resultat$prevision_disponible)
                               sprintf(" | Prevision %s : %.3f", resultat$prochain_trimestre, resultat$valeur_prevue)
                             else ""),
         x = NULL, y = "Delta-log", color = NULL) +
    theme_minimal(base_size = 10)
  ggsave(chemin, p, width = 9, height = 4, dpi = 150)
}

# ------------------------------------------------------------------------------
# Boucle principale
# ------------------------------------------------------------------------------
recapitulatif <- data.frame()

for (branche in BRANCHES) {
  cat(sprintf("\n=== %s ===\n", branche))
  facteur_df <- charger_facteur(branche)
  if (is.null(facteur_df)) {
    cat(sprintf("  [%s] Facteur non trouve -- executer 02_construire_facteurs_par_branche.R d'abord\n", branche))
    next
  }
  series_branche <- charger_series_branche(branche)
  if (length(series_branche) == 0) {
    cat(sprintf("  [%s] Aucune serie exploitable\n", branche))
    next
  }

  for (nom_serie in names(series_branche)) {
    resultat <- tryCatch(
      estimer_serie(series_branche[[nom_serie]], facteur_df, nom_serie, branche),
      error = function(e) { cat(sprintf("  [ERREUR] %s : %s\n", nom_serie, e$message)); NULL }
    )
    if (is.null(resultat)) {
      cat(sprintf("  [%s] pas assez de donnees -- ignoree\n", str_trunc(nom_serie, 50)))
      next
    }

    nom_fichier <- str_trunc(str_replace_all(nom_serie, "[^A-Za-z0-9]+", "_"), 60)
    prefixe <- paste0(branche, "_", nom_fichier)

    # Coefficients
    coefs <- as.data.frame(summary(resultat$modele)$coefficients)
    coefs$variable <- rownames(coefs)
    write_csv(coefs, file.path(DOSSIER_RESULTATS, paste0(prefixe, "_coefficients.csv")))

    # Ajustement + prevision
    previsions_df <- resultat$ajustement
    if (resultat$prevision_disponible) {
      previsions_df <- rbind(previsions_df, data.frame(
        Date = resultat$prochain_trimestre, y = NA, ajuste = resultat$valeur_prevue
      ))
    }
    write_csv(previsions_df, file.path(DOSSIER_RESULTATS, paste0(prefixe, "_previsions.csv")))

    # Figure
    tracer_figure_ajustement(resultat, file.path(DOSSIER_FIGURES, paste0(prefixe, "_ajustement.png")))

    cat(sprintf("  [%s] q=%d r=%d AIC=%.1f n=%d%s\n",
                str_trunc(nom_serie, 45), resultat$q, resultat$r, resultat$aic, resultat$n_obs,
                if (resultat$prevision_disponible) sprintf(" | prevu %s: %.3f", resultat$prochain_trimestre, resultat$valeur_prevue) else " | pas de prevision (facteur pas assez recent)"))

    recapitulatif <- rbind(recapitulatif, data.frame(
      branche = branche, serie = nom_serie, q = resultat$q, r = resultat$r,
      AIC = resultat$aic, n_obs = resultat$n_obs,
      prevision_disponible = resultat$prevision_disponible,
      valeur_prevue = resultat$valeur_prevue
    ))
  }
}

write_csv(recapitulatif, file.path(DOSSIER_RESULTATS, "recapitulatif_jagged_edge.csv"))
cat("\n=== RECAPITULATIF (extrait) ===\n")
print(head(recapitulatif, 15))
cat(sprintf("\n%d series traitees au total. Resultats dans '%s/', figures dans '%s/'.\n",
            nrow(recapitulatif), DOSSIER_RESULTATS, DOSSIER_FIGURES))