# ==============================================================================
# 05_raffinement_branches_restantes.R
#
# Complement au script 04 : pour les branches ou le modele combine a
# echoue ou n'a rien retenu, mais dont au moins une equation individuelle
# est statistiquement significative (p<0.10) :
#
#   1. Industrie_transformation : le modele combine avec les 15 candidats
#      echouait (trop de parametres pour le train) -- reessaye ici avec
#      seulement les 6 meilleurs candidats (par R2 individuel), qui
#      partagent la meme fenetre de disponibilite (n=20).
#
#   2. Immobilier, Commerce, Industrie_extraction : le modele combine
#      echoue ou ne retient rien -- on adopte formellement leur MEILLEURE
#      equation individuelle comme bridge equation de la branche.
#
# ENTREE : csv/, resultats/*_individuelles.csv (deja produits par 04_)
# SORTIE : resultats/<branche>_modele_retenu.csv, figures/<branche>_modele_retenu.png
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lubridate)
  library(ggplot2); library(stringr)
})

DOSSIER_CSV <- "csv"
DOSSIER_RESULTATS <- "resultats"
DOSSIER_FIGURES <- "figures"
DATE_DEBUT_TRAIN <- as.Date("2010-01-01")
DATE_FIN_TRAIN <- as.Date("2021-01-01")

deltalog_serie <- function(v) {
  v <- ifelse(is.na(v) | v <= 0, NA, v)
  c(NA, diff(log(v)))
}

# ==============================================================================
# CAS 1 -- Industrie_transformation : modele combine reduit (top 6 par R2)
# ==============================================================================
cat("=== Industrie_transformation : modele reduit (top 6) ===\n")

indiv <- read_csv(file.path(DOSSIER_RESULTATS, "Industrie_transformation_individuelles.csv"),
                    show_col_types = FALSE) %>% arrange(desc(R2))
top6_noms <- indiv$indicateur[1:6]

df <- read_csv(file.path(DOSSIER_CSV, "Industrie_transformation_trimestriel.csv"),
                 show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
df$Date <- as.Date(df$Date)
cible_col <- names(df)[2]

sous <- df %>% select(Date, all_of(c(cible_col, top6_noms))) %>% arrange(Date)
for (col in names(sous)[-1]) sous[[col]] <- deltalog_serie(sous[[col]])
sous <- sous %>% filter(Date >= DATE_DEBUT_TRAIN, Date <= DATE_FIN_TRAIN)
sous_complet <- sous[complete.cases(sous), ]

cat(sprintf("Lignes completes : %d (seuil requis : 12)\n", nrow(sous_complet)))

if (nrow(sous_complet) >= 12) {
  modele_max <- lm(as.formula(paste0("`", cible_col, "` ~ .")), data = sous_complet %>% select(-Date))
  modele_min <- lm(as.formula(paste0("`", cible_col, "` ~ 1")), data = sous_complet %>% select(-Date))
  modele_final <- step(modele_min, scope = list(lower = modele_min, upper = modele_max),
                         direction = "both", trace = 0)
  s <- summary(modele_final)
  n_retenus <- length(coef(modele_final)) - 1
  cat(sprintf("Modele retenu : %d regresseur(s), R2=%.3f, n=%d\n", n_retenus, s$r.squared, nrow(sous_complet)))

  coefs_df <- as.data.frame(s$coefficients)
  coefs_df$variable <- rownames(coefs_df)
  write_csv(coefs_df, file.path(DOSSIER_RESULTATS, "Industrie_transformation_modele_retenu.csv"))

  sous_complet$Ajuste <- predict(modele_final)
  df_long <- sous_complet %>% select(Date, all_of(cible_col), Ajuste) %>%
    rename(Observe = !!cible_col) %>%
    pivot_longer(cols = c(Observe, Ajuste), names_to = "type", values_to = "valeur")
  p <- ggplot(df_long, aes(x = Date, y = valeur, color = type)) +
    geom_line(linewidth = 0.6) +
    scale_color_manual(values = c("Observe" = "black", "Ajuste" = "#1F4E78")) +
    labs(title = "Industrie de transformation \u2014 modele reduit (top 6 candidats)",
         subtitle = sprintf("%d regresseurs retenus, R\u00b2 = %.3f, n = %d", n_retenus, s$r.squared, nrow(sous_complet)),
         x = NULL, y = "Delta-log(VA)", color = NULL) +
    theme_minimal(base_size = 11)
  ggsave(file.path(DOSSIER_FIGURES, "Industrie_transformation_modele_retenu.png"), p, width = 8, height = 4, dpi = 150)
  cat("Sauvegarde : resultats/Industrie_transformation_modele_retenu.csv + figure\n")
}

# ==============================================================================
# CAS 2 -- Meilleure equation individuelle adoptee (3 branches)
# ==============================================================================
BRANCHES_MEILLEURE_INDIV <- c("Immobilier", "Commerce", "Industrie_extraction")

for (branche in BRANCHES_MEILLEURE_INDIV) {
  cat(sprintf("\n=== %s : adoption de la meilleure equation individuelle ===\n", branche))
  indiv <- read_csv(file.path(DOSSIER_RESULTATS, paste0(branche, "_individuelles.csv")),
                      show_col_types = FALSE) %>% arrange(p_value)
  meilleur <- indiv[1, ]

  if (meilleur$p_value >= 0.10) {
    cat(sprintf("  Aucun indicateur significatif (meilleur p=%.3f) -- rien a adopter, AR(4) recommande\n", meilleur$p_value))
    next
  }

  cat(sprintf("  Retenu : %s (beta1=%.3f, R2=%.3f, p=%.4f, n=%d)\n",
              meilleur$indicateur, meilleur$beta1, meilleur$R2, meilleur$p_value, meilleur$n_obs))

  write_csv(meilleur, file.path(DOSSIER_RESULTATS, paste0(branche, "_modele_retenu.csv")))

  # Figure d'ajustement pour cette equation adoptee
  df <- read_csv(file.path(DOSSIER_CSV, paste0(branche, "_trimestriel.csv")),
                   show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  df_mens <- tryCatch(read_csv(file.path(DOSSIER_CSV, paste0(branche, "_mensuel.csv")),
                                  show_col_types = FALSE, locale = locale(encoding = "UTF-8")),
                        error = function(e) NULL)
  df$Date <- as.Date(df$Date)
  cible_col <- names(df)[2]

  # Chercher l'indicateur retenu dans le fichier trimestriel ou mensuel
  if (meilleur$indicateur %in% names(df)) {
    sous <- df %>% select(Date, all_of(c(cible_col, meilleur$indicateur))) %>% arrange(Date)
  } else if (!is.null(df_mens) && meilleur$indicateur %in% names(df_mens)) {
    df_mens$Date <- as.Date(df_mens$Date)
    ind_trim <- df_mens %>% select(Date, valeur = all_of(meilleur$indicateur)) %>%
      filter(!is.na(valeur)) %>% mutate(Trimestre = floor_date(Date, "quarter")) %>%
      group_by(Trimestre) %>% summarise(valeur = mean(valeur, na.rm=TRUE), .groups="drop") %>%
      rename(Date = Trimestre)
    names(ind_trim)[2] <- meilleur$indicateur
    sous <- df %>% select(Date, all_of(cible_col)) %>% left_join(ind_trim, by = "Date") %>% arrange(Date)
  } else next

  sous[[cible_col]] <- deltalog_serie(sous[[cible_col]])
  sous[[meilleur$indicateur]] <- deltalog_serie(sous[[meilleur$indicateur]])
  sous <- sous %>% filter(Date >= DATE_DEBUT_TRAIN, Date <= DATE_FIN_TRAIN) %>%
    filter(!is.na(.data[[cible_col]]), !is.na(.data[[meilleur$indicateur]]))

  modele <- lm(as.formula(paste0("`", cible_col, "` ~ `", meilleur$indicateur, "`")), data = sous)
  sous$Ajuste <- predict(modele)
  df_long <- sous %>% select(Date, all_of(cible_col), Ajuste) %>% rename(Observe = !!cible_col) %>%
    pivot_longer(cols = c(Observe, Ajuste), names_to = "type", values_to = "valeur")
  p <- ggplot(df_long, aes(x = Date, y = valeur, color = type)) +
    geom_line(linewidth = 0.6) +
    scale_color_manual(values = c("Observe" = "black", "Ajuste" = "#1F4E78")) +
    labs(title = paste0(gsub("_"," ",branche), " \u2014 meilleure equation individuelle"),
         subtitle = sprintf("R\u00b2 = %.3f, n = %d", meilleur$R2, nrow(sous)),
         x = NULL, y = "Delta-log(VA)", color = NULL) +
    theme_minimal(base_size = 11)
  ggsave(file.path(DOSSIER_FIGURES, paste0(branche, "_modele_retenu.png")), p, width = 8, height = 4, dpi = 150)
  cat(sprintf("  Sauvegarde : resultats/%s_modele_retenu.csv + figure\n", branche))
}

cat("\nTermine.\n")
