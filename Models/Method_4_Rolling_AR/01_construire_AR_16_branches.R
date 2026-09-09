# ==============================================================================
# 01_construire_AR_16_branches.R
#
# Methode 2 (comparaison a Higgins, Table 5, modele "Rolling AR(2)") --
# Construit un AR(p) INDEPENDANT pour CHACUNE des 16 branches (pas
# seulement les 7 sans bridge equation) -- p choisi par AIC, individuellement
# par branche (meme methode que l'Etape 4 du Modele 1, etendue a toutes
# les branches).
#
#   Delta-log(VA_t) = alpha + sum_{k=1}^{p} gamma_k * Delta-log(VA_t-k) + eps_t
#
# ENTREE : VA_reelle_par_branche.xlsx (meme fichier source que le Modele 1)
# SORTIE : resultats/<branche>_AR_coefficients.csv,
#          resultats/<branche>_AR_grille_AIC.csv,
#          resultats/recapitulatif_AR_16branches.csv,
#          figures/<branche>_AR_ajustement.png
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(readxl)
  library(lubridate); library(ggplot2); library(stringr)
})

DATE_DEBUT_TRAIN <- as.Date("2010-01-01")
DATE_FIN_TRAIN   <- as.Date("2021-01-01")
P_MAX <- 8   # meme grille que le Modele 1, Etape 4 (coherence de comparaison)

dir.create("resultats", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Chargement des 16 VA (meme fichier source que le Modele 1)
# ------------------------------------------------------------------------------
parse_trimestre <- function(s) {
  m <- regmatches(s, regexec("T(\\d)-(\\d{4})", s))[[1]]
  if (length(m) == 3) as.Date(sprintf("%d-%02d-01", as.integer(m[3]), (as.integer(m[2])-1)*3+1)) else NA
}
va_brut <- read_excel("VA_reelle_par_branche.xlsx", skip = 2)
names(va_brut)[1] <- "Trimestre"
va_brut$Date <- sapply(va_brut$Trimestre, parse_trimestre) |> as.Date(origin="1970-01-01")
BRANCHES_16 <- names(va_brut)[!(names(va_brut) %in% c("Trimestre","Date"))]
cat(sprintf("%d branches chargees\n", length(BRANCHES_16)))

decaler <- function(x, k) { n <- length(x); if (k==0) return(x); c(rep(NA,k), x[1:(n-k)]) }

# ------------------------------------------------------------------------------
# Pour UNE branche : Delta-log, grille p in 1..8, selection AIC, reestimation,
# prevision hors-echantillon pour CHAQUE trimestre 2021T2-2026T1 (rolling,
# un pas a la fois, en utilisant les valeurs REELLEMENT REALISEES comme
# retards a chaque etape -- coherent avec la methode d'evaluation du
# Modele 1, Etape 7)
# ------------------------------------------------------------------------------
estimer_AR_branche <- function(nom_branche) {
  df <- va_brut %>% select(Date, valeur = all_of(nom_branche)) %>% arrange(Date) %>% filter(valeur > 0)
  df$delta_log <- c(NA, diff(log(df$valeur)))
  df <- df %>% filter(!is.na(delta_log)) %>% select(Date, delta_log)

  for (k in 1:P_MAX) df[[paste0("lag", k)]] <- decaler(df$delta_log, k)

  df_train <- df %>% filter(Date >= DATE_DEBUT_TRAIN, Date <= DATE_FIN_TRAIN)
  cols_max <- paste0("lag", 1:P_MAX)
  df_train_commun <- df_train[complete.cases(df_train %>% select(delta_log, all_of(cols_max))), ]
  if (nrow(df_train_commun) < P_MAX + 5) return(NULL)

  tableau_aic <- data.frame()
  for (p in 1:P_MAX) {
    formule <- as.formula(paste("delta_log ~", paste(paste0("lag",1:p), collapse=" + ")))
    modele_p <- lm(formule, data = df_train_commun)
    tableau_aic <- rbind(tableau_aic, data.frame(p = p, AIC = AIC(modele_p)))
  }
  p_etoile <- tableau_aic$p[which.min(tableau_aic$AIC)]

  cols_final <- paste0("lag", 1:p_etoile)
  df_train_final <- df_train[complete.cases(df_train %>% select(delta_log, all_of(cols_final))), ]
  formule_finale <- as.formula(paste("delta_log ~", paste(cols_final, collapse=" + ")))
  modele_final <- lm(formule_finale, data = df_train_final)
  s <- summary(modele_final)

  list(nom_branche = nom_branche, modele = modele_final, s = s, p_etoile = p_etoile,
        tableau_aic = tableau_aic, donnees_train = df_train_final, n_obs = nrow(df_train_final),
        df_complet = df)  # df_complet : toute la serie (pour la prevision hors-echantillon roulante)
}

tracer_figure_AR <- function(resultat) {
  df <- resultat$donnees_train
  df$Ajuste <- predict(resultat$modele)
  df_long <- df %>% select(Date, delta_log, Ajuste) %>% rename(Observe = delta_log) %>%
    pivot_longer(cols = c(Observe, Ajuste), names_to = "type", values_to = "valeur")
  p <- ggplot(df_long, aes(x = Date, y = valeur, color = type)) +
    geom_line(linewidth = 0.6) +
    scale_color_manual(values = c("Observe" = "black", "Ajuste" = "#7030A0")) +
    labs(title = paste0("Rolling AR(", resultat$p_etoile, ") \u2014 ", gsub("_", " ", resultat$nom_branche)),
         subtitle = sprintf("p* = %d (AIC), R\u00b2 = %.3f, n = %d", resultat$p_etoile, resultat$s$r.squared, resultat$n_obs),
         x = NULL, y = "Delta-log(VA)", color = NULL) +
    theme_minimal(base_size = 11)
  ggsave(file.path("figures", paste0(str_replace_all(resultat$nom_branche, "[^A-Za-z0-9]+","_"), "_AR_ajustement.png")),
         p, width = 8, height = 4, dpi = 150)
}

# ------------------------------------------------------------------------------
# Boucle principale : les 16 branches
# ------------------------------------------------------------------------------
recapitulatif <- data.frame()
tous_resultats <- list()

for (branche in BRANCHES_16) {
  cat(sprintf("=== %s ===\n", branche))
  resultat <- tryCatch(estimer_AR_branche(branche), error = function(e) { cat("  ERREUR:", e$message, "\n"); NULL })
  if (is.null(resultat)) {
    recapitulatif <- rbind(recapitulatif, data.frame(branche=branche, p_etoile=NA, R2=NA, n_obs=NA, statut="ECHEC"))
    next
  }

  nom_fichier <- str_replace_all(branche, "[^A-Za-z0-9]+", "_")
  coefs_df <- as.data.frame(resultat$s$coefficients); coefs_df$variable <- rownames(coefs_df)
  write_csv(coefs_df, file.path("resultats", paste0(nom_fichier, "_AR_coefficients.csv")))
  write_csv(resultat$tableau_aic, file.path("resultats", paste0(nom_fichier, "_AR_grille_AIC.csv")))
  tracer_figure_AR(resultat)

  cat(sprintf("  p*=%d (AIC), R2=%.3f, n=%d\n", resultat$p_etoile, resultat$s$r.squared, resultat$n_obs))
  recapitulatif <- rbind(recapitulatif, data.frame(
    branche = branche, p_etoile = resultat$p_etoile, R2 = resultat$s$r.squared,
    n_obs = resultat$n_obs, statut = "OK"))
  tous_resultats[[branche]] <- resultat

  # Sauvegarde de la serie complete (Delta-log observe) pour l'agregation
  # et le test hors-echantillon (scripts suivants)
  write_csv(resultat$df_complet %>% select(Date, delta_log),
            file.path("csv", paste0(nom_fichier, "_deltalog_complet.csv")))
}

write_csv(recapitulatif, "resultats/recapitulatif_AR_16branches.csv")
saveRDS(tous_resultats, "resultats/tous_modeles_AR.rds")
cat("\n=== RECAPITULATIF ===\n")
print(recapitulatif)
cat(sprintf("\n%d/%d branches estimees avec succes. Resultats dans 'resultats/', figures dans 'figures/'.\n",
            sum(recapitulatif$statut=="OK"), nrow(recapitulatif)))
