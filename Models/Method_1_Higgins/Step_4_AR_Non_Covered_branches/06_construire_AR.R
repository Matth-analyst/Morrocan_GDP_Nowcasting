# ==============================================================================
# 06_construire_AR4.R
#
# Pour les 7 branches sans bridge equation exploitable (4 sans indicateur
# depuis la Phase 2 : Services aux entreprises, Administration publique,
# Education-sante, Autres services ; + 3 confirmees sans lien statistique
# a l'Etape 4 : Agriculture, Transports, Information-communication) --
# repli en AR(4), exactement la methode que Higgins (2014) utilise pour
# les sous-composantes sans indicateur disponible (ex. les dortoirs) :
#
#   Delta-log(VA_t) = alpha + gamma_1*Delta-log(VA_t-1) + ... 
#                            + gamma_4*Delta-log(VA_t-4) + eps_t
#
# ENTREE : csv/<branche>_trimestriel.csv (colonne 2 = VA, seule utilisee)
# SORTIE : resultats/<branche>_AR4_coefficients.csv,
#          resultats/recapitulatif_AR4.csv,
#          figures/<branche>_AR4_ajustement.png
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2); library(stringr); library(lubridate)
})

DOSSIER_CSV <- "csv"
DOSSIER_RESULTATS <- "resultats"
DOSSIER_FIGURES <- "figures"
DATE_DEBUT_TRAIN <- as.Date("2010-01-01")
DATE_FIN_TRAIN <- as.Date("2021-01-01")
P_MAX <- 8  # grille testee : p in {1,...,8}, selection par AIC, propre a chaque branche

dir.create(DOSSIER_RESULTATS, showWarnings = FALSE)
dir.create(DOSSIER_FIGURES, showWarnings = FALSE)

BRANCHES <- c(
  "Agriculture", "Transports", "Information_communication",
  "Services_aux_entreprises", "Administration_publique",
  "Education_sante", "Autres_services"
)

decaler <- function(x, k) { n <- length(x); if (k==0) return(x); c(rep(NA,k), x[1:(n-k)]) }

estimer_AR_AIC <- function(nom_branche) {
  chemin <- file.path(DOSSIER_CSV, paste0(nom_branche, "_trimestriel.csv"))
  if (!file.exists(chemin)) return(NULL)

  df <- read_csv(chemin, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  df$Date <- as.Date(df$Date)
  cible_col <- names(df)[2]
  df <- df %>% select(Date, valeur = all_of(cible_col)) %>% arrange(Date) %>% filter(valeur > 0)
  df$delta_log <- c(NA, diff(log(df$valeur)))
  df <- df %>% filter(!is.na(delta_log))

  for (k in 1:P_MAX) df[[paste0("lag", k)]] <- decaler(df$delta_log, k)

  df_train <- df %>% filter(Date >= DATE_DEBUT_TRAIN, Date <= DATE_FIN_TRAIN)

  # Echantillon COMMUN (le plus contraint, p=P_MAX) pour que les AIC de
  # tous les p testes soient calcules sur exactement le meme nombre
  # d'observations -- indispensable pour une comparaison d'AIC valide.
  cols_max <- paste0("lag", 1:P_MAX)
  df_train_commun <- df_train[complete.cases(df_train %>% select(delta_log, all_of(cols_max))), ]
  if (nrow(df_train_commun) < P_MAX + 5) {
    cat(sprintf("  [%s] Pas assez de points meme pour p=1 (%d disponibles) -- ignoree\n", nom_branche, nrow(df_train_commun)))
    return(NULL)
  }

  tableau_aic <- data.frame()
  for (p in 1:P_MAX) {
    formule <- as.formula(paste("delta_log ~", paste(paste0("lag",1:p), collapse=" + ")))
    modele_p <- lm(formule, data = df_train_commun)
    tableau_aic <- rbind(tableau_aic, data.frame(p = p, AIC = AIC(modele_p)))
  }
  p_etoile <- tableau_aic$p[which.min(tableau_aic$AIC)]

  # Reestimation finale avec p* SUR TOUTES LES LIGNES DISPONIBLES pour ce p
  # precis (pas seulement l'echantillon commun a P_MAX, pour ne pas perdre
  # de donnees utiles une fois p* connu)
  cols_final <- paste0("lag", 1:p_etoile)
  df_train_final <- df_train[complete.cases(df_train %>% select(delta_log, all_of(cols_final))), ]
  formule_finale <- as.formula(paste("delta_log ~", paste(cols_final, collapse=" + ")))
  modele_final <- lm(formule_finale, data = df_train_final)
  s <- summary(modele_final)

  derniers <- tail(df$delta_log, p_etoile)
  if (length(derniers) == p_etoile && !any(is.na(derniers))) {
    nouvelle_ligne <- as.data.frame(t(rev(derniers)))
    names(nouvelle_ligne) <- cols_final
    prevision <- predict(modele_final, newdata = nouvelle_ligne)
    date_prevision <- tail(df$Date, 1) %m+% months(3)
  } else { prevision <- NA; date_prevision <- NA }

  list(nom_branche = nom_branche, modele = modele_final, s = s, p_etoile = p_etoile,
        tableau_aic = tableau_aic, donnees_train = df_train_final, n_obs = nrow(df_train_final),
        prevision = prevision, date_prevision = date_prevision)
}

tracer_figure_AR <- function(resultat) {
  df <- resultat$donnees_train
  df$Ajuste <- predict(resultat$modele)
  df_long <- df %>% select(Date, delta_log, Ajuste) %>% rename(Observe = delta_log) %>%
    pivot_longer(cols = c(Observe, Ajuste), names_to = "type", values_to = "valeur")

  p <- ggplot(df_long, aes(x = Date, y = valeur, color = type)) +
    geom_line(linewidth = 0.6) +
    scale_color_manual(values = c("Observe" = "black", "Ajuste" = "#548235")) +
    labs(title = paste0("AR(", resultat$p_etoile, ") \u2014 ", gsub("_", " ", resultat$nom_branche)),
         subtitle = sprintf("p* = %d (choisi par AIC), R\u00b2 = %.3f, n = %d%s",
                             resultat$p_etoile, resultat$s$r.squared, resultat$n_obs,
                             if (!is.na(resultat$prevision)) sprintf(" | Prevision %s : %.4f", resultat$date_prevision, resultat$prevision) else ""),
         x = NULL, y = "Delta-log(VA)", color = NULL) +
    theme_minimal(base_size = 11)
  ggsave(file.path(DOSSIER_FIGURES, paste0(resultat$nom_branche, "_AR_ajustement.png")), p, width = 8, height = 4, dpi = 150)
}

recapitulatif <- data.frame()

for (branche in BRANCHES) {
  cat(sprintf("=== %s ===\n", branche))
  resultat <- tryCatch(estimer_AR_AIC(branche), error = function(e) { cat("  ERREUR:", e$message, "\n"); NULL })
  if (is.null(resultat)) {
    recapitulatif <- rbind(recapitulatif, data.frame(branche=branche, p_etoile=NA, R2=NA, n_obs=NA, prevision=NA, statut="ECHEC"))
    next
  }

  coefs_df <- as.data.frame(resultat$s$coefficients)
  coefs_df$variable <- rownames(coefs_df)
  write_csv(coefs_df, file.path(DOSSIER_RESULTATS, paste0(branche, "_AR_coefficients.csv")))
  write_csv(resultat$tableau_aic, file.path(DOSSIER_RESULTATS, paste0(branche, "_AR_grille_AIC.csv")))
  tracer_figure_AR(resultat)

  cat(sprintf("  p*=%d (AIC), R2=%.3f, n=%d%s\n", resultat$p_etoile, resultat$s$r.squared, resultat$n_obs,
              if (!is.na(resultat$prevision)) sprintf(" | prevision %s: %.4f", resultat$date_prevision, resultat$prevision) else ""))

  recapitulatif <- rbind(recapitulatif, data.frame(
    branche = branche, p_etoile = resultat$p_etoile, R2 = resultat$s$r.squared,
    n_obs = resultat$n_obs, prevision = resultat$prevision, statut = "OK"
  ))
}

write_csv(recapitulatif, file.path(DOSSIER_RESULTATS, "recapitulatif_AR.csv"))
cat("\n=== RECAPITULATIF ===\n")
print(recapitulatif)
cat(sprintf("\nTermine. Resultats dans '%s/', figures dans '%s/'.\n", DOSSIER_RESULTATS, DOSSIER_FIGURES))
