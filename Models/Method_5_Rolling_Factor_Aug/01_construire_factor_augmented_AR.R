# ==============================================================================
# 01_construire_factor_augmented_AR.R
#
# Methode 3 (comparaison a Higgins, Table 5, modele 2 "Rolling Factor
# Augmented AR(2)") -- Pour les 12 branches disposant d'un facteur commun
# (Etape 2b du Modele 1), estime :
#
#   Delta-log(VA_t) = alpha + sum_{k=1}^{q} gamma_k*Delta-log(VA_t-k)
#                            + sum_{j=0}^{r} beta_j*F_t-j + eps_t
#
# avec (q,r) choisis par AIC (grille {0,...,8} x {0,...,8}), INDEPENDAMMENT
# pour chaque branche -- Higgins fixe q=2 pour son modele de comparaison,
# nous ne reproduisons pas ce choix arbitraire (deja documente et ecarte
# pour la Methode 2).
#
# Les 4 branches SANS facteur (aucun indicateur retenu en Phase 2) ne sont
# PAS traitees ici -- reprises telles quelles depuis la Methode 2 (AR pur),
# copiees dans resultats/ et csv/ avant ce script.
#
# ENTREE : VA_reelle_par_branche.xlsx, facteurs/<branche>_facteur.csv
# SORTIE : resultats/<branche>_FactorAR_coefficients.csv,
#          resultats/<branche>_FactorAR_grille_AIC.csv,
#          resultats/recapitulatif_FactorAR.csv,
#          figures/<branche>_FactorAR_ajustement.png
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(readxl)
  library(lubridate); library(ggplot2); library(stringr)
})

DATE_DEBUT_TRAIN <- as.Date("2010-01-01")
DATE_FIN_TRAIN   <- as.Date("2021-01-01")
Q_MAX <- 8; R_MAX <- 8   # grille elargie (coherence avec Etape 3 du Modele 1, meme principe)

dir.create("resultats", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

parse_trimestre <- function(s) {
  m <- regmatches(s, regexec("T(\\d)-(\\d{4})", s))[[1]]
  if (length(m) == 3) as.Date(sprintf("%d-%02d-01", as.integer(m[3]), (as.integer(m[2])-1)*3+1)) else NA
}
va_brut <- read_excel("VA_reelle_par_branche.xlsx", skip = 2)
names(va_brut)[1] <- "Trimestre"
va_brut$Date <- sapply(va_brut$Trimestre, parse_trimestre) |> as.Date(origin="1970-01-01")
BRANCHES_16 <- names(va_brut)[!(names(va_brut) %in% c("Trimestre","Date"))]

# Les 12 branches disposant d'un facteur (fichiers presents dans facteurs/)
FICHIERS_FACTEUR <- list.files("facteurs", pattern = "_facteur\\.csv$")
NOMS_FICHIER_AVEC_FACTEUR <- str_remove(FICHIERS_FACTEUR, "_facteur\\.csv$")
cat(sprintf("%d branches avec facteur trouvees\n", length(NOMS_FICHIER_AVEC_FACTEUR)))

# Mapping nom-fichier (facteur) -> nom VA (BRANCHES_16), par correspondance
# de "squelette" alphanumerique (meme technique deja utilisee et validee
# dans le Modele 1 pour resoudre les problemes d'encodage recurrents de
# cet environnement R, locale "C")
squelette <- function(s) {
  s <- gsub("U\\+?[0-9A-Fa-f]{4}", "", s)
  # Remplacement par echappements Unicode explicites (\uXXXX) -- insensibles
  # a l'encodage du fichier source R lui-meme, contrairement aux caracteres
  # accentues litteraux qui se sont reveles peu fiables dans cet
  # environnement (locale systeme "C").
  s <- gsub("\u00e9|\u00e8|\u00ea", "e", s)
  s <- gsub("\u00c9|\u00c8|\u00ca", "E", s)
  s <- gsub("\u00e0", "a", s); s <- gsub("\u00c0", "A", s)
  s <- gsub("\u00f4", "o", s); s <- gsub("\u00d4", "O", s)
  s <- gsub("\u00ee", "i", s); s <- gsub("\u00ce", "I", s)
  s <- gsub("\u00e7", "c", s); s <- gsub("\u00c7", "C", s)
  toupper(gsub("[^A-Za-z0-9]", "", iconv(s, to="ASCII//TRANSLIT", sub="")))
}
CORRESPONDANCE_MANUELLE <- c(
  "Finances_assurances" = "Finances et assurances",
  "Industrie_extraction" = "Industrie d'extraction",
  "Industrie_transformation" = "Industrie de transformation"
)

mapping_nom_va <- sapply(NOMS_FICHIER_AVEC_FACTEUR, function(nf) {
  if (nf %in% names(CORRESPONDANCE_MANUELLE)) return(unname(CORRESPONDANCE_MANUELLE[nf]))
  sq_nf <- squelette(nf)
  idx <- which(squelette(BRANCHES_16) == sq_nf)
  if (length(idx) == 1) return(BRANCHES_16[idx])
  idx2 <- which(sapply(BRANCHES_16, function(b) grepl(sq_nf, squelette(b), fixed=TRUE) ||
                                                  grepl(squelette(b), sq_nf, fixed=TRUE)))
  if (length(idx2) == 1) return(BRANCHES_16[idx2])
  NA
})
cat("Correspondance fichier -> branche VA :\n")
print(data.frame(fichier = NOMS_FICHIER_AVEC_FACTEUR, nom_va = mapping_nom_va))

decaler <- function(x, k) { n <- length(x); if (k==0) return(x); c(rep(NA,k), x[1:(n-k)]) }

# ------------------------------------------------------------------------------
# Pour UNE branche : Delta-log(VA), facteur, grille (q,r), selection AIC
# ------------------------------------------------------------------------------
estimer_factor_ar <- function(nom_fichier, nom_va) {
  facteur_df <- read_csv(file.path("facteurs", paste0(nom_fichier, "_facteur.csv")), show_col_types = FALSE)
  facteur_df$Date <- as.Date(facteur_df$Date)

  df <- va_brut %>% select(Date, valeur = all_of(nom_va)) %>% arrange(Date) %>% filter(valeur > 0)
  df$delta_log <- c(NA, diff(log(df$valeur)))
  df <- df %>% filter(!is.na(delta_log)) %>% select(Date, delta_log)

  fusion <- inner_join(df, facteur_df, by = "Date") %>% arrange(Date)
  y <- fusion$delta_log; f <- fusion$facteur; n <- length(y)

  X_y <- sapply(1:Q_MAX, function(k) decaler(y, k)); colnames(X_y) <- paste0("y_lag", 1:Q_MAX)
  X_f <- sapply(0:R_MAX, function(j) decaler(f, j)); colnames(X_f) <- paste0("f_lag", 0:R_MAX)

  dates_all <- fusion$Date
  ligne_complete <- complete.cases(cbind(y, X_y, X_f)) & dates_all >= DATE_DEBUT_TRAIN & dates_all <= DATE_FIN_TRAIN
  y_sub <- y[ligne_complete]; X_y_sub <- X_y[ligne_complete,,drop=FALSE]; X_f_sub <- X_f[ligne_complete,,drop=FALSE]
  if (length(y_sub) < 10) return(NULL)

  meilleur_aic <- Inf; meilleur_q <- 0; meilleur_r <- 0; tableau_aic <- data.frame()
  for (q in 0:Q_MAX) for (r in 0:R_MAX) {
    X_modele <- if (q > 0) cbind(X_y_sub[, 1:q, drop=FALSE], X_f_sub[, 1:(r+1), drop=FALSE]) else X_f_sub[, 1:(r+1), drop=FALSE]
    df_modele <- data.frame(y = y_sub, X_modele)
    modele <- tryCatch(lm(y ~ ., data = df_modele), error = function(e) NULL)
    if (is.null(modele)) next
    a <- AIC(modele)
    tableau_aic <- rbind(tableau_aic, data.frame(q=q, r=r, AIC=a))
    if (a < meilleur_aic) { meilleur_aic <- a; meilleur_q <- q; meilleur_r <- r }
  }
  if (nrow(tableau_aic)==0) return(NULL)

  q <- meilleur_q; r <- meilleur_r
  X_y_final <- if (q>0) sapply(1:q, function(k) decaler(y,k)) else NULL
  X_f_final <- sapply(0:r, function(j) decaler(f,j)); if (r==0) X_f_final <- matrix(X_f_final, ncol=1)
  if (q>0 && is.null(dim(X_y_final))) X_y_final <- matrix(X_y_final, ncol=q)
  X_complet <- if (q>0) cbind(X_y_final, X_f_final) else X_f_final
  noms_col <- c(if(q>0) paste0("y_lag",1:q) else NULL, paste0("f_lag",0:r))
  colnames(X_complet) <- noms_col

  df_final <- data.frame(Date = dates_all, y = y, X_complet)
  df_final <- df_final %>% filter(Date >= DATE_DEBUT_TRAIN, Date <= DATE_FIN_TRAIN)
  df_final_complet <- df_final[complete.cases(df_final),]

  modele_final <- lm(y ~ ., data = df_final_complet %>% select(-Date))
  s <- summary(modele_final)

  list(nom_va = nom_va, q = q, r = r, aic = meilleur_aic, n_obs = nrow(df_final_complet),
        modele = modele_final, s = s, tableau_aic = tableau_aic,
        donnees_train = df_final_complet %>% mutate(Ajuste = predict(modele_final)))
}

tracer_figure <- function(resultat, nom_fichier) {
  df_long <- resultat$donnees_train %>% select(Date, y, Ajuste) %>% rename(Observe = y) %>%
    pivot_longer(cols=c(Observe,Ajuste), names_to="type", values_to="valeur")
  p <- ggplot(df_long, aes(x=Date, y=valeur, color=type)) + geom_line(linewidth=0.6) +
    scale_color_manual(values=c("Observe"="black","Ajuste"="#C55A11")) +
    labs(title=paste0("Factor-Augmented AR \u2014 ", gsub("_"," ",resultat$nom_va)),
         subtitle=sprintf("q=%d, r=%d (AIC=%.1f, n=%d)", resultat$q, resultat$r, resultat$aic, resultat$n_obs),
         x=NULL, y="Delta-log", color=NULL) + theme_minimal(base_size=10)
  ggsave(file.path("figures", paste0(nom_fichier, "_FactorAR_ajustement.png")), p, width=8, height=4, dpi=150)
}

recapitulatif <- data.frame()
for (i in seq_along(NOMS_FICHIER_AVEC_FACTEUR)) {
  nf <- NOMS_FICHIER_AVEC_FACTEUR[i]; nv <- unname(mapping_nom_va[i])
  cat(sprintf("=== %s ===\n", nv))
  if (is.na(nv)) { cat("  Correspondance introuvable -- ignoree\n"); next }
  resultat <- tryCatch(estimer_factor_ar(nf, nv), error = function(e) { cat("  ERREUR:", e$message, "\n"); NULL })
  if (is.null(resultat)) { cat("  echec estimation\n"); next }

  coefs_df <- as.data.frame(resultat$s$coefficients); coefs_df$variable <- rownames(coefs_df)
  write_csv(coefs_df, file.path("resultats", paste0(nf, "_FactorAR_coefficients.csv")))
  write_csv(resultat$tableau_aic, file.path("resultats", paste0(nf, "_FactorAR_grille_AIC.csv")))
  tracer_figure(resultat, nf)

  cat(sprintf("  q=%d r=%d AIC=%.1f R2=%.3f n=%d\n", resultat$q, resultat$r, resultat$aic, resultat$s$r.squared, resultat$n_obs))
  recapitulatif <- rbind(recapitulatif, data.frame(
    branche=nv, fichier=nf, q=resultat$q, r=resultat$r, AIC=resultat$aic,
    R2=resultat$s$r.squared, n_obs=resultat$n_obs))

  # Sauvegarde de la serie Delta-log complete (pour agregation/test, script suivant)
  df_complet <- va_brut %>% select(Date, valeur = all_of(nv)) %>% arrange(Date) %>% filter(valeur>0)
  df_complet$delta_log <- c(NA, diff(log(df_complet$valeur)))
  write_csv(df_complet %>% filter(!is.na(delta_log)) %>% select(Date, delta_log),
            file.path("csv", paste0(nf, "_deltalog_complet.csv")))
}

write_csv(recapitulatif, "resultats/recapitulatif_FactorAR.csv")
cat("\n=== RECAPITULATIF (12 branches avec facteur) ===\n"); print(recapitulatif)
cat(sprintf("\n%d/12 branches estimees. Resultats dans 'resultats/', figures dans 'figures/'.\n", nrow(recapitulatif)))
