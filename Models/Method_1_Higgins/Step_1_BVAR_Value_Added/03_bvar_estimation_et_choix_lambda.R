# ============================================================================
# 03_bvar_estimation_et_choix_lambda.R
# ----------------------------------------------------------------------------
# ETAPE 1 (partie 3) -- BVAR trimestriel : fonction generique de construction
# des observations fictives + estimation OLS, puis recherche de lambda sur
# une grille, evaluee HORS ECHANTILLON (pas en echantillon) pour eviter le
# risque de sur-ajustement identifie tout au long de ce travail.
#
# Design : UNE SEULE fonction (estimer_bvar) construit les 3 blocs
# d'observations fictives et estime le BVAR par OLS sur donnees augmentees,
# pour n'importe quel (p, lambda) donne. La recherche de lambda consiste
# simplement a appeler cette meme fonction en boucle sur une grille de
# valeurs -- jamais reecrite deux fois, jamais de risque de divergence
# entre la version "construction" et la version "recherche".
#
# Grille de lambda : {0.05, 0.10, 0.15, 0.20, 0.30} -- reprise de la
# methode de calibration que Higgins (2014) utilise lui-meme pour son BVAR
# mixte-frequence (annexe, page 34) : "lambda=0.10 was chosen because it
# gave the best forecasting performance among 0.05, 0.10, 0.15, 0.20 and
# 0.25" -- on ajoute 0.30 comme point supplementaire au-dela de son maximum.
#
# Entrees : VA_reelle_par_branche.xlsx
#           resultats/sigma_i.rds     (produit par 02_calcul_sigma_i.R)
# Sorties : resultats/bvar_modele_final.rds
#           resultats/recherche_lambda.csv
#           figures/03_rmsfe_par_lambda.png
# ============================================================================

# --- Packages ----------------------------------------------------------------
suppressMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
})

# --- Chemins et parametres -----------------------------------------------
FICHIER_ENTREE    <- "VA_reelle_par_branche.xlsx"
FICHIER_SIGMA     <- "resultats/sigma_i.rds"
DOSSIER_RESULTATS <- "resultats"
DOSSIER_FIGURES   <- "figures"
dir.create(DOSSIER_RESULTATS, showWarnings = FALSE, recursive = TRUE)
dir.create(DOSSIER_FIGURES,   showWarnings = FALSE, recursive = TRUE)

P_RETARDS     <- 5                              # coherent avec sigma_i.rds (etape 2)
GRILLE_LAMBDA <- c(0.01, 0.02, 0.03, 0.05, 0.10, 0.15, 0.20, 0.30) # grille elargie vers le bas
# --- Lecture et transformation (identique aux scripts precedents) ----------
convertir_trimestre_en_date <- function(x) {
  trimestre <- as.integer(str_sub(x, 2, 2))
  annee     <- as.integer(str_sub(x, 4, 7))
  mois      <- (trimestre - 1) * 3 + 1
  as.Date(sprintf("%d-%02d-01", annee, mois))
}

donnees <- read_excel(FICHIER_ENTREE, sheet = "VA par branche", skip = 2) %>%
  rename(trimestre_lbl = Trimestre) %>%
  mutate(date = convertir_trimestre_en_date(trimestre_lbl)) %>%
  arrange(date)

BRANCHES <- setdiff(names(donnees), c("trimestre_lbl", "date"))
N <- length(BRANCHES)

donnees_dlog <- donnees %>%
  mutate(across(all_of(BRANCHES), ~ log(pmax(.x, 1e-6)))) %>%
  arrange(date) %>%
  mutate(across(all_of(BRANCHES), ~ .x - lag(.x))) %>%
  filter(!if_any(all_of(BRANCHES), is.na))

Y_complet    <- as.matrix(donnees_dlog[, BRANCHES])
dates_vec    <- donnees_dlog$date
sigma_i      <- readRDS(FICHIER_SIGMA)[BRANCHES]   # meme ordre que les colonnes de Y

cat(sprintf("Matrice Y : %d trimestres x %d branches (de %s a %s)\n\n",
            nrow(Y_complet), N, format(min(dates_vec), "%Y-%m"), format(max(dates_vec), "%Y-%m")))

# ============================================================================
# FONCTION GENERIQUE -- construction des 3 blocs + estimation OLS augmentee
# ============================================================================
#' Estime un BVAR a prior Minnesota (Banbura, Giannone & Reichlin, 2010)
#' par observations fictives, pour un (p, lambda) donne.
#'
#' @param Y       matrice (T x n) des Δlog, deja stationnaires
#' @param p       nombre de retards
#' @param lambda  tightness du prior (plus petit = plus contraint)
#' @param sigma_i vecteur nomme (n), echelle propre de chaque variable
#' @return liste : B (coefficients), residus, noms des variables
estimer_bvar <- function(Y, p, lambda, sigma_i) {
  n  <- ncol(Y)
  Tn <- nrow(Y)

  # --- Observations REELLES : Y_reg = c + A1*y(t-1) + ... + Ap*y(t-p) ------
  Y_reg <- Y[(p + 1):Tn, , drop = FALSE]
  X_reg <- matrix(1, nrow = Tn - p, ncol = 1 + n * p)   # colonne 1 = constante
  for (l in 1:p) {
    X_reg[, (2 + (l - 1) * n):(1 + l * n)] <- Y[(p + 1 - l):(Tn - l), , drop = FALSE]
  }

  # --- BLOC 1 : prior sur les coefficients (delta_i = 0 pour tous) --------
  # Une observation fictive par (variable, retard) -- n*p lignes au total.
  Yd1 <- matrix(0, nrow = n * p, ncol = n)
  Xd1 <- matrix(0, nrow = n * p, ncol = 1 + n * p)
  for (l in 1:p) {
    rows <- ((l - 1) * n + 1):(l * n)
    Xd1[rows, (2 + (l - 1) * n):(1 + l * n)] <- diag(sigma_i * l / lambda)
  }

  # --- BLOC 2 : prior sur la matrice de covariance des residus ------------
  Yd2 <- matrix(0, nrow = n, ncol = n)
  diag(Yd2) <- sigma_i
  Xd2 <- matrix(0, nrow = n, ncol = 1 + n * p)

  # --- BLOC 3 : prior lache sur la constante (quasi neutre) ---------------
  EPSILON <- 1e-5
  Yd3 <- matrix(0, nrow = 1, ncol = n)
  Xd3 <- matrix(0, nrow = 1, ncol = 1 + n * p)
  Xd3[1, 1] <- EPSILON

  # --- Empilement donnees reelles + les 3 blocs fictifs, puis OLS ---------
  Y_aug <- rbind(Y_reg, Yd1, Yd2, Yd3)
  X_aug <- rbind(X_reg, Xd1, Xd2, Xd3)

  B <- solve(t(X_aug) %*% X_aug) %*% t(X_aug) %*% Y_aug   # (1+np) x n
  residus <- Y_reg - X_reg %*% B

  list(B = B, p = p, lambda = lambda, noms = colnames(Y), residus = residus)
}

#' Prevision a un pas a partir d'un modele deja estime et des p dernieres
#' observations disponibles (peuvent venir d'une fenetre plus longue que
#' celle d'estimation).
prevoir_bvar <- function(modele, Y_recent) {
  p  <- modele$p
  Tn <- nrow(Y_recent)
  x_new <- matrix(c(1, as.vector(t(Y_recent[Tn:(Tn - p + 1), , drop = FALSE]))), nrow = 1)
  setNames(as.vector(x_new %*% modele$B), modele$noms)
}

# ============================================================================
# RECHERCHE DE LAMBDA -- evaluation HORS ECHANTILLON, jamais en echantillon
# ============================================================================
# Fenetre de validation : les 20% de trimestres les plus recents, jamais
# utilises pour choisir lambda -- seulement pour MESURER l'erreur de
# prevision a chaque origine glissante. Principe identique a celui de la
# refonte methodologique rigoureuse (train pour estimer, validation pour
# arbitrer, jamais le meme sous-echantillon pour les deux).
Tn_total       <- nrow(Y_complet)
idx_debut_val  <- floor(Tn_total * 0.80)
n_trimestres_val <- Tn_total - idx_debut_val

cat(sprintf("Fenetre de validation : %d derniers trimestres (%s a %s)\n\n",
            n_trimestres_val, format(dates_vec[idx_debut_val + 1], "%Y-%m"),
            format(dates_vec[Tn_total], "%Y-%m")))

#' RMSFE d'un lambda donne, evalue en fenetre glissante sur la validation.
#' A chaque origine, le BVAR est RE-ESTIME avec seulement les donnees
#' disponibles jusqu'a cette origine -- jamais de fuite d'information.
evaluer_lambda <- function(lambda) {
  erreurs <- vector("numeric", n_trimestres_val)
  for (h in 1:n_trimestres_val) {
    idx <- idx_debut_val + h - 1
    Y_train <- Y_complet[1:idx, , drop = FALSE]
    modele  <- estimer_bvar(Y_train, P_RETARDS, lambda, sigma_i)
    prevision <- prevoir_bvar(modele, Y_train)
    reel      <- Y_complet[idx + 1, ]
    # erreur agregee : moyenne simple des erreurs au carre sur les 16 branches
    erreurs[h] <- mean((reel - prevision)^2)
  }
  sqrt(mean(erreurs))
}

cat("Recherche de lambda en cours (reestimation a chaque origine)...\n")
resultats_lambda <- tibble(
  lambda = GRILLE_LAMBDA,
  rmsfe  = sapply(GRILLE_LAMBDA, evaluer_lambda)
)

cat(strrep("=", 60), "\n")
cat("RESULTAT DE LA RECHERCHE DE LAMBDA (hors echantillon)\n")
cat(strrep("=", 60), "\n\n")
print(as.data.frame(resultats_lambda %>% mutate(rmsfe = round(rmsfe, 6))), row.names = FALSE)

LAMBDA_RETENU <- resultats_lambda$lambda[which.min(resultats_lambda$rmsfe)]
cat(sprintf("\n=> Lambda retenu : %.2f (RMSFE = %.6f)\n\n", LAMBDA_RETENU, min(resultats_lambda$rmsfe)))

# --- Sortie 1 : CSV de la recherche ------------------------------------------
write.csv(resultats_lambda, file.path(DOSSIER_RESULTATS, "recherche_lambda.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# --- Sortie 2 : figure RMSFE vs lambda ---------------------------------------
p_fig <- ggplot(resultats_lambda, aes(lambda, rmsfe)) +
  geom_line(color = "#2E74B5", linewidth = 0.8) +
  geom_point(size = 3, color = "#2E74B5") +
  geom_point(data = resultats_lambda %>% filter(lambda == LAMBDA_RETENU),
             size = 5, shape = 21, fill = "#C55A11", color = "black") +
  labs(title = "RMSFE hors échantillon selon λ",
       subtitle = sprintf("λ retenu = %.2f (point orange)", LAMBDA_RETENU),
       x = expression(lambda), y = "RMSFE") +
  theme_minimal(base_size = 11)
ggsave(file.path(DOSSIER_FIGURES, "03_rmsfe_par_lambda.png"), p_fig, width = 7, height = 5, dpi = 150)

# --- Estimation finale, sur TOUT l'echantillon, avec le lambda retenu ------
modele_final <- estimer_bvar(Y_complet, P_RETARDS, LAMBDA_RETENU, sigma_i)
saveRDS(modele_final, file.path(DOSSIER_RESULTATS, "bvar_modele_final.rds"))

cat(sprintf("Modele final estime (p=%d, lambda=%.2f) et enregistre : %s\n",
            P_RETARDS, LAMBDA_RETENU, file.path(DOSSIER_RESULTATS, "bvar_modele_final.rds")))
cat("\nEtape terminee.\n")
