# ============================================================================
# 04_verification_stabilite_VAR.R
# ----------------------------------------------------------------------------
# ETAPE 1 (partie 4, finale) -- BVAR trimestriel : verification de la
# STABILITE du modele estime.
#
# Principe : un VAR(p) s'ecrit sous forme compagne comme un VAR(1) sur un
# vecteur etendu de dimension (n*p) :
#
#   Y_t = c + A_1 Y_{t-1} + ... + A_p Y_{t-p} + e_t
#   <=> Z_t = C + F Z_{t-1} + E_t ,  Z_t = (Y_t, Y_{t-1}, ..., Y_{t-p+1})
#
# ou F est la MATRICE COMPAGNE (n*p x n*p). Le VAR est stable (stationnaire)
# si et seulement si TOUTES les valeurs propres de F ont un module
# STRICTEMENT INFERIEUR A 1 (elles sont a l'interieur du cercle unite).
#
# Pourquoi ce test est necessaire : un BVAR dont certaines racines
# sortiraient du cercle unite produirait des previsions qui DIVERGENT dans
# le temps (une croissance qui s'auto-amplifie indefiniment) -- un defaut
# qu'aucun des indicateurs de performance (RMSFE, Diebold-Mariano) ne
# detecte directement, puisqu'ils ne regardent qu'une prevision a un seul
# pas. Ce test n'avait jamais ete fait explicitement dans les versions
# precedentes de ce travail -- une lacune corrigee ici.
#
# Entree  : resultats/bvar_modele_final.rds (produit par le script 03)
# Sorties : resultats/valeurs_propres_VAR.csv
#           figures/04_cercle_unite_valeurs_propres.png
#           Un verdict clair imprime dans la console
# ============================================================================

suppressMessages({
  library(dplyr)
  library(ggplot2)
})

DOSSIER_RESULTATS <- "resultats"
DOSSIER_FIGURES   <- "figures"
dir.create(DOSSIER_RESULTATS, showWarnings = FALSE, recursive = TRUE)
dir.create(DOSSIER_FIGURES,   showWarnings = FALSE, recursive = TRUE)

modele <- readRDS(file.path(DOSSIER_RESULTATS, "bvar_modele_final.rds"))
B <- modele$B          # (1 + n*p) x n
p <- modele$p
noms <- modele$noms
n <- length(noms)

cat(sprintf("Modele charge : n=%d branches, p=%d retards, lambda=%.2f\n\n", n, p, modele$lambda))

# --- Reconstruction des matrices A_1, ..., A_p a partir de B ---------------
# B[(2+(l-1)*n):(1+l*n), ] contient, pour le retard l : n lignes (indexees
# par la variable predictrice k) x n colonnes (indexees par l'equation j).
# La convention standard d'un VAR (A_l[j,k] = effet de y_{k,t-l} sur y_{j,t})
# demande donc de TRANSPOSER ce sous-bloc.
extraire_A_l <- function(B, l, n) {
  bloc <- B[(2 + (l - 1) * n):(1 + l * n), , drop = FALSE]   # n x n
  t(bloc)                                                     # A_l, n x n
}

liste_A <- lapply(1:p, extraire_A_l, B = B, n = n)

# --- Construction de la matrice compagne F (n*p x n*p) ---------------------
# Ligne de blocs du haut : [A_1 | A_2 | ... | A_p]
# En dessous : blocs identite decales (memoire des retards precedents)
F_compagne <- matrix(0, nrow = n * p, ncol = n * p)
F_compagne[1:n, ] <- do.call(cbind, liste_A)
if (p > 1) {
  F_compagne[(n + 1):(n * p), 1:(n * (p - 1))] <- diag(n * (p - 1))
}

# --- Valeurs propres et modules ---------------------------------------------
valeurs_propres <- eigen(F_compagne, only.values = TRUE)$values
modules <- Mod(valeurs_propres)

resultats_vp <- tibble(
  valeur_propre = valeurs_propres,
  partie_reelle = round(Re(valeurs_propres), 4),
  partie_imaginaire = round(Im(valeurs_propres), 4),
  module = round(modules, 4)
) %>% arrange(desc(module))

# --- Sortie 1 : tableau et verdict -------------------------------------------
cat(strrep("=", 70), "\n")
cat("VERIFICATION DE LA STABILITE DU BVAR -- valeurs propres de la matrice compagne\n")
cat(strrep("=", 70), "\n\n")
cat("Les 10 valeurs propres de plus grand module :\n\n")
print(as.data.frame(head(resultats_vp %>% select(partie_reelle, partie_imaginaire, module), 10)),
      row.names = FALSE)

module_max <- max(modules)
cat(sprintf("\nModule maximal observe : %.4f\n", module_max))
cat(strrep("-", 70), "\n")

if (module_max < 1) {
  cat("=> VERDICT : le VAR est STABLE.\n",
      "   Toutes les valeurs propres sont a l'interieur du cercle unite\n",
      "   (module < 1) -- les previsions ne divergent pas dans le temps,\n",
      "   le modele est bien pose pour la prevision.\n", sep = "")
} else {
  n_instables <- sum(modules >= 1)
  cat(sprintf(
    "=> ALERTE : le VAR est INSTABLE -- %d valeur(s) propre(s) ont un module >= 1.\n",
    n_instables),
    "   Les previsions a plusieurs pas peuvent diverger. Causes possibles :\n",
    "   - lambda trop eleve (prior trop faible, coefficients insuffisamment contraints)\n",
    "   - p trop eleve par rapport a la longueur de l'echantillon\n",
    "   - une ou plusieurs branches presentant un comportement quasi non stationnaire\n",
    "     meme apres transformation en Δlog (a re-verifier avec le script 01)\n", sep = "")
}
cat(strrep("-", 70), "\n")

# --- Sortie 2 : CSV complet ---------------------------------------------------
chemin_csv <- file.path(DOSSIER_RESULTATS, "valeurs_propres_VAR.csv")
write.csv(resultats_vp, chemin_csv, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("\nValeurs propres completes enregistrees : %s\n", chemin_csv))

# --- Sortie 3 : figure -- valeurs propres dans le plan complexe -------------
cercle_unite <- tibble(theta = seq(0, 2 * pi, length.out = 200)) %>%
  mutate(x = cos(theta), y = sin(theta))

p_fig <- ggplot() +
  geom_path(data = cercle_unite, aes(x, y), color = "grey50", linetype = "dashed") +
  geom_point(data = resultats_vp, aes(partie_reelle, partie_imaginaire, color = module >= 1),
             size = 3) +
  scale_color_manual(values = c(`FALSE` = "#2E74B5", `TRUE` = "#C0392B"),
                       labels = c(`FALSE` = "Module < 1 (stable)", `TRUE` = "Module >= 1 (instable)"),
                       name = NULL) +
  coord_fixed() +
  labs(title = "Valeurs propres de la matrice compagne du BVAR",
       subtitle = sprintf("Module maximal observe : %.4f", module_max),
       x = "Partie réelle", y = "Partie imaginaire") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

chemin_fig <- file.path(DOSSIER_FIGURES, "04_cercle_unite_valeurs_propres.png")
ggsave(chemin_fig, p_fig, width = 7, height = 7, dpi = 150)
cat(sprintf("Figure enregistree : %s\n", chemin_fig))

cat("\nEtape 1 (BVAR trimestriel) entierement terminee.\n")
