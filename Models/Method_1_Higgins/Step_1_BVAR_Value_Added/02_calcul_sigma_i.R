# ============================================================================
# 02_calcul_sigma_i.R
# ----------------------------------------------------------------------------
# ETAPE 1 (partie 2) -- BVAR trimestriel : calcul de sigma_i, l'echelle
# propre de chaque branche, necessaire a la construction des observations
# fictives du prior Minnesota (Banbura, Giannone & Reichlin, 2010).
#
# Principe : sigma_i n'est PAS l'ecart-type brut de la serie -- c'est
# l'ecart-type des RESIDUS d'un AR(p) univarie ajuste sur le Δlog de la
# branche i, prise isolement (pas dans le systeme a 16 variables). Cette
# quantite isole la part "imprevisible" propre a chaque branche, une fois
# retire ce que son propre passe permettait deja de predire -- c'est elle
# qui sert a calibrer la force du prior, pas la variance totale de la
# serie (qui inclurait aussi la part autocorrelee, donc previsible).
#
#   sigma_i = sqrt( Var(residus de l'AR(p) ajuste sur Δlog_i) )
#
# Le nombre de retards p utilise ici doit etre LE MEME que celui retenu
# pour le BVAR final (p=5, par coherence avec Higgins 2014) -- Banbura,
# Giannone & Reichlin (2010) calibrent sigma_i avec le meme ordre que le
# VAR lui-meme.
#
# Entree  : VA_reelle_par_branche.xlsx (feuille "VA par branche")
# Sorties : resultats/sigma_i.csv
#           resultats/sigma_i.rds        (reutilise par les etapes suivantes)
#           figures/02_sigma_i_par_branche.png
# ============================================================================

# --- Packages ---------------------------------------------------------------
suppressMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(forcats)
})

# --- Chemins et parametres ---------------------------------------------------
FICHIER_ENTREE    <- "VA_reelle_par_branche.xlsx"
DOSSIER_RESULTATS <- "resultats"
DOSSIER_FIGURES   <- "figures"
dir.create(DOSSIER_RESULTATS, showWarnings = FALSE, recursive = TRUE)
dir.create(DOSSIER_FIGURES,   showWarnings = FALSE, recursive = TRUE)

P_RETARDS <- 5   # doit rester identique au nombre de retards du BVAR final

# --- Lecture et transformation (identique au script 01) --------------------
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

donnees_dlog <- donnees %>%
  mutate(across(all_of(BRANCHES), ~ log(pmax(.x, 1e-6)), .names = "log_{.col}")) %>%
  arrange(date) %>%
  mutate(across(starts_with("log_"), ~ .x - lag(.x), .names = "d{.col}"))

cat(sprintf("Calcul de sigma_i sur %d branches, AR(%d) univarie par branche.\n\n",
            length(BRANCHES), P_RETARDS))

# --- Fonction : ajuste un AR(p) univarie et renvoie l'ecart-type residuel --
# ar.ols() avec aic=FALSE force l'ordre exact p (pas de selection automatique) ;
# demean=TRUE retire la moyenne avant ajustement (la constante est traitee
# separement dans le BVAR lui-meme, bloc 3 des observations fictives).
calculer_sigma_i <- function(serie_dlog, p) {
  serie_dlog <- serie_dlog[!is.na(serie_dlog)]
  if (length(serie_dlog) < p + 5) {
    warning("Serie trop courte pour un AR(", p, ") fiable -- repli sur l'ecart-type brut.")
    return(list(sigma = sd(serie_dlog), methode = "repli (ecart-type brut)"))
  }
  fit <- tryCatch(
    ar.ols(serie_dlog, order.max = p, aic = FALSE, demean = TRUE, intercept = TRUE),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(list(sigma = sd(serie_dlog), methode = "repli (ecart-type brut, echec ar.ols)"))
  }
  # fit$var.pred est la matrice (1x1 ici) de variance des residus
  list(sigma = sqrt(as.numeric(fit$var.pred)), methode = sprintf("AR(%d), residus", p))
}

# --- Application a chaque branche -------------------------------------------
resultats_sigma <- lapply(BRANCHES, function(branche) {
  serie <- donnees_dlog[[paste0("dlog_", branche)]]
  res <- calculer_sigma_i(serie, P_RETARDS)
  tibble(
    branche       = branche,
    sigma_i       = round(res$sigma, 5),
    ecart_type_brut = round(sd(serie, na.rm = TRUE), 5),
    methode       = res$methode
  )
}) %>% bind_rows() %>%
  arrange(desc(sigma_i))

# --- Sortie 1 : tableau dans la console --------------------------------------
cat(strrep("=", 90), "\n")
cat(sprintf("SIGMA_I PAR BRANCHE -- echelle de la partie imprevisible du Δlog (AR(%d))\n", P_RETARDS))
cat(strrep("=", 90), "\n\n")
print(as.data.frame(resultats_sigma), row.names = FALSE)

cat("\n", strrep("-", 90), "\n", sep = "")
cat(sprintf("Branche la plus volatile (sigma_i le plus eleve)  : %s (%.5f)\n",
            resultats_sigma$branche[1], resultats_sigma$sigma_i[1]))
cat(sprintf("Branche la plus stable   (sigma_i le plus faible) : %s (%.5f)\n",
            resultats_sigma$branche[nrow(resultats_sigma)], resultats_sigma$sigma_i[nrow(resultats_sigma)]))
cat(strrep("-", 90), "\n")
cat("\nRappel : plus sigma_i est eleve, plus la contrainte du prior sera forte\n",
    "pour cette branche (elle pese davantage dans les observations fictives) --\n",
    "coherent avec la prudence attendue sur une branche difficile a predire.\n", sep = "")

# --- Sortie 2 : CSV + RDS (reutilise par les etapes suivantes) --------------
chemin_csv <- file.path(DOSSIER_RESULTATS, "sigma_i.csv")
write.csv(resultats_sigma, chemin_csv, row.names = FALSE, fileEncoding = "UTF-8")

# Vecteur nomme sigma_i, dans l'ordre des BRANCHES (pas trie) -- c'est cette
# forme qui sera directement utilisee dans la construction des observations
# fictives (etape suivante).
sigma_i_vecteur <- setNames(
  resultats_sigma$sigma_i[match(BRANCHES, resultats_sigma$branche)],
  BRANCHES
)
saveRDS(sigma_i_vecteur, file.path(DOSSIER_RESULTATS, "sigma_i.rds"))

cat(sprintf("\nResultats enregistres : %s\n", chemin_csv))
cat(sprintf("Vecteur sigma_i (pour l'etape suivante) : %s\n",
            file.path(DOSSIER_RESULTATS, "sigma_i.rds")))

# --- Sortie 3 : figure -- sigma_i trie, par branche -------------------------
p_fig <- ggplot(resultats_sigma, aes(x = fct_reorder(branche, sigma_i), y = sigma_i)) +
  geom_col(fill = "#2E74B5") +
  coord_flip() +
  labs(title = "Écart-type résiduel (sigma_i) par branche",
       subtitle = sprintf("Résidus d'un AR(%d) univarié ajusté sur le Δlog de chaque branche", P_RETARDS),
       x = NULL, y = expression(sigma[i])) +
  theme_minimal(base_size = 11)

chemin_fig <- file.path(DOSSIER_FIGURES, "02_sigma_i_par_branche.png")
ggsave(chemin_fig, p_fig, width = 8, height = 7, dpi = 150)
cat(sprintf("Figure enregistree : %s\n", chemin_fig))

cat("\nEtape terminee.\n")
