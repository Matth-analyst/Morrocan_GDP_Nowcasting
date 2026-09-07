# ============================================================================
# 01_transformation_deltalog_ADF.R
# ----------------------------------------------------------------------------
# ETAPE 1 (partie 1) -- BVAR trimestriel : justification du choix de la
# transformation en Δlog (taux de croissance) plutot que le log-niveau.
#
# Principe : Higgins (2014) utilise le log-niveau (delta_i=1, prior de
# marche aleatoire) pour ses 13 composantes du PIB (Tableau A1, toutes
# transformees en "LogLevel"). Chez nous, la transformation retenue doit
# etre justifiee empiriquement, pas recopiee sans verification -- d'ou le
# test de Dickey-Fuller augmente (ADF), applique a la fois :
#   (a) sur le LOG-NIVEAU de la VA de chaque branche
#   (b) sur le Δlog (taux de croissance trimestriel) de la VA
#
# Si (a) n'est pas stationnaire mais (b) l'est, la transformation en Δlog
# est justifiee : le prior de retour a la moyenne (delta_i=0) est le bon
# choix pour notre BVAR, pas le prior de marche aleatoire de Higgins.
#
# Entree  : VA_reelle_par_branche.xlsx (feuille "VA par branche" ;
#           colonne A = trimestre, colonnes B:Q = les 16 branches)
# Sorties : resultats/tests_stationnarite_ADF.csv
#           figures/01a_toutes_branches_log_niveau.png
#           figures/01b_toutes_branches_deltalog.png
#           Un tableau recapitulatif imprime dans la console
# ============================================================================

# --- Packages -----------------------------------------------------------
suppressMessages({
  library(readxl)      # lecture du classeur Excel
  library(dplyr)       # manipulation de donnees
  library(tidyr)       # passage large -> long
  library(tseries)      # adf.test()
  library(ggplot2)      # figures
  library(stringr)      # nettoyage des noms de colonnes
})

# --- Chemins --------------------------------------------------------------
FICHIER_ENTREE   <- "VA_reelle_par_branche.xlsx"
DOSSIER_RESULTATS <- "resultats"
DOSSIER_FIGURES   <- "figures"
dir.create(DOSSIER_RESULTATS, showWarnings = FALSE, recursive = TRUE)
dir.create(DOSSIER_FIGURES,   showWarnings = FALSE, recursive = TRUE)

# --- Lecture des donnees ----------------------------------------------------
donnees_brutes <- read_excel(FICHIER_ENTREE, sheet = "VA par branche", skip = 2)

# Conversion du trimestre ("T1-1998") en une date (premier mois du trimestre),
# pour un tri chronologique fiable et un axe de temps correct sur les figures.
convertir_trimestre_en_date <- function(x) {
  trimestre <- as.integer(str_sub(x, 2, 2))
  annee     <- as.integer(str_sub(x, 4, 7))
  mois      <- (trimestre - 1) * 3 + 1
  as.Date(sprintf("%d-%02d-01", annee, mois))
}

donnees <- donnees_brutes %>%
  rename(trimestre_lbl = Trimestre) %>%
  mutate(date = convertir_trimestre_en_date(trimestre_lbl)) %>%
  arrange(date)

BRANCHES <- setdiff(names(donnees), c("trimestre_lbl", "date"))
cat(sprintf("Donnees importees : %d trimestres (%s a %s), %d branches.\n\n",
            nrow(donnees), format(min(donnees$date), "%Y-%m"),
            format(max(donnees$date), "%Y-%m"), length(BRANCHES)))

# --- Construction du log-niveau et du Δlog, pour chaque branche -----------
donnees_log <- donnees %>%
  mutate(across(all_of(BRANCHES), ~ log(pmax(.x, 1e-6)), .names = "log_{.col}"))

donnees_dlog <- donnees_log %>%
  arrange(date) %>%
  mutate(across(starts_with("log_"), ~ .x - lag(.x), .names = "d{.col}"))

# --- Fonction utilitaire : test ADF avec gestion des erreurs ---------------
test_adf_securise <- function(serie) {
  serie <- serie[!is.na(serie)]
  if (length(serie) < 10) return(list(statistique = NA_real_, p_value = NA_real_))
  res <- tryCatch(suppressWarnings(adf.test(serie, alternative = "stationary")),
                   error = function(e) NULL)
  if (is.null(res)) return(list(statistique = NA_real_, p_value = NA_real_))
  list(statistique = as.numeric(res$statistic), p_value = res$p.value)
}

# --- Application du test ADF a chaque branche, niveau ET Δlog -------------
SEUIL_SIGNIFICATIVITE <- 0.05

resultats <- lapply(BRANCHES, function(branche) {
  serie_log  <- donnees_log[[paste0("log_", branche)]]
  serie_dlog <- donnees_dlog[[paste0("dlog_", branche)]]

  test_niveau <- test_adf_securise(serie_log)
  test_dlog   <- test_adf_securise(serie_dlog)

  tibble(
    branche                 = branche,
    adf_stat_log_niveau     = round(test_niveau$statistique, 3),
    p_value_log_niveau      = round(test_niveau$p_value, 4),
    stationnaire_niveau     = ifelse(test_niveau$p_value < SEUIL_SIGNIFICATIVITE, "Oui", "Non"),
    adf_stat_deltalog       = round(test_dlog$statistique, 3),
    p_value_deltalog        = round(test_dlog$p_value, 4),
    stationnaire_deltalog   = ifelse(test_dlog$p_value < SEUIL_SIGNIFICATIVITE, "Oui", "Non")
  )
}) %>% bind_rows()

# --- Sortie 1 : tableau recapitulatif dans la console ----------------------
cat(strrep("=", 100), "\n")
cat("TEST DE DICKEY-FULLER AUGMENTE (ADF) -- log-niveau vs Δlog, seuil = 5%\n")
cat(strrep("=", 100), "\n\n")
print(as.data.frame(resultats), row.names = FALSE)

n_stat_niveau <- sum(resultats$stationnaire_niveau == "Oui")
n_stat_dlog   <- sum(resultats$stationnaire_deltalog == "Oui")

cat("\n", strrep("-", 100), "\n", sep = "")
cat(sprintf("Stationnaires EN NIVEAU (log)  : %d / %d branches\n", n_stat_niveau, length(BRANCHES)))
cat(sprintf("Stationnaires EN Δlog          : %d / %d branches\n", n_stat_dlog, length(BRANCHES)))
cat(strrep("-", 100), "\n\n")

if (n_stat_dlog > n_stat_niveau) {
  cat("=> CONCLUSION : la transformation en Δlog (taux de croissance) est",
      "empiriquement justifiee.\n",
      "   Le prior de retour a la moyenne (delta_i = 0) est adapte a nos",
      "16 branches,\n",
      "   contrairement au prior de marche aleatoire (delta_i = 1, utilise",
      "par Higgins\n",
      "   pour ses composantes en log-niveau).\n")
} else {
  cat("=> ATTENTION : le resultat ne confirme pas la superiorite du Δlog.",
      "A examiner branche par branche avant de choisir la transformation.\n")
}

# --- Sortie 2 : fichier CSV des resultats ----------------------------------
chemin_csv <- file.path(DOSSIER_RESULTATS, "tests_stationnarite_ADF.csv")
write.csv(resultats, chemin_csv, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("\nResultats detailles enregistres : %s\n", chemin_csv))

# --- Sortie 3 : DEUX figures, chacune avec les 16 branches -----------------
# Plutot qu'une seule figure a 32 panneaux (16 branches x 2 series, illisible),
# on separe : une figure pour le log-niveau (toutes branches), une pour le
# Δlog (toutes branches) -- chacune en facet_wrap, echelle libre par panneau
# puisque les 16 branches n'ont pas du tout la meme amplitude.

df_log_long <- donnees_log %>%
  select(date, starts_with("log_")) %>%
  pivot_longer(-date, names_to = "branche", values_to = "valeur") %>%
  mutate(branche = str_remove(branche, "^log_"))

df_dlog_long <- donnees_dlog %>%
  select(date, starts_with("dlog_")) %>%
  pivot_longer(-date, names_to = "branche", values_to = "valeur") %>%
  mutate(branche = str_remove(branche, "^dlog_"))

p_niveau <- ggplot(df_log_long, aes(date, valeur)) +
  geom_line(color = "#2E74B5", linewidth = 0.5, na.rm = TRUE) +
  facet_wrap(~branche, scales = "free_y", ncol = 4) +
  labs(title = "Log-niveau de la VA -- les 16 branches",
       subtitle = "Tendances non stationnaires (confirme par le test ADF)",
       x = NULL, y = "log(VA)") +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 7.5),
        axis.text = element_text(size = 6.5))

chemin_fig_niveau <- file.path(DOSSIER_FIGURES, "01a_toutes_branches_log_niveau.png")
ggsave(chemin_fig_niveau, p_niveau, width = 12, height = 10, dpi = 150)
cat(sprintf("Figure (log-niveau, 16 branches) enregistree : %s\n", chemin_fig_niveau))

p_dlog <- ggplot(df_dlog_long, aes(date, valeur)) +
  geom_line(color = "#C55A11", linewidth = 0.5, na.rm = TRUE) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.3, linetype = "dashed") +
  facet_wrap(~branche, scales = "free_y", ncol = 4) +
  labs(title = "Δlog de la VA (taux de croissance trimestriel) -- les 16 branches",
       subtitle = "Oscillation autour de zero, sans tendance (confirme par le test ADF)",
       x = NULL, y = "Δlog(VA)") +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 7.5),
        axis.text = element_text(size = 6.5))

chemin_fig_dlog <- file.path(DOSSIER_FIGURES, "01b_toutes_branches_deltalog.png")
ggsave(chemin_fig_dlog, p_dlog, width = 12, height = 10, dpi = 150)
cat(sprintf("Figure (Δlog, 16 branches) enregistree : %s\n", chemin_fig_dlog))

cat("\nEtape terminee.\n")