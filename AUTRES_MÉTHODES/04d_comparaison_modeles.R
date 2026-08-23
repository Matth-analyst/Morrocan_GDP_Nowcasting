# ============================================================================
# 04d_comparaison_modeles.R -- Tableau comparatif des 4 approches
#                               (branches couvertes uniquement)
# ============================================================================
# NOUVEAU SCRIPT -- a lancer APRES 03_bvar_trimestriel.R, 04_bridge_equations.R,
# 04b_midas.R et 04c_dfm.R (relit uniquement leurs .rds, ne recalcule rien).
# ============================================================================
source("R/00_setup.R")

bvar   <- readRDS(file.path(DOSSIER_RESULTATS, "bvar_modele.rds"))
bridge <- readRDS(file.path(DOSSIER_RESULTATS, "bridge_equations.rds"))
midas  <- readRDS(file.path(DOSSIER_RESULTATS, "midas_equations.rds"))
dfm    <- readRDS(file.path(DOSSIER_RESULTATS, "dfm_equations.rds"))

recuperer <- function(liste, branche, champ) {
  if (is.null(liste[[branche]])) return(NA_real_)
  liste[[branche]][[champ]]
}

comparaison <- tibble(branche = BRANCHES_COUVERTES) %>%
  mutate(
    BVAR   = bvar$prevision[branche],
    Bridge = sapply(branche, recuperer, liste = bridge, champ = "prevision_bridge"),
    MIDAS  = sapply(branche, recuperer, liste = midas,  champ = "prevision_midas"),
    DFM    = sapply(branche, recuperer, liste = dfm,    champ = "prevision_dfm"),
    `Bridge+BVAR combine (delta)` = sapply(branche, recuperer, liste = bridge, champ = "prevision_finale")
  )

cat("\n=== COMPARAISON DES APPROCHES -- branches couvertes (Δlog, prochain trimestre) ===\n\n")
print(comparaison %>% mutate(across(where(is.numeric), ~round(., 4))))

write.csv(comparaison, file.path(DOSSIER_RESULTATS, "comparaison_modeles.csv"), row.names = FALSE)
cat("\nTableau comparatif sauvegarde dans resultats/comparaison_modeles.csv\n")

# --- Figure : les 4 previsions cote a cote, par branche --------------------
df_fig <- comparaison %>%
  select(branche, BVAR, Bridge, MIDAS, DFM) %>%
  pivot_longer(-branche, names_to = "modele", values_to = "prevision") %>%
  filter(!is.na(prevision))

p_comp <- ggplot(df_fig, aes(branche, prevision, fill = modele)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c(BVAR = "#7F7F7F", Bridge = "#2E74B5",
                                MIDAS = "#C55A11", DFM = "#4C8C4A")) +
  labs(title = "Comparaison des approches de nowcasting par branche couverte",
       x = NULL, y = "Δlog prévu (prochain trimestre)", fill = NULL)
ggsave(file.path(DOSSIER_FIGURES, "09_comparaison_modeles.png"), p_comp, width = 9, height = 6, dpi = 150)

cat("\nFigure comparative enregistree dans figures/09_comparaison_modeles.png\n")
