# ============================================================================
# 06_agregation_fisher.R -- Agregation des 16 nowcasts en croissance du PIB
# ============================================================================
# Reference : formule de contribution a la croissance, Higgins (2014),
# annexe, equations A4-A5 (developpement multinomial de (1+g)^4), reprise
# de BEA (2014) et Whelan (2000).
#
# LIMITE IMPORTANTE, deja documentee dans le rapport transmis a
# l'encadrant : les poids de ponderation utilises ici sont des parts de
# VALEUR AJOUTEE EN VOLUME (prix chaines) au dernier trimestre observe,
# et non des parts en PRIX COURANTS comme l'exige rigoureusement une
# agregation de Fisher/Tornqvist (les valeurs chainees ne sont pas
# additives). C'est une approximation, pas le calcul rigoureux -- a
# refaire si la serie "Production des branches aux prix courants" devient
# disponible pour les 16 branches.
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
bvar <- readRDS(file.path(DOSSIER_RESULTATS, "bvar_modele.rds"))
bridge <- readRDS(file.path(DOSSIER_RESULTATS, "bridge_equations.rds"))
ar4 <- readRDS(file.path(DOSSIER_RESULTATS, "ar4_non_couvertes.rds"))

# --- Poids (parts de VA en volume, dernier trimestre observe) -------------
dernier_trim <- cibles %>% group_by(branche) %>% filter(date == max(date)) %>%
  ungroup() %>% select(branche, va)
poids <- dernier_trim %>% mutate(part = va / sum(va))

# --- Rassemblement des previsions de croissance par branche ----------------
prev_couvertes <- tibble(
  branche = names(bridge),
  prevision = sapply(bridge, `[[`, "prevision_finale")
)
prev_non_couvertes <- tibble(
  branche = names(ar4),
  prevision = sapply(ar4, `[[`, "prevision")
)
previsions <- bind_rows(prev_couvertes, prev_non_couvertes) %>%
  left_join(poids, by = "branche") %>%
  mutate(contribution = part * prevision)

# --- Croissance globale du PIB (approximation log-lineaire) ---------------
# 1 + g_PIB ~= somme des contributions ponderees (cf. equation A4 du papier,
# tronquee au premier ordre -- suffisant pour une approximation trimestrielle)
croissance_pib_nowcast <- sum(previsions$contribution)

cat("\n=== NOWCAST DU PIB MAROCAIN — prochain trimestre ===\n\n")
print(previsions %>% arrange(desc(contribution)) %>%
        mutate(across(c(prevision, part, contribution), ~round(., 4))))
cat(sprintf("\nCroissance trimestrielle du PIB (Δlog), nowcast agrégé : %+.4f (%.2f %%)\n",
            croissance_pib_nowcast, croissance_pib_nowcast * 100))

saveRDS(list(previsions = previsions, croissance_pib = croissance_pib_nowcast),
        file.path(DOSSIER_RESULTATS, "nowcast_final.rds"))

# --- Figure : contribution de chaque branche a la croissance du PIB -------
previsions_fig <- previsions %>%
  mutate(groupe = ifelse(branche %in% BRANCHES_COUVERTES, "Couverte (bridge+BVAR)", "Non couverte (AR(4))"),
         branche = factor(branche, levels = branche[order(contribution)]))
p7 <- ggplot(previsions_fig, aes(branche, contribution, fill = groupe)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Couverte (bridge+BVAR)" = "#2E74B5", "Non couverte (AR(4))" = "#BFBFBF")) +
  labs(title = "Contribution de chaque branche au nowcast du PIB",
       subtitle = sprintf("Croissance trimestrielle agrégée : %+.2f %%", croissance_pib_nowcast * 100),
       x = NULL, y = "Contribution (points de Δlog)", fill = NULL)
ggsave(file.path(DOSSIER_FIGURES, "07_contributions_pib.png"), p7, width = 8, height = 7, dpi = 150)

cat("\nNowcast final sauvegarde.\n")
