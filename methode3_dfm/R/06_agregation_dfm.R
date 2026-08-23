source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
dfm <- readRDS(file.path(DOSSIER_RESULTATS, "dfm_resultats.rds"))
ar4 <- readRDS(file.path(DOSSIER_RESULTATS, "ar4_non_couvertes.rds"))

dernier_trim <- cibles %>% group_by(branche) %>% filter(date == max(date)) %>% ungroup() %>% select(branche, va)
poids <- dernier_trim %>% mutate(part = va / sum(va))

prev_couvertes <- tibble(branche = names(dfm), prevision = sapply(dfm, `[[`, "prevision_finale"))
prev_non_couvertes <- tibble(branche = names(ar4), prevision = sapply(ar4, `[[`, "prevision"))

previsions <- bind_rows(prev_couvertes, prev_non_couvertes) %>%
  left_join(poids, by = "branche") %>% mutate(contribution = part * prevision)

croissance_pib_nowcast <- sum(previsions$contribution, na.rm = TRUE)

cat("\n=== NOWCAST DU PIB (variante DFM) : prochain trimestre ===\n\n")
print(previsions %>% arrange(desc(contribution)) %>% mutate(across(c(prevision, part, contribution), ~round(., 4))))
cat(sprintf("\nCroissance trimestrielle du PIB (Δlog), nowcast DFM : %+.4f (%.2f %%)\n",
            croissance_pib_nowcast, croissance_pib_nowcast * 100))

saveRDS(list(previsions = previsions, croissance_pib = croissance_pib_nowcast),
        file.path(DOSSIER_RESULTATS, "nowcast_dfm.rds"))

previsions_fig <- previsions %>%
  mutate(groupe = ifelse(branche %in% names(dfm)[sapply(dfm, `[[`, "methode") == "DFM"],
                          "DFM", ifelse(branche %in% names(dfm), "Repli Méthode 1", "AR(4)")),
         branche = factor(branche, levels = branche[order(contribution)]))
p <- ggplot(previsions_fig, aes(branche, contribution, fill = groupe)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c("DFM" = "#2E74B5", "Repli Méthode 1" = "#7F9CB5", "AR(4)" = "#BFBFBF")) +
  labs(title = "Contribution de chaque branche au nowcast du PIB : Méthode DFM",
       subtitle = sprintf("Croissance trimestrielle agrégée : %+.2f %%", croissance_pib_nowcast * 100),
       x = NULL, y = "Contribution (points de Δlog)", fill = NULL)
ggsave(file.path(DOSSIER_FIGURES, "07_contributions_pib_dfm.png"), p, width = 8, height = 7, dpi = 150)
cat("\nNowcast DFM sauvegarde.\n")
