# ============================================================================
# 06c_agregation_dfm.R -- Nowcast national du PIB, variante DFM
# ============================================================================
# NOUVEAU SCRIPT. Meme logique que 06b_agregation_midas.R, avec les
# previsions DFM (04c_dfm.R) a la place de MIDAS. Repli sur bridge+BVAR pour
# toute branche couverte ou le DFM n'est pas calculable (moins de 2
# indicateurs mensuels, ou echec de convergence) -- attendu pour un nombre
# de branches plus important qu'avec MIDAS, puisque le DFM exige au moins 2
# series mensuelles par branche.
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
bridge <- readRDS(file.path(DOSSIER_RESULTATS, "bridge_equations.rds"))
dfm <- readRDS(file.path(DOSSIER_RESULTATS, "dfm_equations.rds"))
ar4 <- readRDS(file.path(DOSSIER_RESULTATS, "ar4_non_couvertes.rds"))

dernier_trim <- cibles %>% group_by(branche) %>% filter(date == max(date)) %>%
  ungroup() %>% select(branche, va)
poids <- dernier_trim %>% mutate(part = va / sum(va))

prev_couvertes <- tibble(branche = BRANCHES_COUVERTES) %>%
  rowwise() %>%
  mutate(
    prevision = if (!is.null(dfm[[branche]])) dfm[[branche]]$prevision_dfm
                else if (!is.null(bridge[[branche]])) bridge[[branche]]$prevision_finale
                else NA_real_,
    methode = if (!is.null(dfm[[branche]])) "DFM"
              else if (!is.null(bridge[[branche]])) "Repli bridge+BVAR"
              else "Non disponible"
  ) %>%
  ungroup() %>%
  filter(!is.na(prevision))

prev_non_couvertes <- tibble(branche = names(ar4),
                              prevision = sapply(ar4, `[[`, "prevision"),
                              methode = "AR(4)")

previsions <- bind_rows(prev_couvertes, prev_non_couvertes) %>%
  left_join(poids, by = "branche") %>%
  mutate(contribution = part * prevision)

croissance_pib_dfm <- sum(previsions$contribution, na.rm = TRUE)

cat("\n=== NOWCAST DU PIB -- VARIANTE DFM ===\n\n")
print(previsions %>% arrange(desc(contribution)) %>%
        mutate(across(c(prevision, part, contribution), ~ round(., 4))))
cat(sprintf("\nCroissance trimestrielle du PIB (Δlog), variante DFM : %+.4f (%.2f %%)\n",
            croissance_pib_dfm, croissance_pib_dfm * 100))
cat(sprintf("(dont %d branche(s) couverte(s) avec un vrai DFM, %d en repli bridge+BVAR sur %d branches couvertes)\n",
            sum(prev_couvertes$methode == "DFM"), sum(prev_couvertes$methode == "Repli bridge+BVAR"),
            length(BRANCHES_COUVERTES)))

saveRDS(list(previsions = previsions, croissance_pib = croissance_pib_dfm),
        file.path(DOSSIER_RESULTATS, "nowcast_dfm.rds"))
cat("\nNowcast variante DFM sauvegarde.\n")
