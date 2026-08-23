# ============================================================================
# 06b_agregation_midas.R -- Nowcast national du PIB, variante MIDAS
# ============================================================================
# NOUVEAU SCRIPT. Meme structure que 06_agregation_fisher.R, mais utilise les
# previsions MIDAS (04b_midas.R) pour les branches couvertes ou elles sont
# disponibles. Quand MIDAS n'est pas calculable pour une branche couverte
# (pas d'indicateur mensuel, ou echantillon insuffisant), on retombe sur la
# prevision bridge+BVAR deja combinee (04_bridge_equations.R) -- la meilleure
# information disponible pour cette branche plutot qu'une case vide qui
# biaiserait le total. Chaque repli est trace explicitement.
#
# Les 9 branches non couvertes utilisent l'AR(4) habituel, inchange par
# rapport a 06_agregation_fisher.R.
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
bridge <- readRDS(file.path(DOSSIER_RESULTATS, "bridge_equations.rds"))
midas <- readRDS(file.path(DOSSIER_RESULTATS, "midas_equations.rds"))
ar4 <- readRDS(file.path(DOSSIER_RESULTATS, "ar4_non_couvertes.rds"))

dernier_trim <- cibles %>% group_by(branche) %>% filter(date == max(date)) %>%
  ungroup() %>% select(branche, va)
poids <- dernier_trim %>% mutate(part = va / sum(va))

prev_couvertes <- tibble(branche = BRANCHES_COUVERTES) %>%
  rowwise() %>%
  mutate(
    prevision = if (!is.null(midas[[branche]])) midas[[branche]]$prevision_midas
                else if (!is.null(bridge[[branche]])) bridge[[branche]]$prevision_finale
                else NA_real_,
    methode = if (!is.null(midas[[branche]])) "MIDAS"
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

croissance_pib_midas <- sum(previsions$contribution, na.rm = TRUE)

cat("\n=== NOWCAST DU PIB -- VARIANTE MIDAS ===\n\n")
print(previsions %>% arrange(desc(contribution)) %>%
        mutate(across(c(prevision, part, contribution), ~ round(., 4))))
cat(sprintf("\nCroissance trimestrielle du PIB (Δlog), variante MIDAS : %+.4f (%.2f %%)\n",
            croissance_pib_midas, croissance_pib_midas * 100))
cat(sprintf("(dont %d branche(s) couverte(s) avec un vrai MIDAS, %d en repli bridge+BVAR sur %d branches couvertes)\n",
            sum(prev_couvertes$methode == "MIDAS"), sum(prev_couvertes$methode == "Repli bridge+BVAR"),
            length(BRANCHES_COUVERTES)))

saveRDS(list(previsions = previsions, croissance_pib = croissance_pib_midas),
        file.path(DOSSIER_RESULTATS, "nowcast_midas.rds"))
cat("\nNowcast variante MIDAS sauvegarde.\n")
