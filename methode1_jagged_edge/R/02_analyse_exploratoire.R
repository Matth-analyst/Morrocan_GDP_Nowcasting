# ============================================================================
# 02_analyse_exploratoire.R -- Statistiques descriptives, stationnarite,
#                               visualisations
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles.rds"))
indicateurs <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs.rds"))

# ----------------------------------------------------------------------------
# 1) Vue d'ensemble : trajectoires des 16 VA (niveaux)
# ----------------------------------------------------------------------------
p1 <- ggplot(cibles, aes(date, va)) +
  geom_line(color = "#2E74B5", linewidth = 0.5) +
  facet_wrap(~ branche, scales = "free_y", ncol = 4) +
  labs(title = "Valeur ajoutée trimestrielle par branche (1998–2026)",
       subtitle = "Base 2014, prix chaînés, rétropolée — source HCP",
       x = NULL, y = "Mdh") +
  theme(strip.text = element_text(size = 7.5), axis.text = element_text(size = 6))
ggsave(file.path(DOSSIER_FIGURES, "01_va_niveaux_toutes_branches.png"), p1, width = 12, height = 8, dpi = 150)

# ----------------------------------------------------------------------------
# 2) Taux de croissance trimestriels (Δlog) -- la transformation utilisee
#    partout dans le papier GDPNow (equations 4-15) pour rendre les series
#    comparables et approximativement stationnaires
# ----------------------------------------------------------------------------
cibles <- cibles %>%
  arrange(branche, date) %>%
  group_by(branche) %>%
  mutate(dlog_va = c(NA, diff(log(va)))) %>%
  ungroup()

p2 <- ggplot(cibles %>% filter(!is.na(dlog_va)), aes(date, dlog_va)) +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_line(color = "#C55A11", linewidth = 0.4) +
  facet_wrap(~ branche, scales = "free_y", ncol = 4) +
  labs(title = "Taux de croissance trimestriel (Δlog) par branche",
       x = NULL, y = "Δlog") +
  theme(strip.text = element_text(size = 7.5), axis.text = element_text(size = 6))
ggsave(file.path(DOSSIER_FIGURES, "02_croissance_toutes_branches.png"), p2, width = 12, height = 8, dpi = 150)

# ----------------------------------------------------------------------------
# 3) Tests de stationnarite (Dickey-Fuller augmente) -- niveaux vs Δlog
#    Justifie le choix de travailler en taux de croissance plutot qu'en
#    niveaux pour toute la suite du pipeline (BVAR, bridge equations).
# ----------------------------------------------------------------------------
test_stationnarite <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 12) return(c(p_niveau = NA, p_dlog = NA))
  p_niveau <- tryCatch(adf.test(x, k = 4)$p.value, error = function(e) NA)
  dlog <- diff(log(x[x > 0]))
  p_dlog <- tryCatch(adf.test(dlog, k = 4)$p.value, error = function(e) NA)
  c(p_niveau = p_niveau, p_dlog = p_dlog)
}

resultats_adf <- cibles %>%
  group_by(branche) %>%
  summarise(res = list(test_stationnarite(va))) %>%
  mutate(p_niveau = map_dbl(res, "p_niveau"), p_dlog = map_dbl(res, "p_dlog")) %>%
  select(-res) %>%
  mutate(stationnaire_niveau = p_niveau < 0.05,
         stationnaire_dlog = p_dlog < 0.05)

write.csv(resultats_adf, file.path(DOSSIER_RESULTATS, "tests_stationnarite.csv"), row.names = FALSE)
cat("\n--- Tests de stationnarite (Dickey-Fuller augmente, H0 = racine unitaire) ---\n")
print(resultats_adf, n = 20)
cat(sprintf("\nStationnaire en niveau : %d/16 branches\n", sum(resultats_adf$stationnaire_niveau, na.rm=TRUE)))
cat(sprintf("Stationnaire en Δlog   : %d/16 branches\n", sum(resultats_adf$stationnaire_dlog, na.rm=TRUE)))
cat("=> Confirme le choix de modeliser les taux de croissance (Δlog) plutot que\n")
cat("   les niveaux, conformement a la pratique standard (Higgins 2014 ; toutes\n")
cat("   les equations du papier GDPNow sont ecrites en Δlog).\n")

# ----------------------------------------------------------------------------
# 4) Matrice de correlation des taux de croissance entre branches
#    (justifie l'interet d'un BVAR multivarie plutot que 16 AR univaries
#    independants -- Banbura, Giannone, Reichlin 2010)
# ----------------------------------------------------------------------------
mat_large <- cibles %>%
  select(branche, date, dlog_va) %>%
  pivot_wider(names_from = branche, values_from = dlog_va) %>%
  arrange(date)
mat_cor <- cor(mat_large[,-1], use = "pairwise.complete.obs")

mat_cor_df <- as.data.frame(as.table(mat_cor))
names(mat_cor_df) <- c("branche1", "branche2", "correlation")
p3 <- ggplot(mat_cor_df, aes(branche1, branche2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "#C55A11", mid = "white", high = "#2E74B5", midpoint = 0, limits = c(-1,1)) +
  labs(title = "Corrélation croisée des taux de croissance trimestriels entre branches",
       x = NULL, y = NULL, fill = "r") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7))
ggsave(file.path(DOSSIER_FIGURES, "03_correlation_croisee_branches.png"), p3, width = 9, height = 8, dpi = 150)

# ----------------------------------------------------------------------------
# 5) Nuages de points indicateur vs croissance de la VA (les 41 series
#    retenues), avec droite de regression -- verification visuelle du
#    critere 5 (corrélation) applique en amont
# ----------------------------------------------------------------------------
indic_trim <- indicateurs %>%
  group_by(branche, indicateur, source, r_declare, signe_atypique) %>%
  group_modify(~ {
    df <- .x %>% arrange(date)
    if (unique(.x$frequence) == "mensuel") {
      df <- df %>% mutate(trimestre = floor_date(date, "quarter")) %>%
        group_by(trimestre) %>% summarise(valeur = mean(valeur, na.rm = TRUE)) %>%
        rename(date = trimestre)
    }
    df %>% arrange(date) %>% mutate(dlog = c(NA, diff(log(pmax(valeur, 1e-6)))))
  }) %>%
  ungroup() %>%
  filter(!is.na(dlog))

indic_vs_cible <- indic_trim %>%
  inner_join(cibles %>% select(branche, date, dlog_va), by = c("branche", "date"))

p4 <- ggplot(indic_vs_cible, aes(dlog, dlog_va, color = signe_atypique)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6) +
  scale_color_manual(values = c(`FALSE` = "#2E74B5", `TRUE` = "#C00000"),
                      labels = c("Signe cohérent", "Signe atypique")) +
  facet_wrap(~ branche, scales = "free", ncol = 3) +
  labs(title = "Taux de croissance des indicateurs vs. taux de croissance de la VA",
       subtitle = "Chaque point = un trimestre ; droite = régression linéaire simple",
       x = "Δlog indicateur", y = "Δlog VA branche", color = NULL) +
  theme(strip.text = element_text(size = 7))
ggsave(file.path(DOSSIER_FIGURES, "04_nuages_indicateurs_vs_cible.png"), p4, width = 11, height = 12, dpi = 150)

cat("\nFigures exploratoires enregistrees dans", DOSSIER_FIGURES, "\n")
saveRDS(indic_trim, file.path(DOSSIER_RESULTATS, "indicateurs_trimestriels.rds"))
saveRDS(cibles, file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
