# ============================================================================
# 06_figures.R -- Les figures du rapport
# ============================================================================
source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")

fig <- function(x) file.path(DOSSIER_FIGURES, x)
theme_sobre <- ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                 legend.position = "bottom", legend.title = ggplot2::element_blank())

ag <- lire_csv(file.path(DOSSIER_RESULTATS, "04_agregat.csv")) %>%
  dplyr::mutate(origine = as.Date(origine))
m1 <- lire_csv(file.path("..", "Method1BvarBridge", "resultats",
                         "06_agregat.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(origine, m1 = nowcast_niveau)
cmp <- ag %>% dplyr::inner_join(m1, by = "origine")

# --- 1. les deux methodes contre le realise ---------------------------------
d1 <- cmp %>%
  tidyr::pivot_longer(c(reel_ag, dfm_ag, m1), names_to = "serie",
                      values_to = "v") %>%
  dplyr::mutate(serie = dplyr::recode(serie, reel_ag = "réalisé",
                                      dfm_ag = "facteurs dynamiques",
                                      m1 = "méthode 1"))
g <- ggplot2::ggplot(d1, ggplot2::aes(origine, 100 * v, colour = serie,
                                      linetype = serie)) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey70", linewidth = .3) +
  ggplot2::geom_line(linewidth = .7) +
  ggplot2::scale_colour_manual(values = c("réalisé" = "grey20",
                                          "facteurs dynamiques" = "#B45309",
                                          "méthode 1" = "#2E74B5")) +
  ggplot2::scale_linetype_manual(values = c("réalisé" = "solid",
                                            "facteurs dynamiques" = "dashed",
                                            "méthode 1" = "solid")) +
  ggplot2::labs(title = "Croissance trimestrielle de la valeur ajoutée totale",
                subtitle = "réalisé, et ce que chaque méthode aurait annoncé",
                x = NULL, y = "%") + theme_sobre
ggplot2::ggsave(fig("04_agregat_compare.png"), g, width = 8, height = 4.4, dpi = 150)

# --- 2. prevu contre realise ------------------------------------------------
d2 <- cmp %>%
  tidyr::pivot_longer(c(dfm_ag, m1), names_to = "methode", values_to = "prev") %>%
  dplyr::mutate(methode = dplyr::recode(methode, dfm_ag = "facteurs dynamiques",
                                        m1 = "méthode 1"))
g <- ggplot2::ggplot(d2, ggplot2::aes(100 * reel_ag, 100 * prev)) +
  ggplot2::geom_abline(slope = 1, colour = "grey65", linetype = "dashed") +
  ggplot2::geom_point(ggplot2::aes(colour = methode), size = 1.7, alpha = .8) +
  ggplot2::geom_smooth(ggplot2::aes(colour = methode), method = "lm",
                       se = FALSE, linewidth = .6, formula = y ~ x) +
  ggplot2::scale_colour_manual(values = c("facteurs dynamiques" = "#B45309",
                                          "méthode 1" = "#2E74B5")) +
  ggplot2::facet_wrap(~ methode) +
  ggplot2::labs(title = "Prévu contre réalisé", x = "réalisé (%)",
                y = "prévu (%)") + theme_sobre +
  ggplot2::theme(legend.position = "none")
ggplot2::ggsave(fig("04_prevu_realise.png"), g, width = 8, height = 4.2, dpi = 150)

# --- 3. ratio par branche ---------------------------------------------------
pb <- lire_csv(file.path(DOSSIER_RESULTATS, "04_previsions_branche.csv")) %>%
  dplyr::mutate(origine = as.Date(origine))
m1i <- lire_csv(file.path("..", "Method1BvarBridge", "resultats",
                          "09_previsions_intra.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>% dplyr::filter(scenario == "M3")
bv <- lire_csv(file.path("..", "Method1BvarBridge", "resultats",
                         "03e_previsions_corrigees.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(branche, origine, bvar = prevision)
cb <- pb %>% dplyr::select(branche, origine, dfm = prevision, reel) %>%
  dplyr::left_join(m1i %>% dplyr::select(branche, origine, combinee),
                   by = c("branche", "origine")) %>%
  dplyr::left_join(bv, by = c("branche", "origine")) %>%
  dplyr::mutate(m1 = dplyr::coalesce(combinee, bvar)) %>%
  dplyr::filter(!is.na(dfm), !is.na(m1), !is.na(reel)) %>%
  dplyr::group_by(branche) %>%
  dplyr::summarise(`facteurs dynamiques` = sqrt(mean((reel - dfm)^2)) / stats::sd(reel),
                   `méthode 1` = sqrt(mean((reel - m1)^2)) / stats::sd(reel),
                   .groups = "drop")
ecrire_csv(cb, file.path(DOSSIER_RESULTATS, "04_comparaison_branches.csv"))
d3 <- cb %>% tidyr::pivot_longer(-branche, names_to = "methode",
                                 values_to = "ratio")
ordre <- cb$branche[order(cb$`facteurs dynamiques`)]
g <- ggplot2::ggplot(d3, ggplot2::aes(factor(branche, levels = ordre), ratio,
                                      fill = methode)) +
  ggplot2::geom_hline(yintercept = 1, colour = "grey55", linetype = "dashed") +
  ggplot2::geom_col(position = "dodge", width = .72) +
  ggplot2::scale_fill_manual(values = c("facteurs dynamiques" = "#B45309",
                                        "méthode 1" = "#2E74B5")) +
  ggplot2::coord_flip() +
  ggplot2::labs(title = "Ratio d'erreur par branche",
                subtitle = "sous 1, le modèle bat la moyenne historique",
                x = NULL, y = "ratio") + theme_sobre
ggplot2::ggsave(fig("04_ratio_branches.png"), g, width = 8, height = 5.4, dpi = 150)

# --- 4. configurations (r, p) -----------------------------------------------
gr <- lire_csv(file.path(DOSSIER_RESULTATS, "03_grille_rp.csv"))
g <- ggplot2::ggplot(gr, ggplot2::aes(factor(r), ratio, fill = factor(p))) +
  ggplot2::geom_hline(yintercept = 1, colour = "grey55", linetype = "dashed") +
  ggplot2::geom_boxplot(outlier.size = .8, width = .6) +
  ggplot2::labs(title = "Ratio selon le nombre de facteurs et l'ordre du VAR",
                subtitle = "chaque boîte rassemble les douze branches couvertes",
                x = "nombre de facteurs r", y = "ratio", fill = "ordre p") +
  theme_sobre + ggplot2::theme(legend.title = ggplot2::element_text())
ggplot2::ggsave(fig("03_grille_rp.png"), g, width = 7.5, height = 4.2, dpi = 150)

# --- 5. selection recursive -------------------------------------------------
sr <- lire_csv(file.path(DOSSIER_RESULTATS, "04_selection_rp.csv")) %>%
  dplyr::mutate(origine = as.Date(origine), config = paste0("r=", r, ", p=", p))
g <- ggplot2::ggplot(sr, ggplot2::aes(origine, branche, fill = config)) +
  ggplot2::geom_tile(colour = "white", linewidth = .2) +
  ggplot2::scale_fill_brewer(palette = "Blues") +
  ggplot2::labs(title = "Configuration retenue à chaque origine",
                subtitle = "choisie sur la performance des origines précédentes",
                x = NULL, y = NULL) + theme_sobre
ggplot2::ggsave(fig("04_selection_rp.png"), g, width = 8.5, height = 4.6, dpi = 150)

# --- 6. taux d'observation des panels ---------------------------------------
bl <- lire_csv(file.path(DOSSIER_RESULTATS, "02_panel_bilan.csv"))
d6 <- bl %>% dplyr::select(branche, mensuelles, trimestrielles) %>%
  tidyr::pivot_longer(-branche, names_to = "type", values_to = "n")
g <- ggplot2::ggplot(d6, ggplot2::aes(stats::reorder(branche, n, sum), n,
                                      fill = type)) +
  ggplot2::geom_col(width = .72) + ggplot2::coord_flip() +
  ggplot2::scale_fill_manual(values = c(mensuelles = "#2E74B5",
                                        trimestrielles = "#B45309")) +
  ggplot2::labs(title = "Composition du panel par branche",
                subtitle = "les séries trimestrielles sont conservées, non exclues",
                x = NULL, y = "nombre de séries") + theme_sobre
ggplot2::ggsave(fig("02_panel.png"), g, width = 8, height = 4.6, dpi = 150)

cat(sprintf("\n%d figures ecrites\n", length(list.files(DOSSIER_FIGURES))))
