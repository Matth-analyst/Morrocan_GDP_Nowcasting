# ============================================================================
# 13_benchmarks.R -- PHASES 16, 17 et 20 : le systeme complexe sert-il ?
# ============================================================================
# LA QUESTION DU PLAN
#   Phase 16 : "Conserver au minimum un AR(2) [...] estime recursivement.
#   L'objectif est de determiner si le systeme complexe apporte reellement une
#   amelioration."
#
#   C'est la question qui decide si tout ce qui precede valait la peine. Un
#   nowcast fonde sur 431 indicateurs, un BVAR a prior de Minnesota, seize
#   passerelles et une agregation en prix courants doit faire mieux qu'un AR(2)
#   sur l'agregat -- sans quoi il faut le dire.
#
# CE QUI EST COMPARE, SUR L'AGREGAT
#   Le plan demande AR(2) au minimum, et si possible AR(1), AR(4), un modele
#   naif et la moyenne historique. On ajoute la decomposition demandee par la
#   phase 20 -- BVAR seul, passerelle seule, combinaison -- pour identifier
#   D'OU vient la performance, et non seulement si elle existe.
#
#   Tous les etalons sont estimes RECURSIVEMENT sur l'agregat lui-meme : a
#   chaque origine, sur les seules donnees anterieures. Un AR(2) estime une fois
#   sur tout l'echantillon serait un etalon truque.
#
# UNE PRECISION QUI CHANGE LA LECTURE
#   Les etalons portent sur l'AGREGAT, pas sur les branches. Ils ne prevoient
#   donc pas seize series puis ne les agregent : ils modelisent directement la
#   croissance de la valeur ajoutee totale. C'est ce qui en fait des etalons
#   honnetes -- un praticien qui n'aurait ni indicateurs ni BVAR ferait
#   exactement cela.
#
# SORTIES
#   resultats/13_previsions_benchmarks.csv
#   resultats/13_evaluation.csv
#   resultats/13_dm_contre_systeme.csv
#   figures/13_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
suppressPackageStartupMessages(library(forecast))

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("13_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("13_", x))

SEUIL_DM <- 0.10

cat("\n[1/5] L'agregat et ses composantes\n")
ag <- lire_csv(file.path(DOSSIER_RESULTATS, "06_agregat.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(origine, trimestre, reel = reel_niveau,
                systeme = nowcast_niveau, bvar = bvar_niveau) %>%
  dplyr::arrange(origine)
cat(sprintf("      %d trimestres, de %s a %s\n", nrow(ag),
            ag$trimestre[1], ag$trimestre[nrow(ag)]))

# La serie agregee complete, pour estimer les etalons sur l'historique ANTERIEUR
# a la premiere origine du backtest. Sans cela, l'AR(2) de la premiere origine
# n'aurait que deux observations.
va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g))
poids_reels <- va %>% dplyr::group_by(date) %>%
  dplyr::mutate(w = NA_real_) %>% dplyr::ungroup()

# Avant 2014 il n'y a pas de poids en prix courants ; on utilise les poids en
# VOLUME, dont l'ecart a ete mesure a 0,047 point de croissance en mediane sur
# la periode ou les deux coexistent (voir rapport des phases 4 et 13, §19.9).
# C'est une approximation, et elle ne sert QU'A alimenter l'historique des
# etalons -- jamais a evaluer quoi que ce soit.
niveaux <- charger_va()
hist_ag <- niveaux %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total = sum(va), .groups = "drop") %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(g = c(NA_real_, diff(log(total)))) %>%
  dplyr::filter(!is.na(g)) %>%
  dplyr::select(date, g)
cat(sprintf("      historique agrege : %d trimestres depuis %s\n",
            nrow(hist_ag), date_vers_trimestre(min(hist_ag$date))))

# On raccorde : sur les origines du backtest, la serie de reference est
# l'agregat en prix courants ; avant, l'agregat en volume.
serie <- hist_ag %>%
  dplyr::left_join(ag %>% dplyr::select(date = origine, reel), by = "date") %>%
  dplyr::mutate(g = dplyr::coalesce(reel, g)) %>%
  dplyr::select(date, g)

# ============================================================================
# 2) LES ETALONS, ESTIMES RECURSIVEMENT
# ============================================================================
cat("\n[2/5] Etalons\n")
ar_prev <- function(g, p) {
  n <- length(g)
  if (n <= p + 5L) return(NA_real_)
  X <- cbind(1, sapply(seq_len(p), function(l) g[(p + 1 - l):(n - l)]))
  y <- g[(p + 1):n]
  b <- tryCatch(qr.solve(X, y), error = function(e) NULL)
  if (is.null(b)) return(NA_real_)
  sum(b * c(1, rev(g[(n - p + 1):n])))
}

benchmarks <- purrr::map_dfr(seq_len(nrow(ag)), function(i) {
  cible <- ag$origine[i]
  h <- serie$g[serie$date < cible]
  if (length(h) < 12L) return(NULL)
  tibble::tibble(
    origine = cible, trimestre = ag$trimestre[i], reel = ag$reel[i],
    `Systeme complet`   = ag$systeme[i],
    `BVAR agrege`       = ag$bvar[i],
    `AR(1)`             = ar_prev(h, 1L),
    `AR(2)`             = ar_prev(h, 2L),
    `AR(4)`             = ar_prev(h, 4L),
    `Moyenne historique` = mean(h),
    `Naif (trimestre precedent)` = h[length(h)])
})
ecrire_csv(benchmarks, chemin_res("previsions_benchmarks.csv"))
cat(sprintf("      %d origines, %d modeles\n", nrow(benchmarks), 7L))

# ============================================================================
# 3) EVALUATION  (phase 17 du plan)
# ============================================================================
cat("\n[3/5] Evaluation\n")
long <- benchmarks %>%
  tidyr::pivot_longer(-c(origine, trimestre, reel),
                      names_to = "modele", values_to = "prevision") %>%
  dplyr::filter(!is.na(prevision)) %>%
  dplyr::mutate(erreur = reel - prevision)
ecrire_csv(long, chemin_res("erreurs_par_trimestre.csv"))

evaluer <- function(d, lab) d %>% dplyr::group_by(modele) %>%
  dplyr::summarise(n = dplyr::n(),
                   MAE = mean(abs(erreur)),
                   RMSFE = sqrt(mean(erreur^2)),
                   biais = mean(erreur),
                   ratio = sqrt(mean(erreur^2)) / stats::sd(reel),
                   correlation = suppressWarnings(stats::cor(prevision, reel)),
                   .groups = "drop") %>%
  dplyr::mutate(periode = lab) %>% dplyr::arrange(RMSFE)

evaluation <- dplyr::bind_rows(
  evaluer(long, "toutes origines"),
  evaluer(long %>% dplyr::filter(lubridate::year(origine) == 2020), "2020"),
  evaluer(long %>% dplyr::filter(lubridate::year(origine) != 2020), "hors 2020"))
ecrire_csv(evaluation, chemin_res("evaluation.csv"))

for (per in c("toutes origines", "hors 2020", "2020")) {
  cat(sprintf("\n      --- %s ---\n", per))
  print(as.data.frame(evaluation %>% dplyr::filter(periode == per) %>%
    dplyr::transmute(modele, n,
                     `MAE (pt)` = round(100 * MAE, 3),
                     `RMSFE (pt)` = round(100 * RMSFE, 3),
                     `biais (pt)` = round(100 * biais, 3),
                     ratio = round(ratio, 3),
                     `correl.` = round(correlation, 2))), row.names = FALSE)
}

# ============================================================================
# 4) LES ECARTS SONT-ILS SIGNIFICATIFS ?  (phase 18, applique aux etalons)
# ============================================================================
cat("\n[4/5] Diebold-Mariano contre le systeme complet\n")
ref <- long %>% dplyr::filter(modele == "Systeme complet") %>%
  dplyr::arrange(origine)
dm <- purrr::map_dfr(setdiff(unique(long$modele), "Systeme complet"), function(m) {
  alt <- long %>% dplyr::filter(modele == m) %>% dplyr::arrange(origine)
  com <- intersect(ref$origine, alt$origine)
  ea <- ref$erreur[ref$origine %in% com]
  eb <- alt$erreur[alt$origine %in% com]
  t <- tryCatch(forecast::dm.test(ea, eb, h = 1L, power = 2),
                error = function(e) NULL)
  tibble::tibble(etalon = m, n = length(com),
                 RMSFE_systeme = sqrt(mean(ea^2)),
                 RMSFE_etalon = sqrt(mean(eb^2)),
                 p_value = if (is.null(t)) NA_real_ else unname(t$p.value))
}) %>%
  dplyr::mutate(verdict = dplyr::case_when(
    is.na(p_value) ~ "indecidable",
    p_value >= SEUIL_DM ~ "non significatif",
    RMSFE_systeme < RMSFE_etalon ~ "systeme meilleur",
    TRUE ~ "etalon meilleur")) %>%
  dplyr::arrange(p_value)
ecrire_csv(dm, chemin_res("dm_contre_systeme.csv"))
print(as.data.frame(dm %>% dplyr::transmute(
  etalon, `RMSFE systeme (pt)` = round(100 * RMSFE_systeme, 3),
  `RMSFE etalon (pt)` = round(100 * RMSFE_etalon, 3),
  `p` = round(p_value, 3), verdict)), row.names = FALSE)

meilleur <- evaluation %>% dplyr::filter(periode == "toutes origines") %>%
  dplyr::slice_min(RMSFE, n = 1, with_ties = FALSE)
sys <- evaluation %>% dplyr::filter(periode == "toutes origines",
                                    modele == "Systeme complet")
ar2 <- evaluation %>% dplyr::filter(periode == "toutes origines",
                                    modele == "AR(2)")
cat(sprintf("\n      systeme complet : RMSFE %.3f pt | AR(2) : %.3f pt | gain %.0f %%\n",
            100 * sys$RMSFE, 100 * ar2$RMSFE,
            100 * (1 - sys$RMSFE / ar2$RMSFE)))
cat(sprintf("      meilleur modele toutes origines : %s\n", meilleur$modele))

# ============================================================================
# 5) FIGURES  (dont le trace demande par la phase 25)
# ============================================================================
cat("\n[5/5] Figures\n")
g1 <- benchmarks %>%
  dplyr::select(origine, reel, `Systeme complet`, `AR(2)`) %>%
  tidyr::pivot_longer(-c(origine, reel), names_to = "modele", values_to = "prevu") %>%
  ggplot2::ggplot(ggplot2::aes(origine)) +
  ggplot2::geom_line(ggplot2::aes(y = 100 * reel, colour = "Realise"),
                     linewidth = 0.55) +
  ggplot2::geom_line(ggplot2::aes(y = 100 * prevu, colour = modele),
                     linewidth = 0.5) +
  ggplot2::scale_colour_manual(values = c("Realise" = "black",
                                          "Systeme complet" = "#1f4e79",
                                          "AR(2)" = "#b03a2e")) +
  ggplot2::labs(x = NULL, y = "croissance trimestrielle (%)", colour = NULL,
                title = "Valeur ajoutee totale : realise, systeme et etalon AR(2)",
                subtitle = "le trace demande par la phase 25 du plan")
ggplot2::ggsave(chemin_fig("systeme_contre_ar2.png"), g1,
                width = 10, height = 4.5, dpi = 150)

g2 <- evaluation %>% dplyr::filter(periode != "toutes origines") %>%
  ggplot2::ggplot(ggplot2::aes(stats::reorder(modele, -RMSFE), 100 * RMSFE,
                               fill = periode)) +
  ggplot2::geom_col(position = "dodge", width = 0.7) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "RMSFE (points de croissance)", fill = NULL,
                title = "Le systeme complet contre ses etalons",
                subtitle = "l'echelle de 2020 ecrase celle des periodes calmes")
ggplot2::ggsave(chemin_fig("rmsfe_par_modele.png"), g2,
                width = 9.5, height = 5, dpi = 150)

g3 <- long %>%
  ggplot2::ggplot(ggplot2::aes(origine, 100 * erreur, colour = modele)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
  ggplot2::geom_line(linewidth = 0.4) +
  ggplot2::facet_wrap(~ modele, ncol = 2) +
  ggplot2::labs(x = NULL, y = "erreur (points)",
                title = "Erreur trimestre par trimestre",
                subtitle = "conservee pour chaque modele, comme le demande la phase 17") +
  ggplot2::theme(legend.position = "none")
ggplot2::ggsave(chemin_fig("erreurs.png"), g3, width = 10, height = 7, dpi = 150)

cat("      figures/13_systeme_contre_ar2.png\n")
cat("      figures/13_rmsfe_par_modele.png\n      figures/13_erreurs.png\n")
cat("\nBenchmarks termines.\n")
