# ============================================================================
# 16_direct_indirect.R -- Prevoir la VA totale : par les branches ou directement ?
# ============================================================================
# LA QUESTION
#   La cible du projet est la VALEUR AJOUTEE TOTALE. Tout ce qui precede la
#   prevoit de facon INDIRECTE : seize branches prevues separement, puis
#   agregees par un indice de Laspeyres. C'est une voie legitime, mais ce n'est
#   qu'une des deux.
#
#   La voie DIRECTE consiste a modeliser l'agregat lui-meme, avec les memes
#   indicateurs, sans passer par les branches.
#
# CE QUI LES SEPARE, ET POURQUOI AUCUNE THEORIE NE TRANCHE
#   L'indirecte exploite la structure sectorielle -- chaque branche recoit les
#   indicateurs qui la concernent -- mais elle cumule seize erreurs
#   d'estimation, et chaque branche est estimee sur une serie bien plus bruyante
#   que l'agregat : de 0,7 % d'ecart-type pour l'education-sante a 20 % pour la
#   peche, contre 2,28 % pour l'agregat.
#
#   La directe estime beaucoup moins de parametres sur une serie plus lisse,
#   mais elle perd l'information sectorielle : un indicateur de debarquements de
#   peche, tres informatif pour sa branche, se dilue dans un agregat ou la peche
#   pese 0,7 %.
#
#   La litterature sur le nowcasting ne tranche pas non plus : le resultat
#   depend de la structure de correlation des erreurs, donc des donnees. C'est
#   une question EMPIRIQUE.
#
# CE QUE LES ETALONS DEJA CALCULES NE DISENT PAS
#   Les AR(1) a AR(4) du script 13 sont une forme de prevision directe, mais
#   SANS INDICATEURS. Ils repondent a "faut-il un modele ?", pas a "faut-il
#   passer par les branches ?". Ce script comble cet ecart.
#
# LE DISPOSITIF
#   Meme protocole recursif que partout ailleurs : a chaque origine, selection
#   des indicateurs sur la seule information anterieure, passerelle estimee sur
#   t < T, terme autoregressif de l'agregat. Les indicateurs candidats sont ceux
#   de TOUTES les branches -- pour l'agregat, la distinction sectorielle n'a
#   plus lieu d'etre.
#
# SORTIES
#   resultats/16_previsions_directes.csv
#   resultats/16_comparaison.csv
#   figures/16_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")
source("R/fonctions/kalman.R")
suppressPackageStartupMessages(library(forecast))

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("16_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("16_", x))

PREMIERE_CIBLE <- as.Date("2014-06-30")
SEUIL_R <- 0.15; SEUIL_P <- 0.10
MIN_OBS_SEL <- 20L; MAX_RETENUS <- 5L

cat("\n[1/5] La cible agregee\n")
va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, date, g)
poids <- lire_csv(file.path(DOSSIER_RESULTATS, "06_poids.csv")) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::transmute(branche,
                   cible = fin_trimestre(debut_trimestre(date) %m+% months(3)), w)

# L'agregat REALISE, construit exactement comme a l'etape 6 : indice de
# Laspeyres, poids en prix courants du trimestre precedent.
agregat <- va %>%
  dplyr::inner_join(poids, by = c("branche", "date" = "cible")) %>%
  dplyr::group_by(date) %>% dplyr::filter(dplyr::n() == length(TOUTES_BRANCHES)) %>%
  dplyr::summarise(g = log(sum(w * exp(g))), .groups = "drop") %>%
  dplyr::arrange(date)
cat(sprintf("      %d trimestres, de %s a %s | ecart-type %.2f pt\n",
            nrow(agregat), date_vers_trimestre(min(agregat$date)),
            date_vers_trimestre(max(agregat$date)), 100 * stats::sd(agregat$g)))

# Avant 2014 il n'y a pas de poids en prix courants. L'historique necessaire a
# l'estimation est donc complete par l'agregat en VOLUME, dont l'ecart a ete
# mesure a 0,047 point de croissance en mediane. Il sert UNIQUEMENT d'historique
# d'estimation, jamais de realisation evaluee.
hist_volume <- charger_va() %>% dplyr::group_by(date) %>%
  dplyr::summarise(t = sum(va), .groups = "drop") %>% dplyr::arrange(date) %>%
  dplyr::mutate(g = c(NA_real_, diff(log(t)))) %>%
  dplyr::filter(!is.na(g)) %>% dplyr::select(date, g)
cible_agregee <- hist_volume %>%
  dplyr::left_join(agregat %>% dplyr::rename(g_pc = g), by = "date") %>%
  dplyr::mutate(g = dplyr::coalesce(g_pc, g)) %>% dplyr::select(date, g)

origines <- agregat$date[agregat$date >= PREMIERE_CIBLE]
cat(sprintf("      %d origines evaluees\n", length(origines)))

# ============================================================================
# 2) LA PASSERELLE DIRECTE
# ============================================================================
cat("\n[2/5] Passerelle sur l'agregat\n")
couverture <- charger_couverture()
ind <- charger_indicateurs(branches = couverture$couvertes)
longueur <- ind %>% dplyr::count(id_serie, name = "n")
ind <- ind %>% dplyr::filter(id_serie %in% longueur$id_serie[longueur$n >= 36L])
meta <- charger_metadonnees() %>% dplyr::select(id_serie, agregation, transformation)
cat(sprintf("      %d indicateurs candidats, toutes branches confondues\n",
            dplyr::n_distinct(ind$id_serie)))

traiter <- function(cible) {
  info <- information_set_intra(ind, cible, scenario = "M3") %>%
    dplyr::filter(!(frequence == "trimestriel" &
                      date >= debut_trimestre(cible) & date <= fin_trimestre(cible)))
  mois_T <- mois_du_trimestre(cible)
  mens <- info %>% dplyr::filter(frequence == "mensuel") %>%
    dplyr::group_by(id_serie, indicateur) %>%
    dplyr::group_modify(function(g, cle) {
      mq <- as.Date(setdiff(mois_T, g$date), origin = "1970-01-01")
      if (length(mq) == 0L) return(dplyr::select(g, date, valeur))
      r <- prevoir_mois_manquants(g$date, g$valeur, mq)
      if (is.null(r)) return(dplyr::select(g, date, valeur))
      dplyr::select(r, date, valeur)
    }) %>% dplyr::ungroup()
  bl <- dplyr::bind_rows(
    mens %>% dplyr::mutate(frequence = "mensuel", branche = "agregat") %>%
      dplyr::select(id_serie, branche, indicateur, frequence, date, valeur),
    info %>% dplyr::filter(frequence == "trimestriel") %>%
      dplyr::mutate(branche = "agregat") %>%
      dplyr::select(id_serie, branche, indicateur, frequence, date, valeur)) %>%
    dplyr::left_join(meta, by = "id_serie")
  trim <- indicateurs_trimestriels(bl, tolerant = TRUE)
  d <- trim %>% dplyr::group_by(id_serie) %>%
    dplyr::summarise(n = sum(!is.na(x)), .groups = "drop")
  x_b <- trim %>%
    dplyr::filter(id_serie %in% d$id_serie[d$n >= MIN_OBS_SEL], !is.na(x)) %>%
    dplyr::select(id_serie, date, x)
  if (nrow(x_b) == 0L) return(NULL)
  x_l <- x_b %>% tidyr::pivot_wider(names_from = id_serie, values_from = x)

  g_ag <- cible_agregee %>% dplyr::select(date, g)
  s <- selectionner_disponible(g_ag, x_b, x_l, cible, SEUIL_R, SEUIL_P,
                               MIN_OBS_SEL, MAX_RETENUS)
  r <- if (!is.null(s)) estimer_bridge_ar(g_ag, x_l, s$ids, cible, avec_ar = TRUE)
       else NULL
  tibble::tibble(origine = cible,
                 n_retenus = if (is.null(s)) 0L else length(s$ids),
                 directe = if (is.null(r)) NA_real_ else r$prevision,
                 derniere_obs = if (is.null(r)) as.Date(NA) else r$derniere_obs,
                 reel = agregat$g[agregat$date == cible])
}

t0 <- Sys.time()
n_coeurs <- max(1L, min(3L, parallel::detectCores() - 1L))
cl <- parallel::makeCluster(n_coeurs)
rep_travail <- getwd()
parallel::clusterExport(cl, "rep_travail", envir = environment())
parallel::clusterEvalQ(cl, {
  setwd(rep_travail)
  suppressMessages({
    source("R/00_setup.R"); source("R/fonctions/information_set.R")
    source("R/fonctions/transformations.R"); source("R/fonctions/donnees.R")
    source("R/fonctions/passerelle.R"); source("R/fonctions/kalman.R")
  })
  NULL
})
parallel::clusterExport(cl, c("ind", "meta", "agregat", "cible_agregee", "origines",
                              "SEUIL_R", "SEUIL_P", "MIN_OBS_SEL", "MAX_RETENUS",
                              "traiter"), envir = environment())
directes <- dplyr::bind_rows(parallel::parLapply(cl, seq_along(origines),
                                                 function(i) traiter(origines[i])))
parallel::stopCluster(cl)
cat(sprintf("      %s | %d previsions sur %d\n",
            format(round(difftime(Sys.time(), t0, units = "mins"), 1)),
            sum(!is.na(directes$directe)), nrow(directes)))
stopifnot("[ANTI-LOOK-AHEAD]" = all(directes$derniere_obs < directes$origine, na.rm = TRUE))
cat("      controle : aucune estimation ne contient sa propre cible\n")
cat(sprintf("      indicateurs retenus : mediane %d\n",
            stats::median(directes$n_retenus)))
ecrire_csv(directes, chemin_res("previsions_directes.csv"))

# ============================================================================
# 3) COMPARAISON
# ============================================================================
cat("\n[3/5] Directe contre indirecte\n")
bench <- lire_csv(file.path(DOSSIER_RESULTATS, "13_previsions_benchmarks.csv")) %>%
  dplyr::mutate(origine = as.Date(origine))
comp <- bench %>%
  dplyr::select(origine, trimestre, reel,
                indirecte = `Systeme complet`,
                bvar_indirect = `BVAR agrege`, ar2 = `AR(2)`) %>%
  dplyr::left_join(directes %>% dplyr::select(origine, directe), by = "origine")

# combinaison des deux voies : moyenne simple, puis poids optimise recursivement
comp <- comp %>% dplyr::arrange(origine)
comp$mixte <- NA_real_
for (i in seq_len(nrow(comp))) {
  h <- comp[seq_len(i - 1L), ]
  h <- h[!is.na(h$directe) & !is.na(h$indirecte) & !is.na(h$reel), ]
  if (is.na(comp$directe[i]) || is.na(comp$indirecte[i])) {
    comp$mixte[i] <- dplyr::coalesce(comp$indirecte[i], comp$directe[i])
  } else if (nrow(h) < 8L) {
    comp$mixte[i] <- 0.5 * comp$directe[i] + 0.5 * comp$indirecte[i]
  } else {
    pd <- poids_combinaison(tibble::tibble(reel = h$reel, bvar = h$indirecte,
                                           bridge = h$directe), min_obs = 8L)
    comp$mixte[i] <- pd$delta * comp$indirecte[i] + (1 - pd$delta) * comp$directe[i]
  }
}

evaluer <- function(d, lab) {
  d %>% tidyr::pivot_longer(c(indirecte, directe, mixte, bvar_indirect, ar2),
                            names_to = "voie", values_to = "prev") %>%
    dplyr::filter(!is.na(prev), !is.na(reel)) %>%
    dplyr::group_by(voie) %>%
    dplyr::summarise(n = dplyr::n(), MAE = mean(abs(reel - prev)),
                     RMSFE = sqrt(mean((reel - prev)^2)),
                     ratio = sqrt(mean((reel - prev)^2)) / stats::sd(reel),
                     correl = suppressWarnings(stats::cor(prev, reel)),
                     .groups = "drop") %>%
    dplyr::mutate(periode = lab) %>% dplyr::arrange(RMSFE)
}
comparaison <- dplyr::bind_rows(
  evaluer(comp, "toutes origines"),
  evaluer(comp %>% dplyr::filter(lubridate::year(origine) == 2020), "2020"),
  evaluer(comp %>% dplyr::filter(lubridate::year(origine) != 2020), "hors 2020"))
ecrire_csv(comparaison, chemin_res("comparaison.csv"))

etiq <- c(indirecte = "Indirecte (16 branches agregees)",
          directe = "Directe (passerelle sur l'agregat)",
          mixte = "Combinaison des deux voies",
          bvar_indirect = "BVAR agrege", ar2 = "AR(2)")
for (per in c("toutes origines", "hors 2020", "2020")) {
  cat(sprintf("\n      --- %s ---\n", per))
  print(as.data.frame(comparaison %>% dplyr::filter(periode == per) %>%
    dplyr::transmute(voie = etiq[voie], n,
                     `MAE (pt)` = round(100 * MAE, 3),
                     `RMSFE (pt)` = round(100 * RMSFE, 3),
                     ratio = round(ratio, 3),
                     `correl.` = round(correl, 2))), row.names = FALSE)
}

# ============================================================================
# 4) L'ECART EST-IL SIGNIFICATIF ?
# ============================================================================
cat("\n[4/5] Diebold-Mariano\n")
ok <- comp %>% dplyr::filter(!is.na(directe), !is.na(indirecte), !is.na(reel))
paires <- list(c("indirecte", "directe"), c("indirecte", "mixte"),
               c("directe", "mixte"))
dm <- purrr::map_dfr(paires, function(p) {
  ea <- ok$reel - ok[[p[1]]]; eb <- ok$reel - ok[[p[2]]]
  t <- tryCatch(forecast::dm.test(ea, eb, h = 1L, power = 2), error = function(e) NULL)
  tibble::tibble(reference = etiq[p[1]], alternative = etiq[p[2]], n = nrow(ok),
                 RMSFE_ref = sqrt(mean(ea^2)), RMSFE_alt = sqrt(mean(eb^2)),
                 p_value = if (is.null(t)) NA_real_ else unname(t$p.value))
}) %>% dplyr::mutate(verdict = dplyr::case_when(
  is.na(p_value) ~ "indecidable",
  p_value >= 0.10 ~ "non significatif",
  RMSFE_alt < RMSFE_ref ~ "alternative meilleure",
  TRUE ~ "reference meilleure"))
ecrire_csv(dm, chemin_res("dm_direct_indirect.csv"))
print(as.data.frame(dm %>% dplyr::transmute(
  reference = substr(reference, 1, 22), alternative = substr(alternative, 1, 26),
  `RMSFE ref` = round(100 * RMSFE_ref, 3), `RMSFE alt` = round(100 * RMSFE_alt, 3),
  p = round(p_value, 3), verdict)), row.names = FALSE)

# ============================================================================
# 5) FIGURES
# ============================================================================
cat("\n[5/5] Figures\n")
g1 <- comp %>%
  dplyr::select(origine, reel, indirecte, directe, mixte) %>%
  tidyr::pivot_longer(c(indirecte, directe, mixte), names_to = "voie",
                      values_to = "prev") %>%
  dplyr::mutate(voie = etiq[voie]) %>%
  ggplot2::ggplot(ggplot2::aes(origine)) +
  ggplot2::geom_line(ggplot2::aes(y = 100 * reel), linewidth = 0.5) +
  ggplot2::geom_line(ggplot2::aes(y = 100 * prev), colour = "#b03a2e",
                     linewidth = 0.5) +
  ggplot2::facet_wrap(~ voie, ncol = 1) +
  ggplot2::labs(x = NULL, y = "croissance trimestrielle (%)",
                title = "Prevoir la VA totale : par les branches ou directement",
                subtitle = "en noir le realise, en rouge la prevision")
ggplot2::ggsave(chemin_fig("voies.png"), g1, width = 10, height = 7, dpi = 150)

g2 <- comparaison %>% dplyr::filter(periode != "toutes origines") %>%
  dplyr::mutate(voie = etiq[voie]) %>%
  ggplot2::ggplot(ggplot2::aes(stats::reorder(voie, -RMSFE), 100 * RMSFE,
                               fill = periode)) +
  ggplot2::geom_col(position = "dodge", width = 0.7) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "RMSFE (points)", fill = NULL,
                title = "Les deux voies vers la valeur ajoutee totale")
ggplot2::ggsave(chemin_fig("comparaison.png"), g2, width = 10, height = 4.5, dpi = 150)

cat("      figures/16_voies.png\n      figures/16_comparaison.png\n")
cat("\nComparaison directe / indirecte terminee.\n")
