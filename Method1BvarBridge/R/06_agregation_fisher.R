# ============================================================================
# 06_agregation_fisher.R -- ETAPE 6 : agreger les branches en valeur ajoutee totale
# ============================================================================
# Plan de correction : etape 6, et phase 11 ("Poids des branches et
# agregation").
#
# CE QUE LE PLAN DEMANDE
#   "Le poids sectoriel ne doit pas provenir d'une periode future." Pour une
#   cible T, on utilise donc les poids du trimestre precedent :
#
#       VA_totale(T) = somme_j  w_{j,T-1} * VA_j(T)      avec somme_j w = 1
#
#   et il faut supprimer "l'utilisation de poids fixes provenant du dernier
#   trimestre de la base dans les backtests historiques".
#
# D'OU VIENNENT LES POIDS, ET POURQUOI 2014
#   Les poids doivent etre en PRIX COURANTS. Un indice de volume pondere par
#   des parts en volume n'a pas de sens economique : ce que l'on veut, c'est le
#   poids de chaque branche dans l'activite, et ce poids se mesure en valeur.
#
#   La serie de valeur ajoutee nominale commence au premier trimestre 2014. Le
#   premier trimestre pour lequel un poids w_{j,T-1} existe est donc T2-2014 --
#   et c'est la raison, posee des la phase 3, pour laquelle toutes les
#   evaluations du projet commencent la. Cette borne n'est pas un choix
#   statistique, c'est une contrainte de donnees.
#
# DEUX FORMULES D'AGREGATION, ET ELLES NE DONNENT PAS LA MEME CHOSE
#   Le plan ecrit l'agregation en NIVEAUX. Applique aux taux de croissance,
#   cela donne un indice de volume de Laspeyres :
#
#       g_total = log( somme_j w_{j,T-1} * exp(g_{j,T}) )                  (A)
#
#   La version linearisee, que l'on rencontre souvent, est :
#
#       g_total = somme_j w_{j,T-1} * g_{j,T}                              (B)
#
#   (B) est l'approximation au premier ordre de (A). L'ecart est un effet de
#   Jensen : exp etant convexe, (B) SOUS-ESTIME systematiquement (A), et
#   d'autant plus que les croissances de branche sont dispersees. Sur cet
#   echantillon le biais a ete mesure a -0,086 point par trimestre en phase 3,
#   toutes les erreurs allant dans le meme sens -- ce qui est la signature d'un
#   biais et non d'un bruit.
#
#   On retient donc (A), et l'on rapporte l'ecart avec (B) plutot que de le
#   passer sous silence.
#
# CE QUE L'ON AGREGE
#   La construction du nowcast (phase 12 du plan) assemble, pour chaque
#   branche, le meilleur modele disponible :
#     - branches couvertes par le vivier : combinaison BVAR + passerelle, au
#       scenario M3 (phase 13) ;
#     - branches non couvertes : BVAR seul, l'etape 5 ayant montre qu'il y bat
#       l'AR(4) prevu par le plan (ratio median 0,961 contre 1,14).
#   A defaut de combinaison pour une branche couverte a une origine donnee, on
#   retombe sur le BVAR : l'agregation exige les SEIZE branches, un agregat
#   partiel n'aurait aucun sens.
#
# LA LIMITE QUE LE PLAN DEMANDE DE DOCUMENTER
#   Les comptes sont en VOLUMES CHAINES NON ADDITIFS : la somme des valeurs
#   ajoutees de branche en prix chaines n'est pas la valeur ajoutee totale
#   chainee. Toute agregation de ces series est donc approximative, quelle que
#   soit la formule employee. On ne mesure pas ici la valeur ajoutee totale
#   publiee, mais un indice de volume agrege a partir des branches -- et c'est
#   le meme indice qui sert de realisation et de prevision, donc la comparaison
#   reste licite.
#
# SORTIES
#   resultats/06_poids.csv
#   resultats/06_agregat.csv
#   resultats/06_evaluation_agregat.csv
#   resultats/06_ecart_formules.csv
#   figures/06_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("06_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0("06_", x))

# ============================================================================
# 1) LES POIDS, EN PRIX COURANTS
# ============================================================================
cat("\n[1/6] Poids en prix courants\n")

# Les poids sont construits par R/06a_poids.R, etape distincte parce que 03e en
# a besoin avant que 06 ne puisse tourner (voir l'en-tete de 06a).
f_poids <- chemin_res("poids.csv")
if (!file.exists(f_poids)) source("R/06a_poids.R")
poids <- lire_csv(f_poids) %>% dplyr::mutate(date = as.Date(date))
stopifnot("Les poids ne somment pas a 1" =
            all(abs(tapply(poids$w, poids$date, sum) - 1) < 1e-10))
cat(sprintf("      %d trimestres de poids, de %s a %s\n",
            dplyr::n_distinct(poids$date),
            date_vers_trimestre(min(poids$date)),
            date_vers_trimestre(max(poids$date))))
# Le poids utilise pour la cible T est celui de T-1 : il est disponible avant T.
poids_decales <- poids %>%
  dplyr::mutate(origine = fin_trimestre(debut_trimestre(date) %m+% months(3))) %>%
  dplyr::select(branche, origine, w, date_du_poids = date)

# ============================================================================
# 2) LA CIBLE ET LES PREVISIONS PAR BRANCHE
# ============================================================================
cat("\n[2/6] Previsions par branche\n")
va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::select(branche, origine = date, reel = g)

# Previsions BVAR. On prend celles CORRIGEES par la regle d'instabilite
# (R/03e) si elles existent : une branche dont la variance a rompu y recoit sa
# moyenne recente au lieu de la prevision du BVAR, qui n'y ajoutait que du
# bruit. Voir R/fonctions/branches_instables.R.
f_corr <- file.path(DOSSIER_RESULTATS, "03e_previsions_corrigees.csv")
f_bvar <- if (file.exists(f_corr)) f_corr else
  file.path(DOSSIER_RESULTATS, "03_previsions_recursives.csv")
cat(sprintf("      previsions de branche : %s
", basename(f_bvar)))
bvar <- lire_csv(f_bvar) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(branche, origine, bvar = prevision)

f_intra <- file.path(DOSSIER_RESULTATS, "09_previsions_intra.csv")
comb <- if (file.exists(f_intra)) {
  lire_csv(f_intra) %>% dplyr::mutate(origine = as.Date(origine)) %>%
    dplyr::filter(scenario == "M3") %>%
    dplyr::select(branche, origine, combinee)
} else {
  cat("      ! phase 13 absente : le nowcast se reduira au BVAR\n")
  tibble::tibble(branche = character(0), origine = as.Date(character(0)),
                 combinee = numeric(0))
}

base <- va %>%
  dplyr::inner_join(poids_decales, by = c("branche", "origine")) %>%
  dplyr::left_join(bvar,  by = c("branche", "origine")) %>%
  dplyr::left_join(comb,  by = c("branche", "origine")) %>%
  dplyr::mutate(
    source = dplyr::case_when(!is.na(combinee) ~ "combinaison",
                              !is.na(bvar)     ~ "BVAR",
                              TRUE             ~ "aucune"),
    nowcast = dplyr::coalesce(combinee, bvar))

# L'agregation exige les SEIZE branches : un agregat partiel melangerait des
# perimetres differents d'un trimestre a l'autre et serait incomparable.
completes <- base %>% dplyr::group_by(origine) %>%
  dplyr::summarise(n = sum(!is.na(nowcast)), .groups = "drop") %>%
  dplyr::filter(n == length(TOUTES_BRANCHES))
base <- base %>% dplyr::filter(origine %in% completes$origine)
cat(sprintf("      %d origines completes sur %d\n",
            nrow(completes), dplyr::n_distinct(va$origine[va$origine >= min(base$origine)])))
cat("      provenance des previsions de branche :\n")
print(as.data.frame(base %>% dplyr::count(source, name = "previsions")),
      row.names = FALSE)

# ============================================================================
# 3) AGREGATION
# ============================================================================
cat("\n[3/6] Agregation\n")

#' Indice de volume de Laspeyres, formule (A) du plan : agregation en NIVEAUX.
agreger_niveau <- function(w, g) log(sum(w * exp(g)))
#' Version linearisee, formule (B) : approximation au premier ordre.
agreger_lineaire <- function(w, g) sum(w * g)

agregat <- base %>%
  dplyr::group_by(origine) %>%
  dplyr::summarise(
    reel_niveau     = agreger_niveau(w, reel),
    reel_lineaire   = agreger_lineaire(w, reel),
    nowcast_niveau  = agreger_niveau(w, nowcast),
    nowcast_lineaire = agreger_lineaire(w, nowcast),
    bvar_niveau     = if (all(!is.na(bvar))) agreger_niveau(w, bvar) else NA_real_,
    .groups = "drop") %>%
  dplyr::arrange(origine) %>%
  dplyr::mutate(trimestre = date_vers_trimestre(origine), .after = origine)
ecrire_csv(agregat, chemin_res("agregat.csv"))

ecart <- agregat %>%
  dplyr::mutate(ecart_reel = reel_niveau - reel_lineaire) %>%
  dplyr::summarise(
    ecart_moyen   = mean(ecart_reel),
    ecart_median  = stats::median(ecart_reel),
    ecart_max     = max(abs(ecart_reel)),
    part_positifs = mean(ecart_reel > 0))
ecrire_csv(ecart, chemin_res("ecart_formules.csv"))
cat(sprintf("      ecart (A) - (B) sur la realisation : %+.4f point en moyenne, %+.4f en mediane\n",
            100 * ecart$ecart_moyen, 100 * ecart$ecart_median))
cat(sprintf("      ecart maximal %.3f point | %.0f%% des ecarts sont positifs\n",
            100 * ecart$ecart_max, 100 * ecart$part_positifs))
cat("      (A) est l'indice de Laspeyres, (B) sa linearisation : l'ecart est un effet de Jensen\n")

# ============================================================================
# 4) EVALUATION DE L'AGREGAT
# ============================================================================
cat("\n[4/6] Evaluation\n")
# Etalons : la moyenne recursive de l'agregat et la marche aleatoire. Ils sont
# calcules sur l'agregat lui-meme, pas sur les branches : c'est la performance
# de l'agregat qui est jugee.
ev <- agregat %>%
  dplyr::mutate(
    moyenne = dplyr::lag(cumsum(reel_niveau) / seq_along(reel_niveau)),
    marche  = dplyr::lag(reel_niveau))

mesurer <- function(reel, prevu, lab) {
  ok <- !is.na(reel) & !is.na(prevu)
  if (sum(ok) < 4L) return(NULL)
  e <- reel[ok] - prevu[ok]
  tibble::tibble(modele = lab, n = sum(ok),
                 RMSFE = sqrt(mean(e^2)), MAE = mean(abs(e)), biais = mean(e),
                 sd_reel = stats::sd(reel[ok]),
                 ratio = sqrt(mean(e^2)) / stats::sd(reel[ok]),
                 correlation = suppressWarnings(stats::cor(prevu[ok], reel[ok])))
}
evaluation <- dplyr::bind_rows(
  mesurer(ev$reel_niveau, ev$nowcast_niveau, "Nowcast du projet"),
  mesurer(ev$reel_niveau, ev$bvar_niveau,    "BVAR seul, agrege"),
  mesurer(ev$reel_niveau, ev$moyenne,        "Moyenne recursive"),
  mesurer(ev$reel_niveau, ev$marche,         "Marche aleatoire"),
  mesurer(ev$reel_niveau, ev$nowcast_lineaire, "Nowcast, formule lineaire (B)"))
ecrire_csv(evaluation, chemin_res("evaluation_agregat.csv"))
cat("\n      --- agregat, toutes origines ---\n")
print(evaluation %>% dplyr::arrange(ratio) %>%
        dplyr::transmute(modele, n, `RMSFE (%)` = round(100 * RMSFE, 3),
                         `ratio` = round(ratio, 3),
                         `correl.` = round(correlation, 2),
                         `biais (pt)` = round(100 * biais, 3)), n = 10)

hors <- ev %>% dplyr::filter(lubridate::year(origine) != 2020)
evaluation_hors <- dplyr::bind_rows(
  mesurer(hors$reel_niveau, hors$nowcast_niveau, "Nowcast du projet"),
  mesurer(hors$reel_niveau, hors$bvar_niveau,    "BVAR seul, agrege"),
  mesurer(hors$reel_niveau, hors$moyenne,        "Moyenne recursive"))
cat("\n      --- hors 2020 ---\n")
print(evaluation_hors %>% dplyr::arrange(ratio) %>%
        dplyr::transmute(modele, n, `ratio` = round(ratio, 3),
                         `correl.` = round(correlation, 2)), n = 5)

# ============================================================================
# 5) CONTROLES
# ============================================================================
cat("\n[5/6] Controles\n")
stopifnot("[ANTI-LOOK-AHEAD] un poids provient du trimestre cible ou d'apres" =
            all(base$date_du_poids < base$origine))
cat("      les poids sont tous anterieurs a leur cible\n")
n_poids_distincts <- dplyr::n_distinct(base$date_du_poids)
cat(sprintf("      %d jeux de poids distincts utilises (un par origine) -- aucun poids fixe\n",
            n_poids_distincts))
stopifnot("Des poids fixes ont ete utilises" = n_poids_distincts > 1L)

# ============================================================================
# 6) FIGURES
# ============================================================================
cat("\n[6/6] Figures\n")
g1 <- agregat %>%
  ggplot2::ggplot(ggplot2::aes(origine)) +
  ggplot2::geom_line(ggplot2::aes(y = 100 * reel_niveau, colour = "Realise"),
                     linewidth = 0.55) +
  ggplot2::geom_line(ggplot2::aes(y = 100 * nowcast_niveau, colour = "Nowcast"),
                     linewidth = 0.55) +
  ggplot2::geom_line(ggplot2::aes(y = 100 * bvar_niveau, colour = "BVAR seul"),
                     linewidth = 0.4, linetype = "dashed") +
  ggplot2::labs(x = NULL, y = "croissance trimestrielle (%)", colour = NULL,
                title = "Valeur ajoutee totale : realise et nowcast",
                subtitle = "agregation en niveaux, poids en prix courants du trimestre precedent")
ggplot2::ggsave(chemin_fig("agregat.png"), g1, width = 10, height = 4.5, dpi = 150)

g2 <- poids %>%
  ggplot2::ggplot(ggplot2::aes(date, 100 * w, fill = branche)) +
  ggplot2::geom_area(colour = "white", linewidth = 0.1) +
  ggplot2::labs(x = NULL, y = "part dans la valeur ajoutee (%)", fill = NULL,
                title = "Poids des branches en prix courants",
                subtitle = "recalcules a chaque trimestre : aucun poids fixe dans le backtest") +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 7))
ggplot2::ggsave(chemin_fig("poids.png"), g2, width = 11, height = 5.5, dpi = 150)

g3 <- agregat %>%
  dplyr::mutate(ecart = 100 * (reel_niveau - reel_lineaire)) %>%
  ggplot2::ggplot(ggplot2::aes(origine, ecart)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
  ggplot2::geom_col(width = 70) +
  ggplot2::labs(x = NULL, y = "point de croissance",
                title = "Ce que coute la linearisation de l'agregation",
                subtitle = "indice de Laspeyres (A) moins sa version linearisee (B), sur la realisation")
ggplot2::ggsave(chemin_fig("ecart_formules.png"), g3, width = 10, height = 4, dpi = 150)

cat("      figures/06_agregat.png\n      figures/06_poids.png\n")
cat("      figures/06_ecart_formules.png\n")
cat("\nEtape 6 terminee.\n")
