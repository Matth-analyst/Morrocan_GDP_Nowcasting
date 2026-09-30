# ============================================================================
# 02_analyse_exploratoire.R -- PHASE 2 : transformations et analyse exploratoire
# ============================================================================
# Plan de correction : section 2 ("Phase 2") et section 32 ("Etape 2").
#
# OBJECTIF DE L'ETAPE
#   Rendre les transformations compatibles avec le backtest, et documenter la
#   base avant modelisation.
#
# DISTINCTION CENTRALE DE CETTE PHASE
#   Il faut separer deux choses que la version precedente melangeait :
#
#   (a) L'ANALYSE EXPLORATOIRE, faite sur l'echantillon complet. Elle est
#       legitime : elle sert a decrire la base et a justifier les choix de
#       specification dans le memoire. Statistiques descriptives, tests ADF,
#       correlations croisees, cartes de disponibilite.
#       CONDITION ABSOLUE : aucun de ses resultats ne doit devenir un parametre
#       du modele. Un test ADF sur 1998-2026 ne peut pas servir a decider,
#       trimestre par trimestre, comment transformer une serie -- la regle de
#       transformation vient des metadonnees (phase 1), fixees sur la nature
#       economique de la variable, pas sur un test plein echantillon.
#
#   (b) LES TRANSFORMATIONS UTILISEES PAR LE MODELE, qui doivent etre locales :
#       appliquees a l'origine T, elles doivent donner exactement le meme
#       resultat que la transformation plein echantillon tronquee en T.
#       C'est le cas de dlog, diff et niveau. Ce n'est PAS le cas d'une
#       standardisation, d'un lissage ou d'une imputation plein echantillon,
#       qui devront etre recalcules a chaque origine.
#       Le controle 3 ci-dessous verifie cette propriete sur les 434 series.
#
#   La version precedente calculait Δlog une fois pour toutes dans ce script
#   et sauvegardait "cibles_avec_dlog", relu ensuite par le backtest. Comme
#   Δlog est local, cela ne creait pas de fuite -- mais la construction
#   invitait a l'erreur, puisque rien n'empechait d'y ajouter une operation
#   non locale. Ici la transformation est une FONCTION appelee a la demande,
#   pas un fichier fige.
#
# SORTIES
#   resultats/02_stats_descriptives_va.csv
#   resultats/02_tests_stationnarite.csv
#   resultats/02_correlations_branches.csv
#   resultats/02_diagnostic_indicateurs.csv
#   resultats/02_controle_transformations.csv
#   figures/02_*.png
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")

suppressPackageStartupMessages(library(tseries))

PREFIXE <- "02"
chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0(PREFIXE, "_", x))
chemin_fig <- function(x) file.path(DOSSIER_FIGURES,   paste0(PREFIXE, "_", x))

cat("\n[1/6] Lecture de la base\n")
va          <- charger_va()
indicateurs <- charger_indicateurs()
metadonnees <- charger_metadonnees()
couverture  <- charger_couverture()

cat(sprintf("      cible : %d branches, %d obs. | indicateurs : %d series, %d obs.\n",
            dplyr::n_distinct(va$branche), nrow(va),
            dplyr::n_distinct(indicateurs$id_serie), nrow(indicateurs)))

# ============================================================================
# 1) TRANSFORMATION DE LA CIBLE
# ============================================================================
# Les 16 VA sont des volumes strictement positifs : Δlog pour toutes.
# Convention d'echelle du projet : Δlog brut, pas 100·Δlog (cf.
# R/fonctions/transformations.R). Le passage en pourcentage se fait a
# l'affichage uniquement.

if (any(va$va <= 0)) {
  stop("VA negative ou nulle detectee : ",
       paste(unique(va$branche[va$va <= 0]), collapse = ", "), call. = FALSE)
}

va <- va %>%
  dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(dlog_va = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup()

cat("[2/6] Cible transformee en Δlog\n")

# ============================================================================
# 2) STATISTIQUES DESCRIPTIVES DE LA CIBLE
# ============================================================================
stats_va <- va %>%
  dplyr::group_by(branche) %>%
  dplyr::summarise(
    n_trimestres = dplyr::n(),
    debut        = min(date),
    fin          = max(date),
    va_moyenne   = mean(va),
    part_moyenne = NA_real_,                      # completee ci-dessous
    croissance_moy_pct = 100 * mean(dlog_va, na.rm = TRUE),
    volatilite_pct     = 100 * stats::sd(dlog_va, na.rm = TRUE),
    min_pct      = 100 * min(dlog_va, na.rm = TRUE),
    max_pct      = 100 * max(dlog_va, na.rm = TRUE),
    date_min     = date[which.min(replace(dlog_va, is.na(dlog_va), Inf))],
    date_max     = date[which.max(replace(dlog_va, is.na(dlog_va), -Inf))],
    .groups = "drop"
  )

# part moyenne de chaque branche dans la VA totale (descriptif seulement :
# les poids du modele seront ceux disponibles avant T, cf. phase 11)
parts <- va %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(part = va / sum(va)) %>%
  dplyr::group_by(branche) %>%
  dplyr::summarise(part_moyenne = mean(part), .groups = "drop")
stats_va <- stats_va %>%
  dplyr::select(-part_moyenne) %>%
  dplyr::left_join(parts, by = "branche") %>%
  dplyr::arrange(dplyr::desc(part_moyenne))

ecrire_csv(stats_va, chemin_res("stats_descriptives_va.csv"))

cat("[3/6] Statistiques descriptives\n")
print(stats_va %>%
        dplyr::transmute(branche, part_moyenne = round(100 * part_moyenne, 1),
                         croissance_moy_pct = round(croissance_moy_pct, 2),
                         volatilite_pct = round(volatilite_pct, 2),
                         min_pct = round(min_pct, 1), date_min), n = 20)

# ============================================================================
# 3) TESTS DE STATIONNARITE -- EXPLORATOIRE
# ============================================================================
# ANALYSE PLEIN ECHANTILLON, QUI NE PILOTE AUCUN CHOIX DU MODELE.
# Elle documente le choix de modeliser les taux de croissance plutot que les
# niveaux (Higgins 2014 ecrit toutes ses equations en Δlog). La regle de
# transformation, elle, vient des metadonnees de la phase 1.
#
# H0 du test de Dickey-Fuller augmente : racine unitaire (non stationnarite).
# p < 0.05 -> on rejette H0 -> serie compatible avec la stationnarite.

test_adf <- function(x, k = 4L) {
  x <- x[!is.na(x)]
  if (length(x) < 4L * k) return(NA_real_)
  suppressWarnings(tryCatch(tseries::adf.test(x, k = k)$p.value,
                            error = function(e) NA_real_))
}

stationnarite <- va %>%
  dplyr::group_by(branche) %>%
  dplyr::summarise(
    p_niveau = test_adf(va),
    p_dlog   = test_adf(dlog_va),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    stationnaire_niveau = p_niveau < 0.05,
    stationnaire_dlog   = p_dlog < 0.05
  ) %>%
  dplyr::arrange(dplyr::desc(stationnaire_dlog), p_dlog)

ecrire_csv(stationnarite, chemin_res("tests_stationnarite.csv"))

cat("\n[4/6] Tests ADF (exploratoire, plein echantillon)\n")
cat(sprintf("      stationnaires en niveau : %d/16 | en Δlog : %d/16\n",
            sum(stationnarite$stationnaire_niveau, na.rm = TRUE),
            sum(stationnarite$stationnaire_dlog, na.rm = TRUE)))
non_stat <- stationnarite$branche[!stationnarite$stationnaire_dlog %in% TRUE]
if (length(non_stat) > 0L) {
  cat(sprintf("      non stationnaires en Δlog : %s\n",
              paste(non_stat, collapse = ", ")))
}

# ============================================================================
# 4) CORRELATIONS CROISEES ENTRE BRANCHES
# ============================================================================
# Justifie l'interet d'un BVAR multivarie plutot que 16 AR independants
# (Banbura, Giannone & Reichlin 2010). Exploratoire : le BVAR de la phase 3
# sera reestime a chaque origine, ses correlations ne viennent pas d'ici.

mat <- va %>%
  dplyr::select(branche, date, dlog_va) %>%
  tidyr::pivot_wider(names_from = branche, values_from = dlog_va) %>%
  dplyr::arrange(date)
cor_mat <- stats::cor(mat[, TOUTES_BRANCHES], use = "pairwise.complete.obs")

cor_long <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
names(cor_long) <- c("branche_1", "branche_2", "correlation")
ecrire_csv(cor_long, chemin_res("correlations_branches.csv"))

paires <- cor_long %>%
  dplyr::filter(branche_1 < branche_2) %>%
  dplyr::arrange(dplyr::desc(abs(correlation)))
cat("\n      5 paires de branches les plus correlees (Δlog) :\n")
for (i in 1:5) {
  cat(sprintf("        %-28s %-28s r = %+.2f\n",
              paires$branche_1[i], paires$branche_2[i], paires$correlation[i]))
}

# ============================================================================
# 5) DIAGNOSTIC DES INDICATEURS
# ============================================================================
# Ni selection ni jugement de qualite : on decrit le jagged edge, on applique
# les regles de transformation de la phase 1, et on signale ce qui posera
# probleme en aval (series trop courtes, trop trouees, ou dont la regle de
# transformation est incompatible avec les valeurs observees).

indic_t <- appliquer_transformations(indicateurs)

diagnostic <- indic_t %>%
  dplyr::group_by(id_serie, branche, indicateur, frequence, unite,
                  agregation, transformation) %>%
  dplyr::summarise(
    n_obs        = dplyr::n(),
    debut        = min(date),
    fin          = max(date),
    n_transforme = sum(!is.na(valeur_t)),
    n_sauts      = sum(is.na(valeur_t)) - 1L,   # hors 1re obs., toujours NA
    valeur_min   = min(valeur, na.rm = TRUE),
    moyenne_t    = mean(valeur_t, na.rm = TRUE),
    volatilite_t = stats::sd(valeur_t, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(metadonnees %>% dplyr::select(id_serie, n_attendu, taux_manquant),
                   by = "id_serie") %>%
  dplyr::mutate(
    n_sauts = pmax(n_sauts, 0L),
    # trimestres exploitables : une serie mensuelle fournit au mieux un point
    # trimestriel tous les 3 mois
    trimestres_utiles = dplyr::if_else(frequence == "mensuel",
                                       n_transforme %/% 3L, n_transforme)
  ) %>%
  dplyr::arrange(branche, frequence, indicateur)

ecrire_csv(diagnostic, chemin_res("diagnostic_indicateurs.csv"))

cat("\n[5/6] Diagnostic des indicateurs\n")
cat(sprintf("      %d series transformees | %d observations transformees\n",
            dplyr::n_distinct(diagnostic$id_serie), sum(diagnostic$n_transforme)))
cat(sprintf("      series a plus de 20%% de trous : %d\n",
            sum(diagnostic$taux_manquant > 0.20, na.rm = TRUE)))
cat(sprintf("      series offrant moins de 20 trimestres exploitables : %d\n",
            sum(diagnostic$trimestres_utiles < 20L)))

cat("\n      Trimestres exploitables par branche (mediane sur les series) :\n")
print(diagnostic %>%
        dplyr::group_by(branche) %>%
        dplyr::summarise(n_series = dplyr::n(),
                         median_trim = stats::median(trimestres_utiles),
                         min_trim = min(trimestres_utiles),
                         .groups = "drop") %>%
        dplyr::arrange(median_trim), n = 20)

# ============================================================================
# 6) CONTROLE 3 -- LES TRANSFORMATIONS SONT-ELLES LOCALES ?
# ============================================================================
# Section 30 du plan. Pour chaque serie : transformer puis tronquer en T doit
# donner exactement le meme resultat que tronquer en T puis transformer, a
# plusieurs origines T. Si un jour quelqu'un glisse un lissage ou une
# standardisation plein echantillon dans la chaine, ce controle echoue et le
# script s'arrete.

cat("\n[6/6] Controle anti-look-ahead des transformations\n")

controle <- indicateurs %>%
  dplyr::group_by(id_serie, branche, indicateur, frequence, transformation) %>%
  dplyr::summarise(
    locale = verifier_transformation_locale(valeur, date,
                                            dplyr::first(transformation),
                                            dplyr::first(frequence)),
    .groups = "drop"
  )

controle_va <- va %>%
  dplyr::group_by(branche) %>%
  dplyr::summarise(
    locale = verifier_transformation_locale(va, date, "dlog", "trimestriel"),
    .groups = "drop"
  ) %>%
  dplyr::transmute(id_serie = paste("CIBLE", branche, sep = " :: "),
                   branche, indicateur = "VA", frequence = "trimestriel",
                   transformation = "dlog", locale)

controle <- dplyr::bind_rows(controle_va, controle)
ecrire_csv(controle, chemin_res("controle_transformations.csv"))

echecs <- controle %>% dplyr::filter(!locale)
if (nrow(echecs) > 0L) {
  print(as.data.frame(echecs), row.names = FALSE)
  stop(nrow(echecs), " serie(s) dont la transformation n'est pas locale : ",
       "elles introduiraient de l'information future dans le backtest.",
       call. = FALSE)
}
cat(sprintf("      %d series verifiees (16 VA + %d indicateurs) : transformations locales\n",
            nrow(controle), nrow(controle) - 16L))

# --- Contre-exemple pedagogique ---------------------------------------------
# On montre qu'une standardisation plein echantillon, elle, echoue au meme
# controle : c'est exactement le type d'operation que la section 2 du plan
# interdit, et la raison d'etre de standardiser_recursif().
demo <- va %>% dplyr::filter(branche == "Construction") %>% dplyr::arrange(date)
x <- demo$dlog_va[!is.na(demo$dlog_va)]
z_plein <- as.numeric(scale(x))                   # moyenne/ecart-type du futur inclus
z_tronque <- as.numeric(scale(x[1:40]))           # a l'origine T = 40e trimestre
ecart <- max(abs(z_plein[1:40] - z_tronque))
cat(sprintf("      contre-exemple : scale() plein echantillon vs tronque a T=40,\n"))
cat(sprintf("                       ecart max sur les memes 40 points = %.3f ecart-type\n", ecart))
cat("                       -> une standardisation doit etre recalculee a chaque origine\n")
cat("                          (standardiser_recursif, R/fonctions/transformations.R)\n")

# ============================================================================
# FIGURES
# ============================================================================
cat("\n[figures] Generation\n")

p1 <- ggplot(va, aes(date, va)) +
  geom_line(color = "#2E74B5", linewidth = 0.45) +
  facet_wrap(~ branche, scales = "free_y", ncol = 4) +
  labs(title = "Valeur ajoutée trimestrielle par branche",
       subtitle = "Base 2014, prix chaînés, rétropolée — source HCP",
       x = NULL, y = "Mdh") +
  theme(strip.text = element_text(size = 7), axis.text = element_text(size = 6))
ggsave(chemin_fig("va_niveaux.png"), p1, width = 12, height = 8, dpi = 150)

p2 <- ggplot(va %>% dplyr::filter(!is.na(dlog_va)), aes(date, 100 * dlog_va)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_line(color = "#C55A11", linewidth = 0.4) +
  facet_wrap(~ branche, scales = "free_y", ncol = 4) +
  labs(title = "Taux de croissance trimestriel par branche",
       subtitle = "Δlog exprimé en pourcentage",
       x = NULL, y = "%") +
  theme(strip.text = element_text(size = 7), axis.text = element_text(size = 6))
ggsave(chemin_fig("va_croissance.png"), p2, width = 12, height = 8, dpi = 150)

ordre <- stats::hclust(stats::as.dist(1 - cor_mat))$order
cor_fig <- cor_long %>%
  dplyr::mutate(branche_1 = factor(branche_1, levels = TOUTES_BRANCHES[ordre]),
                branche_2 = factor(branche_2, levels = TOUTES_BRANCHES[ordre]))
p3 <- ggplot(cor_fig, aes(branche_1, branche_2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "#C55A11", mid = "white", high = "#2E74B5",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Corrélation des taux de croissance entre branches",
       subtitle = "Δlog trimestriel, branches regroupées par similarité",
       x = NULL, y = NULL, fill = "r") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7))
ggsave(chemin_fig("correlations_branches.png"), p3, width = 9, height = 8, dpi = 150)

# Carte de disponibilite : le jagged edge, serie par serie
dispo <- indicateurs %>%
  dplyr::mutate(annee = lubridate::year(date)) %>%
  dplyr::distinct(branche, id_serie, frequence, annee) %>%
  dplyr::group_by(branche, frequence, annee) %>%
  dplyr::summarise(n_series = dplyr::n_distinct(id_serie), .groups = "drop")
p4 <- ggplot(dispo, aes(annee, n_series, fill = frequence)) +
  geom_col() +
  facet_wrap(~ branche, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c(mensuel = "#2E74B5", trimestriel = "#BFBFBF")) +
  labs(title = "Nombre d'indicateurs disponibles, année par année",
       subtitle = "Le vivier s'étoffe dans le temps : la sélection récursive n'aura pas le même choix selon l'origine",
       x = NULL, y = "séries disponibles", fill = NULL) +
  theme(strip.text = element_text(size = 7), axis.text = element_text(size = 6))
ggsave(chemin_fig("disponibilite_indicateurs.png"), p4, width = 12, height = 8, dpi = 150)

p5 <- ggplot(diagnostic, aes(taux_manquant, trimestres_utiles, color = frequence)) +
  geom_point(alpha = 0.6, size = 1.3) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c(mensuel = "#2E74B5", trimestriel = "#C55A11")) +
  labs(title = "Profondeur d'historique contre taux de trous, par série",
       subtitle = "Ligne : 20 trimestres exploitables — en dessous, une bridge equation est difficilement estimable",
       x = "part de périodes manquantes", y = "trimestres exploitables", color = NULL)
ggsave(chemin_fig("diagnostic_series.png"), p5, width = 9, height = 6, dpi = 150)

# --- Stationnarite : p-values ADF, niveau contre Δlog -----------------------
adf_fig <- stationnarite %>%
  tidyr::pivot_longer(c(p_niveau, p_dlog), names_to = "serie", values_to = "p") %>%
  dplyr::mutate(serie = factor(serie, levels = c("p_niveau", "p_dlog"),
                               labels = c("niveau", "Δlog")),
                branche = factor(branche,
                                 levels = stationnarite$branche[order(stationnarite$p_niveau,
                                                                      decreasing = TRUE)]))
p6 <- ggplot(adf_fig, aes(p, branche, color = serie, shape = serie)) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "grey45") +
  geom_point(size = 2.4) +
  scale_color_manual(values = c(niveau = "#8C8C8C", `Δlog` = "#2E74B5")) +
  scale_shape_manual(values = c(niveau = 1, `Δlog` = 16)) +
  labs(title = "Test de Dickey-Fuller augmenté : niveau contre taux de croissance",
       subtitle = "H0 = racine unitaire. À gauche du trait (p < 0,05), H0 est rejetée : série compatible avec la stationnarité",
       x = "p-value", y = NULL, color = NULL, shape = NULL) +
  theme(axis.text.y = element_text(size = 8))
ggsave(chemin_fig("stationnarite.png"), p6, width = 9, height = 6, dpi = 150)

# --- Distribution des taux de croissance, par branche -----------------------
ordre_vol <- stats_va$branche[order(stats_va$volatilite_pct)]
p7 <- ggplot(va %>% dplyr::filter(!is.na(dlog_va)) %>%
               dplyr::mutate(branche = factor(branche, levels = ordre_vol)),
             aes(100 * dlog_va, branche)) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_boxplot(outlier.size = 0.9, outlier.color = "#C55A11",
               fill = "grey93", color = "grey35", linewidth = 0.35) +
  labs(title = "Distribution des taux de croissance trimestriels, par branche",
       subtitle = "Branches ordonnées par volatilité croissante ; les points isolés sont les trimestres extrêmes",
       x = "Δlog (%)", y = NULL) +
  theme(axis.text.y = element_text(size = 8))
ggsave(chemin_fig("distribution_croissance.png"), p7, width = 9, height = 6, dpi = 150)

# --- Regles de transformation retenues --------------------------------------
regles <- metadonnees %>%
  dplyr::count(agregation, transformation, name = "n_series") %>%
  dplyr::mutate(agregation = factor(agregation, levels = c("sum", "mean", "last")))
p8 <- ggplot(regles, aes(agregation, n_series, fill = transformation)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = n_series), position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  scale_fill_manual(values = c(dlog = "#2E74B5", diff = "#8C8C8C", niveau = "#C55A11")) +
  labs(title = "Règles d'agrégation et de transformation des 434 indicateurs",
       subtitle = "Agrégation : passage de 3 mois à 1 trimestre. Transformation : mise en forme stationnaire",
       x = NULL, y = "nombre de séries", fill = NULL)
ggsave(chemin_fig("regles_transformation.png"), p8, width = 8, height = 5, dpi = 150)

# --- Le ragged edge, vu de pres ---------------------------------------------
# Une branche a fort jagged edge : chaque ligne est une serie, chaque case un
# mois observe. C'est la forme de bord irreguliere que la phase 4 devra traiter.
b_demo <- "Hébergement-restauration"
edge <- indicateurs %>%
  dplyr::filter(branche == b_demo, frequence == "mensuel",
                date >= as.Date("2012-01-01")) %>%
  dplyr::mutate(indicateur = factor(indicateur,
                                    levels = names(sort(tapply(date, indicateur, min)))))
p9 <- ggplot(edge, aes(date, indicateur)) +
  geom_tile(fill = "#2E74B5", height = 0.75) +
  labs(title = sprintf("Calendrier d'observation des indicateurs mensuels — %s", b_demo),
       subtitle = "Une case = un mois observé. Les blancs sont les trous réels des séries, et les bords droits inégaux forment le ragged edge",
       x = NULL, y = NULL) +
  theme(axis.text.y = element_text(size = 6.5), panel.grid.major.y = element_blank())
ggsave(chemin_fig("ragged_edge.png"), p9, width = 10, height = 7, dpi = 150)

# --- Contre-exemple : standardisation plein echantillon vs recursive --------
# Piece centrale de la phase 2 : montrer visuellement ce qu'une operation non
# locale fait entrer d'information future dans une serie historique.
demo_fig <- va %>% dplyr::filter(branche == "Construction", !is.na(dlog_va)) %>%
  dplyr::arrange(date)
z_ref <- as.numeric(scale(demo_fig$dlog_va))
z_rec <- standardiser_recursif(demo_fig$dlog_va)
z_tr  <- rep(NA_real_, nrow(demo_fig))
z_tr[1:40] <- as.numeric(scale(demo_fig$dlog_va[1:40]))

comp <- dplyr::bind_rows(
  tibble::tibble(date = demo_fig$date, z = z_ref,
                 methode = "scale() sur tout l'échantillon"),
  tibble::tibble(date = demo_fig$date, z = z_tr,
                 methode = "scale() tronqué à l'origine T = 40"),
  tibble::tibble(date = demo_fig$date, z = z_rec,
                 methode = "standardisation récursive")
) %>% dplyr::filter(!is.na(z))

p10 <- ggplot(comp, aes(date, z, color = methode, linetype = methode)) +
  geom_vline(xintercept = demo_fig$date[40], color = "grey55", linewidth = 0.4) +
  annotate("text", x = demo_fig$date[40], y = max(comp$z, na.rm = TRUE),
           label = "  origine T = 40", hjust = 0, size = 3, color = "grey35") +
  geom_line(linewidth = 0.55) +
  scale_color_manual(values = c("scale() sur tout l'échantillon" = "#C55A11",
                                "scale() tronqué à l'origine T = 40" = "#2E74B5",
                                "standardisation récursive" = "grey35")) +
  scale_linetype_manual(values = c("scale() sur tout l'échantillon" = "solid",
                                   "scale() tronqué à l'origine T = 40" = "solid",
                                   "standardisation récursive" = "22")) +
  labs(title = "Pourquoi une standardisation doit être récursive — branche Construction",
       subtitle = "Les deux courbes pleines décrivent les mêmes 40 trimestres ; elles diffèrent parce que l'une connaît le futur",
       x = NULL, y = "écarts-types", color = NULL, linetype = NULL) +
  guides(color = guide_legend(nrow = 3), linetype = guide_legend(nrow = 3))
ggsave(chemin_fig("standardisation_lookahead.png"), p10, width = 9.5, height = 6, dpi = 150)

cat("\n=== PHASE 2 TERMINEE ===\n")
cat("Resultats :\n")
for (f in list.files(DOSSIER_RESULTATS, pattern = paste0("^", PREFIXE), full.names = TRUE)) {
  cat("  ", f, "\n")
}
cat("Figures :\n")
for (f in list.files(DOSSIER_FIGURES, pattern = paste0("^", PREFIXE), full.names = TRUE)) {
  cat("  ", f, "\n")
}
cat("\nLes transformations ne sont PAS figees dans un fichier : elles sont appliquees\n")
cat("a la demande par appliquer_transformations() / transformer_serie(), afin que\n")
cat("chaque origine du backtest les recalcule sur sa propre information.\n")
