# Validation du comblement : on perce des trous ARTIFICIELS dans des series
# completes, on comble, et on compare a la valeur reellement observee.
# Deux etalons de comparaison, pour savoir si le Kalman vaut son cout :
#   - interpolation lineaire entre les deux mois voisins ;
#   - moyenne du mois calendaire (saisonnalite naive).
source("R/00_setup.R"); source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R"); source("R/fonctions/donnees.R")
source("R/fonctions/kalman.R")
set.seed(20260912)

cv <- charger_couverture(); BC <- cv$couvertes
ind <- charger_indicateurs(branches = BC, frequences = "mensuel")

# series COMPLETES et assez longues pour servir de banc d'essai
bancs <- ind %>%
  dplyr::group_by(id_serie, branche) %>%
  dplyr::arrange(date, .by_group = TRUE) %>%
  dplyr::summarise(n = dplyr::n(),
                   attendu = length(calendrier_mensuel(min(date), max(date))),
                   .groups = "drop") %>%
  dplyr::filter(n == attendu, n >= 72)
cat("series completes utilisables comme banc d'essai :", nrow(bancs), "\n")

essais <- purrr::map_dfr(utils::head(bancs$id_serie, 60), function(sid) {
  s <- ind %>% dplyr::filter(id_serie == sid) %>% dplyr::arrange(date)
  n <- nrow(s)
  # 8 trous isoles, tires au hasard hors des bords
  idx <- sort(sample(seq(13, n - 13), 8))
  idx <- idx[c(TRUE, diff(idx) > 3)]          # trous non adjacents
  troue <- s; troue$valeur[idx] <- NA
  obs <- troue %>% dplyr::filter(!is.na(valeur))

  k <- combler_trous_kalman(obs$date, obs$valeur, max(s$date))
  if (is.null(k)) return(NULL)
  vrai <- s$valeur[idx]; dates_trou <- s$date[idx]
  est_k <- k$valeur[match(dates_trou, k$date)]

  # etalon 1 : interpolation lineaire
  est_lin <- stats::approx(obs$date, obs$valeur, xout = dates_trou)$y
  # etalon 2 : moyenne du meme mois calendaire
  moy_mois <- obs %>% dplyr::mutate(m = lubridate::month(date)) %>%
    dplyr::group_by(m) %>% dplyr::summarise(mm = mean(valeur), .groups = "drop")
  est_sais <- moy_mois$mm[match(lubridate::month(dates_trou), moy_mois$m)]

  ech <- stats::sd(s$valeur)
  tibble::tibble(id_serie = sid, date = dates_trou, vrai = vrai,
                 kalman = est_k, lineaire = est_lin, saisonnier = est_sais,
                 echelle = ech)
})

essais <- essais %>% dplyr::filter(!is.na(kalman), echelle > 0)
cat("trous artificiels evalues :", nrow(essais), "sur",
    dplyr::n_distinct(essais$id_serie), "series\n\n")

resume <- essais %>%
  tidyr::pivot_longer(c(kalman, lineaire, saisonnier),
                      names_to = "methode", values_to = "estime") %>%
  dplyr::filter(!is.na(estime)) %>%
  dplyr::group_by(methode) %>%
  dplyr::summarise(
    n = dplyr::n(),
    `erreur / ecart-type` = round(sqrt(mean(((vrai - estime) / echelle)^2)), 3),
    `erreur abs. mediane` = round(stats::median(abs((vrai - estime) / echelle)), 3),
    `biais relatif` = round(mean((estime - vrai) / echelle), 3),
    .groups = "drop") %>%
  dplyr::arrange(`erreur / ecart-type`)
cat("=== QUALITE DU COMBLEMENT, rapportee a l'ecart-type de la serie ===\n")
print(as.data.frame(resume))

cat("\n=== part des trous ou le Kalman bat chaque etalon ===\n")
cmp <- essais %>% dplyr::filter(!is.na(lineaire), !is.na(saisonnier))
cat(sprintf("  contre interpolation lineaire : %.0f%%\n",
            100 * mean(abs(cmp$vrai - cmp$kalman) < abs(cmp$vrai - cmp$lineaire))))
cat(sprintf("  contre moyenne saisonniere    : %.0f%%\n",
            100 * mean(abs(cmp$vrai - cmp$kalman) < abs(cmp$vrai - cmp$saisonnier))))

cat("\n=== effet sur le TRIMESTRE, ce qui est le vrai enjeu ===\n")
# Un comblement mediocre au mois peut suffire au trimestre, puisque l'agregation
# moyenne les erreurs. C'est la seule question qui compte pour la passerelle.
trim <- essais %>%
  dplyr::mutate(trimestre = fin_trimestre(date)) %>%
  dplyr::group_by(id_serie, trimestre) %>%
  dplyr::summarise(err_k = abs(sum(vrai) - sum(kalman)) / (3 * dplyr::first(echelle)),
                   err_l = abs(sum(vrai) - sum(lineaire)) / (3 * dplyr::first(echelle)),
                   .groups = "drop")
cat(sprintf("  erreur mediane sur la somme trimestrielle, Kalman  : %.3f ecart-type\n",
            stats::median(trim$err_k, na.rm = TRUE)))
cat(sprintf("  erreur mediane sur la somme trimestrielle, lineaire: %.3f ecart-type\n",
            stats::median(trim$err_l, na.rm = TRUE)))
ecrire_csv(resume, file.path(DOSSIER_RESULTATS, "04c_validation_kalman.csv"))
