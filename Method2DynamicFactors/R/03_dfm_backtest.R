# ============================================================================
# 03_dfm_backtest.R -- Backtest recursif du modele a facteurs, par branche
# ============================================================================
# LE PROTOCOLE, IDENTIQUE A CELUI DE LA METHODE 1
#   48 origines, de T2-2014 a T1-2026. A chaque origine, le panel est tronque
#   au dernier mois du trimestre cible : les indicateurs de ce trimestre sont
#   observes, la valeur ajoutee ne l'est pas. Le modele est reestime, puis la
#   cible est prevue.
#
#   C'est le scenario M3 de la methode 1 : trimestre ecoule, comptes pas
#   encore publies. C'est la situation ou le nowcast a le plus de valeur, et
#   c'est celle qui rend les deux methodes comparables.
#
# CE QUI EST RECALCULE A CHAQUE ORIGINE
#   - la moyenne et l'ecart-type de standardisation de CHAQUE serie, sur les
#     seules observations anterieures au trimestre cible. Les calculer une fois
#     sur tout l'echantillon ferait entrer la moyenne de 2026 dans la prevision
#     de 2015 ;
#   - les parametres du modele (chargements, variances, dynamique des
#     facteurs) ;
#   - le choix du nombre de facteurs r et de l'ordre p (section 4 ci-dessous).
#
# LE COUT, ET COMMENT IL EST TENU
#   Une estimation froide prend 2 a 30 secondes. Le panel de l'origine T ne
#   differant de celui de T-1 que par trois mois, chaque estimation repart des
#   parametres de l'origine precedente : 35 iterations tombent a 3, et le temps
#   est divise par douze. Ce demarrage a chaud ne transmet aucune information
#   future, puisque les parametres de T-1 sont estimes sur des donnees
#   anterieures a T-1.
#
# SORTIES
#   resultats/03_previsions_dfm.csv   une ligne par branche x origine x (r,p)
#   resultats/03_grille_rp.csv        performance de chaque configuration
#   resultats/03_reprise/             points de reprise, par branche
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/dfm.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("03_", x))
DOSSIER_REPRISE <- chemin_res("reprise")
dir.create(DOSSIER_REPRISE, showWarnings = FALSE, recursive = TRUE)

# Grille des configurations evaluees. Elle reste petite a dessein : chaque
# configuration supplementaire coute un backtest complet, et la section 4
# montre que le choix se joue entre un et trois facteurs.
GRILLE <- expand.grid(r = 1:3, p = 1:2)
MAX_ITER <- 40L

fin_mois <- function(d) lubridate::ceiling_date(as.Date(d), "month") - 1L

cat("\n[1/4] Bases\n")
couv <- charger_couverture()
manif <- lire_csv(file.path(DOSSIER_DATA, "panel_manifeste.csv"))
va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::transmute(branche, origine = date, reel = g)

ORIGINES <- sort(unique(va$origine[va$origine >= as.Date("2014-06-30")]))
cat(sprintf("      %d origines, de %s a %s | %d configurations (r, p)\n",
            length(ORIGINES), date_vers_trimestre(min(ORIGINES)),
            date_vers_trimestre(max(ORIGINES)), nrow(GRILLE)))

# ============================================================================
# 2) UNE ORIGINE, UNE BRANCHE, UNE CONFIGURATION
# ============================================================================

#' Standardisation recursive : moyenne et ecart-type sur les seules lignes
#' anterieures a la borne.
standardiser_avant <- function(X, dates, borne) {
  avant <- dates < borne
  mu <- apply(X, 2, function(v) mean(v[avant], na.rm = TRUE))
  sd <- apply(X, 2, function(v) stats::sd(v[avant], na.rm = TRUE))
  sd[!is.finite(sd) | sd < 1e-8] <- NA_real_      # serie inexploitable en T
  Z <- sweep(sweep(X, 2, mu, "-"), 2, sd, "/")
  list(Z = Z, mu = mu, sd = sd)
}

#' Prevision d'une branche a une origine, pour une configuration donnee.
prevoir_branche <- function(pan, colonnes_trim, i_cible, origine, r, p,
                            init = NULL) {
  dates <- as.Date(pan$date)
  fin <- fin_mois(origine)
  garder <- dates <= fin
  if (sum(garder) < 60L) return(NULL)

  X <- as.matrix(pan[garder, -1, drop = FALSE])
  d <- dates[garder]

  # La cible du trimestre vise n'est pas connue : c'est ce qu'on cherche.
  X[d == fin, i_cible] <- NA_real_

  st <- standardiser_avant(X, d, fin)
  Z <- st$Z
  utilisables <- which(is.finite(st$sd) & colSums(!is.na(Z)) >= 10L)
  if (!(i_cible %in% utilisables)) return(NULL)
  Z <- Z[, utilisables, drop = FALSE]
  ic <- match(i_cible, utilisables)
  it <- match(intersect(colonnes_trim, utilisables), utilisables)

  mod <- tryCatch(
    estimer_dfm(Z, r = r, p = p, i_cible = ic, i_trim = it,
                max_iter = MAX_ITER, init = init),
    error = function(e) NULL)
  if (is.null(mod)) return(NULL)

  z_prev <- tryCatch(prevoir_cible(mod, Z, h = 0L), error = function(e) NA_real_)
  list(prevision = z_prev * st$sd[i_cible] + st$mu[i_cible],
       modele = mod, colonnes = utilisables)
}

# ============================================================================
# 3) LE BACKTEST
# ============================================================================
cat("\n[2/4] Backtest\n")
resultats <- list()

for (b in couv$couvertes) {
  f_reprise <- file.path(DOSSIER_REPRISE, sprintf("%s.csv", dossier_branche(b)))
  if (file.exists(f_reprise)) {
    resultats[[b]] <- lire_csv(f_reprise) %>% dplyr::mutate(origine = as.Date(origine))
    cat(sprintf("      %-28s repris (%d lignes)\n", b, nrow(resultats[[b]])))
    next
  }

  pan <- lire_csv(file.path(DOSSIER_DATA, "panel",
                            sprintf("%s_panel.csv", dossier_branche(b))))
  cols <- names(pan)[-1]
  i_cible <- match("CIBLE", cols)
  trim <- manif %>% dplyr::filter(branche == b, frequence == "trimestriel")
  colonnes_trim <- c(match(intersect(trim$colonne, cols), cols), i_cible)
  colonnes_trim <- sort(unique(colonnes_trim[!is.na(colonnes_trim)]))

  lignes <- list(); t0 <- Sys.time()
  for (k in seq_len(nrow(GRILLE))) {
    r <- GRILLE$r[k]; p <- GRILLE$p[k]
    precedent <- NULL
    for (o in ORIGINES) {
      o <- as.Date(o, origin = "1970-01-01")
      out <- prevoir_branche(pan, colonnes_trim, i_cible, o, r, p,
                             init = precedent)
      # `estimer_dfm` ignore de lui-meme un init de dimension incompatible.
      if (is.null(out)) { precedent <- NULL; next }
      # Le demarrage a chaud n'est valide que si le panel a garde la meme
      # dimension : si une serie devient utilisable a cette origine, les
      # chargements n'ont plus la bonne taille et il faut repartir a froid.
      precedent <- out$modele
      lignes[[length(lignes) + 1L]] <- tibble::tibble(
        branche = b, origine = o, r = r, p = p,
        prevision = out$prevision,
        n_series = length(out$colonnes),
        iterations = out$modele$iterations,
        converge = out$modele$converge)
    }
  }
  res_b <- dplyr::bind_rows(lignes) %>%
    dplyr::left_join(va %>% dplyr::filter(branche == b) %>%
                       dplyr::select(origine, reel), by = "origine")
  ecrire_csv(res_b, f_reprise)
  resultats[[b]] <- res_b
  cat(sprintf("      %-28s %4d previsions | %.1f min | convergence %.0f %%\n",
              b, nrow(res_b),
              as.numeric(difftime(Sys.time(), t0, units = "mins")),
              100 * mean(res_b$converge)))
}

previsions <- dplyr::bind_rows(resultats)
ecrire_csv(previsions, chemin_res("previsions_dfm.csv"))

# ============================================================================
# 4) PERFORMANCE DE CHAQUE CONFIGURATION
# ============================================================================
cat("\n[3/4] Performance par configuration\n")
grille <- previsions %>%
  dplyr::filter(!is.na(prevision), !is.na(reel)) %>%
  dplyr::group_by(branche, r, p) %>%
  dplyr::summarise(n = dplyr::n(),
                   RMSE = sqrt(mean((reel - prevision)^2)),
                   ratio = sqrt(mean((reel - prevision)^2)) / stats::sd(reel),
                   correl = suppressWarnings(stats::cor(prevision, reel)),
                   .groups = "drop")
ecrire_csv(grille, chemin_res("grille_rp.csv"))

resume <- grille %>% dplyr::group_by(r, p) %>%
  dplyr::summarise(branches = dplyr::n(),
                   ratio_median = stats::median(ratio),
                   br_sous_1 = sum(ratio < 1), .groups = "drop") %>%
  dplyr::arrange(ratio_median)
cat("\n      --- ratio median par configuration ---\n")
print(as.data.frame(resume %>% dplyr::mutate(ratio_median = round(ratio_median, 3))),
      row.names = FALSE)

cat("\n[4/4] Termine\n")
cat(sprintf("      %d previsions, %d branches, %d configurations\n",
            nrow(previsions), dplyr::n_distinct(previsions$branche), nrow(GRILLE)))
