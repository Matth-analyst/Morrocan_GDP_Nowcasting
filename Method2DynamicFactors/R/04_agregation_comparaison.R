# ============================================================================
# 04_agregation_comparaison.R -- De la branche a l'agregat, et face a la
#                                methode 1
# ============================================================================
# TROIS ETAPES
#
#   1. Le couple (r, p) est choisi RECURSIVEMENT : a chaque origine, on retient
#      la configuration qui a le mieux fait sur les origines PRECEDENTES. C'est
#      le critere qui compte -- l'erreur de prevision -- et non un critere
#      d'information calcule dans l'echantillon. Aucune information posterieure
#      a T n'intervient.
#
#   2. Les quatre branches sans indicateur recoivent un AR(p), ordre choisi par
#      critere bayesien a chaque origine.
#
#      ATTENTION, CE N'EST PAS LE TRAITEMENT DE LA METHODE 1. Cette ligne a
#      longtemps affirme le contraire. Verification faite sur les sorties, la
#      methode 1 applique son BVAR a ces quatre branches : sur les 192
#      previsions concernees, aucune ne coincide avec celle calculee ici.
#
#      L'ecart n'est pas neutre. Rapporte a l'ecart-type de chaque branche,
#      l'AR(p) vaut 1,485 contre 0,968 pour le BVAR sur les services aux
#      entreprises, 1,372 contre 1,006 sur l'administration publique, 1,050
#      contre 0,941 sur les autres services, et 0,909 contre 0,954 sur
#      l'education-sante : il perd trois fois sur quatre. Ces branches pesant
#      un quart de la valeur ajoutee, le ratio de l'agregat passerait de 0,983
#      a 0,972 si elles etaient traitees comme dans la methode 1.
#
#      La comparaison DEFAVORISE donc les facteurs dynamiques. Le classement
#      n'en est pas change, mais l'ecart annonce est un majorant, et c'est ainsi
#      qu'il doit etre lu.
#
#   3. L'agregation suit la meme formule que la methode 1 : indice de volume de
#      Laspeyres a poids en prix courants du trimestre precedent, verifies
#      identiques octet pour octet.
#
# SORTIES
#   resultats/04_selection_rp.csv     le couple retenu a chaque origine
#   resultats/04_previsions_branche.csv  la prevision finale par branche
#   resultats/04_agregat.csv          l'agregat et sa realisation
#   resultats/04_comparaison.csv      DFM contre methode 1
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("04_", x))
BURN_IN <- 8L    # origines servant a departager avant le premier choix

cat("\n[1/5] Bases\n")
couv <- charger_couverture()
prev <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_dfm.csv")) %>%
  dplyr::mutate(origine = as.Date(origine))
va <- charger_va() %>% dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
  dplyr::transmute(branche, origine = date, reel = g)
ORIGINES <- sort(unique(prev$origine))
cat(sprintf("      %d previsions | %d branches couvertes | %d origines\n",
            nrow(prev), dplyr::n_distinct(prev$branche), length(ORIGINES)))

# ============================================================================
# 2) SELECTION RECURSIVE DU COUPLE (r, p)
# ============================================================================
cat("\n[2/5] Selection recursive de r et p\n")

selection <- list(); retenues <- list()
for (b in unique(prev$branche)) {
  pb <- prev %>% dplyr::filter(branche == b) %>%
    dplyr::mutate(err2 = (reel - prevision)^2)
  for (k in seq_along(ORIGINES)) {
    o <- ORIGINES[k]
    passe <- pb %>% dplyr::filter(origine < o, !is.na(err2))
    if (k <= BURN_IN || nrow(passe) < 12L) {
      r_ret <- 1L; p_ret <- 1L; motif <- "defaut"
    } else {
      cum <- passe %>% dplyr::group_by(r, p) %>%
        dplyr::summarise(e = mean(err2), n = dplyr::n(), .groups = "drop") %>%
        dplyr::filter(n >= 8L) %>% dplyr::arrange(e)
      if (nrow(cum) == 0L) { r_ret <- 1L; p_ret <- 1L; motif <- "defaut" }
      else { r_ret <- cum$r[1]; p_ret <- cum$p[1]; motif <- "recursif" }
    }
    ligne <- pb %>% dplyr::filter(origine == o, r == r_ret, p == p_ret)
    selection[[length(selection) + 1L]] <- tibble::tibble(
      branche = b, origine = o, r = r_ret, p = p_ret, motif = motif)
    retenues[[length(retenues) + 1L]] <- tibble::tibble(
      branche = b, origine = o,
      prevision = if (nrow(ligne) == 1L) ligne$prevision[1] else NA_real_)
  }
}
selection <- dplyr::bind_rows(selection)
retenues  <- dplyr::bind_rows(retenues)
ecrire_csv(selection, chemin_res("selection_rp.csv"))

cat("      couples retenus, toutes branches et origines confondues :\n")
print(as.data.frame(selection %>% dplyr::count(r, p, name = "origines") %>%
                      dplyr::arrange(dplyr::desc(origines))), row.names = FALSE)
cat(sprintf("      part des origines ou le choix est recursif : %.0f %%\n",
            100 * mean(selection$motif == "recursif")))

# ============================================================================
# 3) LES QUATRE BRANCHES SANS INDICATEUR : AR(p) RECURSIF
# ============================================================================
cat("\n[3/5] Branches non couvertes : AR(p) par critere bayesien\n")

prevoir_ar <- function(g, dates, cible, p_max = 6L) {
  h <- g[dates < cible]
  if (length(h) < 20L) return(NA_real_)
  meilleur <- NULL; bic_min <- Inf
  for (p in 1:p_max) {
    if (length(h) < 4L * p + 10L) next
    fit <- tryCatch(stats::arima(h, order = c(p, 0, 0), method = "ML"),
                    error = function(e) NULL)
    if (is.null(fit)) next
    bic <- -2 * fit$loglik + (p + 1) * log(length(h))
    if (bic < bic_min) { bic_min <- bic; meilleur <- fit }
  }
  if (is.null(meilleur)) return(mean(h))
  as.numeric(stats::predict(meilleur, n.ahead = 1L)$pred[1])
}

ar_lignes <- list()
for (b in couv$non_couvertes) {
  vb <- va %>% dplyr::filter(branche == b) %>% dplyr::arrange(origine)
  for (o in ORIGINES) {
    o <- as.Date(o, origin = "1970-01-01")
    ar_lignes[[length(ar_lignes) + 1L]] <- tibble::tibble(
      branche = b, origine = o,
      prevision = prevoir_ar(vb$reel, vb$origine, o))
  }
  cat(sprintf("      %-28s %d previsions\n", b, length(ORIGINES)))
}
ar_prev <- dplyr::bind_rows(ar_lignes)

branches_prev <- dplyr::bind_rows(retenues, ar_prev) %>%
  dplyr::left_join(va, by = c("branche", "origine"))
ecrire_csv(branches_prev, chemin_res("previsions_branche.csv"))

# ============================================================================
# 4) AGREGATION
# ============================================================================
cat("\n[4/5] Agregation\n")
poids <- lire_csv(file.path(DOSSIER_RESULTATS, "06_poids.csv")) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::transmute(branche,
                   origine = fin_trimestre(debut_trimestre(date) %m+% months(3)), w)

agreger_niveau <- function(w, g) log(sum(w * exp(g)))

base <- branches_prev %>%
  dplyr::inner_join(poids, by = c("branche", "origine")) %>%
  dplyr::filter(!is.na(prevision), !is.na(reel))
completes <- base %>% dplyr::count(origine) %>%
  dplyr::filter(n == length(TOUTES_BRANCHES))
agregat <- base %>% dplyr::filter(origine %in% completes$origine) %>%
  dplyr::group_by(origine) %>%
  dplyr::summarise(reel_ag = agreger_niveau(w, reel),
                   dfm_ag  = agreger_niveau(w, prevision), .groups = "drop") %>%
  dplyr::arrange(origine) %>%
  dplyr::mutate(trimestre = date_vers_trimestre(origine), .after = origine)
ecrire_csv(agregat, chemin_res("agregat.csv"))
cat(sprintf("      %d origines completes sur %d\n", nrow(agregat), length(ORIGINES)))

e <- agregat$reel_ag - agregat$dfm_ag
cat(sprintf("      RMSE %.4f | ratio %.3f | correlation %+.3f\n",
            sqrt(mean(e^2)), sqrt(mean(e^2)) / stats::sd(agregat$reel_ag),
            stats::cor(agregat$dfm_ag, agregat$reel_ag)))

# ============================================================================
# 5) FACE A LA METHODE 1
# ============================================================================
cat("\n[5/5] Comparaison avec la methode 1\n")
f_m1 <- file.path("..", "Method1BvarBridge", "resultats", "06_agregat.csv")
if (!file.exists(f_m1)) {
  cat("      agregat de la methode 1 introuvable : comparaison non produite\n")
} else {
  m1 <- lire_csv(f_m1) %>% dplyr::mutate(origine = as.Date(origine)) %>%
    dplyr::select(origine, reel_m1 = reel_niveau, m1 = nowcast_niveau)
  cmp <- agregat %>% dplyr::inner_join(m1, by = "origine")
  stopifnot("Les deux methodes doivent viser la meme realisation" =
              max(abs(cmp$reel_ag - cmp$reel_m1)) < 1e-10)

  mesures <- function(prev, reel, nom) {
    err <- reel - prev
    tibble::tibble(methode = nom, n = length(err),
                   RMSE = sqrt(mean(err^2)),
                   MAE = mean(abs(err)),
                   ratio = sqrt(mean(err^2)) / stats::sd(reel),
                   correlation = stats::cor(prev, reel),
                   biais = mean(-err))
  }
  res <- dplyr::bind_rows(
    mesures(cmp$dfm_ag, cmp$reel_ag, "Modele a facteurs dynamiques"),
    mesures(cmp$m1, cmp$reel_ag, "Methode 1 (BVAR + passerelles)"))
  ecrire_csv(res, chemin_res("comparaison.csv"))
  print(as.data.frame(res %>% dplyr::mutate(dplyr::across(where(is.numeric),
                                                          ~round(.x, 4)))),
        row.names = FALSE)

  # Diebold-Mariano, avec la correction de petit echantillon.
  d <- (cmp$reel_ag - cmp$dfm_ag)^2 - (cmp$reel_ag - cmp$m1)^2
  n <- length(d); h <- 1L
  gam <- stats::acf(d, lag.max = h - 1L, type = "covariance", plot = FALSE)$acf
  vd <- sum(gam) / n
  dm <- mean(d) / sqrt(max(vd, 1e-12))
  corr <- sqrt((n + 1 - 2 * h + h * (h - 1) / n) / n)
  dm_c <- dm * corr
  pv <- 2 * stats::pt(-abs(dm_c), df = n - 1L)
  cat(sprintf("\n      Diebold-Mariano : DM = %+.3f | p = %.4f | %s\n", dm_c, pv,
              if (pv < 0.05) "ecart significatif" else "ecart non significatif"))
  ecrire_csv(tibble::tibble(statistique = dm_c, p_value = pv, n = n),
             chemin_res("diebold_mariano.csv"))
}

cat("\nAgregation et comparaison terminees.\n")
