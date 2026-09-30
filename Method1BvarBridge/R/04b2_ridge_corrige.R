# Re-execute UNIQUEMENT les variantes ridge, apres correction du comptage des
# degres de liberte dans la GCV. On les compare au perimetre commun des
# variantes deja calculees, pour que le classement reste valable.
source("R/00_setup.R"); source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R"); source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")

MIN_OBS_SEL <- 20L
cv <- charger_couverture(); BC <- cv$couvertes
ind <- charger_indicateurs(branches = BC)
trim <- indicateurs_trimestriels(ind)
d <- trim %>% dplyr::group_by(id_serie) %>%
  dplyr::summarise(n = sum(!is.na(x)), .groups = "drop")
tok <- trim %>% dplyr::filter(id_serie %in% d$id_serie[d$n >= MIN_OBS_SEL], !is.na(x)) %>%
  dplyr::select(id_serie, branche, date, x)
va <- charger_va() %>% dplyr::arrange(branche, date) %>% dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>% dplyr::select(branche, date, g)
origines <- sort(unique(va$date[va$date >= as.Date("2014-06-30")]))

passe_ridge <- function(lab, ar, pool, largeur) {
  purrr::map_dfr(BC, function(b) {
    g_b <- va %>% dplyr::filter(branche == b) %>% dplyr::select(date, g)
    x_b <- tok %>% dplyr::filter(branche == b) %>% dplyr::select(id_serie, date, x)
    if (pool) x_b <- dplyr::bind_rows(x_b, tok %>% dplyr::filter(branche != b) %>%
      dplyr::transmute(id_serie = paste0(id_serie, " [pool]"), date, x))
    if (!nrow(x_b)) return(NULL)
    xl <- x_b %>% tidyr::pivot_wider(names_from = id_serie, values_from = x)
    purrr::map_dfr(seq_along(origines), function(i) {
      ci <- origines[i]
      r <- estimer_bridge_ridge(g_b, xl, ci, avec_ar = ar, largeur = largeur)
      reel <- g_b$g[g_b$date == ci]
      tibble::tibble(specification = lab, branche = b, origine = ci,
        k = if (is.null(r)) NA_integer_ else r$n_indicateurs,
        ddl = if (is.null(r)) NA_real_ else r$ddl_effectifs,
        lambda = if (is.null(r)) NA_real_ else r$lambda,
        prevision = if (is.null(r)) NA_real_ else r$prevision,
        derniere_obs = if (is.null(r)) as.Date(NA) else r$derniere_obs,
        reel = if (length(reel) == 1L) reel else NA_real_)
    })
  })
}

SPECS <- list(
  list(lab = "6. ridge (sans AR)",              ar = FALSE, pool = FALSE, l = 32L),
  list(lab = "6+4. ridge + AR",                 ar = TRUE,  pool = FALSE, l = 32L),
  list(lab = "6+4. ridge + AR, fenetre 48",     ar = TRUE,  pool = FALSE, l = 48L),
  list(lab = "6+4+5. ridge + AR + fonds commun", ar = TRUE, pool = TRUE,  l = 32L))

res <- purrr::map_dfr(SPECS, function(v) {
  cat(sprintf("  %-34s", v$lab)); utils::flush.console()
  r <- passe_ridge(v$lab, v$ar, v$pool, v$l)
  cat(sprintf(" %3d/%d | k median %s | ddl median %s | lambda median %s\n",
      sum(!is.na(r$prevision)), nrow(r),
      format(stats::median(r$k, na.rm = TRUE)),
      format(round(stats::median(r$ddl, na.rm = TRUE), 1)),
      format(signif(stats::median(r$lambda, na.rm = TRUE), 3))))
  r
})
stopifnot(all(res$derniere_obs < res$origine, na.rm = TRUE))
ecrire_csv(res, file.path(DOSSIER_RESULTATS, "04b_ridge_corrige.csv"))

anciens <- lire_csv(file.path(DOSSIER_RESULTATS, "04b_previsions_variantes.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::filter(!grepl("^6", specification))
tous <- dplyr::bind_rows(anciens %>% dplyr::select(specification, branche, origine,
                                                   prevision, reel),
                         res %>% dplyr::select(specification, branche, origine,
                                               prevision, reel))
n_spec <- dplyr::n_distinct(tous$specification)
commun <- tous %>% dplyr::filter(!is.na(prevision)) %>%
  dplyr::count(branche, origine) %>% dplyr::filter(n == n_spec) %>%
  dplyr::select(branche, origine)
cat(sprintf("\n--- PERIMETRE COMMUN : %d cas ---\n", nrow(commun)))
b <- tous %>% dplyr::inner_join(commun, by = c("branche", "origine")) %>%
  dplyr::group_by(specification, branche) %>%
  dplyr::summarise(ratio = sqrt(mean((reel - prevision)^2)) / stats::sd(reel),
                   correlation = suppressWarnings(stats::cor(prevision, reel)),
                   .groups = "drop") %>%
  dplyr::group_by(specification) %>%
  dplyr::summarise(`ratio med.` = round(stats::median(ratio), 3),
                   `br. < 1` = sum(ratio < 1),
                   `correl.` = round(stats::median(correlation, na.rm = TRUE), 2),
                   .groups = "drop") %>% dplyr::arrange(`ratio med.`)
print(as.data.frame(b))
bv <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_recursives.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::inner_join(commun, by = c("branche", "origine")) %>%
  dplyr::group_by(branche) %>%
  dplyr::summarise(ratio = sqrt(mean((reel - prevision)^2)) / stats::sd(reel),
                   .groups = "drop")
cat(sprintf("\nBVAR seul, meme perimetre : ratio median %.3f | %d/%d < 1\n",
            stats::median(bv$ratio), sum(bv$ratio < 1), nrow(bv)))
