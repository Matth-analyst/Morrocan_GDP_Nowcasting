# ============================================================================
# 03d_bvar_etendu.R -- Le BVAR recursif, depuis 2008
# ============================================================================
# POURQUOI CE SCRIPT EXISTE
#   La phase 3 fait commencer le backtest a T2-2014. Cette borne est justifiee
#   -- mais pour une autre raison que celle a laquelle elle a servi.
#
#   Elle vient de la valeur ajoutee NOMINALE, qui commence au premier trimestre
#   2014 et fournit les poids en prix courants qu'exige une agregation de type
#   Fisher ou Tornqvist. C'est une contrainte de l'AGREGATION, phase 11.
#
#   Le BVAR, lui, n'utilise que la valeur ajoutee REELLE, disponible depuis
#   1998. Rien ne l'empeche de remonter plus haut. La borne de 2014 lui a ete
#   appliquee par alignement, pour que toutes les phases s'evaluent sur le meme
#   ensemble de trimestres -- ce qui etait raisonnable, mais n'a jamais ete
#   reinterroge.
#
#   Elle bloque aujourd'hui le backtest etendu : sans previsions BVAR avant
#   2014, aucune comparaison avec la passerelle n'est possible sur la crise de
#   2008-2009, et les tests retombent a 48 origines au lieu de 72.
#
# CE QUE FAIT CE SCRIPT
#   La meme chose que la phase 3, avec la MEME specification retenue, mais sur
#   des origines qui remontent a T2-2008 :
#
#       selection des hyperparametres   critere d'ajustement (BGR)
#       regle de detection des chocs    z = 4, k = 3
#       fenetre d'estimation de sigma   60 trimestres
#       theta = 1, rho = 1              aucun levier supplementaire
#
#   Aucun reglage n'est refait : ce serait choisir une specification sur un
#   echantillon different de celui qui a servi a la retenir. Les origines
#   communes doivent redonner EXACTEMENT les memes previsions qu'en phase 3,
#   et le script le verifie.
#
# SORTIES
#   resultats/03d_previsions_bvar_etendu.csv
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/bvar.R")

PREMIERE_CIBLE <- as.Date("2008-06-30")
BASCULE        <- as.Date("2014-06-30")

# --- la specification retenue en phase 3, reprise a l'identique --------------
SELECTION      <- "bgr"
REGLE_CHOC     <- list(z = 4, k = 3)
FENETRE_SIGMA  <- 60
THETA          <- 1
RHO            <- 1
P_GRILLE       <- 1:5
D_GRILLE       <- c(0.5, 1, 1.5, 2)
BRANCHES_REFERENCE <- c("Industrie de transformation", "Commerce", "Agriculture")
P_DEFAUT       <- 5L
LAMBDA_DEFAUT  <- 0.15

cat("\n[1/4] Construction de la matrice\n")
va <- charger_va() %>%
  dplyr::arrange(branche, date) %>%
  dplyr::group_by(branche) %>%
  dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
  dplyr::ungroup()
large <- va %>% dplyr::select(branche, date, g) %>%
  tidyr::pivot_wider(names_from = branche, values_from = g) %>%
  dplyr::arrange(date) %>%
  dplyr::filter(dplyr::if_all(dplyr::all_of(TOUTES_BRANCHES), ~ !is.na(.)))
dates_vec <- large$date
Y <- as.matrix(large[, TOUTES_BRANCHES])
cat(sprintf("      Y : %d trimestres x %d branches (%s -> %s)\n",
            nrow(Y), ncol(Y), date_vers_trimestre(dates_vec[1]),
            date_vers_trimestre(dates_vec[nrow(Y)])))

origines <- dates_vec[dates_vec >= PREMIERE_CIBLE]
cat(sprintf("      %d origines, de %s a %s (dont %d avant %s)\n",
            length(origines), date_vers_trimestre(origines[1]),
            date_vers_trimestre(origines[length(origines)]),
            sum(origines < BASCULE), date_vers_trimestre(BASCULE)))

# ============================================================================
# 2) PASSE RECURSIVE
# ============================================================================
cat("\n[2/4] Estimation recursive\n")
t0 <- Sys.time()
previsions <- purrr::map_dfr(seq_along(origines), function(i) {
  cible <- origines[i]
  r <- prevision_bvar_recursive(
    Y, dates_vec, cible, p = P_DEFAUT, lambda = LAMBDA_DEFAUT,
    dates_choc = NULL, fenetre = Inf, selection = SELECTION,
    p_grille = P_GRILLE, ref = BRANCHES_REFERENCE, regle_choc = REGLE_CHOC,
    d_grille = D_GRILLE, theta = THETA, rho = RHO, fenetre_sigma = FENETRE_SIGMA)
  if (is.null(r)) return(NULL)
  tibble::tibble(
    origine = cible, trimestre = date_vers_trimestre(cible),
    p = r$p, lambda = r$lambda, d = r$d, n_chocs = r$n_chocs,
    n_obs_estimation = r$n_obs, derniere_obs = r$derniere_obs,
    branche = names(r$prevision),
    prevision = as.numeric(r$prevision),
    reel = as.numeric(Y[dates_vec == cible, ]))
})
cat(sprintf("      %s | %d previsions (%d origines x %d branches)\n",
            format(round(difftime(Sys.time(), t0, units = "mins"), 2)),
            nrow(previsions), dplyr::n_distinct(previsions$origine),
            dplyr::n_distinct(previsions$branche)))

stopifnot("[ANTI-LOOK-AHEAD] une estimation a vu sa cible" =
            all(previsions$derniere_obs < previsions$origine))
cat("      controle : aucune estimation ne contient sa propre cible\n")

# ============================================================================
# 3) CONTROLE DE COHERENCE AVEC LA PHASE 3
# ============================================================================
cat("\n[3/4] Coherence avec la phase 3\n")
# Sur les origines communes, la specification etant identique et l'information
# disponible aussi, les previsions doivent coincider au bit pres. Un ecart
# signalerait que ce script ne reproduit pas la phase 3 -- et rendrait toute
# comparaison entre les deux echantillons illegitime.
ref <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_recursives.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::select(branche, origine, prevision_phase3 = prevision)

verif <- previsions %>%
  dplyr::inner_join(ref, by = c("branche", "origine")) %>%
  dplyr::mutate(ecart = abs(prevision - prevision_phase3))
cat(sprintf("      %d previsions comparables | ecart maximal %.2e\n",
            nrow(verif), max(verif$ecart)))
if (max(verif$ecart) > 1e-8) {
  mauvais <- verif %>% dplyr::arrange(dplyr::desc(ecart)) %>% utils::head(5)
  print(as.data.frame(mauvais %>% dplyr::select(branche, trimestre, prevision,
                                                prevision_phase3, ecart)))
  stop("Les previsions ne reproduisent pas la phase 3 sur les origines communes.",
       call. = FALSE)
}
cat("      les origines communes reproduisent la phase 3 a l'identique\n")

ecrire_csv(previsions, file.path(DOSSIER_RESULTATS, "03d_previsions_bvar_etendu.csv"))

# ============================================================================
# 4) CE QUE LES ORIGINES ANTERIEURES APPORTENT
# ============================================================================
cat("\n[4/4] Performance sur les deux periodes\n")
bilan <- previsions %>%
  dplyr::mutate(periode = ifelse(origine < BASCULE, "2008-2014 (nouveau)",
                                 "2014-2026 (phase 3)")) %>%
  dplyr::group_by(periode, branche) %>%
  dplyr::filter(dplyr::n() >= 4L) %>%
  dplyr::summarise(n = dplyr::n(),
                   ratio = sqrt(mean((reel - prevision)^2)) / stats::sd(reel),
                   .groups = "drop") %>%
  dplyr::group_by(periode) %>%
  dplyr::summarise(branches = dplyr::n(), origines = max(n),
                   ratio_median = round(stats::median(ratio), 3),
                   n_ok = sum(ratio < 1), .groups = "drop")
print(as.data.frame(bilan))

hyper <- previsions %>% dplyr::distinct(origine, p, lambda, d, n_chocs) %>%
  dplyr::mutate(periode = ifelse(origine < BASCULE, "2008-2014", "2014-2026"))
cat("\n      hyperparametres retenus, par periode :\n")
print(as.data.frame(hyper %>% dplyr::group_by(periode) %>%
  dplyr::summarise(p_median = stats::median(p),
                   lambda_median = round(stats::median(lambda), 4),
                   d_median = stats::median(d),
                   chocs_median = stats::median(n_chocs), .groups = "drop")))

cat("\nBVAR etendu termine.\n")
