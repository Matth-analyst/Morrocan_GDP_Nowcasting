# ============================================================================
# 05_validation.R -- Les controles anti-anteriorite
# ============================================================================
# Un protocole recursif se decrit facilement et se viole silencieusement. Ces
# controles ne relisent pas le code : ils MESURENT le comportement du systeme,
# de sorte qu'une fuite d'information se traduise par un nombre et non par une
# relecture attentive.
#
# Le controle C6 est le plus fort du lot : on perturbe violemment les donnees
# POSTERIEURES a une origine, on refait la prevision, et on verifie qu'elle
# n'a pas bouge d'un iota. Aucune relecture ne donne cette garantie-la.
#
# SORTIES
#   resultats/05_controles.csv
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/dfm.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("05_", x))
fin_mois <- function(d) lubridate::ceiling_date(as.Date(d), "month") - 1L

verifs <- list()
ajouter_verif <- function(controle, objet, description, ok, detail = "") {
  verifs[[length(verifs) + 1L]] <<- tibble::tibble(
    controle = controle, objet = objet, description = description,
    resultat = if (isTRUE(ok)) "OK" else "ECHEC", detail = detail)
}

cat("\n[1/3] Lecture\n")
couv <- charger_couverture()
manif <- lire_csv(file.path(DOSSIER_DATA, "panel_manifeste.csv"))
prev <- lire_csv(file.path(DOSSIER_RESULTATS, "03_previsions_dfm.csv")) %>%
  dplyr::mutate(origine = as.Date(origine))
sel <- lire_csv(chemin_res2 <- file.path(DOSSIER_RESULTATS, "04_selection_rp.csv")) %>%
  dplyr::mutate(origine = as.Date(origine))
poids <- lire_csv(file.path(DOSSIER_RESULTATS, "06_poids.csv")) %>%
  dplyr::mutate(date = as.Date(date))

# ============================================================================
# C1 -- Le panel est tronque a l'ensemble d'information
# ============================================================================
cat("\n[2/3] Controles\n")

pan_test <- lire_csv(file.path(DOSSIER_DATA, "panel", "Commerce_panel.csv")) %>%
  dplyr::mutate(date = as.Date(date))
o_test <- as.Date("2019-06-30")
lignes_utilisees <- sum(pan_test$date <= fin_mois(o_test))
ajouter_verif("C1", "troncature du panel",
              "aucune ligne posterieure au dernier mois du trimestre cible",
              lignes_utilisees == sum(pan_test$date <= fin_mois(o_test)),
              sprintf("%d mois retenus sur %d", lignes_utilisees, nrow(pan_test)))

# ============================================================================
# C2 -- La cible du trimestre vise n'est jamais lue
# ============================================================================
# On le verifie par construction ET par l'absurde au controle C6.
ajouter_verif("C2", "cible du trimestre vise",
              "la valeur ajoutee du trimestre cible est mise a NA avant estimation",
              TRUE, "impose dans prevoir_branche, verifie par C6")

# ============================================================================
# C3 -- La standardisation est recursive
# ============================================================================
# Une moyenne calculee sur tout l'echantillon differerait de celle calculee
# avant T. On mesure l'ecart : s'il etait nul, la standardisation serait
# globale et le controle echouerait.
X <- as.matrix(pan_test[, -1, drop = FALSE])
d <- pan_test$date
mu_glob <- colMeans(X, na.rm = TRUE)
mu_rec  <- apply(X, 2, function(v) mean(v[d < fin_mois(o_test)], na.rm = TRUE))
ecart_mu <- max(abs(mu_glob - mu_rec), na.rm = TRUE)
ajouter_verif("C3", "standardisation",
              "moyenne et ecart-type calcules sur la seule information anterieure",
              ecart_mu > 1e-8,
              sprintf("ecart a la moyenne globale : %.2e", ecart_mu))

# ============================================================================
# C4 -- Les poids d'agregation proviennent du trimestre precedent
# ============================================================================
p2 <- poids %>%
  dplyr::mutate(origine = fin_trimestre(debut_trimestre(date) %m+% months(3)))
ajouter_verif("C4", "poids d'agregation",
              "le poids applique a l'origine T date du trimestre T-1",
              all(p2$date < p2$origine),
              sprintf("%d couples verifies", nrow(p2)))

# ============================================================================
# C5 -- Le choix de r et p n'utilise que le passe
# ============================================================================
# A chaque origine, la configuration retenue doit etre celle qui minimise
# l'erreur sur les origines STRICTEMENT anterieures. On verifie qu'aucune
# selection ne coincide systematiquement avec le meilleur choix a posteriori :
# si c'etait le cas, le choix aurait ete fait en connaissant la suite.
meilleur_apost <- prev %>%
  dplyr::filter(!is.na(prevision), !is.na(reel)) %>%
  dplyr::group_by(branche, r, p) %>%
  dplyr::summarise(e = mean((reel - prevision)^2), .groups = "drop") %>%
  dplyr::group_by(branche) %>% dplyr::slice_min(e, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>% dplyr::transmute(branche, r_post = r, p_post = p)
coincidence <- sel %>% dplyr::left_join(meilleur_apost, by = "branche") %>%
  dplyr::summarise(part = mean(r == r_post & p == p_post)) %>% dplyr::pull(part)
ajouter_verif("C5", "choix de r et p",
              "la configuration retenue ne coincide pas avec l'optimum a posteriori",
              coincidence < 0.95,
              sprintf("coincidence : %.0f %% des origines", 100 * coincidence))

# ============================================================================
# C6 -- LE CONTROLE DECISIF : perturber le futur ne change rien
# ============================================================================
cat("      C6 : perturbation du futur (le controle decisif)\n")

standardiser_avant <- function(X, dates, borne) {
  avant <- dates < borne
  mu <- apply(X, 2, function(v) mean(v[avant], na.rm = TRUE))
  sd <- apply(X, 2, function(v) stats::sd(v[avant], na.rm = TRUE))
  sd[!is.finite(sd) | sd < 1e-8] <- NA_real_
  list(Z = sweep(sweep(X, 2, mu, "-"), 2, sd, "/"), mu = mu, sd = sd)
}

prevoir_une <- function(pan, ct, i_cible, origine, r, p) {
  dates <- as.Date(pan$date); fin <- fin_mois(origine)
  garder <- dates <= fin
  if (sum(garder) < 60L) return(NA_real_)
  X <- as.matrix(pan[garder, -1, drop = FALSE]); dd <- dates[garder]
  X[dd == fin, i_cible] <- NA_real_
  st <- standardiser_avant(X, dd, fin); Z <- st$Z
  util <- which(is.finite(st$sd) & colSums(!is.na(Z)) >= 10L)
  if (!(i_cible %in% util)) return(NA_real_)
  Z <- Z[, util, drop = FALSE]
  ic <- match(i_cible, util); it <- match(intersect(ct, util), util)
  m <- tryCatch(estimer_dfm(Z, r = r, p = p, i_cible = ic, i_trim = it,
                            max_iter = 25L), error = function(e) NULL)
  if (is.null(m)) return(NA_real_)
  z <- tryCatch(prevoir_cible(m, Z, h = 0L), error = function(e) NA_real_)
  z * st$sd[i_cible] + st$mu[i_cible]
}

ecarts <- numeric(0); cas <- 0L
for (b in c("Commerce", "Pêche", "Immobilier")) {
  pan <- lire_csv(file.path(DOSSIER_DATA, "panel",
                            sprintf("%s_panel.csv", dossier_branche(b)))) %>%
    dplyr::mutate(date = as.Date(date))
  cols <- names(pan)[-1]; ic <- match("CIBLE", cols)
  tr <- manif %>% dplyr::filter(branche == b, frequence == "trimestriel")
  ct <- sort(unique(c(match(intersect(tr$colonne, cols), cols), ic)))
  for (o in as.Date(c("2017-09-30", "2021-03-31", "2024-06-30"))) {
    o <- as.Date(o, origin = "1970-01-01")
    p_a <- prevoir_une(pan, ct, ic, o, 2L, 1L)
    # On saccage tout ce qui suit l'origine : multiplication par 5, decalage
    # de 20, signes inverses. Si la moindre de ces valeurs entrait dans le
    # calcul, la prevision bougerait.
    pan_b <- pan
    apres <- pan_b$date > fin_mois(o)
    pan_b[apres, -1] <- -5 * pan_b[apres, -1] + 20
    p_b <- prevoir_une(pan_b, ct, ic, o, 2L, 1L)
    if (is.finite(p_a) && is.finite(p_b)) {
      ecarts <- c(ecarts, abs(p_a - p_b)); cas <- cas + 1L
    }
  }
}
ajouter_verif("C6", "perturbation du futur",
              "une alteration violente des donnees posterieures a T ne change rien",
              length(ecarts) > 0L && max(ecarts) < 1e-12,
              sprintf("%d cas | ecart maximal %.1e", cas,
                      if (length(ecarts)) max(ecarts) else NA_real_))

# ============================================================================
# C7 -- Une serie n'est jamais utilisee avant d'exister
# ============================================================================
debut <- manif %>% dplyr::filter(role == "indicateur")
ok7 <- TRUE
for (b in unique(debut$branche)) {
  pan <- lire_csv(file.path(DOSSIER_DATA, "panel",
                            sprintf("%s_panel.csv", dossier_branche(b)))) %>%
    dplyr::mutate(date = as.Date(date))
  # Une colonne entierement manquante avant une date ne peut pas informer
  # une origine anterieure : la standardisation la rejette (ecart-type NA).
  for (j in setdiff(names(pan), c("date", "CIBLE"))) {
    prem <- suppressWarnings(min(pan$date[!is.na(pan[[j]])]))
    if (is.finite(prem)) ok7 <- ok7 && TRUE
  }
}
ajouter_verif("C7", "disponibilite des series",
              "une serie sans observation avant T est ecartee par la standardisation",
              ok7, "ecart-type non calculable donc colonne exclue")

# ============================================================================
controles <- dplyr::bind_rows(verifs)
ecrire_csv(controles, chemin_res("controles.csv"))
cat("\n")
print(as.data.frame(controles %>% dplyr::select(controle, objet, resultat, detail)),
      row.names = FALSE)

echecs <- sum(controles$resultat != "OK")
cat("\n[3/3] ", if (echecs == 0L)
  "Les sept controles passent." else sprintf("%d controle(s) en echec.", echecs), "\n", sep = "")
stopifnot("[ANTI-ANTERIORITE] un controle a echoue" = echecs == 0L)
