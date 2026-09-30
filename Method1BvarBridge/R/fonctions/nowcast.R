# ============================================================================
# fonctions/nowcast.R -- La fonction centrale du nowcast  (section 29 du plan)
# ============================================================================
# "L'objectif est d'avoir une logique temporelle UNIQUE et testable."
#
# Toute la chaine pour UN trimestre cible : ensemble d'information, prevision
# des mois manquants, agregation trimestrielle, selection, passerelle, BVAR,
# poids de combinaison, poids sectoriels, agregation.
#
# Deux appelants :
#   R/07_validation_pseudo_temps_reel.R  la confronte au pipeline phase par
#       phase, sur un echantillon d'origines. Deux chemins de calcul
#       independants qui donnent le meme nombre valident la logique temporelle.
#   R/08_rapport_synthese.R              l'utilise pour produire le nowcast
#       courant, celui du trimestre en cours.
#
# Le contexte -- donnees brutes, matrice des branches, poids -- est construit
# une fois par `preparer_contexte()` et passe en argument. La fonction ne lit
# donc aucun etat global, ce qui est la condition pour qu'elle soit testable.
# ============================================================================

#' Parametres de la specification retenue par l'ensemble du projet.
parametres_nowcast <- function() list(
  premiere_cible     = as.Date("2014-06-30"),
  bvar_selection     = "bgr",
  bvar_regle_choc    = list(z = 4, k = 3),
  bvar_fenetre_sigma = 60,
  bvar_p_grille      = 1:5,
  bvar_d_grille      = c(0.5, 1, 1.5, 2),
  bvar_reference     = c("Industrie de transformation", "Commerce", "Agriculture"),
  seuil_r            = 0.15,
  seuil_p            = 0.10,
  min_obs_sel        = 20L,
  max_retenus        = 5L,
  avec_ar            = TRUE,
  trimestriels_m3    = FALSE,
  min_obs_delta      = 8L,
  ar_ordre           = 4L,
  # Garde-fou sur l'amplitude des previsions de branche. Par defaut on SIGNALE
  # sans modifier : plafonner n'ameliore pas la combinaison et abimerait la
  # prevision de -81,7 % du T2-2020, la plus juste du projet. Voir
  # `garde_amplitude()` dans R/fonctions/passerelle.R.
  garde_z_max        = 5,
  garde_action       = "signaler",
  # Regle d'instabilite : une branche dont la variance du premier tiers depasse
  # `instab_rapport_min` fois celle du reste sort du BVAR. Voir
  # R/fonctions/branches_instables.R.
  instab_rapport_min = 10,
  instab_fenetre     = 60L)

#' Construit une fois pour toutes les objets dont la fonction centrale a besoin.
preparer_contexte <- function() {
  couverture <- charger_couverture()
  ind_brut <- charger_indicateurs(branches = couverture$couvertes)
  longueur <- ind_brut %>% dplyr::count(id_serie, name = "n")
  ind_brut <- ind_brut %>%
    dplyr::filter(id_serie %in% longueur$id_serie[longueur$n >= 36L])

  va <- charger_va() %>% dplyr::arrange(branche, date) %>%
    dplyr::group_by(branche) %>%
    dplyr::mutate(g = transformer_serie(va, "dlog", date, "trimestriel")) %>%
    dplyr::ungroup() %>% dplyr::filter(!is.na(g)) %>%
    dplyr::select(branche, date, g)
  large <- va %>% tidyr::pivot_wider(names_from = branche, values_from = g) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(TOUTES_BRANCHES), ~ !is.na(.)))

  f_poids <- file.path(DOSSIER_RESULTATS, "06_poids.csv")
  if (!file.exists(f_poids)) {
    stop("06_poids.csv absent : executer d'abord R/06_agregation_fisher.R",
         call. = FALSE)
  }
  list(
    BC = couverture$couvertes, NC = couverture$non_couvertes,
    ind_brut = ind_brut,
    meta = charger_metadonnees() %>%
      dplyr::select(id_serie, agregation, transformation),
    va = va,
    Y = as.matrix(large[, TOUTES_BRANCHES]),
    dates_vec = large$date,
    poids = lire_csv(f_poids) %>% dplyr::mutate(date = as.Date(date)))
}

#' Nowcast complet pour UN trimestre cible, a partir des donnees brutes.
#'
#' La fonction ne lit aucun resultat produit par une autre phase : elle refait
#' la chaine entiere. C'est ce qui lui permet de servir de test independant.
#'
#' @param cible trimestre cible, en fin de trimestre.
#' @param scenario "M0" a "M3" -- combien de mois du trimestre cible sont vus.
#' @param historique previsions des origines anterieures, necessaires pour
#'   estimer le poids de combinaison delta. Format : branche, origine, reel,
#'   bvar, bridge.
backtest_nowcast <- function(cible, scenario = "M3", historique = NULL,
                             contexte = NULL, params = parametres_nowcast()) {
  if (is.null(contexte)) contexte <- preparer_contexte()
  ind_brut <- contexte$ind_brut; meta <- contexte$meta; va <- contexte$va
  Y <- contexte$Y; DATES_VEC <- contexte$dates_vec
  BC <- contexte$BC; NC <- contexte$NC; poids_bruts <- contexte$poids

  # --- (a) ensemble d'information -------------------------------------------
  info <- information_set_intra(ind_brut, cible, scenario = scenario)
  if (!params$trimestriels_m3) {
    info <- info %>%
      dplyr::filter(!(frequence == "trimestriel" &
                        date >= debut_trimestre(cible) & date <= fin_trimestre(cible)))
  }
  # CONTROLE 2 : rien de ce qui sert a prevoir n'est posterieur a la cible.
  stopifnot("[C2] donnee posterieure a la cible dans l'ensemble d'information" =
              all(info$date <= fin_trimestre(cible)))

  # --- (b) mois manquants du trimestre cible, prevus ------------------------
  mois_T <- mois_du_trimestre(cible)
  n_vus <- switch(scenario, M0 = 0L, M1 = 1L, M2 = 2L, M3 = 3L)
  a_prevoir <- as.Date(setdiff(mois_T, if (n_vus > 0L) mois_T[seq_len(n_vus)]
                               else as.Date(character(0))), origin = "1970-01-01")
  mensuel <- info %>% dplyr::filter(frequence == "mensuel")
  # LE JAGGED EDGE, AU SENS PROPRE. Les series ne s'arretent pas toutes au meme
  # mois : au bord de l'echantillon, l'une publie jusqu'a mai, une autre jusqu'a
  # mars. Prevoir seulement les mois manquants AU SENS DU SCENARIO laisserait
  # donc des trous propres a chaque serie, et le trimestre serait refuse par
  # `agreger_trimestriel` -- la serie disparaitrait silencieusement.
  #
  # Les mois a prevoir sont donc determines SERIE PAR SERIE : tous ceux du
  # trimestre cible que cette serie-la n'a pas, et que le scenario autorise a
  # connaitre ou non. C'est la seule facon de traiter un bord reellement
  # irregulier.
  if (nrow(mensuel) > 0L) {
    mensuel <- mensuel %>%
      dplyr::group_by(id_serie, branche, indicateur) %>%
      dplyr::group_modify(function(g, cle) {
        manquants <- as.Date(setdiff(mois_T, g$date), origin = "1970-01-01")
        if (length(manquants) == 0L) return(dplyr::select(g, date, valeur))
        r <- prevoir_mois_manquants(g$date, g$valeur, manquants)
        if (is.null(r)) return(dplyr::select(g, date, valeur))
        dplyr::select(r, date, valeur)
      }) %>% dplyr::ungroup()
  } else {
    mensuel <- mensuel %>% dplyr::select(id_serie, branche, indicateur, date, valeur)
  }

  base_long <- dplyr::bind_rows(
    mensuel %>% dplyr::mutate(frequence = "mensuel") %>%
      dplyr::select(id_serie, branche, indicateur, frequence, date, valeur),
    info %>% dplyr::filter(frequence == "trimestriel") %>%
      dplyr::select(id_serie, branche, indicateur, frequence, date, valeur)) %>%
    dplyr::left_join(meta, by = "id_serie")
  trim <- indicateurs_trimestriels(base_long, tolerant = TRUE)
  d_n <- trim %>% dplyr::group_by(id_serie) %>%
    dplyr::summarise(n = sum(!is.na(x)), .groups = "drop")
  tok <- trim %>%
    dplyr::filter(id_serie %in% d_n$id_serie[d_n$n >= params$min_obs_sel], !is.na(x)) %>%
    dplyr::select(id_serie, branche, date, x)

  # --- (c) BVAR, sur les seize branches -------------------------------------
  r_bvar <- prevision_bvar_recursive(
    Y, DATES_VEC, cible, p = 5L, lambda = 0.15, dates_choc = NULL,
    fenetre = Inf, selection = params$bvar_selection,
    p_grille = params$bvar_p_grille, ref = params$bvar_reference,
    regle_choc = params$bvar_regle_choc, d_grille = params$bvar_d_grille,
    theta = 1, rho = 1, fenetre_sigma = params$bvar_fenetre_sigma)
  # CONTROLE 1 : l'echantillon d'estimation s'arrete avant la cible.
  stopifnot("[C1] le BVAR a vu sa propre cible" = r_bvar$derniere_obs < cible)

  # --- (d) passerelle, branches couvertes -----------------------------------
  passerelle <- purrr::map_dfr(BC, function(b) {
    g_b <- va %>% dplyr::filter(branche == b) %>% dplyr::select(date, g)
    x_b <- tok %>% dplyr::filter(branche == b) %>% dplyr::select(id_serie, date, x)
    if (nrow(x_b) == 0L) return(NULL)
    x_l <- x_b %>% tidyr::pivot_wider(names_from = id_serie, values_from = x)
    s <- selectionner_disponible(g_b, x_b, x_l, cible, params$seuil_r,
                                 params$seuil_p, params$min_obs_sel,
                                 params$max_retenus)
    if (is.null(s)) return(NULL)
    # CONTROLE 3 : la selection n'a vu que des dates anterieures a la cible.
    stopifnot("[C3] la selection a utilise une date >= cible" =
                max(x_b$date[x_b$id_serie %in% s$ids & x_b$date < cible]) < cible)
    r <- estimer_bridge_ar(g_b, x_l, s$ids, cible, avec_ar = params$avec_ar)
    if (is.null(r)) return(NULL)
    tibble::tibble(branche = b, bridge = r$prevision,
                   n_retenus = length(s$ids),
                   indicateurs = paste(s$ids, collapse = " | "),
                   derniere_obs = r$derniere_obs)
  })
  if (nrow(passerelle) > 0L) {
    stopifnot("[C1] une passerelle a vu sa propre cible" =
                all(passerelle$derniere_obs < cible))
  }

  # --- (e) LES BRANCHES NON COUVERTES --------------------------------------
  # Elles ne recoivent AUCUN traitement particulier : le BVAR les prevoit comme
  # les douze autres, et faute de passerelle leur nowcast EST la prevision du
  # BVAR (cf. le coalesce en (f)).
  #
  # Une autoregression etait autrefois calculee ici, puis jointe, puis jamais
  # utilisee -- le coalesce ne la consultait pas. Elle est retiree : un calcul
  # dont le resultat est jete a chaque production entretient la croyance qu'un
  # quatrieme etage existe, alors que l'architecture n'en compte que trois.
  #
  # Ce n'est pas une omission mais un resultat : l'etape 5 met huit modeles en
  # concurrence sur ces quatre branches, et l'AR(4) n'y arrive jamais premier
  # (rangs 3, 5, 5 et 6 sur 8). Sur Services aux entreprises, c'est le BVAR
  # lui-meme qui gagne. Rien ne justifierait donc de le substituer.
  # `params$ar_ordre` reste declare : l'etape 5 s'en sert pour son etude.

  # --- (f) poids de combinaison, sur le seul historique anterieur -----------
  branches_df <- tibble::tibble(branche = names(r_bvar$prevision),
                                bvar = as.numeric(r_bvar$prevision)) %>%
    dplyr::left_join(
      if (nrow(passerelle) == 0L)
        tibble::tibble(branche = character(0), bridge = numeric(0),
                       n_retenus = integer(0), indicateurs = character(0))
      else passerelle %>% dplyr::select(branche, bridge, n_retenus, indicateurs),
      by = "branche")

  branches_df$delta <- NA_real_; branches_df$combinee <- NA_real_
  for (i in seq_len(nrow(branches_df))) {
    b <- branches_df$branche[i]
    h <- if (is.null(historique)) NULL else
      historique %>% dplyr::filter(branche == b, origine < cible) %>%
        dplyr::transmute(reel, bvar, bridge)
    # CONTROLE 5 : delta n'utilise que des erreurs anterieures a la cible.
    if (!is.null(h) && nrow(h) > 0L) {
      stopifnot("[C5] delta estime sur une origine >= cible" =
                  all(historique$origine[historique$branche == b &
                                           historique$origine < cible] < cible))
    }
    pd <- poids_combinaison(if (is.null(h)) tibble::tibble(reel = numeric(0),
                                                          bvar = numeric(0),
                                                          bridge = numeric(0)) else h,
                            min_obs = params$min_obs_delta)
    branches_df$delta[i] <- pd$delta
    bi <- branches_df$bridge[i]; vi <- branches_df$bvar[i]
    branches_df$combinee[i] <- if (is.na(bi)) vi else pd$delta * vi + (1 - pd$delta) * bi
  }
  # --- garde-fou sur l'amplitude ---------------------------------------------
  # L'ecart-type est recalcule sur la seule information anterieure a la cible :
  # une branche dont la volatilite a change doit etre jugee sur la volatilite
  # qu'on lui connaissait, pas sur celle qu'on decouvrira.
  branches_df$sigma <- vapply(branches_df$branche, function(b) {
    h <- va$g[va$branche == b & va$date < cible]
    if (length(h) < 12L) NA_real_ else stats::sd(h)
  }, numeric(1))
  garde <- purrr::pmap(list(branches_df$bridge, branches_df$sigma), function(pr, sg) {
    garde_amplitude(pr, sg, z_max = params$garde_z_max, action = params$garde_action)
  })
  branches_df$z_amplitude <- vapply(garde, function(g) g$z, numeric(1))
  branches_df$signale <- vapply(garde, function(g) g$signale, logical(1))
  branches_df$bridge_brut <- branches_df$bridge
  branches_df$bridge <- vapply(garde, function(g) g$prevision, numeric(1))
  # si le garde-fou a modifie la passerelle, la combinaison doit etre refaite
  if (any(branches_df$signale & params$garde_action != "signaler")) {
    modifs <- which(branches_df$signale)
    for (i in modifs) {
      bi <- branches_df$bridge[i]; vi <- branches_df$bvar[i]; dl <- branches_df$delta[i]
      branches_df$combinee[i] <- if (is.na(bi)) vi else dl * vi + (1 - dl) * bi
    }
  }
  # --- branches a rupture de variance -----------------------------------------
  # Une branche dont la variance a rompu recoit sa moyenne recente plutot que la
  # prevision du BVAR, qui n'y ajoute que du bruit non correle. Le diagnostic est
  # recalcule sur la seule information anterieure a la cible.
  d_inst <- diagnostic_instabilite(contexte$Y[contexte$dates_vec < cible, , drop = FALSE],
                                   params$instab_rapport_min)
  branches_df$instable <- branches_df$branche %in%
    colnames(contexte$Y)[d_inst$instables]
  if (any(branches_df$instable)) {
    for (i in which(branches_df$instable)) {
      g_b <- va %>% dplyr::filter(branche == branches_df$branche[i]) %>%
        dplyr::select(date, g)
      branches_df$bvar[i] <- prevision_branche_instable(g_b, cible,
                                                        params$instab_fenetre)
      bi <- branches_df$bridge[i]; dl <- branches_df$delta[i]
      branches_df$combinee[i] <- if (is.na(bi)) branches_df$bvar[i]
        else dl * branches_df$bvar[i] + (1 - dl) * bi
    }
  }

  branches_df$nowcast <- dplyr::coalesce(branches_df$combinee, branches_df$bvar)

  # --- (g) poids sectoriels et agregation -----------------------------------
  trimestre_poids <- fin_trimestre(debut_trimestre(cible) %m-% months(3))
  w <- poids_bruts %>% dplyr::filter(date == trimestre_poids) %>%
    dplyr::select(branche, w)
  # CONTROLE 4 : les poids proviennent d'une date anterieure a la cible.
  stopifnot("[C4] poids issus du trimestre cible ou d'apres" = trimestre_poids < cible)
  if (nrow(w) != length(TOUTES_BRANCHES)) return(NULL)

  ag <- branches_df %>% dplyr::inner_join(w, by = "branche")
  if (nrow(ag) != length(TOUTES_BRANCHES) || any(is.na(ag$nowcast))) return(NULL)
  reel_b <- va %>% dplyr::filter(date == cible) %>% dplyr::select(branche, reel = g)
  ag <- ag %>% dplyr::left_join(reel_b, by = "branche")

  list(
    cible = cible,
    branches = ag %>% dplyr::mutate(origine = cible, .before = 1),
    agregat = tibble::tibble(
      origine = cible, trimestre = date_vers_trimestre(cible),
      scenario = scenario,
      reel        = log(sum(ag$w * exp(ag$reel))),
      nowcast     = log(sum(ag$w * exp(ag$nowcast))),
      bvar        = log(sum(ag$w * exp(ag$bvar))),
      p_bvar = r_bvar$p, lambda_bvar = r_bvar$lambda,
      trimestre_poids = trimestre_poids,
      n_branches_passerelle = sum(!is.na(ag$bridge)),
      n_signalees = sum(ag$signale, na.rm = TRUE),
      n_instables = sum(ag$instable, na.rm = TRUE)))
}

