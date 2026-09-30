# ============================================================================
# fonctions/passerelle.R -- Phase 4 : agregation, selection, bridge, delta
# ============================================================================
# Plan de correction : phases 5 (selection recursive), 6 (agregation mensuelle
# vers trimestrielle), 7 (equations bridge) et 10 (poids de combinaison).
#
# Tout ce qui est ici obeit a la meme regle que la phase 3 : pour une cible T,
# rien de ce qui entre dans le calcul ne peut dater d'apres l'information
# autorisee. La difference avec la phase 3 tient a ce que les indicateurs sont
# disponibles AVANT la valeur ajoutee : un bridge utilise donc X_T pour prevoir
# g_T, ce qui n'est pas du look-ahead mais la raison d'etre de la methode.
#
# Convention du projet : toutes les dates sont au DERNIER jour de leur periode.
# ============================================================================

# ----------------------------------------------------------------------------
# 1) AGREGATION MENSUELLE -> TRIMESTRIELLE  (phase 6 du plan)
# ----------------------------------------------------------------------------

#' Agrege une serie mensuelle en serie trimestrielle.
#'
#' La regle depend de la NATURE ECONOMIQUE de la variable, pas d'une convention
#' uniforme. La version 1 appliquait `mean(dlog)` a tout, ce qui est faux pour
#' un flux : la production trimestrielle est la SOMME des trois mois, pas leur
#' moyenne, et la moyenne des croissances mensuelles n'est pas la croissance de
#' la somme.
#'
#'   sum   flux    -- production, ventes, nuitees, credits distribues
#'   mean  stock   -- effectifs, encours, indices, taux
#'   last  stock   -- niveau de fin de periode, quand seul le point final compte
#'
#' TRIMESTRE INCOMPLET : la fonction renvoie NA des qu'un des trois mois manque.
#' C'est un refus deliberé, et non une commodite. Sommer deux mois au lieu de
#' trois sous-estime le trimestre d'un tiers ; en moyenner deux introduit un
#' biais des que la serie est saisonniere. Dans les deux cas l'erreur est
#' SILENCIEUSE : la serie agregee garde une allure plausible, et le biais
#' ressort en bout de chaine sans qu'on puisse le rattacher a sa cause. Les
#' extremites manquantes du trimestre courant relevent des scenarios M0/M1/M2
#' (phase 13), ou les mois absents sont PREVUS, pas ignores.
#'
#' @param dates vecteur de dates, en fin de mois.
#' @param valeurs vecteur numerique de meme longueur.
#' @param agregation "sum", "mean" ou "last".
#' @param exiger_complet si TRUE (defaut), un trimestre incomplet vaut NA.
#' @return data.frame date (fin de trimestre), valeur, n_mois.
agreger_trimestriel <- function(dates, valeurs, agregation = "mean",
                                exiger_complet = TRUE) {
  stopifnot(length(dates) == length(valeurs))
  agregation <- match.arg(agregation, c("sum", "mean", "last"))

  garde <- !is.na(dates) & !is.na(valeurs)
  if (!any(garde)) {
    return(tibble::tibble(date = as.Date(character(0)),
                          valeur = numeric(0), n_mois = integer(0)))
  }
  d <- as.Date(dates[garde]); v <- as.numeric(valeurs[garde])
  ordre <- order(d); d <- d[ordre]; v <- v[ordre]

  cle <- fin_trimestre(d)
  out <- tibble::tibble(date = cle, valeur = v) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      n_mois = dplyr::n(),
      valeur = switch(agregation,
                      sum  = sum(valeur),
                      mean = mean(valeur),
                      last = dplyr::last(valeur)),
      .groups = "drop") %>%
    dplyr::arrange(date)

  if (exiger_complet) {
    out <- out %>% dplyr::mutate(valeur = ifelse(n_mois < 3L, NA_real_, valeur))
  }
  out %>% dplyr::select(date, valeur, n_mois)
}

#' Construit la base trimestrielle des indicateurs d'une branche.
#'
#' Les series mensuelles sont agregees selon leur metadonnee ; les series deja
#' trimestrielles sont reprises telles quelles. La transformation (dlog, diff,
#' niveau) est ensuite appliquee AU NIVEAU TRIMESTRIEL -- jamais au niveau
#' mensuel avant agregation, ce qui reviendrait a moyenner des croissances.
#'
#' @param ind base longue issue de `charger_indicateurs()`.
#' @param tolerant si TRUE, une serie dont la transformation echoue est ECARTEE
#'   au lieu d'interrompre le calcul. A n'activer que la ou des valeurs sont
#'   FABRIQUEES -- prevision des mois manquants en phase 13 : une prevision
#'   bornee peut rendre un agregat nul et faire echouer dlog. Sur des donnees
#'   observees, l'echec signale une metadonnee fausse et doit rester bloquant.
#' @return id_serie, branche, indicateur, date, x (valeur transformee), n_mois.
indicateurs_trimestriels <- function(ind, tolerant = FALSE) {
  ind %>%
    dplyr::group_by(id_serie, branche, indicateur, frequence,
                    agregation, transformation) %>%
    dplyr::group_modify(function(g, cle) {
      agg <- if (cle$frequence == "mensuel") {
        agreger_trimestriel(g$date, g$valeur, cle$agregation)
      } else {
        tibble::tibble(date = fin_trimestre(g$date), valeur = g$valeur,
                       n_mois = 3L)
      }
      agg <- dplyr::arrange(agg, date)
      x <- if (tolerant) {
        tryCatch(transformer_serie(agg$valeur, cle$transformation, agg$date,
                                   "trimestriel"),
                 error = function(e) rep(NA_real_, nrow(agg)))
      } else {
        transformer_serie(agg$valeur, cle$transformation, agg$date, "trimestriel")
      }
      agg %>% dplyr::mutate(x = x) %>%
        dplyr::select(date, valeur, x, n_mois)
    }) %>%
    dplyr::ungroup()
}

# ----------------------------------------------------------------------------
# 2) SELECTION RECURSIVE DES INDICATEURS  (phase 5 du plan)
# ----------------------------------------------------------------------------

#' Selectionne les indicateurs d'une branche sur la seule information anterieure.
#'
#' Criteres du plan : correlation contemporaine entre l'indicateur et la
#' croissance de la branche, retenue si
#'
#'     |r| >= SEUIL_R   et   p < SEUIL_P
#'
#' Deux garde-fous s'y ajoutent, absents du plan mais necessaires ici.
#'
#'   (a) HISTORIQUE MINIMAL. Une correlation calculee sur huit trimestres n'est
#'       pas une correlation : son ecart-type vaut environ 1/sqrt(n-3), soit
#'       0,45 pour n = 8. Le seuil |r| >= 0,15 serait alors franchi par pur
#'       bruit une fois sur deux. On exige donc `min_obs` trimestres apparies.
#'
#'   (b) BORNE SUR LE NOMBRE DE RETENUS. Un bridge estime par MCO sur une
#'       cinquantaine de trimestres ne supporte pas vingt regresseurs. On garde
#'       les `max_retenus` meilleures correlations en valeur absolue.
#'
#' @param g_branche data.frame date, g -- croissance de la VA de la branche.
#' @param x_branche data.frame id_serie, date, x -- indicateurs transformes.
#' @param target_date trimestre cible : seules les dates STRICTEMENT
#'   anterieures servent a selectionner.
#' @return data.frame id_serie, n, correlation, p_value, retenu, rang.
selectionner_indicateurs <- function(g_branche, x_branche, target_date,
                                     seuil_r = 0.15, seuil_p = 0.10,
                                     min_obs = 20L, max_retenus = 5L) {
  gg <- g_branche %>% dplyr::filter(date < target_date)
  xx <- x_branche  %>% dplyr::filter(date < target_date)
  if (nrow(gg) == 0L || nrow(xx) == 0L) return(NULL)

  stats <- xx %>%
    dplyr::inner_join(gg, by = "date") %>%
    dplyr::filter(!is.na(x), !is.na(g)) %>%
    dplyr::group_by(id_serie) %>%
    dplyr::summarise(
      n = dplyr::n(),
      correlation = if (dplyr::n() >= 4L && stats::sd(x) > 0 && stats::sd(g) > 0) {
        suppressWarnings(stats::cor(x, g))
      } else NA_real_,
      p_value = if (dplyr::n() >= 4L && stats::sd(x) > 0 && stats::sd(g) > 0) {
        suppressWarnings(stats::cor.test(x, g)$p.value)
      } else NA_real_,
      .groups = "drop")

  stats <- stats %>%
    dplyr::mutate(
      eligible = n >= min_obs & !is.na(correlation) &
                 abs(correlation) >= seuil_r & p_value < seuil_p) %>%
    dplyr::arrange(dplyr::desc(eligible), dplyr::desc(abs(correlation)))

  stats %>%
    dplyr::mutate(rang = dplyr::row_number(),
                  retenu = eligible & rang <= max_retenus) %>%
    dplyr::select(id_serie, n, correlation, p_value, eligible, rang, retenu)
}

# ----------------------------------------------------------------------------
# 3) EQUATION BRIDGE  (phase 7 du plan)
# ----------------------------------------------------------------------------

#' Estime et applique une equation de passerelle pour une cible.
#'
#'     g_{j,T} = alpha_j + sum_k beta_k X_{k,T} + eps_{j,T}
#'
#' Les coefficients sont estimes sur t < T ; la prevision utilise X_{.,T}, qui
#' est disponible avant g_{j,T}. C'est precisement ce qui fait l'interet d'un
#' bridge, et ce n'est pas du look-ahead : aucune valeur de la VA a la date T
#' n'intervient.
#'
#' @return liste prevision, n_obs, n_indicateurs, r2_ajuste, derniere_obs.
estimer_bridge <- function(g_branche, x_large, ids, target_date) {
  if (length(ids) == 0L) return(NULL)

  base <- g_branche %>%
    dplyr::inner_join(x_large, by = "date") %>%
    dplyr::arrange(date)

  train <- base %>%
    dplyr::filter(date < target_date) %>%
    dplyr::filter(!is.na(g), dplyr::if_all(dplyr::all_of(ids), ~ !is.na(.)))
  cible <- base %>%
    dplyr::filter(date == target_date) %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(ids), ~ !is.na(.)))

  # Il faut assez de degres de liberte, et la ligne cible doit exister.
  if (nrow(train) < length(ids) + 6L || nrow(cible) != 1L) return(NULL)

  # Barriere anti-look-ahead : redondante avec le filtre ci-dessus, et c'est
  # voulu. Le jour ou quelqu'un modifie le filtre, l'erreur doit etre bruyante.
  stopifnot("[ANTI-LOOK-AHEAD] bridge : la cible est dans l'echantillon" =
              max(train$date) < target_date)

  fo  <- stats::as.formula(paste("g ~", paste(sprintf("`%s`", ids), collapse = " + ")))
  ajust <- stats::lm(fo, data = train)
  pred  <- stats::predict(ajust, newdata = cible)

  list(prevision = unname(pred[1]),
       n_obs = nrow(train), n_indicateurs = length(ids),
       r2_ajuste = summary(ajust)$adj.r.squared,
       derniere_obs = max(train$date))
}

# ----------------------------------------------------------------------------
# 4) POIDS DE COMBINAISON  (phase 10 du plan)
# ----------------------------------------------------------------------------

#' Poids recursif entre BVAR et bridge.
#'
#'     g_hat = delta * g_BVAR + (1 - delta) * g_bridge
#'
#'     delta_{j,T} = argmin_{delta dans [0,1]} sum_{t<T} [ g_{j,t} - g_hat_t(delta) ]^2
#'
#' La somme des carres est quadratique en delta, donc l'optimum interieur est
#' explicite. En notant e1 = g - g_BVAR et e2 = g - g_bridge les erreurs des
#' deux composantes sur l'historique, l'erreur combinee vaut
#' delta*e1 + (1-delta)*e2, et l'annulation de la derivee donne
#'
#'     delta* = sum( e2 * (e2 - e1) ) / sum( (e2 - e1)^2 )
#'
#' que l'on tronque a [0, 1]. Pas d'optimisation numerique : la solution est
#' fermee, donc exacte et instantanee.
#'
#' Si `constante` n'est pas NA, elle court-circuite tout ce qui precede : le
#' poids vaut cette valeur, sans estimation. C'est le reglage retenu par le
#' projet (DELTA_CONSTANT = 0,5, voir R/00_setup.R). Les scripts qui doivent
#' COMPARER les regles -- R/14_robustesse.R -- passent explicitement
#' `constante = NA` pour retrouver le delta estime.
#'
#' @param hist data.frame date, reel, bvar, bridge -- historique t < T.
#' @param min_obs nombre minimal de trimestres apparies exige.
#' @param constante poids impose, ou NA pour l'estimer.
#' @return liste delta, n_obs, mode ("constant", "estime",
#'   "interieur_impossible", "defaut").
poids_combinaison <- function(hist, min_obs = 8L, delta_defaut = 0.5,
                              constante = if (exists("DELTA_CONSTANT"))
                                            DELTA_CONSTANT else NA_real_) {
  h <- hist %>%
    dplyr::filter(!is.na(reel), !is.na(bvar), !is.na(bridge))
  if (length(constante) == 1L && !is.na(constante)) {
    stopifnot("delta doit etre dans [0, 1]" = constante >= 0 && constante <= 1)
    return(list(delta = constante, n_obs = nrow(h), mode = "constant"))
  }
  if (nrow(h) < min_obs) {
    return(list(delta = delta_defaut, n_obs = nrow(h), mode = "defaut"))
  }
  e1 <- h$reel - h$bvar      # erreur du BVAR
  e2 <- h$reel - h$bridge    # erreur du bridge
  den <- sum((e2 - e1)^2)
  if (!is.finite(den) || den < .Machine$double.eps) {
    return(list(delta = delta_defaut, n_obs = nrow(h),
                mode = "interieur_impossible"))
  }
  d <- sum(e2 * (e2 - e1)) / den
  list(delta = min(1, max(0, d)), n_obs = nrow(h), mode = "estime")
}

# ============================================================================
# 5) VARIANTES DE LA PASSERELLE  (pistes d'amelioration, phase 4 bis)
# ============================================================================
# La version de base perd 165 origines-branches sur 576 pour une raison
# purement algorithmique : elle classe les series par correlation SEULE, sans
# verifier qu'elles sont observables au trimestre cible ni qu'elles le sont
# CONJOINTEMENT. L'estimation fait ensuite une suppression par liste sur les
# cinq series retenues, si bien qu'une seule serie trouee annule l'equation.
# Les fonctions ci-dessous levent cette contrainte et ouvrent quatre autres
# pistes, toutes departagees hors echantillon.
# ----------------------------------------------------------------------------

#' Decale une base d'indicateurs d'un trimestre : la valeur datee T-1 devient
#' un regresseur disponible a la date T. Sert a tester les correlations
#' RETARDEES -- un indicateur qui precede la branche est invisible pour un
#' critere purement contemporain.
retarder_indicateurs <- function(x_b, retard = 1L) {
  if (retard == 0L) return(x_b)
  x_b %>%
    dplyr::mutate(date = fin_trimestre(
      debut_trimestre(date) %m+% months(3L * retard)))
}

#' Selection consciente de la DISPONIBILITE.
#'
#' Trois differences avec `selectionner_indicateurs()` :
#'
#'   (a) une serie non observee au trimestre cible n'est pas candidate. Elle ne
#'       pourrait de toute facon pas servir a prevoir T ;
#'   (b) le set est construit de proche en proche, du plus correle au moins
#'       correle, et une serie n'est ajoutee QUE SI elle laisse assez de lignes
#'       d'entrainement conjointes. Une serie trouee est donc ecartee au profit
#'       de la suivante, au lieu d'annuler l'equation ;
#'   (c) le nombre de regresseurs s'adapte : on s'arrete des qu'ajouter une
#'       serie de plus couterait plus de lignes qu'elle n'apporte de signal.
#'
#' @return liste ids (colonnes de x_large) et table de selection, ou NULL.
selectionner_disponible <- function(g_branche, x_long, x_large, target_date,
                                    seuil_r = 0.15, seuil_p = 0.10,
                                    min_obs = 20L, max_retenus = 5L,
                                    marge_ddl = 6L) {
  gg <- g_branche %>% dplyr::filter(date < target_date)
  if (nrow(gg) == 0L) return(NULL)

  # (a) candidates : observees au trimestre cible
  ligne_cible <- x_large %>% dplyr::filter(date == target_date)
  if (nrow(ligne_cible) != 1L) return(NULL)
  valeurs_cible <- unlist(ligne_cible[1, ])
  observables <- setdiff(names(ligne_cible)[!is.na(valeurs_cible)], "date")
  if (length(observables) == 0L) return(NULL)

  stats_sel <- x_long %>%
    dplyr::filter(id_serie %in% observables, date < target_date) %>%
    dplyr::inner_join(gg, by = "date") %>%
    dplyr::filter(!is.na(x), !is.na(g)) %>%
    dplyr::group_by(id_serie) %>%
    dplyr::summarise(
      n = dplyr::n(),
      correlation = if (dplyr::n() >= 4L && stats::sd(x) > 0 && stats::sd(g) > 0)
        suppressWarnings(stats::cor(x, g)) else NA_real_,
      p_value = if (dplyr::n() >= 4L && stats::sd(x) > 0 && stats::sd(g) > 0)
        suppressWarnings(stats::cor.test(x, g)$p.value) else NA_real_,
      .groups = "drop") %>%
    dplyr::filter(n >= min_obs, !is.na(correlation),
                  abs(correlation) >= seuil_r, p_value < seuil_p) %>%
    dplyr::arrange(dplyr::desc(abs(correlation)))
  if (nrow(stats_sel) == 0L) return(NULL)

  # (b)-(c) construction gloutonne sous contrainte de lignes conjointes
  base <- g_branche %>% dplyr::inner_join(x_large, by = "date") %>%
    dplyr::filter(date < target_date, !is.na(g))
  retenus <- character(0)
  for (k in stats_sel$id_serie) {
    essai <- c(retenus, k)
    n_joint <- sum(stats::complete.cases(base[, essai, drop = FALSE]))
    if (n_joint >= length(essai) + marge_ddl) retenus <- essai
    if (length(retenus) >= max_retenus) break
  }
  if (length(retenus) == 0L) return(NULL)
  list(ids = retenus,
       table = stats_sel %>% dplyr::mutate(retenu = id_serie %in% retenus))
}

#' Passerelle, avec terme autoregressif optionnel.
#'
#'     g_{j,T} = alpha + phi g_{j,T-1} + sum_k beta_k X_{k,T} + eps
#'
#' Le terme AR n'est pas un detail : la passerelle de base ignore toute la
#' persistance de la branche, que le BVAR exploite de son cote. Les deux
#' composantes de la combinaison portent alors des informations disjointes et
#' le poids delta n'a presque rien a arbitrer.
estimer_bridge_ar <- function(g_branche, x_large, ids, target_date,
                              avec_ar = FALSE, marge_ddl = 6L) {
  if (length(ids) == 0L) return(NULL)
  gb <- g_branche %>% dplyr::arrange(date) %>%
    dplyr::mutate(g_ret = dplyr::lag(g))
  base <- gb %>% dplyr::inner_join(x_large, by = "date") %>% dplyr::arrange(date)

  reg <- if (avec_ar) c("g_ret", ids) else ids
  if (!all(ids %in% names(x_large))) return(NULL)
  ok <- stats::complete.cases(base[, reg[reg %in% names(base)], drop = FALSE])
  train <- base[ok & base$date < target_date & !is.na(base$g), , drop = FALSE]

  # LA LIGNE CIBLE NE PEUT PAS VENIR D'UNE JOINTURE AVEC LA VA.
  # En backtest, la VA du trimestre cible existe et la jointure la trouve. Dans
  # un VRAI nowcast elle n'est pas encore publiee -- c'est la definition meme de
  # l'exercice -- et la jointure ne rend rien : la passerelle echouait alors
  # silencieusement pour toutes les branches, et le systeme retombait sur le
  # BVAR sans qu'aucun message ne le signale.
  #
  # La ligne cible est donc CONSTRUITE : les indicateurs viennent de x_large a
  # la date cible, et le terme autoregressif du trimestre PRECEDENT, qui lui est
  # connu. C'est ce que fait `estimer_bridge_ridge` depuis l'origine.
  ligne_x <- x_large[x_large$date == target_date, , drop = FALSE]
  if (nrow(ligne_x) != 1L) return(NULL)
  cible <- ligne_x[, c("date", ids), drop = FALSE]
  if (avec_ar) {
    g_prec <- gb$g[gb$date == trimestre_precedent(target_date)]
    if (length(g_prec) != 1L || is.na(g_prec)) return(NULL)
    cible$g_ret <- g_prec
  }
  if (nrow(train) < length(reg) + marge_ddl) return(NULL)
  if (!all(stats::complete.cases(cible[, reg, drop = FALSE]))) return(NULL)

  stopifnot("[ANTI-LOOK-AHEAD] bridge : la cible est dans l'echantillon" =
              max(train$date) < target_date)

  fo <- stats::as.formula(paste("g ~", paste(sprintf("`%s`", reg), collapse = " + ")))
  aj <- stats::lm(fo, data = train)
  list(prevision = unname(stats::predict(aj, newdata = cible)[1]),
       n_obs = nrow(train), n_indicateurs = length(ids),
       r2_ajuste = summary(aj)$adj.r.squared, derniere_obs = max(train$date))
}

#' Composantes principales au lieu d'une selection dure.
#'
#' La selection jette 80 series pour en garder 5. Une reduction de dimension
#' les garde toutes et resume leur information commune. Trois gains : le set
#' n'est jamais vide, il n'y a plus de suppression par liste sur cinq series a
#' la fois, et la colinearite entre series d'une meme branche -- forte, puisque
#' ce sont souvent des postes d'un meme agregat -- cesse d'etre un probleme.
#'
#' Le panneau doit etre RECTANGULAIRE pour que l'ACP ait un sens. On prend donc
#' les series observees au trimestre cible, puis on ne garde que celles qui
#' couvrent entierement la fenetre `largeur`. Centrage et reduction se font sur
#' la seule information ANTERIEURE a la cible, et la ligne cible est projetee
#' avec ces memes moments -- jamais recalcules en l'incluant.
composantes_principales <- function(x_large, target_date, k = 3L,
                                    largeur = 28L, min_series = 3L) {
  ligne_cible <- x_large %>% dplyr::filter(date == target_date)
  if (nrow(ligne_cible) != 1L) return(NULL)
  valeurs_cible <- unlist(ligne_cible[1, ])
  observables <- setdiff(names(ligne_cible)[!is.na(valeurs_cible)], "date")
  if (length(observables) < min_series) return(NULL)

  passe <- x_large %>% dplyr::filter(date < target_date) %>% dplyr::arrange(date)
  if (nrow(passe) < 12L) return(NULL)
  fen <- utils::tail(passe, largeur)

  complets <- observables[vapply(observables,
                                 function(cc) all(!is.na(fen[[cc]])), logical(1))]
  if (length(complets) < min_series) return(NULL)

  M <- as.matrix(fen[, complets, drop = FALSE])
  centre <- colMeans(M); echelle <- apply(M, 2, stats::sd)
  garde <- echelle > 0
  if (sum(garde) < min_series) return(NULL)
  M <- M[, garde, drop = FALSE]
  centre <- centre[garde]; echelle <- echelle[garde]
  Ms <- scale(M, center = centre, scale = echelle)

  acp <- stats::prcomp(Ms, center = FALSE, scale. = FALSE)
  k_eff <- min(k, ncol(acp$rotation))
  noms <- paste0("CP", seq_len(k_eff))

  xc <- as.numeric(unlist(ligne_cible[1, names(centre)]))
  proj <- matrix((xc - centre) / echelle, nrow = 1) %*%
    acp$rotation[, seq_len(k_eff), drop = FALSE]

  scores <- as.data.frame(acp$x[, seq_len(k_eff), drop = FALSE])
  names(scores) <- noms
  scores$date <- fen$date
  cible_sc <- as.data.frame(proj); names(cible_sc) <- noms
  cible_sc$date <- target_date

  list(donnees = tibble::as_tibble(dplyr::bind_rows(scores, cible_sc)),
       ids = noms, n_series = ncol(M))
}

# ----------------------------------------------------------------------------
# 6) PASSERELLE CONTRACTEE (RIDGE)  -- piste 6
# ----------------------------------------------------------------------------
# CE QUE LES VARIANTES PRECEDENTES ONT MONTRE
#   Les variantes les plus riches en indicateurs ont la MEILLEURE correlation
#   prevu-realise (0,27 pour le fonds commun avec retards) et le PIRE ratio
#   (1,32). Ce couple n'est pas celui d'un modele aveugle : c'est celui d'un
#   modele qui voit juste et amplifie trop. L'information supplementaire est
#   reelle, c'est l'estimateur des moindres carres qui est trop lache pour elle.
#
#   La reponse n'est donc pas de selectionner plus durement -- ce qui jette
#   l'information -- mais de CONTRACTER les coefficients. C'est exactement le
#   raisonnement du prior de Minnesota de la phase 3, transpose a la passerelle.
#
# LE MODELE
#   Sur regresseurs centres-reduits et cible centree :
#
#       b(lambda) = ( X'X + lambda I )^{-1} X'y
#
#   lambda = 0 redonne les MCO ; lambda -> l'infini annule tous les
#   coefficients. Entre les deux, on dose. La matrice devient inversible meme
#   quand X'X ne l'est pas, ce qui autorise PLUS de regresseurs que
#   d'observations -- le fonds commun devient utilisable.
#
# LE CHOIX DE LAMBDA
#   Par validation croisee generalisee (Golub, Heath & Wahba, 1979), qui a une
#   forme close et n'exige aucun decoupage de l'echantillon :
#
#       GCV(lambda) = (1/n) ||y - X b(lambda)||^2 / ( 1 - tr(H_lambda)/n )^2
#
#   avec H_lambda = X ( X'X + lambda I )^{-1} X' la matrice chapeau. La trace
#   tr(H) est le nombre EFFECTIF de parametres : elle vaut k quand lambda = 0 et
#   tend vers 0 quand lambda croit. Le denominateur penalise donc la complexite,
#   exactement comme la vraisemblance marginale le fait en phase 3.
#
#   Tout se calcule sur les seules donnees t < T. En decomposant X = U D V' en
#   valeurs singulieres, les deux quantites s'obtiennent pour toute la grille de
#   lambda sans reinverser la moindre matrice.
#
# LE TERME AUTOREGRESSIF N'EST PAS PENALISE
#   g_{j,T-1} entre dans l'equation sans contraction : c'est le seul regresseur
#   dont on sait deja, par la phase 3, qu'il porte du signal. Le contracter vers
#   zero comme un indicateur quelconque reviendrait a defaire ce que le terme
#   AR apporte -- et c'est lui qui a produit le seul gain de qualite mesure.

#' Panneau rectangulaire de regresseurs disponibles au trimestre cible.
#'
#' Une ridge a besoin d'une matrice complete. On prend les series observees en
#' T, puis on ne garde que celles qui couvrent entierement la fenetre retenue.
#' C'est la meme logique que pour l'ACP, et elle remplace la suppression par
#' liste sur cinq series : ici le critere est explicite et se mesure.
panneau_disponible <- function(x_large, target_date, largeur = 32L,
                               min_series = 2L, min_obs = 12L) {
  ligne_cible <- x_large %>% dplyr::filter(date == target_date)
  if (nrow(ligne_cible) != 1L) return(NULL)
  valeurs <- unlist(ligne_cible[1, ])
  observables <- setdiff(names(ligne_cible)[!is.na(valeurs)], "date")
  if (length(observables) < min_series) return(NULL)

  passe <- x_large %>% dplyr::filter(date < target_date) %>% dplyr::arrange(date)
  if (nrow(passe) < min_obs) return(NULL)
  fen <- utils::tail(passe, largeur)

  complets <- observables[vapply(observables,
                                 function(cc) all(!is.na(fen[[cc]])), logical(1))]
  if (length(complets) < min_series) return(NULL)
  list(ids = complets, passe = fen, cible = ligne_cible)
}

#' Passerelle estimee par ridge, lambda choisi par GCV.
#'
#' @param penaliser_ar si FALSE (defaut), le terme AR echappe a la contraction.
#' @param lambda_grille grille de recherche, en echelle logarithmique.
#' @return liste prevision, lambda, ddl_effectifs, n_obs, n_indicateurs,
#'   derniere_obs.
estimer_bridge_ridge <- function(g_branche, x_large, target_date,
                                 avec_ar = TRUE, penaliser_ar = FALSE,
                                 largeur = 32L, min_series = 2L,
                                 lambda_grille = 10^seq(-4, 4, length.out = 60)) {
  pan <- panneau_disponible(x_large, target_date, largeur = largeur,
                            min_series = min_series)
  if (is.null(pan)) return(NULL)

  gb <- g_branche %>% dplyr::arrange(date) %>% dplyr::mutate(g_ret = dplyr::lag(g))

  train <- pan$passe %>% dplyr::inner_join(gb, by = "date") %>%
    dplyr::filter(!is.na(g)) %>% dplyr::arrange(date)
  if (avec_ar) train <- train %>% dplyr::filter(!is.na(g_ret))
  if (nrow(train) < 10L) return(NULL)

  cible_ar <- gb$g[gb$date == trimestre_precedent(target_date)]
  if (avec_ar && length(cible_ar) != 1L) return(NULL)

  stopifnot("[ANTI-LOOK-AHEAD] ridge : la cible est dans l'echantillon" =
              max(train$date) < target_date)

  X  <- as.matrix(train[, pan$ids, drop = FALSE])
  xc <- as.numeric(unlist(pan$cible[1, pan$ids]))

  # centrage-reduction sur les seules donnees d'entrainement
  mu <- colMeans(X); sd_ <- apply(X, 2, stats::sd)
  garde <- is.finite(sd_) & sd_ > 0
  if (sum(garde) < min_series) return(NULL)
  X <- X[, garde, drop = FALSE]; mu <- mu[garde]; sd_ <- sd_[garde]
  xc <- xc[garde]
  Xs <- scale(X, center = mu, scale = sd_)
  xs <- (xc - mu) / sd_

  y   <- train$g
  y_m <- mean(y)
  yc  <- y - y_m

  # Le terme AR, non penalise, est projete hors de l'espace penalise : on
  # regresse d'abord y et chaque colonne de X sur [1, g_ret], et la ridge porte
  # sur les residus. C'est la facon exacte de laisser un regresseur libre.
  if (avec_ar && !penaliser_ar) {
    Z  <- cbind(1, train$g_ret)
    zc <- c(1, cible_ar)
    P  <- Z %*% MASS::ginv(t(Z) %*% Z) %*% t(Z)
    ry <- as.numeric(yc - P %*% yc)
    RX <- Xs - P %*% Xs
    coef_z <- MASS::ginv(t(Z) %*% Z) %*% t(Z) %*% cbind(yc, Xs)
    base_cible <- as.numeric(zc %*% coef_z)      # partie expliquee par [1, AR]
    y_util <- ry; X_util <- RX
    x_util <- xs - base_cible[-1]
    decalage <- y_m + base_cible[1]
    n_libre <- ncol(Z)     # constante + terme AR : non penalises mais CONSOMMES
  } else {
    reg_sup <- if (avec_ar) cbind(train$g_ret) else NULL
    if (!is.null(reg_sup)) {
      Xs <- cbind(g_ret = scale(reg_sup)[, 1], Xs)
      xs <- c((cible_ar - mean(reg_sup)) / stats::sd(reg_sup), xs)
    }
    y_util <- yc; X_util <- Xs; x_util <- xs; decalage <- y_m
    n_libre <- 1L          # la constante, absorbee par le centrage de y
  }

  n <- nrow(X_util)
  dec <- svd(X_util)
  d <- dec$d; Uty <- as.numeric(t(dec$u) %*% y_util)

  # GCV sur toute la grille, sans reinversion : tout passe par les d_i^2.
  gcv <- vapply(lambda_grille, function(lam) {
    filtre <- d^2 / (d^2 + lam)
    ddl    <- sum(filtre) + n_libre
    resid  <- sum((Uty * (1 - filtre))^2) + (sum(y_util^2) - sum(Uty^2))
    den    <- (1 - ddl / n)^2
    if (!is.finite(den) || den <= 1e-10) return(Inf)
    (resid / n) / den
  }, numeric(1))
  if (all(!is.finite(gcv))) return(NULL)
  lam <- lambda_grille[which.min(gcv)]

  b <- dec$v %*% ((d / (d^2 + lam)) * Uty)
  pred <- decalage + sum(x_util * as.numeric(b))
  ddl  <- sum(d^2 / (d^2 + lam)) + n_libre

  list(prevision = pred, lambda = lam, ddl_effectifs = ddl,
       n_obs = n, n_indicateurs = ncol(X_util), derniere_obs = max(train$date))
}


# ----------------------------------------------------------------------------
# 7) GARDE-FOU SUR L'AMPLITUDE DES PREVISIONS DE BRANCHE
# ----------------------------------------------------------------------------
# CE QUI A MOTIVE CE GARDE-FOU, ET CE QUE LA MESURE A CORRIGE
#   Le nowcast de T2-2026 donne -24,7 % sur la peche, ce qui parait aberrant.
#   Rapporte a la volatilite de la branche -- ecart-type trimestriel de 17,5 % --
#   cela ne fait que z = 1,41, et les cinq indicateurs retenus pointent tous a la
#   baisse entre -0,3 et -1,7 ecart-type. Ce n'est donc PAS une aberration : la
#   peche est simplement une branche tres volatile. Juger une prevision sur son
#   pourcentage brut, sans normaliser, induit en erreur.
#
#   Le garde-fou ne porte donc pas sur le pourcentage mais sur
#
#       z = | g_prevu | / sigma_{j, t<T}
#
#   ou sigma est recalcule a chaque origine sur la seule information anterieure.
#
# CE QUE LES DONNEES DISENT, ET C'EST NUANCE
#   Sur les 325 previsions du backtest, 9 seulement depassent z = 2, et 2
#   depassent z = 5. Ces deux-la sont l'hebergement-restauration en 2020 :
#
#     T2-2020  z = 17,5   realise -85,7 %   passerelle -81,7 %   SPECTACULAIREMENT JUSTE
#     T3-2020  z = 10,2   realise +15,3 %   passerelle +104,4 %  spectaculairement faux
#
#   Plafonner a 5 sigma ameliore la passerelle SEULE de 0,950 a 0,924 : en erreur
#   quadratique, corriger le rebond de T3 rapporte plus que ne coute la
#   degradation de l'effondrement de T2. Mais la COMBINAISON, qui est ce que le
#   systeme utilise reellement, reste a 0,977 quel que soit le plafond -- delta
#   amortit deja les ecarts.
#
#   Et un plafond a 10 sigma DEGRADE (0,978), parce qu'il ne rattrape que le bon
#   cas sans toucher au mauvais.
#
# CE QUE CE GARDE-FOU EST, ET CE QU'IL N'EST PAS
#   Ce n'est PAS une amelioration de la precision : sur la combinaison, il ne
#   change rien. C'est une PROTECTION OPERATIONNELLE. En backtest, une prevision
#   aberrante se voit contre sa realisation ; sur le trimestre courant, elle
#   partirait dans l'agregat publie sans que rien ne la contredise.
#
#   Il faut en assumer le cout : plafonner AURAIT ABIME le meilleur resultat du
#   projet, la prevision de -81,7 % contre -85,7 % realise au T2-2020. C'est
#   pourquoi le mode par defaut est le SIGNALEMENT, pas le plafonnement.

#' Evalue et, si demande, borne l'amplitude d'une prevision de branche.
#'
#' @param prevision prevision de croissance (en log-difference).
#' @param sigma ecart-type de la branche, estime sur t < T uniquement.
#' @param z_max seuil au-dela duquel la prevision est signalee.
#' @param action "signaler" (defaut) laisse la valeur intacte et la marque ;
#'   "plafonner" la ramene a sign(prevision) * z_max * sigma ;
#'   "ecarter" la remplace par NA, ce qui fait retomber la branche sur le BVAR.
#' @return liste prevision (eventuellement modifiee), z, signale, prevision_brute.
garde_amplitude <- function(prevision, sigma, z_max = 5,
                            action = c("signaler", "plafonner", "ecarter")) {
  # Un parametre absent ne doit pas faire tomber le calcul : on retombe sur le
  # comportement le plus conservateur, qui est de ne rien modifier.
  if (is.null(z_max) || !is.finite(z_max)) z_max <- 5
  if (is.null(action) || length(action) == 0L) action <- "signaler"
  action <- match.arg(action[1], c("signaler", "plafonner", "ecarter"))
  if (is.na(prevision) || is.na(sigma) || !is.finite(sigma) || sigma <= 0) {
    return(list(prevision = prevision, z = NA_real_, signale = FALSE,
                prevision_brute = prevision))
  }
  z <- abs(prevision) / sigma
  signale <- z > z_max
  ajustee <- prevision
  if (signale) {
    ajustee <- switch(action,
                      signaler   = prevision,
                      plafonner  = sign(prevision) * z_max * sigma,
                      ecarter    = NA_real_)
  }
  list(prevision = ajustee, z = z, signale = signale,
       prevision_brute = prevision)
}
