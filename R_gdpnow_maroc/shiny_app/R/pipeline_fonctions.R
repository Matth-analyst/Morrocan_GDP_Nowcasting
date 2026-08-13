# ============================================================================
# pipeline_fonctions.R -- Toute la logique de modelisation, sous forme de
#                          fonctions reutilisables (appelees par l'app Shiny
#                          a chaque ajout de donnees / demande de recalcul).
# ============================================================================
# Repris et refactorise depuis les scripts R/01 a R/07 du pipeline batch
# (dossier R_gdpnow_maroc/R/). Meme methode, memes references :
#   Higgins (2014) ; Banbura, Giannone & Reichlin (2010) ; Litterman (1986) ;
#   Diebold & Mariano (1995).
# ============================================================================

suppressMessages({
  library(readxl); library(dplyr); library(tidyr); library(purrr)
  library(stringr); library(lubridate); library(forecast); library(tseries)
})

BRANCHES_COUVERTES <- c("Pêche", "Industrie d'extraction", "Industrie de transformation",
  "Électricité, gaz, eau", "Immobilier", "Hébergement-restauration", "Finances et assurances")
BRANCHES_NON_COUVERTES <- c("Agriculture", "Construction", "Commerce", "Transports",
  "Information-communication", "Administration publique", "Éducation-santé",
  "Services aux entreprises", "Autres services")
TOUTES_BRANCHES <- c(BRANCHES_COUVERTES, BRANCHES_NON_COUVERTES)

# ----------------------------------------------------------------------------
# 1) IMPORT
# ----------------------------------------------------------------------------
parse_trimestre <- function(x) {
  m <- str_match(x, "^T([1-4])-(\\d{4})$")
  if (is.na(m[1,1])) return(as.Date(NA))
  t <- as.integer(m[1,2]); y <- as.integer(m[1,3])
  as.Date(sprintf("%d-%02d-01", y, (t - 1) * 3 + 1))
}
parse_mois <- function(x) suppressWarnings(as.Date(paste0(x, "-01"), format = "%Y-%m-%d"))

lire_feuille_branche <- function(chemin, nom_feuille) {
  raw <- as.data.frame(read_excel(chemin, sheet = nom_feuille, col_names = FALSE))
  source_cible <- raw[2, 1]

  cible <- tibble(
    trimestre_lbl = as.character(raw[6:nrow(raw), 1]),
    va = suppressWarnings(as.numeric(raw[6:nrow(raw), 2]))
  ) %>%
    filter(!is.na(trimestre_lbl), !is.na(va)) %>%
    mutate(date = map_vec(trimestre_lbl, parse_trimestre)) %>%
    filter(!is.na(date)) %>% arrange(date) %>%
    transmute(branche = nom_feuille, date, va)

  resultat <- list(cible = cible, source_cible = source_cible, indicateurs = NULL)
  ncols <- ncol(raw)
  if (ncols < 4) return(resultat)

  indicateurs_liste <- list(); col_date_courante <- NULL; type_date_courante <- NULL
  for (c in 4:ncols) {
    lbl4 <- as.character(raw[4, c]); lbl3 <- as.character(raw[3, c])
    if (!is.na(lbl4) && lbl4 %in% c("Mois", "Trimestre")) {
      col_date_courante <- c
      type_date_courante <- if (lbl4 == "Mois") "mensuel" else "trimestriel"
      next
    }
    if (!is.na(lbl3) && str_detect(lbl3, "^Indicateurs")) next
    if (is.null(col_date_courante) || is.na(lbl3) || lbl3 == "") next

    valeurs <- suppressWarnings(as.numeric(raw[6:nrow(raw), c]))
    dates_lbl <- as.character(raw[6:nrow(raw), col_date_courante])
    if (all(is.na(valeurs))) next
    dates <- if (type_date_courante == "mensuel") map_vec(dates_lbl, parse_mois) else map_vec(dates_lbl, parse_trimestre)

    r_extrait <- str_match(lbl3, "\\(r=([-+]?[0-9.]+)\\)")[1,2]
    nom_propre <- str_trim(str_remove(lbl3, "\\s*\\(r=[-+]?[0-9.]+\\)"))
    signe_atypique <- str_detect(nom_propre, "\\[signe atypique\\]")
    nom_propre <- str_trim(str_remove(nom_propre, "\\s*\\[signe atypique\\]"))

    df_ind <- tibble(date = dates, valeur = valeurs) %>%
      filter(!is.na(date), !is.na(valeur)) %>% arrange(date) %>%
      transmute(branche = nom_feuille, indicateur = nom_propre, source = as.character(raw[4, c]),
                frequence = type_date_courante, r_declare = as.numeric(r_extrait),
                signe_atypique = signe_atypique, date, valeur)
    indicateurs_liste[[length(indicateurs_liste) + 1]] <- df_ind
  }
  resultat$indicateurs <- if (length(indicateurs_liste) > 0) bind_rows(indicateurs_liste) else NULL
  resultat
}

#' Importe l'integralite du classeur (cibles + indicateurs, toutes branches)
importer_donnees <- function(chemin_classeur) {
  brut <- map(TOUTES_BRANCHES, ~ lire_feuille_branche(chemin_classeur, .x))
  names(brut) <- TOUTES_BRANCHES
  sources_cibles <- tibble(branche = TOUTES_BRANCHES,
                            source = map_chr(brut, ~ as.character(.x$source_cible %||% NA)))
  list(cibles = map_dfr(brut, "cible"), indicateurs = map_dfr(brut, "indicateurs"),
       sources_cibles = sources_cibles)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

#' Tableau de synthese des sources des variables cibles (une ligne par branche)
tableau_sources_cibles <- function(cibles, sources_cibles) {
  cibles %>% group_by(branche) %>%
    summarise(debut = min(date), fin = max(date), n = n(), .groups = "drop") %>%
    left_join(sources_cibles, by = "branche") %>%
    mutate(couverte = branche %in% BRANCHES_COUVERTES) %>%
    arrange(match(branche, TOUTES_BRANCHES))
}

#' Tableau de synthese des sources des indicateurs infra-annuels (une ligne par serie)
tableau_sources_indicateurs <- function(indicateurs) {
  if (is.null(indicateurs) || nrow(indicateurs) == 0) return(tibble())
  indicateurs %>%
    group_by(branche, indicateur, source, frequence, signe_atypique) %>%
    summarise(debut = min(date), fin = max(date), n = n(), .groups = "drop") %>%
    arrange(match(branche, TOUTES_BRANCHES), indicateur)
}

#' Fusionne les donnees importees avec des observations ajoutees manuellement
#' via l'application (memes formats de table que cibles/indicateurs).
fusionner_donnees_ajoutees <- function(cibles, indicateurs, cibles_ajoutees, indicateurs_ajoutes) {
  if (!is.null(cibles_ajoutees) && nrow(cibles_ajoutees) > 0) {
    cibles <- bind_rows(cibles, cibles_ajoutees) %>%
      distinct(branche, date, .keep_all = TRUE) %>% arrange(branche, date)
  }
  if (!is.null(indicateurs_ajoutes) && nrow(indicateurs_ajoutes) > 0) {
    indicateurs <- bind_rows(indicateurs, indicateurs_ajoutes) %>%
      distinct(branche, indicateur, date, .keep_all = TRUE) %>% arrange(branche, indicateur, date)
  }
  list(cibles = cibles, indicateurs = indicateurs)
}

# ----------------------------------------------------------------------------
# 2) ANALYSE EXPLORATOIRE
# ----------------------------------------------------------------------------
ajouter_dlog <- function(cibles) {
  cibles %>% arrange(branche, date) %>% group_by(branche) %>%
    mutate(dlog_va = c(NA, diff(log(pmax(va, 1e-6))))) %>% ungroup()
}

tester_stationnarite <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 12) return(c(p_niveau = NA, p_dlog = NA))
  p_niveau <- tryCatch(adf.test(x, k = 4)$p.value, error = function(e) NA)
  dlog <- diff(log(pmax(x[x > 0], 1e-6)))
  p_dlog <- tryCatch(adf.test(dlog, k = 4)$p.value, error = function(e) NA)
  c(p_niveau = p_niveau, p_dlog = p_dlog)
}

analyser_stationnarite <- function(cibles) {
  cibles %>% group_by(branche) %>%
    summarise(res = list(tester_stationnarite(va)), .groups = "drop") %>%
    mutate(p_niveau = map_dbl(res, "p_niveau"), p_dlog = map_dbl(res, "p_dlog")) %>%
    select(-res) %>%
    mutate(stationnaire_niveau = p_niveau < 0.05, stationnaire_dlog = p_dlog < 0.05)
}

indicateurs_vers_trimestriel <- function(indicateurs) {
  if (is.null(indicateurs) || nrow(indicateurs) == 0) return(tibble())
  indicateurs %>%
    group_by(branche, indicateur, source, r_declare, signe_atypique) %>%
    group_modify(~ {
      df <- .x %>% arrange(date)
      if (unique(.x$frequence)[1] == "mensuel") {
        df <- df %>% mutate(trimestre = floor_date(date, "quarter")) %>%
          group_by(trimestre) %>% summarise(valeur = mean(valeur, na.rm = TRUE)) %>%
          rename(date = trimestre)
      }
      df %>% arrange(date) %>% mutate(dlog = c(NA, diff(log(pmax(valeur, 1e-6)))))
    }) %>% ungroup() %>% filter(!is.na(dlog))
}

# ----------------------------------------------------------------------------
# 3) BVAR TRIMESTRIEL (prior Minnesota, observations fictives)
# ----------------------------------------------------------------------------
construire_matrice_Y <- function(cibles) {
  mat <- cibles %>% select(branche, date, dlog_va) %>%
    pivot_wider(names_from = branche, values_from = dlog_va) %>% arrange(date) %>%
    filter(if_all(everything(), ~ !is.na(.)))
  branches_dispo <- intersect(TOUTES_BRANCHES, names(mat))
  list(dates = mat$date, Y = as.matrix(mat[, branches_dispo]))
}

estimer_bvar <- function(Y, p = 5, lambda = 0.15) {
  n <- ncol(Y); Tn <- nrow(Y)
  sigma_i <- sapply(1:n, function(i) {
    fit <- tryCatch(ar.ols(Y[, i], order.max = p, aic = FALSE, demean = TRUE), error = function(e) NULL)
    if (is.null(fit)) sd(Y[, i]) else sqrt(fit$var.pred)
  })
  Y_reg <- Y[(p+1):Tn, , drop = FALSE]
  X_reg <- matrix(1, nrow = Tn - p, ncol = 1 + n * p)
  for (l in 1:p) X_reg[, (2+(l-1)*n):(1+l*n)] <- Y[(p+1-l):(Tn-l), , drop = FALSE]

  Yd1 <- matrix(0, n*p, n); Xd1 <- matrix(0, n*p, 1+n*p)
  for (l in 1:p) { rows <- ((l-1)*n+1):(l*n); Xd1[rows, (2+(l-1)*n):(1+l*n)] <- diag(sigma_i*l/lambda) }
  Yd2 <- matrix(0, n, n); diag(Yd2) <- sigma_i; Xd2 <- matrix(0, n, 1+n*p)
  Yd3 <- matrix(0, 1, n); Xd3 <- matrix(0, 1, 1+n*p); Xd3[1,1] <- 1e-5

  Y_aug <- rbind(Y_reg, Yd1, Yd2, Yd3); X_aug <- rbind(X_reg, Xd1, Xd2, Xd3)
  B <- solve(t(X_aug) %*% X_aug) %*% t(X_aug) %*% Y_aug

  x_new <- matrix(c(1, as.vector(t(Y[Tn:(Tn-p+1), , drop=FALSE]))), nrow = 1)
  prevision <- setNames(as.vector(x_new %*% B), colnames(Y))
  list(B = B, prevision = prevision, p = p, lambda = lambda, sigma_i = sigma_i)
}

# ----------------------------------------------------------------------------
# 4) BRIDGE EQUATIONS (branches couvertes) + combinaison avec le BVAR
# ----------------------------------------------------------------------------
bridge_un_indicateur <- function(cible_df, indic_df) {
  df <- inner_join(cible_df, indic_df, by = "date") %>% filter(!is.na(dlog_va), !is.na(dlog))
  if (nrow(df) < 10) return(NULL)
  fit <- lm(dlog_va ~ dlog, data = df)
  ar1 <- tryCatch(Arima(indic_df$dlog[!is.na(indic_df$dlog)], order = c(1,0,0)), error = function(e) NULL)
  dlog_prevu <- if (!is.null(ar1)) as.numeric(forecast(ar1, h=1)$mean) else tail(indic_df$dlog, 1)
  list(prevision = as.numeric(predict(fit, newdata = data.frame(dlog = dlog_prevu))),
       ajuste = fitted(fit), dates_ajuste = df$date[!is.na(df$dlog_va) & !is.na(df$dlog)],
       r2 = summary(fit)$r.squared, n = nrow(df))
}

estimer_bridge_equations <- function(cibles, indic_trim, prevision_bvar, p_bvar = 5) {
  resultats <- list()
  for (b in intersect(BRANCHES_COUVERTES, unique(cibles$branche))) {
    cible_b <- cibles %>% filter(branche == b) %>% select(date, dlog_va)
    indics_b <- indic_trim %>% filter(branche == b) %>% distinct(indicateur) %>% pull(indicateur)
    if (length(indics_b) == 0) next

    previsions_indiv <- c(); ajustes_liste <- list()
    for (ind_nom in indics_b) {
      indic_df <- indic_trim %>% filter(branche == b, indicateur == ind_nom) %>% select(date, dlog)
      res <- bridge_un_indicateur(cible_b, indic_df)
      if (!is.null(res)) {
        previsions_indiv[ind_nom] <- res$prevision
        ajustes_liste[[ind_nom]] <- tibble(date = res$dates_ajuste, ajuste = res$ajuste)
      }
    }
    if (length(previsions_indiv) == 0) next
    prevision_bridge <- mean(previsions_indiv, na.rm = TRUE)

    ajuste_moyen <- bind_rows(ajustes_liste, .id = "indicateur") %>%
      group_by(date) %>% summarise(bridge = mean(ajuste, na.rm = TRUE))

    df_comb <- cible_b %>% inner_join(ajuste_moyen, by = "date")
    serie_dlog <- cible_b$dlog_va[!is.na(cible_b$dlog_va)]
    dates_dlog <- cible_b$date[!is.na(cible_b$dlog_va)]
    ar_branche <- tryCatch(Arima(serie_dlog, order = c(p_bvar, 0, 0)), error = function(e) NULL)
    if (!is.null(ar_branche)) {
      df_comb$bvar_ajuste <- as.numeric(fitted(ar_branche))[match(df_comb$date, dates_dlog)]
    } else df_comb$bvar_ajuste <- NA
    df_comb <- df_comb %>% filter(!is.na(bvar_ajuste), !is.na(bridge))

    if (nrow(df_comb) >= 5) {
      grille <- seq(0, 1, 0.01)
      sse <- sapply(grille, function(d) sum((df_comb$dlog_va - (d*df_comb$bvar_ajuste + (1-d)*df_comb$bridge))^2))
      delta_opt <- grille[which.min(sse)]
    } else delta_opt <- 0.5

    prevision_bvar_b <- if (b %in% names(prevision_bvar)) prevision_bvar[b] else NA
    prevision_finale <- if (!is.na(prevision_bvar_b)) {
      delta_opt * prevision_bvar_b + (1 - delta_opt) * prevision_bridge
    } else prevision_bridge

    resultats[[b]] <- list(previsions_individuelles = previsions_indiv, prevision_bridge = prevision_bridge,
                            delta = delta_opt, prevision_bvar = prevision_bvar_b,
                            prevision_finale = prevision_finale, nb_indicateurs = length(previsions_indiv))
  }
  resultats
}

# ----------------------------------------------------------------------------
# 5) AR(4) -- branches non couvertes
# ----------------------------------------------------------------------------
estimer_ar4 <- function(cibles, branches = BRANCHES_NON_COUVERTES) {
  resultats <- list()
  for (b in intersect(branches, unique(cibles$branche))) {
    serie <- cibles %>% filter(branche == b) %>% arrange(date) %>% pull(dlog_va)
    serie <- serie[!is.na(serie)]
    if (length(serie) < 10) next
    fit <- tryCatch(Arima(serie, order = c(4, 0, 0)), error = function(e) NULL)
    if (is.null(fit)) next
    prev <- forecast(fit, h = 1)
    resultats[[b]] <- list(prevision = as.numeric(prev$mean),
                            ic80_bas = as.numeric(prev$lower[,"80%"]),
                            ic80_haut = as.numeric(prev$upper[,"80%"]))
  }
  resultats
}

# ----------------------------------------------------------------------------
# 6) AGREGATION FISHER (approximation, cf. limites documentees)
# ----------------------------------------------------------------------------
agreger_nowcast <- function(cibles, resultats_bridge, resultats_ar4) {
  dernier_trim <- cibles %>% group_by(branche) %>% filter(date == max(date)) %>% ungroup() %>% select(branche, va)
  poids <- dernier_trim %>% mutate(part = va / sum(va))

  prev_couvertes <- if (length(resultats_bridge) > 0) {
    tibble(branche = names(resultats_bridge), prevision = sapply(resultats_bridge, `[[`, "prevision_finale"))
  } else tibble(branche = character(), prevision = numeric())
  prev_non_couvertes <- if (length(resultats_ar4) > 0) {
    tibble(branche = names(resultats_ar4), prevision = sapply(resultats_ar4, `[[`, "prevision"))
  } else tibble(branche = character(), prevision = numeric())

  previsions <- bind_rows(prev_couvertes, prev_non_couvertes) %>%
    left_join(poids, by = "branche") %>% mutate(contribution = part * prevision)

  list(previsions = previsions, croissance_pib = sum(previsions$contribution, na.rm = TRUE))
}

# ----------------------------------------------------------------------------
# 7) PIPELINE COMPLET (appelable en un seul clic depuis l'app)
# ----------------------------------------------------------------------------
executer_pipeline_complet <- function(cibles, indicateurs, p_bvar = 5, lambda = 0.15) {
  cibles <- ajouter_dlog(cibles)
  indic_trim <- indicateurs_vers_trimestriel(indicateurs)
  stationnarite <- analyser_stationnarite(cibles)

  my <- construire_matrice_Y(cibles)
  bvar <- estimer_bvar(my$Y, p = p_bvar, lambda = lambda)

  bridge <- estimer_bridge_equations(cibles, indic_trim, bvar$prevision, p_bvar = p_bvar)
  ar4 <- estimer_ar4(cibles)
  nowcast <- agreger_nowcast(cibles, bridge, ar4)

  prochain_trimestre <- max(my$dates) %m+% months(3)

  list(cibles = cibles, indicateurs = indicateurs, indic_trim = indic_trim,
       stationnarite = stationnarite, bvar = bvar, bridge = bridge, ar4 = ar4,
       nowcast = nowcast, date_prevision = prochain_trimestre,
       date_maj = Sys.time())
}

# ----------------------------------------------------------------------------
# 8) VALIDATION HORS ECHANTILLON (backtest simplifie, reutilise pour l'app)
# ----------------------------------------------------------------------------
backtest_pseudo_temps_reel <- function(cibles, indicateurs, h_test = 8, p = 5, lambda = 0.15) {
  cibles <- ajouter_dlog(cibles)
  indic_trim <- indicateurs_vers_trimestriel(indicateurs)
  my <- construire_matrice_Y(cibles)
  Y_complet <- my$Y; dates_vec <- my$dates; Tn <- nrow(Y_complet)
  if (Tn <= h_test + p + 5) return(NULL)

  dernier_trim <- cibles %>% group_by(branche) %>% filter(date == max(date)) %>% ungroup() %>% select(branche, va)
  poids <- setNames(dernier_trim$va / sum(dernier_trim$va), dernier_trim$branche)
  poids <- poids[colnames(Y_complet)]

  erreurs_modele <- c(); erreurs_ar2 <- c(); dates_test <- dates_vec[(Tn-h_test+1):Tn]

  for (h in seq_along(dates_test)) {
    idx <- Tn - h_test + h - 1
    date_limite <- dates_vec[idx]
    Y_tr <- Y_complet[1:idx, , drop = FALSE]
    bvar_tr <- estimer_bvar(Y_tr, p, lambda)

    prev_branches <- c()
    for (b in colnames(Y_complet)) {
      if (b %in% BRANCHES_COUVERTES) {
        cible_b <- cibles %>% filter(branche == b) %>% select(date, dlog_va)
        indics_b <- indic_trim %>% filter(branche == b)
        pb <- NA
        if (nrow(indics_b) > 0) {
          cible_tr <- cible_b %>% filter(date <= date_limite, !is.na(dlog_va))
          prevs <- c()
          for (ind_nom in unique(indics_b$indicateur)) {
            ind_df <- indics_b %>% filter(indicateur == ind_nom, date <= date_limite) %>% select(date, dlog)
            df <- inner_join(cible_tr, ind_df, by = "date") %>% filter(!is.na(dlog))
            if (nrow(df) < 10) next
            fit <- lm(dlog_va ~ dlog, data = df)
            ar1 <- tryCatch(Arima(ind_df$dlog[!is.na(ind_df$dlog)], order=c(1,0,0)), error=function(e) NULL)
            dprev <- if (!is.null(ar1)) as.numeric(forecast(ar1,h=1)$mean) else tail(ind_df$dlog,1)
            prevs[ind_nom] <- as.numeric(predict(fit, newdata=data.frame(dlog=dprev)))
          }
          if (length(prevs) > 0) pb <- mean(prevs, na.rm = TRUE)
        }
        prev_branches[b] <- if (is.na(pb)) bvar_tr$prevision[b] else 0.5*bvar_tr$prevision[b] + 0.5*pb
      } else {
        fit_ar4 <- tryCatch(Arima(Y_tr[, b], order = c(4,0,0)), error = function(e) NULL)
        prev_branches[b] <- if (is.null(fit_ar4)) bvar_tr$prevision[b] else as.numeric(forecast(fit_ar4,h=1)$mean)
      }
    }
    prevision_pib <- sum(prev_branches[names(poids)] * poids)
    reel_pib <- sum(Y_complet[idx+1, names(poids)] * poids)
    erreurs_modele[h] <- reel_pib - prevision_pib

    pib_total_tr <- as.vector(Y_tr %*% poids[colnames(Y_tr)])
    fit_ar2 <- Arima(pib_total_tr, order = c(2,0,0))
    erreurs_ar2[h] <- reel_pib - as.numeric(forecast(fit_ar2, h=1)$mean)
  }

  rmsfe_modele <- sqrt(mean(erreurs_modele^2)); rmsfe_ar2 <- sqrt(mean(erreurs_ar2^2))
  test_dm <- tryCatch(dm.test(erreurs_modele, erreurs_ar2, h = 1, power = 2), error = function(e) NULL)

  list(dates_test = dates_test, erreurs_modele = erreurs_modele, erreurs_ar2 = erreurs_ar2,
       rmsfe_modele = rmsfe_modele, rmsfe_ar2 = rmsfe_ar2, test_dm = test_dm)
}
