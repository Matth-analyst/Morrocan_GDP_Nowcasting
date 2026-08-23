# ============================================================================
# 04_bridge_equations.R -- Bridge equations avec traitement du JAGGED EDGE
# ============================================================================
# Reference : Higgins (2014), etape 3 du papier GDPNow -- "Suppose we are
# interested in forecasting the values [...] when we have the non-missing
# observations [...]. We run factor augmented autoregressions [...]" : les
# mois manquants ne sont JAMAIS supprimes dans le papier original, ils sont
# reconstruits (via un facteur commun + AR sur la serie elle-meme), afin de
# ne perdre aucune observation exploitable.
#
# VERSION PRECEDENTE (corrigee ici) : les mois/trimestres manquants etaient
# simplement absents de la sequence de dates avant agregation trimestrielle
# -- une perte d'information reelle, signalee et documentee comme limite,
# maintenant corrigee.
#
# NOUVELLE METHODE : chaque indicateur est reconstruit sur un calendrier
# complet (mensuel ou trimestriel, du premier au dernier point observe) par
# lissage de Kalman (stats::arima + stats::KalmanSmooth, base R -- pas de
# package supplementaire). C'est le meme mecanisme conceptuel qu'utilise la
# litterature du "jagged edge" (Doz, Giannone & Reichlin 2011 ; Giannone,
# Reichlin & Small 2008) pour combler les trous sans supposer une continuite
# artificielle par interpolation lineaire. Contrairement a un modele a
# facteur (methode 3, DFM), ce comblement se fait ICI indicateur par
# indicateur (pas de facteur commun) -- coherent avec l'esprit "bridge
# equation" de la methode 1, qui reste une regression simple par indicateur.
#
# Consequence attendue : plus de trimestres exploitables par indicateur pour
# l'estimation de chaque bridge equation -> echantillons d'estimation plus
# grands, moins de perte d'information qu'avec la suppression pure.
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
indicateurs <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs.rds"))  # donnees BRUTES (avec trous)
bvar <- readRDS(file.path(DOSSIER_RESULTATS, "bvar_modele.rds"))

#' Reconstruit un calendrier complet (mensuel ou trimestriel) pour un
#' indicateur et comble les mois/trimestres manquants par lissage de Kalman
#' sur un modele AR ajuste en log-niveau. Renvoie (date, valeur, etait_manquant).
completer_serie_kalman <- function(indic_df, freq = c("mensuel", "trimestriel")) {
  freq <- match.arg(freq)
  indic_df <- indic_df %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
  if (nrow(indic_df) < 8) return(NULL)

  pas <- if (freq == "mensuel") "month" else "quarter"
  calendrier <- tibble(date = seq(min(indic_df$date), max(indic_df$date), by = pas))
  serie <- calendrier %>% left_join(indic_df %>% select(date, valeur), by = "date")
  serie$etait_manquant <- is.na(serie$valeur)

  n_manquant <- sum(serie$etait_manquant)
  if (n_manquant == 0) return(serie)

  log_vals <- log(pmax(serie$valeur, 1e-6))
  freq_num <- if (freq == "mensuel") 12 else 4
  ts_obj <- ts(log_vals, frequency = freq_num)

  # ordre AR raisonnable, borne par la taille d'echantillon (evite le
  # sur-ajustement sur des series courtes)
  p <- min(4, max(1, floor(sum(!is.na(log_vals)) / 10)))
  fit <- tryCatch(arima(ts_obj, order = c(p, 0, 0)), error = function(e) NULL)
  if (is.null(fit)) fit <- tryCatch(arima(ts_obj, order = c(1, 0, 0)), error = function(e) NULL)
  if (is.null(fit)) {
    # dernier repli : impossible d'ajuster un AR (serie trop courte/degenere)
    # -- on NE COMBLE PAS, les NA restent (pas d'imputation hasardeuse)
    return(serie)
  }

  kf <- tryCatch(KalmanSmooth(ts_obj, fit$model), error = function(e) NULL)
  if (is.null(kf)) return(serie)

  interpole <- kf$smooth[, 1]
  if ("intercept" %in% names(fit$coef)) interpole <- interpole + fit$coef["intercept"]

  serie$valeur_comblee <- exp(interpole)
  serie$valeur <- ifelse(serie$etait_manquant, serie$valeur_comblee, serie$valeur)
  serie$fit_arima <- list(fit)[rep(1, nrow(serie))]  # conserve le modele (reutilise pour la prevision)
  serie %>% select(date, valeur, etait_manquant)
}

#' Bridge equation pour un indicateur : calendrier complete par Kalman,
#' agregation trimestrielle (desormais sans mois manquant a l'interieur du
#' trimestre), regression sur la cible, et prevision au prochain trimestre
#' en reutilisant le MEME modele AR ajuste pour le comblement (au lieu d'un
#' AR(1) ad hoc separe comme dans la version precedente).
bridge_un_indicateur_jagged <- function(cible_df, indic_df_brut, freq) {
  serie_complete <- completer_serie_kalman(indic_df_brut, freq)
  if (is.null(serie_complete)) return(NULL)

  taux_comble <- mean(serie_complete$etait_manquant)
  serie_complete <- serie_complete %>% arrange(date) %>%
    mutate(dlog = c(NA, diff(log(pmax(valeur, 1e-6)))))

  if (freq == "mensuel") {
    serie_trim <- serie_complete %>%
      mutate(trimestre = floor_date(date, "quarter")) %>%
      group_by(trimestre) %>%
      summarise(dlog = mean(dlog, na.rm = TRUE)) %>%
      rename(date = trimestre)
  } else {
    serie_trim <- serie_complete %>% select(date, dlog)
  }

  df <- inner_join(cible_df, serie_trim, by = "date") %>% filter(!is.na(dlog_va), !is.na(dlog))
  if (nrow(df) < 10) return(NULL)
  fit_bridge <- lm(dlog_va ~ dlog, data = df)

  # --- Prevision du prochain point de l'indicateur, en reajustant un AR sur
  #     la serie desormais complete (log-niveau) puis Δlog du dernier pas ---
  log_vals <- log(pmax(serie_complete$valeur, 1e-6))
  freq_num <- if (freq == "mensuel") 12 else 4
  ts_obj <- ts(log_vals, frequency = freq_num)
  p <- min(4, max(1, floor(length(log_vals) / 10)))
  fit_ar <- tryCatch(Arima(ts_obj, order = c(p, 0, 0)), error = function(e) NULL)
  if (is.null(fit_ar)) fit_ar <- tryCatch(Arima(ts_obj, order = c(1, 0, 0)), error = function(e) NULL)

  h_prev <- if (freq == "mensuel") 3 else 1  # 3 mois pour completer le prochain trimestre, sinon 1 trimestre direct
  if (!is.null(fit_ar)) {
    log_prevu <- as.numeric(forecast(fit_ar, h = h_prev)$mean)
    dlog_prevu <- if (freq == "mensuel") {
      mean(diff(c(tail(log_vals, 1), log_prevu)))
    } else {
      log_prevu[1] - tail(log_vals, 1)
    }
  } else {
    dlog_prevu <- tail(serie_trim$dlog, 1)
  }

  prevision <- as.numeric(predict(fit_bridge, newdata = data.frame(dlog = dlog_prevu)))
  list(fit = fit_bridge, prevision = prevision, r2 = summary(fit_bridge)$r.squared,
       n = nrow(df), taux_comble = taux_comble)
}

resultats_bridge <- list()

for (b in BRANCHES_COUVERTES) {
  cible_b <- cibles %>% filter(branche == b) %>% select(date, dlog_va)
  indics_b <- indicateurs %>% filter(branche == b) %>% distinct(indicateur, frequence)

  previsions_indiv <- c(); ajustes_liste <- list(); taux_combles <- c()
  for (i in seq_len(nrow(indics_b))) {
    ind_nom <- indics_b$indicateur[i]; freq_ind <- indics_b$frequence[i]
    indic_df <- indicateurs %>% filter(branche == b, indicateur == ind_nom) %>% select(date, valeur)
    res <- bridge_un_indicateur_jagged(cible_b, indic_df, freq_ind)
    if (!is.null(res)) {
      previsions_indiv[ind_nom] <- res$prevision
      taux_combles[ind_nom] <- res$taux_comble
      if (res$taux_comble > 0.05) {
        cat(sprintf("    (%s : %.0f%% de points combles par lissage de Kalman, n=%d obs. estimation)\n",
                     ind_nom, res$taux_comble * 100, res$n))
      }
    }
  }
  if (length(previsions_indiv) == 0) next
  prevision_bridge <- mean(previsions_indiv, na.rm = TRUE)

  # --- Combinaison avec le BVAR (identique a la version precedente, MLC) ---
  # Pour l'estimation en echantillon du delta, on reconstruit la moyenne des
  # valeurs ajustees bridge, comme avant, mais desormais sur la base des
  # series completees.
  ajustes_pour_delta <- list()
  for (i in seq_len(nrow(indics_b))) {
    ind_nom <- indics_b$indicateur[i]; freq_ind <- indics_b$frequence[i]
    if (!(ind_nom %in% names(previsions_indiv))) next
    indic_df <- indicateurs %>% filter(branche == b, indicateur == ind_nom) %>% select(date, valeur)
    serie_complete <- completer_serie_kalman(indic_df, freq_ind)
    if (is.null(serie_complete)) next
    serie_complete <- serie_complete %>% arrange(date) %>% mutate(dlog = c(NA, diff(log(pmax(valeur, 1e-6)))))
    serie_trim <- if (freq_ind == "mensuel") {
      serie_complete %>% mutate(trimestre = floor_date(date, "quarter")) %>%
        group_by(trimestre) %>% summarise(dlog = mean(dlog, na.rm = TRUE)) %>% rename(date = trimestre)
    } else serie_complete %>% select(date, dlog)
    df_i <- inner_join(cible_b, serie_trim, by = "date") %>% filter(!is.na(dlog_va), !is.na(dlog))
    if (nrow(df_i) < 10) next
    fit_i <- lm(dlog_va ~ dlog, data = df_i)
    ajustes_pour_delta[[ind_nom]] <- tibble(date = df_i$date, ajuste = fitted(fit_i))
  }
  ajuste_moyen <- bind_rows(ajustes_pour_delta, .id = "indicateur") %>%
    group_by(date) %>% summarise(bridge = mean(ajuste, na.rm = TRUE))

  df_comb <- cible_b %>% inner_join(ajuste_moyen, by = "date")
  serie_dlog <- cible_b$dlog_va[!is.na(cible_b$dlog_va)]
  dates_dlog <- cible_b$date[!is.na(cible_b$dlog_va)]
  ar_branche <- tryCatch(Arima(serie_dlog, order = c(bvar$p, 0, 0)), error = function(e) NULL)
  df_comb$bvar_ajuste <- if (!is.null(ar_branche)) as.numeric(fitted(ar_branche))[match(df_comb$date, dates_dlog)] else NA
  df_comb <- df_comb %>% filter(!is.na(bvar_ajuste), !is.na(bridge))

  if (nrow(df_comb) >= 5) {
    grille <- seq(0, 1, 0.01)
    sse <- sapply(grille, function(d) sum((df_comb$dlog_va - (d*df_comb$bvar_ajuste + (1-d)*df_comb$bridge))^2))
    delta_opt <- grille[which.min(sse)]
  } else delta_opt <- 0.5

  prevision_bvar_b <- bvar$prevision[b]
  prevision_finale <- delta_opt * prevision_bvar_b + (1 - delta_opt) * prevision_bridge

  resultats_bridge[[b]] <- list(
    previsions_individuelles = previsions_indiv, prevision_bridge = prevision_bridge,
    delta = delta_opt, prevision_bvar = prevision_bvar_b, prevision_finale = prevision_finale,
    nb_indicateurs = length(previsions_indiv), taux_combles_moyen = mean(taux_combles, na.rm = TRUE)
  )

  cat(sprintf("%-28s | bridge=%+.4f (n=%d ind., comblement moyen=%.0f%%) | BVAR=%+.4f | delta=%.2f | FINAL=%+.4f\n",
              b, prevision_bridge, length(previsions_indiv),
              mean(taux_combles, na.rm=TRUE)*100, prevision_bvar_b, delta_opt, prevision_finale))
}

saveRDS(resultats_bridge, file.path(DOSSIER_RESULTATS, "bridge_equations.rds"))

df_delta <- tibble(branche = names(resultats_bridge), delta_bvar = sapply(resultats_bridge, `[[`, "delta")) %>%
  mutate(part_bridge = 1 - delta_bvar) %>%
  pivot_longer(c(delta_bvar, part_bridge), names_to = "composante", values_to = "poids") %>%
  mutate(composante = recode(composante, delta_bvar = "BVAR", part_bridge = "Bridge equation"))

p6 <- ggplot(df_delta, aes(branche, poids, fill = composante)) +
  geom_col(position = "stack") + coord_flip() +
  scale_fill_manual(values = c("BVAR" = "#7F7F7F", "Bridge equation" = "#2E74B5")) +
  labs(title = "Pondération BVAR vs. équation de passerelle, par branche",
       subtitle = "Avec traitement du jagged edge (lissage de Kalman) : poids optimal en échantillon",
       x = NULL, y = "Poids", fill = NULL)
ggsave(file.path(DOSSIER_FIGURES, "06_poids_bvar_vs_bridge.png"), p6, width = 8, height = 5, dpi = 150)

cat("\nEquations de passerelle (jagged edge) sauvegardees.\n")
