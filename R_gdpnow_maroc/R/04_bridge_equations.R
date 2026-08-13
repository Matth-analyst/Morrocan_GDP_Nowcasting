# ============================================================================
# 04_bridge_equations.R -- Equations de passerelle + combinaison avec le BVAR
# ============================================================================
# Reference : Higgins (2014), etapes 4a-4b (equations 6, 7, 8 du papier).
#
# Pour chaque branche couverte :
#  (a) chaque indicateur retenu donne une prevision "bridge" individuelle
#      par regression simple Delta-log(VA) ~ Delta-log(indicateur)
#      (equation 6 du papier) ;
#  (b) les previsions individuelles sont agregees par MOYENNE SIMPLE, faute
#      de disposer des poids de valeur ajoutee nominale par sous-branche
#      (SH_T^i, equation 7 du papier) -- limite explicitement documentee,
#      a corriger si ces poids deviennent disponibles ;
#  (c) la prevision "bridge" agregee est combinee avec la prevision BVAR
#      (script 03) par moindres carres restreints, delta in [0,1]
#      (equation 8 du papier) : plus la bridge equation explique bien la
#      cible en echantillon, plus son poids (1-delta) est eleve.
# ============================================================================
source("R/00_setup.R")
cibles <- readRDS(file.path(DOSSIER_RESULTATS, "cibles_avec_dlog.rds"))
indic_trim <- readRDS(file.path(DOSSIER_RESULTATS, "indicateurs_trimestriels.rds"))
bvar <- readRDS(file.path(DOSSIER_RESULTATS, "bvar_modele.rds"))

#' Ajuste une bridge equation simple pour un indicateur donne et renvoie
#' la prevision au prochain trimestre + les valeurs ajustees en echantillon
bridge_un_indicateur <- function(cible_df, indic_df) {
  df <- inner_join(cible_df, indic_df, by = "date") %>% filter(!is.na(dlog_va), !is.na(dlog))
  if (nrow(df) < 10) return(NULL)
  fit <- lm(dlog_va ~ dlog, data = df)
  # prevision : derniere valeur connue de l'indicateur (a defaut d'une
  # vraie donnee du trimestre a venir, on utilise l'AR(1) de l'indicateur
  # comme prevision de son propre Δlog -- equivalent simplifie de l'etape
  # 3 du papier original, "autoregressions augmentees par facteur")
  ind_ar1 <- tryCatch(Arima(indic_df$dlog, order = c(1,0,0)), error = function(e) NULL)
  dlog_prevu <- if (!is.null(ind_ar1)) as.numeric(forecast(ind_ar1, h = 1)$mean) else tail(indic_df$dlog, 1)
  prevision <- as.numeric(predict(fit, newdata = data.frame(dlog = dlog_prevu)))
  list(fit = fit, ajuste = fitted(fit), dates_ajuste = df$date[!is.na(df$dlog_va) & !is.na(df$dlog)],
       prevision = prevision, r2 = summary(fit)$r.squared)
}

resultats_bridge <- list()

for (b in BRANCHES_COUVERTES) {
  cible_b <- cibles %>% filter(branche == b) %>% select(date, dlog_va)
  indics_b <- indic_trim %>% filter(branche == b) %>% distinct(indicateur) %>% pull(indicateur)

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

  # --- valeurs ajustees "bridge" moyennees en echantillon, pour l'etape (c)
  ajuste_moyen <- bind_rows(ajustes_liste, .id = "indicateur") %>%
    group_by(date) %>% summarise(bridge = mean(ajuste, na.rm = TRUE))

  # --- combinaison BVAR / bridge par moindres carres restreints (delta in [0,1])
  df_comb <- cible_b %>% inner_join(ajuste_moyen, by = "date")
  # approx. de la composante BVAR en echantillon : valeur ajustee d'un
  # AR(p) univarie sur la meme branche (proxy simple du "BVAR own-lag"
  # en echantillon, faute de re-estimer le BVAR complet a chaque date)
  ar_branche <- Arima(cible_b$dlog_va[!is.na(cible_b$dlog_va)], order = c(bvar$p, 0, 0))
  df_comb$bvar_ajuste <- as.numeric(fitted(ar_branche))[match(df_comb$date, cible_b$date[!is.na(cible_b$dlog_va)])]
  df_comb <- df_comb %>% filter(!is.na(bvar_ajuste), !is.na(bridge))

  grille_delta <- seq(0, 1, by = 0.01)
  sse <- sapply(grille_delta, function(d) {
    pred <- d * df_comb$bvar_ajuste + (1 - d) * df_comb$bridge
    sum((df_comb$dlog_va - pred)^2)
  })
  delta_opt <- grille_delta[which.min(sse)]

  prevision_bvar_branche <- bvar$prevision[b]
  prevision_finale <- delta_opt * prevision_bvar_branche + (1 - delta_opt) * prevision_bridge

  resultats_bridge[[b]] <- list(
    previsions_individuelles = previsions_indiv,
    prevision_bridge = prevision_bridge,
    delta = delta_opt,
    prevision_bvar = prevision_bvar_branche,
    prevision_finale = prevision_finale,
    nb_indicateurs = length(previsions_indiv)
  )

  cat(sprintf("%-28s | bridge=%+.4f (n=%d ind.) | BVAR=%+.4f | delta=%.2f | FINAL=%+.4f\n",
              b, prevision_bridge, length(previsions_indiv), prevision_bvar_branche,
              delta_opt, prevision_finale))
}

saveRDS(resultats_bridge, file.path(DOSSIER_RESULTATS, "bridge_equations.rds"))

# --- Figure : poids delta (BVAR) vs (1-delta) (bridge) par branche --------
df_delta <- tibble(
  branche = names(resultats_bridge),
  delta_bvar = sapply(resultats_bridge, `[[`, "delta")
) %>% mutate(part_bridge = 1 - delta_bvar) %>%
  pivot_longer(c(delta_bvar, part_bridge), names_to = "composante", values_to = "poids") %>%
  mutate(composante = recode(composante, delta_bvar = "BVAR", part_bridge = "Bridge equation"))

p6 <- ggplot(df_delta, aes(branche, poids, fill = composante)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("BVAR" = "#7F7F7F", "Bridge equation" = "#2E74B5")) +
  labs(title = "Pondération BVAR vs. équation de passerelle, par branche",
       subtitle = "Poids optimal (delta) estimé par moindres carrés restreints en échantillon",
       x = NULL, y = "Poids", fill = NULL)
ggsave(file.path(DOSSIER_FIGURES, "06_poids_bvar_vs_bridge.png"), p6, width = 8, height = 5, dpi = 150)

cat("\nEquations de passerelle sauvegardees.\n")
