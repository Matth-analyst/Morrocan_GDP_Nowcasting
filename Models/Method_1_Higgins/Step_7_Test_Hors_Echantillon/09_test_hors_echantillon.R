# ==============================================================================
# 08_ponderation_et_aggregation.R
#
# Etape 6 (Higgins 2014, methodologie de ponderation Tornqvist/Fisher) --
# Calcule les parts nominales SH_t^i de chaque branche, puis agrege les 16
# previsions combinees (BVAR+Bridge/AR, delta borne, Etape 5) en une seule
# prevision de croissance du PIB :
#
#   Delta-log(PIB_t) = sum_i SH_{t-1}^i * Delta-log(VA_i,t^combine)
#
# ENTREE : VA_nominale_base2014.csv (2014-2026), coefficients_complets_BVAR.csv,
#          resultats_bridge/*, resultats_AR/*, delta_par_branche.csv, csv/*
# SORTIE : resultats/parts_nominales.csv, resultats/croissance_combinee_par_branche.csv,
#          resultats/croissance_PIB_agregee.csv, figures/*.png
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(readxl)
  library(lubridate); library(ggplot2); library(stringr)
})

DATE_DEBUT_TRAIN <- as.Date("2010-01-01")
DATE_FIN_TRAIN <- as.Date("2021-01-01")
DATE_DEBUT_TEST <- as.Date("2021-04-01")   # premier trimestre JAMAIS vu par aucune estimation
DATE_FIN_TEST <- as.Date("2026-01-01")     # derniere date disponible dans nos donnees
DATE_DEBUT_BASE2014 <- as.Date("2014-01-01")  # premiere date ou les parts nominales reelles existent
P_BVAR <- 5

# ==============================================================================
# PARTIE A -- Charger la VA reelle des 16 branches (deja fait a l'Etape 5),
# et reconstruire les series combinees (BVAR + Bridge/AR, delta borne)
# ==============================================================================
parse_trimestre <- function(s) {
  m <- regmatches(s, regexec("T(\\d)-(\\d{4})", s))[[1]]
  if (length(m) == 3) as.Date(sprintf("%d-%02d-01", as.integer(m[3]), (as.integer(m[2])-1)*3+1)) else NA
}
va_brut <- read_excel("VA_reelle_par_branche.xlsx", skip = 2)
names(va_brut)[1] <- "Trimestre"
va_brut$Date <- sapply(va_brut$Trimestre, parse_trimestre) |> as.Date(origin="1970-01-01")
BRANCHES_16 <- names(va_brut)[!(names(va_brut) %in% c("Trimestre","Date"))]

va_ld <- va_brut %>% select(Date, all_of(BRANCHES_16)) %>% arrange(Date)
for (b in BRANCHES_16) va_ld[[b]] <- c(NA, diff(log(va_ld[[b]])))
va_ld <- va_ld %>% filter(!is.na(.data[[BRANCHES_16[1]]]))

coefs_bvar <- read_csv("coefficients_complets_BVAR.csv", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

reconstruire_bvar_branche <- function(nom_branche) {
  coefs_eq <- coefs_bvar %>% filter(equation == nom_branche)
  constante <- coefs_eq %>% filter(type == "constante") %>% pull(coefficient)
  # ETENDU jusqu'a DATE_FIN_TEST -- les COEFFICIENTS restent ceux deja estimes
  # sur le train (2010-2021), jamais reestimes ; seule la fenetre d'application
  # s'etend, exactement l'esprit d'un vrai test hors-echantillon.
  dates_valides <- va_ld$Date[va_ld$Date >= DATE_DEBUT_TRAIN & va_ld$Date <= DATE_FIN_TEST]
  fitted <- numeric(length(dates_valides))
  for (i in seq_along(dates_valides)) {
    idx_t <- which(va_ld$Date == dates_valides[i])
    if (idx_t <= P_BVAR) { fitted[i] <- NA; next }
    val <- constante
    for (l in 1:P_BVAR) for (j in BRANCHES_16) {
      coef_lj <- coefs_eq %>% filter(type=="A_l", predicteur==j, retard==l) %>% pull(coefficient)
      val <- val + coef_lj * va_ld[[j]][idx_t - l]
    }
    fitted[i] <- val
  }
  data.frame(Date = dates_valides, BVAR = fitted)
}

deltalog_serie <- function(v, nom="") {
  dv <- grepl("var\\. trim|variation", nom, ignore.case=TRUE)
  if (dv) return(v/100)
  v <- ifelse(is.na(v)|v<=0, NA, v)
  c(NA, diff(log(v)))
}
tronquer_nom <- function(nom) str_trunc(str_replace_all(nom, "[^A-Za-z0-9]+", "_"), 25)
squelette <- function(s) {
  s <- gsub("U\\+?[0-9A-Fa-f]{4}", "", s)
  toupper(gsub("[^A-Za-z0-9]", "", iconv(s, to="ASCII//TRANSLIT", sub="")))
}

reconstruire_bridge_combine_generique <- function(coefs, nom_branche_fichier) {
  vars_modele <- coefs$variable[coefs$variable != "(Intercept)"]
  intercept <- coefs$Estimate[coefs$variable=="(Intercept)"]
  chemin_trim <- file.path("csv", paste0(nom_branche_fichier, "_trimestriel.csv"))
  chemin_mens <- file.path("csv", paste0(nom_branche_fichier, "_mensuel.csv"))
  candidats <- list()
  if (file.exists(chemin_trim)) {
    dft <- read_csv(chemin_trim, show_col_types=FALSE, locale=locale(encoding="UTF-8")); dft$Date <- as.Date(dft$Date)
    for (col in names(dft)[-1]) candidats[[tronquer_nom(col)]] <- dft %>% select(Date, valeur=all_of(col))
  }
  if (file.exists(chemin_mens)) {
    dfm <- read_csv(chemin_mens, show_col_types=FALSE, locale=locale(encoding="UTF-8")); dfm$Date <- as.Date(dfm$Date)
    for (col in names(dfm)[-1]) {
      s <- dfm %>% select(Date, valeur=all_of(col)) %>% filter(!is.na(valeur)) %>%
        mutate(Trimestre=floor_date(Date,"quarter")) %>% group_by(Trimestre) %>%
        summarise(valeur=mean(valeur,na.rm=TRUE),.groups="drop") %>% rename(Date=Trimestre)
      candidats[[tronquer_nom(col)]] <- s
    }
  }
  base <- data.frame(Date = va_ld$Date); base$Fitted <- intercept
  for (v in vars_modele) {
    if (!(v %in% names(candidats))) next
    s <- candidats[[v]]; s$ld <- deltalog_serie(s$valeur, v)
    coef_v <- coefs$Estimate[coefs$variable==v]
    base <- base %>% left_join(s %>% select(Date, ld), by="Date")
    base$Fitted <- base$Fitted + coef_v * base$ld; base$ld <- NULL
  }
  base %>% select(Date, Bridge = Fitted) %>% filter(!is.na(Bridge))
}

reconstruire_bridge_reduit_it <- function() {
  coefs <- read_csv(file.path("resultats_bridge", "Industrie_transformation_modele_retenu.csv"), show_col_types=FALSE, locale=locale(encoding="UTF-8"))
  coefs$variable <- gsub("^`|`$", "", coefs$variable)
  vars_modele <- coefs$variable[coefs$variable != "(Intercept)"]
  intercept <- coefs$Estimate[coefs$variable=="(Intercept)"]
  dft <- read_csv(file.path("csv", "Industrie_transformation_trimestriel.csv"), show_col_types=FALSE, locale=locale(encoding="UTF-8")); dft$Date <- as.Date(dft$Date)
  base <- data.frame(Date = va_ld$Date); base$Fitted <- intercept
  noms_dft_squelette <- squelette(names(dft))
  for (v in vars_modele) {
    v_sq <- squelette(v); idx_match <- which(noms_dft_squelette == v_sq)
    if (length(idx_match) != 1) next
    nom_reel <- names(dft)[idx_match]
    s <- dft %>% select(Date, valeur = all_of(nom_reel)); s$ld <- deltalog_serie(s$valeur, nom_reel)
    coef_v <- coefs$Estimate[coefs$variable==v]
    base <- base %>% left_join(s %>% select(Date, ld), by="Date")
    base$Fitted <- base$Fitted + coef_v * base$ld; base$ld <- NULL
  }
  base %>% select(Date, Bridge = Fitted) %>% filter(!is.na(Bridge))
}

reconstruire_bridge_individuel <- function(nom_branche_fichier) {
  coefs <- read_csv(file.path("resultats_bridge", paste0(nom_branche_fichier, "_modele_retenu.csv")), show_col_types=FALSE, locale=locale(encoding="UTF-8"))
  nom_indic <- coefs$indicateur[1]; beta0 <- coefs$beta0[1]; beta1 <- coefs$beta1[1]
  chemin_trim <- file.path("csv", paste0(nom_branche_fichier, "_trimestriel.csv"))
  chemin_mens <- file.path("csv", paste0(nom_branche_fichier, "_mensuel.csv"))
  s <- NULL
  if (file.exists(chemin_trim)) {
    dft <- read_csv(chemin_trim, show_col_types=FALSE, locale=locale(encoding="UTF-8")); dft$Date <- as.Date(dft$Date)
    if (nom_indic %in% names(dft)) s <- dft %>% select(Date, valeur=all_of(nom_indic))
  }
  if (is.null(s) && file.exists(chemin_mens)) {
    dfm <- read_csv(chemin_mens, show_col_types=FALSE, locale=locale(encoding="UTF-8")); dfm$Date <- as.Date(dfm$Date)
    if (nom_indic %in% names(dfm)) {
      s <- dfm %>% select(Date, valeur=all_of(nom_indic)) %>% filter(!is.na(valeur)) %>%
        mutate(Trimestre=floor_date(Date,"quarter")) %>% group_by(Trimestre) %>%
        summarise(valeur=mean(valeur,na.rm=TRUE),.groups="drop") %>% rename(Date=Trimestre)
    }
  }
  if (is.null(s)) return(NULL)
  s$ld <- deltalog_serie(s$valeur, nom_indic); s$Bridge <- beta0 + beta1 * s$ld
  s %>% select(Date, Bridge) %>% filter(!is.na(Bridge))
}

reconstruire_AR <- function(nom_branche_fichier) {
  coefs <- read_csv(file.path("resultats_AR", paste0(nom_branche_fichier, "_AR_coefficients.csv")), show_col_types=FALSE, locale=locale(encoding="UTF-8"))
  intercept <- coefs$Estimate[coefs$variable=="(Intercept)"]
  lags <- coefs %>% filter(variable != "(Intercept)"); p_etoile <- nrow(lags)
  chemin <- file.path("csv", paste0(nom_branche_fichier, "_trimestriel.csv"))
  df <- read_csv(chemin, show_col_types=FALSE, locale=locale(encoding="UTF-8")); df$Date <- as.Date(df$Date)
  cible_col <- names(df)[2]
  df <- df %>% select(Date, valeur=all_of(cible_col)) %>% arrange(Date) %>% filter(valeur>0)
  df$ld <- c(NA, diff(log(df$valeur)))
  decaler <- function(x,k) { n<-length(x); c(rep(NA,k), x[1:(n-k)]) }
  df$Fitted <- intercept
  for (k in 1:p_etoile) { coef_k <- lags$Estimate[lags$variable==paste0("lag",k)]; df$Fitted <- df$Fitted + coef_k * decaler(df$ld, k) }
  df %>% select(Date, Bridge = Fitted) %>% filter(!is.na(Bridge))
}

MAPPING <- list(
  list(idx=1,  fichier="Industrie_transformation", type="reduit_special"),
  list(idx=2,  fichier="Finances_assurances", type="combine"),
  list(idx=3,  fichier="Hebergement_restauration", type="combine"),
  list(idx=4,  fichier="Peche", type="combine"),
  list(idx=5,  fichier="Industrie_extraction", type="individuel"),
  list(idx=6,  fichier="Construction", type="combine"),
  list(idx=7,  fichier="Information_communication", type="AR"),
  list(idx=8,  fichier="Immobilier", type="individuel"),
  list(idx=9,  fichier="Agriculture", type="AR"),
  list(idx=10, fichier="Commerce", type="individuel"),
  list(idx=11, fichier="Transports", type="AR"),
  list(idx=12, fichier="Electricite_gaz_eau", type="combine"),
  list(idx=13, fichier="Services_aux_entreprises", type="AR"),
  list(idx=14, fichier="Administration_publique", type="AR"),
  list(idx=15, fichier="Education_sante", type="AR"),
  list(idx=16, fichier="Autres_services", type="AR")
)
for (i in seq_along(MAPPING)) MAPPING[[i]]$nom_va <- BRANCHES_16[MAPPING[[i]]$idx]

delta_df <- read_csv("delta_par_branche.csv", show_col_types = FALSE, locale=locale(encoding="UTF-8"))

cat("=== Reconstruction des 16 series combinees (BVAR + Bridge/AR, delta borne) ===\n")
toutes_series_combinees <- list()
for (m in MAPPING) {
  bvar_df <- reconstruire_bvar_branche(m$nom_va) %>% filter(!is.na(BVAR))
  bridge_df <- tryCatch({
    if (m$type == "combine") reconstruire_bridge_combine_generique(
      read_csv(file.path("resultats_bridge", paste0(m$fichier, "_combinee_coefficients.csv")), show_col_types=FALSE, locale=locale(encoding="UTF-8")), m$fichier)
    else if (m$type == "individuel") reconstruire_bridge_individuel(m$fichier)
    else if (m$type == "AR") reconstruire_AR(m$fichier)
    else if (m$type == "reduit_special") reconstruire_bridge_reduit_it()
  }, error = function(e) NULL)

  delta_row <- delta_df %>% filter(branche == m$nom_va)
  delta_final <- delta_row$delta[1]

  if (is.null(bridge_df) || nrow(bridge_df)==0 || is.na(delta_final)) {
    cat(sprintf("  [%s] reconstruction impossible -- ignoree\n", m$nom_va)); next
  }

  fusion <- inner_join(bvar_df, bridge_df, by="Date")
  fusion$Combine <- delta_final*fusion$BVAR + (1-delta_final)*fusion$Bridge
  toutes_series_combinees[[m$nom_va]] <- fusion %>% select(Date, Combine)
  cat(sprintf("  [%s] OK, %d points, delta=%.3f\n", m$nom_va, nrow(fusion), delta_final))
}

# Assembler en un seul tableau large (Date x 16 branches)
croissance_combinee <- Reduce(function(a,b) full_join(a,b,by="Date"),
  lapply(names(toutes_series_combinees), function(n) {
    d <- toutes_series_combinees[[n]]; names(d)[2] <- n; d
  }))
croissance_combinee <- croissance_combinee %>% arrange(Date)
write_csv(croissance_combinee, "resultats/croissance_combinee_par_branche.csv")
cat(sprintf("\nTableau assemble : %d trimestres x %d branches\n", nrow(croissance_combinee), ncol(croissance_combinee)-1))

# ==============================================================================
# PARTIE B -- Calcul des parts nominales SH_t^i
# ==============================================================================
cat("\n=== Calcul des parts nominales (Tornqvist/Fisher) ===\n")

va_nom <- read_csv("csv/VA_nominale_base2014.csv", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
va_nom$Date <- as.Date(va_nom$Date)

# Mapping nom fichier nominal -> nom BRANCHES_16 (meme ordre que MAPPING$idx)
NOMS_NOMINAL <- names(va_nom)[-1]
# Les 16 colonnes nominales sont dans le MEME ORDRE que la nomenclature
# base 2014 standard -- correspondance directe verifiee avec BRANCHES_16
# (Agriculture, Peche, ... Autres services), position par position.
names(va_nom)[-1] <- BRANCHES_16  # renommer directement aux noms harmonises

parts_reelles <- va_nom
total_row <- rowSums(parts_reelles[,-1])
for (b in BRANCHES_16) parts_reelles[[b]] <- parts_reelles[[b]] / total_row

# Extension des parts nominales au-dela de 2021 : on utilise les vraies
# parts nominales trimestrielles la ou elles existent (jusqu'a 2026-T1,
# derniere date disponible dans le fichier source)
parts_completes <- parts_reelles %>% arrange(Date) %>%
  filter(Date >= DATE_DEBUT_TRAIN, Date <= DATE_FIN_TEST)
write_csv(parts_completes, "resultats/parts_nominales.csv")

# ==============================================================================
# PARTIE C -- TEST HORS-ECHANTILLON : appliquer coefficients figes (train)
# aux trimestres 2021T2-2026T1, jamais vus par aucune estimation
# ==============================================================================
cat("\n=== TEST HORS-ECHANTILLON (2021T2 - 2026T1) ===\n")

parts_decalees <- parts_completes
parts_decalees$Date <- parts_decalees$Date %m+% months(3)

resultat_final <- croissance_combinee %>%
  filter(Date >= DATE_DEBUT_TEST, Date <= DATE_FIN_TEST) %>%
  left_join(parts_decalees, by="Date", suffix=c("_croissance","_part"))

resultat_final$PIB_agrege <- NA
for (i in seq_len(nrow(resultat_final))) {
  s <- 0; poids_total <- 0
  for (b in BRANCHES_16) {
    col_c <- paste0(b, "_croissance"); col_p <- paste0(b, "_part")
    if (col_c %in% names(resultat_final) && col_p %in% names(resultat_final)) {
      c_val <- resultat_final[[col_c]][i]; p_val <- resultat_final[[col_p]][i]
      if (!is.na(c_val) && !is.na(p_val)) { s <- s + p_val*c_val; poids_total <- poids_total + p_val }
    }
  }
  resultat_final$PIB_agrege[i] <- if (poids_total > 0) s / poids_total else NA
}

# --- Reference REELLE : Delta-log de la somme des 16 VA REELLES (le vrai
# realise, celui qu'on cherche a predire -- PAS la reference interne
# utilisee a l'Etape 6 pour le train) --------------------------------------
va_somme <- va_brut %>% mutate(Somme16 = rowSums(across(all_of(BRANCHES_16)))) %>%
  select(Date, Somme16) %>% arrange(Date)
va_somme$Somme16_ld <- c(NA, diff(log(va_somme$Somme16)))

resultat_final <- resultat_final %>% left_join(va_somme %>% select(Date, Somme16_ld), by="Date")
write_csv(resultat_final %>% select(Date, PIB_agrege, Realise = Somme16_ld),
          "resultats/test_hors_echantillon.csv")

# --- RMSFE, sur croissance annualisee (comme Higgins, Table 5) -------------
resultat_valide <- resultat_final %>% filter(!is.na(PIB_agrege), !is.na(Somme16_ld))
pib_agrege_annualise <- 100*((exp(resultat_valide$PIB_agrege))^4 - 1)
pib_realise_annualise <- 100*((exp(resultat_valide$Somme16_ld))^4 - 1)
rmsfe <- sqrt(mean((pib_agrege_annualise - pib_realise_annualise)^2))
corr_test <- cor(resultat_valide$PIB_agrege, resultat_valide$Somme16_ld)

cat(sprintf("\nRESULTAT DU TEST HORS-ECHANTILLON :\n"))
cat(sprintf("  n = %d trimestres (jamais vus par aucune estimation)\n", nrow(resultat_valide)))
cat(sprintf("  Correlation = %.3f\n", corr_test))
cat(sprintf("  RMSFE (croissance annualisee, comme Higgins Table 5) = %.2f points\n", rmsfe))

write_csv(data.frame(n=nrow(resultat_valide), correlation=corr_test, RMSFE=rmsfe),
          "resultats/metriques_test.csv")

# --- Figure ------------------------------------------------------------------
df_fig <- resultat_final %>% select(Date, Prevu=PIB_agrege, Realise=Somme16_ld) %>%
  pivot_longer(cols=c(Prevu, Realise), names_to="serie", values_to="valeur")
p <- ggplot(df_fig, aes(x=Date, y=valeur, color=serie)) + geom_line(linewidth=0.8) +
  geom_point(size=1.5) +
  labs(title="Test hors-echantillon : prevu vs realise (2021T2-2026T1)",
       subtitle=sprintf("n=%d, correlation=%.3f, RMSFE=%.2f points (croissance annualisee)", nrow(resultat_valide), corr_test, rmsfe),
       x=NULL, y="Delta-log", color=NULL) + theme_minimal(base_size=11)
ggsave("figures/test_hors_echantillon.png", p, width=9, height=4.5, dpi=150)

cat("\nTermine. Resultats dans 'resultats/', figure dans 'figures/'.\n")
