# ==============================================================================
# 07_estimer_delta_par_branche.R
#
# Etape 5 (Higgins 2014, equation 8) -- Pour chaque branche, estime delta
# par regression restreinte (RLS, sans constante, coefficients sommant a 1) :
#
#   Delta-log(VA_t) = delta * Delta-log(VA_t^BVAR) + (1-delta) * Delta-log(VA_t^Bridge/AR) + eps_t
#
# Astuce de reformulation (RLS) : en soustrayant BVAR_t des deux cotes,
#   y_t - BVAR_t = (1-delta) * (Bridge_t - BVAR_t) + eps_t
# une regression simple SANS INTERCEPT donne directement (1-delta), d'ou delta.
#
# ENTREE : coefficients_complets_BVAR.csv (Phase 1), VA_reelle_par_branche.xlsx,
#          resultats_bridge/*, resultats_AR/*, csv/*
# SORTIE : resultats/delta_par_branche.csv, figures/<branche>_combinaison.png
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(readxl)
  library(lubridate); library(ggplot2); library(stringr)
})

DATE_DEBUT_TRAIN <- as.Date("2010-01-01")
DATE_FIN_TRAIN <- as.Date("2021-01-01")
dir.create("resultats", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# ==============================================================================
# PARTIE A -- Charger la VA des 16 branches, Delta-log, format trimestriel Date
# ==============================================================================
parse_trimestre <- function(s) {
  m <- regmatches(s, regexec("T(\\d)-(\\d{4})", s))[[1]]
  if (length(m) == 3) as.Date(sprintf("%d-%02d-01", as.integer(m[3]), (as.integer(m[2])-1)*3+1)) else NA
}

va_brut <- read_excel("VA_reelle_par_branche.xlsx", skip = 2)
names(va_brut)[1] <- "Trimestre"
va_brut$Date <- sapply(va_brut$Trimestre, parse_trimestre) |> as.Date(origin="1970-01-01")

BRANCHES_16 <- names(va_brut)[!(names(va_brut) %in% c("Trimestre","Date"))]
cat("16 branches (ordre BVAR) :\n"); print(BRANCHES_16)

va_ld <- va_brut %>% select(Date, all_of(BRANCHES_16)) %>% arrange(Date)
for (b in BRANCHES_16) va_ld[[b]] <- c(NA, diff(log(va_ld[[b]])))
va_ld <- va_ld %>% filter(!is.na(.data[[BRANCHES_16[1]]]))

# ==============================================================================
# PARTIE B -- Reconstruire les valeurs ajustees du BVAR (Phase 1), pour
# chacune des 16 equations, sur le train
# ==============================================================================
coefs_bvar <- read_csv("coefficients_complets_BVAR.csv", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
P_BVAR <- 5

reconstruire_bvar_branche <- function(nom_branche) {
  coefs_eq <- coefs_bvar %>% filter(equation == nom_branche)
  constante <- coefs_eq %>% filter(type == "constante") %>% pull(coefficient)

  dates_valides <- va_ld$Date[va_ld$Date >= DATE_DEBUT_TRAIN & va_ld$Date <= DATE_FIN_TRAIN]
  fitted <- numeric(length(dates_valides))
  for (i in seq_along(dates_valides)) {
    idx_t <- which(va_ld$Date == dates_valides[i])
    if (idx_t <= P_BVAR) { fitted[i] <- NA; next }
    val <- constante
    for (l in 1:P_BVAR) {
      for (j in BRANCHES_16) {
        coef_lj <- coefs_eq %>% filter(type=="A_l", predicteur==j, retard==l) %>% pull(coefficient)
        val <- val + coef_lj * va_ld[[j]][idx_t - l]
      }
    }
    fitted[i] <- val
  }
  data.frame(Date = dates_valides, BVAR = fitted)
}

# ==============================================================================
# PARTIE C -- Reconstruire les valeurs ajustees Bridge/AR, par branche
# ==============================================================================
deltalog_serie <- function(v, nom="") {
  dv <- grepl("var\\. trim|variation", nom, ignore.case=TRUE)
  if (dv) return(v/100)
  v <- ifelse(is.na(v)|v<=0, NA, v)
  c(NA, diff(log(v)))
}
tronquer_nom <- function(nom) str_trunc(str_replace_all(nom, "[^A-Za-z0-9]+", "_"), 25)

reconstruire_bridge_reduit_it <- function() {
  # Cas particulier : le modele reduit "top 6" d'Industrie de transformation
  # (script 05) utilise les noms de COLONNES COMPLETS (pas tronques), avec
  # des guillemets inverses ajoutes automatiquement par R (summary.lm) autour
  # des noms contenant des caracteres speciaux -- a retirer avant de matcher.
  coefs <- read_csv(file.path("resultats_bridge", "Industrie_transformation_modele_retenu.csv"), show_col_types=FALSE, locale=locale(encoding="UTF-8"))
  coefs$variable <- gsub("^`|`$", "", coefs$variable)  # retire les guillemets inverses en debut/fin

  vars_modele <- coefs$variable[coefs$variable != "(Intercept)"]
  intercept <- coefs$Estimate[coefs$variable=="(Intercept)"]

  dft <- read_csv(file.path("csv", "Industrie_transformation_trimestriel.csv"), show_col_types=FALSE, locale=locale(encoding="UTF-8"))
  dft$Date <- as.Date(dft$Date)

  base <- data.frame(Date = va_ld$Date)
  base$Fitted <- intercept
  squelette <- function(s) {
    s <- gsub("U\\+?[0-9A-Fa-f]{4}", "", s)  # retire les echappements Unicode litteraux mal encodes ("U+2014", "U00E9"...)
    toupper(gsub("[^A-Za-z0-9]", "", iconv(s, to="ASCII//TRANSLIT", sub="")))
  }
  noms_dft_squelette <- squelette(names(dft))
  for (v in vars_modele) {
    v_sq <- squelette(v)
    idx_match <- which(noms_dft_squelette == v_sq)
    if (length(idx_match) != 1) { warning(paste("Variable non retrouvee (reduit IT):", v)); next }
    nom_reel <- names(dft)[idx_match]
    s <- dft %>% select(Date, valeur = all_of(nom_reel))
    s$ld <- deltalog_serie(s$valeur, nom_reel)
    coef_v <- coefs$Estimate[coefs$variable==v]
    base <- base %>% left_join(s %>% select(Date, ld), by="Date")
    base$Fitted <- base$Fitted + coef_v * base$ld
    base$ld <- NULL
  }
  base %>% select(Date, Bridge = Fitted) %>% filter(!is.na(Bridge))
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

  base <- data.frame(Date = va_ld$Date)
  base$Fitted <- intercept
  for (v in vars_modele) {
    if (!(v %in% names(candidats))) { warning(paste("Variable non retrouvee:", v)); next }
    s <- candidats[[v]]
    s$ld <- deltalog_serie(s$valeur, v)
    coef_v <- coefs$Estimate[coefs$variable==v]
    base <- base %>% left_join(s %>% select(Date, ld), by="Date")
    base$Fitted <- base$Fitted + coef_v * base$ld
    base$ld <- NULL
  }
  base %>% select(Date, Bridge = Fitted) %>% filter(!is.na(Bridge))
}

reconstruire_bridge_combine <- function(nom_branche_fichier, nom_va_va) {
  coefs <- read_csv(file.path("resultats_bridge", paste0(nom_branche_fichier, "_combinee_coefficients.csv")), show_col_types=FALSE, locale=locale(encoding="UTF-8"))
  reconstruire_bridge_combine_generique(coefs, nom_branche_fichier)
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
  s$ld <- deltalog_serie(s$valeur, nom_indic)
  s$Bridge <- beta0 + beta1 * s$ld
  s %>% select(Date, Bridge) %>% filter(!is.na(Bridge))
}

reconstruire_AR <- function(nom_branche_fichier) {
  coefs <- read_csv(file.path("resultats_AR", paste0(nom_branche_fichier, "_AR_coefficients.csv")), show_col_types=FALSE, locale=locale(encoding="UTF-8"))
  intercept <- coefs$Estimate[coefs$variable=="(Intercept)"]
  lags <- coefs %>% filter(variable != "(Intercept)")
  p_etoile <- nrow(lags)

  chemin <- file.path("csv", paste0(nom_branche_fichier, "_trimestriel.csv"))
  df <- read_csv(chemin, show_col_types=FALSE, locale=locale(encoding="UTF-8")); df$Date <- as.Date(df$Date)
  cible_col <- names(df)[2]
  df <- df %>% select(Date, valeur=all_of(cible_col)) %>% arrange(Date) %>% filter(valeur>0)
  df$ld <- c(NA, diff(log(df$valeur)))

  decaler <- function(x,k) { n<-length(x); c(rep(NA,k), x[1:(n-k)]) }
  df$Fitted <- intercept
  for (k in 1:p_etoile) {
    coef_k <- lags$Estimate[lags$variable==paste0("lag",k)]
    df$Fitted <- df$Fitted + coef_k * decaler(df$ld, k)
  }
  df %>% select(Date, Bridge = Fitted) %>% filter(!is.na(Bridge))
}

# ==============================================================================
# PARTIE D -- Mapping branche (nom BVAR/VA) <-> nom fichier, et type de modele
# ==============================================================================
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
# Resoudre nom_va dynamiquement depuis BRANCHES_16 (jamais de litteral accentue
# tape en dur -- la locale systeme "C" de cet environnement R fait echouer
# toute comparaison de chaine accentuee codee en dur dans le fichier source)
for (i in seq_along(MAPPING)) MAPPING[[i]]$nom_va <- BRANCHES_16[MAPPING[[i]]$idx]

# ==============================================================================
# PARTIE E -- Boucle principale : reconstruire, fusionner, estimer delta (RLS)
# ==============================================================================
recap <- data.frame()

for (m in MAPPING) {
  cat(sprintf("\n=== %s ===\n", m$nom_va))
  bvar_df <- reconstruire_bvar_branche(m$nom_va) %>% filter(!is.na(BVAR))

  bridge_df <- tryCatch({
    if (m$type == "combine") reconstruire_bridge_combine(m$fichier, m$nom_va)
    else if (m$type == "individuel") reconstruire_bridge_individuel(m$fichier)
    else if (m$type == "AR") reconstruire_AR(m$fichier)
    else if (m$type == "reduit_special") reconstruire_bridge_reduit_it()
  }, error = function(e) { cat("  ERREUR reconstruction Bridge/AR:", e$message, "\n"); NULL })

  if (is.null(bridge_df) || nrow(bridge_df)==0) {
    cat("  Reconstruction Bridge/AR impossible -- ignoree\n")
    recap <- rbind(recap, data.frame(branche=m$nom_va, type=m$type, delta_brut=NA, delta=NA, borne=NA, n=NA, statut="ECHEC"))
    next
  }

  fusion <- inner_join(va_ld %>% select(Date, VA = all_of(m$nom_va)), bvar_df, by="Date") %>%
    inner_join(bridge_df, by="Date") %>%
    filter(Date >= DATE_DEBUT_TRAIN, Date <= DATE_FIN_TRAIN)

  if (nrow(fusion) < 8) {
    cat(sprintf("  Seulement %d points communs -- insuffisant\n", nrow(fusion)))
    recap <- rbind(recap, data.frame(branche=m$nom_va, type=m$type, delta_brut=NA, delta=NA, borne=NA, n=nrow(fusion), statut="ECHEC (n<8)"))
    next
  }

  # RLS : (VA - BVAR) ~ 0 + (Bridge - BVAR)  =>  coefficient = (1 - delta)
  fusion$y_moins_bvar <- fusion$VA - fusion$BVAR
  fusion$x_diff <- fusion$Bridge - fusion$BVAR
  modele_rls <- lm(y_moins_bvar ~ 0 + x_diff, data = fusion)
  un_moins_delta <- coef(modele_rls)[1]
  delta_brut <- 1 - un_moins_delta

  # Bornage : delta doit rester dans [0,1] pour que la combinaison reste une
  # vraie moyenne ponderee (convexe) des deux previsions -- au-dela, un des
  # deux poids devient negatif, et le "combine" peut depasser les deux
  # previsions individuelles (extrapolation, pas interpolation). Regle
  # generale appliquee a toutes les branches, pas un cas particulier :
  # ramener a la borne la plus proche des lors que delta sort de [0,1].
  delta <- pmin(pmax(delta_brut, 0), 1)
  borne <- delta != delta_brut

  cat(sprintf("  delta_brut = %.3f%s (n=%d)\n", delta_brut,
              if (borne) sprintf(" -> BORNE a %.0f", delta) else "", nrow(fusion)))

  fusion$Combine <- delta*fusion$BVAR + (1-delta)*fusion$Bridge
  df_long <- fusion %>% select(Date, VA, BVAR, Bridge, Combine) %>%
    pivot_longer(cols=c(VA,BVAR,Bridge,Combine), names_to="serie", values_to="valeur")
  p <- ggplot(df_long, aes(x=Date, y=valeur, color=serie)) + geom_line() +
    labs(title=paste0(m$nom_va, " \u2014 delta = ", round(delta,3)), x=NULL, y="Delta-log(VA)") +
    theme_minimal(base_size=10)
  ggsave(file.path("figures", paste0(m$fichier, "_combinaison.png")), p, width=8, height=4, dpi=150)

  recap <- rbind(recap, data.frame(branche=m$nom_va, type=m$type, delta_brut=delta_brut, delta=delta, borne=borne, n=nrow(fusion), statut="OK"))
}

write_csv(recap, "resultats/delta_par_branche.csv")
cat("\n=== RECAPITULATIF ===\n")
print(recap)
