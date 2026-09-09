# ==============================================================================
# 01_tracking_model.R
#
# Methode 5 (Higgins, Table 5, modele 5 "Tracking model: Monthly version")
# Citation exacte : « each of the non-consumption component forecasts is a
# weighted average of the monthly indicator based forecast and the
# quarterly BVAR forecast. Here we set the weight on the BVAR forecasts
# to 0. »
#
# C'est-a-dire : delta = 0 pour toutes les branches -- uniquement les
# bridge equations / AR (Method_1, Etapes 3-4), JAMAIS combines avec le
# BVAR, agreges directement par les poids nominaux (meme methode que les
# 4 methodes precedentes).
#
#   Delta-log(PIB_t) = sum_i SH_{t-1}^i * Delta-log(VA_i,t^Bridge/AR)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(readxl)
  library(lubridate); library(ggplot2); library(stringr)
})

DATE_DEBUT_TRAIN <- as.Date("2010-01-01")
DATE_FIN_TRAIN   <- as.Date("2021-01-01")

dir.create("resultats", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

parse_trimestre <- function(s) {
  m <- regmatches(s, regexec("T(\\d)-(\\d{4})", s))[[1]]
  if (length(m) == 3) as.Date(sprintf("%d-%02d-01", as.integer(m[3]), (as.integer(m[2])-1)*3+1)) else NA
}
va_brut <- read_excel("VA_reelle_par_branche.xlsx", skip = 2)
names(va_brut)[1] <- "Trimestre"
va_brut$Date <- sapply(va_brut$Trimestre, parse_trimestre) |> as.Date(origin="1970-01-01")
BRANCHES_16 <- names(va_brut)[!(names(va_brut) %in% c("Trimestre","Date"))]

deltalog_serie <- function(v, nom="") {
  dv <- grepl("var\\. trim|variation", nom, ignore.case=TRUE)
  if (dv) return(v/100)
  v <- ifelse(is.na(v)|v<=0, NA, v)
  c(NA, diff(log(v)))
}
tronquer_nom <- function(nom) str_trunc(str_replace_all(nom, "[^A-Za-z0-9]+", "_"), 25)
squelette <- function(s) {
  s <- gsub("U\\+?[0-9A-Fa-f]{4}", "", s)
  s <- gsub("\u00e9|\u00e8|\u00ea","e",s); s <- gsub("\u00c9|\u00c8|\u00ca","E",s)
  toupper(gsub("[^A-Za-z0-9]", "", iconv(s, to="ASCII//TRANSLIT", sub="")))
}
decaler <- function(x,k) { n<-length(x); if(k==0) return(x); c(rep(NA,k), x[1:(n-k)]) }

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
  base <- data.frame(Date = va_brut$Date); base$Fitted <- intercept
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
  base <- data.frame(Date = va_brut$Date); base$Fitted <- intercept
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
  df$Fitted <- intercept
  for (k in 1:p_etoile) { coef_k <- lags$Estimate[lags$variable==paste0("lag",k)]; df$Fitted <- df$Fitted + coef_k*decaler(df$ld,k) }
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

cat("=== Reconstruction des 16 series Bridge/AR SEULES (delta=0, pas de BVAR) ===\n")
toutes_series <- list()
for (m in MAPPING) {
  bridge_df <- tryCatch({
    if (m$type == "combine") reconstruire_bridge_combine_generique(
      read_csv(file.path("resultats_bridge", paste0(m$fichier, "_combinee_coefficients.csv")), show_col_types=FALSE, locale=locale(encoding="UTF-8")), m$fichier)
    else if (m$type == "individuel") reconstruire_bridge_individuel(m$fichier)
    else if (m$type == "AR") reconstruire_AR(m$fichier)
    else if (m$type == "reduit_special") reconstruire_bridge_reduit_it()
  }, error = function(e) NULL)
  if (!is.null(bridge_df)) { toutes_series[[m$nom_va]] <- bridge_df; cat(sprintf("  [%s] OK, %d points\n", m$nom_va, nrow(bridge_df))) }
}

croissance <- Reduce(function(a,b) full_join(a,b,by="Date"),
  lapply(names(toutes_series), function(n) { d<-toutes_series[[n]]; names(d)[2]<-n; d })) %>% arrange(Date)
write_csv(croissance, "resultats/croissance_Tracking_par_branche.csv")

va_nom <- read_csv("csv/VA_nominale_base2014.csv", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
va_nom$Date <- as.Date(va_nom$Date); names(va_nom)[-1] <- BRANCHES_16
parts_reelles <- va_nom; total_row <- rowSums(parts_reelles[,-1])
for (b in BRANCHES_16) parts_reelles[[b]] <- parts_reelles[[b]]/total_row
premiere_part <- parts_reelles %>% filter(Date == as.Date("2014-01-01"))
dates_manquantes <- seq(DATE_DEBUT_TRAIN, as.Date("2014-01-01")-months(3), by="3 months")
extension <- premiere_part[rep(1,length(dates_manquantes)),]; extension$Date <- dates_manquantes
parts_completes <- bind_rows(extension, parts_reelles) %>% arrange(Date) %>% filter(Date <= as.Date("2026-01-01"))
write_csv(parts_completes, "resultats/parts_nominales.csv")

parts_decalees <- parts_completes; parts_decalees$Date <- parts_decalees$Date %m+% months(3)
resultat <- croissance %>% left_join(parts_decalees, by="Date", suffix=c("_croissance","_part"))

resultat$PIB_agrege <- NA
for (i in seq_len(nrow(resultat))) {
  s <- 0; pt <- 0
  for (b in BRANCHES_16) {
    cc <- paste0(b,"_croissance"); cp <- paste0(b,"_part")
    if (cc %in% names(resultat) && cp %in% names(resultat)) {
      cv <- resultat[[cc]][i]; pv <- resultat[[cp]][i]
      if (!is.na(cv) && !is.na(pv)) { s <- s+pv*cv; pt <- pt+pv }
    }
  }
  resultat$PIB_agrege[i] <- if (pt>0) s/pt else NA
}

va_somme <- va_brut %>% mutate(Somme16=rowSums(across(all_of(BRANCHES_16)))) %>% select(Date,Somme16) %>% arrange(Date)
va_somme$Somme16_ld <- c(NA, diff(log(va_somme$Somme16)))
resultat <- resultat %>% left_join(va_somme %>% select(Date,Somme16_ld), by="Date")
write_csv(resultat %>% select(Date, PIB_agrege, Realise=Somme16_ld), "resultats/PIB_agrege_Tracking.csv")

resultat_train <- resultat %>% filter(Date>=DATE_DEBUT_TRAIN, Date<=DATE_FIN_TRAIN, !is.na(PIB_agrege), !is.na(Somme16_ld))
cat(sprintf("\nEn-echantillon (train) : n=%d, correlation=%.3f\n", nrow(resultat_train), cor(resultat_train$PIB_agrege, resultat_train$Somme16_ld)))

df_fig <- resultat %>% filter(Date>=DATE_DEBUT_TRAIN) %>% select(Date, PIB_agrege, Reference=Somme16_ld) %>%
  pivot_longer(cols=c(PIB_agrege,Reference), names_to="serie", values_to="valeur")
p <- ggplot(df_fig, aes(x=Date,y=valeur,color=serie)) + geom_line(linewidth=0.7) +
  labs(title="Tracking model (Bridge/AR seuls) vs reference", x=NULL, y="Delta-log", color=NULL) + theme_minimal(base_size=11)
ggsave("figures/PIB_agrege_Tracking_vs_reference.png", p, width=9, height=4.5, dpi=150)
cat("\nTermine.\n")
