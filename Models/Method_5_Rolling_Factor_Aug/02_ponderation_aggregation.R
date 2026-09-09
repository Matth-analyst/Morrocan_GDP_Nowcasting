# ==============================================================================
# 02_ponderation_aggregation.R
#
# Methode 3 -- Agrege les 16 branches : 11 en Factor-Augmented AR (q,r par
# AIC, script 01), 5 en AR pur (Agriculture -- facteur trop court, +
# les 4 branches sans aucun indicateur -- resultats repris de la Methode 2).
# Meme methode de ponderation nominale que les Etapes 6 des deux methodes
# precedentes.
#
# ENTREE : resultats/*_FactorAR_coefficients.csv (11),
#          resultats/*_AR_coefficients.csv (5, repris de la Methode 2),
#          csv/*_deltalog_complet.csv, csv/VA_nominale_base2014.csv
# SORTIE : resultats/PIB_agrege_FactorAR.csv, figures/*.png
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(readxl)
  library(lubridate); library(ggplot2); library(stringr)
})

DATE_DEBUT_TRAIN <- as.Date("2010-01-01")
DATE_FIN_TRAIN   <- as.Date("2021-01-01")

parse_trimestre <- function(s) {
  m <- regmatches(s, regexec("T(\\d)-(\\d{4})", s))[[1]]
  if (length(m) == 3) as.Date(sprintf("%d-%02d-01", as.integer(m[3]), (as.integer(m[2])-1)*3+1)) else NA
}
va_brut <- read_excel("VA_reelle_par_branche.xlsx", skip = 2)
names(va_brut)[1] <- "Trimestre"
va_brut$Date <- sapply(va_brut$Trimestre, parse_trimestre) |> as.Date(origin="1970-01-01")
BRANCHES_16 <- names(va_brut)[!(names(va_brut) %in% c("Trimestre","Date"))]

decaler <- function(x, k) { n <- length(x); if (k==0) return(x); c(rep(NA,k), x[1:(n-k)]) }

MAPPING <- list(
  list(nom_va="Commerce", fichier="Commerce", type="factorAR"),
  list(nom_va="Construction", fichier="Construction", type="factorAR"),
  list(nom_va="Electricite, gaz, eau", fichier="Electricite_gaz_eau", type="factorAR"),
  list(nom_va="Finances et assurances", fichier="Finances_assurances", type="factorAR"),
  list(nom_va="Hebergement-restauration", fichier="Hebergement_restauration", type="factorAR"),
  list(nom_va="Immobilier", fichier="Immobilier", type="factorAR"),
  list(nom_va="Industrie d'extraction", fichier="Industrie_extraction", type="factorAR"),
  list(nom_va="Industrie de transformation", fichier="Industrie_transformation", type="factorAR"),
  list(nom_va="Information-communication", fichier="Information_communication", type="factorAR"),
  list(nom_va="Peche", fichier="Peche", type="factorAR"),
  list(nom_va="Transports", fichier="Transports", type="factorAR"),
  list(nom_va="Agriculture", fichier="Agriculture", type="AR_pur"),
  list(nom_va="Services aux entreprises", fichier="Services_aux_entreprises", type="AR_pur"),
  list(nom_va="Administration publique", fichier="Administration_publique", type="AR_pur"),
  list(nom_va="Education-sante", fichier="Education_sante", type="AR_pur"),
  list(nom_va="Autres services", fichier="Autres_services", type="AR_pur")
)

sq <- function(s){
  s<-gsub("\u00e9|\u00e8|\u00ea","e",s); s<-gsub("\u00c9|\u00c8|\u00ca","E",s)
  toupper(gsub("[^A-Za-z0-9]","",iconv(s,to="ASCII//TRANSLIT",sub="")))
}
for (i in seq_along(MAPPING)) {
  cible_sq <- sq(MAPPING[[i]]$nom_va)
  idx <- which(sapply(BRANCHES_16, function(b) sq(b) == cible_sq))
  if (length(idx) == 0) idx <- which(sapply(BRANCHES_16, function(b) grepl(sq(MAPPING[[i]]$fichier), sq(b), fixed=TRUE)))
  MAPPING[[i]]$nom_va <- unname(BRANCHES_16[idx[1]])
}

reconstruire_factorAR <- function(m) {
  facteur_df <- read_csv(file.path("facteurs", paste0(m$fichier, "_facteur.csv")), show_col_types=FALSE)
  facteur_df$Date <- as.Date(facteur_df$Date)
  coefs <- read_csv(file.path("resultats", paste0(m$fichier, "_FactorAR_coefficients.csv")), show_col_types=FALSE, locale=locale(encoding="UTF-8"))
  intercept <- coefs$Estimate[coefs$variable=="(Intercept)"]

  df <- va_brut %>% select(Date, valeur=all_of(m$nom_va)) %>% arrange(Date) %>% filter(valeur>0)
  df$delta_log <- c(NA, diff(log(df$valeur)))
  df <- df %>% filter(!is.na(delta_log)) %>% select(Date, delta_log)
  fusion <- inner_join(df, facteur_df, by="Date") %>% arrange(Date)

  y_lags <- coefs$variable[grepl("^y_lag", coefs$variable)]
  f_lags <- coefs$variable[grepl("^f_lag", coefs$variable)]
  fusion$Fitted <- intercept
  for (vn in y_lags) { k <- as.integer(gsub("y_lag","",vn)); coef_v <- coefs$Estimate[coefs$variable==vn]; fusion$Fitted <- fusion$Fitted + coef_v*decaler(fusion$delta_log,k) }
  for (vn in f_lags) { j <- as.integer(gsub("f_lag","",vn)); coef_v <- coefs$Estimate[coefs$variable==vn]; fusion$Fitted <- fusion$Fitted + coef_v*decaler(fusion$facteur,j) }
  fusion %>% select(Date, Combine=Fitted) %>% filter(!is.na(Combine))
}

reconstruire_AR_pur <- function(m) {
  coefs <- read_csv(file.path("resultats", paste0(m$fichier, "_AR_coefficients.csv")), show_col_types=FALSE, locale=locale(encoding="UTF-8"))
  intercept <- coefs$Estimate[coefs$variable=="(Intercept)"]
  lags <- coefs %>% filter(variable != "(Intercept)"); p_etoile <- nrow(lags)
  df <- read_csv(file.path("csv", paste0(m$fichier, "_deltalog_complet.csv")), show_col_types=FALSE, locale=locale(encoding="UTF-8"))
  df$Date <- as.Date(df$Date); df$Fitted <- intercept
  for (k in 1:p_etoile) { coef_k <- lags$Estimate[lags$variable==paste0("lag",k)]; df$Fitted <- df$Fitted + coef_k*decaler(df$delta_log,k) }
  df %>% select(Date, Combine=Fitted) %>% filter(!is.na(Combine))
}

cat("=== Reconstruction des 16 series (Factor-AR ou AR pur) ===\n")
toutes_series <- list()
for (m in MAPPING) {
  r <- tryCatch(if (m$type=="factorAR") reconstruire_factorAR(m) else reconstruire_AR_pur(m),
                 error=function(e){cat(sprintf("  [%s] erreur: %s\n", m$nom_va, e$message)); NULL})
  if (!is.null(r)) { toutes_series[[m$nom_va]] <- r; cat(sprintf("  [%s] (%s) OK, %d points\n", m$nom_va, m$type, nrow(r))) }
}

croissance <- Reduce(function(a,b) full_join(a,b,by="Date"),
  lapply(names(toutes_series), function(n) { d<-toutes_series[[n]]; names(d)[2]<-n; d })) %>% arrange(Date)
write_csv(croissance, "resultats/croissance_FactorAR_par_branche.csv")

va_nom <- read_csv("csv/VA_nominale_base2014.csv", show_col_types=FALSE, locale=locale(encoding="UTF-8"))
va_nom$Date <- as.Date(va_nom$Date); names(va_nom)[-1] <- BRANCHES_16
parts_reelles <- va_nom; total_row <- rowSums(parts_reelles[,-1])
for (b in BRANCHES_16) parts_reelles[[b]] <- parts_reelles[[b]]/total_row
premiere_part <- parts_reelles %>% filter(Date==as.Date("2014-01-01"))
dates_manquantes <- seq(DATE_DEBUT_TRAIN, as.Date("2014-01-01")-months(3), by="3 months")
extension <- premiere_part[rep(1,length(dates_manquantes)),]; extension$Date <- dates_manquantes
parts_completes <- bind_rows(extension, parts_reelles) %>% arrange(Date)

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
write_csv(resultat %>% select(Date, PIB_agrege, Realise=Somme16_ld), "resultats/PIB_agrege_FactorAR.csv")

resultat_train <- resultat %>% filter(Date>=DATE_DEBUT_TRAIN, Date<=DATE_FIN_TRAIN, !is.na(PIB_agrege), !is.na(Somme16_ld))
cat(sprintf("\nEn-echantillon (train) : n=%d, correlation=%.3f\n", nrow(resultat_train), cor(resultat_train$PIB_agrege, resultat_train$Somme16_ld)))

df_fig <- resultat %>% filter(Date>=DATE_DEBUT_TRAIN) %>% select(Date, PIB_agrege, Reference=Somme16_ld) %>%
  pivot_longer(cols=c(PIB_agrege,Reference), names_to="serie", values_to="valeur")
p <- ggplot(df_fig, aes(x=Date,y=valeur,color=serie)) + geom_line(linewidth=0.7) +
  labs(title="Factor-Augmented AR agrege vs reference", x=NULL, y="Delta-log", color=NULL) + theme_minimal(base_size=11)
ggsave("figures/PIB_agrege_FactorAR_vs_reference.png", p, width=9, height=4.5, dpi=150)
cat("\nTermine.\n")
