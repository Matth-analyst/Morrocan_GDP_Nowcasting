# ==============================================================================
# 01_bvar_et_ponderation.R
#
# Methode 4 (comparaison a Higgins, Table 5, modele 4 "Quarterly BVAR") --
# Higgins (citation exacte) : « We use the same 5-lag quarterly BVAR [...]
# The growth forecasted is then computed using the chain-weighting formula. »
#
# C'est le BVAR DEJA CONSTRUIT (Method_1, Etape 1, corrige -- train
# 2010-2021, sans fuite de donnees), agrege DIRECTEMENT par les poids
# nominaux (Etape 6) -- SANS AUCUNE combinaison avec une bridge equation
# ou un AR (contrairement a la Methode 1) :
#
#   Delta-log(PIB_t) = sum_i SH_{t-1}^i * Delta-log(VA_i,t^BVAR)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(readxl)
  library(lubridate); library(ggplot2)
})

DATE_DEBUT_TRAIN <- as.Date("2010-01-01")
DATE_FIN_TRAIN   <- as.Date("2021-01-01")
DATE_FIN_TEST    <- as.Date("2026-01-01")
P_BVAR <- 5

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

va_ld <- va_brut %>% select(Date, all_of(BRANCHES_16)) %>% arrange(Date)
for (b in BRANCHES_16) va_ld[[b]] <- c(NA, diff(log(va_ld[[b]])))
va_ld <- va_ld %>% filter(!is.na(.data[[BRANCHES_16[1]]]))

coefs_bvar <- read_csv("coefficients_complets_BVAR.csv", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

reconstruire_bvar_branche <- function(nom_branche) {
  coefs_eq <- coefs_bvar %>% filter(equation == nom_branche)
  constante <- coefs_eq %>% filter(type == "constante") %>% pull(coefficient)
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
  data.frame(Date = dates_valides, BVAR = fitted) %>% filter(!is.na(BVAR))
}

cat("=== Reconstruction du BVAR pour les 16 branches ===\n")
toutes_series <- list()
for (b in BRANCHES_16) {
  r <- reconstruire_bvar_branche(b)
  toutes_series[[b]] <- r
  cat(sprintf("  [%s] %d points\n", b, nrow(r)))
}
croissance_BVAR <- Reduce(function(a,b) full_join(a,b,by="Date"),
  lapply(names(toutes_series), function(n) { d<-toutes_series[[n]]; names(d)[2]<-n; d })) %>% arrange(Date)
write_csv(croissance_BVAR, "resultats/croissance_BVAR_par_branche.csv")

va_nom <- read_csv("csv/VA_nominale_base2014.csv", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
va_nom$Date <- as.Date(va_nom$Date); names(va_nom)[-1] <- BRANCHES_16
parts_reelles <- va_nom; total_row <- rowSums(parts_reelles[,-1])
for (b in BRANCHES_16) parts_reelles[[b]] <- parts_reelles[[b]]/total_row
premiere_part <- parts_reelles %>% filter(Date == as.Date("2014-01-01"))
dates_manquantes <- seq(DATE_DEBUT_TRAIN, as.Date("2014-01-01")-months(3), by="3 months")
extension <- premiere_part[rep(1,length(dates_manquantes)),]; extension$Date <- dates_manquantes
parts_completes <- bind_rows(extension, parts_reelles) %>% arrange(Date) %>% filter(Date<=DATE_FIN_TEST)
write_csv(parts_completes, "resultats/parts_nominales.csv")

parts_decalees <- parts_completes; parts_decalees$Date <- parts_decalees$Date %m+% months(3)
resultat <- croissance_BVAR %>% left_join(parts_decalees, by="Date", suffix=c("_croissance","_part"))

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
write_csv(resultat %>% select(Date, PIB_agrege, Realise=Somme16_ld), "resultats/PIB_agrege_BVAR.csv")

resultat_train <- resultat %>% filter(Date>=DATE_DEBUT_TRAIN, Date<=DATE_FIN_TRAIN, !is.na(PIB_agrege), !is.na(Somme16_ld))
cat(sprintf("\nEn-echantillon (train) : n=%d, correlation=%.3f\n", nrow(resultat_train), cor(resultat_train$PIB_agrege, resultat_train$Somme16_ld)))

df_fig <- resultat %>% filter(Date>=DATE_DEBUT_TRAIN) %>% select(Date, PIB_agrege, Reference=Somme16_ld) %>%
  pivot_longer(cols=c(PIB_agrege,Reference), names_to="serie", values_to="valeur")
p <- ggplot(df_fig, aes(x=Date,y=valeur,color=serie)) + geom_line(linewidth=0.7) +
  labs(title="BVAR simple agrege vs reference (2010-2026)", x=NULL, y="Delta-log", color=NULL) + theme_minimal(base_size=11)
ggsave("figures/PIB_agrege_BVAR_vs_reference.png", p, width=9, height=4.5, dpi=150)
cat("\nTermine.\n")
