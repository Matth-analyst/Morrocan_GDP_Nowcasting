# ==============================================================================
# 02_ponderation_aggregation.R
#
# Methode 2 -- Agrege les 16 series AR (script 01) en une seule prevision de
# croissance du PIB, EXACTEMENT la meme methode de ponderation nominale que
# le Modele 1 (Etape 6, Higgins 2014, equations 7-8) :
#
#   Delta-log(PIB_t) = sum_i SH_{t-1}^i * Delta-log(VA_i,t^AR)
#
# Seule difference avec le Modele 1 : ici, Delta-log(VA_i,t) vient
# UNIQUEMENT de l'AR de la branche (pas de BVAR, pas de Bridge equation,
# pas de delta) -- exactement le modele de comparaison "Rolling AR"
# de Higgins (Table 5, modele 1), applique a notre propre echantillon.
#
# ENTREE : csv/<branche>_deltalog_complet.csv (script 01),
#          csv/VA_nominale_base2014.csv, VA_reelle_par_branche.xlsx
# SORTIE : resultats/parts_nominales.csv, resultats/PIB_agrege_AR.csv,
#          figures/PIB_agrege_AR_vs_reference.png
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

# ------------------------------------------------------------------------------
# PARTIE A -- Reconstruire les valeurs AJUSTEES (in-sample, train) pour
# chacune des 16 series AR, a partir des coefficients deja estimes (script 01)
# ------------------------------------------------------------------------------
decaler <- function(x, k) { n <- length(x); if (k==0) return(x); c(rep(NA,k), x[1:(n-k)]) }

reconstruire_AR_ajuste <- function(nom_branche) {
  nom_fichier <- str_replace_all(nom_branche, "[^A-Za-z0-9]+", "_")
  coefs <- read_csv(file.path("resultats", paste0(nom_fichier, "_AR_coefficients.csv")),
                      show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  intercept <- coefs$Estimate[coefs$variable == "(Intercept)"]
  lags <- coefs %>% filter(variable != "(Intercept)")
  p_etoile <- nrow(lags)

  df <- read_csv(file.path("csv", paste0(nom_fichier, "_deltalog_complet.csv")),
                   show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  df$Date <- as.Date(df$Date)
  df$Fitted <- intercept
  for (k in 1:p_etoile) {
    coef_k <- lags$Estimate[lags$variable == paste0("lag", k)]
    df$Fitted <- df$Fitted + coef_k * decaler(df$delta_log, k)
  }
  df %>% select(Date, AR = Fitted) %>% filter(!is.na(AR))
}

cat("=== Reconstruction des 16 series AR ajustees ===\n")
toutes_series <- list()
for (b in BRANCHES_16) {
  r <- tryCatch(reconstruire_AR_ajuste(b), error = function(e) { cat(sprintf("  [%s] erreur: %s\n", b, e$message)); NULL })
  if (!is.null(r)) { toutes_series[[b]] <- r; cat(sprintf("  [%s] OK, %d points\n", b, nrow(r))) }
}

croissance_AR <- Reduce(function(a,b) full_join(a,b,by="Date"),
  lapply(names(toutes_series), function(n) { d <- toutes_series[[n]]; names(d)[2] <- n; d })) %>% arrange(Date)
write_csv(croissance_AR, "resultats/croissance_AR_par_branche.csv")

# ------------------------------------------------------------------------------
# PARTIE B -- Parts nominales (identique au Modele 1, Etape 6)
# ------------------------------------------------------------------------------
va_nom <- read_csv("csv/VA_nominale_base2014.csv", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
va_nom$Date <- as.Date(va_nom$Date)
names(va_nom)[-1] <- BRANCHES_16
parts_reelles <- va_nom
total_row <- rowSums(parts_reelles[,-1])
for (b in BRANCHES_16) parts_reelles[[b]] <- parts_reelles[[b]] / total_row

premiere_part <- parts_reelles %>% filter(Date == as.Date("2014-01-01"))
dates_manquantes <- seq(DATE_DEBUT_TRAIN, as.Date("2014-01-01") - months(3), by = "3 months")
extension <- premiere_part[rep(1, length(dates_manquantes)), ]; extension$Date <- dates_manquantes
parts_completes <- bind_rows(extension, parts_reelles) %>% arrange(Date)
write_csv(parts_completes, "resultats/parts_nominales.csv")

# ------------------------------------------------------------------------------
# PARTIE C -- Agregation, SH_{t-1}, sur toute la plage disponible (train +
# fenetre de test, comme le Modele 1 -- ce script sert aux deux usages,
# le script 03 se chargera de restreindre a la fenetre de test)
# ------------------------------------------------------------------------------
parts_decalees <- parts_completes
parts_decalees$Date <- parts_decalees$Date %m+% months(3)

resultat <- croissance_AR %>% left_join(parts_decalees, by = "Date", suffix = c("_croissance", "_part"))

resultat$PIB_agrege_AR <- NA
for (i in seq_len(nrow(resultat))) {
  s <- 0; poids_total <- 0
  for (b in BRANCHES_16) {
    col_c <- paste0(b, "_croissance"); col_p <- paste0(b, "_part")
    if (col_c %in% names(resultat) && col_p %in% names(resultat)) {
      c_val <- resultat[[col_c]][i]; p_val <- resultat[[col_p]][i]
      if (!is.na(c_val) && !is.na(p_val)) { s <- s + p_val*c_val; poids_total <- poids_total + p_val }
    }
  }
  resultat$PIB_agrege_AR[i] <- if (poids_total > 0) s / poids_total else NA
}

va_somme <- va_brut %>% mutate(Somme16 = rowSums(across(all_of(BRANCHES_16)))) %>% select(Date, Somme16) %>% arrange(Date)
va_somme$Somme16_ld <- c(NA, diff(log(va_somme$Somme16)))
resultat <- resultat %>% left_join(va_somme %>% select(Date, Somme16_ld), by = "Date")

write_csv(resultat %>% select(Date, PIB_agrege_AR, Realise = Somme16_ld), "resultats/PIB_agrege_AR.csv")

resultat_train <- resultat %>% filter(Date >= DATE_DEBUT_TRAIN, Date <= DATE_FIN_TRAIN, !is.na(PIB_agrege_AR), !is.na(Somme16_ld))
cat(sprintf("\nEn-echantillon (train) : n=%d, correlation=%.3f\n",
            nrow(resultat_train), cor(resultat_train$PIB_agrege_AR, resultat_train$Somme16_ld)))

df_fig <- resultat %>% filter(Date >= DATE_DEBUT_TRAIN) %>% select(Date, PIB_agrege_AR, Reference = Somme16_ld) %>%
  pivot_longer(cols = c(PIB_agrege_AR, Reference), names_to = "serie", values_to = "valeur")
p <- ggplot(df_fig, aes(x = Date, y = valeur, color = serie)) + geom_line(linewidth = 0.7) +
  labs(title = "Rolling AR agrege vs reference (somme des 16 VA reelles)", x = NULL, y = "Delta-log", color = NULL) +
  theme_minimal(base_size = 11)
ggsave("figures/PIB_agrege_AR_vs_reference.png", p, width = 9, height = 4.5, dpi = 150)

cat("\nTermine. Resultats dans 'resultats/', figure dans 'figures/'.\n")
