# ==============================================================================
# 02_test_hors_echantillon.R
#
# Methode 4 -- Test hors-echantillon, meme fenetre que les 3 methodes
# precedentes (2021T2-2026T1, 20 trimestres).
# ==============================================================================

suppressPackageStartupMessages({ library(dplyr); library(readr); library(ggplot2); library(tidyr) })

DATE_DEBUT_TEST <- as.Date("2021-04-01")
DATE_FIN_TEST   <- as.Date("2026-01-01")

resultat <- read_csv("resultats/PIB_agrege_BVAR.csv", show_col_types = FALSE)
resultat$Date <- as.Date(resultat$Date)

resultat_test <- resultat %>% filter(Date >= DATE_DEBUT_TEST, Date <= DATE_FIN_TEST,
                                       !is.na(PIB_agrege), !is.na(Realise))

pib_prevu_annualise <- 100*((exp(resultat_test$PIB_agrege))^4 - 1)
pib_realise_annualise <- 100*((exp(resultat_test$Realise))^4 - 1)
rmsfe <- sqrt(mean((pib_prevu_annualise - pib_realise_annualise)^2))
corr_test <- cor(resultat_test$PIB_agrege, resultat_test$Realise)

cat(sprintf("RESULTAT DU TEST HORS-ECHANTILLON (Methode 4, BVAR simple) :\n"))
cat(sprintf("  n = %d trimestres\n", nrow(resultat_test)))
cat(sprintf("  Correlation = %.3f\n", corr_test))
cat(sprintf("  RMSFE (croissance annualisee) = %.2f points\n", rmsfe))

write_csv(data.frame(n=nrow(resultat_test), correlation=corr_test, RMSFE=rmsfe), "resultats/metriques_test_BVAR.csv")

df_fig <- resultat_test %>% select(Date, Prevu=PIB_agrege, Realise) %>%
  pivot_longer(cols=c(Prevu,Realise), names_to="serie", values_to="valeur")
p <- ggplot(df_fig, aes(x=Date,y=valeur,color=serie)) + geom_line(linewidth=0.8) + geom_point(size=1.5) +
  labs(title="Methode 4 (BVAR simple) -- Test hors-echantillon (2021T2-2026T1)",
       subtitle=sprintf("n=%d, correlation=%.3f, RMSFE=%.2f points", nrow(resultat_test), corr_test, rmsfe),
       x=NULL, y="Delta-log", color=NULL) + theme_minimal(base_size=11)
ggsave("figures/test_hors_echantillon_BVAR.png", p, width=9, height=4.5, dpi=150)
cat("\nTermine.\n")
