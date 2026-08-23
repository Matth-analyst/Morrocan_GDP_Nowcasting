source("R/00_setup.R")

indicateurs_dfm <- read.csv("data/indicateurs_dfm.csv", encoding = "UTF-8") %>%
  mutate(date = as.Date(date)) %>% as_tibble()

cat("Indicateurs DFM importés :", n_distinct(indicateurs_dfm$branche, indicateurs_dfm$indicateur),
    "séries,", nrow(indicateurs_dfm), "observations.\n\n")
print(indicateurs_dfm %>% distinct(branche, indicateur) %>% count(branche, name = "n_series"))

saveRDS(indicateurs_dfm, file.path(DOSSIER_RESULTATS, "indicateurs_dfm.rds"))
