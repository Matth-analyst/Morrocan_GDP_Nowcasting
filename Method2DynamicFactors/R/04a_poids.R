# ============================================================================
# 06a_poids.R -- Les poids d'agregation, en prix courants
# ============================================================================
# Extrait de 06_agregation_fisher.R le 14 septembre 2026, apres qu'un run
# complet depuis un dossier resultats/ vide a revele une DEPENDANCE CIRCULAIRE :
#
#     03e_branches_instables.R  lit  06_poids.csv   (pour evaluer l'agregat)
#     06_agregation_fisher.R    lit  03e_previsions_corrigees.csv
#
# Le cycle ne se voyait pas tant que 06_poids.csv trainait d'une execution
# precedente -- exactement le genre d'etat cache qu'un run de bout en bout est
# cense mettre au jour.
#
# La sortie du cycle est simple : les poids ne dependent d'AUCUNE prevision.
# Ils se calculent a partir du seul fichier de valeur ajoutee nominale. Ils
# constituent donc une etape a part entiere, placee avant 03e.
#
# Sortie : resultats/06_poids.csv  (nom inchange : rien d'autre n'a bouge)
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")

chemin_res <- function(x) file.path(DOSSIER_RESULTATS, paste0("06_", x))

cat("\n[1/6] Poids en prix courants\n")

# Correspondance explicite entre la nomenclature du fichier nominal et celle du
# projet. L'ordre des colonnes coincide, mais s'appuyer sur cette coincidence
# sans la declarer serait fragile : une colonne ajoutee en amont decalerait
# silencieusement toutes les branches.
CORRESP_NOMINALE <- c(
  "Agriculture"                                = "Agriculture",
  "Pêche"                                  = "Pêche",
  "Industrie d’extraction"                 = "Industrie d'extraction",
  "Industrie de transformation"                 = "Industrie de transformation",
  "Distribution d’électricité et de gaz- Distribution d’eau, réseau d’assainissement, traitement des déchets" = "Électricité, gaz, eau",
  "Construction"                                = "Construction",
  "Commerce de gros et de détail; réparation de véhicules automobiles et de motocycles" = "Commerce",
  "Transports et entreposage"                   = "Transports",
  "Activités d’hébergement et de restauration" = "Hébergement-restauration",
  "Information et communication"                = "Information-communication",
  "Activités financières et d’assurances" = "Finances et assurances",
  "Activités immobilières"            = "Immobilier",
  "Recherches et développement et services rendus aux entreprises" = "Services aux entreprises",
  "Administration publique et défense; sécurité sociale obligatoire" = "Administration publique",
  "Education, santé humaine et activités d’action sociale" = "Éducation-santé",
  "Autres services"                             = "Autres services")

nominale <- lire_csv(file.path(DOSSIER_SOURCES, "VA_nominale_base2014.csv"))
names(nominale)[1] <- "date"
colonnes <- names(nominale)[-1]

# Appariement par LIBELLE quand il est reconnu, par position sinon -- et l'on
# signale tout ce qui n'a pas ete reconnu, au lieu de le deviner en silence.
reconnues <- colonnes %in% names(CORRESP_NOMINALE)
if (!all(reconnues)) {
  cat("      ! libelles non reconnus, appariement par position :\n")
  for (cc in colonnes[!reconnues]) cat(sprintf("        %s\n", substr(cc, 1, 70)))
}
if (length(colonnes) != length(TOUTES_BRANCHES)) {
  stop("Le fichier nominal compte ", length(colonnes), " branches au lieu de ",
       length(TOUTES_BRANCHES), call. = FALSE)
}
noms_projet <- ifelse(reconnues, CORRESP_NOMINALE[colonnes], TOUTES_BRANCHES)
stopifnot("La correspondance ne couvre pas les 16 branches du projet" =
            setequal(noms_projet, TOUTES_BRANCHES))
names(nominale)[-1] <- noms_projet
cat(sprintf("      %d branches appariees (%d par libelle)\n",
            length(noms_projet), sum(reconnues)))

poids <- nominale %>%
  dplyr::mutate(date = fin_trimestre(as.Date(date))) %>%
  tidyr::pivot_longer(-date, names_to = "branche", values_to = "va_nominale") %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(w = va_nominale / sum(va_nominale)) %>%
  dplyr::ungroup()
stopifnot("Les poids ne somment pas a 1" =
            all(abs(tapply(poids$w, poids$date, sum) - 1) < 1e-10))
ecrire_csv(poids, chemin_res("poids.csv"))
cat(sprintf("      %d trimestres de poids, de %s a %s\n",
            dplyr::n_distinct(poids$date),
            date_vers_trimestre(min(poids$date)),
            date_vers_trimestre(max(poids$date))))

