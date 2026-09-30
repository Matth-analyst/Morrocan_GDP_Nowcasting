# ============================================================================
# 08a_nowcast_courant.R -- Le nowcast du trimestre en cours
# ============================================================================
# Extrait de 08_rapport_synthese.R le 15 septembre 2026, apres qu'un run complet
# depuis un dossier vide a revele une SECONDE DEPENDANCE CIRCULAIRE, de la meme
# famille que celle des poids :
#
#     12c_intervalle_combinaison.R  lit  08_nowcast_courant_agregat.csv
#     08_rapport_synthese.R         lit  12c_nowcast_publie.csv
#
# Elle etait plus discrete que la premiere : 12c protege sa lecture par un
# file.exists(), donc il ne plantait pas -- il ecrivait "nowcast courant absent"
# et passait. Sur un dossier deja peuple, le fichier trainait et tout semblait
# normal. Sur un dossier vide, l'intervalle publie disparaissait en silence.
#
# Le nowcast courant ne depend d'aucun intervalle : il se calcule des que le
# backtest intra-trimestriel est disponible. C'est donc une etape a part,
# placee avant 12c.
#
# Le chiffre est produit par la FONCTION CENTRALE (R/fonctions/nowcast.R), la
# meme que celle validee a l'etape 7 contre le pipeline phase par phase : c'est
# ce qui garantit que le chiffre publie suit exactement le chemin evalue.
#
# Sorties : resultats/08_nowcast_courant_agregat.csv
#           resultats/08_nowcast_courant_branches.csv   (noms inchanges)
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/passerelle.R")
source("R/fonctions/kalman.R")
source("R/fonctions/bvar.R")
source("R/fonctions/branches_instables.R")
source("R/fonctions/nowcast.R")

r <- function(x) file.path(DOSSIER_RESULTATS, x)

cat("\n[1/3] Nowcast du trimestre courant\n")
contexte <- preparer_contexte()
derniere_va <- max(contexte$va$date)
CIBLE <- fin_trimestre(debut_trimestre(derniere_va) %m+% months(3))

# Combien de mois du trimestre cible sont deja observes ? Le scenario n'est pas
# choisi, il est CONSTATE : c'est la situation operationnelle reelle.
mois_cible <- mois_du_trimestre(CIBLE)
mois_vus <- sum(mois_cible %in% unique(contexte$ind_brut$date))
SCENARIO <- c("M0", "M1", "M2", "M3")[mois_vus + 1L]
cat(sprintf("      derniere VA : %s | cible : %s | %d mois observes -> scenario %s\n",
            date_vers_trimestre(derniere_va), date_vers_trimestre(CIBLE),
            mois_vus, SCENARIO))

historique <- lire_csv(r("09_previsions_intra.csv")) %>%
  dplyr::mutate(origine = as.Date(origine)) %>%
  dplyr::filter(scenario == "M3") %>%
  dplyr::select(branche, origine, reel, bvar, bridge)

t0 <- Sys.time()
nc <- backtest_nowcast(CIBLE, scenario = SCENARIO, historique = historique,
                       contexte = contexte)
cat(sprintf("      %s\n", format(round(difftime(Sys.time(), t0, units = "mins"), 2))))
if (is.null(nc)) stop("Le nowcast courant n'a pas pu etre produit.", call. = FALSE)

nowcast_branches <- nc$branches %>%
  dplyr::transmute(branche, w,
                   bvar_pct = 100 * bvar,
                   bridge_pct = 100 * bridge,
                   delta, nowcast_pct = 100 * nowcast,
                   source = ifelse(is.na(bridge), "BVAR seul", "combinaison"),
                   n_retenus,
                   # Garde-fou : z rapporte la prevision a la volatilite de la
                   # branche, estimee sur la seule information anterieure. Une
                   # branche tres volatile peut afficher un gros pourcentage sans
                   # que ce soit anormal -- c'est le cas de la peche.
                   ecart_type_pct = 100 * sigma,
                   z_amplitude = z_amplitude,
                   signale)
ecrire_csv(nowcast_branches, r("08_nowcast_courant_branches.csv"))
agregat_courant <- 100 * nc$agregat$nowcast
ecrire_csv(nc$agregat %>% dplyr::mutate(nowcast_pct = 100 * nowcast),
           r("08_nowcast_courant_agregat.csv"))
cat(sprintf("      VA totale, %s : %+.2f %%\n",
            date_vers_trimestre(CIBLE), agregat_courant))
