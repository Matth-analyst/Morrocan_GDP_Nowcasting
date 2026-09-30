# ============================================================================
# 08_rapport_synthese.R -- ETAPE 9 : les resultats finaux
# ============================================================================
# Plan de correction, etape 9 : "produire les resultats finaux a partir du
# nouveau backtest".
#
# Ce document ne reprend pas le detail pedagogique des rapports de phase -- il
# y renvoie. Il donne l'architecture finale, les chiffres qui en sortent, le
# nowcast du trimestre en cours, et une appreciation honnete de ce que le
# systeme sait et ne sait pas faire.
#
# Le nowcast courant est produit ici par la FONCTION CENTRALE
# (R/fonctions/nowcast.R), la meme que celle validee a l'etape 7 contre le
# pipeline phase par phase. C'est ce qui garantit que le chiffre publie suit
# exactement le chemin qui a ete evalue.
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
source("R/fonctions/rapport.R")

CHEMIN <- file.path(DOSSIER_RAPPORT, "rapport_synthese.html")
init_compteurs()
assign(".n_equation", 0L, envir = globalenv())

r <- function(x) file.path(DOSSIER_RESULTATS, x)

# ============================================================================
# 1) LE NOWCAST COURANT
# ============================================================================
cat("\n[1/3] Nowcast du trimestre courant\n")

# Le calcul est fait par R/08a_nowcast_courant.R, etape distincte parce que 12c
# en a besoin avant que ce rapport ne puisse etre compose (voir l'en-tete 08a).
if (!file.exists(r("08_nowcast_courant_agregat.csv"))) source("R/08a_nowcast_courant.R")
nowcast_branches <- lire_csv(r("08_nowcast_courant_branches.csv"))
nc_agregat       <- lire_csv(r("08_nowcast_courant_agregat.csv"))
CIBLE            <- as.Date(nc_agregat$origine[1])
SCENARIO         <- nc_agregat$scenario[1]
agregat_courant  <- nc_agregat$nowcast_pct[1]
derniere_va      <- fin_trimestre(debut_trimestre(CIBLE) %m-% months(3))
mois_vus         <- match(SCENARIO, c("M0", "M1", "M2", "M3")) - 1L
cat(sprintf("      cible : %s | scenario %s | VA totale : %+.2f %%\n",
            date_vers_trimestre(CIBLE), SCENARIO, agregat_courant))

# ============================================================================
# 2) LECTURE DES RESULTATS
# ============================================================================
cat("\n[2/3] Lecture des resultats\n")
ev_ag   <- lire_csv(r("06_evaluation_agregat.csv"))
ag      <- lire_csv(r("06_agregat.csv")) %>% dplyr::mutate(origine = as.Date(origine))
intra   <- lire_csv(r("09_perimetre_commun.csv"))
ic      <- lire_csv(r("10_intervalles_ratio.csv"))
dm_gr   <- lire_csv(r("10_dm_groupe.csv"))
etape5  <- lire_csv(r("05_par_periode.csv"))
ctrl    <- lire_csv(r("07_controles_antilookahead.csv"))
nonreg  <- lire_csv(r("07_non_regression.csv"))
diag3   <- lire_csv(r("03_diagnostics_branches.csv"))
ep      <- lire_csv(r("09b_par_episode.csv"))
instab  <- lire_csv(r("03e_diagnostic_instabilite.csv"))
effet   <- lire_csv(r("03e_effet_correction.csv"))
A_IC <- all(file.exists(r(c("12c_nowcast_publie.csv", "12c_couverture.csv",
                            "12_calibration.csv", "12_decomposition.csv"))))
if (A_IC) {
  ic_now  <- lire_csv(r("12c_nowcast_publie.csv"))
  ic_couv <- lire_csv(r("12c_couverture.csv"))
  calib   <- lire_csv(r("12_calibration.csv"))
  decomp  <- lire_csv(r("12_decomposition.csv"))
}

A_BENCH <- all(file.exists(r(c(
  "13_evaluation.csv", "13_dm_contre_systeme.csv",
  "14_robustesse_delta.csv", "14_robustesse_fenetre.csv",
  "14_robustesse_selection.csv", "14_saisonnalite.csv",
  "15_contributions_branches.csv", "15_diagnostics_residus.csv",
  "15_episodes.csv", "15_stabilite_coefficients.csv",
  "16_comparaison.csv", "16_dm_direct_indirect.csv"))))
if (A_BENCH) {
  bench  <- lire_csv(r("13_evaluation.csv"))
  dm13   <- lire_csv(r("13_dm_contre_systeme.csv"))
  rob_d  <- lire_csv(r("14_robustesse_delta.csv"))
  rob_f  <- lire_csv(r("14_robustesse_fenetre.csv"))
  rob_s  <- lire_csv(r("14_robustesse_selection.csv"))
  sais   <- lire_csv(r("14_saisonnalite.csv"))
  courbe_d <- if (file.exists(r("14b_courbe_delta.csv")))
                lire_csv(r("14b_courbe_delta.csv")) else NULL
  comp_d   <- if (file.exists(r("14b_estime_contre_constante.csv")))
                lire_csv(r("14b_estime_contre_constante.csv")) else NULL
  stab_d   <- if (file.exists(r("14b_stabilite_delta.csv")))
                lire_csv(r("14b_stabilite_delta.csv")) else NULL
  contrib <- lire_csv(r("15_contributions_branches.csv"))
  resid  <- lire_csv(r("15_diagnostics_residus.csv"))
  epis   <- lire_csv(r("15_episodes.csv"))
  stab   <- lire_csv(r("15_stabilite_coefficients.csv"))
  comp16 <- lire_csv(r("16_comparaison.csv"))
  dm16   <- lire_csv(r("16_dm_direct_indirect.csv"))
}

ratio_ag  <- ev_ag$ratio[ev_ag$modele == "Nowcast du projet"]
ratio_bv  <- ev_ag$ratio[ev_ag$modele == "BVAR seul, agrege"]
correl_ag <- ev_ag$correlation[ev_ag$modele == "Nowcast du projet"]

cat("[3/3] Composition\n")
h <- character(0)
ajouter <- function(...) h <<- c(h, paste0(..., collapse = ""))

ajouter(entete_rapport(
  "GDPNow-Maroc &middot; Méthode 1",
  "Synthèse — Le système de nowcasting, ce qu'il vaut et ce qu'il ignore",
  paste("Architecture finale, résultats du backtest pseudo temps réel,",
        "nowcast du trimestre courant, et limites."),
  "<code>data/</code>, <code>resultats/</code> &middot; plan de correction, étape 9"))

sections <- c(
  "Ce que le système produit",
  "L'architecture, en quatre étages",
  "Le protocole d'évaluation",
  "Résultat principal : l'agrégat",
  "Résultat par branche, et les branches que le BVAR abîme",
  "La valeur de l'information au fil du trimestre",
  "Ce que les corrections ont changé",
  "Le nowcast du trimestre courant",
  "Les étalons : ce que le système bat, et de combien",
  "Robustesse : ce qui ne change pas les conclusions",
  "D'où vient l'erreur",
  "Direct ou indirect : faut-il passer par les seize branches ?",
  "Ce que le système ne sait pas faire",
  "Reproduire ces résultats")
ajouter(sommaire_rapport(sections))

ajouter(chiffres_cles(c(
  "ratio du nowcast agrégé" = nb(ratio_ag, 3),
  "corrélation prévu / réalisé" = nb(correl_ag, 2),
  "BVAR seul, même agrégat" = nb(ratio_bv, 3),
  "trimestres évalués" = nb(nrow(ag)),
  "nowcast du trimestre courant" = sprintf("%+.2f %%", agregat_courant),
  "intervalle (couverture 75 %)" = if (A_IC) {
    l <- ic_now %>% dplyr::filter(niveau == 0.80)
    sprintf("%+.2f à %+.2f %%", l$bas_pct, l$haut_pct)
  } else "&mdash;"
)))

# ---------------------------------------------------------------- 1
ajouter("<h2 id='s1'><span class='num'>1.</span>Ce que le système produit</h2>")
ajouter("<p>Une estimation de la croissance trimestrielle de la <strong>valeur ",
        "ajoutée totale</strong> du Maroc, et des seize branches qui la composent, ",
        "<em>avant</em> que les comptes nationaux ne la publient.</p>")
ajouter(definition("Valeur ajoutée totale, et non PIB",
  paste0("<p>La distinction n'est pas cosmétique. Le PIB vaut la valeur ajoutée ",
         "totale <strong>plus les impôts sur les produits nets des subventions</strong>, ",
         "qui ne figurent pas dans cette base. Ce qui est prévu ici est donc la ",
         "somme des branches, pas le PIB.</p>",
         "<p>S'y ajoute une limite comptable : les comptes sont en <strong>volumes ",
         "chaînés non additifs</strong>. La somme des valeurs ajoutées de branche en ",
         "prix chaînés n'est pas exactement la valeur ajoutée totale chaînée. ",
         "L'agrégat calculé ici est un indice de volume reconstruit à partir des ",
         "branches — et c'est le même indice qui sert de réalisation et de prévision, ",
         "donc la comparaison reste licite.</p>")))

# ---------------------------------------------------------------- 2
ajouter("<h2 id='s2'><span class='num'>2.</span>L'architecture, en quatre étages</h2>")
etages <- data.frame(
  Étage = c("1. BVAR trimestriel", "2. Passerelles", "3. Combinaison", "4. Agrégation"),
  `Ce qu'il fait` = c(
    "relie les seize branches entre elles par leur passé, sous prior de Minnesota",
    "relie chaque branche couverte à ses indicateurs mensuels, agrégés en trimestriel",
    "pondère les deux précédents à parts égales, δ = 0,5 (section 10.1)",
    "somme les branches avec des poids en prix courants du trimestre précédent"),
  `Couverture` = c("16 branches", "12 branches", "12 branches",
                   "16 branches"),
  `Phase` = c("3", "4 et 13", "4 (phase 10 du plan)", "6 (phase 11 du plan)"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(etages, aligne_droite = integer(0)))
ajouter(legende_tableau("Les quatre étages du système."))
ajouter("<p>Pour les <strong>quatre branches sans indicateur</strong> — services aux ",
        "entreprises, administration publique, éducation-santé, autres services — le ",
        "plan prévoyait un AR(4). L'étape 5 a montré qu'il est <strong>battu par le ",
        "BVAR</strong> (ratio médian 1,14 contre 0,961), et c'est donc le BVAR qui les ",
        "prend en charge. Ces branches n'ont pas d'indicateurs, mais elles ne sont pas ",
        "isolées : le BVAR les relie aux douze autres.</p>")
ajouter(definition("Une branche peut être sortie du BVAR",
  paste0("<p>Un cinquième mécanisme s'ajoute aux quatre étages, et il est ",
         "défensif. À chaque origine, une branche dont la <strong>variance du ",
         "premier tiers de l'échantillon dépasse dix fois celle du reste</strong> ",
         "est retirée du BVAR : elle reçoit sa moyenne sur les 60 derniers ",
         "trimestres.</p>",
         "<p>Le BVAR reste inchangé pour toutes les autres — on ne touche pas à la ",
         "donnée commune, on se contente de ne pas <em>utiliser</em> sa prévision ",
         "là où elle nuit. La section 5 expose le cas qui a motivé cette règle.</p>")))

ajouter("<p>Le nowcast d'une branche s'écrit :</p>")
ajouter(eq(paste0(m("&#285;<sub>j,T</sub>"), " = ", m("&delta;<sub>j,T</sub>"), " ",
                  m("&#285;<sub>j,T</sub><sup>BVAR</sup>"),
                  " <span class='op'>+</span> ( 1 <span class='op'>&minus;</span> ",
                  m("&delta;<sub>j,T</sub>"), " ) ",
                  m("&#285;<sub>j,T</sub><sup>passerelle</sup>"))))
ajouter("<p>et l'agrégat, en indice de volume de Laspeyres :</p>")
ajouter(eq(paste0(m("&#285;<sub>T</sub><sup>total</sup>"), " = log ",
                  "<span class='op'>(</span> ",
                  "<span class='big'>&sum;</span><sub class='num'>j</sub> ",
                  m("w<sub>j,T&minus;1</sub>"), " exp( ", m("&#285;<sub>j,T</sub>"),
                  " ) <span class='op'>)</span>")))
ajouter("<p>Les poids ", m("w<sub>j,T&minus;1</sub>"), " sont les parts en ",
        "<strong>prix courants</strong> du trimestre précédent — disponibles avant la ",
        "cible, et recalculés à chaque origine. C'est ce qui borne l'évaluation à ",
        "T2-2014 : la valeur ajoutée nominale ne commence qu'en 2014.</p>")

# ---------------------------------------------------------------- 3
ajouter("<h2 id='s3'><span class='num'>3.</span>Le protocole d'évaluation</h2>")
ajouter("<p>Tout est réestimé à <strong>chaque</strong> trimestre cible, sur la seule ",
        "information disponible alors : hyperparamètres du prior, échelles, dates de ",
        "choc, liste d'indicateurs, coefficients, poids de combinaison, poids ",
        "sectoriels. Rien n'est calibré une fois pour toutes.</p>")
t3 <- ctrl %>% dplyr::transmute(Contrôle = controle, Objet = objet,
                                `Ce qui est vérifié` = description,
                                Résultat = resultat, Portée = detail)
ajouter(tbl(as.data.frame(t3), aligne_droite = integer(0)))
ajouter(legende_tableau("Les cinq contrôles anti-look-ahead de la section 30 du plan."))
ajouter(definition("Le contrôle qui vaut le plus",
  paste0("<p>Les cinq contrôles ci-dessus vérifient chacun une phase. Aucun ne peut ",
         "voir une <em>incohérence entre phases</em> — et c'est exactement ce type de ",
         "défaut qui s'est produit : la borne de 2014, qui vient des poids, avait été ",
         "transposée au BVAR qui n'en a pas besoin.</p>",
         "<p>L'étape 7 ajoute donc un <strong>test de non-régression</strong> : une ",
         "fonction unique refait toute la chaîne depuis les données brutes, sans lire ",
         "aucun résultat intermédiaire, et ses sorties sont confrontées à celles du ",
         "pipeline. Sur ", nb(nrow(nonreg)), " comparaisons, l'écart maximal est de ",
         "<strong>4,4 &times; 10<sup>&minus;16</sup></strong> — la précision machine. ",
         "Deux chemins de calcul indépendants donnent le même nombre.</p>")))

# ---------------------------------------------------------------- 4
ajouter("<h2 id='s4'><span class='num'>4.</span>Résultat principal : l'agrégat</h2>")
t4 <- ev_ag %>% dplyr::arrange(ratio) %>%
  dplyr::transmute(Modèle = modele, `n` = n,
                   `RMSFE (%)` = nb(100 * RMSFE, 2),
                   `Ratio` = nb(ratio, 3),
                   `Corrélation` = nb(correlation, 2),
                   `Biais (pt)` = nb(100 * biais, 2))
ajouter(tbl(as.data.frame(t4), aligne_droite = 2:6))
ajouter(legende_tableau(paste0(
  "Valeur ajoutée totale, ", nrow(ag), " trimestres, T2-2014 à T1-2026.")))
ajouter(sprintf(paste0("<p>Le nowcast complet donne un ratio de <strong>%s</strong> et ",
                       "une corrélation de <strong>%s</strong>. Le BVAR seul agrégé ",
                       "donne %s.</p>"),
                nb(ratio_ag, 3), nb(correl_ag, 2), nb(ratio_bv, 3)))
ajouter(intuition(paste0(
  "<p>Le ratio rapporte l'erreur à l'écart-type de la série : sous 1, le modèle fait ",
  "mieux que prédire la moyenne historique. À ", nb(ratio_ag, 3), ", l'erreur vaut ",
  sprintf("%.0f", 100 * ratio_ag), " % de ce qu'on commettrait sans modèle.</p>",
  "<p>Fait notable : <strong>l'apport de la passerelle est plus net sur l'agrégat que ",
  "sur la moyenne des branches</strong>. Les branches où elle est bonne sont aussi ",
  "celles qui pèsent, et les erreurs qu'elle corrige ne se compensent pas dans ",
  "l'agrégation.</p>")))
ajouter(figure("06_agregat.png", "Valeur ajoutée totale : réalisé et nowcast",
               paste0("Le nowcast suit la réalisation, y compris l'effondrement de ",
                      "2020 que le BVAR seul, en pointillé, ne voit pas. En période ",
                      "calme les trois courbes se confondent davantage — c'est là que ",
                      "le gain se réduit.")))
ajouter(figure("06_poids.png", "Poids des branches en prix courants",
               paste0("Recalculés à chaque trimestre. Le plan demandait explicitement ",
                      "de supprimer les poids fixes issus du dernier trimestre de la ",
                      "base ; un contrôle bloquant refuse désormais un jeu de poids ",
                      "unique.")))

# ---------------------------------------------------------------- 5
ajouter("<h2 id='s5'><span class='num'>5.</span>Résultat par branche, et les branches que le BVAR abîme</h2>")
t5 <- diag3 %>% dplyr::arrange(ratio) %>%
  dplyr::transmute(Branche = branche,
                   `Écart-type (%)` = nb(100 * ecart_type_reel, 2),
                   `Ratio BVAR` = nb(ratio, 3))
ajouter(tbl(as.data.frame(t5), aligne_droite = 2:3))
ajouter(legende_tableau("Qualité du BVAR par branche, avant correction, 48 origines."))
ajouter("<p>La dispersion entre branches est bien plus grande que l'écart entre ",
        "modèles. Les branches volatiles sont les mieux prévues, ce qui est en partie ",
        "mécanique : leur dénominateur est grand.</p>")

ajouter("<h3>5.1 Un ratio supérieur à 1 ne veut pas dire « imprévisible »</h3>")
ajouter(intuition(paste0(
  "<p>Une série purement aléatoire donnerait un ratio de 1,00 : on ne ferait ni ",
  "mieux ni pire qu'en prédisant la moyenne. Un ratio de 1,26 signifie donc que le ",
  "modèle <strong>ajoute du bruit</strong> — il fait activement du mal.</p>",
  "<p>Le calcul le confirme sur l'administration publique. Sa corrélation ",
  "prévu-réalisé vaut &minus;0,08, et sa prévision a un écart-type de 0,48 % face à ",
  "un réalisé de 0,68 %. Un modèle de corrélation nulle qui ajoute cette variance ",
  "donne mécaniquement :</p>",
  eq(paste0("ratio <span class='op'>&asymp;</span> <span class='op'>&radic;</span>",
            "<span class='op'>(</span> 1 <span class='op'>+</span> ",
            "<span class='fr'><span class='hi'>0,48<sup>2</sup></span>",
            "<span class='lo'>0,68<sup>2</sup></span></span>",
            " <span class='op'>)</span> = 1,22")),
  "<p>soit le 1,262 observé. Ce n'est donc pas un problème de modèle mais de ",
  "<em>donnée</em>.</p>")))

ajouter("<h3>5.2 Le diagnostic</h3>")
ajouter("<p>Écart-type de la croissance trimestrielle de l'administration publique, ",
        "par période :</p>")
t5b <- data.frame(
  Période = c("1998-2001", "2002-2005", "2006-2009", "2010-2013", "2014-2026"),
  `Écart-type` = c("12,9 %", "0,6 %", "1,9 %", "1,3 %", "0,7 %"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(t5b, aligne_droite = 2))
ajouter(legende_tableau("Une rupture d'un facteur 20, confinée aux quatre premières années."))
ajouter("<p>Les niveaux le confirment : T4-1998 = 13 457, T1-1999 = <strong>9 696</strong>, ",
        "T2-1999 = 12 137. Une branche de salaires publics ne perd pas 28 % en un ",
        "trimestre pour les regagner aussitôt. Et l'éducation-santé, branche publique ",
        "comparable, reste à 0,7-1,4 % sur toute la période : le problème n'est pas une ",
        "propriété des branches publiques, c'est un <strong>artefact de rétropolation</strong> ",
        "propre à cette série.</p>")
ajouter("<p>La règle formalise ce diagnostic :</p>")
ajouter(eq(paste0("rapport<sub>j</sub> = ",
                  "<span class='fr'><span class='hi'>var( ", m("g<sub>j</sub>"),
                  " , premier tiers )</span><span class='lo'>var( ",
                  m("g<sub>j</sub>"), " , reste )</span></span>",
                  " <span class='op'>&gt;</span> 10")))
t5c <- instab %>% dplyr::arrange(dplyr::desc(rapport_variance)) %>%
  utils::head(6) %>%
  dplyr::transmute(Branche = branche,
                   `Rapport de variance` = nb(rapport_variance, 1),
                   `Sortie du BVAR` = ifelse(instable, "oui", ""))
ajouter(tbl(as.data.frame(t5c), aligne_droite = 2))
ajouter(legende_tableau("Diagnostic d'instabilité, échantillon complet."))
ajouter("<p>La séparation est franche — <strong>49,3 contre 3,1</strong> pour la ",
        "suivante — et la règle se déclenche sur <strong>une seule branche aux 48 ",
        "origines</strong>. Un déclenchement erratique aurait signalé du bruit plutôt ",
        "qu'un diagnostic. Le seuil de 10 est un ordre de grandeur : tout seuil entre ",
        "5 et 40 donne le même résultat.</p>")
ajouter(figure("03e_diagnostic.png", "Diagnostic d'instabilité par branche",
               paste0("Échelle logarithmique. L'administration publique se détache de ",
                      "près d'un ordre de grandeur ; le trait marque le seuil.")))

ajouter("<h3>5.3 Trois corrections rejetées avant celle-ci</h3>")
t5d <- data.frame(
  Correction = c("Indicatrices idiosyncratiques", "Winsorisation de toutes les branches",
                 "Winsorisation ciblée sur la seule branche",
                 "Sortie du BVAR"),
  `Branche` = c("1,064", "1,048", "1,054", "1,006"),
  `Médiane 16 branches` = c("1,089", "1,000", "1,001", "0,978"),
  `Agrégat` = c("—", "—", "0,976", "0,961"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(t5d, aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Sans correction : branche 1,262, médiane 0,978, agrégat 0,967.")))
ajouter(definition("Pourquoi les trois premières échouent",
  paste0("<p>Toutes réparent la branche et abîment l'ensemble — y compris la ",
         "winsorisation ciblée, qui ne touche pourtant qu'une seule colonne.</p>",
         "<p>La raison tient à ce que le BVAR est <strong>multivarié</strong> : la ",
         "colonne de l'administration publique est un régresseur dans les quinze ",
         "autres équations. La modifier les déplace toutes. Autrement dit, les ",
         "observations de 1998-2001 sont <em>fausses pour cette branche</em> mais ",
         "portent de l'information exploitable <em>pour les autres</em>.</p>",
         "<p>La correction retenue ne touche donc pas la donnée commune. Elle se ",
         "contente de ne pas <strong>utiliser</strong> la prévision du BVAR là où ",
         "elle nuit — et les quinze autres branches restent identiques au millième ",
         "près.</p>")))
ajouter("<p>Le résultat de fond est un peu ironique : <strong>le meilleur modèle pour ",
        "cette branche est de ne rien prévoir</strong>. Sa croissance post-2002 est un ",
        "bruit de 0,68 % d'écart-type sans structure exploitable, et toute tentative de ",
        "la modéliser ajoute de la variance sans ajouter de signal.</p>")
ajouter(figure("03e_series_instables.png", "La série déclarée instable",
               paste0("La rupture de variance se voit à l'œil nu : les quatre ",
                      "premières années n'ont aucune commune mesure avec la suite.")))

# ---------------------------------------------------------------- 6
ajouter("<h2 id='s6'><span class='num'>6.</span>La valeur de l'information au fil du trimestre</h2>")
ajouter("<p>C'est le résultat le plus instructif du projet, et il a failli être ",
        "manqué : jusqu'à la phase 13, la passerelle était systématiquement jugée sur ",
        "un <strong>trimestre complet</strong> — l'instant précis où son avantage est ",
        "nul, puisque le BVAR dispose alors de la même information.</p>")
t6 <- intra %>% dplyr::filter(periode == "toutes origines") %>%
  dplyr::select(scenario, modele, ratio_median) %>%
  tidyr::pivot_wider(names_from = modele, values_from = ratio_median) %>%
  dplyr::arrange(scenario) %>%
  dplyr::transmute(Scénario = scenario,
                   `Passerelle` = nb(bridge, 3),
                   `Combinaison` = nb(combinee, 3),
                   `BVAR seul` = nb(bvar, 3))
ajouter(tbl(as.data.frame(t6), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Ratio médian par scénario, périmètre commun. M0 : aucun mois du trimestre cible ",
  "observé ; M3 : les trois.")))
ajouter("<p>Le BVAR est <strong>plat par construction</strong> : il ne lit que le passé ",
        "de la valeur ajoutée, qui ne change pas d'un mois à l'autre. L'écart entre M0 ",
        "et M3 mesure donc exactement ce que les indicateurs mensuels apportent.</p>")
ajouter(figure("09_valeur_information.png", "La valeur de l'information",
               paste0("Le BVAR est une horizontale ; les deux autres courbes descendent ",
                      "à mesure que les mois sont observés et croisent cette ",
                      "horizontale. La combinaison passe devant dès M1.")))
ajouter("<h3>Un avantage concentré sur les ruptures</h3>")
t6b <- ep %>% dplyr::filter(scenario == "M3") %>%
  dplyr::mutate(modele = dplyr::recode(modele, bvar = "BVAR",
                                       bridge = "Passerelle", combinee = "Combinaison")) %>%
  dplyr::select(episode, modele, mae) %>%
  tidyr::pivot_wider(names_from = modele, values_from = mae) %>%
  dplyr::transmute(Épisode = episode, BVAR = nb(BVAR, 4),
                   Passerelle = nb(Passerelle, 4), Combinaison = nb(Combinaison, 4))
ajouter(tbl(as.data.frame(t6b), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Erreur absolue moyenne en M3, backtest étendu à 2008 sur quatre branches.")))
ajouter("<p>Sur la crise de 2008-2009 comme sur 2020, la combinaison bat le BVAR ; en ",
        "période calme, le BVAR reste devant. <strong>L'apport des indicateurs est ",
        "conditionnel aux ruptures</strong>, et cela se vérifie maintenant sur deux ",
        "épisodes indépendants.</p>")

# ---------------------------------------------------------------- 7
ajouter("<h2 id='s7'><span class='num'>7.</span>Ce que les corrections ont changé</h2>")
corr <- data.frame(
  `Ce que faisait la version 1` = c(
    "un BVAR estimé une fois sur tout l'échantillon, relu pour « prévoir » le passé",
    "hyperparamètres p = 5 et λ = 0,15 repris de Higgins (2014)",
    "une indicatrice COVID unique pour T2 et T3 2020, deux trimestres de signes opposés",
    "<code>mean(dlog)</code> appliqué à tous les indicateurs",
    "une sélection d'indicateurs figée, calculée sur tout l'échantillon",
    "un poids δ unique, optimisé sur toute la période — donc avec du look-ahead",
    "des poids sectoriels fixes issus du dernier trimestre de la base",
    "un BVAR appliqué uniformément, y compris là où il dégrade la prévision"),
  `Ce que fait la version corrigée` = c(
    "tout est réestimé à chaque origine, et cinq contrôles bloquants le vérifient",
    "choisis à chaque origine par critère d'ajustement : λ médian 0,046, p médian 2 ou 3",
    "une règle de détection testable, qui identifie aussi 2008, 2019 et T4-2020",
    "somme, moyenne ou dernier mois selon la nature économique, refus des trimestres incomplets",
    "recalculée à chaque origine : 9 listes distinctes par branche en médiane",
    "δ = 0,5 constant, après comparaison de cinq règles sur le protocole complet",
    "poids en prix courants du trimestre précédent, 48 jeux distincts",
    "une branche à rupture de variance est sortie du BVAR et reçoit sa moyenne récente"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(corr, aligne_droite = integer(0)))
ajouter(legende_tableau("Les corrections structurantes du plan."))
ajouter("<div class='encadre alerte'><span class='etiq'>Une correction de données, hors plan</span>",
        "<p>La phase 1 a révélé que <strong>174 séries mensuelles sur 316</strong> ne ",
        "sont pas des flux mensuels mais des <strong>cumuls depuis janvier</strong> : ",
        "elles baissent en janvier dans 100 % des cas contre 1 % les autres mois.</p>",
        "<p>Les agréger ou les différencier en l'état fabriquait un effondrement ",
        "artificiel à chaque premier trimestre, sur la majorité de la base ",
        "d'indicateurs. Ce défaut n'était pas dans le plan de correction, et rien ne ",
        "le signalait : les séries gardaient une allure parfaitement plausible.</p></div>")

# ---------------------------------------------------------------- 8
ajouter("<h2 id='s8'><span class='num'>8.</span>Le nowcast du trimestre courant</h2>")
ajouter(sprintf(paste0("<p>Cible : <strong>%s</strong>. La dernière valeur ajoutée ",
                       "publiée est celle de %s, et <strong>%d des trois mois</strong> ",
                       "du trimestre cible sont observés — soit le scénario ",
                       "<strong>%s</strong>. Le scénario n'est pas choisi : il est ",
                       "constaté.</p>"),
                date_vers_trimestre(CIBLE), date_vers_trimestre(derniere_va),
                mois_vus, SCENARIO))
ajouter(sprintf(paste0("<p style='text-align:center;font-size:1.5em;margin:1.2em 0'>",
                       "<strong>Valeur ajoutée totale, %s : %+.2f %%</strong></p>"),
                date_vers_trimestre(CIBLE), agregat_courant))
t8 <- nowcast_branches %>% dplyr::arrange(dplyr::desc(nowcast_pct)) %>%
  dplyr::transmute(Branche = branche,
                   `Poids (%)` = nb(100 * w, 1),
                   `BVAR (%)` = nb(bvar_pct, 2),
                   `Passerelle (%)` = ifelse(is.na(bridge_pct), "—",
                                             nb(bridge_pct, 2)),
                   `δ` = nb(delta, 2),
                   `Nowcast (%)` = nb(nowcast_pct, 2),
                   `σ branche (%)` = nb(ecart_type_pct, 1),
                   `z` = ifelse(is.na(z_amplitude), "—", nb(z_amplitude, 2)),
                   Source = source)
ajouter(tbl(as.data.frame(t8), aligne_droite = 2:6))
ajouter(legende_tableau(paste0("Nowcast par branche pour ", date_vers_trimestre(CIBLE),
                               ", scénario ", SCENARIO, ".")))
ajouter(definition("Le garde-fou sur l'amplitude",
  paste0("<p>La colonne ", m("z"), " rapporte la prévision de passerelle à ",
         "l'écart-type de la branche, estimé sur la seule information antérieure. ",
         "C'est la bonne échelle de lecture : la pêche affiche un pourcentage ",
         "spectaculaire mais un ", m("z"), " modeste, parce que sa croissance ",
         "trimestrielle a un écart-type de 17,5 %.</p>",
         "<p>Le seuil d'alerte est fixé à ", m("z"), " = 5, et le mode par défaut ",
         "est le <strong>signalement</strong>, non le plafonnement. La mesure le ",
         "justifie : sur le backtest, plafonner améliore la passerelle seule ",
         "(0,950 → 0,924) mais <strong>ne change rien à la combinaison</strong> ",
         "(0,977 quel que soit le seuil), et surtout cela abîmerait la prévision ",
         "de &minus;81,7 % contre &minus;85,7 % réalisé au T2-2020 — la plus juste ",
         "du projet. Le garde-fou protège donc contre une valeur aberrante publiée ",
         "sans contrôle possible ; il n'améliore pas la précision.</p>",
         sprintf("<p>À cette origine, <strong>%d branche(s)</strong> franchissent le seuil.</p>",
                 sum(nowcast_branches$signale, na.rm = TRUE)))))

ajouter("<h3>8.1 L'intervalle autour de ce chiffre</h3>")
if (A_IC) {
  t8b <- ic_now %>% dplyr::arrange(niveau) %>%
    dplyr::left_join(ic_couv %>% dplyr::filter(echelle == ic_now$echelle[1]) %>%
                       dplyr::select(niveau, couverture), by = "niveau") %>%
    dplyr::transmute(`Niveau annoncé` = sprintf("%.0f %%", 100 * niveau),
                     `Intervalle` = sprintf("[%+.2f ; %+.2f]", bas_pct, haut_pct),
                     `Couverture observée` = sprintf("%.0f %%", 100 * couverture))
  ajouter(tbl(as.data.frame(t8b), aligne_droite = 1:3))
  ajouter(legende_tableau(paste0(
    "Intervalles conformes, largeur fixée par les erreurs passées du modèle, ",
    "calculées récursivement.")))
  ajouter(definition("Pourquoi l'intervalle n'est pas propagé analytiquement",
    paste0("<p>La tentation serait d'écrire, branche par branche :</p>",
           eq(paste0("V<sub>comb</sub> = ", m("&delta;<sup>2</sup>"), " V<sub>BVAR</sub>",
                     " <span class='op'>+</span> (1<span class='op'>&minus;</span>",
                     m("&delta;"), ")<sup>2</sup> V<sub>pass</sub>",
                     " <span class='op'>+</span> 2", m("&delta;"),
                     "(1<span class='op'>&minus;</span>", m("&delta;"), ") ",
                     m("&rho;"), " &hellip;")),
           "<p>puis d'agréger. La corrélation ", m("&rho;"),
           " entre les deux composantes serait estimable sur l'historique. Mais ",
           "l'agrégation exige la <strong>covariance croisée des branches</strong> ",
           "pour la passerelle — et les seize passerelles sont des régressions ",
           "<em>indépendantes</em>, estimées séparément sur des indicateurs ",
           "différents. Cette covariance n'est pas identifiée par le modèle.</p>",
           "<p>La supposer nulle sous-estimerait gravement l'intervalle : les ",
           "branches se trompent <em>ensemble</em> lors des ruptures, c'est ce que ",
           "2020 a montré. L'intervalle est donc construit <strong>directement au ",
           "niveau de l'agrégat</strong>, où cette covariance n'a plus à être ",
           "modélisée — elle est déjà contenue dans l'erreur agrégée observée.</p>")))
  ajouter("<div class='encadre alerte'><span class='etiq'>Annoncer la couverture observée, pas le niveau nominal</span>",
          sprintf(paste0("<p>La couverture reste <strong>inférieure au nominal de ",
                         "5 à 9 points</strong> : un intervalle annoncé à 80 %% ne ",
                         "contient le réalisé que %.0f %% du temps.</p>"),
                  100 * ic_couv$couverture[ic_couv$niveau == 0.80 &
                                             ic_couv$echelle == ic_now$echelle[1]]),
          "<p>Le déficit est <strong>le même avant et après 2020</strong> — 74 % ",
          "contre 76 % — donc il n'est pas dû au choc : c'est le coût de ",
          "l'incertitude sur les <em>hyperparamètres</em>, qui sont choisis à chaque ",
          "origine puis traités comme connus.</p>",
          "<p>La formulation honnête est donc « intervalle dont la couverture ",
          "mesurée est de 75 % », et non « intervalle à 80 % ».</p></div>")
} else {
  ajouter("<p>Intervalle non calculé : exécuter <code>R/12_incertitude_parametrique.R</code> ",
          "puis <code>R/12c_intervalle_combinaison.R</code>.</p>")
}

ajouter(definition("Comment lire ce chiffre",
  paste0("<p>Il est produit par la <strong>même fonction</strong> que celle validée à ",
         "l'étape 7 contre le pipeline complet : le chiffre publié suit exactement le ",
         "chemin qui a été évalué, sans raccourci.</p>",
         "<p>C'est un taux <strong>trimestriel</strong>, du T1-2026 au T2-2026, et il ",
         "porte sur la <strong>valeur ajoutée totale</strong>, non sur le PIB. Sur les ",
         "48 trimestres du backtest, la croissance trimestrielle agrégée a une médiane ",
         "de +0,80 % et un écart-type de 2,26 points : ce nowcast se situe donc au ",
         "90<sup>e</sup> percentile — un trimestre fort, pas un chiffre neutre.</p>")))

# ---------------------------------------------------------------- 9
ajouter("<h2 id='s9'><span class='num'>9.</span>Les étalons : ce que le système bat, et de combien</h2>")
if (!A_BENCH) {
  ajouter("<p>Section non produite : exécuter <code>R/13_benchmarks.R</code> à ",
          "<code>R/16_direct_indirect.R</code>.</p>")
} else {
ajouter("<p>La section 4 comparait le système au BVAR seul. La question posée ici est ",
        "plus exigeante : <strong>combien de ce résultat un modèle trivial ",
        "obtiendrait-il sans aucun indicateur ?</strong> Six étalons sont donc réestimés ",
        "sur exactement le même protocole récursif, aux mêmes 48 origines.</p>")
t9a <- bench %>% dplyr::filter(periode == "toutes origines") %>%
  dplyr::arrange(ratio) %>%
  dplyr::transmute(Modèle = modele,
                   `RMSFE (%)` = nb(100 * RMSFE, 2),
                   `Ratio` = nb(ratio, 3),
                   `Corrélation` = nb(correlation, 2),
                   `Biais (pt)` = nb(100 * biais, 2))
ajouter(tbl(as.data.frame(t9a), aligne_droite = 2:5))
ajouter(legende_tableau(paste0(
  "Agrégat, 48 trimestres, T2-2014 à T1-2026. Tous les étalons sont réestimés ",
  "récursivement : à chaque origine, l'AR n'utilise que les trimestres antérieurs.")))
ajouter(intuition(paste0(
  "<p>L'ordre est net et il tient sur toute la colonne : <strong>système, puis BVAR, ",
  "puis moyenne historique, puis les AR, puis le naïf</strong>. Aucun étalon ne passe ",
  "sous 1 — autrement dit, <em>aucun modèle univarié ne bat la simple moyenne de long ",
  "terme</em> sur la croissance trimestrielle marocaine.</p>",
  "<p>C'est la colonne <strong>corrélation</strong> qui est la plus parlante. Elle est ",
  "<em>négative</em> pour les trois AR : ces modèles ne se contentent pas d'être ",
  "imprécis, ils vont systématiquement dans le mauvais sens. La croissance trimestrielle ",
  "est faiblement — et négativement — autocorrélée, si bien qu'extrapoler le passé ",
  "revient à parier contre la réalisation. Seuls le système (",
  nb(bench$correlation[bench$modele == "Systeme complet" &
                       bench$periode == "toutes origines"], 2),
  ") et le BVAR obtiennent une corrélation franchement positive.</p>")))

ajouter("<h3>9.1 Et sans 2020 ?</h3>")
t9b <- bench %>% dplyr::filter(periode != "toutes origines") %>%
  dplyr::select(modele, periode, ratio) %>%
  tidyr::pivot_wider(names_from = periode, values_from = ratio) %>%
  dplyr::arrange(`hors 2020`) %>%
  dplyr::transmute(Modèle = modele,
                   `Ratio 2020 (4 trim.)` = nb(`2020`, 3),
                   `Ratio hors 2020 (44 trim.)` = nb(`hors 2020`, 3))
ajouter(tbl(as.data.frame(t9b), aligne_droite = 2:3))
ajouter(legende_tableau("Le même classement, scindé sur la seule année de rupture."))
ajouter(figure("13_rmsfe_par_modele.png", "Les sept modèles, par RMSFE",
               paste0("Le système est à gauche, le naïf à droite. L'écart au BVAR est ",
                      "visible mais mince au regard de la distance qui sépare ces deux-là ",
                      "des modèles univariés.")))
ajouter(figure("13_systeme_contre_ar2.png", "Système et AR(2), trimestre par trimestre",
               paste0("Les deux courbes se confondent en régime calme. Elles ne se ",
                      "séparent que sur les ruptures — c'est la lecture graphique du ",
                      "tableau des épisodes de la section 11.3.")))
ajouter("<div class='encadre alerte'><span class='etiq'>Le classement global est porté par quatre trimestres</span>",
        sprintf(paste0("<p>Hors 2020, le ratio du système remonte à <strong>%s</strong> ",
                       "et sa corrélation tombe à <strong>%s</strong>. L'écart au BVAR ",
                       "devient ténu — %s contre %s — et la moyenne historique n'est ",
                       "plus qu'à %s.</p>"),
                nb(bench$ratio[bench$modele == "Systeme complet" & bench$periode == "hors 2020"], 3),
                nb(bench$correlation[bench$modele == "Systeme complet" & bench$periode == "hors 2020"], 2),
                nb(bench$ratio[bench$modele == "Systeme complet" & bench$periode == "hors 2020"], 3),
                nb(bench$ratio[bench$modele == "BVAR agrege" & bench$periode == "hors 2020"], 3),
                nb(bench$ratio[bench$modele == "Moyenne historique" & bench$periode == "hors 2020"], 3)),
        "<p>Il faut le dire sans détour : <strong>l'essentiel de la performance ",
        "affichée provient de la capacité à voir 2020 arriver</strong>. C'est une ",
        "qualité réelle et c'est précisément ce qu'on demande à un nowcast — un modèle ",
        "utile est un modèle qui sert quand la conjoncture décroche. Mais le chiffre de ",
        "0,86 ne doit pas être présenté comme une performance de régime courant.</p>",
        "<p>Le classement, lui, ne s'inverse jamais : le système reste premier dans les ",
        "deux sous-périodes.</p></div>")

ajouter("<h3>9.2 Ces écarts sont-ils significatifs ?</h3>")
t9c <- dm13 %>% dplyr::arrange(p_value) %>%
  dplyr::transmute(`Étalon` = etalon,
                   `RMSFE étalon (%)` = nb(100 * RMSFE_etalon, 2),
                   `Écart (%)` = nb(100 * (RMSFE_etalon - RMSFE_systeme), 2),
                   `p` = nb(p_value, 3),
                   `Verdict` = verdict)
ajouter(tbl(as.data.frame(t9c), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Diebold-Mariano bilatéral, correction de Harvey-Leybourne-Newbold pour petit ",
  "échantillon, contre le système complet (RMSFE ",
  nb(100 * dm13$RMSFE_systeme[1], 2), " %).")))
ajouter(definition("Pourquoi des écarts de 40 % ne sont pas significatifs",
  paste0("<p>Le test compare la perte différentielle :</p>",
         eq("d<sub>t</sub> = e<sub>syst,t</sub><sup>2</sup> <span class='op'>&minus;</span> e<sub>etalon,t</sub><sup>2</sup>"),
         "<p>et sa statistique est ", m("DM"), " = ", m("d&#772;"), " / ",
         m("&radic;(V&#770;(d&#772;))"), ". Or ", m("d<sub>t</sub>"),
         " est dominé par quatre trimestres de 2020, où les erreurs des deux modèles ",
         "sont grandes et leur différence l'est aussi. La <em>variance</em> de ",
         m("d<sub>t</sub>"), " explose donc avec sa moyenne, et le rapport reste ",
         "petit.</p>",
         "<p>Avec ", m("n"), " = 48, le test ne peut trancher qu'un écart régulier. Un ",
         "écart concentré sur une crise — exactement notre cas — reste hors de portée. ",
         "<strong>Cela ne dit pas que le système ne vaut pas mieux ; cela dit que ",
         "48 trimestres ne suffisent pas à le prouver.</strong> C'est une limite de ",
         "l'échantillon, pas un verdict sur le modèle.</p>")))
}

# ---------------------------------------------------------------- 10
ajouter("<h2 id='s10'><span class='num'>10.</span>Robustesse : ce qui ne change pas les conclusions</h2>")
if (A_BENCH) {
ajouter("<p>Un résultat qui dépend d'un réglage n'est pas un résultat. Trois choix ont ",
        "donc été rejoués de bout en bout, chacun sur le protocole complet.</p>")

ajouter("<h3>10.1 Le poids de combinaison, et d'où vient 0,5</h3>")
ajouter("<p>Le poids ", m("&delta;"), " est <strong>fixé à 0,5</strong>, sans ",
        "estimation. Trois mesures fondent ce choix, et il faut les prendre dans ",
        "l'ordre.</p>")

ajouter("<h4>La forme de la courbe de perte</h4>")
if (!is.null(courbe_d)) {
  t10c <- courbe_d %>%
    dplyr::filter(abs(delta * 10 - round(delta * 10)) < 1e-9) %>%
    dplyr::select(delta, periode, ratio) %>%
    tidyr::pivot_wider(names_from = periode, values_from = ratio) %>%
    dplyr::arrange(delta) %>%
    dplyr::transmute(`δ` = nb(delta, 2),
                     `Ensemble` = nb(`toutes origines`, 3),
                     `2020` = nb(`2020`, 3),
                     `Hors 2020` = nb(`hors 2020`, 3))
  ajouter(tbl(as.data.frame(t10c), aligne_droite = 1:4))
  ajouter(legende_tableau(paste0(
    "Ratio de l'agrégat pour chaque valeur du poids. δ = 0 donne la passerelle ",
    "seule, δ = 1 le BVAR seul. Balayage au pas de 0,05, une ligne sur deux ",
    "affichée.")))
  ajouter(figure("14b_courbe_delta.png", "La courbe de perte selon le poids",
                 paste0("Les points marquent le minimum de chaque période, la ",
                        "verticale le réglage retenu. Hors 2020 la courbe a un creux ",
                        "intérieur ; en 2020 elle décroît jusqu'au bout.")))
  plat <- courbe_d %>% dplyr::filter(periode == "hors 2020") %>%
    dplyr::mutate(ec = ratio - min(ratio)) %>% dplyr::filter(ec <= 0.01)
  opt_h <- courbe_d %>% dplyr::filter(periode == "hors 2020") %>%
    dplyr::slice_min(ratio, n = 1, with_ties = FALSE)
  ajouter(sprintf(paste0("<p>Deux propriétés s'y lisent. <strong>Hors 2020, la courbe ",
                         "a un minimum intérieur</strong>, à δ = %s : c'est ce qui ",
                         "valide la combinaison elle-même, puisqu'en régime normal le ",
                         "mélange bat chacune de ses deux composantes. Et elle est ",
                         "<strong>plate</strong> — tout δ de %s à %s se tient à moins ",
                         "de 0,01 point du minimum.</p>"),
                  nb(opt_h$delta, 2), nb(min(plat$delta), 2), nb(max(plat$delta), 2)))
  ajouter(intuition(paste0(
    "<p>C'est la propriété classique d'une perte quadratique : <strong>s'écarter de ",
    "l'optimum coûte au second ordre, alors que l'estimer coûte au premier</strong>. ",
    "Quand la courbe est plate, l'erreur d'estimation du poids pèse plus lourd que ",
    "l'écart au poids idéal.</p>",
    "<p>Et surtout, <strong>la position de l'optimum dépend du régime</strong> : 0 en ",
    "2020, ", nb(opt_h$delta, 1), " hors 2020. Or au moment de publier, on ignore dans ",
    "quel régime on se trouve. C'est exactement la configuration où l'on fixe un ",
    "paramètre au lieu de l'estimer.</p>")))
}

ajouter("<h4>Ce que fait réellement l'estimateur</h4>")
if (!is.null(stab_d)) {
  vs <- function(m) stab_d$valeur[stab_d$mesure == m]
  ajouter(sprintf(paste0("<p>Le ", m("&delta;"), " estimé par moindres carrés ",
                         "n'est <em>pas</em> instable, contrairement à ce qu'on ",
                         "pourrait supposer : son saut médian d'un trimestre au suivant ",
                         "est <strong>nul</strong>, son écart-type intra-branche vaut ",
                         "%s, et %.0f %% seulement des sauts dépassent 0,25. Il retombe ",
                         "en revanche sur sa valeur par défaut dans <strong>%.0f %% des ",
                         "cas</strong>, faute de huit trimestres appariés.</p>"),
                  nb(vs("ecart-type intra-branche moyen"), 2),
                  100 * vs("part des sauts superieurs a 0,25"),
                  100 * vs("part des couples au mode par defaut")))
}
if (!is.null(comp_d)) {
  t10d <- comp_d %>%
    dplyr::transmute(`Règle` = regle, `Ratio` = nb(ratio, 3),
                     `Corrélation` = nb(correlation, 3))
  ajouter(tbl(as.data.frame(t10d), aligne_droite = 2:3))
  ajouter(legende_tableau(paste0(
    "La comparaison décisive : le δ estimé opposé à une constante placée à son ",
    "PROPRE niveau moyen, ce qui isole l'apport de sa variation indépendamment de ",
    "son niveau.")))
  ecart_d <- comp_d$ratio[1] - comp_d$ratio[2]
  ajouter(sprintf(paste0("<p>À niveau moyen égal, <strong>la constante fait mieux de ",
                         "%s point de ratio</strong> et de %s de corrélation. La ",
                         "variation que l'estimateur introduit d'une branche et d'un ",
                         "trimestre à l'autre <strong>n'apporte donc aucune ",
                         "information</strong> : elle n'ajoute que du bruit.</p>"),
                  nb(ecart_d, 3), nb(comp_d$correlation[2] - comp_d$correlation[1], 2)))
  ajouter(definition("Le « forecast combination puzzle »",
    paste0("<p>Ce résultat n'a rien de propre au Maroc : c'est l'un des constats les ",
           "mieux établis de la littérature sur la combinaison de prévisions. ",
           "Bates &amp; Granger (1969) posent le principe et donnent le poids ",
           "optimal ; Stock &amp; Watson (2004) constatent sur des centaines de séries ",
           "macroéconomiques que <em>la moyenne simple bat les poids estimés</em> ; ",
           "Smith &amp; Wallis (2009) puis Claeskens <em>et al.</em> (2016) en donnent ",
           "l'explication — estimer le poids ajoute une variance d'échantillonnage qui ",
           "excède le biais qu'on corrige.</p>",
           "<p>Le poids optimal théorique vaut</p>",
           eq(paste0(m("&delta;*"), " = ",
                     "<span class='fr'><span class='hi'>",
                     m("&sigma;<sub>2</sub><sup>2</sup>"),
                     " <span class='op'>&minus;</span> ",
                     m("&sigma;<sub>12</sub>"), "</span><span class='lo'>",
                     m("&sigma;<sub>1</sub><sup>2</sup>"), " <span class='op'>+</span> ",
                     m("&sigma;<sub>2</sub><sup>2</sup>"),
                     " <span class='op'>&minus;</span> 2",
                     m("&sigma;<sub>12</sub>"), "</span></span>")),
           "<p>et <strong>les poids égaux sont exactement optimaux quand les deux ",
           "composantes ont la même variance d'erreur</strong>. 0,5 est donc le cas de ",
           "référence : la valeur qu'on adopte quand on refuse de prétendre savoir ",
           "laquelle des deux prévisions est la meilleure — et la seule qui se ",
           "justifie <em>a priori</em>, sans regarder les données.</p>")))
}

if (!is.null(courbe_d)) {
  c05 <- courbe_d %>% dplyr::filter(periode == "hors 2020", delta == 0.5)
  opt_h <- courbe_d %>% dplyr::filter(periode == "hors 2020") %>%
    dplyr::slice_min(ratio, n = 1, with_ties = FALSE)
  ajouter("<div class='encadre alerte'><span class='etiq'>Une réserve à ne pas dissimuler</span>",
          paste0("<p>Sur cet échantillon, <strong>δ ≈ 0,2 à 0,3 domine 0,5 dans ",
                 "les trois périodes</strong>. Retenir 0,3 parce que la courbe le dit ",
                 "reviendrait toutefois à ajuster un paramètre sur 48 trimestres dont ",
                 "quatre décident de presque tout — et la position de l'optimum change ",
                 "selon qu'on inclut 2020 ou non.</p>"),
          sprintf(paste0("<p>Le coût de ce refus est mesuré : <strong>%s point de ratio ",
                         "hors 2020</strong>. C'est le prix d'un réglage qui ne doit ",
                         "rien à l'échantillon.</p></div>"),
                  nb(c05$ratio - opt_h$ratio, 3)))
}

ajouter("<h4>Au niveau des branches, le réglage ne décide de rien</h4>")
t10a <- rob_d %>% dplyr::arrange(ratio_median) %>%
  dplyr::transmute(`Règle sur δ` = regle, `Branches` = branches,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Branches sous 1` = n_ok)
ajouter(tbl(as.data.frame(t10a), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Ratio médian PAR BRANCHE selon la règle de pondération. C'est l'échelle à ",
  "laquelle les cinq règles sont indiscernables ; la question se tranche sur ",
  "l'agrégat, ci-dessus.")))
ajouter(figure("14_delta.png", "Le ratio par branche selon la règle de pondération",
               paste0("Toutes les règles se tiennent dans un mouchoir de poche. ",
                      "Graphiquement, c'est l'absence d'écart qui est le résultat.")))
ajouter(sprintf(paste0("<p>L'amplitude totale y est de <strong>%s point de ratio</strong> ",
                       "entre la meilleure règle et la pire — et δ = 0,5, la règle ",
                       "retenue, y est même bonne dernière de ce mouchoir de poche. ",
                       "Rien ne s'y joue.</p>"),
                nb(max(rob_d$ratio_median) - min(rob_d$ratio_median), 3)))

ajouter("<h3>10.2 La fenêtre d'estimation</h3>")
t10b <- rob_f %>%
  dplyr::transmute(`Spécification` = specification,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Corrélation médiane` = nb(correl_mediane, 3),
                   `Branches sous 1` = n_branches_ok)
ajouter(tbl(as.data.frame(t10b), aligne_droite = 2:4))
ajouter(legende_tableau("Fenêtre extensible (retenue) contre fenêtres glissantes."))
ajouter("<p>Ici le verdict est tranché, et dans un seul sens : <strong>les fenêtres ",
        "glissantes détruisent la corrélation</strong> — 0,24 contre 0,02 et 0,05 — ",
        "tout en dégradant le ratio. Une fenêtre de 40 ou 60 trimestres jette ",
        "précisément les épisodes rares dont le modèle a besoin pour reconnaître une ",
        "rupture. La fenêtre extensible est donc un choix, pas une commodité.</p>")

ajouter("<h3>10.3 Le seuil de sélection des indicateurs</h3>")
t10c <- rob_s %>% dplyr::filter(perimetre == "perimetre commun") %>%
  dplyr::arrange(ratio_median) %>%
  dplyr::transmute(`Variante` = variante, `Branches` = branches,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Origines produites` = sprintf("%.0f %%", 100 * taux))
ajouter(tbl(as.data.frame(t10c), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Périmètre commun aux cinq variantes, pour que la comparaison porte sur les mêmes ",
  "trimestres.")))
ajouter(figure("14_selection.png", "Seuil de sélection et nombre d'indicateurs retenus",
               paste0("Les variantes récursives se superposent exactement ; seule la ",
                      "sélection fixe s'en écarte, en retenant moins d'indicateurs et en ",
                      "produisant moins d'origines.")))
ajouter("<p>Les quatre variantes récursives sont <strong>rigoureusement identiques</strong> ",
        "— le seuil ne mord jamais. En revanche la sélection <em>fixe</em>, calculée une ",
        "fois sur tout l'échantillon, est <strong>moins bonne</strong> alors même ",
        "qu'elle bénéficie d'un look-ahead. Le point mérite d'être souligné : ",
        "<strong>tricher ne paie pas ici</strong>, parce qu'un indicateur utile en 2014 ",
        "ne l'est plus en 2022 et qu'une sélection figée le conserve à tort.</p>")

ajouter("<h3>10.4 La saisonnalité résiduelle</h3>")
ajouter(sprintf(paste0("<p>Une régression de la croissance sur les indicatrices de ",
                       "trimestre, branche par branche, donne un ", m("R<sup>2</sup>"),
                       " médian de <strong>%s</strong> (maximum %s, sur la pêche). ",
                       "<strong>Les séries sont bien désaisonnalisées</strong> : il n'y a ",
                       "pas de gain caché à récupérer de ce côté.</p>"),
                nb(median(sais$R2_saison), 3), nb(max(sais$R2_saison), 3)))
}

# ---------------------------------------------------------------- 11
ajouter("<h2 id='s11'><span class='num'>11.</span>D'où vient l'erreur</h2>")
if (A_BENCH) {
ajouter("<h3>11.1 Quelles branches pèsent dans l'erreur agrégée</h3>")
t11a <- contrib %>% dplyr::arrange(dplyr::desc(contribution_abs)) %>%
  head(8) %>%
  dplyr::transmute(Branche = branche,
                   `Poids moyen (%)` = nb(100 * poids_moyen, 1),
                   `Erreur propre (pt)` = nb(100 * erreur_branche, 2),
                   `Contribution (pt)` = nb(100 * contribution_abs, 3))
ajouter(tbl(as.data.frame(t11a), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Contribution = poids &times; erreur absolue moyenne. Huit premières branches sur ",
  nrow(contrib), ".")))
ajouter(figure("15_contributions.png", "Contribution de chaque branche à l'erreur agrégée",
               paste0("La barre combine poids et erreur propre. Les branches lourdes ",
                      "dominent, même quand leur erreur relative est modeste.")))
ajouter(intuition(paste0(
  "<p>La contribution est un <strong>produit</strong>, et les deux facteurs jouent en ",
  "sens inverse. L'hébergement-restauration se trompe de 10,9 points en moyenne — de ",
  "loin le pire — mais ne pèse que 3,9 %, si bien qu'il arrive quatrième. L'industrie de ",
  "transformation se trompe quatre fois moins, mais pèse 16,8 % et arrive première.</p>",
  "<p>Conséquence pratique : <strong>l'effort d'amélioration doit viser l'industrie de ",
  "transformation, l'agriculture et le commerce</strong> — 40 % du poids à eux trois — ",
  "et non les branches dont le ratio est le plus laid.</p>")))

ajouter("<h3>11.2 Les résidus de l'agrégat</h3>")
t11b <- resid %>%
  dplyr::transmute(Test = test,
                   `Statistique` = ifelse(is.na(statistique), "—", nb(statistique, 3)),
                   `p` = ifelse(is.na(p_value), "—", nb(p_value, 4)),
                   Lecture = lecture)
ajouter(tbl(as.data.frame(t11b), aligne_droite = 2:3))
ajouter(legende_tableau("Diagnostics sur l'erreur de nowcast agrégée, 48 trimestres."))
ajouter("<p>Les deux premiers tests sont <strong>de bonnes nouvelles</strong> : pas ",
        "d'autocorrélation résiduelle — le modèle n'a pas laissé de structure ",
        "exploitable dans ses erreurs — ni d'hétéroscédasticité conditionnelle.</p>")
ajouter("<div class='encadre'><span class='etiq'>La non-normalité, elle, est structurante</span>",
        "<p>Shapiro-Wilk rejette massivement la normalité. Les résidus ont des ",
        "<strong>queues épaisses</strong> : beaucoup de trimestres presque parfaits, ",
        "quelques-uns très mauvais.</p>",
        "<p>C'est exactement la raison pour laquelle les intervalles de la section 8 sont ",
        "<strong>conformes</strong> et non gaussiens. Un intervalle ",
        m("&mu;"), " &plusmn; 1,96", m("&sigma;"), " serait mal calibré par construction ",
        "sur une telle distribution : trop large en régime calme, trop étroit lors des ",
        "ruptures — c'est-à-dire faux là où il compte.</p></div>")
ajouter(figure("15_erreurs.png", "L'erreur de nowcast, trimestre par trimestre",
               paste0("Beaucoup de trimestres proches de zéro, quelques-uns très ",
                      "au-dessus : c'est la signature des queues épaisses que ",
                      "Shapiro-Wilk détecte.")))

ajouter("<h3>11.3 L'erreur par épisode</h3>")
t11c <- epis %>%
  dplyr::transmute(`Épisode` = episode, `n` = n,
                   `Croissance moy. (%)` = nb(100 * croissance_moyenne, 2),
                   `RMSFE nowcast (%)` = nb(100 * RMSFE_nowcast, 2),
                   `RMSFE AR(2) (%)` = nb(100 * RMSFE_ar2, 2),
                   `Gain (%)` = sprintf("%+.0f", 100 * gain_sur_ar2))
ajouter(tbl(as.data.frame(t11c), aligne_droite = 2:6))
ajouter(legende_tableau("Découpage des 48 trimestres en cinq régimes conjoncturels."))
ajouter(figure("15_episodes.png", "Erreur par régime conjoncturel",
               paste0("Le nowcast domine nettement sur les ruptures. Sur les ",
                      "ralentissements, les trois barres sont de même hauteur.")))
ajouter("<p>Le gain sur l'AR(2) vaut <strong>+27 % sur 2020</strong>, +21 % sur les ",
        "chocs agricoles, +16 % en forte croissance — et <strong>exactement zéro sur les ",
        "ralentissements</strong> (+0,2 %).</p>")
ajouter(intuition(paste0(
  "<p>Cette ligne à zéro est la plus instructive du rapport, parce qu'elle est ",
  "<em>contre-intuitive</em> : on attendrait qu'un nowcast serve d'abord quand la ",
  "conjoncture se retourne.</p>",
  "<p>L'explication tient à la nature des indicateurs. Ils sont <strong>coïncidents</strong>, ",
  "non avancés : ils enregistrent un effondrement pendant qu'il se produit — d'où ",
  "l'excellent 2020 — mais un ralentissement graduel ne laisse pas de signature ",
  "mensuelle assez nette pour se distinguer du bruit. Le modèle voit les chocs, pas ",
  "les inflexions.</p>")))

ajouter("<h3>11.4 Les coefficients sont-ils stables ?</h3>")
t11d <- stab %>% dplyr::arrange(dplyr::desc(amplitude)) %>% head(6) %>%
  dplyr::transmute(Branche = branche, `Moyenne` = nb(moyenne, 3),
                   `Écart-type` = nb(ecart_type, 3),
                   `Amplitude` = nb(amplitude, 3))
ajouter(tbl(as.data.frame(t11d), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Coefficient autorégressif d'ordre 1 du BVAR, réestimé aux 48 origines. Six branches ",
  "les plus instables.")))
ajouter(figure("15_stabilite.png", "Le coefficient autorégressif au fil des origines",
               paste0("Une seule branche change de signe — l'administration publique, ",
                      "avant que la rupture de 1999 ne sorte de l'échantillon.")))
ajouter("<p>L'administration publique change de <strong>signe</strong> au fil des ",
        "origines — de &minus;0,37 à +0,29. C'est la trace chiffrée de la rupture de ",
        "niveau de 1999 traitée à la section 5 : tant qu'elle était dans l'échantillon, ",
        "le coefficient était piloté par un artefact. Les autres branches restent dans ",
        "une plage étroite.</p>")
}

# ---------------------------------------------------------------- 12
ajouter("<h2 id='s12'><span class='num'>12.</span>Direct ou indirect : faut-il passer par les seize branches ?</h2>")
if (A_BENCH) {
ajouter("<p>Tout le système repose sur un choix jamais testé jusqu'ici : prévoir ",
        "<strong>seize branches puis agréger</strong>. L'alternative est d'estimer une ",
        "passerelle directement sur la croissance de l'agrégat, en sautant les ",
        "branches.</p>")
ajouter(eq(paste0("g&#770;<sup>indirect</sup><sub>T</sub> = log &sum;<sub>j</sub> w<sub>j,T&minus;1</sub> e<sup>g&#770;<sub>j,T</sub></sup>",
                  "<span style='margin-left:2em'></span>",
                  "g&#770;<sup>direct</sup><sub>T</sub> = ", m("&alpha;"),
                  " <span class='op'>+</span> ", m("&beta;"), "&prime;x<sub>T</sub>")))
t12 <- comp16 %>%
  dplyr::mutate(voie = dplyr::recode(voie,
    directe = "Directe (passerelle sur l'agrégat)",
    indirecte = "Indirecte (16 branches agrégées) — retenue",
    mixte = "Combinaison des deux voies",
    bvar_indirect = "BVAR seul, agrégé", ar2 = "AR(2)")) %>%
  dplyr::select(voie, periode, ratio) %>%
  tidyr::pivot_wider(names_from = periode, values_from = ratio) %>%
  dplyr::arrange(`toutes origines`) %>%
  dplyr::transmute(`Voie` = voie,
                   `Ratio global` = nb(`toutes origines`, 3),
                   `2020` = nb(`2020`, 3),
                   `Hors 2020` = nb(`hors 2020`, 3))
ajouter(tbl(as.data.frame(t12), aligne_droite = 2:4))
ajouter(legende_tableau("Même protocole récursif, mêmes 48 origines, même cible."))
ajouter(figure("16_voies.png", "Les deux voies, trimestre par trimestre",
               paste0("La voie directe colle remarquablement au réalisé en 2020 puis ",
                      "diverge ensuite : c'est le basculement que le tableau ",
                      "chiffre.")))
ajouter("<div class='encadre alerte'><span class='etiq'>Le classement s'inverse selon la période</span>",
        "<p>Sur l'ensemble, la voie directe paraît nettement meilleure : ",
        "<strong>0,727 contre 0,860</strong>. Mais la décomposition renverse le tableau ",
        "— <strong>0,269 en 2020</strong>, un résultat spectaculaire, et ",
        "<strong>1,352 hors 2020</strong>, c'est-à-dire pire que de ne rien prévoir.</p>",
        "<p>La voie indirecte fait l'inverse : 0,756 en 2020, et <strong>0,975 hors ",
        "2020</strong>, donc utile dans les deux régimes.</p>",
        "<p>Autrement dit, la voie directe n'est pas un meilleur modèle : c'est un ",
        "<em>détecteur de choc</em>. Quatre trimestres sur quarante-huit lui offrent son ",
        "classement global.</p></div>")
t12b <- dm16 %>%
  dplyr::transmute(`Référence` = reference, `Alternative` = alternative,
                   `p` = nb(p_value, 3), `Verdict` = verdict)
ajouter(tbl(as.data.frame(t12b), aligne_droite = 3))
ajouter(legende_tableau("Diebold-Mariano entre les trois voies."))
ajouter(figure("16_comparaison.png", "Ratio des trois voies, par période",
               paste0("Aucune voie n'est meilleure dans les deux régimes à la fois. ",
                      "C'est la raison, non statistique, du choix retenu.")))
ajouter(sprintf(paste0("<p>Aucun écart n'est significatif — le plus favorable atteint ",
                       "p = %s. <strong>Les données ne permettent pas de départager les ",
                       "deux voies.</strong></p>"), nb(min(dm16$p_value), 3)))
ajouter(definition("Pourquoi la voie indirecte reste retenue malgré un ratio global moins bon",
  paste0("<p>Trois raisons, aucune n'étant statistique — puisque le test ne tranche ",
         "pas.</p>",
         "<p><strong>La stabilité entre régimes.</strong> Un modèle dont le ratio passe ",
         "de 0,27 à 1,35 selon la période n'est pas utilisable en production : on ne ",
         "sait pas, au moment de publier, dans lequel des deux régimes on se ",
         "trouve.</p>",
         "<p><strong>Le nombre de paramètres.</strong> La voie directe régresse une seule ",
         "série de 48 points sur un panier d'indicateurs agrégés. Le surajustement y est ",
         "structurellement plus probable, et le ratio de 1,35 hors 2020 en est ",
         "vraisemblablement la manifestation.</p>",
         "<p><strong>La lisibilité.</strong> La voie indirecte fournit une décomposition ",
         "par branche — d'où vient la croissance, quelle branche porte l'erreur ",
         "(section 11.1). La voie directe ne produit qu'un nombre.</p>",
         "<p>La combinaison des deux voies, testée aussi, ne départage rien : 0,860, ",
         "soit exactement la voie indirecte.</p>")))
}

# ---------------------------------------------------------------- 13
ajouter("<h2 id='s13'><span class='num'>13.</span>Ce que le système ne sait pas faire</h2>")
ajouter("<h3>13.1 Les écarts ne sont pas tous statistiquement établis</h3>")
t9 <- ic %>% dplyr::filter(scenario == "M3") %>%
  dplyr::transmute(Modèle = modele, `Ratio` = nb(ratio, 3),
                   `Intervalle à 90 %` = sprintf("[%s ; %s]", nb(borne_basse, 3),
                                                 nb(borne_haute, 3)))
ajouter(tbl(as.data.frame(t9), aligne_droite = 2:3))
ajouter(legende_tableau("Bootstrap par blocs sur le ratio par branche, scénario M3."))
ajouter("<p>Les intervalles se chevauchent. Au niveau des branches, seul le scénario M1 ",
        "rend la combinaison significativement meilleure que le BVAR. Ce qui résiste ",
        "aux tests dans les quatre scénarios, c'est que <strong>la combinaison bat ",
        "chacune de ses deux composantes prises isolément</strong>.</p>")
ajouter("<h3>13.2 Les données ne sont pas millésimées</h3>")
ajouter("<p>Le projet utilise la dernière version révisée des comptes. Les règles ",
        "anti-look-ahead portent sur les <strong>dates</strong>, jamais sur les ",
        "<strong>millésimes</strong>. Les prévisions sont donc évaluées contre une ",
        "réalité qui n'était pas connue à l'époque, ce qui <strong>flatte</strong> la ",
        "performance mesurée dans une proportion non mesurable sans une base de ",
        "millésimes.</p>")
ajouter("<h3>13.3 Les délais de publication sont ignorés</h3>")
ajouter("<p>Le scénario M2, celui du nowcast courant, suppose les deux premiers mois ",
        "connus à la fin du deuxième mois. En réalité un indicateur paraît avec quatre ",
        "à huit semaines de délai. <strong>L'avantage de calendrier est donc ",
        "surestimé</strong> : notre M2 correspond plutôt au M3 réel.</p>")
ajouter("<h3>13.4 Un backtest court, et une seule crise bien couverte</h3>")
ajouter(sprintf(paste0("<p>L'agrégat n'est évalué que sur <strong>%d trimestres</strong>, ",
                       "borne imposée par la valeur ajoutée nominale qui commence en ",
                       "2014. C'est court pour juger un nowcast, et cette borne ne ",
                       "pourra pas être repoussée sans données supplémentaires.</p>"),
                nrow(ag)))
ajouter("<p>Le backtest étendu à 2008 a permis d'évaluer un second épisode de rupture, ",
        "mais sur <strong>quatre branches seulement</strong> — et pas celles où la ",
        "passerelle est forte.</p>")
ajouter("<h3>13.5 La règle d'instabilité n'est vérifiée que sur un cas</h3>")
ajouter("<p>La règle de la section 5 est formulée généralement, mais elle ne s'est ",
        "déclenchée que sur <strong>une seule branche</strong>. Si une autre la ",
        "déclenchait un jour, rien ne garantit que le traitement lui conviendrait : ",
        "l'administration publique répond bien à sa moyenne parce que sa dynamique ",
        "récente est un bruit sans structure, ce qui n'a aucune raison d'être vrai ",
        "ailleurs.</p>")
ajouter("<p>C'est une règle <em>testée</em> sur un cas, non <em>validée</em> sur ",
        "plusieurs. La distinction doit figurer dans toute présentation du système.</p>")

ajouter("<h3>13.6 L'incertitude sur les hyperparamètres n'est pas couverte</h3>")
ajouter("<p>La distribution prédictive du BVAR propage l'incertitude sur les ",
        "coefficients et sur la matrice de covariance — le prior de Minnesota étant ",
        "conjugué normal-inverse-Wishart, on y tire directement, sans échantillonneur ",
        "de Gibbs ni diagnostic de convergence.</p>")
if (A_IC) {
  t96 <- calib %>% dplyr::transmute(
    `Niveau nominal` = sprintf("%.0f %%", 100 * niveau),
    `Couverture` = sprintf("%.0f %%", 100 * couverture),
    `Écart` = sprintf("%+.0f pt", 100 * ecart))
  ajouter(tbl(as.data.frame(t96), aligne_droite = 1:3))
  ajouter(legende_tableau("Calibration de la distribution prédictive du BVAR, avant recalibration."))
  ajouter(sprintf(paste0("<p>Mais elle traite les <strong>hyperparamètres</strong> — ",
                         m("p"), ", ", m("&lambda;"), ", ", m("d"),
                         ", la règle de détection des chocs, la fenêtre ", m("&sigma;"),
                         " — comme connus. C'est donc une <strong>borne basse</strong>, ",
                         "et la calibration le montre : un intervalle à 90 %% ne ",
                         "contient le réalisé que %.0f %% du temps.</p>"),
                  100 * calib$couverture[calib$niveau == 0.90]))
  ajouter(sprintf(paste0("<p>La recalibration conforme ramène l'écart de moitié, mais ",
                         "un déficit de 5 points subsiste, <strong>identique avant et ",
                         "après 2020</strong>. Le corriger supposerait de moyenner sur ",
                         "la grille de ", m("&lambda;"), ", ", m("p"), " et ", m("d"),
                         " à chaque origine, au prix d'un coût de calcul multiplié par ",
                         "la taille de la grille.</p>")))
  ajouter(sprintf(paste0("<p>Un chiffre de cadrage, et il explique beaucoup : ",
                         "l'incertitude <em>paramétrique</em> ne pèse que <strong>",
                         "%.0f %% de la variance</strong> prédictive. Le reste est ",
                         "l'aléa de choc. Autrement dit, <strong>mieux estimer ne ",
                         "resserrera pas l'intervalle</strong> — ce qui éclaire ",
                         "rétrospectivement l'échec des sept raffinements techniques de ",
                         "la section 13.7.</p>"),
                  100 * decomp$part_parametrique))
}

ajouter("<h3>13.7 Le pouvoir prédictif reste modeste</h3>")
ajouter("<p>Sept raffinements techniques ont été testés et ont échoué : sélection ",
        "conjointe, pondération géométrique, ridge, comblement par Kalman, poids ",
        "dépendant de l'état, seuil renforcé et sélection conditionnelle sur les ",
        "indicateurs trimestriels. Deux hypothèses ont été réfutées : que la ",
        "saisonnalité soit mal traitée, et que les erreurs se compensent à ",
        "l'agrégation.</p>")
ajouter("<p>Le faisceau est cohérent : <strong>le plafond est dans les données</strong>, ",
        "et aucun raffinement d'estimation ne crée de l'information qui n'y est pas. ",
        "Ce qui a fonctionné, ce ne sont pas les modèles plus savants, mais deux ",
        "changements de cadrage : évaluer <em>au bon moment du trimestre</em>, et ",
        "<em>agréger</em> plutôt que moyenner des branches.</p>")

# ---------------------------------------------------------------- 14
ajouter("<h2 id='s14'><span class='num'>14.</span>Reproduire ces résultats</h2>")
scripts <- data.frame(
  Script = c("R/01_import_donnees.R", "R/02_analyse_exploratoire.R",
             "R/03_bvar_trimestriel.R", "R/03c_selection_conjointe.R",
             "R/03d_bvar_etendu.R", "R/03e_branches_instables.R",
             "R/04_bridge_equations.R",
             "R/04b_variantes_passerelle.R", "R/04c_kalman_trous.R",
             "R/04d_combinaison_etat.R", "R/05_ar4_non_couvertes.R",
             "R/06_agregation_fisher.R", "R/07_validation_pseudo_temps_reel.R",
             "R/09_test_affinement_intra_trimestre.R", "R/09b_backtest_etendu.R",
             "R/10_incertitude.R", "R/12_incertitude_parametrique.R",
             "R/12b_recalibration.R", "R/12c_intervalle_combinaison.R",
             "R/13_benchmarks.R", "R/14_robustesse.R", "R/15_diagnostics.R",
             "R/16_direct_indirect.R", "R/08_rapport_synthese.R"),
  Étape = c("1", "2", "3", "3 bis", "3 ter", "3 quater", "4", "4 bis", "4 ter", "4 quater",
            "5", "6", "7", "8", "8 bis", "—", "—", "—",
            "—", "—", "—", "—", "—", "9"),
  Durée = c("~1 min", "~1 min", "~40 min", "~30 min", "< 1 min", "< 1 min",
            "~5 min", "~10 min", "~25 min", "~5 min", "< 1 min", "< 1 min",
            "~2 min", "~2 h 30", "~1 h", "~5 min", "~3 min", "< 1 min",
            "< 1 min", "~15 min", "~40 min", "~2 min", "~20 min", "~2 min"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(scripts, aligne_droite = integer(0)))
ajouter(legende_tableau("Les scripts, dans l'ordre du plan."))
ajouter("<p>L'ensemble représente <strong>plus de six heures</strong> de calcul. Les ",
        "phases longues écrivent des <strong>points de reprise</strong> par tâche : une ",
        "relance relit ce qui existe au lieu de le refaire.</p>")
ajouter("<h3>Rapports détaillés</h3>")
ajouter("<ul>",
        "<li><code>report/rapport_phase2.html</code> — données, transformations, ",
        "stationnarité ;</li>",
        "<li><code>report/rapport_phase3.html</code> — le BVAR expliqué de bout en ",
        "bout, 19 sections et 35 équations ;</li>",
        "<li><code>report/rapport_phases4_13.html</code> — les passerelles, les six ",
        "pistes d'amélioration, le nowcasting intra-trimestriel, 20 sections ;</li>",
        "<li><code>report/rapport_synthese.html</code> — ce document.</li>",
        "</ul>")
ajouter("<h3>Ce qui reste du plan</h3>")
ajouter("<p>Les phases 15 à 26 sont traitées aux sections 9 à 12 : étalons externes, ",
        "tests de robustesse systématiques, diagnostics de l'erreur, et comparaison des ",
        "voies directe et indirecte.</p>")
ajouter("<p>Ce qui reste ouvert ne relève plus du calcul mais de la ",
        "<strong>donnée</strong> : la série officielle de valeur ajoutée totale publiée ",
        "par le HCP — qui permettrait d'évaluer l'agrégat contre la grandeur publiée ",
        "plutôt que contre l'indice reconstruit ici — le calendrier de diffusion des ",
        "comptes trimestriels, et les millésimes successifs. Ces trois éléments lèveraient ",
        "les limites 13.2 et 13.3 ; aucun ne peut être produit à partir des fichiers ",
        "disponibles.</p>")

ajouter(pied_rapport("R/08_rapport_synthese.R"))
ecrire_rapport(h, "GDPNow-Maroc — Synthèse", CHEMIN)
cat(sprintf("[rapport] %s (%.1f Mo, %d figures, %d tableaux, %d equations)\n",
            CHEMIN, file.size(CHEMIN) / 1024^2, .n_figure, .n_tableau,
            get(".n_equation", envir = globalenv())))
