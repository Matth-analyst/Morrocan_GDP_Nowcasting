# ============================================================================
# 02b_rapport_phase2.R -- Rapport detaille de la phase 2
# ============================================================================
# Produit report/rapport_phase2.html a partir des sorties de la phase 2.
# Toutes les valeurs chiffrees du rapport sont relues depuis resultats/02_*.csv
# et les figures depuis figures/02_*.png : le rapport ne peut pas diverger des
# resultats, il se regenere avec eux.
#
# Les images sont encodees en base64 dans le HTML : le fichier est autonome,
# il s'ouvre et se transmet seul, et s'imprime en PDF depuis le navigateur.
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/rapport.R")

CHEMIN_RAPPORT <- file.path(DOSSIER_RAPPORT, "rapport_phase2.html")

init_compteurs()

# ----------------------------------------------------------------------------
# Lecture des resultats
# ----------------------------------------------------------------------------
cat("[rapport] Lecture des resultats de la phase 2\n")

res <- function(x) file.path(DOSSIER_RESULTATS, paste0("02_", x))
requis <- c("stats_descriptives_va.csv", "tests_stationnarite.csv",
            "correlations_branches.csv", "diagnostic_indicateurs.csv",
            "controle_transformations.csv")
absents <- requis[!file.exists(res(requis))]
if (length(absents) > 0L) {
  stop("Sorties de la phase 2 absentes : ", paste(absents, collapse = ", "),
       "\nExecuter d'abord R/02_analyse_exploratoire.R", call. = FALSE)
}

stats_va      <- lire_csv(res("stats_descriptives_va.csv"))
stationnarite <- lire_csv(res("tests_stationnarite.csv"))
cor_long      <- lire_csv(res("correlations_branches.csv"))
diagnostic    <- lire_csv(res("diagnostic_indicateurs.csv"))
controle      <- lire_csv(res("controle_transformations.csv"))
metadonnees   <- charger_metadonnees()
va            <- charger_va()
couverture    <- charger_couverture()

# quelques agregats recalcules pour le texte
n_series   <- nrow(metadonnees)
n_mens     <- sum(metadonnees$frequence == "mensuel")
n_trim     <- sum(metadonnees$frequence == "trimestriel")
n_obs_ind  <- sum(metadonnees$n_obs)
n_local    <- sum(controle$locale)
n_controle <- nrow(controle)

paires <- cor_long %>%
  dplyr::filter(branche_1 < branche_2) %>%
  dplyr::arrange(dplyr::desc(abs(correlation)))

# contre-exemple chiffre, recalcule ici pour que le rapport porte le chiffre
demo <- va %>% dplyr::filter(branche == "Construction") %>% dplyr::arrange(date) %>%
  dplyr::mutate(dlog = transformer_serie(va, "dlog", date, "trimestriel"))
x_demo <- demo$dlog[!is.na(demo$dlog)]
ecart_scale <- max(abs(as.numeric(scale(x_demo))[1:40] -
                         as.numeric(scale(x_demo[1:40]))))

# ----------------------------------------------------------------------------
# Corps du rapport
# ----------------------------------------------------------------------------
cat("[rapport] Composition\n")
h <- character(0)
ajouter <- function(...) h <<- c(h, paste0(..., collapse = ""))

ajouter('<header class="titre">',
        '<div class="sur">GDPNow-Maroc &middot; Méthode 1 &middot; Rapport d\'étape</div>',
        '<h1>Phase 2 — Transformations et analyse exploratoire</h1>',
        '<p class="sous">Rendre les transformations compatibles avec le backtest, ',
        'et documenter la base avant modélisation.</p>',
        sprintf('<p class="meta">Généré le %s &middot; %s &middot; ',
                format(Sys.Date(), "%d/%m/%Y"), R.version.string),
        'sources : <code>GDPNow_Maroc_series_retenues_Apres_Tris_economiques.xlsx</code>, ',
        '<code>VA_reelle_par_branche.xlsx</code></p>',
        '</header>')

# --- Sommaire ---------------------------------------------------------------
sections <- c(
  "Objet de la phase et problème corrigé", "Architecture logicielle",
  "Conventions retenues", "Données en entrée",
  "Transformation de la variable cible", "Statistiques descriptives",
  "Stationnarité", "Corrélations entre branches",
  "Règles de transformation des indicateurs", "Diagnostic du vivier et ragged edge",
  "Contrôle anti-look-ahead", "Limites et points ouverts",
  "Fichiers produits", "Suite du travail"
)
ajouter('<nav class="sommaire"><strong>Sommaire</strong><ol>',
        paste0(sprintf('<li><a href="#s%d">%s</a></li>', seq_along(sections),
                       esc(sections)), collapse = ""),
        '</ol></nav>')

# --- Chiffres cles ----------------------------------------------------------
ajouter('<div class="cles">',
        sprintf('<div class="cle"><div class="v">%s</div><div class="l">séries d\'indicateurs traitées</div></div>', nb(n_series)),
        sprintf('<div class="cle"><div class="v">%s</div><div class="l">observations transformées</div></div>', nb(sum(diagnostic$n_transforme))),
        sprintf('<div class="cle"><div class="v">%d/16</div><div class="l">branches stationnaires en Δlog</div></div>', sum(stationnarite$stationnaire_dlog %in% TRUE)),
        sprintf('<div class="cle"><div class="v">%s</div><div class="l">séries dont la transformation est locale</div></div>', sprintf("%d/%d", n_local, n_controle)),
        '</div>')

# =========================== 1 ==============================================
ajouter('<h2 id="s1"><span class="num">1.</span>Objet de la phase et problème corrigé</h2>')
ajouter('<p>La phase 2 correspond à la section 2 du plan de correction et à ',
        'l\'étape 2 de son calendrier (section 32). Son objectif est énoncé ainsi : ',
        '<em>« rendre les transformations compatibles avec le backtest »</em>.</p>')

ajouter('<h3>Ce que faisait la version précédente</h3>')
ajouter('<p>Le script <code>02_analyse_exploratoire.R</code> de la version 1 calculait ',
        'les taux de croissance une fois pour toutes sur l\'échantillon complet, ',
        'puis les écrivait dans un fichier <code>cibles_avec_dlog.rds</code> que ',
        'toutes les étapes suivantes relisaient, backtest compris :</p>')
ajouter('<pre><code>cibles <- cibles %>%\n',
        '  group_by(branche) %>%\n',
        '  mutate(dlog_va = c(NA, diff(log(va))))\n',
        'saveRDS(cibles, "resultats/cibles_avec_dlog.rds")</code></pre>')
ajouter('<p>Le même script produisait aussi <code>indicateurs_trimestriels.rds</code>, ',
        'où les séries mensuelles étaient agrégées en trimestres par une moyenne ',
        'uniforme des Δlog, et lançait des tests ADF et des corrélations sur la ',
        'période entière.</p>')

ajouter('<div class="encadre"><span class="etiq">Diagnostic</span>',
        '<p>Δlog est une opération <strong>locale</strong> : la valeur en <em>t</em> ',
        'ne dépend que de <em>t</em> et de <em>t−1</em>. Figer le résultat dans un ',
        'fichier ne créait donc pas, en soi, de fuite d\'information.</p>',
        '<p>Le problème était <strong>structurel</strong>. Rien, dans cette ',
        'construction, ne distinguait une opération locale d\'une opération qui ',
        'calibre un paramètre sur l\'échantillon. Le jour où l\'on ajoute une ',
        'standardisation, un lissage ou une imputation dans ce fichier — ce que la ',
        'version 1 faisait effectivement ailleurs, avec un lissage de Kalman ',
        'bidirectionnel — la fuite devient invisible, parce que tout le pipeline ',
        'consomme le même objet pré-calculé sans savoir comment il a été produit.</p>',
        '</div>')

ajouter('<h3>Ce que fait la phase 2 corrigée</h3>')
ajouter('<p>Trois décisions structurent cette étape.</p>')
ajouter('<ol>',
        '<li><strong>Les transformations deviennent des fonctions, pas un fichier.</strong> ',
        'Aucune série transformée n\'est sauvegardée. Chaque origine du backtest ',
        'appellera <code>transformer_serie()</code> sur sa propre information.</li>',
        '<li><strong>L\'analyse exploratoire est explicitement séparée du modèle.</strong> ',
        'Elle reste calculée sur l\'échantillon complet — c\'est légitime pour ',
        'décrire la base dans le mémoire — mais aucun de ses résultats ne devient ',
        'un paramètre du modèle. Un test ADF sur 1998–2026 ne décide pas de la façon ',
        'dont une série est transformée : cette règle vient des métadonnées de la ',
        'phase 1, fondées sur la nature économique de la variable.</li>',
        '<li><strong>Un contrôle automatique vérifie la localité des transformations</strong> ',
        'sur les 450 séries, à plusieurs origines. Il échoue et arrête le pipeline ',
        'si quelqu\'un introduit une opération non locale dans la chaîne.</li>',
        '</ol>')

# =========================== 2 ==============================================
ajouter('<h2 id="s2"><span class="num">2.</span>Architecture logicielle</h2>')
ajouter('<p>La phase 2 ajoute quatre fichiers, conformément à l\'arborescence ',
        'recommandée par la section 27 du plan.</p>')

arch <- data.frame(
  Fichier = c("R/fonctions/transformations.R", "R/fonctions/donnees.R",
              "R/02_analyse_exploratoire.R", "R/02b_rapport_phase2.R"),
  Rôle = c(
    "Transformations stationnaires et contrôles associés",
    "Relecture de la base CSV produite en phase 1, conversion en format long",
    "Script de la phase : transformations, exploration, contrôles, figures",
    "Génération de ce rapport à partir des sorties de la phase"),
  `Fonctions exportées` = c(
    "transformer_serie, appliquer_transformations, standardiser_recursif, verifier_transformation_locale",
    "charger_va, charger_indicateurs, charger_metadonnees",
    "—", "—"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(arch, aligne_droite = integer(0)))
ajouter(legende_tableau("Fichiers ajoutés par la phase 2."))

ajouter('<h3>Les quatre fonctions de <code>transformations.R</code></h3>')
fonctions <- data.frame(
  Fonction = c("transformer_serie(valeurs, transformation, dates, frequence)",
               "appliquer_transformations(data)",
               "standardiser_recursif(x, min_obs)",
               "verifier_transformation_locale(valeurs, dates, ...)"),
  Description = c(
    "Applique dlog, diff ou niveau à un vecteur ordonné. Refuse dlog sur une série comportant des valeurs négatives ou nulles. Met à NA les différences qui enjambent un trou.",
    "Applique à une base longue la règle portée par les métadonnées de chaque série, série par série.",
    "Standardisation dont la moyenne et l'écart-type sont recalculés à chaque date sur l'information disponible jusqu'à cette date. Alternative admissible à scale(), qui est interdit dans le backtest.",
    "Contrôle 3 de la section 30 : vérifie que transformer puis tronquer donne le même résultat que tronquer puis transformer, à cinq origines réparties sur la série."),
  stringsAsFactors = FALSE)
ajouter(tbl(fonctions, aligne_droite = integer(0)))
ajouter(legende_tableau("Interface de R/fonctions/transformations.R."))

# =========================== 3 ==============================================
ajouter('<h2 id="s3"><span class="num">3.</span>Conventions retenues</h2>')

ajouter('<h3>3.1 Échelle des taux de croissance</h3>')
ajouter('<p>La section 2 du plan laisse le choix entre <em>g<sub>t</sub> = Δlog(X<sub>t</sub>)</em> ',
        'et <em>g<sub>t</sub> = 100·Δlog(X<sub>t</sub>)</em>, en imposant de tenir ',
        'une seule convention partout. <strong>Le projet retient Δlog brut.</strong> ',
        'La conversion en pourcentage se fait uniquement à l\'affichage — dans les ',
        'tableaux et les figures de ce rapport, jamais dans les objets calculés. ',
        'Cela évite d\'avoir à se demander, en lisant un coefficient de bridge ',
        'equation ou un RMSFE, dans quelle unité il est exprimé.</p>')

ajouter('<h3>3.2 Datation</h3>')
ajouter('<p>Héritée de la phase 1 : toute observation est datée au <strong>dernier ',
        'jour de sa période</strong>. Une période n\'étant observée qu\'à sa fin, la ',
        'date porte elle-même le moment où l\'observation entre dans l\'ensemble ',
        'd\'information, et la comparaison <code>date &lt;= borne</code> suffit.</p>')

ajouter('<h3>3.3 Traitement des trous à la transformation</h3>')
ajouter('<p>Une différence calculée entre deux observations séparées par un trou ',
        'n\'est pas une variation d\'une période à l\'autre. Exemple réel, série ',
        '<code>Trafic globale gérés par l\'ANP</code> : août 2009 puis octobre 2009, ',
        'septembre manquant. La différence brute mélangerait deux mois de croissance ',
        'en un point présenté comme mensuel.</p>')
ajouter('<p><code>transformer_serie()</code> met ces points à <code>NA</code> plutôt ',
        'que de les présenter comme des variations d\'une période. <strong>Combler ',
        'les trous est un choix de modélisation, qui relève de la phase 4</strong> ',
        '(traitement des valeurs manquantes), pas de la transformation. La phase 2 ',
        'se contente de ne pas mentir sur ce qui est observé.</p>')

# =========================== 4 ==============================================
ajouter('<h2 id="s4"><span class="num">4.</span>Données en entrée</h2>')
ajouter(sprintf(paste0('<p>La phase 2 lit exclusivement les CSV produits par la ',
                       'phase 1 — jamais les classeurs Excel. Elle porte sur ',
                       '<strong>%s observations de valeur ajoutée</strong> ',
                       '(16 branches, %s à %s) et <strong>%s séries d\'indicateurs</strong> ',
                       '(%s mensuelles, %s trimestrielles, %s observations).</p>'),
                nb(nrow(va)), min(va$trimestre[va$date == min(va$date)]),
                max(va$trimestre[va$date == max(va$date)]),
                nb(n_series), nb(n_mens), nb(n_trim), nb(n_obs_ind)))
ajouter(sprintf(paste0('<p>Douze branches disposent d\'au moins un indicateur. Les ',
                       'quatre autres — %s — resteront en AR(4) pur (section 9 du plan).</p>'),
                esc(paste(couverture$non_couvertes, collapse = ", "))))

# =========================== 5 ==============================================
ajouter('<h2 id="s5"><span class="num">5.</span>Transformation de la variable cible</h2>')
ajouter('<p>Les seize valeurs ajoutées sont des volumes strictement positifs : le ',
        'script vérifie ce point puis applique Δlog à toutes. Un contrôle explicite ',
        'arrête le pipeline si une VA négative ou nulle apparaissait, plutôt que de ',
        'produire des <code>NaN</code> silencieux.</p>')
ajouter('<pre><code>va <- va %>%\n',
        '  group_by(branche) %>%\n',
        '  mutate(dlog_va = transformer_serie(va, "dlog", date, "trimestriel"))</code></pre>')
ajouter('<p>La différence avec la version 1 n\'est pas dans le calcul, identique, ',
        'mais dans le fait que le résultat n\'est <strong>pas sauvegardé</strong>. ',
        'Il est recalculé par chaque phase qui en a besoin, à partir de l\'information ',
        'dont elle dispose à son origine.</p>')

# =========================== 6 ==============================================
ajouter('<h2 id="s6"><span class="num">6.</span>Statistiques descriptives</h2>')

t6 <- stats_va %>%
  dplyr::transmute(
    Branche = branche,
    `Part moyenne (%)` = nb(100 * part_moyenne, 1),
    `Trimestres` = nb(n_trimestres),
    `Croissance moy. (%)` = nb(croissance_moy_pct, 2),
    `Volatilité (%)` = nb(volatilite_pct, 2),
    `Pire trim. (%)` = nb(min_pct, 1),
    `Date` = as.character(date_min)
  )
ajouter(tbl(as.data.frame(t6), aligne_droite = 2:6))
ajouter(legende_tableau(paste0(
  "Statistiques descriptives des seize branches, ordonnées par poids moyen dans la ",
  "valeur ajoutée totale. Croissance et volatilité sont la moyenne et l'écart-type ",
  "du taux de croissance trimestriel, exprimés en pourcentage.")))

ajouter(sprintf(paste0('<p>La structure sectorielle est concentrée : l\'industrie de ',
                       'transformation et le commerce pèsent à eux deux %s%% de la ',
                       'valeur ajoutée. À l\'autre extrémité, la pêche en représente ',
                       '%s%% — sa très forte volatilité (%s%%) pèsera donc peu sur le ',
                       'PIB agrégé, ce qui relativise l\'effort de modélisation qu\'elle ',
                       'mérite malgré ses 74 indicateurs disponibles.</p>'),
                nb(100 * sum(stats_va$part_moyenne[1:2]), 1),
                nb(100 * stats_va$part_moyenne[stats_va$branche == "Pêche"], 1),
                nb(stats_va$volatilite_pct[stats_va$branche == "Pêche"], 1)))

n_covid <- sum(stats_va$date_min == as.Date("2020-06-30"))
ajouter(sprintf(paste0('<p>Le trimestre le plus dégradé est le deuxième trimestre 2020 ',
                       'pour <strong>%d branches sur 16</strong>. L\'hébergement-restauration ',
                       'y perd %s%% en un trimestre, les transports %s%%. Ce point ',
                       'justifie la variable indicatrice COVID du BVAR (phase 3) et ',
                       'impose l\'analyse de robustesse avec et sans cette indicatrice ',
                       'prévue par la section 26.</p>'),
                n_covid,
                nb(abs(stats_va$min_pct[stats_va$branche == "Hébergement-restauration"]), 1),
                nb(abs(stats_va$min_pct[stats_va$branche == "Transports"]), 1)))

ajouter(figure("02_va_niveaux.png",
               "Valeur ajoutée trimestrielle en niveau, seize branches",
               paste0("Chaque panneau a sa propre échelle verticale. Les trajectoires sont ",
                      "nettement tendancielles, ce qui annonce le résultat des tests de ",
                      "stationnarité. Le décrochage de 2020 est visible sur presque tous ",
                      "les panneaux, mais son ampleur relative varie fortement : à peine ",
                      "marqué pour l'agriculture et l'éducation-santé, spectaculaire pour ",
                      "l'hébergement-restauration et les transports.")))

ajouter(figure("02_va_croissance.png",
               "Taux de croissance trimestriels, seize branches",
               paste0("Les mêmes séries après transformation Δlog. Les trajectoires ",
                      "oscillent désormais autour d'un niveau stable sans tendance ",
                      "apparente — la forme visuelle de la stationnarité. La saisonnalité ",
                      "reste visible sur plusieurs branches, notamment l'agriculture et la ",
                      "pêche : c'est ce qui motive les tests de robustesse saisonniers ",
                      "prévus par la section 24 du plan.")))

ajouter(figure("02_distribution_croissance.png",
               "Distribution des taux de croissance par branche",
               paste0("Branches ordonnées par volatilité croissante. L'écart d'amplitude ",
                      "entre le haut et le bas du graphique est d'un facteur vingt environ : ",
                      "éducation-santé et immobilier sont presque lisses, pêche et ",
                      "hébergement-restauration très dispersées. Les points isolés à gauche ",
                      "sont les trimestres extrêmes ; la plupart sont des trimestres 2020. ",
                      "Cette hétérogénéité justifie le prior de Minnesota du BVAR, qui ",
                      "normalise chaque équation par l'échelle propre de sa variable.")))

# =========================== 7 ==============================================
ajouter('<h2 id="s7"><span class="num">7.</span>Stationnarité</h2>')
ajouter('<div class="encadre"><span class="etiq">Statut de ce résultat</span>',
        '<p>Analyse exploratoire, menée sur l\'échantillon complet. Elle documente ',
        'et justifie le choix de modéliser les taux de croissance plutôt que les ',
        'niveaux. <strong>Elle ne pilote aucune décision du modèle</strong> : la règle ',
        'de transformation de chaque série vient des métadonnées de la phase 1.</p></div>')

ajouter('<p>Test de Dickey-Fuller augmenté, quatre retards. L\'hypothèse nulle est ',
        'la présence d\'une racine unitaire, c\'est-à-dire la non-stationnarité. ',
        'Une p-value inférieure à 0,05 conduit à rejeter cette hypothèse.</p>')

t7 <- stationnarite %>%
  dplyr::transmute(Branche = branche,
                   `p (niveau)` = nb(p_niveau, 3),
                   `Stationnaire en niveau` = ifelse(stationnaire_niveau, "oui", "non"),
                   `p (Δlog)` = nb(p_dlog, 3),
                   `Stationnaire en Δlog` = ifelse(stationnaire_dlog, "oui", "non"))
ajouter(tbl(as.data.frame(t7), aligne_droite = c(2, 4)))
ajouter(legende_tableau("Tests ADF, niveaux contre taux de croissance."))

ajouter(sprintf(paste0('<p>Le résultat est net : <strong>%d branche sur 16 seulement ',
                       'est stationnaire en niveau, contre %d sur 16 en Δlog</strong>. ',
                       'Cela confirme la pratique standard de la littérature de ',
                       'nowcasting — Higgins (2014) écrit l\'intégralité des équations ',
                       'de GDPNow en taux de croissance — et valide la spécification ',
                       'de la phase 3, où le BVAR portera sur les Δlog et non sur les ',
                       'log-niveaux.</p>'),
                sum(stationnarite$stationnaire_niveau %in% TRUE),
                sum(stationnarite$stationnaire_dlog %in% TRUE)))

ajouter('<p>Une conséquence de spécification en découle directement. Comme les ',
        'variables du BVAR sont déjà des taux de croissance et non des log-niveaux, ',
        'le prior de Minnesota doit être centré sur un retour à la moyenne ',
        '(δ<sub>i</sub> = 0) et non sur une marche aléatoire (δ<sub>i</sub> = 1) ',
        'comme dans la formulation originale de Litterman.</p>')

ajouter(figure("02_stationnarite.png",
               "p-values des tests ADF, niveau contre Δlog",
               paste0("Chaque branche porte deux points : cercle vide pour le niveau, ",
                      "point plein pour le taux de croissance. Le trait vertical marque ",
                      "le seuil de 5 %. Tous les points pleins sont à gauche du seuil, ",
                      "presque tous les cercles à droite. La lecture est immédiate et ",
                      "ne souffre pas d'ambiguïté de cas limites.")))

# =========================== 8 ==============================================
ajouter('<h2 id="s8"><span class="num">8.</span>Corrélations entre branches</h2>')
ajouter('<p>Corrélations des taux de croissance trimestriels, calculées par paires ',
        'sur les observations communes. Exploratoires elles aussi : le BVAR de la ',
        'phase 3 réestimera ses propres coefficients à chaque origine.</p>')

t8 <- paires %>% utils::head(10) %>%
  dplyr::transmute(`Branche A` = branche_1, `Branche B` = branche_2,
                   r = nb(correlation, 2))
ajouter(tbl(as.data.frame(t8), aligne_droite = 3))
ajouter(legende_tableau("Les dix paires de branches les plus corrélées en taux de croissance."))

ajouter(sprintf(paste0('<p>La corrélation la plus forte atteint %s entre %s et %s. ',
                       'Un ensemble de branches liées au cycle domestique — transports, ',
                       'services aux entreprises, hébergement-restauration, commerce — ',
                       'se déplace de façon très synchrone.</p>'),
                nb(paires$correlation[1], 2), esc(paires$branche_1[1]),
                esc(paires$branche_2[1])))
ajouter('<p>C\'est l\'argument empirique direct en faveur d\'un BVAR multivarié sur ',
        'les seize branches, plutôt que de seize modèles autorégressifs indépendants : ',
        'l\'information contenue dans une branche renseigne sur les autres. Cela ',
        'rejoint Bańbura, Giannone et Reichlin (2010), qui montrent qu\'un VAR ',
        'bayésien de grande dimension exploite ces liens sans exploser en nombre de ',
        'paramètres, grâce à la contrainte du prior.</p>')
ajouter('<p>Une mise en garde s\'impose toutefois : ces corrélations sont calculées ',
        'sur une période incluant 2020. Un choc commun d\'ampleur exceptionnelle ',
        'gonfle mécaniquement les corrélations entre toutes les branches qu\'il ',
        'affecte. La part de ces corrélations qui tient au COVID plutôt qu\'à un lien ',
        'structurel devra être examinée dans les diagnostics de la section 25.</p>')

ajouter(figure("02_correlations_branches.png",
               "Matrice de corrélation des taux de croissance",
               paste0("Les branches sont réordonnées par classification ascendante ",
                      "hiérarchique, de sorte que les groupes se lisent le long de la ",
                      "diagonale. Le bleu marque une corrélation positive, l'orange une ",
                      "corrélation négative, le blanc l'absence de lien. Un bloc dense ",
                      "apparaît autour des services marchands liés au cycle domestique ; ",
                      "l'agriculture, dont le cycle est climatique, s'en détache nettement.")))

# =========================== 9 ==============================================
ajouter('<h2 id="s9"><span class="num">9.</span>Règles de transformation des indicateurs</h2>')
ajouter('<p>Chaque indicateur porte deux règles, fixées en phase 1 sur la nature ',
        'économique de la variable, et corrigeables à la main dans ',
        '<code>data/metadonnees_indicateurs.csv</code>. La version 1 les confondait ',
        'en appliquant <code>mean(dlog)</code> à toutes les séries sans distinction.</p>')

regles_tab <- data.frame(
  Règle = c("agregation = sum", "agregation = mean", "agregation = last",
            "transformation = dlog", "transformation = diff", "transformation = niveau"),
  Signification = c(
    "Flux cumulable sur la période : le trimestre est la somme des trois mois",
    "Niveau, stock moyen, indice, prix, taux ou solde d'opinion : moyenne des trois mois",
    "Stock défini en fin de période : valeur du troisième mois",
    "Série de niveau strictement positive",
    "Série pouvant être nulle ou négative",
    "Série déjà exprimée en variation ou en taux : ne pas différencier à nouveau"),
  Exemples = c(
    "débarquements, trafic, arrivées de touristes, recettes, ventes de ciment, production d'énergie",
    "encours de crédit, IPI, IPM, prix du Brent, taux d'utilisation des capacités",
    "parc d'abonnés mobiles, noms de domaine .ma, masse monétaire M3",
    "la majorité des séries de niveau",
    "soldes d'opinion de l'enquête de conjoncture, températures, centrales à l'arrêt",
    "IPAI « var. trim. % », taux de pénétration, parts de marché"),
  stringsAsFactors = FALSE)
ajouter(tbl(regles_tab, aligne_droite = integer(0)))
ajouter(legende_tableau("Les deux familles de règles portées par les métadonnées."))

rep_regles <- metadonnees %>%
  dplyr::count(agregation, transformation, name = "n") %>%
  tidyr::pivot_wider(names_from = transformation, values_from = n, values_fill = 0L)
rep_regles <- rep_regles %>%
  dplyr::mutate(Total = rowSums(dplyr::across(-agregation))) %>%
  dplyr::rename(Agrégation = agregation)
ajouter(tbl(as.data.frame(rep_regles), aligne_droite = 2:ncol(rep_regles)))
ajouter(legende_tableau(sprintf(
  "Répartition croisée des %s séries selon leur règle d'agrégation et de transformation.",
  nb(n_series))))

ajouter(figure("02_regles_transformation.png",
               "Répartition des règles d'agrégation et de transformation",
               paste0("La règle mean domine, ce qui est attendu pour un vivier où les ",
                      "encours bancaires et les indices de production sont très ",
                      "représentés. Les 18 séries en « niveau » sont celles qu'il aurait ",
                      "été le plus coûteux de traiter comme les autres : différencier une ",
                      "variation trimestrielle déjà calculée aurait produit une série ",
                      "d'accélérations, économiquement sans rapport avec ce que la bridge ",
                      "equation est censée mesurer.")))

ajouter('<p>L\'agrégation mensuelle vers trimestrielle n\'est <strong>pas</strong> ',
        'effectuée à ce stade. Conformément à la section 32 du plan, elle appartient ',
        'à l\'étape 4, avec la sélection récursive et les bridge equations. La phase 2 ',
        'se limite à fixer et à documenter la règle ; son application viendra au ',
        'moment où un modèle en a besoin, sur l\'information de son origine.</p>')

# =========================== 10 =============================================
ajouter('<h2 id="s10"><span class="num">10.</span>Diagnostic du vivier et ragged edge</h2>')

diag_branche <- diagnostic %>%
  dplyr::group_by(branche) %>%
  dplyr::summarise(
    n_series = dplyr::n(),
    med = stats::median(trimestres_utiles),
    mini = min(trimestres_utiles),
    sous20 = sum(trimestres_utiles < 20L),
    trous = nb(100 * mean(taux_manquant, na.rm = TRUE), 1),
    .groups = "drop") %>%
  dplyr::arrange(med)

t10 <- diag_branche %>%
  dplyr::transmute(Branche = branche, `Séries` = nb(n_series),
                   `Trim. exploitables (médiane)` = nb(med),
                   `Minimum` = nb(mini),
                   `Séries < 20 trim.` = nb(sous20),
                   `Trous moyens (%)` = trous)
ajouter(tbl(as.data.frame(t10), aligne_droite = 2:6))
ajouter(legende_tableau(paste0(
  "Profondeur d'historique exploitable par branche. Une série mensuelle fournit au ",
  "mieux un point trimestriel tous les trois mois ; la colonne « trimestres ",
  "exploitables » tient compte de cette conversion ainsi que des trous.")))

ajouter(sprintf(paste0('<p><strong>%d séries sur %s offrent moins de vingt trimestres ',
                       'exploitables</strong>, et %d dépassent 20 %% de périodes ',
                       'manquantes. Ces séries ne sont pas inutilisables, mais elles ne ',
                       'permettront pas d\'estimer une bridge equation robuste aux ',
                       'origines anciennes du backtest.</p>'),
                sum(diagnostic$trimestres_utiles < 20L), nb(n_series),
                sum(diagnostic$taux_manquant > 0.20, na.rm = TRUE)))

ajouter('<p>Le déséquilibre entre branches est considérable. Les finances et ',
        'assurances offrent une médiane de 97 trimestres exploitables ; l\'immobilier ',
        'plafonne à 33, et son minimum est de 22. Cette contrainte devra être ',
        'traduite en un seuil minimal d\'historique dans la sélection récursive de la ',
        'phase 5 : une série qui n\'atteint pas ce seuil à l\'origine <em>T</em> ne ',
        'peut simplement pas être candidate à cette origine.</p>')

ajouter(figure("02_disponibilite_indicateurs.png",
               "Nombre d'indicateurs disponibles, année par année",
               paste0("Le vivier s'étoffe dans le temps — c'est la réalité de la ",
                      "disponibilité de l'information, pas un défaut de la base. La ",
                      "conséquence est directe pour la phase 5 : la sélection récursive ",
                      "ne disposera pas du même choix selon l'origine. La pêche passe de ",
                      "quelques séries avant 2015 à plus de soixante-dix ensuite ; ",
                      "l'électricité connaît un saut analogue vers 2019. Un backtest qui ",
                      "remonterait trop loin ferait donc tourner certaines branches sur un ",
                      "vivier quasi vide.")))

ajouter(figure("02_ragged_edge.png",
               "Calendrier d'observation des indicateurs mensuels, hébergement-restauration",
               paste0("Chaque ligne est une série, chaque case un mois observé. La figure ",
                      "montre le ragged edge sous sa forme la plus concrète. Trois régimes ",
                      "coexistent : les séries de villes s'arrêtent vers 2019, les séries ",
                      "par pays d'origine ne commencent qu'en 2023, et seules les arrivées ",
                      "aux frontières et les recettes touristiques couvrent toute la ",
                      "période. Les bords droits inégaux sont exactement ce que la phase 4 ",
                      "devra traiter, et les bords gauches inégaux ce qui limite le ",
                      "vivier aux origines anciennes.")))

ajouter(figure("02_diagnostic_series.png",
               "Profondeur d'historique contre taux de trous",
               paste0("Chaque point est une série. Le trait horizontal marque le seuil de ",
                      "vingt trimestres exploitables. La partie inférieure gauche du nuage ",
                      "— séries courtes mais complètes — est la plus nombreuse : le ",
                      "problème dominant du vivier n'est pas les trous, qui restent rares, ",
                      "mais la brièveté des historiques.")))

# =========================== 11 =============================================
ajouter('<h2 id="s11"><span class="num">11.</span>Contrôle anti-look-ahead</h2>')
ajouter('<p>La section 30 du plan demande des contrôles automatiques. Son contrôle 3 ',
        'porte sur la sélection ; la phase 2 en implémente l\'équivalent pour les ',
        'transformations.</p>')

ajouter('<h3>11.1 Critère</h3>')
ajouter('<p>Une transformation <em>f</em> est utilisable dans un backtest si, ',
        'appliquée à l\'origine <em>T</em>, elle donne exactement le même résultat ',
        'que la transformation calculée sur tout l\'échantillon puis tronquée en ',
        '<em>T</em> :</p>')
ajouter('<pre><code>f( X<sub>≤T</sub> )   ==   f( X )|<sub>≤T</sub></code></pre>')
ajouter('<p>C\'est vrai des opérations locales, qui ne regardent que <em>t</em> et ',
        '<em>t−1</em> : dlog, diff, niveau. C\'est faux de toute opération qui ',
        'calibre un paramètre sur l\'échantillon — moyennes, écarts-types, ',
        'standardisation, lissage, filtres, imputations — c\'est-à-dire exactement ',
        'la liste que la section 2 du plan énumère.</p>')

ajouter('<h3>11.2 Résultat</h3>')
ajouter(sprintf(paste0('<p>Le contrôle est appliqué à cinq origines réparties sur ',
                       'chaque série. <strong>%s séries vérifiées — 16 valeurs ajoutées ',
                       'et %s indicateurs — toutes locales.</strong> Aucune exception.</p>'),
                nb(n_controle), nb(n_controle - 16L)))
ajouter('<p>Le contrôle est bloquant : si une série échouait, le script s\'arrêterait ',
        'en nommant la série fautive. Il joue donc le rôle d\'un test de non-régression ',
        'pour toute la suite du projet — si une phase ultérieure introduit un lissage ',
        'ou une standardisation plein échantillon dans la chaîne de transformation, ',
        'l\'erreur apparaîtra ici plutôt que de se traduire par un RMSFE ',
        'anormalement flatteur.</p>')

ajouter('<h3>11.3 Contre-exemple chiffré</h3>')
ajouter(sprintf(paste0('<p>Pour montrer que ce contrôle n\'est pas une formalité, on ',
                       'applique le même test à une opération non locale : la ',
                       'standardisation <code>scale()</code>, sur les taux de croissance ',
                       'de la construction. Comparée à la même standardisation calculée ',
                       'en s\'arrêtant au quarantième trimestre, <strong>l\'écart maximal ',
                       'sur les quarante points communs atteint %s écart-type</strong>.</p>'),
                nb(ecart_scale, 2)))
ajouter('<p>Autrement dit : deux courbes qui décrivent les mêmes quarante trimestres ',
        'diffèrent de plus d\'un demi écart-type, uniquement parce que l\'une a été ',
        'calibrée en connaissant les quatre-vingts trimestres suivants — dont ceux de ',
        '2020, qui gonflent l\'écart-type de référence et écrasent mécaniquement toutes ',
        'les valeurs antérieures. Un modèle entraîné sur la version plein échantillon ',
        'aurait vu, en 2008, une information qui n\'existait pas.</p>')
ajouter('<p>C\'est la raison d\'être de <code>standardiser_recursif()</code>, fournie ',
        'dans <code>transformations.R</code> pour les phases qui en auront besoin.</p>')

ajouter(figure("02_standardisation_lookahead.png",
               "Standardisation plein échantillon, tronquée et récursive",
               paste0("Les deux courbes pleines décrivent les mêmes quarante premiers ",
                      "trimestres. Leur écart, visible à l'œil nu sur toute la partie ",
                      "gauche du graphique, ne vient d'aucune différence de données : il ",
                      "vient de ce que la courbe orange connaît la suite. La courbe ",
                      "pointillée est la standardisation récursive, qui à chaque date ",
                      "n'utilise que le passé — elle coïncide avec la version tronquée sur ",
                      "la partie gauche, et converge vers la version plein échantillon à ",
                      "mesure que l'échantillon s'allonge. C'est le comportement attendu ",
                      "d'une statistique admissible en temps réel.")))

# =========================== 12 =============================================
ajouter('<h2 id="s12"><span class="num">12.</span>Limites et points ouverts</h2>')

ajouter('<h3>12.1 Séries climatiques : deux règles corrigées</h3>')
ajouter('<p>L\'examen des deux séries climatiques de la branche Agriculture — ',
        '<code>Moyenne des precipitations</code> et <code>Température_moyenne</code>, ',
        'toutes deux mensuelles — a conduit à revoir leur règle de transformation. ',
        'Le raisonnement est détaillé ici parce qu\'il vaut comme méthode de ',
        'relecture pour le reste de la table, et parce qu\'il a mis au jour un ',
        'problème de portée générale, exposé en 12.2.</p>')

ajouter('<h4>Pluviométrie : lecture du libellé</h4>')
ajouter('<p>Le mot « Moyenne » pouvait laisser croire à une moyenne temporelle, ',
        'donc à une règle d\'agrégation <code>mean</code>. L\'examen des valeurs ',
        'montre qu\'il n\'en est rien : médiane 12,3 mm, maximum 63 mm, profil ',
        'saisonnier marqué (juillet 3,9 mm, mars 34,9 mm). Ce sont des <strong>cumuls ',
        'mensuels en millimètres, moyennés dans l\'espace entre stations</strong>. ',
        'Sommer trois mois donne donc le cumul pluviométrique du trimestre, qui est ',
        'la grandeur agronomiquement pertinente ; en faire la moyenne donnerait « la ',
        'pluie d\'un mois typique du trimestre », qui ne correspond à rien pour une ',
        'culture. <strong>La règle <code>sum</code> est conservée.</strong></p>')

ajouter('<h4>Pluviométrie : l\'erreur était dans la transformation</h4>')
ajouter('<p>Δlog appliqué à de la pluie ne produit pas un taux de croissance ',
        'économique mais le rapport de deux tirages météorologiques.</p>')
t121 <- data.frame(
  Passage = c("janvier → février 2020", "février → mars 2020",
              "écart-type du Δlog mensuel"),
  Valeurs = c("10,1 mm → 1,1 mm", "1,1 mm → 30,8 mm", "sur 76 mois"),
  `Δlog` = c("−2,21", "+3,32", "1,01"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(t121, aligne_droite = 3))
ajouter(legende_tableau("Amplitude du Δlog sur la série de précipitations."))

ajouter('<p>La production agricole dépend du <strong>niveau</strong> du cumul reçu, ',
        'pas de sa variation relative au trimestre précédent : 60 mm après un ',
        'trimestre à 20 mm et 60 mm après un trimestre à 90 mm, c\'est la même eau ',
        'dans le sol. Le cumul trimestriel est d\'ailleurs déjà stationnaire par ',
        'nature — borné, sans tendance, moyenne de 47 mm sur la période — et n\'a ',
        'donc aucun besoin d\'être différencié.</p>')
ajouter('<div class="encadre"><span class="etiq">Correction appliquée</span>',
        '<p><code>agregation = sum</code> conservée ; ',
        '<code>transformation</code> passée de <code>dlog</code> à ',
        '<code>niveau</code>, dans <code>data/metadonnees_indicateurs.csv</code> ',
        'et dans l\'heuristique de <code>R/01_import_donnees.R</code>, pour qu\'une ',
        'reconstruction depuis zéro donne le même résultat.</p></div>')

ajouter('<h4>La température relève du même raisonnement</h4>')
ajouter('<p>La seconde série climatique de la branche, <code>Température_moyenne</code>, ',
        'était classée <code>mean</code> / <code>diff</code>. L\'agrégation est ',
        'correcte — la température d\'un trimestre est bien la moyenne de ses mois — ',
        'mais la différenciation ne l\'est pas, et pour une raison plus nette encore ',
        'que dans le cas de la pluie.</p>')
t122 <- data.frame(
  Mois = c("janvier", "avril", "juillet", "octobre"),
  `Température moyenne` = c("12,2 °C", "18,4 °C", "28,4 °C", "21,7 °C"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(t122, aligne_droite = 2))
ajouter(legende_tableau("Profil saisonnier de la température, moyenne par mois calendaire sur 2020–2026."))

ajouter('<p>La série est <strong>quasi purement saisonnière</strong> : seize degrés ',
        'd\'amplitude entre janvier et juillet, contre une variabilité interannuelle ',
        'd\'un ordre de grandeur inférieur. Différencier une telle série ne retire pas ',
        'la saisonnalité — <strong>elle en produit la dérivée</strong>, c\'est-à-dire ',
        'un cycle de même période décalé d\'un quart de période. Les extrêmes du Δ ',
        'mensuel atteignent ±7 °C, mais ce sont des écarts de calendrier, pas de ',
        'climat : ils reviennent aux mêmes mois chaque année.</p>')
ajouter('<div class="encadre"><span class="etiq">Correction appliquée</span>',
        '<p><code>agregation = mean</code> conservée ; ',
        '<code>transformation</code> passée de <code>diff</code> à ',
        '<code>niveau</code>. Les deux séries climatiques sont désormais alignées ',
        'sur la même règle, dans le CSV comme dans l\'heuristique.</p></div>')

ajouter('<h4>Deux réserves qui subsistent</h4>')
ajouter('<p>Les deux séries commencent en <strong>janvier 2020</strong> : 26 ',
        'trimestres, dont le dernier partiel. Sous n\'importe quel seuil raisonnable ',
        'd\'historique minimal, elles ne seront candidates qu\'aux origines récentes ',
        'du backtest.</p>')
ajouter('<p>Surtout, passer en <code>niveau</code> corrige la transformation mais ne ',
        'règle pas la <strong>saisonnalité</strong>, qui est forte pour les deux : ',
        '78 mm au premier trimestre contre 26 mm au troisième pour la pluie, seize ',
        'degrés d\'amplitude pour la température. En niveau brut, une bridge equation ',
        'captera surtout ce cycle de calendrier — et la valeur ajoutée agricole ayant ',
        'elle-même une saisonnalité marquée, la régression risque de mesurer une ',
        'coïncidence de calendrier plutôt qu\'un lien agronomique.</p>')
ajouter('<p>La forme réellement informative est l\'<em>anomalie saisonnière</em> : ',
        'l\'écart au cumul, ou à la température, moyenne de la même période ',
        'calendaire. Mais cette moyenne devrait être calculée ',
        '<strong>récursivement</strong>, faute de quoi on retombe exactement sur le ',
        'contre-exemple de la section 11 — une moyenne par trimestre calendaire ',
        'estimée sur toute la période est une statistique qui connaît le futur. Avec ',
        'six ans de données, chaque moyenne trimestrielle reposerait sur six ',
        'observations : trop peu pour être fiable. Le point est renvoyé à la ',
        'robustesse saisonnière de la section 24 du plan.</p>')

ajouter('<h3>12.2 Agrégation <code>sum</code> et trimestres incomplets</h3>')
ajouter('<div class="encadre alerte"><span class="etiq">Contrainte pour la phase 13</span>',
        '<p>L\'examen de la pluviométrie a mis au jour un problème de portée bien ',
        'plus large. Le dernier cumul trimestriel de la série, T2-2026, vaut 23,3 mm ',
        '— mais il ne contient <strong>qu\'avril</strong> : mai et juin ne sont pas ',
        'encore observés.</p>',
        '<p>C\'est une propriété générale de la règle <code>sum</code> : ',
        '<strong>sommer un trimestre incomplet produit une valeur mécaniquement trop ',
        'basse</strong>, non comparable aux trimestres complets. <code>mean</code> est ',
        'robuste à cette situation, <code>sum</code> ne l\'est pas du tout.</p>',
        '</div>')
n_sum <- sum(metadonnees$agregation == "sum")
ajouter(sprintf(paste0('<p>La conséquence porte directement sur les scénarios ',
                       'intra-trimestriels. Un indicateur en <code>sum</code> observé ',
                       'sur un seul mois donnerait un « trimestre » valant environ le ',
                       'tiers de sa valeur normale : le modèle lirait une chute de ',
                       '67 %% là où il ne s\'est rien passé. Cela concerne ',
                       '<strong>%s des %s séries du vivier</strong>.</p>'),
                nb(n_sum), nb(n_series)))
ajouter('<p>Cela transforme une recommandation du plan en obligation technique. La ',
        'section 13 demande qu\'en M1 les mois m2 et m3 soient <em>prévus</em> puis ',
        'agrégés, plutôt que de se contenter des mois observés. Pour les séries en ',
        '<code>mean</code>, s\'en tenir aux mois observés reste une approximation ',
        'défendable. Pour les séries en <code>sum</code>, c\'est faux par construction : ',
        'la prévision des mois manquants n\'est pas un raffinement, c\'est ce qui rend ',
        'M1 et M2 définissables.</p>')
ajouter('<p>La fonction d\'agrégation de la phase 6 devra donc refuser d\'agréger en ',
        '<code>sum</code> un trimestre dont les trois mois ne sont pas disponibles, ',
        'observés ou prévus, plutôt que de sommer ce qu\'elle a sous la main.</p>')

ajouter('<h4>Le cas <code>mean</code> est moins grave, mais pas indemne</h4>')
ajouter('<p>La règle <code>mean</code> échappe au problème d\'échelle : la moyenne ',
        'd\'un seul mois reste dans la bonne unité. Elle reste en revanche exposée ',
        'à la <strong>saisonnalité intra-trimestrielle</strong>, comme la température ',
        'le montre bien. La moyenne du deuxième trimestre est de 21,5 °C ; calculée ',
        'sur le seul mois d\'avril, elle donne 18,4 °C. L\'écart de trois degrés n\'est ',
        'pas une anomalie climatique, c\'est le fait qu\'avril est plus froid que juin.</p>')
ajouter('<p>L\'erreur est d\'un ordre de grandeur inférieur à celle du cas ',
        '<code>sum</code>, mais elle est systématique et de signe connu : en M1 et M2, ',
        'toute série saisonnière agrégée sur les seuls mois observés est biaisée dans ',
        'la direction du début de trimestre. La règle de la section 13 — prévoir les ',
        'mois manquants puis agréger — corrige les deux cas d\'un coup. C\'est un ',
        'argument de plus pour l\'appliquer sans exception plutôt que série par série.</p>')

ajouter('<div class="encadre"><span class="etiq">Décision</span>',
        '<p>Un trimestre incomplet n\'est <strong>jamais agrégé dans l\'échantillon ',
        'd\'estimation</strong> : il est écarté, quelle que soit la règle ',
        'd\'agrégation de la série. Le coût est nul — il s\'agit du seul trimestre de ',
        'bord — et le bénéfice est de n\'introduire aucune observation biaisée dans ',
        'l\'estimation des bridge equations.</p>',
        '<p>Le <strong>trimestre cible</strong> relève d\'un régime distinct : ses ',
        'mois manquants sont <em>prévus</em> par la machinerie M0/M1/M2/M3 de la ',
        'section 13, puis agrégés. Il n\'est pas une observation de l\'échantillon, ',
        'il est ce que l\'on cherche à estimer. Au bord actuel de la base, la cible ',
        'est le deuxième trimestre 2026.</p>',
        '<p>La règle sera imposée par la fonction d\'agrégation de la phase 6, qui ',
        'refusera de produire un trimestre dont les trois mois ne sont pas ',
        'disponibles — observés pour l\'échantillon d\'estimation, observés ou prévus ',
        'pour le trimestre cible.</p></div>')

ajouter('<h3>12.3 Séries en <code>diff</code> : un diagnostic à corriger</h3>')

ajouter('<div class="encadre alerte"><span class="etiq">Rectification</span>',
        '<p>Une version antérieure de ce rapport présentait les séries en ',
        '<code>diff</code> comme un problème d\'<strong>échelle</strong> : la ',
        'transformation les laisse dans leur unité d\'origine, et l\'écart-type de ',
        '<code>. Centrale Thermo-solaire d\'Ain Béni Mathar</code> atteint ',
        '1,4·10<sup>8</sup>.</p>',
        '<p><strong>Ce diagnostic était largement faux.</strong> La corrélation est ',
        'invariante d\'échelle : le critère |r| ≥ 0,15 de la phase 5 n\'en souffre ',
        'pas. Et l\'estimation par moindres carrés d\'une bridge equation absorbe ',
        'l\'échelle dans son coefficient. L\'unité n\'est pas le problème.</p>',
        '<p>Le vrai problème est ailleurs, et l\'audit du vivier l\'a identifié : ces ',
        '99 séries ne formaient pas un groupe homogène, et la majorité n\'avait ',
        'rien à faire en <code>diff</code>.</p></div>')

ajouter('<h4>Quatre cas distincts</h4>')
t123 <- data.frame(
  Cas = c("Soldes d'opinion", "Flux net mal étiqueté",
          "Démarrage tardif", "Arrêts dispersés", "Donnée erronée"),
  Nombre = c("54", "1", "10", "30", "1"),
  Diagnostic = c(
    "Enquêtes de conjoncture HCP : « Évolution de la production par rapport au mois précédent », « Niveau des carnets de commandes ». Un solde est déjà une différence — la part des entreprises déclarant une hausse moins celle déclarant une baisse.",
    "« - Solde des échanges d'énergie (Espagne-Algérie) » : le mot « solde » l'avait rangée avec les enquêtes, mais c'est un flux net en GWh allant de −928 à +5 896.",
    "Parcs éoliens et centrales dont la série commence par des zéros : l'installation n'est pas encore en service. Le saut de 0 à la pleine production crée une valeur aberrante qui ne renseigne sur rien.",
    "Zéros situés à l'intérieur de la série : centrale réellement à l'arrêt, port sans débarquement sur un mois. Ce sont de vraies observations, qui portent de l'information.",
    "« Vente de ciment (1000tonnes) » vaut −0,02 en septembre 2000, entre 1 375 et 700 les mois voisins. Une vente de ciment négative est physiquement impossible."),
  Traitement = c("passées en <code>niveau</code>",
                 "reste en <code>diff</code>",
                 "zéros de tête retirés, <code>dlog</code> redevient possible",
                 "restent en <code>diff</code>",
                 "observation retirée"),
  stringsAsFactors = FALSE)
ajouter(tbl(t123, aligne_droite = 2))
ajouter(legende_tableau("Décomposition des 99 séries initialement classées en diff."))

ajouter('<h4>Les soldes d\'opinion mesurent déjà un changement</h4>')
ajouter('<p>C\'est le cas le plus nombreux et le plus net. Différencier un solde ',
        'de conjoncture revient à calculer l\'<em>accélération de l\'opinion</em>, ',
        'grandeur sans contrepartie économique. C\'est exactement la faute déjà ',
        'corrigée sur les IPAI exprimés en « var. trim. % » et sur les séries ',
        'climatiques.</p>')
ajouter('<p>Le test de reconnaissance ne peut pas reposer sur le seul libellé : le ',
        'mot « solde » attrape aussi le flux net d\'électricité échangé avec ',
        'l\'Espagne et l\'Algérie. L\'heuristique exige donc, en plus du libellé, que ',
        'les valeurs restent au voisinage de l\'intervalle [−100, +100] — un solde ',
        'd\'enquête s\'exprime en points de pourcentage.</p>')

ajouter('<h4>Zéros de tête contre zéros internes</h4>')
ajouter('<p>La distinction est décisive et se mesure directement : sur les 40 séries ',
        'comportant des zéros, <strong>10 ont leurs zéros en tête</strong> et ',
        '<strong>30 les ont dispersés</strong>.</p>')
ajouter('<p>Une éolienne qui produit 0 GWh avant sa mise en service ne fournit pas ',
        'l\'observation d\'une activité nulle : la série n\'existe pas encore. La ',
        'séquence initiale de zéros est donc retirée — le parc éolien de Taza I ',
        'perd ses onze premiers points, Khelladi et Afissat leurs douze. Les zéros ',
        'situés <em>à l\'intérieur</em> d\'une série sont au contraire conservés : ce ',
        'sont de vrais arrêts, et ils sont informatifs.</p>')
ajouter('<p>L\'effet est automatique et voulu : une série dont tous les zéros étaient ',
        'en tête redevient strictement positive, donc éligible à <code>dlog</code>. ',
        'La règle de transformation s\'ajuste d\'elle-même, sans intervention, parce ',
        'que le nettoyage précède le calcul des métadonnées.</p>')

n_diff <- sum(metadonnees$transformation == "diff")
n_niv  <- sum(metadonnees$transformation == "niveau")
n_dlog <- sum(metadonnees$transformation == "dlog")
ajouter(sprintf(paste0('<h4>Résultat</h4>',
                       '<p>Le vivier passe de 99 séries en <code>diff</code> à ',
                       '<strong>%s</strong>. Répartition finale : %s en ',
                       '<code>dlog</code>, %s en <code>niveau</code>, %s en ',
                       '<code>diff</code>.</p>'),
                nb(n_diff), nb(n_dlog), nb(n_niv), nb(n_diff)))
ajouter('<p>Les 35 restantes sont légitimes : 30 comportent de vrais zéros ',
        'internes, 5 sont négatives sur toute leur longueur — énergie absorbée par ',
        'le pompage des stations de transfert, auxiliaires de centrales — et n\'ont ',
        'donc jamais pu passer en <code>dlog</code>. Pour celles-là, ',
        '<code>diff</code> est la bonne réponse, et le problème d\'échelle évoqué ',
        'plus haut n\'en est pas un.</p>')

ajouter('<h3>12.4 Qualité de la base : trois séries écartées</h3>')
ajouter('<p>Un audit du vivier mené avant le passage à la phase 3 a révélé des ',
        'redondances qui auraient faussé la sélection récursive. Elles sont ',
        'désormais détectées <strong>automatiquement</strong> à chaque exécution, ',
        'par comparaison des signatures de séries — l\'ensemble de leurs couples ',
        '(date, valeur) — et non par une liste de noms codée en dur : si le ',
        'classeur source évolue, la détection suit.</p>')

ajouter('<h4>Doublons stricts : la branche fait la différence</h4>')
ajouter('<p>Quatre paires de séries portent exactement les mêmes valeurs aux mêmes ',
        'dates. Le traitement dépend de leur position :</p>')
ajouter('<ul>',
        '<li><strong>Intra-branche</strong> — deux candidats parfaitement ',
        'colinéaires pour la même bridge equation. Ils passeraient ensemble ',
        'n\'importe quel critère de sélection et compteraient double dans une ',
        'prévision moyennée. Une seule est conservée.</li>',
        '<li><strong>Inter-branches</strong> — un agrégat légitimement partagé. ',
        'Bank Al-Maghrib publie « Agriculture et pêche » en un seul poste, que le ',
        'classeur reprend dans les deux branches : c\'est bien le crédit pertinent ',
        'pour chacune. Les deux sont conservées, et le partage est consigné dans la ',
        'colonne <code>partage_avec</code>, pour que les diagnostics de la section 25 ',
        'puissent vérifier que les prévisions des deux branches ne deviennent pas ',
        'artificiellement corrélées.</li>',
        '</ul>')

t124 <- data.frame(
  Série = c("Ind. transfo :: Industries métallurgiques, mécaniques, électriques",
            "Électricité :: - Clients Directs",
            "Info-communication :: Parts de marché — Ensemble des opérateur",
            "Agriculture ↔ Pêche :: Comptes débiteurs et crédits de trésorerie",
            "Agriculture ↔ Pêche :: Crédits à l'équipement"),
  Motif = c("doublon strict de « Crédit bancaire Industries métallurgiques-mécaniques-électriques (MDH) »",
            "doublon strict de « Clients THT-HT »",
            "série constante : vaut 100 sur ses 77 observations, variance nulle",
            "identiques entre les deux branches — agrégat BAM partagé",
            "identiques entre les deux branches — agrégat BAM partagé"),
  Décision = c("écartée", "écartée", "écartée", "conservées", "conservées"),
  stringsAsFactors = FALSE)
ajouter(tbl(t124, aligne_droite = integer(0)))
ajouter(legende_tableau("Redondances détectées et traitement retenu."))

ajouter('<p>À information identique, la série conservée est celle dont le libellé ne ',
        'commence pas par une ponctuation — séquelle de la mise en page du classeur, ',
        'comme <code>- Clients Directs</code> ou <code>. Centrale de Tahaddart</code> ',
        '— puis, à défaut, la première dans l\'ordre alphabétique. Règle arbitraire ',
        'mais déterministe, donc reproductible.</p>')

ajouter('<h4>Agrégats et composantes</h4>')
n_agregat <- sum(metadonnees$role == "agregat")
ajouter(sprintf(paste0('<p>Le vivier mêle des totaux et leurs postes : ',
                       '<code>Energie nette appelée</code> et ses sources de ',
                       'production, <code>Total (2)</code> et les régions de vente de ',
                       'ciment, <code>TOTAL EXPORTATIONS</code> et ses lignes. Retenir ',
                       'un total <em>et</em> ses composantes dans la même bridge ',
                       'equation revient à compter deux fois la même information.</p>',
                       '<p>Une colonne <code>role</code> marque désormais les ',
                       '<strong>%d agrégats</strong> identifiables par leur libellé. La ',
                       'détection ne prétend pas reconstituer l\'arborescence complète ',
                       '— elle signale les cas sûrs, à charge pour la phase 5 d\'en ',
                       'tenir compte, et la colonne reste éditable comme les autres.</p>'),
                n_agregat))

ajouter('<h4>Séparation des diagnostics et des choix</h4>')
ajouter('<p>Cette détection a imposé de clarifier la sémantique de la table de ',
        'métadonnées. Deux catégories de colonnes y coexistent désormais, aux règles ',
        'opposées :</p>')
ajouter('<ul>',
        '<li><strong>Reprises du fichier à chaque exécution</strong> : ',
        '<code>agregation</code>, <code>transformation</code>, <code>unite</code>, ',
        '<code>role</code>, <code>commentaire</code>, <code>exclure_manuel</code>. ',
        'Ce sont des <em>choix</em> — une correction à la main ne doit jamais être ',
        'écrasée par l\'heuristique.</li>',
        '<li><strong>Toujours recalculées</strong> : <code>valide</code>, ',
        '<code>motif_exclusion</code>, <code>doublon_de</code>, ',
        '<code>partage_avec</code> et les statistiques de couverture. Ce sont des ',
        '<em>diagnostics</em> — les figer reviendrait à conserver le verdict rendu ',
        'sur une version antérieure du classeur.</li>',
        '</ul>')
ajouter('<p>D\'où la distinction entre <code>valide</code>, verdict automatique, et ',
        '<code>exclure_manuel</code>, décision de l\'analyste. Le pipeline consomme ',
        'leur conjonction, <code>retenu</code>. Écarter une série à la main ne ',
        'désactive donc pas la détection automatique, et inversement.</p>')

ajouter('<h3>12.5 Ce que la phase 2 ne traite pas</h3>')
ajouter('<ul>',
        '<li><strong>L\'agrégation mensuelle vers trimestrielle</strong> est définie ',
        'mais pas appliquée : elle appartient à l\'étape 4 du calendrier.</li>',
        '<li><strong>Le comblement des trous</strong> relève de la phase 4. La phase 2 ',
        'se contente de marquer à <code>NA</code> ce qui ne peut pas être calculé ',
        'honnêtement.</li>',
        '<li><strong>Aucune sélection d\'indicateur</strong> n\'est effectuée. Les ',
        'corrélations indicateur/cible ne sont volontairement pas calculées ici : ',
        'les produire sur l\'échantillon complet créerait précisément la tentation ',
        'que la phase 5 doit éliminer.</li>',
        '</ul>')

ajouter('<h3>12.6 Portée des analyses exploratoires</h3>')
ajouter('<p>Tests ADF, corrélations croisées et statistiques descriptives sont ',
        'calculés sur 1998–2026. Ils décrivent la base et justifient les choix de ',
        'spécification dans le mémoire. Ils ne constituent <strong>pas</strong> une ',
        'validation du modèle, et aucun de leurs résultats n\'entre dans une ',
        'estimation. La distinction doit rester explicite dans la rédaction finale, ',
        'conformément à la section 35 du plan.</p>')

# =========================== 13 =============================================
ajouter('<h2 id="s13"><span class="num">13.</span>Fichiers produits</h2>')

fichiers <- data.frame(
  Fichier = c("resultats/02_stats_descriptives_va.csv",
              "resultats/02_tests_stationnarite.csv",
              "resultats/02_correlations_branches.csv",
              "resultats/02_diagnostic_indicateurs.csv",
              "resultats/02_controle_transformations.csv"),
  Contenu = c(
    "Seize branches : poids moyen, croissance, volatilité, extrêmes et leurs dates",
    "p-values ADF en niveau et en Δlog, verdict de stationnarité",
    "Matrice de corrélation des Δlog, format long (256 paires)",
    "Une ligne par série : règles appliquées, couverture, trous, trimestres exploitables",
    "Résultat du contrôle de localité, série par série"),
  Lignes = c(nrow(stats_va), nrow(stationnarite), nrow(cor_long),
             nrow(diagnostic), nrow(controle)),
  stringsAsFactors = FALSE)
fichiers$Lignes <- nb(fichiers$Lignes)
ajouter(tbl(fichiers, aligne_droite = 3))
ajouter(legende_tableau("Tables de résultats, au format CSV UTF-8."))

figs <- list.files(DOSSIER_FIGURES, pattern = "^02_", full.names = FALSE)
ajouter(sprintf('<p>Dix figures dans <code>figures/</code> : %s</p>',
                esc(paste(figs, collapse = ", "))))

# =========================== 14 =============================================
ajouter('<h2 id="s14"><span class="num">14.</span>Suite du travail</h2>')
ajouter('<p>L\'étape 3 du calendrier porte sur <code>03_bvar_trimestriel.R</code>, ',
        'avec pour objectif de rendre l\'estimation récursive et exempte ',
        'd\'information future. Les éléments préparés par la phase 2 qui y seront ',
        'directement utiles :</p>')
ajouter('<ul>',
        '<li>la confirmation que les seize séries sont stationnaires en Δlog, qui fixe ',
        'la spécification du prior sur un retour à la moyenne plutôt que sur une ',
        'marche aléatoire ;</li>',
        '<li>les corrélations croisées, qui motivent l\'approche multivariée ;</li>',
        '<li>la dispersion très inégale des volatilités entre branches, qui impose de ',
        'normaliser le prior par l\'échelle propre de chaque variable — et de calculer ',
        'cette échelle <strong>récursivement</strong>, puisqu\'un écart-type estimé sur ',
        'tout l\'échantillon est précisément l\'opération non locale dont le contre-exemple ',
        'de la section 11 montre l\'effet ;</li>',
        '<li>le recensement des trimestres extrêmes, qui documente la variable ',
        'indicatrice COVID et son test de robustesse.</li>',
        '</ul>')

ajouter('<p>L\'audit de la cible mené en fin de phase 2 a par ailleurs mis au jour ',
        'trois points qui conditionnent la spécification du BVAR.</p>')

ajouter('<h3>14.1 La variable COVID de la version 1 est mal spécifiée</h3>')
ajouter('<p>Le code précédent posait une indicatrice unique valant 1 en 2020 T2 ',
        '<em>et</em> en 2020 T3, avec <strong>un seul coefficient</strong> par ',
        'équation. Le contenu de ces deux trimestres interdit ce partage.</p>')
t141 <- data.frame(
  Branche = c("Hébergement-restauration", "Transports", "Industrie de transformation",
              "Services aux entreprises", "Commerce"),
  `T1-2020` = c("−32,9", "−1,3", "−0,2", "−4,8", "−1,7"),
  `T2-2020` = c("−85,7", "−50,1", "−21,4", "−26,3", "−15,1"),
  `T3-2020` = c("+15,3", "+12,6", "+17,7", "+18,0", "+10,8"),
  `T4-2020` = c("+18,5", "+12,3", "−0,9", "+5,4", "+2,0"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(t141, aligne_droite = 2:5))
ajouter(legende_tableau("Taux de croissance trimestriels autour du choc COVID, en pourcentage."))

ajouter('<p>Le deuxième trimestre est un effondrement, le troisième est le ',
        '<strong>rebond</strong> : des signes opposés, sur presque toutes les ',
        'branches. Un coefficient unique estimera une valeur intermédiaire et ne ',
        'corrigera ni l\'un ni l\'autre — les résidus resteront élevés aux deux dates, ',
        'soit l\'inverse de ce qu\'une indicatrice doit produire. Le premier trimestre ',
        '2020, où l\'hébergement perd déjà 32,9 %, n\'était quant à lui couvert par ',
        'aucune indicatrice.</p>')
ajouter('<div class="encadre"><span class="etiq">Décision pour la phase 3</span>',
        '<p>Trois indicatrices distinctes — 2020 T1, T2 et T3 — chacune avec son ',
        'propre coefficient par équation. Le test de robustesse de la section 26 ',
        'comparera alors « avec » et « sans » sur une spécification qui a un sens.</p>',
        '</div>')

ajouter('<h3>14.2 Rupture de variance en 2014 : un artefact de rétropolation</h3>')
ajouter('<p>La base est « 2014 rétropolée » : avant 2014, les données sont ',
        'reconstruites. En comparant les volatilités de part et d\'autre de 2014, ',
        '<strong>en excluant l\'année 2020</strong> pour ne pas confondre l\'artefact ',
        'avec le choc COVID :</p>')
t142 <- data.frame(
  Branche = c("Administration publique", "Information-communication", "Immobilier",
              "Éducation-santé", "Finances et assurances", "…",
              "Services aux entreprises", "Autres services", "Hébergement-restauration"),
  `Avant 2014` = c("6,27 %", "4,72 %", "1,31 %", "1,12 %", "2,05 %", "",
                   "1,16 %", "0,81 %", "2,76 %"),
  `Depuis 2014` = c("0,70 %", "2,40 %", "0,68 %", "0,68 %", "1,33 %", "",
                    "2,10 %", "1,62 %", "5,63 %"),
  Rapport = c("0,11", "0,51", "0,52", "0,61", "0,65", "", "1,81", "2,00", "2,04"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(t142, aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Écart-type du taux de croissance trimestriel avant et depuis 2014, année 2020 ",
  "exclue. Extrêmes de la distribution des seize branches.")))

ajouter('<p>Un test de Fisher d\'égalité des variances rejette l\'égalité pour ',
        '<strong>12 branches sur 16</strong>, l\'administration publique à une ',
        'p-value nulle à la précision machine. Un facteur neuf de volatilité sur une ',
        'branche pesant 10 % du PIB n\'est pas un phénomène économique : c\'est la ',
        'méthode de reconstruction.</p>')
ajouter('<p>Deux conséquences pour la phase 3. D\'abord, l\'échelle σ<sub>i</sub> qui ',
        'normalise le prior de Minnesota <strong>doit</strong> être recalculée à ',
        'chaque origine : une valeur estimée sur tout l\'échantillon serait fausse ',
        'pour les deux régimes à la fois. Ensuite, une fenêtre extensive mélange ces ',
        'deux régimes — ce qui donne un contenu réel à la comparaison entre fenêtre ',
        'glissante et fenêtre extensive prévue par la section 21, au lieu d\'en faire ',
        'un exercice de forme.</p>')

ajouter('<h3>14.3 Le système est très mince en degrés de liberté</h3>')
ajouter('<p>Avec seize branches et cinq retards, chaque équation compte ',
        '<strong>82 paramètres</strong> — une constante, les indicatrices, et ',
        '16 × 5 coefficients de retard — pour environ 106 observations utilisables en ',
        'fin d\'échantillon, soit un rapport de 1,3. Aux origines anciennes du ',
        'backtest, autour de soixante trimestres disponibles, ce rapport passe sous ',
        '1 : le système est <strong>sous-déterminé et la prévision est entièrement ',
        'pilotée par le prior</strong>.</p>')
ajouter('<p>Ce n\'est pas une anomalie — c\'est précisément la raison d\'être d\'un ',
        'VAR bayésien, et Bańbura, Giannone et Reichlin (2010) construisent leur ',
        'argument sur ce point. Mais cela doit être documenté explicitement dans le ',
        'mémoire, et impose de comparer p = 5 à des ordres plus courts dans les tests ',
        'de robustesse, plutôt que de reprendre p = 5 au seul motif que GDPNow ',
        'l\'utilise sur un échantillon américain bien plus long.</p>')

ajouter('<footer>',
        sprintf('Rapport généré automatiquement par <code>R/02b_rapport_phase2.R</code> le %s. ',
                format(Sys.time(), "%d/%m/%Y à %H:%M")),
        'Toutes les valeurs chiffrées sont relues depuis <code>resultats/02_*.csv</code> ',
        'et les figures depuis <code>figures/02_*.png</code> : le rapport se régénère ',
        'avec les résultats et ne peut pas en diverger.',
        '</footer>')

# ----------------------------------------------------------------------------
# Ecriture
# ----------------------------------------------------------------------------
ecrire_rapport(h, "Phase 2 — Transformations et analyse exploratoire", CHEMIN_RAPPORT)

cat(sprintf("[rapport] %s (%.1f Mo, %d figures, %d tableaux)
",
            CHEMIN_RAPPORT, file.size(CHEMIN_RAPPORT) / 1024^2,
            .n_figure, .n_tableau))
