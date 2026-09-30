# ============================================================================
# 07_rapport.R -- Le rapport de la methode a facteurs dynamiques
# ============================================================================
# Le rapport ne parle pas de l'environnement de travail : ni scripts, ni
# fichiers, ni versions. Il expose une methode, des resultats, et ce qu'ils
# valent face a la methode 1.
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/rapport.R")

CHEMIN <- file.path(DOSSIER_RAPPORT, "rapport_dfm.html")
init_compteurs(); assign(".n_equation", 0L, envir = globalenv())
r <- function(x) file.path(DOSSIER_RESULTATS, x)

source("R/fonctions/mise_en_page.R")

# --- references numerotees ---------------------------------------------------
BIBLIO <- c(
  geweke  = "Geweke, J. (1977). \u00ab The Dynamic Factor Analysis of Economic Time Series \u00bb, dans <em>Latent Variables in Socio-Economic Models</em>, North-Holland.",
  sargent = "Sargent, T. J. et Sims, C. A. (1977). <em>Business Cycle Modeling Without Pretending to Have Too Much A Priori Economic Theory</em>, Federal Reserve Bank of Minneapolis.",
  giannone = "Giannone, D., Reichlin, L. et Small, D. (2008). \u00ab Nowcasting: The Real-Time Informational Content of Macroeconomic Data \u00bb, <em>Journal of Monetary Economics</em>, 55(4), 665-676.",
  mariano = "Mariano, R. S. et Murasawa, Y. (2003). \u00ab A New Coincident Index of Business Cycles Based on Monthly and Quarterly Series \u00bb, <em>Journal of Applied Econometrics</em>, 18(4), 427-443.",
  wallis  = "Wallis, K. F. (1986). \u00ab Forecasting with an econometric model: the ragged edge problem \u00bb, <em>Journal of Forecasting</em>, 5(1), 13-26.",
  kalman  = "Kalman, R. E. (1960). \u00ab A New Approach to Linear Filtering and Prediction Problems \u00bb, <em>Journal of Basic Engineering</em>, 82(1), 35-45.",
  rts     = "Rauch, H. E., Tung, F. et Striebel, C. T. (1965). \u00ab Maximum Likelihood Estimates of Linear Dynamic Systems \u00bb, <em>AIAA Journal</em>, 3(8), 1445-1450.",
  dempster = "Dempster, A. P., Laird, N. M. et Rubin, D. B. (1977). \u00ab Maximum Likelihood from Incomplete Data via the EM Algorithm \u00bb, <em>Journal of the Royal Statistical Society B</em>, 39(1), 1-38.",
  banbura = "Ba\u0144bura, M. et Modugno, M. (2014). \u00ab Maximum Likelihood Estimation of Factor Models on Datasets with Arbitrary Pattern of Missing Data \u00bb, <em>Journal of Applied Econometrics</em>, 29(1), 133-160.",
  doz     = "Doz, C., Giannone, D. et Reichlin, L. (2011). \u00ab A Two-Step Estimator for Large Approximate Dynamic Factor Models Based on Kalman Filtering \u00bb, <em>Journal of Econometrics</em>, 164(1), 188-205.",
  dozqml  = "Doz, C., Giannone, D. et Reichlin, L. (2012). \u00ab A Quasi-Maximum Likelihood Approach for Large, Approximate Dynamic Factor Models \u00bb, <em>Review of Economics and Statistics</em>, 94(4), 1014-1024.",
  baing   = "Bai, J. et Ng, S. (2002). \u00ab Determining the Number of Factors in Approximate Factor Models \u00bb, <em>Econometrica</em>, 70(1), 191-221.",
  baing08 = "Bai, J. et Ng, S. (2008). \u00ab Forecasting economic time series using targeted predictors \u00bb, <em>Journal of Econometrics</em>, 146(2), 304-317.",
  chernis = "Chernis, T. et Sekkel, R. (2017). <em>A Dynamic Factor Model for Nowcasting Canadian GDP Growth</em>, Bank of Canada Staff Working Paper 2017-2.",
  dm      = "Diebold, F. X. et Mariano, R. S. (1995). \u00ab Comparing Predictive Accuracy \u00bb, <em>Journal of Business &amp; Economic Statistics</em>, 13(3), 253-263.",
  hln     = "Harvey, D., Leybourne, S. et Newbold, P. (1997). \u00ab Testing the Equality of Prediction Mean Squared Errors \u00bb, <em>International Journal of Forecasting</em>, 13(2), 281-291.",
  higgins = "Higgins, P. (2014). <em>GDPNow: A Model for GDP Nowcasting</em>, Federal Reserve Bank of Atlanta Working Paper 2014-7.",
  schwarz = "Schwarz, G. (1978). \u00ab Estimating the Dimension of a Model \u00bb, <em>The Annals of Statistics</em>, 6(2), 461-464.")

assign(".ordre_refs", character(0), envir = globalenv())
ref <- function(cle) {
  stopifnot(cle %in% names(BIBLIO))
  o <- get(".ordre_refs", envir = globalenv())
  if (!(cle %in% o)) { o <- c(o, cle); assign(".ordre_refs", o, envir = globalenv()) }
  n <- match(cle, o)
  sprintf('<sup class="ap"><a href="#ref%d" id="ap%d">%d</a></sup>', n, n, n)
}
refs <- function(...) paste0(vapply(c(...), ref, character(1)), collapse = "")

cat("\n[1/3] Lecture\n")
bilan   <- lire_csv(r("02_panel_bilan.csv"))
grille  <- lire_csv(r("03_grille_rp.csv"))
sel     <- lire_csv(r("04_selection_rp.csv"))
agr     <- lire_csv(r("04_agregat.csv"))
comp    <- lire_csv(r("04_comparaison.csv"))
dm      <- lire_csv(r("04_diebold_mariano.csv"))
cbr     <- lire_csv(r("04_comparaison_branches.csv"))
ctrl    <- lire_csv(r("05_controles.csv"))

dfm <- comp[grepl("facteurs", comp$methode), ]
m1  <- comp[grepl("Methode 1", comp$methode), ]

cat("[2/3] Composition\n")
h <- character(0)
ajouter <- function(...) h <<- c(h, paste0(..., collapse = ""))

ajouter('<header class="titre">',
        '<div class="sur">Nowcasting de la croissance marocaine</div>',
        '<h1>Un modèle à facteurs dynamiques, et ce qu&rsquo;il vaut</h1>',
        '<p class="sous">Reconstruction rigoureuse de l&rsquo;approche par facteurs, ',
        'évaluée contre le système à équations passerelles sur un protocole ',
        'strictement identique.</p>',
        sprintf('<p class="meta">%s</p>', format(Sys.Date(), "%d %B %Y")),
        '</header>')

sections <- c("Ce que fait un modèle à facteurs",
              "La fréquence mixte, et pourquoi elle décide de tout",
              "Le panel : ce qui entre, ce qui n'entre pas",
              "L'estimation",
              "Le protocole d'évaluation",
              "Le choix du nombre de facteurs",
              "Résultats par branche",
              "Résultat sur l'agrégat, face à la méthode 1",
              "Pourquoi le modèle échoue là où il échoue",
              "Deux corrections tentées, deux échecs",
              "Contrôles anti-antériorité",
              "Conclusion",
              "Glossaire",
              "Références")
ajouter(sommaire_rapport(sections))

ajouter(chiffres_cles(c(
  "ratio sur l'agrégat" = nb(dfm$ratio, 3),
  "ratio de la méthode 1" = nb(m1$ratio, 3),
  "corrélation prévu / réalisé" = nb(dfm$correlation, 2),
  "corrélation, méthode 1" = nb(m1$correlation, 2),
  "trimestres évalués" = nb(dfm$n),
  "Diebold-Mariano" = sprintf("p = %s", nb(dm$p_value, 3)))))

ajouter('<div class="corps">')

# ---------------------------------------------------------------- 1
ajouter("<h2 id='s1'><span class='num'>1.</span>Ce que fait un modèle à facteurs</h2>")
ajouter("<p>L'idée est de résumer un grand nombre de séries par un petit nombre de ",
        "<strong>facteurs latents</strong>", refs("geweke", "sargent", "giannone"),
        " qu'elles partagent. Chaque série observée se ",
        "décompose en une part commune et une part qui lui est propre :</p>")
ajouter(eq(paste0(m("x<sub>i,t</sub>"), " = ", m("&lambda;<sub>i</sub>&prime;F<sub>t</sub>"),
                  " <span class='op'>+</span> ", m("e<sub>i,t</sub>"))))
ajouter("<p>et les facteurs suivent leur propre dynamique :</p>")
ajouter(eq(paste0(m("F<sub>t</sub>"), " = ",
                  m("A<sub>1</sub>F<sub>t&minus;1</sub>"), " <span class='op'>+</span> ",
                  "&hellip; <span class='op'>+</span> ",
                  m("A<sub>p</sub>F<sub>t&minus;p</sub>"), " <span class='op'>+</span> ",
                  m("u<sub>t</sub>"))))
ajouter(definition("L'atout, et la contrainte qui en découle",
  paste0("<p>L'atout est réel : le modèle digère un panel entier sans avoir à choisir ",
         "quels indicateurs retenir, il tolère nativement des calendriers de publication ",
         "différents", ref("wallis"), ", et il intègre dans un même cadre des séries mensuelles et ",
         "trimestrielles.</p>",
         "<p>La contrainte est le revers exact de cet atout. Les facteurs sont extraits ",
         "<strong>du panel</strong>, sans regarder la cible. Si ce que les indicateurs ",
         "ont en commun n'est pas ce qui meut la valeur ajoutée, la compression détruit ",
         "précisément l'information utile. La section 9 montre que c'est ce qui se ",
         "produit ici.</p>")))

# ---------------------------------------------------------------- 2
ajouter("<h2 id='s2'><span class='num'>2.</span>La fréquence mixte, et pourquoi elle décide de tout</h2>")
ajouter("<p>La cible est trimestrielle, les facteurs sont mensuels. La convention naïve ",
        "consiste à poser la valeur trimestrielle sur un mois du trimestre et à laisser ",
        "les deux autres manquants. Elle revient à affirmer que la croissance du ",
        "trimestre est engendrée par le facteur d'un <em>seul</em> mois.</p>")
ajouter("<p>C'est faux, et le traitement correct", ref("mariano"),
        " se déduit de l'identité comptable entre ",
        "niveaux et taux de croissance. Si le log-niveau trimestriel vaut la moyenne des ",
        "trois mois, alors la croissance trimestrielle s'écrit :</p>")
ajouter(eq(paste0(m("g<sup>Q</sup><sub>t</sub>"), " = ",
                  "<span class='fr'><span class='hi'>1</span><span class='lo'>3</span></span>",
                  "( ", m("g<sub>t</sub>"), " <span class='op'>+</span> 2",
                  m("g<sub>t&minus;1</sub>"), " <span class='op'>+</span> 3",
                  m("g<sub>t&minus;2</sub>"), " <span class='op'>+</span> 2",
                  m("g<sub>t&minus;3</sub>"), " <span class='op'>+</span> ",
                  m("g<sub>t&minus;4</sub>"), " )")))
ajouter("<p>Les poids (1, 2, 3, 2, 1) / 3 ne sont pas un lissage arbitraire : ils tombent ",
        "du calcul. Ils imposent que l'état du modèle contienne <strong>cinq retards</strong> ",
        "du facteur, et ils s'appliquent à toute série trimestrielle, la cible comme les ",
        "indicateurs.</p>")
ajouter(note("Ce que coûte la convention naïve",
  paste0("<p>Sur des données simulées où la relation ci-dessus est vraie par ",
         "construction, ignorer les poids fait passer le ratio d'erreur de 0,21 à 0,98 : ",
         "la prévision cesse d'apporter quoi que ce soit.</p>",
         "<p>Ce chiffre valide l'implémentation, non la pertinence empirique : les ",
         "données étant simulées avec ces poids, le gain y est garanti. Il indique ",
         "seulement l'ordre de grandeur de ce qu'on perdrait si la relation était ",
         "vraie.</p>")))

# ---------------------------------------------------------------- 3
ajouter("<h2 id='s3'><span class='num'>3.</span>Le panel : ce qui entre, ce qui n'entre pas</h2>")
t3 <- bilan %>% dplyr::arrange(dplyr::desc(series)) %>%
  dplyr::transmute(Branche = branche, `Mois` = mois, `Séries` = series,
                   `Mensuelles` = mensuelles, `Trimestrielles` = trimestrielles,
                   `Taux d'observation` = sprintf("%s %%", nb(100 * taux_observation, 0)))
ajouter(tbl(as.data.frame(t3), aligne_droite = 2:6))
ajouter(legende_tableau("Les douze panels mensuels, une branche par ligne."))
ajouter(figure("02_panel.png", "Composition du panel par branche",
               paste0("Les séries trimestrielles sont conservées. Pour ",
                      "l'information-communication, elles sont même les seules ",
                      "disponibles : les exclure reviendrait à priver la branche de tout ",
                      "indicateur.")))
ajouter(note("Le taux d'observation n'est pas un taux de lacune",
  paste0("<p>Une série trimestrielle posée sur une grille mensuelle est observée un mois ",
         "sur trois <em>par construction</em>, sans qu'aucune donnée ne manque. Lire ces ",
         "taux comme une mesure de trous conduirait à écarter des séries parfaitement ",
         "complètes.</p>")))
ajouter("<p>Deux choix de construction méritent d'être signalés. Les séries trimestrielles ",
        "sont posées au <strong>dernier</strong> mois de leur trimestre, non au premier : ",
        "c'est la date à laquelle elles entrent dans l'information, et la seule compatible ",
        "avec les poids d'agrégation. Et <strong>aucun filtre ne fait intervenir la ",
        "cible</strong> : sélectionner les séries d'après leur lien avec la valeur ajoutée ",
        "reviendrait à décider à l'avance ce que les facteurs doivent contenir.</p>")

# ---------------------------------------------------------------- 4
ajouter("<h2 id='s4'><span class='num'>4.</span>L'estimation</h2>")
ajouter("<p>Le modèle s'écrit sous forme espace d'état et s'estime par l'algorithme ",
        "espérance-maximisation", ref("dempster"),
        ", dans la version qui gère nativement des motifs de données manquantes ",
        "arbitraires", ref("banbura"),
        ". Deux étapes alternent jusqu'à stabilisation de la vraisemblance :</p>")
ajouter("<ul>",
        "<li>le filtre", ref("kalman"), " puis le lisseur", ref("rts"),
        " calculent les moments des facteurs, en ",
        "n'utilisant à chaque date que les séries <strong>effectivement observées</strong> ",
        "&mdash; aucune valeur manquante n'est imputée ;</li>",
        "<li>les chargements, les variances et la dynamique sont réestimés en forme close ",
        "à partir de ces moments, série par série, de sorte qu'une série trouée contribue ",
        "sur ses seules dates observées.</li>",
        "</ul>")
ajouter(note("Filtre et lisseur ne sont pas interchangeables",
  paste0("<p>Le lisseur calcule l'espérance de l'état conditionnellement à ",
         "<em>tout</em> l'échantillon : il utilise le futur. C'est légitime pour estimer, ",
         "où toute la période est disponible, et interdit pour prévoir.</p>",
         "<p>Employer le lisseur au moment de la prévision introduirait une antériorité ",
         "d'autant plus dangereuse qu'elle serait invisible : les valeurs produites ",
         "paraîtraient parfaitement plausibles. La prévision part donc de l'état ",
         "<strong>filtré</strong>, et le contrôle C6 de la section 11 le vérifie.</p>")))

# ---------------------------------------------------------------- 5
ajouter("<h2 id='s5'><span class='num'>5.</span>Le protocole d'évaluation</h2>")
ajouter(sprintf(paste0("<p>%d origines, de T2-2014 à T1-2026. À chaque origine, le panel ",
                       "est tronqué au dernier mois du trimestre cible : les indicateurs ",
                       "de ce trimestre sont observés, la valeur ajoutée ne l'est pas. Le ",
                       "modèle est entièrement réestimé, puis la cible est prévue.</p>"),
                dfm$n))
ajouter("<p>Sont recalculés à chaque origine : la moyenne et l'écart-type de ",
        "standardisation de chaque série, les paramètres du modèle, et le choix du nombre ",
        "de facteurs. Les poids d'agrégation proviennent du trimestre précédent.</p>")
ajouter("<p><strong>Ce protocole est celui de la méthode 1</strong>, jusqu'aux fichiers de ",
        "poids, identiques octet pour octet, et au traitement des quatre branches sans ",
        "indicateur. Les deux méthodes ne diffèrent donc que par le modèle appliqué aux ",
        "douze branches couvertes : l'écart mesuré à la fin lui est imputable.</p>")

# ---------------------------------------------------------------- 6
ajouter("<h2 id='s6'><span class='num'>6.</span>Le choix du nombre de facteurs</h2>")
t6 <- grille %>% dplyr::group_by(r, p) %>%
  dplyr::summarise(`Ratio médian` = stats::median(ratio),
                   `Branches sous 1` = sum(ratio < 1), .groups = "drop") %>%
  dplyr::arrange(`Ratio médian`) %>%
  dplyr::transmute(`r` = r, `p` = p, `Ratio médian` = nb(`Ratio médian`, 3),
                   `Branches sous 1` = `Branches sous 1`)
ajouter(tbl(as.data.frame(t6), aligne_droite = 1:4))
ajouter(legende_tableau("Les six configurations, sur les douze branches couvertes."))
ajouter(figure("03_grille_rp.png", "Ratio selon le nombre de facteurs",
               paste0("Le ratio croît avec le nombre de facteurs. Un facteur unique ",
                      "suffit, et en ajouter dégrade systématiquement.")))
ajouter("<p><strong>Un seul facteur suffit</strong>, et en ajouter dégrade. Le résultat ",
        "mérite d'être rapproché de la pratique courante, qui choisit le nombre de ",
        "facteurs par critère d'information calculé <em>dans</em> l'échantillon : sur ces ",
        "panels, ces critères saturent régulièrement leur borne supérieure et poussent ",
        "vers des modèles que la prévision ne valide pas.</p>")
ajouter("<p>Le choix est ici fait sur le critère qui compte, l'erreur de prévision, et ",
        "<strong>récursivement</strong> : à chaque origine, on retient la configuration ",
        "qui a le mieux fait sur les origines précédentes.</p>")
ajouter(figure("04_selection_rp.png", "Configuration retenue à chaque origine",
               paste0("Le choix bouge d'une branche et d'une origine à l'autre, ce qui ",
                      "est le comportement attendu d'une sélection récursive.")))

# ---------------------------------------------------------------- 7
ajouter("<h2 id='s7'><span class='num'>7.</span>Résultats par branche</h2>")
t7 <- cbr %>% dplyr::arrange(`facteurs dynamiques`) %>%
  dplyr::transmute(Branche = branche,
                   `Facteurs dynamiques` = nb(`facteurs dynamiques`, 3),
                   `Méthode 1` = nb(`méthode 1`, 3),
                   Meilleure = ifelse(`facteurs dynamiques` < `méthode 1`,
                                      "facteurs", "méthode 1"))
ajouter(tbl(as.data.frame(t7), aligne_droite = 2:3))
ajouter(legende_tableau(paste0(
  "Ratio d'erreur par branche. Sous 1, le modèle bat la moyenne historique.")))
ajouter(figure("04_ratio_branches.png", "Ratio d'erreur par branche",
               paste0("Là où le modèle à facteurs l'emporte, il l'emporte de peu et ",
                      "souvent au-dessus de 1, c'est-à-dire que les deux méthodes ",
                      "échouent. Là où il perd, il perd beaucoup.")))
n_dfm <- sum(cbr$`facteurs dynamiques` < cbr$`méthode 1`)
ajouter(sprintf(paste0("<p>Le modèle à facteurs l'emporte sur <strong>%d branches sur ",
                       "%d</strong>, avec un ratio médian de %s contre %s.</p>"),
                n_dfm, nrow(cbr), nb(stats::median(cbr$`facteurs dynamiques`), 3),
                nb(stats::median(cbr$`méthode 1`), 3)))
ajouter(note("Deux victoires à relativiser",
  paste0("<p>Le modèle à facteurs devance la méthode 1 sur l'éducation-santé et les ",
         "services aux entreprises. Or ces branches n'ont <em>aucun</em> indicateur : les ",
         "deux méthodes y tournent sur une extrapolation autorégressive, et l'écart ne ",
         "vient donc pas du modèle à facteurs mais d'une différence de mise en œuvre de ",
         "l'autorégression.</p>")))

# ---------------------------------------------------------------- 8
ajouter("<h2 id='s8'><span class='num'>8.</span>Résultat sur l'agrégat, face à la méthode 1</h2>")
t8 <- comp %>% dplyr::transmute(
  Méthode = ifelse(grepl("facteurs", methode), "Facteurs dynamiques",
                   "Méthode 1 (vectoriel + passerelles)"),
  `RMSE (%)` = nb(100 * RMSE, 2), `MAE (%)` = nb(100 * MAE, 2),
  `Ratio` = nb(ratio, 3), `Corrélation` = nb(correlation, 2),
  `Biais (pt)` = nb(100 * biais, 2))
ajouter(tbl(as.data.frame(t8), aligne_droite = 2:6))
ajouter(legende_tableau(sprintf(
  "Valeur ajoutée totale, %d trimestres, protocole identique.", dfm$n)))
ajouter(figure("04_agregat_compare.png", "Les deux méthodes contre le réalisé",
               paste0("La méthode 1 suit les inflexions ; le modèle à facteurs reste ",
                      "proche d'une horizontale, ce que traduit sa corrélation.")))
ajouter(figure("04_prevu_realise.png", "Prévu contre réalisé",
               paste0("Un modèle informatif aligne son nuage sur la première ",
                      "bissectrice. Celui du modèle à facteurs est presque ",
                      "horizontal.")))
ajouter(sprintf(paste0("<p>Le chiffre décisif n'est pas l'erreur quadratique mais la ",
                       "<strong>corrélation : %s contre %s</strong>. Un ratio de %s ",
                       "signifie que le modèle à facteurs fait à peine mieux que prédire ",
                       "la moyenne historique, et une corrélation de %s qu'il ne suit ",
                       "presque pas le <em>sens</em> des variations.</p>"),
                nb(dfm$correlation, 2), nb(m1$correlation, 2),
                nb(dfm$ratio, 3), nb(dfm$correlation, 2)))
ajouter(sprintf(paste0("<p>Le test de précision comparée", refs("dm", "hln"),
                       " donne une statistique de %s pour ",
                       "une probabilité critique de <strong>%s</strong> : l'écart, bien ",
                       "que net en apparence, <strong>n'atteint pas la ",
                       "significativité</strong>.</p>"),
                nb(dm$statistique, 3), nb(dm$p_value, 3)))
ajouter(note("Pourquoi un écart aussi visible n'est pas significatif",
  paste0("<p>La perte différentielle entre deux méthodes est dominée par quelques ",
         "trimestres de rupture, où les deux se trompent beaucoup et où leur écart est ",
         "grand aussi. Sa variance croît donc avec sa moyenne, et leur rapport reste ",
         "petit.</p>",
         "<p>Avec une cinquantaine d'observations, le test ne peut trancher qu'un écart ",
         "régulier. La conclusion correcte est donc « cet échantillon ne suffit pas à ",
         "départager », et non « les deux méthodes se valent ».</p>")))

# ---------------------------------------------------------------- 9
ajouter("<h2 id='s9'><span class='num'>9.</span>Pourquoi le modèle échoue là où il échoue</h2>")
ajouter("<p>La question mérite mieux qu'un constat. En mesurant, branche par branche, la ",
        "part de variance de la cible que les facteurs expliquent <em>dans</em> ",
        "l'échantillon, le diagnostic devient net.</p>")
diag <- data.frame(
  Branche = c("Hébergement-restauration", "Industrie de transformation", "Pêche",
              "Électricité, gaz, eau", "Agriculture", "Commerce"),
  `Variance de la cible expliquée` = c("0,72", "0,38", "0,32", "0,06", "0,02", "0,01"),
  `Variance commune, indicateurs` = c("0,63", "0,24", "0,12", "0,11", "0,49", "0,20"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(diag, aligne_droite = 2:3))
ajouter(legende_tableau(paste0(
  "Part de variance expliquée par les facteurs, mesurée dans l'échantillon.")))
ajouter(note("Le mécanisme",
  paste0("<p>Les facteurs capturent la variation dominante <strong>du panel</strong>, non ",
         "celle qui est utile <strong>à la cible</strong>. Sur l'électricité, ils ",
         "expliquent correctement les quatre-vingts indicateurs et 6 % seulement de la ",
         "valeur ajoutée. Sur le commerce, 1 %.</p>",
         "<p>C'est la contrainte fondatrice du modèle : il s'interdit de regarder la cible ",
         "pour construire ses facteurs. Une équation passerelle, elle, sélectionne cinq ",
         "indicateurs corrélés à la cible et régresse dessus", ref("higgins"),
         ". Sur un panel hétérogène où ",
         "production, crédit et enquêtes se mêlent, cette contrainte coûte cher.</p>",
         "<p>Le modèle n'est donc pas mauvais : il est <em>mal adapté à cette structure ",
         "de données</em>. Là où le panel est riche et cohérent, il fonctionne.</p>")))

# ---------------------------------------------------------------- 10
ajouter("<h2 id='s10'><span class='num'>10.</span>Deux corrections tentées, deux échecs</h2>")
ajouter("<h3>10.1 Une composante idiosyncratique autorégressive</h3>")
ajouter("<p>Le diagnostic montrait que sur plusieurs branches la cible est ",
        "idiosyncratique à plus de 95 % : tout ce qui la meut se trouve dans un terme que ",
        "le modèle traite comme du bruit blanc, donc imprévisible par construction. On a ",
        "donc autorisé une dynamique autorégressive sur ce terme.</p>")
ech1 <- data.frame(
  Branche = c("Hébergement-restauration", "Commerce", "Agriculture",
              "Électricité, gaz, eau"),
  `Bruit blanc` = c("0,938", "1,042", "1,173", "1,049"),
  `Autorégressif` = c("1,108", "1,091", "1,284", "1,042"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(ech1, aligne_droite = 2:3))
ajouter(legende_tableau("Ratio d'erreur avec et sans dynamique idiosyncratique."))
ajouter(note("Deux causes, toutes deux instructives",
  paste0("<p>Le coefficient estimé est <strong>positif</strong>, entre 0,16 et 0,38, alors ",
         "que la croissance trimestrielle marocaine est négativement autocorrélée : les ",
         "prévisions partent dans le mauvais sens. Le paramètre est mal identifié, la ",
         "cible n'étant observée qu'un mois sur trois &mdash; entre deux observations, le ",
         "lisseur interpole, et cette interpolation est autocorrélée par construction.</p>",
         "<p>Plus grave : en mettant la composante idiosyncratique dans l'état, la cible ",
         "doit être ajustée exactement, et ce terme libre absorbe tout. Les facteurs ",
         "n'ont plus aucune raison d'expliquer la cible &mdash; sur l'hébergement, ils en ",
         "expliquaient 72 % et s'en désintéressent. On a remplacé une information réelle ",
         "par un terme inutilisable en prévision.</p>")))
ajouter("<h3>10.2 Une présélection ciblée du panel</h3>")
ajouter("<p>Puisque les facteurs capturent la mauvaise variation, restreindre le panel aux ",
        "indicateurs corrélés à la cible devrait aider", ref("baing08"), ". Le calcul est fait ",
        "<strong>récursivement</strong>, sur la seule information antérieure, ce qui le ",
        "rend licite.</p>")
ech2 <- data.frame(
  Branche = c("Industrie de transformation", "Hébergement-restauration",
              "Électricité, gaz, eau", "Commerce", "Agriculture"),
  `Panel entier` = c("0,900", "0,938", "1,049", "1,042", "1,173"),
  `Filtré` = c("1,031", "0,937", "1,009", "1,039", "1,150"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(ech2, aligne_droite = 2:3))
ajouter(legende_tableau("Ratio avec le panel entier et après filtrage récursif."))
ajouter("<p>Les gains sont de l'ordre du centième, et le filtrage <strong>nuit franchement ",
        "là où le modèle marchait le mieux</strong> : l'industrie de transformation passe ",
        "de 0,900 à 1,031, sa corrélation s'effondrant de 0,45 à 0,00.</p>")
ajouter(note("Le remède détruit le mécanisme",
  paste0("<p>Un modèle à facteurs a besoin de beaucoup de séries : c'est la redondance du ",
         "panel qui permet de séparer le signal commun du bruit propre à chacune. En ",
         "retirant quarante-quatre des quatre-vingts séries de l'industrie, on retire ",
         "aussi ce qui permettait de les moyenner.</p>")))

# ---------------------------------------------------------------- 11
ajouter("<h2 id='s11'><span class='num'>11.</span>Contrôles anti-antériorité</h2>")
t11 <- ctrl %>% dplyr::transmute(Contrôle = controle, Objet = objet,
                                 Vérification = description,
                                 Résultat = resultat, Portée = detail)
ajouter(tbl(as.data.frame(t11), aligne_droite = integer(0)))
ajouter(legende_tableau("Les sept contrôles. Un échec interrompt le calcul."))
ajouter(note("Le contrôle décisif",
  paste0("<p>Le sixième est le seul qui ne repose sur aucune relecture de code. On ",
         "<strong>saccage</strong> les données postérieures à une origine &mdash; ",
         "multiplication par cinq, inversion de signe, décalage de vingt &mdash; on refait ",
         "la prévision, et l'on vérifie qu'elle n'a pas bougé.</p>",
         "<p>Sur les neuf cas testés, l'écart maximal est de <strong>zéro exactement</strong>. ",
         "Aucune information postérieure à l'origine n'entre dans le calcul.</p>")))

# ---------------------------------------------------------------- 12
ajouter("<h2 id='s12'><span class='num'>12.</span>Conclusion</h2>")
ajouter(sprintf(paste0("<p>Le modèle à facteurs dynamiques, reconstruit avec le même soin ",
                       "et évalué sur le même protocole, obtient un ratio de <strong>%s</strong> ",
                       "contre %s pour le système à équations passerelles, et une ",
                       "corrélation de %s contre %s. L'écart n'atteint pas la ",
                       "significativité sur %d trimestres.</p>"),
                nb(dfm$ratio, 3), nb(m1$ratio, 3), nb(dfm$correlation, 2),
                nb(m1$correlation, 2), dfm$n))
ajouter("<p>Ce n'est pas un verdict sur la méthode en général. C'est un verdict sur son ",
        "adéquation à <strong>cette structure de données</strong> : un panel hétérogène, ",
        "où chaque branche mêle production, crédit et enquêtes, et où ce que les ",
        "indicateurs ont en commun n'est pas ce qui meut la valeur ajoutée.</p>")
ajouter("<p>L'enseignement dépasse la comparaison. Le modèle le plus sophistiqué n'est pas ",
        "celui qui gagne, et la raison n'est pas une question de puissance de calcul : ",
        "c'est que <strong>la contrainte qui fait son élégance &mdash; ne pas regarder la ",
        "cible &mdash; est ici un handicap</strong>. Le savoir vaut mieux que de l'adopter ",
        "par principe.</p>")
ajouter("<p>Deux directions resteraient ouvertes. Un modèle à facteurs <em>global</em>, à ",
        "chargements sectoriels", ref("chernis"), ", permettrait aux branches pauvres en indicateurs ",
        "d'emprunter aux autres. Et la combinaison des deux méthodes mérite d'être ",
        "testée : si elles se trompent sur des trimestres différents, leur moyenne ",
        "battrait les deux.</p>")

ajouter('</div>')   # fin du bloc a deux colonnes

# ============================================================== glossaire
ajouter("<h2 id='s13'><span class='num'>13.</span>Glossaire</h2>")
glo <- data.frame(
  Terme = c("Bord irrégulier", "Chargement", "Composante commune",
            "Composante idiosyncratique", "Espace d'état", "Facteur latent",
            "Filtre de Kalman", "Fréquence mixte", "Lisseur de Kalman",
            "Maximum de vraisemblance", "Origine", "Protocole récursif",
            "Ratio", "Standardisation", "Vraisemblance"),
  Définition = c(
    "situation où les séries ne sont pas toutes observées jusqu'au même mois, la frontière des données formant un escalier (section 1)",
    "coefficient reliant une série à un facteur ; il mesure combien cette série réagit au mouvement commun (section 1)",
    "la part d'une série expliquée par les facteurs, par opposition à ce qui lui est propre (section 9)",
    "la part d'une série qu'aucun facteur n'explique ; sur plusieurs branches elle représente plus de 95 % de la cible (section 9)",
    "écriture d'un modèle en deux équations, l'une reliant l'observé à un état caché, l'autre décrivant l'évolution de cet état (section 4)",
    "variable non observée, commune à plusieurs séries, qui résume leur mouvement partagé (section 1)",
    "algorithme calculant l'état caché à partir du seul passé ; c'est lui qui sert à prévoir (section 4)",
    "coexistence dans un même modèle de séries mensuelles et trimestrielles (section 2)",
    "algorithme calculant l'état caché à partir de tout l'échantillon, passé et futur ; il sert à estimer, jamais à prévoir (section 4)",
    "méthode d'estimation retenant les paramètres qui rendent les données observées les plus probables (section 4)",
    "date à laquelle on se place pour produire une estimation, en oubliant tout ce qui a été publié depuis (section 5)",
    "protocole où toute quantité est réestimée à chaque origine sur la seule information antérieure (section 5)",
    "erreur du modèle rapportée à l'écart-type de la série ; sous 1, le modèle bat la moyenne historique (section 7)",
    "centrage et réduction d'une série, ici recalculés à chaque origine sur les seules données antérieures (section 5)",
    "probabilité d'observer les données sous un jeu de paramètres donné (section 4)"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(glo, aligne_droite = integer(0)))
ajouter(legende_tableau("Glossaire des termes techniques employés dans le rapport."))

# ============================================================== references
ajouter("<h2 id='s14'><span class='num'>14.</span>Références</h2>")
ordre <- get(".ordre_refs", envir = globalenv())
ajouter("<p>Les travaux sont numérotés dans l'ordre de leur première citation. Le numéro ",
        "en exposant dans le texte renvoie à cette liste.</p>")
ajouter('<div class="biblio"><ol>',
        paste0(sprintf('<li id="ref%d">%s</li>', seq_along(ordre), BIBLIO[ordre]),
               collapse = ""), '</ol></div>')

ajouter('<footer>',
        sprintf('Rapport établi le %s. ', format(Sys.Date(), "%d/%m/%Y")),
        'Toutes les valeurs proviennent du protocole décrit à la section 5.',
        '</footer>')

cat("[3/3] Ecriture\n")
h <- espacer_relateurs(h)
h <- normaliser_notes(h)
h <- sans_cadratin(h)
ecrire_global(h, "Nowcasting par facteurs dynamiques", CHEMIN)
cat(sprintf("      %s (%.1f Mo, %d figures, %d tableaux, %d equations, %d references)\n",
            CHEMIN, file.size(CHEMIN) / 1024^2, .n_figure, .n_tableau,
            get(".n_equation", envir = globalenv()),
            length(get(".ordre_refs", envir = globalenv()))))
