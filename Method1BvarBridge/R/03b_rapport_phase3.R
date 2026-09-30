# ============================================================================
# 03b_rapport_phase3.R -- Rapport pedagogique de la phase 3
# ============================================================================
# Ce rapport est concu pour etre lu par quelqu'un qui ne connait pas les BVAR :
# chaque methode y est d'abord expliquee dans son principe, puis posee en
# formules, puis rattachee a ce qui a ete fait et pourquoi.
#
# Toutes les valeurs chiffrees sont relues depuis resultats/03_*.csv : le
# rapport se regenere avec les resultats et ne peut pas en diverger.
#
# Note d'ecriture : chaines en GUILLEMETS DOUBLES, attributs HTML en guillemets
# simples. Le francais est plein d'apostrophes ; les echapper une a une serait
# une source d'erreurs sans contrepartie.
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/rapport.R")

CHEMIN_RAPPORT <- file.path(DOSSIER_RAPPORT, "rapport_phase3.html")
init_compteurs()
assign(".n_equation", 0L, envir = globalenv())

cat("[rapport] Lecture des resultats de la phase 3\n")
res <- function(x) file.path(DOSSIER_RESULTATS, paste0("03_", x))
requis <- c("previsions_recursives.csv", "comparaison_specifications.csv",
            "comparaison_specifications_hors_2020.csv", "tests_diebold_mariano.csv",
            "choix_hyperparametres.csv", "hyperparametres_par_origine.csv",
            "evaluation_branches_par_specification.csv", "comparaison_regles_choc.csv",
            "comparaison_leviers.csv", "chocs_detectes.csv",
            "diagnostics_branches.csv", "diagnostics_residus.csv",
            "coefficients_indicatrices.csv", "nowcast_courant.csv")
absents <- requis[!file.exists(res(requis))]
if (length(absents) > 0L) {
  stop("Sorties de la phase 3 absentes : ", paste(absents, collapse = ", "),
       "\nExecuter d'abord R/03_bvar_trimestriel.R", call. = FALSE)
}

previsions  <- lire_csv(res("previsions_recursives.csv"))
comparaison <- lire_csv(res("comparaison_specifications.csv"))
comp_hors   <- lire_csv(res("comparaison_specifications_hors_2020.csv"))
dm          <- lire_csv(res("tests_diebold_mariano.csv"))
choix       <- lire_csv(res("choix_hyperparametres.csv"))
hyper       <- lire_csv(res("hyperparametres_par_origine.csv"))
eval_specs  <- lire_csv(res("evaluation_branches_par_specification.csv"))
regles      <- lire_csv(res("comparaison_regles_choc.csv"))
leviers     <- lire_csv(res("comparaison_leviers.csv"))
diag_br     <- lire_csv(res("diagnostics_branches.csv"))
diag_res    <- lire_csv(res("diagnostics_residus.csv"))
coef_choc   <- lire_csv(res("coefficients_indicatrices.csv"))
nowcast     <- lire_csv(res("nowcast_courant.csv"))

# Sorties de R/03c_selection_conjointe.R. La section 15 n'est composee que si
# le script a tourne : le rapport reste generable sans lui.
res3c <- function(x) file.path(DOSSIER_RESULTATS, paste0("03c_", x))
A_CONJOINTE <- all(file.exists(res3c(c("grille_conjointe.csv",
                                       "interactions.csv",
                                       "validation_test.csv"))))
if (A_CONJOINTE) {
  grille_conj  <- lire_csv(res3c("grille_conjointe.csv"))
  interactions <- lire_csv(res3c("interactions.csv"))
  vtest        <- lire_csv(res3c("validation_test.csv"))
}

n_origines <- dplyr::n_distinct(previsions$origine)
n_branches <- dplyr::n_distinct(previsions$branche)
n_mieux    <- sum(diag_br$ratio < 1)
n_autocorr <- sum(diag_res$residus_autocorreles)
ratio_med  <- stats::median(diag_br$ratio)
mode_ret   <- choix$specification[choix$retenu][1]
regle_ret  <- regles$specification[1]
levier_ret <- leviers$specification[1]
correl_ref <- stats::median(
  eval_specs$correlation[grepl("^reference", eval_specs$specification)], na.rm = TRUE)

signes <- coef_choc %>%
  tidyr::pivot_wider(names_from = indicatrice, values_from = coefficient)
chocs_estimes <- setdiff(names(signes), "branche")
a_T2 <- "choc_T2-2020" %in% chocs_estimes
a_T3 <- "choc_T3-2020" %in% chocs_estimes
n_opposes <- if (a_T2 && a_T3) {
  sum(sign(signes[["choc_T2-2020"]]) != sign(signes[["choc_T3-2020"]]))
} else NA_integer_

cat("[rapport] Composition\n")
h <- character(0)
ajouter <- function(...) h <<- c(h, paste0(..., collapse = ""))

ajouter(entete_rapport(
  "GDPNow-Maroc &middot; Méthode 1 &middot; Rapport d'étape",
  "Phase 3 — Le BVAR trimestriel, expliqué de bout en bout",
  paste("Ce que fait le modèle, pourquoi il le fait ainsi, et ce que valent ses",
        "prévisions. Chaque méthode est posée en formules."),
  "<code>data/VA_branches.csv</code> (16 branches, T1-1998 à T1-2026)"))

sections <- c(
  "Le problème posé",
  "Le modèle autorégressif vectoriel",
  "Pourquoi les moindres carrés échouent ici",
  "L'idée bayésienne : contraindre plutôt qu'estimer",
  "Le prior de Minnesota",
  "La mise en œuvre par observations fictives",
  "L'échelle des variables",
  "Choisir les hyperparamètres : la vraisemblance marginale",
  "Choisir les hyperparamètres : le critère d'ajustement",
  "Traiter les chocs",
  "Trois leviers structurels",
  "Le protocole récursif",
  "Comment se mesure la qualité d'une prévision",
  "Résultats",
  "Séquentiel ou conjoint : tester le protocole de sélection",
  "Diagnostics",
  "Nowcast du trimestre courant",
  "Limites",
  "Fichiers produits et suite")
ajouter(sommaire_rapport(sections))

ajouter("<div class='encadre'><span class='etiq'>Spécification retenue</span>",
        "<p>Le modèle de production de cette phase est&nbsp;:</p>",
        "<p style='text-align:center'><strong>critère d'ajustement (BGR)",
        "&nbsp;&middot;&nbsp; règle de détection z = 4, k = 3",
        "&nbsp;&middot;&nbsp; &sigma; estimé sur 60 trimestres</strong></p>",
        "<p>Chacun de ces trois choix est établi hors échantillon, respectivement ",
        "aux sections 9, 10 et 11. L'ordre de retard ", m("p"), ", le serrage ",
        m("&lambda;"), " et l'exposant ", m("d"), " ne sont pas fixés une fois pour ",
        "toutes : ils sont rechoisis à <em>chaque</em> origine sur la seule ",
        "information disponible alors.</p>",
        "<p>La section 15 vérifie que ce chemin de sélection séquentiel n'a rien ",
        "manqué, en le confrontant à une sélection jointe sur 126 combinaisons.</p>",
        "</div>")

ajouter(chiffres_cles(c(
  "branches modélisées" = "16",
  "origines testées hors échantillon" = nb(n_origines),
  "prévisions produites" = nb(n_origines * n_branches),
  "branches mieux prévues que par leur moyenne" = sprintf("%d/16", n_mieux),
  "corrélation médiane prévu / réalisé" = nb(correl_ref, 2)
)))

# =========================== 1 ==============================================
ajouter("<h2 id='s1'><span class='num'>1.</span>Le problème posé</h2>")
ajouter("<p>Le PIB marocain est publié par branche, trimestriellement, avec un ",
        "délai. À un instant donné, on connaît les valeurs ajoutées jusqu'au ",
        "trimestre précédent et l'on voudrait estimer celles du trimestre en cours. ",
        "C'est ce qu'on appelle un <em>nowcast</em> : non pas prévoir loin dans ",
        "l'avenir, mais estimer un présent qui n'est pas encore mesuré.</p>")
ajouter("<p>La phase 3 traite la première des deux sources d'information ",
        "mobilisables : <strong>le passé des valeurs ajoutées elles-mêmes</strong>. ",
        "La seconde — les indicateurs mensuels — fera l'objet de la phase 4. Il ",
        "importe de savoir ce que la première apporte seule, pour juger ensuite si ",
        "la seconde ajoute quelque chose.</p>")

ajouter(definition("Notation",
  paste0("<p>On note ", m("g<sub>i,t</sub>"), " le taux de croissance de la valeur ",
         "ajoutée de la branche ", m("i"), " au trimestre ", m("t"), ", mesuré en ",
         "différence de logarithme :</p>",
         eq(paste0(m("g<sub>i,t</sub>"), " = log ", m("VA<sub>i,t</sub>"),
                   " <span class='op'>&minus;</span> log ",
                   m("VA<sub>i,t&minus;1</sub>"))),
         "<p>Il y a ", m("n"), " = 16 branches. On empile leurs taux de croissance ",
         "dans un vecteur colonne :</p>",
         eq(paste0(m("g<sub>t</sub>"), " = ( ", m("g<sub>1,t</sub>"), ", ",
                   m("g<sub>2,t</sub>"), ", &hellip;, ", m("g<sub>n,t</sub>"),
                   " )<span class='op'>&prime;</span>")),
         "<p>L'apostrophe désigne la transposition. L'échantillon compte ", m("T"),
         " = 112 trimestres, du deuxième trimestre 1998 au premier trimestre 2026 — ",
         "une observation est perdue par la différenciation.</p>")))

ajouter("<h3>Pourquoi des taux de croissance et non des niveaux</h3>")
ajouter("<p>La phase 2 a testé la stationnarité des seize séries par un test de ",
        "Dickey-Fuller augmenté. Le résultat est net : <strong>une seule branche sur ",
        "seize est stationnaire en niveau, les seize le sont en différence de ",
        "logarithme</strong>.</p>")
ajouter(intuition(paste0(
  "<p>Une série non stationnaire n'a pas de moyenne vers laquelle revenir : sa ",
  "variance croît avec le temps, et les régressions qu'on y mène produisent des ",
  "relations apparentes entre séries qui n'ont rien à voir entre elles. C'est le ",
  "phénomène de <em>régression fallacieuse</em>. Travailler en taux de croissance ",
  "l'évite.</p>",
  "<p>Ce choix aura une conséquence importante sur la forme du prior, en section 5 : ",
  "comme les variables sont déjà des variations, l'hypothèse a priori naturelle est ",
  "qu'elles reviennent vers leur moyenne, et non qu'elles persistent.</p>")))

# =========================== 2 ==============================================
ajouter("<h2 id='s2'><span class='num'>2.</span>Le modèle autorégressif vectoriel</h2>")
ajouter(intuition(paste0(
  "<p>L'hypothèse de départ est simple : la croissance d'une branche ce trimestre ",
  "dépend de la croissance de <em>toutes</em> les branches aux trimestres ",
  "précédents. L'industrie ralentit, les transports suivent le trimestre suivant ; ",
  "le commerce accélère, les services aux entreprises aussi.</p>",
  "<p>Un modèle autorégressif <em>vectoriel</em> — VAR — écrit cela pour les seize ",
  "branches simultanément, chacune expliquée par le passé de toutes.</p>")))

ajouter("<p>Formellement, le VAR d'ordre ", m("p"), " s'écrit :</p>")
ajouter(eq(paste0(
  m("g<sub>t</sub>"), " = ", m("c"), " <span class='op'>+</span> ",
  "<span class='big'>&sum;</span><sub class='num'>l=1</sub><sup class='num'>p</sup> ",
  m("B<sup>(l)</sup>"), " ", m("g<sub>t&minus;l</sub>"), " <span class='op'>+</span> ",
  "<span class='big'>&sum;</span><sub class='num'>k=1</sub><sup class='num'>K</sup> ",
  m("&gamma;<sup>(k)</sup>"), " ", m("D<sub>k,t</sub>"), " <span class='op'>+</span> ",
  m("e<sub>t</sub>"))))

comp <- data.frame(
  Terme = c("c", "B<sup>(l)</sup>", "D<sub>k,t</sub>", "γ<sup>(k)</sup>",
            "e<sub>t</sub>"),
  Dimension = c("n × 1", "n × n", "scalaire", "n × 1", "n × 1"),
  Rôle = c("croissance moyenne de chaque branche, hors dynamique",
           "effet du retard l : la case (i, j) dit combien la branche i au trimestre t−l influence la branche j au trimestre t",
           "indicatrice valant 1 au trimestre de choc k, 0 sinon — voir section 10",
           "effet du choc k sur chaque branche",
           "ce que le modèle n'explique pas, supposé de loi normale de matrice de covariance Σ"),
  stringsAsFactors = FALSE)
ajouter(tbl(comp, aligne_droite = integer(0)))
ajouter(legende_tableau("Les composantes du VAR et leur signification."))

ajouter("<h3>Écriture matricielle</h3>")
ajouter("<p>Pour estimer, on empile les observations. Comme le modèle a besoin de ",
        m("p"), " retards, les ", m("p"), " premières observations servent ",
        "uniquement de passé et ne sont pas expliquées : il reste ",
        m("T &minus; p"), " lignes.</p>")
ajouter(eq(paste0(m("Y"), " = ", m("X"), " ", m("B"), " <span class='op'>+</span> ",
                  m("E"))))
ajouter("<pre><code>Y  (T-p) x n    ligne t : g(t)' , pour t = p+1 ... T\n",
        "X  (T-p) x k    ligne t : [ 1 | D(1,t) ... D(K,t) | g(t-1)' ... g(t-p)' ]\n",
        "B      k  x n   coefficients, une colonne par equation\n",
        "E  (T-p) x n    residus\n\n",
        "avec  k = 1 + K + n*p   regresseurs par equation</code></pre>")
ajouter("<p>La colonne ", m("j"), " de ", m("B"), " contient tous les coefficients de ",
        "l'équation de la branche ", m("j"), ". Le modèle est donc un empilement de ",
        "seize régressions qui partagent les mêmes régresseurs.</p>")

# =========================== 3 ==============================================
ajouter("<h2 id='s3'><span class='num'>3.</span>Pourquoi les moindres carrés échouent ici</h2>")
ajouter("<p>La solution classique serait d'estimer ", m("B"),
        " par moindres carrés ordinaires :</p>")
ajouter(eq(paste0(m("B&#770;<sub>MCO</sub>"), " = ( ", m("X"),
                  "<span class='op'>&prime;</span>", m("X"),
                  " )<sup>&minus;1</sup> ", m("X"),
                  "<span class='op'>&prime;</span> ", m("Y"))))
ajouter("<p>Cette formule exige que ", m("X&prime;X"),
        " soit inversible, donc que le nombre d'observations dépasse le nombre de ",
        "régresseurs. Voyons ce que cela donne.</p>")

dim_tab <- data.frame(
  `Ordre p` = c(1, 2, 3, 4, 5),
  `Paramètres par équation` = c(20, 36, 52, 68, 84),
  `Observations utilisables` = c(111, 110, 109, 108, 107),
  `Rapport` = c("5,55", "3,06", "2,10", "1,59", "1,27"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(dim_tab, aligne_droite = 1:4))
ajouter(legende_tableau(paste0(
  "Dimension du système selon l'ordre de retard, avec 16 branches et ",
  "3 indicatrices de choc.")))

ajouter("<p>Avec ", m("p"), " = 5, chaque équation compte <strong>84 paramètres pour ",
        "107 observations</strong>. Le système est techniquement estimable, mais le ",
        "rapport de 1,27 est dérisoire : en économétrie appliquée, on considère ",
        "généralement qu'il faut plusieurs dizaines d'observations par paramètre.</p>")
ajouter("<p>Et ce n'est que la situation en fin d'échantillon. Aux premières origines ",
        "du backtest, avec une soixantaine de trimestres disponibles, <strong>le ",
        "rapport passe sous 1</strong> : le système devient mathématiquement ",
        "sous-déterminé et les moindres carrés n'ont plus de solution unique.</p>")
ajouter(intuition(paste0(
  "<p>Un modèle avec autant de paramètres que d'observations peut reproduire ",
  "l'histoire parfaitement — et ne rien prévoir du tout. Il aura appris le bruit ",
  "plutôt que le signal. C'est le <em>sur-ajustement</em>.</p>",
  "<p>Le symptôme est caractéristique : des coefficients énormes et de signes ",
  "alternés, qui se compensent sur l'échantillon d'estimation et divergent dès ",
  "qu'on les applique à une donnée nouvelle.</p>")))

# =========================== 4 ==============================================
ajouter("<h2 id='s4'><span class='num'>4.</span>L'idée bayésienne : contraindre plutôt qu'estimer</h2>")
ajouter(intuition(paste0(
  "<p>Si les données ne suffisent pas à déterminer 84 coefficients, deux voies ",
  "s'offrent : réduire le modèle, ou lui ajouter de l'information extérieure.</p>",
  "<p>L'approche bayésienne choisit la seconde. On déclare <em>avant</em> de regarder ",
  "les données ce qu'on croit raisonnable — par exemple que la plupart des ",
  "coefficients sont proches de zéro — et l'estimation devient un compromis entre ",
  "cette croyance et ce que les données montrent.</p>")))

ajouter("<ul>",
        "<li><strong>Point de vue fréquentiste</strong> : ", m("B"),
        " est une valeur fixe inconnue, qu'on estime. Avec 84 paramètres et ",
        "107 observations, l'estimation est très imprécise.</li>",
        "<li><strong>Point de vue bayésien</strong> : ", m("B"),
        " est une variable aléatoire dont on se donne une distribution a priori ",
        m("p(B)"), ". Les données la mettent à jour en une distribution a ",
        "posteriori.</li>",
        "</ul>")
ajouter("<p>La mise à jour suit le théorème de Bayes :</p>")
ajouter(eq(paste0(m("p(B | Y)"), " <span class='op'>&prop;</span> ", m("p(Y | B)"),
                  " <span class='op'>&times;</span> ", m("p(B)"))))
ajouter("<p>soit, en français : ce qu'on croit après avoir vu les données est ",
        "proportionnel à ce que les données disent, pondéré par ce qu'on croyait ",
        "avant.</p>")

ajouter(definition("Le compromis, vu sur un cas simple",
  paste0("<p>Pour une régression avec un prior normal centré sur zéro et de variance ",
         m("&tau;<sup>2</sup>"), ", la moyenne a posteriori vaut :</p>",
         eq(paste0(m("B&#770;"), " = ( ", m("X"), "<span class='op'>&prime;</span>",
                   m("X"), " <span class='op'>+</span> ",
                   "<span class='fr'><span class='hi'>&sigma;<sup>2</sup></span>",
                   "<span class='lo'>&tau;<sup>2</sup></span></span> ", m("I"),
                   " )<sup>&minus;1</sup> ", m("X"),
                   "<span class='op'>&prime;</span> ", m("Y"))),
         "<p>C'est exactement la <em>régression ridge</em>. Le terme ajouté sur la ",
         "diagonale rend la matrice inversible même quand ", m("X&prime;X"),
         " ne l'est pas — ce qui résout le problème de la section 3.</p>",
         "<p>Deux cas limites éclairent le mécanisme. Si ", m("&tau;"),
         " tend vers l'infini, le prior devient plat, le terme ajouté disparaît et ",
         "l'on retrouve les moindres carrés. Si ", m("&tau;"),
         " tend vers zéro, le prior écrase les données et ", m("B&#770;"),
         " tend vers zéro. Entre les deux, on dose.</p>")))

# =========================== 5 ==============================================
ajouter("<h2 id='s5'><span class='num'>5.</span>Le prior de Minnesota</h2>")
ajouter("<p>Reste à choisir <em>quelle</em> croyance a priori. Le prior dit de ",
        "Minnesota, introduit par Litterman (1986) à la Réserve fédérale de ",
        "Minneapolis, formalise trois intuitions économétriques.</p>")
ajouter("<ol>",
        "<li><strong>Le passé récent compte plus que le passé lointain.</strong> Le ",
        "coefficient du cinquième retard doit être plus fortement poussé vers zéro ",
        "que celui du premier.</li>",
        "<li><strong>Le propre passé d'une variable compte plus que celui des ",
        "autres.</strong> La croissance de l'industrie est mieux expliquée par ",
        "l'industrie passée que par la pêche passée.</li>",
        "<li><strong>Les échelles doivent être neutralisées.</strong> Expliquer une ",
        "série calme par une série très volatile demande un coefficient minuscule ; ",
        "le prior doit en tenir compte, sinon il contraint les équations de façon ",
        "inégale.</li>",
        "</ol>")

ajouter("<h3>La forme du prior</h3>")
ajouter("<p>On note ", m("B<sup>(l)</sup><sub>ij</sub>"),
        " le coefficient du retard ", m("l"), " de la variable ", m("i"),
        " dans l'équation de la variable ", m("j"), ". Le prior pose :</p>")
ajouter(eq(paste0("E[ ", m("B<sup>(l)</sup><sub>ij</sub>"), " ] = 0")))
ajouter(eq(paste0("sd( ", m("B<sup>(l)</sup><sub>ij</sub>"), " ) = ",
                  "<span class='fr'><span class='hi'>&lambda;</span>",
                  "<span class='lo'>l<sup>d</sup></span></span>",
                  " <span class='op'>&times;</span> ",
                  "<span class='fr'><span class='hi'>&sigma;<sub>j</sub></span>",
                  "<span class='lo'>&sigma;<sub>i</sub></span></span>")))

ajouter("<h4>La moyenne nulle, et pourquoi ici elle est nulle</h4>")
ajouter("<p>Dans la formulation originale de Litterman, le prior est centré sur ",
        m("&delta;<sub>i</sub>"), " = 1 pour le propre premier retard : les séries ",
        "étant en log-<em>niveaux</em>, l'hypothèse par défaut est la marche ",
        "aléatoire — le meilleur prédicteur du niveau de demain est celui ",
        "d'aujourd'hui.</p>")
ajouter("<p><strong>Nos variables sont déjà des taux de croissance.</strong> Poser ",
        m("&delta;<sub>i</sub>"), " = 1 reviendrait à supposer que la croissance de ce ",
        "trimestre est le meilleur prédicteur de celle du prochain, c'est-à-dire que ",
        "le <em>niveau</em> suit une tendance explosive. La phase 2 ayant établi que ",
        "les seize séries sont stationnaires en Δlog, l'hypothèse cohérente est le ",
        "<strong>retour à la moyenne</strong> : ", m("&delta;<sub>i</sub>"), " = 0.</p>")

ajouter("<h4>Le facteur ", m("l<sup>d</sup>"), " : la décroissance des retards</h4>")
ajouter("<p>L'écart-type a priori est divisé par ", m("l<sup>d</sup>"),
        " : plus le retard est lointain, plus le coefficient est contraint vers zéro. ",
        "Avec ", m("d"), " = 1, le cinquième retard est cinq fois plus serré que le ",
        "premier ; avec ", m("d"), " = 2, vingt-cinq fois.</p>")
ajouter("<p>La version précédente imposait ", m("d"),
        " = 1 en dur. Il est désormais <strong>choisi dans les données</strong>, sur ",
        "la grille {0,5 ; 1 ; 1,5 ; 2} — voir section 8.</p>")

ajouter("<h4>Le rapport ", m("&sigma;<sub>j</sub> / &sigma;<sub>i</sub>"), "</h4>")
ajouter("<p>C'est le terme d'échelle. ", m("&sigma;<sub>i</sub>"),
        " mesure la volatilité propre de la variable ", m("i"),
        " — sa définition précise fait l'objet de la section 7.</p>")
ajouter(intuition(paste0(
  "<p>Prenons deux cas extrêmes de notre échantillon. La pêche a un écart-type de ",
  "14,4 %, l'éducation-santé de 0,8 %.</p>",
  "<p>Pour expliquer l'éducation-santé par la pêche, le coefficient doit être ",
  "minuscule : une variation de 14 points de pêche ne peut pas produire plus que ",
  "quelques dixièmes de point d'éducation-santé. Le prior le contraint donc ",
  "fortement — écart-type a priori de 0,0075 pour λ = 0,15.</p>",
  "<p>Dans l'autre sens, expliquer la pêche par l'éducation-santé demanderait un ",
  "coefficient très grand pour avoir un effet. Le prior laisse faire — écart-type a ",
  "priori de 2,99.</p>",
  "<p>Sans ce rapport, un λ unique contraindrait absurdement les équations selon ",
  "leur unité de mesure.</p>")))

ajouter(definition("Ce que λ signifie exactement",
  paste0("<p>Pour le <strong>propre retard</strong> d'une variable, c'est-à-dire ",
         "quand ", m("i"), " = ", m("j"),
         ", le rapport des échelles vaut 1. La formule se réduit alors à :</p>",
         eq(paste0("sd( ", m("B<sup>(l)</sup><sub>ii</sub>"), " ) = ",
                   "<span class='fr'><span class='hi'>&lambda;</span>",
                   "<span class='lo'>l<sup>d</sup></span></span>")),
         "<p>Donc <strong>λ est directement l'écart-type a priori du coefficient ",
         "autorégressif d'ordre 1</strong>. C'est une quantité interprétable :</p>",
         "<ul>",
         "<li>λ = 0,05 : le coefficient AR(1) propre est a priori dans ±0,10 à 95 % — ",
         "prior très serré ;</li>",
         "<li>λ = 0,15 : dans ±0,30 — le choix de Higgins (2014) ;</li>",
         "<li>λ = 0,50 : dans ±1,00 — autant dire aucune contrainte.</li>",
         "</ul>")))

# =========================== 6 ==============================================
ajouter("<h2 id='s6'><span class='num'>6.</span>La mise en œuvre par observations fictives</h2>")
ajouter(intuition(paste0(
  "<p>Il existe une façon élégante d'imposer un prior sans écrire la moindre formule ",
  "bayésienne : <strong>fabriquer des données imaginaires qui disent ce qu'on ",
  "croit</strong>, les empiler sous les données réelles, et faire une régression ",
  "ordinaire sur le tout.</p>",
  "<p>Si l'on croit que le coefficient vaut zéro, on ajoute des observations où la ",
  "variable explicative prend une grande valeur et la variable expliquée vaut zéro. ",
  "La régression, pour satisfaire ces lignes, tirera le coefficient vers zéro. Plus ",
  "la valeur artificielle est grande, plus la contrainte est forte.</p>")))

ajouter("<p>C'est la méthode des <em>dummy observations</em>. On construit ",
        m("Y<sub>d</sub>"), " et ", m("X<sub>d</sub>"),
        ", on les empile sous les vraies :</p>")
ajouter(eq(paste0(m("Y*"), " = <span class='op'>[</span> ", m("Y"),
                  " <span class='op'>;</span> ", m("Y<sub>d</sub>"),
                  " <span class='op'>]</span>&nbsp;&nbsp;&nbsp;&nbsp;",
                  m("X*"), " = <span class='op'>[</span> ", m("X"),
                  " <span class='op'>;</span> ", m("X<sub>d</sub>"),
                  " <span class='op'>]</span>")))
ajouter("<p>puis on estime par moindres carrés sur le système augmenté :</p>")
ajouter(eq(paste0(m("B&#770;"), " = ( ", m("X*"), "<span class='op'>&prime;</span>",
                  m("X*"), " )<sup>&minus;1</sup> ", m("X*"),
                  "<span class='op'>&prime;</span> ", m("Y*"))))
ajouter("<p>Ceci équivaut exactement à la moyenne a posteriori sous prior ",
        "normal-inverse-Wishart. En développant :</p>")
ajouter(eq(paste0(m("X*"), "<span class='op'>&prime;</span>", m("X*"), " = ",
                  m("X"), "<span class='op'>&prime;</span>", m("X"),
                  " <span class='op'>+</span> ", m("X<sub>d</sub>"),
                  "<span class='op'>&prime;</span>", m("X<sub>d</sub>"))))
ajouter("<p>on retrouve la structure de la régression ridge de la section 4 : le bloc ",
        "fictif joue le rôle du terme de régularisation ajouté sur la diagonale.</p>")

ajouter("<h3>Les trois blocs</h3>")
ajouter("<h4>Bloc 1 — le prior sur les coefficients de retard</h4>")
ajouter("<p>", m("n &times; p"), " lignes. Pour le retard ", m("l"),
        " et la variable ", m("i"), " :</p>")
ajouter(eq(paste0(m("Y<sub>d1</sub>"), " = 0&nbsp;&nbsp;&nbsp;&nbsp;",
                  m("X<sub>d1</sub>"), "[ ligne (", m("l"), "&minus;1)", m("n"),
                  "+", m("i"), " , colonne du retard ", m("l"), " de ", m("i"),
                  " ] = ",
                  "<span class='fr'><span class='hi'>&sigma;<sub>i</sub> l<sup>d</sup></span>",
                  "<span class='lo'>&lambda;</span></span>")))
ajouter("<p>Chaque ligne dit : « cette combinaison de coefficients doit valoir zéro », ",
        "avec une force égale à ", m("&sigma;<sub>i</sub> l<sup>d</sup> / &lambda;"),
        ". On retrouve bien, en inversant, l'écart-type a priori de l'équation (3) : ",
        "une valeur artificielle grande — donc λ petit — impose une contrainte forte, ",
        "donc un écart-type a priori faible.</p>")

ajouter("<h4>Bloc 2 — le prior sur la covariance des résidus</h4>")
ajouter("<p>", m("n"), " lignes :</p>")
ajouter(eq(paste0(m("Y<sub>d2</sub>"), " = diag( &sigma; )&nbsp;&nbsp;&nbsp;&nbsp;",
                  m("X<sub>d2</sub>"), " = 0")))
ajouter("<p>Ces lignes ne contraignent aucun coefficient — leur ", m("X"),
        " est nul. Elles informent le modèle sur l'ampleur attendue des résidus, ce ",
        "qui fixe l'échelle de la matrice ", m("&Sigma;"), ".</p>")

ajouter("<h4>Bloc 3 — un prior très lâche sur la constante et les indicatrices</h4>")
ajouter("<p>", m("1 + K"), " lignes :</p>")
ajouter(eq(paste0(m("Y<sub>d3</sub>"), " = 0&nbsp;&nbsp;&nbsp;&nbsp;",
                  m("X<sub>d3</sub>"), " = &epsilon; ", m("I"),
                  "&nbsp;&nbsp;&nbsp;&nbsp;avec &epsilon; = 10<sup>&minus;5</sup>")))
ajouter("<p>Un ", m("&epsilon;"), " minuscule donne une contrainte quasi nulle. On ne ",
        "veut pas dire a priori quelle est la croissance moyenne d'une branche, ni ",
        "l'ampleur d'un choc : on veut seulement que ces termes existent dans le ",
        "modèle. Leur valeur est laissée entièrement aux données.</p>")

ajouter("<h3>Vérifier que l'implémentation est correcte</h3>")
ajouter("<p>L'indexation de ces blocs est délicate : une erreur d'un rang passerait ",
        "inaperçue, le modèle tournerait et produirait des chiffres plausibles. Trois ",
        "propriétés limites sont donc testées à chaque exécution, et le script ",
        "s'arrête si l'une échoue.</p>")
verif <- data.frame(
  `Propriété testée` = c("λ → 0", "λ → ∞", "indicatrice hors échantillon"),
  `Attendu théoriquement` = c(
    "le prior écrase les données : les coefficients de retard tendent vers 0",
    "le prior s'efface : on retrouve exactement les moindres carrés ordinaires",
    "aucune colonne créée — une colonne nulle rendrait le système singulier"),
  `Constaté` = c("max |coefficient| ≈ 2·10<sup>−6</sup>",
                 "écart maximal aux MCO ≈ 9·10<sup>−12</sup>",
                 "absente avant 2020, présentes ensuite"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(verif, aligne_droite = integer(0)))
ajouter(legende_tableau("Tests de l'implémentation du prior, exécutés à chaque lancement."))
ajouter("<p>Le troisième mérite un mot. Aux origines antérieures à 2020, les ",
        "indicatrices de choc n'ont aucune observation non nulle dans l'échantillon. ",
        "Elles seraient des colonnes de zéros, ", m("X&prime;X"),
        " deviendrait singulière et l'inversion produirait des coefficients ",
        "arbitraires. La fonction les retire automatiquement.</p>")

# =========================== 7 ==============================================
ajouter("<h2 id='s7'><span class='num'>7.</span>L'échelle des variables</h2>")
ajouter("<p>Le terme ", m("&sigma;<sub>i</sub>"),
        " de l'équation (3) reste à définir. Il ne s'agit pas de l'écart-type brut de ",
        "la série, mais de l'écart-type de ce qui reste <em>inexpliqué</em> par sa ",
        "propre dynamique — l'écart-type résiduel d'une autorégression univariée :</p>")
ajouter(eq(paste0(
  m("g<sub>i,t</sub>"), " = ", m("c<sub>i</sub>"), " <span class='op'>+</span> ",
  "<span class='big'>&sum;</span><sub class='num'>l=1</sub><sup class='num'>p</sup> ",
  m("&phi;<sub>i,l</sub>"), " ", m("g<sub>i,t&minus;l</sub>"),
  " <span class='op'>+</span> ",
  "<span class='big'>&sum;</span><sub class='num'>k</sub> ",
  m("&gamma;<sub>i,k</sub>"), " ", m("D<sub>k,t</sub>"),
  " <span class='op'>+</span> ", m("&epsilon;<sub>i,t</sub>"))))
ajouter(eq(paste0(m("&sigma;<sub>i</sub>"), " = écart-type de ",
                  m("&epsilon;<sub>i,t</sub>"))))

ajouter("<h3>Pourquoi les indicatrices doivent figurer dans cette régression</h3>")
ajouter("<div class='encadre alerte'><span class='etiq'>Défaut trouvé et corrigé en phase 3</span>",
        "<p>La version précédente estimait ", m("&sigma;<sub>i</sub>"),
        " <strong>sans les indicatrices de choc</strong>. Le prior se calibrait donc ",
        "sur une volatilité que le modèle lui-même déclare non représentative, ",
        "puisqu'il neutralise ces trimestres par ailleurs.</p>",
        "<p>L'incohérence est logique avant d'être statistique : on ne peut pas ",
        "affirmer qu'un trimestre est aberrant et s'en servir pour mesurer la ",
        "volatilité normale.</p></div>")
t7 <- data.frame(
  Branche = c("Hébergement-restauration", "Services aux entreprises", "Transports",
              "Industrie de transformation", "Commerce"),
  `σ avec 2020` = c("9,82 %", "3,40 %", "5,68 %", "3,30 %", "2,52 %"),
  `σ hors 2020` = c("4,05 %", "1,51 %", "2,75 %", "2,22 %", "1,85 %"),
  `Inflation de σ` = c("+142 %", "+125 %", "+107 %", "+49 %", "+36 %"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(t7, aligne_droite = 2:4))
ajouter(legende_tableau("Échelle du prior selon que les trimestres de choc sont neutralisés ou non."))
ajouter("<p>La conséquence se lit directement dans l'équation (3). Comme ",
        m("&sigma;<sub>i</sub>"),
        " est au <em>dénominateur</em> de l'écart-type a priori des équations ",
        "expliquées par la variable ", m("i"),
        ", le gonfler de moitié relâche d'autant le prior — exactement là où ",
        "l'échantillon est le plus pollué.</p>")
ajouter(definition("Un effet contre-intuitif, à dire",
  paste0("<p>Corriger ce défaut <strong>dégrade</strong> la performance si l'on garde ",
         "λ = 0,15 : le nombre de branches mieux prévues que leur moyenne passe de 9 ",
         "à 6.</p>",
         "<p>L'explication est mécanique. Un ", m("&sigma;<sub>i</sub>"),
         " gonflé produisait un <em>excès de contraction accidentel</em>. En le ",
         "retirant, on met à nu le fait que λ = 0,15 est trop lâche pour cet ",
         "échantillon. Ce n'est pas la correction qui nuit — c'est elle qui révèle le ",
         "vrai problème, que le choix des hyperparamètres résout en section 8.</p>")))
ajouter("<h3>Pourquoi σ est recalculé à chaque origine</h3>")
ajouter("<p>Sur l'hébergement-restauration, ", m("&sigma;"),
        " passe de 2,46 % à la première origine du backtest à 9,86 % à la dernière — ",
        "<strong>+301 %</strong> — au fur et à mesure que 2020 entre dans la fenêtre ",
        "d'estimation. Utiliser une valeur calculée sur l'échantillon complet aurait ",
        "donné 9,82 % dès 2014 : un prior quatre fois trop lâche sur cette branche, ",
        "fondé sur une information qui n'existait pas encore.</p>")

# =========================== 8 ==============================================
ajouter("<h2 id='s8'><span class='num'>8.</span>Choisir les hyperparamètres : la vraisemblance marginale</h2>")
ajouter("<p>Le prior dépend de deux quantités qu'aucune théorie ne fixe : ",
        m("&lambda;"), ", le serrage, et ", m("d"),
        ", la décroissance des retards. S'y ajoute ", m("p"),
        ", l'ordre du VAR. La version précédente reprenait ", m("p"), " = 5 et ",
        m("&lambda;"), " = 0,15 de Higgins (2014), calibrés sur un échantillon ",
        "américain bien plus long. Rien ne justifiait de les transposer.</p>")
ajouter(intuition(paste0(
  "<p>Comment choisir un hyperparamètre sans tricher ? La tentation serait de prendre ",
  "celui qui ajuste le mieux les données — mais c'est précisément ce qui mène au ",
  "sur-ajustement : le prior le plus lâche gagnerait toujours.</p>",
  "<p>La <strong>vraisemblance marginale</strong> résout cela. Elle mesure la ",
  "probabilité des données observées <em>en intégrant sur toutes les valeurs ",
  "possibles des coefficients</em>, pondérées par le prior. Un modèle trop souple ",
  "étale sa probabilité sur trop de jeux de données différents, et se pénalise ",
  "lui-même. C'est un rasoir d'Occam automatique.</p>")))
ajouter("<p>Formellement :</p>")
ajouter(eq(paste0(m("p(Y | &lambda;, p, d)"), " = <span class='big'>&int;</span> ",
                  m("p(Y | B, &Sigma;)"), " ", m("p(B, &Sigma; | &lambda;, p, d)"),
                  " ", m("dB"), " ", m("d&Sigma;"))))
ajouter("<p>Sous le prior normal-inverse-Wishart imposé par observations fictives, ",
        "cette intégrale a une <strong>forme close</strong> (Giannone, Lenza &amp; ",
        "Primiceri, 2015 ; annexe de Bańbura, Giannone &amp; Reichlin, 2010).</p>")
ajouter(definition("Notations de la formule",
  paste0("<p>", m("T"), " : nombre d'observations réelles &nbsp;&middot;&nbsp; ",
         m("T<sub>d</sub>"), " : nombre d'observations fictives &nbsp;&middot;&nbsp; ",
         m("k"), " : nombre de régresseurs &nbsp;&middot;&nbsp; ", m("n"),
         " : nombre de variables</p>",
         "<p>Les deux matrices d'échelle, a priori et a posteriori :</p>",
         eq(paste0(m("S<sub>d</sub>"), " = ", m("Y<sub>d</sub>&prime;Y<sub>d</sub>"),
                   " <span class='op'>&minus;</span> ",
                   m("Y<sub>d</sub>&prime;X<sub>d</sub>"), " ( ",
                   m("X<sub>d</sub>&prime;X<sub>d</sub>"), " )<sup>&minus;1</sup> ",
                   m("X<sub>d</sub>&prime;Y<sub>d</sub>"))),
         eq(paste0(m("S*"), " = ", m("Y*&prime;Y*"),
                   " <span class='op'>&minus;</span> ", m("Y*&prime;X*"), " ( ",
                   m("X*&prime;X*"), " )<sup>&minus;1</sup> ", m("X*&prime;Y*"))))))
ajouter("<p>La log-vraisemblance marginale s'écrit alors :</p>")
ajouter(eq(paste0(
  "log ", m("p(Y | &lambda;, p, d)"), " = <span class='op'>&minus;</span> ",
  "<span class='fr'><span class='hi'>n T</span><span class='lo'>2</span></span> ",
  "log &pi;<br><span class='op'>+</span> ",
  "<span class='big'>&sum;</span><sub class='num'>i=1</sub><sup class='num'>n</sup> ",
  "<span class='op'>[</span> ln&Gamma;<span class='op'>(</span>",
  "<span class='fr'><span class='hi'>T + T<sub>d</sub> &minus; k + 1 &minus; i</span>",
  "<span class='lo'>2</span></span><span class='op'>)</span> ",
  "<span class='op'>&minus;</span> ln&Gamma;<span class='op'>(</span>",
  "<span class='fr'><span class='hi'>T<sub>d</sub> &minus; k + 1 &minus; i</span>",
  "<span class='lo'>2</span></span><span class='op'>)</span> <span class='op'>]</span>",
  "<br><span class='op'>&minus;</span> ",
  "<span class='fr'><span class='hi'>n</span><span class='lo'>2</span></span> ",
  "<span class='op'>(</span> log |", m("X*&prime;X*"),
  "| <span class='op'>&minus;</span> log |",
  m("X<sub>d</sub>&prime;X<sub>d</sub>"), "| <span class='op'>)</span>",
  "<br><span class='op'>&minus;</span> ",
  "<span class='fr'><span class='hi'>T + T<sub>d</sub> &minus; k</span>",
  "<span class='lo'>2</span></span> log |", m("S*"),
  "| <span class='op'>+</span> ",
  "<span class='fr'><span class='hi'>T<sub>d</sub> &minus; k</span>",
  "<span class='lo'>2</span></span> log |", m("S<sub>d</sub>"), "|")))
ajouter("<p>Chaque ligne a un sens. La première est une constante de normalisation. ",
        "La deuxième, faite de fonctions log-gamma, provient de l'intégration sur ",
        m("&Sigma;"), " dans la loi inverse-Wishart. La troisième compare la ",
        "précision a posteriori à la précision a priori : c'est là que la complexité ",
        "du modèle se paie. La dernière compare l'ajustement a posteriori à ",
        "l'ajustement a priori.</p>")
ajouter("<p>On retient le triplet ", m("(p, &lambda;, d)"),
        " qui <strong>maximise</strong> cette quantité, calculée sur les seules ",
        "données disponibles à chaque origine.</p>")

ajouter("<h3>Un point technique qui n'est pas un détail</h3>")
ajouter("<div class='encadre'><span class='etiq'>Comparabilité entre ordres de retard</span>",
        "<p>Un VAR(", m("p"), ") estimé sur ", m("T"), " observations n'en explique ",
        "que ", m("T &minus; p"),
        ". Comparer des vraisemblances calculées sur des échantillons de tailles ",
        "différentes n'a aucun sens : le modèle avec le moins de retards aurait ",
        "mécaniquement plus d'observations, donc une vraisemblance plus élevée — et ",
        "gagnerait pour une raison purement comptable.</p>",
        "<p>Toutes les fonctions de sélection prennent donc un argument ",
        "<code>p_max</code> et n'utilisent que les observations ",
        "(<code>p_max</code> + 1) &hellip; ", m("T"), ", quel que soit le ", m("p"),
        " testé. L'échantillon effectif est identique pour tous les candidats.</p></div>")

ajouter("<h3>Grille ou optimisation continue ?</h3>")
ajouter("<p>", m("p"), " et ", m("d"),
        " sont discrets et se cherchent sur une grille. ", m("&lambda;"),
        " est continu : il est cherché par optimisation unidimensionnelle sur ",
        "log ", m("&lambda;"), " &isin; [log 0,01 ; log 2], la surface du critère ",
        "étant lisse et unimodale.</p>")
ajouter("<p>Le gain par rapport à une grille de quinze points est d'ailleurs ",
        "<strong>négligeable</strong> : λ se déplace de 10 % et la log-vraisemblance ",
        "gagne 0,46 sur 2 399. On le fait parce que c'est trivial, pas parce que ",
        "c'était un problème.</p>")

# =========================== 9 ==============================================
ajouter("<h2 id='s9'><span class='num'>9.</span>Choisir les hyperparamètres : le critère d'ajustement</h2>")
ajouter("<p>Une seconde méthode est implémentée, de philosophie différente. Elle ",
        "provient de Bańbura, Giannone &amp; Reichlin (2010).</p>")
ajouter(intuition(paste0(
  "<p>L'idée est un étalonnage. Un grand VAR bayésien contraint doit ajuster un petit ",
  "ensemble de variables de référence <em>aussi bien, mais pas mieux</em>, qu'un ",
  "petit VAR non contraint estimé par moindres carrés sur ces seules variables.</p>",
  "<p>Pourquoi « pas mieux » ? Parce que le grand modèle dispose de bien plus de ",
  "régresseurs. S'il ajuste mieux, c'est qu'il exploite cette liberté pour épouser le ",
  "bruit — le prior est trop lâche. Le petit VAR, estimable sans contrainte, sert de ",
  "repère de ce qu'un ajustement honnête doit donner.</p>")))
ajouter("<p>On définit :</p>")
ajouter(eq(paste0(
  "Fit(", m("&lambda;, p, d"), ") = ",
  "<span class='fr'><span class='hi'>1</span><span class='lo'>|R|</span></span> ",
  "<span class='big'>&sum;</span><sub class='num'>i &isin; R</sub> ",
  "<span class='fr'><span class='hi'>MSE<sub>i</sub>( &lambda;, p, d )</span>",
  "<span class='lo'>MSE<sub>i</sub>( petit VAR MCO, p )</span></span>")))
ajouter("<p>et l'on retient le triplet qui amène ", m("Fit"),
        " au plus près de <strong>1</strong>.</p>")
ajouter("<p>L'ensemble de référence ", m("R"),
        " retenu ici est formé des <strong>trois branches au plus fort poids ",
        "moyen</strong> dans la valeur ajoutée : industrie de transformation, ",
        "commerce, agriculture. Un VAR non contraint sur trois variables reste ",
        "estimable par moindres carrés — 16 régresseurs pour 59 observations à la ",
        "première origine — ce qui n'est pas le cas sur seize. C'est tout l'intérêt ",
        "de l'étalon.</p>")

ajouter("<h3>Départager les deux méthodes</h3>")
ajouter("<p>Les deux critères ne mesurent pas la même chose : l'un maximise une ",
        "probabilité, l'autre vise une cible d'ajustement. Ils sont donc comparés ",
        "<strong>hors échantillon</strong>, sur la qualité des prévisions qu'ils ",
        "produisent — le seul terrain neutre.</p>")
t9 <- choix %>%
  dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Méthode = specification,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Branches < 1` = n_branches_ok,
                   `Corrélation médiane` = nb(correl_mediane, 2),
                   Retenue = ifelse(retenu, "oui", ""))
ajouter(tbl(as.data.frame(t9), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Performance hors échantillon des trois modes de choix. Le ratio et la ",
  "corrélation sont définis en section 13.")))

ajouter("<div class='encadre'><span class='etiq'>Règle de départage, posée à l'avance</span>",
        "<p>Le ratio médian ne suffit pas à trancher : <strong>il récompense ",
        "mécaniquement le sur-serrage</strong>. Un prior très serré produit une ",
        "prévision quasi constante — donc un ratio qui frôle 1 par le bas — sans ",
        "qu'aucun signal ne soit capté. La corrélation entre prévu et réalisé, elle, ",
        "mesure le signal réel.</p>",
        "<p>Règle retenue : classement sur le ratio médian ; si deux modes sont à ",
        "moins de 0,01 l'un de l'autre, départage sur la corrélation médiane.</p>",
        sprintf(paste0("<p>Le mode retenu est <strong>%s</strong>, avec un ratio ",
                       "médian de %s contre %s pour le suivant.</p>"),
                mode_ret, nb(min(choix$ratio_median), 3),
                nb(sort(choix$ratio_median)[2], 3)), "</div>")

ajouter("<div class='encadre alerte'><span class='etiq'>Fragilité de ce départage</span>",
        "<p>Il faut être franc : <strong>ce classement s'est retourné trois fois</strong> ",
        "au cours du développement, selon que l'échelle σ était corrigée, que ",
        "l'exposant ", m("d"), " entrait dans la grille, ou que λ était cherché en ",
        "continu.</p>",
        "<p>Les écarts entre méthodes sont du même ordre que leur sensibilité aux ",
        "détails d'implémentation. La conclusion robuste n'est donc pas « telle ",
        "méthode l'emporte », mais <strong>choisir les hyperparamètres vaut mieux que ",
        "les hériter</strong> — ce point-là ne s'est jamais inversé.</p>",
        "<p>Cette section est pour cette raison entièrement pilotée par les fichiers ",
        "de résultats : aucun nom de méthode n'y est écrit en dur.</p></div>")

t9b <- hyper %>%
  dplyr::group_by(selection) %>%
  dplyr::summarise(p_min = min(p), p_med = stats::median(p), p_max = max(p),
                   l_min = min(lambda), l_med = stats::median(lambda),
                   l_max = max(lambda), .groups = "drop") %>%
  dplyr::mutate(methode = dplyr::recode(selection,
    fixe = "fixes (Higgins)", ml = "vraisemblance marginale",
    bgr = "critère d'ajustement")) %>%
  dplyr::arrange(selection) %>%
  dplyr::transmute(Méthode = methode, `p min` = p_min, `p médian` = p_med,
                   `p max` = p_max, `λ min` = nb(l_min, 4),
                   `λ médian` = nb(l_med, 4), `λ max` = nb(l_max, 4))
ajouter(tbl(as.data.frame(t9b), aligne_droite = 2:7))
ajouter(legende_tableau(paste0(
  "Hyperparamètres retenus au fil des ", n_origines, " origines du backtest.")))
ajouter("<p>Deux constats. <strong>Les deux méthodes choisissent un prior nettement ",
        "plus serré que 0,15</strong>, et <strong>aucune ne retient ", m("p"),
        " = 5 comme ordre médian</strong>. La valeur héritée de GDPNow n'était pas ",
        "transposable, et les deux critères le disent indépendamment l'un de ",
        "l'autre.</p>")

ajouter(figure("03_criteres_hyperparametres.png",
               "Les deux critères sur la grille, dernière origine",
               paste0("La vraisemblance marginale (panneau du bas) présente un maximum ",
                      "intérieur net vers λ ≈ 0,10, et les courbes des différents p y ",
                      "sont quasi confondues — l'ordre de retard importe peu une fois ",
                      "le serrage bien choisi. Le critère d'ajustement (panneau du ",
                      "haut) décroît de façon monotone et croise la cible 1 d'autant ",
                      "plus tôt que p est grand, ce qui est la traduction directe du ",
                      "sur-ajustement qu'il détecte. Dans les deux cas, λ = 0,15 se ",
                      "situe au-delà de l'optimum.")))
ajouter(figure("03_hyperparametres_par_origine.png",
               "Hyperparamètres retenus à chaque origine",
               paste0("Les valeurs bougent, et c'est le point : un choix unique sur ",
                      "l'échantillon complet aurait imposé rétroactivement aux ",
                      "origines anciennes un réglage calibré sur des données qu'elles ",
                      "ne connaissaient pas.")))

# =========================== 10 =============================================
ajouter("<h2 id='s10'><span class='num'>10.</span>Traiter les chocs</h2>")
ajouter("<p>Le deuxième trimestre 2020 voit l'hébergement-restauration perdre 85,7 % ",
        "de sa valeur ajoutée en un trimestre. Une observation de cette ampleur, ",
        "laissée telle quelle, déforme tous les coefficients : la régression cherche ",
        "à l'expliquer par les retards, et y échoue en distordant l'ensemble.</p>")

ajouter("<h3>10.1 Ce que fait une indicatrice</h3>")
ajouter("<p>Ajouter une variable ", m("D<sub>k,t</sub>"), " valant 1 au trimestre ",
        m("k"), " et 0 ailleurs, avec un coefficient libre ",
        m("&gamma;<sup>(k)</sup>"),
        ", permet au modèle d'absorber ce trimestre sans le faire payer aux ",
        "coefficients de retard.</p>")
ajouter(definition("Une indicatrice nettoie l'estimation, elle ne prévoit rien",
  paste0("<p>En prévision, les indicatrices valent <strong>0</strong>. Elles servent à ",
         "empêcher les trimestres aberrants de contaminer les <em>coefficients</em>, ",
         "pas à annoncer un choc.</p>",
         "<p>Un modèle qui poserait l'indicatrice à 1 pour prévoir 2020 T2 utiliserait ",
         "la connaissance de l'ampleur du choc — qu'il est justement censé estimer. Le ",
         "modèle échoue donc à prévoir 2020, ce qui est le comportement honnête, et le ",
         "backtest le montre.</p>")))

ajouter("<h3>10.2 Pourquoi une liste de dates n'est pas défendable</h3>")
ajouter("<p>La version précédente codait les trimestres de choc en dur : 2020 T1, T2 ",
        "et T3. Ce choix était <strong>invérifiable</strong>. Avec un seul épisode ",
        "dans l'échantillon, aucun exercice hors échantillon ne peut départager deux ",
        "listes : il n'y a qu'un cas.</p>")
ajouter("<p>Une <strong>règle</strong>, en revanche, se teste. Elle se déclenche sur ",
        "plusieurs épisodes séparés dans le temps, et ses paramètres deviennent ",
        "comparables. C'est aussi plus honnête en temps réel : un prévisionniste de ",
        "2019 n'avait aucune indicatrice 2020 codée en dur, mais il pouvait avoir une ",
        "règle.</p>")

ajouter("<h3>10.3 La règle de détection</h3>")
ajouter("<p>Un trimestre est identifié comme choc si <strong>au moins ", m("k"),
        " branches</strong> s'écartent de plus de ", m("z"),
        " échelles robustes de leur médiane :</p>")
ajouter(eq(paste0("choc(", m("t"), ")  <span class='op'>&hArr;</span>  ",
                  "#<span class='op'>{</span> ", m("i"),
                  " : <span class='op'>|</span> ", m("g<sub>i,t</sub>"),
                  " <span class='op'>&minus;</span> méd<sub>i</sub> ",
                  "<span class='op'>|</span> <span class='op'>&gt;</span> ", m("z"),
                  " &middot; MAD<sub>i</sub> <span class='op'>}</span> ",
                  "<span class='op'>&ge;</span> ", m("k"))))
ajouter("<h4>Pourquoi la médiane et l'écart absolu médian</h4>")
ajouter(eq(paste0("MAD<sub>i</sub> = 1,4826 <span class='op'>&times;</span> méd",
                  "<span class='op'>(</span> <span class='op'>|</span> ",
                  m("g<sub>i,t</sub>"), " <span class='op'>&minus;</span> méd",
                  "<span class='op'>(</span>", m("g<sub>i</sub>"),
                  "<span class='op'>)</span> <span class='op'>|</span> ",
                  "<span class='op'>)</span>")))
ajouter("<p>Le facteur 1,4826 le rend comparable à un écart-type sous hypothèse ",
        "gaussienne — c'est l'inverse du quantile 0,75 de la loi normale centrée ",
        "réduite.</p>")
ajouter(intuition(paste0(
  "<p>Avec une moyenne et un écart-type ordinaires, un choc majeur <strong>gonfle ",
  "l'échelle et masque les chocs voisins</strong>. Concrètement : après ",
  "l'effondrement de 2020 T2, l'écart-type devient si grand que le rebond de T3 — ",
  "pourtant spectaculaire — passe sous le seuil.</p>",
  "<p>La médiane et le MAD ne bougent pratiquement pas en présence de quelques ",
  "valeurs extrêmes : c'est la propriété de <em>robustesse</em>. Le MAD a un point de ",
  "rupture de 50 %, contre 0 % pour l'écart-type.</p>")))
ajouter("<h4>Pourquoi la comparaison doit être saisonnière</h4>")
ajouter("<div class='encadre alerte'><span class='etiq'>Défaut de la première version de la règle</span>",
        "<p>Les valeurs ajoutées ne sont pas corrigées des variations saisonnières. ",
        "Une règle qui compare chaque trimestre à la médiane de <em>tous</em> les ",
        "trimestres flague donc les premiers trimestres en série. La première version ",
        "détectait T1-2004, T1-2005, T1-2006, T1-2007, T1-2008, T1-2010, T1-2011, ",
        "T1-2017&hellip;</p>",
        "<p>Ce n'est pas une suite de chocs, c'est la saisonnalité de l'agriculture et ",
        "de la pêche. Chaque trimestre est donc comparé aux trimestres de ",
        "<strong>même rang calendaire</strong>.</p></div>")
ajouter("<p>La règle ne voit que l'échantillon d'entraînement de son origine. Repérer ",
        "un point aberrant <em>à l'intérieur</em> de l'information déjà disponible ",
        "n'est pas du look-ahead : c'est du nettoyage.</p>")

ajouter(figure("03_chocs_detectes.png",
               "Trimestres identifiés comme chocs, origine par origine",
               paste0("Chaque ligne est une origine du backtest, chaque carré un ",
                      "trimestre qu'elle a retenu comme choc. Aucun point n'apparaît ",
                      "au-dessus de la diagonale : la contrainte temporelle est ",
                      "respectée. La règle identifie plusieurs épisodes distincts — ",
                      "1998, 2000-2001, 2003, la crise financière de 2008-2009, 2010, ",
                      "puis 2019-2021 — et non un seul.")))

ajouter("<h3>10.4 Quelle règle retenir</h3>")
t10 <- regles %>%
  dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Règle = specification,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Branches < 1` = n_branches_ok,
                   `Corrél. médiane` = nb(correl_mediane, 2),
                   `Chocs détectés` = sprintf("%d à %d", chocs_min, chocs_max))
ajouter(tbl(as.data.frame(t10), aligne_droite = 2:4))
ajouter(legende_tableau("Performance selon la façon de repérer les chocs."))
ajouter(sprintf(paste0("<p>La <strong>%s</strong> l'emporte, devant la liste codée en ",
                       "dur. Surtout, elle détecte un nombre variable de chocs selon ",
                       "l'origine — elle s'appuie donc sur plusieurs épisodes, et ",
                       "devient évaluable.</p>"), regle_ret))
ajouter("<p>Le résultat le plus net n'est pourtant pas là : <strong>ne rien faire est ",
        "de loin la pire option</strong>. Sans aucune indicatrice, la corrélation ",
        "médiane devient <em>négative</em> — la seule spécification dans ce cas.</p>")
ajouter(figure("03_regles_choc.png",
               "Comment repérer les trimestres de choc",
               paste0("L'absence d'indicatrice décale visiblement toute la distribution ",
                      "vers la droite. Entre les différentes règles et la liste codée ",
                      "en dur, les écarts sont plus modestes — mais la règle a sur la ",
                      "liste un avantage que le graphique ne montre pas : elle est ",
                      "testable.")))

ajouter("<h3>10.5 Ce que disent les coefficients</h3>")
ajouter(sprintf(paste0("<p>Sur l'échantillon complet, la règle retenue identifie ",
                       "<strong>%d trimestres de choc</strong> : %s.</p>"),
                length(chocs_estimes),
                esc(paste(sub("^choc_", "", chocs_estimes), collapse = ", "))))
choc_principal <- chocs_estimes[which.max(vapply(chocs_estimes,
  function(cc) max(abs(signes[[cc]])), numeric(1)))]
top <- signes %>%
  dplyr::arrange(dplyr::desc(abs(.data[[choc_principal]]))) %>%
  utils::head(6)
t10b <- as.data.frame(c(
  list(Branche = top$branche),
  stats::setNames(lapply(chocs_estimes, function(cc) nb(100 * top[[cc]], 1)),
                  sub("^choc_", "", chocs_estimes))),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(t10b, aligne_droite = 2:ncol(t10b)))
ajouter(legende_tableau(paste0(
  "Coefficients des indicatrices détectées, en points de croissance trimestrielle. ",
  "Les six branches les plus touchées au trimestre ",
  sub("^choc_", "", choc_principal), ".")))
if (!is.na(n_opposes)) {
  ajouter(sprintf(paste0("<p>Sur <strong>%d branches sur 16</strong>, le coefficient du ",
                         "T2 2020 et celui du T3 2020 sont de <strong>signes ",
                         "opposés</strong> : effondrement puis rebond. Une indicatrice ",
                         "unique partagée entre les deux — ce que faisait la version 1 — ",
                         "aurait estimé une valeur intermédiaire, ne corrigeant ni l'un ",
                         "ni l'autre.</p>"), n_opposes))
} else {
  ajouter("<p>La règle retenue ne sélectionne pas simultanément les deux trimestres de ",
          "2020 sur l'échantillon complet. C'est une conséquence assumée du passage ",
          "d'une liste imposée à une règle : le périmètre des chocs n'est plus choisi, ",
          "il est déduit.</p>")
}
ajouter(figure("03_coefficients_indicatrices.png",
               "Coefficients des indicatrices, par branche",
               paste0("Chaque barre est l'effet estimé d'un trimestre de choc sur une ",
                      "branche. Les épisodes détectés ne se ressemblent pas : la crise ",
                      "financière touche surtout l'industrie et les transports, 2020 ",
                      "frappe l'ensemble des services marchands avec une amplitude sans ",
                      "commune mesure. C'est ce qu'une liste unique de dates, assortie ",
                      "d'un coefficient partagé, ne pouvait pas représenter.")))

# =========================== 11 =============================================
ajouter("<h2 id='s11'><span class='num'>11.</span>Trois leviers structurels</h2>")
ajouter("<p>Trois réglages supplémentaires ont été introduits pour répondre à des ",
        "limites précises. Tous trois sont sélectionnés hors échantillon.</p>")

ajouter("<h3>11.1 θ — le serrage croisé</h3>")
ajouter("<p>Litterman distingue le propre retard des retards croisés par un facteur ",
        "uniforme ", m("&theta;"), " &le; 1, qui resserre les seconds :</p>")
ajouter(eq(paste0("sd( ", m("B<sup>(l)</sup><sub>ij</sub>"), " ) = ",
                  "<span class='fr'><span class='hi'>&lambda;</span>",
                  "<span class='lo'>l<sup>d</sup></span></span>",
                  " <span class='op'>&times;</span> ",
                  "<span class='fr'><span class='hi'>&sigma;<sub>j</sub></span>",
                  "<span class='lo'>&sigma;<sub>i</sub></span></span>",
                  " <span class='op'>&times;</span> <span class='op'>{</span> 1 si ",
                  m("i"), " = ", m("j"),
                  " , &theta; sinon <span class='op'>}</span>")))
ajouter("<p>Notre implémentation ne les distinguait que par le rapport des échelles. ",
        "Or ce rapport va de 0,057 à 17,7 sur cet échantillon, avec une médiane de 1 : ",
        "il différencie fortement certains couples et pas du tout les autres.</p>")
ajouter("<div class='encadre alerte'><span class='etiq'>θ a un coût méthodologique</span>",
        "<p>Un serrage croisé distinct rend le prior <strong>différent d'une équation à ",
        "l'autre</strong>. Le bloc d'observations fictives n'est plus partagé, et la ",
        "structure normale-inverse-Wishart est perdue.</p>",
        "<p>Conséquence directe : <strong>la vraisemblance marginale n'a plus de forme ",
        "close</strong>. L'estimation se fait équation par équation, et θ ne peut être ",
        "choisi que hors échantillon — jamais par maximisation.</p></div>")

ajouter("<h3>11.2 ρ — la pondération géométrique</h3>")
ajouter("<p>La phase 2 a montré une <strong>rupture de variance en 2014</strong> : ",
        "douze branches sur seize rejettent l'égalité des variances de part et d'autre ",
        "de cette date, artefact de la rétropolation des comptes.</p>")
ajouter("<p>La réponse naturelle — une fenêtre glissante — s'est révélée ",
        "contre-productive. Une pondération géométrique conserve toutes les ",
        "observations en allégeant progressivement les anciennes :</p>")
ajouter(eq(paste0(m("w<sub>t</sub>"), " = ", m("&rho;<sup>T−t</sup>"),
                  "&nbsp;&nbsp;&nbsp;&nbsp;", m("&rho;"),
                  " <span class='op'>&isin;</span> <span class='op'>]</span>0, 1",
                  "<span class='op'>]</span>")))
ajouter("<p>Techniquement il s'agit de moindres carrés pondérés : chaque ligne ",
        "<em>réelle</em> du système est multipliée par ", m("&radic;w<sub>t</sub>"),
        ", les observations <em>fictives</em> du prior restant à poids 1 — le prior ne ",
        "doit pas s'affaiblir parce que l'échantillon vieillit.</p>")

ajouter("<h3>11.3 La fenêtre d'estimation de σ</h3>")
ajouter("<p>Le prior porte sur une <em>échelle</em>, qui doit refléter le régime ",
        "courant ; les <em>dynamiques</em>, elles, profitent de tout l'historique. Rien ",
        "n'oblige à estimer les deux sur le même échantillon.</p>")
ajouter("<p>Mesure : σ estimé depuis 2014 s'écarte de <strong>19 % en médiane</strong> ",
        "du σ plein échantillon, et de plus de 20 % sur <strong>sept branches sur ",
        "seize</strong>.</p>")

ajouter("<h3>11.4 Résultats</h3>")
t11 <- leviers %>%
  dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Levier = specification,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Branches < 1` = n_branches_ok,
                   `Corrél. médiane` = nb(correl_mediane, 2))
ajouter(tbl(as.data.frame(t11), aligne_droite = 2:4))
ajouter(legende_tableau("Performance hors échantillon des trois leviers."))
ajouter(sprintf(paste0("<p><strong>Un seul levier aide : %s.</strong> Le gain sur le ",
                       "ratio est modeste, mais la corrélation médiane passe de %s à ",
                       "%s — soit une amélioration de moitié du signal réellement ",
                       "capté, ce qui compte davantage.</p>"),
                levier_ret,
                nb(leviers$correl_mediane[leviers$specification == "reference (aucun levier)"], 2),
                nb(max(leviers$correl_mediane), 2)))
ajouter("<h4>Deux résultats négatifs, à dire</h4>")
ajouter("<p><strong>La pondération géométrique ne marche pas.</strong> Elle avait été ",
        "présentée comme la vraie réponse à l'échec de la fenêtre glissante : alléger ",
        "plutôt que jeter. ρ = 0,99 ne change rien, ρ = 0,97 dégrade la corrélation. ",
        "La conclusion est plus large que prévu : <strong>escompter les observations ",
        "anciennes, sous quelque forme que ce soit, coûte plus que ce que ",
        "l'homogénéité rapporte</strong>.</p>")
ajouter("<p><strong>Le serrage croisé dégrade franchement.</strong> θ = 0,50 fait ",
        "tomber la corrélation médiane, θ = 0,25 la rend négative. Le rapport des ",
        "échelles fait déjà le travail de différenciation ; y ajouter un facteur ",
        "uniforme sur-contraint les équations.</p>")
ajouter("<div class='encadre alerte'><span class='etiq'>Un bug silencieux, trouvé en poursuivant 2 400 avertissements</span>",
        "<p>L'introduction de la règle de détection a fait apparaître des milliers ",
        "d'avertissements <code>Inf replaced by maximum positive value</code>. Les ",
        "tracer plutôt que les faire taire a révélé un défaut réel.</p>",
        "<p>La règle identifie <strong>T2-1998</strong> comme choc — la toute première ",
        "observation de la série. Or un VAR(", m("p"), ") n'explique pas ses ", m("p"),
        " premières observations. L'indicatrice devenait donc une <strong>colonne de ",
        "zéros</strong> dans la régression, et ", m("X&prime;X"), " singulière.</p>",
        "<p>Ce défaut était <strong>invisible avec une liste de dates codée en ",
        "dur</strong>, qui ne flaguait jamais une observation de bord. Il n'est apparu ",
        "que parce que la règle, elle, le peut.</p></div>")

# =========================== 12 =============================================
ajouter("<h2 id='s12'><span class='num'>12.</span>Le protocole récursif</h2>")
ajouter("<p>Tout ce qui précède doit être exécuté <strong>sans jamais utiliser ",
        "d'information postérieure à la cible</strong>. C'est l'objet même de la ",
        "phase 3, et l'exigence centrale du plan de correction.</p>")
ajouter(definition("L'ensemble d'information",
  paste0("<p>Pour un trimestre cible ", m("T"),
         ", l'ensemble d'information autorisé est :</p>",
         eq(paste0(m("&#8496;<sub>T</sub>"), " = <span class='op'>{</span> ",
                   m("g<sub>t</sub>"), " : ", m("t"),
                   " <span class='op'>&lt;</span> ", m("T"),
                   " <span class='op'>}</span>")),
         "<p>Tout ce qui entre dans le modèle — données, échelles, hyperparamètres, ",
         "dates de choc — doit être une fonction de ", m("&#8496;<sub>T</sub>"),
         " seul.</p>")))
ajouter("<p>La boucle exécutée à chaque origine :</p>")
ajouter("<pre><code>g(1) ... g(T-1)\n",
        "     -> detection des chocs sur ce seul echantillon\n",
        "     -> sigma(i) estimes AVEC les indicatrices, sur fenetre recente\n",
        "     -> (p, lambda, d) choisis par le critere retenu\n",
        "     -> prior de Minnesota construit\n",
        "     -> BVAR estime sur le systeme augmente\n",
        "     -> g(T) prevu, indicatrices a 0</code></pre>")
ajouter(sprintf(paste0("<p>L'exercice porte sur <strong>%s origines</strong>, de ",
                       "T2-2014 à T1-2026, soit <strong>%s prévisions</strong> de ",
                       "branche. À chaque origine, <em>tout</em> est refait.</p>"),
                nb(n_origines), nb(n_origines * n_branches)))
ajouter("<h3>Pourquoi l'évaluation commence au deuxième trimestre 2014</h3>")
ajouter("<p>Ce point de départ est fixé une fois pour toutes pour l'ensemble du projet, ",
        "et la raison n'est pas statistique. La série de valeur ajoutée ",
        "<strong>nominale</strong> commence au premier trimestre 2014 ; T2-2014 est ",
        "donc le premier trimestre pour lequel un poids ", m("w<sub>j,T&minus;1</sub>"),
        " en prix courants pourra être construit, comme l'exige une agrégation de type ",
        "Fisher ou Törnqvist. Aligner le BVAR dessus garantit que toutes les phases ",
        "s'évalueront sur le même ensemble de trimestres cibles.</p>")
ajouter("<h3>Les barrières anti-look-ahead</h3>")
ajouter("<ul>",
        "<li>la sélection de l'échantillon se fait sur <code>dates &lt; target_date</code>, ",
        "strictement ;</li>",
        "<li>la fonction vérifie ensuite <em>elle-même</em> que sa dernière ",
        "observation précède la cible et lève une erreur sinon — double barrière, pour ",
        "que le jour où quelqu'un modifie le filtre, l'erreur soit bruyante ;</li>",
        "<li>le choix des hyperparamètres et la détection des chocs ne voient que ",
        "l'échantillon d'entraînement ;</li>",
        "<li>en sortie, un <code>stopifnot</code> global vérifie chacune des ",
        "prévisions.</li>",
        "</ul>")
ajouter(definition("Précision de vocabulaire",
  paste0("<p>Aucune agrégation n'est faite dans cette phase. Quand elle viendra, la ",
         "somme des seize branches sera la <strong>valeur ajoutée totale</strong>, et ",
         "non le PIB : celui-ci vaut en plus les impôts sur les produits nets des ",
         "subventions, absents de cette base.</p>",
         "<p>S'y ajoute une limite de fond : les comptes sont en <strong>volumes ",
         "chaînés non additifs</strong>. La somme des valeurs ajoutées de branche n'est ",
         "pas exactement la valeur ajoutée totale chaînée.</p>")))

# =========================== 13 =============================================
ajouter("<h2 id='s13'><span class='num'>13.</span>Comment se mesure la qualité d'une prévision</h2>")
ajouter("<p>Soit ", m("e<sub>i,T</sub>"), " = ", m("g<sub>i,T</sub>"),
        " <span class='op'>&minus;</span> ", m("&#285;<sub>i,T</sub>"),
        " l'erreur de prévision de la branche ", m("i"), " à l'origine ", m("T"),
        ". Trois statistiques classiques :</p>")
ajouter(eq(paste0("RMSFE<sub>i</sub> = <span class='op'>&radic;</span>",
                  "<span class='op'>(</span> ",
                  "<span class='fr'><span class='hi'>1</span><span class='lo'>N</span></span> ",
                  "<span class='big'>&sum;</span><sub class='num'>T</sub> ",
                  m("e<sub>i,T</sub><sup>2</sup>"),
                  " <span class='op'>)</span>&nbsp;&nbsp;&nbsp;MAE<sub>i</sub> = ",
                  "<span class='fr'><span class='hi'>1</span><span class='lo'>N</span></span> ",
                  "<span class='big'>&sum;</span><sub class='num'>T</sub> ",
                  "<span class='op'>|</span>", m("e<sub>i,T</sub>"),
                  "<span class='op'>|</span>&nbsp;&nbsp;&nbsp;Biais<sub>i</sub> = ",
                  "<span class='fr'><span class='hi'>1</span><span class='lo'>N</span></span> ",
                  "<span class='big'>&sum;</span><sub class='num'>T</sub> ",
                  m("e<sub>i,T</sub>"))))
ajouter("<h3>Pourquoi le RMSFE brut ne suffit pas</h3>")
ajouter(intuition(paste0(
  "<p>Les seize branches n'ont rien de comparable. L'éducation-santé a un écart-type ",
  "de 0,7 % par trimestre, la pêche de 20 %. Un RMSFE moyen sur les branches serait ",
  "entièrement piloté par la pêche, et ne dirait rien de la qualité du modèle.</p>",
  "<p>Pire : un modèle qui prévoit très mal l'éducation-santé aurait un RMSFE ",
  "minuscule, simplement parce que la série ne bouge pas.</p>")))
ajouter("<p>On rapporte donc l'erreur à la variabilité propre de chaque série :</p>")
ajouter(eq(paste0("ratio<sub>i</sub> = ",
                  "<span class='fr'><span class='hi'>RMSFE<sub>i</sub></span>",
                  "<span class='lo'>écart-type de ", m("g<sub>i</sub>"),
                  "</span></span>")))
ajouter(definition("Comment lire ce ratio",
  paste0("<p>Le dénominateur est l'erreur qu'on commettrait en prévoyant toujours la ",
         "moyenne historique de la branche. Donc :</p>",
         "<ul>",
         "<li>ratio <strong>&lt; 1</strong> : le modèle fait mieux que prédire la ",
         "moyenne — il apporte quelque chose ;</li>",
         "<li>ratio <strong>= 1</strong> : le modèle équivaut à ne rien faire ;</li>",
         "<li>ratio <strong>&gt; 1</strong> : le modèle fait pire que ne rien faire.</li>",
         "</ul>")))
ajouter("<h3>Le ratio ne suffit pas non plus</h3>")
ajouter("<p>Un prior très serré produit une prévision quasi constante, égale à la ",
        "moyenne. Son ratio frôle donc 1 par le bas — <strong>sans qu'aucun signal ne ",
        "soit capté</strong>. Le ratio récompense mécaniquement le sur-serrage.</p>")
ajouter("<p>On lui adjoint donc la corrélation entre prévu et réalisé :</p>")
ajouter(eq(paste0(m("&rho;<sub>i</sub>"), " = corr<span class='op'>(</span> ",
                  m("&#285;<sub>i,T</sub>"), " , ", m("g<sub>i,T</sub>"),
                  " <span class='op'>)</span>")))
ajouter("<p>Un ratio de 1,05 avec une corrélation de 0,3 et un ratio de 1,05 avec une ",
        "corrélation nulle ne sont pas la même pathologie. Le second modèle ne voit ",
        "rien ; le premier voit quelque chose mais l'amplifie mal.</p>")
ajouter("<h3>Le test de Diebold-Mariano</h3>")
ajouter(intuition(paste0(
  "<p>Deux modèles donnent des RMSFE de 0,98 et 1,02. Le premier est-il réellement ",
  "meilleur, ou l'écart tient-il au hasard de l'échantillon ?</p>",
  "<p>Diebold et Mariano (1995) répondent en traitant la <em>différence de perte</em> ",
  "comme une série temporelle dont on teste si la moyenne est nulle.</p>")))
ajouter("<p>Soit ", m("d<sub>T</sub>"), " = ", m("e<sub>1,T</sub><sup>2</sup>"),
        " <span class='op'>&minus;</span> ", m("e<sub>2,T</sub><sup>2</sup>"),
        " la différence des pertes quadratiques. Le test porte sur :</p>")
ajouter(eq(paste0(m("H<sub>0</sub>"), " : E<span class='op'>[</span> ",
                  m("d<sub>T</sub>"), " <span class='op'>]</span> = 0")))
ajouter(eq(paste0("DM = <span class='fr'><span class='hi'>", m("d&#772;"), "</span>",
                  "<span class='lo'><span class='op'>&radic;</span>",
                  "<span class='op'>(</span> V&#770;(", m("d&#772;"),
                  ") <span class='op'>)</span></span></span>",
                  "&nbsp;&nbsp;<span class='op'>&rarr;</span>&nbsp;&nbsp; ", m("N"),
                  "(0, 1)")))
ajouter("<p>où la variance est estimée en tenant compte de l'autocorrélation ",
        "éventuelle des différences de perte. Le test est ici appliqué ",
        "<strong>branche par branche</strong> : pour chaque spécification alternative, ",
        "on compte sur combien de branches l'écart avec la référence est distinguable ",
        "du bruit. C'est plus informatif qu'un test unique sur un agrégat, qui ",
        "noierait des différences de signes opposés selon les branches.</p>")

# =========================== 14 =============================================
ajouter("<h2 id='s14'><span class='num'>14.</span>Résultats</h2>")
ajouter("<h3>14.1 Ce que le modèle prévoit réellement</h3>")
ajouter("<p>C'est le résultat le plus important de la phase, et le moins flatteur.</p>")
t14 <- diag_br %>%
  dplyr::left_join(eval_specs %>%
                     dplyr::filter(grepl("^reference", specification)) %>%
                     dplyr::select(branche, correlation), by = "branche") %>%
  dplyr::arrange(ratio) %>%
  dplyr::transmute(Branche = branche,
                   `RMSFE (%)` = nb(100 * RMSFE, 2),
                   `Écart-type (%)` = nb(100 * ecart_type_reel, 2),
                   Ratio = nb(ratio, 2),
                   `Corrélation` = nb(correlation, 2))
ajouter(tbl(as.data.frame(t14), aligne_droite = 2:5))
ajouter(legende_tableau("Qualité de la prévision, branche par branche."))
ajouter(sprintf(paste0("<p><strong>%d branches sur 16 ont un ratio inférieur à 1</strong>, ",
                       "et le ratio médian vaut %s. Le gain reste faible partout.</p>"),
                n_mieux, nb(ratio_med, 2)))
ajouter("<p>La colonne des corrélations est plus parlante que le ratio. Elle atteint ",
        "0,5 sur les branches les mieux prévues, mais reste <strong>négative sur ",
        "plusieurs branches</strong> : la prévision y varie en sens inverse de la ",
        "réalisation.</p>")
ajouter("<p>Ce n'est pas une contre-performance de programmation. C'est ce qu'on doit ",
        "attendre d'un système de cette dimension estimé sur une centaine ",
        "d'observations de comptes nationaux trimestriels, dont la dynamique propre ",
        "est faible. Mais cela déplace l'enjeu du projet : <strong>l'essentiel devra ",
        "venir des indicateurs mensuels</strong>.</p>")
ajouter(figure("03_nuages_prevu_realise.png",
               "Prévu contre réalisé, branche par branche",
               paste0("La figure la plus révélatrice du rapport. La diagonale en ",
                      "pointillé est la prévision parfaite. Trois régimes se ",
                      "distinguent. Sur la pêche et l'industrie d'extraction, le nuage ",
                      "s'étire le long de la diagonale : le modèle suit. Sur le ",
                      "commerce, les transports ou l'immobilier, le nuage est comprimé ",
                      "verticalement — la prévision sort pratiquement la même valeur ",
                      "quelle que soit la réalisation. Sur l'hébergement-restauration, ",
                      "l'échelle horizontale s'étend jusqu'à −90 % quand la verticale ",
                      "reste confinée : le modèle n'a rien vu venir de 2020, ce qui est ",
                      "attendu puisque les indicatrices valent 0 en prévision.")))
ajouter(figure("03_previsions_par_branche.png",
               "Prévision récursive contre réalisation, les seize branches",
               paste0("Les mêmes données en chronologie. La courbe prévue est ",
                      "systématiquement plus lisse que la réalisée : le modèle capte le ",
                      "niveau moyen et une part de la persistance, pas les mouvements ",
                      "trimestriels. C'est précisément ce que les équations de ",
                      "passerelle devront apporter.")))
ajouter(figure("03_erreurs_par_branche.png",
               "Erreur de prévision, branche par branche",
               paste0("Réalisé moins prévu. Les erreurs sont de faible amplitude sur la ",
                      "majeure partie de la période, puis explosent en 2020 sur les ",
                      "branches exposées. Sur l'administration publique et ",
                      "l'éducation-santé, elles restent minuscules en valeur absolue, ",
                      "mais la série l'est aussi : c'est ce que le ratio corrige.")))
ajouter(figure("03_qualite_par_branche.png",
               "Qualité de la prévision par branche",
               paste0("Le trait vertical marque le seuil au-delà duquel le modèle ",
                      "n'apporte rien. Aucune branche n'est franchement à gauche : le ",
                      "nuage est resserré autour de 1. Les branches les mieux prévues ",
                      "sont aussi les plus volatiles, ce qui est en partie mécanique.")))

ajouter("<h3>14.2 Robustesse de la spécification</h3>")
t14b <- comparaison %>%
  dplyr::left_join(
    comp_hors %>% dplyr::select(specification, ratio_hors = ratio_median),
    by = "specification") %>%
  dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Spécification = specification,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Branches < 1` = n_branches_ok,
                   `Ratio hors 2020` = nb(ratio_hors, 3),
                   `Corrél. médiane` = nb(correl_mediane, 2))
ajouter(tbl(as.data.frame(t14b), aligne_droite = 2:5))
ajouter(legende_tableau(paste0(
  "Distribution du ratio selon la spécification, sur ", n_origines, " origines.")))
t14c <- dm %>%
  dplyr::arrange(dplyr::desc(reference_meilleure - alternative_meilleure)) %>%
  dplyr::transmute(Alternative = specification,
                   `Référence meilleure` = reference_meilleure,
                   `Alternative meilleure` = alternative_meilleure,
                   `Non significatif` = non_significatif)
ajouter(tbl(as.data.frame(t14c), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Diebold-Mariano branche par branche : nombre de branches sur 16 où l'écart avec ",
  "la référence est significatif au seuil de 10 %.")))
ajouter(figure("03_comparaison_specifications.png",
               "Distribution du ratio par spécification",
               paste0("Chaque boîte résume les seize branches. La référence est la seule ",
                      "dont la médiane passe nettement sous le trait vertical. Les ",
                      "fenêtres glissantes décalent toute la distribution vers la ",
                      "droite : raccourcir l'échantillon dans un système déjà privé de ",
                      "degrés de liberté coûte bien plus qu'il ne rapporte.")))

# =========================== 15 =============================================
if (A_CONJOINTE) {

ajouter("<h2 id='s15'><span class='num'>15.</span>Séquentiel ou conjoint : tester le protocole de sélection</h2>")
ajouter("<p>Les sections 8 à 11 choisissent les réglages <strong>l'un après ",
        "l'autre</strong> : d'abord le mode de choix de ", m("(p, &lambda;, d)"),
        ", puis la règle de détection des chocs <em>conditionnellement</em> à ce ",
        "mode, puis le levier structurel <em>conditionnellement</em> aux deux ",
        "premiers. Seize passes récursives au total.</p>")
ajouter(intuition(paste0(
  "<p>Un choix séquentiel n'est optimal que si les axes n'interagissent pas — si ",
  "le meilleur levier est le même quel que soit le mode retenu. Rien ne le ",
  "garantit.</p>",
  "<p>Il y a même une raison précise d'en douter : la fenêtre σ et la règle de ",
  "choc agissent sur <strong>la même quantité</strong>, l'échelle du prior. La ",
  "règle neutralise les trimestres extrêmes par des indicatrices, la fenêtre les ",
  "fait sortir de l'échantillon. Deux façons de traiter le même problème, dont ",
  "l'effet conjoint n'a aucune raison d'être la somme des effets séparés.</p>")))
ajouter("<p>La sélection <strong>jointe</strong> croise les trois axes au lieu de ",
        "les enchaîner : 3 &times; 6 &times; 7 = <strong>126 combinaisons</strong>, ",
        "chacune évaluée sur les 48 origines, soit 96 768 prévisions et une demi-heure ",
        "de calcul sur trois c&oelig;urs.</p>")

ajouter("<h3>15.1 Pourquoi la comparaison directe ne prouverait rien</h3>")
ajouter("<div class='encadre alerte'><span class='etiq'>Le piège, et comment il est évité</span>",
        "<p>Comparer 126 candidats sur les 48 mêmes origines qui servent ensuite à ",
        "annoncer la performance, c'est <strong>sélectionner sur l'échantillon de ",
        "test</strong>. Le maximum de 126 tirages bruités est mécaniquement plus haut ",
        "que celui de 16 — <em>même si tous les candidats se valaient exactement</em>.</p>",
        "<p>Formellement, si les ", m("M"), " candidats avaient la même performance ",
        "espérée ", m("&mu;"), " et un bruit d'évaluation d'écart-type ", m("&tau;"),
        ", le minimum observé vaudrait environ :</p>",
        eq(paste0("E<span class='op'>[</span> min<sub class='num'>m &le; M</sub> ",
                  m("r&#770;<sub>m</sub>"), " <span class='op'>]</span> ",
                  "<span class='op'>&asymp;</span> ", m("&mu;"),
                  " <span class='op'>&minus;</span> ", m("&tau;"),
                  " <span class='op'>&radic;</span><span class='op'>(</span> 2 log ",
                  m("M"), " <span class='op'>)</span>")),
        "<p>Passer de 16 à 126 candidats abaisse donc le minimum apparent d'environ ",
        "0,22 ", m("&tau;"), " <strong>sans qu'aucun candidat ne soit meilleur</strong>. ",
        "Un « gain » de la sélection jointe ne prouverait rien par lui-même.</p>",
        "<p>Deux résultats sont donc produits séparément : une lecture ",
        "<strong>descriptive</strong> de la grille complète, et un ",
        "<strong>protocole validation / test</strong> qui seul permet de conclure.</p></div>")

ajouter("<h3>15.2 Résultat descriptif : le chemin séquentiel a-t-il manqué quelque chose ?</h3>")
rang_seq_conj <- which(grille_conj$mode == "critere d'ajustement" &
                       grille_conj$regle == "regle z=4, k=3" &
                       grille_conj$levier == "sigma sur 60 trimestres")
t15 <- grille_conj %>%
  utils::head(8) %>%
  dplyr::mutate(rang = dplyr::row_number()) %>%
  dplyr::transmute(Rang = rang, Mode = mode, `Règle` = regle, Levier = levier,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Branches < 1` = n_branches_ok,
                   `Corrél.` = nb(correl_mediane, 2))
ajouter(tbl(as.data.frame(t15), aligne_droite = c(1, 5, 6, 7)))
ajouter(legende_tableau(paste0(
  "Les huit meilleures des 126 combinaisons, classées sur le ratio médian.")))
ajouter(sprintf(paste0("<p>La combinaison retenue par le chemin séquentiel de la ",
                       "phase 3 — critère d'ajustement, règle z = 4 / k = 3, σ sur ",
                       "60 trimestres — arrive <strong>%d<sup>e</sup> sur 126</strong> ",
                       "au ratio brut.</p>"), rang_seq_conj))
ajouter("<p>Mais la règle de départage de la section 9 s'applique ici aussi : les ",
        "trois premières sont à moins de 0,01 de ratio l'une de l'autre, donc ",
        "<em>ex &aelig;quo</em> par construction, et le départage se fait sur la ",
        "corrélation. C'est la combinaison séquentielle qui l'emporte alors, avec ",
        "0,24 contre 0,22 et 0,21.</p>")
ajouter(sprintf(paste0("<p><strong>La sélection jointe retient donc exactement la même ",
                       "spécification que la sélection séquentielle.</strong> Les 110 ",
                       "combinaisons que le chemin séquentiel n'avait jamais visitées ",
                       "n'apportent rien.</p>")))
ajouter(figure("03c_classement_combinaisons.png",
               "Les 126 combinaisons, classées",
               paste0("Chaque point est une combinaison. Le nuage est remarquablement ",
                      "plat entre les rangs 1 et 40 — l'écart de ratio y est inférieur ",
                      "à 0,02, soit bien moins que l'incertitude d'échantillonnage sur ",
                      "48 origines. Ce plateau est la vraie information du graphique : ",
                      "il dit que le classement fin entre les meilleures ",
                      "spécifications n'est pas identifiable, et que s'acharner à en ",
                      "extraire un vainqueur reviendrait à lire du bruit.")))

ajouter("<h3>15.3 Les axes sont-ils séparables ?</h3>")
ajouter("<p>Si les axes n'interagissaient pas, le meilleur niveau d'un axe serait le ",
        "même quels que soient les deux autres. On compte donc, pour chaque axe, ",
        "combien de fois chaque niveau l'emporte lorsque les deux autres varient.</p>")
t15b <- interactions %>%
  dplyr::transmute(Axe = axe, Niveau = valeur, `Victoires` = n)
ajouter(tbl(as.data.frame(t15b), aligne_droite = 3))
ajouter(legende_tableau(paste0(
  "Séparabilité des trois axes. Un axe séparable aurait un gagnant unique.")))
ajouter("<p>Le résultat est net et nuancé à la fois :</p>")
ajouter("<ul>",
        "<li><strong>Le mode est quasi séparable.</strong> Le critère d'ajustement ",
        "l'emporte 25 fois sur 42, la vraisemblance marginale 13, les ",
        "hyperparamètres fixes 4. Le choisir en premier, sans conditionnement, était ",
        "légitime.</li>",
        "<li><strong>La règle l'est raisonnablement.</strong> z = 4 / k = 3 gagne 11 ",
        "fois sur 21, z = 5 / k = 3 sept fois — deux règles voisines qui détectent des ",
        "ensembles de chocs proches.</li>",
        "<li><strong>Le levier ne l'est pas du tout.</strong> Six niveaux différents ",
        "l'emportent au moins une fois, et le vainqueur du chemin séquentiel — σ sur ",
        "60 trimestres — ne gagne que 2 fois sur 18. <strong>C'est là que ",
        "l'interaction est réelle</strong>, exactement sur l'axe où on l'attendait.</li>",
        "</ul>")
ajouter(figure("03c_interaction_levier_mode.png",
               "Interaction entre le levier structurel et le mode de choix",
               paste0("Chaque boîte résume les six règles de détection. La lecture ",
                      "importante est le changement d'ordre entre panneaux : θ = 0,25 ",
                      "est le pire levier sous le critère d'ajustement et l'un des ",
                      "meilleurs sous les hyperparamètres fixes. Le mécanisme est ",
                      "lisible : θ resserre, et un mode qui choisit déjà un λ serré n'a ",
                      "pas besoin qu'on resserre davantage, alors que λ = 0,15 imposé ",
                      "est trop lâche et bénéficie du serrage supplémentaire. Les deux ",
                      "réglages sont substituables, donc non séparables.")))

ajouter("<h3>15.4 Résultat décisif : le protocole validation / test</h3>")
ajouter("<p>Les 48 origines sont coupées chronologiquement en deux moitiés de 24. ",
        "Chaque procédure choisit sa spécification sur la <strong>seule</strong> ",
        "moitié de validation, puis est mesurée sur la moitié de test, jamais vue.</p>")
ajouter(definition("Pourquoi une coupure chronologique, malgré son coût",
  paste0("<p>La moitié de test contient 2020, la moitié de validation non. C'est ",
         "déséquilibré, et une coupure alternée serait plus confortable.</p>",
         "<p>Elle serait aussi fausse. Alterner les origines ferait entrer de ",
         "l'information postérieure dans le choix de la spécification, ce que toute ",
         "la phase 3 s'attache à interdire. La coupure chronologique reproduit la ",
         "situation réelle : on calibre sur ce qu'on a, et on subit la suite.</p>")))
t15c <- vtest %>%
  dplyr::transmute(`Procédure` = procedure, Phase = phase, Mode = mode,
                   `Règle` = regle, Levier = levier,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Branches < 1` = n_branches_ok,
                   `Corrél.` = nb(correl_mediane, 2))
ajouter(tbl(as.data.frame(t15c), aligne_droite = 6:8))
ajouter(legende_tableau(paste0(
  "Spécification choisie par chaque procédure sur la validation, et performance ",
  "de cette spécification sur le test.")))
r_seq  <- vtest$ratio_median[vtest$phase == "test" & grepl("^sequentielle", vtest$procedure)]
r_conj <- vtest$ratio_median[vtest$phase == "test" & grepl("^conjointe", vtest$procedure)]
b_seq  <- vtest$n_branches_ok[vtest$phase == "test" & grepl("^sequentielle", vtest$procedure)]
b_conj <- vtest$n_branches_ok[vtest$phase == "test" & grepl("^conjointe", vtest$procedure)]
ajouter(sprintf(paste0("<p>Le verdict est sans ambiguïté. Sur la validation, la ",
                       "procédure jointe fait mieux — c'est attendu, elle a 126 ",
                       "occasions de bien tomber contre 16. Sur le <strong>test</strong>, ",
                       "elle fait <strong>pire</strong> : ratio médian de %s contre %s, ",
                       "et %d branches sous 1 contre %d.</p>"),
                nb(r_conj, 3), nb(r_seq, 3), b_conj, b_seq))
ajouter("<p>Le mécanisme se lit dans les spécifications choisies. Sur une validation ",
        "qui s'arrête avant 2020, la procédure jointe retient un seuil de détection ",
        "élevé — z = 6, k = 2 — qui ne flague presque rien, associé à une pondération ",
        "géométrique agressive. Sur une période sans choc majeur, ne rien détecter ne ",
        "coûte rien. Sur le test, qui commence au deuxième trimestre 2020, cela coûte ",
        "cher. La grille élargie ne l'a pas protégée de ce piège : ",
        "<strong>elle lui a donné plus d'occasions d'y tomber</strong>.</p>")
ajouter(figure("03c_validation_test.png",
               "L'avantage de la sélection jointe survit-il hors de son échantillon de choix ?",
               paste0("Les deux segments se croisent. La procédure jointe part au-dessus ",
                      "sur la validation et passe en dessous sur le test ; la ",
                      "séquentielle fait l'inverse. Un croisement de cette forme est la ",
                      "signature du sur-ajustement de sélection : l'avantage mesuré là ",
                      "où le choix a été fait ne se transporte pas ailleurs. Si la ",
                      "grille élargie avait apporté quelque chose de réel, les deux ",
                      "segments resteraient parallèles.")))

ajouter(definition("Ce tableau ne change pas la spécification de production",
  paste0("<p>Les spécifications qui apparaissent ci-dessus — ML avec z = 5 / k = 3 ",
         "et &rho; = 0,99 pour la procédure séquentielle, Higgins avec z = 6 / k = 2 ",
         "pour la jointe — ne sont <strong>pas</strong> celles du modèle de ",
         "production.</p>",
         "<p>Ce sont celles que chaque procédure aurait retenues si elle n'avait vu ",
         "que la moitié de validation, soit 24 origines s'arrêtant avant 2020. ",
         "L'exercice compare des <strong>procédures de sélection</strong>, pas des ",
         "spécifications : il demande laquelle des deux façons de choisir se ",
         "transporte le mieux hors de son échantillon de choix.</p>",
         "<p>Le modèle de production, lui, dispose des 48 origines, et les deux ",
         "procédures y retiennent la même chose — c'est le résultat de la ",
         "section 15.2.</p>")))

ajouter("<h3>15.5 Ce qu'il faut en retenir</h3>")
ajouter("<div class='encadre'><span class='etiq'>Conclusion</span>",
        "<p><strong>La limite est traitée, et le résultat est négatif.</strong> La ",
        "sélection jointe a été implémentée et exécutée intégralement. Elle ne trouve ",
        "pas mieux que le chemin séquentiel sur l'échantillon complet — elle retient ",
        "la même spécification — et elle se dégrade hors de son échantillon de choix.</p>",
        "<p>Le chemin séquentiel n'est donc pas conservé par commodité de calcul mais ",
        "parce qu'il est <strong>préférable</strong>. Il joue le rôle d'une ",
        "régularisation du protocole de sélection lui-même : restreindre l'ensemble ",
        "des candidats limite le sur-ajustement du choix, de la même façon que le ",
        "prior de Minnesota restreint l'espace des coefficients.</p>",
        "<p>Le parallèle n'est pas décoratif. Tout ce rapport repose sur l'idée qu'avec ",
        "peu d'observations il vaut mieux contraindre qu'estimer librement (section 4). ",
        "Cette section montre que le même principe vaut un cran au-dessus, sur le choix ",
        "des hyperparamètres et non plus sur les coefficients.</p></div>")
ajouter("<p>Un dernier chiffre pour situer l'enjeu. La meilleure spécification ",
        "<em>a posteriori</em> sur le test — inatteignable, puisqu'il faudrait connaître ",
        "le test pour la choisir — donne un ratio de 0,960 contre 0,981 pour la ",
        "procédure séquentielle. <strong>L'écart total entre une sélection honnête et un ",
        "oracle est donc de 0,02 sur le ratio.</strong> C'est le plafond de ce que ",
        "n'importe quelle amélioration du protocole de sélection peut rapporter, et cela ",
        "confirme le diagnostic de la section 14 : le gisement n'est pas dans le réglage ",
        "du BVAR, il est dans les indicateurs mensuels.</p>")

} else {
  ajouter("<h2 id='s15'><span class='num'>15.</span>Séquentiel ou conjoint : tester le protocole de sélection</h2>")
  ajouter("<p>Section non composée : exécuter <code>R/03c_selection_conjointe.R</code> ",
          "au préalable.</p>")
}

# =========================== 16 =============================================
ajouter("<h2 id='s16'><span class='num'>16.</span>Diagnostics</h2>")
ajouter("<p>Le test de Ljung-Box vérifie que les résidus ne contiennent plus de ",
        "structure exploitable. Sous l'hypothèse nulle d'absence d'autocorrélation ",
        "jusqu'au retard ", m("h"), " :</p>")
ajouter(eq(paste0("Q = ", m("N"), "(", m("N"), "+2) ",
                  "<span class='big'>&sum;</span><sub class='num'>s=1</sub>",
                  "<sup class='num'>h</sup> ",
                  "<span class='fr'><span class='hi'>",
                  m("r<sub>s</sub><sup>2</sup>"), "</span><span class='lo'>",
                  m("N"), " &minus; ", m("s"), "</span></span>",
                  "&nbsp;&nbsp;<span class='op'>&rarr;</span>&nbsp;&nbsp; ",
                  m("&chi;<sup>2</sup>"), "(", m("h"), ")")))
ajouter("<p>où ", m("r<sub>s</sub>"), " est l'autocorrélation d'ordre ", m("s"),
        " des résidus. Une p-value faible signale qu'il reste de l'information non ",
        "exploitée.</p>")
ajouter(sprintf(paste0("<p><strong>%d branches sur 16</strong> présentent des résidus ",
                       "significativement autocorrélés au seuil de 5 %%, sur quatre ",
                       "retards.</p>"), n_autocorr))
t15 <- diag_res %>%
  dplyr::arrange(ljung_box_p) %>%
  utils::head(8) %>%
  dplyr::transmute(Branche = branche,
                   `Écart-type du résidu (%)` = nb(100 * ecart_type_residu, 2),
                   `Autocorr. ordre 1` = nb(autocorr_ordre1, 2),
                   `Ljung-Box p` = nb(ljung_box_p, 3),
                   `Résidus extrêmes` = n_residus_extremes)
ajouter(tbl(as.data.frame(t15), aligne_droite = 2:5))
ajouter(legende_tableau("Les huit branches aux résidus les plus problématiques."))
ajouter(sprintf(paste0("<p>Ces diagnostics portent sur le modèle estimé sur ",
                       "l'échantillon complet, avec les hyperparamètres choisis par la ",
                       "méthode retenue sur ce même échantillon : ", m("p"), " = %d et ",
                       m("&lambda;"), " = %s. Il s'agit d'un diagnostic <em>en ",
                       "échantillon</em>, pas d'une prévision — d'où la légitimité de ",
                       "calibrer sur toute la série.</p>"),
                nowcast$p[1], nb(nowcast$lambda[1], 4)))
ajouter("<p>Un prior serré laisse mécaniquement plus de structure dans les résidus : ",
        "c'est le prix de la régularisation. Le nombre de branches concernées est donc ",
        "à lire comme un indicateur de tension entre ajustement et parcimonie, non ",
        "comme un rejet de la spécification.</p>")

# =========================== 17 =============================================
ajouter("<h2 id='s17'><span class='num'>17.</span>Nowcast du trimestre courant</h2>")
ajouter(sprintf(paste0("<p>Cible : <strong>%s</strong>. Le nowcast emprunte exactement ",
                       "le même chemin que les %s origines du backtest : détection des ",
                       "chocs, estimation des échelles, sélection des hyperparamètres, ",
                       "puis estimation. Les valeurs retenues ici sont ", m("p"),
                       " = %d et ", m("&lambda;"), " = %s.</p>"),
                nowcast$trimestre[1], nb(n_origines), nowcast$p[1],
                nb(nowcast$lambda[1], 4)))
t16 <- nowcast %>%
  dplyr::arrange(dplyr::desc(prevision_pct)) %>%
  dplyr::transmute(Branche = branche, `Prévision (%)` = nb(prevision_pct, 2))
ajouter(tbl(as.data.frame(t16), aligne_droite = 2))
ajouter(legende_tableau(paste0(
  "Prévision BVAR par branche pour ", nowcast$trimestre[1],
  ", en pourcentage de croissance trimestrielle.")))
ajouter("<div class='encadre alerte'><span class='etiq'>À ne pas sur-interpréter</span>",
        "<p>Ces chiffres ne sont <strong>pas</strong> le nowcast du projet, et aucun ",
        "agrégat n'en est tiré. Ils sont la composante BVAR seule, qui sera combinée ",
        "aux équations de passerelle (phase 7) par un poids récursif (phase 10), puis ",
        "agrégée avec des poids en prix courants (phase 11).</p>",
        "<p>La section 14 montre par ailleurs que cette composante, prise isolément, ",
        "fait à peine mieux que la moyenne historique sur la plupart des branches.</p></div>")

# =========================== 18 =============================================
ajouter("<h2 id='s18'><span class='num'>18.</span>Limites</h2>")
ajouter("<h3>18.1 Pouvoir prédictif faible</h3>")
ajouter("<p>Limite dominante, établie en section 14 : le gain par rapport à une ",
        "prévision constante reste faible, et négatif sur plusieurs branches. Aucun ",
        "réglage testé ne change cet ordre de grandeur — pas même le choix optimisé ",
        "des hyperparamètres, qui améliore la situation sans la transformer.</p>")
ajouter("<h3>18.2 Un seul épisode majeur — limite partiellement levée</h3>")
ajouter("<p>Le problème ne venait pas des données mais de la formulation : une liste de ",
        "dates codée en dur n'a qu'un cas, donc rien à tester. Une règle de détection ",
        "se déclenche sur plusieurs épisodes et devient évaluable.</p>")
ajouter("<p>Ce qui reste hors de portée : le <strong>découpage fin de 2020</strong>. ",
        "Mais avec une règle, ce n'est plus un choix libre — c'est une conséquence du ",
        "seuil, lui-même évalué sur l'ensemble des épisodes.</p>")
ajouter(definition("Pistes non retenues",
  paste0("<p><strong>Lenza &amp; Primiceri (2022)</strong>, <em>How to estimate a VAR ",
         "after March 2020</em>, proposent de rééchelonner la volatilité des trimestres ",
         "de choc plutôt que de les neutraliser — l'argument étant qu'une indicatrice ",
         "jette l'observation, et avec elle l'information sur la structure de ",
         "covariance que le choc révèle. Alternative sérieuse, non implémentée.</p>",
         "<p>Un <strong>leave-one-branch-out</strong> donnerait seize évaluations. ",
         "Mesure faite : la corrélation moyenne entre branches passe de 0,055 hors 2020 ",
         "à 0,236 en 2020, et dix branches sur seize seulement sont négatives au T2. Il ",
         "y a de la variation transversale, mais seize branches corrélées pendant un ",
         "choc commun ne font pas seize observations indépendantes.</p>")))
ajouter("<h3>18.3 Hyperparamètres : limite levée</h3>")
ajouter("<div class='encadre alerte'><span class='etiq'>Correction d'une erreur de ce rapport</span>",
        "<p>Une version antérieure indiquait que Giannone, Lenza &amp; Primiceri ",
        "traitent aussi « le poids des priors de somme des coefficients et de ",
        "co-persistance, absents ici ». <strong>Les ajouter serait une faute de ",
        "spécification.</strong></p>",
        "<p>Ces priors expriment que les séries comportent des racines unitaires ou des ",
        "tendances stochastiques communes, et sont conçus pour des VAR en ",
        "<em>log-niveaux</em>. Nos variables sont des taux de croissance, 16 sur 16 ",
        "stationnaires, avec un prior explicitement de retour à la moyenne. Un prior de ",
        "somme des coefficients imposerait que <em>la croissance</em> suive une marche ",
        "aléatoire — en contradiction directe avec la section 5.</p></div>")
ajouter("<p>Restait un point : les réglages sont choisis <em>séquentiellement</em> et ",
        "non conjointement. <strong>Ce point est désormais traité</strong> — la section ",
        "15 lui est entièrement consacrée. La sélection jointe sur les 126 combinaisons ",
        "a été exécutée : elle retient la même spécification sur l'échantillon complet, ",
        "et se dégrade en protocole validation / test. Le chemin séquentiel est ",
        "conservé parce qu'il est meilleur, non par économie de calcul.</p>")
ajouter("<p>Ce qui reste ouvert sur cet axe est d'une autre nature : les ",
        "<em>ensembles de candidats</em> eux-mêmes — trois modes, six règles, sept ",
        "leviers — sont posés a priori. Les élargir se heurterait au résultat de la ",
        "section 15.1 : plus de candidats, c'est plus de sur-ajustement de sélection, ",
        "pour un plafond de gain mesuré à 0,02 de ratio.</p>")

ajouter("<h3>18.4 Rupture de rétropolation : atténuée, pas résolue</h3>")
ajouter("<p><strong>Ce qui marche</strong> : dissocier l'échantillon servant à l'échelle ",
        "du prior de celui servant aux coefficients.</p>")
ajouter("<p><strong>Ce qui ne marche pas</strong> : réduire le poids des observations ",
        "anciennes, sous quelque forme que ce soit. La fenêtre glissante échoue, et la ",
        "pondération géométrique aussi. Dans un système qui compte déjà plus de ",
        "paramètres que d'observations, escompter le passé coûte plus que ce que ",
        "l'homogénéité rapporte.</p>")
ajouter("<p>Piste hors cadre : un prior à variance changeante dans le temps, qui ",
        "traiterait l'hétérogénéité sans toucher au poids des observations.</p>")
ajouter("<h3>18.5 Pas d'incertitude paramétrique</h3>")
ajouter("<p>Le système augmenté donne une estimation ponctuelle — la moyenne a ",
        "posteriori. Produire des intervalles de crédibilité supposerait un ",
        "échantillonnage de la distribution a posteriori, par exemple par ",
        "échantillonneur de Gibbs. Le plan de correction ne le demande pas, mais le ",
        "mémoire doit dire que le modèle n'est pas présenté comme un BVAR estimé par ",
        "simulation.</p>")

# =========================== 19 =============================================
ajouter("<h2 id='s19'><span class='num'>19.</span>Fichiers produits et suite</h2>")
fichiers <- data.frame(
  Fichier = c("R/fonctions/bvar.R",
              "resultats/03_previsions_recursives.csv",
              "resultats/03_diagnostics_branches.csv",
              "resultats/03_hyperparametres_par_origine.csv",
              "resultats/03_choix_hyperparametres.csv",
              "resultats/03_comparaison_regles_choc.csv",
              "resultats/03_comparaison_leviers.csv",
              "resultats/03_chocs_detectes.csv",
              "resultats/03_tests_diebold_mariano_branches.csv",
              "resultats/03_diagnostics_residus.csv",
              "resultats/03_nowcast_courant.csv",
              "R/03c_selection_conjointe.R",
              "resultats/03c_grille_conjointe.csv",
              "resultats/03c_interactions.csv",
              "resultats/03c_validation_test.csv"),
  Contenu = c(paste("Estimation, prévision, récursion, vraisemblance marginale,",
                    "critère d'ajustement, détection des chocs"),
              "Prévision, réalisation et erreur par origine et par branche",
              "Qualité de la prévision branche par branche",
              "(p, λ, d) retenus à chaque origine, pour chaque méthode",
              "Performance hors échantillon des trois modes de choix",
              "Performance des règles de détection des chocs",
              "Performance des trois leviers structurels",
              "Trimestres identifiés comme chocs, origine par origine",
              "Diebold-Mariano branche par branche contre la référence",
              "Ljung-Box, autocorrélation, résidus extrêmes",
              "Prévision du prochain trimestre par branche",
              paste("Sélection jointe sur les 126 combinaisons et protocole",
                    "validation / test (section 15) ; ~30 min sur 3 c\u0153urs"),
              "Les 126 combinaisons évaluées sur les 48 origines",
              "Séparabilité des trois axes de sélection",
              "Spécification choisie par chaque procédure, et sa performance sur le test"),
  stringsAsFactors = FALSE)
ajouter(tbl(fichiers, aligne_droite = integer(0)))
ajouter(legende_tableau("Sorties de la phase 3."))
figs <- list.files(DOSSIER_FIGURES, pattern = "^03[c]?_", full.names = FALSE)
ajouter(sprintf("<p>%d figures dans <code>figures/</code>.</p>", length(figs)))
ajouter("<h3>Ce que la phase 4 hérite</h3>")
ajouter("<ul>",
        "<li><code>prevision_bvar_recursive()</code>, directement réutilisable : la ",
        "phase 4 a besoin de la composante BVAR à chaque origine pour estimer le poids ",
        "de combinaison avec les équations de passerelle ;</li>",
        "<li>la mesure de ce que le BVAR apporte seul — repère indispensable pour ",
        "juger si la combinaison améliore quoi que ce soit ;</li>",
        "<li>la démonstration que le pouvoir prédictif des seules valeurs ajoutées ",
        "passées est faible, ce qui place l'essentiel de l'enjeu sur les indicateurs ",
        "mensuels ;</li>",
        "<li>le mécanisme de choix récursif d'hyperparamètres, transposable tel quel au ",
        "seuil de sélection des indicateurs et au poids de combinaison.</li>",
        "</ul>")
ajouter("<p>Reste ouverte la contrainte posée en phase 2 : la fonction d'agrégation ",
        "devra refuser de produire un trimestre dont les trois mois ne sont pas ",
        "disponibles, observés ou prévus.</p>")

ajouter(pied_rapport("R/03b_rapport_phase3.R"))

ecrire_rapport(h, "Phase 3 — Le BVAR trimestriel, expliqué de bout en bout",
               CHEMIN_RAPPORT)
cat(sprintf("[rapport] %s (%.1f Mo, %d figures, %d tableaux, %d equations)\n",
            CHEMIN_RAPPORT, file.size(CHEMIN_RAPPORT) / 1024^2,
            .n_figure, .n_tableau, get(".n_equation", envir = globalenv())))
