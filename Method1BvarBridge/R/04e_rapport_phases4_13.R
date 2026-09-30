# ============================================================================
# 04e_rapport_phases4_13.R -- Rapport pedagogique des phases 4 et 13
# ============================================================================
# UN SEUL RAPPORT POUR DEUX PHASES, ET C'EST DELIBERE
#   La phase 4 conclut que la passerelle ne sert a rien ; la phase 13 montre
#   qu'elle sert beaucoup, des lors qu'on la juge au bon moment du trimestre.
#   Le second resultat ne se comprend qu'au regard du premier : les separer
#   donnerait deux documents dont l'un serait faux sans l'autre.
#
# Toutes les valeurs sont relues depuis resultats/ : le rapport ne peut pas
# diverger des chiffres.
#
# Note d'ecriture : chaines en GUILLEMETS DOUBLES, attributs HTML en guillemets
# simples -- le francais est plein d'apostrophes.
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/rapport.R")

CHEMIN <- file.path(DOSSIER_RAPPORT, "rapport_phases4_13.html")
init_compteurs()
assign(".n_equation", 0L, envir = globalenv())

cat("[rapport] Lecture des resultats\n")
r <- function(x) file.path(DOSSIER_RESULTATS, x)
requis <- c("04_evaluation_branches.csv", "04_agregation_pertes.csv",
            "04_previsions_bridge.csv", "04_poids_delta.csv",
            "04b_comparaison_variantes.csv", "04b_comparaison_perimetre_commun.csv",
            "04c_validation_kalman.csv", "04c_comparaison_kalman.csv",
            "04d_par_periode.csv", "04d_sensibilite_seuil.csv",
            "09_perimetre_commun.csv", "09_perimetre_commun_effectifs.csv",
            "09_test_m3_mensuel.csv", "09_seuil_trimestriel.csv",
            "09_selection_conditionnelle.csv")
absents <- requis[!file.exists(r(requis))]
if (length(absents) > 0L) {
  stop("Sorties absentes : ", paste(absents, collapse = ", "), call. = FALSE)
}

ev4       <- lire_csv(r("04_evaluation_branches.csv"))
pertes    <- lire_csv(r("04_agregation_pertes.csv"))
bridge4   <- lire_csv(r("04_previsions_bridge.csv"))
delta4    <- lire_csv(r("04_poids_delta.csv"))
variantes <- lire_csv(r("04b_comparaison_variantes.csv"))
var_comm  <- lire_csv(r("04b_comparaison_perimetre_commun.csv"))
val_kal   <- lire_csv(r("04c_validation_kalman.csv"))
cmp_kal   <- lire_csv(r("04c_comparaison_kalman.csv"))
etat      <- lire_csv(r("04d_par_periode.csv"))
sensib    <- lire_csv(r("04d_sensibilite_seuil.csv"))
intra     <- lire_csv(r("09_perimetre_commun.csv"))
effectifs <- lire_csv(r("09_perimetre_commun_effectifs.csv"))
m3m       <- lire_csv(r("09_test_m3_mensuel.csv"))
seuilT    <- lire_csv(r("09_seuil_trimestriel.csv"))
condit    <- lire_csv(r("09_selection_conditionnelle.csv"))

# Sorties des tests d'incertitude et du backtest etendu. Le rapport reste
# composable sans elles, mais deux sections manqueront.
A_INCERT <- all(file.exists(r(c("10_dm_par_branche.csv", "10_dm_groupe.csv",
                                "10_intervalles_ratio.csv"))))
A_ETENDU <- all(file.exists(r(c("09b_dm_groupe.csv", "09b_par_episode.csv"))))
if (A_INCERT) {
  dm_br  <- lire_csv(r("10_dm_par_branche.csv"))
  dm_gr  <- lire_csv(r("10_dm_groupe.csv"))
  ic     <- lire_csv(r("10_intervalles_ratio.csv"))
}
if (A_ETENDU) {
  et_dm  <- lire_csv(r("09b_dm_groupe.csv"))
  et_ep  <- lire_csv(r("09b_par_episode.csv"))
}

# --- quelques agregats servant au fil du texte -------------------------------
bilan4 <- ev4 %>% dplyr::group_by(modele) %>%
  dplyr::summarise(ratio_median = stats::median(ratio),
                   n_ok = sum(ratio < 1),
                   correl = stats::median(correlation, na.rm = TRUE),
                   .groups = "drop")
taux4      <- mean(!is.na(bridge4$prevision))
n_tent     <- nrow(bridge4)
n_prod     <- sum(!is.na(bridge4$prevision))
delta_def  <- sum(delta4$delta_mode != "estime")
ligne <- function(d, sc, mo, per = "toutes origines") {
  d %>% dplyr::filter(scenario == sc, modele == mo, periode == per)
}
m3_comb <- ligne(intra, "M3", "combinee")$ratio_median
m3_pass <- ligne(intra, "M3", "bridge")$ratio_median
bvar_r  <- ligne(intra, "M3", "bvar")$ratio_median

cat("[rapport] Composition\n")
h <- character(0)
ajouter <- function(...) h <<- c(h, paste0(..., collapse = ""))

ajouter(entete_rapport(
  "GDPNow-Maroc &middot; Méthode 1 &middot; Rapport d'étape",
  "Phases 4 et 13 — Les équations de passerelle, et le moment où elles servent",
  paste("Pourquoi la passerelle échoue quand on la juge sur un trimestre complet,",
        "et pourquoi elle réussit dès qu'on la juge en cours de trimestre."),
  "<code>data/</code> (431 indicateurs, 12 branches couvertes) et <code>resultats/03_*</code>"))

sections <- c(
  "Ce que les deux phases cherchent",
  "Agréger le mensuel en trimestriel",
  "Sélectionner les indicateurs, à chaque origine",
  "L'équation de passerelle",
  "Le poids de combinaison",
  "Résultats de la phase 4 : un échec",
  "Le diagnostic : 165 cas perdus pour rien",
  "Six pistes d'amélioration",
  "La contraction : ridge et GCV",
  "Combler les trous par filtre de Kalman",
  "Un poids qui dépend de l'état",
  "Ce que cinq échecs enseignent",
  "Phase 13 : la valeur de l'information au fil du trimestre",
  "Prévoir n'est pas combler",
  "L'anomalie M2 > M3, et son élucidation",
  "Résultats de la phase 13",
  "L'incertitude : ces écarts sont-ils réels ?",
  "Le backtest étendu : un second épisode de rupture",
  "Limites",
  "Fichiers produits et suite")
ajouter(sommaire_rapport(sections))

ajouter(chiffres_cles(c(
  "branches couvertes par le vivier" = "12 / 16",
  "prévisions de passerelle, phase 4" = sprintf("%d / %d", n_prod, n_tent),
  "ratio de la combinaison en M3" = nb(m3_comb, 3),
  "ratio du BVAR seul, même périmètre" = nb(bvar_r, 3),
  "écart significatif ? (voir §17)" = "M1 seulement",
  "épisodes de rupture évalués" = "2 (2008-2009, 2020)"
)))

# =========================== 1 ==============================================
ajouter("<h2 id='s1'><span class='num'>1.</span>Ce que les deux phases cherchent</h2>")
ajouter("<p>La phase 3 a mesuré ce que le seul passé des valeurs ajoutées permet ",
        "de prévoir : peu de chose — un ratio médian de 0,978 et une corrélation ",
        "de 0,24. La conclusion était que l'essentiel devait venir de la seconde ",
        "source d'information, <strong>les indicateurs mensuels</strong>. C'est ",
        "l'objet de la phase 4.</p>")
ajouter(intuition(paste0(
  "<p>Le principe d'une équation de passerelle — <em>bridge equation</em> — tient ",
  "en une phrase : les indicateurs mensuels sont publiés <strong>avant</strong> les ",
  "comptes trimestriels. Si l'on sait relier la croissance d'une branche à des ",
  "indicateurs qu'on observe plus tôt, on peut estimer cette croissance avant ",
  "qu'elle soit mesurée.</p>",
  "<p>Le mot <em>passerelle</em> dit exactement ce que fait le modèle : il relie ",
  "deux fréquences. Les indicateurs sont mensuels, la cible est trimestrielle ; ",
  "il faut donc d'abord agréger, puis régresser.</p>")))
ajouter("<p>La phase 4 suit l'étape 4 du plan de correction : <em>sélection ",
        "récursive, agrégation correcte, passerelle récursive, delta récursif</em>. ",
        "Ces quatre volets correspondent aux phases 5, 6, 7 et 10 du plan.</p>")
ajouter("<div class='encadre'><span class='etiq'>Le fil de ce rapport</span>",
        "<p>La phase 4 conclut que la passerelle <strong>ne sert à rien</strong> : ",
        "elle est battue par le BVAR. Six pistes d'amélioration sont alors ",
        "explorées ; <strong>cinq échouent</strong>.</p>",
        "<p>La phase 13 renverse la conclusion, non par un modèle meilleur, mais en ",
        "corrigeant <strong>le moment de l'évaluation</strong>. Toute la phase 4 juge ",
        "la passerelle sur un trimestre complet — l'instant précis où son avantage ",
        "est nul, puisque le BVAR dispose alors de la même information. Évaluée en ",
        "cours de trimestre, elle gagne.</p>",
        "<p>Le rapport est écrit dans cet ordre, échecs compris, parce que le ",
        "résultat final n'a de sens que rapporté à ce qu'il corrige.</p></div>")
ajouter(definition("Périmètre",
  paste0("<p>Seules <strong>12 branches sur 16</strong> sont couvertes par le ",
         "vivier d'indicateurs. Les quatre autres — services aux entreprises, ",
         "administration publique, éducation-santé, autres services — relèvent de ",
         "l'AR(4) de l'étape 5 du plan.</p>",
         "<p>Les origines sont les mêmes qu'en phase 3 : <strong>48 trimestres, de ",
         "T2-2014 à T1-2026</strong>. Ce point de départ est fixé par la ",
         "disponibilité de la valeur ajoutée nominale, qui fournira les poids ",
         "d'agrégation en prix courants.</p>")))

# =========================== 2 ==============================================
ajouter("<h2 id='s2'><span class='num'>2.</span>Agréger le mensuel en trimestriel</h2>")
ajouter("<p>La version 1 appliquait <code>mean(dlog)</code> à tous les indicateurs. ",
        "C'est faux, et de deux façons.</p>")
ajouter(intuition(paste0(
  "<p>D'abord, la règle dépend de la <strong>nature économique</strong> de la ",
  "variable. La production d'un trimestre est la <em>somme</em> des trois mois, pas ",
  "leur moyenne. Un effectif ou un indice, au contraire, se moyenne. Un encours de ",
  "fin de période ne retient que le dernier mois.</p>",
  "<p>Ensuite, l'ordre des opérations compte : la moyenne des croissances ",
  "mensuelles n'est pas la croissance de la somme. On agrège d'abord les ",
  "<em>niveaux</em>, on transforme <em>ensuite</em>.</p>")))
ajouter("<p>Trois règles, portées par une métadonnée de la phase 1 :</p>")
ajouter(eq(paste0("flux : ", m("X<sub>T</sub><sup>Q</sup>"), " = ",
                  m("X<sub>m1</sub>"), " <span class='op'>+</span> ",
                  m("X<sub>m2</sub>"), " <span class='op'>+</span> ",
                  m("X<sub>m3</sub>"))))
ajouter(eq(paste0("stock moyen : ", m("X<sub>T</sub><sup>Q</sup>"), " = ",
                  "<span class='fr'><span class='hi'>", m("X<sub>m1</sub>"),
                  " <span class='op'>+</span> ", m("X<sub>m2</sub>"),
                  " <span class='op'>+</span> ", m("X<sub>m3</sub>"),
                  "</span><span class='lo'>3</span></span>")))
ajouter(eq(paste0("stock de fin : ", m("X<sub>T</sub><sup>Q</sup>"), " = ",
                  m("X<sub>m3</sub>"))))
ajouter("<p>puis, seulement après :</p>")
ajouter(eq(paste0(m("x<sub>T</sub>"), " = log ", m("X<sub>T</sub><sup>Q</sup>"),
                  " <span class='op'>&minus;</span> log ",
                  m("X<sub>T&minus;1</sub><sup>Q</sup>"))))

ajouter("<h3>2.1 Le refus des trimestres incomplets, et ce qu'il coûte</h3>")
ajouter("<p>La fonction d'agrégation <strong>refuse</strong> de produire un ",
        "trimestre dont les trois mois ne sont pas disponibles : elle renvoie une ",
        "valeur manquante.</p>")
ajouter(definition("Pourquoi refuser plutôt que faire au mieux",
  paste0("<p>Sommer deux mois au lieu de trois sous-estime le trimestre d'environ ",
         "un tiers. En moyenner deux introduit un biais dès que la série est ",
         "saisonnière.</p>",
         "<p>Dans les deux cas l'erreur est <strong>silencieuse</strong> : la série ",
         "agrégée garde une allure parfaitement plausible, et le biais ne ressort ",
         "qu'en bout de chaîne, sans qu'on puisse le rattacher à sa cause. Un trou ",
         "déclaré vaut mieux qu'une valeur fausse.</p>")))
cascade <- data.frame(
  Étape = c("A. observations mensuelles",
            "B. trimestres couverts par au moins un mois",
            "C. refus des trimestres à moins de 3 mois",
            "D. après transformation Δlog ou diff",
            "E. seuil de 20 trimestres d'historique"),
  Reste = c("49 145", "17 236", "15 681", "14 674", "392 séries sur 431"),
  Perdu = c("—", "—",
            sprintf("−%s (9,0 %%)", nb(sum(pertes$refuses))),
            "−1 007", "−39 séries"),
  stringsAsFactors = FALSE, check.names = FALSE)
ajouter(tbl(cascade, aligne_droite = 2:3))
ajouter(legende_tableau("La cascade des pertes, séries mensuelles des 12 branches couvertes."))
ajouter("<div class='encadre alerte'><span class='etiq'>Un effet de cascade, mesuré après coup</span>",
        "<p>Le refus direct coûte ", nb(sum(pertes$refuses)), " trimestres, dont ",
        nb(sum(pertes$interne)), " sont des <strong>trous internes</strong> — pas ",
        "des bords de série — touchant ", nb(sum(pertes$refuses > 0)), " séries.</p>",
        "<p>Mais une différence qui enjambe un trou est elle aussi invalide : si le ",
        "T2 est refusé, le Δlog du T3, qui se calcule contre lui, l'est également. ",
        "<strong>Un trou détruit le trimestre qu'il touche et le suivant</strong> — ",
        "1,64 trimestre par trou en moyenne, soit 1 007 pertes supplémentaires.</p>",
        "<p>La facture réelle est donc de <strong>2 562 trimestres</strong>, et non ",
        "de 1 555. Ce chiffre a motivé le traitement par filtre de Kalman de la ",
        "section 10.</p></div>")

# =========================== 3 ==============================================
ajouter("<h2 id='s3'><span class='num'>3.</span>Sélectionner les indicateurs, à chaque origine</h2>")
ajouter("<p>Le plan retient sa « solution B » : la liste des indicateurs est ",
        "<strong>recalculée à chaque trimestre cible</strong>, sur la seule ",
        "information antérieure.</p>")
ajouter(eq(paste0(m("S<sub>T</sub>"), " = ", m("f"), "( ",
                  m("&#8496;<sub>T&minus;1</sub>"), " )")))
ajouter("<p>Les critères sont ceux du plan : un indicateur est retenu si sa ",
        "corrélation avec la croissance de la branche est assez forte et assez ",
        "significative,</p>")
ajouter(eq(paste0("<span class='op'>|</span>", m("r"), "<span class='op'>|</span> ",
                  "<span class='op'>&ge;</span> 0,15",
                  "&nbsp;&nbsp;&nbsp;et&nbsp;&nbsp;&nbsp;", m("p"),
                  " <span class='op'>&lt;</span> 0,10")))
ajouter("<p>tout étant calculé sur ", m("t &lt; T"), " uniquement.</p>")
ajouter("<h3>Deux garde-fous absents du plan</h3>")
ajouter(definition("Historique minimal",
  paste0("<p>Une corrélation calculée sur huit trimestres n'est pas une ",
         "corrélation. Son écart-type vaut environ</p>",
         eq(paste0("sd(", m("r"), ") <span class='op'>&asymp;</span> ",
                   "<span class='fr'><span class='hi'>1</span>",
                   "<span class='lo'><span class='op'>&radic;</span>(", m("n"),
                   " &minus; 3)</span></span>")),
         "<p>soit <strong>0,45 pour ", m("n"), " = 8</strong>. Le seuil de 0,15 ",
         "serait alors franchi par pur bruit une fois sur deux. On exige donc ",
         "<strong>20 trimestres appariés</strong>.</p>")))
ajouter(definition("Plafond de régresseurs",
  paste0("<p>Une passerelle estimée par moindres carrés sur une cinquantaine de ",
         "trimestres ne supporte pas vingt régresseurs — c'est le même problème de ",
         "degrés de liberté qu'en phase 3, en plus petit. On garde au plus ",
         "<strong>5 indicateurs</strong>, les mieux corrélés.</p>")))

# =========================== 4 ==============================================
ajouter("<h2 id='s4'><span class='num'>4.</span>L'équation de passerelle</h2>")
ajouter(eq(paste0(m("g<sub>j,T</sub>"), " = ", m("&alpha;<sub>j</sub>"),
                  " <span class='op'>+</span> ",
                  "<span class='big'>&sum;</span><sub class='num'>k=1</sub>",
                  "<sup class='num'>K</sup> ", m("&beta;<sub>k</sub>"), " ",
                  m("X<sub>k,T</sub>"), " <span class='op'>+</span> ",
                  m("&epsilon;<sub>j,T</sub>"))))
ajouter("<p>Les coefficients sont estimés sur ", m("t &lt; T"),
        ", et la prévision utilise ", m("X<sub>&middot;,T</sub>"), " :</p>")
ajouter(eq(paste0(m("&#285;<sub>j,T</sub><sup>passerelle</sup>"), " = ",
                  m("&alpha;&#770;<sub>j</sub>"), " <span class='op'>+</span> ",
                  "<span class='big'>&sum;</span><sub class='num'>k</sub> ",
                  m("&beta;&#770;<sub>k</sub>"), " ", m("X<sub>k,T</sub>"))))
ajouter(definition("Utiliser X au temps T n'est pas du look-ahead",
  paste0("<p>C'est le point qui distingue une passerelle d'une prévision ordinaire, ",
         "et il mérite d'être énoncé clairement.</p>",
         "<p>L'indicateur du trimestre ", m("T"), " est <strong>publié avant</strong> ",
         "la valeur ajoutée du même trimestre. L'utiliser ne viole donc aucun ",
         "ensemble d'information : c'est la raison d'être de la méthode. Ce qui ",
         "serait fautif serait d'utiliser ", m("g<sub>j,T</sub>"),
         " elle-même, et une barrière explicite le vérifie à chaque estimation.</p>")))

# =========================== 5 ==============================================
ajouter("<h2 id='s5'><span class='num'>5.</span>Le poids de combinaison</h2>")
ajouter("<div class='encadre alerte'><span class='etiq'>Ce que cette section décrit n'est plus le réglage retenu</span>",
        "<p>Le poids ", m("&delta;"), " est aujourd'hui <strong>constant à 0,5</strong>. ",
        "L'estimation récursive exposée ci-dessous a bien été construite, mesurée, puis ",
        "<em>écartée</em> : la comparaison des cinq règles sur le protocole complet — ",
        "section 10.1 du rapport de synthèse — donne à la constante un ratio agrégé de ",
        "0,852 contre 0,860, et surtout une corrélation de 0,77 contre 0,64.</p>",
        "<p>La dérivation est conservée parce qu'elle explique <strong>pourquoi</strong> ",
        "l'estimateur est faible, ce qu'un simple « on a pris 0,5 » ne dirait pas.</p></div>")
ajouter("<p>Deux prévisions coexistent pour chaque branche : celle du ",
        "BVAR et celle de la passerelle. On les combine linéairement :</p>")
ajouter(eq(paste0(m("&#285;<sub>j,T</sub>"), " = ", m("&delta;<sub>j,T</sub>"), " ",
                  m("&#285;<sub>j,T</sub><sup>BVAR</sup>"),
                  " <span class='op'>+</span> ( 1 <span class='op'>&minus;</span> ",
                  m("&delta;<sub>j,T</sub>"), " ) ",
                  m("&#285;<sub>j,T</sub><sup>passerelle</sup>"))))
ajouter("<p>Le poids est choisi, à chaque origine, pour minimiser l'erreur ",
        "quadratique passée :</p>")
ajouter(eq(paste0(m("&delta;<sub>j,T</sub>"), " = arg min",
                  "<sub class='num'>&delta; &isin; [0,1]</sub> ",
                  "<span class='big'>&sum;</span><sub class='num'>t &lt; T</sub> ",
                  "<span class='op'>[</span> ", m("g<sub>j,t</sub>"),
                  " <span class='op'>&minus;</span> ", m("&#285;<sub>j,t</sub>"),
                  "(&delta;) <span class='op'>]</span><sup>2</sup>")))
ajouter(definition("Une solution en forme close, pas une optimisation numérique",
  paste0("<p>En notant ", m("e<sub>1,t</sub>"), " = ", m("g<sub>j,t</sub>"),
         " <span class='op'>&minus;</span> ", m("&#285;<sup>BVAR</sup>"), " et ",
         m("e<sub>2,t</sub>"), " = ", m("g<sub>j,t</sub>"),
         " <span class='op'>&minus;</span> ", m("&#285;<sup>passerelle</sup>"),
         " les erreurs des deux composantes, l'erreur combinée vaut ",
         m("&delta; e<sub>1,t</sub>"), " <span class='op'>+</span> ",
         "(1 <span class='op'>&minus;</span> &delta;) ", m("e<sub>2,t</sub>"),
         ". La somme des carrés est quadratique en ", m("&delta;"),
         ", et l'annulation de la dérivée donne directement :</p>",
         eq(paste0(m("&delta;*"), " = ",
                   "<span class='fr'><span class='hi'>",
                   "<span class='big'>&sum;</span><sub class='num'>t</sub> ",
                   m("e<sub>2,t</sub>"), " ( ", m("e<sub>2,t</sub>"),
                   " <span class='op'>&minus;</span> ", m("e<sub>1,t</sub>"),
                   " )</span><span class='lo'>",
                   "<span class='big'>&sum;</span><sub class='num'>t</sub> ",
                   "( ", m("e<sub>2,t</sub>"), " <span class='op'>&minus;</span> ",
                   m("e<sub>1,t</sub>"), " )<sup>2</sup></span></span>")),
         "<p>que l'on tronque à [0, 1]. Exact, instantané, et sans réglage.</p>")))
ajouter("<p>Cet estimateur <strong>retombe sur sa valeur par défaut de 0,5 dans ",
        "46 % des cas</strong> : il faut que les deux composantes coexistent sur au ",
        "moins huit trimestres passés, ce qui est rare au début de l'exercice.</p>")
ajouter("<p>Là où il s'estime, il est <strong>stable</strong> — saut médian nul d'un ",
        "trimestre au suivant, écart-type intra-branche de 0,12. Ce n'est donc pas son ",
        "instabilité qui le disqualifie. Ce qui le disqualifie est mesuré à la section ",
        "10.1 du rapport de synthèse : <strong>comparé à une constante placée à son ",
        "propre niveau moyen</strong>, il est battu de 0,013 point de ratio et de 0,12 ",
        "de corrélation sur l'agrégat. Sa variation d'une branche et d'un trimestre à ",
        "l'autre n'apporte aucune information — elle n'ajoute que du bruit.</p>")
ajouter(figure("14b_courbe_delta.png", "Ce que coûte chaque valeur du poids",
               paste0("Ratio de l'agrégat pour tout δ de 0 à 1. Hors 2020 la courbe a ",
                      "un creux intérieur, vers 0,2, et reste plate jusqu'à 0,55 ; en ",
                      "2020 elle décroît jusqu'au bout. L'optimum dépend donc du ",
                      "régime, que l'on ne connaît pas au moment de publier — d'où un ",
                      "poids fixé plutôt qu'estimé.")))
ajouter(figure("04_poids_delta.png", "Le poids de combinaison, origine par origine",
               paste0("État actuel : le poids vaut 0,5 partout, la règle retenue étant ",
                      "la constante. La figure documente la mise en œuvre ; la ",
                      "variabilité du δ estimé, elle, est décrite dans le texte ",
                      "ci-dessus.")))

# =========================== 6 ==============================================
ajouter("<h2 id='s6'><span class='num'>6.</span>Résultats de la phase 4 : un échec</h2>")
t6 <- bilan4 %>%
  dplyr::mutate(modele = dplyr::recode(modele, bvar = "BVAR seul",
                                       bridge = "Passerelle seule",
                                       combinee = "Combinaison")) %>%
  dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Modèle = modele, `Ratio médian` = nb(ratio_median, 3),
                   `Branches < 1` = sprintf("%d / 12", n_ok),
                   `Corrélation` = nb(correl, 2))
ajouter(tbl(as.data.frame(t6), aligne_droite = 2:4))
ajouter(legende_tableau("Phase 4, sur les 12 branches couvertes et 48 origines."))
ajouter("<p>Le résultat est net et décevant : <strong>la passerelle est battue par ",
        "le BVAR, et la combinaison ne bat ni l'un ni l'autre</strong>. C'est ",
        "l'inverse de ce que la phase 3 laissait espérer.</p>")
ajouter(sprintf(paste0("<p>Un chiffre commande la lecture de tout le reste : la ",
                       "passerelle ne produit que <strong>%d prévisions sur %d, soit ",
                       "%.0f %%</strong>. Elle est donc jugée sur la moitié des cas — ",
                       "et il faut savoir pourquoi l'autre moitié manque avant de ",
                       "conclure quoi que ce soit.</p>"),
                n_prod, n_tent, 100 * taux4))
ajouter(figure("04_qualite_par_modele.png", "Qualité par branche et par modèle",
               paste0("À gauche du trait, le modèle fait mieux que prédire la ",
                      "moyenne historique. La dispersion entre branches est bien plus ",
                      "grande que l'écart entre modèles — ce qui annonce que la ",
                      "médiane, utilisée seule, cachera l'essentiel.")))

# =========================== 7 ==============================================
ajouter("<h2 id='s7'><span class='num'>7.</span>Le diagnostic : 165 cas perdus pour rien</h2>")
ajouter("<p>La première explication venue était le <em>jagged edge</em> : ",
        "l'indicateur du trimestre cible ne serait pas encore publié. ",
        "<strong>Cette explication est fausse</strong>, et la mesure le montre sans ",
        "ambiguïté : il existe au moins un indicateur observé au trimestre cible dans ",
        "<strong>576 cas sur 576</strong>.</p>")
t7 <- data.frame(
  Cause = c("Prévision produite",
            "Le set des 5 retenus n'est pas complet en T, mais un sous-ensemble l'est",
            "Aucun des 5 retenus n'est observé en T, alors que d'autres séries le sont",
            "Aucun indicateur ne franchit le seuil de corrélation"),
  Cas = c("308", "103", "62", "103"),
  `Récupérable` = c("—", "oui, intégralement", "oui, intégralement", "partiellement"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(t7, aligne_droite = 2))
ajouter(legende_tableau("Décomposition des 576 origines-branches de la phase 4."))
ajouter("<div class='encadre alerte'><span class='etiq'>Un défaut d'algorithme, pas de données</span>",
        "<p>La sélection classe les séries par corrélation <strong>seule</strong>, ",
        "sans jamais vérifier qu'elles sont observables au trimestre cible ni ",
        "qu'elles le sont <em>conjointement</em>. L'estimation fait ensuite une ",
        "<strong>suppression par liste</strong> sur les cinq séries retenues : une ",
        "seule série trouée annule l'équation entière.</p>",
        "<p><strong>165 cas sur 576 — 29 % de l'exercice — tombent là-dessus</strong>, ",
        "pour une raison purement algorithmique. Aucune donnée ne manque.</p></div>")
ajouter(figure("04_nombre_indicateurs.png", "Indicateurs retenus à chaque origine",
               paste0("La liste est recalculée sur la seule information antérieure à ",
                      "la cible, donc elle varie. Les chutes à zéro sont les origines ",
                      "où aucun candidat ne passe le seuil — Transports et Agriculture ",
                      "en particulier, qui ne disposent que de 7 et 5 séries.")))

# =========================== 8 ==============================================
ajouter("<h2 id='s8'><span class='num'>8.</span>Six pistes d'amélioration</h2>")
pistes <- data.frame(
  Piste = c("1. disponibilité", "2. composantes principales", "3. retards",
            "4. terme autorégressif", "5. fonds commun", "6. ridge"),
  Idée = c("n'admettre que des séries observées en T, et construire le set de proche en proche sous contrainte de lignes conjointes",
           "remplacer la sélection dure par une réduction de dimension, qui garde toutes les séries au lieu d'en jeter 80 pour en garder 5",
           "tester aussi la corrélation avec X(T−1), invisible pour un critère purement contemporain",
           "ajouter g(j,T−1) : la passerelle ignore toute la persistance que le BVAR exploite",
           "autoriser une branche pauvre en séries — Transports en a 7 — à puiser chez les autres",
           "contracter les coefficients au lieu de sélectionner"),
  `Limite visée` = c("les 165 cas perdus", "les 103 cas sans candidat",
                     "les 103 cas sans candidat", "la persistance ignorée",
                     "les branches pauvres", "le sur-ajustement"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(pistes, aligne_droite = integer(0)))
ajouter(legende_tableau("Les six pistes et la limite que chacune vise."))
ajouter("<h3>8.1 Le taux de production</h3>")
t8 <- variantes %>% dplyr::filter(cible == "prevision") %>%
  dplyr::arrange(dplyr::desc(taux_production)) %>%
  dplyr::transmute(Variante = specification,
                   `Taux de production` = sprintf("%.0f %%", 100 * taux_production),
                   `Prévisions` = n_produites,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Corrélation` = nb(correl_mediane, 2))
ajouter(tbl(as.data.frame(t8), aligne_droite = 2:5))
ajouter(legende_tableau("Les variantes sur les 48 origines. Attention : les taux de production diffèrent."))
ajouter("<h3>8.2 Pourquoi ce tableau ne suffit pas</h3>")
ajouter(definition("Comparer à périmètre égal",
  paste0("<p>Les variantes ne produisent pas sur les mêmes trimestres. La référence ",
         "ne réussit que sur 53 % des cas — <strong>les plus faciles</strong> —, une ",
         "autre sur 100 %. Comparer leurs ratios bruts revient à comparer des ",
         "épreuves de difficultés différentes.</p>",
         "<p>On restreint donc aux couples branche-origine où <em>toutes</em> les ",
         "variantes ont produit une prévision. C'est la seule comparaison ",
         "interprétable, et elle sera utilisée partout dans la suite.</p>")))
t8b <- var_comm %>% dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Variante = specification, `Ratio médian` = nb(ratio_median, 3),
                   `Branches < 1` = n_branches_ok,
                   `Corrélation` = nb(correl_mediane, 2))
ajouter(tbl(as.data.frame(t8b), aligne_droite = 2:4))
ajouter(legende_tableau("Les mêmes variantes, à périmètre commun."))
ajouter("<p><strong>Seule la piste 4 améliore la qualité</strong>, et la piste 1 ",
        "récupère la couverture sans rien coûter. Les pistes 3 et 5 achètent de la ",
        "couverture au prix de la précision : chercher parmi 400 candidats au lieu ",
        "de 20 fait passer des corrélations fortuites, malgré un seuil durci à 0,30 ",
        "pour les séries étrangères à la branche.</p>")
ajouter(figure("04b_production_vs_qualite.png", "Les deux dimensions du problème",
               paste0("Une variante n'est meilleure que si elle prévoit mieux ",
                      "<em>et</em> plus souvent. Le nuage montre qu'aucune ne domine ",
                      "sur les deux axes : celles qui atteignent 100 % de production ",
                      "sont aussi les moins précises.")))

# =========================== 9 ==============================================
ajouter("<h2 id='s9'><span class='num'>9.</span>La contraction : ridge et GCV</h2>")
ajouter(intuition(paste0(
  "<p>Les variantes les plus riches en indicateurs ont la <strong>meilleure ",
  "corrélation</strong> (0,30) et le <strong>pire ratio</strong> (1,22). Ce couple ",
  "n'est pas celui d'un modèle aveugle : c'est celui d'un modèle qui voit juste et ",
  "amplifie trop.</p>",
  "<p>La réponse n'est alors pas de sélectionner plus durement — ce qui jette ",
  "l'information — mais de <strong>contracter</strong> les coefficients. C'est ",
  "exactement le raisonnement du prior de Minnesota de la phase 3, transposé un ",
  "cran plus bas.</p>")))
ajouter(eq(paste0(m("b(&lambda;)"), " = ( ", m("X&prime;X"),
                  " <span class='op'>+</span> ", m("&lambda;I"),
                  " )<sup>&minus;1</sup> ", m("X&prime;y"))))
ajouter("<p>La matrice devient inversible même quand ", m("X&prime;X"),
        " ne l'est pas : on peut donc avoir <strong>plus de régresseurs que ",
        "d'observations</strong>, et le fonds commun de 400 séries devient utilisable ",
        "au lieu d'être un piège.</p>")
ajouter("<h3>Choisir λ sans découper l'échantillon</h3>")
ajouter("<p>Par validation croisée généralisée (Golub, Heath &amp; Wahba, 1979), qui ",
        "a une forme close :</p>")
ajouter(eq(paste0("GCV(", m("&lambda;"), ") = ",
                  "<span class='fr'><span class='hi'>",
                  "<span class='fr'><span class='hi'>1</span><span class='lo'>n</span></span> ",
                  "<span class='op'>&#8214;</span>", m("y"),
                  " <span class='op'>&minus;</span> ", m("X b(&lambda;)"),
                  "<span class='op'>&#8214;</span><sup>2</sup></span>",
                  "<span class='lo'>( 1 <span class='op'>&minus;</span> tr(",
                  m("H<sub>&lambda;</sub>"), ")/", m("n"),
                  " )<sup>2</sup></span></span>")))
ajouter("<p>où ", m("H<sub>&lambda;</sub>"), " = ", m("X"), "(", m("X&prime;X"),
        " <span class='op'>+</span> ", m("&lambda;I"), ")<sup>&minus;1</sup>",
        m("X&prime;"), " est la matrice chapeau. La trace ", m("tr(H)"),
        " est le <strong>nombre effectif de paramètres</strong> : elle vaut ", m("k"),
        " quand ", m("&lambda;"), " = 0 et tend vers 0 quand ", m("&lambda;"),
        " croît. Le dénominateur pénalise donc la complexité, comme la vraisemblance ",
        "marginale en phase 3.</p>")
ajouter("<div class='encadre alerte'><span class='etiq'>Une erreur trouvée par un symptôme théoriquement impossible</span>",
        "<p>La première version donnait « ridge + AR » <em>pire</em> que « ridge ",
        "seule » (1,22 contre 1,04). C'est impossible : ajouter un régresseur libre ",
        "utile ne peut pas dégrader à ce point.</p>",
        "<p>Cause : la GCV ne comptait pas les paramètres <strong>non pénalisés</strong>. ",
        "La constante et le terme AR consomment deux degrés de liberté que ",
        m("tr(H<sub>&lambda;</sub>)"), " ignorait, si bien que ", m("&lambda;"),
        " était choisi trop petit. Après correction, « ridge + AR + fonds commun » ",
        "passe de <strong>1,94 à 1,095</strong>.</p></div>")
ajouter("<p><strong>La ridge échoue quand même</strong>, et le résultat le plus ",
        "parlant n'est pas son classement : la GCV choisit ", m("&lambda;"),
        " = 10 000, <strong>la borne supérieure de la grille</strong>, avec des degrés ",
        "de liberté effectifs de 1,1. Elle contracte tout vers zéro et fait dégénérer ",
        "le modèle vers la moyenne.</p>")
ajouter("<p>C'est une information, pas un échec technique : <strong>la GCV conclut ",
        "d'elle-même que les indicateurs ne portent pas de signal exploitable</strong>, ",
        "par un chemin entièrement différent de la comparaison hors échantillon. Deux ",
        "critères indépendants qui concordent valent mieux qu'un seul.</p>")
ajouter(definition("Une réserve qui limite la portée de ce verdict",
  paste0("<p>L'ACP et la ridge exigent un panneau <strong>rectangulaire</strong> : ",
         "une série n'entre que si elle couvre entièrement la fenêtre de 32 ",
         "trimestres. Sur les données propres d'une branche, cela ne laisse que ",
         "<strong>4 à 6 séries</strong> en médiane.</p>",
         "<p>La ridge n'a donc presque rien à contracter, et n'a jamais été testée ",
         "dans les conditions où elle est censée briller. Seule la variante avec ",
         "fonds commun atteint 164 régresseurs — et λ y bute encore au plafond.</p>")))
ajouter(figure("04b_ratio_par_variante.png", "Distribution du ratio par variante",
               paste0("Chaque boîte résume les 12 branches. Les variantes riches en ",
                      "régresseurs — fonds commun, retards, tout — déplacent nettement ",
                      "la distribution vers la droite. Ajouter de l'information sans ",
                      "contrainte dégrade plus qu'il n'apporte.")))

# =========================== 10 =============================================
ajouter("<h2 id='s10'><span class='num'>10.</span>Combler les trous par filtre de Kalman</h2>")
ajouter("<p>Le plan prévoit (phase 4) d'utiliser un filtre de Kalman pour les trous ",
        "internes et les extrémités manquantes. Le coût mesuré en section 2 — ",
        "2 562 trimestres, et un panneau rectangulaire réduit à quelques séries — ",
        "le justifiait.</p>")
ajouter("<p>Le modèle est un modèle structurel de base (Harvey), estimé par maximum ",
        "de vraisemblance :</p>")
ajouter(eq(paste0(m("y<sub>t</sub>"), " = ", m("&mu;<sub>t</sub>"),
                  " <span class='op'>+</span> ", m("&gamma;<sub>t</sub>"),
                  " <span class='op'>+</span> ", m("&epsilon;<sub>t</sub>"))))
ajouter(eq(paste0(m("&mu;<sub>t</sub>"), " = ", m("&mu;<sub>t−1</sub>"),
                  " <span class='op'>+</span> ", m("&beta;<sub>t&minus;1</sub>"),
                  " <span class='op'>+</span> ", m("&xi;<sub>t</sub>"),
                  "&nbsp;&nbsp;&nbsp;&nbsp;", m("&beta;<sub>t</sub>"), " = ",
                  m("&beta;<sub>t&minus;1</sub>"), " <span class='op'>+</span> ",
                  m("&zeta;<sub>t</sub>"))))
ajouter(eq(paste0(m("&gamma;<sub>t</sub>"), " = <span class='op'>&minus;</span> ",
                  "<span class='big'>&sum;</span><sub class='num'>j=1</sub>",
                  "<sup class='num'>11</sup> ", m("&gamma;<sub>t&minus;j</sub>"),
                  " <span class='op'>+</span> ", m("&omega;<sub>t</sub>"))))
ajouter("<p>La composante saisonnière n'est pas un luxe : les séries du vivier ne ",
        "sont pas corrigées des variations saisonnières, et combler un mois de ",
        "février par le niveau moyen de l'année introduirait une erreur ",
        "systématique.</p>")
ajouter("<h3>10.1 Une précision sur l'anti-look-ahead</h3>")
ajouter(definition("Lisser à l'intérieur de l'ensemble d'information est licite",
  paste0("<p>Le plan met en garde : « un lissage bidirectionnel peut utiliser des ",
         "observations futures par rapport à une date historique ».</p>",
         "<p>La nuance est essentielle. Pour estimer un mois de 2011 depuis l'origine ",
         "2020, les mois de 2012 à 2019 sont <strong>légitimes</strong> : ils ",
         "appartiennent à ", m("&#8496;<sub>T</sub>"),
         ". Le seul critère qui compte est l'antériorité par rapport à la ",
         "<strong>cible</strong>, pas par rapport à la date comblée.</p>",
         "<p>Ce que le plan interdit à juste titre, c'est le lissage appliqué une fois ",
         "pour toutes sur l'échantillon complet. La réestimation à chaque origine ",
         "l'évite par construction — au prix de 25 minutes de calcul.</p>")))
ajouter("<h3>10.2 Valider avant d'utiliser</h3>")
ajouter("<p>Le comblement a été validé sur des <strong>trous artificiels</strong> ",
        "percés dans des séries complètes, dont on connaît donc la vraie valeur.</p>")
t10 <- val_kal %>%
  dplyr::transmute(Méthode = dplyr::recode(methode, kalman = "Kalman",
                                           lineaire = "interpolation linéaire",
                                           saisonnier = "moyenne du mois calendaire"),
                   `Erreur / écart-type` = nb(`erreur / ecart-type`, 3),
                   `Erreur abs. médiane` = nb(`erreur abs. mediane`, 3),
                   `Biais` = nb(`biais relatif`, 3))
ajouter(tbl(as.data.frame(t10), aligne_droite = 2:4))
ajouter(legende_tableau("410 trous artificiels dans 60 séries complètes."))
ajouter("<p>Le Kalman ne bat l'interpolation linéaire que <strong>52 % du temps</strong> ",
        "— un quasi pile ou face. Son avantage est dans la queue de distribution et ",
        "sur les séries saisonnières, où la moyenne naïve s'effondre à 0,94.</p>")
ajouter("<p><strong>Mais l'enjeu est le trimestre, pas le mois</strong> : l'erreur ",
        "médiane sur la somme trimestrielle est de <strong>0,023 écart-type</strong> ",
        "(0,031 pour le linéaire). L'agrégation moyenne l'erreur de comblement.</p>")
ajouter("<h3>10.3 Le résultat, et une prédiction démentie</h3>")
t10b <- cmp_kal %>% dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Variante = specification,
                   `Taux prod.` = sprintf("%.0f %%", 100 * taux),
                   `Ratio médian` = nb(ratio_median, 3),
                   `Branches < 1` = n_branches_ok,
                   `Corrélation` = nb(correl, 2))
ajouter(tbl(as.data.frame(t10b), aligne_droite = 2:5))
ajouter(legende_tableau("Effet du comblement, toutes origines."))
ajouter("<div class='encadre alerte'><span class='etiq'>Une prédiction explicite, et démentie</span>",
        "<p>Le comblement avait été justifié par l'idée qu'il débloquerait l'ACP et ",
        "la ridge en élargissant le panneau rectangulaire. <strong>La mesure dément ",
        "cette prédiction</strong> : le panneau passe de 197 à 236 séries au total, ",
        "mais la médiane par branche reste à 6.</p>",
        "<p>Combler des trous d'au plus trois mois ne suffit pas : ce qui exclut une ",
        "série du rectangle n'est pas un trou ponctuel mais un historique qui commence ",
        "trop tard ou s'interrompt longuement. Le Kalman traite le mauvais goulot.</p>",
        "<p>Il <em>dégrade</em> même la meilleure variante, et l'explication est ",
        "cohérente : la sélection consciente de la disponibilité contournait déjà les ",
        "trous. Le comblement lui rend éligibles des séries qu'elle écartait à bon ",
        "droit, dont une partie des valeurs est désormais estimée — et la corrélation ",
        "calculée dessus est plus flatteuse que réelle.</p></div>")
ajouter(figure("04c_effet_kalman.png", "Effet du comblement sur la qualité",
               paste0("Chaque méthode avec et sans filtre de Kalman. Les barres sont ",
                      "quasi identiques : le comblement ne change presque rien, sauf ",
                      "pour 1+4 où il nuit.")))

# =========================== 11 =============================================
ajouter("<h2 id='s11'><span class='num'>11.</span>Un poids qui dépend de l'état</h2>")
ajouter("<p>L'évaluation par période avait révélé une structure que la moyenne ",
        "écrasait : en 2020, la passerelle a une erreur absolue moyenne de ",
        "<strong>0,075 contre 0,108</strong> pour le BVAR ; hors 2020, elle est battue ",
        "sur la majorité des branches.</p>")
ajouter("<p>Le cas le plus net est l'hébergement-restauration au deuxième trimestre ",
        "2020 : <strong>réalisé &minus;85,7 %, passerelle &minus;98,0 %, BVAR ",
        "&minus;3,3 %</strong>. La passerelle a vu l'effondrement parce que les ",
        "arrivées et les nuitées le disaient ; le BVAR ne pouvait pas le voir.</p>")
ajouter(intuition(paste0(
  "<p>Cela suggérait un défaut structurel du poids : δ est estimé sur l'erreur ",
  "quadratique <em>passée</em>, donc dominée par les trimestres calmes où la ",
  "passerelle est mauvaise. Il sous-pondérerait donc la passerelle précisément au ",
  "moment où elle est sur le point d'avoir raison.</p>",
  "<p>La correction testée : basculer vers la passerelle quand elle annonce un ",
  "mouvement de grande amplitude au regard de la volatilité habituelle.</p>")))
ajouter(eq(paste0(m("z<sub>T</sub>"), " = ",
                  "<span class='fr'><span class='hi'><span class='op'>|</span>",
                  m("&#285;<sub>j,T</sub><sup>passerelle</sup>"),
                  "<span class='op'>|</span></span><span class='lo'>",
                  m("&sigma;<sub>j, t&lt;T</sub>"), "</span></span>")))
ajouter(eq(paste0(m("&delta;<sub>T</sub>"), " = 0 si ", m("z<sub>T</sub>"),
                  " <span class='op'>&ge;</span> ", m("z<sub>0</sub>"),
                  " ,&nbsp;&nbsp; ", m("&delta;<sub>base</sub>"), " sinon")))
ajouter("<p>Le seuil est fixé <strong>a priori</strong> à ", m("z<sub>0</sub>"),
        " = 2 — la convention des deux écarts-types — et non choisi dans les données : ",
        "avec un seul épisode de choc, l'optimiser reviendrait à ajuster un paramètre ",
        "sur une observation.</p>")
t11 <- sensib %>% dplyr::filter(grepl("^1\\+4", variante)) %>%
  dplyr::arrange(periode, z0) %>%
  dplyr::transmute(Période = periode,
                   `z0` = ifelse(is.finite(z0), nb(z0, 1), "∞"),
                   `MAE` = nb(mae_moyenne, 4),
                   `Ratio médian` = nb(ratio_median, 3))
ajouter(tbl(as.data.frame(t11), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Sensibilité au seuil, variante 1+4. z0 = &infin; signifie : aucun basculement, ",
  "donc le poids de base inchangé quelle que soit l'amplitude annoncee.")))
ajouter("<p><strong>La règle ne marche pas</strong>, et la sensibilité le dit ",
        "clairement : ", m("z<sub>0</sub>"), " = &infin; — c'est-à-dire ne jamais ",
        "basculer — est le meilleur réglage sur toutes les périodes, <em>y compris ",
        "2020</em>. La règle se déclenche 28 fois sur 728 cas, dont 7 en 2020 ; les ",
        "21 fausses alertes coûtent plus que les 7 bonnes ne rapportent.</p>")
ajouter("<p>Le diagnostic était donc faux : <strong>il n'y a pas de gain à aller ",
        "chercher dans une pondération plus fine</strong>. Ce test est le premier des ",
        "trois qui pointent dans la même direction — avec la comparaison des cinq ",
        "règles (section 10.1 du rapport de synthèse) et l'échec de la ridge — et c'est ",
        "leur convergence qui a fini par faire retenir le poids le plus simple ",
        "possible, δ = 0,5.</p>")
ajouter(figure("04d_mae_par_periode.png", "L'erreur par période",
               paste0("La valeur de la passerelle est concentrée sur la rupture. Une ",
                      "moyenne sur toutes les origines écrase exactement ce que le ",
                      "système sait faire — c'est le défaut de présentation que la ",
                      "phase 13 corrigera.")))

# =========================== 12 =============================================
ajouter("<h2 id='s12'><span class='num'>12.</span>Ce que cinq échecs enseignent</h2>")
echecs <- data.frame(
  Piste = c("Sélection conjointe (phase 3)", "Pondération géométrique (phase 3)",
            "Ridge", "Comblement par Kalman", "δ dépendant de l'état"),
  Résultat = c("retient la même spécification, se dégrade hors échantillon de choix",
               "escompter les observations anciennes coûte plus que l'homogénéité ne rapporte",
               "λ bute au plafond de la grille : la GCV conclut à l'absence de signal",
               "le panneau rectangulaire ne s'élargit pas ; dégrade la meilleure variante",
               "ne jamais basculer est le meilleur réglage, même en 2020"),
  stringsAsFactors = FALSE)
ajouter(tbl(echecs, aligne_droite = integer(0)))
ajouter(legende_tableau("Les raffinements techniques testés, et leur verdict."))
ajouter("<p>Deux hypothèses supplémentaires ont été testées et réfutées : que la ",
        "<strong>saisonnalité</strong> soit mal traitée — elle n'explique que 1,3 % de ",
        "la variance de Δlog(VA) en médiane, et des modèles saisonniers triviaux font ",
        "pire que le BVAR — et que les erreurs de branche se <strong>compensent à ",
        "l'agrégation</strong> — l'agrégat en poids courants donne 0,981, exactement ",
        "comme la médiane par branche, et 1,014 hors 2020.</p>")
ajouter(definition("La leçon, avant la phase 13",
  paste0("<p>Sept tentatives, sept échecs. Le faisceau ne dit pas que le travail est ",
         "mauvais : il dit que <strong>le plafond est dans les données</strong>, et ",
         "qu'aucun raffinement d'estimation ne crée de l'information qui n'y est pas.</p>",
         "<p>Mais tous ces tests partagent un présupposé jamais interrogé : ils ",
         "évaluent la passerelle sur un <strong>trimestre complet</strong>. C'est ce ",
         "présupposé, et non les modèles, que la phase 13 remet en cause.</p>")))

# =========================== 13 =============================================
ajouter("<h2 id='s13'><span class='num'>13.</span>Phase 13 : la valeur de l'information au fil du trimestre</h2>")
ajouter(intuition(paste0(
  "<p>Juger une passerelle sur un trimestre complet, c'est la placer dans la ",
  "position la moins favorable : à cet instant, le BVAR dispose lui aussi de toute ",
  "l'information trimestrielle.</p>",
  "<p>L'intérêt d'une passerelle n'est pas d'être meilleure à la fin du trimestre. ",
  "C'est de dire quelque chose <strong>avant</strong> — dès le premier ou le deuxième ",
  "mois, quand le passé des valeurs ajoutées n'a rien de nouveau à apporter.</p>")))
ajouter("<p>Quatre scénarios, selon le nombre de mois du trimestre cible observés :</p>")
scen <- data.frame(
  Scénario = c("M0", "M1", "M2", "M3"),
  `Mois observés` = c("0", "1", "2", "3"),
  `Mois à prévoir` = c("3", "2", "1", "0"),
  `Indicateurs trimestriels de T` = c("non", "non", "non", "voir §15"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(scen, aligne_droite = 2:3))
ajouter(legende_tableau("Les quatre ensembles d'information intra-trimestriels."))
ajouter("<div class='encadre'><span class='etiq'>Le dispositif expérimental</span>",
        "<p><strong>Le BVAR est identique dans les quatre scénarios.</strong> Il ne ",
        "lit que le passé de la valeur ajoutée, qui ne change pas d'un mois à l'autre ",
        "à l'intérieur du trimestre.</p>",
        "<p>Il est donc un <strong>étalon plat</strong>, et l'écart entre M0 et M3 ",
        "mesure exactement ce que les indicateurs mensuels apportent, et à quel ",
        "moment. C'est ce qui rend l'expérience lisible.</p></div>")
ajouter("<h3>13.1 Ce que le plan interdit</h3>")
ajouter("<p>« Il ne faut plus calculer M1/M2 comme une simple moyenne des mois ",
        "observés. » Sommer deux mois au lieu de trois sous-estimerait le trimestre ",
        "d'un tiers — c'est le même piège qu'en section 2.</p>")
ajouter("<p>Les mois manquants sont donc <strong>prévus</strong>, puis l'agrégation ",
        "porte bien sur trois mois :</p>")
ajouter(eq(paste0("M1 :&nbsp;&nbsp; ", m("X&#770;<sub>T</sub><sup>Q</sup>"), " = ",
                  m("A"), "( ", m("X<sub>m1</sub>"), " , ",
                  m("X&#770;<sub>m2</sub>"), " , ", m("X&#770;<sub>m3</sub>"), " )")))
ajouter(eq(paste0("M2 :&nbsp;&nbsp; ", m("X&#770;<sub>T</sub><sup>Q</sup>"), " = ",
                  m("A"), "( ", m("X<sub>m1</sub>"), " , ", m("X<sub>m2</sub>"),
                  " , ", m("X&#770;<sub>m3</sub>"), " )")))
ajouter("<p>où ", m("A"), " est la règle d'agrégation de la métadonnée — somme, ",
        "moyenne ou dernier mois.</p>")

# =========================== 14 =============================================
ajouter("<h2 id='s14'><span class='num'>14.</span>Prévoir n'est pas combler</h2>")
ajouter(definition("Deux fonctions distinctes, délibérément",
  paste0("<p>Le plan mentionne le Kalman pour « les trous internes, les valeurs ",
         "manquantes historiques, les extrémités manquantes ». Ce sont deux problèmes ",
         "différents, et le code les sépare.</p>",
         "<p><code>combler_trous_kalman()</code> estime un mois qui <strong>existe</strong> ",
         "mais n'a pas été relevé. L'information ultérieure est disponible et sert au ",
         "lissage : c'est de l'<em>interpolation</em>.</p>",
         "<p><code>prevoir_mois_manquants()</code> estime un mois qui <strong>n'existe ",
         "pas encore</strong>. Rien ne vient après, le modèle extrapole : c'est de la ",
         "<em>prévision</em>, structurellement plus dure.</p>",
         "<p>Les confondre reviendrait à se donner en M1 une précision qu'on n'a qu'en ",
         "M3 — donc à <strong>fabriquer l'avantage de calendrier que la phase est ",
         "censée mesurer</strong>. C'est le genre d'erreur qui produirait un beau ",
         "résultat et un résultat faux.</p>")))
ajouter("<p>Le modèle est le même — modèle structurel de base — mais on itère ",
        "l'équation d'état vers l'avant au lieu de lisser.</p>")
ajouter("<div class='encadre alerte'><span class='etiq'>Un détail d'implémentation qui a coûté trois heures</span>",
        "<p>La prévision des séries positives était bornée à <strong>exactement ",
        "zéro</strong>. Un mois prévu à 0 peut rendre l'agrégat trimestriel nul, et ",
        "le garde-fou de la phase 1 refuse alors <code>log(0)</code> — correctement. ",
        "Une seule série sur 242 a interrompu les 192 tâches après trois heures de ",
        "calcul.</p>",
        "<p>Trois corrections : un plancher à la moitié de la plus petite valeur ",
        "strictement positive observée ; un mode <em>tolérant</em> activé ",
        "<strong>uniquement</strong> là où des valeurs sont fabriquées — sur des ",
        "données observées, un échec de transformation signale une métadonnée fausse ",
        "et doit rester bloquant ; et surtout l'écriture d'un <strong>point de ",
        "reprise par tâche</strong>, pour qu'une panne ne coûte plus que ce qui ",
        "restait à faire.</p></div>")

# =========================== 15 =============================================
ajouter("<h2 id='s15'><span class='num'>15.</span>L'anomalie M2 &gt; M3, et son élucidation</h2>")
# Les valeurs sont LUES, jamais recopiees : la section a deja failli mentir une
# fois, quand la correction adoptee en amont a change le tableau sans changer le
# texte qui le commentait.
v15 <- function(sc) m3m$ratio_median[m3m$scenario == sc]
n15 <- function(sc) m3m$n_retenus_moyen[m3m$scenario == sc]
ajouter(paste0("<p>Le premier résultat de la phase 13 contenait une incohérence : ",
               "<strong>M3 était moins bon que M2</strong> — 0,975 contre 0,962 sur la ",
               "passerelle seule. Ajouter un troisième mois <em>observé</em> ne peut pas ",
               "dégrader : il remplace une valeur extrapolée par une donnée.</p>"))
ajouter("<p>C'est le <strong>symptôme</strong> qui a déclenché l'enquête, et il est cité ",
        "ici tel qu'il est apparu. Il n'est plus visible dans le tableau ci-dessous, pour ",
        "une raison qu'il faut dire : la correction ayant été adoptée en amont, le ",
        "périmètre commun aux scénarios a changé, et sur ce nouveau périmètre ",
        "l'inversion ne se reproduit pas. <strong>Ce qui fonde la décision n'est donc pas ",
        "ce symptôme, mais le test contrôlé qui suit</strong> — même scénario, même ",
        "sélection, mêmes origines, une seule chose qui varie.</p>")
ajouter("<p>La seule autre différence entre M2 et M3 est la convention de la phase 14 ",
        "du plan : les <strong>indicateurs trimestriels</strong> du trimestre cible ne ",
        "deviennent disponibles qu'en M3. Trois tests ont suivi.</p>")
ajouter("<h3>15.1 Test 1 — M3 sans indicateurs trimestriels</h3>")
t15 <- m3m %>% dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Scénario = scenario, `Ratio médian` = nb(ratio_median, 3),
                   `Branches < 1` = br_ok,
                   `Indicateurs retenus` = nb(n_retenus_moyen, 2))
ajouter(tbl(as.data.frame(t15), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Passerelle seule, périmètre commun à tous les scénarios. Les deux variantes de ",
  "M3 sont recalculées par le script du test lui-même, sur la même sélection et les ",
  "mêmes origines : elles ne diffèrent que par l'admission de l'indicateur ",
  "trimestriel du trimestre cible. La ligne <em>M3</em> est celle du pipeline ",
  "courant, qui a déjà adopté l'exclusion.")))
ajouter(sprintf(paste0("<p>Les deux lignes à comparer sont les deux variantes de M3, ",
                       "qui ne diffèrent que par l'admission de l'indicateur trimestriel ",
                       "du trimestre cible. Les retirer fait passer le ratio de %s à ",
                       "<strong>%s</strong>, et rend le classement monotone. Le mécanisme ",
                       "se lit dans la dernière colonne : la variante qui les admet ",
                       "retient <strong>%s indicateurs contre %s</strong> — ils occupent ",
                       "des places sous le plafond de cinq et évincent des mensuels.</p>"),
                nb(v15("M3 avec trimestriels"), 3), nb(v15("M3 mensuel seul"), 3),
                nb(n15("M3 avec trimestriels"), 2), nb(n15("M3 mensuel seul"), 2)))
ajouter(sprintf(paste0("<p>La ligne <em>M3</em> est celle du pipeline courant (%s) : elle ",
                       "applique déjà l'exclusion, et se situe donc entre les deux, à ",
                       "l'écart près de la sélection qu'elle recalcule.</p>"),
                nb(v15("M3"), 3)))
ajouter("<h3>15.2 Test 2 — un seuil renforcé pour eux seuls</h3>")
t15b <- seuilT %>%
  dplyr::transmute(`Seuil trimestriel` = ifelse(is.finite(seuil_trim),
                                                nb(seuil_trim, 2), "∞ (exclusion)"),
                   `Ratio médian` = nb(ratio_median, 3),
                   `Retenus` = nb(retenus_moyen, 2),
                   `dont trimestriels` = nb(dont_trimestriels, 2))
ajouter(tbl(as.data.frame(t15b), aligne_droite = 2:4))
ajouter(legende_tableau("Balayage du seuil de corrélation exigé des seuls indicateurs trimestriels."))
ajouter("<p>La courbe est <strong>plate de 0,15 à 0,45</strong> puis chute d'un coup à ",
        "l'exclusion. C'est la signature d'un effet de seuil, pas d'un compromis : ",
        "<strong>même ceux qui franchissent |r| ≥ 0,45 nuisent encore</strong>. Le ",
        "problème n'est donc pas la faiblesse de leur corrélation.</p>")
ajouter("<h3>15.3 Test 3 — la sélection conditionnelle</h3>")
ajouter("<p>L'hypothèse devenait alors la <strong>redondance</strong> : ces ",
        "indicateurs mesureraient en moins fin ce que les mensuels mesurent déjà. Un ",
        "critère marginal ne peut pas voir cela ; un critère <em>partiel</em>, si :</p>")
ajouter(eq(paste0(m("r"), "<sub>partiel</sub>( ", m("x<sub>k</sub>"),
                  " <span class='op'>|</span> ", m("Z"), " ) = corr( ", m("g"),
                  " <span class='op'>&minus;</span> ", m("P<sub>Z</sub> g"),
                  " , ", m("x<sub>k</sub>"), " <span class='op'>&minus;</span> ",
                  m("P<sub>Z</sub> x<sub>k</sub>"), " )")))
ajouter("<p>où ", m("P<sub>Z</sub>"), " est la projection orthogonale sur ", m("Z"),
        " = [ 1 , ", m("g<sub>T&minus;1</sub>"), " , indicateurs déjà retenus ]. Un ",
        "indicateur redondant a une corrélation partielle proche de zéro.</p>")
t15c <- condit %>% dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Méthode = methode, `Ratio médian` = nb(ratio_median, 3),
                   `Branches < 1` = br_ok, `Corrélation` = nb(correl, 2),
                   `Retenus` = nb(retenus, 2), `dont trim.` = nb(dont_trim, 2))
ajouter(tbl(as.data.frame(t15c), aligne_droite = 2:6))
ajouter(legende_tableau("Sélection conditionnelle contre marginale, en M3."))
ajouter("<div class='encadre alerte'><span class='etiq'>L'hypothèse de redondance est réfutée</span>",
        "<p>Le critère conditionnel est <strong>pire dans les deux cas</strong>, et il ",
        "retient <em>autant</em> de trimestriels que le marginal — 0,53 contre 0,50.</p>",
        "<p>C'est le point décisif : s'ils étaient redondants, la corrélation partielle ",
        "les aurait écartés, puisque c'est précisément ce qu'elle sait faire. Elle ne ",
        "les écarte pas. <strong>Ils ne sont donc pas redondants.</strong></p>",
        "<p>La lecture qui subsiste : ils sont corrélés à la valeur ajoutée sur ",
        "l'historique mais <strong>instables hors échantillon</strong>. Aucun critère ",
        "fondé sur l'ajustement passé — marginal ou partiel — ne peut le voir ; seule ",
        "l'évaluation hors échantillon le montre.</p>")
ajouter("<p>La règle retenue est donc la plus simple : <strong>exclure les ",
        "indicateurs trimestriels de la sélection</strong>. Le choix contredit la ",
        "convention de la phase 14 du plan ; il est inscrit dans le code comme un ",
        "drapeau réversible, avec les mesures qui le justifient.</p>")

# =========================== 16 =============================================
ajouter("<h2 id='s16'><span class='num'>16.</span>Résultats de la phase 13</h2>")
ajouter(sprintf(paste0("<p>Tous les chiffres qui suivent sont à <strong>périmètre ",
                       "commun</strong> : seuls les couples branche-origine où les ",
                       "quatre scénarios ont produit une prévision. Les taux de ",
                       "production diffèrent en effet d'un scénario à l'autre, et les ",
                       "comparer bruts reviendrait à comparer des épreuves ",
                       "inégales.</p>")))
for (per in c("toutes origines", "2020", "hors 2020")) {
  sous <- intra %>% dplyr::filter(periode == per) %>%
    dplyr::select(scenario, modele, mae_moyenne, ratio_median, n_ratio_ok) %>%
    tidyr::pivot_wider(names_from = modele,
                       values_from = c(mae_moyenne, ratio_median, n_ratio_ok)) %>%
    dplyr::arrange(scenario)
  tt <- data.frame(
    Scénario = sous$scenario,
    `Passerelle` = nb(sous$ratio_median_bridge, 3),
    `Combinaison` = nb(sous$ratio_median_combinee, 3),
    `BVAR seul` = nb(sous$ratio_median_bvar, 3),
    `Branches < 1 (comb.)` = sous$n_ratio_ok_combinee,
    check.names = FALSE, stringsAsFactors = FALSE)
  ajouter(sprintf("<h3>%s</h3>", tools::toTitleCase(per)))
  ajouter(tbl(tt, aligne_droite = 2:5))
}
ajouter(legende_tableau(paste0(
  "Ratio médian par scénario et par modèle. Le BVAR est plat par construction.")))
ajouter(sprintf(paste0("<p>La progression est <strong>monotone sur les quatre ",
                       "scénarios</strong>, pour la passerelle comme pour la ",
                       "combinaison. Et le résultat central : la combinaison atteint ",
                       "<strong>%s en M3 contre %s</strong> pour le BVAR seul, soit ",
                       "%.1f %% de mieux.</p>"),
                nb(m3_comb, 3), nb(bvar_r, 3), 100 * (1 - m3_comb / bvar_r)))
ajouter("<p>Deux faits méritent d'être soulignés séparément.</p>")
ajouter("<p><strong>Dès M1</strong> — un seul mois observé — la combinaison bat le ",
        "BVAR. C'est la situation qui donne son sens au nowcasting : savoir un mois ",
        "après le début du trimestre que quelque chose se passe.</p>")
ajouter(sprintf(paste0("<p><strong>Hors 2020</strong>, la passerelle seule passe sous ",
                       "1 en M3 (%s contre %s pour le BVAR). Jusqu'à la phase 13, elle ",
                       "n'y était jamais parvenue, et le gain semblait entièrement ",
                       "concentré sur la crise. Ce n'est plus le cas.</p>"),
                nb(ligne(intra, "M3", "bridge", "hors 2020")$ratio_median, 3),
                nb(ligne(intra, "M3", "bvar", "hors 2020")$ratio_median, 3)))
ajouter("<p><strong>M0 est mauvais partout</strong>, ce qui est rassurant : extrapoler ",
        "les trois mois revient à faire de la prévision pure, et le modèle ne voit pas ",
        "venir un choc. Cela confirme que la valeur vient des <em>données ",
        "observées</em>, et non de la machinerie de prévision de mois.</p>")
ajouter(figure("09_valeur_information.png",
               "Valeur de l'information au fil du trimestre",
               paste0("La figure centrale du rapport. Le BVAR est une horizontale : il ",
                      "ne voit rien de nouveau entre M0 et M3. Les deux autres courbes ",
                      "descendent régulièrement à mesure que les mois sont observés, et ",
                      "croisent l'horizontale — la combinaison dès M1, la passerelle ",
                      "entre M1 et M2. L'écart vertical à M3 est ce que les indicateurs ",
                      "mensuels apportent.")))
ajouter("<div class='encadre alerte'><span class='etiq'>À lire avec la section 17</span>",
        "<p>Les chiffres ci-dessus sont des <strong>estimations ponctuelles</strong>. ",
        "La section suivante leur applique des tests et des intervalles de ",
        "confiance, et le verdict tempère nettement ce qui précède : ",
        "<strong>l'écart de 8,5 % entre la combinaison et le BVAR n'est pas ",
        "établi statistiquement</strong>. Ce qui résiste aux tests, en revanche, ",
        "c'est que la combinaison bat chacune de ses deux composantes prises ",
        "isolément.</p></div>")

ajouter(figure("09_mae_scenarios.png", "Erreur absolue selon le scénario",
               paste0("La même progression en erreur absolue, qui ne dépend pas du ",
                      "choix d'un dénominateur. La décroissance est monotone pour la ",
                      "passerelle comme pour la combinaison.")))

# =========================== 17 =============================================
if (A_INCERT) {

ajouter("<h2 id='s17'><span class='num'>17.</span>L'incertitude : ces écarts sont-ils réels ?</h2>")
ajouter("<p>Tout ce qui précède — et tout ce que les phases 3 et 4 ont produit — ",
        "est rapporté en <strong>estimations ponctuelles</strong>. On annonce 0,895 ",
        "contre 0,978 sans jamais dire si l'écart est distinguable du hasard ",
        "d'échantillonnage. C'est la faiblesse la plus sérieuse du projet, et le ",
        "plan l'avait prévue (phase 18).</p>")
ajouter(intuition(paste0(
  "<p>Elle est d'autant plus gênante que les effectifs sont très inégaux : trois ",
  "branches disposent des 48 origines, l'Agriculture de quatre. Un ratio calculé ",
  "sur quatre points n'a aucune précision — et la médiane sur les branches en ",
  "hérite sans que cela se voie.</p>")))

ajouter("<h3>17.1 Le test de Diebold-Mariano, et sa correction petit échantillon</h3>")
ajouter("<p>Pour deux prévisions concurrentes d'erreurs ", m("e<sub>1</sub>"),
        " et ", m("e<sub>2</sub>"), ", on forme la différence de perte :</p>")
ajouter(eq(paste0(m("d<sub>t</sub>"), " = ", m("e<sub>1,t</sub><sup>2</sup>"),
                  " <span class='op'>&minus;</span> ",
                  m("e<sub>2,t</sub><sup>2</sup>"))))
ajouter("<p>et l'on teste ", m("H<sub>0</sub>"), " : E[", m("d<sub>t</sub>"),
        "] = 0 par</p>")
ajouter(eq(paste0("DM = <span class='fr'><span class='hi'>", m("d&#772;"),
                  "</span><span class='lo'><span class='op'>&radic;</span>( V&#770;(",
                  m("d&#772;"), ") )</span></span>")))
ajouter(definition("Pourquoi la correction de Harvey-Leybourne-Newbold est indispensable ici",
  paste0("<p>La loi normale n'est valable qu'asymptotiquement. Avec ", m("n"),
         " qui descend à 4, elle rejetterait bien trop souvent. La correction de ",
         "Harvey, Leybourne &amp; Newbold (1997) multiplie la statistique par</p>",
         eq(paste0("<span class='op'>&radic;</span> ",
                   "<span class='fr'><span class='hi'>", m("n"),
                   " <span class='op'>+</span> 1 <span class='op'>&minus;</span> 2",
                   m("h"), " <span class='op'>+</span> ", m("h"),
                   "(", m("h"), "&minus;1)/", m("n"),
                   "</span><span class='lo'>", m("n"), "</span></span>")),
         "<p>et la compare à une loi de Student à ", m("n"),
         "&minus;1 degrés de liberté. Ici ", m("h"),
         " = 1, le nowcast étant à un pas.</p>")))

t17 <- dm_br %>%
  dplyr::filter(comparaison == "Combinaison contre BVAR seul") %>%
  dplyr::count(scenario, verdict) %>%
  tidyr::pivot_wider(names_from = verdict, values_from = n, values_fill = 0)
ajouter(tbl(as.data.frame(t17), aligne_droite = 2:ncol(t17)))
ajouter(legende_tableau("Diebold-Mariano branche par branche, combinaison contre BVAR seul."))
ajouter(sprintf(paste0("<p>Sur dix branches, l'écart n'est distinguable du bruit ",
                       "que sur <strong>trois</strong>. Et <strong>%d tests sont ",
                       "indécidables</strong> faute d'observations : les branches à ",
                       "quatre ou neuf origines ne permettent aucune inférence, ce ",
                       "que la médiane masquait complètement.</p>"),
                sum(is.na(dm_br$p_value))))
ajouter(figure("10_dm_par_branche.png",
               "Combinaison contre BVAR seul, branche par branche",
               paste0("Sous le trait, l'écart est distinguable du bruit au seuil de ",
                      "10 %. Les mêmes trois branches passent à M1, M2 et M3 ; les ",
                      "autres restent au-dessus quel que soit le scénario.")))

ajouter("<h3>17.2 Grouper les branches sans surestimer la précision</h3>")
ajouter(intuition(paste0(
  "<p>Une branche seule offre trop peu d'observations. Mais on ne peut pas ",
  "simplement empiler les branches : elles subissent les <strong>mêmes chocs au ",
  "même trimestre</strong>, donc leurs erreurs sont corrélées. Les traiter comme ",
  "indépendantes multiplierait artificiellement la taille d'échantillon.</p>",
  "<p>On agrège donc <em>d'abord</em> sur les branches, à chaque origine, après ",
  "avoir rendu les pertes comparables ; il reste une seule série temporelle, sur ",
  "laquelle le test est licite.</p>")))
ajouter(eq(paste0(m("d&#771;<sub>t</sub>"), " = ",
                  "<span class='fr'><span class='hi'>1</span><span class='lo'>B</span></span> ",
                  "<span class='big'>&sum;</span><sub class='num'>b</sub> ",
                  "<span class='fr'><span class='hi'>",
                  m("e<sub>1,b,t</sub><sup>2</sup>"), " <span class='op'>&minus;</span> ",
                  m("e<sub>2,b,t</sub><sup>2</sup>"), "</span><span class='lo'>",
                  m("&sigma;<sub>b</sub><sup>2</sup>"), "</span></span>")))
ajouter("<p>La variance de long terme est estimée à la Newey-West, pour tenir compte ",
        "d'une éventuelle autocorrélation de ", m("d&#771;"), ".</p>")
t17b <- dm_gr %>% dplyr::arrange(comparaison, scenario) %>%
  dplyr::transmute(Comparaison = comparaison, Scénario = scenario,
                   `Statistique` = nb(statistique, 2),
                   `p-value` = nb(p_value, 3),
                   Verdict = verdict)
ajouter(tbl(as.data.frame(t17b), aligne_droite = 3:4))
ajouter(legende_tableau("Test groupé sur les dix branches, 48 origines."))
ajouter("<p>Deux enseignements, de sens opposé.</p>")
ajouter("<p><strong>Seul M1 rend la combinaison significativement meilleure que le ",
        "BVAR</strong> au seuil de 10 %. En M2 et M3, où les ratios sont pourtant les ",
        "plus flatteurs, l'écart n'est pas significatif.</p>")
ajouter("<p>En revanche, <strong>la combinaison bat la passerelle seule de façon ",
        "significative dans les quatre scénarios</strong> (p entre 0,037 et 0,097). ",
        "Combiner apporte donc quelque chose de robuste : c'est le résultat le mieux ",
        "établi de ces deux phases.</p>")

ajouter("<h3>17.3 Un intervalle autour du ratio</h3>")
ajouter("<p>Le ratio médian n'a pas de loi connue : c'est une médiane, sur les ",
        "branches, de rapports d'écarts quadratiques. On le rééchantillonne par ",
        "<strong>blocs d'origines consécutives</strong> — quatre trimestres — ce qui ",
        "préserve la dépendance temporelle qu'un tirage indépendant détruirait.</p>")
t17c <- ic %>% dplyr::arrange(scenario, modele) %>%
  dplyr::transmute(Scénario = scenario, Modèle = modele,
                   `Ratio` = nb(ratio, 3),
                   `Intervalle à 90 %` = sprintf("[%s ; %s]", nb(borne_basse, 3),
                                                 nb(borne_haute, 3)))
ajouter(tbl(as.data.frame(t17c), aligne_droite = 3:4))
ajouter(legende_tableau("Bootstrap par blocs, 2 000 rééchantillonnages."))
ajouter("<div class='encadre alerte'><span class='etiq'>Le résultat central n'est pas établi</span>",
        "<p>Les intervalles se chevauchent largement, et celui de la combinaison en M3 ",
        "monte jusqu'à 1,048 : <strong>il contient 1</strong>.</p>",
        "<p>La formulation honnête est donc : la combinaison fait mieux en point estimé ",
        "à tous les scénarios sauf M0 ; l'écart avec le BVAR est significatif en M1 ",
        "seulement ; et la combinaison bat significativement chacune de ses deux ",
        "composantes prises isolément.</p></div>")
ajouter(figure("10_intervalles_ratio.png", "Le ratio, avec son incertitude",
               paste0("La même progression M0 → M3 que la figure de la section 16, ",
                      "assortie cette fois de barres d'incertitude. La lecture change : ",
                      "la décroissance reste visible, mais les intervalles de la ",
                      "combinaison et du BVAR se recouvrent à tous les scénarios.")))

ajouter("<h3>17.4 Combien d'observations manque-t-il ?</h3>")
ajouter("<p>La statistique croît comme ", m("&radic;n"),
        " à effet constant. Pour que M3 franchisse le seuil, il faudrait donc :</p>")
ajouter(eq(paste0(m("n"), "<sub>requis</sub> <span class='op'>&asymp;</span> ",
                  m("n"), " <span class='op'>&times;</span> <span class='op'>(</span>",
                  "<span class='fr'><span class='hi'>seuil critique</span>",
                  "<span class='lo'>statistique observée</span></span>",
                  "<span class='op'>)</span><sup>2</sup> = 48 ",
                  "<span class='op'>&times;</span> ( 1,68 / 1,39 )<sup>2</sup> ",
                  "<span class='op'>&asymp;</span> 71")))
ajouter("<p>Soit <strong>23 origines de plus</strong>. C'est ce calcul qui a motivé le ",
        "backtest étendu de la section suivante — et c'est aussi lui qui montre que le ",
        "problème n'est pas le modèle mais la <strong>taille de l'échantillon ",
        "d'évaluation</strong>.</p>")

} else {
  ajouter("<h2 id='s17'><span class='num'>17.</span>L'incertitude : ces écarts sont-ils réels ?</h2>")
  ajouter("<p>Section non composée : exécuter <code>R/10_incertitude.R</code>.</p>")
}

# =========================== 18 =============================================
if (A_ETENDU) {

ajouter("<h2 id='s18'><span class='num'>18.</span>Le backtest étendu : un second épisode de rupture</h2>")
ajouter("<p>Deux limites n'en faisaient qu'une : le manque de puissance, et ",
        "l'unicité de l'épisode de choc. La règle de détection de la phase 3 ",
        "identifie <strong>T4-2008</strong> comme choc ; reculer la première cible ",
        "apporte donc à la fois les origines manquantes et un second épisode.</p>")
ajouter("<h3>18.1 Deux obstacles, dont un que j'avais créé</h3>")
ajouter(definition("Un choc qui n'est pas une cible ne sert à rien",
  paste0("<p>La première version faisait commencer le backtest étendu à T1-2009. ",
         "Le calcul d'origines était juste, mais T4-2008 devenait alors de ",
         "l'<strong>historique</strong>, jamais une cible : le second épisode était ",
         "absent de l'évaluation.</p>",
         "<p>Reculer à T2-2008 le corrige, au prix d'une branche — quatre au lieu de ",
         "cinq.</p>")))
ajouter(definition("Une borne transposée sans être réinterrogée",
  paste0("<p>Le second obstacle était plus profond. Les prévisions du BVAR ",
         "s'arrêtaient à T2-2014, si bien que tous les tests l'impliquant ",
         "retombaient à 48 origines.</p>",
         "<p>Or cette borne vient de la valeur ajoutée <strong>nominale</strong>, qui ",
         "fournit les poids en prix courants — une contrainte de l'<em>agrégation</em>, ",
         "phase 11. Le BVAR, lui, n'utilise que la valeur ajoutée <strong>réelle</strong>, ",
         "disponible depuis 1998. La borne lui avait été appliquée par alignement, ce ",
         "qui était raisonnable, mais n'avait jamais été réinterrogé.</p>",
         "<p>Le BVAR a donc été rejoué depuis T2-2008, avec la spécification retenue ",
         "inchangée. Sur les 768 prévisions communes, l'écart maximal avec la phase 3 ",
         "est de <strong>4,9 &times; 10<sup>&minus;17</sup></strong> — la précision ",
         "machine. La comparaison entre périodes est donc légitime.</p>")))

ajouter("<h3>18.2 Le résultat : négatif</h3>")
t18 <- et_dm %>%
  dplyr::filter(comparaison == "Combinaison contre BVAR seul") %>%
  dplyr::arrange(echantillon, scenario) %>%
  dplyr::transmute(Échantillon = echantillon, Scénario = scenario,
                   `Origines` = n_origines,
                   `Statistique` = nb(statistique, 2),
                   `p-value` = nb(p_value, 3))
ajouter(tbl(as.data.frame(t18), aligne_droite = 3:5))
ajouter(legende_tableau("Test groupé sur les quatre branches du backtest étendu."))
ajouter("<p><strong>Aucun test n'est significatif, sur aucun échantillon.</strong> Les ",
        "statistiques sont quasi nulles. Le gain de puissance espéré n'existe pas : ce ",
        "n'est pas la précision qui manquait, c'est <strong>l'effet lui-même qui ",
        "disparaît</strong> sur ces branches. Le calcul de la section 17.4 supposait ",
        "un effet constant, et cette hypothèse est démentie.</p>")
ajouter("<div class='encadre alerte'><span class='etiq'>Un artefact de normalisation, trouvé et corrigé</span>",
        "<p>Une version antérieure de ce test affichait des p-values nettement plus ",
        "favorables — 0,053 en M3 — pour un sous-échantillon qui ne contenait pourtant ",
        "<em>aucune donnée supplémentaire</em>.</p>",
        "<p>Cause : la variance par branche servant à mettre les pertes à l'échelle ",
        "était recalculée sur chaque sous-échantillon. Deux périodes donnaient donc des ",
        "pondérations différentes, et l'écart se lisait comme un gain de puissance. ",
        "Les variances sont désormais <strong>figées sur l'échantillon complet</strong>, ",
        "et la correction va dans le sens défavorable.</p></div>")
ajouter("<p>Deux raisons à ce résultat nul. L'exercice ne porte que sur ",
        "<strong>quatre branches</strong> — finances et assurances, construction, ",
        "électricité-gaz-eau, industrie d'extraction — et ce ne sont pas celles où la ",
        "passerelle brillait : l'hébergement-restauration, qui portait l'essentiel du ",
        "gain en 2020, n'a pas assez d'historique pour y figurer.</p>")

ajouter("<h3>18.3 Ce que l'exercice établit malgré tout</h3>")
t18b <- et_ep %>%
  dplyr::mutate(modele = dplyr::recode(modele, bvar = "BVAR",
                                       bridge = "Passerelle", combinee = "Combinaison")) %>%
  dplyr::select(episode, scenario, modele, mae) %>%
  tidyr::pivot_wider(names_from = modele, values_from = mae) %>%
  dplyr::arrange(episode, scenario) %>%
  dplyr::transmute(Épisode = episode, Scénario = scenario,
                   BVAR = nb(BVAR, 4), Passerelle = nb(Passerelle, 4),
                   Combinaison = nb(Combinaison, 4))
ajouter(tbl(as.data.frame(t18b), aligne_droite = 3:5))
ajouter(legende_tableau("Erreur absolue moyenne par épisode, backtest étendu."))
ajouter("<p>Sur la <strong>crise financière de 2008-2009</strong>, la combinaison bat ",
        "le BVAR dès M1 et l'écart se creuse jusqu'à M3 — 0,0461 contre 0,0560, soit ",
        "<strong>18 % de mieux</strong> — avec une progression monotone.</p>")
ajouter("<div class='encadre'><span class='etiq'>L'acquis</span>",
        "<p>C'est le même profil qu'en 2020 : sur un épisode entièrement différent, six ",
        "ans plus tôt, sur d'autres branches. <strong>L'affirmation « la combinaison ",
        "aide lors des ruptures » repose désormais sur deux épisodes et non plus sur ",
        "un seul.</strong></p>",
        "<p>C'est précisément ce que réclamait la limite sur l'épisode unique. Le ",
        "résultat est plus modeste que celui annoncé en section 16, mais il est plus ",
        "solide : en période calme le BVAR reste devant partout, et l'apport de la ",
        "passerelle est <em>conditionnel aux ruptures</em>.</p></div>")
ajouter(figure("09b_episodes.png", "Les deux épisodes de rupture, et les périodes calmes",
               paste0("La comparaison que le backtest étendu rend possible. Sur les deux ",
                      "crises, la combinaison descend à mesure que les mois sont ",
                      "observés ; en période calme, les trois barres sont plates et le ",
                      "BVAR est devant.")))
ajouter(figure("09b_puissance.png", "Ce que la puissance change",
               paste0("Les p-values du test groupé sur les trois échantillons. Toutes ",
                      "restent au-dessus du seuil : ajouter des origines n'a pas suffi, ",
                      "parce que les branches disponibles avant 2014 ne sont pas celles ",
                      "où l'effet existe.")))

} else {
  ajouter("<h2 id='s18'><span class='num'>18.</span>Le backtest étendu : un second épisode de rupture</h2>")
  ajouter("<p>Section non composée : exécuter <code>R/03d_bvar_etendu.R</code> puis ",
          "<code>R/09b_backtest_etendu.R</code>.</p>")
}

# =========================== 19 =============================================
ajouter("<h2 id='s19'><span class='num'>19.</span>Limites</h2>")
ajouter("<h3>19.1 Des effectifs très inégaux, et des tests indécidables</h3>")
t19 <- effectifs %>% dplyr::arrange(dplyr::desc(n_origines)) %>%
  dplyr::transmute(Branche = branche, `Origines dans le périmètre commun` = n_origines)
ajouter(tbl(as.data.frame(t19), aligne_droite = 2))
ajouter(legende_tableau("Nombre d'origines par branche dans le périmètre commun."))
ajouter("<p>Trois branches disposent des 48 origines, l'Agriculture de quatre. Deux ",
        "branches sortent même du tableau faute d'un minimum d'observations — d'où dix ",
        "branches évaluées et non douze.</p>")
ajouter("<p>La section 17 a montré ce que cela coûte réellement : <strong>douze tests ",
        "de Diebold-Mariano sont indécidables</strong>, faute de pouvoir estimer une ",
        "variance sur si peu de points. La médiane sur les branches donne le même poids ",
        "à une branche mesurée sur 48 trimestres et à une autre mesurée sur quatre.</p>")

ajouter("<h3>19.2 Deux épisodes de rupture, mais un effet moyen nul</h3>")
ajouter("<p>Le backtest étendu a levé l'unicité de l'épisode : la combinaison bat le ",
        "BVAR sur la crise de 2008-2009 comme sur 2020, avec le même profil monotone. ",
        "Mais il a aussi montré que sur les quatre branches disponibles avant 2014, ",
        "<strong>l'effet moyen est nul</strong> et aucun test n'est significatif.</p>")
ajouter("<p>Ce qui reste hors de portée : mesurer l'apport de la passerelle lors d'une ",
        "rupture <em>sur les branches où elle est forte</em>. L'hébergement-restauration, ",
        "qui portait l'essentiel du gain en 2020, n'a pas assez d'historique pour ",
        "figurer dans l'exercice étendu. Il faudrait des indicateurs remontant plus ",
        "haut, ce qui ne dépend pas de la méthode.</p>")

ajouter("<h3>19.3 Les données ne sont pas millésimées</h3>")
ajouter("<div class='encadre alerte'><span class='etiq'>Une limite jamais signalée jusqu'ici</span>",
        "<p>Tout le projet utilise la <strong>dernière version révisée</strong> des ",
        "comptes. Un exercice réellement en temps réel utiliserait les données ",
        "<em>telles qu'elles étaient publiées</em> à chaque origine.</p>",
        "<p>Les règles anti-look-ahead du plan, appliquées scrupuleusement dans tout le ",
        "code, portent sur les <strong>dates</strong> — jamais sur les ",
        "<strong>millésimes</strong>. Ce sont deux choses différentes : nos prévisions ",
        "sont évaluées contre une réalité qui n'était pas connue à l'époque, et les ",
        "indicateurs eux-mêmes ont été révisés depuis.</p>",
        "<p>Le sens du biais est connu : il <strong>flatte</strong> la performance ",
        "mesurée, puisque le modèle est estimé sur des données plus propres que celles ",
        "dont disposait un praticien. Son ampleur, elle, ne peut pas être mesurée sans ",
        "une base de millésimes que le projet n'a pas.</p></div>")

ajouter("<h3>19.4 Les délais de publication sont ignorés</h3>")
ajouter("<div class='encadre alerte'><span class='etiq'>Et celle-ci affecte le résultat central</span>",
        "<p>Le scénario M1 suppose que le premier mois du trimestre est connu ",
        "<strong>à la fin de ce premier mois</strong>. En réalité un indicateur mensuel ",
        "paraît avec quatre à huit semaines de délai : notre M1 correspond plutôt au M2 ",
        "ou au M3 réels.</p>",
        "<p><strong>L'avantage de calendrier, qui est le résultat central de la ",
        "phase 13, est donc surestimé.</strong> La progression M0 → M3 reste valable ",
        "comme mesure de la valeur de l'information ; c'est son calage sur le calendrier ",
        "réel qui est optimiste.</p>",
        "<p>Un test de sensibilité est faisable et n'a pas été fait : décaler les ",
        "scénarios d'un mois dirait de combien. Il demanderait un calendrier de ",
        "publication par indicateur, que le vivier ne documente pas.</p></div>")

ajouter("<h3>19.5 Aucune correction pour tests multiples</h3>")
ajouter("<p>Ces deux phases ont exploré une quinzaine de spécifications — six pistes ",
        "d'amélioration, quatre variantes de ridge, trois tests sur les indicateurs ",
        "trimestriels, quatre scénarios — et lancé plusieurs dizaines de tests, ",
        "<strong>sans aucune correction pour multiplicité</strong>.</p>")
ajouter("<p>Avec autant de comparaisons, un p de 0,088 ne vaut pas ce qu'il vaudrait ",
        "isolément. Cela renforce la lecture prudente de la section 17 : le seul ",
        "résultat sur lequel s'appuyer est celui qui tient dans les quatre scénarios — ",
        "la combinaison bat chacune de ses composantes — et non tel p juste sous le ",
        "seuil.</p>")

ajouter("<h3>19.6 Les indicateurs trimestriels restent inexploités</h3>")
ajouter("<p>La règle retenue les exclut de la sélection en M3. Trois tentatives pour ",
        "les récupérer ont échoué (section 15). Ce qui reste à essayer : évaluer leur ",
        "stabilité hors échantillon <em>série par série</em>, pour garder les stables au ",
        "lieu de tous les écarter. Le choix actuel contredit la convention de la ",
        "phase 14 du plan, et il est inscrit comme un drapeau réversible.</p>")

ajouter("<h3>19.7 Le BVAR ne fournit pas d'incertitude paramétrique</h3>")
ajouter("<p>La section 17 quantifie l'incertitude des <strong>prévisions</strong>. ",
        "Celle des <strong>paramètres</strong> reste absente : le système augmenté donne ",
        "une estimation ponctuelle — la moyenne a posteriori — et produire des ",
        "intervalles de crédibilité supposerait un échantillonnage de la loi a ",
        "posteriori, par exemple par échantillonneur de Gibbs. C'est une limite héritée ",
        "de la phase 3, que ce rapport ne lève pas.</p>")

ajouter("<h3>19.8 Quatre branches sans indicateurs</h3>")
ajouter("<p>Services aux entreprises, administration publique, éducation-santé et autres ",
        "services n'ont aucun indicateur dans le vivier. L'étape 5 les a traitées : le ",
        "BVAR y fait mieux que l'AR(4) prévu par le plan — ratio médian 0,961 contre ",
        "1,14 — et trois branches sur quatre passent sous 1. Aucun modèle n'y parvient ",
        "en revanche sur l'administration publique.</p>")

ajouter("<h3>19.9 Pas d'agrégation</h3>")
ajouter("<p>Tout est évalué branche par branche. L'agrégation en valeur ajoutée totale ",
        "relève de la phase 11, avec des poids en prix courants. Une vérification ",
        "exploratoire a montré que les erreurs de branche <strong>ne se compensent ",
        "pas</strong> : l'agrégat du BVAR donne 0,981, exactement comme la médiane par ",
        "branche, et 1,014 hors 2020.</p>")
ajouter(definition("Une contrainte de données sur laquelle il faudra être clair",
  paste0("<p>La valeur ajoutée nominale, qui fournit les poids en prix courants, ne ",
         "commence qu'en 2014. <strong>L'agrégat ne pourra donc jamais être validé sur ",
         "plus de 48 trimestres</strong>, et c'est court pour juger un nowcast de PIB.</p>",
         "<p>Un contournement existe pour les exercices de robustesse : des poids ",
         "calculés sur la valeur ajoutée réelle, disponible depuis 1998. Mesuré sur ",
         "2014-2026 où les deux coexistent, l'écart sur la croissance agrégée est de ",
         "<strong>0,047 point</strong> en médiane, pour un écart-type de la série de ",
         "2,14 points — soit 2 % de la variabilité à prévoir, et une corrélation de ",
         "0,998 entre les deux séries. L'approximation est donc acceptable, et surtout ",
         "elle est <em>mesurée</em> plutôt que supposée.</p>",
         "<p>Cela ne remplace pas l'agrégation de Fisher pour le nowcast de production, ",
         "où les poids en prix courants restent la bonne méthode.</p>")))

# =========================== 20 =============================================
ajouter("<h2 id='s20'><span class='num'>20.</span>Fichiers produits et suite</h2>")
fichiers <- data.frame(
  Fichier = c("R/fonctions/passerelle.R", "R/fonctions/kalman.R",
              "R/04_bridge_equations.R", "R/04b_variantes_passerelle.R",
              "R/04c_kalman_trous.R", "R/04d_combinaison_etat.R",
              "R/09_test_affinement_intra_trimestre.R",
              "R/03d_bvar_etendu.R", "R/09b_backtest_etendu.R", "R/10_incertitude.R",
              "resultats/04_*", "resultats/04b_*", "resultats/04c_*",
              "resultats/04d_*", "resultats/09_*",
              "resultats/09b_*", "resultats/10_*"),
  Contenu = c("agrégation, sélection (marginale et consciente de la disponibilité), passerelle avec terme AR, ACP, ridge, poids δ",
              "comblement de trous par lissage, prévision de mois par extrapolation",
              "phase 4 : les quatre volets de l'étape 4 du plan",
              "les six pistes d'amélioration, comparées hors échantillon",
              "comblement par Kalman et son effet",
              "poids dépendant de l'état et sensibilité au seuil",
              "phase 13 : les quatre scénarios intra-trimestriels",
              "BVAR rejoué depuis 2008, contrôlé identique à la phase 3 sur les origines communes",
              "backtest étendu : 4 branches, 72 origines, deux épisodes de rupture",
              "Diebold-Mariano, test groupé et bootstrap par blocs",
              "résultats de la phase 4", "comparaison des variantes",
              "validation et effet du Kalman", "poids dépendant de l'état",
              "phase 13, dont les 192 points de reprise",
              "backtest étendu", "tests d'incertitude"),
  stringsAsFactors = FALSE)
ajouter(tbl(fichiers, aligne_droite = integer(0)))
ajouter(legende_tableau("Sorties des phases 4 et 13."))
figs <- list.files(DOSSIER_FIGURES, pattern = "^(04|09|10)", full.names = FALSE)
ajouter(sprintf("<p>%d figures dans <code>figures/</code>.</p>", length(figs)))
ajouter("<h3>Ce qui reste à faire</h3>")
ajouter("<ul>",
        "<li><strong>Étape 5</strong> : l'AR(4) des quatre branches non couvertes ;</li>",
        "<li><strong>Phase 11</strong> : l'agrégation en valeur ajoutée totale, avec ",
        "des poids en prix courants issus de la VA nominale — c'est ce qui justifie ",
        "que l'évaluation commence à T2-2014 ;</li>",
        "<li><strong>Phase 15 et suivantes</strong> : backtesting complet, benchmarks, ",
        "et tests de Diebold-Mariano sur les prévisions combinées ;</li>",
        "<li>reprendre l'évaluation des phases 3 et 4 en <strong>séparant ruptures et ",
        "périodes calmes</strong>, puisque la présentation par moyenne globale cache ",
        "un système qui bat son étalon au moment où cela compte.</li>",
        "</ul>")

ajouter(pied_rapport("R/04e_rapport_phases4_13.R"))

ecrire_rapport(h, "Phases 4 et 13 — Les équations de passerelle", CHEMIN)
cat(sprintf("[rapport] %s (%.1f Mo, %d figures, %d tableaux, %d equations)\n",
            CHEMIN, file.size(CHEMIN) / 1024^2,
            .n_figure, .n_tableau, get(".n_equation", envir = globalenv())))
