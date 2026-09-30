# Note méthodologique — GDPNow-Maroc, Méthode 1

**Nowcasting de la valeur ajoutée totale par branche : BVAR trimestriel, équations passerelles, agrégation de Laspeyres.**

*Version 3 — 15 septembre 2026. Cette version remplace intégralement la version 2 (23 août 2026), qui décrivait le système avant l'application du plan de correction méthodologique. Les résultats chiffrés de la version 2 ne sont plus valides : ils provenaient d'un protocole qui laissait fuiter de l'information postérieure à la date de prévision.*

Document autonome : il se lit sans le reste du dossier. Les démonstrations détaillées, les tableaux complets et les figures se trouvent dans les quatre rapports HTML de `report/`.

---

## 1. Ce que le système prévoit exactement

Le système produit, à une date `T` quelconque, une estimation de :

> **la croissance trimestrielle en volume de la valeur ajoutée totale, reconstruite à partir des seize branches de la nomenclature HCP par un indice de Laspeyres à poids en prix courants du trimestre précédent.**

Trois précisions, chacune nécessaire :

- **en volume** : il ne s'agit pas d'un montant en dirhams, mais d'un taux de croissance d'un indice de volume ;
- **reconstruite** : l'agrégat est calculé ici à partir des seize branches. Ce n'est pas le total publié par le HCP, dont nous ne disposons pas ;
- **valeur ajoutée**, non PIB : il manque les impôts sur les produits nets de subventions.

La licéité de cette reconstruction a été vérifiée : l'écart entre notre indice et la somme des niveaux chaînés est nul en moyenne (+0,0075 point, *t* = 0,30, *p* = 0,76) et sa dérive cumulée sur 48 trimestres atteint +0,36 %.

**Chiffre courant.** Au 15 septembre 2026, avec deux mois du trimestre cible observés (scénario M2) : **T2-2026 = +1,72 %**, intervalle **[+0,12 ; +2,91]** dont la couverture mesurée est de 77 %.

---

## 2. Architecture

Adaptation du modèle GDPNow de la Federal Reserve Bank d'Atlanta (Higgins, 2014) à une logique **d'offre** — production par branche d'activité — plutôt que de demande.

### 2.1 Trois étages

| Étage | Portée | Rôle |
|---|---|---|
| **BVAR trimestriel** | les 16 branches | prévision de base, prior de Minnesota par observations fictives |
| **Équations passerelles** | les branches disposant d'indicateurs à l'origine considérée | traduisent les indicateurs infra-trimestriels en croissance de la VA |
| **Agrégation** | agrégat | indice de Laspeyres, poids en prix courants de `T−1` |

Les deux premiers étages sont combinés à poids fixe δ = 0,5 lorsque les deux
existent. **Là où la passerelle manque, le nowcast de la branche est la
prévision du BVAR** — c'est le sens du `coalesce` de l'étape d'agrégation.

Quatre branches n'ont aucun indicateur infra-trimestriel dans le vivier :
**services aux entreprises, administration publique, éducation-santé, autres
services**. Elles reçoivent donc le BVAR à toutes les origines. Trois autres
branches, pourtant couvertes, peuvent s'y trouver ponctuellement ramenées quand
aucun de leurs indicateurs n'est disponible à la date considérée : au trimestre
courant, sept branches sur seize sont prévues par le BVAR seul.

> **Il n'y a pas de quatrième étage autorégressif.** Les versions antérieures de
> cette note décrivaient un AR(p) appliqué aux quatre branches non couvertes.
> Vérification faite sur les sorties, cette prévision n'entre pas dans le chiffre
> publié : pour ces branches, `nowcast` est rigoureusement égal à `bvar`
> (égalité au 10⁻¹²), et le backtest intra-trimestriel ne contient aucune ligne
> les concernant. Une autorégression était bien calculée dans la fonction
> centrale, mais son résultat n'était jamais consulté ; ce calcul mort a été
> retiré. Voir §5.4.

### 2.2 Les équations

Pour une branche *j* couverte, le nowcast combine deux prévisions :

```
ĝ(j,T) = δ · ĝ_BVAR(j,T) + (1 − δ) · ĝ_passerelle(j,T)   avec δ = 0,5
```

Le poids est **constant**, et non estimé. La justification complète est en section 7.1. En un mot : comparée à une constante placée à son propre niveau moyen, l'estimation récursive de δ fait *moins bien* — sa variation n'apporte pas d'information, elle ajoute du bruit. Un paramètre estimé de moins, c'est un risque de surajustement de moins, et un chiffre publié qui ne dépend plus d'une optimisation invisible.

La passerelle est une régression de la croissance trimestrielle de la VA sur les indicateurs agrégés au trimestre, augmentée du retard de la VA :

```
g(j,t) = α + β′ x(j,t) + γ g(j,t−1) + ε(j,t)
```

L'agrégat est un indice de volume de Laspeyres :

```
ĝ(T) = log Σ_j w(j,T−1) · exp(ĝ(j,T))
```

où `w(j,T−1)` est la part de la branche *j* dans la valeur ajoutée **en prix courants** du trimestre `T−1` — donc connue avant `T`.

### 2.3 Pourquoi des poids nominaux sur des prévisions en volume

C'est exactement la construction d'un indice de Laspeyres en volume : les quantités de la période courante sont valorisées aux prix de la période de base. Utiliser des poids en volume reviendrait à valoriser aux prix d'une année de référence de plus en plus lointaine. La formule linéarisée `Σ w·g` a été calculée en parallèle : l'écart (le *gap* de Jensen) est de l'ordre de 0,01 point et n'affecte aucune conclusion.

---

## 3. La convention d'information

C'est le point le plus important du dossier, et celui qui invalide la version 2 de cette note.

### 3.1 Principe

À chaque origine `T`, **toute** quantité utilisée est recalculée sur la seule information disponible à cette date : les paramètres du BVAR, la sélection des indicateurs, les coefficients des passerelles, le poids δ, les poids d'agrégation, les hyperparamètres, les moyennes et écarts-types de standardisation. Rien n'est estimé une fois pour toutes sur l'échantillon complet.

### 3.2 Mise en œuvre

Une fonction centrale, `backtest_nowcast()`, produit le nowcast d'une origine donnée. Le chiffre publié et le chiffre du backtest empruntent **le même chemin de code** : c'est la garantie que ce qui a été évalué est bien ce qui est publié. Cinq contrôles bloquants (`stopifnot`) refusent l'exécution si une donnée postérieure à `T` est touchée.

### 3.3 La reproductibilité, vérifiée

Le pipeline a été rejoué **de bout en bout depuis un dossier de résultats vide**, le 15 septembre 2026. Sur les 108 fichiers de sortie : **88 sont identiques à l'octet près**, 15 ne diffèrent que par du bruit de virgule flottante — moins de 10⁻⁹ en relatif, ce qui recouvre tous les résultats principaux — et 5 ont changé pour une raison identifiée.

L'exercice a révélé deux défauts qu'un dossier déjà peuplé masquait entièrement : deux **dépendances circulaires** (les poids d'agrégation et le chiffre publié étaient calculés au milieu d'étapes qui en dépendaient) et **cinq scripts** jamais intégrés au pipeline, dont les sorties traînaient d'exécutions antérieures. Les deux cycles sont rompus par extraction (`06a_poids.R`, `08a_nowcast_courant.R`), les cinq scripts réintégrés, et un audit statique des dépendances (`outils/audit_dependances.py`) empêche la réapparition du défaut.

### 3.4 Ce que cela a coûté

La correction de trois fuites d'information a dégradé les résultats affichés, et c'est le signe qu'elle était nécessaire :

| Fuite corrigée | Effet |
|---|---|
| Passerelle construisant sa ligne cible par jointure avec la VA de `T` | les 16 branches retombaient sur le BVAR ; +0,80 % → +2,14 % sur le nowcast |
| Ragged edge appliqué globalement au lieu de série par série | 19 à 40 % des séries silencieusement écartées ; ratio branche 0,895 → 0,912 |
| Variances de standardisation recalculées par sous-échantillon | *p* = 0,053 → 0,125 sur un test d'épisode |

### 3.5 Ce qui reste non traité

Les **millésimes**. Les règles anti-look-ahead portent sur les dates, jamais sur les versions successives des comptes. Les prévisions sont évaluées contre une VA révisée que le modèle n'aurait pas eue en temps réel. Les **délais de publication** sont également ignorés : la convention retenue suppose tout indicateur du mois *m* connu le dernier jour de *m*. Un test de sensibilité chiffre ce que coûterait le délai le plus dommageable — privé de la VA de `T−1`, le BVAR passe d'un ratio de 0,967 à 1,010 et sa corrélation s'effondre de +0,50 à −0,14.

---

## 4. Le vivier d'indicateurs

### 4.1 Volumétrie

431 séries réparties sur 12 branches, après les contrôles de cohérence : **392 séries retenues**.

| Branche | Mensuelles | Trimestrielles | Total |
|---|---|---|---|
| Industrie de transformation | 46 | 42 | 88 |
| Électricité, gaz, eau | 77 | 3 | 80 |
| Pêche | 72 | 2 | 74 |
| Construction | 29 | 3 | 32 |
| Hébergement-restauration | 29 | 3 | 32 |
| Information-communication | 0 | 32 | 32 |
| Finances et assurances | 25 | 5 | 30 |
| Commerce | 17 | 3 | 20 |
| Industrie d'extraction | 11 | 6 | 17 |
| Immobilier | 4 | 10 | 14 |
| Transports | 4 | 3 | 7 |
| Agriculture | 2 | 3 | 5 |

Le détail série par série est dans `data/metadonnees_indicateurs.csv` ; le sommaire dans `data/sommaire_vivier.csv`.

### 4.2 Séries cumulées

186 séries du vivier se sont révélées **cumulées depuis janvier** — un cumul annuel remis à zéro chaque janvier, et non un flux mensuel. Détection par test de remise à zéro en janvier : le taux de décroissance décembre → janvier atteignait 100 % sur ces séries contre 1 % sur les autres. Elles sont dé-cumulées à l'import, avec un garde-fou : la correction est refusée si elle produit plus de 5 % de flux négatifs, ce qui a permis d'écarter 28 faux positifs (séries trimestrielles à forte saisonnalité).

### 4.3 Sélection : deux niveaux distincts

**Niveau 1 — la constitution du vivier**, faite une fois, en amont du pipeline. Le classeur source réunit 434 séries, retenues sur des **critères purement économiques** : un indicateur y figure parce qu'il décrit plausiblement l'activité de la branche à laquelle il est rattaché — le ciment pour la construction, les nuitées pour l'hébergement, les débarquements pour la pêche. Aucun calcul n'intervient dans ce tri, et en particulier aucune corrélation avec la valeur ajoutée.

Le pipeline prend ce vivier **tel quel** et n'en écarte que trois séries, sur deux verdicts objectifs :

| Motif | Séries |
|---|---|
| Série constante : variance nulle | 1 |
| Doublon strict d'une autre série | 2 |

Une série constante n'apporte rien par définition ; un doublon strict est la même information comptée deux fois, et l'inclure donnerait un poids double à un signal unique. **Ni l'un ni l'autre de ces verdicts ne regarde la cible** : ils se lisent dans la série elle-même.

**Niveau 2 — la sélection statistique, entièrement récursive.** À chaque origine `T`, parmi les 431 séries du vivier, une procédure gloutonne consciente de la disponibilité retient les indicateurs de la passerelle. Corrélation et probabilité critique sont recalculées sur le seul historique antérieur à `T` :

```
gg <- g_branche %>% filter(date < target_date)
correlation = cor(x, g)   ;   p_value = ...
filter(abs(correlation) >= 0,15, p_value < 0,10)
```

avec un minimum de 20 trimestres appariés et un plafond de cinq régresseurs. La liste change d'une origine à l'autre : **neuf listes distinctes par branche en médiane** sur les 48 origines.

**Ce point mérite d'être souligné, car il a coûté.** Une version antérieure du projet sélectionnait 51 séries par test de corrélation calculé sur l'échantillon **complet**. C'était un look-ahead : on choisissait en 2014 ce qui aurait bien marché jusqu'en 2026. Revenir au vivier entier et reporter toute la sélection statistique au niveau récursif était la condition pour que le backtest signifie quelque chose. Les résultats affichés s'en sont trouvés dégradés, ce qui est le signe que la correction était nécessaire.

### 4.4 Les seuils de la sélection récursive, et une règle écartée

Deux seuils gouvernent la sélection, et tous deux sont **des choix assumés**, non dérivés d'une règle externe.

**α = 0,10** plutôt que 0,05, parce qu'il s'agit d'une sélection exploratoire suivie d'une évaluation hors échantillon, et non d'un test confirmatoire. Écarter à tort un indicateur utile coûterait plus qu'en admettre un inutile, que la suite éliminera.

**|r| ≥ 0,15** contre le *piège du grand échantillon* : avec près de 300 observations mensuelles, une corrélation de 0,12 devient « significative » sans avoir la moindre portée économique. La significativité statistique et la pertinence économique divergent quand *n* est grand, et le plancher rétablit la seconde.

Le test lui-même est l'inférence standard sur le coefficient de Pearson :

```
t = r √((n − 2)/(1 − r²))   ~   Student(n − 2)
```

**Aucune règle de taille d'échantillon *a priori* n'est appliquée**, et c'est délibéré. La référence classique en la matière exige `N ≥ 104 + k` observations pour tester un coefficient individuel — soit 105 trimestres quand la valeur ajoutée trimestrielle marocaine n'en compte que 113 au total. L'appliquer ne laisserait aucune marge pour l'évaluation hors échantillon, qui est pourtant la seule preuve qui compte. Cette règle a par ailleurs été construite pour des contextes d'enquête, où l'on peut recruter davantage d'observations ; une série macroéconomique a la longueur que l'histoire lui a donnée. La littérature du nowcasting n'en applique d'ailleurs aucune : la validité s'y établit *a posteriori*, par la performance hors échantillon.

Le seul plancher retenu est pratique : **20 trimestres appariés**, en deçà desquels une corrélation n'a pas de sens.

---

## 5. Les briques, en détail

### 5.1 BVAR trimestriel

Prior de Minnesota implémenté par **observations fictives** (Litterman, 1986 ; Bańbura, Giannone & Reichlin, 2010), ce qui place le modèle dans le cas conjugué normal-inverse-Wishart et donne une vraisemblance marginale en forme close.

Les hyperparamètres — nombre de retards *p*, resserrement λ, décroissance *d* — sont choisis **à chaque origine**, par le critère d'ajustement de Bańbura-Giannone-Reichlin. Trois règles ont été comparées : ce critère (ratio médian 0,997), la vraisemblance marginale de Giannone-Lenza-Primiceri (1,042), et les hyperparamètres fixes de Higgins, *p* = 5 et λ = 0,15 (1,054). La sélection **jointe** sur la grille complète a aussi été testée : sans gain sur la sélection séquentielle.

Les ruptures sont traitées par indicatrices idiosyncratiques détectées par une règle *z* > 4 sur 3 branches simultanées. Sans indicatrices, le ratio médian se dégrade de 0,984 à 1,034 et seules 4 branches sur 16 restent sous 1.

### 5.2 Branches à rupture de variance

Une branche dont la variance du premier tiers de l'échantillon dépasse dix fois celle du reste est déclarée **instable** : le BVAR y estime des dynamiques sur un artefact. L'administration publique est le cas déclencheur — écart-type de 12,9 % contre 0,6 à 1,9 % pour les branches comparables, et une chute de niveau de 13 457 à 9 696 entre T4-1998 et T1-1999 qui est un changement de définition, non un événement économique.

La règle est générale, non ciblée : toute branche satisfaisant le critère est sortie du BVAR et prévue par la moyenne de ses 60 derniers trimestres. Trois corrections alternatives ont été essayées puis rejetées — indicatrices idiosyncratiques, winsorisation générale, winsorisation ciblée — aucune n'atteignant le ratio agrégé de 0,961 obtenu par la sortie du BVAR.

### 5.3 Équations passerelles

Agrégation mensuelle → trimestrielle selon la nature de la série : somme pour les flux, moyenne pour les stocks et les indices, dernière valeur pour les variables de niveau de fin de période. Un trimestre incomplet donne `NA` : jamais une extrapolation implicite.

Le **ragged edge** est traité série par série : à l'origine `T`, chaque indicateur a sa propre profondeur d'observation, et les mois manquants du trimestre cible sont prévus, non supprimés.

Le poids δ vaut 0,5 (section 7.1). Une **garde d'amplitude** écarte toute prévision de branche s'écartant de plus de 5 écarts-types de son historique.

### 5.4 Branches non couvertes

Elles reçoivent la prévision du BVAR, sans traitement particulier. Ce n'est pas
un défaut de conception mais un **résultat mesuré** : l'étape 5 met huit modèles
en concurrence sur ces quatre branches, sur l'ensemble du protocole, et
l'autorégression d'ordre 4 n'y arrive jamais première.

| Branche | Rang de l'AR(4) | Ratio | Meilleur modèle | Ratio |
|---|---|---|---|---|
| Services aux entreprises | 6ᵉ sur 8 | 1,625 | BVAR | 0,968 |
| Administration publique | 3ᵉ sur 8 | 1,097 | moyenne récursive | 1,040 |
| Éducation-santé | 5ᵉ sur 8 | 0,988 | marche aléatoire | 0,757 |
| Autres services | 5ᵉ sur 8 | 1,179 | SARIMA | 0,874 |

Sur *services aux entreprises*, c'est le BVAR lui-même qui l'emporte. Ailleurs,
les vainqueurs sont des modèles différents d'une branche à l'autre — moyenne
récursive, marche aléatoire, SARIMA — et aucun ne domine l'ensemble. Substituer
au BVAR un modèle choisi branche par branche sur la foi d'un classement établi
sur 48 points reviendrait à sélectionner sur l'échantillon d'évaluation
lui-même : exactement ce que le reste du protocole s'interdit.

L'étape 5 est donc conservée pour ce qu'elle est — l'étude qui justifie de **ne
pas** remplacer le BVAR — et ses sorties n'alimentent aucune autre étape.

### 5.5 Comblement des trous

Modèle structurel de séries temporelles (niveau, pente, saisonnier), lissage de Kalman pour les trous **internes** de trois mois au maximum, filtre pour l'extrapolation de fin de série. La distinction entre lissage et filtre est stricte : un lissage à l'extrémité de la série utiliserait le futur.

Le comblement par Kalman des trous a été **testé puis écarté** comme piste d'amélioration : il ne rapporte rien de mesurable. Le mécanisme reste en place pour l'extrapolation du ragged edge, où il est indispensable.

---

## 6. Résultats

### 6.1 Protocole

Backtest pseudo temps réel sur **48 trimestres**, de T2-2014 à T1-2026 — la période où la VA nominale, nécessaire aux poids, est disponible. Un backtest étendu à 2008 a été mené sur les quatre branches dont la VA en volume remonte assez loin, pour disposer d'un second épisode de crise indépendant de 2020.

Le **ratio** rapporte le RMSFE à l'écart-type de la série : sous 1, le modèle fait mieux que prédire la moyenne historique.

### 6.2 L'agrégat

| Modèle | RMSFE | Ratio | Corrélation |
|---|---|---|---|
| **Système complet** | **1,94 %** | **0,852** | **0,77** |
| BVAR seul, agrégé | 2,19 % | 0,961 | 0,60 |
| Moyenne historique | 2,28 % | 1,001 | −0,23 |
| AR(1) | 2,46 % | 1,078 | −0,45 |
| AR(2) | 2,52 % | 1,105 | −0,51 |
| AR(4) | 2,54 % | 1,113 | −0,18 |
| Naïf (trimestre précédent) | 3,33 % | 1,462 | −0,09 |

Deux lectures s'imposent.

**Aucun modèle univarié ne bat la moyenne de long terme.** La croissance trimestrielle marocaine est faiblement — et négativement — autocorrélée : extrapoler le passé revient à parier contre la réalisation, d'où les corrélations négatives des trois AR.

**Le classement global est porté par quatre trimestres.** Hors 2020, le ratio du système remonte à 0,955 et sa corrélation tombe à 0,28 ; l'écart au BVAR devient ténu. L'essentiel de la performance affichée vient de la capacité à voir 2020 arriver. C'est une qualité réelle — un nowcast sert précisément quand la conjoncture décroche — mais le chiffre de 0,860 ne doit pas être présenté comme une performance de régime courant. Le classement, lui, ne s'inverse dans aucune sous-période.

**Aucun écart n'est statistiquement significatif** (Diebold-Mariano avec correction de Harvey-Leybourne-Newbold, *p* ∈ [0,10 ; 0,23]). L'explication tient à la structure du test : la perte différentielle est dominée par 2020, où les erreurs des deux modèles sont grandes *et* leur différence aussi, si bien que la variance croît avec la moyenne. Avec *n* = 48, un écart concentré sur une crise reste hors de portée. Cela ne dit pas que le système ne vaut pas mieux ; cela dit que 48 trimestres ne suffisent pas à le prouver.

### 6.3 Nowcasting intra-trimestriel

Quatre ensembles d'information : M0 (aucun mois du trimestre cible observé) à M3 (les trois, plus les indicateurs trimestriels).

| Scénario | BVAR | Passerelle | Combinaison |
|---|---|---|---|
| M0 | 0,978 | 1,112 | 1,015 |
| M1 | 0,978 | 1,015 | 0,944 |
| M2 | 0,978 | 0,987 | 0,932 |
| M3 | 0,978 | 0,950 | **0,912** |

Le BVAR est **plat par construction** : il ne lit que le passé de la VA, qui ne change pas d'un mois à l'autre. L'écart entre M0 et M3 mesure donc exactement ce que les indicateurs infra-trimestriels apportent. La combinaison passe devant le BVAR dès M1.

### 6.4 D'où vient l'erreur

Le gain sur l'AR(2) vaut **+27 % sur 2020**, +18 % sur les chocs agricoles, +14 % en forte croissance, +13 % en période calme — et il tombe à **+5 % sur les ralentissements**, de loin le régime où le système apporte le moins.

Ce dernier point est contre-intuitif et mérite d'être compris. Les indicateurs mobilisés sont **coïncidents**, non avancés : ils enregistrent un effondrement pendant qu'il se produit, mais un ralentissement graduel ne laisse pas de signature mensuelle assez nette pour se distinguer du bruit. **Le modèle voit les chocs, pas les inflexions.**

Les résidus de l'agrégat ne présentent ni autocorrélation (Ljung-Box, *p* = 0,131) ni hétéroscédasticité conditionnelle (*p* = 0,695), mais rejettent massivement la normalité (Shapiro-Wilk, *p* = 1,3 × 10⁻⁷) : queues épaisses, beaucoup de trimestres presque parfaits et quelques-uns très mauvais.

### 6.5 Incertitude

La distribution prédictive du BVAR est tirée directement dans le cas conjugué normal-inverse-Wishart — sans échantillonneur de Gibbs, donc sans diagnostic de convergence. Elle est **mal calibrée par excès de confiance** : un intervalle nominal à 90 % ne contient le réalisé que 83 % du temps.

La cause est identifiée : les hyperparamètres sont choisis à chaque origine puis traités comme connus. Une recalibration conforme réduit l'écart de moitié ; un déficit de 5 points subsiste, **identique avant et après 2020**, donc structurel et non imputable au choc.

L'intervalle publié est construit **directement au niveau de l'agrégat**, et non propagé branche par branche. La raison est que la propagation analytique exigerait la covariance croisée des erreurs de passerelle entre branches — or les seize passerelles sont des régressions indépendantes, estimées séparément, et cette covariance n'est pas identifiée par le modèle. La supposer nulle sous-estimerait gravement l'intervalle, puisque les branches se trompent *ensemble* lors des ruptures.

Enfin, la décomposition de variance donne un chiffre de cadrage qui éclaire tout le reste : **l'incertitude paramétrique ne pèse que 12 % de la variance prédictive**. Le reste est l'aléa de choc. Mieux estimer ne resserrera pas l'intervalle.

La formulation honnête est donc « intervalle dont la couverture mesurée est de 77 % », et non « intervalle à 80 % ».

---

## 7. Robustesse

Quatre réglages rejoués de bout en bout sur le protocole complet. Le premier a changé le modèle retenu ; les trois autres confirment des choix déjà faits.

### 7.1 Le poids de combinaison, et pourquoi il est constant

Le poids est fixé à **0,5**, sans estimation. Trois mesures fondent ce choix, et il faut les prendre dans l'ordre.

**La courbe de perte, d'abord.** Le ratio de l'agrégat a été calculé pour tout δ de 0 à 1 par pas de 0,05.

| δ | Ensemble | 2020 | Hors 2020 |
|---|---|---|---|
| 0,0 | 0,774 | **0,650** | 0,952 |
| 0,1 | 0,786 | 0,668 | 0,949 |
| **0,2** | 0,800 | 0,687 | **0,948** |
| 0,3 | 0,816 | 0,708 | 0,948 |
| 0,4 | 0,833 | 0,729 | 0,951 |
| 0,5 | 0,852 | 0,752 | 0,955 |
| 0,7 | 0,892 | 0,799 | 0,968 |
| 1,0 | 0,961 | 0,875 | 1,000 |

Deux propriétés s'y lisent. **Hors 2020, la courbe a un minimum intérieur**, vers δ = 0,2 : c'est ce qui valide la combinaison elle-même, puisqu'en régime normal le mélange bat chacune de ses composantes. Et elle est **plate** — hors 2020, tout δ de 0 à 0,55 se tient à moins de 0,01 point de ratio du minimum. C'est la propriété classique d'une perte quadratique : s'écarter de l'optimum coûte au second ordre, alors que l'estimer coûte au premier.

Mais **la position de l'optimum dépend du régime** : 0 en 2020, 0,2 hors 2020. Or au moment de publier, on ignore dans quel régime on se trouve. C'est exactement la configuration où l'on fixe un paramètre au lieu de l'estimer.

**Ce que fait l'estimateur, ensuite.** Le δ estimé par moindres carrés sur l'historique n'est pas instable, contrairement à ce qu'on pourrait croire : son saut médian d'un trimestre au suivant est **nul**, son écart-type intra-branche vaut 0,12, et seul 1 % des sauts dépassent 0,25. Il retombe sur sa valeur par défaut dans 46 % des cas, faute de huit trimestres appariés.

**La comparaison décisive, enfin.** Elle consiste à opposer le δ estimé non pas à 0,5, mais à **une constante placée à son propre niveau moyen** — ce qui isole l'apport de sa *variation*, indépendamment de son niveau :

| Règle | Ratio | Corrélation |
|---|---|---|
| δ estimé (moyenne 0,48) | 0,860 | 0,644 |
| Constante au même niveau (0,48) | **0,848** | **0,765** |
| Constante retenue (0,50) | 0,852 | 0,767 |

À niveau moyen égal, **la constante fait mieux de 0,013 point de ratio et de 0,12 de corrélation**. La variation que l'estimateur introduit d'une branche et d'un trimestre à l'autre n'apporte donc aucune information : elle n'ajoute que du bruit. C'est le *forecast combination puzzle* — Bates & Granger (1969) pour la combinaison, Stock & Watson (2004) pour le constat empirique, Smith & Wallis (2009) et Claeskens *et al.* (2016) pour l'explication par l'erreur d'estimation — observé sur nos propres données.

**Pourquoi 0,5 plutôt qu'une autre constante.** Le poids optimal théorique vaut δ\* = (σ₂² − σ₁₂)/(σ₁² + σ₂² − 2σ₁₂), et **les poids égaux sont exactement optimaux quand les deux composantes ont la même variance d'erreur**. 0,5 est donc le cas de référence : la valeur qu'on adopte quand on refuse de prétendre savoir laquelle des deux prévisions est la meilleure. C'est la seule valeur justifiable *a priori*, sans regarder les données.

**Une réserve, à ne pas dissimuler.** Sur cet échantillon, **δ ≈ 0,2 à 0,3 domine 0,5 dans les trois colonnes**. Retenir 0,3 parce que la courbe le dit reviendrait toutefois à ajuster un paramètre sur 48 trimestres dont quatre décident de presque tout — et la position de l'optimum change selon qu'on inclut 2020 ou non. Le coût de ce refus est mesuré : **+0,007 point de ratio hors 2020**, ce qui est le prix d'un réglage qui ne doit rien à l'échantillon.

La figure et les tableaux sont produits par `R/14b_courbe_delta.R`.

### 7.2 Les autres réglages

Au niveau des **branches**, pour mémoire, les cinq règles comparées par la phase 23 tiennent dans 0,006 point de ratio médian — le réglage n'y décide de rien, et c'est à l'échelle de l'agrégat que la question se tranche.

**La fenêtre d'estimation est un choix, elle.** Les fenêtres glissantes de 40 et 60 trimestres détruisent la corrélation — 0,24 contre 0,02 et 0,05 — tout en dégradant le ratio : elles jettent précisément les épisodes rares dont le modèle a besoin pour reconnaître une rupture. La fenêtre extensible est retenue.

**Le seuil de sélection ne mord jamais.** Les quatre variantes récursives testées donnent des résultats rigoureusement identiques. La sélection **fixe**, calculée une fois sur tout l'échantillon, est moins bonne (0,953 contre 0,905) alors qu'elle bénéficie d'un look-ahead : un indicateur utile en 2014 ne l'est plus en 2022, et une sélection figée le conserve à tort.

**La saisonnalité résiduelle est nulle.** Une régression de la croissance sur les indicatrices de trimestre donne un R² médian de 0,013 par branche, maximum 0,069 sur la pêche. Les séries sont bien désaisonnalisées ; il n'y a pas de gain caché de ce côté.

---

## 8. Voie directe contre voie indirecte

Tout le système repose sur un choix : prévoir seize branches puis agréger. L'alternative — estimer une passerelle directement sur la croissance de l'agrégat — a été testée sur le même protocole.

| Voie | Ratio global | 2020 | Hors 2020 |
|---|---|---|---|
| Combinaison des deux | 0,677 | 0,408 | 1,109 |
| Directe | 0,727 | 0,269 | 1,352 |
| **Indirecte (retenue)** | **0,852** | **0,752** | **0,955** |

Sur l'ensemble, la voie directe paraît nettement meilleure. La décomposition renverse le tableau : spectaculaire en 2020, et **pire que de ne rien prévoir** hors 2020. Ce n'est pas un meilleur modèle, c'est un détecteur de choc. La combinaison des deux voies hérite du même défaut, en plus atténué : 1,109 hors 2020.

Aucun écart n'est significatif (*p* ≥ 0,370) : les données ne départagent pas les deux voies. La voie indirecte est retenue pour trois raisons, aucune n'étant statistique — **la stabilité entre régimes** (un modèle dont le ratio passe de 0,27 à 1,35 n'est pas utilisable en production, puisqu'on ne sait pas, au moment de publier, dans quel régime on se trouve), **la parcimonie** (la voie directe régresse une seule série de 48 points sur un panier agrégé, d'où un surajustement structurellement plus probable), et **la lisibilité** (la voie indirecte fournit une décomposition par branche, la voie directe un seul nombre).

---

## 9. Ce qui a été essayé et n'a pas marché

Sept raffinements ont été testés puis écartés, chacun sur le protocole complet : sélection jointe des hyperparamètres, pondération géométrique, régression ridge avec GCV, comblement des trous par Kalman, δ dépendant de l'état, seuil renforcé pour les indicateurs trimestriels, sélection conditionnelle.

Deux hypothèses ont été réfutées : la saisonnalité résiduelle (R² médian 0,013) et la compensation des erreurs à l'agrégation.

Le faisceau est cohérent, et la décomposition de variance de la section 6.5 en donne la raison : **le plafond est dans les données**. Aucun raffinement d'estimation ne crée de l'information qui n'y est pas. Ce qui a fonctionné, ce ne sont pas les modèles plus savants, mais deux changements de cadrage : évaluer *au bon moment du trimestre*, et *agréger* plutôt que moyenner des branches.

---

## 10. Limites assumées

1. **Les millésimes.** Évaluation contre une VA révisée ; les révisions ne sont pas reconstituables sans les données correspondantes.
2. **Les délais de publication.** Tout indicateur du mois *m* est supposé connu le dernier jour de *m*. Le test de sensibilité chiffre le coût du délai le plus dommageable (section 3.4).
3. **Aucune comparaison au total officiel.** L'agrégat est comparé à l'indice reconstruit à partir des seize branches, non au total publié par le HCP.
4. **Un backtest court.** 48 trimestres, dont une seule crise pleinement couverte. Le backtest étendu à 2008 n'a pu porter que sur quatre branches.
5. **La constitution du vivier n'est pas récursive** (section 4.3), entorse résiduelle bornée par le résultat de la section 7 sur la sélection fixe.
6. **Le pouvoir prédictif reste modeste** hors périodes de rupture, et le système n'apporte rien sur les ralentissements graduels (section 6.4).
7. **Les tests multiples ne sont pas corrigés.** De nombreuses spécifications ont été comparées ; les *p*-values rapportées ne tiennent pas compte de cette multiplicité.
8. **α = 0,10 et |r| ≥ 0,15** restent des choix assumés, non dérivés d'une règle externe.

---

## 11. Références

| Référence | Usage |
|---|---|
| Higgins, P. (2014). *GDPNow: A Model for GDP "Nowcasting"*, FRB Atlanta WP 2014-7 | architecture générale, ragged edge, combinaison |
| Litterman, R. (1986). *Forecasting with Bayesian Vector Autoregressions* | prior de Minnesota |
| Bańbura, M., Giannone, D. & Reichlin, L. (2010). *Large Bayesian VARs*, JAE 25(1) | observations fictives, critère d'ajustement |
| Giannone, D., Lenza, M. & Primiceri, G. (2015). *Prior Selection for Vector Autoregressions*, REStat 97(2) | vraisemblance marginale en forme close |
| Doz, C., Giannone, D. & Reichlin, L. (2011). *A Two-Step Estimator for Large Approximate Dynamic Factor Models*, JoE 164(1) | mécanisme du ragged edge |
| Diebold, F. & Mariano, R. (1995). *Comparing Predictive Accuracy*, JBES 13(3) | test de précision comparée |
| Bates, J. & Granger, C. (1969). *The Combination of Forecasts*, OR Quarterly 20(4) | principe de la combinaison, poids optimal |
| Stock, J. & Watson, M. (2004). *Combination Forecasts of Output Growth*, JoF 23(6) | constat empirique : les poids égaux battent les poids estimés |
| Smith, J. & Wallis, K. (2009). *A Simple Explanation of the Forecast Combination Puzzle*, OBES 71(3) | explication par l'erreur d'estimation des poids |
| Claeskens, G., Magnus, J., Vasnev, A. & Wang, W. (2016). *The Forecast Combination Puzzle: A Simple Theoretical Explanation*, IJF 32(3) | traitement théorique du même phénomène |
| Harvey, D., Leybourne, S. & Newbold, P. (1997). *Testing the Equality of Prediction Mean Squared Errors*, IJF 13(2) | correction petit échantillon du DM |
| Golub, G., Heath, M. & Wahba, G. (1979). *Generalized Cross-Validation*, Technometrics 21(2) | choix du λ de la ridge (piste écartée) |
| Green, S.B. (1991). *How Many Subjects Does It Take To Do A Regression Analysis?*, MBR 26(3) | règle de taille d'échantillon, examinée puis écartée (section 4.4) |
| Bai, J. & Ng, S. (2008). *Forecasting economic time series using targeted predictors*, JoE 146(2) | discutée puis écartée (pertinente pour un DFM, non pour une passerelle univariée) |
| Wooldridge, J. *Introductory Econometrics* | inférence standard sur le coefficient de Pearson |
| Gujarati, D. *Basic Econometrics* | seuil de non-redondance |

---

## 12. Documents liés

- `report/rapport_phase2.html` — données, transformations, stationnarité
- `report/rapport_phase3.html` — le BVAR de bout en bout, 19 sections et 35 équations
- `report/rapport_phases4_13.html` — passerelles, pistes d'amélioration, nowcasting intra-trimestriel
- `report/rapport_synthese.html` — architecture, résultats, étalons, robustesse, diagnostics, limites
- `PLAN_CORRECTIONS_NOWCASTING.md` — le plan de correction appliqué, 36 sections
- `README.md` — installation, exécution, structure des fichiers
