# Nowcasting de la valeur ajoutée trimestrielle du Maroc

Estimer la croissance de la valeur ajoutée d'un trimestre **avant que les
comptes nationaux ne la publient**, à partir des indicateurs conjoncturels déjà
disponibles.

Le système adapte la logique du modèle GDPNow de la Federal Reserve Bank
d'Atlanta (Higgins, 2014) à une approche **par l'offre** : la valeur ajoutée est
prévue branche par branche, dans la nomenclature du HCP à seize branches, puis
agrégée. Deux méthodes ont été construites jusqu'au bout et comparées sur le même
protocole.

---

## Organisation du dépôt

```
STAGE/
├── SourceData/              les classeurs qui alimentent les chaînes
├── RawData/                 la collecte brute, par institution productrice
├── Method1BvarBridge/       méthode 1 : BVAR + équations de passerelle
├── Method2DynamicFactors/   méthode 2 : modèle à facteurs dynamiques
├── NowcastApp/              l'application de production
├── Report/                  le rapport complet, en LaTeX
├── Documentation/           documents de référence
└── Archive/                 travaux antérieurs, hors suivi git
```

Les classeurs sources existent **en un seul exemplaire**, dans `SourceData/`.
Les deux méthodes y puisent par un chemin relatif. Auparavant chacune gardait sa
copie, soit trois exemplaires du même fichier, donc trois occasions de diverger
sans que rien ne le signale.

---

## Les données

`SourceData/` contient ce dont les chaînes ont besoin :

| Fichier | Rôle |
|---|---|
| `GDPNow_Maroc_series_retenues_Apres_Tris_economiques.xlsx` | le vivier d'indicateurs infra-trimestriels, 434 séries dont 431 retenues |
| `VA_reelle_par_branche.xlsx` | la cible : valeur ajoutée en volume, 16 branches, depuis 1998 |
| `VA_nominale_base2014.csv` | la valeur ajoutée nominale, dont se déduisent les poids d'agrégation |
| `Etude_sectorielle_Maroc_complete.xlsx` | l'étude sectorielle préalable au tri |
| `ipc_maroc_2010_2026_raccorde.csv` | l'indice des prix reconstruit et raccordé |

Le vivier est le produit d'un **tri économique**. Aucun critère statistique
n'intervient dans sa constitution : la justification est économique de bout en
bout. Toute la sélection statistique vient ensuite, à l'intérieur du protocole,
et elle est refaite à chaque trimestre d'évaluation.

`RawData/` conserve la collecte d'origine, par institution : Bank Al-Maghrib et
l'ANCFCC, les annuaires statistiques du HCP, Manar-Stat du ministère de
l'Économie et des Finances, et les bases consolidées multi-sources. Ces fichiers
ne sont pas lus par les chaînes ; ils documentent d'où viennent les séries.

---

## Méthode 1 : vecteur autorégressif bayésien et équations de passerelle

`Method1BvarBridge/`. C'est la méthode de production, celle qui produit le
chiffre publié. Trois étages :

1. **BVAR trimestriel** sur les seize branches, prior de Minnesota imposé par
   observations fictives. L'ordre *p* et le resserrement *λ* sont re-choisis à
   chaque origine sur la seule information antérieure.
2. **Équations de passerelle** pour les branches disposant d'indicateurs
   infra-trimestriels. La sélection est refaite à chaque origine et ne considère
   que les séries effectivement disponibles à cette date : le bord irrégulier est
   traité série par série.
3. **Agrégation** par indice de volume de Laspeyres, à poids en valeur du
   trimestre précédent.

Les deux premiers étages sont combinés à poids fixe δ = 0,5 lorsque les deux
existent. Là où la passerelle manque, le nowcast de la branche est la prévision
du BVAR : c'est le cas permanent de quatre branches sans indicateur dans le
vivier, et le cas ponctuel de branches couvertes dont aucun indicateur n'est
disponible à la date considérée.

La documentation détaillée est dans `Method1BvarBridge/README.md`, la
justification méthodologique dans `NOTE_METHODOLOGIQUE.md`, et le développement
complet dans `report/rapport_global.html` (30 sections, 68 figures).

## Méthode 2 : modèle à facteurs dynamiques

`Method2DynamicFactors/`. Reprise rigoureuse de l'approche d'un collègue, sur le
**même vivier**, avec le **même protocole d'évaluation** et la **même formule
d'agrégation**, afin que l'écart mesuré à la fin soit imputable au modèle et non
à l'habillage.

Une réserve, mesurée et non supposée : sur les quatre branches sans indicateur,
la méthode 2 emploie une autorégression là où la méthode 1 emploie son vecteur
autorégressif. Ces quatre branches pèsent un quart de la valeur ajoutée, et
l'autorégression y fait moins bien dans trois cas sur quatre. En les traitant
comme la méthode 1, le ratio de la méthode 2 passerait de 0,983 à **0,972**. La
comparaison ci-dessous défavorise donc légèrement les facteurs dynamiques, sans
que cela change le classement.

Le panel de chaque branche est résumé par quelques facteurs communs suivant un
VAR, estimés par maximum de vraisemblance via l'algorithme EM, avec agrégation
temporelle de Mariano et Murasawa pour relier les mois au trimestre. Le nombre de
facteurs et l'ordre du VAR sont choisis **récursivement**, sur la performance
hors échantillon des origines précédentes.

Développement complet dans `Method2DynamicFactors/report/rapport_dfm.html`.

---

## Résultats

Quarante-huit trimestres en temps réel simulé. Le **ratio** rapporte l'erreur
quadratique moyenne à l'écart-type de la série visée : sous 1, le modèle fait
mieux que de prédire la moyenne historique, ce qui est la référence la plus
sévère.

| Modèle | RMSE | Ratio | Corrélation |
|---|---|---|---|
| **Méthode 1, système complet** | **1,94 %** | **0,852** | **0,77** |
| BVAR seul, agrégé | 2,19 % | 0,961 | 0,60 |
| Méthode 2, facteurs dynamiques | 2,24 % | 0,983 | 0,14 |
| Moyenne historique | 2,28 % | 1,001 | −0,23 |

La méthode 1 devance la méthode 2 sur l'agrégat, mais le test de
Diebold-Mariano donne p = 0,137 : l'écart n'est **pas** statistiquement
significatif. Quarante-huit points, c'est peu. Un écart non significatif ne dit
pas que les deux méthodes se valent, il dit que l'échantillon ne permet pas de
trancher.

Le diagnostic de la méthode 2 est plus instructif que son classement. La part de
variance de la cible que ses facteurs expliquent varie énormément d'une branche à
l'autre, et elle est très faible là où le panel est pourtant riche. Les facteurs
captent la variation commune des indicateurs, qui n'est pas la variation
pertinente pour la valeur ajoutée. Le détail branche par branche est en section 9
de `Method2DynamicFactors/report/rapport_dfm.html`.

La méthode 1 est donc retenue pour la production, sans que la méthode 2 soit
écartée comme fausse : elle documente une alternative sérieuse, et l'écart qui la
sépare reste dans le bruit d'échantillonnage.

---

## La règle qui gouverne tout

À chaque origine `T`, **toute** quantité est recalculée sur la seule information
disponible à cette date : hyperparamètres, sélection des indicateurs,
coefficients, poids de combinaison, poids d'agrégation, moyennes et écarts-types
de standardisation.

La même fonction produit le chiffre publié et chacun des 48 points du backtest :
**le chiffre publié emprunte le chemin de code qui a été évalué**. Des contrôles
bloquants refusent l'exécution si une donnée postérieure à `T` est touchée, et le
plus fort d'entre eux ne relit pas le code : il saccage les données postérieures
à la cible, refait le calcul, et vérifie que le résultat n'a pas bougé.

Corriger trois fuites d'information a **dégradé** les résultats affichés, ce qui
est l'indice que la correction était juste.

---

## Reproduire

Depuis `Method1BvarBridge/` :

```bash
Rscript run_pipeline.R
```

37 scripts, de l'import du classeur au rapport final. `reprendre_pipeline.R`
permet de repartir d'une étape nommée. Depuis `Method2DynamicFactors/`, la chaîne
compte 9 scripts et lit les sorties de la méthode 1 pour la comparaison ; elle
suppose donc que la première a tourné.

Dépendances R : `readxl`, `dplyr`, `tidyr`, `tibble`, `purrr`, `stringr`,
`lubridate`, `ggplot2`, et pour l'application `shiny`, `bslib`, `DT`, `plotly`,
`writexl`.

Le rapport complet se compile depuis `Report/` par `Rscript compiler.R`, qui
s'appuie sur TinyTeX.

## L'application de production

```bash
Rscript NowcastApp/lancer.R
```

Interface Shiny : le nowcast du trimestre, sa marge d'incertitude, sa
décomposition par branche, l'exploration des séries, le détail des modèles, la
validation, et la mise à jour des données au fil des publications.

L'application vit à côté de la chaîne et non dedans. Elle **lit** les sorties de
`Method1BvarBridge/` et n'**écrit** que dans son propre dossier : productions
horodatées, journal, contrôles, observations saisies. Une exécution de
l'application ne peut donc pas altérer la chaîne.

Le recalcul tourne dans un processus détaché, de sorte que l'interface reste
utilisable pendant qu'il travaille, et il passe par la fonction centrale validée
contre le pipeline : le chiffre affiché suit exactement le chemin de calcul
évalué. Six contrôles d'antériorité sont mesurés avant toute écriture. Mode
d'emploi dans `NowcastApp/README.md`.

---

## Conventions

Trois règles, respectées partout :

1. Les classeurs sources sont **lus, jamais modifiés**. Les observations saisies
   dans l'application vont dans un fichier à part et sont fusionnées à la
   lecture.
2. Toute observation est datée au **dernier jour** de sa période. Les classeurs
   datent au premier jour ; la conversion se fait à la lecture.
3. Les échanges entre étapes se font par **CSV uniquement**, jamais par `.rds`.

---

## Ce que le système ne sait pas faire

À dire avant qu'on ne le demande :

* il n'apporte presque rien sur les **ralentissements graduels** : les
  indicateurs sont coïncidents, ils enregistrent les chocs, pas les inflexions ;
* il est évalué contre des **comptes révisés**, que le modèle n'aurait pas eus en
  temps réel : la performance mesurée est un majorant ;
* **quatre branches** restent sans indicateur infra-trimestriel. Elles pèsent un
  quart de la valeur ajoutée, et pour elles le scénario d'information ne change
  rien. C'est la principale marge de progrès, et elle se trouve du côté de la
  collecte, non du côté de l'estimation.

---

## Archive

`Archive/` conserve tout ce qui a précédé et qui n'alimente plus rien : les
méthodes explorées puis abandonnées (MIDAS, une première version à facteurs
dynamiques, des autorégressions glissantes), les versions intermédiaires des
scripts, les sauvegardes horodatées, la première application Shiny et les
documents de cadrage. Ce dossier est **hors suivi git** : il existe pour la
traçabilité du travail, pas pour être relu.

---

## Références principales

Higgins, P. (2014), *GDPNow: A Model for GDP Nowcasting*, FRB Atlanta WP 2014-7.
Bańbura, M., Giannone, D., Reichlin, L. (2010), *Large Bayesian VARs*, JAE 25(1).
Litterman, R. (1986), *Forecasting with Bayesian Vector Autoregressions*.
Giannone, D., Lenza, M., Primiceri, G. (2015), *Prior Selection for Vector
Autoregressions*, REStat 97(2).
Doz, C., Giannone, D., Reichlin, L. (2012), *A Quasi-Maximum Likelihood Approach
for Large Approximate Dynamic Factor Models*, REStat 94(4).
Mariano, R., Murasawa, Y. (2003), *A New Coincident Index of Business Cycles*,
Journal of Applied Econometrics 18(4).
Diebold, F., Mariano, R. (1995), *Comparing Predictive Accuracy*, JBES 13(3).
Harvey, D., Leybourne, S., Newbold, P. (1997), *Testing the Equality of
Prediction Mean Squared Errors*, IJF 13(2).
