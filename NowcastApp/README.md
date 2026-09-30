# Application de production du nowcast

Interface Shiny qui produit et diffuse le chiffre du trimestre en cours. Elle ne
contient aucun modèle : elle lit les sorties de la chaîne `Method1BvarBridge/` et,
pour recalculer, appelle sa fonction centrale.

## Lancer

```bash
Rscript NowcastApp/lancer.R
```

Le script vérifie les bibliothèques, localise la chaîne et contrôle la présence
des sorties indispensables avant d'ouvrir le navigateur sur
`http://127.0.0.1:4321`. Si quelque chose manque, il le dit et s'arrête plutôt
que d'afficher une page vide. Depuis RStudio : `source("NowcastApp/lancer.R")`.

## Deux dossiers, et il faut les distinguer

L'application vit à la racine du dépôt, à côté de la chaîne et non dedans. Deux
dossiers entrent donc en jeu, et les confondre serait l'erreur la plus naturelle
ici.

| | Rôle |
|---|---|
| `NowcastApp/` | où l'application **écrit**, et seulement là : productions horodatées, journal, contrôles, observations saisies dans l'interface |
| `Method1BvarBridge/` | où l'application **lit** : sorties du pipeline, données par branche, figures, rapport. Elle n'y écrit jamais |

Cette séparation est ce qui permet d'affirmer, sans avoir à le vérifier à chaque
fois, qu'une exécution de l'application ne peut pas altérer la chaîne. Le nom du
dossier de la chaîne n'est pas codé en dur : l'application la reconnaît à ce
qu'elle contient, de sorte qu'un renommage ne casse rien en silence.

## Les dix onglets

| Onglet | Contenu |
|---|---|
| **Tableau de bord** | Quatre indicateurs de tête (nowcast, trimestre visé et scénario, intervalle à 80 %, dernier calcul), contribution de chaque branche, répartition par mode de traitement, huit derniers trimestres, détail par branche |
| **Exploration** | Les seize séries cibles en niveau et en croissance, stationnarité, statistiques descriptives, corrélation entre branches, indicateurs contre cible, bord irrégulier |
| **Modèle & prévisions** | Hyperparamètres re-choisis à chaque origine, règles de spécification comparées, branches à rupture de variance, indicateurs retenus origine par origine, courbe de perte en δ, branches non couvertes, agrégation de Laspeyres, voie directe contre indirecte |
| **Incertitude** | Intervalle publié, couverture effective, enveloppe prédictive sur tout le protocole, décomposition chocs contre paramètres |
| **Validation** | Backtest, étalons et Diebold-Mariano, épisodes, contribution des branches à l'erreur, apport de chaque mois d'information, contrôles d'antériorité, figures de la chaîne |
| **Mise à jour des données** | Ajout d'observations et recalcul (voir plus bas) |
| **Export** | Classeur de diffusion (xlsx, neuf feuilles) et détail par branche (csv) |
| **Sources** | Périmètre, période et statut des seize cibles et du vivier d'indicateurs |
| **Rapport** | Le rapport global de la méthode 1, affiché dans l'application |
| **À propos** | Méthodologie, résultats, limites assumées, références |

Un bouton bascule l'ensemble en mode sombre. Les graphiques sont re-thématisés
côté serveur, faute de quoi leur fond resterait blanc.

## Mise à jour des données

Le grand classeur est la source et **n'est jamais modifié**. Une observation
saisie dans l'interface est rangée dans `donnees_ajoutees/`, conservée d'une
session à l'autre, et fusionnée aux données au moment du recalcul. Resaisir la
même période remplace la valeur au lieu de la dupliquer, et un bouton rend les
données à leur état d'origine.

Trois types d'ajout :

- **un indicateur infra-trimestriel**, le cas courant : un mois de plus sur une
  série déjà utilisée. La date saisie est ramenée au dernier jour de sa période,
  mensuelle ou trimestrielle, sans quoi elle ne rejoindrait aucune grille ;
- **une valeur ajoutée en volume**, à la publication d'un nouveau trimestre. La
  matrice des branches et ses taux de croissance sont reconstruits ;
- **une valeur ajoutée nominale**, dont se déduisent les poids d'agrégation. Elle
  n'est prise en compte que lorsque les **seize** branches du trimestre sont
  renseignées : une part calculée sur quinze branches serait fausse pour toutes.

Publier un trimestre demande donc les deux dernières : le volume, qui déplace la
cible, et le nominal, qui fournit les poids de la cible suivante. Un panneau
*Où en est le trimestre* indique en permanence ce qui manque, branche par
branche. Tant que l'un des deux est incomplet, le recalcul échoue en le disant.

### Garde-fous à la saisie

Une valeur est jugée **avant** enregistrement, sur la variation qu'elle implique
par rapport à la dernière observation, rapportée à la dispersion des variations
passées. Le critère porte sur la variation et non sur l'amplitude du niveau,
parce qu'une série qui croît bat son propre maximum à chaque période : un
contrôle sur le niveau crierait au loup à chaque saisie légitime. La dispersion
est mesurée par l'écart absolu médian, pour qu'une série contenant déjà 2020 ne
devienne pas aveugle.

Au-delà de cinq fois cette dispersion, l'application avertit. Au-delà de dix,
elle demande confirmation. Une période déjà observée est signalée avec sa valeur
actuelle, puisque la saisie la remplacera.

### Effet du dernier recalcul

Après chaque production, un panneau compare la précédente à la nouvelle, sur le
même trimestre : écart de l'agrégat, changement de scénario, et le détail par
branche avec prévision avant, après, et effet sur l'agrégat, c'est-à-dire l'écart
pondéré par le poids sectoriel. La somme des effets reconstitue le déplacement de
l'agrégat, au terme de Jensen près.

## Le recalcul

Le bouton *Recalculer* ne fait pas le calcul : il lance un **processus R
détaché** qui s'en charge, et rend la main en moins d'un dixième de seconde. Les
deux ne communiquent que par des fichiers, un fichier d'état relu chaque seconde
et les sorties habituelles.

L'application reste donc entièrement utilisable pendant le calcul : on peut
consulter les autres onglets, et le chiffre précédent reste affiché jusqu'à ce
que le nouveau soit écrit. Le calcul survit aussi à la fermeture de la fenêtre.
Comptez de une à quatre minutes selon la charge de la machine. Une barre
d'avancement suit les étapes ; si le processus est interrompu, l'état est
considéré comme abandonné au bout de quinze minutes et un nouveau lancement
redevient possible.

Le calcul passe par `backtest_nowcast()`, la fonction centrale validée contre le
pipeline phase par phase : **le chiffre affiché suit donc exactement le chemin de
calcul qui a été évalué**. Vérifié depuis ce dossier : écart nul avec la sortie
de la chaîne.

Le trimestre visé et le scénario ne se règlent pas, ils se constatent. La cible
est le trimestre qui suit la dernière valeur ajoutée disponible, et le scénario
le nombre de mois de ce trimestre déjà présents dans les indicateurs. Offrir un
réglage reviendrait à permettre de fabriquer un chiffre en supposant plus
d'information qu'il n'y en a.

## Contrôles du recalcul

`backtest_nowcast()` exerce ses propres contrôles bloquants à chaque appel, mais
en silence : si tout va bien, on n'apprend rien. L'application mesure donc, sur
le calcul qui vient d'avoir lieu, six contrôles affichés dans l'onglet
*Validation*, distincts de ceux du protocole qui portent sur quarante-huit
origines.

| | Ce qui est vérifié |
|---|---|
| A1 | aucune observation postérieure à la fin du trimestre visé n'entre dans le calcul |
| A2 | la valeur ajoutée du trimestre visé n'est pas dans l'échantillon |
| A3 | les poids proviennent d'un trimestre antérieur à la cible |
| A4 | seize branches pondérées, de somme un |
| A5 | le poids de combinaison est constant, donc non estimé |
| A6 | perturber les données postérieures à la cible ne change rien |

Ils s'exécutent **avant toute écriture** : un chiffre dont un contrôle échoue
n'est ni archivé ni affiché. Aucun contrôle n'est déclaré réussi par défaut.
Quand une vérification est sans objet, elle est marquée comme telle, jamais comme
une réussite : annoncer une garantie qu'on n'a pas exercée serait pire que de
n'en annoncer aucune.

A6 est le contrôle décisif. On saccage les données postérieures au trimestre
visé, signe inversé, facteur cinq, décalage de vingt, on refait entièrement le
nowcast, et l'on vérifie qu'il n'a pas bougé. Aucune relecture de code ne donne
cette garantie. Il coûte deux nowcasts complets et se lance donc à la demande
depuis l'onglet *Validation*. En production, la cible étant le dernier trimestre,
il n'existe le plus souvent aucune donnée postérieure : le contrôle est alors
marqué sans objet.

## Ce que l'application écrit

| Fichier | Contenu |
|---|---|
| `productions/AAAAMMJJ_HHMMSS_agregat.csv` | l'agrégat, archivé à chaque exécution |
| `productions/AAAAMMJJ_HHMMSS_branches.csv` | la décomposition par branche, idem |
| `productions/dernier_*.csv` | la dernière exécution, pour l'affichage |
| `productions/journal_executions.csv` | une ligne par exécution : horodatage, chiffre, réglages, observations ajoutées, durée |
| `productions/derniers_controles.csv` | les contrôles d'antériorité du dernier calcul |
| `productions/controle_perturbation.csv` | le résultat du contrôle décisif, quand il a été exécuté |
| `productions/derniere_comparaison*.csv` | l'effet du dernier recalcul, agrégat et détail par branche |
| `productions/tache_statut.csv` | l'état du calcul en cours, relu par l'interface |
| `donnees_ajoutees/ajouts_*.csv` | les observations saisies dans l'interface |

L'archivage horodaté permet de reconstituer après coup le chiffre annoncé à une
date donnée : c'est la condition pour qu'un nowcast diffusé soit défendable.

Le nowcast affiché est **le plus récent** des deux, celui de l'application ou
celui de la chaîne, et non systématiquement celui de l'application : sans quoi
une exécution complète du pipeline resterait invisible derrière un chiffre plus
ancien. Sa provenance et son horodatage sont visibles en permanence.

Deux bandeaux signalent l'état du système : les sorties de la chaîne absentes, et
le fait que les mesures de qualité, les contrôles et la **largeur** des
intervalles décrivent la dernière exécution complète du protocole, avec sa date.
Les **bornes** de l'intervalle, elles, sont recentrées sur le chiffre affiché :
la calibration conforme produit des écarts, pas des bornes, et ces écarts restent
valides quand le point se déplace.

## Organisation

```
NowcastApp/
├── app.R                  interface et logique
├── R/fonctions_app.R      lecture des sorties, fusion des ajouts, contrôles
├── tache/production.R     le calcul, exécuté dans un processus détaché
├── lancer.R               point d'entrée, avec vérification préalable
├── donnees_ajoutees/      les observations saisies dans l'interface
├── productions/           les nowcasts produits, horodatés, et le journal
└── README.md
```

Le script de calcul est **hors de `R/`** à dessein : Shiny source
automatiquement tout fichier de ce dossier au démarrage, ce qui lançait un calcul
complet à chaque ouverture de l'application.

## Dépendances

`shiny`, `bslib`, `DT`, `plotly`, `ggplot2`, `dplyr`, `tidyr`, `tibble`,
`writexl`, plus celles de la chaîne (`readxl`, `lubridate`, `purrr`, `stringr`).

## Limites connues

L'application n'a aucune authentification et écrit des fichiers. En local c'est
sans objet ; partagée, n'importe qui pourrait saisir des observations qui entrent
dans un chiffre diffusé.

Elle ne couvre que la méthode 1, celle qui produit le chiffre publié. La méthode
à facteurs dynamiques reste un travail de comparaison, documenté dans son propre
rapport. Les mêler dans un outil de production inviterait à choisir au cas par
cas, ce qu'un protocole interdit précisément.
