# GDPNow-Maroc — Interface Shiny

Interface interactive du modèle de nowcasting sectoriel du PIB marocain :
tableau de bord, exploration des données, détail des modèles, validation
hors échantillon, et **ajout de nouvelles observations au fil du temps**.

## Installation

```r
install.packages(c(
  "shiny", "bslib", "DT", "plotly",
  "dplyr", "tidyr", "ggplot2", "lubridate", "purrr", "stringr",
  "readxl", "forecast", "tseries"
))
```

(Sur Ubuntu/Debian, tous ces packages sont aussi installables via
`apt install r-cran-shiny r-cran-bslib r-cran-dt r-cran-plotly ...` — plus
rapide, binaires précompilés.)

## Lancer l'application

Depuis le dossier `shiny_app/` :

```r
shiny::runApp(".")
```

ou, en ligne de commande :

```bash
Rscript -e 'shiny::runApp(".", port = 3838, launch.browser = TRUE)'
```

## Structure

```
shiny_app/
├── app.R                       Interface (UI) + logique serveur
├── R/
│   └── pipeline_fonctions.R    Tout le moteur de modélisation, en fonctions
│                                réutilisables (import, BVAR, bridge
│                                equations, AR(4), agrégation, backtest)
├── data/
│   ├── Series_retenues_modelisation.xlsx   Classeur source (16 branches)
│   ├── ajouts_cibles.csv                   Créé automatiquement à la
│   │                                        première observation ajoutée
│   └── ajouts_indicateurs.csv              Idem, pour les indicateurs
└── README.md
```

## Les 6 onglets

1. **Tableau de bord** — nowcast du prochain trimestre, contribution de
   chaque branche, tableau détaillé des 16 prévisions.
2. **Exploration** — trajectoire de chaque branche (niveau ou taux de
   croissance), test de stationnarité (ADF), corrélation croisée entre
   branches, nuages de points indicateur/cible pour les branches couvertes.
3. **Modèle & prévisions** — détail du BVAR (16 branches), des équations
   de passerelle (poids δ BVAR/bridge, nombre d'indicateurs), et des AR(4).
4. **Validation** — backtest en pseudo temps réel à la demande (curseur
   pour choisir le nombre de trimestres testés), RMSFE, test de
   Diebold-Mariano.
5. **Ajouter des données** — voir ci-dessous.
6. **À propos** — méthodologie et références complètes.

## Fonctionnalité clé : ajouter des données au fil du temps

L'onglet **« Ajouter des données »** permet, sans toucher au code :

1. **Ajouter une observation à la variable cible** (VA d'une branche pour
   un trimestre donné) — utile dès qu'un nouveau trimestre est publié par
   le HCP.
2. **Ajouter une observation à un indicateur** existant, ou **créer un
   nouvel indicateur** à la volée (champ de saisie libre) — utile pour
   intégrer une nouvelle source de données découverte après coup.

Chaque ajout est **sauvegardé de façon permanente** dans
`data/ajouts_cibles.csv` / `data/ajouts_indicateurs.csv` — ces fichiers
sont relus à chaque démarrage de l'application, donc les données ajoutées
restent disponibles d'une session à l'autre (pas besoin de tout ressaisir).

Le bouton **« Recalculer le modèle avec les nouvelles données »** relance
l'intégralité du pipeline (BVAR, bridge equations, AR(4), agrégation) sur
les données à jour, et rafraîchit tous les onglets. Le recalcul prend
environ 1 à 2 secondes.

## Limites à connaître

- Le classeur source (`Series_retenues_modelisation.xlsx`) reste la base
  de référence : les nouvelles observations viennent s'y ajouter, mais ne
  le modifient pas directement — pour une mise à jour "officielle" du
  classeur, il faut régénérer celui-ci séparément (voir le pipeline batch
  dans `R_gdpnow_maroc/R/`).
- Ajouter une observation à un **nouvel indicateur** ne garantit pas que
  cet indicateur passera les critères de sélection rigoureux (fréquence,
  longueur, fraîcheur, densité, corrélation) — il est simplement intégré
  tel quel dans les bridge equations dès qu'il a au moins 10 points
  communs avec la cible. À toi de juger de sa pertinence avant de t'y fier.
- Le backtest (onglet Validation) ré-estime tout le pipeline à chaque
  trimestre testé — le temps de calcul augmente avec le curseur (jusqu'à
  ~10 secondes pour 12 trimestres).
