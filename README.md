# Morrocan GDP Nowcasting — GDPNow-Maroc

Nowcasting sectoriel du PIB marocain : classeur de données synthétique fiabilisé
(`Etude_sectorielle_Maroc_2_complete.xlsx`) et modèle de nowcasting branche par
branche (`R_gdpnow_maroc/`), avec une application Shiny interactive.

Le rapport méthodologique complet (ancrage littérature, équations, résultats,
limites) est disponible à la racine : [`Rapport_GDPNow_Maroc.pdf`](Rapport_GDPNow_Maroc.pdf)
— également consultable directement dans l'onglet **Rapport** de l'application.

## Lancer l'application Shiny

### Prérequis

- **R** (testé avec la version 4.6.1) : https://cran.r-project.org/bin/windows/base/
- Les packages R suivants :

```r
install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "lubridate", "purrr",
                    "stringr", "zoo", "tseries", "forecast",
                    "shiny", "bslib", "DT", "plotly"))
```

> Si l'installation dans la bibliothèque système échoue (droits insuffisants),
> installez dans une bibliothèque utilisateur :
> ```r
> dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE)
> install.packages(c(...), lib = Sys.getenv("R_LIBS_USER"))
> ```

### Option 1 — depuis un terminal (PowerShell / Git Bash)

Depuis la racine du projet :

```bash
"C:\Program Files\R\R-4.6.1\bin\Rscript.exe" -e ".libPaths(c(Sys.getenv('R_LIBS_USER'), .libPaths())); shiny::runApp('R_gdpnow_maroc/shiny_app', port=8901, launch.browser=TRUE)"
```

Adaptez le chemin de `Rscript.exe` à votre installation de R. L'application
s'ouvre automatiquement dans votre navigateur par défaut, sur
`http://localhost:8901`.

### Option 2 — depuis RStudio ou une console R

```r
setwd("chemin/vers/R_gdpnow_maroc/shiny_app")
shiny::runApp()
```

Ou, dans RStudio, ouvrez `R_gdpnow_maroc/shiny_app/app.R` puis cliquez sur
**Run App**.

### Premier démarrage un peu plus long

Au tout premier lancement (ou après modification de
`R_gdpnow_maroc/shiny_app/data/Series_retenues_modelisation.xlsx`), le
chargement du classeur Excel et le calcul initial du modèle prennent environ
10 secondes. Ce résultat est ensuite mis en cache disque
(`data/.cache_import_base.rds`) et partagé par toutes les sessions : les
connexions suivantes sont quasi instantanées, y compris entre plusieurs
utilisateurs simultanés.

## Pages de l'application

| Page | Contenu |
|---|---|
| **Tableau de bord** | Nowcast du PIB en cours, contribution par branche |
| **Exploration** | Trajectoires, stationnarité, corrélations, indicateurs vs. cible |
| **Modèle & prévisions** | Détail BVAR, équations de passerelle, AR(4) |
| **Validation** | Backtest en pseudo temps réel (RMSFE, test de Diebold-Mariano) |
| **Ajouter des données** | Saisie de nouvelles observations et recalcul du modèle |
| **Sources** | Institution productrice, période et fiabilité de chaque série utilisée |
| **Rapport** | Le rapport méthodologique complet, intégré dans l'app |
| **À propos** | Résumé de la méthodologie et des limites assumées |

## Structure du dépôt

```
STAGE/
├── Etude_sectorielle_Maroc_2_complete.xlsx   Classeur synthétique fiabilisé
├── Rapport_GDPNow_Maroc.pdf                  Rapport méthodologique
├── R_gdpnow_maroc/
│   ├── R/                 Pipeline batch (import -> BVAR -> bridge -> agrégation -> backtest)
│   ├── shiny_app/          Application interactive (voir ci-dessus)
│   ├── data/               Classeur source du modèle (16 branches, 41 indicateurs)
│   ├── figures/             Graphiques générés par le pipeline batch
│   └── resultats/          Objets .rds intermédiaires
└── scripts_et_documentation/scripts/   Scripts d'audit et de restauration du classeur synthétique
```
