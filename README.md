# Dossier STAGE — organisé par source / institution

Reprend l'intégralité du contenu nettoyé de STAGE_epure (format unique
CSV, doublons supprimés, noms de fichiers correctement encodés), mais
**restructuré par institution productrice ou de collecte** plutôt que
par secteur d'activité.

## Structure

```
STAGE_par_source/
│
├── Manar-Stat (Ministere de l'Economie et des Finances)/
│   ├── primaire/                  -> cultures, elevage, peche
│   ├── secondaire/                -> construction, eau, energie, industrie, mines
│   ├── tertiaire/                 -> assurances, telecommunications, tourisme, transports
│   ├── racine (comptes nationaux, sectoriel, conjoncture, prix)/
│   │     -> PIB trimestriel (13 feuilles), Solde d'opinion (6 enquêtes
│   │        de conjoncture), Précipitations (+ 6 feuilles de détail
│   │        régional/station), indices du commerce extérieur, énergie,
│   │        tourisme, phosphates, ciment, etc.
│   └── archives_brutes_non_extraites/
│         -> manar_primaire.zip, manar_secondaire.zip, manar_tertiaire.zip
│            (archives de la collecte automatisée, conservées en l'état)
│
├── Bank Al-Maghrib et ANCFCC/
│   ├── credit_bancaire_par_branche/   -> les 5 fichiers de ventilation
│   │                                     du crédit bancaire (12, 13, 15,
│   │                                     16, 17), téléchargés directement
│   │                                     depuis bkam.ma
│   └── immobilier_ipai/
│         ├── bulletins_pdf/           -> les 72 bulletins trimestriels
│         │                               IPAI (BAM/ANCFCC)
│         ├── ipai_variations_propre.csv / ipai_variations_large.csv
│         └── README_extraction_ipai.md
│
├── HCP - Annuaires Statistiques (non exploites)/
│   └── 26 archives "Annuaire Statistique du Maroc" (2000-2025, RAR/ZIP)
│       — publication HCP distincte du portail Manar-Stat, jamais
│       extraite ni exploitée dans la conversation jusqu'ici
│
├── Bases consolidees (multi-sources)/
│   ├── manar_panel.csv                -> panel long-format, toutes
│   │                                     séries Manar-Stat collectées
│   └── BDD SECTORIEL_MENSUEL.csv / BDD SECTORIEL_TRIM.csv
│         -> base déjà filtrée sur les séries à jour, mélangeant
│            explicitement Manar-Stat et Bank Al-Maghrib (établi
│            précédemment : crédits BAM, IPAI, à côté des séries Manar)
│
└── scripts_et_documentation/
    ├── scripts/            -> collecte_manar.py, converter.py, deleter.py,
    │                          harmoniser.py, journal_collecte.py
    ├── log_collecte.md     -> journal de la collecte automatisée initiale
    └── log_nettoyage.md    -> journal du nettoyage/dédoublonnage
```

## Logique de classement retenue

Le classement suit la **source de collecte directe** (le portail ou
l'institution effectivement interrogé), établie au fil de la
conversation à partir de preuves concrètes :

- **Manar-Stat** : tout ce qui est documenté dans `log_collecte.md`
  (scraping du portail manar.finances.gov.ma) + les fichiers racine dont
  l'intitulé correspond à un tableau précis de l'arborescence Manar-Stat
  qu'on a explorée ensemble.
- **Bank Al-Maghrib / ANCFCC** : les fichiers de crédit bancaire par
  branche ont été téléchargés directement depuis bkam.ma (liens trouvés
  et vérifiés dans la conversation), hors du portail Manar-Stat. Les
  bulletins IPAI sont des publications BAM/ANCFCC.
- **HCP** : les annuaires statistiques sont une publication distincte,
  téléchargée séparément (pas via le portail Manar-Stat).
- **Bases consolidées** : fichiers dont le contenu mélange plusieurs
  sources (établi précédemment pour `BDD SECTORIEL.xlsx`, qui contient
  aussi bien des séries Manar-Stat que des lignes de crédit bancaire BAM).

## Une réserve à connaître

Pour les fichiers scrapés depuis Manar-Stat (dossiers `primaire/`,
`secondaire/`, `tertiaire/`), l'institution *productrice* d'origine
(ONEE pour l'électricité, ANRT pour les télécoms, OCP pour les
phosphates, HCP pour les enquêtes de conjoncture, etc.) peut être
différente de l'institution de *collecte* (Manar-Stat, qui agrège et
republie ces données). Ce classement reflète **où la donnée a été
récupérée**, pas nécessairement qui l'a produite à l'origine — une
distinction à garder en tête si tu cites tes sources dans le rapport de
stage (il peut être utile de citer les deux : "Manar-Stat, données ONEE"
par exemple).
