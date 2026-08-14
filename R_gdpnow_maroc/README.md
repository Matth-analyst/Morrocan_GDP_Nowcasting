# GDPNow-Maroc — Pipeline de nowcasting sectoriel du PIB (R)

> ⚠️ **Errata (14/08/2026)** — Une erreur d'alignement des dates a été
> détectée et corrigée dans le classeur source `Series_retenues_modelisation.xlsx`.
> Voir la section « Errata » en bas de ce document pour le détail complet
> (cause, portée, impact sur les résultats).

Implémentation complète, en R, du modèle de nowcasting sectoriel du PIB marocain,
adapté du modèle GDPNow de la Federal Reserve Bank d'Atlanta (Higgins, 2014) à
une logique offre (production par branche d'activité) plutôt que demande.

## Ancrage dans la littérature

| Brique | Référence |
|---|---|
| Architecture générale (BVAR + bridge equations + agrégation) | Higgins, P. (2014). *GDPNow: A Model for GDP "Nowcasting"*. FRB Atlanta Working Paper 2014-7. |
| Équations de passerelle (bridge equations) | Miller, P.J. & Chin, D.M. (1996). *Using Monthly Data to Improve Quarterly Model Forecasts*. FRB Minneapolis Quarterly Review. |
| Prior bayésien Minnesota (BVAR) | Litterman, R. (1986). *Forecasting with Bayesian Vector Autoregressions*. |
| Méthode des observations fictives, grand BVAR | Bańbura, M., Giannone, D. & Reichlin, L. (2010). *Large Bayesian VARs*. Journal of Applied Econometrics, 25(1). |
| Validation hors échantillon (RMSFE) | Stock, J.H. & Watson, M.W. (2007). *Why Has U.S. Inflation Become Harder to Forecast?* |
| Test de significativité de l'écart de précision | Diebold, F.X. & Mariano, R.S. (1995). *Comparing Predictive Accuracy*. |
| Sélection des indicateurs par corrélation | Fernández Cerezo, A. (2023). *A supply-side GDP nowcasting model*. Banco de España Economic Bulletin. |

## Structure du dépôt

```
R_gdpnow_maroc/
├── data/
│   └── Series_retenues_modelisation.xlsx   (classeur source, 16 branches)
├── R/
│   ├── 00_setup.R                    Configuration, packages, thème graphique
│   ├── 01_import_donnees.R           Lecture du classeur -> format tidy
│   ├── 02_analyse_exploratoire.R     EDA, tests de stationnarité, corrélations
│   ├── 03_bvar_trimestriel.R         BVAR Minnesota, 16 branches, 5 retards
│   ├── 04_bridge_equations.R         Bridge equations + combinaison BVAR (7 branches)
│   ├── 05_ar4_non_couvertes.R        AR(4) pur (9 branches sans indicateur)
│   ├── 06_agregation_fisher.R        Agrégation en nowcast du PIB total
│   ├── 07_validation_pseudo_temps_reel.R   Backtest RMSFE + test Diebold-Mariano
│   └── 08_rapport_synthese.R         Tableau de synthèse final
├── figures/     (8 graphiques PNG, générés par le pipeline)
├── resultats/   (objets .rds intermédiaires, réutilisables)
└── run_pipeline.R   Exécute l'ensemble des 8 étapes dans l'ordre
```

## Comment lancer

```r
# Depuis le dossier R_gdpnow_maroc/ :
install.packages(c("readxl","dplyr","tidyr","ggplot2","lubridate",
                    "purrr","stringr","zoo","tseries","forecast"))
source("run_pipeline.R")
```

Temps d'exécution total : quelques secondes (échantillon de 113 trimestres,
16 branches). Chaque script peut aussi être lancé séparément (dans l'ordre),
à condition que les scripts précédents aient déjà tourné au moins une fois
(chaque étape lit les `.rds` produits par la précédente).

## Ce que fait chaque étape, en résumé

**01 — Import.** Transforme le classeur Excel (mise en page GDPNow-like,
un bloc cible + blocs d'indicateurs par fréquence) en trois tables `tidy` :
cibles (16 branches), indicateurs (41 séries), avec dates réelles.

**02 — Exploration.** Teste la stationnarité (Dickey-Fuller augmenté) en
niveau et en taux de croissance (Δlog) pour les 16 branches, trace les
trajectoires, la matrice de corrélation croisée entre branches, et les
nuages de points indicateur/cible pour vérifier visuellement le critère de
corrélation appliqué en amont. **Résultat obtenu : 16/16 branches
stationnaires en Δlog contre 1/16 en niveau** — justifie le choix de
modéliser des taux de croissance dans toute la suite.

**03 — BVAR trimestriel.** Estime un BVAR à 16 variables (les Δlog de VA),
5 retards, prior Minnesota (delta=0, retour à la moyenne, puisque les
variables sont déjà des taux de croissance) via la méthode des observations
fictives (Bańbura, Giannone & Reichlin, 2010), λ=0,15 (même valeur que le
BVAR trimestriel de composantes de quantité dans GDPNow). Produit une
prévision à un pas pour les 16 branches.

**04 — Bridge equations.** Pour les 7 branches couvertes, régresse la
croissance de la VA sur celle de chaque indicateur retenu (régression
simple), moyenne les prévisions individuelles (faute de poids de valeur
ajoutée nominale par sous-branche — limite documentée), puis combine cette
prévision "bridge" avec la prévision BVAR par moindres carrés restreints
(poids δ ∈ [0,1] optimisé en échantillon, équation 8 de Higgins 2014).

**05 — AR(4).** Pour les 9 branches sans indicateur validé, prévision
autorégressive pure — la méthode que Higgins (2014) applique lui-même aux
sous-composantes sans série mensuelle disponible.

**06 — Agrégation.** Combine les 16 prévisions de branche, pondérées par
leur part de valeur ajoutée (en volume, dernier trimestre observé), en un
nowcast de croissance du PIB total.

**07 — Validation.** Backtest en pseudo temps réel sur les 8 derniers
trimestres : à chaque origine, tout le pipeline (BVAR, bridge equations,
AR(4)) est ré-estimé sur les données tronquées, une prévision à un pas est
comparée à la valeur réellement observée. Comparaison à un repère AR(2) sur
le PIB total, avec test de Diebold-Mariano.

## Résultats obtenus (à la date de génération de ce pipeline)

- **Nowcast du prochain trimestre : +0,86 %** (Δlog agrégé)
- **Stationnarité** : 16/16 branches en Δlog
- **Backtest (8 trimestres)** : RMSFE modèle complet = 0,0074 ; RMSFE
  repère AR(2) = 0,0067 — **le modèle ne bat pas le repère AR(2) sur cette
  fenêtre de test**, écart non significatif au test de Diebold-Mariano
  (p = 0,236)

## Limites méthodologiques assumées (à rappeler dans le rapport)

1. **Poids d'agrégation approximatifs** : parts de valeur ajoutée *en
   volume* (prix chaînés), non additives en toute rigueur — la vraie
   pondération Fisher/Törnqvist demanderait les valeurs *à prix courants*,
   non disponibles pour les 16 branches à ce stade.
2. **Absence de poids de sous-branche** (SH_T^i) : les prévisions "bridge"
   de plusieurs indicateurs sont moyennées à parts égales, pas pondérées
   par leur poids économique réel (équation 7 de Higgins 2014, non
   reproduite faute de données).
3. **9 branches sur 16 sans indicateur** (~ majorité du PIB non couverte
   par un vrai signal infra-annuel) — traitées en AR(4) pur, une pratique
   documentée dans la littérature de référence mais appliquée ici à une
   échelle bien plus large que dans le cas américain original.
4. **Résultat du backtest non favorable au modèle** sur la fenêtre testée
   (8 trimestres) — échantillon de test très court, à ne pas
   sur-interpréter, mais à ne pas cacher non plus.
5. **7 des 41 indicateurs retenus ont un signe de corrélation
   contre-intuitif** (repéré lors de la sélection rigoureuse) — inclus
   dans les bridge equations sans traitement particulier ; un signal à
   creuser avant toute utilisation en production.
6. **Choc Covid-19 (2020)** visible sur la quasi-totalité des séries
   (cf. figure `02_croissance_toutes_branches.png`) — aucune variable
   indicatrice de rupture structurelle n'a été introduite dans le BVAR,
   ce qui peut affecter l'estimation du prior et des écarts-types.

## Errata (14/08/2026)

**Le problème.** Dans le classeur `Series_retenues_modelisation.xlsx`,
lorsqu'une branche avait plusieurs indicateurs de même fréquence (par
exemple les 6 séries mensuelles de la branche Pêche), une seule colonne
de dates était partagée entre eux, remplie « si la cellule est vide ».
Or différents indicateurs d'une même branche n'ont pas forcément
exactement le même nombre d'observations ni la même couverture
temporelle (155 à 173 mois selon la série, pour la Pêche). Résultat :
au-delà du point où les séries divergeaient, les valeurs se
retrouvaient silencieusement alignées sur de mauvaises dates.

**Portée.** Toutes les branches couvertes ayant plus d'un indicateur de
même fréquence étaient concernées (Pêche, Industrie de transformation,
Électricité-gaz-eau, Immobilier, Finances et assurances). Les branches à
un seul indicateur (Industrie d'extraction, Hébergement-restauration)
n'étaient pas affectées, faute de partage possible.

**Ce qui N'était PAS affecté.** La sélection des 41 séries elle-même et
leurs coefficients de corrélation affichés (calculés en Python à partir
de vraies paires date/valeur, jamais via le classeur Excel) restent
corrects et inchangés. Seul l'export Excel, et tout ce qui en a été
recalculé côté R (bridge equations, backtest, figures), était concerné.

**Correction.** Chaque indicateur dispose désormais de sa propre colonne
de dates dédiée, immédiatement à sa gauche — plus aucun partage. Vérifié
systématiquement sur les 7 branches couvertes après correction : toutes
les séquences de dates sont strictement croissantes.

**Impact sur les résultats.** Le pipeline complet a été relancé avec les
données corrigées. Le nowcast agrégé du PIB reste inchangé
(+0,86 %), mais un résultat intermédiaire a changé : le poids δ (BVAR)
de la branche Finances et assurances passe de 0,06 à 0,00 (la bridge
equation, recalculée sur des données désormais correctement alignées,
explique légèrement moins bien la cible qu'estimé précédemment). Le
RMSFE du backtest est quasiment inchangé (0,0074 contre 0,0067 pour
l'AR(2), p-value du test de Diebold-Mariano à 0,238 contre 0,236
précédemment).

### Deuxième correction (même jour) — valeurs manquantes rendues visibles

**Le problème.** Une fois le premier bug corrigé (une colonne de dates
dédiée par indicateur), un second défaut est apparu : les mois ou
trimestres sans valeur observée étaient simplement **absents** de la
séquence plutôt que représentés par une cellule vide. Par exemple, pour
l'indicateur AINBIDA (branche Pêche), la séquence de dates passait
directement de 2010-10 à 2011-02, sans qu'aucune trace ne signale que
novembre 2010, décembre 2010 et janvier 2011 étaient des mois sans
donnée — un lecteur pouvait à tort penser que la série était continue à
cet endroit.

**Vérification effectuée.** Les valeurs elles-mêmes (10, 10, 25796, 3...)
ont été confrontées au fichier source brut de l'Office National des
Pêches (`Débarquements des produits de la pêche côtière et artisanale
par port en quantité (mensuel).csv`) et se sont révélées exactes — le
problème ne portait donc que sur la représentation des trous, pas sur
les valeurs elles-mêmes.

**Correction.** Chaque indicateur affiche désormais un calendrier
**complet et continu** (mois par mois, ou trimestre par trimestre, du
premier au dernier point observé), avec une **cellule vide, surlignée en
orange clair**, pour chaque période sans donnée. Le nombre exact de
périodes manquantes est indiqué en toutes lettres dans la ligne source
de chaque indicateur (ex. « 34 mois manquants sur 189 » pour AINBIDA).

**Impact sur les résultats.** Aucun — les valeurs et leurs dates réelles
étaient déjà correctes après la première correction ; seule leur mise en
forme dans le classeur a changé. Le pipeline R a été relancé par
précaution : résultats strictement identiques à ceux de la première
correction.

### Troisième vérification (même jour) — audit systématique des 41 séries

À la demande explicite de l'utilisateur, les 41 séries retenues ont été
recomparées une par une à leurs fichiers sources bruts (Manar-Stat, Bank
Al-Maghrib, Office National des Pêches), au-delà du seul cas d'AINBIDA
déjà traité.

**Résultat** : 40 séries sur 41 correctement extraites (valeurs et
nombre d'observations strictement conformes à la source). **Une
troisième erreur, distincte des deux premières, a été trouvée et
corrigée** :

- **Recettes touristiques (branche Hébergement-restauration)** : la
  série extraite était **tronquée** à 217 observations (2007-12 à
  2025-12), alors que la source réelle (`Tourisme.xlsx`) remonte à
  **1994-01**, soit 381 observations disponibles — 164 points d'historique
  perdus (43 %), sans lien avec les deux bugs précédents. Corrigé en
  ré-extrayant directement depuis le fichier source. La corrélation à la
  cible a été recalculée sur la série complète (r passe de +0,44 à
  +0,36, toujours largement au-dessus du seuil de rétention de 0,25).

**Impact sur les résultats.** Léger mais réel : le nowcast agrégé passe
de +0,86 % à **+0,85 %**, et le poids δ (BVAR) de la branche
Hébergement-restauration passe de 0,24 à 0,34 (la bridge equation,
disposant désormais de 17 ans d'historique supplémentaire, explique
mieux la cible qu'avant).

**Ce qui n'a pas pu être vérifié de façon exhaustive** : les 3 séries
IPAI (branche Immobilier) n'ont pas été re-confrontées à leurs bulletins
PDF sources dans cette passe — elles avaient déjà fait l'objet d'une
vérification et de plusieurs corrections dédiées, documentées
séparément, lors de leur extraction initiale.
