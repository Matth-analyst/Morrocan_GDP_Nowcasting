# GDPNow-Maroc — Pipeline de nowcasting sectoriel du PIB (R)

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
