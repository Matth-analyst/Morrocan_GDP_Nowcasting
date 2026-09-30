# Plan de correction méthodologique --- Nowcasting du PIB marocain

## 0. Convention d'information retenue

Pour chaque trimestre cible `T`, on adopte la convention suivante :

-   toutes les données des trimestres précédents sont disponibles ;
-   les trois observations mensuelles du trimestre `T` sont disponibles
    ;
-   les indicateurs trimestriels de `T` sont disponibles ;
-   aucune observation de `T+1` ou d'une période ultérieure n'est
    utilisée pour produire le nowcast de `T`.

Ainsi :

\[ `\mathcal `{=tex}I_T={X_t:t`\leq `{=tex}T}. \]

Cette convention ne constitue pas un pseudo-temps réel strict fondé sur
les dates de publication et les vintages, puisque ces informations ne
sont pas disponibles. Il s'agit d'un nowcasting fondé sur la date
d'observation, avec une analyse complémentaire de l'information
intra-trimestrielle.

La règle absolue du projet est :

> Pour prévoir le trimestre `T`, aucune information datant de `T+1` ou
> après ne peut entrer dans le modèle.

------------------------------------------------------------------------

# 1. Phase 1 --- Importation et structuration des données

**Fichier principal :** `R/01_import_donnees.R`

### Objectif

Construire une base propre contenant :

-   les séries de valeur ajoutée trimestrielles ;
-   les indicateurs trimestriels ;
-   les indicateurs mensuels ;
-   les fréquences ;
-   les unités ;
-   les branches associées ;
-   les dates d'observation.

### Correctifs

1.  Harmoniser toutes les dates.
2.  Identifier explicitement la fréquence de chaque série.
3.  Définir une date de fin pour chaque trimestre.
4.  Construire une fonction d'information set :

``` r
information_set <- function(data, target_date) {
  data |>
    dplyr::filter(date <= target_date)
}
```

Pour un trimestre `T`, `target_date` correspond à son dernier mois.

Exemple :

``` text
2024Q1 -> 2024-03
2024Q2 -> 2024-06
2024Q3 -> 2024-09
2024Q4 -> 2024-12
```

### Contrôle obligatoire

À chaque origine :

``` r
stopifnot(max(data_T$date, na.rm = TRUE) <= target_date)
```

------------------------------------------------------------------------

# 2. Phase 2 --- Analyse exploratoire et transformations

**Fichier :** `R/02_analyse_exploratoire.R`

Les transformations doivent être compatibles avec l'information
disponible à chaque origine.

Pour une série de niveau positive :

\[ g_t = `\Delta`{=tex}`\log`{=tex}(X_t) \]

ou :

\[ g_t=100`\Delta`{=tex}`\log`{=tex}(X_t). \]

Il faut choisir une convention et la conserver partout.

### Attention

Ne pas :

1.  transformer toute la série avec une procédure utilisant toute la
    période ;
2.  puis seulement couper les données.

Pour un backtest de `T`, les paramètres calculés doivent provenir des
données disponibles à cette origine.

Cela concerne notamment :

-   moyennes ;
-   écarts-types ;
-   standardisation ;
-   lissage ;
-   filtres ;
-   imputations ;
-   paramètres estimés.

------------------------------------------------------------------------

# 3. Phase 3 --- Construction du ragged edge

Le ragged edge représente les différentes dates de disponibilité des
séries.

Avec la convention principale de l'étude, à la fin de `T` :

-   les trois mois de `T` sont disponibles pour les séries mensuelles ;
-   les observations trimestrielles de `T` sont disponibles.

Donc il ne faut pas prévoir une observation de `T` si elle existe
réellement.

La règle est :

``` text
observation disponible -> conserver
observation absente    -> éventuellement prévoir
```

Le ragged edge reste particulièrement utile pour les scénarios M0/M1/M2.

------------------------------------------------------------------------

# 4. Phase 4 --- Traitement des valeurs manquantes et Kalman/AR

Le Kalman/AR peut servir à traiter :

-   trous internes ;
-   valeurs manquantes historiques ;
-   extrémités manquantes dans M0/M1/M2.

Il ne doit jamais introduire d'information future.

Pour une origine `T` :

\[ X\_{`\text{utilisé}`{=tex}}`\subseteq `{=tex}X\_{`\leq `{=tex}T}. \]

Le code doit respecter une coupure du type :

``` r
data_cut <- data |>
  dplyr::filter(date <= date_limite)
```

### Attention au lissage

Un lissage bidirectionnel peut utiliser des observations futures par
rapport à une date historique. Pour le backtest, privilégier un filtrage
compatible avec l'ensemble d'information disponible.

------------------------------------------------------------------------

# 5. Phase 5 --- Sélection récursive des indicateurs

**Choix retenu : Solution B.**

La sélection est recalculée à chaque trimestre cible.

Pour chaque `T` :

\[ S_T=f(`\mathcal `{=tex}I\_{T-1}). \]

Il est recommandé de sélectionner les indicateurs avec les données
antérieures au trimestre cible, puis d'utiliser cette sélection pour
prévoir `T`.

### Procédure

Pour chaque `T` :

``` text
Données jusqu'à T-1
        ↓
Tests de sélection
        ↓
Liste des indicateurs retenus S_T
        ↓
Modèle pour T
```

Les critères actuels peuvent être conservés :

\[ p\<0.10 \]

et :

\[ \|r\|`\geq0.15`{=tex}. \]

Mais ces statistiques doivent être calculées uniquement sur
l'information disponible avant `T`.

### Traçabilité

Sauvegarder pour chaque `T` :

-   indicateur ;
-   branche ;
-   corrélation ;
-   p-value ;
-   statut sélectionné/non sélectionné.

La liste des indicateurs peut donc varier au cours du temps.

------------------------------------------------------------------------

# 6. Phase 6 --- Agrégation des indicateurs mensuels en trimestriels

Il faut corriger l'application uniforme de `mean(dlog)`.

La méthode d'agrégation dépend de la nature économique de l'indicateur.

### Variables de niveau/stock

Selon la définition de la variable :

\[ X_T\^Q=`\frac{X_{m1}+X_{m2}+X_{m3}}{3}`{=tex}. \]

### Variables de flux

Pour un flux :

\[ X_T\^Q=X\_{m1}+X\_{m2}+X\_{m3}. \]

Puis :

\[ g_T\^Q= `\log`{=tex}(X_T^Q)-`\log`{=tex}(X\_{T-1}^Q). \]

### Indices/taux

Une moyenne trimestrielle peut être pertinente selon la définition
économique.

### Correctif logiciel

Créer une métadonnée par indicateur :

``` text
aggregation = mean
aggregation = sum
aggregation = last
aggregation = growth
```

et appliquer la règle correspondante.

------------------------------------------------------------------------

# 7. Phase 7 --- Équations Bridge

**Fichier :** `R/04_bridge_equations.R`

Pour une branche `j` :

\[ g\_{j,T} =
`\alpha`{=tex}\_j+`\beta`{=tex}*jX_T+`\varepsilon`{=tex}*{j,T}. \]

Avec plusieurs indicateurs :

\[ g\_{j,T} =
`\alpha`{=tex}*j+`\sum`{=tex}*{k=1}\^{K}`\beta`{=tex}*kX*{k,T}+`\varepsilon`{=tex}\_{j,T}.
\]

### Estimation récursive

Pour prévoir `T`, les coefficients doivent être estimés avec :

\[ t\<T. \]

Donc :

``` r
train <- data |>
  dplyr::filter(quarter < target_quarter)
```

et jamais avec la valeur réalisée de `T`.

### Prévision

\[ `\widehat `{=tex}g\_{j,T}\^{Bridge} =
`\hat`{=tex}`\alpha`{=tex}\_j+`\hat`{=tex}`\beta `{=tex}X_T. \]

La sélection récursive de la phase 5 doit être appliquée avant cette
estimation.

------------------------------------------------------------------------

# 8. Phase 8 --- BVAR trimestriel

**Fichier :** `R/03_bvar_trimestriel.R`

Conserver dans un premier temps l'architecture :

-   16 branches ;
-   `p = 5` ;
-   prior Minnesota ;
-   `lambda = 0.15` ;
-   dummy COVID T2/T3 2020.

### Estimation récursive

Pour chaque `T` :

\[ Y_1,`\ldots`{=tex},Y\_{T-1} `\rightarrow `{=tex}BVAR_T
`\rightarrow `{=tex}`\widehat `{=tex}Y_T. \]

Aucune observation de `T+1` ou après ne doit être utilisée.

### Précision méthodologique

L'implémentation actuelle par système augmenté avec pseudo-observations
du prior doit être présentée comme une estimation BVAR à prior Minnesota
par système augmenté. Elle ne correspond pas à une simulation complète
de la distribution a posteriori.

------------------------------------------------------------------------

# 9. Phase 9 --- Branches non couvertes : AR(4)

**Fichier :** `R/05_ar4_non_couvertes.R`

Pour chaque branche non couverte :

\[ g\_{j,t} = c_j+ `\phi`{=tex}*{j1}g*{j,t-1} +`\cdots`{=tex}+
`\phi`{=tex}*{j4}g*{j,t-4} +`\epsilon`{=tex}\_{j,t}. \]

Pour prévoir `T` :

\[ {g\_{j,t}:t\<T} `\rightarrow `{=tex}AR(4)
`\rightarrow`{=tex}`\widehat `{=tex}g\_{j,T}. \]

Le modèle doit être réestimé à chaque origine du backtest.

### Saison

Pour les branches très saisonnières, tester en robustesse :

-   SARIMA ;
-   AR avec composantes saisonnières ;
-   variables indicatrices saisonnières.

L'AR(4) reste le benchmark de base.

------------------------------------------------------------------------

# 10. Phase 10 --- Combinaison BVAR + Bridge

Pour chaque branche couverte :

\[ `\widehat `{=tex}g\_{j,T} =
`\delta`{=tex}*{j,T}`\widehat `{=tex}g*{j,T}\^{BVAR} +
(1-`\delta`{=tex}*{j,T})`\widehat `{=tex}g*{j,T}\^{Bridge}, \]

avec :

\[ 0`\leq`{=tex}`\delta`{=tex}\_{j,T}`\leq1`{=tex}. \]

### Delta récursif

Pour chaque `T` :

\[ `\delta`{=tex}*{j,T} =
`\arg`{=tex}`\min`{=tex}*{`\delta`{=tex}`\in[0,1]`{=tex}}
`\sum`{=tex}\_{t\<T} `\left[
g_{j,t}
-
\widehat g_{j,t}(\delta)
\right]`{=tex}\^2. \]

Ainsi le poids est déterminé uniquement avec l'historique disponible
avant `T`.

### Important

Ne plus utiliser un `delta` optimisé sur toute la période pour tous les
trimestres historiques.

Tester ensuite en robustesse :

-   `delta = 0.5` ;
-   delta récursif ;
-   delta contraint ;
-   fenêtres rolling/expanding.

------------------------------------------------------------------------

# 11. Phase 11 --- Poids des branches et agrégation

**Fichier :** `R/06_agregation_fisher.R`

Le poids sectoriel ne doit pas provenir d'une période future.

Si `T` est le trimestre cible, utiliser par défaut :

\[ w\_{j,T-1}. \]

Alors :

\[ `\widehat{GDP}`{=tex}*T =
`\sum`{=tex}*jw*{j,T-1}`\widehat{VA}`{=tex}*{j,T}. \]

avec :

\[ `\sum`{=tex}*jw*{j,T-1}=1. \]

### À supprimer

L'utilisation de poids fixes provenant du dernier trimestre de la base,
par exemple 2026, dans les backtests historiques.

### Limite

Si les comptes sont en volumes chaînés non additifs, l'agrégation
linéaire est une approximation. Cette limite doit être explicitement
documentée.

------------------------------------------------------------------------

# 12. Phase 12 --- Construction du nowcast

Pour les branches couvertes :

\[ `\widehat `{=tex}g\_{j,T} =
`\delta`{=tex}*{j,T}`\widehat `{=tex}g*{j,T}\^{BVAR} +
(1-`\delta`{=tex}*{j,T})`\widehat `{=tex}g*{j,T}\^{Bridge}. \]

Pour les branches non couvertes :

\[ `\widehat `{=tex}g\_{j,T} = `\widehat `{=tex}g\_{j,T}\^{AR}. \]

Puis agrégation des 16 branches :

\[ `\widehat `{=tex}g\_{GDP,T} = F(
`\widehat `{=tex}g\_{1,T},`\ldots`{=tex},`\widehat `{=tex}g\_{16,T},
w\_{1,T-1},`\ldots`{=tex},w\_{16,T-1} ). \]

------------------------------------------------------------------------

# 13. Phase 13 --- Nowcasting intra-trimestriel M0/M1/M2/M3

**Fichier :** `R/09_test_affinement_intra_trimestre.R`

Le nowcast principal de fin de trimestre est **M3**.

Les quatre scénarios permettent d'étudier la valeur de l'information :

``` text
M0 = 0 mois observé
M1 = 1 mois observé
M2 = 2 mois observés
M3 = 3 mois observés
```

## M0

Prévoir les trois mois :

\[ `\hat `{=tex}X\_{m1},`\hat `{=tex}X\_{m2},`\hat `{=tex}X\_{m3}. \]

Puis construire l'indicateur trimestriel prévu.

## M1

Observer `m1`, prévoir `m2` et `m3` :

\[
`\widehat `{=tex}X_T=A(X\_{m1},`\hat `{=tex}X\_{m2},`\hat `{=tex}X\_{m3}).
\]

## M2

Observer `m1`, `m2`, prévoir `m3` :

\[ `\widehat `{=tex}X_T=A(X\_{m1},X\_{m2},`\hat `{=tex}X\_{m3}). \]

## M3

Les trois mois sont connus :

\[ `\widehat `{=tex}X_T=A(X\_{m1},X\_{m2},X\_{m3}). \]

Il ne faut donc plus calculer M1/M2 comme une simple moyenne des mois
observés.

------------------------------------------------------------------------

# 14. Phase 14 --- Indicateurs trimestriels dans M0/M1/M2/M3

Selon la convention retenue :

``` text
M0 -> indicateur trimestriel T indisponible
M1 -> indicateur trimestriel T indisponible
M2 -> indicateur trimestriel T indisponible
M3 -> indicateur trimestriel T disponible
```

Ainsi M3 représente l'ensemble d'information de fin de trimestre.

Cette distinction permet de mesurer séparément l'apport des indicateurs
mensuels et des indicateurs trimestriels.

------------------------------------------------------------------------

# 15. Phase 15 --- Backtesting

**Fichier :** `R/07_validation_pseudo_temps_reel.R`

Pour chaque trimestre cible `T`, exécuter exactement la même séquence :

``` text
1. Définir T
2. Construire I_T
3. Préparer les transformations
4. Construire le ragged edge
5. Traiter les valeurs manquantes
6. Sélectionner récursivement les indicateurs avec I_(T-1)
7. Construire les indicateurs trimestriels
8. Estimer les Bridge
9. Estimer le BVAR
10. Estimer les AR(4)
11. Estimer les delta récursifs
12. Récupérer les poids disponibles avant T
13. Produire le nowcast de T
14. Comparer au PIB réalisé de T
15. Sauvegarder les erreurs
```

Le PIB réalisé de `T` sert uniquement à **évaluer** la prévision, pas à
l'estimer.

------------------------------------------------------------------------

# 16. Phase 16 --- Benchmarks

Conserver au minimum un AR(2) :

\[
GDP_t=c+`\phi`{=tex}*1GDP*{t-1}+`\phi`{=tex}*2GDP*{t-2}+`\epsilon`{=tex}\_t.
\]

Il doit lui aussi être estimé récursivement.

Ajouter si possible :

-   AR(1) ;
-   AR(4) ;
-   modèle naïf ;
-   moyenne historique.

L'objectif est de déterminer si le système complexe apporte réellement
une amélioration.

------------------------------------------------------------------------

# 17. Phase 17 --- Évaluation

Pour chaque modèle calculer :

### MAE

\[ MAE= `\frac`{=tex}1N`\sum`{=tex}\_T\|`\hat `{=tex}y_T-y_T\|. \]

### RMSFE

\[ RMSFE= `\sqrt{
\frac1N
\sum_T(\hat y_T-y_T)^2
}`{=tex}. \]

### Biais

\[ Bias= `\frac`{=tex}1N`\sum`{=tex}\_T(`\hat `{=tex}y_T-y_T). \]

Conserver les erreurs trimestre par trimestre.

------------------------------------------------------------------------

# 18. Phase 18 --- Diebold-Mariano

Comparer :

\[ e\_{1,T}=y_T-`\hat `{=tex}y\_{1,T} \]

et :

\[ e\_{2,T}=y_T-`\hat `{=tex}y\_{2,T}. \]

Avec une perte quadratique :

\[ L(e)=e\^2. \]

Tester :

\[ H_0: E\[L(e_1)-L(e_2)\]=0. \]

Le benchmark principal peut être l'AR(2).

Les deux modèles doivent être évalués :

-   sur les mêmes trimestres ;
-   au même horizon ;
-   avec les mêmes réalisations.

------------------------------------------------------------------------

# 19. Phase 19 --- Évaluation M0/M1/M2/M3

Calculer :

\[ RMSFE\_{M0},`\quad`{=tex} RMSFE\_{M1},`\quad`{=tex}
RMSFE\_{M2},`\quad`{=tex} RMSFE\_{M3}. \]

Si l'information supplémentaire est utile, on s'attend généralement à
une amélioration :

\[ RMSFE\_{M3}\<RMSFE\_{M2}\<RMSFE\_{M1}\<RMSFE\_{M0}. \]

Mais cette monotonie n'est pas imposée : elle doit être testée
empiriquement.

------------------------------------------------------------------------

# 20. Phase 20 --- Robustesse des modèles

Comparer :

``` text
BVAR seul
Bridge seul
BVAR + Bridge
AR(4)
Benchmark AR(2)
```

Cela permet d'identifier la source de la performance.

------------------------------------------------------------------------

# 21. Phase 21 --- Robustesse de la fenêtre

Comparer :

### Expanding window

Toutes les observations disponibles avant `T`.

### Rolling window

Une fenêtre fixe :

\[ T-W,`\ldots`{=tex},T-1. \]

Tester plusieurs valeurs de `W` si l'échantillon le permet.

------------------------------------------------------------------------

# 22. Phase 22 --- Robustesse de la sélection

Comparer :

1.  sélection récursive ;
2.  sélection fixe ;
3.  aucune sélection ;
4.  différents seuils.

Par exemple :

\[ \|r\|`\geq0.10`{=tex}, `\quad`{=tex} \|r\|`\geq0.15`{=tex},
`\quad`{=tex} \|r\|`\geq0.20`{=tex}. \]

La sélection récursive est le modèle méthodologique principal retenu.

------------------------------------------------------------------------

# 23. Phase 23 --- Robustesse du delta

Comparer :

\[ `\delta=0.5`{=tex} \]

contre :

\[ `\delta`{=tex}\_T=`\arg`{=tex}`\min `{=tex}SSE \]

et éventuellement :

\[ 0.2`\leq`{=tex}`\delta`{=tex}\_T`\leq0.8`{=tex}. \]

------------------------------------------------------------------------

# 24. Phase 24 --- Robustesse saisonnière

Pour les indicateurs et branches fortement saisonniers :

``` text
AR(4)
vs
SARIMA
vs
AR + variables saisonnières
```

La sélection finale doit être fondée sur les performances hors
échantillon.

------------------------------------------------------------------------

# 25. Phase 25 --- Diagnostics

Examiner :

-   autocorrélation des résidus ;
-   hétéroscédasticité ;
-   valeurs extrêmes ;
-   stabilité des coefficients ;
-   erreurs de prévision ;
-   épisodes de forte erreur.

Tracer au minimum :

``` text
PIB réel
PIB nowcast
PIB benchmark AR(2)
```

sur la période de test.

Analyser séparément :

-   COVID-19 ;
-   chocs agricoles ;
-   périodes de forte croissance ;
-   ralentissements ;
-   épisodes touristiques ;
-   éventuels changements structurels.

------------------------------------------------------------------------

# 26. Phase 26 --- Analyse du COVID

Conserver le dummy COVID dans le BVAR dans le modèle principal si les
diagnostics le justifient.

Effectuer une robustesse :

``` text
BVAR avec dummy COVID
vs
BVAR sans dummy COVID
```

Comparer les performances et les prévisions.

------------------------------------------------------------------------

# 27. Phase 27 --- Architecture logicielle recommandée

``` text
project/
│
├── README.md
├── NOTE_METHODOLOGIQUE.md
├── run_pipeline.R
│
├── R/
│   ├── 00_setup.R
│   ├── 01_import_donnees.R
│   ├── 02_analyse_exploratoire.R
│   ├── 03_bvar_trimestriel.R
│   ├── 04_bridge_equations.R
│   ├── 05_ar4_non_couvertes.R
│   ├── 06_agregation_fisher.R
│   ├── 07_validation_pseudo_temps_reel.R
│   ├── 08_rapport_synthese.R
│   ├── 09_test_affinement_intra_trimestre.R
│   │
│   └── fonctions/
│       ├── information_set.R
│       ├── aggregation.R
│       ├── selection_recursive.R
│       ├── bridge_recursive.R
│       ├── delta_recursive.R
│       └── backtest.R
│
├── data/
├── results/
└── figures/
```

------------------------------------------------------------------------

# 28. Fonction centrale `information_set()`

La logique temporelle doit être centralisée :

``` r
information_set <- function(data, target_date) {
  data |>
    dplyr::filter(date <= target_date)
}
```

Aucun modèle ne devrait recevoir directement toute la base historique.

Il reçoit uniquement l'information correspondant à son origine.

------------------------------------------------------------------------

# 29. Fonction centrale `backtest_nowcast()`

Architecture recommandée :

``` r
backtest_nowcast <- function(target_quarter, data, params) {

  info <- information_set(
    data,
    quarter_end(target_quarter)
  )

  prepared <- prepare_data(info, params)

  selected <- select_indicators_recursive(
    data = prepared,
    target_quarter = target_quarter
  )

  bridge <- estimate_bridge_recursive(
    data = prepared,
    target_quarter = target_quarter,
    selected = selected
  )

  bvar <- estimate_bvar_recursive(
    data = prepared,
    target_quarter = target_quarter
  )

  ar <- estimate_ar_branches(
    data = prepared,
    target_quarter = target_quarter
  )

  delta <- estimate_delta_recursive(
    data = prepared,
    target_quarter = target_quarter
  )

  nowcast <- aggregate_forecasts(
    bridge = bridge,
    bvar = bvar,
    ar = ar,
    delta = delta
  )

  nowcast
}
```

L'objectif est d'avoir une logique temporelle unique et testable.

------------------------------------------------------------------------

# 30. Contrôles anti-look-ahead

Ajouter des contrôles automatiques.

### Contrôle 1 --- données d'estimation

``` r
stopifnot(
  max(training_data$date, na.rm = TRUE) < target_date
)
```

### Contrôle 2 --- données utilisées pour la prévision

``` r
stopifnot(
  all(prediction_data$date <= target_date)
)
```

### Contrôle 3 --- sélection

Vérifier que les performances futures ne servent jamais à sélectionner
les indicateurs du trimestre cible.

### Contrôle 4 --- poids

Vérifier que les poids sectoriels proviennent d'une date antérieure ou
égale à l'origine autorisée.

### Contrôle 5 --- delta

Vérifier que le delta est estimé uniquement sur les erreurs disponibles
avant `T`.

------------------------------------------------------------------------

# 31. Structure des résultats

Pour chaque trimestre :

``` text
target_quarter
real_gdp
nowcast_combo
nowcast_bvar
nowcast_bridge
nowcast_ar
benchmark_ar2
error_combo
error_bvar
error_bridge
error_ar
```

Ajouter :

``` text
selected_indicators
delta_branch_1
...
delta_branch_16
weight_branch_1
...
weight_branch_16
```

Pour M0/M1/M2/M3 :

``` text
target_quarter
nowcast_M0
nowcast_M1
nowcast_M2
nowcast_M3
real_gdp
error_M0
error_M1
error_M2
error_M3
```

------------------------------------------------------------------------

# 32. Ordre exact de modification du projet

Il ne faut pas tout modifier simultanément.

## Étape 1

`01_import_donnees.R`

Objectif :

> définir correctement les dates et l'ensemble d'information.

## Étape 2

`02_analyse_exploratoire.R`

Objectif :

> rendre les transformations compatibles avec le backtest.

## Étape 3

`03_bvar_trimestriel.R`

Objectif :

> rendre l'estimation récursive et exempte d'information future.

## Étape 4

`04_bridge_equations.R`

Objectifs :

> sélection récursive + agrégation correcte + bridge récursif + delta
> récursif.

## Étape 5

`05_ar4_non_couvertes.R`

Objectif :

> estimation récursive des AR.

## Étape 6

`06_agregation_fisher.R`

Objectif :

> utiliser des poids disponibles avant le trimestre cible.

## Étape 7

`07_validation_pseudo_temps_reel.R`

Objectif :

> orchestrer correctement le backtest complet.

## Étape 8

`09_test_affinement_intra_trimestre.R`

Objectif :

> reconstruire M0/M1/M2/M3 avec observations réelles + prévisions des
> mois manquants.

## Étape 9

`08_rapport_synthese.R`

Objectif :

> produire les résultats finaux à partir du nouveau backtest.

------------------------------------------------------------------------

# 33. Priorités absolues

Si certaines corrections doivent être priorisées :

### Priorité 1

\[ `\boxed{\text{Aucune information postérieure à }T}`{=tex} \]

### Priorité 2

\[ `\boxed{\text{Sélection récursive des indicateurs}}`{=tex} \]

### Priorité 3

\[ `\boxed{\text{Estimation récursive des Bridge et BVAR}}`{=tex} \]

### Priorité 4

\[ `\boxed{\text{Delta récursif}}`{=tex} \]

### Priorité 5

\[ `\boxed{\text{Poids sectoriels disponibles avant }T}`{=tex} \]

### Priorité 6

\[ `\boxed{\text{M0/M1/M2/M3 correctement construits}}`{=tex} \]

### Priorité 7

\[ `\boxed{\text{Agrégation mensuelle économiquement cohérente}}`{=tex}
\]

### Priorité 8

\[ `\boxed{\text{Saisonnalité et robustesse}}`{=tex} \]

------------------------------------------------------------------------

# 34. Schéma méthodologique final

``` text
                         DONNÉES
                            |
                            v
                 Définition du trimestre T
                            |
                            v
                  INFORMATION SET I_T
                    données <= T
                            |
                            v
                    TRANSFORMATIONS
                            |
                            v
                     RAGGED EDGE
                            |
                            v
                 TRAITEMENT MISSING
                            |
                            v
                SÉLECTION RÉCURSIVE
                  indicateurs S_T
                            |
              +-------------+-------------+
              |             |             |
              v             v             v
            BVAR          BRIDGE         AR(4)
              |             |             |
              +-------------+-------------+
                            |
                            v
                    DELTA RÉCURSIF
                            |
                            v
                  PRÉVISION DES 16 VA
                            |
                            v
                POIDS DISPONIBLES À T-1
                            |
                            v
                    AGRÉGATION PIB
                            |
                            v
                       NOWCAST T
                            |
                            v
                    PIB RÉEL DE T
                            |
                            v
                        ERREUR T
                            |
                            v
               RMSFE / MAE / DM / BIAIS
```

------------------------------------------------------------------------

# 35. Positionnement scientifique

Le modèle doit être présenté comme :

> **Un modèle de nowcasting du PIB marocain fondé sur un ensemble
> d'information défini par la date d'observation, avec sélection
> récursive des indicateurs, estimation récursive des modèles et analyse
> de l'apport de l'information intra-trimestrielle.**

Il ne faut pas le présenter comme un véritable système de vintages «
real-time », puisque les dates historiques de publication et les
révisions ne sont pas disponibles.

La distinction doit être explicitement mentionnée dans le mémoire ou
l'article.

------------------------------------------------------------------------

# 36. Règle d'or

Pour chaque trimestre cible `T`, poser la question :

> **« Cette information appartient-elle à l'ensemble d'information
> autorisé au moment de mon nowcast de T ? »**

Si non, elle doit être exclue.

La méthodologie finale repose donc sur quatre principes :

\[ `\boxed{
\begin{aligned}
1.&\quad \text{Information limitée à }T,\\
2.&\quad \text{Sélection récursive},\\
3.&\quad \text{Estimation récursive},\\
4.&\quad \text{Évaluation strictement hors échantillon}.
\end{aligned}}`{=tex} \]

Ces quatre principes constituent la colonne vertébrale de toute la
correction du projet.
