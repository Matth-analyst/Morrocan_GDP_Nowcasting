# Note méthodologique complète — Méthode 1 (BVAR + Bridge equations + AR(4))

Ce document consolide, en un seul endroit, l'ensemble des méthodes de
sélection et des techniques de modélisation appliquées dans ce dossier —
à lire indépendamment du reste de la conversation.

**Version 2** : tous les seuils de la cascade de critères ont été
recalculés et rejustifiés individuellement (voir section 2), après
constat que plusieurs d'entre eux avaient été recopiés d'une étape à
l'autre sans être repensés pour le cas d'usage exact.

---

## 1. Architecture générale

Adaptation du modèle GDPNow (Federal Reserve Bank d'Atlanta) à une logique
**offre** (production par branche d'activité, nomenclature HCP) plutôt que
demande. Trois briques, appliquées selon la couverture de chaque branche :

| Brique | Branches concernées | Rôle |
|---|---|---|
| **BVAR trimestriel** | Les 16 branches | Prévision de repli, prior bayésien Minnesota |
| **Bridge equations** | 8 branches disposant d'au moins un indicateur validé | Combine indicateur(s) d'activité et BVAR |
| **AR(4) pur** | 8 branches sans indicateur validé | Extrapolation de tendance uniquement |

La bridge equation estime : `Δlog(VA) = β0 + β1·Δlog(indicateur) + ε`,
soit **k = 1 prédicteur** (hors constante) — ce nombre encadre toute la
section suivante.

---

## 2. Sélection des indicateurs — la cascade de 8 critères, chacun justifié

| # | Critère | Seuil retenu |
|---|---|---|
| 1 | Fréquence | Mensuelle ou trimestrielle |
| 2 | Longueur brute (pré-filtre) | ≥ 12 observations natives |
| 3 | Fraîcheur | ≤ 450 j (mensuel) / ≤ 365 j (trimestriel) |
| 4 | Densité interne | ≥ 80 % |
| 5 | Pertinence statistique | Test de Student (α=0,10) **et** \|r\| ≥ 0,15, sur ≥ 12 trimestres communs |
| 6 | Cohérence du signe | Observé, non filtrant |
| 7 | Non-redondance | Corrélation croisée < 0,85 |
| 8 | Cible unique | Version la plus longue, ≥ 40 observations |

### Critère 1 — Fréquence
Justification structurelle, non statistique : une série annuelle ne peut
informer une prévision infra-annuelle. Aucun seuil numérique à justifier.

### Critère 2 — Longueur brute (pré-filtre léger)
**Ce n'est plus le vrai filtre de puissance statistique** (déplacé au
critère 5, correctement dimensionné — voir plus bas). Ici, 12 observations
natives sert seulement de garde-fou pour que l'inférence de fréquence
(sur l'écart médian entre observations) ait un sens, et pour disposer d'au
moins un an de mensuel ou trois ans de trimestriel avant tout calcul.

### Critère 3 — Fraîcheur
**Choix pragmatique, assumé comme tel** — pas de règle statistique externe.
Au-delà d'un an sans nouvelle publication (+ marge pour le délai de
publication habituel des comptes trimestriels HCP, ~60-90 jours), une
série est présumée interrompue plutôt que simplement en retard.

### Critère 4 — Densité interne (≥ 80 %)
**Justifié par une référence vérifiée** : Schulz, K.F. & Grimes, D.A.
(2002), *« Sample size slippages in randomised trials: exclusions and the
lost and wayward »*, The Lancet, 359(9308), 781-785 — règle de référence
largement citée selon laquelle la validité d'une analyse est menacée à
partir de 20 % de données manquantes. Le seuil de densité (≥80 %, soit
≤20 % manquant) colle précisément à cette limite, plutôt qu'un chiffre
choisi à l'instinct (le seuil précédent, 75 %, se situait légèrement
au-delà de cette limite documentée — corrigé ici).

### Critère 5 — Pertinence statistique (le cœur de la révision)

**La référence académique la plus directement pertinente pour dimensionner
un échantillon en fonction du nombre de prédicteurs** est Green, S.B.
(1991), *« How Many Subjects Does It Take To Do A Regression Analysis? »*,
Multivariate Behavioral Research, 26(3), 499-510 — 3600+ citations. Green
propose :

$$N \geq 50 + 8k \quad \text{(ajustement global)} \qquad N \geq 104 + k \quad \text{(test d'un coefficient individuel)}$$

Pour k=1 (notre bridge equation), cela donnerait **N ≥ 105 observations**.

**Cette règle est explicitement écartée comme inapplicable ici** : notre
cible ne compte au maximum que 113 trimestres au total (le PIB marocain
trimestriel n'existe pas avant 1998 — une contrainte de fait, pas un
choix). Retenir 105 ne laisserait presque aucune marge pour réserver les 8
trimestres du backtest. Plus fondamentalement, la règle de Green a été
développée pour des contextes d'enquête/expérimentation (où l'on peut
généralement recruter davantage de sujets), pas pour des séries
macroéconomiques dont la longueur est fixée par l'histoire. **La
littérature du nowcasting elle-même (Higgins 2014 ; Bańbura, Giannone &
Reichlin 2010 ; Ghysels, Santa-Clara & Valkanov 2004) n'applique jamais de
règle de taille d'échantillon a priori** — la validité s'établit *a
posteriori*, par la performance hors échantillon (RMSFE, test de
Diebold-Mariano), exactement ce que fait notre propre backtest.

**Seuil minimal retenu à la place** : au moins **10 degrés de liberté
résiduels** (n − k − 1 ≥ 10), pratique minimale usuelle pour qu'une
régression OLS ne soit pas dégénérée. Pour k=1 : **n ≥ 12 trimestres**.
C'est un plancher d'entrée, pas une garantie de fiabilité — la vraie
preuve de validité reste le backtest.

**Le test lui-même** — inférence standard sur le coefficient de Pearson
(tout manuel d'économétrie, ex. Wooldridge, *Introductory Econometrics*) :

$$t = r\sqrt{\frac{n-2}{1-r^2}} \sim \text{Student}(n-2) \text{ sous } H_0: \rho=0$$

**α = 0,10 (plutôt que 0,05)** : choix assumé, pas issu d'une source
externe précise — pratique répandue en sélection exploratoire de
variables de retenir un seuil plus permissif qu'en test confirmatoire,
pour ne pas exclure à tort un indicateur à ce stade préliminaire (il reste
ensuite soumis aux critères 6 et 7, et surtout au backtest).

**Plancher \|r\| ≥ 0,15** : choix assumé (pas une règle externe), destiné
spécifiquement à contrer le « piège du grand échantillon » constaté en
pratique sur ce jeu de données (avec n=294 par exemple, une corrélation de
0,12 devient déjà « significative » sans portée économique réelle).

**Une référence écartée à dessein** : Bai & Ng (2008) a été envisagée puis
écartée — leur méthode de seuillage sert à sélectionner des prédicteurs
pour la construction de **facteurs communs** (pertinent pour la Méthode 3,
DFM), pas pour une régression univariée simple comme la bridge equation.

### Critère 6 — Cohérence du signe
Observé et documenté (marquage ⚠️), non filtrant — décision assumée pour
ne pas sur-restreindre à ce stade ; à examiner individuellement avant tout
usage en production (voir section 5).

### Critère 7 — Non-redondance (< 0,85)
**Justifié par la littérature de la multicolinéarité** : une corrélation
croisée entre deux prédicteurs supérieure à 0,8-0,9 est le seuil d'alerte
le plus couramment cité dans les guides appliqués de diagnostic de
multicolinéarité (repris notamment dans Gujarati, *Basic Econometrics*, et
dans la quasi-totalité des ressources de référence sur le sujet). 0,85 se
situe au milieu de cette fourchette usuelle.

### Critère 8 — Cible unique par branche
Nécessité logique/définitionnelle : élimine la redondance des ~26
variantes de VA par branche (bases 2014, 2007, rétropolées...), qui ne
sont pas des informations différentes mais des codages multiples du même
signal.

---

## 3. Traitement des valeurs manquantes — le jagged edge

**Principe rejeté explicitement** : la suppression pure des mois/trimestres
manquants (pratique initiale, corrigée sur demande explicite). Cette
suppression contredit la méthode de référence de Higgins (2014), qui
**prévoit** les points manquants plutôt que de les supprimer.

**Méthode retenue** : chaque indicateur est reconstruit sur un calendrier
complet. Les trous sont comblés par **lissage de Kalman**
(`stats::arima` + `stats::KalmanSmooth`, base R) sur un modèle AR ajusté
en log-niveau — le même mécanisme conceptuel que la littérature du jagged
edge (Doz, Giannone & Reichlin, 2011).

**Limite assumée** : ce mécanisme ne traite pas la saisonnalité (cas
concret : recettes touristiques, voir README.md pour le détail).

---

## 4. Le reste de l'architecture (BVAR, bridge, AR(4), agrégation)

- **BVAR trimestriel** : prior Minnesota par observations fictives
  (Litterman, 1986 ; Bańbura, Giannone & Reichlin, 2010), 5 retards, λ=0,15.
- **Bridge equations** : combinaison avec le BVAR par moindres carrés
  restreints (poids δ optimisé en échantillon, Higgins 2014 équation 8).
- **AR(4)** : pour les branches sans indicateur validé.
- **Agrégation** : pondération par part de valeur ajoutée en volume
  (limite connue : non additive en toute rigueur).
- **Validation** : backtest en pseudo temps réel (8 derniers trimestres),
  repère AR(2), test de Diebold-Mariano (1995).

---

## 5. Liste complète des 51 séries retenues

⚠️ = signe de corrélation contre-intuitif.

| Branche | Indicateur | Fréquence | r | p-value | n | Source |
|---|---|---|---|---|---|---|
| Pêche | RKOUNTE ⚠️ | mensuel | -0.335 | 0.010 | 157 | Débarquements des produits de la pêche c |
| Pêche | AINBIDA | mensuel | +0.398 | 0.011 | 155 | Débarquements des produits de la pêche c |
| Pêche | ESSAOUIRA ⚠️ | mensuel | -0.283 | 0.025 | 173 | Débarquements des produits de la pêche c |
| Pêche | DAKHLA (STOCK C) ⚠️ | mensuel | -0.263 | 0.037 | 172 | Débarquements des produits de la pêche c |
| Pêche | TARFAYA ⚠️ | mensuel | -0.252 | 0.046 | 171 | Débarquements des produits de la pêche c |
| Pêche | MOHAMMEDIA ⚠️ | mensuel | -0.250 | 0.048 | 172 | Débarquements des produits de la pêche c |
| Pêche | IMI OUADDAR ⚠️ | mensuel | -0.236 | 0.065 | 173 | Débarquements des produits de la pêche c |
| Pêche | ATLANTIQUE ⚠️ | mensuel | -0.232 | 0.068 | 173 | Débarquements des produits de la pêche c |
| Pêche | POISSON BLANC ⚠️ | mensuel | -0.220 | 0.080 | 179 | Débarquements des produits de la pêche c |
| Pêche | SID ELGHAZI ⚠️ | mensuel | -0.221 | 0.082 | 173 | Débarquements des produits de la pêche c |
| Pêche | Comptes débiteurs et crédits de trésorerie — Pêche (MDH ⚠️ | trimestriel | -0.198 | 0.084 | 78 | Bank Al-Maghrib |
| Pêche | JEBHA ⚠️ | mensuel | -0.211 | 0.097 | 173 | Débarquements des produits de la pêche c |
| Industrie d'extraction | IPM — Autres industries extractives | trimestriel | +0.425 | 0.006 | 41 | Manar-Stat (Indice production minière, b |
| Industrie de transformation | IPI — Métallurgie | trimestriel | +0.727 | 0.000 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | IPI — Fabrication d’autres produits minéraux non métall | trimestriel | +0.710 | 0.000 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | Taux d’Utilisation des Capacités | mensuel | +0.565 | 0.000 | 188 | Manar-Stat (Ministere de l'Economie et d |
| Industrie de transformation | IPI — Travail du bois et fabrication d’articles en bois | trimestriel | +0.638 | 0.000 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | IPI — Fabrication de produits métalliques, à l’exclusio | trimestriel | +0.615 | 0.000 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | IPI — Réparation et installation de machines et équipem | trimestriel | +0.608 | 0.000 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | IPI — Fabrication de machines et équipements n.c.a. | trimestriel | +0.506 | 0.001 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | IPI — Fabrication de textiles | trimestriel | +0.489 | 0.001 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | IPI — Industrie automobile | trimestriel | +0.481 | 0.002 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | IPI — Industrie d’habillement | trimestriel | +0.473 | 0.002 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | IPI — Fabrication de produits à base de tabac | trimestriel | +0.466 | 0.002 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | IPI — Fabrication de boissons | trimestriel | +0.453 | 0.003 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | IPI — Fabrication d’équipements électriques | trimestriel | +0.326 | 0.040 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | IPI — Fabrication de meubles | trimestriel | +0.305 | 0.056 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | IPI — Industrie pharmaceutique | trimestriel | +0.297 | 0.063 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | IPI — Fabrication de produits en caoutchouc et en plast | trimestriel | +0.287 | 0.072 | 41 | Manar-Stat (Indice production industriel |
| Industrie de transformation | IPI — Industrie du cuir et de la chaussure (à l’excepti | trimestriel | +0.276 | 0.084 | 41 | Manar-Stat (Indice production industriel |
| Électricité, gaz, eau | Production et distribution d’électricité | trimestriel | +0.613 | 0.000 | 41 | Indice de production énergétique base 20 |
| Électricité, gaz, eau | - Solde des échanges d'énergie (Espagne-Algérie) : | mensuel | +0.493 | 0.004 | 130 | energie.csv |
| Électricité, gaz, eau | dont Production de source renouvelable (éolien) | mensuel | +0.545 | 0.007 | 72 | energie.csv |
| Électricité, gaz, eau | Petcoke | mensuel | +0.192 | 0.054 | 304 | Consommation de l'ONEE en combustibles ( |
| Immobilier | Crédits aux promoteurs immobiliers (1) | mensuel | +0.365 | 0.000 | 294 | 12- Ventilation du crédit bancaire par o |
| Hébergement-restauration | Recettes touristiques (mensuel) | mensuel | +0.360 | 0.000 | 381 | Manar-Stat |
| Hébergement-restauration | Comptes débiteurs et crédits de trésorerie — Hébergemen ⚠️ | trimestriel | -0.222 | 0.052 | 78 | Bank Al-Maghrib |
| Finances et assurances | Secteur privé | mensuel | +0.407 | 0.000 | 294 | 13- Ventilation du crédit bancaire par s |
| Finances et assurances | Autres(2) | mensuel | +0.326 | 0.001 | 294 | 13- Ventilation du crédit bancaire par s |
| Finances et assurances | Sociétés non financières privées | mensuel | +0.310 | 0.002 | 294 | 13- Ventilation du crédit bancaire par s |
| Finances et assurances | Comptes débiteurs et crédits de trésorerie | mensuel | +0.301 | 0.003 | 294 | 12- Ventilation du crédit bancaire par o |
| Finances et assurances | Autres sociétés financières | mensuel | +0.288 | 0.004 | 294 | 13- Ventilation du crédit bancaire par s |
| Finances et assurances | Particuliers et Marocains Résidant à l'Etranger | mensuel | +0.286 | 0.004 | 294 | 13- Ventilation du crédit bancaire par s |
| Finances et assurances | Crédit bancaire Activités financières (MDH) | trimestriel | +0.297 | 0.009 | 78 | Bank Al-Maghrib |
| Finances et assurances | Crédit bancaire | mensuel | +0.258 | 0.027 | 222 | Bank Al-Maghrib |
| Finances et assurances | Crédits à l'équipement | mensuel | +0.215 | 0.035 | 294 | 12- Ventilation du crédit bancaire par o |
| Finances et assurances | ISBLSM | mensuel | +0.208 | 0.041 | 294 | 13- Ventilation du crédit bancaire par s |
| Finances et assurances | Autres secteurs résidents | mensuel | +0.195 | 0.055 | 294 | 13- Ventilation du crédit bancaire par s |
| Finances et assurances | Dépôts à vue auprés des banques | mensuel | +0.222 | 0.078 | 196 | BDD SECTORIEL (source institutionnelle c |
| Finances et assurances | Masse monétaire (M3) | mensuel | +0.221 | 0.080 | 196 | BDD SECTORIEL (source institutionnelle c |
| Construction | Vente de ciment (1000tonnes) | mensuel | +0.216 | 0.022 | 373 | Manar-Stat (Ministere de l'Economie et d |

---

## 6. Références de littérature mobilisées

| Référence | Usage |
|---|---|
| Higgins, P. (2014). *GDPNow*. FRB Atlanta WP 2014-7 | Architecture générale, jagged edge |
| Litterman, R. (1986) | Prior Minnesota du BVAR |
| Bańbura, Giannone & Reichlin (2010). *Large Bayesian VARs* | Observations fictives |
| Doz, Giannone & Reichlin (2011) | Mécanisme du jagged edge |
| Diebold & Mariano (1995) | Test de significativité du backtest |
| **Green, S.B. (1991)**. *How Many Subjects...*, Multivariate Behavioral Research 26(3) | Référence de dimensionnement d'échantillon — discutée puis écartée comme inapplicable au contexte macro, avec justification explicite |
| **Schulz & Grimes (2002)**. *Sample size slippages...*, The Lancet 359(9308) | Seuil de densité interne (critère 4) |
| Bai & Ng (2008) | Discutée puis écartée (pertinente pour la Méthode 3, pas la bridge equation) |
| Wooldridge — *Introductory Econometrics* | Test de significativité standard sur r |
| Gujarati — *Basic Econometrics* (et guides usuels de diagnostic) | Seuil de non-redondance (critère 7) |

---

## 7. Limites assumées, non corrigées dans cette livraison

1. Saisonnalité non traitée
2. Poids d'agrégation approximatifs (valeur ajoutée en volume, non prix courants)
3. Signe contre-intuitif non filtré pour certaines séries (marquage ⚠️ uniquement)
4. Absence de poids de sous-branche pour la moyenne des prévisions bridge
5. 8 branches sur 16 toujours sans indicateur (AR(4) pur)
6. α=0,10 et les planchers économiques (\|r\|≥0,15) restent des choix
   assumés, non dérivés d'une règle externe — transparence complète sur
   ce point plutôt que de leur prêter une autorité qu'ils n'ont pas
