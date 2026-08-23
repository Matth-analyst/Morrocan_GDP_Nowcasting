# Note méthodologique complète — Méthode 2 (MIDAS)

Document autonome. **Version 2** : critères recalculés et rejustifiés
individuellement, en cohérence avec la révision faite pour la Méthode 1.

---

## 1. Principe du MIDAS

**Référence** : Ghysels, Santa-Clara & Valkanov (2004) ; spécification
**U-MIDAS** (Foroni, Marcellino & Schumacher, 2015) :

$$\Delta\log(VA)_t = \beta_0 + \beta_1 m_{1,t} + \beta_2 m_{2,t} + \beta_3 m_{3,t} + \varepsilon_t$$

**k = 3 prédicteurs** (hors constante) — ce nombre encadre toute la
sélection ci-dessous, et la distingue structurellement de la Méthode 1
(k=1).

---

## 2. Critère de sélection dédié — chaque seuil justifié

| # | Critère | Seuil |
|---|---|---|
| 1 | Fréquence | Mensuelle uniquement (structurel : MIDAS exige m1,m2,m3) |
| 2 | Longueur brute (pré-filtre) | ≥ 12 observations natives |
| 3 | Fraîcheur | ≤ 450 jours |
| 4 | Densité interne | ≥ 80 % (Schulz & Grimes, 2002 — identique Méthode 1) |
| 5 | Pertinence statistique | Test F joint (α=0,10) **et** R² ≥ 0,05, sur ≥ 14 trimestres complets |

### Critère 5 en détail — pourquoi 14 trimestres, pas 12

Même logique que la Méthode 1 (n − k − 1 ≥ 10 degrés de liberté
résiduels), mais **k=3 ici, pas k=1**. La version précédente de ce critère
avait été recopiée de la Méthode 1 sans être adaptée — avec seulement 12
trimestres et 4 paramètres à estimer (3 coefficients + constante), il ne
restait que 8 degrés de liberté, insuffisant. Corrigé : **n ≥ 14
trimestres** (14 − 3 − 1 = 10).

**Pourquoi pas la règle de Green (1991)** : pour k=3, elle imposerait
N ≥ 107 — mais en **trimestres**, ce qui correspondrait à environ 27 ans
de données mensuelles *sans le moindre trou*, une exigence qu'aucune de
nos séries n'atteint. Comme pour la Méthode 1, cette règle (conçue pour
des contextes d'enquête) est explicitement écartée au profit d'un plancher
minimal d'entrée, la vraie validation restant le backtest hors échantillon.

### Le test F joint plutôt que 3 tests séparés

$$H_0 : \beta_1 = \beta_2 = \beta_3 = 0$$

Pas un test individuel par mois : 3 tests à 10 % chacun donneraient ~27 %
de chance qu'au moins un ressorte « significatif » par hasard sous H0
(inflation du taux de faux positifs). Le test F est la façon
statistiquement correcte de poser la question « ces 3 coefficients,
ensemble, apportent-ils de l'information ? ».

**R² ≥ 0,05** : plancher assumé (comme le plancher \|r\| de la Méthode 1),
pour éviter qu'un test F « significatif » sur un grand échantillon ne
valide un modèle au pouvoir explicatif dérisoire.

---

## 3. Résultat de la sélection — 37 séries

| Branche | Séries Méthode 1 (bridge, k=1) | Séries Méthode 2 (MIDAS, k=3) |
|---|---|---|
| Pêche | 12 | 10 |
| Industrie de transformation | 17 | 2 (la plupart des indicateurs sont trimestriels) |
| Électricité, gaz, eau | 4 | 10 |
| Finances et assurances | 13 | 9 |
| Immobilier | 1 | 2 |
| Hébergement-restauration | 2 | 3 |
| Construction | 1 | 1 |
| Industrie d'extraction | 1 | 0 (unique indicateur trimestriel, MIDAS inapplicable) |

---

## 4. Jagged edge — identique à la Méthode 1

Même mécanisme (lissage de Kalman) pour reconstruire chaque indicateur
sur un calendrier mensuel complet avant de construire m1/m2/m3.

---

## 5. Repli pour Industrie d'extraction

Aucun indicateur mensuel éligible → réutilisation de la prévision
bridge+BVAR déjà combinée de la Méthode 1.

---

## 6. Résultat — signal d'alerte confirmé après la révision des critères

| | Méthode 1 | Méthode 2 (MIDAS) |
|---|---|---|
| Nowcast | +0,56 % | **+0,21 %** |
| RMSFE backtest | 0,0072 | **0,0101** |
| RMSFE repère AR(2) | 0,0067 | 0,0067 |
| Diebold-Mariano | p=0,394 (non significatif) | **p=0,096 (significatif, en défaveur de MIDAS)** |

**Résultat inchangé après la révision des critères** — la correction du
seuil de trimestres (12→14) et de densité (75%→80%) n'a pas modifié la
composition des 37 séries de façon significative. La sous-performance de
MIDAS n'était donc pas un artefact d'un seuil mal calibré : c'est un
résultat robuste à cette révision.

**Explication la plus probable** (Foroni, Marcellino & Schumacher, 2015) :
le U-MIDAS (3 coefficients libres, non contraints) expose à un
sur-ajustement en échantillon sur des séries courtes — une spécification
contrainte (Almon) réduirait probablement ce risque, non implémentée ici.

---

## 7. Références de littérature mobilisées

| Référence | Usage |
|---|---|
| Ghysels, Santa-Clara & Valkanov (2004) | Cadre général MIDAS |
| Foroni, Marcellino & Schumacher (2015) | Spécification U-MIDAS ; explication de la sous-performance |
| Doz, Giannone & Reichlin (2011) | Jagged edge |
| Diebold & Mariano (1995) | Test de significativité du backtest |
| Green (1991) | Discutée puis écartée (voir section 2) |
| Schulz & Grimes (2002) | Seuil de densité (identique Méthode 1) |

---

## 8. Liste complète des 37 séries retenues

| Branche | Indicateur | R² | F p-value | n trimestres | Source |
|---|---|---|---|---|---|
| Pêche | NADOR | 0.251 | 0.004 | 49 | Débarquements des produits de la pêche c |
| Pêche | NTIRIFT | 0.204 | 0.017 | 48 | Débarquements des produits de la pêche c |
| Pêche | LABOUIRDA | 0.190 | 0.022 | 49 | Débarquements des produits de la pêche c |
| Pêche | IMESSOUANE | 0.162 | 0.045 | 49 | Débarquements des produits de la pêche c |
| Pêche | LASSARGA | 0.160 | 0.051 | 48 | Débarquements des produits de la pêche c |
| Pêche | RAS KEBDANA | 0.155 | 0.054 | 49 | Débarquements des produits de la pêche c |
| Pêche | Imoutlan | 0.153 | 0.060 | 48 | Débarquements des produits de la pêche c |
| Pêche | Débarquement de produits de pêche EN VALEUR  | 0.144 | 0.065 | 50 | Manar-Stat |
| Pêche | TIFNIT | 0.167 | 0.071 | 42 | Débarquements des produits de la pêche c |
| Pêche | TAFEDNA | 0.175 | 0.085 | 38 | Débarquements des produits de la pêche c |
| Industrie de transformation | Taux d’Utilisation des Capacités | 0.299 | 0.000 | 61 | Manar-Stat (Ministere de l'Economie et d |
| Industrie de transformation | TUC industrielle | 0.299 | 0.000 | 61 | Taux d’utilisation des capacités (mensue |
| Électricité, gaz, eau | dont Production de source renouvelable (éolien) | 0.402 | 0.030 | 21 | energie.csv |
| Électricité, gaz, eau | . Centrale de Tahaddart | 0.207 | 0.041 | 39 | energie.csv |
| Électricité, gaz, eau | Parc Eolien de Tarfaya | 0.159 | 0.052 | 48 | Énergie appelée nette (mensuel).csv |
| Électricité, gaz, eau | .Parc éolien Oualidia | 0.355 | 0.065 | 20 | energie.csv |
| Électricité, gaz, eau | dont Turbinage de la STEP | 0.110 | 0.066 | 65 | Énergie appelée nette (mensuel).csv |
| Électricité, gaz, eau | . Centrale de SAFIEC | 0.246 | 0.066 | 29 | energie.csv |
| Électricité, gaz, eau | . Parc Eolien de Tarfaya (TAREC) | 0.174 | 0.067 | 41 | energie.csv |
| Électricité, gaz, eau | . Solaire (Centrale solaire d'Assa + Tafilalet PV) | 0.173 | 0.068 | 41 | energie.csv |
| Électricité, gaz, eau | .Parc éolien Akhfennir | 0.166 | 0.078 | 41 | energie.csv |
| Électricité, gaz, eau | Distributeurs | 0.062 | 0.080 | 110 | Ventes ONEE d'électricité (mensuel).csv |
| Immobilier | Crédits aux promoteurs immobiliers (1) | 0.127 | 0.005 | 97 | 12- Ventilation du crédit bancaire par o |
| Immobilier | Crédits à l'habitat (MDH) (Jan 18) | 0.090 | 0.088 | 73 | Bank Al-Maghrib |
| Hébergement-restauration | Recettes touristiques (mensuel) | 0.180 | 0.000 | 111 | Manar-Stat |
| Hébergement-restauration | Arrivés de touristes aux frontières | 0.130 | 0.021 | 73 | Manar-Stat |
| Hébergement-restauration | Recettes touristiques (mensuel cumulé) | 0.068 | 0.055 | 112 | Tourisme_Recettes touristiques.csv |
| Finances et assurances | Comptes débiteurs et crédits de trésorerie | 0.174 | 0.000 | 97 | 12- Ventilation du crédit bancaire par o |
| Finances et assurances | Secteur privé | 0.155 | 0.001 | 97 | 13- Ventilation du crédit bancaire par s |
| Finances et assurances | Autres(2) | 0.148 | 0.002 | 97 | 13- Ventilation du crédit bancaire par s |
| Finances et assurances | Sociétés non financières privées | 0.140 | 0.003 | 97 | 13- Ventilation du crédit bancaire par s |
| Finances et assurances | Crédits à l'équipement | 0.118 | 0.008 | 97 | 12- Ventilation du crédit bancaire par o |
| Finances et assurances | Autres secteurs résidents | 0.116 | 0.009 | 97 | 13- Ventilation du crédit bancaire par s |
| Finances et assurances | Masse monétaire (M3) | 0.141 | 0.027 | 64 | BDD SECTORIEL (source institutionnelle c |
| Finances et assurances | Particuliers et Marocains Résidant à l'Etranger | 0.083 | 0.043 | 97 | 13- Ventilation du crédit bancaire par s |
| Finances et assurances | Crédits à caractére financier(2) | 0.073 | 0.069 | 97 | 12- Ventilation du crédit bancaire par o |
| Construction | Vente de ciment (1000tonnes) | 0.072 | 0.050 | 108 | Manar-Stat (Ministere de l'Economie et d |

---

## 9. Limites assumées

1. **Sous-performance significative vs. AR(2)**, confirmée robuste à la révision des critères
2. Saisonnalité non traitée
3. Repli statique pour Industrie d'extraction dans le backtest
4. Aucun suivi du signe économique des coefficients m1/m2/m3
5. α=0,10 et R²≥0,05 restent des choix assumés, non dérivés d'une règle externe

## 10. Mise à jour finale — spécification Almon implémentée et testée

La piste recommandée en section 9 a été mise en œuvre : remplacement des 3
coefficients libres (β1, β2, β3, U-MIDAS) par une contrainte d'Almon de
degré 1 (Almon, S., 1965, *« The Distributed Lag Between Capital
Appropriations and Expenditures »*, Econometrica, 33(1), 178-196 ; reprise
dans le cadre MIDAS par Ghysels, Sinko & Valkanov, 2007) : β_j = a + b·j,
soit 2 paramètres au lieu de 3.

**Résultat (16 trimestres de test)** :

| | U-MIDAS (libre) | Almon (contraint) |
|---|---|---|
| RMSFE | 0,0085 | **0,0081** |
| Diebold-Mariano (p-value) | 0,087 (significatif, défavorable) | **0,109 (non significatif)** |

**Amélioration réelle, conforme au diagnostic** (Foroni, Marcellino &
Schumacher, 2015) : contraindre les coefficients réduit bien le
sur-ajustement. MIDAS n'est plus prouvé significativement pire que l'AR(2)
— mais reste, en valeur absolue, moins précis que la Méthode 1 (RMSFE
0,0065) dans toutes les configurations testées, y compris en combinaison.

## 11. Recommandation finale

**Conserver la spécification Almon** comme version de référence de MIDAS
si cette méthode doit être présentée, mais **retenir la Méthode 1 comme
modèle principal** — MIDAS reste, dans toutes les configurations testées
(seule, combinée, U-MIDAS ou Almon), la méthode la moins performante des
trois.
