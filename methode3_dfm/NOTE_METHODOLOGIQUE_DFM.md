# Note méthodologique complète — Méthode 3 (DFM)

Document autonome, à lire indépendamment du reste de la conversation.

---

## 1. Principe du modèle à facteur dynamique

**Références** : Stock & Watson (2002), *« Macroeconomic Forecasting Using
Diffusion Indexes »* ; Doz, Giannone & Reichlin (2011), *« A Two-Step
Estimator for Large Approximate Dynamic Factor Models »*. Implémentation :
package R `dfms` (algorithme EM, lissage de Kalman).

**Différence fondamentale avec les Méthodes 1 et 2** : un DFM n'estime pas
une relation entre un indicateur et la cible. Il extrait **un facteur
commun latent** à partir de plusieurs indicateurs qui co-évoluent entre
eux, puis relie ce facteur (une seule série résumée) à la cible. La
sélection des indicateurs doit donc répondre à une question différente :
non pas « cet indicateur est-il corrélé à la VA ? » mais **« cet
indicateur partage-t-il une dynamique commune avec les autres
indicateurs du même groupe ? »**

---

## 2. Sélection des indicateurs — depuis la base complète

Conformément à la demande, la sélection est repartie de la **base
complète** (727 indicateurs bruts au total sur les 8 branches couvertes),
**pas** du sous-ensemble déjà filtré par les Méthodes 1 ou 2.

| # | Critère | Seuil |
|---|---|---|
| 1 | Fréquence | Mensuelle uniquement (nécessité structurelle du facteur mensuel) |
| 2 | Longueur brute | ≥ 12 observations natives |
| 3 | Fraîcheur | ≤ 450 jours |
| 4 | Densité interne | ≥ 80 % (Schulz & Grimes, 2002 — identique Méthodes 1 et 2) |
| 5 | Co-mouvement (item-reste) | r_reste ≥ 0,30, ≥ 2 indicateurs retenus par branche |

### Critère 5 — la vraie différence avec les Méthodes 1 et 2

**Référence** : Nunnally, J.C. & Bernstein, I.H. (1994), *Psychometric
Theory* (3e éd.), McGraw-Hill — seuil de référence le plus cité en
analyse factorielle exploratoire pour la corrélation item-reste
(« corrected item-total correlation ») : un item doit corréler à au moins
**0,30** avec le score moyen des autres items pour être considéré comme
une contribution significative au facteur commun. Cette méthode, conçue à
l'origine pour décider si un item de questionnaire appartient au même
construit psychologique que les autres, répond exactement au même
problème statistique ici : décider si un indicateur macroéconomique
appartient au même facteur latent que les autres candidats de sa branche.

Pour chaque indicateur i d'une branche, on calcule sa corrélation avec la
moyenne des autres candidats (l'indicateur étant exclu de son propre
score de comparaison, pour ne pas gonfler artificiellement sa corrélation
avec lui-même — version « corrigée » recommandée par Nunnally & Bernstein
plutôt que la corrélation item-total brute).

**Le critère de non-redondance (critère 7 des Méthodes 1 et 2) est
explicitement écarté ici** — une forte corrélation entre deux indicateurs
n'est pas un problème pour un DFM, c'est précisément ce qui permet au
facteur de capter le signal commun plutôt que le bruit individuel de
chaque série.

---

## 3. Résultat de la sélection — 141 séries, très inégalement réparties

| Branche | Nb indicateurs retenus | r_reste (min - max) |
|---|---|---|
| Pêche | 65 | 0,311 - 0,981 |
| Industrie d'extraction | 3 | 0,795 - 0,982 |
| Industrie de transformation | 2 | 0,891 - 0,907 |
| Électricité, gaz, eau | 62 | 0,433 - 0,995 |
| Finances et assurances | 9 | 0,340 - 0,763 |

**Trois branches à zéro** (Immobilier, Hébergement-restauration,
Construction) — vérifié individuellement, pas un bug : leurs candidats
mensuels ne partagent pas de dynamique commune suffisante (ex. Immobilier :
crédit habitat et crédit promoteurs immobiliers ont un r_reste de
0,06-0,16 seulement — deux réalités économiques différentes, pas un
défaut de données).

**Pêche et Électricité concentrent l'essentiel des séries retenues (65 et
62)**, avec des corrélations item-reste extrêmement élevées (jusqu'à
0,98) — économiquement cohérent : des dizaines de ports de pêche mesurent
au fond le même phénomène national (cycle de la pêche), un cas d'usage
qui correspond exactement à ce pour quoi un modèle à facteur est conçu.

---

## 4. Estimation du facteur et prévision

Pour chaque branche éligible (≥2 indicateurs retenus) :
1. Calendrier mensuel complet, Δlog standardisé (`scale()`)
2. DFM à 1 facteur, 2 retards (`dfms::DFM(X, r=1, p=2)`), estimé par
   algorithme EM (converge en 26-39 itérations selon la branche)
3. Le comblement des données manquantes se fait nativement dans
   l'algorithme EM (le lisseur de Kalman gère les valeurs manquantes à
   l'échelle du système entier) — pas besoin ici du comblement série par
   série de la Méthode 1, une différence méthodologique assumée : c'est
   la même famille de méthode (lissage de Kalman) appliquée au niveau
   multivarié plutôt que série par série.
4. Facteur trimestrialisé (moyenne des 3 mois), régression simple
   Δlog(VA) sur le facteur, prévision du facteur par AR(2) puis
   prévision de la VA par la régression ajustée

**Repli pour Immobilier, Hébergement-restauration, Construction** :
aucun facteur estimable → réutilisation de la prévision bridge+BVAR de
la Méthode 1.

---

## 5. Résultat — un signal partagé, mais pas de miracle

| | Méthode 1 (bridge) | Méthode 2 (MIDAS) | Méthode 3 (DFM) |
|---|---|---|---|
| Nowcast | +0,56 % | +0,21 % | +0,50 % |
| RMSFE backtest | 0,0072 | 0,0101 | 0,0082 |
| RMSFE AR(2) | 0,0067 | 0,0067 | 0,0067 |
| Écart relatif | -6,8 % | -50,0 % | -22,1 % |
| Diebold-Mariano | p=0,394 (non signif.) | p=0,096 (signif., défavorable) | p=0,284 (non signif.) |

**Aucune des trois méthodes ne bat significativement l'AR(2)** sur cette
fenêtre de test de 8 trimestres. Le DFM se classe entre la Méthode 1 (la
moins mauvaise) et MIDAS (la plus en difficulté), sans écart
statistiquement confirmé dans un sens ou dans l'autre.

### Un résultat frappant à ne pas balayer : R² très faibles malgré des r_reste très élevés

Pêche (R²=0,002) et Électricité (R²=0,006) ont les pools d'indicateurs les
plus densément corrélés entre eux (r_reste jusqu'à 0,98), mais le facteur
qui en résulte explique presque rien de la valeur ajoutée sectorielle.
Explication la plus probable : le facteur commun extrait de dizaines de
séries de captures physiques par port capture un phénomène de volume
(cycle naturel de la pêche, saisonnalité), alors que la valeur ajoutée
sectorielle dépend aussi des prix et de la transformation — deux choses
que le facteur, construit uniquement à partir de volumes physiques, ne
capture pas. Une piste non explorée ici : construire le facteur à partir
d'un mélange volumes/valeurs plutôt que des seules séries physiques.

---

## 6. Références de littérature mobilisées

| Référence | Usage |
|---|---|
| Stock & Watson (2002) | Cadre général des modèles à facteur pour la prévision macro |
| Doz, Giannone & Reichlin (2011) | Estimation par algorithme EM d'un DFM approximatif |
| Nunnally & Bernstein (1994). *Psychometric Theory* | Critère de sélection dédié (corrélation item-reste, seuil 0,30) |
| Schulz & Grimes (2002) | Seuil de densité (identique Méthodes 1 et 2) |
| Diebold & Mariano (1995) | Test de significativité du backtest |
| Bai & Ng (2008) | Discutée dans les notes précédentes comme référence proche mais non directement appliquée ici (voir Méthode 1) |

---

## 7. Limites assumées

1. R² très faibles pour Pêche et Électricité malgré un facteur statistiquement bien identifié (voir section 5)
2. Aucune des trois méthodes ne bat significativement l'AR(2) — résultat honnête, pas maquillé
3. Le seuil item-reste (0,30) vient de la psychométrie, jamais testé formellement en contexte macroéconomique — adaptation assumée, pas une reprise standard de la littérature du nowcasting
4. 3 branches sur 8 couvertes n'ont aucun facteur estimable, repli statique sur la Méthode 1
5. Un seul facteur extrait par branche (r=1) — un DFM à plusieurs facteurs n'a pas été testé, faute de temps

## 8. Recommandation

Aucune des trois méthodes ne se détache clairement. Le DFM a l'avantage
d'exploiter un panel beaucoup plus large sans souffrir du sur-ajustement
observé pour MIDAS, mais son pouvoir explicatif réel (R²) reste faible
pour les branches où le pool est le plus riche — un signal à creuser
(mélanger volumes et valeurs, tester plusieurs facteurs) avant de le
préférer à la Méthode 1.
