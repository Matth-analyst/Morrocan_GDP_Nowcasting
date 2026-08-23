# Méthode 1 (BVAR + Bridge equations + AR(4)) — version « jagged edge »

## Ce qui a changé par rapport à la version précédente

**Avant** : les mois/trimestres manquants d'un indicateur étaient simplement
absents de la séquence de dates avant l'agrégation trimestrielle — perte
d'information réelle, en contradiction avec la méthode originale de Higgins
(2014), qui **prévoit** les points manquants plutôt que de les supprimer
(section « autorégressions augmentées par facteur » du papier GDPNow).

**Maintenant** : chaque indicateur est reconstruit sur un **calendrier
complet** (du premier au dernier point observé), les trous étant comblés par
**lissage de Kalman** (`stats::arima` + `stats::KalmanSmooth`, base R, aucun
package supplémentaire) — le même mécanisme conceptuel que la littérature du
jagged edge (Doz, Giannone & Reichlin, 2011), appliqué ici indicateur par
indicateur plutôt qu'à un facteur commun (réservé à la méthode 3, DFM).

Le même modèle ajusté pour le comblement sert aussi à la **prévision du
prochain point** (au lieu d'un AR(1) séparé, ad hoc, dans la version
précédente) — plus cohérent, un seul modèle pour les deux usages.

## Résultat testé (exécution réelle, pas seulement relecture du code)

Le pipeline complet (8 scripts) a été exécuté de bout en bout sans erreur.

| | Avant (suppression) | Après (jagged edge) |
|---|---|---|
| Nowcast du prochain trimestre | +0,85 % | **+0,48 %** |
| RMSFE backtest (8 trimestres) | 0,0074 | 0,0075 |
| RMSFE repère AR(2) | 0,0067 | 0,0067 |
| Test de Diebold-Mariano (p-value) | 0,247 | 0,122 |

Comblement effectif observé : 17-19 % pour les 6 indicateurs de la Pêche,
17-23 % pour les 3 séries IPAI (Immobilier) — les branches qui gagnent le
plus d'information avec ce changement, cohérent avec ce qu'on savait déjà de
leur densité.

## Limite découverte pendant les tests — saisonnalité non traitée

**Un problème réel, distinct du jagged edge, a été mis au jour en testant.**
La branche Hébergement-restauration (indicateur : recettes touristiques)
affiche un changement de prévision très marqué (+0,017 → **-0,065**) alors
que son taux de comblement est quasi nul (1 %) — ce n'est donc pas le
comblement des trous qui en est la cause.

**Cause identifiée** : les recettes touristiques sont fortement
**saisonnières** (ex. 19 595 Mdh en août 2025, contre 7 669 en février 2025).
Le modèle AR(4) non saisonnier utilisé pour prévoir le prochain point
confond une baisse saisonnière normale (creux janvier-mars) avec un
ralentissement conjoncturel réel. Ce problème existait déjà, de façon moins
visible, dans la version précédente (AR(1) simple, moins sensible à ce
type d'erreur) — le nouveau mécanisme de prévision, plus robuste par
ailleurs, l'a rendu manifeste sur ce cas précis.

**Ce n'est pas corrigé dans cette livraison** — une vraie correction
demanderait une composante saisonnière explicite (SARIMA avec terme
saisonnier période=12, ou des indicatrices de mois dans la régression), un
chantier à part entière qui mériterait son propre test avant d'être généralisé
à toutes les séries mensuelles concernées (recettes touristiques, et
probablement d'autres séries à forte saisonnalité non encore identifiées).

## Recommandation

1. Traiter ce dossier comme la référence à jour pour la Méthode 1
2. Ouvrir un chantier dédié à la saisonnalité avant de considérer le modèle
   comme définitif — en particulier pour toute série dont le nom laisse
   penser à un profil saisonnier marqué (tourisme, agriculture, BTP...)

## Mise à jour — nouveau critère de sélection des indicateurs (test de significativité)

Le critère de corrélation (seuil fixe |r| ≥ 0,25) a été remplacé par un test
de significativité statistique standard (test de Student sur le coefficient
de Pearson, seuil α=0,10 bilatéral), complété d'un plancher économique
|r| ≥ 0,15 pour éviter qu'un grand échantillon ne valide une corrélation
triviale (voir échange dédié pour la justification complète — le seuil fixe
ignorait la taille d'échantillon, ce qui pénalisait à tort les séries
longues et pouvait laisser passer à tort les séries courtes).

**Conséquence** : 41 → **51 séries retenues**, et **Construction** rejoint
désormais le groupe des branches couvertes (8 sur 16, contre 7 avant) grâce
aux ventes de ciment (r=0,22 sur 373 observations, invisible sous l'ancien
seuil fixe).

| Branche | Avant (seuil fixe) | Après (test + plancher) |
|---|---|---|
| Pêche | 6 | 12 |
| Finances et assurances | 8 | 13 |
| Industrie de transformation | 17 | 17 |
| Construction | 0 | 1 (nouvelle branche couverte) |
| Hébergement-restauration | 1 | 2 |
| Industrie d'extraction | 1 | 1 |
| Électricité, gaz, eau | 4 | 4 |
| Immobilier | 4 | 1 |

**Sur Immobilier** : la chute à 1 n'est pas un artefact — vérifiée après
réintégration des 3 séries IPAI (précédemment retenues) et nettoyage des
doublons du fichier source. Aucune des 17 combinaisons catégorie×indicateur
IPAI testées ne passe simultanément tous les critères, principalement à
cause de trous de densité réels dans les catégories les moins courantes
(Villa, Bureau, Appartement — 40 à 70 % de densité seulement selon la
catégorie).

**Sur Hébergement-restauration** : la série "Recettes touristiques" a été
réintégrée sur son historique complet (381 observations, 1994-2026, contre
217 précédemment tronquées — voir l'errata plus haut), ce qui explique
pourquoi elle reste retenue et gagne même une seconde série.

### Résultat du pipeline complet avec les 51 séries (8 branches couvertes)

| | 41 séries (7 branches) | 51 séries (8 branches) |
|---|---|---|
| Nowcast du prochain trimestre | +0,48 % | **+0,56 %** |
| RMSFE backtest (8 trimestres) | 0,0075 | 0,0072 |
| RMSFE repère AR(2) | 0,0067 | 0,0067 |
| Test de Diebold-Mariano (p-value) | 0,122 | 0,394 |

Le modèle ne bat toujours pas le repère AR(2) sur cette fenêtre de test
courte (résultat honnête, cohérent avec tous les tests précédents) — mais
se rapproche légèrement de sa performance, et couvre désormais une branche
de plus.

**δ (poids BVAR) par branche couverte** : Pêche 1,00 · Construction 1,00 ·
Immobilier 0,52 · Industrie d'extraction 0,31 · Finances et assurances 0,22
· Industrie de transformation 0,15 · Hébergement-restauration 0,09 ·
Électricité, gaz, eau 0,03. Pêche et Construction retombent entièrement sur
le BVAR (δ=1) — leurs indicateurs, une fois testés, n'apportent finalement
rien de plus une fois combinés en échantillon.

## Mise à jour finale — tests complémentaires (indicatrice Covid, filtrage des signes, affinement intra-trimestriel)

**Indicatrice Covid dans le BVAR** (T2-2020, T3-2020, prior aussi lâche que
la constante) : RMSFE inchangé (0,0065 sur 16 trimestres de test) — la
fenêtre de backtest (2022-2026) est hors du choc, donc sans effet direct
sur ce test précis. Conservée néanmoins dans la version finale, par
rigueur méthodologique (traitement correct d'une rupture structurelle
connue), même si son effet mesuré est neutre ici.

**Filtrage des 12 séries à signe contre-intuitif (⚠️)** : testé, RMSFE
légèrement dégradé (0,0065 → 0,0067). **Décision : conservées.** Même à
signe contre-intuitif, ces séries portent une information statistiquement
significative — les retirer retire aussi du signal réel, pas seulement du
bruit.

**Test de l'affinement intra-trimestriel** (le mécanisme central de
GDPNow — la prévision s'améliore-t-elle à mesure que les mois du
trimestre cible arrivent ?) : amélioration marginale et non significative
(RMSFE 0,0045 → 0,0045 → 0,0044 sur 3 mois, p=0,690). Cause identifiée :
2 des 8 branches couvertes (Pêche, Construction) ont un poids δ=1,00 (100%
BVAR), ignorant structurellement toute nouvelle donnée d'indicateur. Un
plafonnement de δ à 0,8 a été testé pour forcer un poids minimal aux
indicateurs — sans effet mesurable (RMSFE quasi identique, p=0,719) : la
cause est plus profonde que la seule pondération (qualité intrinsèque des
indicateurs de Pêche, indicateur unique pour Construction, et le lissage
de Kalman qui peut lui-même atténuer la « surprise » apportée par une
nouvelle observation réelle).

### Résultat final retenu

| | Valeur |
|---|---|
| Configuration | 51 séries, jagged edge, indicatrice Covid, sans filtrage de signe |
| Nowcast du prochain trimestre | **+0,66 %** |
| RMSFE (16 trimestres de test) | **0,0065** |
| RMSFE repère AR(2) | 0,0062 |
| Écart | -4,2 % |
| Diebold-Mariano | p=0,260 (non significatif) |

Ce résultat n'a été battu par aucune des configurations alternatives
testées au cours de ce travail (variantes de critères, combinaisons de
méthodes, correctifs ciblés) — retenu comme résultat final.
