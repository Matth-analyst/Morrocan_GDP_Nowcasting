# Plan de modélisation — GDPNow-Maroc

## Cible de référence (commune à toutes les branches)

Sur les ~26 variantes de cible recensées par branche (base 2014, base 2007, contributions,
taux de croissance...), on retient **une seule série** :

> **VA par branche, base 2014 rétropolée sur 28 branches** (`PIB trimestriel_PIB base2014
> rétropolé 28 branc.csv`) — **T1-1998 à T1-2026, 113 observations**, disponible pour les
> 16 branches sans exception (y compris les 4 branches sans indicateur).

**Justification** : c'est la seule version disponible pour toutes les branches, avec
l'historique le plus long (contre 49 observations pour la version « base 2014 » simple,
2014-2026 seulement) — un facteur x2,3 sur la taille d'échantillon, décisif pour
l'estimation du BVAR. Les autres versions (base 2007, contributions...) sont conservées
uniquement comme test de robustesse a posteriori, jamais comme cible d'entraînement.

---

## Groupe A — Architecture complète (BVAR trimestriel + bridge equation mensuelle)

Branches disposant d'au moins un indicateur mensuel dense (>100 observations) et à jour
(2025-2026). C'est le seul groupe où la logique GDPNow d'origine s'applique fidèlement :
le nowcast peut s'affiner en cours de trimestre à mesure que les publications mensuelles
arrivent.

| Branche | Indicateurs principaux retenus | Fréquence / couverture | Justification |
|---|---|---|---|
| **Pêche** | Débarquements EN QUANTITÉ (total) ; débarquements par espèce (céphalopodes, poisson pélagique, poisson blanc, coquillages, crustacés) | Mensuelle, 2008M12–2026M05 (179 obs) | Indicateur de volume directement lié à l'activité de la branche ; désagrégation par espèce permet une bridge equation granulaire à la GDPNow (Table A5b du papier original) |
| **Industrie d'extraction** | Exportations phosphate + dérivés ; Exportations OCP ; Exportations autres extractions minières | Mensuelle, 2018–2026 (90 à 137 obs selon la série) | Remplace l'ancienne série "Phosphates" (arrêtée en 2016) par les séries OCP plus récentes, à jour jusqu'à mai 2026 |
| **Industrie de transformation** | TUC industrielle ; carnets de commandes ; évolution production/ventes (enquête de conjoncture BAM) | Mensuelle, 2010M01–2025M08 (178-188 obs) | Enquête qualitative BAM, référence classique pour ce type de branche (équivalent direct de l'ISM Manufacturing du papier GDPNow) |
| **Électricité, gaz, eau** | Énergie nette appelée ; production thermique, hydraulique ; nombre de distributeurs/clients BT | Mensuelle, 1994-1996 à 2026M05 (362-387 obs) | Très longue série, fraîche, granularité par source de production utile pour capter la composante "eau" faible par ailleurs |
| **Construction** | Total des régions (indicateur national agrégé) | Mensuelle, 1995M01–2026M05 (373 obs) | Seule série vraiment longue et à jour pour cette branche ; les déclinaisons régionales s'arrêtent en 2015, à ne garder qu'en complément qualitatif |
| **Commerce** | Importations (valeur) ; Flux IDE | Mensuelle, 1998-2009 à 2026M05 (202-330 obs) | Proxy de la demande de biens importés destinés à la distribution ; IDE comme signal d'investissement dans le secteur |
| **Transports** | Trafic aérien ; trafic ANP ; personnes morales/physiques (trafic portuaire) | Mensuelle, 2012-2018 à 2023-2026 (93 à 164 obs) | Couverture correcte mais hétérogène — le trafic ANP s'arrête en 2023, le trafic aérien reste à jour (avril 2026), à privilégier en cas d'arbitrage |
| **Hébergement-restauration** | Recettes touristiques (mensuel cumulé) ; arrivées de touristes ; arrivées de MRE | Mensuelle, 1994-1996–2026M05 (205 à 383 obs) | Série la plus longue et la plus dense de tout le classeur ; qualité GDPNow-like exemplaire |
| **Finances et assurances** | Crédits à caractère financier ; comptes débiteurs/trésorerie ; crédits à l'équipement ; OPCVM | Mensuelle, 2001M12–2026M05 (294 obs) | Panel bancaire dense (Bank Al-Maghrib), équivalent direct des statistiques monétaires du papier GDPNow |
| **Immobilier** | Crédits aux promoteurs immobiliers ; crédits à l'immobilier/habitat | Mensuelle, 2001-2017 à 2026M05 (104-294 obs) | Le volume de crédit précède l'activité de construction/vente ; IPAI (transactions, trimestriel) en complément pour le signal de volume de marché |

---

## Groupe B — Bridge equation trimestrielle uniquement (pas d'affinement mensuel)

| Branche | Indicateurs principaux retenus | Fréquence / couverture | Justification |
|---|---|---|---|
| **Information-communication** | Parc téléphonie mobile global ; parc par opérateur ; Internet ADSL ; Publiphones | Trimestrielle, 2002-2003–2026T1 (81 à 97 obs) | Aucune série mensuelle disponible dans le classeur (uniquement des publications trimestrielles ANRT) ; le nowcast pour cette branche ne pourra pas s'affiner en cours de trimestre, à documenter comme limite assumée |

**Remarque méthodologique** : contrairement au découpage envisagé précédemment dans la
conversation, la réévaluation des données montre que la majorité des branches qu'on
pensait « trimestrielles seulement » (Industrie d'extraction, Transports, Construction)
disposent en fait d'un vrai panel mensuel une fois qu'on regarde au-delà des séries
historiques interrompues. Seule l'Information-communication reste structurellement
bloquée en trimestriel.

---

## Groupe C — AR(4) pur (aucun indicateur d'activité disponible)

| Branche | Méthode | Variable cible | Justification |
|---|---|---|---|
| **Administration publique** | AR(4) sur la VA elle-même | VA base 2014 rétropolée, 1998T1-2026T1 | Aucun indicateur d'activité ; VA dominée par la masse salariale publique (pas de proxy mensuel identifié à ce stade — le domaine "Finances publiques" de Manar-Stat reste à explorer) |
| **Éducation-santé** | AR(4) sur la VA elle-même | VA base 2014 rétropolée, 1998T1-2026T1 | Idem — VA à composante non marchande dominante, un proxy budgétaire (masse salariale enseignants/santé) reste à construire séparément |
| **Services aux entreprises** | AR(4) sur la VA elle-même | VA base 2014 rétropolée, 1998T1-2026T1 | Idem — l'enquête HCP ETCE (services marchands non financiers) identifiée en amont comme piste, jamais collectée |
| **Autres services** | AR(4) sur la VA elle-même | VA base 2014 rétropolée, 1998T1-2026T1 | Branche résiduelle par nature, pas de proxy dédié pertinent à chercher |

---

## Cas particulier — Agriculture (module climatique dédié)

| Branche | Méthode | Variable cible | Indicateurs principaux | Justification |
|---|---|---|---|---|
| **Agriculture** | Régression climatique dédiée (pas une bridge equation classique) + AR(4) de repli | VA base 2014 rétropolée, 1998T1-2026T1 | Précipitations moyennes ; Température moyenne (mensuelles, 2020-2026, 76 obs) ; données "campagne agricole" (50 observations, fréquence propre à la campagne céréalière) | La VA agricole dépend d'un choc climatique largement exogène et déconnecté du cycle économique classique — logique déjà actée dans la conversation (étape 5 de l'architecture). Le crédit bancaire "Agriculture et pêche" (mensuel, 78 obs) sert de repère complémentaire mais capte surtout la branche pêche |

---

## Synthèse

| Groupe | Branches | Nb | Traitement |
|---|---|---|---|
| A | Pêche, Industrie d'extraction, Industrie de transformation, Électricité-gaz-eau, Construction, Commerce, Transports, Hébergement-restauration, Finances et assurances, Immobilier | 10 | BVAR + bridge equation mensuelle |
| B | Information-communication | 1 | BVAR + bridge equation trimestrielle |
| C | Administration publique, Éducation-santé, Services aux entreprises, Autres services | 4 | AR(4) pur |
| Spécial | Agriculture | 1 | Module climatique + AR(4) de repli |

**16 branches couvertes au total**, avec un socle mensuel solide pour 10 d'entre elles —
une base nettement plus favorable que ce qu'on anticipait au début de la collecte.

---

## Bilan du tri : combien de séries retenues, et pourquoi

Sur les **1233 séries recensées** dans la feuille Métadonnées du classeur (hors l'onglet
« PIB et impôts », traité à part), **669 sont retenues pour la modélisation, soit 54%**.

| | Nombre |
|---|---|
| Séries totales recensées | 1233 |
| — dont cibles (VA par branche, toutes versions/bases confondues) | 221 |
| — dont indicateurs | 1012 |
| **Cibles retenues** (1 par branche) | **16** |
| Cibles écartées | 205 |
| **Indicateurs retenus** (≥20 observations) | **653** |
| Indicateurs écartés (<20 observations) | 359 |
| **Total retenu** | **669** |

### Pourquoi 205 cibles sont écartées

Chaque branche dispose de plusieurs versions de sa VA trimestrielle : base 2014, base
2007, base 2014 « contribution », base 2014 « taux de croissance », rétropolé 14
branches, rétropolé 28 branches, etc. — jusqu'à **26 variantes pour une seule branche**
(Industrie de transformation). Ce ne sont pas 26 informations différentes mais **26
codages du même signal économique**, produits par des méthodologies ou des bases de
calcul différentes du HCP. Utiliser toutes ces variantes comme si elles étaient des
séries indépendantes reviendrait à démultiplier artificiellement le poids d'une seule
information réelle dans le modèle.

**Règle de sélection retenue** : une seule cible par branche — la version « base 2014
rétropolée sur 28 branches » quand elle existe (15 branches sur 16, historique jusqu'à
1998, 113 observations), sinon la version « base 2014 » simple (cas de l'Agriculture
uniquement, 49 observations, aucune version rétropolée disponible pour cette branche
dans le classeur). Les 205 autres variantes ne sont pas supprimées du classeur — elles
restent disponibles comme **test de robustesse a posteriori** (vérifier que les
résultats du modèle ne changent pas radicalement selon la base de PIB retenue), mais
elles n'entrent jamais dans l'estimation elle-même.

### Pourquoi 359 indicateurs sont écartés

Le critère retenu est un **seuil minimal de 20 observations**. En-dessous, une série est
trop courte pour calibrer un coefficient de régression de façon fiable, et totalement
inutilisable pour un test hors-échantillon digne de ce nom (qui nécessite déjà de
réserver une partie des observations pour la validation). Exemple concret : les séries
de pêche par espèce (poisson pélagique, céphalopodes, poisson blanc, coquillages,
crustacés) n'ont que 14 à 15 points chacune — largement en-dessous du seuil.

Le choix de 20 comme seuil est un compromis pragmatique, pas une règle statistique
absolue : il exclut les cas les plus manifestement inexploitables sans être trop
restrictif sur des séries encore courtes mais potentiellement utiles une fois complétées
par de futures publications. Ce seuil pourra être resserré (par exemple à 30-40
observations, pour permettre un vrai découpage estimation/validation) une fois les
premiers tests de corrélation effectués — voir la réserve ci-dessous.

### Une limite qui reste ouverte : aucun test de pertinence statistique

Le tri décrit ici ne filtre que sur la **forme** des données (redondance, longueur de
série) — pas sur leur **pouvoir prédictif réel**. Les 653 indicateurs retenus n'ont pas
encore été testés en corrélation avec la VA de leur branche (l'étape que Fernández
Cerezo, 2023, place en tout premier dans sa méthodologie). Il est probable qu'une fois
ce test effectué, une partie significative de ces 653 séries s'avère peu ou pas
corrélée à la VA correspondante et soit écartée à son tour. Le chiffre de 669 séries
retenues est donc un **plancher de départ pour l'estimation**, pas le jeu de données
final du modèle.
