# Extraction IPAI — Indice des Prix des Actifs Immobiliers (BAM/ANCFCC)

Source : 72 bulletins trimestriels PDF fournis dans le dossier STAGE
(publications Bank Al-Maghrib / ANCFCC).

## Fichiers produits

- `ipai_variations_propre.csv` — format long, une ligne par (trimestre,
  catégorie, indicateur). Séparateur `;`, encodage UTF-8 BOM.
- `ipai_variations_large.xlsx` — format large (pivot), une ligne par
  trimestre, une colonne par catégorie × indicateur × type de variation.
  Fichier à utiliser directement pour une équation de passerelle.

## Couverture finale

**100% des trimestres théoriques couverts : 64 sur 64, de T1-2010 à
T1-2026, sans aucun trou.**

Répartition par niveau de confiance :
- **490 lignes "haute"** — extraites d'un tableau structuré présent dans
  le bulletin (deux formats de tableau reconnus : "Variation (en %)" et
  "Variation (%)", ce dernier utilisé dans les bulletins les plus anciens
  avec des libellés de catégories différents : "National" pour Global,
  "Appartements/Maisons/Villas" au pluriel, "Commercial" pour
  Professionnel — tous réconciliés vers une nomenclature commune).
- **24 lignes "basse"** — extraites par expression régulière d'un texte
  narratif (bulletins anciens sans tableau), limitées à l'indicateur
  Global.
- **8 lignes "haute (lecture manuelle vérifiée)"** — pour les 5 bulletins
  qui résistaient à toute extraction automatique fiable (texte purement
  narratif, chiffre absent ou ambigu pour l'indicateur Global), les
  valeurs ont été lues et vérifiées manuellement dans le texte source,
  avec citation exacte de la phrase d'origine (voir détail plus bas).
- **1 ligne "approximation"** — T1-2013, dont le texte source est
  purement qualitatif ("quasi-stagnation") sans aucun chiffre précis ; la
  valeur 0,0% est une approximation conventionnelle de cette formulation,
  pas une donnée observée. À td'utiliser avec prudence si cette
  observation pèse dans une estimation.

## Ce qui est extrait

Pour chaque catégorie de bien (Global, Résidentiel, Appartement, Maison,
Villa, Foncier, Professionnel, Local commercial, Bureau) et pour deux
indicateurs (prix, transactions) : la **variation trimestrielle** (T/T-1)
et la **variation annuelle** (T/T-4), en pourcentage. Ce sont des taux de
croissance, directement utilisables dans une équation de passerelle
(Δlog), sans reconstruction de niveau d'indice.

**Réserve à noter :** dans les bulletins antérieurs à ~2012, la
décomposition par catégorie se limite au résidentiel (Appartement,
Maison, Villa) — les catégories Foncier et Professionnel n'existaient pas
encore dans la méthodologie BAM/ANCFCC de l'époque. La colonne "Global"
pour ces trimestres reflète donc l'ensemble résidentiel, pas l'indice
global tel que défini aujourd'hui (résidentiel + foncier + professionnel).
Point à mentionner explicitement si cet historique long est utilisé dans
un modèle.

## Détail des 5 cas résolus manuellement

| Trimestre | Fichier source | Citation exacte | Valeur retenue |
|---|---|---|---|
| T3-2016 | DERI-IPAI T3 2016 .pdf | Tableau complet retrouvé après correction du script (variante d'en-tête "Variation (%)" non reconnue initialement) | Tableau complet, 18 lignes |
| T1-2011 | DERI-IPAI-2011 Q1.pdf | Tableau complet retrouvé (catégories "National"/"Appartements" etc.) | Tableau complet, 8 lignes |
| T1-2014 | DERI-IPAI T1 2014.pdf | "ont stagné au T1-2014" (trim.) ; "l'IPAI s'est légèrement accru de 0,1%" (annuel) ; transactions : "diminution de 2,6%" / "hausse de 10,1%" | trim=0,0 / ann=0,1 ; transactions trim=-2,6 / ann=10,1 |
| T2-2010 | DERI-IPAI-2010 Q2.pdf | "D'un trimestre à l'autre... augmenté de 2,2%" (trim.) ; "en hausse de 1,4% au 2ème trimestre 2010" (glissement annuel) ; transactions : "baisse de 5,7% d'un trimestre à l'autre" | trim=2,2 / ann=1,4 ; transactions trim=-5,7 / ann=non disponible |
| T2-2011 | DERI-IPAI-2011 Q2.pdf | "baisse trimestrielle de 1,6%" ; "progression de 1,9%" (annuel) ; transactions : "régressé de 17,5% d'un trimestre à l'autre" | trim=-1,6 / ann=1,9 ; transactions trim=-17,5 / ann=non disponible |
| T4-2010 | DERI-IPAI-2010 Q4.pdf | "ont baissé de 2% d'un trimestre à l'autre" ; "en baisse de 0,9%" (glissement annuel) — correction d'un bug de signe de l'extraction automatique initiale | trim=-2,0 / ann=-0,9 |
| T1-2013 | DERIIPAIT12013.pdf | "quasi-stagnation" (texte purement qualitatif, aucun chiffre précis pour le Global, ni trimestriel ni annuel) | trim=0,0 / ann=0,0 (approximation) |

## Méthode (résumé)

1. Détection du format de chaque bulletin (tableau structuré vs texte
   narratif) et extraction automatique correspondante.
2. Une première passe laissait 10 PDF sans extraction fiable. L'inspection
   individuelle de chacun a permis de distinguer : des cas où le script
   était trop strict (tableau ou chiffre présent mais non détecté à cause
   d'une variante de mise en page), et des cas où le bulletin ne donne
   réellement aucun chiffre précis pour l'indicateur Global.
3. Les cas de mise en page non reconnue ont été corrigés dans le script
   (élargissement de la détection de tableaux, réconciliation des noms de
   catégories entre époques).
4. Les derniers cas irréductibles à l'automatisation (texte purement
   narratif) ont été lus et complétés manuellement, avec citation exacte
   de la phrase source pour traçabilité.

Deux corrections de qualité ont été faites lors de cette dernière passe :
un bug de signe sur T4-2010 (le mot "baisse" n'avait pas été traduit en
valeur négative) et un chiffre suspect sur T1-2013 (une valeur "6,7"
capturée par erreur, ne correspondant à aucune phrase du texte source,
remplacée par une approximation explicite).
