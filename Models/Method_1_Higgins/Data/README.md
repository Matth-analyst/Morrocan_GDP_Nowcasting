# GDPNow-Maroc — Dossier de travail

Ce dossier rassemble l'ensemble du travail réalisé sur la **Phase 2** (sélection
des indicateurs, par branche) et la **reconstruction de l'IPC** (nécessaire à la
déflation des séries nominales). Chaque script Python est conservé comme
**empreinte reproductible** : n'importe qui disposant des mêmes données sources
peut relancer le pipeline dans l'ordre indiqué ci-dessous et retrouver les mêmes
résultats.

---

## Structure du dossier

```
GDPNow_Maroc/
├── README.md                                    <- ce fichier
│
├── classeurs/                                    <- LES DEUX VERSIONS DU CLASSEUR FINAL
│   ├── GDPNow_Maroc_classeur_complet_final_SANS_deflation.xlsx  <- avant deflation (7 blocs)
│   └── classeur_complet_avec_deflation.xlsx                      <- avec deflation (8 blocs, reference actuelle)
│
├── 01_Phase2_Selection_Indicateurs/
│   ├── scripts/                                  <- pipeline complet, a executer dans l'ordre
│   ├── rapports_justification_par_branche/        <- 1 rapport LaTeX par branche (filtre economique + qualite)
│   ├── rapports_synthese_niveau1_niveau2/         <- ancres (Niveau 1) + test statistique (Niveau 2)
│   └── archive_versions_intermediaires/           <- versions superseedees, gardees pour tracabilite
│
├── 02_IPC_Deflation/
│   ├── scripts/                                   <- reconstruction de l'IPC 2010-2026
│   ├── donnees/                                    <- serie IPC finale (csv + pkl)
│   └── rapport_ipc_reconstruction.tex              <- toutes les formules, pour reproduction fidele
│
├── 03_Deflation_Series_Nominales/
│   ├── scripts/                                   <- deflation des 58 candidats nominaux + retest
│   └── rapport_deflation_retest.tex                <- resultats complets, avec formules
│
├── 04_Denton_Series_Annuelles/
│   ├── scripts/                                    <- interpolation Denton, cas assurance
│   └── rapport_denton_assurance.tex                 <- methode + resultat (negatif, documente)
│
├── 05_Stock_Watson_Facteur_Commun/
│   ├── scripts/                                    <- facteur EM, cas Agriculture
│   └── rapport_stock_watson_agriculture.tex          <- methode + resultat (negatif, concluant)
│
└── 06_Substitution_Proxy/
    └── rapport_substitution_conjoncture.tex          <- diagnostic requalifie (pas de substitution necessaire)
```

**Règle suivie dans tout ce dossier** : aucune ancienne version de classeur n'est jamais supprimée --- chaque
modification produit un nouveau fichier, l'ancien reste accessible (dans
`classeurs/` pour les deux versions principales, dans
`archive_versions_intermediaires/` pour les étapes de construction plus
anciennes).

---

## 1. Phase 2 — Sélection des indicateurs

### Principe général (voir aussi les rapports LaTeX de chaque branche)

Pour chacune des 16 branches, la procédure est la même, en 3 filtres successifs :
1. **Filtre économique a priori** — un indicateur n'est candidat que s'il a un lien
   mécanique identifiable avec l'activité de la branche, jugé *avant* tout calcul.
2. **Filtre de qualité** — longueur minimale (12 observations), densité (≥80%,
   sauf IPAI pour Immobilier : seuil dédié à 65%, voir le rapport correspondant).
3. **Sélection à deux niveaux** :
   - **Niveau 1 (ancres)** — 1 à 3 candidats par branche désignés *a priori* comme
     les plus directement représentatifs de l'activité (inspiré de la méthode de
     Higgins, 2014, qui ne teste jamais ses propres choix). **Jamais testés
     statistiquement, toujours conservés.**
   - **Niveau 2 (exploratoires)** — tous les autres candidats retenus, testés par
     corrélation (Student, sur la période d'entraînement T1-2010 à T1-2021) avec
     correction FDR (Benjamini-Hochberg) **par branche**.

### Ordre d'exécution des scripts (`scripts/`)

| # | Script | Rôle |
|---|--------|------|
| 01–12 | `0X_extraction_filtre_<branche>.py` | Extraction brute + filtre économique + filtre de qualité, une branche par script |
| 13 | `13_definition_ancres_niveau1_et_classeur.py` | Désigne les ancres (Niveau 1) pour les 11 branches "standard" et construit un premier classeur |
| 14 | `14_test_statistique_niveau2_exploratoires_avec_FDR.py` | Test + FDR sur les candidats non-ancres, 11 branches standard |
| 15 | `15_test_statistique_niveau2_immobilier.py` | Même test, cas particulier Immobilier (credit + IPAI) |
| 16 | `16_construire_classeur_final_7blocs.py` | Classeur final : CIBLE / ANCRES-Trim / ANCRES-Mens / SÉLECTION N2-Trim / SÉLECTION N2-Mens / NON SÉLECTIONNÉ-Trim / NON SÉLECTIONNÉ-Mens |

**Prérequis** : chaque script d'extraction (01–12) attend le classeur source
`Etude_sectorielle_Maroc_2_complete.xlsx` au même niveau, et écrit ses résultats
intermédiaires (`.pkl`) dans des sous-dossiers `data/` et `resultats/` qu'il crée
lui-même. Les scripts 13 à 16 relisent ces `.pkl` — il faut donc avoir exécuté
01–12 avant 13, et 13 avant 14/15/16.

### Résultat final

`classeurs/GDPNow_Maroc_classeur_complet_final.xlsx` — 434 séries indicateurs,
réparties en :
- 24 ancres (jamais testées)
- 16 sélectionnées au Niveau 2 (test + FDR)
- 394 non retenues (conservées dans le classeur, pour traçabilité et
  réexploration future)

### `archive_versions_intermediaires/`

Contient les versions de classeur et de scripts **superseedées** au fil du
travail (ex. la première version à 1 ancre/branche avant la correction à 2
ancres, ou une version où les ancres étaient encore testées par erreur). Gardées
pour comprendre l'historique des décisions, **ne pas les utiliser comme référence
courante** — c'est toujours `classeurs/GDPNow_Maroc_classeur_complet_final.xlsx`
qui fait foi.

---

## 2. Reconstruction de l'IPC (`02_IPC_Deflation/`)

### Pourquoi

Plusieurs indicateurs retenus (crédit bancaire, recettes touristiques, M3,
primes d'assurance) sont en dirhams **courants** (nominaux), alors que la cible
de chaque branche (VA) est toujours en volume **réel**. Il faut déflater ces
séries avant de les tester ou de les utiliser — voir `rapport_ipc_reconstruction.tex`
pour la justification complète et toutes les formules.

### Méthode, en un coup d'œil

Aucune série IPC officielle unique ne couvre 2010–2026 en mensuel. Le script
raccorde deux bases HCP :
- **2010–2016** : interpolation Denton simplifiée depuis des moyennes annuelles
  (base 2006), contrainte pour que chaque moyenne mensuelle interpolée
  reproduise exactement la moyenne annuelle officielle.
- **2017–2026** : série mensuelle officielle (base 2017), utilisée telle quelle.
- **Raccordement** : coefficient calculé sur les 12 mois de chevauchement (2017),
  où les deux bases sont connues simultanément — vérifié stable (écart-type des
  12 ratios : 0,0022) avant d'être appliqué.

### Exécution

```bash
cd 02_IPC_Deflation/scripts
# placer le fichier HCP "IPC 2017 par grandes divisions, Mensuel" dans :
#   donnees_source/ipc_mensuel_base2017.xlsx
python3 01_reconstruction_ipc_2010_2026.py
```

Produit `ipc_maroc_2010_2026_raccorde.csv` (199 points, janvier 2010 à juillet
2026) — déjà présent dans `donnees/` pour référence immédiate, sans ré-exécution
nécessaire.

**Seule approximation non directement sourcée** : la moyenne annuelle 2016
(interpolée entre 2015 et 2017, faute de publication HCP retrouvée) — signalée
explicitement dans le script et dans le rapport LaTeX.

### Ce qui reste à faire (prochaine étape)

Appliquer cette série pour déflater les indicateurs nominaux déjà retenus, puis
les retester avec la même procédure Niveau 2 (script 14/15) — **fait, voir
`03_Deflation_Series_Nominales/`**.

---

## 3. Déflation des séries nominales (`03_Deflation_Series_Nominales/`)

### Ce qui a été fait

Les 58 candidats nominaux identifiés dans 11 branches (crédit bancaire, comptes
débiteurs, crédits à l'équipement, M3, recettes touristiques, primes
d'assurance, commerce extérieur) sont déflatés avec l'IPC reconstruit
(`02_IPC_Deflation/donnees/ipc_maroc_2010_2026_raccorde.csv`), puis retestés
avec la même procédure que le Niveau 2 (Student + FDR par branche, ancres
jamais retestées).

### Résultat

**5 séries, rejetées sous leur forme nominale, sont retenues une fois
déflatées** — dont 2 pour Électricité, gaz, eau, qui ne disposait jusqu'ici
d'aucune série au-delà de ses 2 ancres. Détail complet dans
`rapport_deflation_retest.tex`.

### Ordre d'exécution

```bash
cd 03_Deflation_Series_Nominales/scripts
python3 01_deflation_et_retest.py        # deflation + retest, produit resultats_deflation.pkl
python3 02_maj_classeur_avec_deflation.py # ajoute le bloc "SELECTION APRES DEFLATION" au classeur
```

Le classeur mis à jour (`classeur_complet_avec_deflation.xlsx`, dans
`01_Phase2_Selection_Indicateurs/classeurs/`) contient désormais **8 blocs**
par feuille : CIBLE, ANCRES (Trim/Mens), SÉLECTION N2 (Trim/Mens),
**SÉLECTION APRÈS DÉFLATION** (nouveau, sarcelle — uniquement pour les 3
branches concernées), NON SÉLECTIONNÉ (Trim/Mens). Les versions nominales des
5 séries récupérées restent visibles dans « Non sélectionné », par
transparence (434 candidats d'origine + 5 versions déflatées ajoutées = 439
séries au total dans le classeur).

### Ce qu'il reste à faire ensuite

Les autres techniques de gestion des valeurs manquantes identifiées chez
Higgins (2014) :

| Technique | Sur quoi | Statut |
|---|---|---|
| Déflation | Séries nominales | ✅ Fait (dossier 03) |
| Denton (basse→haute fréquence) | Séries annuelles écartées | ⚠️ Un cas traité (dossier 04), résultat négatif |
| Raccordement (splice) | Ciment régional (Construction) | ❌ Abandonné (diagnostic changé — voir dossier 04) |
| Stock & Watson (démarrage tardif) | Précipitations/Température (Agriculture) | ✅ Fait (dossier 05), résultat négatif mais concluant |
| Substitution par proxy | Enquête de conjoncture | ✅ Fait (dossier 06) — diagnostic requalifié |

**Les 6 techniques de gestion des valeurs manquantes identifiées chez Higgins
(2014) ont maintenant toutes été traitées** (appliquées avec succès, testées
avec un résultat négatif honnêtement documenté, ou requalifiées après
vérification) — voir chaque dossier pour le détail.

---

## 6. Substitution par proxy (`06_Substitution_Proxy/`)

### Ce qui a été fait

Vérification de la piste envisagée pour l'enquête de conjoncture dans
l'industrie (Bank Al-Maghrib), dont notre base s'arrêtait en octobre 2024.

### Résultat --- un diagnostic requalifié, pas une substitution

**L'enquête elle-même n'a jamais cessé d'être publiée** (confirmé active
jusqu'en janvier 2026) --- c'est notre propre collecte de données qui s'est
arrêtée, pas la source. Aucune substitution par proxy n'est donc nécessaire.

**Ce qui bloque la correction complète** : contrairement aux bulletins IPAI
(PDF, extraits avec succès), le site `bkam.ma` bloque les requêtes
automatisées (protection anti-robot) --- empêchant l'extraction directe des
valeurs manquantes (novembre 2024 à aujourd'hui). Action recommandée mais non
réalisée : téléchargement manuel des bulletins récents, puis extraction par
la même méthode que celle utilisée pour l'IPAI.

### Mise à jour --- 213 bulletins fournis par l'utilisateur, extraction tentée

L'utilisateur a fourni directement 213 bulletins BAM (2009--2026), contournant
le blocage anti-robot. Extraction menée via `pdfplumber` (vraie structure de
tableau) :

- **95 bulletins extraits avec succès** (2014--2026), comblant 15 des 19 mois
  du trou d'origine.
- **57 séries par branche** (5 branches × 12 questions) correctement extraites.
- **Un bug non résolu** limite l'extraction du tableau global à 7 questions
  (Production/Commandes/Ventes/Prix) --- ne fonctionne que pour 1 bulletin sur
  95. La série de prix globale, la plus intéressante car jamais testée avant,
  reste donc inexploitée.
- **117 bulletins non extraits** : encodage de noms de fichiers corrompu
  (~15 fichiers) ou mise en page sans bordures de tableau détectables
  (bulletins ~2016--2018).

Détail complet, avec toutes les limites honnêtement documentées, dans
`rapport_extraction_bkam.tex`.

---

## 5. Facteur commun par Stock & Watson (`05_Stock_Watson_Facteur_Commun/`)

### Ce qui a été fait

Un facteur commun extrait par algorithme EM itératif sur les 5 candidats de la
branche Agriculture (crédit bancaire, démarrant 2006 ; précipitations et
température, démarrant 2020) — la même technique que Higgins (2014) cite pour
son propre facteur à 124 séries (ISM Nonmanufacturing, disponible seulement
depuis juillet 1997). Formules complètes (standardisation, PCA par SVD,
ré-estimation des valeurs manquantes, convergence) dans
`rapport_stock_watson_agriculture.tex`.

### Résultat

**Négatif, mais concluant** : le facteur, désormais testable sur toute la
période d'entraînement (n=45, contre n=4 pour les précipitations seules avant
cette méthode), échoue tout de même au test statistique ($r=0{,}084$,
$p=0{,}58$). **Ce résultat permet de trancher une ambiguïté qu'on ne pouvait
pas lever avant** : l'échec d'Agriculture n'est pas dû à un manque de recul
temporel — c'est un vrai manque de lien statistique, confirmé sur un
échantillon complet cette fois.

### Conclusion pour Agriculture

Après quatre approches indépendantes (test direct, déflation implicite via
le crédit, tentative Denton écartée par raisonnement, et Stock & Watson),
**Agriculture reste en AR(4) pur** — un constat robuste, pas issu d'un seul
test insuffisant.

---

## 4. Interpolation de Denton (`04_Denton_Series_Annuelles/`)

### Ce qui a été fait

Un cas test complet : **primes d'assurance vie et non-vie** (Finances,
annuelles 1976–2024), interpolées en trimestriel via le profil appris sur
« Primes versées » (trimestrielle, 2016–2024, déjà candidat retenu) —
formules complètes et contrainte de conservation exacte dans
`rapport_denton_assurance.tex`.

### Résultat

**Négatif, documenté honnêtement** : les deux séries interpolées ($r=-0{,}073$
et $-0{,}081$) échouent au test statistique, malgré une interpolation
techniquement correcte (contrainte Denton vérifiée exacte à $10^{-6}$ près).

### Un cas abandonné avant calcul, documenté aussi

La **production céréalière** (Agriculture) avait été envisagée en premier,
avec les précipitations comme série liée — abandonnée avant tout calcul :
une récolte est un événement saisonnier concentré (mai-juillet), pas un flux
continu ; la répartir proportionnellement aux précipitations mensuelles
produirait une « production » non nulle en plein hiver, un résultat
économiquement absurde. Ce raisonnement est détaillé dans le rapport.

### Sur le raccordement du ciment (technique abandonnée)

En creusant la table source brute (`ventes de ciment (1).xlsx`, feuille
« consommation régionale »), la vraie structure s'est révélée différente de
ce qui était supposé : les 16 anciennes et 12 nouvelles régions **partagent
les mêmes colonnes de dates** (ce ne sont pas deux périodes chronologiques
séquentielles à raccorder, mais deux classifications parallèles du même
total national). Le nouveau découpage est simplement **incomplet** dans
cette source (données seulement jusqu'en 2004), pas "plus récent". Un vrai
raccordement chronologique n'a donc pas de sens ici — le sujet est mis de
côté, faute d'une source plus actuelle identifiée pour le découpage à 12
régions.

### Portée non couverte

Ce travail ne couvre qu'un seul cas parmi plusieurs centaines de séries
annuelles identifiées dans la base (voir l'inventaire dans les logs
d'exécution) — l'extension systématique à d'autres branches reste à faire,
et demanderait d'identifier, pour chaque série annuelle, une série liée
pertinente déjà disponible à plus haute fréquence.

---

## Conventions générales du pipeline

- **Transformation** : tous les tests statistiques utilisent le taux de
  croissance $\Delta\log(x_t) = \log(x_t) - \log(x_{t-1})$, jamais les niveaux.
- **Période d'entraînement** : T1-2010 à T1-2021, fixée une fois pour toutes
  avant tout calcul (pas de re-détermination a posteriori).
- **Test statistique** : corrélation de Pearson + test de Student
  ($\alpha=0{,}10$, plancher $|r|\geq0{,}15$), avec correction FDR
  (Benjamini-Hochberg) pour tout groupe de candidats testés en concurrence.
- **Séries mensuelles testées contre une cible trimestrielle** : moyenne simple
  des 3 mois du trimestre avant calcul du $\Delta\log$.
