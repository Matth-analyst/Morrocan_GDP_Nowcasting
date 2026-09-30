# GDPNow-Maroc — Méthode 1

**Nowcasting de la croissance trimestrielle de la valeur ajoutée marocaine.** BVAR trimestriel, équations passerelles sur indicateurs infra-trimestriels, agrégation par indice de Laspeyres.

*Version 3 — 15 septembre 2026. Ce fichier remplace la version du 23 août 2026, antérieure au plan de correction méthodologique : les chiffres qu'elle annonçait provenaient d'un protocole qui laissait fuiter de l'information postérieure à la date de prévision.*

- **Méthodologie** : `NOTE_METHODOLOGIQUE.md`
- **Résultats détaillés** : `report/rapport_synthese.html`
- **Plan de correction appliqué** : `PLAN_CORRECTIONS_NOWCASTING.md`

---

## Résultat courant

| | |
|---|---|
| Nowcast T2-2026 (2 mois observés, scénario M2) | **+1,72 %** |
| Intervalle, couverture mesurée 77 % | [+0,12 ; +2,91] |
| Ratio sur 48 trimestres de backtest | 0,852 |
| Corrélation prévu / réalisé | 0,77 |
| BVAR seul, même agrégat | 0,961 |

Le **ratio** rapporte l'erreur à l'écart-type de la série : sous 1, le modèle fait mieux que prédire la moyenne historique.

Deux réserves à ne pas omettre en citant ces chiffres. **Hors 2020, le ratio remonte à 0,955** : l'essentiel de la performance vient de la capacité à voir la crise arriver. Et **aucun écart n'est statistiquement significatif** — 48 trimestres ne suffisent pas à établir un avantage concentré sur quatre d'entre eux.

---

## Installation

R ≥ 4.3. Aucun package hors CRAN, aucun package de nowcasting spécialisé : tout le bayésien et tout le Kalman sont écrits dans `R/fonctions/`.

```r
install.packages(c("readxl", "dplyr", "tidyr", "tibble",
                   "purrr", "stringr", "lubridate", "ggplot2"))
```

Trois fichiers source sont lus dans `../SourceData/`, à la racine du dépôt, et ne
sont **jamais modifiés** par le pipeline :

- `GDPNow_Maroc_series_retenues_Apres_Tris_economiques.xlsx`, le vivier d'indicateurs ;
- `VA_reelle_par_branche.xlsx`, la cible, valeur ajoutée en volume par branche ;
- `VA_nominale_base2014.csv`, la valeur ajoutée en prix courants, pour les poids d'agrégation.

Ils sont partagés avec la méthode 2 : un seul exemplaire, donc aucune occasion de
divergence silencieuse entre les deux chaînes.

---

## Exécution

```bash
Rscript run_pipeline.R
```

Compter **plus de six heures**. Les phases longues écrivent des points de reprise par tâche : une relance relit ce qui existe au lieu de le refaire. Un script isolé se lance de la même façon, à condition que ses dépendances aient été produites.

L'ordre de `run_pipeline.R` suit les **dépendances**, non la numérotation — d'où les trois scripts dont le numéro paraît déplacé (`06a` avant `03e`, `08a` avant `12c`). Les raisons sont en commentaire dans le fichier et dans l'en-tête des scripts concernés : chacun rompt un cycle où deux étapes s'attendaient l'une l'autre.

| Script | Rôle | Durée |
|---|---|---|
| `01_import_donnees.R` | import, datation fin de période, dé-cumul, CSV par branche | ~1 min |
| `02_analyse_exploratoire.R` | transformations, stationnarité | ~1 min |
| `02b_rapport_phase2.R` | rapport de la phase 2 | ~1 min |
| `03_bvar_trimestriel.R` | BVAR récursif, prior de Minnesota | ~40 min |
| `03c_selection_conjointe.R` | sélection jointe des hyperparamètres (écartée) | ~30 min |
| `03d_bvar_etendu.R` | BVAR rejoué depuis 2008 | < 1 min |
| `06a_poids.R` | poids en prix courants — `03e` en a besoin | < 1 min |
| `03e_branches_instables.R` | règle de rupture de variance | < 1 min |
| `03b_rapport_phase3.R` | rapport de la phase 3 | ~2 min |
| `04_bridge_equations.R` | agrégation mensuelle, sélection, passerelles, δ | ~5 min |
| `04b_variantes_passerelle.R` | six pistes d'amélioration | ~10 min |
| `04b2_ridge_corrige.R` | ridge rejouée après correction de la GCV | ~5 min |
| `04c0_validation_kalman.R` | trous artificiels : le Kalman vaut-il son coût | ~2 min |
| `04c_kalman_trous.R` | comblement par Kalman (écarté) | ~25 min |
| `04d_combinaison_etat.R` | δ dépendant de l'état (écarté) | ~5 min |
| `09_test_affinement_intra_trimestre.R` | M0/M1/M2/M3 | ~2 h 30 |
| `09c_seuil_trimestriel.R` | seuil renforcé sur les indicateurs trimestriels | ~20 min |
| `09d_selection_conditionnelle.R` | sélection conditionnelle contre marginale | ~25 min |
| `09e_test_m3_mensuel.R` | l'anomalie M2 > M3 : les deux variantes de M3 | ~15 min |
| `05_ar4_non_couvertes.R` | huit modèles comparés sur les quatre branches sans indicateur — étude, ne nourrit aucune étape | < 1 min |
| `06_agregation_fisher.R` | indice de Laspeyres, évaluation de l'agrégat | < 1 min |
| `09b_backtest_etendu.R` | backtest étendu à 2008, quatre branches | ~1 h |
| `10_incertitude.R` | Diebold-Mariano, bootstrap par blocs | ~5 min |
| `11_sensibilite_delais.R` | sensibilité aux délais de publication | ~2 min |
| `12_incertitude_parametrique.R` | loi prédictive du BVAR, calibration | ~3 min |
| `12b_recalibration.R` | recalibration conforme | < 1 min |
| `08a_nowcast_courant.R` | le chiffre publié — `12c` en a besoin | ~2 min |
| `12c_intervalle_combinaison.R` | intervalle autour du chiffre publié | < 1 min |
| `07_validation_pseudo_temps_reel.R` | fonction centrale et contrôles anti-look-ahead | ~2 min |
| `13_benchmarks.R` | six étalons réestimés récursivement | ~15 min |
| `14_robustesse.R` | δ, fenêtre, sélection, saisonnalité | ~40 min |
| `15_diagnostics.R` | contributions, résidus, épisodes, stabilité | ~2 min |
| `16_direct_indirect.R` | voie directe contre voie indirecte | ~20 min |
| `04e_rapport_phases4_13.R` | rapport des phases 4 et 13 | ~2 min |
| `08_rapport_synthese.R` | rapport de synthèse | ~2 min |

---

## Vérifier le pipeline

Deux outils, à lancer depuis la racine.

```bash
python outils/audit_dependances.py
```

Lit les `lire_csv()` / `ecrire_csv()` de chaque script et vérifie qu'aucun ne lit un fichier que personne ne produit, que l'ordre de `run_pipeline.R` respecte les dépendances, et qu'aucun script n'est oublié. Écrit après qu'un run depuis un `resultats/` vide a révélé deux dépendances circulaires et cinq scripts jamais intégrés — des défauts qu'un dossier déjà peuplé masque entièrement.

```bash
python outils/comparer_sorties.py _sauvegarde_AAAAMMJJ_HHMM
```

Compare les sorties à une sauvegarde en séparant les écarts réels du bruit de virgule flottante (seuil 10⁻⁹ en relatif). À utiliser après toute modification de structure, pour vérifier qu'elle n'a rien changé aux chiffres.

`reprendre_pipeline.R <script>` relance à partir d'une étape donnée, quand une exécution s'est arrêtée en cours de route.

---

## Structure

```
├── R/
│   ├── 00_setup.R              conventions, nomenclature des 16 branches
│   ├── fonctions/              le cœur du projet
│   │   ├── bvar.R              prior de Minnesota, observations fictives, postérieur
│   │   ├── posterior.R         tirages normal-inverse-Wishart
│   │   ├── passerelle.R        agrégation, sélection, passerelles, δ, garde d'amplitude
│   │   ├── kalman.R            modèle structurel, lissage et filtre
│   │   ├── nowcast.R           backtest_nowcast() — la fonction centrale
│   │   ├── information_set.R   ensemble d'information à une date
│   │   ├── branches_instables.R  règle de rupture de variance
│   │   ├── donnees.R           lecture et écriture CSV
│   │   ├── transformations.R   stationnarisation, standardisation récursive
│   │   └── rapport.R           composition HTML des rapports
│   └── NN_*.R                  les étapes du pipeline
├── data/                       CSV par branche + métadonnées du vivier
├── resultats/                  toutes les sorties chiffrées
├── figures/                    les figures des rapports
├── report/                     les quatre rapports HTML
├── app/                        l'application de production (Shiny)
│   ├── app.R                   interface et logique, neuf onglets
│   ├── R/fonctions_app.R       lecture des sorties, fusion des ajouts, production
│   ├── lancer.R                point d'entrée, avec vérification préalable
│   ├── donnees_ajoutees/       les observations saisies dans l'interface
│   └── LISEZMOI.md             mode d'emploi
└── run_pipeline.R
```

**Conventions du projet**, respectées partout :

1. Les classeurs source ne sont jamais modifiés.
2. Toute observation est datée au **dernier jour** de sa période. Les classeurs datent au premier jour ; la conversion se fait à la lecture.
3. Les échanges entre étapes se font par **CSV uniquement** — pas de `.rds`.

---

## Application de production

```bash
Rscript ../NowcastApp/lancer.R
```

Interface Shiny sur `http://127.0.0.1:4321`, bâtie sur le patron de
l'application du travail précédent (`R_gdpnow_maroc/shiny_app`) et complétée de
tout ce que le projet a produit depuis. Dix onglets : tableau de bord,
exploration des séries, détail des modèles, incertitude, validation, mise à jour
des données, export, sources, rapport, méthodologie. Mode clair et mode sombre.

L'onglet **Mise à jour des données** permet d'ajouter une observation — un mois
d'indicateur, un trimestre de valeur ajoutée en volume, ou la valeur ajoutée
nominale dont se déduisent les poids — puis de relancer la chaîne. Une mise à
jour trimestrielle complète se fait donc sans quitter l'application. Le classeur
n'est jamais touché : les saisies vont dans `NowcastApp/donnees_ajoutees/` et sont
fusionnées à la lecture. Chaque valeur est contrôlée avant enregistrement, et un
panneau compare la production précédente à la nouvelle, branche par branche.

Le recalcul s'exécute dans un **processus détaché** : l'application reste
utilisable pendant la minute et demie que dure le calcul, et le calcul survit à
la fermeture de la fenêtre. Il passe par `backtest_nowcast()` — la même fonction
que le backtest, donc le même chemin de code que celui qui a été évalué ; l'écart
entre les deux voies a été mesuré à 10⁻¹⁵. Six contrôles d'antériorité sont
mesurés sur ce calcul-là, **avant toute écriture**, dont un contrôle décisif par
perturbation des données postérieures à la cible. Le trimestre visé et le scénario ne se règlent pas :
ils se constatent. L'application n'écrase aucun fichier de la chaîne ; elle
archive ses propres productions, horodatées, dans `NowcastApp/productions/`, avec
un journal cumulatif. L'application vit d'ailleurs à côté de cette chaîne et non
dedans : elle lit `resultats/`, `data/`, `figures/` et `report/`, et n'écrit que
chez elle. Détail dans `NowcastApp/README.md`.

---

## La règle qui gouverne tout

À chaque origine `T`, **toute** quantité est recalculée sur la seule information disponible à cette date : hyperparamètres, sélection des indicateurs, coefficients, poids δ, poids d'agrégation, moyennes et écarts-types de standardisation.

La fonction `backtest_nowcast()` produit aussi bien le chiffre publié que chacun des 48 points du backtest : **le chiffre publié emprunte le chemin de code qui a été évalué**. Cinq contrôles bloquants refusent l'exécution si une donnée postérieure à `T` est touchée.

Corriger trois fuites d'information a dégradé les résultats affichés — c'est ce qui indique que la correction était juste. Le détail est en section 3 de la note méthodologique.

---

## Rapports

| Fichier | Contenu |
|---|---|
| `report/rapport_global.html` | **le rapport unique** — 30 sections, de la donnée brute au chiffre publié, avec les développements mathématiques, statistiques et économiques |
| `report/rapport_phase2.html` | données, transformations, stationnarité |
| `report/rapport_phase3.html` | le BVAR de bout en bout — 19 sections, 35 équations |
| `report/rapport_phases4_13.html` | passerelles, pistes d'amélioration, nowcasting intra-trimestriel |
| `report/rapport_synthese.html` | architecture, résultats, étalons, robustesse, diagnostics, limites — 14 sections |

---

## Ce que le système ne sait pas faire

À dire avant qu'on ne le demande :

- il n'apporte **rien sur les ralentissements graduels** (+0,2 % contre l'AR(2)) — les indicateurs sont coïncidents, ils enregistrent les chocs, pas les inflexions ;
- il est évalué contre des **comptes révisés**, que le modèle n'aurait pas eus en temps réel ;
- il suppose tout indicateur du mois *m* connu le **dernier jour de *m*** ;
- il n'est **jamais comparé au total officiel** du HCP, dont nous ne disposons pas ;
- ses intervalles restent **sous-couverts de 5 points** après recalibration, et ce déficit est structurel : il vient de l'incertitude sur les hyperparamètres.

Et un résultat qui vaut avertissement pour la suite : **l'incertitude paramétrique ne pèse que 12 % de la variance prédictive**. Le reste est l'aléa de choc. Mieux estimer ne resserrera pas l'intervalle — c'est ce qui explique rétrospectivement pourquoi les sept raffinements techniques tentés ont tous échoué.

---

## Suite

Trois données lèveraient l'essentiel de ces limites, et aucune ne se calcule : elles se demandent au HCP. La **valeur ajoutée totale trimestrielle en volume chaîné** — pour évaluer l'agrégat contre la grandeur publiée plutôt que contre l'indice reconstruit ici. Le **calendrier de diffusion** des comptes trimestriels — pour remplacer la convention de disponibilité immédiate par les délais réels. Les **millésimes successifs** — pour évaluer contre ce que le modèle aurait vraiment vu.
