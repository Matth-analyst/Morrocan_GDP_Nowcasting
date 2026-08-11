# Scripts de sélection des séries — GDPNow-Maroc

Ces trois scripts reproduisent exactement la sélection rigoureuse des 41 séries
utilisée dans l'étude (document « Critères et état des lieux » et classeur
`Series_retenues_modelisation.xlsx`).

## Ordre d'exécution

```bash
pip install openpyxl numpy pandas

python3 1_extraction_donnees_brutes.py
python3 1b_extraction_branches_sans_indicateur.py
python3 2_application_criteres.py
```

## Ce que fait chaque script

| Script | Rôle | Entrée | Sortie |
|---|---|---|---|
| `1_extraction_donnees_brutes.py` | Extrait cible + indicateurs bruts des 12 branches principales | `Etude_sectorielle_Maroc_2_complete.xlsx` | `parsed_branches_v2.pkl` |
| `1b_extraction_branches_sans_indicateur.py` | Extrait la cible des 4 branches ajoutées après coup (sans indicateur) | `Etude_sectorielle_Maroc_2_complete.xlsx` | `extra_branches.pkl` |
| `2_application_criteres.py` | Applique la cascade de 8 critères et produit la liste finale | `parsed_branches_v2.pkl` | `criteria_results.pkl` |

## Avant de lancer

Modifier la constante `SRC` en tête des scripts 1 et 1b pour pointer vers
l'emplacement réel du classeur source sur ta machine.

## Pour retester d'autres seuils

Tous les seuils du script 2 sont regroupés en constantes en haut du fichier
(`SEUIL_LONGUEUR_MIN`, `SEUIL_FRAICHEUR_MENSUEL_JOURS`, etc.) — les modifier
directement là plutôt que dans le corps du code, pour garder une trace claire
de ce qui a été testé.

## Limites connues à garder en tête

- Le critère 6 (cohérence du signe économique) n'est pas un filtre automatique
  — seulement calculé et conservé dans le résultat (`r` peut être négatif).
  7 des 41 séries retenues ont un signe contre-intuitif à examiner avant
  intégration définitive dans le modèle (voir `annexe_verification_series.md`).
- La correspondance entre une série et son « institution productrice » réelle
  (utilisée dans le classeur final) provient d'une table séparée (feuille
  Métadonnées du classeur source), pas de ces trois scripts.
