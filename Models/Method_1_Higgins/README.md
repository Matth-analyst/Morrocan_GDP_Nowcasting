# GDPNow-Maroc — Dossier complet

**Voir en priorité : `00_RAPPORT_SYNTHESE_correction_et_optimisation.pdf`**

Ce document explique la dernière intervention sur ce dossier : une fuite de
données trouvée dans le BVAR (Étape 1, ne respectait pas la période
d'entraînement 2010-2021), sa correction, et le test (puis l'abandon
délibéré, pour cohérence) d'une fenêtre de démarrage différente pour le
BVAR seul.

## Décision finale

**Train = 2010-2021 partout, sans exception** (y compris le BVAR) ---
un gain marginal (2005-2021 pour le BVAR seul) a été mesuré (RMSFE 5,37 vs
5,43) puis délibérément écarté : l'asymétrie de fenêtre entre le BVAR et
le reste du pipeline (dont la sélection des indicateurs n'a jamais été
retestée sur une fenêtre différente) n'était pas justifiable pour un gain
aussi faible.

## Résultat final (honnête, sans fuite, cohérent de bout en bout)

- Test hors-échantillon (Étape 7) : **RMSFE = 5,43** (croissance annualisée),
  corrélation = 0,292, sur 20 trimestres jamais vus (2021T2-2026T1)
- Comparaison : Higgins (2014) obtient RMSFE = 1,15 sur son propre marché
  (États-Unis, 14 ans de vraies prévisions temps réel)

## Structure

```
Data/                                          <- Phase 2 : selection des indicateurs (2010-2021)
Step_1_BVAR_Value_Added/                       <- BVAR, 2010-2021 (corrige, fuite fermee)
Step_2_Common_Factors_Per_Branch_JaggedEdge/   <- facteurs + jagged edge
Step_3_Bridge_Equations/                       <- bridge equations (12 branches)
Step_4_AR_Non_Covered_branches/                <- AR, ordre p par AIC (7 branches)
Step_5_Combinaison_Bridge_BVAR/                <- delta par branche (RLS, borne)
Step_6_Ponderation_Aggregation/                <- poids nominaux + agregation PIB
Step_7_Test_Hors_Echantillon/                  <- le vrai test, 2021-2026
```

Chaque dossier contient son propre script R numéroté, son rapport LaTeX
(`.tex` + `.pdf`), ses `resultats/` et `figures/`. Exécuter dans l'ordre
1 → 7 pour reproduire l'ensemble depuis les données sources.
