# -*- coding: utf-8 -*-
"""
01_facteur_stock_watson_agriculture.py -- Extraction d'un facteur commun
sur les 5 candidats de la branche Agriculture (3 credit, demarrant en
2006 ; precipitations et temperature, demarrant en 2020) par la methode
de Stock & Watson (2002), citee par Higgins (2014) pour ce cas precis :
"a method for estimating principal components when some series are
missing at the beginning of the sample".

Principe (algorithme EM iteratif) :
1. Standardiser chaque serie (Delta-log, centree-reduite sur sa propre
   periode disponible).
2. Initialiser les valeurs manquantes (2006-2019 pour precipitations et
   temperature) a 0 (la moyenne, une fois standardise).
3. Extraire le premier facteur principal (PCA) du panel ainsi complete.
4. Ré-estimer les valeurs manquantes par regression de chaque serie sur
   le facteur estime.
5. Repeter 3-4 jusqu'a convergence (le facteur ne change plus).
"""
import pickle
import numpy as np
import pandas as pd
from datetime import date

with open('/home/claude/phase2/data/agriculture_brut.pkl','rb') as f:
    brut = pickle.load(f)

def get_serie(col):
    return dict(next(i for i in brut['indicateurs'] if i['colonne']==col)['serie'])

def log_diff_mensuel(serie):
    s = pd.Series(serie).sort_index()
    s.index = pd.to_datetime(s.index)
    s = s[s>0]
    return np.log(s).diff().dropna()

# --- Les 5 candidats, en Delta-log ------------------------------------------
series_brutes = {
    'Crédit bancaire': get_serie(7),
    'Comptes débiteurs': get_serie(8),
    "Crédits à l'équipement": get_serie(9),
    'Précipitations': get_serie(18),
    'Température': get_serie(19),
}

# Toutes trimestrialisees (credit est deja trimestriel ; precipitations/
# temperature sont mensuelles -> moyenne trimestrielle avant Delta-log,
# pour rester coherent avec le reste du pipeline)
def to_quarterly(serie):
    s = pd.Series(serie).sort_index()
    s.index = pd.to_datetime(s.index)
    q = s.resample("QS").mean().dropna()
    return q

series_ld = {}
for nom, s in series_brutes.items():
    sq = to_quarterly(s)
    ld = np.log(sq[sq>0]).diff().dropna()
    series_ld[nom] = ld

# --- Construction du panel complet (union de tous les trimestres) ----------
toutes_dates = sorted(set().union(*[s.index for s in series_ld.values()]))
panel = pd.DataFrame(index=toutes_dates, columns=series_ld.keys(), dtype=float)
for nom, s in series_ld.items():
    panel.loc[s.index, nom] = s.values

print(f"Panel complet : {panel.shape[0]} trimestres x {panel.shape[1]} series")
print(f"Periode : {panel.index.min()} a {panel.index.max()}")
print(f"\nValeurs manquantes par serie :\n{panel.isna().sum()}")

# --- Standardisation (moyenne 0, ecart-type 1, sur les valeurs disponibles) --
moyennes = panel.mean()
ecarts_types = panel.std()
panel_std = (panel - moyennes) / ecarts_types

# --- Algorithme EM ------------------------------------------------------------
panel_rempli = panel_std.fillna(0.0)  # initialisation
MAX_ITER = 50
TOLERANCE = 1e-6
facteur_precedent = None

for iteration in range(MAX_ITER):
    # PCA : facteur = 1ere composante principale (vecteur propre dominant de
    # la matrice de covariance), calculee par SVD pour la stabilite numerique
    X = panel_rempli.values
    X_centre = X - X.mean(axis=0)
    U, S, Vt = np.linalg.svd(X_centre, full_matrices=False)
    facteur = U[:, 0] * S[0]  # premiere composante principale
    charge = Vt[0, :]          # charges (loadings) de chaque serie sur le facteur

    # Normaliser le signe (convention : le facteur est positivement correle
    # au credit bancaire, la serie la plus longue et la plus fiable)
    if charge[0] < 0:
        facteur, charge = -facteur, -charge

    # Verifier la convergence
    if facteur_precedent is not None:
        delta = np.max(np.abs(facteur - facteur_precedent))
        if delta < TOLERANCE:
            print(f"\nConvergence atteinte a l'iteration {iteration} (delta={delta:.2e})")
            break
    facteur_precedent = facteur.copy()

    # Re-estimer les valeurs manquantes par regression sur le facteur
    for j, nom in enumerate(panel.columns):
        manquants = panel_std[nom].isna()
        if manquants.any():
            observes = ~manquants
            # regression simple : valeur = charge_j * facteur (par construction PCA)
            panel_rempli.loc[manquants, nom] = charge[j] * facteur[manquants.values]
        else:
            panel_rempli[nom] = panel_std[nom].where(observes if 'observes' in dir() else True, panel_rempli[nom])

print(f"\nFacteur extrait : {len(facteur)} trimestres, {panel.index.min()} a {panel.index.max()}")
print(f"Charges (loadings) de chaque serie sur le facteur :")
for nom, c in zip(panel.columns, charge):
    print(f"   {nom:25s} {c:+.3f}")

facteur_serie = [(d.date(), v) for d, v in zip(panel.index, facteur)]

with open('facteur_stock_watson_agriculture.pkl','wb') as f:
    pickle.dump(dict(facteur=facteur_serie, charges=dict(zip(panel.columns, charge)),
                       panel_original=panel, n_iterations=iteration+1), f)
print(f"\nSauvegarde : facteur_stock_watson_agriculture.pkl")
