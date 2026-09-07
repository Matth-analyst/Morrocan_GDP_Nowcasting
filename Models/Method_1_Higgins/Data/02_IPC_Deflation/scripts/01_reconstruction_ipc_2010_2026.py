# -*- coding: utf-8 -*-
"""
01_reconstruction_ipc_2010_2026.py -- Reconstruit une serie IPC mensuelle
unique, base 2017, de janvier 2010 a juillet 2026, par raccordement de
deux bases officielles HCP. Toutes les formules et le detail de la
methode sont exposes dans rapport_ipc_reconstruction.tex (meme dossier).

ENTREES ATTENDUES (a placer dans ./donnees_source/) :
  - ipc_mensuel_base2017.xlsx   : fichier HCP "IPC 2017 par grandes
    divisions, Mensuel" (fourni par l'utilisateur), feuille contenant les
    colonnes Mois / Alimentation / Produits Non Alimentaires / Indice General

Les valeurs annuelles base 2006 (2010-2017) et les 12 valeurs mensuelles
reelles de 2017 (base 2006) utilisees pour le raccordement sont des
constantes sourcees individuellement (voir rapport_ipc_reconstruction.tex,
section 2, pour la reference exacte de chaque valeur) -- elles sont
codees en dur ci-dessous par transparence, faute d'un fichier telechargeable
unique les regroupant.

SORTIE : ipc_maroc_2010_2026_raccorde.csv (date, IPC_base2017)
"""
from openpyxl import load_workbook
import numpy as np
import pandas as pd
from datetime import date
import pickle

# ============================================================================
# ETAPE 0 -- Lecture de la serie mensuelle officielle, base 2017 (2017-2026)
# ============================================================================
FICHIER_BASE2017 = './donnees_source/ipc_mensuel_base2017.xlsx'

wb = load_workbook(FICHIER_BASE2017, read_only=True, data_only=True)
ws = wb.active

ipc_mensuel_b2017 = {}
for r in range(3, 140):
    mois_str = ws.cell(row=r, column=2).value
    indice = ws.cell(row=r, column=5).value
    if mois_str and isinstance(mois_str, str) and '/' in mois_str:
        an, mo = mois_str.split('/')
        d = date(int(an), int(mo), 1)
        if isinstance(indice, (int, float)):
            ipc_mensuel_b2017[d] = float(indice)

print(f"Base 2017 : {len(ipc_mensuel_b2017)} points, "
      f"{min(ipc_mensuel_b2017)} a {max(ipc_mensuel_b2017)}")

# ============================================================================
# ETAPE 1 -- Moyennes annuelles officielles, base 2006 (2010-2017)
# Sources exactes : voir rapport_ipc_reconstruction.tex, section 2.2
# ============================================================================
IPC_ANNUEL_BASE2006 = {
    2010: 108.4, 2011: 109.4, 2012: 110.8, 2013: 112.9, 2014: 113.4,
}
# 2015 : calculee, +1.6% officiel (HCP, rapport annuel 2015) par rapport a 2014
IPC_ANNUEL_BASE2006[2015] = IPC_ANNUEL_BASE2006[2014] * 1.016

# Les 12 valeurs mensuelles REELLES base 2006, annee 2017 (point de raccord)
# Source : rapport regional HCP Tanger, detail mensuel national 2017
IPC_MENSUEL_2017_BASE2006 = {
    1: 117.7, 2: 117.4, 3: 116.7, 4: 116.9, 5: 117.5, 6: 117.9,
    7: 117.3, 8: 117.7, 9: 118.7, 10: 118.6, 11: 119.1, 12: 119.7,
}
IPC_ANNUEL_BASE2006[2017] = np.mean(list(IPC_MENSUEL_2017_BASE2006.values()))

# 2016 : SEULE valeur non directement sourcee -- interpolee log-lineairement
# entre 2015 et 2017 (aucune publication HCP directe trouvee pour cette annee)
log_2015 = np.log(IPC_ANNUEL_BASE2006[2015])
log_2017 = np.log(IPC_ANNUEL_BASE2006[2017])
IPC_ANNUEL_BASE2006[2016] = np.exp((log_2015 + log_2017) / 2)

print("\nSerie annuelle base 2006 (2010-2017) :")
for an in sorted(IPC_ANNUEL_BASE2006):
    marque = " <- INTERPOLE (seule valeur non sourcee)" if an == 2016 else ""
    print(f"  {an}: {IPC_ANNUEL_BASE2006[an]:.3f}{marque}")

# ============================================================================
# ETAPE 2 -- Interpolation Denton simplifiee : annuel -> mensuel (2010-2016)
# Formules exactes : rapport_ipc_reconstruction.tex, equations 2 et 3
# ============================================================================
annees = sorted(IPC_ANNUEL_BASE2006.keys())  # 2010..2017
log_annuel = {a: np.log(IPC_ANNUEL_BASE2006[a]) for a in annees}

mensuel_b2006_brut = {}
for i, a in enumerate(annees[:-1]):
    a_suivant = annees[i + 1]
    log_debut, log_fin = log_annuel[a], log_annuel[a_suivant]
    for m in range(1, 13):
        frac = (m - 1) / 12.0
        log_val = log_debut + (log_fin - log_debut) * frac
        mensuel_b2006_brut[date(a, m, 1)] = np.exp(log_val)

# Contrainte Denton : la moyenne mensuelle de chaque annee doit egaler
# exactement sa moyenne annuelle officielle (equation 3)
mensuel_b2006 = {}
for a in annees[:-1]:
    valeurs_annee = {d: v for d, v in mensuel_b2006_brut.items() if d.year == a}
    moyenne_interpolee = np.mean(list(valeurs_annee.values()))
    facteur_correction = IPC_ANNUEL_BASE2006[a] / moyenne_interpolee
    for d, v in valeurs_annee.items():
        mensuel_b2006[d] = v * facteur_correction

print("\nVerification Denton (moyenne interpolee == moyenne officielle) :")
for a in annees[:-1]:
    m = np.mean([v for d, v in mensuel_b2006.items() if d.year == a])
    print(f"  {a}: {m:.3f}  (officiel: {IPC_ANNUEL_BASE2006[a]:.3f})")

# ============================================================================
# ETAPE 3 -- Coefficient de raccordement (equation 4), sur les 12 mois 2017
# ============================================================================
ratios = []
for m in range(1, 13):
    v_b2006 = IPC_MENSUEL_2017_BASE2006[m]
    v_b2017 = ipc_mensuel_b2017[date(2017, m, 1)]
    ratios.append(v_b2006 / v_b2017)
k = np.mean(ratios)
print(f"\nCoefficient de raccordement k = {k:.5f}  (ecart-type des 12 ratios: {np.std(ratios):.5f})")

# ============================================================================
# ETAPE 4 -- Fusion : 2010-2016 rebascule sur l'echelle base 2017 + 2017-2026 tel quel
# ============================================================================
serie_finale = {}
for d, v in mensuel_b2006.items():
    if d.year < 2017:
        serie_finale[d] = v / k
for d, v in ipc_mensuel_b2017.items():
    serie_finale[d] = v  # valeurs officielles, jamais modifiees

serie_finale = sorted(serie_finale.items())
print(f"\nSerie finale : {len(serie_finale)} points, {serie_finale[0][0]} a {serie_finale[-1][0]}")

# ============================================================================
# SORTIE
# ============================================================================
df = pd.DataFrame(serie_finale, columns=['date', 'IPC_base2017'])
df.to_csv('ipc_maroc_2010_2026_raccorde.csv', index=False, encoding='utf-8-sig')

with open('ipc_complet_raccorde.pkl', 'wb') as f:
    pickle.dump(dict(serie=serie_finale, k=k, ratios=ratios,
                       ipc_annuel_base2006=IPC_ANNUEL_BASE2006), f)

print("\nFichiers ecrits : ipc_maroc_2010_2026_raccorde.csv, ipc_complet_raccorde.pkl")
