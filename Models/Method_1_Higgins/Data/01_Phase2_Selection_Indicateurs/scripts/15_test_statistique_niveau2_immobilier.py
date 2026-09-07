# -*- coding: utf-8 -*-
"""
15_test_statistique_niveau2_immobilier.py -- Phase 2, Niveau 2 : test
statistique + FDR pour la branche Immobilier (cas particulier, traite
separement des 11 autres branches car sa cible et ses candidats melangent
deux sources -- credit (Bank Al-Maghrib) et IPAI (bulletins BAM/ANCFCC
reconstruits, voir dossier 02_IPC_Deflation pour la meme logique de
reconstruction appliquee a l'IPC).

Meme methode que 14_test_statistique_niveau2_exploratoires_avec_FDR.py :
Student sur Delta-log, train = T1-2010 a T1-2021, FDR (Benjamini-Hochberg)
sur les candidats NON-ancres uniquement. Les ancres (Credit a l'immobilier,
IPAI Residentiel-transactions -- voir script 13) ne sont jamais testees ici.
"""
import pickle
import numpy as np
import pandas as pd
from scipy import stats
from datetime import date
from statsmodels.stats.multitest import multipletests

DOSSIER_PHASE2 = '../../'  # a adapter selon l'emplacement reel des .pkl
DATE_DEBUT_ESTIMATION = date(2010, 1, 1)
DATE_FIN_TRAIN = date(2021, 1, 1)
SEUIL_ALPHA = 0.10
SEUIL_R_PLANCHER = 0.15
NB_MIN_TRIM = 12

def log_diff(serie):
    s = pd.Series({d: v for d, v in serie}).sort_index()
    s.index = pd.to_datetime(s.index)
    s = s[s > 0]
    return np.log(s).diff().dropna()

def to_quarterly(serie):
    s = pd.Series({d: v for d, v in serie}).sort_index()
    s.index = pd.to_datetime(s.index)
    q = s.resample("QS").mean().dropna()
    return [(d.date(), v) for d, v in q.items()]

def tester(cible_ld_train, serie, freq):
    serie_q = to_quarterly(serie) if freq == 'mensuel' else serie
    ind_ld = log_diff(serie_q)
    ind_ld_train = ind_ld[(ind_ld.index >= pd.Timestamp(DATE_DEBUT_ESTIMATION)) &
                            (ind_ld.index <= pd.Timestamp(DATE_FIN_TRAIN))]
    common = cible_ld_train.index.intersection(ind_ld_train.index)
    n = len(common)
    if n < NB_MIN_TRIM:
        return dict(r=None, p=None, n=n)
    r = np.corrcoef(cible_ld_train.loc[common], ind_ld_train.loc[common])[0, 1]
    if np.isnan(r):
        return dict(r=None, p=None, n=n)
    t = r * np.sqrt((n - 2) / (1 - r**2)) if abs(r) < 1 else np.inf
    p = 2 * (1 - stats.t.cdf(abs(t), df=n - 2))
    return dict(r=round(r, 3), p=round(p, 4), n=n)

# --- Chargement des donnees brutes Immobilier ---------------------------
with open(f'{DOSSIER_PHASE2}/data/immobilier_brut.pkl', 'rb') as f:
    immo_brut = pickle.load(f)

cible = sorted(immo_brut['cible_retropolee'])
cible_ld = log_diff(cible)
cible_ld_train = cible_ld[(cible_ld.index >= pd.Timestamp(DATE_DEBUT_ESTIMATION)) &
                            (cible_ld.index <= pd.Timestamp(DATE_FIN_TRAIN))]

# --- Reconstruction des candidats (memes 4 credit + 9 IPAI que le script 13) ---
COLS_CREDIT_IMMO = {5, 6, 71, 72}
credit_series = [ind for ind in immo_brut['indicateurs'] if ind['colonne'] in COLS_CREDIT_IMMO]
for c in credit_series:
    c['freq'] = 'mensuel'

ipai_etendu = pd.read_csv('../../02_IPC_Deflation/../ipai_variations_propre_etendue.csv',
                            sep=';', encoding='utf-8-sig')  # ajuster le chemin si deplace

def trim_to_date(t):
    trim, an = t.split('-')
    return date(int(an), (int(trim[1]) - 1) * 3 + 1, 1)

IPAI_RETENUES = [("Professionnel", "prix"), ("Foncier", "prix"), ("Bureau", "prix"),
                  ("Résidentiel", "transactions"), ("Local commercial", "transactions"),
                  ("Local commercial", "prix"), ("Professionnel", "transactions"),
                  ("Foncier", "transactions"), ("Bureau", "transactions")]
ipai_indicateurs = []
for cat, ind in IPAI_RETENUES:
    sub = ipai_etendu[(ipai_etendu['categorie'] == cat) & (ipai_etendu['indicateur'] == ind)]
    serie = [(trim_to_date(row['trimestre']), row['variation_trimestrielle']) for _, row in sub.iterrows()]
    ipai_indicateurs.append(dict(nom=f"IPAI {cat} — {ind} (var. trim. %)", serie=serie, freq='trimestriel'))
for ind in immo_brut['indicateurs']:
    if 'prose' in ind['nom'].lower():
        ipai_indicateurs.append(dict(nom="IPAI Global — prix (série 1998–2017, var. trim. %)",
                                       serie=ind['serie'], freq='trimestriel'))

# --- Separation ancres (Niveau 1, jamais testees) / reste (a tester) -----
ancre_credit = [c for c in credit_series if 'immobilier' in c['nom'].lower()][:1]
ancre_ipai = [i for i in ipai_indicateurs if 'Résidentiel' in i['nom'] and 'transactions' in i['nom']]
ancres = ancre_credit + ancre_ipai
reste = [c for c in credit_series if c not in ancre_credit] + \
        [i for i in ipai_indicateurs if i not in ancre_ipai]

print(f"Ancres (Niveau 1, non testees) : {len(ancres)}")
print(f"Candidats exploratoires a tester : {len(reste)}")

# --- Test + FDR sur le reste uniquement -----------------------------------
res_reste = []
for ind in reste:
    r = tester(cible_ld_train, ind['serie'], ind['freq'])
    res_reste.append(dict(nom=ind['nom'], freq=ind['freq'], **r, obj=ind))

testables = [r for r in res_reste if r['p'] is not None]
if testables:
    p_values = [r['p'] for r in testables]
    rejet_h0, p_adj, _, _ = multipletests(p_values, alpha=SEUIL_ALPHA, method='fdr_bh')
    for r, sig, padj in zip(testables, rejet_h0, p_adj):
        r['p_bh'] = round(padj, 4)
        r['retenu'] = bool(sig and abs(r['r']) >= SEUIL_R_PLANCHER)
for r in res_reste:
    if 'retenu' not in r:
        r['retenu'] = False
        r['p_bh'] = None

retenus = [r for r in res_reste if r['retenu']]
print(f"\nRESULTAT : {len(retenus)} / {len(reste)} candidats exploratoires retenus (Niveau 2)")
for r in res_reste:
    print(f"  {r['nom'][:45]:45s} r={r['r']} p={r['p']} p_bh={r.get('p_bh')} n={r['n']} -> "
          f"{'RETENU' if r['retenu'] else 'rejete'}")

with open('resultats_niveau2_immobilier.pkl', 'wb') as f:
    pickle.dump(dict(cible=cible, res_ancres=[dict(nom=a['nom'], freq=a['freq']) for a in ancres],
                       res_reste=res_reste, ancres_objs=ancres, reste_objs=reste), f)
print("\nSauvegarde : resultats_niveau2_immobilier.pkl")
