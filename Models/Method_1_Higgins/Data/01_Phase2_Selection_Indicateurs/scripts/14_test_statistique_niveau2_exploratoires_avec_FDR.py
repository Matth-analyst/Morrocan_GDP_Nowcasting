# -*- coding: utf-8 -*-
"""
14_test_statistique_niveau2.py -- Niveau 2 de la selection : test
statistique sur les 434 candidats (24 ancres, sans FDR ; 410 restants,
avec correction FDR par branche), train = T1-2010 a T1-2021.
"""
import pickle
import numpy as np
import pandas as pd
from scipy import stats
from datetime import date
from statsmodels.stats.multitest import multipletests

DOSSIER = '/home/claude/phase2'
DATE_DEBUT_ESTIMATION = date(2010, 1, 1)
DATE_FIN_TRAIN = date(2021, 1, 1)
SEUIL_ALPHA = 0.10
SEUIL_R_PLANCHER = 0.15
NB_MIN_TRIM = 12

def log_diff(serie):
    s = pd.Series({d:v for d,v in serie}).sort_index()
    s.index = pd.to_datetime(s.index)
    s = s[s>0]
    return np.log(s).diff().dropna()

def to_quarterly(serie):
    s = pd.Series({d:v for d,v in serie}).sort_index()
    s.index = pd.to_datetime(s.index)
    q = s.resample("QS").mean().dropna()
    return [(d.date(),v) for d,v in q.items()]

def tester_candidat(cible_ld_train, ind, freq):
    serie_q = to_quarterly(ind['serie']) if freq=='mensuel' else ind['serie']
    ind_ld = log_diff(serie_q)
    ind_ld_train = ind_ld[(ind_ld.index>=pd.Timestamp(DATE_DEBUT_ESTIMATION))&(ind_ld.index<=pd.Timestamp(DATE_FIN_TRAIN))]
    common = cible_ld_train.index.intersection(ind_ld_train.index)
    n = len(common)
    if n < NB_MIN_TRIM:
        return dict(r=None, p=None, n=n, motif=f"n={n}<{NB_MIN_TRIM}")
    r = np.corrcoef(cible_ld_train.loc[common], ind_ld_train.loc[common])[0,1]
    if np.isnan(r):
        return dict(r=None, p=None, n=n, motif="r indefini")
    t = r*np.sqrt((n-2)/(1-r**2)) if abs(r)<1 else np.inf
    p = 2*(1-stats.t.cdf(abs(t), df=n-2))
    return dict(r=round(r,3), p=round(p,4), n=n, motif=None)

ANCRES = {
    'agriculture':       [{'colonne': None, 'nom_match': 'Moyenne des precipitations'},
                            {'colonne': None, 'nom_match': 'Température_moyenne'}],
    'peche':              [{'colonne': 21, 'nom_match': None}, {'colonne': 17, 'nom_match': None}],
    'industrie_transfo':  [{'colonne': None, 'nom_match': 'Taux d’Utilisation des Capacités'},
                            {'colonne': None, 'nom_match': 'IPI — Industries alimentaires'}],
    'ind_extraction':     [{'colonne': 28, 'nom_match': None},
                            {'colonne': None, 'nom_match': 'IPM — Industries extractives'}],
    'finances':           [{'colonne': None, 'nom_match': 'Masse monétaire (M3)'},
                            {'colonne': 5, 'nom_match': None}],
    'hebergement':        [{'colonne': None, 'nom_match': 'Recettes touristiques'},
                            {'colonne': 5, 'nom_match': None}],
    'construction':       [{'colonne': None, 'nom_match': 'Vente de ciment'},
                            {'colonne': None, 'nom_match': 'Crédit bancaire Bâtiment'}],
    'commerce':           [{'colonne': None, 'nom_match': 'TOTAL EXPORTATIONS'},
                            {'colonne': None, 'nom_match': 'TOTAL IMPORTATIONS'}],
    'transports':         [{'colonne': None, 'nom_match': "Trafic globale gérés par l'ANP"},
                            {'colonne': None, 'nom_match': 'Trafic aérien'}],
    'electricite':        [{'colonne': 100, 'nom_match': None}, {'colonne': 101, 'nom_match': None}],
    'infocom':            [{'colonne': None, 'nom_match': 'Parc téléphonie mobile global'},
                            {'colonne': None, 'nom_match': 'Parc Internet global'}],
}

def trouver_ancres(indicateurs, specs):
    trouves = []
    for spec in specs:
        for ind in indicateurs:
            if spec['colonne'] is not None and ind['colonne'] == spec['colonne']:
                trouves.append(ind); break
            if spec.get('nom_match') and spec['nom_match'] in ind['nom']:
                trouves.append(ind); break
    return trouves

BRANCHES_STANDARD = [
    ('Agriculture', 'agriculture'), ('Peche', 'peche'),
    ('Industrie transformation', 'industrie_transfo'), ('Industrie extraction', 'ind_extraction'),
    ('Finances assurances', 'finances'), ('Hebergement restauration', 'hebergement'),
    ('Construction', 'construction'), ('Commerce', 'commerce'),
    ('Transports', 'transports'), ('Electricite gaz eau', 'electricite'),
    ('Information communication', 'infocom'),
]

resultats_globaux = {}

for nom_feuille, prefix in BRANCHES_STANDARD:
    with open(f'{DOSSIER}/data/{prefix}_brut.pkl', 'rb') as f:
        brut = pickle.load(f)
    with open(f'{DOSSIER}/resultats/{prefix}_sans_fraicheur.pkl', 'rb') as f:
        indicateurs = pickle.load(f)

    cible = sorted(brut['cible_retropolee'])
    cible_ld = log_diff(cible)
    cible_ld_train = cible_ld[(cible_ld.index>=pd.Timestamp(DATE_DEBUT_ESTIMATION))&(cible_ld.index<=pd.Timestamp(DATE_FIN_TRAIN))]

    ancres = trouver_ancres(indicateurs, ANCRES.get(prefix, []))
    ancres_ids = set(id(a) for a in ancres)
    reste = [i for i in indicateurs if id(i) not in ancres_ids]

    # --- Ancres : test simple, SANS FDR ---
    res_ancres = []
    for a in ancres:
        r = tester_candidat(cible_ld_train, a, a['freq'])
        retenu = (r['p'] is not None) and (r['p']<SEUIL_ALPHA) and (abs(r['r'])>=SEUIL_R_PLANCHER)
        res_ancres.append(dict(nom=a['nom'], colonne=a['colonne'], freq=a['freq'], **r, retenu=retenu, type='ancre'))

    # --- Reste : test + FDR ---
    res_reste = []
    for ind in reste:
        r = tester_candidat(cible_ld_train, ind, ind['freq'])
        res_reste.append(dict(nom=ind['nom'], colonne=ind['colonne'], freq=ind['freq'], categorie=ind.get('categorie'), **r, obj=ind))

    testables = [r for r in res_reste if r['p'] is not None]
    if testables:
        p_values = [r['p'] for r in testables]
        rejet_h0, p_adj, _, _ = multipletests(p_values, alpha=SEUIL_ALPHA, method='fdr_bh')
        for r, sig, padj in zip(testables, rejet_h0, p_adj):
            r['p_bh'] = round(padj,4)
            r['retenu'] = bool(sig and abs(r['r'])>=SEUIL_R_PLANCHER)
    for r in res_reste:
        if 'retenu' not in r: r['retenu'] = False; r['p_bh'] = None

    n_ancres_retenues = sum(1 for r in res_ancres if r['retenu'])
    n_reste_retenues = sum(1 for r in res_reste if r['retenu'])
    print(f"{nom_feuille:28s} : ancres retenues {n_ancres_retenues}/{len(ancres)}  |  reste retenu {n_reste_retenues}/{len(reste)}")

    resultats_globaux[prefix] = dict(nom_feuille=nom_feuille, cible=cible, res_ancres=res_ancres,
                                       res_reste=res_reste, ancres_objs=ancres, reste_objs=reste)

with open(f'{DOSSIER}/resultats/niveau2_resultats.pkl','wb') as f:
    pickle.dump(resultats_globaux, f)
print("\nSauvegarde : resultats/niveau2_resultats.pkl")
