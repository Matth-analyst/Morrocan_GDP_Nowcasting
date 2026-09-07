# -*- coding: utf-8 -*-
"""
01_deflation_et_retest.py -- Deflate tous les candidats nominaux (credit
bancaire, M3, recettes touristiques, primes, commerce exterieur) avec la
serie IPC reconstruite, puis retest (Student + FDR par branche, meme
procedure que le Niveau 2) pour voir si leur statut change.

Les ANCRES ne sont jamais retestees (elles restent ancres par construction,
Niveau 1) -- mais leur version deflatee est tout de meme calculee et
comparee, pour verifier si la deflation change substantiellement leur
force de correlation (a titre informatif, sans consequence sur leur statut).
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

# --- Chargement de l'IPC reconstruit ---------------------------------------
ipc_df = pd.read_csv('/home/claude/Data_reorganise/GDPNow_Maroc/02_IPC_Deflation/donnees/ipc_maroc_2010_2026_raccorde.csv')
ipc_df['date'] = pd.to_datetime(ipc_df['date'])
IPC = {row['date'].date(): row['IPC_base2017'] for _, row in ipc_df.iterrows()}

def deflater(serie):
    """x_reel_t = x_nominal_t / (IPC_t / 100), IPC le plus proche du mois de la donnee."""
    dates_ipc = sorted(IPC.keys())
    resultat = []
    for d, v in serie:
        # aligner sur le 1er du mois le plus proche dans IPC
        d_mois = date(d.year, d.month, 1)
        if d_mois in IPC:
            ipc_val = IPC[d_mois]
            resultat.append((d, v / (ipc_val/100)))
    return resultat

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

def tester(cible_ld_train, serie, freq):
    serie_q = to_quarterly(serie) if freq=='mensuel' else serie
    ind_ld = log_diff(serie_q)
    ind_ld_train = ind_ld[(ind_ld.index>=pd.Timestamp(DATE_DEBUT_ESTIMATION))&(ind_ld.index<=pd.Timestamp(DATE_FIN_TRAIN))]
    n = 0
    return ind_ld_train

MOTS_NOMINAUX = ['crédit bancaire', 'comptes débiteurs', "crédits à l'équipement",
                  'crédits à l', 'masse monétaire', 'recettes touristiques',
                  'primes', 'crédits aux promoteurs', 'financement participatif',
                  'total exportations', 'total importations']

ANCRES_NOMS = {
    'agriculture': [],
    'peche': [],
    'industrie_transfo': [],
    'ind_extraction': [],
    'finances': ['Crédit bancaire'],
    'hebergement': ['Recettes touristiques'],
    'construction': ['Crédit bancaire Bâtiment'],
    'commerce': ['TOTAL EXPORTATIONS', 'TOTAL IMPORTATIONS'],
    'transports': [],
    'electricite': [],
    'infocom': [],
    'immobilier': ["Crédits à l'immobilier", 'Crédits à l’immobilier'],
}

branches = ['agriculture','peche','industrie_transfo','ind_extraction','immobilier',
            'finances','hebergement','construction','commerce','transports',
            'electricite']

resultats_complets = {}

for prefix in branches:
    with open(f'{DOSSIER}/data/{prefix}_brut.pkl','rb') as f:
        brut = pickle.load(f)

    cible = sorted(brut['cible_retropolee'])
    cible_ld = log_diff(cible)
    cible_ld_train = cible_ld[(cible_ld.index>=pd.Timestamp(DATE_DEBUT_ESTIMATION))&(cible_ld.index<=pd.Timestamp(DATE_FIN_TRAIN))]

    nominaux = [i for i in brut['indicateurs'] if any(m in i['nom'].lower() for m in MOTS_NOMINAUX)]
    if not nominaux:
        continue

    resultats_branche = []
    for ind in nominaux:
        serie_deflatee = deflater(ind['serie'])
        if len(serie_deflatee) < NB_MIN_TRIM:
            continue

        # determiner la freq
        dates_only = sorted(d for d,v in ind['serie'])
        ecarts = [(dates_only[i+1]-dates_only[i]).days for i in range(len(dates_only)-1)]
        med = np.median(ecarts) if ecarts else 999
        freq = "mensuel" if med<=40 else "trimestriel"

        # test AVANT deflation (nominal)
        serie_q_nom = to_quarterly(ind['serie']) if freq=='mensuel' else ind['serie']
        ind_ld_nom = log_diff(serie_q_nom)
        ind_ld_nom_train = ind_ld_nom[(ind_ld_nom.index>=pd.Timestamp(DATE_DEBUT_ESTIMATION))&(ind_ld_nom.index<=pd.Timestamp(DATE_FIN_TRAIN))]
        common_nom = cible_ld_train.index.intersection(ind_ld_nom_train.index)
        n_nom = len(common_nom)
        r_nom = np.corrcoef(cible_ld_train.loc[common_nom], ind_ld_nom_train.loc[common_nom])[0,1] if n_nom>=NB_MIN_TRIM else None

        # test APRES deflation (reel)
        serie_q_def = to_quarterly(serie_deflatee) if freq=='mensuel' else serie_deflatee
        ind_ld_def = log_diff(serie_q_def)
        ind_ld_def_train = ind_ld_def[(ind_ld_def.index>=pd.Timestamp(DATE_DEBUT_ESTIMATION))&(ind_ld_def.index<=pd.Timestamp(DATE_FIN_TRAIN))]
        common_def = cible_ld_train.index.intersection(ind_ld_def_train.index)
        n_def = len(common_def)
        if n_def < NB_MIN_TRIM:
            continue
        r_def = np.corrcoef(cible_ld_train.loc[common_def], ind_ld_def_train.loc[common_def])[0,1]
        t_def = r_def*np.sqrt((n_def-2)/(1-r_def**2)) if abs(r_def)<1 else np.inf
        p_def = 2*(1-stats.t.cdf(abs(t_def), df=n_def-2))

        est_ancre = any(a in ind['nom'] for a in ANCRES_NOMS.get(prefix, []))

        resultats_branche.append(dict(
            nom=ind['nom'], colonne=ind['colonne'], freq=freq, est_ancre=est_ancre,
            r_nominal=round(r_nom,3) if r_nom is not None else None,
            r_deflate=round(r_def,3), p_deflate=round(p_def,4), n=n_def
        ))

    # FDR sur les non-ancres uniquement
    non_ancres = [r for r in resultats_branche if not r['est_ancre']]
    if non_ancres:
        p_values = [r['p_deflate'] for r in non_ancres]
        rejet_h0, p_adj, _, _ = multipletests(p_values, alpha=SEUIL_ALPHA, method='fdr_bh')
        for r, sig, padj in zip(non_ancres, rejet_h0, p_adj):
            r['p_bh'] = round(padj,4)
            r['retenu_deflate'] = bool(sig and abs(r['r_deflate'])>=SEUIL_R_PLANCHER)
    for r in resultats_branche:
        if r['est_ancre']:
            r['p_bh'] = None
            r['retenu_deflate'] = None  # sans objet, reste ancre par construction

    resultats_complets[prefix] = resultats_branche

    print(f"\n=== {prefix} ({len(resultats_branche)} series nominales testables) ===")
    for r in resultats_branche:
        statut = "ANCRE (conservee)" if r['est_ancre'] else ("RETENU" if r['retenu_deflate'] else "rejete")
        print(f"  {r['nom'][:45]:45s} r_nom={r['r_nominal']} -> r_reel={r['r_deflate']:+.3f} "
              f"p_bh={r.get('p_bh')} n={r['n']} [{statut}]")

with open(f'{DOSSIER}/deflation/resultats_deflation.pkl','wb') as f:
    pickle.dump(resultats_complets, f)
print("\nSauvegarde : deflation/resultats_deflation.pkl")
