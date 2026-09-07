# -*- coding: utf-8 -*-
"""
02_selection_agriculture.py -- Phase 2 : selection des indicateurs pour
Agriculture, procedure complete (filtre economique a priori -> qualite ->
test statistique sur train uniquement).
"""
import pickle
import numpy as np
import pandas as pd
from scipy import stats
from datetime import date

with open('/home/claude/phase2/data/agriculture_brut.pkl','rb') as f:
    brut = pickle.load(f)

TODAY = date(2026, 8, 19)
DATE_DEBUT_ESTIMATION = date(2010, 1, 1)
DATE_FIN_TRAIN = date(2021, 1, 1)

# ============================================================================
# ETAPE 1 -- Filtre economique a priori (ecrit AVANT tout calcul statistique)
# ============================================================================
# Raisonnement : un indicateur est plausible pour Agriculture s'il mesure
# soit une condition de FINANCEMENT direct du secteur (credit agricole),
# soit une condition CLIMATIQUE reconnue comme determinant de premier ordre
# du rendement agricole (precipitations, temperature -- litterature
# agronomique standard, effet a court terme sur les recoltes en cours).
#
# Les series de rendement/production/superficies par culture (50 series,
# colonnes 42-91) sont ECARTEES ICI, mais pour une raison de FREQUENCE
# (annuelle), pas de plausibilite economique -- elles seraient economiquement
# tres pertinentes, mais structurellement inutilisables dans un modele
# trimestriel sans agregation ad hoc non retenue dans ce travail.
#
# Doublons ecartes : les colonnes 15,16,17 ("Agriculture et pêche", tables
# de ventilation) sont des DUPLICATA EXACTS des colonnes 7,8,9 (Bank
# Al-Maghrib), verifie par recalage de date -- 78/78 valeurs identiques une
# fois les dates realignees en debut de trimestre. Conserves uniquement
# sous leur premiere occurrence (colonnes 7,8,9).
COLONNES_DOUBLONS_A_EXCLURE = {15, 16, 17}

candidats_economiques = []
for ind in brut['indicateurs']:
    if ind['colonne'] in COLONNES_DOUBLONS_A_EXCLURE:
        continue
    freq_native = None
    dates_only = sorted(d for d,v in ind['serie'])
    ecarts = [(dates_only[i+1]-dates_only[i]).days for i in range(len(dates_only)-1)]
    med = np.median(ecarts) if ecarts else 999
    if med <= 40: freq_native = "mensuel"
    elif med <= 100: freq_native = "trimestriel"
    else: freq_native = "annuel"

    if freq_native == "annuel":
        continue  # C1 -- frequence : ecarte ici, cf. justification ci-dessus

    candidats_economiques.append(dict(**ind, freq=freq_native))

print(f"Candidats apres filtre economique + frequence : {len(candidats_economiques)}")
for c in candidats_economiques:
    print(f"  - {c['nom']:35s} ({c['freq']}, {len(c['serie'])} obs) -- {c['source'][:50]}")

# ============================================================================
# ETAPE 2 -- Filtre de qualite de la donnee
# ============================================================================
SEUIL_LONGUEUR_MIN = 12
SEUIL_FRAICHEUR_JOURS = 450
SEUIL_DENSITE_MIN = 0.80

def expected_count(dmin, dmax, freq):
    months = (dmax.year-dmin.year)*12 + (dmax.month-dmin.month) + 1
    return months if freq == "mensuel" else months/3

candidats_qualite = []
rejets_qualite = []
for c in candidats_economiques:
    dates_only = sorted(d for d,v in c['serie'])
    dmin, dmax = dates_only[0], dates_only[-1]
    if len(c['serie']) < SEUIL_LONGUEUR_MIN:
        rejets_qualite.append((c['nom'], 'C2-longueur', len(c['serie']))); continue
    if (TODAY - dmax).days > SEUIL_FRAICHEUR_JOURS:
        rejets_qualite.append((c['nom'], 'C3-fraicheur', str(dmax))); continue
    exp = expected_count(dmin, dmax, c['freq'])
    densite = len(c['serie']) / exp if exp > 0 else 0
    if densite < SEUIL_DENSITE_MIN:
        rejets_qualite.append((c['nom'], 'C4-densite', f"{densite:.0%}")); continue
    candidats_qualite.append(dict(**c, densite=densite, debut=dmin, fin=dmax))

print(f"\nCandidats apres filtre de qualite : {len(candidats_qualite)}")
for c in candidats_qualite:
    print(f"  - {c['nom']:35s} densite={c['densite']:.0%}  {c['debut']} a {c['fin']}")
for nom, code, detail in rejets_qualite:
    print(f"  [rejete {code}] {nom} -- {detail}")

# ============================================================================
# ETAPE 3 -- Test statistique, sur TRAIN uniquement
# ============================================================================
def log_diff(serie):
    s = pd.Series({d:v for d,v in serie}).sort_index()
    s = s[s>0]
    return np.log(s).diff().dropna()

def to_quarterly(serie):
    s = pd.Series({d:v for d,v in serie}).sort_index()
    s.index = pd.to_datetime(s.index)
    q = s.resample("QS").mean().dropna()
    return [(d.date(), v) for d,v in q.items()]

cible = sorted(brut['cible_retropolee'])
cible_ld = log_diff(cible)
cible_ld_train = cible_ld[(cible_ld.index>=DATE_DEBUT_ESTIMATION)&(cible_ld.index<=DATE_FIN_TRAIN)]

SEUIL_ALPHA = 0.10
SEUIL_R_PLANCHER = 0.15
NB_MIN_TRIM_TRAIN = 12

resultats_test = []
for c in candidats_qualite:
    serie_q = to_quarterly(c['serie']) if c['freq']=='mensuel' else c['serie']
    ind_ld = log_diff(serie_q)
    ind_ld_train = ind_ld[(ind_ld.index>=DATE_DEBUT_ESTIMATION)&(ind_ld.index<=DATE_FIN_TRAIN)]
    common = cible_ld_train.index.intersection(ind_ld_train.index)
    n = len(common)
    if n < NB_MIN_TRIM_TRAIN:
        resultats_test.append(dict(nom=c['nom'], r=None, p=None, n_train=n, retenu=False,
                                     motif=f"trop peu de trimestres train (n={n}<{NB_MIN_TRIM_TRAIN})"))
        continue
    r = np.corrcoef(cible_ld_train.loc[common], ind_ld_train.loc[common])[0,1]
    if np.isnan(r):
        resultats_test.append(dict(nom=c['nom'], r=None, p=None, n_train=n, retenu=False, motif="r indefini"))
        continue
    t = r*np.sqrt((n-2)/(1-r**2)) if abs(r)<1 else np.inf
    p = 2*(1-stats.t.cdf(abs(t), df=n-2))
    retenu = (p < SEUIL_ALPHA) and (abs(r) >= SEUIL_R_PLANCHER)
    motif = "retenu" if retenu else ("|r| sous le plancher" if abs(r)<SEUIL_R_PLANCHER else "non significatif")
    resultats_test.append(dict(nom=c['nom'], r=round(r,3), p=round(p,3), n_train=n, retenu=retenu, motif=motif))

print(f"\n=== Resultat du test statistique (train {DATE_DEBUT_ESTIMATION} a {DATE_FIN_TRAIN}) ===")
for r in resultats_test:
    print(f"  {r['nom']:35s} r={r['r']} p={r['p']} n={r['n_train']:3d} -> {r['motif']}")

n_retenus = sum(1 for r in resultats_test if r['retenu'])
print(f"\nTOTAL RETENU POUR AGRICULTURE : {n_retenus}")

with open('/home/claude/phase2/resultats/agriculture_selection.pkl','wb') as f:
    pickle.dump(dict(candidats_qualite=candidats_qualite, resultats_test=resultats_test), f)
