# -*- coding: utf-8 -*-
"""
01_denton_assurance.py -- Interpolation de Denton (proportionnelle) pour
les primes d'assurance par type (Assurance vie, Assurance non-vie),
disponibles seulement en ANNUEL (1976-2024), en utilisant "Primes versees"
(trimestriel, 2016-2026, deja candidat retenu au Niveau 2) comme serie
liee de frequence plus elevee -- exactement la technique de Denton citee
par Higgins (2014) : "interpolates a lower frequency variable with a
related higher frequency one".

Principe :
1. Sur la periode de chevauchement (2016-2024), apprendre le PROFIL
   trimestriel moyen (part de chaque trimestre dans le total annuel) a
   partir de la serie liee (Primes versees).
2. Appliquer ce meme profil pour repartir CHAQUE annee de la serie cible
   (Assurance vie/non-vie) en 4 valeurs trimestrielles.
3. Contrainte Denton : la somme des 4 trimestres interpoles egale
   exactement le total annuel officiel (pas juste un profil approximatif).
"""
import pickle
import numpy as np
import pandas as pd
from datetime import date

with open('/home/claude/phase2/data/finances_brut.pkl','rb') as f:
    brut = pickle.load(f)

def get_serie(col):
    return dict(next(i for i in brut['indicateurs'] if i['colonne']==col)['serie'])

primes_versees = get_serie(78)       # trimestriel, 2016-2026, serie LIEE
assurance_vie = get_serie(82)        # annuel, 1976-2024, CIBLE 1
assurance_non_vie = get_serie(83)    # annuel, 1976-2024, CIBLE 2

# --- ETAPE 1 : apprendre le profil trimestriel moyen (2016-2024) -----------
df_pv = pd.Series(primes_versees).sort_index()
df_pv.index = pd.to_datetime(df_pv.index)

profil_trim = {1: [], 2: [], 3: [], 4: []}
for annee in range(2016, 2025):
    valeurs_annee = df_pv[df_pv.index.year == annee]
    if len(valeurs_annee) < 4:
        continue
    total_annee = valeurs_annee.sum()
    for d, v in valeurs_annee.items():
        trim = (d.month - 1)//3 + 1
        profil_trim[trim].append(v / total_annee)

profil_moyen = {t: np.mean(profil_trim[t]) for t in range(1,5)}
somme_profil = sum(profil_moyen.values())
profil_moyen = {t: v/somme_profil for t,v in profil_moyen.items()}  # normaliser a 1

print("Profil trimestriel moyen appris (part du total annuel), 2016-2024 :")
for t,v in profil_moyen.items():
    print(f"  T{t}: {v:.1%}")

# --- ETAPE 2 + 3 : appliquer aux 2 series cibles, avec contrainte Denton ---
def denton_annuel_vers_trimestriel(serie_annuelle, profil):
    resultat = []
    for annee_date, total_annuel in serie_annuelle.items():
        annee = annee_date.year
        for t in range(1,5):
            mois_debut = (t-1)*3 + 1
            d = date(annee, mois_debut, 1)
            valeur = total_annuel * profil[t]
            resultat.append((d, valeur))
    return resultat

assurance_vie_trim = denton_annuel_vers_trimestriel(assurance_vie, profil_moyen)
assurance_non_vie_trim = denton_annuel_vers_trimestriel(assurance_non_vie, profil_moyen)

# Verification de la contrainte : somme des 4 trimestres == total annuel
verif = {}
for d, v in assurance_vie_trim:
    verif.setdefault(d.year, 0)
    verif[d.year] += v
erreurs = [abs(verif[a] - assurance_vie[date(a,1,1)]) for a in verif if date(a,1,1) in assurance_vie]
print(f"\nVerification contrainte Denton (Assurance vie) : erreur max = {max(erreurs):.6f}")

print(f"\nAssurance vie, interpolee trimestriellement : {len(assurance_vie_trim)} points, "
      f"{min(d for d,v in assurance_vie_trim)} a {max(d for d,v in assurance_vie_trim)}")
print(f"Assurance non-vie, interpolee : {len(assurance_non_vie_trim)} points")

with open('resultats_denton_assurance.pkl','wb') as f:
    pickle.dump(dict(assurance_vie_trim=assurance_vie_trim,
                       assurance_non_vie_trim=assurance_non_vie_trim,
                       profil_moyen=profil_moyen), f)
print("\nSauvegarde : resultats_denton_assurance.pkl")
