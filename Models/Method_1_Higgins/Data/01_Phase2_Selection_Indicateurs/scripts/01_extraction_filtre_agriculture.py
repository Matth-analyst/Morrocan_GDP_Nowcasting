# -*- coding: utf-8 -*-
"""
01_extraction_agriculture.py -- Phase 2, Etape 1 : extraction de tous les
indicateurs bruts disponibles pour la branche Agriculture, depuis le
classeur source. Aucun filtre applique ici -- inventaire complet.
"""
from openpyxl import load_workbook
from datetime import date
import re
import pickle

FICHIER = '/mnt/user-data/uploads/Etude_sectorielle_Maroc_2_complete.xlsx'
BRANCHE = 'Agriculture'

def parse_date_cell(v):
    if v is None: return None
    if hasattr(v, 'year'): return date(v.year, v.month, 1)
    s = str(v).strip()
    m = re.match(r'T(\d)[\-\s](\d{4})', s)
    if m: t,y = int(m.group(1)),int(m.group(2)); return date(y,(t-1)*3+1,1)
    m = re.match(r'(\d{4})T(\d)', s)
    if m: y,t = int(m.group(1)),int(m.group(2)); return date(y,(t-1)*3+1,1)
    m = re.match(r'(\d{4})M(\d{1,2})', s)
    if m: y,mo = int(m.group(1)),int(m.group(2)); return date(y,mo,1)
    mois_fr = {'janv':1,'févr':2,'fevr':2,'mars':3,'avr':4,'mai':5,'juin':6,
               'juil':7,'août':8,'aout':8,'sept':9,'oct':10,'nov':11,'déc':12,'dec':12}
    m = re.match(r'([a-zéû]+)\.?-(\d{2,4})', s.lower())
    if m:
        mo_str,y = m.group(1),int(m.group(2))
        if y<100: y += 2000 if y<50 else 1900
        if mo_str in mois_fr: return date(y,mois_fr[mo_str],1)
    m = re.match(r'^(\d{4})$', s)
    if m: return date(int(m.group(1)),1,1)
    return None

wb = load_workbook(FICHIER, read_only=True, data_only=True)
ws = wb[BRANCHE]

DATE_MARKERS = {'Mois','Trimestre','Année'}
col_dates = {}
for c in range(1, ws.max_column+1):
    v3, v4 = ws.cell(row=3,column=c).value, ws.cell(row=4,column=c).value
    if v4 in DATE_MARKERS: col_dates[c] = v4
    elif v3 in DATE_MARKERS: col_dates[c] = v3

date_cols_sorted = sorted(col_dates.keys())

# Pour chaque colonne de dates, les colonnes de valeurs associees vont
# jusqu'a la colonne de dates suivante (exclue), sauf la colonne de dates
# elle-meme.
blocs = []
for i, dc in enumerate(date_cols_sorted):
    fin = date_cols_sorted[i+1] if i+1 < len(date_cols_sorted) else ws.max_column+1
    val_cols = [c for c in range(dc, fin) if c != dc]
    blocs.append((dc, val_cols))

# --- Extraction de la CIBLE (colonnes 1-2, et la version retropolee 11-12) --
def extraire_serie(date_col, val_col, ligne_debut=5):
    serie = []
    r = ligne_debut
    vides_consecutifs = 0
    while vides_consecutifs < 30 and r < ws.max_row + 1:
        d_raw = ws.cell(row=r, column=date_col).value
        v_raw = ws.cell(row=r, column=val_col).value
        d = parse_date_cell(d_raw)
        if d is None and v_raw is None:
            vides_consecutifs += 1
        else:
            vides_consecutifs = 0
            if d is not None and isinstance(v_raw, (int, float)):
                serie.append((d, float(v_raw)))
        r += 1
    return serie

cible_base = extraire_serie(1, 2)
cible_retropolee = extraire_serie(11, 12)
print(f"Cible base 2014 : {len(cible_base)} obs")
print(f"Cible retropolee : {len(cible_retropolee)} obs")

# --- Extraction de TOUS les indicateurs (hors colonnes 21-39, variantes de cible) ---
COLONNES_A_EXCLURE_CIBLE = set(range(21, 40))  # variantes du PIB Agriculture lui-meme

indicateurs = []
for dc, val_cols in blocs:
    if dc in (1, 11):  # colonnes de la cible elle-meme, deja extraites
        continue
    for vc in val_cols:
        if vc in COLONNES_A_EXCLURE_CIBLE:
            continue
        nom = ws.cell(row=3, column=vc).value
        source = ws.cell(row=4, column=vc).value
        if not nom:
            continue
        serie = extraire_serie(dc, vc)
        if len(serie) < 4:
            continue
        indicateurs.append(dict(nom=str(nom), source=str(source) if source else '', serie=serie, colonne=vc))

print(f"\n{len(indicateurs)} indicateurs bruts extraits pour {BRANCHE}\n")
for ind in indicateurs:
    dates_only = [d for d,v in ind['serie']]
    print(f"  col{ind['colonne']:3d} | {ind['nom'][:45]:45s} | {len(ind['serie']):3d} obs | {min(dates_only)} a {max(dates_only)} | {ind['source'][:40]}")

with open('/home/claude/phase2/data/agriculture_brut.pkl', 'wb') as f:
    pickle.dump(dict(cible_base=cible_base, cible_retropolee=cible_retropolee, indicateurs=indicateurs), f)
print("\nSauvegarde : /home/claude/phase2/data/agriculture_brut.pkl")
