# -*- coding: utf-8 -*-
"""
construire_classeur_niveau2.py -- Ajoute un 5e bloc VIOLET "SERIES
SELECTIONNEES (Niveau 2)" -- resultat du test statistique -- a chaque
feuille de branche.
"""
import pickle
import pandas as pd
from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter
from datetime import date

DOSSIER = '/home/claude/phase2'
FILL_CIBLE = PatternFill("solid", fgColor="548235")
FILL_ANCRE = PatternFill("solid", fgColor="BF9000")
FILL_SELECT = PatternFill("solid", fgColor="7030A0")   # violet
FILL_T = PatternFill("solid", fgColor="1F4E78")
FILL_M = PatternFill("solid", fgColor="C55A11")
THIN = Side(style="thin", color="BFBFBF")
BORDER = Border(left=THIN, right=THIN, top=THIN, bottom=THIN)
SEP = 2

wb = Workbook()
wb.remove(wb.active)

def construire_dict_series(items):
    d = {}
    for ind in items:
        nom = ind['nom']; base = nom; i = 2
        while nom in d:
            nom = f"{base} ({i})"; i += 1
        d[nom] = dict(ind['serie'])
    return d

def ecrire_bloc(ws, col_depart, titre, fill, series_dict):
    if not series_dict: return col_depart
    df = pd.DataFrame(series_dict).sort_index()
    n_col = len(df.columns)
    c1 = get_column_letter(col_depart); c2 = get_column_letter(col_depart + n_col)
    ws.merge_cells(f"{c1}1:{c2}1")
    cell = ws.cell(row=1, column=col_depart, value=titre)
    cell.font = Font(size=11, bold=True, color="FFFFFF"); cell.fill = fill
    cell.alignment = Alignment(vertical="center", horizontal="center")
    cell2 = ws.cell(row=3, column=col_depart, value="Date")
    cell2.font = Font(bold=True, color="FFFFFF"); cell2.fill = fill; cell2.border = BORDER
    ws.column_dimensions[get_column_letter(col_depart)].width = 12
    for j, col in enumerate(df.columns):
        cc = col_depart + 1 + j
        c = ws.cell(row=3, column=cc, value=col)
        c.font = Font(bold=True, size=9, color="FFFFFF"); c.fill = fill
        c.alignment = Alignment(horizontal="center", wrap_text=True, vertical="center")
        c.border = BORDER
        ws.column_dimensions[get_column_letter(cc)].width = 15
    ws.row_dimensions[3].height = 55
    for i, (d, row) in enumerate(df.iterrows()):
        r = 4 + i
        c0 = ws.cell(row=r, column=col_depart, value=d)
        c0.number_format = "YYYY-MM-DD"; c0.border = BORDER; c0.font = Font(size=9)
        for j, col in enumerate(df.columns):
            cc = col_depart + 1 + j; val = row[col]
            c = ws.cell(row=r, column=cc, value=round(float(val),4) if pd.notna(val) else None)
            if pd.notna(val): c.number_format = "#,##0.0000"
            c.border = BORDER; c.font = Font(size=9)
    return col_depart + 1 + n_col + SEP

with open(f'{DOSSIER}/resultats/niveau2_resultats.pkl','rb') as f:
    res = pickle.load(f)
with open(f'{DOSSIER}/resultats/niveau2_immobilier.pkl','rb') as f:
    res_immo = pickle.load(f)

BRANCHES_STANDARD = [
    ('Agriculture', 'agriculture'), ('Peche', 'peche'),
    ('Industrie transformation', 'industrie_transfo'), ('Industrie extraction', 'ind_extraction'),
    ('Finances assurances', 'finances'), ('Hebergement restauration', 'hebergement'),
    ('Construction', 'construction'), ('Commerce', 'commerce'),
    ('Transports', 'transports'), ('Electricite gaz eau', 'electricite'),
    ('Information communication', 'infocom'),
]
recap = []

for nom_feuille, prefix in BRANCHES_STANDARD:
    with open(f'{DOSSIER}/data/{prefix}_brut.pkl', 'rb') as f:
        brut = pickle.load(f)
    d = res[prefix]

    # Series retenues au niveau 2 (ancres + reste), avec reference a leur objet serie complet
    ancres_objs = {r['nom']: obj for r, obj in zip(d['res_ancres'], d['ancres_objs'])}
    reste_par_nom = {r['nom']: r for r in d['res_reste']}
    selectionnees = []
    for r in d['res_ancres']:
        if r['retenu']: selectionnees.append(ancres_objs[r['nom']])
    for r in d['res_reste']:
        if r['retenu']: selectionnees.append(r['obj'])

    ws = wb.create_sheet(nom_feuille[:31])
    ws.sheet_view.showGridLines = False
    col = 1
    col = ecrire_bloc(ws, col, "CIBLE", FILL_CIBLE,
                        construire_dict_series([{'nom':f"VA {nom_feuille} (Mdh)",'serie':brut['cible_retropolee']}]))
    col = ecrire_bloc(ws, col, f"SÉRIES SÉLECTIONNÉES - Niveau 2 ({len(selectionnees)})",
                        FILL_SELECT, construire_dict_series(selectionnees))
    ws.freeze_panes = "A4"
    recap.append((nom_feuille, len(selectionnees)))
    print(f"{nom_feuille:28s} : {len(selectionnees)} series retenues au niveau 2")

# --- Immobilier ---
with open(f'{DOSSIER}/data/immobilier_brut.pkl', 'rb') as f:
    immo_brut = pickle.load(f)
ancres_objs_immo = {r['nom']: obj for r, obj in zip(res_immo['res_ancres'], res_immo['ancres_objs'])}
selectionnees_immo = []
for r in res_immo['res_ancres']:
    if r['retenu']: selectionnees_immo.append(ancres_objs_immo[r['nom']])
for r in res_immo['res_reste']:
    if r['retenu']: selectionnees_immo.append(r['obj'])

ws = wb.create_sheet('Immobilier')
ws.sheet_view.showGridLines = False
col = 1
col = ecrire_bloc(ws, col, "CIBLE", FILL_CIBLE, construire_dict_series([{'nom':'VA Immobilier (Mdh)','serie':immo_brut['cible_retropolee']}]))
col = ecrire_bloc(ws, col, f"SÉRIES SÉLECTIONNÉES - Niveau 2 ({len(selectionnees_immo)})",
                    FILL_SELECT, construire_dict_series(selectionnees_immo))
ws.freeze_panes = "A4"
recap.append(('Immobilier', len(selectionnees_immo)))
print(f"{'Immobilier':28s} : {len(selectionnees_immo)} series retenues au niveau 2")

# --- Sommaire ---
ws = wb.create_sheet('Sommaire', 0)
ws.sheet_view.showGridLines = False
ws.merge_cells("A1:C1")
ws["A1"] = "GDPNow-Maroc — Séries sélectionnées après test statistique (Niveau 2)"
ws["A1"].font = Font(size=13, bold=True, color="FFFFFF"); ws["A1"].fill = FILL_SELECT
ws["A1"].alignment = Alignment(vertical="center", indent=1); ws.row_dimensions[1].height = 22

headers = ["Branche", "Séries retenues (Niveau 2)", "Feuille"]
for j, h in enumerate(headers):
    c = ws.cell(row=3, column=j+1, value=h)
    c.font = Font(bold=True, color="FFFFFF"); c.fill = FILL_SELECT; c.border = BORDER
for col_l, w in zip("ABC", [30,24,25]):
    ws.column_dimensions[col_l].width = w

total = 0
for i, (nom, n) in enumerate(recap):
    r=4+i
    ws.cell(row=r,column=1,value=nom).border=BORDER
    ws.cell(row=r,column=2,value=n).border=BORDER
    ws.cell(row=r,column=3,value=nom[:31]).border=BORDER
    total += n
r=4+len(recap)
ws.cell(row=r,column=1,value="TOTAL").font=Font(bold=True); ws.cell(row=r,column=1).border=BORDER
ws.cell(row=r,column=2,value=total).font=Font(bold=True); ws.cell(row=r,column=2).border=BORDER

r2 = r+2
ws.cell(row=r2, column=1, value="Méthode : ancres testées seules (sans FDR) ; reste testé avec correction FDR (Benjamini-Hochberg) par branche")
ws.cell(row=r2+1, column=1, value=f"Total testé : 434 candidats (24 ancres + 410 exploratoires) -> {total} retenus")

wb.save('/mnt/user-data/outputs/GDPNow_Maroc_series_selectionnees_Niveau2.xlsx')
print(f"\nTOTAL: {total}")
