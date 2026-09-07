# -*- coding: utf-8 -*-
"""
construire_classeur_v2.py -- Classeur final, propre : chaque branche a
DEUX tableaux distincts (trimestriel, avec la cible ; mensuel, sans la
cible puisqu'elle est trimestrielle) -- jamais de dates melangees.
"""
import pickle
import pandas as pd
from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter
from datetime import date

DOSSIER = '/home/claude/phase2'
HEADER_FILL_T = PatternFill("solid", fgColor="1F4E78")   # trimestriel : bleu fonce
HEADER_FILL_M = PatternFill("solid", fgColor="C55A11")   # mensuel : orange
THIN = Side(style="thin", color="BFBFBF")
BORDER = Border(left=THIN, right=THIN, top=THIN, bottom=THIN)

wb = Workbook()
wb.remove(wb.active)

def ecrire_tableau(ws, ligne_depart, titre, fill, cible_nom, cible_serie, indicateurs):
    """Ecrit un tableau (cible optionnelle + indicateurs d'UNE SEULE frequence)
    a partir de la ligne_depart. Renvoie la ligne suivante disponible."""
    toutes_series = {}
    if cible_nom is not None:
        toutes_series[cible_nom] = dict(cible_serie)
    for ind in indicateurs:
        nom_col = ind['nom']
        base = nom_col; i = 2
        while nom_col in toutes_series:
            nom_col = f"{base} ({i})"; i += 1
        toutes_series[nom_col] = dict(ind['serie'])

    if not toutes_series:
        return ligne_depart

    df = pd.DataFrame(toutes_series).sort_index()
    n_col = len(df.columns)

    r = ligne_depart
    ws.merge_cells(start_row=r, start_column=1, end_row=r, end_column=n_col+1)
    c = ws.cell(row=r, column=1, value=titre)
    c.font = Font(size=12, bold=True, color="FFFFFF")
    c.fill = fill
    c.alignment = Alignment(vertical="center", indent=1)
    ws.row_dimensions[r].height = 20
    r += 2

    ws.cell(row=r, column=1, value="Date").font = Font(bold=True, color="FFFFFF")
    ws.cell(row=r, column=1).fill = fill
    ws.cell(row=r, column=1).border = BORDER
    ws.column_dimensions["A"].width = 12
    for j, col in enumerate(df.columns):
        cc = j + 2
        cell = ws.cell(row=r, column=cc, value=col)
        cell.font = Font(bold=True, size=9, color="FFFFFF")
        cell.fill = fill
        cell.alignment = Alignment(horizontal="center", wrap_text=True, vertical="center")
        cell.border = BORDER
        ws.column_dimensions[get_column_letter(cc)].width = 16
    ws.row_dimensions[r].height = 55
    r_entetes = r
    r += 1

    for i, (d, row) in enumerate(df.iterrows()):
        rr = r + i
        c0 = ws.cell(row=rr, column=1, value=d)
        c0.number_format = "YYYY-MM-DD"
        c0.border = BORDER
        c0.font = Font(size=9)
        for j, col in enumerate(df.columns):
            val = row[col]
            cell = ws.cell(row=rr, column=j+2,
                            value=round(float(val), 4) if pd.notna(val) else None)
            if pd.notna(val): cell.number_format = "#,##0.0000"
            cell.border = BORDER
            cell.font = Font(size=9)

    ws.freeze_panes = ws.cell(row=r_entetes+1, column=2).coordinate
    return r + len(df) + 3  # 3 lignes de marge avant le tableau suivant

# ============================================================================
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
    with open(f'{DOSSIER}/resultats/{prefix}_sans_fraicheur.pkl', 'rb') as f:
        indicateurs = pickle.load(f)

    ind_trim = [i for i in indicateurs if i['freq']=='trimestriel']
    ind_mens = [i for i in indicateurs if i['freq']=='mensuel']

    ws = wb.create_sheet(nom_feuille[:31])
    ws.sheet_view.showGridLines = False

    cible_nom = f"VA {nom_feuille} (Mdh)"
    ligne = 1
    ligne = ecrire_tableau(ws, ligne, f"TRIMESTRIEL — Cible + {len(ind_trim)} indicateurs",
                             HEADER_FILL_T, cible_nom, brut['cible_retropolee'], ind_trim)
    ligne = ecrire_tableau(ws, ligne, f"MENSUEL — {len(ind_mens)} indicateurs (pas de cible, trimestrielle)",
                             HEADER_FILL_M, None, None, ind_mens)

    recap.append((nom_feuille, len(ind_trim), len(ind_mens)))
    print(f"{nom_feuille:30s} : {len(ind_trim):3d} trim. + {len(ind_mens):3d} mens.")

# ============================================================================
# Immobilier -- cas particulier, tout est trimestriel ici (credit + IPAI)
# ============================================================================
with open(f'{DOSSIER}/data/immobilier_brut.pkl', 'rb') as f:
    immo_brut = pickle.load(f)

COLS_CREDIT_IMMO = {5, 6, 71, 72}
credit_series = [ind for ind in immo_brut['indicateurs'] if ind['colonne'] in COLS_CREDIT_IMMO]
# credit est mensuel (colonnes 5,6,71,72 sont dans le bloc mensuel de la feuille source)
for c in credit_series: c['freq'] = 'mensuel'

ipai_etendu = pd.read_csv(f'{DOSSIER.replace("/phase2","")}/ipai_nouveaux/ipai_variations_propre_etendue.csv',
                            sep=';', encoding='utf-8-sig')
def trim_to_date(t):
    trim, an = t.split('-')
    return date(int(an), (int(trim[1])-1)*3+1, 1)

IPAI_RETENUES = [
    ("Professionnel","prix"), ("Foncier","prix"), ("Bureau","prix"),
    ("Résidentiel","transactions"), ("Local commercial","transactions"),
    ("Local commercial","prix"), ("Professionnel","transactions"),
    ("Foncier","transactions"), ("Bureau","transactions"),
]
ipai_indicateurs = []
for cat, ind in IPAI_RETENUES:
    sub = ipai_etendu[(ipai_etendu['categorie']==cat) & (ipai_etendu['indicateur']==ind)]
    serie = [(trim_to_date(row['trimestre']), row['variation_trimestrielle']) for _, row in sub.iterrows()]
    ipai_indicateurs.append(dict(nom=f"IPAI {cat} — {ind} (var. trim. %)", serie=serie, freq='trimestriel'))
for ind in immo_brut['indicateurs']:
    if 'prose' in ind['nom'].lower():
        ipai_indicateurs.append(dict(nom="IPAI Global — prix (série 1998–2017, var. trim. %)", serie=ind['serie'], freq='trimestriel'))

ws = wb.create_sheet('Immobilier')
ws.sheet_view.showGridLines = False
ligne = 1
ligne = ecrire_tableau(ws, ligne, f"TRIMESTRIEL — Cible + {len(ipai_indicateurs)} indicateurs (IPAI)",
                         HEADER_FILL_T, 'VA Immobilier (Mdh)', immo_brut['cible_retropolee'], ipai_indicateurs)
ligne = ecrire_tableau(ws, ligne, f"MENSUEL — {len(credit_series)} indicateurs (crédit)",
                         HEADER_FILL_M, None, None, credit_series)
recap.append(('Immobilier', len(ipai_indicateurs), len(credit_series)))
print(f"{'Immobilier':30s} : {len(ipai_indicateurs):3d} trim. + {len(credit_series):3d} mens.")

# ============================================================================
# Sommaire
# ============================================================================
ws = wb.create_sheet('Sommaire', 0)
ws.sheet_view.showGridLines = False
ws.merge_cells("A1:E1")
ws["A1"] = "GDPNow-Maroc — Classeur des 434 séries retenues (Phase 2)"
ws["A1"].font = Font(size=14, bold=True, color="FFFFFF")
ws["A1"].fill = HEADER_FILL_T
ws["A1"].alignment = Alignment(vertical="center", indent=1)
ws.row_dimensions[1].height = 22

headers = ["Branche", "Trimestriel", "Mensuel", "Total", "Feuille"]
for j, h in enumerate(headers):
    c = ws.cell(row=3, column=j+1, value=h)
    c.font = Font(bold=True, color="FFFFFF"); c.fill = HEADER_FILL_T; c.border = BORDER
for col, w in zip("ABCDE", [30,14,12,10,25]):
    ws.column_dimensions[col].width = w

total_t = total_m = 0
for i, (nom, nt, nm) in enumerate(recap):
    r = 4+i
    ws.cell(row=r, column=1, value=nom).border = BORDER
    ws.cell(row=r, column=2, value=nt).border = BORDER
    ws.cell(row=r, column=3, value=nm).border = BORDER
    ws.cell(row=r, column=4, value=nt+nm).border = BORDER
    ws.cell(row=r, column=5, value=nom[:31]).border = BORDER
    total_t += nt; total_m += nm

r = 4 + len(recap)
for col, val in zip([1,2,3,4], ["TOTAL", total_t, total_m, total_t+total_m]):
    cc = ws.cell(row=r, column=col, value=val)
    cc.font = Font(bold=True); cc.border = BORDER

r2 = r + 2
ws.cell(row=r2, column=1, value="Légende : bleu = tableau trimestriel (avec la cible VA) ; orange = tableau mensuel (sans cible)")
r3 = r2 + 1
ws.cell(row=r3, column=1, value="Branches sans indicateur disponible (AR(4) pur) : Services aux entreprises, Administration publique, Éducation-santé, Autres services")

wb.save('/mnt/user-data/outputs/GDPNow_Maroc_series_retenues_Phase2.xlsx')
print(f"\nTotal : {total_t} trimestriels + {total_m} mensuels = {total_t+total_m}")
