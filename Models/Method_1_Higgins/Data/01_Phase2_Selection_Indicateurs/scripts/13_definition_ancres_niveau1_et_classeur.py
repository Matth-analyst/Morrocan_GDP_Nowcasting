# -*- coding: utf-8 -*-
"""
construire_classeur_ancres_v2.py -- Classeur avec 22 ancres (2 par branche,
sauf cas particuliers), reflete la version 2 des justifications.
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
FILL_T = PatternFill("solid", fgColor="1F4E78")
FILL_M = PatternFill("solid", fgColor="C55A11")
THIN = Side(style="thin", color="BFBFBF")
BORDER = Border(left=THIN, right=THIN, top=THIN, bottom=THIN)
SEP = 2

wb = Workbook()
wb.remove(wb.active)

# Ancres v2 : liste de specs par branche (colonne prioritaire, sinon nom_match)
ANCRES = {
    'agriculture':       [{'colonne': None, 'nom_match': 'Moyenne des precipitations'},
                            {'colonne': None, 'nom_match': 'Température_moyenne'}],
    'peche':              [{'colonne': 21, 'nom_match': None},   # POISSON PELAGIQUE, quantite
                            {'colonne': 17, 'nom_match': None}],  # CEPHALOPODES, valeur
    'industrie_transfo':  [{'colonne': None, 'nom_match': 'Taux d’Utilisation des Capacités'},
                            {'colonne': None, 'nom_match': 'IPI — Industries alimentaires'}],
    'ind_extraction':     [{'colonne': 28, 'nom_match': None},   # Phosphates, production
                            {'colonne': None, 'nom_match': 'IPM — Industries extractives'}],
    'immobilier':         [{'colonne': None, 'nom_match': "Crédits à l’immobilier"}],  # + IPAI ajoute a part
    'finances':           [{'colonne': None, 'nom_match': 'Masse monétaire (M3)'},
                            {'colonne': 5, 'nom_match': None}],   # Credit bancaire, mensuel global
    'hebergement':        [{'colonne': None, 'nom_match': 'Recettes touristiques'},
                            {'colonne': 5, 'nom_match': None}],   # Arrivees touristes etrangers
    'construction':       [{'colonne': None, 'nom_match': 'Vente de ciment'},
                            {'colonne': None, 'nom_match': 'Crédit bancaire Bâtiment'}],
    'commerce':           [{'colonne': None, 'nom_match': 'TOTAL EXPORTATIONS'},
                            {'colonne': None, 'nom_match': 'TOTAL IMPORTATIONS'}],
    'transports':         [{'colonne': None, 'nom_match': "Trafic globale gérés par l'ANP"},
                            {'colonne': None, 'nom_match': 'Trafic aérien'}],
    'electricite':        [{'colonne': 100, 'nom_match': None},   # Production electricite
                            {'colonne': 101, 'nom_match': None}], # Consommation combustibles
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

    cell = ws.cell(row=3, column=col_depart, value="Date")
    cell.font = Font(bold=True, color="FFFFFF"); cell.fill = fill; cell.border = BORDER
    ws.column_dimensions[get_column_letter(col_depart)].width = 12
    for j, col in enumerate(df.columns):
        cc = col_depart + 1 + j
        cell = ws.cell(row=3, column=cc, value=col)
        cell.font = Font(bold=True, size=9, color="FFFFFF"); cell.fill = fill
        cell.alignment = Alignment(horizontal="center", wrap_text=True, vertical="center")
        cell.border = BORDER
        ws.column_dimensions[get_column_letter(cc)].width = 15
    ws.row_dimensions[3].height = 55

    for i, (d, row) in enumerate(df.iterrows()):
        r = 4 + i
        c0 = ws.cell(row=r, column=col_depart, value=d)
        c0.number_format = "YYYY-MM-DD"; c0.border = BORDER; c0.font = Font(size=9)
        for j, col in enumerate(df.columns):
            cc = col_depart + 1 + j; val = row[col]
            cell = ws.cell(row=r, column=cc, value=round(float(val),4) if pd.notna(val) else None)
            if pd.notna(val): cell.number_format = "#,##0.0000"
            cell.border = BORDER; cell.font = Font(size=9)
    return col_depart + 1 + n_col + SEP

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

    ancres = trouver_ancres(indicateurs, ANCRES[prefix])
    ancres_ids = set(id(a) for a in ancres)
    reste = [i for i in indicateurs if id(i) not in ancres_ids]
    ind_trim = [i for i in reste if i['freq']=='trimestriel']
    ind_mens = [i for i in reste if i['freq']=='mensuel']

    ws = wb.create_sheet(nom_feuille[:31])
    ws.sheet_view.showGridLines = False
    col = 1
    col = ecrire_bloc(ws, col, "CIBLE", FILL_CIBLE,
                        construire_dict_series([{'nom':f"VA {nom_feuille} (Mdh)",'serie':brut['cible_retropolee']}]))
    col = ecrire_bloc(ws, col, f"ANCRES ({len(ancres)})", FILL_ANCRE, construire_dict_series(ancres))
    col = ecrire_bloc(ws, col, f"TRIMESTRIEL ({len(ind_trim)})", FILL_T, construire_dict_series(ind_trim))
    col = ecrire_bloc(ws, col, f"MENSUEL ({len(ind_mens)})", FILL_M, construire_dict_series(ind_mens))
    ws.freeze_panes = "A4"

    recap.append((nom_feuille, len(ancres), len(ind_trim), len(ind_mens)))
    print(f"{nom_feuille:28s} : {len(ancres)} ancre(s) -> {[a['nom'][:35] for a in ancres]}")

# --- Immobilier : cas particulier, ancre credit (mensuel) + ancre IPAI (trimestriel) ---
with open(f'{DOSSIER}/data/immobilier_brut.pkl', 'rb') as f:
    immo_brut = pickle.load(f)
COLS_CREDIT_IMMO = {5, 6, 71, 72}
credit_series = [ind for ind in immo_brut['indicateurs'] if ind['colonne'] in COLS_CREDIT_IMMO]
ipai_etendu = pd.read_csv(f'{DOSSIER.replace("/phase2","")}/ipai_nouveaux/ipai_variations_propre_etendue.csv', sep=';', encoding='utf-8-sig')
def trim_to_date(t):
    trim, an = t.split('-'); return date(int(an), (int(trim[1])-1)*3+1, 1)
IPAI_RETENUES = [("Professionnel","prix"), ("Foncier","prix"), ("Bureau","prix"),
    ("Résidentiel","transactions"), ("Local commercial","transactions"), ("Local commercial","prix"),
    ("Professionnel","transactions"), ("Foncier","transactions"), ("Bureau","transactions")]
ipai_indicateurs = []
for cat, ind in IPAI_RETENUES:
    sub = ipai_etendu[(ipai_etendu['categorie']==cat) & (ipai_etendu['indicateur']==ind)]
    serie = [(trim_to_date(row['trimestre']), row['variation_trimestrielle']) for _, row in sub.iterrows()]
    nom = f"IPAI {cat} — {ind} (var. trim. %)"
    ipai_indicateurs.append(dict(nom=nom, serie=serie))
for ind in immo_brut['indicateurs']:
    if 'prose' in ind['nom'].lower():
        ipai_indicateurs.append(dict(nom="IPAI Global — prix (série 1998–2017, var. trim. %)", serie=ind['serie']))

ancre_credit = [c for c in credit_series if 'immobilier' in c['nom'].lower()][:1]
credit_reste = [c for c in credit_series if c not in ancre_credit]
ancre_ipai = [i for i in ipai_indicateurs if 'Résidentiel' in i['nom'] and 'transactions' in i['nom']]
ipai_reste = [i for i in ipai_indicateurs if i not in ancre_ipai]

toutes_ancres_immo = ancre_credit + ancre_ipai

ws = wb.create_sheet('Immobilier')
ws.sheet_view.showGridLines = False
col = 1
col = ecrire_bloc(ws, col, "CIBLE", FILL_CIBLE, construire_dict_series([{'nom':'VA Immobilier (Mdh)','serie':immo_brut['cible_retropolee']}]))
col = ecrire_bloc(ws, col, f"ANCRES ({len(toutes_ancres_immo)})", FILL_ANCRE, construire_dict_series(toutes_ancres_immo))
col = ecrire_bloc(ws, col, f"TRIMESTRIEL - IPAI ({len(ipai_reste)})", FILL_T, construire_dict_series(ipai_reste))
col = ecrire_bloc(ws, col, f"MENSUEL - CREDIT ({len(credit_reste)})", FILL_M, construire_dict_series(credit_reste))
ws.freeze_panes = "A4"
recap.append(('Immobilier', len(toutes_ancres_immo), len(ipai_reste), len(credit_reste)))
print(f"{'Immobilier':28s} : {len(toutes_ancres_immo)} ancre(s) -> {[a['nom'][:35] for a in toutes_ancres_immo]}")

# --- Sommaire ---
ws = wb.create_sheet('Sommaire', 0)
ws.sheet_view.showGridLines = False
ws.merge_cells("A1:F1")
ws["A1"] = "GDPNow-Maroc — 434 séries retenues, 22 ancres (Phase 2, v2)"
ws["A1"].font = Font(size=13, bold=True, color="FFFFFF"); ws["A1"].fill = FILL_T
ws["A1"].alignment = Alignment(vertical="center", indent=1); ws.row_dimensions[1].height = 22

headers = ["Branche", "Ancres", "Trimestriel (reste)", "Mensuel (reste)", "Total", "Feuille"]
for j, h in enumerate(headers):
    c = ws.cell(row=3, column=j+1, value=h)
    c.font = Font(bold=True, color="FFFFFF"); c.fill = FILL_T; c.border = BORDER
for col_l, w in zip("ABCDEF", [28,10,16,14,10,25]):
    ws.column_dimensions[col_l].width = w

tot_a=tot_t=tot_m=0
for i, (nom, na, nt, nm) in enumerate(recap):
    r=4+i
    ws.cell(row=r,column=1,value=nom).border=BORDER
    ws.cell(row=r,column=2,value=na).border=BORDER
    ws.cell(row=r,column=3,value=nt).border=BORDER
    ws.cell(row=r,column=4,value=nm).border=BORDER
    ws.cell(row=r,column=5,value=na+nt+nm).border=BORDER
    ws.cell(row=r,column=6,value=nom[:31]).border=BORDER
    tot_a+=na; tot_t+=nt; tot_m+=nm
r=4+len(recap)
for c_idx,val in zip([1,2,3,4,5],["TOTAL",tot_a,tot_t,tot_m,tot_a+tot_t+tot_m]):
    cc=ws.cell(row=r,column=c_idx,value=val); cc.font=Font(bold=True); cc.border=BORDER

r2=r+2
ws.cell(row=r2,column=1,value="Légende : vert=cible | or=ANCRES (2 par branche, a priori, non corrigées FDR) | bleu=trimestriel restant | orange=mensuel restant")
ws.cell(row=r2+1,column=1,value="Branches sans indicateur (AR(4) pur) : Services aux entreprises, Administration publique, Éducation-santé, Autres services")

wb.save('/mnt/user-data/outputs/GDPNow_Maroc_series_avec_ancres_v2.xlsx')
print(f"\nTotal ancres: {tot_a}  |  reste trim: {tot_t}  |  reste mens: {tot_m}  |  TOTAL: {tot_a+tot_t+tot_m}")
