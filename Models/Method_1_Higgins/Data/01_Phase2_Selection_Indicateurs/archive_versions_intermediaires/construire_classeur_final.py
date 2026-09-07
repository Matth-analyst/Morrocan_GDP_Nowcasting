# -*- coding: utf-8 -*-
"""
construire_classeur_final.py -- Consolide les 434 series retenues (Phase 2)
dans un classeur Excel unique, une feuille par branche, fidele aux donnees
extraites (pas de transformation, pas de Delta-log -- les valeurs brutes
telles que collectees).
"""
import pickle
import pandas as pd
from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter
from datetime import date

DOSSIER = '/home/claude/phase2'
HEADER_FILL = PatternFill("solid", fgColor="1F4E78")
THIN = Side(style="thin", color="BFBFBF")
BORDER = Border(left=THIN, right=THIN, top=THIN, bottom=THIN)

wb = Workbook()
wb.remove(wb.active)  # on retire la feuille par defaut

# ============================================================================
# Fonction generique d'ecriture d'une feuille "large" (dates en lignes,
# series en colonnes) a partir d'une liste de series (nom, liste de (date,valeur))
# ============================================================================
def ecrire_feuille(nom_feuille, cible_nom, cible_serie, indicateurs):
    ws = wb.create_sheet(nom_feuille[:31])  # Excel limite les noms a 31 caracteres
    ws.sheet_view.showGridLines = False

    # Construire un DataFrame large : index = dates, colonnes = cible + indicateurs
    toutes_series = {cible_nom: dict(cible_serie)}
    for ind in indicateurs:
        # eviter les collisions de noms de colonnes
        nom_col = ind['nom']
        base = nom_col
        i = 2
        while nom_col in toutes_series:
            nom_col = f"{base} ({i})"
            i += 1
        toutes_series[nom_col] = dict(ind['serie'])

    df = pd.DataFrame(toutes_series).sort_index()

    # Titre
    ws.merge_cells(f"A1:{get_column_letter(len(df.columns)+1)}1")
    ws["A1"] = f"{nom_feuille} — Cible + {len(indicateurs)} indicateurs retenus (Phase 2)"
    ws["A1"].font = Font(size=12, bold=True, color="FFFFFF")
    ws["A1"].fill = HEADER_FILL
    ws["A1"].alignment = Alignment(vertical="center", indent=1)
    ws.row_dimensions[1].height = 20

    # En-tetes (ligne 3)
    ws.cell(row=3, column=1, value="Date").font = Font(bold=True, color="FFFFFF")
    ws.cell(row=3, column=1).fill = HEADER_FILL
    ws.cell(row=3, column=1).border = BORDER
    ws.column_dimensions["A"].width = 12

    for j, col in enumerate(df.columns):
        c = j + 2
        cell = ws.cell(row=3, column=c, value=col)
        cell.font = Font(bold=True, size=9, color="FFFFFF")
        cell.fill = HEADER_FILL
        cell.alignment = Alignment(horizontal="center", wrap_text=True, vertical="center")
        cell.border = BORDER
        ws.column_dimensions[get_column_letter(c)].width = 16
    ws.row_dimensions[3].height = 60

    # Donnees
    for i, (d, row) in enumerate(df.iterrows()):
        r = 4 + i
        c0 = ws.cell(row=r, column=1, value=d if isinstance(d, date) else d)
        c0.number_format = "YYYY-MM-DD"
        c0.border = BORDER
        c0.font = Font(size=9)
        for j, col in enumerate(df.columns):
            val = row[col]
            if pd.notna(val):
                cell = ws.cell(row=r, column=j+2, value=round(float(val), 4))
                cell.number_format = "#,##0.0000"
            else:
                cell = ws.cell(row=r, column=j+2, value=None)
            cell.border = BORDER
            cell.font = Font(size=9)

    ws.freeze_panes = "B4"
    return len(df)

# ============================================================================
# Les 11 branches "standard" (target dans le _brut.pkl, indicateurs dans _sans_fraicheur.pkl)
# ============================================================================
BRANCHES_STANDARD = [
    ('Agriculture', 'agriculture'),
    ('Peche', 'peche'),
    ('Industrie transformation', 'industrie_transfo'),
    ('Industrie extraction', 'ind_extraction'),
    ('Finances assurances', 'finances'),
    ('Hebergement restauration', 'hebergement'),
    ('Construction', 'construction'),
    ('Commerce', 'commerce'),
    ('Transports', 'transports'),
    ('Electricite gaz eau', 'electricite'),
    ('Information communication', 'infocom'),
]

recap = []

for nom_feuille, prefix in BRANCHES_STANDARD:
    with open(f'{DOSSIER}/data/{prefix}_brut.pkl', 'rb') as f:
        brut = pickle.load(f)
    with open(f'{DOSSIER}/resultats/{prefix}_sans_fraicheur.pkl', 'rb') as f:
        indicateurs = pickle.load(f)

    cible_nom = f"VA {nom_feuille} (Mdh)"
    n_lignes = ecrire_feuille(nom_feuille, cible_nom, brut['cible_retropolee'], indicateurs)
    recap.append((nom_feuille, len(indicateurs), n_lignes))
    print(f"{nom_feuille:30s} : {len(indicateurs):3d} indicateurs, {n_lignes} lignes de dates")

# ============================================================================
# Immobilier -- cas particulier : 4 credit + 10 IPAI reconstruits
# ============================================================================
with open(f'{DOSSIER}/data/immobilier_brut.pkl', 'rb') as f:
    immo_brut = pickle.load(f)

# Les 4 series de credit (colonnes 5,6,71,72)
COLS_CREDIT_IMMO = {5, 6, 71, 72}
credit_series = [ind for ind in immo_brut['indicateurs'] if ind['colonne'] in COLS_CREDIT_IMMO]

# Les 10 categories IPAI retenues (seuil dedie 65%), reconstruites depuis le
# fichier etendu (avec les 10 bulletins recemment extraits)
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
# "Global prix (prose, a verifier)" (serie 1998-2017) traitee separement (nom different)

ipai_indicateurs = []
for cat, ind in IPAI_RETENUES:
    sub = ipai_etendu[(ipai_etendu['categorie']==cat) & (ipai_etendu['indicateur']==ind)]
    serie = [(trim_to_date(row['trimestre']), row['variation_trimestrielle']) for _, row in sub.iterrows()]
    ipai_indicateurs.append(dict(nom=f"IPAI {cat} — {ind} (var. trim. %)", serie=serie))

# La serie "Global prix (prose, a verifier)" -- ancienne serie 1998-2017, deja dans immo_brut
for ind in immo_brut['indicateurs']:
    if 'prose' in ind['nom'].lower():
        ipai_indicateurs.append(dict(nom="IPAI Global — prix (série 1998–2017, var. trim. %)", serie=ind['serie']))

tous_indicateurs_immo = credit_series + ipai_indicateurs
n_lignes = ecrire_feuille('Immobilier', 'VA Immobilier (Mdh)', immo_brut['cible_retropolee'], tous_indicateurs_immo)
recap.append(('Immobilier', len(tous_indicateurs_immo), n_lignes))
print(f"{'Immobilier':30s} : {len(tous_indicateurs_immo):3d} indicateurs, {n_lignes} lignes de dates")

# ============================================================================
# Feuille Sommaire
# ============================================================================
ws = wb.create_sheet('Sommaire', 0)
ws.sheet_view.showGridLines = False
ws.merge_cells("A1:D1")
ws["A1"] = "GDPNow-Maroc — Classeur des 434 séries retenues (Phase 2)"
ws["A1"].font = Font(size=14, bold=True, color="FFFFFF")
ws["A1"].fill = HEADER_FILL
ws["A1"].alignment = Alignment(vertical="center", indent=1)
ws.row_dimensions[1].height = 22

headers = ["Branche", "Nb indicateurs retenus", "Nb dates (lignes)", "Feuille"]
for j, h in enumerate(headers):
    c = ws.cell(row=3, column=j+1, value=h)
    c.font = Font(bold=True, color="FFFFFF")
    c.fill = HEADER_FILL
    c.border = BORDER
for col, w in zip("ABCD", [30,22,18,25]):
    ws.column_dimensions[col].width = w

total_ind = 0
for i, (nom, n_ind, n_lignes) in enumerate(recap):
    r = 4+i
    ws.cell(row=r, column=1, value=nom).border = BORDER
    ws.cell(row=r, column=2, value=n_ind).border = BORDER
    ws.cell(row=r, column=3, value=n_lignes).border = BORDER
    ws.cell(row=r, column=4, value=nom[:31]).border = BORDER
    total_ind += n_ind

r = 4 + len(recap)
ws.cell(row=r, column=1, value="TOTAL").font = Font(bold=True)
ws.cell(row=r, column=1).border = BORDER
ws.cell(row=r, column=2, value=total_ind).font = Font(bold=True)
ws.cell(row=r, column=2).border = BORDER

# Note sur les 4 branches sans donnees
r2 = r + 2
ws.cell(row=r2, column=1, value="Branches sans indicateur disponible (AR(4) pur) :")
ws.cell(row=r2+1, column=1, value="Services aux entreprises, Administration publique, Éducation-santé, Autres services")

wb.save('/mnt/user-data/outputs/GDPNow_Maroc_series_retenues_Phase2.xlsx')
print(f"\nTotal indicateurs consolides : {total_ind}")
print("Classeur enregistre.")
