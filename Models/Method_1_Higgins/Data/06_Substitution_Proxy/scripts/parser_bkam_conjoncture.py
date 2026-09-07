# -*- coding: utf-8 -*-
"""
parser_bkam_conjoncture.py -- Extraction complete des 213 bulletins BAM
"Enquete mensuelle de conjoncture dans l'industrie" (2009-2026), via
pdfplumber (vraie structure de tableau, pas texte brut).

Extrait, pour chaque bulletin :
- Les resultats globaux (Tableau "Resultats globaux") : 7-9 questions,
  solde d'opinion (SO), niveau national
- Les resultats par branche (Tableau "Resultats par branche") : memes
  questions, 4 ou 5 branches selon la periode (Electrique-electronique
  abandonnee entre 2023 et 2026 -- gere dynamiquement)
"""
import pdfplumber
import re
from pathlib import Path
from datetime import date
import pickle

DOSSIER = Path('/home/claude/bkam_bulletins/Enquete_mensuel_indu')

MOIS_FR = {'janvier':1,'février':2,'fevrier':2,'mars':3,'avril':4,'mai':5,'juin':6,
           'juillet':7,'août':8,'aout':8,'septembre':9,'octobre':10,'novembre':11,'décembre':12,'decembre':12}
NOMS_MOIS_CAPITALISES = [m.capitalize() for m in MOIS_FR.keys()]

QUESTIONS_GLOBALES_T0 = [
    "Production (courant)", "Production (anticipé 3 mois)", "Nouvelles commandes",
    "Ventes (courant)", "Ventes (anticipé 3 mois)",
    "Prix produits finis (courant)", "Prix produits finis (anticipé 3 mois)"
]
QUESTIONS_GLOBALES_T1 = ["Carnets de commandes", "Stocks de produits finis"]

def parse_nom_fichier(nom):
    """Extrait (annee, mois) depuis le nom de fichier, tolerant aux variantes."""
    nom_low = nom.lower()
    # Format abrege type "M0116" ou "M042016" = mois(2 chiffres) + annee(2 ou 4 chiffres)
    m_abrege = re.search(r'm(\d{2})[_\-]?(\d{2,4})', nom_low)
    if m_abrege:
        mois_num = int(m_abrege.group(1))
        an_str = m_abrege.group(2)
        if 1 <= mois_num <= 12:
            annee = int(an_str) if len(an_str)==4 else 2000+int(an_str)
            return annee, mois_num
    # Format "082016" = mois(2)+annee(4) colles
    m_colle = re.search(r'(?<!\d)(\d{2})(\d{4})(?!\d)', nom_low)
    if m_colle:
        mois_num = int(m_colle.group(1))
        annee = int(m_colle.group(2))
        if 1 <= mois_num <= 12 and 2005 <= annee <= 2027:
            return annee, mois_num
    m = re.search(r'(\d{4})', nom_low)
    annee = int(m.group(1)) if m else None
    mois = None
    for mf, num in MOIS_FR.items():
        if mf in nom_low:
            mois = num
            break
    return annee, mois

def parser_cellule_multiligne(cellule, n_attendu):
    """Une cellule pdfplumber multi-ligne '34\\n22\\n25' -> liste de floats."""
    if cellule is None:
        return [None] * n_attendu
    lignes = [l.strip() for l in cellule.split('\n') if l.strip() != '']
    valeurs = []
    for l in lignes:
        try:
            valeurs.append(float(l.replace(',', '.')))
        except ValueError:
            valeurs.append(None)
    while len(valeurs) < n_attendu:
        valeurs.append(None)
    return valeurs[:n_attendu]

def extraire_bulletin(chemin_pdf):
    annee, mois = parse_nom_fichier(chemin_pdf.name)
    if annee is None or mois is None:
        return None
    resultat = dict(annee=annee, mois=mois, fichier=chemin_pdf.name,
                     globaux={}, par_branche={})

    try:
        with pdfplumber.open(chemin_pdf) as pdf:
            for page in pdf.pages:
                tables = page.extract_tables()
                for t in tables:
                    if not t or len(t) < 3:
                        continue
                    header_branches = [c for c in t[0] if c]
                    ligne_labels = t[2][0] if t[2] and t[2][0] else ""
                    n_questions = ligne_labels.count('\n') + 1 if ligne_labels else 0

                    # Tableau GLOBAL (pas d'en-tete de branche, juste Hausse/Stagnation/...)
                    if not header_branches or 'Part des répondants' in str(t[0][1] if len(t[0])>1 else ''):
                        continue
                    if header_branches and any(str(header_branches[0]).lower().startswith(m) for m in MOIS_FR.keys()):
                        # Tableau global : colonnes = [mois_prec: H,S,B,PV,SO] [mois_cour: H,S,B,PV,SO]
                        if len(t) < 3: continue
                        row_data = t[-1]
                        if not row_data or not row_data[0]:
                            continue
                        qlabels = [l.strip() for l in row_data[0].split('\n') if l.strip()]
                        n_q = len(qlabels)
                        # colonne SO du mois courant = position 10 (5eme bloc, si 2 mois x 5 cols)
                        idx_so_courant = 10 if len(row_data) > 10 else (5 if len(row_data)>5 else None)
                        if idx_so_courant is None or idx_so_courant >= len(row_data):
                            continue
                        so_vals = parser_cellule_multiligne(row_data[idx_so_courant], n_q)
                        noms_q = QUESTIONS_GLOBALES_T0 if n_q == 7 else (QUESTIONS_GLOBALES_T1 if n_q==2 else [f"Q{i}" for i in range(n_q)])
                        for nom_q, val in zip(noms_q, so_vals):
                            resultat['globaux'][nom_q] = val
                        continue

                    # Tableau PAR BRANCHE : en-tete = noms de branches repetes 5x chacun
                    branches_uniques = []
                    for b in header_branches:
                        bn = b.replace('\n',' ').strip()
                        if bn not in branches_uniques:
                            branches_uniques.append(bn)
                    if not branches_uniques or 'Industries' not in str(branches_uniques[0]):
                        continue
                    row_data = t[-1]
                    if not row_data or not row_data[0]:
                        continue
                    qlabels = [l.strip() for l in row_data[0].split('\n') if l.strip()]
                    n_q = len(qlabels)
                    noms_q = QUESTIONS_GLOBALES_T0 if n_q == 7 else (QUESTIONS_GLOBALES_T1 if n_q==2 else [f"Q{i}" for i in range(n_q)])

                    n_branches = len(branches_uniques)
                    for bi, branche in enumerate(branches_uniques):
                        idx_so = bi*5 + 5  # position SO dans le bloc de 5 (H,S,B,PV,SO) de cette branche
                        if idx_so >= len(row_data):
                            continue
                        so_vals = parser_cellule_multiligne(row_data[idx_so], n_q)
                        if branche not in resultat['par_branche']:
                            resultat['par_branche'][branche] = {}
                        for nom_q, val in zip(noms_q, so_vals):
                            resultat['par_branche'][branche][nom_q] = val
    except Exception as e:
        resultat['erreur'] = str(e)

    return resultat

# ============================================================================
# Extraction sur les 213 bulletins
# ============================================================================
fichiers = sorted(DOSSIER.glob('*.pdf'))
print(f"{len(fichiers)} fichiers PDF trouves")

tous_bulletins = []
erreurs = []
for f in fichiers:
    r = extraire_bulletin(f)
    if r is None:
        erreurs.append((f.name, "date non reconnue"))
        continue
    if 'erreur' in r:
        erreurs.append((f.name, r['erreur']))
        continue
    if not r['globaux'] and not r['par_branche']:
        erreurs.append((f.name, "aucune donnee extraite"))
        continue
    tous_bulletins.append(r)

print(f"\n{len(tous_bulletins)} bulletins extraits avec succes")
print(f"{len(erreurs)} echecs")
for nom, err in erreurs[:15]:
    print(f"  [ECHEC] {nom}: {err}")

with open('/home/claude/bkam_bulletins/extraction/bulletins_extraits.pkl', 'wb') as f:
    pickle.dump(dict(bulletins=tous_bulletins, erreurs=erreurs), f)
print("\nSauvegarde : bulletins_extraits.pkl")
