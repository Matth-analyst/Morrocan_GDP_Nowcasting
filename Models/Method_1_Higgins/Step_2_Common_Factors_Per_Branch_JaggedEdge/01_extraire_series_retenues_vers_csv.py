# -*- coding: utf-8 -*-
"""
01_extraire_series_retenues_vers_csv.py -- Etape 2b (Facteurs communs par
branche) : extrait, depuis le classeur final (avec deflation), les series
RETENUES uniquement (CIBLE + ANCRES + SELECTION N2 + SELECTION APRES
DEFLATION) -- en excluant explicitement le bloc NON SELECTIONNE -- et
ecrit un CSV par branche, format large (une colonne Date, une colonne
par serie), pret pour import direct en R (read.csv / readr::read_csv).

ENTREE : classeur_complet_avec_deflation.xlsx (meme dossier, ou chemin
         ajustable ci-dessous)
SORTIE : un fichier csv/<nom_branche>.csv par branche disposant d'au
         moins un indicateur retenu (12 fichiers, les 4 branches sans
         indicateur n'ont pas de CSV -- rien a extraire pour elles).
"""
from openpyxl import load_workbook
import pandas as pd
from pathlib import Path

FICHIER_CLASSEUR = 'classeur_complet_avec_deflation.xlsx'
DOSSIER_SORTIE = Path('csv')
DOSSIER_SORTIE.mkdir(exist_ok=True)

# Blocs a INCLURE (les series retenues) -- reconnus par leur titre de bloc
# (ligne 1 de chaque feuille), qui commence par un de ces prefixes.
PREFIXES_BLOCS_RETENUS = ('CIBLE', 'ANCRES', 'SÉLECTION N2', 'SÉLECTION APRÈS DÉFLATION')
# Bloc explicitement EXCLU : 'NON SÉLECTIONNÉ' (les 394 series ecartees)

def lire_feuille_branche(ws):
    """Repere tous les blocs d'une feuille (titre en ligne 1, colonne 'Date'
    en ligne 3), ne garde que ceux dont le titre commence par un prefixe
    retenu, et renvoie DEUX DataFrames distincts -- un pour les blocs
    Trimestriel (dont Cible, toujours trimestrielle), un pour les blocs
    Mensuel -- JAMAIS fusionnes sur une meme colonne de dates, pour eviter
    de melanger deux frequences differentes (erreur deja rencontree et
    corrigee sur le classeur Excel -- la meme regle s'applique ici)."""
    max_col = ws.max_column
    max_row = ws.max_row

    grille = [[None]*(max_col+1) for _ in range(max_row+1)]
    for r_idx, row in enumerate(ws.iter_rows(min_row=1, max_row=max_row, values_only=True), start=1):
        for c_idx, val in enumerate(row, start=1):
            grille[r_idx][c_idx] = val

    positions_date = [c for c in range(1, max_col+1) if grille[3][c] == 'Date']

    series_trim = {}
    series_mens = {}
    for col_date in positions_date:
        titre_bloc = grille[1][col_date]
        if titre_bloc is None:
            continue
        if not any(str(titre_bloc).startswith(p) for p in PREFIXES_BLOCS_RETENUS):
            continue

        # Determiner la frequence du bloc a partir de son titre (CIBLE est
        # toujours trimestrielle ; les autres blocs portent "Trim." ou "Mens."
        # dans leur titre, cf. construction du classeur)
        titre_str = str(titre_bloc)
        if titre_str.startswith('CIBLE') or 'Trim' in titre_str:
            cible_dict = series_trim
        elif 'Mens' in titre_str:
            cible_dict = series_mens
        else:
            # Cas des blocs "SELECTION APRES DEFLATION" sans suffixe explicite
            # -> determiner empiriquement via l'ecart median entre dates
            dates_test = [grille[r][col_date] for r in range(4, min(max_row+1, 20)) if grille[r][col_date]]
            if len(dates_test) >= 2:
                ecarts = [(dates_test[i+1]-dates_test[i]).days for i in range(len(dates_test)-1)]
                med = sorted(ecarts)[len(ecarts)//2] if ecarts else 90
                cible_dict = series_mens if med <= 40 else series_trim
            else:
                cible_dict = series_trim

        positions_suivantes = [c for c in positions_date if c > col_date]
        fin_bloc = min(positions_suivantes) if positions_suivantes else max_col + 1

        for c in range(col_date+1, fin_bloc):
            nom_serie = grille[3][c]
            if nom_serie is None:
                continue
            nom_col = nom_serie
            base, i = nom_col, 2
            while nom_col in cible_dict:
                nom_col = f"{base} ({i})"
                i += 1

            valeurs = {}
            for r in range(4, max_row+1):
                d = grille[r][col_date]
                v = grille[r][c]
                if d is not None and v is not None:
                    valeurs[d] = v
            if valeurs:
                cible_dict[nom_col] = valeurs

    df_trim = None
    df_mens = None
    if series_trim:
        df_trim = pd.DataFrame(series_trim).sort_index()
        df_trim.index.name = 'Date'
    if series_mens:
        df_mens = pd.DataFrame(series_mens).sort_index()
        df_mens.index.name = 'Date'
    return df_trim, df_mens

def nom_fichier_propre(nom_feuille):
    """Nom de fichier sans caracteres speciaux ni accents, pour compatibilite R."""
    remplacements = {'é':'e','è':'e','ê':'e','à':'a','ô':'o','î':'i','ç':'c',' ':'_'}
    n = nom_feuille
    for old, new in remplacements.items():
        n = n.replace(old, new)
    return n

def main():
    wb = load_workbook(FICHIER_CLASSEUR, read_only=True, data_only=True)
    recap = []
    for nom_feuille in wb.sheetnames:
        if nom_feuille == 'Sommaire':
            continue
        ws = wb[nom_feuille]
        df_trim, df_mens = lire_feuille_branche(ws)
        if df_trim is None and df_mens is None:
            print(f"{nom_feuille:28s} : aucune serie retenue (branche sans indicateur)")
            recap.append((nom_feuille, 0, 0, 0, 0))
            continue

        base = nom_fichier_propre(nom_feuille)
        n_trim = n_dates_trim = n_mens = n_dates_mens = 0
        if df_trim is not None:
            chemin = DOSSIER_SORTIE / f"{base}_trimestriel.csv"
            df_trim.to_csv(chemin, encoding='utf-8-sig')
            n_trim, n_dates_trim = len(df_trim.columns), len(df_trim)
            print(f"{nom_feuille:28s} [TRIM] : {n_trim:2d} colonnes (cible incluse), {n_dates_trim:4d} dates -> {chemin}")
        if df_mens is not None:
            chemin = DOSSIER_SORTIE / f"{base}_mensuel.csv"
            df_mens.to_csv(chemin, encoding='utf-8-sig')
            n_mens, n_dates_mens = len(df_mens.columns), len(df_mens)
            print(f"{nom_feuille:28s} [MENS] : {n_mens:2d} colonnes,               {n_dates_mens:4d} dates -> {chemin}")
        recap.append((nom_feuille, n_trim, n_dates_trim, n_mens, n_dates_mens))

    n_fichiers = sum(1 for _,nt,_,nm,_ in recap if nt>0) + sum(1 for _,_,_,nm,_ in recap if nm>0)
    print(f"\n{n_fichiers} fichiers CSV ecrits dans {DOSSIER_SORTIE}/")
    print("\nRAPPEL IMPORTANT : chaque branche produit JUSQU'A DEUX fichiers distincts")
    print("(_trimestriel.csv et _mensuel.csv) -- jamais fusionnes sur une meme colonne")
    print("de dates, pour ne pas melanger deux frequences differentes.")

if __name__ == '__main__':
    main()
