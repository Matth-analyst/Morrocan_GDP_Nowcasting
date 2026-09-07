# -*- coding: utf-8 -*-
"""
00_deflater_toutes_series_monetaires.py -- Corrige une incoherence trouvee
apres coup : seules 5 series (recuperees par le test statistique post-
deflation) avaient ete effectivement deflatees dans le classeur -- les 24
ancres et les 16 series de la selection Niveau 2 d'origine, quand elles
sont monetaires, etaient restees en valeur NOMINALE.

Ce script scanne csv/*.csv, identifie precisement (liste verifiee
manuellement colonne par colonne, PAS un simple mot-cle) les series
monetaires encore nominales, les deflate avec l'IPC reconstruit, et ecrit
le resultat dans csv_avec_series_deflatees/ -- csv/ original n'est jamais
modifie (conserve comme reference "sans deflation").

Regles :
  - La cible (VA, colonne "VA <branche> (Mdh)") n'est JAMAIS deflatee --
    c'est deja une serie en VOLUME (chainee), donc deja reelle par
    construction.
  - Les colonnes deja marquees "(deflate, reel)" sont laissees telles
    quelles (deja traitees, ne pas re-deflater).
  - Les colonnes de la liste ci-dessous (verifiees individuellement,
    branche par branche, contre le classeur source) sont deflatees.
  - Toutes les autres colonnes (volumes, indices, %, comptages) restent
    inchangees -- deflater une serie qui n'est pas monetaire n'a pas de
    sens economique.
"""
import pandas as pd
from pathlib import Path
import glob
import re

DOSSIER_CSV_SOURCE = Path("csv")
DOSSIER_CSV_SORTIE = Path("csv_avec_series_deflatees")
FICHIER_IPC = "ipc_maroc_2010_2026_raccorde.csv"  # a placer au meme niveau

DOSSIER_CSV_SORTIE.mkdir(exist_ok=True)

# ------------------------------------------------------------------------------
# Liste verifiee, colonne par colonne, des series monetaires ENCORE
# NOMINALES a deflater (etablie par verification directe contre le
# classeur source -- pas par mot-cle, plusieurs series monetaires
# n'ayant ni "MDH" ni "dirham" dans leur nom, ex. "TOTAL EXPORTATIONS (1000)")
# ------------------------------------------------------------------------------
SERIES_A_DEFLATER = {
    # (fichier_csv, nom_exact_de_colonne)
    ("Peche_mensuel.csv", "CEPHALOPODES"),
    ("Finances_assurances_mensuel.csv", "Masse monétaire (M3)"),
    ("Finances_assurances_mensuel.csv", "Crédit bancaire"),
    ("Finances_assurances_mensuel.csv", "Secteur privé"),
    ("Finances_assurances_mensuel.csv", "Sociétés non financières privées"),
    ("Hebergement_restauration_mensuel.csv", "Recettes touristiques (mensuel (En millions de dirhams))"),
    ("Construction_trimestriel.csv", "Crédit bancaire Bâtiment et travaux publics (MDH)"),
    ("Commerce_mensuel.csv", "TOTAL EXPORTATIONS (1000)"),
    ("Commerce_mensuel.csv", "TOTAL IMPORTATIONS  (1000)"),
    ("Immobilier_mensuel.csv", "Crédits à l’immobilier \n (MDH) (Jan 18)"),
}

def charger_ipc():
    df = pd.read_csv(FICHIER_IPC)
    df["date"] = pd.to_datetime(df["date"])
    df = df.set_index("date")["IPC_base2017"]
    return df

def deflater_colonne(dates, valeurs, ipc):
    """x_reel_t = x_nominal_t / (IPC_t / 100), IPC aligne au 1er du mois."""
    resultat = []
    for d, v in zip(dates, valeurs):
        if pd.isna(v):
            resultat.append(None)
            continue
        d_mois = pd.Timestamp(d).replace(day=1)
        if d_mois in ipc.index:
            resultat.append(v / (ipc.loc[d_mois] / 100))
        else:
            resultat.append(None)  # hors couverture IPC (avant 2010 ou apres la derniere valeur)
    return resultat

def main():
    ipc = charger_ipc()
    print(f"IPC charge : {len(ipc)} points, {ipc.index.min().date()} a {ipc.index.max().date()}\n")

    fichiers = sorted(DOSSIER_CSV_SOURCE.glob("*.csv"))
    colonnes_deflatees_log = []

    for fichier in fichiers:
        df = pd.read_csv(fichier, encoding="utf-8-sig")
        df["Date"] = pd.to_datetime(df["Date"])
        nb_deflatees_ce_fichier = 0

        for col in df.columns[1:]:
            cle = (fichier.name, col)

            # Deja deflatee dans une etape precedente -- ne pas re-traiter
            if "déflaté" in col.lower() or "deflate" in col.lower():
                continue
            # La cible (VA) n'est jamais deflatee -- deja en volume
            if col.startswith("VA "):
                continue

            if cle in SERIES_A_DEFLATER:
                df[col] = deflater_colonne(df["Date"], df[col], ipc)
                nb_deflatees_ce_fichier += 1
                colonnes_deflatees_log.append((fichier.name, col))

        chemin_sortie = DOSSIER_CSV_SORTIE / fichier.name
        df.to_csv(chemin_sortie, index=False, encoding="utf-8-sig")
        marque = f" <- {nb_deflatees_ce_fichier} colonne(s) deflatee(s)" if nb_deflatees_ce_fichier else ""
        print(f"{fichier.name:45s}{marque}")

    print(f"\n{len(colonnes_deflatees_log)} colonnes deflatees au total, sur {len(SERIES_A_DEFLATER)} attendues :")
    for f, c in colonnes_deflatees_log:
        print(f"  {f} :: {c}")

    manquantes = SERIES_A_DEFLATER - set(colonnes_deflatees_log)
    if manquantes:
        print(f"\n/!\\ ATTENTION -- {len(manquantes)} colonne(s) attendue(s) non trouvee(s) (verifier le nom exact) :")
        for f, c in manquantes:
            print(f"  {f} :: {c}")
    else:
        print("\nToutes les colonnes attendues ont ete trouvees et deflatees. Aucune manquante.")

if __name__ == "__main__":
    main()
