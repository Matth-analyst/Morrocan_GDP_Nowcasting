# -*- coding: utf-8 -*-
"""
Convertit les CSV Manar-Stat (un fichier par tableau, periodes en colonnes) en
un panel unique au format long, avec dates normalisees.

Sortie : manar_panel.csv
    secteur ; rubrique ; tableau ; serie ; niveau ; frequence ; periode ;
    date_debut ; date_fin ; annee ; cumul ; valeur

Conventions retenues
  - Annuelle          2019        -> 2019-01-01 .. 2019-12-31
  - Trimestrielle     2019T2      -> 2019-04-01 .. 2019-06-30
  - Mensuelle         2019M04     -> 2019-04-01 .. 2019-04-30
  - Semestrielle      Semestre1-2019
  - Annee a cheval    2019/2020   -> campagne agricole, 2019-09-01 .. 2020-08-31
                      80/81       -> 1980/1981 (2 chiffres = 19xx)
  - Cumuls            Trim1:Trim3-2019, Janv:Juin-2019 : date_debut au debut de
                      la periode cumulee, date_fin a la fin, colonne cumul=oui

`annee` est l'annee de rattachement (pour une campagne : l'annee de recolte,
c'est-a-dire la seconde).

    python harmoniser.py
"""

import calendar
import csv
import os
import re
import sys
from datetime import date

ROOT = os.path.dirname(os.path.abspath(__file__))
DOSSIERS = ["manar_primaire", "manar_secondaire", "manar_tertiaire"]

MOIS_CUM = {"janv": 1, "fev": 2, "mars": 3, "avr": 4, "mai": 5, "juin": 6,
            "juil": 7, "aout": 8, "sep": 9, "oct": 10, "nov": 11, "dec": 12}


def fin_de_mois(a, m):
    return date(a, m, calendar.monthrange(a, m)[1])


def an4(txt):
    """'80' -> 1980, '2019' -> 2019."""
    n = int(txt)
    return n + 1900 if len(txt) == 2 else n


def analyser_periode(p):
    """Renvoie (frequence, date_debut, date_fin, annee, cumul) ou None."""
    p = p.strip()

    m = re.fullmatch(r"(\d{4})T([1-4])", p)
    if m:
        a, t = int(m.group(1)), int(m.group(2))
        return ("Trimestrielle", date(a, 3 * t - 2, 1), fin_de_mois(a, 3 * t),
                a, "non")

    m = re.fullmatch(r"(\d{4})M(\d{2})", p)
    if m:
        a, mo = int(m.group(1)), int(m.group(2))
        if 1 <= mo <= 12:
            return ("Mensuelle", date(a, mo, 1), fin_de_mois(a, mo), a, "non")
        return None

    m = re.fullmatch(r"Semestre([12])-(\d{4})", p)
    if m:
        s, a = int(m.group(1)), int(m.group(2))
        return ("Semestrielle", date(a, 6 * s - 5, 1), fin_de_mois(a, 6 * s),
                a, "non")

    m = re.fullmatch(r"Trim1:Trim([2-4])-(\d{4})", p)
    if m:
        t, a = int(m.group(1)), int(m.group(2))
        return ("Trimestrielle cumulée", date(a, 1, 1), fin_de_mois(a, 3 * t),
                a, "oui")

    m = re.fullmatch(r"Trim1-(\d{4})", p)
    if m:
        a = int(m.group(1))
        return ("Trimestrielle cumulée", date(a, 1, 1), fin_de_mois(a, 3), a, "oui")

    m = re.fullmatch(r"([A-Za-zéû]+):([A-Za-zéû]+)-(\d{4})", p)
    if m and m.group(1).lower() in MOIS_CUM and m.group(2).lower() in MOIS_CUM:
        d, f, a = (MOIS_CUM[m.group(1).lower()], MOIS_CUM[m.group(2).lower()],
                   int(m.group(3)))
        return ("Mensuelle cumulée", date(a, d, 1), fin_de_mois(a, f), a, "oui")

    m = re.fullmatch(r"([A-Za-zéû]+)-(\d{4})", p)
    if m and m.group(1).lower() in MOIS_CUM:
        mo, a = MOIS_CUM[m.group(1).lower()], int(m.group(2))
        return ("Mensuelle cumulée", date(a, mo, 1), fin_de_mois(a, mo), a, "oui")

    m = re.fullmatch(r"(\d{2,4})/(\d{2,4})", p)
    if m:
        # campagne agricole : septembre de l'annee 1 -> aout de l'annee 2
        a1 = an4(m.group(1))
        a2 = an4(m.group(2))
        if a2 < a1:            # 99/2000 traite correctement, 80/81 aussi
            a2 = a1 + 1
        return ("Année à cheval", date(a1, 9, 1), date(a2, 8, 31), a2, "non")

    m = re.fullmatch(r"(19|20)\d{2}", p)
    if m:
        a = int(p)
        return ("Annuelle", date(a, 1, 1), date(a, 12, 31), a, "non")

    return None


def valeur(txt):
    """'27 060,3' -> 27060.3 ; 'null', '', '-' -> None."""
    t = txt.strip().replace(" ", "").replace(" ", "")
    if t == "" or t.lower() in ("null", "-", "..", "nd", "n/a"):
        return None
    t = t.replace(",", ".")
    # un nombre peut porter plusieurs points si des milliers ont ete pointes
    if t.count(".") > 1:
        e = t.rfind(".")
        t = t[:e].replace(".", "") + t[e:]
    try:
        return float(t)
    except ValueError:
        return None


def main():
    lignes = []
    n_fichiers = 0
    inconnues = {}

    for parent in DOSSIERS:
        base = os.path.join(ROOT, parent)
        if not os.path.isdir(base):
            continue
        secteur = parent.replace("manar_", "").capitalize()
        for rubrique in sorted(os.listdir(base)):
            dossier = os.path.join(base, rubrique)
            if not os.path.isdir(dossier):
                continue
            for nom in sorted(os.listdir(dossier)):
                if not nom.endswith(".csv"):
                    continue
                chemin = os.path.join(dossier, nom)
                with open(chemin, encoding="utf-8-sig", newline="") as fh:
                    rows = list(csv.reader(fh, delimiter=";"))
                if len(rows) < 2:
                    continue
                n_fichiers += 1
                tableau = nom[:-4]
                entetes = rows[0]

                # analyse des colonnes de periodes une seule fois par tableau
                cols = []
                for j in range(1, len(entetes)):
                    info = analyser_periode(entetes[j])
                    if info is None:
                        if entetes[j].strip():
                            inconnues[entetes[j]] = inconnues.get(entetes[j], 0) + 1
                        continue
                    cols.append((j, entetes[j].strip()) + info)

                for r in rows[1:]:
                    if not r or not r[0].strip():
                        continue
                    brut = r[0].replace(" ", " ")
                    niveau = (len(brut) - len(brut.lstrip())) // 4
                    serie = brut.strip()
                    for (j, per, freq, d1, d2, an, cum) in cols:
                        if j >= len(r):
                            continue
                        v = valeur(r[j])
                        if v is None:
                            continue
                        lignes.append([secteur, rubrique, tableau, serie, niveau,
                                       freq, per, d1.isoformat(), d2.isoformat(),
                                       an, cum, repr(v) if v % 1 else str(int(v))])

    sortie = os.path.join(ROOT, "manar_panel.csv")
    with open(sortie, "w", encoding="utf-8-sig", newline="") as fh:
        w = csv.writer(fh, delimiter=";")
        w.writerow(["secteur", "rubrique", "tableau", "serie", "niveau",
                    "frequence", "periode", "date_debut", "date_fin", "annee",
                    "cumul", "valeur"])
        w.writerows(lignes)

    print("manar_panel.csv : %d observations, %d fichiers, %d series"
          % (len(lignes), n_fichiers,
             len(set((l[2], l[3]) for l in lignes))))
    par_freq = {}
    for l in lignes:
        par_freq[l[5]] = par_freq.get(l[5], 0) + 1
    for f, n in sorted(par_freq.items(), key=lambda x: -x[1]):
        print("   %-24s %8d" % (f, n))
    if inconnues:
        print("\nEn-tetes de periode non reconnus :")
        for k, n in sorted(inconnues.items(), key=lambda x: -x[1])[:15]:
            print("   %-30s x%d" % (k, n))


if __name__ == "__main__":
    main()
