# -*- coding: utf-8 -*-
"""
Reconstruit log_collecte.md a partir des fichiers reellement presents sur le
disque (source de verite), et non de l'historique des executions.

    python journal_collecte.py
"""

import csv
import os
import re
from datetime import datetime

ROOT = os.path.dirname(os.path.abspath(__file__))

ORDRE = [
    ("manar_secondaire", "mines", "Mines", 20),
    ("manar_secondaire", "construction", "Construction", 10),
    ("manar_secondaire", "eau", "Eau", 4),
    ("manar_secondaire", "energie", "Energie", 29),
    ("manar_secondaire", "industrie", "Industrie", 31),
    ("manar_tertiaire", "transports", "Transports", 17),
    ("manar_tertiaire", "tourisme", "Tourisme", 15),
    ("manar_tertiaire", "telecommunications", "Poste et telecommunications", 47),
    ("manar_tertiaire", "assurances", "Assurances", 1),
]

# tableaux presents dans l'arborescence mais sans aucune valeur cote portail
VIDES = {
    "Production de l'ONE-BE": "manar_secondaire/eau",
    "Indice de la production industrielle (base 1998) (données trimestrielles)":
        "manar_secondaire/industrie",
    "Parc de la téléphonie mobile par opérateur":
        "manar_tertiaire/telecommunications",
}


def frequence(periode):
    if re.fullmatch(r"\d{4}T[1-4]", periode):
        return "Trimestrielle"
    if re.fullmatch(r"\d{4}M\d{2}", periode):
        return "Mensuelle"
    if re.fullmatch(r"\d{2,4}/\d{2,4}", periode):
        return "Année à cheval"
    if periode.startswith("Semestre"):
        return "Semestrielle"
    if re.match(r"(Janv|Fev|Mars|Avr|Mai|Juin|Juil|Aout|Sep|Oct|Nov|Dec)[-:]", periode):
        return "Mensuelle cumulée"
    if re.fullmatch(r"(19|20)\d{2}", periode):
        return "Annuelle"
    return "?"


def lire(chemin):
    with open(chemin, encoding="utf-8-sig", newline="") as fh:
        lignes = list(csv.reader(fh, delimiter=";"))
    if len(lignes) < 2:
        return None
    entete = [c for c in lignes[0][1:] if c.strip()]
    if not entete:
        return None
    return entete[0], entete[-1], len(lignes) - 1


def main():
    out = []
    out.append("# Journal de collecte — Manar-Stat, domaine Sectoriel\n")
    out.append("Source : banque de donnees Manar-Stat, Direction des Etudes et des")
    out.append("Previsions Financieres (DEPF), Ministere de l'Economie et des Finances")
    out.append("du Maroc — <https://manar.finances.gov.ma>\n")
    out.append("Perimetre : integralite du domaine **Sectoriel** a partir de la rubrique")
    out.append("*Mines* jusqu'a la fin de l'arborescence (Secondaire a partir de Mines,")
    out.append("puis Tertiaire dans son ensemble).\n")
    out.append("Genere le %s.\n" % datetime.now().strftime("%d/%m/%Y a %H:%M"))

    out.append("## Methode\n")
    out.append("Collecte en HTTP pur (`requests`), sans navigateur automatise :\n")
    out.append("1. `AjaxConsultation.getListDomaineTableau` (DWR) — arborescence complete ;")
    out.append("2. `POST Consultation_consulterTable` — ouverture du tableau ;")
    out.append("3. filtre de periodes ZK — **elargissement a la grille 1960-2027** ;")
    out.append("4. `exportToXLS` — export Excel natif du portail ;")
    out.append("5. conversion en CSV (separateur `;`, encodage UTF-8 BOM), colonnes")
    out.append("   entierement vides elidees.\n")
    out.append("> **Point critique.** Sans l'etape 3, le portail ne renvoie que les")
    out.append("> ~10 dernieres periodes de chaque serie. L'elargissement multiplie")
    out.append("> la profondeur historique par 3 a 4 sur la plupart des tableaux")
    out.append("> (ex. production des produits miniers : 2012-2021 -> 1980-2021).\n")
    out.append("`robots.txt` renvoie 404 : aucune regle d'exclusion. Delai de 3 s entre")
    out.append("chaque tableau.\n")

    total_ok = 0
    total_att = 0
    resume = []
    detail = []

    for parent, sous, titre, attendu in ORDRE:
        dossier = os.path.join(ROOT, parent, sous)
        fichiers = sorted(f for f in os.listdir(dossier)
                          if f.endswith(".csv")) if os.path.isdir(dossier) else []
        total_att += attendu
        detail.append("\n### %s — `%s/%s/`\n" % (titre, parent, sous))
        detail.append("| Tableau | Frequence | Debut | Fin | Series |")
        detail.append("|---|---|---|---|---|")
        n_ok = 0
        for f in fichiers:
            r = lire(os.path.join(dossier, f))
            if not r:
                continue
            p1, p2, n = r
            n_ok += 1
            detail.append("| %s | %s | %s | %s | %d |"
                          % (f[:-4].replace("|", "/"), frequence(p1), p1, p2, n))
        total_ok += n_ok
        resume.append("| %s | %d | %d | %d |"
                      % (titre, attendu, n_ok, attendu - n_ok))

    out.append("## Resume\n")
    out.append("| Rubrique | Tableaux | Recuperes | Non recuperes |")
    out.append("|---|---|---|---|")
    out.extend(resume)
    out.append("| **Total** | **%d** | **%d** | **%d** |"
               % (total_att, total_ok, total_att - total_ok))

    out.append("\n## Tableaux non recuperes\n")
    out.append("| Tableau | Rubrique | Raison |")
    out.append("|---|---|---|")
    for nom, rub in VIDES.items():
        out.append("| %s | %s | Present dans l'arborescence mais **aucune valeur "
                   "cote portail** : l'export ne contient que les libelles de lignes "
                   "et les en-tetes de periodes, sans donnee, avec ou sans "
                   "elargissement. |" % (nom, rub))

    out.append("\n## Detail par rubrique\n")
    out.extend(detail)

    out.append("\n## Reserves d'interpretation\n")
    out.append("- Les indices de production existent par **bases successives non")
    out.append("  chainees** (1969, 1978, 1982, 1987, 1992, 1998, 2010, 2015). Le")
    out.append("  raccordement des bases reste a faire.")
    out.append("- Plusieurs series s'arretent tot (produits miniers en valeur : 2004 ;")
    out.append("  phosphates mensuels : 2016 ; grands agregats industriels : 2013-2020).")
    out.append("  Les series les plus fraiches, exploitables en nowcasting, sont l'IPI")
    out.append("  et l'IPM base 2015 (jusqu'a 2026T1) et le taux d'utilisation des")
    out.append("  capacites (jusqu'a 2025M08).")
    out.append("- Les fichiers `.xlsx` conservent l'export natif du portail (feuilles")
    out.append("  *Donnees* et *Description*, cette derniere portant les metadonnees :")
    out.append("  source, unite, echelle).")

    chemin = os.path.join(ROOT, "log_collecte.md")
    with open(chemin, "w", encoding="utf-8") as fh:
        fh.write("\n".join(out) + "\n")
    print("log_collecte.md ecrit : %d tableaux recuperes sur %d"
          % (total_ok, total_att))


if __name__ == "__main__":
    main()
