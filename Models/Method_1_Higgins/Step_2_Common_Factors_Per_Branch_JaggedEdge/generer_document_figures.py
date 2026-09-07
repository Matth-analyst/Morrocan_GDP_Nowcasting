# -*- coding: utf-8 -*-
"""
generer_document_figures.py -- Scanne le dossier ./figures/ et genere un
document LaTeX (annexe_figures.tex) regroupant TOUTES les figures produites
par les scripts 02 (facteurs par branche) et 03 (comblement du jagged edge),
organisees clairement pour reference et verification.

A PLACER A LA RACINE du dossier Step_2_Common_Factors_Per_Branch (au meme
niveau que le sous-dossier figures/), puis executer :

    python3 generer_document_figures.py

Produit : annexe_figures.tex -- a compiler ensuite avec pdflatex, deux
passes (pour la table des matieres) :

    pdflatex annexe_figures.tex
    pdflatex annexe_figures.tex
"""
import os
import re
from pathlib import Path

DOSSIER_FIGURES = Path("figures")
FICHIER_SORTIE = "annexe_figures.tex"

def lister_figures():
    """Separe les figures en deux categories : facteurs (Etape 2b) et
    ajustements (Etape 3), et les regroupe par branche."""
    if not DOSSIER_FIGURES.exists():
        raise FileNotFoundError(
            f"Dossier '{DOSSIER_FIGURES}/' introuvable -- ce script doit etre "
            f"execute a la racine du dossier Step_2_Common_Factors_Per_Branch, "
            f"au meme niveau que 'figures/'."
        )

    fichiers = sorted(DOSSIER_FIGURES.glob("*.png"))
    facteurs = {}     # {branche: chemin}
    ajustements = {}  # {branche: [(nom_serie, chemin), ...]}

    for f in fichiers:
        nom = f.stem  # sans extension
        if nom.endswith("_facteur"):
            branche = nom[: -len("_facteur")]
            facteurs[branche] = f
        elif nom.endswith("_ajustement"):
            reste = nom[: -len("_ajustement")]
            # reste = "<branche>_<serie_nettoyee>" -- la branche est le
            # premier segment reconnu parmi les noms de branches connus
            branche_trouvee, serie = separer_branche_serie(reste)
            ajustements.setdefault(branche_trouvee, []).append((serie, f))

    for branche in ajustements:
        ajustements[branche].sort(key=lambda t: t[0])

    return facteurs, ajustements

# Liste des branches connues (prefixes exacts utilises par les scripts 01-03),
# necessaire pour separer correctement "<branche>_<serie>" -- certains noms
# de branche contiennent eux-memes des underscores.
BRANCHES_CONNUES = [
    "Agriculture", "Peche", "Industrie_transformation", "Industrie_extraction",
    "Finances_assurances", "Hebergement_restauration", "Construction",
    "Commerce", "Transports", "Electricite_gaz_eau",
    "Information_communication", "Immobilier",
]

def separer_branche_serie(texte):
    for branche in sorted(BRANCHES_CONNUES, key=len, reverse=True):
        if texte.startswith(branche + "_"):
            return branche, texte[len(branche) + 1:]
    # repli : branche = premier segment avant le premier underscore
    parties = texte.split("_", 1)
    return parties[0], parties[1] if len(parties) > 1 else ""

def nom_lisible(nom_branche_ou_serie):
    """Remet des espaces et une casse lisible a la place des underscores."""
    return nom_branche_ou_serie.replace("_", " ")

def echapper_latex(texte):
    """Echappe les caracteres speciaux LaTeX dans un nom de fichier/serie."""
    remplacements = {
        "&": r"\&", "%": r"\%", "$": r"\$", "#": r"\#",
        "_": r"\_", "{": r"\{", "}": r"\}",
    }
    for old, new in remplacements.items():
        texte = texte.replace(old, new)
    return texte

# ------------------------------------------------------------------------------
# Generation du .tex
# ------------------------------------------------------------------------------
def generer_latex(facteurs, ajustements):
    lignes = []
    lignes.append(r"""\documentclass[11pt,a4paper]{article}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[french]{babel}
\usepackage[margin=2cm]{geometry}
\usepackage{graphicx}
\usepackage{float}
\usepackage{xcolor}
\usepackage{titlesec}
\usepackage[colorlinks=true,linkcolor=blue!50!black]{hyperref}

\definecolor{bleuprincip}{HTML}{1F4E78}
\definecolor{bleuclair}{HTML}{2E74B5}
\titleformat{\section}{\Large\bfseries\color{bleuprincip}}{\thesection}{0.6em}{}
\titleformat{\subsection}{\large\bfseries\color{bleuclair}}{\thesubsection}{0.6em}{}

\title{\textcolor{bleuprincip}{\Huge\bfseries GDPNow-Maroc}\\[0.3cm]
\Large Annexe --- Toutes les figures\\[0.15cm]
\large\color{gray} Facteurs communs par branche (Étape 2b) et ajustements (Étape 3)}
\author{}
\date{\today}

\begin{document}
\maketitle
\tableofcontents
\newpage
""")

    # --- Section 1 : facteurs par branche ---------------------------------
    lignes.append(r"\section{Facteurs communs par branche (Étape 2b)}")
    lignes.append(f"\n{len(facteurs)} figures, une par branche disposant d'indicateurs retenus.\n")
    for branche in sorted(facteurs.keys()):
        chemin = facteurs[branche]
        lignes.append(r"\begin{figure}[H]")
        lignes.append(r"\centering")
        lignes.append(f"\\includegraphics[width=0.95\\textwidth]{{{chemin.as_posix()}}}")
        lignes.append(f"\\caption{{Facteur commun --- {echapper_latex(nom_lisible(branche))}}}")
        lignes.append(r"\end{figure}")
        lignes.append(r"\clearpage")

    # --- Section 2 : ajustements par branche, sous-sections par serie -----
    lignes.append(r"\section{Ajustements par série (Étape 3 --- jagged edge)}")
    total_series = sum(len(v) for v in ajustements.values())
    lignes.append(f"\n{total_series} figures, une par série retenue.\n")
    for branche in sorted(ajustements.keys()):
        lignes.append(f"\\subsection{{{echapper_latex(nom_lisible(branche))}}}")
        for nom_serie, chemin in ajustements[branche]:
            lignes.append(r"\begin{figure}[H]")
            lignes.append(r"\centering")
            lignes.append(f"\\includegraphics[width=0.92\\textwidth]{{{chemin.as_posix()}}}")
            lignes.append(f"\\caption{{{echapper_latex(nom_lisible(branche))} --- {echapper_latex(nom_lisible(nom_serie))}}}")
            lignes.append(r"\end{figure}")
            lignes.append(r"\clearpage")

    lignes.append(r"\end{document}")
    return "\n".join(lignes)

def main():
    facteurs, ajustements = lister_figures()
    print(f"{len(facteurs)} figures de facteurs trouvees (branches: {sorted(facteurs.keys())})")
    total_series = sum(len(v) for v in ajustements.values())
    print(f"{total_series} figures d'ajustement trouvees, sur {len(ajustements)} branches")

    contenu = generer_latex(facteurs, ajustements)
    with open(FICHIER_SORTIE, "w", encoding="utf-8") as f:
        f.write(contenu)
    print(f"\nFichier genere : {FICHIER_SORTIE}")
    print("Compiler avec :")
    print(f"  pdflatex {FICHIER_SORTIE}")
    print(f"  pdflatex {FICHIER_SORTIE}   (2e passe, pour la table des matieres)")

if __name__ == "__main__":
    main()
