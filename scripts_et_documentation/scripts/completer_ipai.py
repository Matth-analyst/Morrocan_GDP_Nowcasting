# -*- coding: utf-8 -*-
"""
Ajoute a ipai_variations_propre.csv les 15 trimestres extraits manuellement
des bulletins pour combler les trous d'extraction (pas des trous de
bulletins : les PDF existaient, ils n'avaient simplement jamais ete lus).

Trimestres ajoutes : T4-2019, T2-2016, T3-2021, T2-2021, T2-2022, T4-2023,
T1-2024, T2-2024, T3-2024, T4-2024, T1-2025, T2-2025, T3-2025, T4-2025,
T1-2026.

Seul vrai trou restant : T2-2013 (aucun bulletin dans le dossier).

Rupture methodologique documentee par la banque elle-meme dans le bulletin
N°62 (T1-2025) : "l'IPAI est elargi, a partir de T1-25, a toutes les
conservations foncieres du royaume, soit 83 conservations". Le chainage
traverse ce point mais un flag le signale.
"""
import csv
from pathlib import Path

IPAI = (Path(__file__).resolve().parents[2] / 'Bank Al-Maghrib et ANCFCC'
        / 'immobilier_ipai' / 'ipai_variations_propre.csv')

NOUVEAUX = [
    # trimestre, categorie, indicateur, var_trim, var_annuelle, confiance, fichier
    ('T2-2016', 'Global', 'prix', '0.1', '-0.9', 'haute (lecture manuelle verifiee)',
     'DERI-IPAI Q2 2016 ...pdf'),
    ('T4-2019', 'Global', 'prix', '-0.4', '-0.5', 'haute (lecture manuelle verifiee)',
     'DERI-IPAI T4 2019vf.pdf'),
    ('T2-2021', 'Global', 'prix', '-5.4', '-2.0', 'haute (lecture manuelle verifiee)',
     'IPAI T2 FR 2021.pdf'),
    ('T3-2021', 'Global', 'prix', '0.2', '-5.5', 'haute (lecture manuelle verifiee)',
     'IPAI T3 2021 VF.pdf'),
    ('T2-2022', 'Global', 'prix', '0.2', '0.4', 'haute (lecture manuelle verifiee)',
     'IPAI T2 FR 2022.pdf'),
    ('T4-2023', 'Global', 'prix', '0.1', '1.1', 'haute (lecture manuelle verifiee)',
     'IPAI-T4-2023.pdf'),
    ('T1-2024', 'Global', 'prix', '0.4', '0.8', 'haute (lecture manuelle verifiee)',
     'Publication IPAI T1 2024.pdf'),
    ('T2-2024', 'Global', 'prix', '0.0', '-0.4', 'haute (lecture manuelle verifiee)',
     'IPAI T2 2024.pdf'),
    ('T3-2024', 'Global', 'prix', '-0.2', '-0.4', 'haute (lecture manuelle verifiee)',
     'IPAI T3-2024 FR.pdf'),
    ('T4-2024', 'Global', 'prix', '1.1', '0.8', 'haute (lecture manuelle verifiee)',
     'IPAI T4-2024.pdf'),
    ('T1-2025', 'Global', 'prix', '-1.8', '0.0', 'haute (lecture manuelle verifiee) '
     '— RUPTURE METHODOLOGIQUE : IPAI elargi a 83 conservations foncieres (bulletin N62)',
     'Publication IPAI T1 2025.pdf'),
    ('T2-2025', 'Global', 'prix', '-0.2', '0.0', 'haute (lecture manuelle verifiee)',
     'IPAI T2 2025.pdf'),
    ('T3-2025', 'Global', 'prix', '1.1', '1.2', 'haute (lecture manuelle verifiee)',
     'IPAI T3-2025.pdf'),
    ('T4-2025', 'Global', 'prix', '0.2', '0.2', 'haute (lecture manuelle verifiee)',
     'Publication-IPAI T4 2025.pdf'),
    ('T1-2026', 'Global', 'prix', '-2.4', '-0.4', 'haute (lecture manuelle verifiee)',
     'IPAI T1 2026.pdf'),
]


def main():
    rows = list(csv.reader(open(IPAI, encoding='utf-8-sig'), delimiter=';'))
    hdr, data = rows[0], rows[1:]
    existants = {(r[0], r[1], r[2].split(' (')[0].strip()) for r in data}
    ajoutes = 0
    for t, cat, ind, vt, va, conf, fic in NOUVEAUX:
        cle = (t, cat, ind)
        if cle in existants:
            continue
        data.append([t, cat, ind, vt, va, conf, fic])
        ajoutes += 1
    with open(IPAI, 'w', encoding='utf-8-sig', newline='') as fh:
        w = csv.writer(fh, delimiter=';')
        w.writerow(hdr)
        w.writerows(data)
    print('%d nouvelles lignes ajoutees a %s (%d lignes au total)'
          % (ajoutes, IPAI.name, len(data)))


if __name__ == '__main__':
    main()
