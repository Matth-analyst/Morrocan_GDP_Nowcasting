# -*- coding: utf-8 -*-
"""
Indexe TOUS les fichiers de donnees du data lake en un catalogue de series
datees, quel que soit leur format.

Les exports Manar-Stat et Bank Al-Maghrib partagent une structure "large" :
quelque part dans le fichier, une ligne porte les periodes (dates ISO,
2024M03, 2024T1, annees, ou noms de mois sous une ligne d'annees) et les
lignes suivantes portent une etiquette de serie puis ses valeurs.

Ce module trouve cette ligne d'en-tete automatiquement et en deduit les series.

    from catalogue_lake import charger_lake
    series = charger_lake(Path(...))    # -> [{origine, fichier, serie, freq, valeurs}]
"""

import csv
import re
from pathlib import Path

MOIS_NOM = {'janvier': 1, 'fevrier': 2, 'février': 2, 'mars': 3, 'avril': 4,
            'mai': 5, 'juin': 6, 'juillet': 7, 'aout': 8, 'août': 8,
            'septembre': 9, 'octobre': 10, 'novembre': 11, 'decembre': 12,
            'décembre': 12}
MOIS_ABBR = {'janv': 1, 'fevr': 2, 'févr': 2, 'mars': 3, 'avr': 4, 'mai': 5,
             'juin': 6, 'juil': 7, 'aout': 8, 'août': 8, 'sept': 9, 'oct': 10,
             'nov': 11, 'dec': 12, 'déc': 12}


def _cle_periode(txt):
    """-> ('M', 'AAAAMmm') | ('T', 'AAAATn') | None"""
    if txt is None:
        return None
    s = str(txt).strip()
    if not s:
        return None
    m = re.match(r'^(\d{4})-(\d{2})-\d{2}', s)          # 2015-07-01 00:00:00
    if m:
        a, mo = int(m.group(1)), int(m.group(2))
        return ('M', f'{a}M{mo:02d}')
    m = re.fullmatch(r'(\d{4})M(\d{1,2})', s)            # 2024M03
    if m:
        return ('M', f'{int(m.group(1))}M{int(m.group(2)):02d}')
    m = re.fullmatch(r'(\d{4})T([1-4])', s)              # 2024T1
    if m:
        return ('T', s)
    m = re.fullmatch(r'T([1-4])[-/ ](\d{4})', s)         # T1-2024
    if m:
        return ('T', f'{m.group(2)}T{m.group(1)}')
    m = re.fullmatch(r'(\d{4})\s*[-/]\s*T?([1-4])', s)   # 2024-1
    if m:
        return ('T', f'{m.group(1)}T{m.group(2)}')
    m = re.fullmatch(r'(\d{4})\s*:\s*([1-4])', s)        # 2014:1 (comptes nationaux HCP)
    if m:
        return ('T', f'{m.group(1)}T{m.group(2)}')
    m = re.fullmatch(r'([a-zûéèA-Z]+)-(\d{2})', s)       # dec-07, janv-08
    if m:
        mo = MOIS_ABBR.get(m.group(1).lower()[:4])
        if mo:
            a = int(m.group(2))
            a += 2000 if a < 80 else 1900
            return ('M', f'{a}M{mo:02d}')
    return None


def _mois_nom(txt):
    if txt is None:
        return None
    s = str(txt).strip().lower().replace('.', '')
    if s in MOIS_NOM:
        return MOIS_NOM[s]
    s4 = s[:4]
    return MOIS_ABBR.get(s4)


def _nombre(t):
    if t is None:
        return None
    s = str(t).strip().replace('\xa0', '').replace(' ', '')
    if not s or s.lower() in ('null', 'nan', '-', '--', '..', 'nd', 'n/a'):
        return None
    if s.count(',') == 1 and s.count('.') == 0:
        s = s.replace(',', '.')
    else:
        s = s.replace(',', '')
    try:
        v = float(s)
    except ValueError:
        return None
    return None if v != v else v          # rejette NaN


def _lire_lignes(p):
    tete = open(p, encoding='utf-8-sig', errors='replace').readline()
    sep = ';' if tete.count(';') >= tete.count(',') else ','
    with open(p, encoding='utf-8-sig', errors='replace', newline='') as fh:
        return list(csv.reader(fh, delimiter=sep))


def _entete(rows):
    """Trouve la ligne de periodes. -> (index, {col: cle}, granularite)"""
    best = (None, {}, None, 0)
    for i, r in enumerate(rows[:40]):
        cles, gran = {}, None
        for j, c in enumerate(r):
            k = _cle_periode(c)
            if k:
                cles[j] = k[1]
                gran = k[0]
        if len(cles) > best[3]:
            best = (i, cles, gran, len(cles))
    if best[3] >= 4:
        return best[0], best[1], best[2]

    # cas "noms de mois" avec une ligne d'annees au-dessus
    for i, r in enumerate(rows[:40]):
        mois = {j: _mois_nom(c) for j, c in enumerate(r) if _mois_nom(c)}
        if len(mois) < 6:
            continue
        for k in range(i - 1, max(-1, i - 4), -1):
            annees = {j: int(str(c).strip()[:4])
                      for j, c in enumerate(rows[k])
                      if re.fullmatch(r'\s*(19|20)\d{2}(\.0)?\s*', str(c).strip())}
            if not annees:
                continue
            cles, cour = {}, None
            for j in range(len(r)):
                if j in annees:
                    cour = annees[j]
                if cour and j in mois:
                    cles[j] = f'{cour}M{mois[j]:02d}'
            if len(cles) >= 6:
                return i, cles, 'M'
    return None, {}, None


def _etiquette(r, col_min):
    for j in range(min(col_min, len(r))):
        v = str(r[j]).strip()
        if v and not re.fullmatch(r'Unnamed:\s*\d+', v):
            return v
    return None


def charger_lake(racine, exclure=('graphify-out',), mini=6):
    """Indexe tous les .csv sous `racine`. -> liste de dict."""
    out = []
    for p in sorted(Path(racine).rglob('*.csv')):
        if any(x in p.parts for x in exclure):
            continue
        try:
            rows = _lire_lignes(p)
        except Exception:
            continue
        if len(rows) < 2:
            continue
        i, cles, gran = _entete(rows)
        if i is None or not cles:
            continue
        col_min = min(cles)
        freq = 'Mensuelle' if gran == 'M' else 'Trimestrielle'
        vus = {}
        for r in rows[i + 1:]:
            if not r:
                continue
            lab = _etiquette(r, col_min)
            if not lab:
                continue
            vals = {}
            for j, k in cles.items():
                if j < len(r):
                    v = _nombre(r[j])
                    if v is not None:
                        vals[k] = v
            if len(vals) < mini:
                continue
            n = vus.get(lab, 0) + 1
            vus[lab] = n
            out.append({'origine': 'data lake', 'fichier': str(p.relative_to(racine)),
                        'serie': lab if n == 1 else f'{lab} ({n})',
                        'freq': freq, 'valeurs': vals})
    return out


if __name__ == '__main__':
    import sys
    from collections import Counter
    racine = Path(sys.argv[1] if len(sys.argv) > 1 else '.')
    s = charger_lake(racine)
    print(f'{len(s)} series indexees')
    c = Counter(x['freq'] for x in s)
    print(dict(c))
    par_fichier = Counter(x['fichier'].split('\\')[0] for x in s)
    for k, v in par_fichier.most_common(10):
        print(f'  {v:5d}  {k}')
