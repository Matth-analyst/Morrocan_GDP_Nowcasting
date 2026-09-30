# -*- coding: utf-8 -*-
"""Compare chaque CSV de resultats/ au meme fichier d une sauvegarde.

    python outils/comparer_sorties.py _sauvegarde_20260914_1301

Separe ce qui compte de ce qui ne compte pas : un ecart relatif inferieur a
1e-9 est du bruit de virgule flottante (ordre de sommation, BLAS), pas un
changement de resultat. Sert a verifier qu une modification de structure du
pipeline n a rien change aux chiffres.
"""
import io, os, re, glob, sys

B = sys.argv[1] if len(sys.argv) > 1 else '_sauvegarde_20260914_1301'
NOMBRE = re.compile(r'^-?\d+\.?\d*(?:[eE][-+]?\d+)?$')

def cellules(path):
    txt = io.open(path, encoding='utf-8-sig').read().split('\n')
    return [l.split(',') for l in txt if l.strip()]

bruit, reels, structure = [], [], []
for f in sorted(glob.glob('resultats/*.csv')):
    b = os.path.join(B, f)
    if not os.path.exists(b):
        continue
    A, C = cellules(f), cellules(b)
    if len(A) != len(C) or any(len(x) != len(y) for x, y in zip(A, C)):
        structure.append((f, 'dimensions differentes : %d lignes contre %d' % (len(A), len(C))))
        continue
    ecart_max, ou, txt_diff = 0.0, '', 0
    for i, (la, lc) in enumerate(zip(A, C)):
        for j, (a, c) in enumerate(zip(la, lc)):
            if a == c:
                continue
            if NOMBRE.match(a.strip()) and NOMBRE.match(c.strip()):
                x, y = float(a), float(c)
                d = abs(x - y) / max(abs(x), abs(y), 1e-12)
                if d > ecart_max:
                    ecart_max, ou = d, 'ligne %d colonne %d : %s contre %s' % (i + 1, j + 1, a, c)
            else:
                txt_diff += 1
    if ecart_max == 0 and txt_diff == 0:
        continue
    if txt_diff:
        structure.append((f, '%d cellule(s) TEXTE differente(s)' % txt_diff))
    elif ecart_max < 1e-9:
        bruit.append((f, ecart_max))
    else:
        reels.append((f, ecart_max, ou))

print('=' * 78)
print('ECARTS NUMERIQUES REELS (> 1e-9 en relatif)')
print('=' * 78)
for f, d, ou in sorted(reels, key=lambda x: -x[1]):
    print('  %-44s %.2e   %s' % (os.path.basename(f), d, ou))
print('  (aucun)' if not reels else '')

print('=' * 78)
print('DIFFERENCES DE STRUCTURE OU DE TEXTE')
print('=' * 78)
for f, m in structure:
    print('  %-44s %s' % (os.path.basename(f), m))
print('  (aucune)' if not structure else '')

print('=' * 78)
print('BRUIT DE VIRGULE FLOTTANTE SEULEMENT (< 1e-9)')
print('=' * 78)
print('  %d fichier(s) : %s' % (len(bruit), ', '.join(os.path.basename(f) for f, _ in bruit))
      if bruit else '  (aucun)')
