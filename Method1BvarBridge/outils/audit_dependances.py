# -*- coding: utf-8 -*-
"""Audit statique des dependances entre scripts du pipeline.

    python outils/audit_dependances.py      (depuis la racine du projet)

Lit chaque R/*.R, releve les lire_csv() et ecrire_csv(), et verifie trois
choses : qu aucun script ne lit un fichier que personne ne produit, que l ordre
de run_pipeline.R respecte les dependances, et qu aucun script de R/ n est
oublie du pipeline.

Ecrit le 14 septembre 2026, apres qu un run depuis un resultats/ vide a revele
une dependance circulaire (03e <-> 06) et cinq scripts jamais integres au
pipeline : des defauts que seul un dossier vide peut faire apparaitre.
"""
import io, os, re, glob

def norm(p):
    return p.replace('\\', '/')

pipe = io.open('run_pipeline.R', encoding='utf-8').read()
ordre = [norm(x) for x in re.findall(r'"(R/[^"]+\.R)"', pipe)]

scripts = sorted(norm(p) for p in glob.glob('R/*.R'))
racine = {os.path.basename(p) for p in glob.glob('*.csv')}

def helpers(s):
    """nom du helper -> prefixe applique."""
    h = {}
    for m in re.finditer(
            r'(\w+)\s*<-\s*function\s*\(\s*x\s*\)\s*file\.path\(\s*DOSSIER_RESULTATS\s*,\s*'
            r'(?:paste0\(\s*(?:"([^"]*)"|(PREFIXE)\s*,\s*"([^"]*)")\s*,\s*x\s*\)|x)\s*\)', s):
        if m.group(3):                      # forme paste0(PREFIXE, "_", x)
            pre = re.search(r'PREFIXE\s*<-\s*"([^"]*)"', s)
            h[m.group(1)] = (pre.group(1) if pre else '') + (m.group(4) or '')
        else:                               # forme paste0("03c_", x) ou x seul
            h[m.group(1)] = m.group(2) or ''
    return h

lit, ecrit = {}, {}
for f in scripts:
    s = io.open(f, encoding='utf-8').read()
    H = helpers(s)
    L, E = set(), set()
    # Les appels s'etendent souvent sur plusieurs lignes : on recolle chaque
    # appel jusqu'a ce que ses parentheses se referment, sinon un
    # ecrire_csv(x,\n  chemin(...)) passe inapercu et le fichier est declare
    # orphelin a tort.
    brut, tampon, profondeur = [], '', 0
    for ligne in s.split('\n'):
        nue = ligne.split('#')[0] if ligne.lstrip().startswith('#') else ligne
        if ligne.lstrip().startswith('#'):
            continue
        if profondeur > 0:
            tampon += ' ' + ligne.strip()
            profondeur += ligne.count('(') - ligne.count(')')
            if profondeur <= 0:
                brut.append(tampon); tampon, profondeur = '', 0
            continue
        if re.search(r'(lire_csv|ecrire_csv)\s*\(', ligne):
            d = ligne.count('(') - ligne.count(')')
            if d > 0:
                tampon, profondeur = ligne.strip(), d
                continue
        brut.append(ligne)

    for ligne in brut:
        for m in re.finditer(r'(lire_csv|ecrire_csv)\s*\((.*)$', ligne):
            fn, reste = m.group(1), m.group(2)
            for mm in re.finditer(r'(?:(\w+)\s*\(\s*)?"([^"]*\.csv)"', reste):
                helper, nom = mm.group(1), mm.group(2)
                if helper in H:
                    nom = H[helper] + nom
                nom = nom.split('/')[-1]
                (E if fn == 'ecrire_csv' else L).add(nom)
    lit[f], ecrit[f] = L, E

produit_par = {}
for f in scripts:
    for n in ecrit[f]:
        produit_par.setdefault(n, []).append(f)
pos = {f: i for i, f in enumerate(ordre)}

print('=' * 78)
print('1. FICHIERS LUS QUE PERSONNE NE PRODUIT')
print('=' * 78)
orph = {}
for f in scripts:
    for n in sorted(lit[f]):
        if n not in produit_par and n not in racine:
            orph.setdefault(n, []).append(os.path.basename(f))
for n, fs in sorted(orph.items()):
    print('  %-40s lu par %s' % (n, ', '.join(fs)))
print('  (aucun)' if not orph else '')

print('=' * 78)
print('2. DEPENDANCES VIOLEES PAR L ORDRE DU PIPELINE')
print('=' * 78)
v = 0
for f in ordre:
    if f not in lit:
        print('  !! %s dans le pipeline mais absent de R/' % f); v += 1; continue
    for n in sorted(lit[f]):
        if n in racine or n not in produit_par:
            continue
        src = [p for p in produit_par[n] if p in pos]
        if not src:
            print('  %-30s lit %-36s produit HORS pipeline (%s)' %
                  (os.path.basename(f), n, ','.join(os.path.basename(x) for x in produit_par[n])))
            v += 1
        elif min(pos[p] for p in src) > pos[f]:
            print('  %-30s lit %-36s produit plus TARD par %s' %
                  (os.path.basename(f), n, ','.join(os.path.basename(x) for x in src)))
            v += 1
if not v:
    print('  (aucune)')

print('=' * 78)
print('3. SCRIPTS DE R/ ABSENTS DU PIPELINE')
print('=' * 78)
absents = [f for f in scripts if f not in pos and os.path.basename(f) != '00_setup.R']
for f in absents:
    print('  %-36s ecrit %d fichier(s)' % (os.path.basename(f), len(ecrit[f])))
print('  (aucun)' if not absents else '')
