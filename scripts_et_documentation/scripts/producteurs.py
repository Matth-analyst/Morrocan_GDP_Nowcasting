# -*- coding: utf-8 -*-
"""
Determine l'INSTITUTION PRODUCTRICE de chaque serie, a distinguer du fichier
dans lequel la serie a ete prise.

Trois niveaux d'information, du plus fiable au moins fiable :

1. Les fichiers "*_Description.csv" exportes de Manar-Stat portent un champ
   "Source" renseigne par le portail (ex. Office Cherifien des Phosphates).
2. Les producteurs communiques par l'auteur de BDD_SECTORIEL_MENSUEL pour les
   series dont aucun fichier source n'a ete retrouve.
3. A defaut, deduction a partir du dossier d'origine.

Quand le producteur n'est ni atteste ni communique, on ecrit "— (non documenté)"
plutot que de deviner.
"""

import csv
import re
import unicodedata
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def _norm(s):
    s = unicodedata.normalize('NFD', str(s)).encode('ascii', 'ignore').decode()
    return re.sub(r'[^a-z0-9]', '', s.lower())


# --------------------------------------------------------------------------
# 2. producteurs communiques par l'auteur de la base consolidee
#    (series dont le fichier source primaire n'est pas sur le disque)
# --------------------------------------------------------------------------
COMMUNIQUES = [
    (('moyennedesprecipitations', 'temperaturemoyenne'), 'SIG Maroc'),
    (('primesversees', 'prestations'),
     'ACAPS (Autorité de Contrôle des Assurances et de la Prévoyance Sociale)'),
    (('tauxcreditsimmobiliers', 'creditsalequipement', 'creditsalaconsommation',
      'depotsavueaupresdesbanques', 'massemonetairem3', 'avoirsofficielsdereserve',
      'creditbancaire'), 'Bank Al-Maghrib'),
    (('indiceimportations', 'indiceexportations'), 'HCP (Haut-Commissariat au Plan)'),
    (('prixinternationauxdephosphate',), 'Index Mundi'),
]

# --------------------------------------------------------------------------
# 3. deduction par dossier, en dernier recours
# --------------------------------------------------------------------------
#    L'ordre compte : du plus specifique au plus generique. "ipai" doit passer
#    avant "bank al-maghrib", sinon les bulletins immobiliers seraient
#    attribues a la seule banque centrale alors qu'ils sont co-produits.
PAR_DOSSIER = [
    ('immobilier_ipai', 'Bank Al-Maghrib / ANCFCC'),
    ('ipai', 'Bank Al-Maghrib / ANCFCC'),
    ('credit_bancaire_par_branche', 'Bank Al-Maghrib'),
    ('bank al-maghrib', 'Bank Al-Maghrib'),
    ('pib trimestriel', 'HCP (comptes nationaux), relayé par Manar-Stat'),
    ('va cible (hcp)', 'HCP (comptes nationaux)'),
    ('hcp', 'HCP (Haut-Commissariat au Plan)'),
    ('office des changes', 'Office des Changes'),
    ('manar-stat', 'Manar-Stat (DEPF, Ministère de l’Économie et des Finances)'),
    ('manar_panel', 'Manar-Stat (DEPF, Ministère de l’Économie et des Finances)'),
]


def charger_descriptions():
    """-> {stem normalise du tableau: 'Libellé (SIGLE)'}"""
    out = {}
    for p in ROOT.rglob('*_Description.csv'):
        try:
            tete = open(p, encoding='utf-8-sig', errors='replace').readline()
            sep = ';' if tete.count(';') >= tete.count(',') else ','
            rows = list(csv.reader(open(p, encoding='utf-8-sig', errors='replace'),
                                   delimiter=sep))
        except Exception:
            continue
        libelle = sigle = None
        for r in rows[:15]:
            if r and _norm(r[0]).startswith('source'):
                libelle = (r[1] or '').strip() if len(r) > 1 else ''
                sigle = (r[2] or '').strip() if len(r) > 2 else ''
                break
        if not libelle:
            continue
        val = '%s (%s)' % (libelle, sigle) if sigle and sigle != libelle else libelle
        stem = p.name[:-len('_Description.csv')]
        out[_norm(stem)] = val
    return out


class Producteurs:
    def __init__(self):
        self.desc = charger_descriptions()

    def pour(self, fichier, serie, source_declaree=None):
        """fichier : chemin relatif ; serie : nom dans le fichier ;
        source_declaree : ce que le classeur d'origine affichait."""
        ns = _norm(serie)

        # 2. producteurs communiques (prioritaires : ils tranchent les cas
        #    ou aucun fichier source n'existe)
        for cles, inst in COMMUNIQUES:
            if any(ns.startswith(k[:22]) or k.startswith(ns[:22]) for k in cles):
                return inst, 'communiqué par l’auteur de la base'

        if fichier and str(fichier) not in ('—', '?', 'None'):
            # les series annuelles portent un fichier du type
            # "<nom du tableau> (manar_panel)" : on retire le suffixe pour
            # retrouver la fiche descriptive du tableau d'origine
            brut = re.sub(r'\s*\(manar_panel\)\s*$', '', str(fichier))
            nf = _norm(Path(brut).stem)
            # 1. fiche descriptive Manar-Stat
            for stem, inst in self.desc.items():
                if nf == stem or nf.startswith(stem[:26]) or stem.startswith(nf[:26]):
                    return inst, 'fiche descriptive Manar-Stat'
            # 3. deduction par dossier
            chemin = _norm(str(fichier))
            for cle, inst in PAR_DOSSIER:
                if _norm(cle) in chemin:
                    return inst, 'déduit du dossier d’origine'
            if '(manar_panel)' in str(fichier):
                return ('Manar-Stat (DEPF, Ministère de l’Économie et des Finances)',
                        'déduit du dossier d’origine')

        if source_declaree and str(source_declaree) not in ('None', '', '—', '?'):
            return str(source_declaree), 'déclaré dans le classeur d’origine'
        return '— (non documenté)', '—'


if __name__ == '__main__':
    p = Producteurs()
    print('%d fiches descriptives chargees' % len(p.desc))
    for f, s in [
        (r"Manar-Stat (Ministere de l'Economie et des Finances)\secondaire\mines\Indice de production minière base 2015 (trimestriel).csv", 'Industries extractives'),
        ('—', 'Moyenne des precipitations'),
        ('—', 'Masse monétaire (M3)'),
        ('—', 'Prix internationaux de Phosphate'),
        (r'Bank Al-Maghrib et ANCFCC\credit_bancaire_par_branche\17-x.csv', 'Agriculture et pêche'),
    ]:
        print('  %-30s -> %s' % (s[:30], p.pour(f, s)))
