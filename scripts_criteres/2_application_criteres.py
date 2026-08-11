# -*- coding: utf-8 -*-
"""
ÉTAPE 2 — Application de la cascade de critères de sélection.

Rôle : pour chaque branche, choisit une cible unique (parmi les variantes
trouvées à l'étape 1), puis teste chaque indicateur candidat contre les
critères 1 à 5 (dans l'ordre : fréquence, longueur, fraîcheur, densité,
corrélation), et enfin élimine les redondances (critère 7) parmi les
indicateurs qui ont passé les 5 premiers critères.

Entrée  : parsed_branches_v2.pkl (sortie du script 1a)
Sortie  : criteria_results.pkl ({branche: {cible, cible_nobs, retenus, rejets}})

Dépendances : numpy, pandas
    pip install numpy pandas

SEUILS UTILISÉS (version assouplie, après un premier passage trop strict
qui éliminait la quasi-totalité des séries — voir le document
"Critères et état des lieux" pour la justification de chaque seuil) :
    - Longueur minimale        : 24 observations
    - Fraîcheur (mensuel)      : dernière observation à ≤ 450 jours
    - Fraîcheur (trimestriel)  : dernière observation à ≤ 365 jours
    - Densité interne          : ≥ 75 % de valeurs non manquantes
    - Corrélation à la cible   : |r| ≥ 0,25 (sur les taux de croissance Δlog)
    - Redondance croisée       : deux indicateurs corrélés à ≥ 0,85 entre
                                  eux → seul le plus corrélé à la cible
                                  est conservé

NOTE IMPORTANTE : le critère 6 (cohérence du signe économique) n'est PAS
appliqué comme filtre automatique ici — il est seulement calculé (le
signe de r est conservé dans le résultat) pour permettre un examen
manuel a posteriori. C'est ce qui a permis de repérer, après coup, que
7 des 41 séries finalement retenues ont un signe contre-intuitif — à
vérifier avant intégration définitive dans le modèle.
"""
import pickle
import numpy as np
import pandas as pd
from datetime import date

IN = "/home/claude/parsed_branches_v2.pkl"
OUT = "/home/claude/criteria_results.pkl"

TODAY = date(2026, 8, 11)  # date de référence pour le critère de fraîcheur

# --- Seuils des critères (modifier ici pour re-tester d'autres valeurs) ---
SEUIL_LONGUEUR_MIN = 24
SEUIL_FRAICHEUR_MENSUEL_JOURS = 450
SEUIL_FRAICHEUR_TRIMESTRIEL_JOURS = 365
SEUIL_DENSITE_MIN = 0.75
SEUIL_CORRELATION_MIN = 0.25
SEUIL_REDONDANCE_MAX = 0.85
NB_POINTS_COMMUNS_MIN = 8  # taille minimale de l'intersection pour un test de corrélation fiable


def infer_freq(serie):
    """Déduit la fréquence réelle d'une série à partir de l'écart médian
    entre observations consécutives, plutôt que de faire confiance à une
    étiquette de colonne (voir script 1a)."""
    if len(serie) < 2:
        return "inconnue"
    dates = sorted(d for d, v in serie)
    gaps = [(dates[i + 1] - dates[i]).days for i in range(len(dates) - 1)]
    med = np.median(gaps)
    if med <= 40:
        return "mensuel"
    if med <= 100:
        return "trimestriel"
    return "annuel"


def expected_count(dmin, dmax, freq):
    """Nombre d'observations attendu entre deux dates si la série était
    complète à sa fréquence — sert au calcul de densité (critère 4)."""
    months = (dmax.year - dmin.year) * 12 + (dmax.month - dmin.month) + 1
    if freq == "mensuel":
        return months
    if freq == "trimestriel":
        return months / 3
    return months / 12


def freshness_ok(dmax, freq):
    """Critère 3 : la dernière observation doit être suffisamment récente.
    Le seuil trimestriel intègre une marge pour le délai de publication
    habituel des comptes nationaux (2-3 mois après la fin du trimestre)."""
    delta_days = (TODAY - dmax).days
    if freq == "mensuel":
        return delta_days <= SEUIL_FRAICHEUR_MENSUEL_JOURS
    if freq == "trimestriel":
        return delta_days <= SEUIL_FRAICHEUR_TRIMESTRIEL_JOURS
    return True  # l'annuel n'est pas filtré ici (traité à part pour l'agriculture)


def log_diff(serie):
    """Transforme une série de niveaux en taux de croissance (Δlog),
    la transformation standard pour rendre deux séries comparables et
    stationnaires avant de calculer une corrélation."""
    s = pd.Series({d: v for d, v in serie}).sort_index()
    s = s[s > 0]  # log() indéfini pour des valeurs nulles ou négatives
    return np.log(s).diff().dropna()


def to_quarterly(serie_mensuelle):
    """Agrège une série mensuelle en moyenne trimestrielle, pour pouvoir
    la comparer à la cible (toujours trimestrielle)."""
    s = pd.Series({d: v for d, v in serie_mensuelle}).sort_index()
    s.index = pd.to_datetime(s.index)
    q = s.resample("QS").mean().dropna()
    return [(d.date(), v) for d, v in q.items()]


def selectionner_cible(data):
    """Choisit la cible : la plus longue série candidate parmi la cible
    de base et toutes les variantes détectées, à condition d'avoir au
    moins 40 observations (sinon ce n'est probablement pas une vraie
    série de cible mais un fragment)."""
    candidats = [dict(nom="VA (base 2014 simple)", serie=data["cible"])] + data["cibles_add"]
    candidats = [c for c in candidats if len(c["serie"]) >= 40]
    if not candidats:
        return None
    return max(candidats, key=lambda c: len(c["serie"]))


def appliquer_criteres_branche(branche, data):
    cible = selectionner_cible(data)
    if cible is None:
        return dict(cible=None, cible_nobs=0, retenus=[], rejets=[])

    cible_ld = log_diff(cible["serie"])
    retenus, rejets = [], []
    seen_names = set()

    for ind in data["indicateurs"]:
        nom = ind["nom"]
        if nom in seen_names:  # doublon de nom au sein de la meme branche
            continue
        seen_names.add(nom)
        serie = ind["serie"]
        freq = infer_freq(serie)

        # Critère 1 — fréquence
        if freq not in ("mensuel", "trimestriel"):
            rejets.append((nom, "C1-frequence", freq))
            continue

        # Critère 2 — longueur minimale
        if len(serie) < SEUIL_LONGUEUR_MIN:
            rejets.append((nom, "C2-longueur", len(serie)))
            continue

        dates_only = sorted(d for d, v in serie)
        dmin, dmax = dates_only[0], dates_only[-1]

        # Critère 3 — fraîcheur
        if not freshness_ok(dmax, freq):
            rejets.append((nom, "C3-fraicheur", str(dmax)))
            continue

        # Critère 4 — densité interne
        exp = expected_count(dmin, dmax, freq)
        densite = len(serie) / exp if exp > 0 else 0
        if densite < SEUIL_DENSITE_MIN:
            rejets.append((nom, "C4-densite", f"{densite:.0%}"))
            continue

        # Critère 5 — corrélation avec la cible (sur les Δlog, à fréquence trimestrielle)
        serie_pour_corr = to_quarterly(serie) if freq == "mensuel" else serie
        ind_ld = log_diff(serie_pour_corr)
        common = cible_ld.index.intersection(ind_ld.index)
        if len(common) < NB_POINTS_COMMUNS_MIN:
            rejets.append((nom, "C5-trop peu de points communs", len(common)))
            continue
        r = np.corrcoef(cible_ld.loc[common], ind_ld.loc[common])[0, 1]
        if np.isnan(r) or abs(r) < SEUIL_CORRELATION_MIN:
            rejets.append((nom, "C5-correlation", f"r={r:.2f}" if not np.isnan(r) else "r=nan"))
            continue

        retenus.append(dict(nom=nom, source=ind["source"], freq=freq, nobs=len(serie),
                             debut=str(dmin), fin=str(dmax), densite=densite, r=r))

    # Critère 7 — non-redondance entre indicateurs retenus
    if len(retenus) > 1:
        series_map = {}
        for ind in data["indicateurs"]:
            if ind["nom"] in [r["nom"] for r in retenus]:
                s = to_quarterly(ind["serie"]) if infer_freq(ind["serie"]) == "mensuel" else ind["serie"]
                series_map[ind["nom"]] = log_diff(s)
        retenus_tries = sorted(retenus, key=lambda x: abs(x["r"]), reverse=True)
        gardes, noms_gardes = [], []
        for r in retenus_tries:
            redondant = False
            for ng in noms_gardes:
                common = series_map[r["nom"]].index.intersection(series_map[ng].index)
                if len(common) >= NB_POINTS_COMMUNS_MIN:
                    rr = np.corrcoef(series_map[r["nom"]].loc[common], series_map[ng].loc[common])[0, 1]
                    if not np.isnan(rr) and abs(rr) >= SEUIL_REDONDANCE_MAX:
                        redondant = True
                        break
            if redondant:
                rejets.append((r["nom"], "C7-redondance", f"corrélé à {ng}"))
            else:
                gardes.append(r)
                noms_gardes.append(r["nom"])
        retenus = gardes

    return dict(cible=cible, cible_nobs=len(cible["serie"]), retenus=retenus, rejets=rejets)


if __name__ == "__main__":
    with open(IN, "rb") as f:
        all_data = pickle.load(f)

    results = {branche: appliquer_criteres_branche(branche, data)
               for branche, data in all_data.items()}

    total_retenus, total_rejets = 0, 0
    for b, r in results.items():
        n_r, n_j = len(r["retenus"]), len(r["rejets"])
        total_retenus += n_r
        total_rejets += n_j
        print(f"{b:32s} cible={r['cible_nobs']:3d} obs | retenus={n_r:3d} | rejetes={n_j:3d}")

    print(f"\nTOTAL RETENUS (hors cibles) : {total_retenus}")
    print(f"TOTAL REJETES               : {total_rejets}")

    with open(OUT, "wb") as f:
        pickle.dump(results, f)
    print(f"\nRésultats sauvegardés dans {OUT}")
