# Journal de collecte — Manar-Stat, domaine Sectoriel

Source : banque de donnees Manar-Stat, Direction des Etudes et des
Previsions Financieres (DEPF), Ministere de l'Economie et des Finances
du Maroc — <https://manar.finances.gov.ma>

Perimetre : integralite du domaine **Sectoriel** a partir de la rubrique
*Mines* jusqu'a la fin de l'arborescence (Secondaire a partir de Mines,
puis Tertiaire dans son ensemble).

Genere le 03/08/2026 a 18:46.

## Methode

Collecte en HTTP pur (`requests`), sans navigateur automatise :

1. `AjaxConsultation.getListDomaineTableau` (DWR) — arborescence complete ;
2. `POST Consultation_consulterTable` — ouverture du tableau ;
3. filtre de periodes ZK — **elargissement a la grille 1960-2027** ;
4. `exportToXLS` — export Excel natif du portail ;
5. conversion en CSV (separateur `;`, encodage UTF-8 BOM), colonnes
   entierement vides elidees.

> **Point critique.** Sans l'etape 3, le portail ne renvoie que les
> ~10 dernieres periodes de chaque serie. L'elargissement multiplie
> la profondeur historique par 3 a 4 sur la plupart des tableaux
> (ex. production des produits miniers : 2012-2021 -> 1980-2021).

`robots.txt` renvoie 404 : aucune regle d'exclusion. Delai de 3 s entre
chaque tableau.

## Resume

| Rubrique | Tableaux | Recuperes | Non recuperes |
|---|---|---|---|
| Mines | 20 | 20 | 0 |
| Construction | 10 | 10 | 0 |
| Eau | 4 | 3 | 1 |
| Energie | 29 | 29 | 0 |
| Industrie | 31 | 30 | 1 |
| Transports | 17 | 17 | 0 |
| Tourisme | 15 | 15 | 0 |
| Poste et telecommunications | 47 | 46 | 1 |
| Assurances | 1 | 1 | 0 |
| **Total** | **174** | **171** | **3** |

## Tableaux non recuperes

| Tableau | Rubrique | Raison |
|---|---|---|
| Production de l'ONE-BE | manar_secondaire/eau | Present dans l'arborescence mais **aucune valeur cote portail** : l'export ne contient que les libelles de lignes et les en-tetes de periodes, sans donnee, avec ou sans elargissement. |
| Indice de la production industrielle (base 1998) (données trimestrielles) | manar_secondaire/industrie | Present dans l'arborescence mais **aucune valeur cote portail** : l'export ne contient que les libelles de lignes et les en-tetes de periodes, sans donnee, avec ou sans elargissement. |
| Parc de la téléphonie mobile par opérateur | manar_tertiaire/telecommunications | Present dans l'arborescence mais **aucune valeur cote portail** : l'export ne contient que les libelles de lignes et les en-tetes de periodes, sans donnee, avec ou sans elargissement. |

## Detail par rubrique


### Mines — `manar_secondaire/mines/`

| Tableau | Frequence | Debut | Fin | Series |
|---|---|---|---|---|
| Exportation des phosphates et des produits dérivés en valeur (Mensuelle) | Mensuelle | 2016M01 | 2016M06 | 9 |
| Exportation des phosphates et des produits dérivés en valeur | Annuelle | 1998 | 2021 | 4 |
| Exportation des phosphates et des produits dérivés en volume (Mensuelle) | Mensuelle | 1995M04 | 2016M06 | 4 |
| Exportation des phosphates et des produits dérivés en volume | Annuelle | 1995 | 2021 | 4 |
| Exportation des phosphates par pays en valeur | Annuelle | 1990 | 2021 | 26 |
| Exportation des phosphates par pays en volume | Annuelle | 1990 | 2021 | 26 |
| Exportation des produits miniers (en valeur) | Annuelle | 1980 | 2021 | 8 |
| Exportation des produits miniers (en volume) | Annuelle | 1980 | 2021 | 8 |
| Importation de soufre en valeur (Mensuelle) | Mensuelle | 1995M05 | 2011M06 | 3 |
| Importation de soufre en volume (Mensuelle) | Mensuelle | 1995M05 | 2011M06 | 3 |
| Indice de la Production minière base 1998 (Annuelle) | Annuelle | 1998 | 2012 | 9 |
| Indice de la Production minière base 1998 (Trimestrielle) | Trimestrielle | 1999T1 | 2013T2 | 9 |
| Indice de la Production minière base 2010 (Annuelle) | Annuelle | 2010 | 2018 | 3 |
| Indice de la production minière, base 1969 (trimestrielle) | Trimestrielle | 1980T1 | 1982T4 | 5 |
| Indice de production minière base 2015 (trimestriel) | Trimestrielle | 2016T1 | 2026T1 | 3 |
| Production des phosphates et dérivés en volume (Mensuel) | Mensuelle | 1995M04 | 2016M12 | 4 |
| Production des phosphates et dérivés en volume | Annuelle | 1995 | 2021 | 4 |
| Production des produits miniers (en valeur) | Annuelle | 1980 | 2004 | 8 |
| Production des produits miniers (en volume) | Annuelle | 1980 | 2021 | 8 |
| Production et utilisation des phosphates | Annuelle | 1980 | 2021 | 4 |

### Construction — `manar_secondaire/construction/`

| Tableau | Frequence | Debut | Fin | Series |
|---|---|---|---|---|
| Autorisations de construire | Annuelle | 1981 | 2020 | 7 |
| Importation du ciment hydraulique | Annuelle | 1980 | 2021 | 1 |
| Nombre de logements prévues dans les autorisations de construction | Annuelle | 1981 | 2020 | 4 |
| Nombre de pièces prévues dans les autorisations de construction | Annuelle | 1994 | 2020 | 4 |
| Prix moyen couvert | Annuelle | 1980 | 2014 | 7 |
| Surface bâtie | Annuelle | 1981 | 2020 | 7 |
| Surface des planchers (en milliers de m2) | Annuelle | 1981 | 2020 | 7 |
| Valeur prévue des autorisations de construction | Annuelle | 1981 | 2020 | 7 |
| Ventes locales du ciment (mensuel cumulé) | Mensuelle | 1995M01 | 2026M05 | 1 |
| Ventes locales du ciment annuel | Annuelle | 1980 | 2023 | 1 |

### Eau — `manar_secondaire/eau/`

| Tableau | Frequence | Debut | Fin | Series |
|---|---|---|---|---|
| Consommation d'eau potable | Annuelle | 1983 | 2021 | 6 |
| Taux de remplissage des barrages | Annuelle | 1992 | 2021 | 15 |
| Ventes de l'ONE-BE | Annuelle | 1983 | 2021 | 4 |

### Energie — `manar_secondaire/energie/`

| Tableau | Frequence | Debut | Fin | Series |
|---|---|---|---|---|
| Brent (BloomBerg) - Mensuel | Mensuelle | 2016M01 | 2024M11 | 1 |
| Butane ($ mt) Mensuel - Platts | Mensuelle | 2016M01 | 2024M11 | 1 |
| Consommation de l'ONEE en combustibles (Mensuel) | Mensuelle | 1996M02 | 2026M05 | 6 |
| Consommation de l'ONEE en combustibles | Annuelle | 1996 | 2022 | 6 |
| Consommation totale d'énergie (Bilan énergétique) | Annuelle | 1980 | 2021 | 5 |
| Déficit énergétique | Annuelle | 1980 | 2021 | 1 |
| Entrées de la raffinerie (Mensuel) | Mensuelle | 2011M01 | 2014M06 | 3 |
| Indice de la Production énergétique base 2010 (Annuelle) | Annuelle | 2010 | 2018 | 1 |
| Indice de la production énergétique base 1969 (Trimestrielle) | Trimestrielle | 1980T1 | 1982T4 | 4 |
| Indice de la production énergétique base 1978 (Annuelle) | Annuelle | 1980 | 1982 | 4 |
| Indice de la production énergétique base 1978 (Trimestrielle) | Trimestrielle | 1982T2 | 1987T4 | 4 |
| Indice de la production énergétique base 1982 (Annuelle) | Annuelle | 1982 | 1987 | 4 |
| Indice de la production énergétique base 1982 (Trimestrielle) | Trimestrielle | 1987T1 | 1990T2 | 4 |
| Indice de la production énergétique base 1987 (Annuelle) | Annuelle | 1987 | 1992 | 4 |
| Indice de la production énergétique base 1987 (Trimestrielle) | Trimestrielle | 1990T1 | 1993T4 | 4 |
| Indice de la production énergétique base 1992 (Annuelle) | Annuelle | 1992 | 2006 | 4 |
| Indice de la production énergétique base 1992 (Trimestrielle) | Trimestrielle | 1993T3 | 2007T1 | 4 |
| Indice de la production énergétique base 1998 (Annuelle) | Annuelle | 1998 | 2012 | 2 |
| Indice de la production énergétique base 1998 (Trimestrielle) | Trimestrielle | 2005T4 | 2013T2 | 1 |
| Indice de production énergétique base 2015 (trimestriel) | Trimestrielle | 2016T1 | 2026T1 | 1 |
| Nombre de foyers villages branché au réseau électrique | Annuelle | 1996 | 2022 | 2 |
| Production locale d'énergie (bilan énergétique) | Annuelle | 1980 | 2021 | 5 |
| Taux d'éléctrification rurale | Annuelle | 1996 | 2022 | 1 |
| Taux de dépendance énergétique | Annuelle | 1980 | 2021 | 1 |
| Ventes ONEE d'électricité (mensuel) | Mensuelle | 1994M01 | 2026M05 | 5 |
| Ventes ONEE d'électricité | Annuelle | 1994 | 2021 | 5 |
| production des produits pétroliers | Annuelle | 1980 | 2021 | 2 |
| Énergie appelée nette (mensuel) | Mensuelle | 1996M01 | 2026M05 | 16 |
| Énergie appelée nette annuelle | Annuelle | 1994 | 2022 | 16 |

### Industrie — `manar_secondaire/industrie/`

| Tableau | Frequence | Debut | Fin | Series |
|---|---|---|---|---|
| Chiffre d'affaires des entreprises industrielles | Annuelle | 1990 | 2016 | 129 |
| Consommation du sucre | Annuelle | 1980 | 2021 | 4 |
| Création d'entreprises par type d'immatriculation (mensuel) | Mensuelle | 2012M01 | 2024M09 | 3 |
| Ecrasement de blé | Année à cheval | 80/81 | 2020/2021 | 4 |
| Emploi industriel permanant | Annuelle | 1990 | 2013 | 129 |
| Emploi industriel total par grand secteur | Annuelle | 1990 | 2019 | 6 |
| Emploi total des entreprises industrielles | Annuelle | 1990 | 2019 | 129 |
| Evolution annuelle de la création d'entreprises par type d'immatriculation | Annuelle | 1990 | 2019 | 3 |
| Exportation des Produits Artisanaux | Annuelle | 1980 | 2021 | 15 |
| Exportations des entreprises industrielles par grand secteur | Annuelle | 1990 | 2019 | 6 |
| Exportations des entreprises industrielles | Annuelle | 1990 | 2019 | 129 |
| Frais de personnel industriel par grand secteur | Annuelle | 1990 | 2015 | 6 |
| Frais de personnel industriel | Annuelle | 1990 | 2015 | 129 |
| Indice de la production industrielle (base 1998) (données annuelles) | Annuelle | 1998 | 2012 | 109 |
| Indice de la production industrielle (base 2010) | Annuelle | 2010 | 2016 | 23 |
| Indice de production industriel base 2015 (Trimestriel) | Trimestrielle | 2016T1 | 2026T1 | 24 |
| Industrie laitière | Annuelle | 1984 | 2020 | 3 |
| Industrie oléicole | Année à cheval | 83/84 | 2020/2021 | 4 |
| Investissement des entreprises industrielles | Annuelle | 1990 | 2019 | 129 |
| Investissement industriel par grand secteur | Annuelle | 1990 | 2019 | 6 |
| Nombre d'entreprises industrielles | Annuelle | 1990 | 2013 | 129 |
| Nombre de femme employées permanentes des entreprises industrielles | Annuelle | 2004 | 2013 | 129 |
| Production de sucre | Annuelle | 1980 | 2021 | 7 |
| Production des entreprises industrielles par grand secteur | Annuelle | 1990 | 2020 | 6 |
| Production des entreprises industrielles | Annuelle | 1990 | 2020 | 129 |
| Production des graines oléagineuses | Annuelle | 1993 | 2021 | 2 |
| Taux d’utilisation des capacités (mensuel) | Mensuelle | 2010M01 | 2025M08 | 1 |
| Traitement des plantes sucrières | Annuelle | 1980 | 2021 | 3 |
| Valeurs ajoutées des entreprises industrielles | Annuelle | 1990 | 2020 | 129 |
| Valeurs ajoutées industrielles par grand secteur | Annuelle | 1990 | 2020 | 6 |

### Transports — `manar_tertiaire/transports/`

| Tableau | Frequence | Debut | Fin | Series |
|---|---|---|---|---|
| Immatriculation des véhicules par catégorie | Annuelle | 1992 | 2023 | 4 |
| Longueur des lignes ferrées | Annuelle | 1994 | 2015 | 7 |
| Longueur des voies ferrées | Annuelle | 1994 | 2015 | 3 |
| Nombre d'accidents de la circulation constatés | Annuelle | 1980 | 2024 | 3 |
| Nombre de victimes des accidents de la circulation | Annuelle | 1980 | 2024 | 3 |
| Parc des véhicules en circulation | Annuelle | 1980 | 2023 | 4 |
| Réseau routier revêtu | Annuelle | 1992 | 2023 | 5 |
| Taux d'occupation des wagons | Annuelle | 1992 | 2015 | 3 |
| Tonnage Kilométré | Annuelle | 1992 | 2015 | 3 |
| Trafic des marchandises (Ferroviaire) | Annuelle | 1992 | 2015 | 3 |
| Trafic des marchandises (transport maritime) | Annuelle | 1992 | 2014 | 4 |
| Trafic des voyageurs (Ferroviaire) | Annuelle | 1992 | 2015 | 3 |
| Trafic global portuaire annuel | Annuelle | 2003 | 2022 | 5 |
| Trafic global portuaire | Annuelle | 2003 | 2022 | 5 |
| Trafic global portuaires par mode de conditionnement | Annuelle | 1992 | 2021 | 4 |
| Trafic portuaire géré par l'ANP (mensuel) | Mensuelle | 2009M06 | 2023M11 | 5 |
| Voyageur kilométré (Ferroviaire) | Annuelle | 1992 | 2015 | 3 |

### Tourisme — `manar_tertiaire/tourisme/`

| Tableau | Frequence | Debut | Fin | Series |
|---|---|---|---|---|
| Arrivées des touristes mensuel cumulé (en nombre) | Mensuelle | 1996M01 | 2026M05 | 3 |
| Arrivées des touristes par pays | Annuelle | 1984 | 2022 | 28 |
| Arrivées des touristes | Annuelle | 1980 | 2022 | 3 |
| Arrivés de touristes étrangers de Séjour (TES) | Mensuelle | 2023M01 | 2025M02 | 14 |
| Capacité en lits des hôtels homologués | Annuelle | 1987 | 2021 | 8 |
| Flux de touristes par pays | Annuelle | 1984 | 2022 | 35 |
| Nuitées touristique | Mensuelle | 2025M05 | 2026M04 | 3 |
| Nuitées touristiques (Annuel) | Annuelle | 2013 | 2022 | 3 |
| Nuitées touristiques par catégorie | Annuelle | 1980 | 2022 | 8 |
| Nuitées touristiques par pays | Annuelle | 1980 | 2022 | 28 |
| Recettes touristiques (mensuel cumulé) | Mensuelle | 2025M06 | 2026M05 | 1 |
| Recettes touristiques par pays | Annuelle | 1990 | 2024 | 35 |
| Recettes touristiques | Annuelle | 1961 | 2021 | 1 |
| Taux d'occupation dans les hôtels classés (annuel) | Annuelle | 1997 | 2021 | 1 |
| Taux d'occupation dans les hôtels classés par destination (Mensuel) | Mensuelle | 2007M01 | 2019M07 | 13 |

### Poste et telecommunications — `manar_tertiaire/telecommunications/`

| Tableau | Frequence | Debut | Fin | Series |
|---|---|---|---|---|
| Bande passante Internet Internationale | Annuelle | 2002 | 2016 | 1 |
| Chiffre d'affaire du E-commerce | Annuelle | 2008 | 2012 | 1 |
| E-commerce | Annuelle | 2008 | 2012 | 3 |
| Facture moyenne mensuelle par client internet (trimestriel) | Trimestrielle | 2010T4 | 2016T4 | 3 |
| Facture moyenne mensuelle par client internet | Annuelle | 2010 | 2016 | 3 |
| Montant des opération de la Caisse d'Epargne Nationale | Annuelle | 1980 | 2016 | 2 |
| Nombre d'utilisateurs Internet pour 1000 habitant | Annuelle | 1999 | 2015 | 1 |
| Nombre d'utilisateurs Internet | Annuelle | 1995 | 2015 | 1 |
| Nombre d'établissements postaux | Annuelle | 1980 | 2016 | 4 |
| Nombre de mandats de la poste | Annuelle | 1980 | 2021 | 2 |
| Noms de domaine .ma (trimestriel) | Trimestrielle | 2009T3 | 2016T4 | 1 |
| Noms de domaine .ma | Annuelle | 2003 | 2016 | 1 |
| Parc de la téléphonie mobile par opérateur (trimestriel) | Trimestrielle | 2003T4 | 2025T1 | 3 |
| Parc des abonnés Internet (Trimestriel) | Trimestrielle | 2003T4 | 2026T1 | 6 |
| Parc des abonnés Internet-nouveau | Annuelle | 2000 | 2021 | 6 |
| Parc global de la téléphonie fixe (Trimestriel) | Trimestrielle | 2002T1 | 2026T1 | 4 |
| Parc global de la téléphonie fixe | Annuelle | 2015 | 2024 | 8 |
| Parts de marché dans le secteur d'Internet | Annuelle | 2008 | 2016 | 9 |
| Parts de marché dans le secteur de la téléphonie fixe (trimestriel) | Trimestrielle | 2006T1 | 2025T1 | 6 |
| Parts de marché dans le secteur de la téléphonie fixe | Annuelle | 2006 | 2022 | 9 |
| Parts de marché dans le secteur de la téléphonie mobile (trimestriel) | Trimestrielle | 2003T4 | 2025T1 | 4 |
| Parts de marché dans le secteur de la téléphonie mobile | Annuelle | 2004 | 2017 | 9 |
| Recettes de l'activité postale | Annuelle | 1980 | 2016 | 6 |
| Revenu moyen d’une minute de communication (ARPM) (Trimestriel) | Trimestrielle | 2010T1 | 2017T4 | 1 |
| Revenu moyen d’une minute de communication (ARPM) Fixe | Annuelle | 2010 | 2017 | 1 |
| Revenu moyen d’une minute de communication (ARPM) Mobile (trimestriel) | Trimestrielle | 2010T2 | 2017T4 | 3 |
| Revenu moyen d’une minute de communication (ARPM) Mobile | Annuelle | 2010 | 2017 | 3 |
| Taux de pénétration Mobile (Trimestriel) | Trimestrielle | 2003T4 | 2026T1 | 1 |
| Taux de pénétration Mobile | Annuelle | 2003 | 2021 | 3 |
| Taux de pénétration d'Internet (trimestriel) | Trimestrielle | 2010T4 | 2025T1 | 1 |
| Taux de pénétration d'internet | Annuelle | 1998 | 2021 | 1 |
| Taux de pénétration de la téléphonie fixe (trimestriel) | Trimestrielle | 2002T1 | 2025T1 | 1 |
| Trafic SMS sortant (trimestriel) | Trimestrielle | 2010T4 | 2018T3 | 1 |
| Trafic SMS sortant du Mobile | Annuelle | 2005 | 2017 | 1 |
| Trafic voix sortant Mobile (trimestriel) | Trimestrielle | 2010T4 | 2018T3 | 1 |
| Trafic voix sortant du Fixe (Trimestriel) | Trimestrielle | 2010T4 | 2018T3 | 1 |
| Trafic voix sortant du Fixe | Annuelle | 2005 | 2024 | 1 |
| Trafic voix sortant du Mobile | Annuelle | 2005 | 2021 | 1 |
| Usage moyen mensuel sortant par client (Trimestriel) | Trimestrielle | 2010T1 | 2017T4 | 1 |
| Usage moyen mensuel sortant par client Fixe | Annuelle | 2010 | 2024 | 1 |
| Usage moyen mensuel sortant par client Mobile | Annuelle | 2010 | 2017 | 3 |
| Usage moyen mensuel sortant par client mobile (trimestriel) | Trimestrielle | 2010T2 | 2017T4 | 3 |
| Valeur des opérations au centre de chèques postaux | Annuelle | 1980 | 2016 | 2 |
| parc global de la téléphonie mobile (Trimestriel) | Trimestrielle | 2003T4 | 2026T1 | 3 |
| parc global de la téléphonie mobile | Annuelle | 1990 | 2021 | 3 |
| valeur des mandats de la poste | Annuelle | 1980 | 2016 | 2 |

### Assurances — `manar_tertiaire/assurances/`

| Tableau | Frequence | Debut | Fin | Series |
|---|---|---|---|---|
| Primes émises par les sociétés d'assurance | Annuelle | 1976 | 2024 | 22 |

## Reserves d'interpretation

- Les indices de production existent par **bases successives non
  chainees** (1969, 1978, 1982, 1987, 1992, 1998, 2010, 2015). Le
  raccordement des bases reste a faire.
- Plusieurs series s'arretent tot (produits miniers en valeur : 2004 ;
  phosphates mensuels : 2016 ; grands agregats industriels : 2013-2020).
  Les series les plus fraiches, exploitables en nowcasting, sont l'IPI
  et l'IPM base 2015 (jusqu'a 2026T1) et le taux d'utilisation des
  capacites (jusqu'a 2025M08).
- Les fichiers `.xlsx` conservent l'export natif du portail (feuilles
  *Donnees* et *Description*, cette derniere portant les metadonnees :
  source, unite, echelle).


---

## Execution du 03/08/2026 a 18:50

8 tableaux traites — 7 recuperes, 1 en echec.


### Tableaux recuperes

| Rubrique | Id | Tableau | Frequence | Debut | Fin | Series | Periode elargie |
|---|---|---|---|---|---|---|---|
| manar_primaire/cultures | 6277 | Superficies des principales cultures agricoles | Année à cheval | 80/81 | 2023/2024 | 11 | oui |
| manar_primaire/cultures | 6278 | Rendement des principales cultures agricoles | Année à cheval | 80/81 | 2024/2025 | 11 | oui |
| manar_primaire/cultures | 7674 | Productions des principales cultures agricoles | Année à cheval | 79/80 | 2023/2024 | 13 | oui |
| manar_primaire/cultures | 6495 | Prix moyens payés aux producteurs des produits agricoles | Année à cheval | 92/93 | 2020/2021 | 25 | oui |
| manar_primaire/cultures | 7955 | Superficies des principales cultures agricoles (détail) | Année à cheval | 80/81 | 2023/2024 | 30 | oui |
| manar_primaire/cultures | 7956 | Rendement des principales cultures agricoles (détail) | Année à cheval | 80/81 | 2024/2025 | 27 | oui |
| manar_primaire/cultures | 4421 | Production des principales cultures agricoles (détail) | Année à cheval | 79/80 | 2023/2024 | 37 | oui |

### Echecs

| Rubrique | Id | Tableau | Raison |
|---|---|---|---|
| manar_primaire/cultures | 2061 | Commercialisation des quatre céréales principales | aucune periode renseignee |


---

## Execution du 03/08/2026 a 18:51

7 tableaux traites — 7 recuperes, 0 en echec.


### Tableaux recuperes

| Rubrique | Id | Tableau | Frequence | Debut | Fin | Series | Periode elargie |
|---|---|---|---|---|---|---|---|
| manar_primaire/elevage | 4721 | Production de miel | Annuelle | 1980 | 2021 | 3 | oui |
| manar_primaire/elevage | 4722 | Production d'Œufs de consommation | Annuelle | 1969 | 2021 | 3 | oui |
| manar_primaire/elevage | 4723 | Viandes destinées à la consommation | Annuelle | 1998 | 2021 | 9 | oui |
| manar_primaire/elevage | 1721 | Effectif du cheptel (passage octobre-novembre) | Annuelle | 1980 | 2014 | 3 | oui |
| manar_primaire/elevage | 1722 | Effectif du cheptel (passage mars-avril) | Annuelle | 1995 | 2021 | 3 | oui |
| manar_primaire/elevage | 1724 | Abattages contrôlés | Annuelle | 1980 | 2020 | 4 | oui |
| manar_primaire/elevage | 1727 | Poids de la viande des abattages contrôlés | Annuelle | 1980 | 2021 | 4 | oui |


---

## Execution du 03/08/2026 a 19:21

13 tableaux traites — 10 recuperes, 3 en echec.


### Tableaux recuperes

| Rubrique | Id | Tableau | Frequence | Debut | Fin | Series | Periode elargie |
|---|---|---|---|---|---|---|---|
| manar_primaire/peche | 1792 | Production halieutique en valeur | Annuelle | 1980 | 2023 | 3 | oui |
| manar_primaire/peche | 1793 | Production halieutique en volume | Annuelle | 1980 | 2023 | 3 | oui |
| manar_primaire/peche | 2081 | Nombre de bateaux de la pêche | Annuelle | 1984 | 2024 | 3 | oui |
| manar_primaire/peche | 2082 | Capacité de la flotte de pêche | Annuelle | 1984 | 2024 | 3 | oui |
| manar_primaire/peche | 6594 | Débarquements des produits de la pêche côtière et artisanale par espèce en valeur | Annuelle | 2008 | 2022 | 8 | oui |
| manar_primaire/peche | 6595 | Débarquements des produits de la pêche côtière et artisanale par espèce en quantité | Annuelle | 2008 | 2022 | 8 | oui |
| manar_primaire/peche | 6596 | Débarquements des produits de la pêche côtière et artisanale par espèce en quantité (mensuel) | Mensuelle | 2008M12 | 2026M05 | 8 | oui |
| manar_primaire/peche | 6598 | Débarquements des produits de la pêche côtière et artisanale par espèce en valeur (mensuel) | Mensuelle | 2008M12 | 2026M05 | 8 | oui |
| manar_primaire/peche | 6599 | Débarquements des produits de la pêche côtière et artisanale par port en quantité (mensuel) | Mensuelle | 2008M12 | 2026M05 | 66 | oui |
| manar_primaire/peche | 5959 | Débarquements des produits de la pêche côtière et artisanale par port en valeur | Annuelle | 2008 | 2022 | 66 | oui |

### Echecs

| Rubrique | Id | Tableau | Raison |
|---|---|---|---|
| manar_primaire/peche | 6600 | Débarquements des produits de la pêche côtière et artisanale par port en valeur (mensuel) | ('Connection aborted.', ConnectionResetError(10054, 'An existing connection was forcibly closed by the remote host', None, 10054, None)) |
| manar_primaire/peche | 5961 | Débarquements des produits de la pêche côtière et artisanale par port en quantité | page ZK sans bouton d'export (tableau vide ?) |
| manar_primaire/peche | 1790 | Destination des produits de la pêche côtière | ('Connection aborted.', ConnectionResetError(10054, 'An existing connection was forcibly closed by the remote host', None, 10054, None)) |
