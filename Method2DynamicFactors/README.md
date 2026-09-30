# Méthode 2 : modèle à facteurs dynamiques

Reprise de l'approche par facteurs dynamiques, construite pour être **comparable
terme à terme** avec la méthode 1 : même vivier, même protocole d'évaluation,
même formule d'agrégation, de sorte que l'écart mesuré à la fin tienne au modèle
et non à l'habillage. Cet objectif est atteint sur les douze branches couvertes ;
il ne l'est pas tout à fait sur les quatre autres, et la section consacrée à
cette limite en chiffre l'effet.

## Le modèle

Pour chaque branche, le panel d'indicateurs est résumé par un petit nombre de
facteurs communs :

```
x_t = Λ F_t + e_t
F_t = A_1 F_{t−1} + ... + A_p F_{t−p} + u_t
```

Trois points font la rigueur de l'implémentation :

1. **Estimation par maximum de vraisemblance via l'algorithme EM**, qui accepte
   n'importe quel motif de données manquantes. C'est ce qui permet de traiter le
   bord irrégulier sans imputation préalable.
2. **Agrégation temporelle de Mariano et Murasawa** pour relier les mois au
   trimestre, avec les poids (1, 2, 3, 2, 1)/3. Elle impose cinq retards dans
   l'état compagnon, ce qui n'est pas un détail : la négliger reviendrait à
   traiter un taux de croissance trimestriel comme une moyenne de mois.
3. **Filtre et non lisseur** au bord de l'échantillon. Le lisseur utilise toute
   l'information, y compris postérieure ; l'employer à l'extrémité créerait une
   fuite invisible.

Le nombre de facteurs *r* et l'ordre du VAR *p* ne sont pas fixés : ils sont
choisis **récursivement**, à chaque origine, sur la performance hors échantillon
des origines précédentes.

## Une limite de la comparaison, mesurée

Les quatre branches sans indicateur reçoivent ici un AR(p), ordre choisi par
critère bayésien. La méthode 1, elle, leur applique son vecteur autorégressif :
sur les 192 prévisions concernées, **aucune** ne coïncide. Les deux chaînes ne
diffèrent donc pas seulement sur les douze branches couvertes, contrairement à ce
que visait la construction.

L'écart est chiffrable. Sur ces quatre branches, rapporté à l'écart-type de
chacune :

| Branche | AR(p) | Vecteur autorégressif |
|---|---|---|
| Services aux entreprises | 1,485 | 0,968 |
| Administration publique | 1,372 | 1,006 |
| Autres services | 1,050 | 0,941 |
| Éducation-santé | 0,909 | 0,954 |

L'autorégression fait moins bien dans trois cas sur quatre. Comme ces branches
pèsent un quart de la valeur ajoutée, la comparaison **défavorise les facteurs
dynamiques** : en les traitant comme la méthode 1, le ratio de l'agrégat passerait
de 0,983 à 0,972. Le classement ne change pas, mais l'écart annoncé est un
majorant.

## Organisation

```
Method2DynamicFactors/
├── R/
│   ├── 00_setup.R              conventions, nomenclature des 16 branches
│   ├── fonctions/
│   │   ├── espace_etat.R       filtre, lisseur, prévision d'état
│   │   └── dfm.R               compagnon, EM, sélection, prévision de la cible
│   └── NN_*.R                  les étapes de la chaîne
├── tests/                      17 contrôles sur données simulées
├── data/                       panels mensuels par branche
├── resultats/                  toutes les sorties chiffrées
├── figures/
└── report/rapport_dfm.html     le développement complet
```

## Exécution

```bash
Rscript R/01_import.R
Rscript R/02_panel.R
Rscript R/03_dfm_backtest.R
Rscript R/04a_poids.R
Rscript R/04_agregation_comparaison.R
Rscript R/05_validation.R
Rscript R/06_figures.R
Rscript R/07_rapport.R
```

L'étape 4 lit les sorties de la méthode 1 dans `../Method1BvarBridge/resultats/`
pour produire la comparaison : elle suppose donc que la première chaîne a tourné.

Les classeurs sources sont lus dans `../SourceData/` et ne sont jamais modifiés.

## Résultat

Sur les 48 mêmes trimestres que la méthode 1 :

| | RMSE | Ratio | Corrélation |
|---|---|---|---|
| Facteurs dynamiques | 2,24 % | 0,983 | 0,14 |
| Méthode 1 | 1,94 % | 0,852 | 0,77 |

Le test de Diebold-Mariano donne p = 0,137 : l'écart n'est pas statistiquement
significatif sur un échantillon aussi court. Par branche, les facteurs dynamiques
l'emportent dans 5 cas sur 16.

Le diagnostic compte plus que le classement, et il est développé en section 9 du
rapport : la part de variance de la cible expliquée par les facteurs varie
énormément d'une branche à l'autre, et reste faible là où le panel est pourtant
riche. Les facteurs captent la variation commune des indicateurs, qui n'est pas
la variation pertinente pour la valeur ajoutée.

## Contrôles

`R/05_validation.R` exécute sept contrôles d'antériorité, dont le décisif : on
perturbe violemment les données postérieures à une origine, on refait la
prévision, et l'on vérifie qu'elle n'a pas bougé. Aucune relecture de code ne
donne cette garantie.

`tests/` vérifie le moteur lui-même sur des données simulées dont on connaît la
vérité : filtre contre lisseur, équation de Lyapunov, invariance par rotation du
composant commun, convergence de l'EM.
