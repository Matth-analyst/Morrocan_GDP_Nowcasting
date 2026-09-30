# ============================================================================
# rapport_global.R -- Le rapport unique : methode, mathematiques, resultats
# ============================================================================
# Ce rapport se lit seul. Il reprend la chaine complete, de la donnee brute au
# chiffre publie, avec les developpements mathematiques, les justifications
# statistiques et la lecture economique.
#
# REGLE DE REDACTION : le rapport ne parle JAMAIS de l'environnement de
# travail -- ni scripts, ni fichiers, ni dossiers, ni versions de logiciel. Il
# expose une methode et des resultats, pas une mise en oeuvre.
# ============================================================================

source("R/00_setup.R")
source("R/fonctions/information_set.R")
source("R/fonctions/transformations.R")
source("R/fonctions/donnees.R")
source("R/fonctions/rapport.R")

CHEMIN <- file.path(DOSSIER_RAPPORT, "rapport_global.html")

# --- feuille de style : composition en deux colonnes ------------------------
# Le titre, le sommaire, les figures, les tableaux, les equations, le glossaire
# et la bibliographie occupent la largeur entiere ; seul le texte courant est
# compose en deux colonnes, comme dans un article de revue.
css_global <- function() paste0(css_rapport(), '
/* ------------------------------------------------------------------ page */
body{font-size:12.8px;line-height:1.58;}
.page{max-width:1080px;padding:64px 72px 96px;}

/* ----------------------------------------------------------------- titre */
header.titre{border-bottom:1.5px solid var(--encre);padding-bottom:26px;
             margin-bottom:30px;text-align:center;}
header.titre h1{font-size:28px;line-height:1.22;margin:12px auto 10px;max-width:760px;}
header.titre .sous{font-size:13.5px;max-width:700px;margin:0 auto;}
header.titre .meta{font-size:11.5px;margin-top:16px;}

/* -------------------------------------------------------------- sommaire */
.sommaire{font-size:12px;column-count:2;column-gap:34px;padding:16px 22px;}
.sommaire strong{column-span:all;display:block;margin-bottom:6px;}
.cles .v{font-size:19px;} .cle{padding:11px 14px;}

/* ------------------------------------------- le corps, en deux colonnes */
/* Les colonnes sont separees par un BLANC : pas de filet vertical.        */
.corps{column-count:2;column-gap:34px;text-align:justify;
       hyphens:auto;-webkit-hyphens:auto;}
.corps p{margin:0 0 8px;text-indent:0;}
.corps h2,.corps h3,.corps h4{break-after:avoid;}

/* titres : dans la colonne, comme dans une revue */
h2{font-size:15.5px;font-weight:600;margin:26px 0 7px;padding-bottom:5px;
   border-bottom:1px solid var(--trait);letter-spacing:.005em;}
h2 .num{font-size:13px;margin-right:8px;}
h3{font-size:13.2px;font-weight:600;margin:17px 0 5px;}
h4{font-size:12.2px;font-weight:600;font-style:italic;color:var(--encre);
   margin:13px 0 4px;}

/* ------------------------------------- exhibits : pleine largeur, centres */
.corps figure,.corps table,.corps .tabcap{column-span:all;}
figure{margin:22px auto 26px;text-align:center;}
figure img{display:block;margin:0 auto;max-width:86%;height:auto;
           border:1px solid var(--trait);}
figcaption{max-width:86%;margin:9px auto 0;text-align:left;font-size:10.8px;}
table{width:auto;max-width:92%;margin:14px auto 0;font-size:11.2px;}
thead th{padding:5px 9px;} tbody td{padding:4px 9px;}
.tabcap{max-width:92%;margin:6px auto 22px;text-align:left;font-size:10.8px;}

/* ------------------------------------------------------------- equations */
/* Les formules gardent le bloc a filet gris : elles doivent se detacher du
   texte courant. La pile de polices vise le rendu dune composition TeX ;
   Cambria Math, presente sous Windows, est une police mathematique OpenType
   complete et cest elle qui sera retenue dans la plupart des cas. */
.eq{margin:16px 0 18px;padding:2px 0;background:none;border:0;gap:14px;}
.eqc{font-family:"Latin Modern Math","STIX Two Math","Cambria Math","XITS Math",
     "TeX Gyre Termes Math","Asana Math",Cambria,Georgia,"Times New Roman",serif;
     font-size:14.5px;font-weight:600;line-height:1.95;letter-spacing:.004em;}
.eqc .fr .hi{border-bottom:1.3px solid var(--encre);padding:0 .4em .1em;}
.eqc .fr .lo{padding:.12em .4em 0;}
.eqc .big{font-size:1.45em;font-weight:500;}
.eqc sub,.eqc sup{font-weight:600;}
.eqc .rel{font-style:normal;padding:0 .42em;font-weight:500;}
.eqn{font-size:11px;font-weight:400;}

/* math en ligne : meme police, poids normal pour ne pas alourdir le texte */
.mi{font-family:"Latin Modern Math","STIX Two Math","Cambria Math","XITS Math",
    "TeX Gyre Termes Math","Asana Math",Cambria,Georgia,"Times New Roman",serif;
    font-style:italic;font-weight:500;}

/* -------------------------------------------------------------- les notes */
/* Ni cartes ni aplats : un paragraphe en italique, legerement en retrait,  */
/* qui reste dans la colonne et dans le fil de la lecture.                  */
/* Aucun cadre, aucun aplat : le passage se distingue par la mise en italique
   et par un leger retrait. Il peut donc se couper entre deux colonnes sans
   que la coupure se voie, ce que ne permet pas un encadre. */
.note{font-style:italic;font-size:12.4px;line-height:1.56;
      margin:11px 0 12px;padding:0 10px 0 16px;color:#2a2a2a;
      break-inside:auto;}
.note p{margin:0 0 6px;} .note p:last-child{margin-bottom:0;}
.note-t{font-style:normal;font-weight:600;font-variant-caps:all-small-caps;
        letter-spacing:.05em;color:var(--encre);margin-right:.4em;}
.note-a .note-t{color:#8a4708;}
.note-i .note-t{color:var(--accent);}
.note strong{font-style:italic;font-weight:600;}
.note em{font-style:normal;}
.note .eq{font-style:normal;margin:11px 0 12px;padding:2px 0;background:none;border:0;}

/* appels de reference */
sup.ap{font-size:9px;line-height:0;vertical-align:super;font-weight:600;}
sup.ap a{color:var(--accent);text-decoration:none;padding:0 .5px;}

/* ------------------------------------------- fin de document, pleine page */
.pleine{column-span:all;}
.biblio{font-size:11.6px;}
.biblio ol{padding-left:28px;} .biblio li{margin:5px 0;}
.biblio li::marker{font-weight:600;color:var(--doux);}
footer{font-size:11px;}

@media print{
  body{font-size:9.3pt;} .page{max-width:none;padding:0;}
  .corps{column-gap:26px;} figure img{max-width:92%;} table{max-width:96%;}
}
@media (max-width:900px){
  .page{padding:34px 26px 70px;}
  .corps,.sommaire{column-count:1;}
  figure img,figcaption,table,.tabcap{max-width:100%;}
}
')

#' Ecrit le rapport avec la feuille de style propre a ce document.
ecrire_global <- function(corps, titre_page, chemin) {
  html <- paste0(
    '<!doctype html>\n<html lang="fr">\n<head>\n<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
    sprintf('<title>%s</title>\n', titre_page),
    '<style>', css_global(), '</style>\n</head>\n<body>\n<div class="page">\n',
    paste0(corps, collapse = "\n"),
    '\n</div>\n</body>\n</html>\n')
  con <- file(chemin, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(html), con)
  invisible(chemin)
}
init_compteurs()
assign(".n_equation", 0L, envir = globalenv())
r <- function(x) file.path(DOSSIER_RESULTATS, x)

# --- notes : un paragraphe en italique, non une carte -----------------------
# Les encadres du gabarit commun occupent toute la largeur, ce qui hache la
# lecture en deux colonnes. Ici, une note est un paragraphe italique en retrait
# dont le titre court en tete de ligne : elle reste dans le fil du texte.
note <- function(titre, corps, classe = "") {
  corps <- sub("^\\s*<p>", "<p>@@T@@", corps)
  if (!grepl("@@T@@", corps, fixed = TRUE)) corps <- paste0("<p>@@T@@", corps, "</p>")
  titre <- sub("\\.$", "", titre)
  corps <- sub("@@T@@", sprintf('<span class="note-t">%s.</span> ', titre),
               corps, fixed = TRUE)
  sprintf('<div class="note %s">%s</div>', classe, corps)
}
definition <- function(titre, corps) note(titre, corps)
intuition  <- function(corps) note("L\u2019idée", corps, "note-i")

cat("\n[1/3] Lecture des resultats\n")

statsva  <- lire_csv(r("02_stats_descriptives_va.csv"))
station  <- lire_csv(r("02_tests_stationnarite.csv"))
diag_ind <- lire_csv(r("02_diagnostic_indicateurs.csv"))
corrbr   <- lire_csv(r("02_correlations_branches.csv"))
hyper    <- lire_csv(r("03_choix_hyperparametres.csv"))
hyp_orig <- lire_csv(r("03_hyperparametres_par_origine.csv"))
chocs    <- lire_csv(r("03_comparaison_regles_choc.csv"))
diag3    <- lire_csv(r("03_diagnostics_branches.csv"))
instab   <- lire_csv(r("03e_diagnostic_instabilite.csv"))
effet    <- lire_csv(r("03e_effet_correction.csv"))
selpo    <- lire_csv(r("04_selection_par_origine.csv"))
ordre5   <- lire_csv(r("05_ordre_retenu.csv"))
perio5   <- lire_csv(r("05_par_periode.csv"))
poids6   <- lire_csv(r("06_poids.csv"))
ag       <- lire_csv(r("06_agregat.csv")) %>% dplyr::mutate(origine = as.Date(origine))
ev_ag    <- lire_csv(r("06_evaluation_agregat.csv"))
ecartf   <- lire_csv(r("06_ecart_formules.csv"))
ctrl     <- lire_csv(r("07_controles_antilookahead.csv"))
nonreg   <- lire_csv(r("07_non_regression.csv"))
intra    <- lire_csv(r("09_perimetre_commun.csv"))
m3m      <- lire_csv(r("09_test_m3_mensuel.csv"))
epis_br  <- lire_csv(r("09b_par_episode.csv"))
dm_gr    <- lire_csv(r("10_dm_groupe.csv"))
icratio  <- lire_csv(r("10_intervalles_ratio.csv"))
retard   <- lire_csv(r("11_retard_va.csv"))
calib    <- lire_csv(r("12_calibration.csv"))
decomp   <- lire_csv(r("12_decomposition.csv"))
calib2   <- lire_csv(r("12b_calibration_comparee.csv"))
ic_now   <- lire_csv(r("12c_nowcast_publie.csv"))
ic_couv  <- lire_csv(r("12c_couverture.csv"))
bench    <- lire_csv(r("13_evaluation.csv"))
dm13     <- lire_csv(r("13_dm_contre_systeme.csv"))
rob_d    <- lire_csv(r("14_robustesse_delta.csv"))
rob_f    <- lire_csv(r("14_robustesse_fenetre.csv"))
rob_s    <- lire_csv(r("14_robustesse_selection.csv"))
sais     <- lire_csv(r("14_saisonnalite.csv"))
courbe_d <- lire_csv(r("14b_courbe_delta.csv"))
comp_d   <- lire_csv(r("14b_estime_contre_constante.csv"))
stab_d   <- lire_csv(r("14b_stabilite_delta.csv"))
contrib  <- lire_csv(r("15_contributions_branches.csv"))
resid    <- lire_csv(r("15_diagnostics_residus.csv"))
epis     <- lire_csv(r("15_episodes.csv"))
stabco   <- lire_csv(r("15_stabilite_coefficients.csv"))
comp16   <- lire_csv(r("16_comparaison.csv"))
dm16     <- lire_csv(r("16_dm_direct_indirect.csv"))
nc_br    <- lire_csv(r("08_nowcast_courant_branches.csv"))
nc_ag    <- lire_csv(r("08_nowcast_courant_agregat.csv"))
couv     <- charger_couverture()
vivier   <- lire_csv(file.path(DOSSIER_DATA, "sommaire_vivier.csv"))
varia    <- lire_csv(r("04b_comparaison_variantes.csv"))
kalcomp  <- lire_csv(r("04c_comparaison_kalman.csv"))
kalval   <- lire_csv(r("04c_validation_kalman.csv"))
etat     <- lire_csv(r("04d_sensibilite_seuil.csv"))
conj     <- lire_csv(r("03c_grille_conjointe.csv"))
leviers  <- lire_csv(r("03_comparaison_leviers.csv"))
res3     <- lire_csv(r("03_diagnostics_residus.csv"))
pertes4  <- lire_csv(r("04_agregation_pertes.csv"))
dmbr     <- lire_csv(r("10_dm_par_branche.csv"))
dmext    <- lire_csv(r("09b_dm_groupe.csv"))
calend   <- lire_csv(r("11_calendrier_indicateurs.csv"))
specs3   <- lire_csv(r("03_comparaison_specifications.csv"))
erreurs_t <- lire_csv(r("13_erreurs_par_trimestre.csv"))
seuilT   <- lire_csv(r("09_seuil_trimestriel.csv"))
selcond  <- lire_csv(r("09_selection_conditionnelle.csv"))
conjval  <- lire_csv(r("03c_validation_test.csv"))
dm3      <- lire_csv(r("03_tests_diebold_mariano.csv"))
comb_bil <- lire_csv(r("04c_comblement_bilan.csv"))
diagtrim <- lire_csv(r("04_indicateurs_trimestriels_diagnostic.csv"))
meil5    <- lire_csv(r("05_meilleur_par_branche.csv"))
scen9    <- lire_csv(r("09_comparaison_scenarios.csv"))

# --- raccourcis ---------------------------------------------------------------
b_tout <- function(mod) bench[bench$modele == mod & bench$periode == "toutes origines", ]
b_hors <- function(mod) bench[bench$modele == mod & bench$periode == "hors 2020", ]
b_2020 <- function(mod) bench[bench$modele == mod & bench$periode == "2020", ]
cd     <- function(d, per) courbe_d$ratio[abs(courbe_d$delta - d) < 1e-9 &
                                            courbe_d$periode == per]
vs     <- function(x) stab_d$valeur[stab_d$mesure == x]
ip     <- function(sc, mo) intra$ratio_median[intra$scenario == sc &
                                                intra$modele == mo &
                                                intra$periode == "toutes origines"]

ratio_ag  <- ev_ag$ratio[ev_ag$modele == "Nowcast du projet"]
correl_ag <- ev_ag$correlation[ev_ag$modele == "Nowcast du projet"]
ratio_bv  <- ev_ag$ratio[ev_ag$modele == "BVAR seul, agrege"]
n_trim    <- nrow(ag)
cible_lab <- nc_ag$trimestre[1]
now_pct   <- nc_ag$nowcast_pct[1]
ic80      <- ic_now[ic_now$niveau == 0.80, ]
couv80    <- ic_couv$couverture[ic_couv$niveau == 0.80 &
                                  ic_couv$echelle == ic_now$echelle[1]]

cat("\n[2/3] Composition\n")
h <- character(0)
ajouter <- function(...) h <<- c(h, paste0(..., collapse = ""))

#' Pourcentage signe, virgule decimale.
# --- references numerotees ---------------------------------------------------
# Les travaux cites sont numerotes dans l'ordre de leur PREMIERE apparition, et
# la bibliographie finale les restitue dans cet ordre.
BIBLIO <- c(
  higgins = paste0("Higgins, P. (2014). <em>GDPNow: A Model for GDP Nowcasting</em>, ",
                   "Federal Reserve Bank of Atlanta Working Paper 2014-7."),
  wallis  = paste0("Wallis, K. F. (1986). « Forecasting with an econometric model: ",
                   "the ragged edge problem », <em>Journal of Forecasting</em>, 5(1), ",
                   "13-26."),
  dickey  = paste0("Dickey, D. A. et Fuller, W. A. (1979). « Distribution of the ",
                   "Estimators for Autoregressive Time Series with a Unit Root », ",
                   "<em>Journal of the American Statistical Association</em>, 74(366), ",
                   "427-431."),
  green   = paste0("Green, S. B. (1991). « How Many Subjects Does It Take To Do A ",
                   "Regression Analysis? », <em>Multivariate Behavioral Research</em>, ",
                   "26(3), 499-510."),
  litter  = paste0("Litterman, R. B. (1986). « Forecasting with Bayesian Vector ",
                   "Autoregressions: Five Years of Experience », <em>Journal of ",
                   "Business &amp; Economic Statistics</em>, 4(1), 25-38."),
  banbura = paste0("Bańbura, M., Giannone, D. et Reichlin, L. (2010). « Large Bayesian ",
                   "Vector Auto Regressions », <em>Journal of Applied Econometrics</em>, ",
                   "25(1), 71-92."),
  glp     = paste0("Giannone, D., Lenza, M. et Primiceri, G. E. (2015). « Prior ",
                   "Selection for Vector Autoregressions », <em>Review of Economics and ",
                   "Statistics</em>, 97(2), 436-451."),
  hampel  = paste0("Hampel, F. R. (1974). « The Influence Curve and Its Role in Robust ",
                   "Estimation », <em>Journal of the American Statistical ",
                   "Association</em>, 69(346), 383-393."),
  schwarz = paste0("Schwarz, G. (1978). « Estimating the Dimension of a Model », ",
                   "<em>The Annals of Statistics</em>, 6(2), 461-464."),
  doz     = paste0("Doz, C., Giannone, D. et Reichlin, L. (2011). « A Two-Step ",
                   "Estimator for Large Approximate Dynamic Factor Models Based on ",
                   "Kalman Filtering », <em>Journal of Econometrics</em>, 164(1), ",
                   "188-205."),
  harvey_b = paste0("Harvey, A. C. (1989). <em>Forecasting, Structural Time Series ",
                    "Models and the Kalman Filter</em>, Cambridge University Press."),
  bates   = paste0("Bates, J. M. et Granger, C. W. J. (1969). « The Combination of ",
                   "Forecasts », <em>Operational Research Quarterly</em>, 20(4), ",
                   "451-468."),
  stock   = paste0("Stock, J. H. et Watson, M. W. (2004). « Combination Forecasts of ",
                   "Output Growth in a Seven-Country Data Set », <em>Journal of ",
                   "Forecasting</em>, 23(6), 405-430."),
  smith   = paste0("Smith, J. et Wallis, K. F. (2009). « A Simple Explanation of the ",
                   "Forecast Combination Puzzle », <em>Oxford Bulletin of Economics and ",
                   "Statistics</em>, 71(3), 331-355."),
  claes   = paste0("Claeskens, G., Magnus, J. R., Vasnev, A. L. et Wang, W. (2016). ",
                   "« The Forecast Combination Puzzle: A Simple Theoretical ",
                   "Explanation », <em>International Journal of Forecasting</em>, 32(3), ",
                   "754-762."),
  dm      = paste0("Diebold, F. X. et Mariano, R. S. (1995). « Comparing Predictive ",
                   "Accuracy », <em>Journal of Business &amp; Economic Statistics</em>, ",
                   "13(3), 253-263."),
  hln     = paste0("Harvey, D., Leybourne, S. et Newbold, P. (1997). « Testing the ",
                   "Equality of Prediction Mean Squared Errors », <em>International ",
                   "Journal of Forecasting</em>, 13(2), 281-291."),
  kunsch  = paste0("Künsch, H. R. (1989). « The Jackknife and the Bootstrap for General ",
                   "Stationary Observations », <em>The Annals of Statistics</em>, 17(3), ",
                   "1217-1241."),
  vovk    = paste0("Vovk, V., Gammerman, A. et Shafer, G. (2005). <em>Algorithmic ",
                   "Learning in a Random World</em>, Springer."),
  ljung   = paste0("Ljung, G. M. et Box, G. E. P. (1978). « On a Measure of Lack of Fit ",
                   "in Time Series Models », <em>Biometrika</em>, 65(2), 297-303."),
  shapiro = paste0("Shapiro, S. S. et Wilk, M. B. (1965). « An Analysis of Variance ",
                   "Test for Normality », <em>Biometrika</em>, 52(3-4), 591-611."),
  golub   = paste0("Golub, G. H., Heath, M. et Wahba, G. (1979). « Generalized ",
                   "Cross-Validation as a Method for Choosing a Good Ridge Parameter », ",
                   "<em>Technometrics</em>, 21(2), 215-223."),
  demiguel = paste0("DeMiguel, V., Garlappi, L. et Uppal, R. (2009). « Optimal Versus ",
                    "Naive Diversification: How Inefficient is the 1/N Portfolio ",
                    "Strategy? », <em>Review of Financial Studies</em>, 22(5), ",
                    "1915-1953."))

assign(".ordre_refs", character(0), envir = globalenv())

#' Appel de reference : renvoie le numero en exposant, en le creant au besoin.
ref <- function(cle) {
  stopifnot(cle %in% names(BIBLIO))
  o <- get(".ordre_refs", envir = globalenv())
  if (!(cle %in% o)) {
    o <- c(o, cle)
    assign(".ordre_refs", o, envir = globalenv())
  }
  n <- match(cle, o)
  sprintf('<sup class="ap"><a href="#ref%d" id="ap%d">%d</a></sup>', n, n, n)
}

#' Plusieurs references d'affilee, separees par une virgule fine.
refs <- function(...) paste0(vapply(c(...), ref, character(1)), collapse = "")


pc <- function(x, d = 2, unite = TRUE) sprintf("%s%s%s",
                                              ifelse(x >= 0, "+", "−"),
                                              nb(abs(x), d),
                                              if (unite) " %" else "")
# ============================================================================
# EN-TETE
# ============================================================================
ajouter('<header class="titre">',
        '<div class="sur">Nowcasting de la croissance marocaine</div>',
        '<h1>Prévoir la valeur ajoutée trimestrielle avant sa publication</h1>',
        '<p class="sous">Un système à trois étages — vecteur autorégressif ',
        'bayésien, équations passerelles sur indicateurs infra-trimestriels, ',
        'agrégation par indice de volume de Laspeyres — ',
        'construit, évalué et critiqué.</p>',
        sprintf('<p class="meta">Rapport intégral &middot; %s</p>',
                format(Sys.Date(), "%d %B %Y")),
        '</header>')

sections <- c(
  "Ce que le système prévoit, exactement",
  "Le cadre économique : une logique d'offre",
  "La convention d'information",
  "La cible : la valeur ajoutée par branche",
  "Le vivier d'indicateurs et sa constitution",
  "Transformations et stationnarité",
  "Le vecteur autorégressif bayésien",
  "Le choix des hyperparamètres",
  "Ruptures structurelles et branches instables",
  "Des indicateurs mensuels au trimestre",
  "La sélection récursive des indicateurs",
  "Les équations passerelles",
  "Le bord irrégulier de l'information",
  "La combinaison des deux prévisions",
  "Les branches sans indicateur",
  "L'agrégation en valeur ajoutée totale",
  "Le nowcasting au fil du trimestre",
  "Le protocole d'évaluation",
  "Résultat principal et étalons de comparaison",
  "Les écarts sont-ils significatifs ?",
  "Résultats par branche et contributions",
  "Performance par régime conjoncturel",
  "Prévoir directement ou par les branches ?",
  "L'incertitude autour du chiffre",
  "Robustesse des choix de modélisation",
  "Diagnostics du modèle",
  "Ce qui a été essayé sans succès",
  "Le nowcast du trimestre en cours",
  "Lecture économique",
  "Limites et prolongements",
  "Glossaire",
  "Références")
ajouter(sommaire_rapport(sections))

ajouter(chiffres_cles(c(
  "ratio d'erreur sur l'agrégat" = nb(ratio_ag, 3),
  "corrélation prévu / réalisé" = nb(correl_ag, 2),
  "trimestres évalués" = nb(n_trim),
  "branches couvertes par un indicateur" = sprintf("%d / 16", length(couv$couvertes)),
  "nowcast du trimestre en cours" = pc(now_pct),
  "intervalle, couverture mesurée" = sprintf("%.0f %%", 100 * couv80))))

ajouter('<div class="corps">')

# ============================================================================
# 1. CE QUE LE SYSTEME PREVOIT
# ============================================================================
ajouter("<h2 id='s1'><span class='num'>1.</span>Ce que le système prévoit, exactement</h2>")
ajouter("<p>Le système produit, à n'importe quelle date, une estimation de la ",
        "<strong>croissance trimestrielle en volume de la valeur ajoutée ",
        "totale</strong>, reconstruite à partir des seize branches de la nomenclature ",
        "nationale par un indice de volume de Laspeyres à poids en prix courants du ",
        "trimestre précédent.</p>")
ajouter(definition("Trois précisions, chacune nécessaire",
  paste0("<p><strong>En volume.</strong> Il ne s'agit pas d'un montant en dirhams mais ",
         "du taux de croissance d'un indice de volume : l'effet des prix en est ",
         "retiré.</p>",
         "<p><strong>Reconstruite.</strong> L'agrégat est calculé à partir des seize ",
         "branches selon la formule exposée à la section 16. Ce n'est pas le total ",
         "publié par l'institut statistique, qui n'est pas disponible ici — la portée ",
         "de cette différence est discutée à la section 30.</p>",
         "<p><strong>Valeur ajoutée, non produit intérieur brut.</strong> Il manque les ",
         "impôts sur les produits nets de subventions, qui séparent l'une de l'autre.</p>")))
ajouter(sprintf(paste0("<p>Pour le trimestre en cours, le chiffre est de ",
                       "<strong>%s</strong>, dans un intervalle de ",
                       "<strong>[%s ; %s] %%</strong> dont la couverture mesurée sur ",
                       "l'historique est de %s %%.</p>"),
                pc(now_pct), pc(ic80$bas_pct, 2, FALSE), pc(ic80$haut_pct, 2, FALSE),
                nb(100 * couv80)))

ajouter("<h3>1.1 Pourquoi un nowcast, et non une prévision</h3>")
ajouter("<p>Les comptes nationaux trimestriels sont publiés avec un délai de plusieurs ",
        "semaines après la fin du trimestre. Pendant cet intervalle, l'état de ",
        "l'économie du trimestre <em>écoulé</em> est inconnu, alors même que des ",
        "dizaines d'indicateurs mensuels le décrivent déjà — production industrielle, ",
        "crédit bancaire, recettes touristiques, ventes de ciment, débarquements de ",
        "pêche.</p>")
ajouter(intuition(paste0(
  "<p>Le nowcasting ne cherche pas à deviner l'avenir. Il cherche à <strong>lire le ",
  "présent plus tôt</strong> : à traduire une information partielle, dispersée et ",
  "publiée à des rythmes différents en une estimation de la grandeur qui, elle, ne ",
  "sera publiée que plus tard.</p>",
  "<p>C'est ce qui distingue le problème d'une prévision ordinaire. L'horizon est ",
  "court et l'information s'accumule pendant qu'on prévoit : la question n'est pas ",
  "seulement <em>quel modèle</em>, mais <strong>à quel moment du trimestre</strong> ",
  "on se place — d'où la section 17.</p>")))

ajouter("<h3>1.2 Comment lire ce rapport</h3>")
ajouter("<p>Tout ce qui suit s'appuie sur un petit nombre de notations et de mesures. ",
        "Elles sont rassemblées ici plutôt que dispersées, de façon qu'aucune ne soit ",
        "employée avant d'avoir été définie. Un glossaire complet figure en fin de ",
        "document.</p>")

ajouter("<h4>Les notations</h4>")
notn <- data.frame(
  Symbole = c("T", "j", "t, m", "g(j,T)", "x(k,m)", "X(k,T)", "w(j,T)",
              "chapeau : ĝ", "barre : ē", "ℐ(T)"),
  Signification = c(
    "le trimestre que l'on cherche à estimer, dit « trimestre cible »",
    "l'indice d'une branche, de 1 à 16",
    "un trimestre quelconque, un mois quelconque",
    "la croissance de la branche j au trimestre T — la grandeur à prévoir",
    "la valeur de l'indicateur k au mois m",
    "le même indicateur, agrégé au trimestre T",
    "le poids de la branche j dans la valeur ajoutée totale",
    "une quantité estimée ou prévue, par opposition à la valeur réalisée",
    "une moyenne sur l'échantillon",
    "l'ensemble de l'information disponible au moment où l'on estime T"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(notn, aligne_droite = integer(0)))
ajouter(legende_tableau("Les notations employées dans tout le document."))

ajouter("<h4>Les trois mots qui reviennent partout</h4>")
ajouter(definition("Origine, backtest, périmètre commun",
  paste0("<p>Une <strong>origine</strong> est une date à laquelle on se place pour ",
         "produire une estimation. Se placer à l'origine du deuxième trimestre 2018, ",
         "c'est faire comme si l'on était à cette date : on oublie tout ce qui a été ",
         "publié depuis, on réestime le modèle sur les seules données antérieures, et ",
         "on produit le chiffre qu'on aurait produit alors.</p>",
         "<p>Un <strong>backtest</strong> est la répétition de cette opération sur ",
         "toutes les origines passées. Il produit, pour chaque trimestre, un couple ",
         "(ce que le système aurait dit, ce qui s'est réellement passé). C'est de ce ",
         "couple que sortent toutes les mesures de qualité du rapport.</p>",
         "<p>Un <strong>périmètre commun</strong> est l'ensemble des trimestres où ",
         "<em>tous</em> les modèles comparés ont effectivement produit un chiffre. ",
         "Comparer des modèles sur des trimestres différents n'aurait pas de sens : ",
         "celui qui refuse de se prononcer sur les cas difficiles paraîtrait meilleur ",
         "sans l'être.</p>")))

ajouter("<h4>Les quatre mesures de qualité</h4>")
ajouter("<p>Une erreur de prévision est l'écart entre le réalisé et le prévu, ",
        m("e<sub>t</sub>"), " = ", m("g<sub>t</sub>"),
        " <span class='op'>&minus;</span> ", m("g&#770;<sub>t</sub>"),
        ". On la résume de quatre façons, qui ne disent pas la même chose.</p>")
mes <- data.frame(
  Mesure = c("MAE", "RMSFE", "Biais", "Corrélation", "Ratio"),
  `Ce qu'elle calcule` = c(
    "la moyenne des erreurs en valeur absolue",
    "la racine de la moyenne des erreurs au carré",
    "la moyenne des erreurs, avec leur signe",
    "le lien entre prévu et réalisé, entre −1 et +1",
    "le RMSFE divisé par l'écart-type de la série"),
  `Ce qu'elle dit` = c(
    "l'erreur typique ; peu sensible aux cas extrêmes",
    "l'erreur typique, mais qui pénalise lourdement les grosses erreurs",
    "s'il y a une tendance systématique à surestimer ou sous-estimer",
    "si le modèle suit le bon SENS des variations, indépendamment de l'ampleur",
    "si le modèle fait mieux que ne rien prévoir du tout"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(mes, aligne_droite = integer(0)))
ajouter(legende_tableau("Les mesures employées tout au long du rapport."))
ajouter(definition("Le ratio, la mesure principale",
  paste0("<p>C'est celle qui revient le plus souvent, et elle demande une seconde de ",
         "réflexion :</p>",
         eq(paste0("ratio = <span class='fr'><span class='hi'>RMSFE du modèle",
                   "</span><span class='lo'>écart-type de la série</span></span>")),
         "<p>Le dénominateur est l'erreur que commettrait quelqu'un qui prédirait ",
         "toujours la moyenne historique — c'est-à-dire qui ne prévoirait rien. Le ",
         "ratio compare donc le modèle à cette référence minimale.</p>",
         "<p><strong>Un ratio de 0,85 signifie que le modèle commet 85 % de l'erreur ",
         "qu'on commettrait sans lui.</strong> Sous 1, il apporte quelque chose ; ",
         "au-dessus de 1, il dégrade — mieux vaudrait alors ne rien faire.</p>",
         "<p>L'intérêt du ratio sur le RMSFE brut est qu'il est <strong>comparable d'une ",
         "branche à l'autre</strong>. Une branche dix fois plus volatile qu'une autre ",
         "aura mécaniquement un RMSFE dix fois plus grand sans être moins bien prévue. ",
         "Confondre les deux est l'erreur de lecture la plus fréquente sur ce genre de ",
         "tableau.</p>")))
ajouter(intuition(paste0(
  "<p>Pourquoi quatre mesures et non une seule ? Parce qu'un modèle peut être bon sur ",
  "l'une et mauvais sur l'autre, et que la différence est rarement anodine.</p>",
  "<p>Un modèle qui prédirait toujours la moyenne aurait un <em>biais</em> nul et un ",
  "RMSFE honorable — mais une corrélation nulle, puisqu'il ne suivrait aucune ",
  "variation. À l'inverse, un modèle qui anticipe correctement tous les retournements ",
  "mais en exagère l'ampleur aura une excellente corrélation et un mauvais RMSFE.</p>",
  "<p>C'est exactement ce que la section 19 rencontre : certains modèles de référence ",
  "ont une corrélation <em>négative</em>, ce qui est bien pire qu'une corrélation ",
  "nulle — ils vont systématiquement dans le mauvais sens.</p>")))

ajouter("<h4>Comment sont présentés les résultats</h4>")
ajouter("<p>Trois conventions de présentation, tenues partout.</p>")
ajouter("<ul>",
        "<li>Les croissances et les erreurs sont exprimées en <strong>pourcentage ou ",
        "en points de pourcentage</strong>. Un « point » est un écart entre deux ",
        "pourcentages : passer de 2 % à 3 % est une hausse d'un point ;</li>",
        "<li>les résultats sont presque toujours donnés <strong>en trois colonnes</strong> ",
        "— ensemble de la période, année 2020 seule, période hors 2020. La raison ",
        "apparaîtra à la section 19 : les quatre trimestres de la crise sanitaire ",
        "pèsent tellement qu'une moyenne d'ensemble peut masquer un résultat ",
        "inverse ;</li>",
        "<li>chaque figure est accompagnée d'un <strong>commentaire de lecture</strong> ",
        "qui dit ce qu'elle montre et ce qu'il faut en conclure — il n'est pas ",
        "décoratif.</li>",
        "</ul>")


# ============================================================================
# 2. LE CADRE ECONOMIQUE
# ============================================================================
ajouter("<h2 id='s2'><span class='num'>2.</span>Le cadre économique : une logique d'offre</h2>")
ajouter("<p>Le produit intérieur se mesure de trois façons équivalentes : par la ",
        "production (l'optique <em>offre</em>), par les emplois finals (l'optique ",
        "<em>demande</em>), par les revenus distribués. Le système adopte l'optique ",
        "d'offre et décompose la valeur ajoutée totale en seize branches d'activité.</p>")
ajouter(definition("Pourquoi l'offre plutôt que la demande",
  paste0("<p>Le modèle de référence en la matière, développé à la Réserve fédérale ",
         "d'Atlanta", ref("higgins"), ", suit l'optique de la demande : il prévoit séparément la ",
         "consommation, l'investissement, les échanges extérieurs. Ce choix y est ",
         "justifié par la richesse des statistiques mensuelles de dépense.</p>",
         "<p>Ici, la disponibilité statistique est inverse. Les indicateurs ",
         "infra-trimestriels disponibles décrivent massivement l'<em>activité ",
         "productive</em> — indices de production industrielle et minière, énergie, ",
         "ciment, crédit sectoriel, tourisme, pêche — et très peu la dépense. Suivre ",
         "l'optique de la demande reviendrait à modéliser des grandeurs sur lesquelles ",
         "on n'observe presque rien.</p>",
         "<p>L'optique d'offre a de surcroît un avantage interprétatif : chaque ",
         "indicateur se rattache <strong>naturellement</strong> à une branche. Le ",
         "ciment informe la construction, les nuitées l'hébergement. Le lien n'est pas ",
         "statistique, il est comptable.</p>")))

ajouter("<h3>2.1 Une économie à trois vitesses</h3>")
t2 <- statsva %>% dplyr::arrange(dplyr::desc(part_moyenne)) %>%
  dplyr::transmute(Branche = branche,
                   `Part moyenne (%)` = nb(100 * part_moyenne, 1),
                   `Croissance moy. (%)` = nb(croissance_moy_pct, 2),
                   `Volatilité (%)` = nb(volatilite_pct, 2))
ajouter(tbl(as.data.frame(t2), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Les seize branches, par poids décroissant. La croissance et la volatilité sont ",
  "trimestrielles, en volume.")))
ajouter("<p>Trois groupes se distinguent, et ils commandent toute la suite.</p>")
ajouter("<p><strong>Les branches lourdes et régulières</strong> — industrie de ",
        "transformation, commerce, administration — pèsent ensemble près de la moitié ",
        "de la valeur ajoutée avec une volatilité modérée. Elles font le socle.</p>")
ajouter("<p><strong>L'agriculture</strong> est un cas à part : un poids élevé et une ",
        "volatilité qui en fait, à elle seule, le principal moteur de la variance de ",
        "l'agrégat. La pluviométrie marocaine se transmet directement au produit ",
        "national, ce qui est le fait stylisé le plus connu de cette économie.</p>")
ajouter("<p><strong>Les branches légères et très volatiles</strong> — pêche, ",
        "hébergement-restauration — pèsent peu mais bougent beaucoup. Elles dégradent ",
        "les indicateurs de qualité par branche sans peser sur l'agrégat, ce dont il ",
        "faudra se souvenir à la section 21.</p>")
ajouter(figure("02_va_croissance.png", "La croissance trimestrielle des seize branches",
               paste0("L'échelle verticale commune rend visible l'écart d'amplitude ",
                      "entre branches. Le décrochage de 2020 se lit partout, mais son ",
                      "ampleur varie d'un facteur dix.")))
ajouter(figure("02_va_niveaux.png", "Les niveaux de valeur ajoutée par branche",
               paste0("Toutes les séries sont tendancielles, ce qui interdit de les ",
                      "modéliser en niveau. On distingue aussi, sur une branche, une ",
                      "rupture de niveau franche : elle est traitée à la section 9.")))

ajouter("<h3>2.2 Les branches bougent-elles ensemble ?</h3>")
q_corr <- stats::quantile(corrbr$correlation, c(0.1, 0.5, 0.9))
ajouter(sprintf(paste0("<p>La corrélation médiane entre croissances de branches vaut ",
                       "<strong>%s</strong>, avec un premier décile à %s et un dernier ",
                       "à %s. La comouvement est donc réel mais partiel.</p>"),
                nb(q_corr[2], 2), nb(q_corr[1], 2), nb(q_corr[3], 2)))
ajouter(intuition(paste0(
  "<p>Ce chiffre justifie l'architecture entière. S'il était proche de 1, une seule ",
  "équation sur l'agrégat suffirait — les branches ne diraient rien de plus que leur ",
  "somme. S'il était nul, il faudrait seize modèles indépendants et rien ne circulerait ",
  "de l'un à l'autre.</p>",
  "<p>À un niveau intermédiaire, <strong>l'information d'une branche renseigne ",
  "partiellement les autres</strong>, ce qui est exactement la situation où un modèle ",
  "vectoriel — qui relie les branches entre elles — bat seize modèles univariés.</p>")))
ajouter(figure("02_correlations_branches.png", "Corrélations entre branches",
               paste0("Des blocs apparaissent : les branches industrielles entre elles, ",
                      "les services entre eux. L'agriculture reste à l'écart, ce qui ",
                      "est cohérent avec sa dépendance climatique.")))

# ============================================================================
# 3. LA CONVENTION D'INFORMATION
# ============================================================================
ajouter("<h2 id='s3'><span class='num'>3.</span>La convention d'information</h2>")
ajouter("<p>C'est le point le plus important du travail, et celui qui décide de la ",
        "validité de tout le reste.</p>")
ajouter("<p>Soit ", m("T"), " le trimestre que l'on cherche à estimer. On note ",
        m("&#8497;<sub>T</sub>"), " l'<strong>ensemble d'information</strong> ",
        "effectivement disponible au moment où l'estimation est produite :</p>")
ajouter(eq(paste0(m("&#8497;<sub>T</sub>"), " = { ", m("g<sub>j,t</sub>"),
                  " : ", m("t"), " <span class='op'>&lt;</span> ", m("T"),
                  " } <span class='op'>&cup;</span> { ", m("x<sub>k,m</sub>"),
                  " : ", m("m"), " <span class='op'>&le;</span> ", m("m<sub>obs</sub>"),
                  " }")))
ajouter("<p>où ", m("g<sub>j,t</sub>"), " est la croissance de la branche ", m("j"),
        " au trimestre ", m("t"), ", ", m("x<sub>k,m</sub>"), " l'indicateur ",
        m("k"), " au mois ", m("m"), ", et ", m("m<sub>obs</sub>"),
        " le dernier mois observé.</p>")

ajouter("<h3>3.1 La règle</h3>")
ajouter("<div class='encadre'><span class='etiq'>Règle de récursivité</span>",
        "<p>À chaque trimestre cible, <strong>toute</strong> quantité du système est ",
        "recalculée sur le seul ", m("&#8497;<sub>T</sub>"), " : les paramètres du ",
        "modèle vectoriel, ses hyperparamètres, la liste des indicateurs retenus, les ",
        "coefficients des équations passerelles, le poids de combinaison, les poids ",
        "d'agrégation, et jusqu'aux moyennes et écarts-types servant à standardiser les ",
        "séries.</p>",
        "<p>Rien n'est estimé une fois pour toutes sur l'échantillon complet.</p></div>")
ajouter(intuition(paste0(
  "<p>La tentation inverse est forte et rarement innocente. Estimer un paramètre sur ",
  "toute la période, puis évaluer le modèle sur cette même période, revient à ",
  "<strong>laisser le modèle connaître le futur</strong> qu'on lui demande de ",
  "prévoir.</p>",
  "<p>L'effet n'est pas anodin : trois fuites de ce type ont été identifiées et ",
  "corrigées au cours de ce travail, et <em>chaque correction a dégradé les résultats ",
  "affichés</em>. C'est le signe qu'elles étaient nécessaires. Un protocole qui ",
  "n'améliore jamais rien quand on le durcit est un protocole qu'on n'a pas assez ",
  "regardé.</p>")))

ajouter("<h3>3.2 Les contrôles</h3>")
t3 <- ctrl %>% dplyr::transmute(Contrôle = controle, Objet = objet,
                                Vérification = description,
                                Résultat = resultat, Portée = detail)
ajouter(tbl(as.data.frame(t3), aligne_droite = integer(0)))
ajouter(legende_tableau(paste0(
  "Les contrôles anti-antériorité, exécutés à chaque production du système. Un échec ",
  "interrompt le calcul : il n'y a pas de mode dégradé.")))
ajouter(sprintf(paste0("<p>Un contrôle supplémentaire vérifie que la fonction qui ",
                       "produit le chiffre publié <strong>reproduit exactement</strong> ",
                       "la chaîne complète : sur %d comparaisons, l'écart maximal est de ",
                       "l'ordre de %s — c'est-à-dire la précision de la machine, et non ",
                       "une tolérance choisie.</p>"),
                sum(!is.na(nonreg$ecart_bvar)) + sum(!is.na(nonreg$ecart_bridge)),
                format(max(c(abs(nonreg$ecart_bvar), abs(nonreg$ecart_bridge)),
                           na.rm = TRUE), digits = 3)))
ajouter("<p>Ce point mérite d'être souligné : <strong>le chiffre publié et le chiffre ",
        "du backtest empruntent le même chemin</strong>. Ce qui a été évalué est donc ",
        "bien ce qui est diffusé, et non une variante simplifiée.</p>")

# ============================================================================
# 4. LA CIBLE
# ============================================================================
ajouter("<h2 id='s4'><span class='num'>4.</span>La cible : la valeur ajoutée par branche</h2>")
ajouter(sprintf(paste0("<p>La grandeur à prévoir est la valeur ajoutée trimestrielle en ",
                       "volume de chacune des seize branches, disponible sur %d ",
                       "trimestres, de %s à %s.</p>"),
                max(statsva$n_trimestres), min(statsva$debut), max(statsva$fin)))
ajouter("<p>Le système ne travaille jamais sur le niveau mais sur sa ",
        "<strong>différence logarithmique</strong> :</p>")
ajouter(eq(paste0(m("g<sub>j,t</sub>"), " = log ", m("V<sub>j,t</sub>"),
                  " <span class='op'>&minus;</span> log ",
                  m("V<sub>j,t&minus;1</sub>"),
                  " <span class='op'>&asymp;</span> ",
                  "<span class='fr'><span class='hi'>",
                  m("V<sub>j,t</sub>"), " <span class='op'>&minus;</span> ",
                  m("V<sub>j,t&minus;1</sub>"), "</span><span class='lo'>",
                  m("V<sub>j,t&minus;1</sub>"), "</span></span>")))
ajouter(definition("Pourquoi la différence logarithmique",
  paste0("<p>Trois raisons, dont une seule est statistique.</p>",
         "<p><strong>Stationnarité.</strong> Le niveau de la valeur ajoutée est ",
         "tendanciel : sa moyenne dérive, sa variance croît. Un modèle estimé sur des ",
         "séries non stationnaires produit des relations fallacieuses. La différence ",
         "logarithmique élimine la tendance stochastique.</p>",
         "<p><strong>Additivité.</strong> Les taux logarithmiques s'additionnent dans le ",
         "temps : la croissance sur quatre trimestres est la somme des quatre taux. Les ",
         "taux arithmétiques, eux, se composent multiplicativement.</p>",
         "<p><strong>Symétrie.</strong> Une baisse de 50 % suivie d'une hausse de 100 % ",
         "ramène au point de départ ; en logarithme, cela s'écrit ",
         m("&minus;0,69"), " puis ", m("+0,69"),
         ", ce qui est visiblement symétrique. En taux arithmétique, ",
         m("&minus;50 %"), " et ", m("+100 %"),
         " ne le sont pas, et une moyenne de taux arithmétiques est biaisée.</p>")))

ajouter("<h3>4.1 Le test de stationnarité</h3>")
n_niv <- sum(!station$stationnaire_niveau); n_dlog <- sum(station$stationnaire_dlog)
ajouter(sprintf(paste0("<p>Le test de Dickey-Fuller augmenté confirme la nécessité de ",
                       "différencier : sur les seize branches, <strong>%d rejettent la ",
                       "stationnarité en niveau</strong> et <strong>%d l'acceptent après ",
                       "différence logarithmique</strong>.</p>"), n_niv, n_dlog))
ajouter(definition("Ce qu'est une racine unitaire, et pourquoi elle pose problème",
  paste0("<p>Une série a une <strong>racine unitaire</strong> quand un choc qu'elle ",
         "subit ne s'estompe jamais : il déplace définitivement son niveau. C'est le ",
         "cas de la marche aléatoire ", m("y<sub>t</sub>"), " = ",
         m("y<sub>t&minus;1</sub>"), " <span class='op'>+</span> ",
         m("&epsilon;<sub>t</sub>"), ", dont la variance croît sans limite.</p>",
         "<p>Le danger est la <strong>régression fallacieuse</strong> : deux séries ",
         "indépendantes ayant chacune une racine unitaire présentent, si on les régresse ",
         "l'une sur l'autre, une relation apparemment très significative — qui n'existe ",
         "pas. Le test statistique y est trompeur, pas seulement imprécis.</p>",
         "<p>Le test de Dickey-Fuller augmenté", ref("dickey"),
         " vérifie la présence de cette racine. Le ",
         "qualificatif « augmenté » signifie qu'on ajoute des retards de la série ",
         "différenciée pour absorber l'autocorrélation résiduelle, faute de quoi le test ",
         "conclurait à tort.</p>")))
ajouter("<p>Le test régresse la série sur son retard et un terme de tendance :</p>")
ajouter(eq(paste0("&Delta;", m("y<sub>t</sub>"), " = ", m("&alpha;"),
                  " <span class='op'>+</span> ", m("&beta;t"),
                  " <span class='op'>+</span> ", m("&gamma;y<sub>t&minus;1</sub>"),
                  " <span class='op'>+</span> ",
                  "<span class='big'>&sum;</span><sub class='num'>i=1</sub><sup class='num'>p</sup> ",
                  m("&delta;<sub>i</sub>"), "&Delta;", m("y<sub>t&minus;i</sub>"),
                  " <span class='op'>+</span> ", m("&epsilon;<sub>t</sub>"))))
ajouter("<p>et teste ", m("H<sub>0</sub>"), " : ", m("&gamma;"), " = 0, c'est-à-dire ",

        "la présence d'une racine unitaire. Le rejet de ", m("H<sub>0</sub>"),
        " conclut à la stationnarité.</p>")
ajouter(figure("02_stationnarite.png", "Stationnarité avant et après différenciation",
               paste0("Les probabilités critiques en niveau se dispersent au-dessus des ",
                      "seuils usuels ; après différenciation elles s'écrasent toutes ",
                      "contre la borne inférieure du test.")))
ajouter(figure("02_distribution_croissance.png",
               "Distribution des croissances trimestrielles",
               paste0("Les distributions sont plus pointues et à queues plus épaisses ",
                      "qu'une gaussienne. C'est une propriété de la série, non du ",
                      "modèle, et elle justifiera le choix d'intervalles non gaussiens ",
                      "à la section 24.")))

# ============================================================================
# 5. LE VIVIER
# ============================================================================
ajouter("<h2 id='s5'><span class='num'>5.</span>Le vivier d'indicateurs et sa constitution</h2>")
t5 <- vivier %>% dplyr::arrange(dplyr::desc(total)) %>%
  dplyr::transmute(Branche = branche, `Mensuels` = mensuel,
                   `Trimestriels` = trimestriel, `Total` = total,
                   `Début` = substr(debut, 1, 7))
ajouter(tbl(as.data.frame(t5), aligne_droite = 2:4))
ajouter(legende_tableau(sprintf(paste0(
  "Les %d séries candidates, réparties sur %d branches. Les quatre branches absentes ",
  "de ce tableau ne disposent d'aucun indicateur infra-trimestriel."),
  sum(vivier$total), nrow(vivier))))
ajouter(sprintf(paste0("<p>La couverture est <strong>très inégale</strong> : les trois ",
                       "premières branches concentrent %.0f %% des séries, tandis que ",
                       "l'agriculture — qui pèse pourtant %.0f %% de la valeur ajoutée ",
                       "et domine sa variance — n'en compte que cinq.</p>"),
                100 * sum(sort(vivier$total, decreasing = TRUE)[1:3]) / sum(vivier$total),
                100 * statsva$part_moyenne[statsva$branche == "Agriculture"]))
ajouter(intuition(paste0(
  "<p>Ce déséquilibre n'est pas un défaut de collecte : il reflète ce que ",
  "l'appareil statistique mesure à fréquence rapprochée. On observe bien la production ",
  "industrielle, l'énergie, le crédit ; on observe mal les services non marchands, et ",
  "l'agriculture ne se mesure qu'à la récolte.</p>",
  "<p>Conséquence directe : <strong>les branches les plus utiles à prévoir sont ",
  "parfois les moins observées</strong>. C'est une contrainte structurelle, pas un ",
  "choix, et elle borne ce que la méthode peut atteindre.</p>")))

ajouter("<h3>5.1 D'où viennent ces séries, et ce que le système en écarte</h3>")
ajouter("<p>Le classeur source réunit 434 séries, retenues sur des <strong>critères ",
        "purement économiques</strong> : un indicateur y figure parce qu'il décrit ",
        "plausiblement l'activité de la branche à laquelle il est rattaché. Le ciment ",
        "pour la construction, les nuitées pour l'hébergement, les débarquements pour la ",
        "pêche. Aucun calcul n'intervient dans ce tri.</p>")
ajouter(note("Pourquoi ce point décide de la validité du reste",
  paste0("<p>Un tri <em>économique</em> ne regarde pas la cible : juger que le ciment ",
         "informe la construction est un énoncé sur la nature de l'indicateur, stable ",
         "dans le temps, qui aurait été le même à n'importe quelle date.</p>",
         "<p>Un tri <em>statistique</em> calculé sur toute la période serait tout autre ",
         "chose. Écarter les séries « qui ne marchent pas » reviendrait à choisir en 2014 ",
         "ce qui aura bien fonctionné jusqu'en 2026, et la performance mesurée ensuite ",
         "serait gonflée d'autant. Aucune rigueur en aval ne rattrape une antériorité ",
         "située dans le point de départ.</p>")))
ajouter("<p>Le système prend ce vivier <strong>tel quel</strong> et n'en écarte que trois ",
        "séries, sur deux verdicts objectifs.</p>")
excl <- data.frame(
  Motif = c("Série constante : variance nulle", "Doublon strict d'une autre série"),
  `Séries écartées` = c(1L, 2L),
  `Ce que cela justifie` = c(
    "une série sans variance ne peut rien expliquer, par définition",
    "la même information comptée deux fois pèserait double dans la sélection"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(excl, aligne_droite = 2))
ajouter(legende_tableau(paste0(
  "Les seules exclusions du vivier, soit 0,7 % des séries. Ni l'un ni l'autre de ces ",
  "verdicts ne fait intervenir la valeur ajoutée : ils se lisent dans la série ",
  "elle-même.")))
ajouter(figure("02_disponibilite_indicateurs.png",
               "Disponibilité des indicateurs dans le temps",
               paste0("Chaque ligne est une série. Les débuts sont échelonnés sur vingt ",
                      "ans : à une origine ancienne, une grande partie du vivier n'existe ",
                      "pas encore, ce qui explique que la sélection retienne moins ",
                      "d'indicateurs au début de l'exercice.")))
ajouter(figure("02_diagnostic_series.png", "Diagnostic des séries du vivier",
               paste0("Densité, longueur et volatilité de chaque série. Aucun seuil n'y ",
                      "est appliqué : le diagnostic documente le vivier, il ne le filtre ",
                      "pas.")))

ajouter("<h3>5.1.1 Toute la sélection statistique est reportée en aval</h3>")
ajouter("<p>C'est la conséquence directe de ce qui précède. Puisqu'on refuse de ",
        "sélectionner sur la cible une fois pour toutes, la sélection se fait ",
        "<strong>à chaque origine</strong>, sur la seule information antérieure. Elle ",
        "est décrite en détail à la section 11 ; deux seuils la gouvernent, et tous deux ",
        "sont des choix assumés plutôt que des règles empruntées.</p>")
ajouter("<p>Le test est l'inférence standard sur le coefficient de corrélation de ",
        "Pearson entre la série agrégée au trimestre et la croissance de la branche :</p>")
ajouter(eq(paste0(m("t"), " = ", m("r"),
                  " <span class='big'>&radic;</span><span style='text-decoration:overline'>",
                  "<span class='fr'><span class='hi'>", m("n"),
                  " <span class='op'>&minus;</span> 2</span><span class='lo'>1 ",
                  "<span class='op'>&minus;</span> ", m("r<sup>2</sup>"),
                  "</span></span></span> &nbsp;&nbsp;&#8764;&nbsp;&nbsp; ",
                  "Student(", m("n"), " <span class='op'>&minus;</span> 2)")))
ajouter("<p><strong>", m("&alpha;"), " = 0,10</strong> plutôt que 0,05, parce qu'il ",
        "s'agit d'une sélection exploratoire suivie d'une évaluation hors échantillon, ",
        "non d'un test confirmatoire. Écarter à tort un indicateur utile coûterait plus ",
        "qu'en admettre un inutile, que la suite éliminera.</p>")
ajouter("<p><strong>|", m("r"), "| &ge; 0,15</strong> contre le <em>piège du grand ",
        "échantillon</em> : avec près de 300 observations mensuelles, une corrélation de ",
        "0,12 devient « significative » sans avoir la moindre portée économique. La ",
        "significativité statistique et la pertinence économique divergent quand ",
        m("n"), " est grand ; le plancher rétablit la seconde.</p>")
ajouter("<p>S'y ajoute un plancher pratique de <strong>20 trimestres appariés</strong>, ",
        "en deçà desquels une corrélation n'a pas de sens, et un plafond de cinq ",
        "régresseurs.</p>")
ajouter(note("Aucune règle de taille d'échantillon n'est appliquée, et c'est délibéré",
  paste0("<p>La référence classique en la matière", ref("green"), " exige ", m("N"),
         " &ge; 104 <span class='op'>+</span> ", m("k"),
         " observations pour tester un coefficient individuel, soit 105 trimestres pour ",
         "un seul prédicteur.</p>",
         "<p>La valeur ajoutée trimestrielle marocaine n'en compte que 113 au total. ",
         "L'appliquer ne laisserait aucune marge pour l'évaluation hors échantillon, qui ",
         "est pourtant la seule preuve qui compte.</p>",
         "<p>Cette règle a par ailleurs été construite pour des contextes d'enquête, où ",
         "l'on peut recruter davantage d'observations ; une série macroéconomique a la ",
         "longueur que l'histoire lui a donnée. La littérature du nowcasting n'en ",
         "applique d'ailleurs aucune : la validité s'y établit <em>a posteriori</em>, par ",
         "la performance hors échantillon.</p>")))

ajouter("<h3>5.2 Une correction de données préalable</h3>")
ajouter("<p>Une part importante des séries mensuelles s'est révélée <strong>cumulée ",
        "depuis janvier</strong> : chaque valeur y est le cumul de l'année en cours, ",
        "remis à zéro au premier mois, et non le flux du mois.</p>")
ajouter("<p>La détection repose sur un test de remise à zéro :</p>")
ajouter(eq(paste0("part des années où ", m("x<sub>janvier</sub>"),
                  " <span class='op'>&lt;</span> ", m("x<sub>décembre</sub>"),
                  " <span class='op'>&times;</span> 0,5"), numerote = FALSE))
ajouter("<p>La séparation est sans ambiguïté : <strong>proche de 100 % sur les séries ",
        "cumulées, proche de 1 % sur les autres</strong>. La correction consiste à ",
        "différencier à l'intérieur de chaque année civile.</p>")
ajouter("<div class='encadre alerte'><span class='etiq'>Un garde-fou nécessaire</span>",
        "<p>Le test seul produit des <strong>faux positifs</strong> : une série ",
        "trimestrielle fortement saisonnière, dont le premier trimestre est ",
        "structurellement bas, déclenche le critère sans être cumulée.</p>",
        "<p>La correction est donc refusée si elle engendre plus de 5 % de flux ",
        "négatifs — un cumul authentique étant croissant par construction, sa ",
        "différenciation ne peut pas produire de flux négatifs en masse. Ce contrôle a ",
        "écarté 28 séries qui auraient été corrigées à tort.</p></div>")

# ============================================================================
# 6. TRANSFORMATIONS
# ============================================================================
ajouter("<h2 id='s6'><span class='num'>6.</span>Transformations et stationnarité</h2>")
ajouter("<p>Chaque indicateur reçoit une transformation dictée par sa ",
        "<strong>nature économique</strong>, non par un test automatique.</p>")
t6 <- data.frame(
  Nature = c("Flux (production, ventes, recettes)",
             "Stock ou encours (crédit, dépôts)",
             "Indice ou taux (utilisation des capacités, taux d'intérêt)",
             "Série déjà en variation"),
  `Agrégation au trimestre` = c("somme des trois mois", "dernier mois du trimestre",
                                "moyenne des trois mois", "somme"),
  Transformation = c("différence logarithmique", "différence logarithmique",
                     "différence simple", "aucune"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(t6, aligne_droite = integer(0)))
ajouter(legende_tableau("Règles d'agrégation temporelle et de transformation."))
ajouter(figure("02_regles_transformation.png",
               "Répartition des séries par règle appliquée",
               paste0("La majorité des séries sont des flux, agrégés par somme et ",
                      "transformés en différence logarithmique. Les indices et les ",
                      "encours forment deux groupes minoritaires mais économiquement ",
                      "distincts.")))
ajouter(figure("02_diagnostic_series.png", "Diagnostic des séries du vivier",
               paste0("Densité, longueur et volatilité de chaque série. Les points isolés ",
                      "en bas à gauche sont les séries courtes et trouées, que la cascade ",
                      "de critères écarte.")))
ajouter(intuition(paste0(
  "<p>La distinction entre <em>flux</em> et <em>stock</em> n'est pas une subtilité : ",
  "elle décide du chiffre. Sommer un encours de crédit sur trois mois n'a aucun sens ",
  "économique — on additionnerait trois fois le même stock. Prendre le dernier mois ",
  "d'une production mensuelle en perdrait les deux tiers.</p>",
  "<p>Une différence simple plutôt que logarithmique pour un taux tient à ce qu'un ",
  "taux peut être nul ou négatif, où le logarithme n'est pas défini, et à ce que la ",
  "variation d'un taux s'exprime naturellement en points.</p>")))

ajouter("<h3>6.1 La standardisation, et un piège qu'elle tend</h3>")
ajouter("<p>Les indicateurs sont centrés-réduits avant estimation. Cette opération ",
        "banale contient un piège d'antériorité :</p>")
ajouter(eq(paste0(m("z<sub>k,t</sub>"), " = ",
                  "<span class='fr'><span class='hi'>", m("x<sub>k,t</sub>"),
                  " <span class='op'>&minus;</span> ",
                  m("&mu;&#770;<sub>k</sub><sup>(T)</sup>"),
                  "</span><span class='lo'>", m("&sigma;&#770;<sub>k</sub><sup>(T)</sup>"),
                  "</span></span>"),
           numerote = TRUE))
ajouter("<p>L'exposant ", m("(T)"), " est essentiel : la moyenne et l'écart-type sont ",
        "calculés sur les seules observations antérieures à ", m("T"),
        ", et <strong>recalculés à chaque trimestre cible</strong>. Les estimer une fois ",
        "sur l'échantillon complet laisserait la moyenne de 2025 informer la prévision ",
        "de 2015.</p>")
ajouter(figure("02_standardisation_lookahead.png",
               "Standardisation récursive contre standardisation globale",
               paste0("L'écart entre les deux séries n'est pas négligeable aux premières ",
                      "origines, là où l'historique est court : c'est exactement là que ",
                      "l'antériorité ferait le plus de dégâts.")))

# ============================================================================
# 7. LE BVAR
# ============================================================================
ajouter("<h2 id='s7'><span class='num'>7.</span>Le vecteur autorégressif bayésien</h2>")
ajouter("<p>Premier étage du système : un modèle vectoriel reliant les seize branches ",
        "entre elles par leur passé commun.</p>")
ajouter(eq(paste0(m("y<sub>t</sub>"), " = ", m("c"), " <span class='op'>+</span> ",
                  "<span class='big'>&sum;</span><sub class='num'>i=1</sub><sup class='num'>p</sup> ",
                  m("A<sub>i</sub> y<sub>t&minus;i</sub>"),
                  " <span class='op'>+</span> ", m("&epsilon;<sub>t</sub>"),
                  " ,&nbsp;&nbsp; ", m("&epsilon;<sub>t</sub>"),
                  " &#8764; ", m("N"), "( 0 , ", m("&Sigma;"), " )")))
ajouter("<p>où ", m("y<sub>t</sub>"), " est le vecteur des seize croissances de branche ",
        "au trimestre ", m("t"), ", et ", m("p"), " le nombre de retards.</p>")

ajouter("<h3>7.1 Le problème de dimension</h3>")
ajouter(sprintf(paste0("<p>Avec ", m("n"), " = 16 branches et ", m("p"),
                       " retards, le modèle compte ", m("n"), "(", m("np"),
                       " <span class='op'>+</span> 1) coefficients — soit ",
                       "<strong>%d paramètres pour ", m("p"), " = 3</strong>, ",
                       "estimés sur une centaine de trimestres.</p>"), 16 * (16 * 3 + 1)))
ajouter("<p>L'estimation par moindres carrés est alors non seulement imprécise mais ",
        "<strong>numériquement instable</strong> : le modèle ajuste le bruit de ",
        "l'échantillon et extrapole n'importe quoi. C'est la raison d'être du traitement ",
        "bayésien.</p>")
ajouter(intuition(paste0(
  "<p>L'image est la suivante. Faire passer une droite par deux points est immédiat, ",
  "mais la droite ne dit rien : elle passe exactement par les points parce qu'elle a ",
  "autant de paramètres que d'observations.</p>",
  "<p>Un modèle qui dispose de presque autant de paramètres que d'observations fait la ",
  "même chose : il reproduit parfaitement le passé <em>et ne prévoit rien</em>, parce ",
  "qu'il a appris le bruit propre à cet échantillon. Le nombre d'observations moins le ",
  "nombre de paramètres — les <strong>degrés de liberté</strong> — mesure ce qui reste ",
  "pour estimer honnêtement.</p>")))

ajouter(definition("Le vocabulaire bayésien, en trois mots",
  paste0("<p>L'approche classique traite les coefficients comme des nombres fixes mais ",
         "inconnus, et les estime. L'approche bayésienne les traite comme des ",
         "<em>variables aléatoires</em>, dont on met à jour la distribution à mesure ",
         "qu'on observe des données.</p>",
         "<p>La <strong>loi <em>a priori</em></strong> — souvent appelée <em>prior</em> — ",
         "est ce que l'on croit des coefficients <em>avant</em> de regarder les données. ",
         "Elle n'est pas une opinion arbitraire : ici, elle traduit une régularité ",
         "économique bien établie.</p>",
         "<p>La <strong>loi <em>a posteriori</em></strong> est ce que l'on croit ",
         "<em>après</em> les avoir regardées. Elle combine le prior et les données, en ",
         "proportion de ce que chacun apporte.</p>",
         "<p>Une loi <em>a priori</em> est dite <strong>conjuguée</strong> lorsque la ",
         "loi <em>a posteriori</em> appartient à la même famille de distributions. C'est ",
         "une propriété très commode : elle donne le résultat par une formule au lieu ",
         "d'exiger des dizaines de milliers de simulations.</p>")))
ajouter(intuition(paste0(
  "<p>L'idée se résume à un arbitrage. Quand les données sont abondantes et claires, ",
  "elles l'emportent et le prior ne pèse presque rien. Quand elles sont rares ou ",
  "bruitées, le prior retient l'estimation et l'empêche de partir sur du bruit.</p>",
  "<p>C'est exactement notre situation : <strong>plusieurs centaines de coefficients ",
  "pour une centaine d'observations</strong>. Sans prior, l'estimation serait dominée ",
  "par le hasard de l'échantillon.</p>")))
ajouter("<h3>7.2 Le prior de Minnesota</h3>")
ajouter("<p>Le prior", ref("litter"),
        " encode une croyance économique simple et défendable : <strong>en ",
        "l'absence d'information, la meilleure prévision d'une série macroéconomique est ",
        "sa propre valeur retardée</strong>. Formellement, le prior centre chaque ",
        "équation sur une marche aléatoire — ou sur un bruit blanc pour des séries déjà ",
        "différenciées — et resserre d'autant plus fortement que le retard est ",
        "éloigné :</p>")
ajouter(eq(paste0("E[ ", m("(A<sub>i</sub>)<sub>jk</sub>"), " ] = 0 ,&nbsp;&nbsp; ",
                  "V[ ", m("(A<sub>i</sub>)<sub>jk</sub>"), " ] = ",
                  "<span class='fr'><span class='hi'>", m("&lambda;<sup>2</sup>"),
                  "</span><span class='lo'>", m("i<sup>d</sup>"), "</span></span>",
                  " <span class='op'>&times;</span> ",
                  "<span class='fr'><span class='hi'>",
                  m("&sigma;<sub>j</sub><sup>2</sup>"), "</span><span class='lo'>",
                  m("&sigma;<sub>k</sub><sup>2</sup>"), "</span></span>")))
ajouter("<p>Trois hyperparamètres gouvernent l'ensemble :</p>")
ajouter("<ul>",
        "<li>", m("&lambda;"), " — le <strong>resserrement global</strong>. Quand ",
        m("&lambda;"), " tend vers 0, le modèle se réduit au prior ; quand il tend vers ",
        "l'infini, aux moindres carrés ordinaires ;</li>",
        "<li>", m("d"), " — la <strong>décroissance par retard</strong>. Un retard ",
        "lointain est <em>a priori</em> moins informatif qu'un retard proche ;</li>",
        "<li>", m("p"), " — le <strong>nombre de retards</strong> retenus.</li>",
        "</ul>")
ajouter("<p>Le rapport ", m("&sigma;<sub>j</sub><sup>2</sup> / &sigma;<sub>k</sub><sup>2</sup>"),
        " n'est pas décoratif : il rend le prior <strong>invariant aux unités</strong>. ",
        "Sans lui, une branche mesurée en milliards et une autre en millions ne ",
        "recevraient pas le même traitement.</p>")

ajouter("<h3>7.3 La mise en œuvre par observations fictives</h3>")
ajouter("<p>Plutôt que de manipuler directement la loi <em>a priori</em>, on l'implémente ",
        "en ajoutant au système des <strong>observations artificielles</strong>",
        ref("banbura"), " qui portent exactement la croyance voulue. Le système augmenté s'écrit :</p>")
ajouter(eq(paste0(m("Y<sub>aug</sub>"), " = <span class='fr'><span class='hi'>",
                  m("Y"), "</span><span class='lo'>", m("Y<sub>d</sub>"),
                  "</span></span> ,&nbsp;&nbsp;&nbsp;&nbsp; ",
                  m("X<sub>aug</sub>"), " = <span class='fr'><span class='hi'>",
                  m("X"), "</span><span class='lo'>", m("X<sub>d</sub>"),
                  "</span></span>")))
ajouter("<p>et l'estimateur bayésien du mode <em>a posteriori</em> devient un simple ",
        "estimateur des moindres carrés sur ce système augmenté :</p>")
ajouter(eq(paste0(m("B&#770;"), " = ( ", m("X<sub>aug</sub>&prime;X<sub>aug</sub>"),
                  " )<sup>&minus;1</sup> ",
                  m("X<sub>aug</sub>&prime;Y<sub>aug</sub>"))))
ajouter(intuition(paste0(
  "<p>L'astuce est élégante : <strong>on ne change pas d'estimateur, on change de ",
  "données</strong>. Les observations fictives disent au modèle « voici ce que j'ai ",
  "déjà vu », et leur nombre relatif règle le poids de cette conviction face aux ",
  "données réelles.</p>",
  "<p>Le gain n'est pas seulement conceptuel. La formulation conjuguée qui en découle ",
  "donne la loi <em>a posteriori</em> en <strong>forme close</strong> : ni ",
  "échantillonneur de Gibbs, ni chaîne de Markov, ni diagnostic de convergence à ",
  "produire. La section 24 en tirera parti pour l'incertitude.</p>")))
ajouter("<p>La loi <em>a posteriori</em> est normale-inverse-Wishart :</p>")

ajouter(eq(paste0(m("&Sigma;"), " | ", m("Y"), " &#8764; ",
                  m("IW"), "( ", m("S"), " , ", m("&nu;"), " ) ,&nbsp;&nbsp; ",
                  "vec(", m("B"), ") | ", m("&Sigma;"), " , ", m("Y"), " &#8764; ",
                  m("N"), "( vec(", m("B&#770;"), ") , ", m("&Sigma;"),
                  " <span class='op'>&otimes;</span> ( ",
                  m("X<sub>aug</sub>&prime;X<sub>aug</sub>"), " )<sup>&minus;1</sup> )")))
ajouter(definition("Deux objets mathématiques de cette formule",
  paste0("<p>La <strong>loi de Wishart inverse</strong> est, pour une ",
         "<em>matrice</em> de covariance, l'équivalent de ce qu'est la loi du khi-deux ",
         "inverse pour une variance scalaire : c'est la distribution naturelle d'une ",
         "matrice de variances-covariances inconnue. Elle garantit que toute matrice ",
         "tirée est symétrique et définie positive — donc une matrice de covariance ",
         "valide, et non n'importe quel tableau de nombres.</p>",
         "<p>Le symbole ", m("&otimes;"), " est le <strong>produit de Kronecker</strong>. ",
         "Il exprime de façon compacte que <em>toutes</em> les équations du modèle ",
         "partagent la même structure de covariance : l'incertitude sur les coefficients ",
         "de la branche ", m("j"), " est liée à celle de la branche ", m("k"),
         " dans la proportion où leurs chocs le sont.</p>",
         "<p>La notation vec(", m("B"), ") empile simplement les colonnes de la matrice ",
         m("B"), " en un seul vecteur, ce qui permet d'écrire une loi normale ",
         "ordinaire là où il y aurait sinon une matrice.</p>")))
ajouter("<p>avec ", m("S"), " = ", m("Y<sub>aug</sub>&prime;Y<sub>aug</sub>"),
        " <span class='op'>&minus;</span> ", m("Y<sub>aug</sub>&prime;X<sub>aug</sub>B&#770;"),
        " et ", m("&nu;"), " le nombre d'observations augmentées diminué du nombre de ",
        "régresseurs.</p>")

# ============================================================================
# 8. HYPERPARAMETRES
# ============================================================================
ajouter("<h2 id='s8'><span class='num'>8.</span>Le choix des hyperparamètres</h2>")
ajouter("<p>Les trois hyperparamètres ne sont pas fixés une fois pour toutes : ils sont ",
        "<strong>rechoisis à chaque trimestre cible</strong>, sur la seule information ",
        "antérieure. Trois règles ont été comparées.</p>")
t8 <- hyper %>% dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Règle = specification,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Branches sous 1` = n_branches_ok,
                   `Corrélation médiane` = nb(correl_mediane, 3),
                   Retenue = ifelse(retenu, "oui", "—"))
ajouter(tbl(as.data.frame(t8), aligne_droite = 2:4))
ajouter(legende_tableau("Comparaison des règles de choix, sur l'ensemble du backtest."))
ajouter(figure("03_criteres_hyperparametres.png",
               "Les deux critères sur la grille d'hyperparamètres",
               paste0("Les surfaces n'ont pas leur minimum au même endroit : la ",
                      "vraisemblance marginale accepte un resserrement plus lâche que le ",
                      "critère d'ajustement. C'est la source de l'écart mesuré ",
                      "ci-dessus.")))

ajouter("<h3>8.1 Les deux règles en concurrence</h3>")
ajouter("<p>La <strong>vraisemblance marginale</strong>", ref("glp"),
        " est la quantité bayésienne orthodoxe : la probabilité des données sous le modèle, tous paramètres ",
        "intégrés.</p>")
ajouter(definition("Ce qu'est une vraisemblance marginale",
  paste0("<p>La <em>vraisemblance</em> ordinaire mesure la probabilité d'observer les ",
         "données pour des valeurs <strong>données</strong> des coefficients. Elle ",
         "augmente mécaniquement avec le nombre de paramètres : un modèle plus riche ",
         "ajuste toujours mieux, y compris le bruit.</p>",
         "<p>La vraisemblance <strong>marginale</strong> intègre sur toutes les valeurs ",
         "possibles des coefficients, pondérées par le prior. Elle répond donc à une ",
         "question différente : <em>quelle est la probabilité de ces données sous ce ",
         "modèle, tous paramètres confondus ?</em></p>",
         "<p>La différence est décisive : cette intégration <strong>pénalise ",
         "automatiquement la complexité</strong>. Un modèle qui n'ajuste bien que pour ",
         "un réglage très particulier de ses coefficients obtient une vraisemblance ",
         "marginale faible, parce que ce réglage occupe une portion négligeable de ",
         "l'espace des possibles. Aucune pénalité n'a à être ajoutée à la main.</p>")))
ajouter(eq(paste0(m("p"), "( ", m("Y"), " | ", m("&lambda;"), " , ", m("p"), " , ",
                  m("d"), " ) = <span class='big'>&int;</span> ", m("p"), "( ",
                  m("Y"), " | ", m("B"), " , ", m("&Sigma;"), " ) ", m("p"), "( ",
                  m("B"), " , ", m("&Sigma;"), " | ", m("&lambda;"), " , ", m("p"),
                  " , ", m("d"), " ) ", m("dB d&Sigma;"))))
ajouter("<p>Grâce à la conjugaison, elle s'écrit en forme close et ne demande aucune ",
        "simulation. Elle arbitre automatiquement entre ajustement et parcimonie.</p>")
ajouter("<p>Le <strong>critère d'ajustement</strong> procède autrement : il choisit le ",
        "resserrement qui amène l'erreur d'ajustement du modèle à une fraction fixée de ",
        "celle d'un modèle de référence univarié.</p>")
ajouter(eq(paste0("choisir ", m("&lambda;"), " tel que ",
                  "<span class='fr'><span class='hi'>",
                  "erreur du modèle contraint</span><span class='lo'>",
                  "erreur du modèle de référence</span></span>",
                  " = ", m("&tau;"))))
ajouter(sprintf(paste0("<p>C'est cette seconde règle qui est retenue, et l'écart n'est ",
                       "pas négligeable : <strong>%s contre %s</strong> de ratio médian. ",
                       "Les hyperparamètres fixés une fois pour toutes, comme le propose ",
                       "la littérature de référence, arrivent derrière les deux.</p>"),
                nb(min(hyper$ratio_median), 3),
                nb(hyper$ratio_median[grepl("vraisemblance", hyper$specification)], 3)))
ajouter(intuition(paste0(
  "<p>Ce résultat mérite d'être compris plutôt que constaté. La vraisemblance marginale ",
  "optimise l'<strong>ajustement dans l'échantillon</strong>, intégré sur la loi ",
  "<em>a priori</em>. Le critère d'ajustement, lui, vise explicitement un ",
  "<strong>niveau de contrainte</strong>, ce qui revient à imposer une régularisation ",
  "plus forte que celle que les données réclameraient.</p>",
  "<p>Avec une centaine d'observations pour plusieurs centaines de paramètres, cette ",
  "prudence supplémentaire paie hors échantillon. C'est un cas particulier d'un ",
  "principe général : <em>le critère optimal en échantillon n'est pas le critère ",
  "optimal en prévision</em>.</p>")))
med_p <- stats::median(hyp_orig$p[hyp_orig$specification == hyper$specification[hyper$retenu]])
med_l <- stats::median(hyp_orig$lambda[hyp_orig$specification == hyper$specification[hyper$retenu]])
ajouter(sprintf(paste0("<p>Les valeurs effectivement retenues au fil des origines ont ",
                       "pour médiane ", m("p"), " = %d et ", m("&lambda;"),
                       " = %s — un resserrement nettement plus fort que la valeur de ",
                       "0,15 usuellement proposée, et moins de retards.</p>"),
                med_p, nb(med_l, 3)))
ajouter(figure("03_hyperparametres_par_origine.png",
               "Les hyperparamètres retenus, origine par origine",
               paste0("Le resserrement n'est pas constant : il se durcit quand ",
                      "l'échantillon s'enrichit d'épisodes atypiques. C'est le ",
                      "comportement attendu d'une sélection récursive.")))
ajouter("<h3>8.2 Les autres leviers de spécification</h3>")
t8b <- leviers %>% dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Levier = specification, `Ratio médian` = nb(ratio_median, 3),
                   `Branches sous 1` = n_branches_ok,
                   `Corrélation médiane` = nb(correl_mediane, 3))
ajouter(tbl(as.data.frame(t8b), aligne_droite = 2:4))
ajouter(legende_tableau("Leviers de spécification évalués isolément."))
t8c <- specs3 %>% dplyr::arrange(ratio_median) %>%
  dplyr::transmute(`Spécification` = specification, `Fenêtre` = fenetre,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Corrélation médiane` = nb(correl_mediane, 3),
                   `Biais médian (pt)` = nb(100 * biais_median, 2))
ajouter(tbl(as.data.frame(t8c), aligne_droite = 3:5))
ajouter(legende_tableau("Spécifications comparées sur le protocole complet."))
t8d <- dm3 %>%
  dplyr::transmute(`Spécification testée` = specification, `Branches` = n_branches,
                   `Alternative meilleure` = alternative_meilleure,
                   `Référence meilleure` = reference_meilleure,
                   `Non significatif` = non_significatif)
ajouter(tbl(as.data.frame(t8d), aligne_droite = 2:5))
ajouter(legende_tableau(paste0(
  "Tests de précision comparée branche par branche, pour chaque spécification opposée ",
  "à la référence. La dernière colonne domine partout.")))
ajouter(figure("03_comparaison_specifications.png", "Les spécifications comparées",
               paste0("L'écart entre spécifications est du même ordre que celui entre ",
                      "règles de choix des hyperparamètres : aucun de ces réglages ne ",
                      "domine les autres de façon décisive.")))

# ============================================================================
# 9. RUPTURES
# ============================================================================
ajouter("<h2 id='s9'><span class='num'>9.</span>Ruptures structurelles et branches instables</h2>")
ajouter("<p>Deux anomalies distinctes menacent un modèle vectoriel estimé sur données ",
        "longues : les <strong>chocs ponctuels</strong>, et les <strong>ruptures de ",
        "niveau ou de variance</strong> dues à des changements de définition ",
        "statistique. Elles appellent des traitements différents.</p>")

ajouter("<h3>9.1 Les chocs ponctuels</h3>")
ajouter("<p>Un choc est détecté par une règle explicite, testable, appliquée ",
        "récursivement — et non par une liste de dates écrite à la main :</p>")
ajouter(eq(paste0(m("z<sub>j,t</sub>"), " = ",
                  "<span class='fr'><span class='hi'><span class='op'>|</span>",
                  m("g<sub>j,t</sub>"), " <span class='op'>&minus;</span> ",
                  "méd(", m("g<sub>j</sub>"), ")<span class='op'>|</span>",
                  "</span><span class='lo'>1,4826 <span class='op'>&times;</span> ",
                  "MAD(", m("g<sub>j</sub>"), ")</span></span>")))
ajouter("<p>Un trimestre est déclaré choc si ", m("z<sub>j,t</sub>"),
        " dépasse un seuil pour au moins ", m("k"), " branches simultanément. ",
        "Chaque trimestre ainsi identifié reçoit une variable indicatrice.</p>")
ajouter(definition("Pourquoi la médiane et l'écart absolu médian",
  paste0("<p>La moyenne et l'écart-type sont eux-mêmes contaminés par les valeurs ",
         "extrêmes qu'on cherche à détecter : un choc suffisamment violent gonfle ",
         "l'écart-type au point de ne plus paraître extrême. C'est le problème du ",
         "<em>point de rupture</em> de l'estimateur.</p>",
         "<p>La médiane et l'écart absolu médian ont un point de rupture", ref("hampel"),
         " de 50 % : ",
         "il faudrait que la moitié de l'échantillon soit aberrante pour les fausser. ",
         "Le facteur 1,4826 les rend comparables à un écart-type sous hypothèse ",
         "gaussienne, puisque MAD = ", m("&Phi;<sup>&minus;1</sup>"),
         "(0,75) ", m("&sigma;"), " &asymp; 0,6745 ", m("&sigma;"), ".</p>",
         "<p>La condition de <strong>simultanéité sur plusieurs branches</strong> ",
         "distingue un choc macroéconomique — qui touche l'économie entière — d'un ",
         "accident propre à une branche, lequel relève du bruit ordinaire et ne doit pas ",
         "recevoir d'indicatrice.</p>")))
t9 <- chocs %>% dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Règle = specification, `Ratio médian` = nb(ratio_median, 3),
                   `Branches sous 1` = n_branches_ok,
                   `Chocs détectés (médiane)` = chocs_median)
ajouter(tbl(as.data.frame(t9), aligne_droite = 2:4))
ajouter(legende_tableau("Règles de détection comparées sur le protocole complet."))
ajouter(sprintf(paste0("<p>L'absence totale d'indicatrices dégrade nettement le résultat ",
                       "— <strong>%s contre %s</strong> — et fait chuter le nombre de ",
                       "branches utilement prévues. La règle automatique fait mieux que ",
                       "la liste de dates codée en dur, tout en étant vérifiable.</p>"),
                nb(chocs$ratio_median[grepl("aucune", chocs$specification)], 3),
                nb(min(chocs$ratio_median), 3)))
ajouter(figure("03_chocs_detectes.png", "Les trimestres identifiés comme chocs",
               paste0("La règle retrouve 2020 sans qu'on le lui ait dit, mais aussi la ",
                      "crise de 2008 et des épisodes agricoles. Elle n'a donc pas été ",
                      "taillée sur mesure pour la pandémie.")))
ajouter(figure("03_regles_choc.png", "Sensibilité au réglage de la règle de détection",
               paste0("Le nombre de chocs détectés varie fortement avec le seuil, mais ",
                      "le classement des règles par qualité de prévision, lui, est ",
                      "stable.")))
ajouter(figure("03_coefficients_indicatrices.png",
               "Les coefficients des indicatrices de choc",
               paste0("Les deux trimestres de 2020 portent des coefficients de signes ",
                      "opposés et de grande amplitude : un effondrement puis un rebond. ",
                      "Une indicatrice unique pour l'année entière les aurait ",
                      "compensés.")))

ajouter("<h3>9.2 Les ruptures de variance</h3>")
ajouter("<p>Un second problème, plus insidieux : une branche dont la variabilité ",
        "s'effondre au fil du temps parce que sa <em>mesure</em> a changé, non son ",
        "activité. Le modèle y estime alors des dynamiques sur un artefact.</p>")
ajouter("<p>La règle est explicite :</p>")
ajouter(eq(paste0("branche instable &nbsp;&#8660;&nbsp; ",
                  "<span class='fr'><span class='hi'>Var( ", m("g<sub>j,t</sub>"),
                  " ) sur le premier tiers</span><span class='lo'>Var( ",
                  m("g<sub>j,t</sub>"), " ) sur le reste</span></span>",
                  " <span class='op'>&gt;</span> 10")))
t9b <- instab %>% dplyr::arrange(dplyr::desc(rapport_variance)) %>%
  head(5) %>%
  dplyr::transmute(Branche = branche,
                   `Rapport de variance` = nb(rapport_variance, 1),
                   `Instable` = ifelse(instable, "oui", "—"))
ajouter(tbl(as.data.frame(t9b), aligne_droite = 2))
ajouter(legende_tableau("Cinq premières branches par rapport de variance décroissant."))
ajouter(sprintf(paste0("<p>La séparation est franche : <strong>%s pour la branche ",
                       "déclenchante contre %s pour la suivante</strong>. Le seuil de 10 ",
                       "n'a donc pas à être finement calibré — n'importe quelle valeur ",
                       "entre 5 et 40 donnerait le même verdict.</p>"),
                nb(max(instab$rapport_variance), 1),
                nb(sort(instab$rapport_variance, decreasing = TRUE)[2], 1)))
ajouter("<p>Une branche déclarée instable est <strong>sortie du modèle vectoriel</strong> ",
        "et prévue par la moyenne de ses soixante derniers trimestres.</p>")
eff_ag <- effet[effet$niveau == "agregat", ]
ajouter(sprintf(paste0("<p>L'effet sur l'agrégat est mesuré : le ratio passe de %s à ",
                       "<strong>%s</strong>.</p>"),
                nb(eff_ag$ratio[grepl("sans", eff_ag$specification)], 3),
                nb(eff_ag$ratio[grepl("avec", eff_ag$specification)], 3)))
ajouter(intuition(paste0(
  "<p>Le résultat de fond est un peu ironique, et il vaut d'être énoncé : ",
  "<strong>le meilleur traitement d'une branche par un modèle sophistiqué peut ",
  "consister à ne pas la modéliser du tout</strong>.</p>",
  "<p>Quand la série porte davantage d'artefact statistique que de signal économique, ",
  "toute dynamique estimée dessus est une dynamique estimée sur du bruit de mesure. Une ",
  "moyenne récente, qui ne prétend rien, fait mieux.</p>")))
ajouter(figure("03e_series_instables.png", "La série qui déclenche la règle",
               paste0("La rupture de niveau est visible à l'œil nu et se situe à la ",
                      "charnière de deux bases comptables. Ce qui suit est une série ",
                      "presque plate : il n'y a pas de dynamique à estimer.")))
ajouter(figure("03e_diagnostic.png",
               "Le diagnostic d'instabilité sur les seize branches",
               paste0("Une branche se détache très nettement ; les quinze autres se ",
                      "tiennent dans une plage étroite. Le seuil n'a donc pas à être ",
                      "finement calibré.")))

# ============================================================================
# 10. DES MOIS AU TRIMESTRE
# ============================================================================
ajouter("<h2 id='s10'><span class='num'>10.</span>Des indicateurs mensuels au trimestre</h2>")
ajouter("<p>La cible est trimestrielle, la plupart des indicateurs sont mensuels. Le ",
        "passage de l'un à l'autre suit les règles économiques de la section 6 :</p>")
ajouter(eq(paste0(m("X<sub>k,T</sub>"), " = ",
                  "<span class='big'>&sum;</span><sub class='num'>m &isin; T</sub> ",
                  m("x<sub>k,m</sub>"), " &nbsp;&nbsp;(flux)&nbsp;&nbsp;&nbsp; ",
                  "<span class='fr'><span class='hi'>1</span><span class='lo'>3</span></span>",
                  "<span class='big'>&sum;</span><sub class='num'>m &isin; T</sub> ",
                  m("x<sub>k,m</sub>"), " &nbsp;&nbsp;(indice)&nbsp;&nbsp;&nbsp; ",
                  m("x<sub>k,m<sub>3</sub></sub>"), " &nbsp;&nbsp;(stock)")))
ajouter("<div class='encadre'><span class='etiq'>Un trimestre incomplet ne produit rien</span>",
        "<p>Si l'un des trois mois manque, l'agrégation renvoie une valeur manquante — ",
        "elle ne somme pas deux mois en faisant comme si de rien n'était.</p>",
        "<p>La règle paraît sévère ; elle est nécessaire. Sommer deux mois au lieu de ",
        "trois produit une valeur systématiquement <strong>plus basse d'un tiers</strong>, ",
        "que le modèle interpréterait comme un effondrement de l'activité. L'erreur ne ",
        "serait pas aléatoire mais orientée, donc bien pire qu'une donnée absente.</p>",
        "<p>Les mois réellement manquants du trimestre cible sont traités séparément, à ",
        "la section 13 : ils sont <em>prévus</em>, non ignorés.</p></div>")
ajouter(sprintf(paste0("<p>Le coût de cette règle est mesurable : sur l'ensemble des ",
                       "séries et des trimestres candidats, <strong>%s %% sont ",
                       "refusés</strong> pour incomplétude — dont %s %% faute de données ",
                       "en fin de série et %s %% par trou interne.</p>"),
                nb(100 * sum(pertes4$refuses) / sum(pertes4$trimestres), 1),
                nb(100 * sum(pertes4$bord) / max(1, sum(pertes4$refuses)), 0),
                nb(100 * sum(pertes4$interne) / max(1, sum(pertes4$refuses)), 0)))

# ============================================================================
# 11. SELECTION RECURSIVE
# ============================================================================
ajouter("<h2 id='s11'><span class='num'>11.</span>La sélection récursive des indicateurs</h2>")
ajouter("<p>Le vivier compte plusieurs dizaines de séries pour certaines branches. Les ",
        "utiliser toutes serait absurde : une régression sur quarante prédicteurs et ",
        "quarante trimestres n'a aucun degré de liberté. Il faut sélectionner, et ",
        "<strong>sélectionner à chaque origine</strong>.</p>")
ajouter("<p>La procédure est gloutonne et consciente de la disponibilité :</p>")
ajouter("<ol>",
        "<li>écarter toute série <strong>non observée au trimestre cible</strong> — elle ",
        "ne pourrait de toute façon pas servir ;</li>",
        "<li>classer les candidates restantes par corrélation décroissante avec la ",
        "croissance de la branche, sur les seules données antérieures ;</li>",
        "<li>ajouter les séries une à une, <strong>en n'acceptant une série que si elle ",
        "laisse assez de lignes d'entraînement conjointes</strong> ;</li>",
        "<li>s'arrêter au plus tard à cinq régresseurs.</li>",
        "</ol>")
ajouter(intuition(paste0(
  "<p>Le point 3 est celui qui fait la différence, et il vient d'un échec. Une première ",
  "version classait les séries par corrélation seule, puis estimait par suppression des ",
  "lignes incomplètes. Résultat : <strong>une seule série trouée annulait toute ",
  "l'équation</strong>, et près d'un tiers des couples branche-trimestre ne produisaient ",
  "aucune prévision.</p>",
  "<p>La version retenue préfère une série un peu moins corrélée mais observée ",
  "conjointement aux autres. C'est un arbitrage entre <em>qualité du signal</em> et ",
  "<em>quantité d'observations utilisables</em>, et il se tranche en faveur de la ",
  "seconde plus souvent qu'on ne l'imagine.</p>")))
n_sel <- selpo %>% dplyr::filter(retenu) %>% dplyr::count(branche, origine)
ajouter(sprintf(paste0("<p>En moyenne, <strong>%s indicateurs</strong> sont retenus par ",
                       "branche et par trimestre. La liste change d'une origine à ",
                       "l'autre : on dénombre en médiane <strong>%d listes ",
                       "distinctes</strong> par branche sur l'ensemble des origines.</p>"),
                nb(mean(n_sel$n), 1),
                as.integer(stats::median((selpo %>% dplyr::filter(retenu) %>%
                  dplyr::group_by(branche, origine) %>%
                  dplyr::summarise(l = paste(sort(id_serie), collapse = "|"),
                                   .groups = "drop") %>%
                  dplyr::group_by(branche) %>%
                  dplyr::summarise(n = dplyr::n_distinct(l), .groups = "drop"))$n))))
ajouter(figure("04_nombre_indicateurs.png", "Nombre d'indicateurs retenus par origine",
               paste0("Le nombre croît avec l'historique disponible puis se stabilise ",
                      "sous le plafond. Les creux correspondent aux trimestres où peu de ",
                      "séries sont conjointement observées.")))

# ============================================================================
# 12. LES PASSERELLES
# ============================================================================
ajouter("<h2 id='s12'><span class='num'>12.</span>Les équations passerelles</h2>")
ajouter("<p>Une équation passerelle relie la croissance d'une branche aux indicateurs ",
        "retenus pour elle, agrégés au trimestre :</p>")
ajouter(eq(paste0(m("g<sub>j,t</sub>"), " = ", m("&alpha;<sub>j</sub>"),
                  " <span class='op'>+</span> ",
                  "<span class='big'>&sum;</span><sub class='num'>k &isin; S<sub>j,T</sub></sub> ",
                  m("&beta;<sub>j,k</sub> X<sub>k,t</sub>"),
                  " <span class='op'>+</span> ", m("&gamma;<sub>j</sub> g<sub>j,t&minus;1</sub>"),
                  " <span class='op'>+</span> ", m("&epsilon;<sub>j,t</sub>"))))
ajouter("<p>où ", m("S<sub>j,T</sub>"), " est la liste sélectionnée à l'origine ",
        m("T"), ". L'estimation se fait par moindres carrés ordinaires sur les ",
        "trimestres <strong>strictement antérieurs</strong> à ", m("T"),
        ", puis la prévision s'obtient en évaluant l'équation aux valeurs du trimestre ",
        "cible :</p>")
ajouter(eq(paste0(m("g&#770;<sub>j,T</sub><sup>pass</sup>"), " = ",
                  m("&alpha;&#770;<sub>j</sub>"), " <span class='op'>+</span> ",
                  m("&beta;&#770;<sub>j</sub>&prime;X<sub>T</sub>"),
                  " <span class='op'>+</span> ",
                  m("&gamma;&#770;<sub>j</sub> g<sub>j,T&minus;1</sub>"))))
ajouter("<div class='encadre alerte'><span class='etiq'>Le terme autorégressif, et un piège qu'il recèle</span>",
        "<p>Le terme ", m("g<sub>j,T&minus;1</sub>"), " est la croissance du trimestre ",
        "<em>précédent</em>. Il est légitime — il appartient à l'ensemble d'information ",
        "— et il apporte beaucoup, puisque la passerelle ignorerait sans lui toute la ",
        "persistance que le modèle vectoriel exploite.</p>",
        "<p>Mais sa mise en œuvre contient un piège dans lequel il est facile de tomber : ",
        "construire la ligne de prévision en la <em>joignant</em> à la table de la valeur ",
        "ajoutée revient à exiger que ", m("g<sub>j,T</sub>"),
        " — la grandeur cherchée — existe déjà. L'équation ne produit alors de prévision ",
        "que pour des trimestres déjà publiés, ce qui est vide de sens en temps réel.</p>",
        "<p>La ligne cible doit être <strong>construite</strong> à partir des indicateurs ",
        "et du seul retard de la cible, jamais jointe. C'est une exigence de forme, mais ",
        "elle décide de la validité de tout l'étage.</p></div>")
ajouter(figure("04_prevu_realise.png", "Prévu contre réalisé, par branche",
               paste0("Le nuage se resserre autour de la première bissectrice pour les ",
                      "branches bien couvertes. Les points extrêmes correspondent tous à ",
                      "2020 : c'est là que la passerelle se distingue.")))
ajouter(figure("04_qualite_par_modele.png", "Qualité comparée des trois prévisions de branche",
               paste0("Modèle vectoriel, passerelle et combinaison, branche par branche. ",
                      "La combinaison est rarement la meilleure sur une branche donnée, ",
                      "mais elle est rarement la pire — c'est ce qui la rend bonne en ",
                      "moyenne.")))

# ============================================================================
# 13. LE BORD IRREGULIER
# ============================================================================
ajouter("<h2 id='s13'><span class='num'>13.</span>Le bord irrégulier de l'information</h2>")
ajouter("<p>Les indicateurs ne sont pas tous observés jusqu'au même mois : chacun a sa ",
        "propre profondeur. La matrice d'information présente donc un ",
        "<strong>bord irrégulier</strong>", refs("wallis", "doz"),
        " : plutôt qu'une frontière droite, un escalier.</p>")
ajouter(figure("02_ragged_edge.png", "Le bord irrégulier",
               paste0("Chaque ligne est un indicateur, chaque colonne un mois. La ",
                      "frontière de droite n'est pas verticale : certaines séries ",
                      "s'arrêtent deux ou trois mois plus tôt que d'autres.")))
ajouter("<p>Deux traitements distincts, qu'il ne faut surtout pas confondre.</p>")

ajouter("<h3>13.1 Les trous internes : lissage</h3>")
ajouter("<p>Un mois manquant <em>à l'intérieur</em> d'une série est reconstruit par ",
        "lissage de Kalman sur un modèle structurel à composantes inobservées",
        ref("harvey_b"), " :</p>")
ajouter(eq(paste0(m("y<sub>m</sub>"), " = ", m("&mu;<sub>m</sub>"),
                  " <span class='op'>+</span> ", m("&gamma;<sub>m</sub>"),
                  " <span class='op'>+</span> ", m("&epsilon;<sub>m</sub>"),
                  " ,&nbsp;&nbsp; ", m("&mu;<sub>m</sub>"), " = ",
                  m("&mu;<sub>m&minus;1</sub>"), " <span class='op'>+</span> ",
                  m("&beta;<sub>m&minus;1</sub>"), " <span class='op'>+</span> ",
                  m("&eta;<sub>m</sub>"))))
ajouter("<p>où ", m("&mu;"), " est le niveau, ", m("&beta;"), " la pente et ",
        m("&gamma;"), " la composante saisonnière. Le comblement est limité aux trous ",
        "de trois mois au plus.</p>")
ajouter(figure("04c_mois_combles.png", "Les mois effectivement comblés",
               paste0("Le comblement reste marginal en volume : il ne concerne qu'une ",
                      "fraction des séries, et rarement plus d'un ou deux mois ",
                      "consécutifs.")))
ajouter("<p>La qualité du comblement a été vérifiée par une expérience contrôlée : des ",
        "trous <strong>artificiels</strong> sont percés dans des séries complètes, puis ",
        "comblés, et la valeur reconstruite est comparée à la valeur réellement ",
        "observée.</p>")
t13 <- kalval %>%
  dplyr::transmute(Méthode = methode, `n` = n,
                   `Erreur / écart-type` = nb(`erreur / ecart-type`, 3),
                   `Erreur absolue médiane` = nb(`erreur abs. mediane`, 3),
                   `Biais relatif` = nb(`biais relatif`, 3))
ajouter(tbl(as.data.frame(t13), aligne_droite = 2:5))
ajouter(legende_tableau("Validation du comblement sur trous artificiels."))
ajouter(sprintf(paste0("<p>En pratique, le comblement concerne <strong>%s mois en ",
                       "moyenne par origine</strong>, avec un maximum de %s. Il reste ",
                       "donc une opération marginale en volume.</p>"),
                nb(mean(comb_bil$n_comble), 1), nb(max(comb_bil$n_comble))))
ajouter(intuition(paste0(
  "<p>Le lissage structurel bat l'interpolation linéaire et la moyenne saisonnière sur ",
  "les trous isolés. Cela ne signifie pourtant <strong>pas</strong> qu'il améliore la ",
  "prévision finale — la section 27 montre que ce n'est pas le cas.</p>",
  "<p>La nuance est instructive : <em>mieux reconstituer une donnée manquante ",
  "n'améliore pas nécessairement ce qu'on en fait ensuite</em>. Le gain se dilue dans ",
  "l'agrégation trimestrielle, puis dans la sélection, puis dans la combinaison.</p>")))

ajouter("<h3>13.2 Le bord : filtrage</h3>")
ajouter("<p>Les mois manquants <em>en fin</em> de série sont, eux, ",
        "<strong>extrapolés par le filtre</strong>, jamais par le lisseur.</p>")
ajouter(definition("Pourquoi cette distinction est impérative",
  paste0("<p>Le lisseur de Kalman calcule l'espérance de l'état ",
         "<em>conditionnellement à l'échantillon entier</em> : ", m("E[&alpha;<sub>m</sub> | y<sub>1</sub>, &hellip;, y<sub>M</sub>]"),
         ". Il utilise donc le futur pour reconstruire le passé — ce qui est ",
         "exactement ce qu'on veut pour un trou interne, et exactement ce qu'on ne ",
         "veut pas au bord.</p>",
         "<p>Le filtre calcule ", m("E[&alpha;<sub>m</sub> | y<sub>1</sub>, &hellip;, y<sub>m</sub>]"),
         " : il n'utilise que le passé. C'est la seule opération légitime à ",
         "l'extrémité de la série.</p>",
         "<p>Employer le lisseur au bord introduirait une antériorité d'autant plus ",
         "difficile à détecter qu'elle serait <em>numériquement invisible</em> : les ",
         "valeurs produites paraîtraient parfaitement plausibles.</p>")))
ajouter("<p>Une contrainte de positivité complète le dispositif : une série de niveaux ",
        "positifs ne peut pas être extrapolée en territoire négatif, et un plancher ",
        "strictement positif évite qu'une valeur nulle ne se propage ensuite dans un ",
        "logarithme.</p>")

# ============================================================================
# 14. LA COMBINAISON
# ============================================================================
ajouter("<h2 id='s14'><span class='num'>14.</span>La combinaison des deux prévisions</h2>")
ajouter("<p>Chaque branche couverte dispose désormais de deux prévisions : celle du ",
        "modèle vectoriel et celle de la passerelle. On les combine linéairement :</p>")
ajouter(eq(paste0(m("g&#770;<sub>j,T</sub>"), " = ", m("&delta;"), " ",
                  m("g&#770;<sub>j,T</sub><sup>vect</sup>"),
                  " <span class='op'>+</span> ( 1 <span class='op'>&minus;</span> ",
                  m("&delta;"), " ) ", m("g&#770;<sub>j,T</sub><sup>pass</sup>"),
                  " ,&nbsp;&nbsp;&nbsp; ", m("&delta;"), " = 0,5")))
ajouter("<p>Le poids est <strong>constant</strong>, et ce choix a été le plus discuté du ",
        "travail. Trois mesures le fondent, à prendre dans l'ordre.</p>")

ajouter("<h3>14.1 Ce que dit la courbe de perte</h3>")
t14 <- courbe_d %>%
  dplyr::filter(abs(delta * 10 - round(delta * 10)) < 1e-9) %>%
  dplyr::select(delta, periode, ratio) %>%
  tidyr::pivot_wider(names_from = periode, values_from = ratio) %>%
  dplyr::arrange(delta) %>%
  dplyr::transmute(`δ` = nb(delta, 2), `Ensemble` = nb(`toutes origines`, 3),
                   `2020` = nb(`2020`, 3), `Hors 2020` = nb(`hors 2020`, 3))
ajouter(tbl(as.data.frame(t14), aligne_droite = 1:4))
ajouter(legende_tableau(paste0(
  "Ratio de l'agrégat pour chaque valeur du poids, δ = 0 donnant la passerelle seule ",
  "et δ = 1 le modèle vectoriel seul.")))
ajouter(figure("14b_courbe_delta.png", "La perte selon le poids de combinaison",
               paste0("Les points marquent le minimum de chaque période, la verticale le ",
                      "réglage retenu. Hors 2020 la courbe a un creux intérieur ; en ",
                      "2020 elle décroît jusqu'au bout.")))
opt_h <- courbe_d %>% dplyr::filter(periode == "hors 2020") %>%
  dplyr::slice_min(ratio, n = 1, with_ties = FALSE)
plat <- courbe_d %>% dplyr::filter(periode == "hors 2020") %>%
  dplyr::mutate(ec = ratio - min(ratio)) %>% dplyr::filter(ec <= 0.01)
ajouter(sprintf(paste0("<p>Deux propriétés s'y lisent. <strong>Hors 2020, la courbe a un ",
                       "minimum intérieur</strong>, à δ = %s : c'est ce qui valide la ",
                       "combinaison elle-même, puisqu'en régime normal le mélange bat ",
                       "chacune de ses deux composantes. Et elle est ",
                       "<strong>plate</strong> — tout δ de %s à %s se tient à moins de ",
                       "0,01 point du minimum.</p>"),
                nb(opt_h$delta, 2), nb(min(plat$delta), 2), nb(max(plat$delta), 2)))
ajouter(intuition(paste0(
  "<p>C'est la propriété classique d'une perte quadratique : <strong>s'écarter de ",
  "l'optimum coûte au second ordre, alors que l'estimer coûte au premier</strong>. Quand ",
  "la courbe est plate, l'erreur d'estimation du poids pèse plus lourd que l'écart au ",
  "poids idéal.</p>",
  "<p>Et surtout, <strong>la position de l'optimum dépend du régime</strong> : 0 en ",
  "2020, ", nb(opt_h$delta, 1), " hors 2020. Or au moment de publier, on ignore dans ",
  "quel régime on se trouve. C'est exactement la configuration où l'on fixe un paramètre ",
  "au lieu de l'estimer.</p>")))

ajouter("<h3>14.2 Ce que fait l'estimateur</h3>")
ajouter("<p>Le poids optimal au sens des moindres carrés admet une solution fermée. En ",
        "notant ", m("e<sub>1</sub>"), " et ", m("e<sub>2</sub>"),
        " les erreurs passées des deux composantes :</p>")
ajouter(eq(paste0(m("&delta;*"), " = ",
                  "<span class='fr'><span class='hi'>",
                  "<span class='big'>&sum;</span> ", m("e<sub>2,t</sub>"),
                  "( ", m("e<sub>2,t</sub>"), " <span class='op'>&minus;</span> ",
                  m("e<sub>1,t</sub>"), " )</span><span class='lo'>",
                  "<span class='big'>&sum;</span> ( ", m("e<sub>2,t</sub>"),
                  " <span class='op'>&minus;</span> ", m("e<sub>1,t</sub>"),
                  " )<sup>2</sup></span></span>")))
ajouter(sprintf(paste0("<p>Cet estimateur n'est <em>pas</em> instable, contrairement à ce ",
                       "qu'on pourrait supposer : son saut médian d'un trimestre au ",
                       "suivant est <strong>nul</strong>, son écart-type intra-branche ",
                       "vaut %s. Il retombe en revanche sur sa valeur par défaut dans ",
                       "<strong>%.0f %% des cas</strong>, faute d'un historique apparié ",
                       "suffisant.</p>"),
                nb(vs("ecart-type intra-branche moyen"), 2),
                100 * vs("part des couples au mode par defaut")))

ajouter("<h3>14.3 La comparaison décisive</h3>")
ajouter("<p>Elle consiste à opposer le poids estimé non pas à 0,5, mais à ",
        "<strong>une constante placée à son propre niveau moyen</strong> — ce qui isole ",
        "l'apport de sa <em>variation</em>, indépendamment de son niveau.</p>")
t14b <- comp_d %>% dplyr::transmute(Règle = regle, `Ratio` = nb(ratio, 3),
                                    `Corrélation` = nb(correlation, 3))
ajouter(tbl(as.data.frame(t14b), aligne_droite = 2:3))
ajouter(legende_tableau("Le poids estimé, opposé à une constante de même niveau moyen."))
ajouter(figure("14_delta.png", "Le ratio par branche selon la règle de pondération",
               paste0("À l'échelle des branches, toutes les règles se tiennent dans un ",
                      "mouchoir de poche. C'est ce qui a rendu nécessaire la ",
                      "démonstration au niveau de l'agrégat.")))
ajouter(sprintf(paste0("<p>À niveau moyen égal, <strong>la constante fait mieux de %s ",
                       "point de ratio</strong> et de %s de corrélation. La variation que ",
                       "l'estimateur introduit d'une branche et d'un trimestre à l'autre ",
                       "<strong>n'apporte donc aucune information</strong> : elle ",
                       "n'ajoute que du bruit.</p>"),
                nb(comp_d$ratio[1] - comp_d$ratio[2], 3),
                nb(comp_d$correlation[2] - comp_d$correlation[1], 2)))
ajouter(definition("Le paradoxe de la combinaison de prévisions",
  paste0("<p>Ce résultat n'a rien de propre au Maroc : c'est l'un des constats les mieux ",
         "établis de la littérature. Le principe de la combinaison et son poids optimal ",
         "remontent à 1969", ref("bates"), " ; le constat que <em>la moyenne simple bat ",
         "les poids estimés</em> a été vérifié sur des centaines de séries ",
         "macroéconomiques", ref("stock"), " ; son explication tient à ce qu'estimer un ",
         "poids ajoute une variance d'échantillonnage qui excède le biais qu'on ",
         "corrige", refs("smith", "claes"), ". Le même phénomène est connu en gestion de ",
         "portefeuille, où la répartition uniforme résiste à des allocations ",
         "optimisées", ref("demiguel"), ".</p>",
         "<p>Le poids théoriquement optimal vaut</p>",
         eq(paste0(m("&delta;<sup>opt</sup>"), " = ",
                   "<span class='fr'><span class='hi'>",
                   m("&sigma;<sub>2</sub><sup>2</sup>"),
                   " <span class='op'>&minus;</span> ", m("&sigma;<sub>12</sub>"),
                   "</span><span class='lo'>", m("&sigma;<sub>1</sub><sup>2</sup>"),
                   " <span class='op'>+</span> ", m("&sigma;<sub>2</sub><sup>2</sup>"),
                   " <span class='op'>&minus;</span> 2", m("&sigma;<sub>12</sub>"),
                   "</span></span>")),
         "<p>et <strong>les poids égaux sont exactement optimaux quand les deux ",
         "composantes ont la même variance d'erreur</strong>. La valeur 0,5 est donc le ",
         "cas de référence : celle qu'on adopte en refusant de prétendre savoir laquelle ",
         "des deux prévisions est la meilleure — et la seule qui se justifie ",
         "<em>a priori</em>, sans regarder les données.</p>")))
c05 <- courbe_d %>% dplyr::filter(periode == "hors 2020", delta == 0.5)
ajouter("<div class='encadre alerte'><span class='etiq'>Une réserve à ne pas dissimuler</span>",
        "<p>Sur cet échantillon, <strong>δ ≈ 0,2 à 0,3 domine 0,5 dans les trois ",
        "périodes</strong>. Retenir 0,3 parce que la courbe le dit reviendrait toutefois ",
        "à ajuster un paramètre sur un échantillon dont quatre trimestres décident de ",
        "presque tout — et la position de l'optimum change selon qu'on inclut 2020 ou ",
        "non.</p>",
        sprintf(paste0("<p>Le coût de ce refus est mesuré : <strong>%s point de ratio ",
                       "hors 2020</strong>. C'est le prix d'un réglage qui ne doit rien ",
                       "à l'échantillon.</p></div>"),
                nb(c05$ratio - opt_h$ratio, 3)))

ajouter("<h3>14.4 Une garde sur l'amplitude</h3>")
ajouter("<p>Une prévision de branche s'écartant de plus de cinq écarts-types de son ",
        "historique est signalée :</p>")
ajouter(eq(paste0(m("z<sub>j,T</sub>"), " = ",
                  "<span class='op'>|</span>", m("g&#770;<sub>j,T</sub>"),
                  "<span class='op'>|</span> / ", m("&sigma;&#770;<sub>j,t&lt;T</sub>"),
                  " <span class='op'>&gt;</span> 5")))
ajouter("<p>L'écart-type est estimé sur la seule information antérieure. La garde ne ",
        "corrige pas automatiquement : elle documente. Une branche très volatile peut ",
        "légitimement afficher une forte amplitude sans être aberrante.</p>")

# ============================================================================
# 15. BRANCHES NON COUVERTES
# ============================================================================
ajouter("<h2 id='s15'><span class='num'>15.</span>Les branches sans indicateur</h2>")
non_couv <- couv$non_couvertes
ajouter(sprintf(paste0("<p>Quatre branches ne disposent d'aucun indicateur ",
                       "infra-trimestriel : %s. Elles pèsent ensemble %.0f %% de la ",
                       "valeur ajoutée, ce qui interdit de les négliger.</p>"),
                paste(non_couv, collapse = ", "),
                100 * sum(statsva$part_moyenne[statsva$branche %in% non_couv])))
ajouter("<p>Elles reçoivent la prévision du <strong>vecteur autorégressif ",
        "bayésien</strong>, comme les douze autres : faute de passerelle, la ",
        "combinaison se réduit à sa composante vectorielle. Il n'y a pas de ",
        "quatrième étage qui leur serait propre.</p>")
ajouter("<p>Ce n'est pas un défaut de conception mais un résultat. Huit modèles ont ",
        "été mis en concurrence sur ces quatre branches, sur l'ensemble du protocole. ",
        "Parmi eux, l'autorégression dont l'ordre est choisi à chaque origine par ",
        "critère d'information bayésien :</p>")
ajouter(eq(paste0(m("g<sub>j,t</sub>"), " = ", m("c<sub>j</sub>"),
                  " <span class='op'>+</span> ",
                  "<span class='big'>&sum;</span><sub class='num'>i=1</sub><sup class='num'>p*</sup> ",
                  m("&phi;<sub>j,i</sub> g<sub>j,t&minus;i</sub>"),
                  " <span class='op'>+</span> ", m("&epsilon;<sub>j,t</sub>"),
                  " ,&nbsp;&nbsp; ", m("p*"), " = arg min BIC")))
ajouter(definition("Ce qu'est un critère d'information",
  paste0("<p>Un critère d'information", ref("schwarz"),
         " arbitre entre <strong>ajustement</strong> et ",
         "<strong>parcimonie</strong>. Le premier terme récompense un modèle qui colle ",
         "aux données, le second pénalise chaque paramètre supplémentaire.</p>",
         "<p>Sans cette pénalité, on choisirait toujours le modèle le plus riche : ",
         "ajouter un retard ne peut pas dégrader l'ajustement <em>dans</em> ",
         "l'échantillon. Mais ce gain apparent est du <strong>surajustement</strong> — ",
         "le modèle apprend le bruit de son échantillon, qui ne se reproduira pas, et ",
         "prévoit moins bien.</p>")))
ajouter("<p>avec BIC = ", m("&minus;2 log L"), " <span class='op'>+</span> ",
        m("k log n"), ". La pénalité en ", m("log n"),
        " est plus sévère que celle du critère d'Akaike, ce qui convient à un ",
        "échantillon court : on préfère un modèle trop simple à un modèle surajusté.</p>")
ajouter("<p>Le critère d'Akaike pénalise de ", m("2k"), ", le critère bayésien de ",
        m("k log n"), ". Dès que ", m("n"), " dépasse 8, ", m("log n"),
        " excède 2 : le second est donc plus exigeant, et l'écart se creuse avec la ",
        "taille de l'échantillon.</p>")
t15 <- perio5 %>% dplyr::filter(periode == "toutes origines") %>%
  dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Modèle = modele, `Ratio médian` = nb(ratio_median, 3),
                   `Branches sous 1` = n_ok, `Corrélation` = nb(correl, 3))
ajouter(tbl(as.data.frame(t15), aligne_droite = 2:4))
ajouter(legende_tableau(paste0("Les huit variantes mises en concurrence sur les ",
                              "quatre branches non couvertes. Aucune n'est ",
                              "employée pour produire le chiffre : le tableau ",
                              "sert à juger si l'une d'elles mériterait de ",
                              "remplacer le vecteur autorégressif.")))
ajouter(figure("05_comparaison_modeles.png", "Les variantes autorégressives comparées",
               paste0("L'ajout d'une composante saisonnière dégrade franchement : ",
                      "confirmation indépendante que les séries sont déjà ",
                      "désaisonnalisées.")))
ajouter(figure("05_ordre_bic.png", "L'ordre retenu par critère d'information",
               paste0("L'ordre bouge d'une origine à l'autre et reste bas. Le fixer à 4 ",
                      "aurait imposé des paramètres que les données ne réclament pas.")))
ajouter(figure("05_previsions_par_branche.png",
               "Les quatre branches non couvertes, variantes comparées et réalisé",
               paste0("Les trajectoires prévues sont presque plates : en l'absence ",
                      "d'indicateur, le modèle ne peut que reconduire une moyenne ",
                      "ajustée.")))
t15b <- meil5 %>% dplyr::arrange(ratio) %>%
  dplyr::transmute(Branche = branche, `Meilleur modèle` = modele,
                   `Ratio` = nb(ratio, 3))
ajouter(tbl(as.data.frame(t15b), aligne_droite = 3))
ajouter(legende_tableau(paste0("Le meilleur modèle <em>a posteriori</em> pour ",
                              "chacune des quatre branches — et non le modèle ",
                              "employé, qui est le vecteur autorégressif dans ",
                              "les quatre cas.")))
ajouter(sprintf(paste0("<p>L'ordre médian que le critère bayésien retiendrait est ",
                       "<strong>%d</strong>. Ni l'ajout d'indicatrices de choc ni celui ",
                       "d'une composante saisonnière n'améliore le résultat — la seconde ",
                       "le dégrade même franchement, ce qui confirme que les séries sont ",
                       "déjà désaisonnalisées.</p>"),
                as.integer(stats::median(ordre5$p_bic))))
ajouter("<p>Le verdict est net et va dans un seul sens : <strong>aucune des sept ",
        "alternatives ne justifie de déloger le vecteur autorégressif</strong>. Sur ",
        "les services aux entreprises, c'est lui qui l'emporte directement. Ailleurs, ",
        "les vainqueurs diffèrent d'une branche à l'autre — moyenne récursive, marche ",
        "aléatoire, modèle saisonnier — et aucun ne domine l'ensemble. Choisir un ",
        "modèle par branche sur la foi d'un classement établi sur quarante-huit points ",
        "reviendrait à sélectionner sur l'échantillon d'évaluation lui-même, ce que le ",
        "reste du protocole s'interdit partout ailleurs.</p>")
ajouter(intuition(paste0(
  "<p>Il faut être clair sur ce que vaut le chiffre de ces quatre branches : ",
  "<strong>elles ne sont pas réellement prévues</strong>. Le vecteur autorégressif les ",
  "projette à partir de leur propre passé et de celui des quinze autres, mais ",
  "<em>aucune information infra-trimestrielle nouvelle n'y entre entre deux ",
  "trimestres</em>. Le scénario M0 ou M3 ne change rien pour elles.</p>",
  "<p>C'est une limite assumée, et elle est structurelle : l'administration publique, ",
  "l'éducation et la santé, les services aux entreprises ne font tout simplement pas ",
  "l'objet d'une mesure infra-trimestrielle. Aucun raffinement de modèle ne créera une ",
  "information qui n'est pas collectée. Elles pèsent ensemble un quart de la valeur ",
  "ajoutée : c'est la principale marge de progrès du système, et elle se trouve du ",
  "côté de la collecte, non du côté de l'estimation.</p>")))

# ============================================================================
# 16. AGREGATION
# ============================================================================
ajouter("<h2 id='s16'><span class='num'>16.</span>L'agrégation en valeur ajoutée totale</h2>")
ajouter("<p>Seize prévisions de branche doivent devenir un chiffre unique. L'opération ",
        "n'est pas une moyenne : c'est un <strong>indice de volume de Laspeyres</strong>.</p>")
ajouter(eq(paste0(m("g&#770;<sub>T</sub>"), " = log <span class='big'>&sum;</span>",
                  "<sub class='num'>j=1</sub><sup class='num'>16</sup> ",
                  m("w<sub>j,T&minus;1</sub>"), " ",
                  m("e"), "<sup>", m("g&#770;<sub>j,T</sub>"), "</sup>")))
ajouter("<p>où les poids sont les parts de chaque branche dans la valeur ajoutée ",
        "<strong>en prix courants</strong> du trimestre précédent :</p>")
ajouter(eq(paste0(m("w<sub>j,T&minus;1</sub>"), " = ",
                  "<span class='fr'><span class='hi'>",
                  m("V<sub>j,T&minus;1</sub><sup>nominal</sup>"),
                  "</span><span class='lo'><span class='big'>&sum;</span>",
                  "<sub class='num'>i</sub> ",
                  m("V<sub>i,T&minus;1</sub><sup>nominal</sup>"),
                  "</span></span> ,&nbsp;&nbsp; ",
                  "<span class='big'>&sum;</span><sub class='num'>j</sub> ",
                  m("w<sub>j,T&minus;1</sub>"), " = 1")))

ajouter("<h3>16.1 Pourquoi des poids nominaux sur des prévisions en volume</h3>")
ajouter(definition("La logique d'un indice de Laspeyres",
  paste0("<p>La question paraît incohérente et ne l'est pas. Un indice de volume de ",
         "Laspeyres valorise les <em>quantités</em> de la période courante aux ",
         "<em>prix</em> de la période de base :</p>",
         eq(paste0(m("L<sub>T</sub>"), " = ",
                   "<span class='fr'><span class='hi'><span class='big'>&sum;</span>",
                   "<sub class='num'>j</sub> ", m("p<sub>j,T&minus;1</sub> q<sub>j,T</sub>"),
                   "</span><span class='lo'><span class='big'>&sum;</span>",
                   "<sub class='num'>j</sub> ", m("p<sub>j,T&minus;1</sub> q<sub>j,T&minus;1</sub>"),
                   "</span></span>")),
         "<p>Le numérateur et le dénominateur partagent les mêmes prix ",
         m("p<sub>j,T&minus;1</sub>"), " : le rapport ne mesure donc que l'évolution ",
         "des quantités. Et la part ", m("w<sub>j,T&minus;1</sub>"),
         " qui pondère la croissance en volume de la branche ", m("j"),
         " est bien sa part en <strong>valeur</strong> à la période de base.</p>",
         "<p>Employer des poids en volume reviendrait à valoriser aux prix d'une année ",
         "de référence de plus en plus lointaine — ce que les comptes nationaux ",
         "évitent précisément par le chaînage.</p>",
         "<p>Une conséquence en découle, qu'il faut avoir en tête : les volumes chaînés ",
         "<strong>ne sont pas additifs</strong>. La somme des valeurs ajoutées de ",
         "branche en volume ne redonne pas exactement le total, et l'écart croît avec ",
         "l'éloignement de l'année de référence.</p>")))
ajouter("<p>Les poids sont datés de ", m("T&minus;1"),
        " et non de ", m("T"), " : ils appartiennent à l'ensemble d'information. Un ",
        "contrôle bloquant refuse un jeu de poids unique répété sur toutes les origines.</p>")
ajouter(figure("06_poids.png", "Les poids sectoriels, trimestre par trimestre",
               paste0("Recalculés à chaque origine. Les mouvements sont lents, sauf en ",
                      "2020 où la part des services marchands recule brutalement — ce ",
                      "que des poids figés auraient manqué.")))

ajouter("<h3>16.2 Deux formules, et l'écart entre elles</h3>")
ajouter("<p>La formule ci-dessus agrège <strong>en niveaux</strong>. On rencontre ",
        "souvent sa version linéarisée :</p>")
ajouter(eq(paste0(m("g&#770;<sub>T</sub><sup>lin</sup>"), " = ",
                  "<span class='big'>&sum;</span><sub class='num'>j</sub> ",
                  m("w<sub>j,T&minus;1</sub> g&#770;<sub>j,T</sub>"))))
ajouter("<p>C'est l'approximation au premier ordre de la précédente. L'écart est un ",
        "<strong>effet de Jensen</strong> : l'exponentielle étant convexe, la version ",
        "linéarisée sous-estime systématiquement l'autre, et d'autant plus que les ",
        "croissances de branche sont dispersées.</p>")
ajouter(definition("L'inégalité de Jensen, en une ligne",
  paste0("<p>Pour une fonction <strong>convexe</strong> — dont la courbe est tournée ",
         "vers le haut, comme l'exponentielle — la moyenne des images est toujours ",
         "supérieure à l'image de la moyenne :</p>",
         eq(paste0("<span class='big'>&sum;</span><sub class='num'>j</sub> ",
                   m("w<sub>j</sub>"), " ", m("e"), "<sup>", m("g<sub>j</sub>"),
                   "</sup> <span class='op'>&ge;</span> ", m("e"),
                   "<sup><span class='big'>&sum;</span><sub class='num'>j</sub> ",
                   m("w<sub>j</sub> g<sub>j</sub>"), "</sup>")),
         "<p>Passer au logarithme conserve le sens de l'inégalité. L'agrégation en ",
         "niveaux est donc toujours supérieure ou égale à sa linéarisation, avec égalité ",
         "seulement si toutes les branches croissent exactement au même rythme.</p>",
         "<p>L'écart mesure donc la <strong>dispersion entre branches</strong> : nul ",
         "quand elles vont ensemble, maximal quand elles divergent. D'où sa pointe en ",
         "2020.</p>")))
ajouter(sprintf(paste0("<p>Mesuré sur la réalisation, l'écart vaut <strong>%s point en ",
                       "moyenne</strong> et %s en médiane, avec un maximum de %s point. ",
                       "Il est donc réel mais sans portée pratique — et les deux formules ",
                       "sont calculées en parallèle plutôt que l'une supposée ",
                       "négligeable.</p>"),
                nb(100 * ecartf$ecart_moyen, 4), nb(100 * ecartf$ecart_median, 4),
                nb(100 * ecartf$ecart_max, 3)))
ajouter(figure("06_ecart_formules.png", "L'écart entre les deux formules d'agrégation",
               paste0("L'écart se creuse exactement aux trimestres où les branches ",
                      "divergent le plus — 2020 au premier chef. C'est la signature d'un ",
                      "effet de convexité, non d'une erreur de calcul.")))

ajouter("<h3>16.3 L'agrégat est-il une bonne image du total ?</h3>")
ajouter("<p>La question se pose puisque l'agrégat est reconstruit et non publié. Elle a ",
        "été traitée en comparant l'indice obtenu à la somme des niveaux chaînés : ",
        "l'écart moyen est <strong>statistiquement nul</strong> — de l'ordre de ",
        "quelques millièmes de point, sans biais détectable — et sa dérive cumulée sur ",
        "l'ensemble du backtest reste inférieure à un demi-point.</p>")
ajouter("<p>L'agrégation est donc utilisable comme prévision de la valeur ajoutée ",
        "totale, sous les trois réserves de formulation posées à la section 1.</p>")

# ============================================================================
# 17. INTRA-TRIMESTRIEL
# ============================================================================
ajouter("<h2 id='s17'><span class='num'>17.</span>Le nowcasting au fil du trimestre</h2>")
ajouter("<p>C'est le mécanisme central du nowcasting, et celui qui le distingue d'une ",
        "prévision ordinaire : <strong>l'estimation se révise à mesure que les mois du ",
        "trimestre cible sont observés</strong>.</p>")
ajouter(definition("Ce que désignent M0, M1, M2 et M3",
  paste0("<p>La lettre ", m("M"), " est mise pour <em>mois</em>, et le chiffre qui la ",
         "suit compte <strong>combien de mois du trimestre que l'on cherche à estimer ",
         "sont déjà publiés</strong> au moment où l'on produit le chiffre. Formellement, ",
         "le scénario ", m("M<sub>k</sub>"), " correspond à l'ensemble d'information</p>",
         eq(paste0(m("&#8497;<sub>T</sub><sup>(k)</sup>"), " = { ",
                   m("g<sub>j,t</sub>"), " : ", m("t"),
                   " <span class='op'>&lt;</span> ", m("T"),
                   " } <span class='op'>&cup;</span> { ",
                   m("x<sub>i,m</sub>"), " : ", m("m"),
                   " <span class='op'>&le;</span> ", m("m<sub>k</sub>"), " }")),
         "<p>où ", m("m<sub>k</sub>"), " est le ", m("k"),
         "-ième mois du trimestre ", m("T"), " — et, pour ", m("k"),
         " = 0, le dernier mois du trimestre précédent.</p>",
         "<p>Trois précisions, parce que la notation se prête à trois contresens.</p>",
         "<p><strong>Ce ne sont pas des horizons de prévision.</strong> La cible est la ",
         "même dans les quatre cas — le trimestre ", m("T"), ". Seule change ",
         "l'<em>information disponible</em> pour l'estimer. On ne compare pas des ",
         "prévisions à un, deux ou trois trimestres, mais quatre états de connaissance ",
         "d'un même trimestre.</p>",
         "<p><strong>", m("M<sub>0</sub>"), " n'est pas l'absence d'information.</strong> ",
         "Tout le passé de la valeur ajoutée et tout le passé des indicateurs y sont ",
         "disponibles. Ce qui manque, ce sont les mois du trimestre cible ",
         "<em>lui-même</em> : le système doit alors les extrapoler.</p>",
         "<p><strong>Le scénario n'est pas un choix, il est constaté.</strong> À une date ",
         "donnée, on se trouve dans l'un des quatre, et c'est le calendrier de ",
         "publication qui le décide — pas le modélisateur.</p>")))
ajouter("<p>Quatre ensembles d'information sont ainsi distingués.</p>")
tM <- data.frame(
  Scénario = c("M0", "M1", "M2", "M3"),
  `Mois du trimestre cible déjà publiés` = c("aucun", "le premier",
                                             "les deux premiers", "les trois"),
  `Situation concrète` = c(
    "on se place avant ou au tout début du trimestre cible",
    "un mois écoulé et publié",
    "deux mois écoulés et publiés",
    "trimestre terminé, mais comptes nationaux pas encore diffusés"),
  `Mois restant à extrapoler` = c("trois", "deux", "un", "aucun"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(tM, aligne_droite = integer(0)))
ajouter(legende_tableau(paste0(
  "Les quatre ensembles d'information. C'est M3 qui correspond à la situation où le ",
  "nowcast a le plus de valeur pratique : le trimestre est terminé, toute ",
  "l'information infra-trimestrielle est là, et les comptes ne sont pas encore ",
  "publiés.")))
t17 <- intra %>% dplyr::filter(periode == "toutes origines") %>%
  dplyr::select(scenario, modele, ratio_median) %>%
  tidyr::pivot_wider(names_from = modele, values_from = ratio_median) %>%
  dplyr::transmute(Scénario = scenario,
                   `Modèle vectoriel` = nb(bvar, 3),
                   `Passerelle` = nb(bridge, 3),
                   `Combinaison` = nb(combinee, 3))
ajouter(tbl(as.data.frame(t17), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Ratio médian par branche, à périmètre commun aux quatre scénarios pour que la ",
  "comparaison porte sur les mêmes trimestres.")))
ajouter(sprintf(paste0("<p>Le modèle vectoriel est <strong>plat par construction</strong> ",
                       "— il ne lit que le passé de la valeur ajoutée, qui ne change pas ",
                       "d'un mois à l'autre. L'écart entre M0 et M3 mesure donc ",
                       "<em>exactement</em> ce que les indicateurs infra-trimestriels ",
                       "apportent : la combinaison passe de %s à %s.</p>"),
                nb(ip("M0", "combinee"), 3), nb(ip("M3", "combinee"), 3)))
ajouter(figure("09_valeur_information.png", "La valeur de l'information au fil du trimestre",
               paste0("Le modèle vectoriel est une horizontale ; les deux autres courbes ",
                      "descendent à mesure que les mois sont observés et croisent cette ",
                      "horizontale. La combinaison passe devant dès le premier mois.")))
ajouter(figure("09_mae_scenarios.png", "L'erreur absolue moyenne par scénario",
               paste0("La même progression, mesurée en erreur absolue plutôt qu'en ",
                      "ratio : la décroissance est monotone et régulière.")))
t17e <- scen9 %>%
  dplyr::mutate(modele = dplyr::recode(modele, bvar = "Modèle vectoriel",
                                       bridge = "Passerelle", combinee = "Combinaison")) %>%
  dplyr::select(scenario, modele, mae_moyenne) %>%
  tidyr::pivot_wider(names_from = modele, values_from = mae_moyenne) %>%
  dplyr::transmute(`Scénario` = scenario,
                   `Modèle vectoriel` = nb(100 * `Modèle vectoriel`, 2),
                   `Passerelle` = nb(100 * Passerelle, 2),
                   `Combinaison` = nb(100 * Combinaison, 2))
ajouter(tbl(as.data.frame(t17e), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Erreur absolue moyenne en points, sur l'ensemble des branches couvertes. La même ",
  "progression, mesurée sans normalisation par la volatilité.")))
ajouter(intuition(paste0(
  "<p>Cette figure est le résultat le plus proprement « nowcasting » du travail. Elle ",
  "montre que <strong>l'information mensuelle a une valeur mesurable, et qu'elle ",
  "s'accumule</strong>.</p>",
  "<p>Elle éclaire aussi un piège de présentation : évaluer un système de nowcasting ",
  "uniquement en fin de trimestre revient à ignorer ce pour quoi il est fait. Le ",
  "chiffre utile est celui qu'on peut produire <em>avant</em> la publication des ",
  "comptes, pas celui qu'on produirait si l'on attendait.</p>")))

ajouter("<h3>17.1 Une anomalie, et son élucidation</h3>")
ajouter("<p>Le premier résultat présentait une incohérence logique : <strong>le scénario ",
        "le mieux informé était moins bon que le précédent</strong>. Ajouter un mois ",
        "<em>observé</em> ne peut pourtant pas dégrader — il remplace une valeur ",
        "extrapolée par une donnée.</p>")
ajouter("<p>La seule autre différence entre les deux scénarios tenait à une convention : ",
        "les <strong>indicateurs trimestriels</strong> du trimestre cible n'y devenaient ",
        "disponibles qu'au dernier stade. Un test contrôlé — même sélection, mêmes ",
        "origines, une seule chose qui varie — tranche :</p>")
t17b <- m3m %>% dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Variante = scenario, `Ratio médian` = nb(ratio_median, 3),
                   `Branches sous 1` = br_ok,
                   `Indicateurs retenus` = nb(n_retenus_moyen, 2))
ajouter(tbl(as.data.frame(t17b), aligne_droite = 2:4))
ajouter(legende_tableau("Les deux variantes du scénario complet, avec et sans indicateurs trimestriels."))
ajouter(sprintf(paste0("<p>Ces indicateurs ne sont pas marginaux : le vivier en compte ",
                       "<strong>%d</strong>, dont %d passent les critères ",
                       "d'éligibilité.</p>"),
                nrow(diagtrim), sum(diagtrim$eligible)))
v15 <- function(x) m3m$ratio_median[m3m$scenario == x]
n15 <- function(x) m3m$n_retenus_moyen[m3m$scenario == x]
ajouter(sprintf(paste0("<p>Les retirer fait passer le ratio de %s à <strong>%s</strong>. ",
                       "Le mécanisme se lit dans la dernière colonne : la variante qui ",
                       "les admet retient <strong>%s indicateurs contre %s</strong> — ils ",
                       "occupent des places sous le plafond et <em>évincent des ",
                       "mensuels</em>.</p>"),
                nb(v15("M3 avec trimestriels"), 3), nb(v15("M3 mensuel seul"), 3),
                nb(n15("M3 avec trimestriels"), 2), nb(n15("M3 mensuel seul"), 2)))
ajouter("<p>Deux corrections plus douces ont été tentées avant l'exclusion. Un ",
        "<strong>seuil de corrélation renforcé</strong> pour ces seuls indicateurs : la ",
        "courbe reste plate puis chute d'un coup à l'exclusion, ce qui signifie que ",
        "<em>même ceux qui franchissent un seuil élevé nuisent encore</em>. Une ",
        "<strong>sélection sur la corrélation partielle</strong>, qui écarte par ",
        "construction ce qui est redondant : elle dégrade tout et en retient autant — ils ",
        "ne sont donc pas redondants.</p>")
ajouter("<h4>Première correction : un seuil renforcé pour eux seuls</h4>")
t17c <- seuilT %>%
  dplyr::transmute(`Seuil exigé` = ifelse(is.finite(seuil_trim), nb(seuil_trim, 2),
                                          "∞ (exclusion)"),
                   `Ratio médian` = nb(ratio_median, 3),
                   `Branches sous 1` = br_ok,
                   `Retenus` = nb(retenus_moyen, 2),
                   `dont trimestriels` = nb(dont_trimestriels, 2))
ajouter(tbl(as.data.frame(t17c), aligne_droite = 2:5))
ajouter(legende_tableau(paste0(
  "Balayage du seuil de corrélation exigé des seuls indicateurs trimestriels.")))
ajouter("<p>La courbe est <strong>plate sur toute la plage utile</strong> puis chute ",
        "d'un coup à l'exclusion. C'est la signature d'un effet de seuil, non d'un ",
        "compromis : <strong>même ceux qui franchissent le critère le plus exigeant ",
        "nuisent encore</strong>. Le problème n'est donc pas la faiblesse de leur ",
        "corrélation.</p>")
ajouter("<h4>Seconde correction : la sélection conditionnelle</h4>")
ajouter("<p>L'hypothèse devenait alors la <strong>redondance</strong> : ces indicateurs ",
        "mesureraient en moins fin ce que les mensuels mesurent déjà. Un critère marginal ",
        "ne peut pas le voir, un critère <em>partiel</em> le pourrait :</p>")
ajouter(eq(paste0(m("r"), "<sub class='num'>partiel</sub>( ", m("x<sub>k</sub>"),
                  " <span class='op'>|</span> ", m("Z"), " ) = corr( ", m("g"),
                  " <span class='op'>&minus;</span> ", m("P<sub>Z</sub> g"), " , ",
                  m("x<sub>k</sub>"), " <span class='op'>&minus;</span> ",
                  m("P<sub>Z</sub> x<sub>k</sub>"), " )")))
ajouter("<p>où ", m("P<sub>Z</sub>"), " est la projection orthogonale sur les ",
        "indicateurs déjà retenus.</p>")
t17d <- selcond %>% dplyr::arrange(ratio_median) %>%
  dplyr::transmute(`Méthode` = methode, `Ratio médian` = nb(ratio_median, 3),
                   `Branches sous 1` = br_ok, `Corrélation` = nb(correl, 3),
                   `Retenus` = nb(retenus, 2), `dont trimestriels` = nb(dont_trim, 2))
ajouter(tbl(as.data.frame(t17d), aligne_droite = 2:6))
ajouter(legende_tableau("Sélection marginale contre sélection conditionnelle."))
ajouter("<p>Le critère partiel <strong>dégrade tout</strong>, et il en retient autant : ",
        "ces indicateurs ne sont donc pas redondants avec les mensuels.</p>")
ajouter(intuition(paste0(
  "<p>La lecture qui subsiste est la suivante : ces indicateurs sont ",
  "<strong>corrélés à la valeur ajoutée sur l'historique mais instables hors ",
  "échantillon</strong>.</p>",
  "<p>Aucun critère fondé sur l'ajustement passé — marginal ou partiel — ne peut ",
  "détecter cela, par construction : ils y sont bons. Seule l'évaluation hors ",
  "échantillon le révèle. C'est un argument de plus, s'il en fallait, pour ne jamais ",
  "sélectionner sur le seul ajustement.</p>")))

# ============================================================================
# 18. PROTOCOLE
# ============================================================================
ajouter("<h2 id='s18'><span class='num'>18.</span>Le protocole d'évaluation</h2>")
ajouter(sprintf(paste0("<p>L'évaluation porte sur <strong>%d trimestres</strong>, de %s à ",
                       "%s. Chaque point est produit par réestimation complète sur la ",
                       "seule information antérieure.</p>"),
                n_trim, ag$trimestre[1], ag$trimestre[nrow(ag)]))
ajouter("<p>La borne de départ n'est pas un choix statistique : c'est le premier ",
        "trimestre pour lequel des poids d'agrégation en prix courants existent.</p>")

ajouter("<h3>18.1 Les mesures</h3>")
ajouter(eq(paste0("RMSFE = <span class='big'>&radic;</span><span style='text-decoration:overline'>",
                  "<span class='fr'><span class='hi'>1</span><span class='lo'>", m("n"),
                  "</span></span><span class='big'>&sum;</span><sub class='num'>t</sub> ",
                  "( ", m("g<sub>t</sub>"), " <span class='op'>&minus;</span> ",
                  m("g&#770;<sub>t</sub>"), " )<sup>2</sup></span>",
                  " ,&nbsp;&nbsp;&nbsp; MAE = ",
                  "<span class='fr'><span class='hi'>1</span><span class='lo'>", m("n"),
                  "</span></span><span class='big'>&sum;</span><sub class='num'>t</sub> ",
                  "<span class='op'>|</span>", m("g<sub>t</sub>"),
                  " <span class='op'>&minus;</span> ", m("g&#770;<sub>t</sub>"),
                  "<span class='op'>|</span>")))
ajouter("<p>La mesure principale est toutefois le <strong>ratio</strong>, qui rapporte ",
        "l'erreur à la variabilité de la série :</p>")
ajouter(eq(paste0("ratio = ",
                  "<span class='fr'><span class='hi'>RMSFE</span><span class='lo'>",
                  m("&sigma;"), "( ", m("g<sub>t</sub>"), " )</span></span>")))
ajouter(definition("Comment lire un ratio",
  paste0("<p>Un ratio de 1 signifie que le modèle fait aussi bien que prédire ",
         "systématiquement la moyenne historique — c'est-à-dire ne rien prévoir du tout. ",
         "Sous 1, le modèle apporte quelque chose ; au-dessus, il dégrade.</p>",
         "<p>L'intérêt du ratio sur le RMSFE brut est qu'il est <strong>comparable entre ",
         "branches</strong>. Une branche dix fois plus volatile qu'une autre aura ",
         "mécaniquement un RMSFE dix fois plus grand sans être moins bien prévue. C'est ",
         "précisément le cas de la pêche et de l'hébergement, et la confusion entre les ",
         "deux notions est la source d'erreur de lecture la plus fréquente.</p>")))
ajouter("<p>Le <strong>biais</strong> complète le diagnostic : une erreur moyenne ",
        "non nulle signale une tendance systématique à surestimer ou sous-estimer, ce ",
        "qu'un RMSFE ne distingue pas d'une erreur purement aléatoire.</p>")

# ============================================================================
# 19. RESULTAT PRINCIPAL
# ============================================================================
ajouter("<h2 id='s19'><span class='num'>19.</span>Résultat principal et étalons de comparaison</h2>")
ajouter(definition("À quoi sert un étalon de comparaison",
  paste0("<p>Un ratio de 0,85 ne veut rien dire dans l'absolu : tout dépend de ce à quoi ",
         "l'on se compare. Un <strong>étalon</strong> est un modèle volontairement ",
         "simple, dont on sait qu'il n'exploite presque aucune information, et qui sert ",
         "de plancher.</p>",
         "<p>Les six retenus vont du plus naïf au moins naïf. Le ",
         "<strong>naïf</strong> recopie le trimestre précédent. La <strong>moyenne ",
         "historique</strong> prédit toujours la moyenne des trimestres passés. Les ",
         "<strong>autorégressions</strong> AR(1), AR(2) et AR(4) régressent la croissance ",
         "sur un, deux ou quatre de ses propres retards. Le <strong>modèle vectoriel ",
         "seul</strong>, enfin, est notre propre système privé de ses indicateurs.</p>",
         "<p>La condition de validité est stricte : chacun doit être réestimé ",
         "<em>récursivement</em>, aux mêmes origines, sur la même cible. Comparer un ",
         "modèle récursif à un étalon estimé une fois sur tout l'échantillon serait ",
         "biaisé en faveur du second.</p>")))
ajouter("<p>Six modèles de référence ont été réestimés sur <strong>exactement le même ",
        "protocole récursif</strong>, aux mêmes origines. La comparaison n'a de sens ",
        "qu'à cette condition.</p>")
t19 <- bench %>% dplyr::filter(periode == "toutes origines") %>%
  dplyr::arrange(ratio) %>%
  dplyr::transmute(Modèle = modele, `RMSFE (%)` = nb(100 * RMSFE, 2),
                   `Ratio` = nb(ratio, 3), `Corrélation` = nb(correlation, 2),
                   `Biais (pt)` = nb(100 * biais, 2))
ajouter(tbl(as.data.frame(t19), aligne_droite = 2:5))
ajouter(legende_tableau(sprintf(
  "Agrégat, %d trimestres. Tous les étalons sont réestimés récursivement.", n_trim)))
ajouter(figure("13_rmsfe_par_modele.png", "Les sept modèles, par erreur quadratique",
               paste0("Le système est à gauche, le naïf à droite. L'écart au modèle ",
                      "vectoriel est visible mais mince au regard de la distance qui ",
                      "sépare ces deux-là des modèles univariés.")))
ajouter(intuition(paste0(
  "<p>Deux lectures s'imposent, et la seconde est la plus instructive.</p>",
  "<p><strong>Aucun modèle univarié ne bat la moyenne de long terme.</strong> Les trois ",
  "autorégressions dépassent toutes 1. La croissance trimestrielle marocaine est ",
  "faiblement — et <em>négativement</em> — autocorrélée : extrapoler le passé revient à ",
  "parier contre la réalisation, d'où leurs corrélations négatives. Elles ne sont pas ",
  "seulement imprécises, elles vont dans le mauvais sens.</p>",
  "<p>Seuls le système complet et le modèle vectoriel obtiennent une corrélation ",
  "franchement positive. C'est là que réside l'apport, bien plus que dans le niveau du ",
  "RMSFE.</p>")))

ajouter("<h3>19.1 La décomposition qui change la lecture</h3>")
t19b <- bench %>% dplyr::filter(periode != "toutes origines") %>%
  dplyr::select(modele, periode, ratio) %>%
  tidyr::pivot_wider(names_from = periode, values_from = ratio) %>%
  dplyr::arrange(`hors 2020`) %>%
  dplyr::transmute(Modèle = modele, `Ratio en 2020` = nb(`2020`, 3),
                   `Ratio hors 2020` = nb(`hors 2020`, 3))
ajouter(tbl(as.data.frame(t19b), aligne_droite = 2:3))
ajouter(legende_tableau("Le même classement, scindé sur la seule année de rupture."))
ajouter("<div class='encadre alerte'><span class='etiq'>Le classement global est porté par quatre trimestres</span>",
        sprintf(paste0("<p>Hors 2020, le ratio du système remonte à <strong>%s</strong> ",
                       "et sa corrélation tombe à <strong>%s</strong>. L'écart au modèle ",
                       "vectoriel devient ténu — %s contre %s — et la moyenne historique ",
                       "n'est plus qu'à %s.</p>"),
                nb(b_hors("Systeme complet")$ratio, 3),
                nb(b_hors("Systeme complet")$correlation, 2),
                nb(b_hors("Systeme complet")$ratio, 3),
                nb(b_hors("BVAR agrege")$ratio, 3),
                nb(b_hors("Moyenne historique")$ratio, 3)),
        "<p>Il faut le dire sans détour : <strong>l'essentiel de la performance affichée ",
        "provient de la capacité à voir 2020 arriver</strong>. C'est une qualité réelle, ",
        "et c'est précisément ce qu'on demande à un nowcast — un système utile est un ",
        "système qui sert quand la conjoncture décroche.</p>",
        "<p>Mais le chiffre d'ensemble ne doit pas être présenté comme une performance ",
        "de régime courant. Le classement, lui, ne s'inverse dans aucune ",
        "sous-période.</p></div>")
ajouter(figure("06_agregat.png", "La valeur ajoutée totale : réalisé et nowcast",
               paste0("Le nowcast suit la réalisation, y compris l'effondrement de 2020 ",
                      "que le modèle vectoriel seul, en pointillé, ne voit pas. En ",
                      "période calme les courbes se confondent davantage — c'est là que ",
                      "le gain se réduit.")))
ajouter(figure("13_systeme_contre_ar2.png",
               "Le système et l'AR(2), trimestre par trimestre",
               paste0("Les deux courbes se confondent en régime calme et ne se séparent ",
                      "que sur les ruptures : c'est la lecture graphique du tableau des ",
                      "régimes de la section 22.")))
ajouter("<h3>19.2 L'erreur, trimestre par trimestre</h3>")
t19c <- erreurs_t %>% dplyr::filter(modele == "Systeme complet") %>%
  dplyr::mutate(ab = abs(erreur)) %>%
  dplyr::arrange(dplyr::desc(ab)) %>% head(6) %>%
  dplyr::transmute(Trimestre = trimestre,
                   `Réalisé (%)` = nb(100 * reel, 2),
                   `Nowcast (%)` = nb(100 * prevision, 2),
                   `Erreur (pt)` = nb(100 * erreur, 2))
ajouter(tbl(as.data.frame(t19c), aligne_droite = 2:4))
ajouter(legende_tableau("Les six trimestres les plus mal estimés."))
ajouter(figure("13_erreurs.png", "L'erreur des sept modèles dans le temps",
               paste0("Les modèles univariés manquent tous le décrochage de 2020 dans ",
                      "les mêmes proportions. L'écart au système s'y concentre.")))
ajouter(sprintf(paste0("<p>Les plus grosses erreurs se concentrent sur %d trimestres, ",
                       "tous situés en période de rupture. Hors de ces trimestres, ",
                       "l'erreur médiane tombe à <strong>%s point</strong>.</p>"),
                6L, nb(100 * stats::median(abs(erreurs_t$erreur[
                  erreurs_t$modele == "Systeme complet"])), 2)))

# ============================================================================
# 20. SIGNIFICATIVITE
# ============================================================================
ajouter("<h2 id='s20'><span class='num'>20.</span>Les écarts sont-ils significatifs ?</h2>")
ajouter("<p>Un écart de RMSFE n'est pas une preuve. Le test de Diebold-Mariano",
        ref("dm"), " compare la <strong>perte différentielle</strong> de deux ",
        "prévisions :</p>")
ajouter(eq(paste0(m("d<sub>t</sub>"), " = ", m("e<sub>1,t</sub><sup>2</sup>"),
                  " <span class='op'>&minus;</span> ", m("e<sub>2,t</sub><sup>2</sup>"),
                  " ,&nbsp;&nbsp;&nbsp; ", m("DM"), " = ",
                  "<span class='fr'><span class='hi'>", m("d&#772;"),
                  "</span><span class='lo'><span class='big'>&radic;</span>",
                  "<span style='text-decoration:overline'>",
                  m("V&#770;"), "( ", m("d&#772;"), " )</span></span></span>")))
ajouter("<p>sous ", m("H<sub>0</sub>"), " : E[", m("d<sub>t</sub>"),
        "] = 0, les deux prévisions ayant alors la même précision. La variance de long ",
        "terme est estimée par un noyau tenant compte de l'autocorrélation des pertes.</p>")
ajouter("<p>Avec un échantillon court, la statistique est biaisée vers le rejet. La ",
        "correction de petit échantillon", ref("hln"), " la rééchelonne :</p>")
ajouter(eq(paste0(m("DM*"), " = ", m("DM"), " <span class='op'>&times;</span> ",
                  "<span class='big'>&radic;</span><span style='text-decoration:overline'>",
                  "<span class='fr'><span class='hi'>", m("n"),
                  " <span class='op'>+</span> 1 <span class='op'>&minus;</span> 2",
                  m("h"), " <span class='op'>+</span> ",
                  m("h"), "(", m("h"), "<span class='op'>&minus;</span>1)/",
                  m("n"), "</span><span class='lo'>", m("n"),
                  "</span></span></span>")))
t20 <- dm13 %>% dplyr::arrange(p_value) %>%
  dplyr::transmute(`Étalon` = etalon,
                   `RMSFE étalon (%)` = nb(100 * RMSFE_etalon, 2),
                   `Écart (%)` = nb(100 * (RMSFE_etalon - RMSFE_systeme), 2),
                   `p` = nb(p_value, 3), Verdict = verdict)
ajouter(tbl(as.data.frame(t20), aligne_droite = 2:4))
ajouter(legende_tableau(sprintf(paste0(
  "Test bilatéral contre le système complet, dont le RMSFE vaut %s %%."),
  nb(100 * dm13$RMSFE_systeme[1], 2))))
ajouter(definition("Pourquoi des écarts de 20 à 40 % ne sont pas significatifs",
  paste0("<p>La réponse tient dans la structure de la statistique. La perte ",
         "différentielle ", m("d<sub>t</sub>"), " est <strong>dominée par quatre ",
         "trimestres</strong>, ceux de 2020, où les erreurs des deux modèles sont ",
         "grandes et leur différence l'est aussi.</p>",
         "<p>La <em>variance</em> de ", m("d<sub>t</sub>"),
         " explose donc en même temps que sa moyenne, et leur rapport reste petit. Avec ",
         "une cinquantaine d'observations, le test ne peut trancher qu'un écart ",
         "<strong>régulier</strong> ; un écart concentré sur une crise reste hors de ",
         "portée.</p>",
         "<p>La conclusion correcte n'est donc pas « le système ne vaut pas mieux », ",
         "mais <strong>« cet échantillon ne suffit pas à le prouver »</strong>. C'est ",
         "une limite de puissance, pas un verdict sur le modèle.</p>")))
ajouter("<h3>20.1 Ce qui résiste malgré tout aux tests</h3>")
dmc <- dm_gr %>% dplyr::filter(grepl("Passerelle$", comparaison))
ajouter(sprintf(paste0("<p>Un résultat traverse les quatre scénarios : ",
                       "<strong>la combinaison bat chacune de ses deux composantes prises ",
                       "isolément</strong>, et cela atteint la significativité dans ",
                       "%d des %d scénarios face à la passerelle seule.</p>"),
                sum(dmc$p_value < 0.10), nrow(dmc)))
ajouter("<p>C'est le résultat le plus solide du travail : non pas que le système soit ",
        "meilleur qu'un modèle naïf — l'échantillon ne le prouve pas — mais que ",
        "<strong>combiner vaut mieux que choisir</strong>.</p>")
ajouter(definition("Le bootstrap par blocs",
  paste0("<p>Le <em>bootstrap</em> consiste à rééchantillonner les observations avec ",
         "remise, un grand nombre de fois, pour observer comment une statistique varie ",
         "d'un échantillon à l'autre — et en déduire un intervalle de confiance sans ",
         "formule analytique.</p>",
         "<p>Appliqué tel quel à une série temporelle, il est <strong>faux</strong> : ",
         "tirer les trimestres un par un détruit leur ordre, donc l'autocorrélation qui ",
         "les lie. L'intervalle obtenu serait trop étroit.</p>",
         "<p>La version <strong>par blocs</strong>", ref("kunsch"),
         " tire des séquences consécutives de ",
         "trimestres plutôt que des trimestres isolés. La structure temporelle est ",
         "préservée à l'intérieur de chaque bloc, et l'intervalle redevient honnête.</p>")))
ajouter(figure("10_intervalles_ratio.png", "Intervalles de confiance sur le ratio",
               paste0("Obtenus par bootstrap par blocs, qui préserve l'autocorrélation ",
                      "des séries. Les intervalles se chevauchent largement : c'est la ",
                      "traduction graphique du tableau ci-dessus.")))
ajouter("<h3>20.2 Le test branche par branche</h3>")
t20b <- dmbr %>% dplyr::filter(scenario == "M3", significatif) %>%
  dplyr::arrange(p_value) %>%
  dplyr::transmute(Branche = branche, Comparaison = comparaison,
                   `RMSFE référence (%)` = nb(100 * RMSFE_ref, 2),
                   `RMSFE alternative (%)` = nb(100 * RMSFE_alt, 2),
                   `p` = nb(p_value, 3))
if (nrow(t20b) > 0) {
  ajouter(tbl(as.data.frame(t20b), aligne_droite = 3:5))
  ajouter(legende_tableau(paste0(
    "Les seules comparaisons atteignant la significativité au niveau des branches, ",
    "scénario complet.")))
} else {
  ajouter("<p>Aucune comparaison n'atteint la significativité au niveau des branches ",
          "dans le scénario complet.</p>")
}
ajouter(sprintf(paste0("<p>Sur les %d comparaisons menées branche par branche et tous ",
                       "scénarios confondus, <strong>%d atteignent la ",
                       "significativité</strong>. Le constat est cohérent avec celui de ",
                       "l'agrégat : les échantillons par branche sont encore plus courts ",
                       "et les tests encore moins puissants.</p>"),
                nrow(dmbr), sum(dmbr$significatif)))
ajouter(figure("10_dm_par_branche.png", "Les tests branche par branche",
               paste0("La plupart des probabilités critiques se situent loin des seuils ",
                      "usuels. C'est un problème de puissance, non un verdict sur les ",
                      "modèles.")))
ajouter("<h3>20.3 Le backtest étendu, et ce qu'il ajoute</h3>")
t20c <- dmext %>% dplyr::arrange(p_value) %>%
  dplyr::transmute(`Échantillon` = echantillon, `Scénario` = scenario,
                   Comparaison = comparaison, `n` = n_origines,
                   `p` = nb(p_value, 3))
ajouter(tbl(as.data.frame(t20c), aligne_droite = 4:5))
ajouter(legende_tableau(paste0(
  "Tests sur le backtest prolongé en amont, qui ajoute un second épisode de rupture.")))
ajouter(figure("09b_puissance.png", "Puissance du test selon la taille d'échantillon",
               paste0("La courbe montre combien d'observations seraient nécessaires pour ",
                      "détecter un écart de l'ampleur observée. L'ordre de grandeur ",
                      "dépasse largement ce dont on dispose.")))

# ============================================================================
# 21. PAR BRANCHE
# ============================================================================
ajouter("<h2 id='s21'><span class='num'>21.</span>Résultats par branche et contributions</h2>")
t21 <- diag3 %>% dplyr::arrange(ratio) %>%
  dplyr::transmute(Branche = branche,
                   `Écart-type réalisé (%)` = nb(100 * ecart_type_reel, 2),
                   `RMSFE (%)` = nb(100 * RMSFE, 2),
                   `Ratio` = nb(ratio, 3),
                   `Biais (pt)` = nb(100 * biais, 2))
ajouter(tbl(as.data.frame(t21), aligne_droite = 2:5))
ajouter(legende_tableau("Les seize branches, par ratio croissant."))
ajouter(figure("03_qualite_par_branche.png", "La qualité de prévision par branche",
               paste0("Le classement est dominé par la volatilité propre de chaque ",
                      "branche : c'est précisément ce que l'encadré ci-dessous met en ",
                      "garde de mal lire.")))
ajouter(figure("03_nuages_prevu_realise.png", "Prévu contre réalisé, seize branches",
               paste0("Une branche bien prévue donne un nuage aligné sur la première ",
                      "bissectrice. Plusieurs branches donnent un nuage horizontal : le ",
                      "modèle y prédit à peu près sa moyenne.")))
ajouter(figure("03_erreurs_par_branche.png", "Distribution des erreurs par branche",
               paste0("La dispersion des erreurs suit celle de la volatilité de branche, ",
                      "ce qui est attendu et justifie de raisonner en ratio plutôt qu'en ",
                      "niveau d'erreur.")))
ajouter(figure("03_previsions_par_branche.png", "Prévisions et réalisations, par branche",
               paste0("Le détail branche par branche du modèle vectoriel seul. Les ",
                      "trajectoires prévues sont visiblement plus lisses que les ",
                      "réalisations : c'est l'effet du resserrement bayésien.")))
ajouter("<div class='encadre'><span class='etiq'>Un ratio supérieur à 1 ne signifie pas « imprévisible »</span>",
        "<p>La dispersion entre branches est bien plus grande que l'écart entre modèles, ",
        "et la tentation est de conclure que les branches mal classées sont mal ",
        "prévues.</p>",
        "<p>C'est une erreur de lecture. Le ratio rapporte l'erreur à ",
        "l'écart-type <em>de la branche</em>. Une branche très volatile peut afficher ",
        "un RMSFE élevé et un excellent ratio ; une branche très régulière peut avoir un ",
        "RMSFE minuscule et un ratio supérieur à 1, parce qu'il n'y avait tout ",
        "simplement rien à prévoir — sa moyenne suffisait.</p>",
        "<p>C'est pour l'agrégat, et non pour la moyenne des branches, que le système ",
        "est construit : la section suivante montre pourquoi les deux ne coïncident ",
        "pas.</p></div>")

ajouter("<h3>21.1 D'où vient l'erreur de l'agrégat</h3>")
t21b <- contrib %>% dplyr::arrange(dplyr::desc(contribution_abs)) %>%
  head(8) %>%
  dplyr::transmute(Branche = branche,
                   `Poids moyen (%)` = nb(100 * poids_moyen, 1),
                   `Erreur propre (pt)` = nb(100 * erreur_branche, 2),
                   `Contribution (pt)` = nb(100 * contribution_abs, 3))
ajouter(tbl(as.data.frame(t21b), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Contribution = poids × erreur absolue moyenne. Huit premières branches.")))
ajouter(figure("15_contributions.png", "Contribution de chaque branche à l'erreur agrégée",
               paste0("La barre combine poids et erreur propre. Les branches lourdes ",
                      "dominent, même quand leur erreur relative est modeste.")))
ajouter(intuition(paste0(
  "<p>La contribution est un <strong>produit</strong>, et ses deux facteurs jouent en ",
  "sens inverse. L'hébergement-restauration commet de loin la plus grosse erreur propre ",
  "mais ne pèse que quelques pour cent : il n'arrive qu'en quatrième position. ",
  "L'industrie de transformation se trompe quatre fois moins mais pèse près d'un ",
  "sixième de la valeur ajoutée : elle arrive première.</p>",
  "<p>Conséquence pratique, et elle est contre-intuitive : <strong>l'effort ",
  "d'amélioration doit viser les branches lourdes, non celles dont le ratio est le plus ",
  "laid</strong>. Les trois premières font la moitié de l'erreur agrégée.</p>")))

ajouter("<h3>21.2 La stabilité des coefficients</h3>")
t21c <- stabco %>% dplyr::arrange(dplyr::desc(amplitude)) %>% head(6) %>%
  dplyr::transmute(Branche = branche, `Moyenne` = nb(moyenne, 3),
                   `Écart-type` = nb(ecart_type, 3), `Amplitude` = nb(amplitude, 3))
ajouter(tbl(as.data.frame(t21c), aligne_droite = 2:4))
ajouter(legende_tableau(paste0(
  "Coefficient autorégressif d'ordre 1, réestimé à chaque origine. Six branches les ",
  "plus instables.")))
ajouter("<h3>21.3 Les résidus du modèle vectoriel, branche par branche</h3>")
t21d <- res3 %>% dplyr::arrange(ljung_box_p) %>% head(6) %>%
  dplyr::transmute(Branche = branche,
                   `Écart-type du résidu (%)` = nb(100 * ecart_type_residu, 2),
                   `Autocorrélation d'ordre 1` = nb(autocorr_ordre1, 3),
                   `p (Ljung-Box)` = nb(ljung_box_p, 3),
                   `Résidus extrêmes` = n_residus_extremes)
ajouter(tbl(as.data.frame(t21d), aligne_droite = 2:5))
ajouter(legende_tableau(paste0(
  "Six branches dont les résidus s'écartent le plus de l'hypothèse de bruit blanc.")))
ajouter(sprintf(paste0("<p>Sur les seize branches, <strong>%d présentent une ",
                       "autocorrélation résiduelle détectable</strong>. Là où c'est le ",
                       "cas, il reste en principe de la structure à exploiter — mais les ",
                       "tentatives de la section 27 montrent que cette structure ne se ",
                       "traduit pas en gain de prévision.</p>"),
                sum(res3$residus_autocorreles)))
ajouter("<p>Une seule branche change de <strong>signe</strong> au fil des origines : ",
        "c'est la trace chiffrée de la rupture de définition traitée à la section 9. ",
        "Tant qu'elle était dans l'échantillon, le coefficient était piloté par un ",
        "artefact. Les autres branches restent dans une plage étroite, ce qui indique ",
        "une dynamique stable.</p>")
ajouter(figure("15_stabilite.png", "Les coefficients au fil des origines",
               paste0("La réestimation récursive ne produit pas d'errance : les ",
                      "coefficients se déplacent lentement, sauf pour la branche à ",
                      "rupture.")))

# ============================================================================
# 22. EPISODES
# ============================================================================
ajouter("<h2 id='s22'><span class='num'>22.</span>Performance par régime conjoncturel</h2>")
ajouter("<p>Une moyenne sur l'ensemble des trimestres écrase une structure qui est, elle, ",
        "la vraie information. Le backtest est donc découpé en régimes.</p>")
t22 <- epis %>%
  dplyr::transmute(`Épisode` = episode, `n` = n,
                   `Croissance moy. (%)` = nb(100 * croissance_moyenne, 2),
                   `RMSFE système (%)` = nb(100 * RMSFE_nowcast, 2),
                   `RMSFE AR(2) (%)` = nb(100 * RMSFE_ar2, 2),
                   `Gain (%)` = pc(100 * gain_sur_ar2, 0))
ajouter(tbl(as.data.frame(t22), aligne_droite = 2:6))
ajouter(legende_tableau("Les trimestres du backtest, répartis en cinq régimes."))
ajouter(figure("15_episodes.png", "L'erreur par régime conjoncturel",
               paste0("Le système domine nettement sur les ruptures. Sur les ",
                      "ralentissements, les barres se rapprochent.")))
g_ral <- epis$gain_sur_ar2[epis$episode == "Ralentissement"]
g_cov <- epis$gain_sur_ar2[grepl("COVID", epis$episode)]
ajouter(sprintf(paste0("<p>Le gain vaut <strong>%s sur la crise sanitaire</strong> ",
                       "et tombe à <strong>%s sur les ralentissements</strong>, de ",
                       "loin le régime où le système apporte le moins.</p>"),
                pc(100 * g_cov, 0), pc(100 * g_ral, 0)))
ajouter(intuition(paste0(
  "<p>Cette dernière ligne est la plus instructive du rapport, parce qu'elle est ",
  "<em>contre-intuitive</em> : on attendrait d'un nowcast qu'il serve d'abord quand la ",
  "conjoncture se retourne.</p>",
  "<p>L'explication tient à la nature des indicateurs. Ils sont ",
  "<strong>coïncidents, non avancés</strong> : ils enregistrent un effondrement pendant ",
  "qu'il se produit — d'où l'excellente performance en 2020 — mais un ralentissement ",
  "graduel ne laisse pas de signature mensuelle assez nette pour se distinguer du ",
  "bruit.</p>",
  "<p>Formulé autrement : <strong>le système voit les chocs, pas les inflexions</strong>. ",
  "C'est une caractéristique de l'information disponible, non un défaut de ",
  "modélisation, et aucun raffinement d'estimation ne la corrigera.</p>")))
ajouter("<h3>22.1 Une vérification sur un second épisode</h3>")
ajouter("<p>Un reproche évident s'adresse à ce qui précède : tout reposerait sur une ",
        "seule crise. Le backtest a donc été étendu en amont sur les branches dont la ",
        "profondeur historique le permet, de façon à disposer d'un épisode de rupture ",
        "<strong>indépendant</strong> — la crise financière de 2008-2009.</p>")
ajouter(figure("09b_episodes.png", "Erreur par épisode sur le backtest étendu",
               paste0("Sur la crise de 2008 comme sur 2020, la combinaison bat le modèle ",
                      "vectoriel ; en période calme, celui-ci reste devant. Le résultat ",
                      "se reproduit donc sur deux épisodes indépendants.")))
ajouter("<p>La conclusion est cohérente : <strong>l'apport des indicateurs est ",
        "conditionnel aux ruptures</strong>, et ce n'est pas une particularité de 2020.</p>")

# ============================================================================
# 23. DIRECT / INDIRECT
# ============================================================================
ajouter("<h2 id='s23'><span class='num'>23.</span>Prévoir directement ou par les branches ?</h2>")
ajouter("<p>Toute l'architecture repose sur un choix jamais interrogé jusqu'ici : ",
        "prévoir <strong>seize branches puis agréger</strong>. L'alternative consiste à ",
        "estimer une équation passerelle directement sur la croissance de l'agrégat.</p>")
ajouter(eq(paste0(m("g&#770;<sub>T</sub><sup>indirect</sup>"),
                  " = log <span class='big'>&sum;</span><sub class='num'>j</sub> ",
                  m("w<sub>j,T&minus;1</sub>"), " ", m("e"), "<sup>",
                  m("g&#770;<sub>j,T</sub>"), "</sup>",
                  " &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ",
                  m("g&#770;<sub>T</sub><sup>direct</sup>"), " = ", m("&alpha;"),
                  " <span class='op'>+</span> ", m("&beta;&prime;X<sub>T</sub>"))))
t23 <- comp16 %>%
  dplyr::mutate(voie = dplyr::recode(voie,
    directe = "Directe (une équation sur l'agrégat)",
    indirecte = "Indirecte (seize branches agrégées) — retenue",
    mixte = "Combinaison des deux voies",
    bvar_indirect = "Modèle vectoriel seul, agrégé", ar2 = "AR(2)")) %>%
  dplyr::select(voie, periode, ratio) %>%
  tidyr::pivot_wider(names_from = periode, values_from = ratio) %>%
  dplyr::arrange(`toutes origines`) %>%
  dplyr::transmute(Voie = voie, `Ratio global` = nb(`toutes origines`, 3),
                   `2020` = nb(`2020`, 3), `Hors 2020` = nb(`hors 2020`, 3))
ajouter(tbl(as.data.frame(t23), aligne_droite = 2:4))
ajouter(legende_tableau("Même protocole récursif, mêmes origines, même cible."))
ajouter(figure("16_voies.png", "Les deux voies, trimestre par trimestre",
               paste0("La voie directe colle remarquablement au réalisé en 2020 puis ",
                      "diverge ensuite : c'est le basculement que le tableau chiffre.")))
ajouter("<div class='encadre alerte'><span class='etiq'>Le classement s'inverse selon la période</span>",
        "<p>Sur l'ensemble, la voie directe paraît nettement meilleure. La décomposition ",
        "renverse le tableau : <strong>spectaculaire en 2020</strong>, et ",
        "<strong>pire que de ne rien prévoir hors 2020</strong>.</p>",
        "<p>La voie indirecte fait l'inverse : honorable en 2020, et utile hors 2020. ",
        "Autrement dit, la voie directe n'est pas un meilleur modèle — c'est un ",
        "<em>détecteur de choc</em>. Quatre trimestres sur une cinquantaine lui offrent ",
        "son classement global.</p></div>")
t23b <- dm16 %>% dplyr::transmute(`Référence` = reference, `Alternative` = alternative,
                                  `p` = nb(p_value, 3), Verdict = verdict)
ajouter(tbl(as.data.frame(t23b), aligne_droite = 3))
ajouter(legende_tableau("Test de précision comparée entre les trois voies."))
ajouter(figure("16_comparaison.png", "Les trois voies selon la période",
               paste0("Aucune voie n'est meilleure dans les deux régimes à la fois. ",
                      "C'est la raison, non statistique, du choix retenu.")))
ajouter(sprintf(paste0("<p>Aucun écart n'est significatif — le plus favorable atteint ",
                       "p = %s. <strong>Les données ne permettent pas de départager les ",
                       "deux voies.</strong></p>"), nb(min(dm16$p_value), 3)))
ajouter(definition("Pourquoi la voie indirecte est retenue malgré un ratio global moins bon",
  paste0("<p>Trois raisons, et aucune n'est statistique — puisque le test ne tranche ",
         "pas.</p>",
         "<p><strong>La stabilité entre régimes.</strong> Un modèle dont le ratio passe ",
         "de 0,27 à 1,35 selon la période n'est pas utilisable en production : au moment ",
         "de publier, on ne sait pas dans lequel des deux régimes on se trouve.</p>",
         "<p><strong>La parcimonie.</strong> La voie directe régresse une seule série ",
         "d'une cinquantaine de points sur un panier d'indicateurs agrégés. Le ",
         "surajustement y est structurellement plus probable, et le ratio supérieur à 1 ",
         "hors 2020 en est vraisemblablement la manifestation.</p>",
         "<p><strong>La lisibilité.</strong> La voie indirecte fournit une décomposition ",
         "par branche : d'où vient la croissance, quelle branche porte l'erreur. La voie ",
         "directe ne produit qu'un nombre, qu'on ne peut ni expliquer ni contester.</p>")))

# ============================================================================
# 24. INCERTITUDE
# ============================================================================
ajouter("<h2 id='s24'><span class='num'>24.</span>L'incertitude autour du chiffre</h2>")
ajouter("<p>Un nowcast sans intervalle est une opinion. L'incertitude est construite en ",
        "trois temps.</p>")

ajouter("<h3>24.1 La distribution prédictive du modèle vectoriel</h3>")
ajouter("<p>La conjugaison normale-inverse-Wishart permet de tirer directement dans la ",
        "loi <em>a posteriori</em>, sans échantillonneur :</p>")
ajouter("<ol>",
        "<li>tirer ", m("&Sigma;<sup>(s)</sup>"), " dans une loi de Wishart inverse, ",
        "par décomposition de Bartlett ;</li>",
        "<li>tirer ", m("B<sup>(s)</sup>"), " dans la loi normale matricielle ",
        "conditionnelle à ", m("&Sigma;<sup>(s)</sup>"), " ;</li>",
        "<li>propager le modèle d'un pas et ajouter un choc ",
        m("&epsilon;<sup>(s)</sup>"), " tiré dans ", m("N(0, &Sigma;<sup>(s)</sup>)"),
        " ;</li>",
        "<li>agréger les seize branches du tirage par la formule de la section 16.</li>",
        "</ol>")
ajouter("<p>L'agrégation <em>à l'intérieur</em> de chaque tirage est importante : elle ",
        "préserve la corrélation entre branches, qu'une agrégation des quantiles ",
        "marginaux détruirait.</p>")
t24 <- calib %>% dplyr::transmute(`Niveau annoncé` = sprintf("%.0f %%", 100 * niveau),
                                  `Couverture observée` = sprintf("%.0f %%", 100 * couverture),
                                  `Écart` = sprintf("%s pt", pc(100 * ecart, 0)))
ajouter(tbl(as.data.frame(t24), aligne_droite = 1:3))
ajouter(legende_tableau("Calibration de la distribution prédictive, avant recalibration."))
ajouter(figure("12_calibration.png", "Calibration de la distribution prédictive",
               paste0("La courbe observée passe sous la diagonale : les intervalles sont ",
                      "trop étroits, le modèle est trop confiant.")))
ajouter(figure("12_predictive.png", "La distribution prédictive, trimestre par trimestre",
               paste0("Les bandes de quantiles suivent le réalisé, mais se resserrent ",
                      "trop en période calme : c'est la sous-couverture mesurée ",
                      "ci-dessus.")))
ajouter(sprintf(paste0("<p>La distribution est <strong>mal calibrée par excès de ",
                       "confiance</strong> : un intervalle annoncé à 90 %% ne contient le ",
                       "réalisé que %.0f %% du temps.</p>"),
                100 * calib$couverture[calib$niveau == 0.90]))
ajouter(intuition(paste0(
  "<p>La cause est identifiable, et elle est structurelle. La distribution prédictive ",
  "propage l'incertitude sur les <em>coefficients</em> et sur la <em>matrice de ",
  "covariance</em>, mais traite les <strong>hyperparamètres comme connus</strong> — ",
  "alors qu'ils sont choisis à chaque origine.</p>",
  "<p>C'est donc une <strong>borne inférieure</strong> de l'incertitude, et la ",
  "sous-couverture en est la manifestation attendue.</p>")))

ajouter("<h3>24.2 La recalibration conforme</h3>")
ajouter(definition("Ce que veut dire « conforme »",
  paste0("<p>Un intervalle de prévision classique découle d'une hypothèse sur la loi ",
         "des erreurs — le plus souvent la loi normale. Si cette hypothèse est fausse, ",
         "l'intervalle l'est aussi, et rien ne le signale.</p>",
         "<p>Une méthode <strong>conforme</strong>", ref("vovk"),
         " renverse la démarche : elle ne ",
         "suppose aucune loi, et construit l'intervalle à partir des ",
         "<em>erreurs réellement commises</em> dans le passé. Si 80 % des erreurs ",
         "passées sont inférieures à une certaine valeur, on prend cette valeur comme ",
         "demi-largeur de l'intervalle à 80 %.</p>",
         "<p>C'est particulièrement adapté ici, la section 26 montrant que les erreurs ",
         "<strong>ne suivent pas une loi normale</strong> : elles ont des queues ",
         "épaisses, que l'hypothèse gaussienne sous-estimerait.</p>")))
ajouter("<p>Plutôt que de corriger le modèle, on corrige l'intervalle : un facteur ",
        "d'élargissement est calculé sur les erreurs passées, récursivement, de façon à ",
        "ramener la couverture au niveau annoncé.</p>")
t24b <- calib2 %>% dplyr::filter(niveau == 0.90) %>%
  dplyr::transmute(Méthode = methode,
                   `Couverture` = sprintf("%.0f %%", 100 * couverture),
                   `Largeur (pt)` = nb(100 * largeur, 2),
                   `Écart` = sprintf("%s pt", pc(100 * ecart, 0)))
ajouter(tbl(as.data.frame(t24b), aligne_droite = 2:4))
ajouter(legende_tableau("Méthodes de recalibration, au niveau annoncé de 90 %."))
ajouter(figure("12b_facteur.png", "Le facteur d'élargissement au fil des origines",
               paste0("Calculé récursivement sur les erreurs passées. Il se stabilise ",
                      "après les premières origines, puis remonte après 2020 — le choc ",
                      "ayant révélé que les intervalles étaient trop étroits.")))
ajouter(figure("12b_intervalles.png", "Intervalles avant et après recalibration",
               paste0("L'élargissement est visible mais modéré : la recalibration ne ",
                      "transforme pas l'intervalle, elle le corrige.")))
ajouter("<p>La recalibration réduit l'écart de moitié environ, mais <strong>un déficit ",
        "subsiste</strong>, et il est <em>identique avant et après 2020</em> : il ne ",
        "provient donc pas du choc mais bien de l'incertitude sur les hyperparamètres.</p>")

ajouter("<h3>24.3 L'intervalle publié</h3>")
ajouter("<p>L'intervalle diffusé n'est pas celui du modèle vectoriel mais celui du ",
        "<strong>système complet</strong>, construit directement au niveau de l'agrégat ",
        "à partir de ses erreurs passées.</p>")
ajouter(definition("Pourquoi l'intervalle n'est pas propagé analytiquement",
  paste0("<p>La tentation serait d'écrire, branche par branche :</p>",
         eq(paste0("V<sub>comb</sub> = ", m("&delta;<sup>2</sup>"), " V<sub>vect</sub>",
                   " <span class='op'>+</span> (1<span class='op'>&minus;</span>",
                   m("&delta;"), ")<sup>2</sup> V<sub>pass</sub>",
                   " <span class='op'>+</span> 2", m("&delta;"),
                   "(1<span class='op'>&minus;</span>", m("&delta;"), ") ",
                   m("&rho;"), " &hellip;")),
         "<p>puis d'agréger. Mais l'agrégation exige la <strong>covariance croisée des ",
         "erreurs de passerelle entre branches</strong> — or les seize passerelles sont ",
         "des régressions <em>indépendantes</em>, estimées séparément sur des ",
         "indicateurs différents. Cette covariance n'est pas identifiée par le modèle.</p>",
         "<p>La supposer nulle sous-estimerait gravement l'intervalle : les branches se ",
         "trompent <strong>ensemble</strong> lors des ruptures, c'est ce que 2020 a ",
         "montré. L'intervalle est donc construit au niveau où cette covariance n'a plus ",
         "à être modélisée, puisqu'elle est déjà contenue dans l'erreur agrégée ",
         "observée.</p>")))
t24c <- ic_now %>% dplyr::arrange(niveau) %>%
  dplyr::left_join(ic_couv %>% dplyr::filter(echelle == ic_now$echelle[1]) %>%
                     dplyr::select(niveau, couverture), by = "niveau") %>%
  dplyr::transmute(`Niveau annoncé` = sprintf("%.0f %%", 100 * niveau),
                   `Intervalle` = sprintf("[%s ; %s]", pc(bas_pct, 2, FALSE), pc(haut_pct, 2, FALSE)),
                   `Couverture observée` = sprintf("%.0f %%", 100 * couverture))
ajouter(tbl(as.data.frame(t24c), aligne_droite = 1:3))
ajouter(legende_tableau("L'intervalle autour du chiffre publié, et sa couverture mesurée."))
ajouter(figure("12c_couverture.png", "Couverture observée contre couverture annoncée",
               paste0("Les points restent sous la diagonale aux niveaux élevés : le ",
                      "déficit subsiste après recalibration, et c'est ce que la mention ",
                      "de la couverture mesurée sert à rendre explicite.")))
ajouter(sprintf(paste0("<div class='encadre alerte'><span class='etiq'>Annoncer la ",
                       "couverture mesurée, non le niveau nominal</span>",
                       "<p>La formulation honnête est « intervalle dont la couverture ",
                       "mesurée est de %.0f %% », et non « intervalle à 80 %% ».</p>",
                       "<p>Un intervalle est une promesse ; annoncer un niveau que ",
                       "l'historique dément n'est pas une approximation, c'est une ",
                       "promesse fausse.</p></div>"), 100 * couv80))

ajouter("<h3>24.4 D'où vient l'incertitude</h3>")
ajouter(sprintf(paste0("<p>La variance prédictive se décompose en une part ",
                       "<em>paramétrique</em> — ce qu'on ignore des coefficients — et une ",
                       "part d'<em>aléa</em> — le choc futur, irréductible. La première ne ",
                       "pèse que <strong>%.0f %%</strong> du total.</p>"),
                100 * decomp$part_parametrique))
ajouter(intuition(paste0(
  "<p>Ce chiffre de cadrage explique beaucoup, et rétrospectivement. ",
  "<strong>Mieux estimer ne resserrera pas l'intervalle</strong> : l'essentiel de ",
  "l'incertitude n'est pas dans ce qu'on ignore du modèle, mais dans ce que l'avenir ",
  "n'a pas encore décidé.</p>",
  "<p>C'est la clé de lecture de la section 27 : si sept raffinements techniques ",
  "successifs n'ont rien apporté, ce n'est pas qu'ils étaient mal conçus — c'est qu'ils ",
  "s'attaquaient aux 7 %% plutôt qu'aux 93 %%.</p>")))

# ============================================================================
# 25. ROBUSTESSE
# ============================================================================
ajouter("<h2 id='s25'><span class='num'>25.</span>Robustesse des choix de modélisation</h2>")
ajouter("<p>Un résultat qui dépend d'un réglage n'est pas un résultat. Quatre choix ont ",
        "été rejoués de bout en bout.</p>")

ajouter("<h3>25.1 La fenêtre d'estimation</h3>")
t25 <- rob_f %>% dplyr::transmute(`Spécification` = specification,
                                  `Ratio médian` = nb(ratio_median, 3),
                                  `Corrélation médiane` = nb(correl_mediane, 3),
                                  `Branches sous 1` = n_branches_ok)
ajouter(tbl(as.data.frame(t25), aligne_droite = 2:4))
ajouter(legende_tableau("Fenêtre extensible, retenue, contre fenêtres glissantes."))
ajouter("<p>Le verdict est tranché, et dans un seul sens : les fenêtres glissantes ",
        "<strong>détruisent la corrélation</strong> tout en dégradant le ratio. Une ",
        "fenêtre de dix ou quinze ans jette précisément les épisodes rares dont le modèle ",
        "a besoin pour reconnaître une rupture. La fenêtre extensible est donc un choix, ",
        "pas une commodité.</p>")

ajouter("<h3>25.2 Le seuil de sélection</h3>")
t25b <- rob_s %>% dplyr::filter(perimetre == "perimetre commun") %>%
  dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Variante = variante, `Ratio médian` = nb(ratio_median, 3),
                   `Branches sous 1` = n_ok)
ajouter(tbl(as.data.frame(t25b), aligne_droite = 2:3))
ajouter(legende_tableau("Périmètre commun aux variantes, pour comparer les mêmes trimestres."))
ajouter(figure("14_selection.png", "Seuil de sélection et indicateurs retenus",
               paste0("Les variantes récursives se superposent exactement ; seule la ",
                      "sélection fixe s'en écarte.")))
ajouter("<p>Les variantes récursives sont <strong>rigoureusement identiques</strong> : le ",
        "seuil ne mord jamais. En revanche la sélection <em>fixe</em>, calculée une fois ",
        "sur tout l'échantillon, est <strong>moins bonne</strong> alors même qu'elle ",
        "bénéficie d'une antériorité illégitime.</p>")
ajouter(intuition(paste0(
  "<p>Le point mérite d'être souligné parce qu'il est réjouissant : ",
  "<strong>tricher ne paie pas ici</strong>.</p>",
  "<p>La raison est économique, non statistique. Un indicateur utile en 2014 ne l'est ",
  "plus en 2022 — une branche se restructure, une série change de qualité, une relation ",
  "se défait. Une sélection figée conserve à tort ce qui a cessé d'informer, et le ",
  "bénéfice de connaître l'avenir ne compense pas ce défaut d'adaptation.</p>")))

ajouter("<h3>25.3 La saisonnalité résiduelle</h3>")
ajouter(sprintf(paste0("<p>Une régression de la croissance sur les indicatrices de ",
                       "trimestre, branche par branche, donne un coefficient de ",
                       "détermination médian de <strong>%s</strong>, avec un maximum de ",
                       "%s. Les séries sont bien désaisonnalisées : il n'y a pas de gain ",
                       "caché de ce côté.</p>"),
                nb(stats::median(sais$R2_saison), 3), nb(max(sais$R2_saison), 3)))

ajouter("<h3>25.4 Le poids de combinaison</h3>")
t25c <- rob_d %>% dplyr::arrange(ratio_median) %>%
  dplyr::transmute(`Règle sur δ` = regle, `Ratio médian par branche` = nb(ratio_median, 3),
                   `Branches sous 1` = n_ok)
ajouter(tbl(as.data.frame(t25c), aligne_droite = 2:3))
ajouter(legende_tableau(paste0(
  "Ratio médian PAR BRANCHE. C'est l'échelle à laquelle les règles sont ",
  "indiscernables ; la question se tranche sur l'agrégat, section 14.")))
ajouter(sprintf(paste0("<p>L'amplitude totale y est de <strong>%s point de ratio</strong> ",
                       "entre la meilleure règle et la pire. Rien ne s'y joue — et c'est ",
                       "la raison pour laquelle la démonstration de la section 14 a dû ",
                       "être conduite au niveau de l'agrégat.</p>"),
                nb(max(rob_d$ratio_median) - min(rob_d$ratio_median), 3)))

# ============================================================================
# 26. DIAGNOSTICS
# ============================================================================
ajouter("<h2 id='s26'><span class='num'>26.</span>Diagnostics du modèle</h2>")
t26 <- resid %>%
  dplyr::transmute(Test = test,
                   `Statistique` = ifelse(is.na(statistique), "—", nb(statistique, 3)),
                   `p` = ifelse(is.na(p_value), "—", nb(p_value, 4)),
                   Lecture = lecture)
ajouter("<p>Trois tests sont appliqués à l'erreur du système. Chacun vérifie une ",
        "propriété différente, et chacun répond à la question : <em>reste-t-il dans ",
        "l'erreur quelque chose que le modèle aurait dû capter ?</em></p>")
ajouter(definition("Ce que teste chacun",
  paste0("<p><strong>Ljung-Box sur les résidus</strong>", ref("ljung"),
         " teste l'absence ",
         "d'<em>autocorrélation</em> : l'erreur d'un trimestre permet-elle de prédire ",
         "celle du suivant ? Si oui, il reste de la structure exploitable, donc une ",
         "amélioration possible. Une probabilité critique élevée est ici une bonne ",
         "nouvelle — elle signifie qu'on ne détecte rien à récupérer.</p>",
         "<p><strong>Ljung-Box sur les résidus au carré</strong> teste ",
         "l'<em>hétéroscédasticité conditionnelle</em> : les périodes de forte erreur ",
         "se regroupent-elles dans le temps ? C'est l'effet dit ARCH, bien connu en ",
         "finance. S'il était présent, l'intervalle devrait être plus large en période ",
         "agitée et plus étroit en période calme.</p>",
         "<p><strong>Shapiro-Wilk</strong>", ref("shapiro"),
         " teste la <em>normalité</em>. Ce test-ci n'a ",
         "pas vocation à être passé avec succès : son rejet est une information sur la ",
         "nature de l'économie, pas un défaut du modèle.</p>",
         "<p>Dans les trois cas, l'hypothèse nulle est la propriété « souhaitable » — ",
         "pas d'autocorrélation, pas d'effet ARCH, normalité — et une petite ",
         "probabilité critique conduit à la rejeter.</p>")))
ajouter(tbl(as.data.frame(t26), aligne_droite = 2:3))
ajouter(legende_tableau("Diagnostics sur l'erreur de nowcast agrégée."))
ajouter("<p>Les deux premiers tests sont de bonnes nouvelles. L'absence ",
        "d'<strong>autocorrélation résiduelle</strong> signifie que le modèle n'a pas ",
        "laissé de structure exploitable dans ses erreurs — il n'y a pas de gain facile ",
        "à récupérer en modélisant le résidu. L'absence d'<strong>hétéroscédasticité ",
        "conditionnelle</strong> indique que la volatilité de l'erreur ne se regroupe pas ",
        "en périodes.</p>")
ajouter("<div class='encadre'><span class='etiq'>La non-normalité, elle, est structurante</span>",
        "<p>Le test de normalité rejette massivement. Les résidus ont des ",
        "<strong>queues épaisses</strong> : beaucoup de trimestres presque parfaits, ",
        "quelques-uns très mauvais.</p>",
        "<p>C'est exactement la raison pour laquelle les intervalles de la section 24 ",
        "sont <strong>conformes</strong> et non gaussiens. Un intervalle ",
        m("&mu; &plusmn; 1,96&sigma;"), " serait mal calibré <em>par construction</em> ",
        "sur une telle distribution : trop large en régime calme, trop étroit lors des ",
        "ruptures — c'est-à-dire faux là où il compte.</p>",
        "<p>C'est aussi une propriété économique, non un défaut : une économie soumise à ",
        "des chocs climatiques et sanitaires <em>a</em> des queues épaisses. Un modèle ",
        "dont les résidus seraient gaussiens serait, lui, suspect.</p></div>")
ajouter(figure("15_erreurs.png", "L'erreur de nowcast, trimestre par trimestre",
               paste0("Beaucoup de trimestres proches de zéro, quelques-uns très ",
                      "au-dessus : la signature des queues épaisses.")))

# ============================================================================
# 27. LES ECHECS
# ============================================================================
ajouter("<h2 id='s27'><span class='num'>27.</span>Ce qui a été essayé sans succès</h2>")
ajouter("<p>Un rapport qui ne présente que ce qui a marché est un rapport incomplet. ",
        "Sept raffinements ont été construits, mesurés sur le protocole complet, puis ",
        "écartés.</p>")
ech <- data.frame(
  Piste = c("Sélection jointe des hyperparamètres sur la grille complète",
            "Pondération géométrique des indicateurs",
            "Régression pénalisée de type ridge, pénalité choisie par validation croisée généralisée",
            "Comblement des trous internes par lissage de Kalman",
            "Poids de combinaison dépendant de l'état conjoncturel",
            "Seuil de corrélation renforcé pour les indicateurs trimestriels",
            "Sélection sur la corrélation partielle plutôt que marginale"),
  `Ce qu'on en attendait` = c(
    "capturer les interactions entre resserrement, retards et décroissance",
    "donner plus de poids aux indicateurs les plus corrélés",
    "exploiter davantage d'indicateurs sans exploser la variance",
    "récupérer l'information des séries trouées",
    "basculer vers la passerelle quand elle annonce une rupture",
    "conserver les meilleurs indicateurs trimestriels plutôt que tous les exclure",
    "écarter les indicateurs redondants avec ceux déjà retenus"),
  `Résultat` = c("aucun gain sur la sélection séquentielle",
                 "aucun gain",
                 "dégradation ; la pénalité choisie sature la borne supérieure",
                 "aucun gain mesurable",
                 "dégradation sur toutes les périodes, y compris 2020",
                 "plat jusqu'au seuil le plus élevé, puis chute à l'exclusion",
                 "dégradation ; les indicateurs ne sont donc pas redondants"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(ech, aligne_droite = integer(0)))
ajouter(legende_tableau("Les sept pistes évaluées puis écartées."))

ajouter("<h3>27.1 Les six pistes d'élargissement de la passerelle</h3>")
ajouter("<p>Cinq d'entre elles visaient le même défaut : la passerelle de base ne ",
        "produisait de prévision que pour une fraction des couples branche-trimestre. ",
        "Elles ont été évaluées ensemble, sur le même protocole.</p>")
t27 <- varia %>% dplyr::filter(cible == "combinee") %>%
  dplyr::arrange(ratio_median) %>%
  dplyr::transmute(Variante = specification,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Branches sous 1` = n_branches_ok,
                   `Corrélation` = nb(correl_mediane, 3),
                   `Origines produites` = sprintf("%s %%", nb(100 * taux_production, 0)),
                   `Indicateurs retenus` = n_retenus_median)
ajouter(tbl(as.data.frame(t27), aligne_droite = 2:6))
ajouter(legende_tableau(paste0(
  "Les variantes de la passerelle, prévision combinée. La colonne « origines ",
  "produites » mesure le taux de couverture, la première colonne la qualité.")))
ajouter(figure("04b_ratio_par_variante.png", "Qualité des variantes",
               paste0("Aucune variante ne domine la référence sur les deux dimensions à ",
                      "la fois : celles qui produisent le plus sont rarement les plus ",
                      "précises.")))
ajouter(figure("04b_production_vs_qualite.png", "Production contre qualité",
               paste0("L'arbitrage est lisible : gagner en couverture se paie en ",
                      "précision. C'est le résultat central de cette série de tests.")))
ajouter(figure("04b_taux_par_branche.png", "Taux de production par branche et par variante",
               paste0("Le déficit de couverture de la version de base ne se répartit pas ",
                      "uniformément : il frappe surtout les branches dont les séries sont ",
                      "les plus trouées.")))
ajouter(intuition(paste0(
  "<p>L'enseignement n'est pas qu'aucune piste ne marche, mais qu'il existe un ",
  "<strong>arbitrage</strong> entre le nombre de prévisions produites et leur ",
  "qualité.</p>",
  "<p>Élargir le panneau en acceptant des séries plus trouées ou moins corrélées ",
  "permet effectivement de couvrir davantage de trimestres — mais les prévisions ",
  "ajoutées sont les moins fiables, et elles dégradent la moyenne. La variante retenue ",
  "est celle qui privilégie la disponibilité conjointe <em>sans</em> relâcher le critère ",
  "de qualité.</p>")))

ajouter("<h3>27.2 Le comblement des trous</h3>")
t27b <- kalcomp %>% dplyr::arrange(ratio_median) %>%
  dplyr::transmute(`Spécification` = specification,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Branches sous 1` = n_branches_ok,
                   `Corrélation` = nb(correl, 3),
                   `Origines produites` = sprintf("%s %%", nb(100 * taux, 0)))
ajouter(tbl(as.data.frame(t27b), aligne_droite = 2:5))
ajouter(legende_tableau("Effet du comblement des trous sur la prévision finale."))
ajouter(figure("04c_effet_kalman.png", "Ce que change le comblement",
               paste0("L'effet sur la prévision est proche de zéro, alors même que la ",
                      "reconstitution des trous est de bonne qualité (section 13). Le ",
                      "gain se dilue en aval.")))

ajouter("<h3>27.3 Le poids dépendant de l'état</h3>")
ajouter("<p>L'idée était séduisante et reposait sur un diagnostic précis : le poids ",
        "étant estimé sur l'erreur quadratique <em>passée</em>, il est dominé par les ",
        "trimestres calmes où la passerelle est mauvaise. Il sous-pondérerait donc la ",
        "passerelle précisément au moment où elle est sur le point d'avoir raison. La ",
        "correction testée bascule vers la passerelle quand celle-ci annonce un mouvement ",
        "de grande amplitude :</p>")
ajouter(eq(paste0(m("&delta;<sub>T</sub>"), " = 0 si ", m("z<sub>T</sub>"),
                  " <span class='op'>&ge;</span> ", m("z<sub>0</sub>"),
                  " ,&nbsp;&nbsp; ", m("&delta;<sub>base</sub>"), " sinon ,",
                  "&nbsp;&nbsp;&nbsp; ", m("z<sub>T</sub>"), " = ",
                  "<span class='op'>|</span>", m("g&#770;<sup>pass</sup>"),
                  "<span class='op'>|</span> / ", m("&sigma;<sub>t&lt;T</sub>"))))
t27c <- etat %>% dplyr::filter(grepl("^1\\+4", variante), modele == "combinee") %>%
  dplyr::arrange(periode, z0) %>%
  dplyr::transmute(`Période` = periode,
                   `Seuil` = ifelse(is.finite(z0), nb(z0, 1), "∞"),
                   `Ratio médian` = nb(ratio_median, 3),
                   `Bascules` = bascules)
if (nrow(t27c) > 0) {
  ajouter(tbl(as.data.frame(t27c), aligne_droite = 2:4))
  ajouter(legende_tableau(paste0(
    "Sensibilité au seuil de basculement. Un seuil infini signifie : ne jamais ",
    "basculer.")))
}
ajouter(figure("04d_signal_etat.png", "Le signal de basculement",
               paste0("Les déclenchements ne se concentrent pas sur 2020 : la majorité ",
                      "sont de fausses alertes, et elles coûtent plus que les bonnes ne ",
                      "rapportent.")))
ajouter(figure("04d_mae_par_periode.png", "L'erreur par période",
               paste0("La valeur de la passerelle est concentrée sur la rupture. Une ",
                      "moyenne sur toutes les origines écrase exactement ce que le ",
                      "système sait faire — c'est le défaut de présentation que le ",
                      "découpage par régime de la section 22 corrige.")))
ajouter("<p><strong>La règle ne marche pas</strong>, et la sensibilité le dit sans ",
        "ambiguïté : ne jamais basculer est le meilleur réglage sur toutes les périodes, ",
        "<em>y compris 2020</em>.</p>")

ajouter("<h3>27.4 La sélection jointe des hyperparamètres</h3>")
ajouter("<p>La sélection séquentielle choisit le resserrement, puis la règle de ",
        "détection des chocs, puis le levier de spécification. Une sélection ",
        "<strong>jointe</strong> balaie la grille complète des combinaisons, ce qui ",
        "permettrait en principe de capter les interactions entre ces trois choix.</p>")
t27d <- conj %>% dplyr::arrange(ratio_median) %>% head(6) %>%
  dplyr::transmute(Mode = mode, `Règle de choc` = regle, Levier = levier,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Branches sous 1` = n_branches_ok)
ajouter(tbl(as.data.frame(t27d), aligne_droite = 4:5))
ajouter(legende_tableau(sprintf(
  "Six meilleures combinaisons sur les %d explorées par la sélection jointe.",
  nrow(conj))))
ajouter(figure("03c_classement_combinaisons.png", "Le classement des combinaisons",
               paste0("La distribution est resserrée : l'écart entre la meilleure ",
                      "combinaison et la médiane est du même ordre que le bruit ",
                      "d'échantillonnage.")))
ajouter(figure("03c_interaction_levier_mode.png", "Y a-t-il des interactions ?",
               paste0("Les courbes sont à peu près parallèles : l'effet d'un levier ne ",
                      "dépend pas du mode retenu. C'est l'absence d'interaction qui ",
                      "explique que la sélection jointe n'apporte rien.")))
t27e <- conjval %>%
  dplyr::transmute(`Procédure` = procedure, `Phase` = phase,
                   `Ratio médian` = nb(ratio_median, 3),
                   `Branches sous 1` = n_branches_ok,
                   `Corrélation médiane` = nb(correl_mediane, 3))
ajouter(tbl(as.data.frame(t27e), aligne_droite = 3:5))
ajouter(legende_tableau(paste0(
  "Validation sur période réservée : chaque procédure est réglée sur la phase ",
  "d'apprentissage, puis évaluée sur une période qu'elle n'a pas vue.")))
ajouter(figure("03c_validation_test.png", "L'avantage survit-il hors échantillon ?",
               paste0("L'avantage de la sélection jointe apparaît en phase ",
                      "d'apprentissage et disparaît en phase de test. C'est la ",
                      "définition même du surajustement.")))
ajouter(intuition(paste0(
  "<p>Le résultat se comprend une fois la figure lue : <strong>s'il n'y a pas ",
  "d'interaction, optimiser conjointement ne peut pas battre optimiser ",
  "séquentiellement</strong> — cela ne fait qu'ajouter des degrés de liberté, donc du ",
  "surajustement.</p>",
  "<p>La sélection jointe explore par ailleurs bien plus de combinaisons, ce qui ",
  "augmente mécaniquement le risque de retenir un optimum d'échantillon. Une validation ",
  "sur période réservée l'a confirmé : l'avantage apparent en échantillon ne survit pas ",
  "hors échantillon.</p>")))
ajouter("<p>Deux hypothèses ont par ailleurs été réfutées : une saisonnalité résiduelle ",
        "exploitable — le coefficient de détermination médian est de 0,013 — et une ",
        "compensation des erreurs de branche lors de l'agrégation.</p>")
ajouter("<h3>27.1 Une erreur trouvée par un symptôme théoriquement impossible</h3>")
ajouter("<p>La régression pénalisée mérite un mot, parce que son échec initial a révélé ",
        "un défaut de calcul. La première version donnait « pénalisée avec terme ",
        "autorégressif » <em>pire</em> que « pénalisée sans ». C'est impossible : ajouter ",
        "un régresseur non pénalisé et utile ne peut pas dégrader à ce point.</p>")
ajouter("<p>La cause était dans le critère de sélection de la pénalité",
        ref("golub"), ". Le nombre effectif de paramètres y est la trace de la matrice ",
        "chapeau :</p>")
ajouter(eq(paste0("GCV(", m("&lambda;"), ") = ",
                  "<span class='fr'><span class='hi'>",
                  m("n"), "<span class='op'>&times;</span>RSS(", m("&lambda;"),
                  ")</span><span class='lo'>[ ", m("n"), " <span class='op'>&minus;</span> ",
                  "tr(", m("H<sub>&lambda;</sub>"), ") ]<sup>2</sup></span></span>")))
ajouter("<p>et cette trace ne comptait pas les paramètres <strong>non pénalisés</strong> ",
        "— la constante et le terme autorégressif. Deux degrés de liberté manquaient, la ",
        "pénalité était donc choisie trop faible, et le modèle surajustait.</p>")
ajouter(intuition(paste0(
  "<p>La leçon n'est pas technique mais méthodologique : <strong>c'est un résultat ",
  "théoriquement impossible qui a révélé le défaut</strong>, non une relecture du ",
  "code.</p>",
  "<p>Savoir ce qui <em>ne peut pas</em> arriver est un instrument de contrôle plus ",
  "puissant qu'une vérification ligne à ligne. Encore faut-il regarder les résultats ",
  "avec cette question en tête plutôt que de les enregistrer.</p>")))
ajouter("<h3>27.2 Ce que ces échecs disent ensemble</h3>")
ajouter("<p>Le faisceau est cohérent, et la décomposition de variance de la section 24 ",
        "en donne la raison : <strong>le plafond est dans les données</strong>. Aucun ",
        "raffinement d'estimation ne crée de l'information qui n'y est pas.</p>")
ajouter("<p>Ce qui a fonctionné, ce ne sont pas les modèles plus savants, mais deux ",
        "changements de <em>cadrage</em> : évaluer <strong>au bon moment du ",
        "trimestre</strong>, et <strong>agréger</strong> plutôt que moyenner des ",
        "branches. Les deux ne coûtent aucun paramètre supplémentaire.</p>")

# ============================================================================
# 28. NOWCAST COURANT
# ============================================================================
ajouter("<h2 id='s28'><span class='num'>28.</span>Le nowcast du trimestre en cours</h2>")
mois_vus <- match(nc_ag$scenario[1], c("M0", "M1", "M2", "M3")) - 1L
ajouter(sprintf(paste0("<p>Cible : <strong>%s</strong>. <strong>%s des trois mois</strong> ",
                       "du trimestre cible sont publiés, soit le scénario <strong>%s</strong> ",
                       "au sens de la section 17. Le scénario n'est pas choisi : il est ",
                       "<em>constaté</em>.</p>"),
                cible_lab,
                c("Aucun", "Un", "Deux", "Trois")[mois_vus + 1L],
                nc_ag$scenario[1]))
ajouter(sprintf(paste0("<p style='text-align:center;font-size:1.5em;margin:1.2em 0'>",
                       "<strong>Valeur ajoutée totale, %s : %s</strong></p>"),
                cible_lab, pc(now_pct)))
ajouter(sprintf(paste0("<p style='text-align:center'>intervalle <strong>[%s ; %s] %%</strong>",
                       ", couverture mesurée %s %%</p>"),
                pc(ic80$bas_pct, 2, FALSE), pc(ic80$haut_pct, 2, FALSE),
                nb(100 * couv80)))
t28 <- nc_br %>% dplyr::arrange(dplyr::desc(nowcast_pct)) %>%
  dplyr::transmute(Branche = branche, `Poids (%)` = nb(100 * w, 1),
                   `Vectoriel (%)` = nb(bvar_pct, 2),
                   `Passerelle (%)` = ifelse(is.na(bridge_pct), "—", nb(bridge_pct, 2)),
                   `Nowcast (%)` = nb(nowcast_pct, 2),
                   Source = source)
ajouter(tbl(as.data.frame(t28), aligne_droite = 2:5))
ajouter(legende_tableau("Décomposition du chiffre publié, branche par branche."))
ajouter(definition("Comment lire ce chiffre",
  paste0("<p>C'est un taux <strong>trimestriel</strong>, d'un trimestre au suivant, et ",
         "non un glissement annuel. Il porte sur la <strong>valeur ajoutée totale</strong> ",
         "en volume, non sur le produit intérieur brut.</p>",
         sprintf(paste0("<p>Sur les %d trimestres du backtest, la croissance ",
                        "trimestrielle agrégée a une médiane de %s %% et un écart-type ",
                        "de %s points. Ce nowcast se situe donc dans le haut de la ",
                        "distribution : c'est un trimestre fort, pas un chiffre ",
                        "neutre.</p>"),
                 n_trim, nb(100 * stats::median(ag$reel_niveau), 2),
                 nb(100 * stats::sd(ag$reel_niveau), 2)),
         "<p>Il est produit par la même chaîne que celle évaluée dans tout ce qui ",
         "précède : ce qui a été mesuré est bien ce qui est diffusé.</p>")))
ajouter(figure("12c_intervalle.png", "Le chiffre publié et son intervalle",
               paste0("L'intervalle est construit sur les erreurs passées du système ",
                      "complet, calculées récursivement, et non sur une hypothèse ",
                      "gaussienne.")))

# ============================================================================
# 29. LECTURE ECONOMIQUE
# ============================================================================
ajouter("<h2 id='s29'><span class='num'>29.</span>Lecture économique</h2>")
ajouter("<p>Au-delà des indicateurs de performance, le travail dit quelque chose sur ",
        "l'économie qu'il modélise.</p>")

ajouter("<h3>29.1 Une croissance faiblement persistante</h3>")
ajouter("<p>Le résultat le plus net de la section 19 est que <strong>les modèles ",
        "autorégressifs ont une corrélation négative</strong> avec le réalisé. La ",
        "croissance trimestrielle marocaine n'est pas persistante : un bon trimestre ne ",
        "prédit pas un bon trimestre suivant, il tendrait plutôt à annoncer un retour à ",
        "la moyenne.</p>")
ajouter("<p>L'explication est en partie mécanique et tient à l'agriculture. Une bonne ",
        "campagne agricole crée une base élevée qui, l'année suivante, produit un ",
        "glissement négatif à activité inchangée. Cette alternance imprime son rythme à ",
        "l'agrégat national, et elle est <em>anti-persistante</em> par construction.</p>")
ajouter(intuition(paste0(
  "<p>Conséquence pratique, et elle dépasse ce travail : <strong>extrapoler la ",
  "tendance récente est, au Maroc, une stratégie activement mauvaise</strong> — pas ",
  "seulement peu informative.</p>",
  "<p>C'est ce qui justifie économiquement, et pas seulement statistiquement, de ",
  "recourir à des indicateurs contemporains plutôt qu'au passé de la série.</p>")))

ajouter("<h3>29.2 Le poids des chocs d'offre</h3>")
ajouter("<p>La section 22 montre que l'apport du système se concentre sur les ruptures : ",
        "crise sanitaire, chocs agricoles. Ce sont, dans les deux cas, des ",
        "<strong>chocs d'offre</strong> — une capacité de production empêchée ou une ",
        "récolte manquée — et non des retournements de demande.</p>")
ajouter("<p>Ce résultat conforte le choix d'architecture de la section 2. Une économie ",
        "dont la variance est dominée par des chocs d'offre se modélise naturellement ",
        "par branche d'activité : c'est au niveau de la branche que le choc frappe, et ",
        "c'est là que les indicateurs le captent.</p>")

ajouter("<h3>29.3 Ce que le système ne voit pas, et pourquoi</h3>")
ajouter("<p>Le gain quasi nul sur les ralentissements graduels n'est pas un défaut ",
        "d'estimation. Il tient à la nature de l'information collectée : les indicateurs ",
        "disponibles sont <strong>coïncidents</strong>.</p>")
ajouter("<p>Aucun des indicateurs mobilisés n'est un indicateur avancé au sens propre — ",
        "il n'existe pas ici d'enquête de confiance longue, de carnets de commandes, ",
        "d'indicateurs financiers à pouvoir prédictif établi. On mesure l'activité ",
        "pendant qu'elle se déroule, pas les intentions qui la précèdent.</p>")
ajouter("<p>Un système de nowcasting reste donc, par construction, un instrument de ",
        "<em>constat rapide</em> et non d'<em>anticipation</em>. C'est utile — c'est même ",
        "ce qu'on lui demande — mais il faut le dire pour ne pas laisser croire l'inverse.</p>")

ajouter("<h3>29.4 La concentration du risque</h3>")
ajouter("<p>La section 21 montre que trois branches font la moitié de l'erreur agrégée, ",
        "et que l'agriculture domine la variance de l'ensemble. La qualité du nowcast ",
        "dépend donc étroitement de la qualité de l'information sur un petit nombre de ",
        "branches.</p>")
ajouter("<p>Or c'est précisément l'agriculture qui est le moins bien couverte, avec une ",
        "poignée d'indicateurs. <strong>Le principal gisement d'amélioration n'est pas ",
        "méthodologique, il est statistique</strong> : il tient à ce que l'on mesure, et ",
        "à quelle fréquence.</p>")

# ============================================================================
# 30. LIMITES
# ============================================================================
ajouter("<h2 id='s30'><span class='num'>30.</span>Limites et prolongements</h2>")
ajouter("<p>Les limites qui suivent sont ordonnées par gravité décroissante, et aucune ",
        "n'est présentée comme mineure.</p>")

ajouter("<h3>30.1 Les données ne sont pas millésimées</h3>")
ajouter("<p>Les règles d'antériorité portent sur les <strong>dates</strong>, jamais sur ",
        "les <strong>millésimes</strong>. Les prévisions sont évaluées contre une valeur ",
        "ajoutée <em>révisée</em>, que le modèle n'aurait pas eue en temps réel.</p>")
ajouter("<p>Le biais qui en résulte n'est pas de signe évident. Les révisions corrigent ",
        "des erreurs de collecte, ce qui rendrait la cible plus facile ; mais elles ",
        "intègrent aussi de l'information postérieure, ce qui la rendrait plus ",
        "difficile. On ne peut pas trancher sans les données correspondantes, et c'est ",
        "précisément le problème.</p>")

ajouter("<h3>30.2 Les délais de publication sont ignorés</h3>")
ajouter("<p>La convention retenue suppose tout indicateur du mois ", m("m"),
        " connu le dernier jour de ", m("m"), ". C'est <strong>optimiste</strong> : les ",
        "publications réelles interviennent avec des délais de quelques semaines à ",
        "quelques mois, variables d'une source à l'autre.</p>")
ajouter(sprintf(paste0("<p>Un test de sensibilité chiffre le délai le plus dommageable, ",
                       "celui de la valeur ajoutée elle-même : privé du trimestre ",
                       "précédent, le modèle vectoriel passe d'un ratio de %s à %s, et sa ",
                       "corrélation s'effondre de %s à %s.</p>"),
                nb(retard$ratio[1], 3), nb(retard$ratio[2], 3),
                nb(retard$correlation[1], 2), nb(retard$correlation[2], 2)))
ajouter(figure("11_retard_va.png", "Effet du délai de publication de la cible",
               paste0("La perte n'est pas marginale : elle fait basculer le modèle ",
                      "au-dessus de 1, c'est-à-dire en deçà de ce que donnerait la ",
                      "moyenne historique.")))
t30 <- calend %>% dplyr::arrange(retard_mois) %>%
  dplyr::transmute(`Retard supposé` = sprintf("%s mois", nb(retard_mois)),
                   `Position effective` = position,
                   `Scénario effectif` = scenario_effectif,
                   `Ratio combinaison` = nb(ratio_combinaison, 3),
                   `Ratio modèle vectoriel` = nb(ratio_bvar, 3))
ajouter(tbl(as.data.frame(t30), aligne_droite = 4:5))
ajouter(legende_tableau(paste0(
  "Effet d'un retard uniforme de publication des indicateurs : un retard de k mois ",
  "ramène l'ensemble d'information au scénario correspondant.")))
ajouter(figure("11_calendrier.png", "Effet du retard de publication des indicateurs",
               paste0("Un mois de retard suffit à annuler une part notable du gain. Le ",
                      "système est donc sensible au calendrier, et pas seulement à la ",
                      "qualité des séries.")))
ajouter("<p>Corriger ce point ne demande aucun développement méthodologique : il suffit ",
        "du calendrier de diffusion des séries. C'est le prolongement <strong>le plus ",
        "rentable</strong> du travail.</p>")

ajouter("<h3>30.3 L'agrégat n'est pas comparé au total officiel</h3>")
ajouter("<p>L'agrégat est confronté à l'indice reconstruit par la même formule, non au ",
        "total publié. La comparaison interne est donc <em>licite</em> — le même indice ",
        "sert de prévision et de réalisation — mais elle ne dit rien de l'écart au ",
        "chiffre que le lecteur a en tête.</p>")
ajouter("<p>Cet écart a été borné indirectement et paraît faible (section 16.3), mais il ",
        "reste <strong>supposé plutôt que mesuré</strong>.</p>")

ajouter("<h3>30.4 Un backtest court, et une seule crise pleinement couverte</h3>")
ajouter(sprintf(paste0("<p>%d trimestres, dont quatre portent l'essentiel du résultat. ",
                       "C'est peu pour établir quoi que ce soit, et la section 20 en ",
                       "tire les conséquences : <strong>aucun écart n'atteint la ",
                       "significativité</strong>.</p>"), n_trim))
ajouter("<p>L'extension en amont a permis d'ajouter un second épisode de rupture, mais ",
        "sur quatre branches seulement — celles dont la profondeur historique le ",
        "permettait.</p>")

ajouter("<h3>30.5 Les tests multiples ne sont pas corrigés</h3>")
ajouter("<p>De nombreuses spécifications ont été comparées au fil du travail. Les ",
        "probabilités critiques rapportées ne tiennent pas compte de cette multiplicité : ",
        "à force de tester, on finit par trouver.</p>")
ajouter("<p>L'atténuation tient à ce que <strong>la plupart des tests ont conclu à ",
        "l'absence d'effet</strong> — les sept pistes de la section 27 — ce qui est le ",
        "contraire d'une sélection opportuniste. Mais la réserve demeure, et une ",
        "correction formelle serait plus rigoureuse.</p>")

ajouter("<h3>30.6 Le vivier repose sur un tri extérieur au système</h3>")
ajouter("<p>Le classeur source a été constitué en amont, sur des critères économiques. ",
        "Le système en hérite sans pouvoir le reproduire : si une série pertinente n'y ",
        "figure pas, rien dans la chaîne ne la fera apparaître.</p>")
ajouter("<p>Ce n'est pas une antériorité — un jugement de pertinence économique ne ",
        "regarde pas la valeur ajoutée réalisée, et il aurait été le même à n'importe ",
        "quelle date. C'est une <strong>dépendance</strong> : la qualité du vivier borne ",
        "celle du système, et la section 29.4 montre où cela se paie, l'agriculture ",
        "dominant la variance de l'agrégat tout en ne comptant que cinq indicateurs.</p>")
ajouter("<p>Les deux exclusions opérées par le système lui-même, série constante et ",
        "doublon strict, portent sur 0,7 % des séries et ne font intervenir aucune ",
        "corrélation avec la cible. Toute la sélection statistique, elle, est récursive ",
        "(section 11), et la section 25.2 montre qu'une sélection <em>fixe</em> ferait ",
        "moins bien alors même qu'elle bénéficierait d'une antériorité.</p>")

ajouter("<h3>30.7 Le pouvoir prédictif reste modeste hors rupture</h3>")
ajouter("<p>Hors 2020, le ratio frôle 1 et la corrélation est faible. Le système ",
        "n'apporte presque rien en régime courant, et rien du tout sur les ",
        "ralentissements graduels.</p>")
ajouter("<p>Ce n'est pas une faiblesse de mise en œuvre : c'est ce que permet ",
        "l'information disponible, et la décomposition de variance de la section 24 ",
        "indique que l'essentiel de l'incertitude est irréductible.</p>")

ajouter("<h3>30.8 Ce qu'il faudrait pour aller plus loin</h3>")
ajouter("<p>Par ordre d'utilité décroissante, et aucun de ces points ne relève du ",
        "calcul :</p>")
ajouter("<ol>",
        "<li>le <strong>calendrier de diffusion</strong> des indicateurs et des comptes, ",
        "qui lèverait la limite 30.2 — la plus grave et la plus facile à corriger ;</li>",
        "<li>les <strong>millésimes successifs</strong> des comptes trimestriels, qui ",
        "lèveraient la limite 30.1 ;</li>",
        "<li>la <strong>série officielle de valeur ajoutée totale</strong> en volume ",
        "chaîné, qui lèverait la limite 30.3 et permettrait d'évaluer contre la grandeur ",
        "publiée ;</li>",
        "<li>des <strong>indicateurs sur les branches non couvertes</strong>, et surtout ",
        "sur l'agriculture — c'est le gisement identifié à la section 29.4 ;</li>",
        "<li>des <strong>indicateurs avancés</strong>, enquêtes de conjoncture ou ",
        "carnets de commandes, seuls capables de faire progresser le système sur les ",
        "ralentissements.</li>",
        "</ol>")

ajouter("<h3>30.9 Conclusion</h3>")
ajouter(sprintf(paste0("<p>Le système estime la croissance trimestrielle de la valeur ",
                       "ajoutée totale avec une erreur valant <strong>%.0f %% de ce ",
                       "qu'on commettrait sans modèle</strong>, et une corrélation de %s ",
                       "avec le réalisé. Il bat les six étalons testés, dans les deux ",
                       "sous-périodes, sans qu'aucun de ces écarts n'atteigne la ",
                       "significativité statistique sur un échantillon de cette ",
                       "taille.</p>"),
                100 * ratio_ag, nb(correl_ag, 2)))
ajouter("<p>Sa valeur se concentre sur les ruptures, et c'est là qu'on attend un ",
        "nowcast. Elle est faible en régime courant, et nulle sur les inflexions ",
        "graduelles — parce que les indicateurs disponibles sont coïncidents, non ",
        "avancés.</p>")
ajouter("<p>Le principal enseignement méthodologique n'est pas dans le classement des ",
        "modèles. Il est que <strong>les gains sont venus du cadrage, non de la ",
        "sophistication</strong> : évaluer au bon moment du trimestre, agréger plutôt ",
        "que moyenner, fixer un paramètre plutôt que l'estimer. Sept raffinements ",
        "techniques n'ont rien apporté, et la décomposition de l'incertitude explique ",
        "pourquoi — l'essentiel de ce qui reste à expliquer n'est pas dans le modèle, ",
        "mais dans l'avenir.</p>")

ajouter('</div>')   # fin du bloc a deux colonnes

# ============================================================================
# 31. GLOSSAIRE  (pleine largeur)
# ============================================================================
ajouter("<h2 id='s31'><span class='num'>31.</span>Glossaire</h2>")
ajouter("<p>Les termes sont donnés dans l'ordre alphabétique, avec la section où ils ",
        "sont employés.</p>")
glo <- data.frame(
  Terme = c(
    "Agrégation (de fréquence)", "Autocorrélation", "Backtest",
    "Bayésien (modèle)", "Biais", "Bootstrap par blocs", "Bord irrégulier",
    "Conforme (intervalle)", "Conjuguée (loi)", "Corrélation partielle",
    "Couverture", "Degrés de liberté", "Différence logarithmique",
    "Écart absolu médian", "Effet de Jensen", "Ensemble d'information",
    "Équation passerelle", "Étalon", "Filtre de Kalman",
    "Hétéroscédasticité", "Hyperparamètre", "Indicateur coïncident",
    "Laspeyres (indice de)", "Lisseur de Kalman", "M0, M1, M2, M3",
    "Origine", "Périmètre commun", "Point de rupture", "Prior de Minnesota",
    "Probabilité critique", "Racine unitaire", "Ragged edge", "Ratio",
    "Récursif (protocole)", "RMSFE", "Solution de coin",
    "Stationnarité", "Surajustement", "Volume (en)", "Vraisemblance marginale"),
  Définition = c(
    "passage d'une série mensuelle à une série trimestrielle : somme pour un flux, moyenne pour un indice, dernier mois pour un encours (section 10)",
    "lien entre une série et ses propres valeurs passées ; une erreur autocorrélée signale une structure non exploitée (section 26)",
    "réexécution du système à toutes les origines passées, pour comparer ce qu'il aurait dit à ce qui s'est produit (section 18)",
    "modèle où les coefficients sont traités comme des variables aléatoires, dont on met à jour la distribution (section 7)",
    "erreur moyenne avec son signe ; non nul, il révèle une tendance systématique à sur- ou sous-estimer (section 18)",
    "rééchantillonnage par séquences consécutives, qui préserve la structure temporelle de la série (section 20)",
    "situation où les indicateurs ne sont pas tous observés jusqu'au même mois (section 13)",
    "intervalle construit sur les erreurs réellement observées, sans hypothèse de loi (section 24)",
    "loi a priori telle que la loi a posteriori appartient à la même famille, ce qui donne un résultat en forme close (section 7)",
    "corrélation entre deux variables une fois retiré l'effet d'un troisième ensemble de variables (section 17)",
    "proportion des cas où le réalisé tombe effectivement dans l'intervalle annoncé (section 24)",
    "nombre d'observations moins nombre de paramètres estimés ; ce qui reste pour estimer honnêtement (section 7)",
    "log(x_t) − log(x_{t−1}), approximation du taux de croissance, additive dans le temps et symétrique (section 4)",
    "médiane des écarts absolus à la médiane ; mesure de dispersion résistante aux valeurs extrêmes (section 9)",
    "écart entre la moyenne des images par une fonction convexe et l'image de la moyenne (section 16)",
    "ce qui est disponible au moment où l'estimation est produite, noté ℐ(T) (section 3)",
    "régression reliant la croissance d'une branche à ses indicateurs infra-trimestriels (section 12)",
    "modèle simple servant de plancher de comparaison (section 19)",
    "algorithme calculant l'état d'un système à partir du seul passé (section 13)",
    "variance non constante ; conditionnelle, elle se regroupe en périodes (section 26)",
    "paramètre qui gouverne d'autres paramètres — ici le resserrement du prior (section 8)",
    "indicateur qui enregistre l'activité pendant qu'elle se produit, par opposition à un indicateur avancé (section 29)",
    "indice de volume valorisant les quantités courantes aux prix d'une période de base (section 16)",
    "algorithme calculant l'état d'un système à partir de tout l'échantillon, passé et futur (section 13)",
    "les quatre ensembles d'information selon le nombre de mois du trimestre cible déjà publiés (section 17)",
    "date à laquelle on se place pour produire une estimation (section 1.2)",
    "ensemble des trimestres où tous les modèles comparés ont produit un chiffre (section 1.2)",
    "proportion de valeurs aberrantes qu'un estimateur tolère avant de devenir arbitrairement faux (section 9)",
    "loi a priori centrant chaque équation sur sa propre valeur retardée et resserrant les autres (section 7)",
    "probabilité d'observer un écart au moins aussi grand si l'hypothèse nulle est vraie ; petite, elle conduit à la rejeter (section 20)",
    "propriété d'une série dont les chocs ne s'estompent jamais, rendant les régressions fallacieuses (section 4)",
    "terme anglais pour le bord irrégulier de l'information (section 13)",
    "RMSFE divisé par l'écart-type de la série ; sous 1, le modèle bat l'absence de modèle (section 1.2)",
    "protocole où toute quantité est réestimée à chaque origine sur la seule information antérieure (section 3)",
    "racine de la moyenne des erreurs au carré ; pénalise lourdement les grosses erreurs (section 1.2)",
    "valeur d'un paramètre optimisé qui atteint une borne de son domaine plutôt qu'un optimum intérieur (section 14)",
    "propriété d'une série dont la moyenne et la variance ne dérivent pas dans le temps (section 4)",
    "situation où un modèle apprend le bruit de son échantillon et prévoit d'autant plus mal (section 15)",
    "mesuré à prix constants, l'effet des prix étant retiré (section 1)",
    "probabilité des données sous un modèle, tous paramètres intégrés ; pénalise la complexité (section 8)"),
  check.names = FALSE, stringsAsFactors = FALSE)
ajouter(tbl(glo, aligne_droite = integer(0)))
ajouter(legende_tableau("Glossaire des termes techniques employés dans le rapport."))


# ============================================================================
# 32. BIBLIOGRAPHIE  (pleine largeur)
# ============================================================================
ajouter("<h2 id='s32'><span class='num'>32.</span>Références</h2>")
ordre <- get(".ordre_refs", envir = globalenv())
ajouter("<p>Les travaux sont numérotés dans l'ordre de leur première citation. Le ",
        "numéro en exposant dans le texte renvoie à cette liste.</p>")
ajouter('<div class="biblio"><ol>',
        paste0(sprintf('<li id="ref%d">%s</li>', seq_along(ordre), BIBLIO[ordre]),
               collapse = ""),
        '</ol></div>')

ajouter('<footer>',
        sprintf('Rapport établi le %s. ', format(Sys.Date(), "%d/%m/%Y")),
        'Toutes les valeurs chiffrées proviennent du protocole d&rsquo;évaluation ',
        'décrit à la section 18 et sont reproductibles à l&rsquo;identique.',
        '</footer>')

# ============================================================================
cat("\n[3/3] Ecriture\n")
# Le tiret cadratin est proscrit dans ce document : les incises appariees
# deviennent des parentheses, les incises simples des virgules.
sans_cadratin <- function(x) {
  x <- gsub("\u00a0?\u2014\u00a0?", " \u2014 ", x, perl = TRUE)
  x <- gsub("(\\s)\u2014 ([^\u2014<]{1,220}?) \u2014(\\s)", "\\1(\\2)\\3", x, perl = TRUE)
  x <- gsub(" \u2014 ", ", ", x, fixed = TRUE)
  x <- gsub("\u2014", "", x, fixed = TRUE)
  x <- gsub(",\\s*,", ",", x, perl = TRUE)
  x <- gsub(",\\s*([:;.)])", "\\1", x, perl = TRUE)
  x <- gsub("\\(\\s+", "(", x, perl = TRUE)
  x <- gsub("\\s+\\)", ")", x, perl = TRUE)
  x
}
# Les blocs ecrits directement en HTML passent au meme modele de note.
normaliser_notes <- function(x) {
  x <- gsub('<div class=.encadre alerte.><span class=.etiq.>([^<]*)</span>',
            '<div class="note note-a"><p><span class="note-t">\\1.</span> ',
            x, perl = TRUE)
  x <- gsub('<div class=.encadre.><span class=.etiq.>([^<]*)</span>',
            '<div class="note"><p><span class="note-t">\\1.</span> ',
            x, perl = TRUE)
  # la premiere balise <p> qui suivait le titre devient superflue
  gsub('(<span class="note-t">[^<]*</span> )<p>', '\\1', x, perl = TRUE)
}
# En composition mathematique, les relateurs (=, <, >, ~) prennent un blanc de
# part et d autre. Les operateurs le recoivent deja par leur classe ; les
# relateurs, ecrits en clair, sont habilles ici.
espacer_relateurs <- function(x) {
  # h est un VECTEUR : on traite element par element.
  vapply(x, function(el) {
    g <- gregexpr('<div class="eqc">.*?</div>', el, perl = TRUE)
    m <- regmatches(el, g)[[1]]
    if (length(m) == 0L) return(el)
    for (r in c("=", "&ge;", "&le;", "&asymp;", "&#8764;", "&#8660;", "&equiv;")) {
      m <- gsub(paste0(" ", r, " "),
                paste0(' <span class="rel">', r, '</span> '), m, fixed = TRUE)
    }
    regmatches(el, g) <- list(m)
    el
  }, character(1), USE.NAMES = FALSE)
}
h <- espacer_relateurs(h)
h <- normaliser_notes(h)
h <- sans_cadratin(h)
ecrire_global(h, "Nowcasting de la valeur ajoutée marocaine : rapport intégral", CHEMIN)
cat(sprintf("      %s (%.1f Mo, %d figures, %d tableaux, %d equations)\n",
            CHEMIN, file.size(CHEMIN) / 1024^2, .n_figure, .n_tableau,
            get(".n_equation", envir = globalenv())))
