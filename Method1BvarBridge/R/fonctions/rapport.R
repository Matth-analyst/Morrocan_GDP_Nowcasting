# ============================================================================
# fonctions/rapport.R -- Utilitaires de composition des rapports d'etape
# ============================================================================
# Les rapports de phase sont des fichiers HTML autonomes, ecrits dans report/.
# Les images y sont encodees en base64 : le fichier s'ouvre et se transmet
# seul, et s'imprime en PDF depuis le navigateur.
#
# Toutes les valeurs chiffrees d'un rapport sont relues depuis les CSV de
# resultats/ : un rapport ne peut pas diverger des resultats, il se regenere
# avec eux.
#
# Ce fichier ne contient que la mecanique de rendu. Le contenu de chaque
# rapport est dans son propre script (02b_, 03b_, ...).
# ============================================================================

DOSSIER_RAPPORT <- "report"
dir.create(DOSSIER_RAPPORT, showWarnings = FALSE, recursive = TRUE)

#' Echappe les caracteres reserves du HTML.
esc <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Nombre formate a la francaise : espace comme separateur de milliers,
#' virgule decimale, tiret cadratin pour les valeurs manquantes.
nb <- function(x, d = 0) {
  ifelse(is.na(x), "—",
         formatC(round(x, d), format = "f", digits = d,
                 big.mark = " ", decimal.mark = ","))
}

#' Tableau HTML. `aligne_droite` donne les indices des colonnes numeriques,
#' qui sont alignees a droite et composees en chiffres tabulaires.
tbl <- function(df, aligne_droite = NULL, classes = "") {
  if (is.null(aligne_droite)) {
    aligne_droite <- which(vapply(df, is.numeric, logical(1)))
  }
  th <- paste0(sprintf('<th%s>%s</th>',
                       ifelse(seq_along(df) %in% aligne_droite, ' class="num"', ''),
                       esc(names(df))), collapse = "")
  lignes <- vapply(seq_len(nrow(df)), function(i) {
    cellules <- vapply(seq_along(df), function(j) {
      v <- df[[j]][i]
      sprintf('<td%s>%s</td>',
              ifelse(j %in% aligne_droite, ' class="num"', ''),
              if (is.na(v)) "—" else esc(v))
    }, character(1))
    paste0("<tr>", paste0(cellules, collapse = ""), "</tr>")
  }, character(1))
  sprintf('<table class="%s"><thead><tr>%s</tr></thead><tbody>%s</tbody></table>',
          classes, th, paste0(lignes, collapse = ""))
}

#' Reinitialise les compteurs : a appeler au debut de chaque rapport.
init_compteurs <- function() {
  assign(".n_figure",  0L, envir = globalenv())
  assign(".n_tableau", 0L, envir = globalenv())
  invisible(NULL)
}

#' Figure encodee en base64, avec numero, titre et commentaire de lecture.
#' Le commentaire n'est pas decoratif : il dit ce que la figure montre et ce
#' qu'il faut en conclure.
figure <- function(fichier, titre, lecture, dossier = DOSSIER_FIGURES) {
  chemin <- file.path(dossier, fichier)
  if (!file.exists(chemin)) {
    return(sprintf('<p class="manquant">Figure absente : %s</p>', esc(fichier)))
  }
  assign(".n_figure", get(".n_figure", envir = globalenv()) + 1L, envir = globalenv())
  sprintf(paste0('<figure><img src="data:image/png;base64,%s" alt="%s">',
                 '<figcaption><span class="fignum">Figure %d</span> — %s',
                 '<span class="lecture">%s</span></figcaption></figure>'),
          base64enc::base64encode(chemin), esc(titre),
          get(".n_figure", envir = globalenv()), esc(titre), lecture)
}

#' Legende de tableau, numerotee.
legende_tableau <- function(txt) {
  assign(".n_tableau", get(".n_tableau", envir = globalenv()) + 1L, envir = globalenv())
  sprintf('<p class="tabcap"><span class="fignum">Tableau %d</span> — %s</p>',
          get(".n_tableau", envir = globalenv()), txt)
}


# ----------------------------------------------------------------------------
# Rendu mathematique
# ----------------------------------------------------------------------------
# Les formules sont composees en HTML/CSS avec des caracteres Unicode, et non
# via une bibliotheque chargee depuis un CDN : le rapport doit s'ouvrir hors
# ligne et s'imprimer sans dependance externe.

compteur_equation <- function() {
  if (!exists(".n_equation", envir = globalenv())) assign(".n_equation", 0L, envir = globalenv())
  assign(".n_equation", get(".n_equation", envir = globalenv()) + 1L, envir = globalenv())
  get(".n_equation", envir = globalenv())
}

#' Equation en bloc, centree et numerotee.
#' @param corps HTML de l'equation.
#' @param numerote FALSE pour une equation sans numero.
eq <- function(corps, numerote = TRUE) {
  if (!numerote) {
    return(sprintf('<div class="eq"><div class="eqc">%s</div></div>', corps))
  }
  n <- compteur_equation()
  sprintf('<div class="eq"><div class="eqc">%s</div><div class="eqn">(%d)</div></div>',
          corps, n)
}

#' Math en ligne.
m <- function(x) sprintf('<span class="mi">%s</span>', x)

#' Bloc "definition" : encadre leger pour poser une notation.
definition <- function(titre, corps) {
  sprintf('<div class="defn"><span class="etiq">%s</span>%s</div>', titre, corps)
}

#' Bloc "intuition" : l'idee avant la formule.
intuition <- function(corps) {
  paste0('<div class="intu"><span class="etiq">L&rsquo;idée</span>', corps, '</div>')
}

#' Feuille de style : sobre, imprimable, sans aplats de couleur.
css_rapport <- function() '
:root{--encre:#1b1b1b;--doux:#5c5c5c;--trait:#d8d8d8;--fond:#ffffff;
      --accent:#2E74B5;--surface:#f7f7f5;}
*{box-sizing:border-box}
body{margin:0;background:var(--fond);color:var(--encre);
     font-family:"Iowan Old Style","Palatino Linotype",Palatino,Georgia,serif;
     font-size:16px;line-height:1.62;}
.page{max-width:900px;margin:0 auto;padding:56px 34px 96px;}
header.titre{border-bottom:2px solid var(--encre);padding-bottom:22px;margin-bottom:34px;}
header.titre .sur{font-family:ui-sans-serif,system-ui,"Segoe UI",sans-serif;
  font-size:12px;letter-spacing:.14em;text-transform:uppercase;color:var(--doux);}
header.titre h1{font-size:31px;line-height:1.2;margin:10px 0 8px;font-weight:600;}
header.titre .sous{color:var(--doux);font-size:16px;margin:0;}
header.titre .meta{font-family:ui-sans-serif,system-ui,sans-serif;font-size:12.5px;
  color:var(--doux);margin-top:14px;}
h2{font-size:21px;font-weight:600;margin:46px 0 4px;padding-bottom:6px;
   border-bottom:1px solid var(--trait);}
h2 .num{color:var(--doux);font-weight:400;margin-right:10px;font-size:17px;}
h3{font-size:17px;font-weight:600;margin:30px 0 8px;}
h4{font-size:15px;font-weight:600;margin:22px 0 6px;color:var(--doux);}
p{margin:12px 0;}
ul,ol{margin:12px 0;padding-left:22px;} li{margin:6px 0;}
code{font-family:ui-monospace,"Cascadia Mono",Consolas,monospace;font-size:13.5px;
     background:var(--surface);padding:1px 5px;border-radius:3px;}
pre{background:var(--surface);border-left:3px solid var(--trait);padding:14px 16px;
    overflow-x:auto;font-family:ui-monospace,Consolas,monospace;font-size:13px;
    line-height:1.5;margin:16px 0;}
pre code{background:none;padding:0;font-size:13px;}
table{border-collapse:collapse;width:100%;margin:8px 0 4px;font-size:13.5px;
      font-family:ui-sans-serif,system-ui,"Segoe UI",sans-serif;}
thead th{text-align:left;font-weight:600;border-bottom:1.5px solid var(--encre);
         padding:7px 9px;white-space:nowrap;}
tbody td{border-bottom:1px solid var(--trait);padding:6px 9px;vertical-align:top;}
tbody tr:last-child td{border-bottom:1px solid var(--encre);}
.num{text-align:right;font-variant-numeric:tabular-nums;}
.tabcap,figcaption{font-family:ui-sans-serif,system-ui,sans-serif;font-size:12.5px;
  color:var(--doux);line-height:1.5;}
.tabcap{margin:6px 0 22px;}
.fignum{font-weight:600;color:var(--encre);}
figure{margin:26px 0 30px;}
figure img{width:100%;height:auto;border:1px solid var(--trait);background:#fff;}
figcaption{margin-top:9px;}
figcaption .lecture{display:block;margin-top:5px;}
.encadre{background:var(--surface);border:1px solid var(--trait);
         border-left:3px solid var(--accent);padding:14px 18px;margin:20px 0;}
.encadre p:first-child{margin-top:0;} .encadre p:last-child{margin-bottom:0;}
.encadre .etiq{font-family:ui-sans-serif,system-ui,sans-serif;font-size:11px;
  letter-spacing:.1em;text-transform:uppercase;color:var(--doux);display:block;
  margin-bottom:6px;}
.alerte{border-left-color:#B45309;}
.sommaire{background:var(--surface);border:1px solid var(--trait);padding:18px 24px;
  font-family:ui-sans-serif,system-ui,sans-serif;font-size:14px;}
.sommaire ol{margin:6px 0;padding-left:20px;} .sommaire li{margin:3px 0;}
.sommaire a{color:var(--encre);text-decoration:none;}
.sommaire a:hover{text-decoration:underline;}
.cles{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));
      gap:1px;background:var(--trait);border:1px solid var(--trait);margin:22px 0;}
.cle{background:var(--fond);padding:13px 15px;}
.cle .v{font-size:23px;font-weight:600;font-variant-numeric:tabular-nums;}
.cle .l{font-family:ui-sans-serif,system-ui,sans-serif;font-size:11.5px;
  color:var(--doux);line-height:1.35;margin-top:3px;}
.manquant{color:#B45309;font-style:italic;}
footer{margin-top:60px;padding-top:18px;border-top:1px solid var(--trait);
  font-family:ui-sans-serif,system-ui,sans-serif;font-size:12px;color:var(--doux);}

/* --- rendu mathematique --- */
.mi{font-family:"Cambria Math","Latin Modern Math","Times New Roman",serif;
    font-style:italic;white-space:nowrap;}
.eq{display:flex;align-items:center;gap:16px;margin:22px 0;padding:14px 18px;
    background:var(--surface);border-left:3px solid var(--trait);}
.eqc{flex:1;text-align:center;font-family:"Cambria Math","Latin Modern Math",
     "Times New Roman",serif;font-size:17px;line-height:1.9;overflow-x:auto;}
.eqn{font-family:ui-sans-serif,system-ui,sans-serif;font-size:12.5px;
     color:var(--doux);flex:0 0 auto;}
.eqc .op{font-style:normal;padding:0 .25em;}
.eqc .num{font-style:normal;}
.eqc .fr{display:inline-block;vertical-align:middle;text-align:center;
         margin:0 .3em;}
.eqc .fr .hi{display:block;padding:0 .35em;border-bottom:1px solid var(--encre);}
.eqc .fr .lo{display:block;padding:0 .35em;}
.eqc .big{font-size:1.5em;vertical-align:-.15em;font-style:normal;}
.defn{border:1px solid var(--trait);border-left:3px solid var(--doux);
      padding:12px 16px;margin:18px 0;background:#fff;}
.defn p:first-of-type{margin-top:4px;} .defn p:last-child{margin-bottom:0;}
.intu{background:#fbfaf7;border:1px dashed var(--trait);padding:12px 16px;
      margin:18px 0;}
.intu p:first-of-type{margin-top:4px;} .intu p:last-child{margin-bottom:0;}
.defn .etiq,.intu .etiq{font-family:ui-sans-serif,system-ui,sans-serif;
  font-size:11px;letter-spacing:.1em;text-transform:uppercase;color:var(--doux);
  display:block;margin-bottom:4px;}
@media print{.eq{break-inside:avoid;}}
@media print{
  .page{max-width:none;padding:0;} h2{page-break-after:avoid;}
  figure,table{page-break-inside:avoid;} .sommaire{display:none;}
  body{font-size:11pt;}
}
'

#' En-tete d'un rapport.
entete_rapport <- function(surtitre, titre, sous_titre, sources) {
  paste0('<header class="titre">',
         sprintf('<div class="sur">%s</div>', surtitre),
         sprintf('<h1>%s</h1>', titre),
         sprintf('<p class="sous">%s</p>', sous_titre),
         sprintf('<p class="meta">Généré le %s &middot; %s &middot; sources : %s</p>',
                 format(Sys.Date(), "%d/%m/%Y"), R.version.string, sources),
         '</header>')
}

#' Sommaire numerote, renvoyant vers les ancres s1, s2, ...
sommaire_rapport <- function(sections) {
  paste0('<nav class="sommaire"><strong>Sommaire</strong><ol>',
         paste0(sprintf('<li><a href="#s%d">%s</a></li>', seq_along(sections),
                        esc(sections)), collapse = ""),
         '</ol></nav>')
}

#' Bandeau de chiffres cles. `valeurs` est un vecteur nomme :
#' names = libelle affiche sous le chiffre.
chiffres_cles <- function(valeurs) {
  paste0('<div class="cles">',
         paste0(sprintf('<div class="cle"><div class="v">%s</div><div class="l">%s</div></div>',
                        valeurs, names(valeurs)), collapse = ""),
         '</div>')
}

#' Pied de page.
pied_rapport <- function(script) {
  paste0('<footer>',
         sprintf('Rapport généré automatiquement par <code>%s</code> le %s. ',
                 script, format(Sys.time(), "%d/%m/%Y à %H:%M")),
         'Toutes les valeurs chiffrées sont relues depuis les fichiers de ',
         '<code>resultats/</code> et les figures depuis <code>figures/</code> : ',
         'le rapport se régénère avec les résultats et ne peut pas en diverger.',
         '</footer>')
}

#' Assemble et ecrit le fichier HTML.
ecrire_rapport <- function(corps, titre_page, chemin) {
  html <- paste0(
    '<!doctype html>\n<html lang="fr">\n<head>\n<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
    sprintf('<title>%s</title>\n', titre_page),
    '<style>', css_rapport(), '</style>\n</head>\n<body>\n<div class="page">\n',
    paste0(corps, collapse = "\n"),
    '\n</div>\n</body>\n</html>\n')
  con <- file(chemin, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(html), con)
  invisible(chemin)
}
