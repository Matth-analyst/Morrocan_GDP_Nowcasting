# ============================================================================
# fonctions/mise_en_page.R -- Composition en deux colonnes, notes, references
# ============================================================================
# Meme gabarit que le rapport de la premiere methode : le lecteur passe de
# l'un a l'autre sans changer de reperes. Deux colonnes separees par un blanc,
# exhibits pleine largeur et centres, notes en italique dans le fil du texte,
# appels de reference en exposant, glossaire et bibliographie en fin de
# document.
#
# Le tiret cadratin est proscrit : les incises appariees deviennent des
# parentheses, les incises simples des virgules.
# ============================================================================


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


pc <- function(x, d = 2, unite = TRUE) sprintf("%s%s%s",
                                              ifelse(x >= 0, "+", "−"),
                                              nb(abs(x), d),
                                              if (unite) " %" else "")
# ============================================================================

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
