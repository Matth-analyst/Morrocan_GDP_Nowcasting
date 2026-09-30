# ============================================================================
# reprendre_pipeline.R -- Reprendre le pipeline a partir d'un script donne
# ============================================================================
# Meme liste et meme ordre que run_pipeline.R : ce fichier n'en est qu'une vue
# partielle. Utile quand une execution s'est arretee en cours de route et que
# les etapes anterieures ont deja produit leurs sorties.
#
#   Rscript reprendre_pipeline.R                   reprend a DEPART (ci-dessous)
#   Rscript reprendre_pipeline.R 06_agregation     reprend au premier script
#                                                  dont le nom contient cela
#
# Attention : la reprise suppose que les sorties des etapes precedentes sont
# celles de la version ACTUELLE des scripts. Apres une modification en amont,
# c'est run_pipeline.R qu'il faut relancer.
# ============================================================================

DEPART <- "04b2_ridge_corrige"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1L) DEPART <- args[1]

source_pipeline <- function() {
  txt <- readLines("run_pipeline.R", warn = FALSE)
  deb <- grep("^scripts <- c\\(", txt)
  fin <- grep("^\\)", txt)
  fin <- fin[fin > deb][1]
  bloc <- txt[deb:fin]
  unlist(regmatches(bloc, gregexpr('"R/[^"]+\\.R"', bloc))) |>
    gsub(pattern = '"', replacement = "")
}

scripts <- source_pipeline()
i <- grep(DEPART, scripts, fixed = TRUE)
if (length(i) == 0L) {
  stop("Aucun script du pipeline ne correspond a : ", DEPART,
       "\n  scripts disponibles :\n    ", paste(scripts, collapse = "\n    "),
       call. = FALSE)
}
restants <- scripts[seq(i[1], length(scripts))]

cat(strrep("=", 74), "\n", sep = "")
cat(sprintf("REPRISE a %s -- %d scripts sur %d\n",
            restants[1], length(restants), length(scripts)))
cat(strrep("=", 74), "\n", sep = "")

for (s in restants) {
  cat("\n", strrep("=", 74), "\n", "EXECUTION : ", s, "\n",
      strrep("=", 74), "\n", sep = "")
  source(s)
}

cat("\n", strrep("=", 74), "\n", "Reprise terminee.\n", sep = "")
