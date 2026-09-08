# ==============================================================================
# 05_exporter_coefficients_bvar.R
#
# ETAPE 1 (partie 5, AJOUTEE) -- Exporte le modele BVAR final
# (resultats/bvar_modele_final.rds, produit par 03_bvar_estimation...R,
# desormais correctement restreint a 2010-2021) en un CSV plat
# (equation, type, predicteur, retard, coefficient), le format attendu par
# les scripts des Etapes 5, 6 et 7 (coefficients_complets_BVAR.csv).
#
# CORRECTION : ce fichier n'existait auparavant que comme export manuel,
# jamais reproduit par un script -- source d'une incoherence potentielle
# si le modele etait reestime sans jamais regenerer son export. Corrige
# ici par un script dedie, trace et reproductible.
# ==============================================================================

modele <- readRDS("resultats/bvar_modele_final.rds")
B <- modele$B
noms <- modele$noms
p <- modele$p
n <- length(noms)

lignes <- list()

for (j in seq_along(noms)) {
  equation <- noms[j]

  # --- Constante ---------------------------------------------------------
  lignes[[length(lignes)+1]] <- data.frame(
    equation = equation, type = "constante", predicteur = NA, retard = 0,
    coefficient = B[1, j]
  )

  # --- Coefficients A_l, pour chaque retard l et chaque predicteur -------
  for (l in 1:p) {
    for (i in seq_along(noms)) {
      idx_ligne <- 1 + (l - 1) * n + i
      lignes[[length(lignes)+1]] <- data.frame(
        equation = equation, type = "A_l", predicteur = noms[i], retard = l,
        coefficient = B[idx_ligne, j]
      )
    }
  }
}

coefs_complets <- do.call(rbind, lignes)
readr::write_csv(coefs_complets, "resultats/coefficients_complets_BVAR.csv")

cat(sprintf("Export termine : %d lignes (%d equations x [1 constante + %d x %d retards])\n",
            nrow(coefs_complets), n, n, p))
cat("Fichier : resultats/coefficients_complets_BVAR.csv\n")
