# ============================================================================
# tache_production.R -- Le recalcul, dans son propre processus
# ============================================================================
# POURQUOI UN PROCESSUS A PART
#   Le recalcul dure une minute et demie. Execute dans le processus de
#   l'application, il la fige : plus aucune session n'est servie pendant ce
#   temps, et l'utilisateur ne peut meme pas consulter le chiffre precedent.
#
#   Ce script est donc lance detache, par Rscript. Il ne communique avec
#   l'application que par des FICHIERS -- un fichier d'etat qu'il met a jour au
#   fil du calcul, et les sorties habituelles. L'application les relit ; elle ne
#   partage aucune memoire avec lui, et reste entierement disponible.
#
#   C'est aussi ce qui rend le calcul survivable : si l'application est fermee
#   en cours de route, le calcul se termine et ecrit ses resultats.
#
# POURQUOI HORS DE app/R/
#   Shiny source AUTOMATIQUEMENT tout fichier .R place dans le dossier R/ d'une
#   application, au demarrage. Ce script, qui s'execute des qu'il est source,
#   lancait donc un calcul complet a chaque ouverture de l'application -- sans
#   que personne ne l'ait demande. Il vit desormais dans tache/, que Shiny ne
#   parcourt pas.
#
# APPEL
#   Rscript --vanilla tache/production.R <dossier app> [nowcast|perturbation]
#
# ETATS ECRITS DANS productions/tache_statut.csv
#   en_cours   part d'avancement et message
#   termine    le calcul a abouti, les sorties sont ecrites
#   erreur     le calcul a echoue, le message dit pourquoi
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)
racine_app <- if (length(args) >= 1L) args[1] else ".."
mode       <- if (length(args) >= 2L) args[2] else "nowcast"
setwd(racine_app)

suppressMessages({
  library(dplyr); library(tidyr); library(tibble)
})
source("R/fonctions_app.R")

ecrire_statut <- function(etat, part, message) {
  ecrire(tibble::tibble(
    etat = etat, part = part, message = message, mode = mode,
    horodatage = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    pid = Sys.getpid()), CHEMIN_STATUT)
}

ecrire_statut("en_cours", 0.02, "démarrage du processus de calcul")

# On n'ecrit jamais "termine" depuis la fonction de progression : seul
# l'aboutissement effectif du calcul le justifie.
avancer <- function(part, message) ecrire_statut("en_cours", min(part, 0.98), message)

r <- tryCatch({
  charger_chaine()
  if (mode == "perturbation") controle_perturbation(avancer)
  else produire_nowcast(avancer)
}, error = function(e) e)

if (inherits(r, "error")) {
  ecrire_statut("erreur", 1, conditionMessage(r))
} else if (mode == "perturbation") {
  ecrire(r, CHEMIN_PERTURBATION)
  ecrire_statut("termine", 1, sprintf("controle par perturbation : %s - %s",
                                      r$resultat[1], r$detail[1]))
} else {
  ecrire_statut("termine", 1, sprintf("%s / %s : %+.4f %% en %.0f secondes",
                                      r$agregat$trimestre[1],
                                      r$agregat$scenario[1],
                                      r$agregat$nowcast_pct[1], r$duree))
}
