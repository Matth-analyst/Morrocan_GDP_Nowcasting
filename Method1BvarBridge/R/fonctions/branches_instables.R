# ============================================================================
# fonctions/branches_instables.R -- Sortir du BVAR les branches a rupture
# ============================================================================
# LE PROBLEME
#   Le BVAR echoue sur l'administration publique : ratio de 1,262, le pire des
#   seize branches, avec une correlation prevu-realise de -0,08. Un ratio
#   superieur a 1 ne signifie pas "imprevisible" -- une serie purement aleatoire
#   donnerait 1,00 -- mais que le modele AJOUTE du bruit non correle. Le calcul
#   le confirme : avec une correlation nulle et une prevision d'ecart-type
#   0,48 % face a un realise de 0,68 %, le ratio attendu vaut
#   sqrt(1 + 0,48^2/0,68^2) = 1,22, soit le 1,262 observe.
#
# LA CAUSE, QUI EST DANS LA DONNEE
#   Ecart-type de la croissance trimestrielle, par periode :
#
#       1998-2001   12,9 %     <- artefact de retropolation
#       2002-2005    0,6 %
#       2006-2009    1,9 %
#       2010-2013    1,3 %
#       2014-2026    0,7 %
#
#   Les niveaux confirment : T4-1998 = 13 457, T1-1999 = 9 696, T2-1999 =
#   12 137. Une branche de salaires publics ne perd pas 28 % en un trimestre
#   pour les regagner aussitot. L'education-sante, branche publique comparable,
#   reste a 0,7-1,4 % sur toute la periode : le probleme n'est donc pas une
#   propriete des branches publiques, c'est un defaut de cette serie-la.
#
# TROIS CORRECTIONS TESTEES ET REJETEES
#   indicatrices idiosyncratiques  branche 1,262 -> 1,064   mediane 0,978 -> 1,089
#   winsorisation de toutes                 -> 1,048                 -> 1,000
#   winsorisation ciblee                    -> 1,054                 -> 1,001
#
#   Toutes reparent la branche et abiment l'ensemble, y compris la version qui
#   ne touche qu'une seule colonne. La raison est que le BVAR est MULTIVARIE :
#   la colonne de l'administration publique est un regresseur dans les quinze
#   autres equations, et la modifier les deplace toutes. Les observations de
#   1998-2001 sont fausses pour cette branche mais portent de l'information
#   exploitable pour les autres.
#
# LA CORRECTION RETENUE
#   Ne pas toucher a la donnee commune. Sortir la branche du BVAR -- c'est-a-dire
#   ne pas UTILISER sa prevision -- et lui substituer sa moyenne recente. Le BVAR
#   reste strictement inchange pour les quinze autres.
#
#       branche 1,262 -> 1,006     agregat 0,967 -> 0,961, correlation 0,50 -> 0,60
#
#   Le resultat de fond est un peu ironique : le meilleur modele pour cette
#   branche est de NE RIEN PREVOIR. Sa croissance post-2002 est un bruit de
#   0,68 % d'ecart-type sans structure exploitable, et toute tentative de la
#   modeliser ajoute de la variance sans ajouter de signal.
#
# POURQUOI UNE REGLE ET NON UN TRAITEMENT NOMINATIF
#   Ecrire "si branche == administration publique" serait calibrer sur une
#   observation. La regle repose sur un diagnostic mesurable, recalcule a chaque
#   origine sur la seule information anterieure :
#
#       rapport_j = var( g_j, premier tiers ) / var( g_j, reste )
#
#   Sur cet echantillon la separation est franche : 49,3 pour l'administration
#   publique, 3,1 pour la suivante. Le seuil de 10 est un ordre de grandeur, et
#   tout seuil entre 5 et 40 donne le meme resultat. La regle se declenche sur
#   une seule branche aux 48 origines -- elle est stable, pas erratique.
#
#   Il faut neanmoins l'assumer : la regle est VERIFIEE sur un seul cas. Si une
#   autre branche la declenchait un jour, rien ne garantit que le traitement lui
#   conviendrait. C'est une limite a documenter, pas a masquer.
# ============================================================================

#' Diagnostic d'instabilite : rapport de variance premier tiers / reste.
#'
#' @param Y matrice des taux de croissance, une colonne par branche.
#' @param rapport_min seuil au-dela duquel une branche est declaree instable.
#' @param min_obs nombre minimal d'observations pour que le test ait un sens.
#' @return liste instables (logique), rapport (numerique), coupe (indice).
diagnostic_instabilite <- function(Y, rapport_min = 10, min_obs = 24L) {
  n <- nrow(Y)
  if (n < min_obs) {
    return(list(instables = rep(FALSE, ncol(Y)),
                rapport = rep(NA_real_, ncol(Y)), coupe = NA_integer_))
  }
  coupe <- max(8L, floor(n / 3))
  rapport <- vapply(seq_len(ncol(Y)), function(j) {
    v1 <- stats::var(Y[1:coupe, j])
    v2 <- stats::var(Y[(coupe + 1):n, j])
    if (!is.finite(v1) || !is.finite(v2) || v2 <= 0) 0 else v1 / v2
  }, numeric(1))
  list(instables = rapport > rapport_min, rapport = rapport, coupe = coupe)
}

#' Prevision de remplacement : moyenne sur les `fenetre` derniers trimestres.
#'
#' La fenetre de 60 trimestres n'est pas un parametre neuf : c'est celle deja
#' retenue en phase 3 pour l'echelle du prior. Le choix importe peu -- apres le
#' premier tiers, derniere moitie, 40 ou 60 trimestres donnent tous un agregat
#' a 0,960-0,961 -- mais autant ne pas multiplier les reglages.
prevision_branche_instable <- function(g_branche, cible, fenetre = 60L) {
  h <- g_branche$g[g_branche$date < cible]
  if (length(h) < 12L) return(NA_real_)
  mean(utils::tail(h, fenetre))
}

#' Substitue la prevision des branches instables dans un jeu de previsions BVAR.
#'
#' @param previsions data.frame branche, origine, prevision.
#' @param Y,dates_vec matrice et dates des taux de croissance.
#' @param va format long branche, date, g.
#' @return `previsions` avec une colonne `instable` et la prevision substituee.
corriger_branches_instables <- function(previsions, Y, dates_vec, va,
                                        rapport_min = 10, fenetre = 60L) {
  origines <- sort(unique(previsions$origine))
  subs <- purrr::map_dfr(origines, function(cible) {
    ok <- which(dates_vec < cible)
    if (length(ok) < 24L) return(NULL)
    d <- diagnostic_instabilite(Y[ok, , drop = FALSE], rapport_min)
    if (!any(d$instables)) return(NULL)
    purrr::map_dfr(colnames(Y)[d$instables], function(b) {
      g_b <- va %>% dplyr::filter(branche == b) %>% dplyr::select(date, g)
      tibble::tibble(branche = b, origine = cible,
                     remplacement = prevision_branche_instable(g_b, cible, fenetre),
                     rapport_variance = d$rapport[match(b, colnames(Y))])
    })
  })
  if (nrow(subs) == 0L) {
    return(previsions %>% dplyr::mutate(instable = FALSE,
                                        rapport_variance = NA_real_))
  }
  previsions %>%
    dplyr::left_join(subs, by = c("branche", "origine")) %>%
    dplyr::mutate(
      instable = !is.na(remplacement),
      prevision = ifelse(instable, remplacement, prevision)) %>%
    dplyr::select(-remplacement)
}
