# ============================================================================
# fonctions/kalman.R -- Comblement des trous internes par filtre de Kalman
# ============================================================================
# Plan de correction, phase 4 : "Le Kalman/AR peut servir a traiter les trous
# internes, les valeurs manquantes historiques, les extremites manquantes dans
# M0/M1/M2. Il ne doit jamais introduire d'information future."
#
# POURQUOI
#   Le refus strict des trimestres incomplets coute plus cher qu'il n'y parait.
#   Mesure sur les 12 branches couvertes :
#     1 555 trimestres refuses parce qu'un mois manque ;
#     1 007 trimestres perdus EN PLUS, parce qu'une difference (dlog ou diff)
#           a cheval sur un trou est invalide -- un trou detruit le trimestre
#           qu'il touche ET le suivant ;
#   soit 2 562 trimestres au total, et 1,64 trimestre perdu par trou.
#   Le panneau rectangulaire exige par l'ACP et la ridge tombe a 4 series en
#   mediane sur les donnees propres d'une branche : ces deux methodes n'ont
#   jamais pu etre testees dans des conditions correctes.
#
# CE QUI EST COMBLE, ET CE QUI NE L'EST PAS
#   Seuls les trous INTERNES, et seulement s'ils sont courts. Les extremites
#   manquantes du trimestre courant relevent des scenarios M0/M1/M2 de la
#   phase 13 : ce n'est pas le meme probleme, puisqu'il s'agit alors de PREVOIR
#   un mois qui n'existe pas encore, et non d'estimer un mois qui existe mais
#   n'a pas ete releve. Les melanger masquerait la difference entre une donnee
#   manquante et une donnee future.
#
# L'ANTI-LOOK-AHEAD, ET UNE PRECISION QUI COMPTE
#   Le modele est reestime a CHAQUE origine sur la seule information
#   disponible alors. A l'interieur de cet echantillon, on utilise le
#   LISSAGE et non le filtrage : pour estimer un mois de 2011 a l'origine 2020,
#   les mois de 2012 a 2019 sont legitimes -- ils appartiennent a I_T. Ce n'est
#   pas de l'information future par rapport a la CIBLE, qui est le seul
#   critere qui compte. L'interdit du plan porte sur le lissage applique une
#   fois pour toutes sur l'echantillon complet, ce que la reestimation
#   recursive evite par construction.
#
# LE MODELE
#   Modele structurel de base (Harvey) : niveau, pente et saisonnalite
#   stochastiques, estime par maximum de vraisemblance avec le filtre de Kalman,
#   qui traite les valeurs manquantes nativement.
#
#       y_t     = mu_t + gamma_t + eps_t
#       mu_t    = mu_{t-1} + beta_{t-1} + xi_t
#       beta_t  = beta_{t-1} + zeta_t
#       gamma_t = - somme_{j=1}^{11} gamma_{t-j} + omega_t
#
#   La composante saisonniere n'est pas un luxe : les series du vivier ne sont
#   pas corrigees des variations saisonnieres, et combler un mois de fevrier
#   avec le niveau moyen de l'annee introduirait une erreur systematique.
# ============================================================================

#' Calendrier mensuel complet entre deux dates, en fin de mois.
calendrier_mensuel <- function(debut, fin) {
  unique(fin_mois(seq(debut_mois(debut), debut_mois(fin), by = "month")))
}

#' Comble les trous internes d'une serie mensuelle par lissage de Kalman.
#'
#' @param dates,valeurs la serie observee (dates en fin de mois).
#' @param date_limite borne haute INCLUSE de l'ensemble d'information.
#' @param max_trou longueur maximale d'un trou comblable, en mois. Au-dela, on
#'   refuse : estimer six mois consecutifs revient a fabriquer la serie.
#' @param positive si TRUE, la valeur comblee est bornee a zero -- un
#'   debarquement ou une production ne peut pas etre negatif.
#' @return data.frame date, valeur, comble (logique), ou NULL si l'estimation
#'   echoue.
combler_trous_kalman <- function(dates, valeurs, date_limite,
                                 max_trou = 3L, positive = NULL) {
  garde <- !is.na(dates) & !is.na(valeurs) & dates <= date_limite
  if (sum(garde) < 24L) return(NULL)
  d <- as.Date(dates[garde]); v <- as.numeric(valeurs[garde])
  o <- order(d); d <- d[o]; v <- v[o]

  cal <- calendrier_mensuel(min(d), max(d))
  y <- rep(NA_real_, length(cal))
  pos <- match(d, cal)
  ok <- !is.na(pos)
  y[pos[ok]] <- v[ok]

  manquants <- which(is.na(y))
  if (length(manquants) == 0L) {
    return(tibble::tibble(date = cal, valeur = y, comble = FALSE))
  }

  # --- on ne comble que les trous COURTS ------------------------------------
  sequences <- rle(is.na(y))
  fin_seq   <- cumsum(sequences$lengths)
  debut_seq <- fin_seq - sequences$lengths + 1L
  comblables <- rep(FALSE, length(y))
  for (i in seq_along(sequences$values)) {
    if (sequences$values[i] && sequences$lengths[i] <= max_trou) {
      comblables[debut_seq[i]:fin_seq[i]] <- TRUE
    }
  }
  if (!any(comblables)) {
    return(tibble::tibble(date = cal, valeur = y, comble = FALSE))
  }

  if (is.null(positive)) positive <- all(v >= 0)

  # --- modele structurel, saisonnalite comprise ------------------------------
  serie <- stats::ts(y, frequency = 12L)
  mod <- try(suppressWarnings(stats::StructTS(serie, type = "BSM")), silent = TRUE)
  if (inherits(mod, "try-error")) {
    mod <- try(suppressWarnings(stats::StructTS(serie, type = "trend")), silent = TRUE)
  }
  if (inherits(mod, "try-error")) return(NULL)

  lisse <- try(stats::tsSmooth(mod), silent = TRUE)
  if (inherits(lisse, "try-error")) return(NULL)
  composantes <- intersect(colnames(lisse), c("level", "sea"))
  if (length(composantes) == 0L) return(NULL)
  estime <- rowSums(lisse[, composantes, drop = FALSE])

  rempli <- y
  a_combler <- comblables & is.na(y) & is.finite(estime)
  rempli[a_combler] <- estime[a_combler]
  if (positive) rempli[a_combler] <- pmax(0, rempli[a_combler])

  tibble::tibble(date = cal, valeur = rempli, comble = a_combler) %>%
    dplyr::filter(!is.na(valeur))
}

# ----------------------------------------------------------------------------
# PREVISION DES MOIS MANQUANTS DU TRIMESTRE CIBLE  (phase 13)
# ----------------------------------------------------------------------------
# Combler un trou et prevoir une extremite ne sont PAS le meme probleme, et le
# code les separe deliberement.
#
#   combler_trous_kalman()   estime un mois qui EXISTE mais n'a pas ete releve.
#                            L'information ulterieure est disponible et sert au
#                            lissage : c'est un probleme d'interpolation.
#
#   prevoir_mois_manquants() estime un mois qui N'EXISTE PAS ENCORE. Rien ne
#                            vient apres, et le modele extrapole : c'est un
#                            probleme de prevision, structurellement plus dur.
#
# Les confondre reviendrait a se donner en M1 une precision que l'on n'a qu'en
# M3, et donc a fabriquer l'avantage de calendrier que la phase 13 est censee
# mesurer.
#
# Le modele est le meme -- modele structurel de base, niveau, pente et
# saisonnalite -- mais on utilise ici `predict()`, qui itere l'equation d'etat
# vers l'avant, au lieu de `tsSmooth()`.

#' Prevoit les mois manquants a la fin d'une serie mensuelle.
#'
#' @param dates,valeurs serie observee, bornee a l'ensemble d'information.
#' @param mois_cibles vecteur de dates (fin de mois) a prevoir, posterieures a
#'   la derniere observation.
#' @param positive borne la prevision a zero pour les series positives.
#' @return data.frame date, valeur, prevu (logique), ou NULL si l'estimation
#'   echoue.
prevoir_mois_manquants <- function(dates, valeurs, mois_cibles, positive = NULL) {
  if (length(mois_cibles) == 0L) {
    return(tibble::tibble(date = as.Date(dates), valeur = as.numeric(valeurs),
                          prevu = FALSE))
  }
  garde <- !is.na(dates) & !is.na(valeurs)
  if (sum(garde) < 24L) return(NULL)
  d <- as.Date(dates[garde]); v <- as.numeric(valeurs[garde])
  o <- order(d); d <- d[o]; v <- v[o]

  cal <- calendrier_mensuel(min(d), max(d))
  y <- rep(NA_real_, length(cal))
  pos <- match(d, cal); ok <- !is.na(pos)
  y[pos[ok]] <- v[ok]

  cibles <- sort(unique(fin_mois(mois_cibles)))
  cibles <- cibles[cibles > max(cal)]
  if (length(cibles) == 0L) {
    return(tibble::tibble(date = d, valeur = v, prevu = FALSE))
  }
  # nombre de pas a prevoir, en tenant compte d'un eventuel decrochage
  h <- length(calendrier_mensuel(fin_mois(debut_mois(max(cal)) %m+% months(1L)),
                                 max(cibles)))
  if (h < 1L || h > 6L) return(NULL)

  if (is.null(positive)) positive <- all(v >= 0)

  serie <- stats::ts(y, frequency = 12L)
  mod <- try(suppressWarnings(stats::StructTS(serie, type = "BSM")), silent = TRUE)
  if (inherits(mod, "try-error")) {
    mod <- try(suppressWarnings(stats::StructTS(serie, type = "trend")), silent = TRUE)
  }
  if (inherits(mod, "try-error")) return(NULL)

  pr <- try(stats::predict(mod, n.ahead = h), silent = TRUE)
  if (inherits(pr, "try-error")) return(NULL)

  dates_prevues <- calendrier_mensuel(
    fin_mois(debut_mois(max(cal)) %m+% months(1L)), max(cibles))
  vals <- as.numeric(pr$pred)
  if (length(vals) != length(dates_prevues)) return(NULL)
  # Borne basse des series positives. On ne borne PAS a zero : une prevision
  # nulle rend l'agregat trimestriel nul, et log(0) n'existe pas -- le garde-fou
  # de `transformer_serie` interrompt alors tout le calcul. On borne donc a la
  # moitie de la plus petite valeur strictement positive observee, qui est une
  # activite tres faible mais existante. Cas rare : 2,3 % des mois prevus.
  if (positive) {
    positifs <- v[v > 0]
    plancher <- if (length(positifs) > 0L) 0.5 * min(positifs) else 1e-8
    vals <- pmax(plancher, vals)
  }

  dplyr::bind_rows(
    tibble::tibble(date = cal, valeur = y, prevu = FALSE) %>%
      dplyr::filter(!is.na(valeur)),
    tibble::tibble(date = dates_prevues, valeur = vals, prevu = TRUE) %>%
      dplyr::filter(date %in% cibles)) %>%
    dplyr::arrange(date)
}
