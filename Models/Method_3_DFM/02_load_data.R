# ============================================================================
# GDPNow Maroc — 02_load_data.R
# Chargement et construction du panel mensuel par secteur
# ============================================================================
#
# CONVENTION TEMPORELLE :
#
# Les cibles VA et les indicateurs trimestriels sont datés au
# PREMIER JOUR du trimestre :
#
#   01/01/2026 -> T1 2026
#   01/04/2026 -> T2 2026
#   01/07/2026 -> T3 2026
#   01/10/2026 -> T4 2026
#
# Les variables trimestrielles sont donc positionnées directement
# à leur date d'origine dans la grille mensuelle.
#
# Les mois intermédiaires restent NA.
#
# IMPORTANT :
# Cette convention doit être conservée dans tous les fichiers
# suivants de la chaîne de nowcasting.
# ============================================================================
# --------------------------------------------------------------------------
# 1. Charger les 3 blocs (, trimestriel, mensuel cible) d'un secteur
# --------------------------------------------------------------------------

load_secteur <- function(secteur) {
  
  message("Chargement : ", secteur)
  
  f_cible <- file.path(
    DATA_DIR,
    paste0(secteur, "__cible.csv")
  )
  
  f_trim <- file.path(
    DATA_DIR,
    paste0(secteur, "__trimestriel.csv")
  )
  
  f_mens <- file.path(
    DATA_DIR,
    paste0(secteur, "__mensuel.csv")
  )
  
  
  # --------------------------------------------------------------------------
  # Cible trimestrielle
  # --------------------------------------------------------------------------
  
  if (!file.exists(f_cible)) {
    stop(
      "Fichier cible introuvable :\n",
      f_cible
    )
  }
  
  cible <- read.csv(
    f_cible,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  cible[[1]] <- as.Date(
    as.yearqtr(cible[[1]])
  )
  
  names(cible)[1] <- "date"
  
  
  # --------------------------------------------------------------------------
  # Indicateurs trimestriels
  # --------------------------------------------------------------------------
  
  trim <- NULL
  
  if (file.exists(f_trim)) {
    
    trim <- read.csv(
      f_trim,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    trim[[1]] <- as.Date(trim[[1]])
    
    names(trim)[1] <- "date"
  }
  
  
  # --------------------------------------------------------------------------
  # Indicateurs mensuels
  # --------------------------------------------------------------------------
  
  mens <- NULL
  
  if (file.exists(f_mens)) {
    
    mens <- read.csv(
      f_mens,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    mens[[1]] <- as.Date(mens[[1]])
    
    names(mens)[1] <- "date"
  }
  
  
  # --------------------------------------------------------------------------
  # Retour
  # --------------------------------------------------------------------------
  
  list(
    secteur = secteur,
    cible = cible,
    trim = trim,
    mens = mens
  )
}



# ============================================================================
# 2. CONSTRUCTION DU PANEL MENSUEL
#
# Les variables trimestrielles ne sont PAS interpolées.
#
# Elles sont positionnées au PREMIER MOIS du trimestre :
#
# T1 -> janvier
# T2 -> avril
# T3 -> juillet
# T4 -> octobre
#
# Les autres mois restent NA.
# ============================================================================

build_monthly_panel <- function(sd) {
  
  # --------------------------------------------------------------------------
  # Grille mensuelle commune
  # --------------------------------------------------------------------------
  
  all_dates <- c(
    sd$cible$date,
    if (!is.null(sd$trim)) sd$trim$date,
    if (!is.null(sd$mens)) sd$mens$date
  )
  
  m_start <- floor_date(
    min(all_dates),
    "month"
  )
  
  m_end <- floor_date(
    max(all_dates),
    "month"
  )
  
  grid <- seq(
    m_start,
    m_end,
    by = "month"
  )
  
  
  # --------------------------------------------------------------------------
  # Listes de variables
  # --------------------------------------------------------------------------
  
  monthly_list <- list()
  quarterly_list <- list()
  
  
  # --------------------------------------------------------------------------
  # Variables mensuelles
  # --------------------------------------------------------------------------
  
  if (!is.null(sd$mens)) {
    
    for (v in names(sd$mens)[-1]) {
      
      vals <- rep(
        NA_real_,
        length(grid)
      )
      
      idx <- match(
        floor_date(sd$mens$date, "month"),
        grid
      )
      
      valid <- !is.na(idx)
      
      vals[idx[valid]] <- as.numeric(
        sd$mens[[v]][valid]
      )
      
      monthly_list[[v]] <- vals
    }
  }
  
  
  # --------------------------------------------------------------------------
  # Variables trimestrielles
  # --------------------------------------------------------------------------
  
  if (!is.null(sd$trim)) {
    
    for (v in names(sd$trim)[-1]) {
      
      vals <- rep(
        NA_real_,
        length(grid)
      )
      
      # ----------------------------------------------------------
      # Placement à la date de début du trimestre
      #
      # Convention de la base :
      #   01/01 = T1
      #   01/04 = T2
      #   01/07 = T3
      #   01/10 = T4
      #
      # On conserve donc directement les dates du fichier CSV.
      # ----------------------------------------------------------
      
      qmonth <- floor_date(
        sd$trim$date,
        "month"
      )
      
      idx <- match(
        qmonth,
        grid
      )
      
      valid <- !is.na(idx)
      
      vals[idx[valid]] <- as.numeric(
        sd$trim[[v]][valid]
      )
      
      quarterly_list[[v]] <- vals
    }
  }
  
  
  # --------------------------------------------------------------------------
  # CIBLE TRIMESTRIELLE
  # --------------------------------------------------------------------------
  
  y_name <- names(sd$cible)[2]
  
  y <- rep(
    NA_real_,
    length(grid)
  )
  
  # ----------------------------------------------------------
  # La cible VA est déjà datée au début du trimestre :
  #
  #   2025-10-01 = T4 2025
  #   2026-01-01 = T1 2026
  #   2026-04-01 = T2 2026
  #
  # On conserve donc directement la date de la cible.
  # ----------------------------------------------------------
  
  qmonth <- floor_date(
    sd$cible$date,
    "month"
  )
  
  idx <- match(
    qmonth,
    grid
  )
  
  valid <- !is.na(idx)
  
  y[idx[valid]] <- as.numeric(
    sd$cible[[2]][valid]
  )
  
  quarterly_list[[y_name]] <- y
  
  
  # --------------------------------------------------------------------------
  # Assemblage
  #
  # IMPORTANT POUR dfms :
  #
  # variables mensuelles | variables trimestrielles | cible
  # --------------------------------------------------------------------------
  
  data_list <- c(
    monthly_list,
    quarterly_list
  )
  
  
  # --------------------------------------------------------------------------
  # Création du panel xts
  # --------------------------------------------------------------------------
  
  panel <- xts(
    do.call(cbind, data_list),
    order.by = grid
  )
  
  
  # --------------------------------------------------------------------------
  # Métadonnées
  # --------------------------------------------------------------------------
  
  attr(
    panel,
    "monthly_vars"
  ) <- names(monthly_list)
  
  attr(
    panel,
    "quarterly_vars"
  ) <- names(quarterly_list)
  
  attr(
    panel,
    "target"
  ) <- y_name
  
  
  panel
}


# ============================================================================
# 3. CHARGEMENT DE TOUS LES SECTEURS
# ============================================================================

charger_tous_les_panels <- function(
    secteurs = SECTEURS
) {
  
  panels <- list()
  
  for (s in secteurs) {
    
    sd <- tryCatch(
      
      load_secteur(s),
      
      error = function(e) {
        
        message(
          "  -> ECHEC chargement ",
          s,
          " : ",
          conditionMessage(e)
        )
        
        NULL
      }
    )
    
    if (is.null(sd)) {
      next
    }
    
    
    panels[[s]] <- tryCatch(
      
      build_monthly_panel(sd),
      
      error = function(e) {
        
        message(
          "  -> ECHEC construction panel ",
          s,
          " : ",
          conditionMessage(e)
        )
        
        NULL
      }
    )
  }
  
  
  panels
}



# ============================================================================
# 4. INVENTAIRE DES DONNEES
#
# Donne le nombre de séries mensuelles et trimestrielles récupérées
# pour chaque secteur + total général.
# ============================================================================

inventaire_panels <- function(panels) {
  
  inventaire <- data.frame(
    
    Secteur = character(),
    Mensuelles = integer(),
    Trimestrielles = integer(),
    Total = integer(),
    Cible = character(),
    
    stringsAsFactors = FALSE
  )
  
  
  for (s in names(panels)) {
    
    panel <- panels[[s]]
    
    vars_m <- attr(
      panel,
      "monthly_vars"
    )
    
    vars_q <- attr(
      panel,
      "quarterly_vars"
    )
    
    cible <- attr(
      panel,
      "target"
    )
    
    
    inventaire <- rbind(
      
      inventaire,
      
      data.frame(
        
        Secteur = s,
        
        Mensuelles = length(vars_m),
        
        Trimestrielles = length(vars_q),
        
        Total = length(vars_m) + length(vars_q),
        
        Cible = cible,
        
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Ligne TOTAL
  # --------------------------------------------------------------------------
  
  total <- data.frame(
    
    Secteur = "TOTAL",
    
    Mensuelles = sum(
      inventaire$Mensuelles
    ),
    
    Trimestrielles = sum(
      inventaire$Trimestrielles
    ),
    
    Total = sum(
      inventaire$Total
    ),
    
    Cible = NA_character_,
    
    stringsAsFactors = FALSE
  )
  
  
  inventaire <- rbind(
    inventaire,
    total
  )
  
  
  # --------------------------------------------------------------------------
  # Affichage
  # --------------------------------------------------------------------------
  
  cat("\n")
  cat("============================================================\n")
  cat("              INVENTAIRE DES DONNEES CHARGEES\n")
  cat("============================================================\n\n")
  
  print(
    inventaire,
    row.names = FALSE
  )
  
  cat("\n")
  cat("============================================================\n")
  
  
  return(inventaire)
}



# ============================================================================
# 5. MESSAGE DE CHARGEMENT DU FICHIER
# ============================================================================

message(
  "02_load_data.R charge : ",
  "load_secteur(), ",
  "build_monthly_panel(), ",
  "charger_tous_les_panels(), ",
  "inventaire_panels()"
)