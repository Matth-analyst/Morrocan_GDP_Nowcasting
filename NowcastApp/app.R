# ============================================================================
# app.R -- Application de production du nowcast de la valeur ajoutée, Maroc
# ============================================================================
# Permet de :
#   - lire le nowcast courant de la valeur ajoutée totale, sa marge
#     d'incertitude calibrée et sa décomposition par branche
#   - explorer les seize séries cibles et les indicateurs infra-trimestriels
#   - consulter le détail des deux modèles (BVAR à prior de Minnesota,
#     équations de passerelle) et de leur combinaison
#   - juger la méthode : étalons, épisodes, apport de l'information
#     infra-trimestrielle, robustesse, contrôles d'antériorité
#   - AJOUTER DE NOUVELLES OBSERVATIONS (valeur ajoutée ou indicateur) au fil
#     des publications et relancer la chaîne pour mettre à jour le nowcast
#
# Le grand classeur est la source ; il n'est jamais modifié. Les observations
# saisies ici sont rangées à part et fusionnées à la lecture.
# ============================================================================

suppressMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
})

source("R/fonctions_app.R")

# ----------------------------------------------------------------------------
# THEME
# ----------------------------------------------------------------------------
theme_app <- bs_theme(
  version = 5,
  bg = "#FBFCFD", fg = "#1F2937",
  primary = COULEUR_PRIMAIRE, secondary = COULEUR_SECONDAIRE,
  success = COULEUR_OK, danger = COULEUR_ALERTE, warning = COULEUR_ACCENT,
  base_font = font_google("Inter", local = TRUE),
  heading_font = font_google("Inter", wght = 600, local = TRUE),
  "navbar-bg" = COULEUR_PRIMAIRE,
  "border-radius" = "0.6rem",
  "card-border-color" = "#E5E7EB"
) %>%
  bs_add_rules('
    [data-bs-theme="dark"] {
      --bs-body-bg: #12161C;
      --bs-body-color: #E5E7EB;
      --bs-card-bg: #1A1F27;
      --bs-border-color: #2B3038;
    }
    .navbar-brand { font-weight: 700; letter-spacing: -0.02em; }
    .value-box-title { font-size: 0.82rem !important; opacity: 0.85; }
    .value-box-value { font-weight: 700 !important; }
    .card { box-shadow: 0 1px 3px rgba(0,0,0,0.06); }
    .card-header { font-weight: 600; background-color: #F8FAFC !important; }
    [data-bs-theme="dark"] .card-header { background-color: #20262F !important; }
    .signe-atypique { color: #C00000; font-weight: 600; }
    .chiffre-cle { font-size: 2.6rem; font-weight: 700; letter-spacing: -0.02em; }
    .tabl-intervalle td { padding: 0.15rem 1.1rem 0.15rem 0; }
    footer.app-footer { color: #9CA3AF; font-size: 0.78rem; padding: 1.2rem 0;
                        text-align: center; }
    [data-bs-theme="dark"] .dataTable { color: #E5E7EB; }
    [data-bs-theme="dark"] table.dataTable thead th { color: #E5E7EB; }
    [data-bs-theme="dark"] .form-control, [data-bs-theme="dark"] .form-select {
      background-color: #1A1F27; color: #E5E7EB; border-color: #2B3038;
    }
    .bslib-dark-mode-toggle { color: #FFFFFF; }
  ')

# ----------------------------------------------------------------------------
# CHARGEMENT -- UNE SEULE FOIS PAR PROCESSUS R, PAS PAR SESSION UTILISATEUR.
# Les sorties de la chaîne et les séries sources sont des fichiers : les relire
# à chaque connexion ne servirait qu'à rendre l'ouverture lente.
# ----------------------------------------------------------------------------
SORTIES_BASE <- charger_sorties()
DONNEES_BASE <- charger_donnees_base()

TOUTES_BRANCHES_APP <- DONNEES_BASE$branches
BRANCHES_COUVERTES  <- DONNEES_BASE$couvertes
BRANCHES_NON_COUV   <- DONNEES_BASE$non_couvertes

if (dir.exists(file.path(RACINE, "figures"))) {
  addResourcePath("figures", file.path(RACINE, "figures"))
}
if (dir.exists(file.path(RACINE, "report"))) {
  addResourcePath("rapports", file.path(RACINE, "report"))
}

# Un seul rapport est presente : le rapport global. Les rapports de phase
# existent toujours dans report/, mais ils sont des etapes de travail -- les
# offrir a cote du rapport global inviterait a lire une version partielle et
# datee de ce qu'il contient deja en entier.
RAPPORT <- "rapport_global.html"
RAPPORT_PRESENT <- file.exists(file.path(RACINE, "report", RAPPORT))

# ============================================================================
# UI
# ============================================================================
ui <- page_navbar(
  title = "GDPNow-Maroc",
  theme = theme_app,
  window_title = "GDPNow-Maroc : nowcast sectoriel de la valeur ajoutée",
  fillable = FALSE,

  # -------------------------------------------------------------- DASHBOARD
  nav_panel(
    title = "Tableau de bord", icon = icon("gauge-high"),
    uiOutput("bandeau_etat"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Nowcast de la valeur ajoutée totale",
                value = textOutput("vb_nowcast"),
                showcase = icon("chart-line"), theme = "primary"),
      value_box(title = "Trimestre visé", value = textOutput("vb_trimestre"),
                showcase = icon("calendar"), theme = "secondary",
                uiOutput("vb_scenario")),
      value_box(title = "Intervalle à 80 %", value = textOutput("vb_intervalle"),
                showcase = icon("arrows-left-right"), theme = "warning"),
      value_box(title = "Dernier calcul", value = textOutput("vb_maj"),
                showcase = icon("clock"), theme = "secondary",
                uiOutput("vb_origine"))
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Contribution de chaque branche au nowcast"),
        plotlyOutput("plot_contributions", height = "480px")
      ),
      card(
        card_header("Répartition par mode de traitement"),
        plotlyOutput("plot_groupes", height = "215px"),
        hr(),
        p(class = "text-muted small",
          "Combinaison (bleu) : équation de passerelle et modèle vectoriel, ",
          "combinés à poids fixe δ = 0,5. BVAR seul (gris) : branches dont aucun ",
          "indicateur n'a été retenu à cette origine — soit qu'elles n'en aient ",
          "aucun dans le vivier, soit qu'aucun ne fût disponible à cette date."),
        hr(),
        uiOutput("bloc_intervalle")
      )
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Les huit derniers trimestres"),
        plotlyOutput("plot_recent", height = "300px")
      ),
      card(
        card_header("Comment lire ce chiffre"),
        uiOutput("bloc_lecture")
      )
    ),
    card(
      card_header("Détail des prévisions par branche"),
      DTOutput("table_previsions")
    )
  ),

  # ------------------------------------------------------------- EXPLORATION
  nav_panel(
    title = "Exploration", icon = icon("magnifying-glass-chart"),
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header("Sélection"),
        selectInput("explo_branche", "Branche", choices = TOUTES_BRANCHES_APP,
                    selected = "Industrie de transformation"),
        radioButtons("explo_transfo", "Affichage",
                     choices = c("Niveau (VA, Mdh)" = "niveau",
                                 "Taux de croissance (Δlog)" = "dlog"),
                     selected = "dlog"),
        hr(),
        h6("Stationnarité"),
        p(class = "text-muted small",
          "Test de racine unitaire sur le niveau et sur la différence de ",
          "logarithme. C'est ce test qui justifie de modéliser la croissance ",
          "et non le niveau."),
        tableOutput("explo_adf"),
        hr(),
        h6("Statistiques descriptives"),
        tableOutput("explo_desc")
      ),
      navset_card_tab(
        nav_panel("Trajectoire",
                  plotlyOutput("plot_serie_branche", height = "440px")),
        nav_panel("Corrélation entre branches",
                  p(class = "text-muted small mt-2",
                    "Corrélation des taux de croissance trimestriels. C'est ",
                    "cette structure que le BVAR exploite : une branche est ",
                    "prévue par son propre passé et par celui des quinze autres."),
                  plotlyOutput("plot_correlation", height = "560px")),
        nav_panel("Indicateurs contre cible",
                  selectInput("explo_branche_couverte", "Branche couverte",
                              choices = BRANCHES_COUVERTES, width = "420px"),
                  plotlyOutput("plot_indicateurs_cible", height = "500px")),
        nav_panel("Bord irrégulier",
                  p(class = "text-muted small mt-2",
                    "Les séries ne s'arrêtent pas toutes au même mois. C'est ce ",
                    "bord irrégulier que le traitement des mois manquants, ",
                    "série par série, est fait pour absorber."),
                  plotlyOutput("plot_ragged", height = "520px"))
      )
    )
  ),

  # ------------------------------------------------------------------ MODELE
  nav_panel(
    title = "Modèle & prévisions", icon = icon("diagram-project"),
    navset_card_tab(
      nav_panel(
        "BVAR trimestriel",
        p(class = "text-muted",
          "Modèle vectoriel autorégressif bayésien à prior de Minnesota, imposé ",
          "par observations fictives (Litterman 1986 ; Bańbura, Giannone & ",
          "Reichlin 2010), seize branches. L'ordre p et le resserrement λ ne ",
          "sont pas fixés une fois pour toutes : ils sont re-choisis à chaque ",
          "origine sur la seule information antérieure."),
        layout_columns(
          col_widths = c(7, 5),
          plotlyOutput("plot_hyper", height = "360px"),
          div(h6("Comparaison des règles de choix"), DTOutput("table_dm_bvar"))
        ),
        hr(),
        h6("Branches à rupture de variance"),
        p(class = "text-muted small",
          "Une branche dont la variance du premier tiers de l'échantillon ",
          "dépasse dix fois celle du reste sort du BVAR et reçoit sa moyenne ",
          "récente : sur une série qui a changé de régime, le BVAR n'ajoute que ",
          "du bruit non corrélé. Le diagnostic est recalculé à chaque origine."),
        DTOutput("table_instabilite")
      ),
      nav_panel(
        "Équations de passerelle",
        p(class = "text-muted",
          "Régression de la croissance de la valeur ajoutée sur celle des ",
          "indicateurs agrégés au trimestre, augmentée d'un terme ",
          "autorégressif. La liste des indicateurs est re-sélectionnée à chaque ",
          "origine, et seulement parmi ceux effectivement disponibles à cette ",
          "date : une série publiée avec retard ne peut pas être retenue."),
        selectInput("bridge_branche", "Branche", choices = BRANCHES_COUVERTES,
                    width = "420px"),
        plotlyOutput("plot_selection", height = "340px"),
        hr(),
        DTOutput("table_selection")
      ),
      nav_panel(
        "Poids de combinaison δ",
        p(class = "text-muted",
          "δ = 1 donne le BVAR seul, δ = 0 la passerelle seule. Le poids est ",
          "fixé à 0,5, et ce choix se mesure : la courbe ci-dessous montre la ",
          "perte du système sur l'ensemble du protocole pour chaque valeur de δ."),
        layout_columns(
          col_widths = c(7, 5),
          plotlyOutput("plot_courbe_delta", height = "380px"),
          div(h6("Règles alternatives, protocole complet"),
              DTOutput("table_robustesse_delta"),
              p(class = "text-muted small mt-2",
                "L'amplitude totale entre la meilleure règle et la pire est de ",
                "l'ordre du millième de point de ratio médian : le réglage de δ ",
                "ne décide de rien. À performance égale, la constante est ",
                "préférée — un paramètre estimé de moins, donc un risque de ",
                "surajustement de moins, et un chiffre publié qui ne dépend plus ",
                "d'une optimisation invisible."))
        )
      ),
      nav_panel(
        "Branches non couvertes",
        p(class = "text-muted",
          "Quatre branches n'ont aucun indicateur infra-trimestriel dans le ",
          "vivier : services aux entreprises, administration publique, ",
          "éducation-santé, autres services. Elles reçoivent la prévision du ",
          "modèle vectoriel, sans traitement particulier — il n'y a pas de ",
          "quatrième étage autorégressif dans l'architecture."),
        p(class = "text-muted",
          "Ce n'est pas un défaut de conception mais un résultat mesuré : huit ",
          "modèles ont été mis en concurrence sur ces branches, sur l'ensemble ",
          "du protocole. L'autorégression d'ordre 4 n'arrive jamais première, et ",
          "sur services aux entreprises c'est le modèle vectoriel lui-même qui ",
          "l'emporte. Les vainqueurs diffèrent d'une branche à l'autre et aucun ",
          "ne domine : les substituer sur la foi d'un classement établi sur 48 ",
          "points reviendrait à sélectionner sur l'échantillon d'évaluation."),
        DTOutput("table_ar")
      ),
      nav_panel(
        "Agrégation",
        p(class = "text-muted",
          "Indice de volume de Laspeyres : les prévisions de branche sont ",
          "agrégées avec les poids en valeur du trimestre précédent, jamais ceux ",
          "du trimestre visé, qui ne sont pas connus. L'écart à une agrégation ",
          "linéaire mesure l'inégalité de Jensen, c'est-à-dire ce que coûterait ",
          "l'approximation."),
        layout_columns(
          col_widths = c(6, 6),
          div(h6("Poids sectoriels, dernier trimestre disponible"),
              plotlyOutput("plot_poids", height = "440px")),
          div(h6("Écart entre formule exacte et formule linéaire"),
              DTOutput("table_ecart_formules"),
              h6(class = "mt-3", "Voie directe contre voie indirecte"),
              p(class = "text-muted small",
                "Prévoir l'agrégat directement, ou prévoir les seize branches ",
                "puis les agréger. Les deux voies ont été mesurées sur le même ",
                "protocole."),
              DTOutput("table_direct_indirect"))
        )
      )
    )
  ),

  # -------------------------------------------------------------- INCERTITUDE
  nav_panel(
    title = "Incertitude", icon = icon("chart-area"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Intervalle publié pour le trimestre en cours"),
        plotlyOutput("plot_fan", height = "330px"),
        p(class = "text-muted small",
          "Ces bornes ne sont pas déduites d'une hypothèse de loi. Elles sont ",
          "calibrées sur les erreurs effectivement commises par le système lors ",
          "du test en temps réel simulé, puis vérifiées par leur taux de ",
          "couverture constaté.")
      ),
      card(
        card_header("Couverture effective des intervalles"),
        DTOutput("table_couverture"),
        p(class = "text-muted small mt-2",
          "Un intervalle annoncé à 80 % doit contenir la réalisation dans ",
          "environ 80 % des cas. L'écart mesure ce qui sépare la couverture ",
          "constatée de la couverture annoncée.")
      )
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Enveloppe prédictive sur tout le protocole"),
        plotlyOutput("plot_enveloppe", height = "380px")
      ),
      card(
        card_header("D'où vient l'incertitude"),
        uiOutput("bloc_decomposition"),
        hr(),
        h6("Composantes de l'intervalle du trimestre"),
        DTOutput("table_intervalle_comp")
      )
    )
  ),

  # --------------------------------------------------------------- VALIDATION
  nav_panel(
    title = "Validation", icon = icon("check-double"),
    uiOutput("bandeau_protocole"),
    navset_card_tab(
      nav_panel(
        "Backtest",
        p(class = "text-muted",
          "Quarante-huit trimestres en temps réel simulé : à chaque origine, le ",
          "système ne dispose que de ce qui était connu à cette date."),
        plotlyOutput("plot_backtest", height = "360px"),
        hr(),
        layout_columns(
          col_widths = c(6, 6),
          plotlyOutput("plot_nuage", height = "340px"),
          plotlyOutput("plot_erreurs", height = "340px"))
      ),
      nav_panel(
        "Face aux étalons",
        p(class = "text-muted",
          "Le ratio rapporte l'erreur quadratique moyenne à l'écart-type de la ",
          "série visée. Sous 1, le système fait mieux que de prédire la moyenne ",
          "historique : c'est la référence la plus sévère, et la seule qui ",
          "compte."),
        layout_columns(
          col_widths = c(7, 5),
          DTOutput("table_benchmarks"),
          div(h6("Test de Diebold-Mariano contre chaque étalon"),
              p(class = "text-muted small",
                "Avec la correction de petit échantillon de Harvey, Leybourne et ",
                "Newbold (1997). Quarante-huit points, c'est peu : un écart non ",
                "significatif ne dit pas que les méthodes se valent, il dit que ",
                "l'échantillon ne permet pas de trancher."),
              DTOutput("table_dm_etalons"))
        )
      ),
      nav_panel(
        "Par épisode et par branche",
        layout_columns(
          col_widths = c(6, 6),
          div(h6("Comportement selon l'épisode"), DTOutput("table_episodes"),
              p(class = "text-muted small mt-2",
                "Le système apporte beaucoup pendant les chocs, et presque rien ",
                "sur les inflexions graduelles : les indicateurs sont ",
                "coïncidents, ils enregistrent les ruptures, pas les ",
                "retournements lents.")),
          div(h6("Qui porte l'erreur de l'agrégat"),
              plotlyOutput("plot_contrib_erreur", height = "420px"))
        ),
        hr(),
        h6("Qualité par branche et par modèle"),
        DTOutput("table_eval_branches")
      ),
      nav_panel(
        "Apport de l'information intra-trimestrielle",
        p(class = "text-muted",
          "Le scénario dit combien de mois du trimestre visé sont observés. ",
          "L'intérêt d'un nowcast se juge à ceci : le chiffre doit s'améliorer à ",
          "mesure que les mois arrivent."),
        layout_columns(
          col_widths = c(7, 5),
          plotlyOutput("plot_scenarios", height = "360px"),
          div(h6("Sensibilité au calendrier de publication"),
              p(class = "text-muted small",
                "Ce que devient la qualité si les indicateurs arrivent avec du ",
                "retard, ou si la valeur ajoutée elle-même est publiée plus tard."),
              DTOutput("table_calendrier"))
        ),
        hr(),
        DTOutput("table_scenarios")
      ),
      nav_panel(
        "Contrôles d'antériorité",
        p(class = "text-muted",
          "Un protocole récursif se décrit facilement et se viole ",
          "silencieusement. Ces contrôles ne relisent pas la méthode : ils ",
          "mesurent le comportement du système, de sorte qu'une fuite ",
          "d'information se traduise par un échec chiffré et non par une ",
          "relecture attentive. Ils sont bloquants : la chaîne s'arrête si l'un ",
          "d'eux échoue."),
        uiOutput("bloc_controles"),
        DTOutput("table_controles"),
        hr(),
        h6("Contrôles du dernier recalcul"),
        p(class = "text-muted small",
          "Ceux du dessus portent sur le protocole, établi sur quarante-huit ",
          "origines lors de la dernière exécution complète. Ceux-ci portent sur ",
          "le calcul qui vient d'avoir lieu, c'est-à-dire sur le chiffre ",
          "affiché. Ils sont exécutés avant toute écriture : un chiffre dont un ",
          "contrôle échoue n'est ni archivé ni affiché."),
        uiOutput("bloc_controles_run"),
        DTOutput("table_controles_run"),
        hr(),
        h6("Le contrôle décisif"),
        p(class = "text-muted small",
          "Les contrôles ci-dessus vérifient la forme du calcul. Celui-ci en ",
          "vérifie le comportement : on saccage les données postérieures au ",
          "trimestre visé — signe inversé, facteur cinq, décalage de vingt — on ",
          "refait entièrement le nowcast, et l'on vérifie qu'il n'a pas bougé ",
          "d'un iota. Aucune relecture de code ne donne cette garantie. Il coûte ",
          "deux nowcasts complets, environ deux minutes et demie : il se lance ",
          "donc à la demande."),
        actionButton("btn_perturbation",
                     "Exécuter le contrôle par perturbation",
                     icon = icon("flask"), class = "btn-outline-primary"),
        uiOutput("bloc_perturbation")
      ),
      nav_panel(
        "Figures de la chaîne",
        selectInput("figure_choisie", "Figure", choices = NULL, width = "520px"),
        uiOutput("bloc_figure")
      )
    )
  ),

  # ------------------------------------------------------- AJOUT DE DONNEES
  nav_panel(
    title = "Mise à jour des données", icon = icon("circle-plus"),
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Nouvelle observation"),
        p(class = "text-muted small",
          "Le grand classeur n'est jamais modifié. Les observations saisies ici ",
          "sont rangées dans un fichier à part, conservées d'une session à ",
          "l'autre, et fusionnées aux données au moment du recalcul."),
        radioButtons("ajout_type", "Type de série",
                     choices = c("Indicateur infra-trimestriel" = "indicateur",
                                 "Valeur ajoutée en volume (cible)" = "va",
                                 "Valeur ajoutée nominale (poids)" = "nominale"),
                     selected = "indicateur"),
        selectInput("ajout_branche", "Branche", choices = TOUTES_BRANCHES_APP),
        conditionalPanel(
          "input.ajout_type == 'indicateur'",
          selectInput("ajout_serie", "Série", choices = NULL),
          uiOutput("ajout_info_serie"),
          dateInput("ajout_date_ind", "Fin de la période observée",
                    value = Sys.Date(), language = "fr", weekstart = 1)
        ),
        conditionalPanel(
          "input.ajout_type != 'indicateur'",
          selectInput("ajout_trimestre", "Trimestre", choices = NULL)
        ),
        conditionalPanel(
          "input.ajout_type == 'nominale'",
          p(class = "text-muted small",
            "Les poids d'agrégation sont la part de chaque branche dans la ",
            "valeur ajoutée nominale du trimestre. Ils ne se calculent que ",
            "lorsque les seize branches sont renseignées : une part établie sur ",
            "quinze branches serait fausse pour toutes.")
        ),
        numericInput("ajout_valeur", "Valeur", value = NA),
        uiOutput("avis_saisie"),
        actionButton("btn_ajouter", "Ajouter l'observation",
                     icon = icon("plus"), class = "btn-primary w-100"),
        hr(),
        actionButton("btn_recalculer",
                     "Recalculer le nowcast avec les données à jour",
                     icon = icon("rotate"), class = "btn-success w-100"),
        p(class = "text-muted small mt-2",
          "Le recalcul refait la chaîne entière pour le trimestre en cours : ",
          "environ une minute et demie, dans un processus séparé. ",
          "L'application reste utilisable pendant ce temps, et le calcul ",
          "survit à la fermeture de la fenêtre. Le trimestre visé et le nombre ",
          "de mois observés ne se choisissent pas, ils se constatent."),
        uiOutput("bloc_avancement"),
        uiOutput("statut_recalcul")
      ),
      card(
        card_header("Où en est le trimestre"),
        uiOutput("bloc_etat_trimestre")
      ),
      card(
        card_header("Effet du dernier recalcul"),
        uiOutput("bloc_comparaison"),
        DTOutput("table_comparaison")
      ),
      card(
        card_header("Observations ajoutées manuellement"),
        h6("Valeur ajoutée en volume"),
        DTOutput("table_ajouts_va"),
        hr(),
        h6("Valeur ajoutée nominale"),
        DTOutput("table_ajouts_nom"),
        hr(),
        h6("Indicateurs"),
        DTOutput("table_ajouts_ind"),
        hr(),
        actionButton("btn_vider", "Revenir aux seules données du classeur",
                     icon = icon("trash-can"), class = "btn-outline-danger"),
        hr(),
        h6("Journal des exécutions"),
        p(class = "text-muted small",
          "Chaque recalcul laisse une trace horodatée : c'est ce qui permet de ",
          "reconstituer après coup le chiffre annoncé à une date donnée."),
        DTOutput("table_journal")
      )
    )
  ),

  # ------------------------------------------------------------------ EXPORT
  nav_panel(
    title = "Export", icon = icon("download"),
    card(
      card_header("Note de diffusion"),
      p(class = "text-muted",
        "Un seul classeur, autoportant : le chiffre du trimestre, son ",
        "intervalle recentré, la décomposition par branche, la qualité mesurée ",
        "sur l'historique et les contrôles d'antériorité. De quoi défendre le ",
        "chiffre sans revenir à l'application."),
      div(
        downloadButton("dl_classeur", "Télécharger le classeur (xlsx)",
                       class = "btn-primary"),
        tags$span(style = "display:inline-block;width:14px;"),
        downloadButton("dl_csv", "Détail par branche (csv)")),
      hr(),
      h6("Ce que contient le classeur"),
      uiOutput("bloc_contenu_export")
    )
  ),

  # ------------------------------------------------------------------ SOURCES
  nav_panel(
    title = "Sources", icon = icon("database"),
    p(class = "text-muted",
      "Périmètre, période couverte et statut de chaque série : les seize ",
      "variables cibles et le vivier d'indicateurs infra-trimestriels."),
    card(
      card_header("Variables cibles : valeur ajoutée par branche"),
      DTOutput("table_sources_va")
    ),
    card(
      card_header("Vivier d'indicateurs infra-trimestriels"),
      p(class = "text-muted small",
        "Le vivier est constitué du classeur issu du tri économique — aucun ",
        "critère statistique n'y intervient. Toute la sélection statistique est ",
        "faite ensuite, à l'intérieur du protocole, et refaite à chaque origine."),
      DTOutput("table_sources_ind")
    )
  ),

  # ----------------------------------------------------------------- RAPPORTS
  nav_panel(
    title = "Rapport", icon = icon("file-lines"),
    if (RAPPORT_PRESENT) {
      div(style = "margin: 0 -0.75rem;", uiOutput("bloc_rapport"))
    } else {
      p(class = "text-muted", "Le rapport n'a pas encore été produit.")
    }
  ),

  # ------------------------------------------------------------------ A PROPOS
  nav_panel(
    title = "À propos", icon = icon("circle-info"),
    card(
      card_header("Méthodologie"),
      uiOutput("bloc_methodo")
    )
  ),

  nav_spacer(),
  nav_item(input_dark_mode(id = "mode_sombre", mode = "light")),
  footer = tags$footer(class = "app-footer",
    "Nowcast sectoriel de la valeur ajoutée — nomenclature HCP à seize branches. ",
    "Le classeur source est lu, jamais modifié.")
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {

  sorties  <- reactiveVal(SORTIES_BASE)
  produit  <- reactiveVal(NULL)
  erreur   <- reactiveVal(NULL)
  ajouts   <- reactiveVal(Sys.time())    # jeton de rafraichissement

  courant <- reactive({ produit(); nowcast_courant(sorties()) })

  # --------------------------------------------------------------------------
  # COULEURS DES GRAPHIQUES, ADAPTEES AU MODE CLAIR / SOMBRE
  # --------------------------------------------------------------------------
  # input_dark_mode() ne change que le CSS côté client ; les graphiques sont
  # rendus côté serveur et doivent donc être re-thématisés explicitement à
  # chaque bascule, sinon leur fond reste blanc en mode sombre.
  mode_graph <- reactive({
    sombre <- identical(input$mode_sombre, "dark")
    list(sombre = sombre,
         fg = if (sombre) "#E5E7EB" else "#1F2937",
         grille = if (sombre) "#333A44" else "#E5E7EB")
  })

  themer_plotly <- function(p) {
    mg <- mode_graph()
    p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = mg$fg),
      xaxis = list(gridcolor = mg$grille, zerolinecolor = mg$grille, color = mg$fg),
      yaxis = list(gridcolor = mg$grille, zerolinecolor = mg$grille, color = mg$fg),
      legend = list(font = list(color = mg$fg)))
  }

  theme_gg_mode <- function() {
    mg <- mode_graph()
    theme(panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA),
          panel.grid = element_line(color = mg$grille),
          panel.grid.minor = element_blank(),
          text = element_text(color = mg$fg),
          axis.text = element_text(color = mg$fg))
  }

  gg <- function(p) themer_plotly(ggplotly(p, tooltip = "text"))

  # ==========================================================================
  # ETAT DU SYSTEME
  # ==========================================================================
  diagnostic <- reactive(diagnostic_sorties(sorties(), courant()))

  output$bandeau_etat <- renderUI({
    d <- diagnostic()
    blocs <- list()
    if (length(d$manquants)) {
      blocs <- c(blocs, list(div(class = "alert alert-warning py-2 small",
        tags$b("Sorties de la chaîne absentes : "),
        paste(d$manquants, collapse = ", "),
        ". Les onglets concernés resteront vides tant que le pipeline n'aura ",
        "pas été exécuté.")))
    }
    if (isTRUE(d$recalcul_posterieur)) {
      blocs <- c(blocs, list(div(class = "alert alert-info py-2 small",
        tags$b("Le nowcast affiché a été recalculé par l'application. "),
        sprintf("Les mesures de qualité, les contrôles d'antériorité et la largeur des intervalles décrivent la dernière exécution complète du protocole, le %s.",
                d$date_protocole))))
    }
    if (!length(blocs)) return(NULL)
    tagList(blocs)
  })

  output$bandeau_protocole <- renderUI({
    d <- diagnostic()
    if (is.na(d$date_protocole)) return(NULL)
    p(class = "text-muted small",
      sprintf("Ces mesures portent sur la dernière exécution complète du protocole, le %s. Un recalcul depuis l'application produit un chiffre, il ne réévalue pas la méthode.",
              d$date_protocole))
  })

  # ==========================================================================
  # TABLEAU DE BORD
  # ==========================================================================
  output$vb_nowcast <- renderText({
    c0 <- courant(); if (is.null(c0)) return("indisponible")
    sprintf("%+.2f %%", c0$agregat$nowcast_pct[1])
  })
  output$vb_trimestre <- renderText({
    c0 <- courant(); if (is.null(c0)) return("—")
    c0$agregat$trimestre[1]
  })
  output$vb_scenario <- renderUI({
    c0 <- courant(); if (is.null(c0)) return(NULL)
    sc <- c0$agregat$scenario[1]
    p(class = "small mb-0", sprintf("%s : %s", sc, LIBELLE_SCENARIO[[sc]]))
  })
  output$vb_intervalle <- renderText({
    p <- intervalle_courant(sorties(), courant())
    if (is.null(p)) return("—")
    p <- p %>% filter(niveau == 0.8)
    if (!nrow(p)) return("—")
    sprintf("%+.2f à %+.2f %%", p$bas_pct[1], p$haut_pct[1])
  })
  output$vb_maj <- renderText({
    c0 <- courant(); if (is.null(c0)) return("—")
    format(as.POSIXct(c0$horodatage), "%d/%m/%Y %H:%M")
  })
  output$vb_origine <- renderUI({
    c0 <- courant(); if (is.null(c0)) return(NULL)
    p(class = "small mb-0", sprintf("produit par : %s", c0$origine_calcul))
  })

  output$plot_contributions <- renderPlotly({
    c0 <- courant(); validate(need(!is.null(c0), "Aucun nowcast disponible."))
    df <- c0$branches %>%
      mutate(contribution = w * nowcast_pct,
             groupe = ifelse(source == "combinaison",
                             "Combinaison (passerelle + BVAR)", "BVAR seul")) %>%
      arrange(contribution) %>%
      mutate(branche = factor(branche, levels = branche))
    p <- ggplot(df, aes(branche, contribution, fill = groupe,
        text = sprintf("%s<br>Contribution : %+.3f point<br>Prévision : %+.2f %%<br>Poids : %.1f %%",
                       branche, contribution, nowcast_pct, 100 * w))) +
      geom_col() + coord_flip() +
      scale_fill_manual(values = c("Combinaison (passerelle + BVAR)" = COULEUR_PRIMAIRE,
                                   "BVAR seul" = COULEUR_SECONDAIRE)) +
      labs(x = NULL, y = "Contribution à la croissance de l'agrégat (point)",
           fill = NULL) +
      theme_minimal(base_size = 11) + theme(legend.position = "top") +
      theme_gg_mode()
    gg(p) %>% layout(legend = list(orientation = "h", y = 1.08))
  })

  output$plot_groupes <- renderPlotly({
    c0 <- courant(); validate(need(!is.null(c0), "Aucun nowcast."))
    mg <- mode_graph()
    df <- c0$branches %>% count(source, name = "n")
    plot_ly(df, labels = ~source, values = ~n, type = "pie", hole = 0.55,
            marker = list(colors = c(COULEUR_SECONDAIRE, COULEUR_PRIMAIRE),
                          line = list(color = if (mg$sombre) "#12161C" else "#FFFFFF",
                                      width = 1)),
            textinfo = "label+value", textfont = list(color = mg$fg),
            showlegend = FALSE) %>%
      layout(margin = list(l = 10, r = 10, t = 10, b = 10),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
             font = list(color = mg$fg))
  })

  output$bloc_intervalle <- renderUI({
    p <- intervalle_courant(sorties(), courant()); if (is.null(p)) return(NULL)
    tagList(
      h6("Marge d'incertitude"),
      tags$table(class = "tabl-intervalle", tags$tbody(lapply(seq_len(nrow(p)),
        function(i) tags$tr(
          tags$td(sprintf("%.0f %%", 100 * p$niveau[i])),
          tags$td(style = "font-variant-numeric:tabular-nums;",
                  sprintf("de %+.2f %% à %+.2f %%", p$bas_pct[i], p$haut_pct[i])))))),
      if (any(p$recentre)) p(class = "text-muted small mt-2",
        "Le nowcast a été recalculé depuis la dernière calibration : les écarts ",
        "calibrés ont été recentrés sur le chiffre affiché. Leur largeur, elle, ",
        "date de la dernière exécution complète du protocole."))
  })

  output$plot_recent <- renderPlotly({
    s <- sorties(); c0 <- courant()
    validate(need(!is.null(s$agregat), "Agrégat historique indisponible."))
    a <- s$agregat %>% arrange(origine) %>% tail(8) %>%
      transmute(trimestre, valeur = 100 * reel_niveau, type = "réalisé")
    if (!is.null(c0)) a <- bind_rows(a, tibble(
      trimestre = c0$agregat$trimestre[1],
      valeur = c0$agregat$nowcast_pct[1], type = "nowcast"))
    a$trimestre <- factor(a$trimestre, levels = a$trimestre)
    p <- ggplot(a, aes(trimestre, valeur, fill = type,
                       text = sprintf("%s : %+.2f %%", trimestre, valeur))) +
      geom_hline(yintercept = 0, color = "grey60", linewidth = .3) +
      geom_col(width = .7) +
      scale_fill_manual(values = c("réalisé" = COULEUR_SECONDAIRE,
                                   "nowcast" = COULEUR_PRIMAIRE)) +
      labs(x = NULL, y = "%", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme_gg_mode()
    gg(p) %>% layout(legend = list(orientation = "h", y = -0.35))
  })

  output$bloc_lecture <- renderUI({
    c0 <- courant(); s <- sorties(); if (is.null(c0)) return(NULL)
    ratio <- NA_real_
    if (!is.null(s$benchmarks)) {
      b <- s$benchmarks %>% filter(periode == "toutes origines",
                                   modele == "Systeme complet")
      if (nrow(b)) ratio <- b$ratio[1]
    }
    sig <- sum(c0$branches$signale, na.rm = TRUE)
    prec <- NULL
    if (!is.null(s$agregat)) {
      a <- s$agregat %>% arrange(origine)
      if (nrow(a)) prec <- list(t = a$trimestre[nrow(a)],
                                v = 100 * a$reel_niveau[nrow(a)])
    }
    tagList(
      p(class = "text-muted",
        "Ce chiffre est une ", tags$em("estimation avancée"), ": il vise un ",
        "trimestre dont la valeur ajoutée n'est pas encore publiée, à partir des ",
        "indicateurs déjà disponibles. Il ne remplace pas la publication ",
        "officielle, il l'anticipe."),
      if (!is.null(prec)) p(class = "text-muted",
        sprintf("Dernier trimestre réalisé, %s : %+.2f %%. Le nowcast est donc %s de %.2f point.",
                prec$t, prec$v,
                if (c0$agregat$nowcast_pct[1] >= prec$v) "au-dessus" else "en dessous",
                abs(c0$agregat$nowcast_pct[1] - prec$v))),
      p(class = "text-muted",
        sprintf("Sur les quarante-huit trimestres du test en temps réel simulé, l'erreur quadratique du système vaut %.2f fois l'écart-type de la série visée.",
                ratio),
        " Un rapport inférieur à un signifie que le système apporte quelque ",
        "chose face à la simple moyenne historique."),
      p(class = if (sig > 0) "signe-atypique" else "text-muted",
        if (sig == 0)
          "Aucune branche n'affiche ce trimestre une amplitude inhabituelle au regard de sa propre volatilité."
        else sprintf("%d branche(s) affichent une amplitude inhabituelle : elles sont signalées, sans être modifiées.", sig)))
  })

  output$table_previsions <- renderDT({
    c0 <- courant(); validate(need(!is.null(c0), "Aucun nowcast."))
    mg <- mode_graph()
    df <- c0$branches %>%
      mutate(contribution = w * nowcast_pct) %>%
      arrange(desc(contribution)) %>%
      transmute(Branche = branche, Traitement = source,
                `Poids (%)` = round(100 * w, 2),
                `BVAR (%)` = round(bvar_pct, 2),
                `Passerelle (%)` = round(bridge_pct, 2),
                `δ` = delta,
                `Nowcast (%)` = round(nowcast_pct, 2),
                `Contribution (pt)` = round(contribution, 3),
                `Indicateurs` = n_retenus,
                `Écart-type (%)` = round(ecart_type_pct, 2),
                `Amplitude (z)` = round(z_amplitude, 2),
                `Signalée` = ifelse(signale, "Oui", ""))
    couleurs <- if (mg$sombre) c("#1E3A5A", "#2A2E36") else c("#DDEBF7", "#F2F2F2")
    tbl(df, digits = 2, pageLength = 16) %>%
      DT::formatStyle("Traitement",
                      backgroundColor = DT::styleEqual(c("combinaison", "BVAR seul"),
                                                       couleurs),
                      color = mg$fg) %>%
      DT::formatStyle("Signalée", color = DT::styleEqual("Oui", COULEUR_ALERTE),
                      fontWeight = DT::styleEqual("Oui", "bold"))
  })

  # ==========================================================================
  # EXPLORATION
  # ==========================================================================
  output$explo_adf <- renderTable({
    s <- sorties(); req(s$stationnarite)
    d <- s$stationnarite %>% filter(branche == input$explo_branche)
    validate(need(nrow(d) > 0, ""))
    num <- names(d)[vapply(d, is.numeric, logical(1))]
    d %>% select(-branche) %>% mutate(across(all_of(num), ~round(.x, 3)))
  }, width = "100%")

  output$explo_desc <- renderTable({
    s <- sorties(); req(s$descriptives)
    d <- s$descriptives %>% filter(branche == input$explo_branche)
    validate(need(nrow(d) > 0, ""))
    num <- names(d)[vapply(d, is.numeric, logical(1))]
    d %>% transmute(
      `Trimestres` = n_trimestres,
      `Croissance moyenne (%)` = round(croissance_moy_pct, 2),
      `Volatilité (%)` = round(volatilite_pct, 2),
      `Minimum (%)` = round(min_pct, 1),
      `Maximum (%)` = round(max_pct, 1),
      `Part moyenne (%)` = round(100 * part_moyenne, 1)) %>%
      tidyr::pivot_longer(everything(), names_to = "Statistique",
                          values_to = "Valeur")
  }, width = "100%")

  output$plot_serie_branche <- renderPlotly({
    df <- DONNEES_BASE$va %>% filter(branche == input$explo_branche)
    y <- if (input$explo_transfo == "niveau") "va" else "dlog"
    df <- df %>% filter(!is.na(.data[[y]]))
    p <- ggplot(df, aes(date, .data[[y]],
        text = sprintf("%s<br>%s", lbl_trimestre(date),
                       if (y == "va") sprintf("%.0f Mdh", va)
                       else sprintf("%+.2f %%", 100 * dlog)))) +
      geom_hline(yintercept = if (y == "dlog") 0 else NA, color = "grey60") +
      geom_line(aes(group = 1), color = COULEUR_PRIMAIRE, linewidth = .6) +
      labs(x = NULL, y = if (y == "va") "Mdh" else "Δlog",
           title = input$explo_branche) +
      theme_minimal(base_size = 12) + theme_gg_mode()
    gg(p)
  })

  output$plot_correlation <- renderPlotly({
    mg <- mode_graph()
    m <- DONNEES_BASE$va %>% select(branche, date, dlog) %>%
      tidyr::pivot_wider(names_from = branche, values_from = dlog) %>%
      arrange(date)
    mc <- cor(m[, -1], use = "pairwise.complete.obs")
    fond <- if (mg$sombre) "#1A1F27" else "white"
    plot_ly(x = colnames(mc), y = rownames(mc), z = mc, type = "heatmap",
            colors = colorRamp(c(COULEUR_ACCENT, fond, COULEUR_PRIMAIRE)),
            zmin = -1, zmax = 1) %>%
      layout(margin = list(l = 170, b = 170),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
             font = list(color = mg$fg),
             xaxis = list(color = mg$fg), yaxis = list(color = mg$fg))
  })

  output$plot_indicateurs_cible <- renderPlotly({
    b <- input$explo_branche_couverte; req(b)
    # Seuls les indicateurs effectivement retenus au moins une fois par le
    # protocole sont montrés : le vivier complet compte des centaines de séries,
    # et les afficher toutes ne dirait rien de ce que le modèle utilise.
    s <- sorties()
    retenus <- if (is.null(s$selection_origine)) character(0) else
      s$selection_origine %>% filter(branche == b, retenu) %>%
        count(id_serie, sort = TRUE) %>% head(9) %>% pull(id_serie)
    validate(need(length(retenus) > 0,
                  "Aucun indicateur n'a été retenu pour cette branche."))
    ind <- DONNEES_BASE$indicateurs %>% filter(id_serie %in% retenus) %>%
      mutate(trimestre = fin_de_trimestre(date)) %>%
      group_by(id_serie, indicateur, trimestre) %>%
      summarise(x = mean(valeur), .groups = "drop") %>%
      group_by(id_serie) %>% arrange(trimestre) %>%
      mutate(dx = c(NA, diff(x))) %>% ungroup()
    cib <- DONNEES_BASE$va %>% filter(branche == b) %>%
      select(trimestre = date, dlog)
    df <- ind %>% inner_join(cib, by = "trimestre") %>% filter(!is.na(dx), !is.na(dlog))
    validate(need(nrow(df) > 0, "Aucun recouvrement temporel."))
    df$court <- substr(df$indicateur, 1, 34)
    p <- ggplot(df, aes(dx, 100 * dlog, text = sprintf("%s<br>%s", indicateur,
                                                       lbl_trimestre(trimestre)))) +
      geom_point(color = COULEUR_PRIMAIRE, alpha = .6, size = 1.4) +
      geom_smooth(aes(group = 1), method = "lm", formula = y ~ x, se = FALSE,
                  color = COULEUR_ACCENT, linewidth = .5) +
      facet_wrap(~court, scales = "free_x") +
      labs(x = "variation de l'indicateur agrégé au trimestre",
           y = "croissance de la VA (%)") +
      theme_minimal(base_size = 9) + theme_gg_mode()
    gg(p)
  })

  output$plot_ragged <- renderPlotly({
    d <- DONNEES_BASE$indicateurs %>%
      group_by(branche, frequence) %>%
      summarise(fin = max(date), n = n_distinct(id_serie), .groups = "drop")
    p <- ggplot(d, aes(fin, reorder(branche, as.numeric(fin)), color = frequence,
        text = sprintf("%s<br>%s : dernière observation %s<br>%d séries",
                       branche, frequence, format(fin, "%b %Y"), n))) +
      geom_point(size = 3, alpha = .85) +
      scale_color_manual(values = c(mensuel = COULEUR_PRIMAIRE,
                                    trimestriel = COULEUR_ACCENT)) +
      labs(x = "dernière observation disponible", y = NULL, color = NULL) +
      theme_minimal(base_size = 11) + theme_gg_mode()
    gg(p)
  })

  # ==========================================================================
  # MODELE
  # ==========================================================================
  output$plot_hyper <- renderPlotly({
    s <- sorties(); validate(need(!is.null(s$hyperparametres), "Indisponible."))
    d <- s$hyperparametres %>% filter(selection != "fixe") %>%
      mutate(origine = as.Date(origine))
    validate(need(nrow(d) > 0, "Indisponible."))
    d2 <- d %>% select(origine, trimestre, p, lambda) %>%
      tidyr::pivot_longer(c(p, lambda), names_to = "parametre", values_to = "v") %>%
      mutate(parametre = recode(parametre, p = "ordre p",
                                lambda = "resserrement λ"))
    p <- ggplot(d2, aes(origine, v, text = sprintf("%s<br>%s = %.3f", trimestre,
                                                   parametre, v))) +
      geom_step(aes(group = 1), color = COULEUR_PRIMAIRE, linewidth = .6) +
      facet_wrap(~parametre, ncol = 1, scales = "free_y") +
      labs(x = NULL, y = NULL,
           title = "Hyperparamètres re-choisis à chaque origine") +
      theme_minimal(base_size = 10) + theme_gg_mode()
    gg(p)
  })

  output$table_dm_bvar <- renderDT({
    s <- sorties(); validate(need(!is.null(s$dm_bvar), "Indisponible."))
    tbl(s$dm_bvar %>% transmute(
      Spécification = specification, Branches = n_branches,
      `Alternative meilleure` = alternative_meilleure,
      `Référence meilleure` = reference_meilleure,
      `Non significatif` = non_significatif), pageLength = 8)
  })

  output$table_instabilite <- renderDT({
    s <- sorties(); validate(need(!is.null(s$instabilite), "Indisponible."))
    tbl(s$instabilite %>% arrange(desc(rapport_variance)) %>%
      transmute(Branche = branche,
                `Rapport de variance` = rapport_variance,
                `Écartée du BVAR` = ifelse(instable, "Oui", "")),
      digits = 1, pageLength = 16) %>%
      DT::formatStyle("Écartée du BVAR",
                      color = DT::styleEqual("Oui", COULEUR_ALERTE),
                      fontWeight = DT::styleEqual("Oui", "bold"))
  })

  output$plot_selection <- renderPlotly({
    s <- sorties(); req(input$bridge_branche)
    validate(need(!is.null(s$selection_origine), "Indisponible."))
    d <- s$selection_origine %>%
      filter(branche == input$bridge_branche, retenu) %>%
      mutate(origine = as.Date(origine), court = substr(id_serie, 1, 46))
    validate(need(nrow(d) > 0, "Aucun indicateur retenu pour cette branche."))
    p <- ggplot(d, aes(origine, court, colour = abs(correlation),
        text = sprintf("%s<br>%s<br>corrélation %+.2f (p = %.3f)",
                       id_serie, lbl_trimestre(origine), correlation, p_value))) +
      geom_point(shape = 15, size = 2.6) +
      scale_fill_gradient(low = "#CFE2F3", high = COULEUR_PRIMAIRE,
                          aesthetics = c("fill", "colour")) +
      labs(x = NULL, y = NULL, fill = "|r|",
           title = "Indicateurs retenus à chaque origine") +
      theme_minimal(base_size = 9) + theme_gg_mode()
    gg(p)
  })

  output$table_selection <- renderDT({
    s <- sorties(); req(input$bridge_branche)
    validate(need(!is.null(s$selection_origine), "Indisponible."))
    d <- s$selection_origine %>% filter(branche == input$bridge_branche) %>%
      mutate(origine = as.Date(origine))
    derniere <- max(d$origine)
    tbl(d %>% filter(origine == derniere) %>% arrange(rang) %>%
      transmute(Indicateur = sub("^.* :: ", "", id_serie),
                `Obs.` = n, Corrélation = correlation, `p` = p_value,
                Éligible = ifelse(eligible, "Oui", ""), Rang = rang,
                Retenu = ifelse(retenu, "Oui", "")),
      digits = 3, pageLength = 10, filtre = "top") %>%
      DT::formatStyle("Retenu", fontWeight = DT::styleEqual("Oui", "bold"))
  })

  output$plot_courbe_delta <- renderPlotly({
    s <- sorties(); validate(need(!is.null(s$courbe_delta), "Indisponible."))
    d <- s$courbe_delta %>% filter(periode == "toutes origines")
    p <- ggplot(d, aes(delta, ratio, text = sprintf("δ = %.2f<br>ratio %.4f",
                                                    delta, ratio))) +
      geom_line(aes(group = 1), color = COULEUR_PRIMAIRE, linewidth = .7) +
      geom_vline(xintercept = 0.5, color = COULEUR_ACCENT, linetype = "dashed") +
      labs(x = "δ (poids du BVAR)", y = "ratio d'erreur",
           title = "Perte du système pour chaque valeur de δ") +
      theme_minimal(base_size = 11) + theme_gg_mode()
    gg(p)
  })

  output$table_robustesse_delta <- renderDT({
    s <- sorties(); validate(need(!is.null(s$robustesse_delta), "Indisponible."))
    tbl(s$robustesse_delta %>% arrange(ratio_median) %>%
      transmute(Règle = regle, Branches = branches,
                `Ratio médian` = ratio_median, `Branches sous 1` = n_ok),
      digits = 4, pageLength = 8)
  })

  output$table_ar <- renderDT({
    s <- sorties(); validate(need(!is.null(s$eval_ar), "Indisponible."))
    d <- s$eval_ar %>% filter(periode == "toutes origines",
                              branche %in% BRANCHES_NON_COUV)
    if (!is.null(s$ordre_ar)) {
      # L'ordre est re-choisi par critere bayesien a chaque origine : on montre
      # celui de la derniere origine, et combien d'ordres distincts ont ete
      # retenus au fil du protocole.
      o <- s$ordre_ar %>% mutate(origine = as.Date(origine)) %>%
        group_by(branche) %>%
        summarise(`Ordre, dernière origine` = p_bic[which.max(origine)],
                  `Ordres distincts` = n_distinct(p_bic), .groups = "drop")
      d <- d %>% left_join(o, by = "branche")
    }
    tbl(d %>% arrange(ratio) %>%
          transmute(Branche = branche, Modèle = modele, n,
                    MAE = mae, Ratio = ratio, Corrélation = correlation,
                    across(any_of(c("Ordre, dernière origine",
                                    "Ordres distincts")))),
        digits = 3, pageLength = 12)
  })

  output$plot_poids <- renderPlotly({
    s <- sorties(); validate(need(!is.null(s$poids), "Indisponible."))
    d <- s$poids %>% mutate(date = as.Date(date)) %>% filter(date == max(date))
    p <- ggplot(d, aes(reorder(branche, w), 100 * w,
        text = sprintf("%s<br>%.2f %% de la valeur ajoutée<br>%s",
                       branche, 100 * w, lbl_trimestre(date)))) +
      geom_col(fill = COULEUR_PRIMAIRE, width = .7) + coord_flip() +
      labs(x = NULL, y = "part de la valeur ajoutée nominale (%)") +
      theme_minimal(base_size = 10) + theme_gg_mode()
    gg(p)
  })

  output$table_ecart_formules <- renderDT({
    s <- sorties(); validate(need(!is.null(s$ecart_formules), "Indisponible."))
    tbl(s$ecart_formules, digits = 5, pageLength = 6)
  })

  output$table_direct_indirect <- renderDT({
    s <- sorties(); validate(need(!is.null(s$direct_indirect), "Indisponible."))
    tbl(s$direct_indirect %>% filter(periode == "toutes origines") %>%
      transmute(Voie = voie, n, MAE, RMSE = RMSFE, Ratio = ratio,
                Corrélation = correl), digits = 3, pageLength = 6)
  })

  # ==========================================================================
  # INCERTITUDE
  # ==========================================================================
  output$plot_fan <- renderPlotly({
    d <- intervalle_courant(sorties(), courant())
    validate(need(!is.null(d), "Indisponible."))
    d <- d %>% mutate(niveau_lbl = sprintf("%.0f %%", 100 * niveau))
    p <- ggplot(d, aes(y = reorder(niveau_lbl, niveau),
        text = sprintf("%s : de %+.2f %% à %+.2f %%", niveau_lbl, bas_pct, haut_pct))) +
      geom_segment(aes(x = bas_pct, xend = haut_pct, yend = reorder(niveau_lbl, niveau)),
                   linewidth = 6, color = COULEUR_PRIMAIRE, alpha = .35) +
      geom_vline(aes(xintercept = nowcast_pct), color = COULEUR_PRIMAIRE,
                 linewidth = .8) +
      geom_vline(xintercept = 0, color = "grey60", linetype = "dashed") +
      labs(x = "croissance de la valeur ajoutée totale (%)",
           y = "niveau de confiance", title = d$trimestre[1]) +
      theme_minimal(base_size = 11) + theme_gg_mode()
    gg(p)
  })

  output$table_couverture <- renderDT({
    s <- sorties(); validate(need(!is.null(s$couverture_int), "Indisponible."))
    tbl(s$couverture_int %>% transmute(
      `Niveau annoncé (%)` = 100 * niveau, Échelle = echelle, n,
      `Couverture constatée (%)` = 100 * couverture,
      `Largeur moyenne (pt)` = 100 * largeur,
      `Écart (pt)` = 100 * ecart), digits = 1, pageLength = 8)
  })

  output$plot_enveloppe <- renderPlotly({
    s <- sorties(); validate(need(!is.null(s$predictive), "Indisponible."))
    d <- s$predictive %>% mutate(origine = as.Date(origine))
    p <- ggplot(d, aes(origine, text = sprintf(
        "%s<br>réalisé %+.2f %%<br>médiane %+.2f %%<br>80 %% : %+.2f à %+.2f",
        trimestre, 100 * reel, 100 * mediane, 100 * q10, 100 * q90))) +
      geom_ribbon(aes(ymin = 100 * q05, ymax = 100 * q95, group = 1),
                  fill = COULEUR_PRIMAIRE, alpha = .13) +
      geom_ribbon(aes(ymin = 100 * q10, ymax = 100 * q90, group = 1),
                  fill = COULEUR_PRIMAIRE, alpha = .22) +
      geom_line(aes(y = 100 * mediane, group = 1), color = COULEUR_PRIMAIRE,
                linewidth = .6) +
      geom_point(aes(y = 100 * reel), color = COULEUR_ALERTE, size = 1.3) +
      geom_hline(yintercept = 0, color = "grey60", linewidth = .3) +
      labs(x = NULL, y = "%",
           title = "Enveloppe prédictive et réalisations (points rouges)") +
      theme_minimal(base_size = 11) + theme_gg_mode()
    gg(p)
  })

  output$bloc_decomposition <- renderUI({
    s <- sorties(); if (is.null(s$decomposition)) return(NULL)
    d <- s$decomposition
    tagList(
      p(class = "text-muted",
        "L'incertitude d'un nowcast a deux sources : celle des chocs à venir, ",
        "que rien ne peut réduire, et celle des coefficients, qui vient de ce ",
        "qu'on les estime sur un échantillon fini."),
      tags$table(class = "table table-sm",
        tags$tr(tags$td("Écart-type prédictif total"),
                tags$td(tags$b(sprintf("%.4f", d$sd_predictive[1])))),
        tags$tr(tags$td("dont incertitude des paramètres"),
                tags$td(tags$b(sprintf("%.4f", d$sd_parametrique[1])))),
        tags$tr(tags$td("part des paramètres"),
                tags$td(tags$b(sprintf("%.1f %%", 100 * d$part_parametrique[1]))))),
      p(class = "text-muted small",
        "L'essentiel de l'incertitude tient donc aux chocs, non à l'estimation : ",
        "allonger l'échantillon ne resserrerait guère l'intervalle."))
  })

  output$table_intervalle_comp <- renderDT({
    s <- sorties(); validate(need(!is.null(s$intervalle_comp), "Indisponible."))
    tbl(s$intervalle_comp %>% transmute(
      Trimestre = trimestre, Composante = composante, Médiane = mediane,
      `q10` = q10, `q90` = q90, `q05` = q05, `q95` = q95),
      digits = 2, pageLength = 8)
  })

  # ==========================================================================
  # VALIDATION
  # ==========================================================================
  output$plot_backtest <- renderPlotly({
    s <- sorties(); validate(need(!is.null(s$agregat), "Indisponible."))
    d <- s$agregat %>% mutate(origine = as.Date(origine)) %>%
      select(origine, trimestre, réalisé = reel_niveau, nowcast = nowcast_niveau,
             `BVAR seul` = bvar_niveau) %>%
      tidyr::pivot_longer(-c(origine, trimestre), names_to = "serie",
                          values_to = "v")
    p <- ggplot(d, aes(origine, 100 * v, color = serie,
        text = sprintf("%s<br>%s : %+.2f %%", trimestre, serie, 100 * v))) +
      geom_hline(yintercept = 0, color = "grey60", linewidth = .3) +
      geom_line(aes(group = serie), linewidth = .65) +
      scale_color_manual(values = c(réalisé = "#1F2937", nowcast = COULEUR_PRIMAIRE,
                                    `BVAR seul` = COULEUR_ACCENT)) +
      labs(x = NULL, y = "%", color = NULL) +
      theme_minimal(base_size = 11) + theme_gg_mode()
    gg(p) %>% layout(legend = list(orientation = "h", y = -0.16))
  })

  output$plot_nuage <- renderPlotly({
    s <- sorties(); validate(need(!is.null(s$agregat), "Indisponible."))
    p <- ggplot(s$agregat, aes(100 * reel_niveau, 100 * nowcast_niveau,
        text = sprintf("%s<br>réalisé %+.2f %%<br>prévu %+.2f %%", trimestre,
                       100 * reel_niveau, 100 * nowcast_niveau))) +
      geom_abline(slope = 1, color = "grey60", linetype = "dashed") +
      geom_point(color = COULEUR_PRIMAIRE, size = 1.9, alpha = .8) +
      labs(x = "réalisé (%)", y = "prévu (%)", title = "Prévu contre réalisé") +
      theme_minimal(base_size = 11) + theme_gg_mode()
    gg(p)
  })

  output$plot_erreurs <- renderPlotly({
    s <- sorties(); validate(need(!is.null(s$agregat), "Indisponible."))
    d <- s$agregat %>% mutate(origine = as.Date(origine),
                              err = 100 * (reel_niveau - nowcast_niveau))
    p <- ggplot(d, aes(origine, err,
        text = sprintf("%s<br>erreur %+.2f point", trimestre, err))) +
      geom_hline(yintercept = 0, color = "grey55") +
      geom_segment(aes(xend = origine, yend = 0), color = "grey70") +
      geom_point(color = COULEUR_ACCENT, size = 1.6) +
      labs(x = NULL, y = "réalisé moins prévu (point)",
           title = "Erreur de prévision") +
      theme_minimal(base_size = 11) + theme_gg_mode()
    gg(p)
  })

  output$table_benchmarks <- renderDT({
    s <- sorties(); validate(need(!is.null(s$benchmarks), "Indisponible."))
    tbl(s$benchmarks %>% filter(periode == "toutes origines") %>%
      transmute(Modèle = modele, n, MAE, RMSE = RMSFE, Biais = biais,
                Ratio = ratio, Corrélation = correlation) %>% arrange(Ratio),
      digits = 3, pageLength = 10) %>%
      DT::formatStyle("Ratio", fontWeight = DT::styleInterval(1, c("bold", "normal")),
                      color = DT::styleInterval(1, c(COULEUR_OK, COULEUR_ALERTE)))
  })

  output$table_dm_etalons <- renderDT({
    s <- sorties(); validate(need(!is.null(s$dm_etalons), "Indisponible."))
    tbl(s$dm_etalons %>% transmute(
      Étalon = etalon, n, `RMSE système` = RMSFE_systeme,
      `RMSE étalon` = RMSFE_etalon, `p` = p_value, Verdict = verdict),
      digits = 4, pageLength = 8)
  })

  output$table_episodes <- renderDT({
    s <- sorties(); validate(need(!is.null(s$episodes), "Indisponible."))
    tbl(s$episodes %>% transmute(
      Épisode = episode, n, `Croissance moyenne (%)` = 100 * croissance_moyenne,
      `RMSE système (pt)` = 100 * RMSFE_nowcast,
      `RMSE BVAR (pt)` = 100 * RMSFE_bvar,
      `RMSE AR(2) (pt)` = 100 * RMSFE_ar2,
      `Gain sur AR(2)` = gain_sur_ar2), digits = 2, pageLength = 8)
  })

  output$plot_contrib_erreur <- renderPlotly({
    s <- sorties(); validate(need(!is.null(s$contributions), "Indisponible."))
    d <- s$contributions
    p <- ggplot(d, aes(reorder(branche, contribution_abs), 100 * contribution_abs,
        text = sprintf("%s<br>contribution à l'erreur : %.3f point<br>poids moyen %.1f %%",
                       branche, 100 * contribution_abs, 100 * poids_moyen))) +
      geom_col(fill = COULEUR_ACCENT, width = .7) + coord_flip() +
      labs(x = NULL, y = "contribution à l'erreur de l'agrégat (point)") +
      theme_minimal(base_size = 10) + theme_gg_mode()
    gg(p)
  })

  output$table_eval_branches <- renderDT({
    s <- sorties(); validate(need(!is.null(s$eval_branches), "Indisponible."))
    tbl(s$eval_branches %>%
      transmute(Modèle = modele, Branche = branche, n, RMSE = RMSFE, MAE,
                Biais = biais, Ratio = ratio, Corrélation = correlation),
      digits = 3, pageLength = 16, filtre = "top")
  })

  output$plot_scenarios <- renderPlotly({
    s <- sorties(); validate(need(!is.null(s$scenarios), "Indisponible."))
    d <- s$scenarios %>% filter(periode == "toutes origines")
    p <- ggplot(d, aes(scenario, ratio_median, fill = modele,
        text = sprintf("%s — %s<br>ratio médian %.3f<br>%d branches sous 1",
                       scenario, modele, ratio_median, n_ratio_ok))) +
      geom_hline(yintercept = 1, color = "grey55", linetype = "dashed") +
      geom_col(position = "dodge", width = .7) +
      labs(x = NULL, y = "ratio médian", fill = NULL,
           title = "Qualité selon le nombre de mois observés") +
      theme_minimal(base_size = 11) + theme_gg_mode()
    gg(p) %>% layout(legend = list(orientation = "h", y = -0.16))
  })

  output$table_calendrier <- renderDT({
    s <- sorties(); validate(need(!is.null(s$calendrier), "Indisponible."))
    tbl(s$calendrier %>% transmute(
      `Retard (mois)` = retard_mois, Position = position,
      `Scénario effectif` = scenario_effectif,
      `Ratio combinaison` = ratio_combinaison, `Ratio BVAR` = ratio_bvar),
      digits = 3, pageLength = 8)
  })

  output$table_scenarios <- renderDT({
    s <- sorties(); validate(need(!is.null(s$scenarios), "Indisponible."))
    tbl(s$scenarios %>% filter(periode == "toutes origines") %>%
      transmute(Scénario = scenario, Modèle = modele, Branches = branches,
                `MAE moyenne` = mae_moyenne, `Ratio médian` = ratio_median,
                `Branches sous 1` = n_ratio_ok), digits = 3, pageLength = 12)
  })

  output$bloc_controles <- renderUI({
    s <- sorties(); if (is.null(s$controles)) return(NULL)
    n_ech <- sum(s$controles$resultat != "OK")
    div(class = if (n_ech == 0L) "alert alert-success" else "alert alert-danger",
        if (n_ech == 0L)
          sprintf("Les %d contrôles passent. Aucune information postérieure au trimestre visé n'entre dans le calcul.",
                  nrow(s$controles))
        else sprintf("%d contrôle(s) en échec. Le chiffre ne doit pas être diffusé.",
                     n_ech))
  })

  output$table_controles <- renderDT({
    s <- sorties(); validate(need(!is.null(s$controles), "Indisponible."))
    tbl(s$controles %>% transmute(Contrôle = controle, Objet = objet,
                                  Description = description, Résultat = resultat,
                                  Détail = detail), pageLength = 25) %>%
      DT::formatStyle("Résultat",
                      color = DT::styleEqual(c("OK", "ECHEC"),
                                             c(COULEUR_OK, COULEUR_ALERTE)),
                      fontWeight = "bold")
  })

  output$bloc_controles_run <- renderUI({
    produit()
    c_run <- lire(CHEMIN_CONTROLES)
    if (is.null(c_run)) return(p(class = "text-muted small",
      "Aucun recalcul n'a encore été effectué depuis l'application."))
    n_ech <- sum(c_run$resultat == "ECHEC")
    n_so <- sum(c_run$resultat %in% c("SANS OBJET", "À EXÉCUTER"))
    div(class = if (n_ech == 0L) "alert alert-success py-2 small"
                else "alert alert-danger py-2 small",
        if (n_ech == 0L)
          sprintf("Trimestre %s, scénario %s : %d contrôle(s) exercé(s) sans échec%s.",
                  c_run$trimestre[1], c_run$scenario[1], nrow(c_run) - n_so,
                  if (n_so > 0L) sprintf(", %d sans objet sur ce trimestre", n_so) else "")
        else sprintf("%d contrôle(s) en échec : le calcul a été refusé.", n_ech))
  })

  output$table_controles_run <- renderDT({
    produit()
    c_run <- lire(CHEMIN_CONTROLES)
    validate(need(!is.null(c_run), "Aucun recalcul effectué depuis l'application."))
    tbl(c_run %>% transmute(Contrôle = controle, Objet = objet,
                            Description = description, Résultat = resultat,
                            Détail = detail), pageLength = 10) %>%
      DT::formatStyle("Résultat",
        color = DT::styleEqual(c("OK", "ECHEC", "SANS OBJET", "À EXÉCUTER"),
                               c(COULEUR_OK, COULEUR_ALERTE, COULEUR_SECONDAIRE,
                                 COULEUR_ACCENT)),
        fontWeight = "bold")
  })

  output$bloc_perturbation <- renderUI({
    statut()
    r <- lire(CHEMIN_PERTURBATION)
    tagList(
      if (!is.null(r)) div(class = paste("mt-3 py-2 small alert",
          switch(r$resultat[1], OK = "alert-success", ECHEC = "alert-danger",
                 "alert-secondary")),
        tags$b(sprintf("%s — %s, scénario %s. ", r$resultat[1], r$trimestre[1],
                       r$scenario[1])),
        r$detail[1],
        tags$br(),
        tags$span(class = "text-muted", sprintf("Exécuté le %s.", r$horodatage[1])))
      else p(class = "text-muted small mt-2", "Contrôle jamais exécuté."))
  })

  observeEvent(input$btn_perturbation, ignoreInit = TRUE, {
    if (tache_en_cours()) {
      showNotification("Un calcul est déjà en cours.", type = "warning"); return()
    }
    lancer_production("perturbation")
    showNotification("Contrôle par perturbation lancé. Environ deux minutes et demie.",
                     type = "message", duration = 8)
  })

  observe({
    d <- file.path(RACINE, "figures")
    f <- if (dir.exists(d)) sort(list.files(d, pattern = "\\.png$")) else character(0)
    updateSelectInput(session, "figure_choisie", choices = f,
                      selected = if (length(f)) f[1] else NULL)
  })

  output$bloc_figure <- renderUI({
    req(input$figure_choisie)
    tags$img(src = file.path("figures", input$figure_choisie),
             style = "width:100%;border:1px solid #E5E7EB;border-radius:.4rem;")
  })

  # ==========================================================================
  # MISE A JOUR DES DONNEES
  # ==========================================================================
  observe({
    req(input$ajout_branche)
    series <- DONNEES_BASE$indicateurs %>%
      filter(branche == input$ajout_branche) %>%
      distinct(id_serie, indicateur) %>% arrange(indicateur)
    choix <- setNames(series$id_serie, series$indicateur)
    updateSelectInput(session, "ajout_serie", choices = choix)
  })

  output$ajout_info_serie <- renderUI({
    req(input$ajout_serie)
    d <- DONNEES_BASE$indicateurs %>% filter(id_serie == input$ajout_serie)
    m <- DONNEES_BASE$meta %>% filter(id_serie == input$ajout_serie)
    if (!nrow(d)) return(NULL)
    p(class = "text-muted small",
      sprintf("Fréquence %s. Dernière observation : %s. %d observations. %s",
              d$frequence[1], format(max(d$date), "%d/%m/%Y"), nrow(d),
              if (nrow(m)) sprintf("Agrégation « %s », transformation « %s ».",
                                   m$agregation[1], m$transformation[1]) else ""))
  })

  observe({
    req(input$ajout_branche)
    dernier <- DONNEES_BASE$va %>% filter(branche == input$ajout_branche) %>%
      pull(date) %>% max()
    aj <- lire_ajouts_va()
    if (!is.null(aj)) {
      d2 <- aj %>% filter(branche == input$ajout_branche) %>% pull(date)
      if (length(d2)) dernier <- max(dernier, max(d2))
    }
    suite <- as.Date(vapply(1:3, function(k) as.character(trimestre_suivant(dernier, k)),
                            character(1)))
    # Pour la VA nominale, le trimestre utile est d'abord celui qui vient d'etre
    # publie -- c'est de lui que se deduisent les poids de la cible suivante.
    if (identical(input$ajout_type, "nominale")) suite <- c(dernier, suite[1:2])
    choix <- setNames(as.character(suite), lbl_trimestre(suite))
    updateSelectInput(session, "ajout_trimestre", choices = choix,
                      selected = as.character(suite[1]))
  })

  # Les valeurs deja connues de l'objet saisi : elles servent au controle de
  # plausibilite et au reperage d'une periode deja observee.
  reference_saisie <- reactive({
    req(input$ajout_type, input$ajout_branche)
    if (input$ajout_type == "indicateur") {
      req(input$ajout_serie)
      d <- DONNEES_BASE$indicateurs %>% filter(id_serie == input$ajout_serie)
      aj <- lire_ajouts_ind()
      if (!is.null(aj)) {
        aj <- aj %>% filter(id_serie == input$ajout_serie)
        d <- bind_rows(d %>% select(date, valeur), aj %>% select(date, valeur))
      }
      dd <- as.Date(input$ajout_date_ind %||% Sys.Date())
      freq <- DONNEES_BASE$indicateurs$frequence[
        DONNEES_BASE$indicateurs$id_serie == input$ajout_serie][1]
      list(valeurs = d$valeur, dates = d$date,
           date = if (identical(freq, "trimestriel")) fin_de_trimestre(dd)
                  else fin_de_mois(dd))
    } else if (input$ajout_type == "va") {
      d <- DONNEES_BASE$va %>% filter(branche == input$ajout_branche)
      aj <- lire_ajouts_va()
      if (!is.null(aj)) {
        aj <- aj %>% filter(branche == input$ajout_branche)
        d <- bind_rows(d %>% select(date, va), aj %>% select(date, va))
      }
      list(valeurs = d$va, dates = d$date,
           date = as.Date(input$ajout_trimestre %||% NA))
    } else {
      s <- sorties()
      v <- if (is.null(s$poids)) NULL else
        s$poids %>% mutate(date = as.Date(date)) %>%
          filter(branche == input$ajout_branche)
      aj <- lire_ajouts_nominale()
      if (!is.null(aj)) {
        aj <- aj %>% filter(branche == input$ajout_branche)
        v <- bind_rows(v %>% select(date, va_nominale),
                       aj %>% select(date, va_nominale))
      }
      list(valeurs = if (is.null(v)) numeric(0) else v$va_nominale,
           dates = if (is.null(v)) as.Date(character(0)) else v$date,
           date = as.Date(input$ajout_trimestre %||% NA))
    }
  })

  avis_courants <- reactive({
    r <- reference_saisie()
    if (is.null(r) || is.na(input$ajout_valeur %||% NA)) return(list())
    verifier_saisie(r$valeurs, input$ajout_valeur, r$dates, r$date)
  })

  output$avis_saisie <- renderUI({
    a <- avis_courants()
    if (!length(a)) return(NULL)
    classe <- c(info = "alert-secondary", attention = "alert-warning",
                danger = "alert-danger")
    tagList(lapply(a, function(x)
      div(class = paste("alert py-2 px-2 small mb-2", classe[[x$niveau]]),
          x$texte)))
  })

  enregistrer_saisie <- function() {
    if (input$ajout_type == "va") {
      # La valeur ajoutée est datée au dernier jour de son trimestre, comme
      # partout dans le projet.
      ajouter_observation_va(input$ajout_branche, as.Date(input$ajout_trimestre),
                             input$ajout_valeur)
      showNotification(sprintf("Valeur ajoutée en volume enregistrée pour %s, %s.",
                               input$ajout_branche,
                               lbl_trimestre(as.Date(input$ajout_trimestre))),
                       type = "message")
    } else if (input$ajout_type == "nominale") {
      ajouter_observation_nominale(input$ajout_branche,
                                   as.Date(input$ajout_trimestre),
                                   input$ajout_valeur)
      showNotification(sprintf("Valeur ajoutée nominale enregistrée pour %s, %s.",
                               input$ajout_branche,
                               lbl_trimestre(as.Date(input$ajout_trimestre))),
                       type = "message")
    } else {
      d <- DONNEES_BASE$indicateurs %>% filter(id_serie == input$ajout_serie)
      dd <- reference_saisie()$date
      ajouter_observation_ind(input$ajout_serie, d$branche[1], d$indicateur[1],
                              d$frequence[1], dd, input$ajout_valeur)
      showNotification(sprintf("Observation enregistrée pour « %s » au %s.",
                               d$indicateur[1], format(dd, "%d/%m/%Y")),
                       type = "message")
    }
    updateNumericInput(session, "ajout_valeur", value = NA)
    ajouts(Sys.time())
  }

  observeEvent(input$btn_ajouter, {
    if (is.na(input$ajout_valeur)) {
      showNotification("Aucune valeur saisie.", type = "warning"); return()
    }
    # Une saisie franchement invraisemblable demande une confirmation : le
    # signalement a posteriori ne suffit pas, une faute de frappe se corrige.
    a <- avis_courants()
    if (any(vapply(a, function(x) identical(x$niveau, "danger"), logical(1)))) {
      showModal(modalDialog(
        title = "Confirmer une valeur inhabituelle",
        tagList(lapply(a, function(x) p(x$texte)),
                p(tags$b("Enregistrer tout de même cette valeur ?"))),
        footer = tagList(modalButton("Annuler"),
                         actionButton("btn_ajouter_ok", "Enregistrer",
                                      class = "btn-danger"))))
      return()
    }
    enregistrer_saisie()
  })

  observeEvent(input$btn_ajouter_ok, { removeModal(); enregistrer_saisie() })

  observeEvent(input$btn_vider, {
    showModal(modalDialog(
      title = "Revenir aux seules données du classeur",
      "Toutes les observations saisies dans l'application seront supprimées. ",
      "Le classeur source, lui, n'a jamais été modifié.",
      footer = tagList(modalButton("Annuler"),
                       actionButton("btn_vider_ok", "Supprimer",
                                    class = "btn-danger"))))
  })
  observeEvent(input$btn_vider_ok, {
    supprimer_ajouts(); ajouts(Sys.time()); removeModal()
    showNotification("Observations ajoutées supprimées.", type = "message")
  })

  output$bloc_etat_trimestre <- renderUI({
    ajouts(); produit()
    e <- etat_trimestre(DONNEES_BASE, sorties())
    ligne <- function(libelle, n, total, manque) {
      ok <- n == total
      tags$tr(
        tags$td(style = "padding-right:1.2rem;", libelle),
        tags$td(style = "padding-right:1.2rem;font-variant-numeric:tabular-nums;",
                tags$b(sprintf("%d / %d", n, total))),
        tags$td(style = sprintf("color:%s;", if (ok) COULEUR_OK else COULEUR_ACCENT),
                if (ok) "complet" else paste("manque :",
                  paste(utils::head(manque, 3), collapse = ", "),
                  if (length(manque) > 3) sprintf("(+%d)", length(manque) - 3) else "")))
    }
    tagList(
      p(class = "text-muted small",
        sprintf("Prochaine cible : %s. Son agrégation demande les poids du trimestre %s, lesquels se déduisent de la valeur ajoutée nominale.",
                lbl_trimestre(e$cible), lbl_trimestre(e$trimestre_poids))),
      tags$table(class = "table table-sm", tags$tbody(
        ligne(sprintf("VA en volume, %s", lbl_trimestre(e$trimestre_poids)),
              e$n_volume, e$n_branches, e$manque_volume),
        ligne(sprintf("VA nominale, %s", lbl_trimestre(e$trimestre_poids)),
              e$n_poids, e$n_branches, e$manque_poids))),
      div(class = if (e$pret) "alert alert-success py-2 small"
                  else "alert alert-warning py-2 small",
          if (e$pret)
            sprintf("Tout est en place pour produire le nowcast du %s.",
                    lbl_trimestre(e$cible))
          else "Tant que les seize branches ne sont pas renseignées, le recalcul échouera : une part calculée sur un trimestre incomplet serait fausse pour toutes les branches."))
  })

  output$bloc_comparaison <- renderUI({
    produit()
    cmp <- lire(CHEMIN_COMPARAISON)
    if (is.null(cmp) || !nrow(cmp)) {
      return(p(class = "text-muted small",
               "Aucune comparaison disponible : il faut deux productions ",
               "successives sur le même trimestre."))
    }
    sens <- if (cmp$ecart_agregat[1] >= 0) "au-dessus" else "en dessous"
    tagList(
      p(class = "text-muted small",
        sprintf("Trimestre %s. Le chiffre est passé de %+.4f %% à %+.4f %%, soit %.4f point %s.",
                cmp$trimestre[1], cmp$agregat_avant[1], cmp$agregat_apres[1],
                abs(cmp$ecart_agregat[1]), sens)),
      if (!identical(cmp$scenario_avant[1], cmp$scenario_apres[1]))
        div(class = "alert alert-info py-2 small",
            sprintf("Le scénario a changé : %s puis %s. Un mois de plus est observé, le système cesse donc de le prévoir.",
                    cmp$scenario_avant[1], cmp$scenario_apres[1])))
  })

  output$table_comparaison <- renderDT({
    produit()
    d <- lire(CHEMIN_COMPARAISON_DETAIL)
    validate(need(!is.null(d) && nrow(d) > 0, "Aucune comparaison disponible."))
    tbl(d %>% arrange(desc(abs(effet_agregat_pt))) %>%
      transmute(Branche = branche,
                `Avant (%)` = nowcast_avant, `Après (%)` = nowcast_apres,
                `Écart (pt)` = ecart_pct,
                `Effet sur l'agrégat (pt)` = effet_agregat_pt,
                `Traitement` = ifelse(traitement_change,
                                      paste(source_avant, "→", source_apres),
                                      source_apres)),
      digits = 3, pageLength = 8)
  })

  output$table_ajouts_nom <- renderDT({
    ajouts()
    a <- lire_ajouts_nominale()
    validate(need(!is.null(a), "Aucune valeur nominale saisie."))
    tbl(a %>% transmute(Branche = branche, Trimestre = lbl_trimestre(date),
                        `VA nominale (Mdh)` = va_nominale, `Saisi le` = saisi_le),
        digits = 1, pageLength = 5)
  })

  # ==========================================================================
  # EXPORT
  # ==========================================================================
  feuilles_export <- reactive({
    c0 <- courant(); s <- sorties(); req(c0)
    l <- list(
      Nowcast = c0$agregat,
      Branches = c0$branches %>% mutate(contribution_pt = w * nowcast_pct))
    iv <- intervalle_courant(s, c0)
    if (!is.null(iv)) l[["Intervalle"]] <- iv %>% select(-any_of("detail"))
    if (!is.null(s$couverture_int)) l[["Couverture"]] <- s$couverture_int
    if (!is.null(s$benchmarks))     l[["Qualite"]]    <- s$benchmarks
    if (!is.null(s$episodes))       l[["Episodes"]]   <- s$episodes
    if (!is.null(s$agregat))        l[["Historique"]] <- s$agregat
    if (!is.null(s$controles))      l[["Controles"]]  <- s$controles
    cd <- lire(CHEMIN_COMPARAISON_DETAIL)
    if (!is.null(cd)) l[["Comparaison"]] <- cd
    j <- lire(CHEMIN_JOURNAL)
    if (!is.null(j)) l[["Journal"]] <- j
    l
  })

  output$dl_classeur <- downloadHandler(
    filename = function() sprintf("nowcast_%s_%s.xlsx",
      gsub("[^A-Za-z0-9]", "", courant()$agregat$trimestre[1]),
      format(Sys.Date(), "%Y%m%d")),
    content = function(file) writexl::write_xlsx(feuilles_export(), file))

  output$dl_csv <- downloadHandler(
    filename = function() sprintf("nowcast_branches_%s.csv",
      gsub("[^A-Za-z0-9]", "", courant()$agregat$trimestre[1])),
    content = function(file) ecrire(courant()$branches %>%
      mutate(contribution_pt = w * nowcast_pct), file))

  output$bloc_contenu_export <- renderUI({
    tags$ul(class = "text-muted small",
            lapply(names(feuilles_export()), function(x) tags$li(tags$b(x))))
  })

  output$table_ajouts_va <- renderDT({
    ajouts()
    a <- lire_ajouts_va()
    validate(need(!is.null(a), "Aucune valeur ajoutée saisie."))
    tbl(a %>% transmute(Branche = branche, Trimestre = lbl_trimestre(date),
                        `VA (Mdh)` = va, `Saisi le` = saisi_le),
        digits = 1, pageLength = 5)
  })

  output$table_ajouts_ind <- renderDT({
    ajouts()
    a <- lire_ajouts_ind()
    validate(need(!is.null(a), "Aucun indicateur saisi."))
    tbl(a %>% transmute(Branche = branche, Indicateur = indicateur,
                        Fréquence = frequence, Date = format(date, "%Y-%m-%d"),
                        Valeur = valeur, `Saisi le` = saisi_le),
        digits = 3, pageLength = 5)
  })

  # ==========================================================================
  # RECALCUL, DANS UN PROCESSUS SEPARE
  # ==========================================================================
  # Le calcul ne s'execute plus dans la session : celle-ci se contente de le
  # lancer, puis de relire un fichier d'etat. L'application reste donc servie
  # pendant la minute et demie que dure le calcul, et le calcul survit a la
  # fermeture de la fenetre.
  statut <- reactivePoll(
    1000, session,
    checkFunc = function() if (file.exists(CHEMIN_STATUT))
      file.info(CHEMIN_STATUT)$mtime else 0,
    valueFunc = function() lire_statut())

  # Un etat passe a "termine" ou "erreur" : on relit les sorties une seule fois.
  dernier_etat <- reactiveVal(NA_character_)
  observe({
    st <- statut(); req(st)
    cle <- paste(st$etat[1], st$horodatage[1])
    if (identical(cle, dernier_etat())) return()
    precedent <- dernier_etat()
    dernier_etat(cle)
    if (is.na(precedent)) return()          # premier passage : rien a annoncer
    if (identical(st$etat[1], "termine")) {
      sorties(charger_sorties()); produit(Sys.time()); erreur(NULL)
      showNotification(st$message[1], type = "message", duration = 14)
    } else if (identical(st$etat[1], "erreur")) {
      erreur(st$message[1])
      showNotification("Le calcul n'a pas abouti.", type = "error", duration = 12)
    }
  })

  # Le bouton reste le meme objet ; seul son libelle suit l'etat de la tache.
  observe({
    en_cours <- tache_en_cours()
    updateActionButton(session, "btn_recalculer",
      label = if (en_cours) "Calcul en cours…"
              else "Recalculer le nowcast avec les données à jour",
      icon = if (en_cours) icon("hourglass-half") else icon("rotate"))
    updateActionButton(session, "btn_perturbation",
      label = if (en_cours) "Calcul en cours…"
              else "Exécuter le contrôle par perturbation",
      icon = if (en_cours) icon("hourglass-half") else icon("flask"))
  })

  output$bloc_avancement <- renderUI({
    st <- statut()
    if (is.null(st) || !identical(st$etat[1], "en_cours")) return(NULL)
    if (!tache_en_cours())
      return(div(class = "alert alert-warning py-2 small mt-3",
                 "Le calcul lancé le ", st$horodatage[1],
                 " ne donne plus signe de vie : le processus a probablement ",
                 "été interrompu. Un nouveau lancement est possible."))
    pct <- round(100 * st$part[1])
    div(class = "mt-3",
        div(class = "progress", style = "height:6px;",
            div(class = "progress-bar progress-bar-striped progress-bar-animated",
                role = "progressbar", style = sprintf("width:%d%%;", pct))),
        p(class = "text-muted small mt-1 mb-0",
          sprintf("%d %% — %s", pct, st$message[1])))
  })

  observeEvent(input$btn_recalculer, ignoreInit = TRUE, {
    if (tache_en_cours()) {
      showNotification("Un calcul est déjà en cours.", type = "warning")
      return()
    }
    erreur(NULL)
    lancer_production()
    showNotification("Calcul lancé dans un processus séparé. L'application reste utilisable.",
                     type = "message", duration = 8)
  })

  output$statut_recalcul <- renderUI({
    e <- erreur()
    if (!is.null(e)) return(div(class = "alert alert-danger mt-3 small",
                                tags$b("Le calcul n'a pas abouti. "), e))
    c0 <- courant(); if (is.null(c0)) return(NULL)
    aj_v <- lire_ajouts_va(); aj_i <- lire_ajouts_ind()
    p(class = "text-muted small mt-2",
      sprintf("Nowcast courant : %s, scénario %s, calculé le %s par %s. %d observation(s) de valeur ajoutée et %d d'indicateur ajoutées depuis l'interface.",
              c0$agregat$trimestre[1], c0$agregat$scenario[1], c0$horodatage,
              if (identical(c0$origine_calcul, "application")) "l'application"
              else "la chaîne",
              if (is.null(aj_v)) 0L else nrow(aj_v),
              if (is.null(aj_i)) 0L else nrow(aj_i)))
  })

  output$table_journal <- renderDT({
    produit()
    j <- lire(CHEMIN_JOURNAL)
    validate(need(!is.null(j), "Aucune exécution enregistrée."))
    # Un journal ecrit par une version anterieure peut ne pas porter toutes les
    # colonnes : on complete plutot que de refuser d'afficher l'historique.
    for (cc in c("obs_va_ajoutees", "obs_ind_ajoutees")) {
      if (is.null(j[[cc]])) j[[cc]] <- 0L
    }
    tbl(j %>% arrange(desc(horodatage)) %>% transmute(
      Horodatage = horodatage, Trimestre = trimestre, Scénario = scenario,
      `Nowcast (%)` = nowcast_pct, `BVAR (%)` = bvar_pct, `p` = p_bvar,
      `λ` = lambda_bvar, `Passerelles` = n_branches_passerelle,
      `Signalées` = n_signalees,
      `Obs. ajoutées` = obs_va_ajoutees + obs_ind_ajoutees,
      `Durée (s)` = duree_secondes), digits = 3, pageLength = 8)
  })

  # ==========================================================================
  # SOURCES
  # ==========================================================================
  output$table_sources_va <- renderDT({
    mg <- mode_graph()
    df <- tableau_sources_va(DONNEES_BASE) %>%
      transmute(Branche = branche,
                Groupe = ifelse(couverte, "Couverte", "Non couverte"),
                Début = lbl_trimestre(debut), Fin = lbl_trimestre(fin),
                `Nb obs.` = n)
    couleurs <- if (mg$sombre) c("#1E3A5A", "#2A2E36") else c("#DDEBF7", "#F2F2F2")
    tbl(df, pageLength = 16, filtre = "top") %>%
      DT::formatStyle("Groupe",
                      backgroundColor = DT::styleEqual(c("Couverte", "Non couverte"),
                                                       couleurs),
                      color = mg$fg)
  })

  output$table_sources_ind <- renderDT({
    df <- tableau_sources_indicateurs(DONNEES_BASE) %>%
      transmute(Branche = branche, Indicateur = indicateur,
                Fréquence = frequence,
                `Dans le vivier` = ifelse(retenu, "Oui", ""),
                Agrégation = agregation, Transformation = transformation,
                Rôle = role, Unité = unite,
                Début = format(debut, "%Y-%m"), Fin = format(fin, "%Y-%m"),
                `Nb obs.` = n_obs, `Manquant (%)` = 100 * taux_manquant,
                `Motif d'exclusion` = motif_exclusion)
    tbl(df, digits = 1, pageLength = 15, filtre = "top")
  })

  # ==========================================================================
  # RAPPORTS
  # ==========================================================================
  output$bloc_rapport <- renderUI({
    req(RAPPORT_PRESENT)
    tags$iframe(src = file.path("rapports", RAPPORT),
                style = "width:100%;height:86vh;border:none;background:#FFFFFF;")
  })

  # ==========================================================================
  # A PROPOS
  # ==========================================================================
  output$bloc_methodo <- renderUI({
    s <- sorties()
    ratio <- if (!is.null(s$benchmarks)) {
      b <- s$benchmarks %>% filter(periode == "toutes origines",
                                   modele == "Systeme complet")
      if (nrow(b)) b$ratio[1] else NA_real_
    } else NA_real_
    n_ind <- nrow(DONNEES_BASE$meta)
    n_ret <- sum(DONNEES_BASE$meta$retenu)
    markdown(sprintf("
**GDPNow-Maroc** adapte la logique de nowcasting de la Federal Reserve Bank
d'Atlanta (Higgins, 2014) à une approche par l'offre : la valeur ajoutée est
prévue branche par branche, dans la nomenclature du HCP à seize branches, puis
agrégée.

### Architecture

1. **BVAR trimestriel** à prior de Minnesota imposé par observations fictives
   (Litterman 1986 ; Bańbura, Giannone & Reichlin 2010), seize branches. L'ordre
   *p* et le resserrement *λ* sont re-choisis **à chaque origine** sur la seule
   information antérieure, par le critère d'ajustement de Bańbura, Giannone et
   Reichlin. Une branche dont la variance a rompu sort du BVAR et reçoit sa
   moyenne récente.
2. **Équations de passerelle** pour les douze branches couvertes : les
   indicateurs infra-trimestriels sont agrégés au trimestre, puis régressés sur
   la croissance de la valeur ajoutée, avec un terme autorégressif. La sélection
   est refaite à chaque origine et **ne considère que les séries effectivement
   disponibles à cette date** : le bord irrégulier est traité série par série.
3. **Combinaison** des deux prévisions à poids fixe **δ = 0,5**. Le choix est
   mesuré, pas décrété : sur l'ensemble du protocole, l'amplitude entre la
   meilleure règle de pondération et la pire est de l'ordre du millième de point
   de ratio médian. À performance égale, la constante est préférée — un paramètre
   estimé de moins, et un chiffre publié qui ne dépend plus d'une optimisation
   invisible.
4. **Là où la passerelle manque**, le nowcast de la branche est la prévision du
   modèle vectoriel. C'est le cas permanent de quatre branches sans indicateur
   dans le vivier, et le cas ponctuel de branches couvertes dont aucun indicateur
   n'est disponible à l'origine considérée. Il n'y a **pas de quatrième étage
   autorégressif** : huit modèles ont été comparés sur ces quatre branches, et
   aucun ne bat le vectoriel de façon assez nette et assez générale pour
   justifier une substitution.
5. **Agrégation** par indice de volume de Laspeyres, à poids en valeur du
   **trimestre précédent** — ceux du trimestre visé ne sont pas connus.

### Le vivier d'indicateurs

Le vivier est le classeur issu du **tri économique**, %d séries dont %d retenues.
Aucun critère statistique n'intervient dans sa constitution : la justification
est économique de bout en bout. Toute la sélection statistique vient ensuite, à
l'intérieur du protocole, et elle est **récursive**.

### La règle qui gouverne tout

À chaque origine *T*, **toute** quantité est recalculée sur la seule information
disponible à cette date : hyperparamètres, sélection des indicateurs,
coefficients, poids de combinaison, poids d'agrégation, moyennes et écarts-types
de standardisation. La même fonction produit le chiffre publié et chacun des 48
points du backtest : **le chiffre publié emprunte le chemin de code qui a été
évalué**. Des contrôles bloquants refusent l'exécution si une donnée postérieure
à *T* est touchée.

### Résultats

Sur 48 trimestres en temps réel simulé, le ratio d'erreur du système vaut
**%.3f** : le système fait mieux que la moyenne historique, mieux que le BVAR
agrégé seul et mieux que la marche aléatoire. Les intervalles sont calibrés sur
les erreurs constatées, et leur couverture effective est vérifiée.

### Ce que le système ne sait pas faire

- Il n'apporte presque rien sur les **ralentissements graduels** : les indicateurs
  sont coïncidents, ils enregistrent les chocs, pas les inflexions.
- Il est évalué contre des **comptes révisés**, que le modèle n'aurait pas eus en
  temps réel : la performance mesurée est donc un majorant.
- Quatre branches restent **sans indicateur infra-trimestriel**.
- L'échantillon de test compte 48 points : un écart non significatif au test de
  Diebold-Mariano ne dit pas que deux méthodes se valent, il dit que
  l'échantillon ne permet pas de trancher.

### Références principales

Higgins (2014, FRB Atlanta) ; Bańbura, Giannone & Reichlin (2010) ; Litterman
(1986) ; Giannone, Lenza & Primiceri (2015) ; Diebold & Mariano (1995) ; Harvey,
Leybourne & Newbold (1997) ; Bates & Granger (1969) ; Stock & Watson (2004) ;
Smith & Wallis (2009) ; Claeskens et al. (2016).
", n_ind, n_ret, ratio))
  })

  # Les tableaux des onglets Sources et Tableau de bord doivent être à jour dès
  # l'ouverture (et pas seulement calculés la première fois que l'onglet devient
  # visible), notamment pour que le thème clair/sombre choisi au départ s'y
  # applique immédiatement.
  for (o in c("table_sources_va", "table_sources_ind", "table_previsions")) {
    outputOptions(output, o, suspendWhenHidden = FALSE)
  }
}

shinyApp(ui, server)
