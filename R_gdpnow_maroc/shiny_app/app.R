# ============================================================================
# app.R -- Interface Shiny du modele de nowcasting sectoriel GDPNow-Maroc
# ============================================================================
# Permet de :
#   - visualiser le nowcast courant du PIB et sa decomposition par branche
#   - explorer les 16 series (niveaux, croissance, stationnarite, correlations)
#   - consulter le detail des modeles (BVAR, bridge equations, AR(4))
#   - valider le modele hors echantillon (RMSFE, test de Diebold-Mariano)
#   - AJOUTER DE NOUVELLES OBSERVATIONS (cible ou indicateur) au fil du temps
#     et relancer le pipeline complet pour mettre a jour le nowcast
# ============================================================================

Sys.setlocale("LC_ALL", "C.UTF-8")

suppressMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(stringr)
})

source("R/pipeline_fonctions.R")

CHEMIN_CLASSEUR <- "data/Series_retenues_modelisation.xlsx"
CHEMIN_AJOUTS_CIBLES <- "data/ajouts_cibles.csv"
CHEMIN_AJOUTS_INDICATEURS <- "data/ajouts_indicateurs.csv"

COULEUR_PRIMAIRE <- "#2E74B5"
COULEUR_SECONDAIRE <- "#7F7F7F"
COULEUR_ACCENT <- "#C55A11"
COULEUR_OK <- "#4C8C4A"
COULEUR_ALERTE <- "#C00000"

# ----------------------------------------------------------------------------
# THEME
# ----------------------------------------------------------------------------
theme_app <- bs_theme(
  version = 5,
  bg = "#FBFCFD", fg = "#1F2937",
  primary = COULEUR_PRIMAIRE, secondary = COULEUR_SECONDAIRE,
  success = COULEUR_OK, danger = COULEUR_ALERTE, warning = COULEUR_ACCENT,
  base_font = font_google("Inter", local = FALSE),
  heading_font = font_google("Inter", wght = 600, local = FALSE),
  "navbar-bg" = COULEUR_PRIMAIRE,
  "border-radius" = "0.6rem",
  "card-border-color" = "#E5E7EB"
) %>%
  bs_add_rules("
    .navbar-brand { font-weight: 700; letter-spacing: -0.02em; }
    .value-box-title { font-size: 0.82rem !important; opacity: 0.85; }
    .value-box-value { font-weight: 700 !important; }
    .card { box-shadow: 0 1px 3px rgba(0,0,0,0.06); }
    .card-header { font-weight: 600; background-color: #F8FAFC !important; }
    .badge-branche-couverte { background-color: #2E74B5; }
    .badge-branche-non-couverte { background-color: #7F7F7F; }
    .signe-atypique { color: #C00000; font-weight: 600; }
    footer.app-footer { color: #9CA3AF; font-size: 0.78rem; padding: 1.2rem 0; text-align: center; }
  ")

# ----------------------------------------------------------------------------
# CHARGEMENT DES DONNEES DE DEPART (au demarrage de l'app)
# ----------------------------------------------------------------------------
charger_donnees_completes <- function() {
  base <- importer_donnees(CHEMIN_CLASSEUR)
  ajouts_cibles <- if (file.exists(CHEMIN_AJOUTS_CIBLES)) {
    read.csv(CHEMIN_AJOUTS_CIBLES, stringsAsFactors = FALSE) %>% mutate(date = as.Date(date))
  } else NULL
  ajouts_indicateurs <- if (file.exists(CHEMIN_AJOUTS_INDICATEURS)) {
    read.csv(CHEMIN_AJOUTS_INDICATEURS, stringsAsFactors = FALSE) %>% mutate(date = as.Date(date))
  } else NULL
  fusionner_donnees_ajoutees(base$cibles, base$indicateurs, ajouts_cibles, ajouts_indicateurs)
}

# ============================================================================
# UI
# ============================================================================
ui <- page_navbar(
  title = "GDPNow-Maroc",
  theme = theme_app,
  window_title = "GDPNow-Maroc — Nowcasting sectoriel du PIB",
  fillable = FALSE,

  # -------------------------------------------------------------- DASHBOARD
  nav_panel(
    title = "Tableau de bord", icon = icon("gauge-high"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Nowcast du prochain trimestre", value = textOutput("vb_nowcast"),
                 showcase = icon("chart-line"), theme = "primary"),
      value_box(title = "Trimestre visé", value = textOutput("vb_trimestre"),
                 showcase = icon("calendar"), theme = "secondary"),
      value_box(title = "Branches avec indicateur", value = "7 / 16",
                 showcase = icon("layer-group"), theme = "success"),
      value_box(title = "Dernière mise à jour du modèle", value = textOutput("vb_maj"),
                 showcase = icon("clock"), theme = "secondary")
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Contribution de chaque branche au nowcast du PIB"),
        plotlyOutput("plot_contributions", height = "480px")
      ),
      card(
        card_header("Répartition par groupe de traitement"),
        plotlyOutput("plot_groupes", height = "220px"),
        hr(),
        p(class = "text-muted small",
          "Groupe A (bleu) : bridge equation + BVAR, combinés par moindres carrés restreints. ",
          "Groupe B (gris) : AR(4) pur, faute d'indicateur validé.")
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
        selectInput("explo_branche", "Branche", choices = TOUTES_BRANCHES, selected = "Pêche"),
        radioButtons("explo_transfo", "Affichage", choices = c("Niveau (VA, Mdh)" = "niveau",
                                                                 "Taux de croissance (Δlog)" = "dlog"),
                     selected = "dlog"),
        hr(),
        h6("Test de stationnarité (ADF)"),
        tableOutput("explo_adf")
      ),
      navset_card_tab(
        nav_panel("Trajectoire", plotlyOutput("plot_serie_branche", height = "420px")),
        nav_panel("Corrélation croisée entre branches", plotlyOutput("plot_correlation", height = "560px")),
        nav_panel("Indicateurs vs. cible (branches couvertes)",
                  selectInput("explo_branche_couverte", "Branche couverte", choices = BRANCHES_COUVERTES),
                  plotlyOutput("plot_indicateurs_vs_cible", height = "480px"))
      )
    )
  ),

  # ---------------------------------------------------------- MODELE
  nav_panel(
    title = "Modèle & prévisions", icon = icon("diagram-project"),
    navset_card_tab(
      nav_panel("BVAR trimestriel",
        p(class = "text-muted", "Modèle autorégressif bayésien à prior Minnesota (Litterman, 1986 ; ",
          "Bańbura, Giannone & Reichlin, 2010), 16 branches, 5 retards, λ = 0,15."),
        DTOutput("table_bvar")
      ),
      nav_panel("Équations de passerelle (bridge equations)",
        p(class = "text-muted", "Combinaison BVAR / bridge equation par moindres carrés restreints ",
          "(Higgins, 2014, équation 8). δ = poids attribué au BVAR."),
        DTOutput("table_bridge")
      ),
      nav_panel("AR(4) — branches non couvertes",
        p(class = "text-muted", "Prévision autorégressive pure, faute d'indicateur d'activité validé ",
          "(même traitement que Higgins, 2014, applique aux sous-composantes sans série mensuelle)."),
        DTOutput("table_ar4")
      )
    )
  ),

  # ---------------------------------------------------------- VALIDATION
  nav_panel(
    title = "Validation", icon = icon("check-double"),
    card(
      card_header("Backtest en pseudo temps réel"),
      layout_columns(
        col_widths = c(4, 8),
        div(
          sliderInput("backtest_h", "Nombre de trimestres testés", min = 4, max = 12, value = 8, step = 1),
          actionButton("lancer_backtest", "Lancer le backtest", icon = icon("play"), class = "btn-primary"),
          br(), br(),
          uiOutput("backtest_resume")
        ),
        plotlyOutput("plot_backtest", height = "380px")
      )
    )
  ),

  # ---------------------------------------------------------- AJOUT DE DONNEES
  nav_panel(
    title = "Ajouter des données", icon = icon("circle-plus"),
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Nouvelle observation"),
        radioButtons("ajout_type", "Type de série", choices = c("Variable cible (VA)" = "cible",
                                                                   "Indicateur" = "indicateur"), selected = "cible"),
        selectInput("ajout_branche", "Branche", choices = TOUTES_BRANCHES),
        conditionalPanel(
          "input.ajout_type == 'indicateur'",
          selectizeInput("ajout_indicateur", "Indicateur (existant ou nouveau)", choices = NULL,
                          options = list(create = TRUE, placeholder = "Sélectionner ou saisir un nom...")),
          selectInput("ajout_frequence", "Fréquence", choices = c("mensuel", "trimestriel")),
          textInput("ajout_source", "Source (institution productrice)", placeholder = "ex. Bank Al-Maghrib")
        ),
        conditionalPanel(
          "input.ajout_type == 'cible'",
          selectInput("ajout_trimestre", "Trimestre", choices = NULL)
        ),
        conditionalPanel(
          "input.ajout_type == 'indicateur'",
          dateInput("ajout_date", "Date de l'observation", value = Sys.Date())
        ),
        numericInput("ajout_valeur", "Valeur", value = NA),
        actionButton("btn_ajouter", "Ajouter l'observation", icon = icon("plus"), class = "btn-primary w-100"),
        hr(),
        actionButton("btn_recalculer", "Recalculer le modèle avec les nouvelles données",
                     icon = icon("rotate"), class = "btn-success w-100"),
        uiOutput("statut_recalcul")
      ),
      card(
        card_header("Historique des observations ajoutées manuellement"),
        p(class = "text-muted small",
          "Ces observations sont sauvegardées de façon permanente et seront prises en compte à ",
          "chaque recalcul du modèle, y compris lors d'une future session."),
        h6("Cibles ajoutées"),
        DTOutput("table_ajouts_cibles"),
        hr(),
        h6("Indicateurs ajoutés"),
        DTOutput("table_ajouts_indicateurs")
      )
    )
  ),

  # ---------------------------------------------------------- A PROPOS
  nav_panel(
    title = "À propos", icon = icon("circle-info"),
    card(
      card_header("Méthodologie"),
      markdown("
**GDPNow-Maroc** adapte le modèle de nowcasting de la Federal Reserve Bank d'Atlanta
(Higgins, 2014) à une logique offre (production par branche d'activité, nomenclature HCP)
plutôt que demande.

**Architecture du modèle :**

1. **BVAR trimestriel** (16 branches, prior Minnesota, méthode des observations fictives —
   Litterman 1986 ; Bańbura, Giannone & Reichlin 2010) : sert de prévision de repli et de
   composante de base pour toutes les branches.
2. **Équations de passerelle** (7 branches disposant d'un indicateur validé) : régression
   simple reliant la croissance de chaque indicateur à celle de la valeur ajoutée,
   combinée au BVAR par moindres carrés restreints (Higgins 2014, équation 8).
3. **AR(4) pur** (9 branches sans indicateur validé) : même traitement que celui appliqué
   par Higgins (2014) aux sous-composantes sans série mensuelle disponible.
4. **Agrégation** : pondération par part de valeur ajoutée (en volume) pour obtenir le
   nowcast du PIB total.

**Sélection des indicateurs :** 41 séries retenues sur 1235 recensées, après application
de 8 critères (fréquence, longueur, fraîcheur, densité interne, corrélation à la cible,
cohérence du signe, non-redondance, cible unique par branche) — inspirés de
Fernández Cerezo (2023, Banco de España) pour le critère de corrélation.

**Limites assumées :**

- Poids d'agrégation approximatifs (valeur ajoutée en volume, non en prix courants)
- Absence de poids de sous-branche pour la moyenne des prévisions bridge
- 9 branches sur 16 sans indicateur d'activité infra-annuel
- 7 des 41 indicateurs retenus ont un signe de corrélation contre-intuitif
- Choc Covid-19 (2020) non isolé par une variable indicatrice dans le BVAR

**Références principales :** Higgins (2014, FRB Atlanta) ; Bańbura, Giannone & Reichlin
(2010) ; Litterman (1986) ; Diebold & Mariano (1995) ; Fernández Cerezo (2023, Banco de
España) ; Miller & Chin (1996, FRB Minneapolis).
      ")
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {

  # --- Etat reactif central : donnees + resultats du pipeline --------------
  donnees <- reactiveVal(charger_donnees_completes())
  resultats <- reactiveVal(NULL)
  backtest_res <- reactiveVal(NULL)

  recalculer <- function() {
    d <- isolate(donnees())
    withProgress(message = "Recalcul du modèle en cours...", value = 0.3, {
      res <- executer_pipeline_complet(d$cibles, d$indicateurs)
      incProgress(0.7)
      resultats(res)
    })
  }
  isolate(recalculer())  # calcul initial au demarrage

  # --- Mise a jour dynamique du selecteur de trimestre pour l'ajout de cible
  observe({
    req(resultats())
    dates_dispo <- resultats()$cibles %>% filter(branche == input$ajout_branche) %>% pull(date)
    dernier <- if (length(dates_dispo) > 0) max(dates_dispo) else as.Date("2026-01-01")
    prochain <- dernier %m+% months(3)
    lbl <- function(d) sprintf("T%d-%d", (month(d)-1)%/%3 + 1, year(d))
    sequence_dates <- prochain %m+% months(seq(-3, 3, 3))
    choix <- setNames(as.character(sequence_dates), sapply(sequence_dates, lbl))
    updateSelectInput(session, "ajout_trimestre", choices = choix, selected = as.character(prochain))
  })

  observe({
    req(resultats())
    indics_branche <- resultats()$indicateurs %>% filter(branche == input$ajout_branche) %>%
      distinct(indicateur) %>% pull(indicateur)
    updateSelectizeInput(session, "ajout_indicateur", choices = indics_branche, server = TRUE)
  })

  # --------------------------------------------------------------- DASHBOARD
  output$vb_nowcast <- renderText({
    req(resultats())
    sprintf("%+.2f %%", resultats()$nowcast$croissance_pib * 100)
  })
  output$vb_trimestre <- renderText({
    req(resultats())
    d <- resultats()$date_prevision
    sprintf("T%d-%d", (month(d)-1)%/%3 + 1, year(d))
  })
  output$vb_maj <- renderText({
    req(resultats())
    format(resultats()$date_maj, "%d/%m/%Y %H:%M")
  })

  output$plot_contributions <- renderPlotly({
    req(resultats())
    df <- resultats()$nowcast$previsions %>%
      mutate(groupe = ifelse(branche %in% BRANCHES_COUVERTES, "Couverte (bridge + BVAR)", "Non couverte (AR(4))")) %>%
      arrange(contribution) %>%
      mutate(branche = factor(branche, levels = branche))
    p <- ggplot(df, aes(branche, contribution, fill = groupe,
                         text = sprintf("%s<br>Contribution : %.4f<br>Prévision : %+.2f%%<br>Part du PIB : %.1f%%",
                                        branche, contribution, prevision*100, part*100))) +
      geom_col() + coord_flip() +
      scale_fill_manual(values = c("Couverte (bridge + BVAR)" = COULEUR_PRIMAIRE, "Non couverte (AR(4))" = COULEUR_SECONDAIRE)) +
      labs(x = NULL, y = "Contribution au Δlog du PIB", fill = NULL) +
      theme_minimal(base_size = 11) + theme(legend.position = "top")
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", y = 1.08))
  })

  output$plot_groupes <- renderPlotly({
    req(resultats())
    df <- tibble(groupe = c("Couverte", "Non couverte"), n = c(length(BRANCHES_COUVERTES), length(BRANCHES_NON_COUVERTES)))
    plot_ly(df, labels = ~groupe, values = ~n, type = "pie", hole = 0.55,
            marker = list(colors = c(COULEUR_PRIMAIRE, COULEUR_SECONDAIRE)),
            textinfo = "label+value", showlegend = FALSE) %>%
      layout(margin = list(l = 10, r = 10, t = 10, b = 10))
  })

  output$table_previsions <- renderDT({
    req(resultats())
    df <- resultats()$nowcast$previsions %>%
      mutate(groupe = ifelse(branche %in% BRANCHES_COUVERTES, "Couverte", "Non couverte")) %>%
      transmute(Branche = branche, Groupe = groupe,
                `Prévision (Δlog)` = round(prevision, 4),
                `Prévision (%)` = sprintf("%+.2f%%", prevision*100),
                `Part du PIB` = sprintf("%.1f%%", part*100),
                `Contribution` = round(contribution, 5)) %>%
      arrange(desc(Contribution))
    datatable(df, rownames = FALSE, options = list(pageLength = 16, dom = "tip"),
              class = "compact stripe") %>%
      formatStyle("Groupe", backgroundColor = styleEqual(c("Couverte", "Non couverte"), c("#DDEBF7", "#F2F2F2")))
  })

  # --------------------------------------------------------------- EXPLORATION
  output$explo_adf <- renderTable({
    req(resultats())
    resultats()$stationnarite %>% filter(branche == input$explo_branche) %>%
      transmute(`p (niveau)` = round(p_niveau, 3), `p (Δlog)` = round(p_dlog, 3))
  })

  output$plot_serie_branche <- renderPlotly({
    req(resultats())
    df <- resultats()$cibles %>% filter(branche == input$explo_branche)
    y_var <- if (input$explo_transfo == "niveau") "va" else "dlog_va"
    df <- df %>% filter(!is.na(.data[[y_var]]))
    p <- ggplot(df, aes(date, .data[[y_var]])) +
      geom_hline(yintercept = if (input$explo_transfo == "dlog") 0 else NA, color = "grey80") +
      geom_line(color = COULEUR_PRIMAIRE, linewidth = 0.6) +
      labs(x = NULL, y = if (input$explo_transfo == "niveau") "Mdh" else "Δlog",
           title = input$explo_branche) +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })

  output$plot_correlation <- renderPlotly({
    req(resultats())
    mat_large <- resultats()$cibles %>% select(branche, date, dlog_va) %>%
      pivot_wider(names_from = branche, values_from = dlog_va) %>% arrange(date)
    mat_cor <- cor(mat_large[,-1], use = "pairwise.complete.obs")
    plot_ly(x = colnames(mat_cor), y = rownames(mat_cor), z = mat_cor, type = "heatmap",
            colors = colorRamp(c(COULEUR_ACCENT, "white", COULEUR_PRIMAIRE)), zmin = -1, zmax = 1) %>%
      layout(margin = list(l = 150, b = 150))
  })

  output$plot_indicateurs_vs_cible <- renderPlotly({
    req(resultats())
    df <- resultats()$indic_trim %>% filter(branche == input$explo_branche_couverte) %>%
      inner_join(resultats()$cibles %>% select(branche, date, dlog_va), by = c("branche", "date"))
    validate(need(nrow(df) > 0, "Aucun indicateur pour cette branche."))
    p <- ggplot(df, aes(dlog, dlog_va, color = signe_atypique, text = indicateur)) +
      geom_point(alpha = 0.6) +
      scale_color_manual(values = c(`FALSE` = COULEUR_PRIMAIRE, `TRUE` = COULEUR_ALERTE)) +
      facet_wrap(~indicateur, scales = "free") +
      labs(x = "Δlog indicateur", y = "Δlog VA") +
      theme_minimal(base_size = 10) + theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })

  # --------------------------------------------------------------- MODELE
  output$table_bvar <- renderDT({
    req(resultats())
    df <- tibble(Branche = names(resultats()$bvar$prevision),
                 `Prévision BVAR (Δlog)` = round(as.numeric(resultats()$bvar$prevision), 4)) %>%
      arrange(desc(`Prévision BVAR (Δlog)`))
    datatable(df, rownames = FALSE, options = list(pageLength = 16, dom = "tip"), class = "compact stripe")
  })

  output$table_bridge <- renderDT({
    req(resultats())
    br <- resultats()$bridge
    validate(need(length(br) > 0, "Aucune branche couverte."))
    df <- tibble(
      Branche = names(br),
      `Nb indicateurs` = sapply(br, `[[`, "nb_indicateurs"),
      `Prévision bridge` = round(sapply(br, `[[`, "prevision_bridge"), 4),
      `Prévision BVAR` = round(sapply(br, function(x) ifelse(is.na(x$prevision_bvar), NA, x$prevision_bvar)), 4),
      `δ (poids BVAR)` = round(sapply(br, `[[`, "delta"), 2),
      `Prévision finale` = round(sapply(br, `[[`, "prevision_finale"), 4)
    )
    datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = "tip"), class = "compact stripe")
  })

  output$table_ar4 <- renderDT({
    req(resultats())
    a4 <- resultats()$ar4
    validate(need(length(a4) > 0, "Aucune branche non couverte."))
    df <- tibble(Branche = names(a4), `Prévision AR(4)` = round(sapply(a4, `[[`, "prevision"), 4),
                 `IC 80% (bas)` = round(sapply(a4, `[[`, "ic80_bas"), 4),
                 `IC 80% (haut)` = round(sapply(a4, `[[`, "ic80_haut"), 4))
    datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = "tip"), class = "compact stripe")
  })

  # --------------------------------------------------------------- VALIDATION
  observeEvent(input$lancer_backtest, {
    d <- donnees()
    withProgress(message = "Backtest en cours (ré-estimation à chaque trimestre)...", value = 0.2, {
      bt <- backtest_pseudo_temps_reel(d$cibles, d$indicateurs, h_test = input$backtest_h)
      incProgress(0.8)
      backtest_res(bt)
    })
  })

  output$backtest_resume <- renderUI({
    bt <- backtest_res()
    if (is.null(bt)) return(p(class = "text-muted", "Lancez le backtest pour voir les résultats."))
    p_val <- if (!is.null(bt$test_dm)) bt$test_dm$p.value else NA
    signif <- !is.na(p_val) && p_val < 0.10
    tagList(
      tags$table(class = "table table-sm",
        tags$tr(tags$td("RMSFE modèle complet"), tags$td(tags$b(sprintf("%.4f", bt$rmsfe_modele)))),
        tags$tr(tags$td("RMSFE repère AR(2)"), tags$td(tags$b(sprintf("%.4f", bt$rmsfe_ar2)))),
        tags$tr(tags$td("Test de Diebold-Mariano"), tags$td(tags$b(sprintf("p = %.3f", p_val))))
      ),
      div(class = if (signif) "alert alert-success" else "alert alert-secondary",
          if (signif) "Écart de précision statistiquement significatif (seuil 10%)."
          else "Écart non significatif — échantillon de test court, à interpréter avec prudence.")
    )
  })

  output$plot_backtest <- renderPlotly({
    bt <- backtest_res()
    validate(need(!is.null(bt), "Aucun backtest lancé pour l'instant."))
    df <- tibble(date = bt$dates_test, Modèle = abs(bt$erreurs_modele), `AR(2)` = abs(bt$erreurs_ar2)) %>%
      pivot_longer(-date, names_to = "modele", values_to = "erreur")
    p <- ggplot(df, aes(date, erreur, color = modele)) +
      geom_line(linewidth = 0.8) + geom_point(size = 2) +
      scale_color_manual(values = c("Modèle" = COULEUR_PRIMAIRE, "AR(2)" = COULEUR_ACCENT)) +
      labs(x = NULL, y = "Erreur absolue (Δlog)", color = NULL) +
      theme_minimal(base_size = 11)
    ggplotly(p)
  })

  # --------------------------------------------------------------- AJOUT DE DONNEES
  output$table_ajouts_cibles <- renderDT({
    df <- if (file.exists(CHEMIN_AJOUTS_CIBLES)) read.csv(CHEMIN_AJOUTS_CIBLES) else
      data.frame(branche = character(), date = character(), va = numeric())
    datatable(df, rownames = FALSE, options = list(pageLength = 5, dom = "tip"), class = "compact stripe")
  })
  output$table_ajouts_indicateurs <- renderDT({
    df <- if (file.exists(CHEMIN_AJOUTS_INDICATEURS)) read.csv(CHEMIN_AJOUTS_INDICATEURS) else
      data.frame(branche = character(), indicateur = character(), date = character(), valeur = numeric())
    datatable(df, rownames = FALSE, options = list(pageLength = 5, dom = "tip"), class = "compact stripe")
  })

  observeEvent(input$btn_ajouter, {
    req(!is.na(input$ajout_valeur))
    if (input$ajout_type == "cible") {
      nouvelle <- data.frame(branche = input$ajout_branche, date = as.character(as.Date(input$ajout_trimestre)),
                              va = input$ajout_valeur)
      existant <- if (file.exists(CHEMIN_AJOUTS_CIBLES)) read.csv(CHEMIN_AJOUTS_CIBLES, stringsAsFactors = FALSE) else nouvelle[0,]
      write.csv(rbind(existant, nouvelle), CHEMIN_AJOUTS_CIBLES, row.names = FALSE)
      showNotification(sprintf("VA ajoutée pour %s au trimestre sélectionné.", input$ajout_branche), type = "message")
    } else {
      req(input$ajout_indicateur != "")
      nouvelle <- data.frame(branche = input$ajout_branche, indicateur = input$ajout_indicateur,
                              source = input$ajout_source, frequence = input$ajout_frequence,
                              r_declare = NA, signe_atypique = FALSE,
                              date = as.character(input$ajout_date), valeur = input$ajout_valeur)
      existant <- if (file.exists(CHEMIN_AJOUTS_INDICATEURS)) read.csv(CHEMIN_AJOUTS_INDICATEURS, stringsAsFactors = FALSE) else nouvelle[0,]
      write.csv(rbind(existant, nouvelle), CHEMIN_AJOUTS_INDICATEURS, row.names = FALSE)
      showNotification(sprintf("Observation ajoutée pour l'indicateur « %s ».", input$ajout_indicateur), type = "message")
    }
    updateNumericInput(session, "ajout_valeur", value = NA)
  })

  observeEvent(input$btn_recalculer, {
    donnees(charger_donnees_completes())
    recalculer()
    showNotification("Modèle recalculé avec les données à jour.", type = "message", duration = 4)
  })

  output$statut_recalcul <- renderUI({
    req(resultats())
    p(class = "text-muted small mt-2",
      sprintf("Modèle actuel calculé le %s à partir de %d observations de cible et %d observations d'indicateurs.",
              format(resultats()$date_maj, "%d/%m/%Y %H:%M"),
              nrow(resultats()$cibles), nrow(resultats()$indicateurs)))
  })
}

shinyApp(ui, server)
