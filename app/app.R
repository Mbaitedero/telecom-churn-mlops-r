# ==============================================================================
# Fichier : app/app.R
# Rôle    : Frontend Shiny professionnel pour la prédiction du churn télécom.
#           Le modèle reste dans l'API Plumber : aucune dépendance au modèle ici.
#
# Lancement (Git Bash) :
#   API_URL=http://127.0.0.1:8000 \
#     "/c/R/R-4.5.2/bin/x64/Rscript.exe" -e "shiny::runApp('app', port = 3838)"
#
# Ou depuis R :
#   Sys.setenv(API_URL = "http://127.0.0.1:8000")
#   shiny::runApp("app", port = 3838)
# ==============================================================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(DT)
library(httr2)
library(jsonlite)
library(readr)
library(dplyr)

# ==============================================================================
# CONFIGURATION & JOURNALISATION
# ==============================================================================

api_url <- sub("/+$", "", Sys.getenv("API_URL", "http://127.0.0.1:8000"))
MAX_BATCH_ROWS <- 5000L

log_info <- function(...) {
  cat(sprintf("[INFO] %s - %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              paste0(..., collapse = "")))
}

log_error <- function(...) {
  cat(sprintf("[ERROR] %s - %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              paste0(..., collapse = "")))
}

log_info("Démarrage de l'application Telecom Churn")
log_info("API configurée : ", api_url)

# ==============================================================================
# CLIENT API
# ==============================================================================

api_health <- function() {
  tryCatch({
    resp <- httr2::request(paste0(api_url, "/health")) |>
      httr2::req_timeout(3) |>
      httr2::req_perform()
    httr2::resp_body_json(resp, simplifyVector = TRUE)
  }, error = function(e) {
    log_error("Health check impossible : ", conditionMessage(e))
    NULL
  })
}

api_predict <- function(df) {
  payload <- jsonlite::toJSON(
    df,
    dataframe = "rows",
    na = "null",
    auto_unbox = TRUE,
    null = "null"
  )

  resp <- httr2::request(paste0(api_url, "/predict")) |>
    httr2::req_body_raw(charToRaw(as.character(payload)),
                        type = "application/json") |>
    httr2::req_timeout(60) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  body <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = TRUE),
    error = function(e) list(message = httr2::resp_body_string(resp))
  )

  list(status = httr2::resp_status(resp), body = body)
}

# ==============================================================================
# SCHÉMA TELCO & VALIDATION
# ==============================================================================

required_cols <- c(
  "gender", "SeniorCitizen", "Partner", "Dependents", "tenure",
  "PhoneService", "MultipleLines", "InternetService", "OnlineSecurity",
  "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV",
  "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod",
  "MonthlyCharges", "TotalCharges"
)

validate_churn_data <- function(df) {
  errors <- character()
  missing <- setdiff(required_cols, names(df))

  if (length(missing)) {
    errors <- c(errors, paste("Colonnes manquantes :", paste(missing, collapse = ", ")))
  }

  if (nrow(df) < 1) errors <- c(errors, "Le fichier ne contient aucune ligne.")
  if (nrow(df) > MAX_BATCH_ROWS) {
    errors <- c(errors, paste0("Le fichier dépasse la limite de ", MAX_BATCH_ROWS, " lignes."))
  }

  if ("tenure" %in% names(df) &&
      any(is.na(suppressWarnings(as.numeric(df$tenure))) |
          suppressWarnings(as.numeric(df$tenure)) < 0 |
          suppressWarnings(as.numeric(df$tenure)) > 72, na.rm = TRUE)) {
    errors <- c(errors, "La variable tenure doit être comprise entre 0 et 72 mois.")
  }

  if ("MonthlyCharges" %in% names(df) &&
      any(suppressWarnings(as.numeric(df$MonthlyCharges)) < 0, na.rm = TRUE)) {
    errors <- c(errors, "MonthlyCharges ne peut pas être négatif.")
  }

  if ("TotalCharges" %in% names(df) &&
      any(suppressWarnings(as.numeric(df$TotalCharges)) < 0, na.rm = TRUE)) {
    errors <- c(errors, "TotalCharges ne peut pas être négatif.")
  }

  list(valid = length(errors) == 0, errors = errors)
}

extract_predictions <- function(body) {
  p <- body$predictions
  if (is.null(p)) stop("La réponse API ne contient pas le champ 'predictions'.")
  p <- as.data.frame(p, stringsAsFactors = FALSE)

  if (!"prediction" %in% names(p)) {
    stop("Le champ 'prediction' est absent de la réponse API.")
  }
  if (!"churn_probability" %in% names(p)) {
    stop("Le champ 'churn_probability' est absent de la réponse API.")
  }

  p$churn_probability <- as.numeric(p$churn_probability)
  p$prediction <- as.character(p$prediction)
  p
}

# ==============================================================================
# INTERFACE UTILISATEUR
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Telecom Churn | MLOps",
    tags$li(class = "dropdown", style = "padding: 12px 15px;", uiOutput("header_user"))
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Vue d'ensemble", tabName = "overview", icon = icon("dashboard")),
      menuItem("Client individuel", tabName = "prediction", icon = icon("user")),
      menuItem("Analyse de masse", tabName = "batch", icon = icon("table")),
      menuItem("Monitoring", tabName = "monitoring", icon = icon("line-chart")),
      menuItem("Interprétabilité", tabName = "interpretability", icon = icon("lightbulb-o")),
      menuItem("Utilisateurs", tabName = "users", icon = icon("users")),
      menuItem("À propos", tabName = "about", icon = icon("info-circle"))
    ),
    br(),
    div(
      style = "padding: 12px 15px; color: #ecf0f5;",
      h4("État du système", style = "font-weight: 600;"),
      uiOutput("sidebar_api_status"),
      p("Frontend : Shiny", style = "font-size: 12px; color: #c2c7d0;"),
      p("Backend : API Plumber", style = "font-size: 12px; color: #c2c7d0;")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f4f6f9; }
      .box { border-radius: 8px; border-top-width: 3px; box-shadow: 0 2px 6px rgba(0,0,0,.07); }
      .small-box { border-radius: 8px; }
      .risk-card { padding: 22px; border-radius: 9px; margin: 8px 0 16px; }
      .risk-low { background: #e8f7ee; border: 1px solid #b7e4c7; color: #155724; }
      .risk-high { background: #fdecec; border: 1px solid #f5c2c7; color: #842029; }
      .risk-number { font-size: 38px; font-weight: 700; margin: 6px 0; }
      .section-note { color: #6c757d; font-size: 13px; }
      .status-pill { display: inline-block; padding: 5px 9px; border-radius: 14px; font-size: 12px; }
      .status-online { background: #d1e7dd; color: #0f5132; }
      .status-offline { background: #f8d7da; color: #842029; }
      .feature-line { background: #fff; border-left: 4px solid #3c8dbc; padding: 10px 12px; margin: 7px 0; border-radius: 4px; }
      .btn { border-radius: 5px; }
      .main-header .logo { font-weight: 700; }
    "))),
    tabItems(
      users_tab_ui(),
      # ------------------------------------------------------------------------
      # VUE D'ENSEMBLE
      # ------------------------------------------------------------------------
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Plateforme de prédiction du churn",
            p("Application de démonstration pour estimer la probabilité qu'un client résilie son abonnement."),
            p(class = "section-note",
              "Les prédictions sont calculées par l'API Plumber. Les statistiques ci-dessous reflètent uniquement les prédictions réalisées pendant cette session Shiny.")
          )
        ),
        fluidRow(
          valueBoxOutput("kpi_total", width = 3),
          valueBoxOutput("kpi_risk", width = 3),
          valueBoxOutput("kpi_mean", width = 3),
          valueBoxOutput("kpi_api", width = 3)
        ),
        fluidRow(
          box(
            title = "Répartition des prédictions", status = "primary",
            solidHeader = TRUE, width = 6,
            plotlyOutput("overview_risk_plot", height = "300px")
          ),
          box(
            title = "Distribution des probabilités de churn", status = "info",
            solidHeader = TRUE, width = 6,
            plotlyOutput("overview_prob_plot", height = "300px")
          )
        ),
        fluidRow(
          box(
            title = "Dernières prédictions", status = "info",
            solidHeader = TRUE, width = 12,
            DTOutput("recent_predictions")
          )
        )
      ),

      # ------------------------------------------------------------------------
      # PRÉDICTION INDIVIDUELLE
      # ------------------------------------------------------------------------
      tabItem(
        tabName = "prediction",
        fluidRow(
          box(
            title = "Profil du client", status = "primary",
            solidHeader = TRUE, width = 6,
            selectInput("gender", "Genre", choices = c("Female", "Male")),
            selectInput("SeniorCitizen", "Client senior",
                        choices = c("Non" = 0, "Oui" = 1), selected = "0"),
            selectInput("Partner", "En couple", choices = c("No", "Yes")),
            selectInput("Dependents", "Personnes à charge", choices = c("No", "Yes")),
            sliderInput("tenure", "Ancienneté (mois)", min = 0, max = 72, value = 12),
            selectInput("PhoneService", "Service téléphonique", choices = c("No", "Yes"), selected = "Yes"),
            selectInput("MultipleLines", "Lignes multiples",
                        choices = c("No", "Yes", "No phone service"))
          ),
          box(
            title = "Services et contrat", status = "primary",
            solidHeader = TRUE, width = 6,
            selectInput("InternetService", "Service Internet", choices = c("DSL", "Fiber optic", "No")),
            selectInput("OnlineSecurity", "Sécurité en ligne",
                        choices = c("No", "Yes", "No internet service")),
            selectInput("OnlineBackup", "Sauvegarde en ligne",
                        choices = c("No", "Yes", "No internet service")),
            selectInput("DeviceProtection", "Protection de l'appareil",
                        choices = c("No", "Yes", "No internet service")),
            selectInput("TechSupport", "Support technique",
                        choices = c("No", "Yes", "No internet service")),
            selectInput("StreamingTV", "Streaming TV",
                        choices = c("No", "Yes", "No internet service")),
            selectInput("StreamingMovies", "Streaming films",
                        choices = c("No", "Yes", "No internet service")),
            selectInput("Contract", "Type de contrat",
                        choices = c("Month-to-month", "One year", "Two year")),
            selectInput("PaperlessBilling", "Facture dématérialisée",
                        choices = c("No", "Yes"), selected = "Yes"),
            selectInput("PaymentMethod", "Moyen de paiement",
                        choices = c("Electronic check", "Mailed check",
                                    "Bank transfer (automatic)", "Credit card (automatic)")),
            numericInput("MonthlyCharges", "Facture mensuelle", value = 70, min = 0, step = 0.5),
            checkboxInput("auto_total", "Estimer TotalCharges (mensualité × ancienneté)", value = TRUE),
            conditionalPanel(
              condition = "!input.auto_total",
              numericInput("TotalCharges", "Total facturé depuis le début", value = 800, min = 0)
            ),
            actionButton("predict_btn", "Analyser le risque de churn",
                         icon = icon("search"), class = "btn-primary btn-lg")
          )
        ),
        fluidRow(
          box(
            title = "Résultat de la prédiction", status = "success",
            solidHeader = TRUE, width = 8,
            uiOutput("prediction_result"),
            plotlyOutput("single_probability_plot", height = "280px")
          ),
          box(
            title = "Résumé du profil", status = "info",
            solidHeader = TRUE, width = 4,
            uiOutput("profile_summary")
          )
        )
      ),

      # ------------------------------------------------------------------------
      # ANALYSE DE MASSE
      # ------------------------------------------------------------------------
      tabItem(
        tabName = "batch",
        fluidRow(
          box(
            title = "Importer un fichier de clients", status = "primary",
            solidHeader = TRUE, width = 12,
            p("Importe un CSV contenant les variables du jeu Telco. Les colonnes customerID et Churn sont facultatives ; Churn est ignorée si elle est présente."),
            p(class = "section-note", paste0("Limite : ", MAX_BATCH_ROWS, " lignes par traitement.")),
            fileInput("batch_file", "Fichier CSV", accept = c(".csv", "text/csv")),
            downloadButton("download_template", "Télécharger un modèle CSV"),
            br(), br(),
            actionButton("batch_predict_btn", "Lancer les prédictions",
                         icon = icon("play"), class = "btn-primary"),
            actionButton("clear_batch", "Effacer les résultats",
                         icon = icon("trash"), class = "btn-warning")
          )
        ),
        fluidRow(
          valueBoxOutput("batch_total", width = 3),
          valueBoxOutput("batch_risk", width = 3),
          valueBoxOutput("batch_mean", width = 3),
          valueBoxOutput("batch_contract", width = 3)
        ),
        fluidRow(
          box(
            title = "Résultats de l'analyse en lot", status = "info",
            solidHeader = TRUE, width = 12,
            downloadButton("download_batch", "Télécharger les prédictions CSV"),
            br(), br(),
            DTOutput("batch_table")
          )
        )
      ),

      # ------------------------------------------------------------------------
      # MONITORING
      # ------------------------------------------------------------------------
      tabItem(
        tabName = "monitoring",
        fluidRow(
          valueBoxOutput("monitor_total", width = 4),
          valueBoxOutput("monitor_risk", width = 4),
          valueBoxOutput("monitor_mean", width = 4)
        ),
        fluidRow(
          box(
            title = "Évolution des probabilités", status = "primary",
            solidHeader = TRUE, width = 7,
            plotlyOutput("temporal_plot", height = "320px")
          ),
          box(
            title = "Probabilités de churn observées", status = "info",
            solidHeader = TRUE, width = 5,
            plotlyOutput("monitor_histogram", height = "320px")
          )
        ),
        fluidRow(
          box(
            title = "Historique de la session", status = "info",
            solidHeader = TRUE, width = 12,
            DTOutput("history_table")
          )
        ),
        p(class = "section-note",
          "Note : cet historique est conservé en mémoire pendant la session Shiny. Il ne constitue pas un stockage persistant entre plusieurs redémarrages.")
      ),

      # ------------------------------------------------------------------------
      # INTERPRÉTABILITÉ
      # ------------------------------------------------------------------------
      tabItem(
        tabName = "interpretability",
        fluidRow(
          box(
            title = "Variables du profil analysé", status = "primary",
            solidHeader = TRUE, width = 6,
            uiOutput("interpretation_profile")
          ),
          box(
            title = "Lecture de la probabilité", status = "info",
            solidHeader = TRUE, width = 6,
            uiOutput("interpretation_guide")
          )
        ),
        fluidRow(
          box(
            title = "Points de vigilance métier", status = "warning",
            solidHeader = TRUE, width = 12,
            p("Les éléments ci-dessous sont des repères descriptifs, pas une explication causale du modèle."),
            tags$ul(
              tags$li(strong("Ancienneté faible : "),
                      "les clients récemment acquis peuvent nécessiter une attention particulière."),
              tags$li(strong("Contrat mensuel : "),
                      "ce type de contrat laisse généralement plus de flexibilité au client qu'un engagement annuel ou bisannuel."),
              tags$li(strong("Services et support : "),
                      "l'utilisation des services et l'expérience de support peuvent aider les équipes à contextualiser le risque."),
              tags$li(strong("Limite : "),
                      "aucune importance de variable SHAP ou permutation n'est affichée, car l'API fournie ne renvoie pas ces explications.")
            )
          )
        )
      ),

      # ------------------------------------------------------------------------
      # À PROPOS
      # ------------------------------------------------------------------------
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "À propos de Telecom Churn MLOps", status = "primary",
            solidHeader = TRUE, width = 12,
            h3("Plateforme de prédiction du désabonnement télécom"),
            p("Cette application sépare l'interface utilisateur de la logique de prédiction. Shiny collecte les informations du client, envoie les données à l'API Plumber et présente la réponse du modèle."),
            hr(),
            h4("Architecture"),
            tags$ul(
              tags$li(strong("Frontend : "), "R Shiny et shinydashboard."),
              tags$li(strong("API : "), "Plumber, endpoints /health et /predict."),
              tags$li(strong("Communication : "), "requêtes HTTP/JSON via httr2."),
              tags$li(strong("Analyse : "), "Plotly et DT pour les graphiques et tableaux interactifs."),
              tags$li(strong("Traitement en lot : "), "import CSV, prédictions et export des résultats.")
            ),
            hr(),
            h4("Précautions"),
            p("La probabilité produite par le modèle est une estimation et non une certitude. Les décisions de rétention doivent aussi tenir compte du contexte métier, de la qualité des données et de l'évaluation du modèle.")
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVEUR
# ==============================================================================

server <- function(input, output, session) {

  # --- Authentification (app/R/auth_ui.R) : le jeton reste côté serveur ----
  auth <- auth_server(input, output, session)
  auth_token <- function() auth$token()
  users_server("users", auth)


  history <- reactiveVal(data.frame(
    timestamp = as.POSIXct(character()),
    prediction = character(),
    churn_probability = numeric(),
    tenure = numeric(),
    Contract = character(),
    MonthlyCharges = numeric(),
    stringsAsFactors = FALSE
  ))

  batch_results <- reactiveVal(NULL)
  last_profile <- reactiveVal(NULL)
  last_single_result <- reactiveVal(NULL)

  api_status <- reactive({
    invalidateLater(10000, session)
    api_health()
  })

  output$sidebar_api_status <- renderUI({
    h <- api_status()
    if (is.null(h)) {
      span(class = "status-pill status-offline", icon("times-circle"), " API hors ligne")
    } else {
      span(class = "status-pill status-online", icon("check-circle"), " API en ligne")
    }
  })

  output$kpi_total <- renderValueBox({
    valueBox(nrow(history()), "Prédictions individuelles", icon = icon("database"), color = "blue")
  })
  output$kpi_risk <- renderValueBox({
    h <- history()
    n <- if (nrow(h)) sum(h$prediction == "Yes", na.rm = TRUE) else 0
    valueBox(n, "Clients à risque", icon = icon("exclamation-triangle"), color = "red")
  })
  output$kpi_mean <- renderValueBox({
    h <- history()
    v <- if (nrow(h)) mean(h$churn_probability, na.rm = TRUE) * 100 else NA_real_
    valueBox(if (is.finite(v)) paste0(round(v, 1), "%") else "—",
             "Probabilité moyenne", icon = icon("percent"), color = "yellow")
  })
  output$kpi_api <- renderValueBox({
    valueBox(if (is.null(api_status())) "Hors ligne" else "En ligne",
             "Statut API", icon = icon("heartbeat"),
             color = if (is.null(api_status())) "red" else "green")
  })

  # Prépare un client au format attendu par l'API.
  customer <- reactive({
    total <- if (isTRUE(input$auto_total)) {
      round(input$MonthlyCharges * input$tenure, 2)
    } else {
      input$TotalCharges
    }

    data.frame(
      gender = input$gender,
      SeniorCitizen = as.numeric(input$SeniorCitizen),
      Partner = input$Partner,
      Dependents = input$Dependents,
      tenure = as.numeric(input$tenure),
      PhoneService = input$PhoneService,
      MultipleLines = input$MultipleLines,
      InternetService = input$InternetService,
      OnlineSecurity = input$OnlineSecurity,
      OnlineBackup = input$OnlineBackup,
      DeviceProtection = input$DeviceProtection,
      TechSupport = input$TechSupport,
      StreamingTV = input$StreamingTV,
      StreamingMovies = input$StreamingMovies,
      Contract = input$Contract,
      PaperlessBilling = input$PaperlessBilling,
      PaymentMethod = input$PaymentMethod,
      MonthlyCharges = as.numeric(input$MonthlyCharges),
      TotalCharges = as.numeric(total),
      stringsAsFactors = FALSE
    )
  })

  observeEvent(input$predict_btn, {
    log_info("Prédiction individuelle demandée")
    tryCatch({
      req(customer())
      res <- api_predict_auth(customer(), auth_token())

      if (res$status < 200 || res$status >= 300) {
        msg <- paste(unlist(res$body), collapse = " ")
        showNotification(paste("Erreur API :", msg), type = "error", duration = 8)
        return()
      }

      preds <- extract_predictions(res$body)
      prob <- preds$churn_probability[1]
      pred <- preds$prediction[1]

      if (!is.finite(prob) || prob < 0 || prob > 1) {
        stop("La probabilité de churn reçue de l'API doit être comprise entre 0 et 1.")
      }

      row <- data.frame(
        timestamp = Sys.time(),
        prediction = pred,
        churn_probability = prob,
        tenure = customer()$tenure,
        Contract = customer()$Contract,
        MonthlyCharges = customer()$MonthlyCharges,
        stringsAsFactors = FALSE
      )
      history(rbind(history(), row))
      last_profile(customer())
      last_single_result(list(prediction = pred, probability = prob,
                              threshold = res$body$threshold))
      showNotification("Prédiction terminée.", type = "message", duration = 3)
      log_info(sprintf("Prédiction terminée : %s, probabilité %.4f", pred, prob))
    }, error = function(e) {
      log_error(conditionMessage(e))
      showNotification(paste("Impossible de réaliser la prédiction :", conditionMessage(e)),
                       type = "error", duration = 10)
    })
  })

  output$prediction_result <- renderUI({
    r <- last_single_result()
    if (is.null(r)) {
      return(div(class = "alert alert-info",
                 "Renseigne les informations du client puis clique sur « Analyser le risque de churn »."))
    }

    prob_pct <- round(100 * r$probability, 1)
    high <- identical(as.character(r$prediction), "Yes")
    cls <- if (high) "risk-high" else "risk-low"
    label <- if (high) "RISQUE DE CHURN ÉLEVÉ" else "RISQUE DE CHURN PLUS FAIBLE"
    icon_name <- if (high) "exclamation-triangle" else "check-circle"

    div(class = paste("risk-card", cls),
        h3(icon(icon_name), label),
        div(class = "risk-number", paste0(prob_pct, " %")),
        p("Probabilité estimée de churn"),
        div(class = "progress",
            style = "height: 24px;",
            div(class = paste("progress-bar", if (high) "progress-bar-danger" else "progress-bar-success"),
                role = "progressbar",
                style = paste0("width:", prob_pct, "%;"),
                `aria-valuenow` = prob_pct, `aria-valuemin` = "0", `aria-valuemax` = "100",
                paste0(prob_pct, "%"))),
        p(if (high) "Piste métier : examiner les besoins du client et les possibilités de rétention."
          else "Le modèle estime un risque plus faible ; continuer à suivre l'expérience du client."),
        if (!is.null(r$threshold)) p(class = "section-note", paste("Seuil retourné par l'API :", r$threshold))
    )
  })

  output$single_probability_plot <- renderPlotly({
    r <- last_single_result()
    req(r)
    plot_data <- data.frame(
      Classe = c("Churn", "Pas de churn"),
      Probabilite = c(r$probability * 100, (1 - r$probability) * 100)
    )
    plot_ly(plot_data, x = ~Classe, y = ~Probabilite, type = "bar",
            text = ~paste0(round(Probabilite, 1), "%"),
            textposition = "outside",
            marker = list(color = c("#dd4b39", "#00a65a"))) |>
      layout(title = "Probabilités estimées",
             yaxis = list(title = "Probabilité (%)", range = c(0, 110)),
             xaxis = list(title = ""), showlegend = FALSE)
  })

  output$profile_summary <- renderUI({
    df <- last_profile()
    if (is.null(df)) return(p("Aucune prédiction effectuée.", class = "text-muted"))
    div(
      div(class = "feature-line", strong("Ancienneté : "), paste(df$tenure, "mois")),
      div(class = "feature-line", strong("Contrat : "), df$Contract),
      div(class = "feature-line", strong("Facture mensuelle : "),
          paste0(round(df$MonthlyCharges, 2))),
      div(class = "feature-line", strong("Internet : "), df$InternetService),
      div(class = "feature-line", strong("Support technique : "), df$TechSupport),
      div(class = "feature-line", strong("Moyen de paiement : "), df$PaymentMethod)
    )
  })

  output$interpretation_profile <- renderUI({
    df <- last_profile()
    if (is.null(df)) return(p("Effectue une prédiction individuelle pour afficher le profil analysé."))
    tags$div(
      div(class = "feature-line", strong("Ancienneté : "), paste(df$tenure, "mois")),
      div(class = "feature-line", strong("Type de contrat : "), df$Contract),
      div(class = "feature-line", strong("Internet : "), df$InternetService),
      div(class = "feature-line", strong("Support technique : "), df$TechSupport),
      div(class = "feature-line", strong("Sécurité en ligne : "), df$OnlineSecurity),
      div(class = "feature-line", strong("Facture mensuelle : "), round(df$MonthlyCharges, 2))
    )
  })

  output$interpretation_guide <- renderUI({
    r <- last_single_result()
    if (is.null(r)) {
      return(p("Après une prédiction, la probabilité sera affichée ici avec des repères de lecture."))
    }
    p1 <- round(100 * r$probability, 1)
    div(
      h4(paste0("Probabilité estimée : ", p1, " %")),
      p(if (p1 >= 60) "Niveau descriptif élevé : examiner le dossier et envisager une action de rétention adaptée."
        else if (p1 >= 30) "Niveau descriptif intermédiaire : un suivi ciblé peut être utile."
        else "Niveau descriptif plus faible : conserver le suivi habituel."),
      p(class = "section-note",
        "Ces bandes sont des repères d'interface et ne remplacent pas le seuil officiel du modèle. Elles ne prouvent pas la cause de la prédiction.")
    )
  })

  # Analyse de masse
  observeEvent(input$batch_predict_btn, {
    req(input$batch_file)
    log_info("Analyse de masse demandée")
    tryCatch({
      df <- readr::read_csv(input$batch_file$datapath, show_col_types = FALSE,
                            name_repair = "minimal") |> as.data.frame()

      # Les identifiants et la cible ne sont pas des variables d'entrée du modèle.
      df$Churn <- NULL
      validation <- validate_churn_data(df)
      if (!validation$valid) {
        showNotification(paste(validation$errors, collapse = "\n"),
                         type = "error", duration = 10)
        return()
      }

      # Conserver les colonnes requises dans l'ordre attendu par le modèle.
      df_model <- df[, required_cols, drop = FALSE]
      res <- api_predict_auth(df_model, auth_token())
      if (res$status < 200 || res$status >= 300) {
        showNotification(paste("Erreur API :", paste(unlist(res$body), collapse = " ")),
                         type = "error", duration = 10)
        return()
      }

      pred <- extract_predictions(res$body)
      if (nrow(pred) != nrow(df_model)) {
        stop("Le nombre de prédictions reçu ne correspond pas au nombre de lignes envoyées.")
      }

      out <- cbind(df, pred)
      out <- out[order(out$churn_probability, decreasing = TRUE), , drop = FALSE]
      rownames(out) <- NULL
      batch_results(out)
      showNotification(paste(nrow(out), "clients analysés."), type = "message", duration = 4)
      log_info("Analyse de masse terminée : ", nrow(out), " lignes")
    }, error = function(e) {
      log_error(conditionMessage(e))
      showNotification(paste("Échec de l'analyse de masse :", conditionMessage(e)),
                       type = "error", duration = 10)
    })
  })

  observeEvent(input$clear_batch, {
    batch_results(NULL)
    showNotification("Résultats de l'analyse en lot effacés.", type = "message", duration = 2)
  })

  output$batch_total <- renderValueBox({
    x <- batch_results()
    valueBox(if (is.null(x)) 0 else nrow(x), "Clients analysés",
             icon = icon("users"), color = "blue")
  })
  output$batch_risk <- renderValueBox({
    x <- batch_results()
    n <- if (is.null(x)) 0 else sum(x$prediction == "Yes", na.rm = TRUE)
    valueBox(n, "Clients à risque", icon = icon("exclamation-triangle"), color = "red")
  })
  output$batch_mean <- renderValueBox({
    x <- batch_results()
    p <- if (is.null(x) || !nrow(x)) NA_real_ else mean(x$churn_probability, na.rm = TRUE) * 100
    valueBox(if (is.finite(p)) paste0(round(p, 1), "%") else "—",
             "Probabilité moyenne", icon = icon("percent"), color = "yellow")
  })
  output$batch_contract <- renderValueBox({
    x <- batch_results()
    n <- if (is.null(x) || !"Contract" %in% names(x)) 0 else sum(x$Contract == "Month-to-month", na.rm = TRUE)
    valueBox(n, "Contrats mensuels", icon = icon("file-text"), color = "purple")
  })

  output$batch_table <- renderDT({
    x <- batch_results()
    if (is.null(x)) return(datatable(data.frame(Message = "Importe un CSV puis lance les prédictions."),
                                     options = list(dom = "t"), rownames = FALSE))
    datatable(x, rownames = FALSE, filter = "top",
              options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE)) |>
      formatPercentage("churn_probability", digits = 1) |>
      formatStyle("prediction",
                  backgroundColor = styleEqual(c("Yes", "No"), c("#f8d7da", "#d1e7dd")),
                  color = styleEqual(c("Yes", "No"), c("#842029", "#0f5132")))
  })

  output$download_batch <- downloadHandler(
    filename = function() paste0("telecom_churn_predictions_", Sys.Date(), ".csv"),
    content = function(file) {
      req(batch_results())
      readr::write_csv(batch_results(), file)
    }
  )

  output$download_template <- downloadHandler(
    filename = function() "telecom_churn_template.csv",
    content = function(file) {
      template <- data.frame(
        gender = "Female", SeniorCitizen = 0, Partner = "Yes", Dependents = "No",
        tenure = 12, PhoneService = "Yes", MultipleLines = "No",
        InternetService = "Fiber optic", OnlineSecurity = "No", OnlineBackup = "Yes",
        DeviceProtection = "No", TechSupport = "No", StreamingTV = "Yes",
        StreamingMovies = "No", Contract = "Month-to-month", PaperlessBilling = "Yes",
        PaymentMethod = "Electronic check", MonthlyCharges = 70.0, TotalCharges = 840.0,
        stringsAsFactors = FALSE
      )
      readr::write_csv(template, file)
    }
  )

  # Historique et monitoring
  output$monitor_total <- renderValueBox({
    valueBox(nrow(history()), "Prédictions individuelles",
             icon = icon("database"), color = "blue")
  })
  output$monitor_risk <- renderValueBox({
    h <- history()
    n <- if (nrow(h)) sum(h$prediction == "Yes", na.rm = TRUE) else 0
    valueBox(n, "Prédictions à risque", icon = icon("exclamation-triangle"), color = "red")
  })
  output$monitor_mean <- renderValueBox({
    h <- history()
    p <- if (nrow(h)) mean(h$churn_probability, na.rm = TRUE) * 100 else NA_real_
    valueBox(if (is.finite(p)) paste0(round(p, 1), "%") else "—",
             "Probabilité moyenne", icon = icon("percent"), color = "yellow")
  })

  output$overview_risk_plot <- renderPlotly({
    h <- history()
    if (!nrow(h)) return(plotly_empty() |> layout(title = "Aucune prédiction pour le moment"))
    d <- as.data.frame(table(factor(h$prediction, levels = c("Yes", "No"))))
    names(d) <- c("Classe", "Nombre")
    d$Classe <- recode(d$Classe, Yes = "Churn prédit", No = "Pas de churn prédit")
    plot_ly(d, x = ~Classe, y = ~Nombre, type = "bar",
            marker = list(color = c("#dd4b39", "#00a65a")),
            text = ~Nombre, textposition = "outside") |>
      layout(yaxis = list(title = "Nombre"), xaxis = list(title = ""), showlegend = FALSE)
  })

  output$overview_prob_plot <- renderPlotly({
    h <- history()
    if (!nrow(h)) return(plotly_empty() |> layout(title = "Aucune prédiction pour le moment"))
    plot_ly(h, x = ~churn_probability, type = "histogram",
            marker = list(color = "#3c8dbc")) |>
      layout(xaxis = list(title = "Probabilité de churn", tickformat = ".0%"),
             yaxis = list(title = "Nombre de prédictions"))
  })

  output$temporal_plot <- renderPlotly({
    h <- history()
    if (!nrow(h)) return(plotly_empty() |> layout(title = "Aucune prédiction pour le moment"))
    plot_ly(h, x = ~timestamp, y = ~churn_probability, type = "scatter",
            mode = "lines+markers", text = ~paste("Classe :", prediction),
            marker = list(color = "#3c8dbc")) |>
      layout(xaxis = list(title = "Heure"), yaxis = list(title = "Probabilité", tickformat = ".0%"))
  })

  output$monitor_histogram <- renderPlotly({
    h <- history()
    if (!nrow(h)) return(plotly_empty() |> layout(title = "Aucune prédiction pour le moment"))
    plot_ly(h, x = ~churn_probability, type = "histogram",
            marker = list(color = "#00a65a")) |>
      layout(xaxis = list(title = "Probabilité de churn", tickformat = ".0%"),
             yaxis = list(title = "Fréquence"))
  })

  output$recent_predictions <- renderDT({
    h <- history()
    if (!nrow(h)) {
      return(datatable(data.frame(Message = "Aucune prédiction réalisée dans cette session."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    datatable(head(h[order(h$timestamp, decreasing = TRUE), ], 10),
              rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE)) |>
      formatPercentage("churn_probability", digits = 1)
  })

  output$history_table <- renderDT({
    h <- history()
    if (!nrow(h)) {
      return(datatable(data.frame(Message = "Aucune prédiction réalisée dans cette session."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    datatable(h[order(h$timestamp, decreasing = TRUE), ],
              rownames = FALSE, filter = "top",
              options = list(pageLength = 10, scrollX = TRUE)) |>
      formatPercentage("churn_probability", digits = 1)
  })
}

shinyApp(ui = ui, server = server)
