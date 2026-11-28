# ==============================================================================
# Fichier : app/R/auth_ui.R
# Rôle    : Connexion / déconnexion, changement de mot de passe, état de session.
#           Le jeton reste dans la session serveur (jamais envoyé au navigateur).
# Output  : auth_server() renvoie list(state, token(), user(), has(permission))
# ==============================================================================

ROLE_LABELS <- c(
  super_admin = "Super administrateur",
  admin       = "Administrateur",
  data_ai     = "Responsable Data / IA",
  sales_agent = "Agent commercial",
  auditor     = "Analyste / Auditeur"
)

role_label <- function(code) {
  unname(ifelse(code %in% names(ROLE_LABELS), ROLE_LABELS[code], code))
}

# login_modal()
#   Fenêtre de connexion bloquante (non fermable).
login_modal <- function(message = NULL) {
  shiny::modalDialog(
    title = "Connexion", size = "s", easyClose = FALSE, footer = NULL,
    if (!is.null(message)) shiny::div(class = "alert alert-warning", message),
    shiny::textInput("login_identifier", "Identifiant ou e-mail", width = "100%"),
    shiny::passwordInput("login_password", "Mot de passe", width = "100%"),
    shiny::uiOutput("login_error"),
    shiny::actionButton("login_btn", "Se connecter", class = "btn-primary", width = "100%"),
    shiny::tags$script(shiny::HTML(
      "$(document).off('keyup.login').on('keyup.login', '#login_password', function(e){ if (e.which === 13) { $('#login_btn').click(); } });"
    ))
  )
}

# auth_server()
#   À appeler une fois au début de la fonction server() de l'application.
auth_server <- function(input, output, session) {
  state     <- shiny::reactiveVal(NULL)
  login_msg <- shiny::reactiveVal(NULL)
  pw_msg    <- shiny::reactiveVal(NULL)

  reload_soon <- function(ms = 1200) {
    shinyjs::runjs(sprintf("setTimeout(function(){ location.reload(); }, %d);", as.integer(ms)))
  }

  # Appelé par api_call() quand l'API refuse le jeton (session expirée ou révoquée)
  session$userData$on_401 <- function() {
    if (is.null(shiny::isolate(state()))) return(invisible(NULL))
    state(NULL)
    shiny::showNotification("Session expirée : retour à la connexion...",
                            type = "error", duration = 5)
    reload_soon()
    invisible(NULL)
  }

  # Révoque le jeton si l'onglet est fermé
  session$onSessionEnded(function() {
    s <- shiny::isolate(state())
    if (!is.null(s)) try(api_call("POST", "/auth/logout", token = s$token), silent = TRUE)
  })

  shinyjs::hide(selector = "a[data-value='users']")
  shiny::showModal(login_modal())

  output$login_error <- shiny::renderUI({
    msg <- login_msg()
    if (is.null(msg)) NULL else shiny::div(class = "alert alert-danger", msg)
  })

  shiny::observeEvent(input$login_btn, {
    res <- api_call("POST", "/auth/login",
                    body = list(identifier = input$login_identifier,
                                password   = input$login_password))
    shiny::updateTextInput(session, "login_password", value = "")

    if (res$status == 200) {
      b <- res$body
      perms <- unlist(b$permissions)
      state(list(token = b$token, user = b$user, permissions = perms,
                 expires_at = b$expires_at))
      login_msg(NULL)
      shiny::removeModal()
      shinyjs::toggle(selector = "a[data-value='users']", condition = "users.read" %in% perms)
      shiny::showNotification(paste("Bienvenue", b$user$full_name), type = "message", duration = 3)
    } else {
      login_msg(api_error(res))
    }
  })

  output$header_user <- shiny::renderUI({
    s <- state()
    if (is.null(s)) return(NULL)
    shiny::tagList(
      shiny::span(style = "color:#fff; margin-right:14px;",
                  shiny::icon("user"), " ", s$user$full_name,
                  " (", role_label(s$user$role), ")"),
      shiny::actionLink("open_password", "Mot de passe", icon = shiny::icon("key"),
                        style = "color:#fff; margin-right:14px;"),
      shiny::actionLink("logout_btn", "Déconnexion", icon = shiny::icon("right-from-bracket"),
                        style = "color:#fff;")
    )
  })

  shiny::observeEvent(input$logout_btn, {
    s <- state()
    state(NULL)
    if (!is.null(s)) api_call("POST", "/auth/logout", token = s$token)
    reload_soon(200)
  })

  # --- Changement de mot de passe ------------------------------------------------
  output$pw_error <- shiny::renderUI({
    msg <- pw_msg()
    if (is.null(msg)) NULL else shiny::div(class = "alert alert-danger", msg)
  })

  shiny::observeEvent(input$open_password, {
    pw_msg(NULL)
    shiny::showModal(shiny::modalDialog(
      title = "Changer mon mot de passe", easyClose = FALSE,
      shiny::passwordInput("pw_current", "Mot de passe actuel", width = "100%"),
      shiny::passwordInput("pw_new", "Nouveau mot de passe (12 caractères minimum, lettres et chiffres)", width = "100%"),
      shiny::passwordInput("pw_confirm", "Confirmer le nouveau mot de passe", width = "100%"),
      shiny::uiOutput("pw_error"),
      footer = shiny::tagList(shiny::modalButton("Annuler"),
                              shiny::actionButton("pw_save", "Enregistrer", class = "btn-primary"))
    ))
  })

  shiny::observeEvent(input$pw_save, {
    s <- state()
    shiny::req(s)
    if (!identical(input$pw_new, input$pw_confirm)) {
      pw_msg("La confirmation ne correspond pas au nouveau mot de passe.")
      return()
    }
    res <- api_call("POST", "/auth/change-password", token = s$token,
                    body = list(current_password = input$pw_current,
                                new_password = input$pw_new))
    if (res$status == 200) {
      shiny::removeModal()
      shiny::showNotification("Mot de passe modifié. Tes autres sessions ont été fermées.",
                              type = "message", duration = 6)
    } else {
      pw_msg(api_error(res))
    }
  })

  list(
    state = state,
    token = function() { s <- state(); if (is.null(s)) NULL else s$token },
    user  = function() { s <- state(); if (is.null(s)) NULL else s$user },
    has   = function(permission) { s <- state(); !is.null(s) && permission %in% s$permissions }
  )
}
