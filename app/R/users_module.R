# ==============================================================================
# Fichier : app/R/users_module.R
# Rôle    : Écran d'administration des utilisateurs (liste paginée, recherche,
#           filtres, création, modification, activation/désactivation).
#           Toutes les opérations passent par l'API, qui applique les droits :
#           masquer un bouton ici ne protège rien, c'est du confort.
# ==============================================================================

# Rôles que chaque rôle peut gérer (miroir de la règle de l'API, pour l'affichage)
UI_MANAGEABLE <- list(
  super_admin = c("super_admin", "admin", "data_ai", "sales_agent", "auditor"),
  admin       = c("data_ai", "sales_agent", "auditor")
)

ui_val <- function(x, default = "") if (is.null(x)) default else as.character(x)

users_tab_ui <- function(id = "users") {
  ns <- shiny::NS(id)
  shinydashboard::tabItem(
    tabName = "users",
    shiny::fluidRow(
      shinydashboard::box(
        title = "Utilisateurs et rôles", status = "primary", solidHeader = TRUE, width = 12,
        shiny::fluidRow(
          shiny::column(3, shiny::textInput(ns("q"), "Recherche (nom, identifiant, e-mail)", width = "100%")),
          shiny::column(2, shiny::selectInput(
            ns("role"), "Rôle", width = "100%",
            choices = c("Tous" = "", stats::setNames(names(ROLE_LABELS), unname(ROLE_LABELS))))),
          shiny::column(2, shiny::selectInput(
            ns("status"), "Statut", width = "100%",
            choices = c("Tous" = "", "Actif" = "active", "Désactivé" = "disabled"))),
          shiny::column(2, shiny::selectInput(ns("page_size"), "Par page",
                                              choices = c(10, 20, 50), selected = 10, width = "100%")),
          shiny::column(3, shiny::uiOutput(ns("toolbar")))
        ),
        DT::DTOutput(ns("table")),
        shiny::uiOutput(ns("pager"))
      )
    )
  )
}

users_server <- function(id, auth) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    page    <- shiny::reactiveVal(1L)
    refresh <- shiny::reactiveVal(0L)
    editing <- shiny::reactiveVal(NULL)
    pending <- shiny::reactiveVal(NULL)
    form_msg <- shiny::reactiveVal(NULL)
    q_d <- shiny::debounce(shiny::reactive(input$q), 500)

    role_choices <- function() {
      ids <- UI_MANAGEABLE[[ ui_val(auth$user()$role, "none") ]]
      if (is.null(ids)) character() else stats::setNames(ids, role_label(ids))
    }

    shiny::observeEvent(list(q_d(), input$role, input$status, input$page_size),
                        page(1L), ignoreInit = TRUE)

    listing <- shiny::reactive({
      refresh()
      shiny::req(auth$has("users.read"))
      api_call("GET", "/users", token = auth$token(),
               query = list(q = q_d(), role = input$role, status = input$status,
                            page = page(), page_size = input$page_size))
    })

    output$toolbar <- shiny::renderUI({
      shiny::req(auth$has("users.manage"))
      shiny::div(style = "margin-top: 25px;",
                 shiny::actionButton(ns("new"), "Nouvel utilisateur", icon = shiny::icon("plus"),
                                     class = "btn-primary"),
                 shiny::actionButton(ns("reload"), NULL, icon = shiny::icon("rotate")))
    })
    shiny::observeEvent(input$reload, refresh(refresh() + 1L))

    # --- Tableau ---------------------------------------------------------------
    output$table <- DT::renderDT({
      res <- listing()
      msg <- function(text) DT::datatable(data.frame(Message = text), rownames = FALSE,
                                          selection = "none", options = list(dom = "t"))
      if (res$status != 200) return(msg(paste("Erreur :", api_error(res))))
      items <- res$body$items
      if (!length(items)) return(msg("Aucun utilisateur ne correspond."))

      me      <- auth$user()
      can_mng <- auth$has("users.manage")
      mine    <- UI_MANAGEABLE[[ ui_val(me$role, "none") ]]

      rows <- lapply(items, function(u) {
        manageable <- can_mng && ui_val(u$role) %in% mine && grepl("^[0-9a-f-]{36}$", ui_val(u$id))
        is_self    <- identical(tolower(ui_val(u$id)), tolower(ui_val(me$id)))
        btn <- function(op, label, cls) {
          sprintf("<button class=\"btn btn-xs %s\" onclick=\"Shiny.setInputValue('%s', {op:'%s', id:'%s', t:Date.now()}, {priority:'event'})\">%s</button> ",
                  cls, ns("act"), op, u$id, label)
        }
        actions <- ""
        if (manageable) {
          actions <- paste0(
            btn("edit", "Modifier", "btn-default"),
            if (identical(u$status, "active") && !is_self) btn("disable", "Désactiver", "btn-danger"),
            if (identical(u$status, "disabled")) btn("enable", "Réactiver", "btn-success")
          )
        }
        data.frame(
          Identifiant = ui_val(u$username), `Nom complet` = ui_val(u$full_name),
          `E-mail` = ui_val(u$email), Rôle = role_label(ui_val(u$role)),
          Statut = if (identical(u$status, "active")) "Actif" else "Désactivé",
          `Dernière connexion` = ui_val(u$last_login_at, "—"),
          Actions = actions, check.names = FALSE, stringsAsFactors = FALSE)
      })
      show <- do.call(rbind, rows)

      DT::datatable(show, rownames = FALSE, selection = "none",
                    escape = setdiff(names(show), "Actions"),   # champs utilisateur échappés
                    options = list(dom = "t", paging = FALSE, ordering = FALSE))
    })

    output$pager <- shiny::renderUI({
      res <- listing()
      shiny::req(res$status == 200)
      b <- res$body
      pages <- max(1L, as.integer(b$pages))
      shiny::div(style = "margin-top: 10px;",
                 if (b$page <= 1) shinyjs::disabled(shiny::actionButton(ns("prev"), "Précédent"))
                 else shiny::actionButton(ns("prev"), "Précédent"),
                 shiny::span(style = "margin: 0 10px;",
                             sprintf("Page %d / %d (%d utilisateur(s))", as.integer(b$page), pages, as.integer(b$total))),
                 if (b$page >= pages) shinyjs::disabled(shiny::actionButton(ns("next"), "Suivant"))
                 else shiny::actionButton(ns("next"), "Suivant"))
    })
    shiny::observeEvent(input$prev, page(max(1L, page() - 1L)))
    shiny::observeEvent(input$`next`, page(page() + 1L))

    output$form_error <- shiny::renderUI({
      m <- form_msg()
      if (is.null(m)) NULL else shiny::div(class = "alert alert-danger", m)
    })

    # --- Création --------------------------------------------------------------
    shiny::observeEvent(input$new, {
      shiny::req(auth$has("users.manage"))
      form_msg(NULL)
      shiny::showModal(shiny::modalDialog(
        title = "Nouvel utilisateur", easyClose = FALSE,
        shiny::textInput(ns("n_username"), "Identifiant (3 à 50 caractères : lettres, chiffres, . _ -)"),
        shiny::textInput(ns("n_email"), "E-mail"),
        shiny::textInput(ns("n_full_name"), "Nom complet"),
        shiny::passwordInput(ns("n_password"), "Mot de passe initial (12 caractères minimum, lettres et chiffres)"),
        shiny::selectInput(ns("n_role"), "Rôle", choices = role_choices()),
        shiny::uiOutput(ns("form_error")),
        footer = shiny::tagList(shiny::modalButton("Annuler"),
                                shiny::actionButton(ns("n_save"), "Créer", class = "btn-primary"))
      ))
    })

    shiny::observeEvent(input$n_save, {
      shiny::req(auth$has("users.manage"))
      res <- api_call("POST", "/users", token = auth$token(),
                      body = list(username = trimws(input$n_username), email = trimws(input$n_email),
                                  full_name = trimws(input$n_full_name),
                                  password = input$n_password, role = input$n_role))
      shiny::updateTextInput(session, "n_password", value = "")
      if (res$status == 201) {
        shiny::removeModal()
        shiny::showNotification("Utilisateur créé.", type = "message", duration = 4)
        refresh(refresh() + 1L)
      } else {
        form_msg(api_error(res))
      }
    })

    # --- Modification, activation, désactivation ---------------------------------
    shiny::observeEvent(input$act, {
      a <- input$act
      shiny::req(auth$has("users.manage"), is.list(a), grepl("^[0-9a-f-]{36}$", ui_val(a$id)))

      if (identical(a$op, "edit")) {
        res <- api_call("GET", paste0("/users/", a$id), token = auth$token())
        if (res$status != 200) {
          return(shiny::showNotification(api_error(res), type = "error"))
        }
        u <- res$body$user
        editing(u)
        form_msg(NULL)
        is_self <- identical(tolower(u$id), tolower(ui_val(auth$user()$id)))
        choices <- role_choices()
        if (!u$role %in% choices) choices <- c(stats::setNames(u$role, role_label(u$role)), choices)
        role_input <- shiny::selectInput(ns("e_role"), "Rôle", choices = choices, selected = u$role)

        shiny::showModal(shiny::modalDialog(
          title = paste("Modifier", u$username), easyClose = FALSE,
          shiny::textInput(ns("e_full_name"), "Nom complet", u$full_name),
          shiny::textInput(ns("e_email"), "E-mail", u$email),
          if (is_self) shinyjs::disabled(role_input) else role_input,
          if (is_self) shiny::helpText("Tu ne peux pas modifier ton propre rôle."),
          shiny::uiOutput(ns("form_error")),
          footer = shiny::tagList(shiny::modalButton("Annuler"),
                                  shiny::actionButton(ns("e_save"), "Enregistrer", class = "btn-primary"))
        ))
      } else if (a$op %in% c("disable", "enable")) {
        pending(list(id = a$id, op = a$op))
        shiny::showModal(shiny::modalDialog(
          title = "Confirmation", easyClose = TRUE,
          if (identical(a$op, "disable"))
            "Désactiver ce compte ? Toutes ses sessions seront fermées immédiatement."
          else "Réactiver ce compte ?",
          footer = shiny::tagList(shiny::modalButton("Annuler"),
                                  shiny::actionButton(ns("confirm_status"), "Confirmer", class = "btn-danger"))
        ))
      }
    })

    shiny::observeEvent(input$e_save, {
      u <- editing()
      shiny::req(u, auth$has("users.manage"))
      fields <- list()
      if (!identical(trimws(input$e_full_name), u$full_name)) fields$full_name <- trimws(input$e_full_name)
      if (!identical(trimws(input$e_email), u$email)) fields$email <- trimws(input$e_email)
      if (!is.null(input$e_role) && !identical(input$e_role, u$role)) fields$role <- input$e_role
      if (!length(fields)) {
        form_msg("Aucune modification.")
        return()
      }
      res <- api_call("PATCH", paste0("/users/", u$id), token = auth$token(), body = fields)
      if (res$status == 200) {
        shiny::removeModal()
        shiny::showNotification("Utilisateur modifié.", type = "message", duration = 4)
        refresh(refresh() + 1L)
      } else {
        form_msg(api_error(res))
      }
    })

    shiny::observeEvent(input$confirm_status, {
      p <- pending()
      shiny::req(p, auth$has("users.manage"))
      res <- api_call("POST", sprintf("/users/%s/%s", p$id, p$op), token = auth$token())
      shiny::removeModal()
      if (res$status == 200) {
        shiny::showNotification(if (identical(p$op, "disable")) "Compte désactivé." else "Compte réactivé.",
                                type = "message", duration = 4)
        refresh(refresh() + 1L)
      } else {
        shiny::showNotification(api_error(res), type = "error", duration = 8)
      }
      pending(NULL)
    })
  })
}
