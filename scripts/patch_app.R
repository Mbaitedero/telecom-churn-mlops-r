# ==============================================================================
# Script  : scripts/patch_app.R
# Rôle    : Branche l'authentification dans app/app.R (6 modifications ciblées).
#           Tout ou rien : si un point d'ancrage manque ou est ambigu, le
#           fichier n'est pas modifié. Annulation : git checkout app/app.R
# ==============================================================================

path <- "app/app.R"
stopifnot("app/app.R introuvable (lance depuis la racine du projet)" = file.exists(path))

txt <- rawToChar(readBin(path, "raw", file.size(path)))
Encoding(txt) <- "UTF-8"

if (grepl("auth_server(", txt, fixed = TRUE)) {
  cat("app.R contient déjà l'authentification : rien à faire.\n")
  quit(status = 0)
}

nl <- if (grepl("\r\n", txt, fixed = TRUE)) "\r\n" else "\n"

edits <- list(
  list(
    name = "en-tête (zone utilisateur)",
    from = 'dashboardHeader(title = "Telecom Churn | MLOps"),',
    to = paste0('dashboardHeader(\n',
                '    title = "Telecom Churn | MLOps",\n',
                '    tags$li(class = "dropdown", style = "padding: 12px 15px;", uiOutput("header_user"))\n',
                '  ),')
  ),
  list(
    name = "menu Utilisateurs",
    from = 'tabName = "interpretability", icon = icon("lightbulb-o")),',
    to = paste0('tabName = "interpretability", icon = icon("lightbulb-o")),\n',
                '      menuItem("Utilisateurs", tabName = "users", icon = icon("users")),')
  ),
  list(
    name = "onglet Utilisateurs",
    from = "tabItems(",
    to = "tabItems(\n      users_tab_ui(),"
  ),
  list(
    name = "prédiction individuelle avec jeton",
    from = "res <- api_predict(customer())",
    to = "res <- api_predict_auth(customer(), auth_token())"
  ),
  list(
    name = "prédiction en lot avec jeton",
    from = "res <- api_predict(df_model)",
    to = "res <- api_predict_auth(df_model, auth_token())"
  ),
  list(
    name = "serveur : authentification",
    from = "server <- function(input, output, session) {",
    to = paste0("server <- function(input, output, session) {\n\n",
                "  # --- Authentification (app/R/auth_ui.R) : le jeton reste côté serveur ----\n",
                "  auth <- auth_server(input, output, session)\n",
                "  auth_token <- function() auth$token()\n",
                "  users_server(\"users\", auth)\n")
  )
)

count_of <- function(pattern) {
  m <- gregexpr(pattern, txt, fixed = TRUE)[[1]]
  sum(m > 0)
}
counts <- vapply(edits, function(e) count_of(e$from), numeric(1))
bad <- which(counts != 1)
if (length(bad)) {
  stop("Aucune modification appliquée. Points d'ancrage introuvables ou ambigus : ",
       paste0(vapply(edits[bad], `[[`, "", "name"), " (", counts[bad], " occurrence(s))",
              collapse = " ; "))
}

for (e in edits) {
  txt <- sub(e$from, gsub("\n", nl, e$to, fixed = TRUE), txt, fixed = TRUE)
}
writeBin(charToRaw(enc2utf8(txt)), path)
cat("app/app.R modifié (", length(edits), " modifications). Vérifie avec : git diff app/app.R\n", sep = "")
