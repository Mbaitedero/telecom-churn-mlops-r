# ==============================================================================
# Fichier : app/R/api_client.R
# Rôle    : Client HTTP de l'API Plumber pour le frontend Shiny (jeton Bearer).
#           Shiny charge automatiquement les fichiers de app/R/ avant app.R.
# Input   : méthode, chemin, jeton, corps et paramètres de requête
# Output  : list(status = code HTTP (0 si l'API est injoignable), body = JSON décodé)
# ==============================================================================

api_url_base <- function() sub("/+$", "", Sys.getenv("API_URL", "http://127.0.0.1:8000"))

# notify_401()
#   Prévient la session Shiny courante que le jeton n'est plus accepté
#   (le crochet est défini dans auth_server()).
notify_401 <- function() {
  s <- shiny::getDefaultReactiveDomain()
  if (!is.null(s) && is.function(s$userData$on_401)) s$userData$on_401()
  invisible(NULL)
}

# api_error()
#   Message d'erreur lisible pour une réponse de l'API.
api_error <- function(res) {
  if (identical(res$status, 0L)) return("API injoignable")
  msg <- res$body$error
  if (is.null(msg) || !nzchar(msg)) "Erreur inattendue" else as.character(msg)
}

# api_call()
#   Appel générique (JSON). Un 401 avec jeton déclenche la sortie de session.
api_call <- function(method, path, token = NULL, body = NULL, query = NULL, timeout = 30) {
  req <- httr2::request(paste0(api_url_base(), path)) |>
    httr2::req_method(method) |>
    httr2::req_timeout(timeout) |>
    httr2::req_error(is_error = function(resp) FALSE)

  if (!is.null(token)) req <- httr2::req_auth_bearer_token(req, token)
  if (!is.null(query)) {
    query <- Filter(function(x) !is.null(x) && length(x) == 1 && !is.na(x) &&
                      nzchar(as.character(x)), query)
    if (length(query)) req <- do.call(httr2::req_url_query, c(list(req), query))
  }
  if (!is.null(body)) req <- httr2::req_body_json(req, body, auto_unbox = TRUE)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp)) return(list(status = 0L, body = list(error = "API injoignable")))

  parsed <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE),
                     error = function(e) list(error = "Réponse illisible"))
  out <- list(status = httr2::resp_status(resp), body = parsed)
  if (out$status == 401 && !is.null(token)) notify_401()
  out
}

# api_predict_auth()
#   Version authentifiée de api_predict() : envoie le jeton, même format de
#   réponse que l'ancienne fonction (predictions sous forme de data.frame).
api_predict_auth <- function(df, token = NULL) {
  payload <- jsonlite::toJSON(df, dataframe = "rows", na = "null",
                              auto_unbox = TRUE, null = "null")
  req <- httr2::request(paste0(api_url_base(), "/predict")) |>
    httr2::req_body_raw(charToRaw(as.character(payload)), type = "application/json") |>
    httr2::req_timeout(60) |>
    httr2::req_error(is_error = function(resp) FALSE)
  if (!is.null(token)) req <- httr2::req_auth_bearer_token(req, token)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp)) return(list(status = 0L, body = list(error = "API injoignable")))

  body <- tryCatch(httr2::resp_body_json(resp, simplifyVector = TRUE),
                   error = function(e) list(message = httr2::resp_body_string(resp)))
  status <- httr2::resp_status(resp)
  if (status == 401 && !is.null(token)) notify_401()
  list(status = status, body = body)
}
