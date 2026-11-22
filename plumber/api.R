# ==============================================================================
# Fichier : plumber/api.R
# Rôle    : API REST de prédiction du churn télécom, authentifiée.
#   Publiques : GET /health, POST /auth/login
#   Protégées : POST /auth/logout, GET /auth/me, POST /predict
#
# Config (variables d'environnement) :
#   DB_HOST, DB_PORT, DB_NAME, DB_USER, DB_PASSWORD   base PostgreSQL
#   JWT_SECRET       clé de signature (32 caractères minimum)
#   AUTH_REQUIRED    "true" (défaut) | "false" = /predict ouvert (transitoire)
#   API_DOCS         "true" pour activer /__docs__/ (désactivé par défaut)
#   MAX_BODY_BYTES   taille maximale d'une requête (défaut 4 Mo)
# Lancer  : plumber::plumb(file = "plumber/api.R")$run(port = 8000)
# ==============================================================================

library(plumber)
library(parsnip)
library(workflows)
library(recipes)

# --- Racine du projet ---------------------------------------------------------
# plumb() se place dans plumber/ : on retrouve la racine ici ou dans le parent.
root <- if (file.exists("R/preprocessing.R")) {
  "."
} else if (file.exists("../R/preprocessing.R")) {
  ".."
} else {
  stop("Racine du projet introuvable : lance l'API depuis telecom-churn-mlops-r/")
}

source(file.path(root, "R", "preprocessing.R"))
source(file.path(root, "R", "predict.R"))
source(file.path(root, "R", "db.R"))
source(file.path(root, "R", "auth.R"))

# --- Configuration ------------------------------------------------------------
AUTH_REQUIRED  <- !identical(tolower(Sys.getenv("AUTH_REQUIRED", "true")), "false")
DOCS_ENABLED   <- identical(tolower(Sys.getenv("API_DOCS", "false")), "true")
MAX_BODY_BYTES <- as.numeric(Sys.getenv("MAX_BODY_BYTES", "4194304"))
PUBLIC_PATHS   <- c("/health", "/auth/login")

if (!AUTH_REQUIRED) {
  message("[WARN] AUTH_REQUIRED=false : /predict est accessible sans authentification.")
}

# --- Modèle, métadonnées, base de données --------------------------------------
final <- read_final_model(file.path(root, "models", "final_model.rds"))

meta_path  <- file.path(root, "models", "model_meta.json")
model_meta <- if (file.exists(meta_path)) {
  jsonlite::read_json(meta_path)
} else {
  list(version = "unversioned")
}

pool <- pool::dbPool(
  RPostgres::Postgres(),
  host     = Sys.getenv("DB_HOST", "db"),
  port     = as.integer(Sys.getenv("DB_PORT", "5432")),
  dbname   = Sys.getenv("DB_NAME", Sys.getenv("POSTGRES_DB")),
  user     = Sys.getenv("DB_USER", Sys.getenv("APP_DB_USER")),
  password = Sys.getenv("DB_PASSWORD", Sys.getenv("APP_DB_PASSWORD"))
)

# --- Aides ----------------------------------------------------------------------

# Corps de requête trop volumineux, ou illisible : NULL.
parse_json_body <- function(req, simplify_df = FALSE) {
  raw <- req$postBody
  if (is.null(raw) || !nzchar(raw)) return(NULL)
  if (nchar(raw, type = "bytes") > MAX_BODY_BYTES) return(NULL)
  tryCatch(jsonlite::fromJSON(raw, simplifyDataFrame = simplify_df),
           error = function(e) NULL)
}

deny <- function(req, res, action, permission) {
  tryCatch(
    audit_write(pool, action, "denied", user_id = req$auth$user_id,
                metadata = list(permission = permission)),
    error = function(e) NULL
  )
  res$status <- 403
  list(error = "Accès refusé")
}


#* @plumber
function(pr) {
  pr |>
    pr_set_debug(FALSE) |>
    pr_set_docs(DOCS_ENABLED) |>
    pr_set_serializer(serializer_json(auto_unbox = TRUE)) |>
    pr_set_error(function(req, res, err) {
      message(sprintf("[ERROR] %s %s %s [%s] - %s", format(Sys.time()),
                      req$REQUEST_METHOD, req$PATH_INFO,
                      if (is.null(req$request_id)) "-" else req$request_id,
                      conditionMessage(err)))
      res$status <- 500
      list(error = "Erreur interne du serveur", request_id = req$request_id)
    }) |>
    pr_set_404(function(req, res) {
      res$status <- 404
      list(error = "Ressource introuvable")
    }) |>
    pr_hook("exit", function() pool::poolClose(pool))
}


# ==============================================================================
# FILTRES (exécutés dans l'ordre)
# ==============================================================================

#* Identifiant de requête et en-têtes de sécurité
#* @filter request_context
function(req, res) {
  req$request_id <- paste(as.character(openssl::rand_bytes(8)), collapse = "")
  res$setHeader("X-Request-ID", req$request_id)
  res$setHeader("X-Content-Type-Options", "nosniff")
  res$setHeader("Cache-Control", "no-store")
  forward()
}

#* Refuse les corps de requête trop volumineux
#* @filter body_size
function(req, res) {
  len <- suppressWarnings(as.numeric(req$HTTP_CONTENT_LENGTH))
  if (length(len) == 1 && !is.na(len) && len > MAX_BODY_BYTES) {
    res$status <- 413
    return(list(error = "Requête trop volumineuse"))
  }
  forward()
}

#* Authentification par jeton Bearer + session active en base
#* @filter authenticate
function(req, res) {
  path <- req$PATH_INFO
  if (path %in% PUBLIC_PATHS ||
      grepl("^/(__docs__|__swagger__|openapi\\.json)", path)) {
    return(forward())
  }
  if (!AUTH_REQUIRED && path == "/predict") return(forward())   # mode transitoire

  header <- req$HTTP_AUTHORIZATION
  token  <- if (!is.null(header) && grepl("^Bearer ", header)) {
    sub("^Bearer +", "", header)
  } else NA_character_

  auth <- if (!is.na(token)) authenticate_token(pool, token) else NULL
  if (is.null(auth)) {
    res$status <- 401
    res$setHeader("WWW-Authenticate", "Bearer")
    return(list(error = "Authentification requise"))
  }
  req$auth <- auth
  forward()
}


# ==============================================================================
# ROUTES
# ==============================================================================

#* État du service (utilisé par le healthcheck Docker)
#* @get /health
function(res) {
  db_ok <- tryCatch({
    DBI::dbGetQuery(pool, "SELECT 1 AS ok")
    TRUE
  }, error = function(e) FALSE)
  if (!db_ok) res$status <- 503

  list(status = if (db_ok) "ok" else "degraded",
       model_loaded = !is.null(final$model),
       database = db_ok,
       threshold = final$threshold,
       model_version = model_meta$version,
       auth_required = AUTH_REQUIRED)
}

#* Connexion : corps {"identifier": "...", "password": "..."}
#* @parser text
#* @post /auth/login
function(req, res) {
  body <- parse_json_body(req)
  if (is.null(body) || !is.list(body)) {
    res$status <- 400
    return(list(error = "Corps de requête JSON invalide"))
  }

  result <- login(pool, body$identifier, body$password)
  if (isTRUE(result$ok)) {
    return(list(
      token = result$token,
      token_type = "Bearer",
      expires_at = format(result$expires_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      user = result$user,
      permissions = as.list(result$permissions)
    ))
  }

  res$status <- if (identical(result$status, "locked")) 429 else 401
  list(error = result$message)
}

#* Déconnexion : révoque la session du jeton présenté
#* @post /auth/logout
function(req) {
  logout(pool, req$auth$session_id, req$auth$user_id)
  list(status = "logged_out")
}

#* Utilisateur courant et ses permissions
#* @get /auth/me
function(req) {
  list(
    user = list(id = req$auth$user_id, username = req$auth$username,
                full_name = req$auth$full_name, role = req$auth$role),
    permissions = as.list(req$auth$permissions)
  )
}

#* Prédit le risque de churn. Corps JSON : un client (objet) ou plusieurs (liste)
#* @parser text
#* @post /predict
function(req, res) {
  new_data <- parse_json_body(req, simplify_df = TRUE)
  new_data <- tryCatch(as.data.frame(new_data), error = function(e) NULL)

  if (is.null(new_data) || nrow(new_data) < 1) {
    res$status <- 400
    return(list(error = "Corps de requête JSON invalide ou vide"))
  }

  n <- nrow(new_data)
  max_rows <- get_setting(pool, "batch_limits", list(max_rows = 5000))$max_rows
  if (n > max_rows) {
    res$status <- 413
    return(list(error = paste0("Trop de lignes (maximum : ", max_rows, ")")))
  }

  if (AUTH_REQUIRED) {
    permission <- if (n > 1) "predictions.batch" else "predictions.run"
    if (!has_permission(req$auth, permission)) {
      return(deny(req, res, "predict.run", permission))
    }
  }

  absentes <- missing_columns(new_data)
  if (length(absentes) > 0) {
    res$status <- 400
    return(list(error = "Colonnes manquantes", colonnes = absentes))
  }

  tryCatch({
    preds <- predict_churn(final, new_data)
    if (AUTH_REQUIRED && n > 1) {
      audit_write(pool, "predict.batch", "success", user_id = req$auth$user_id,
                  metadata = list(rows = n, model_version = model_meta$version))
    }
    list(threshold = final$threshold,
         model_version = model_meta$version,
         n = n,
         predictions = preds)
  }, error = function(e) {
    message(sprintf("[ERROR] predict [%s] - %s", req$request_id, conditionMessage(e)))
    res$status <- 500
    list(error = "Erreur de prédiction", request_id = req$request_id)
  })
}
