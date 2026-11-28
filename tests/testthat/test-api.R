# ==============================================================================
# Tests d'intégration de l'API : démarre réellement l'API (processus R séparé)
# contre la base de TEST. Les comptes créés restent dans la base de test
# (réinitialisable avec scripts/reset_test_db.sh).
# ==============================================================================

api_prereqs_ok <- function() {
  db_available() &&
    file.exists(here::here("models", "final_model.rds")) &&
    all(vapply(c("callr", "httr2", "httpuv"), requireNamespace, logical(1), quietly = TRUE))
}

start_test_api <- function(auth_required = "true", max_body = 2500) {
  port <- httpuv::randomPort()
  log  <- tempfile(fileext = ".log")
  envs <- c(
    JWT_SECRET = Sys.getenv("JWT_SECRET"), AUTH_REQUIRED = auth_required,
    API_DOCS = "false", MAX_BODY_BYTES = as.character(max_body),
    DB_HOST = Sys.getenv("DB_HOST"), DB_PORT = Sys.getenv("DB_PORT", "5432"),
    DB_NAME = Sys.getenv("DB_NAME"), DB_USER = Sys.getenv("APP_DB_USER"),
    DB_PASSWORD = Sys.getenv("APP_DB_PASSWORD")
  )
  log_tail <- function() paste(utils::tail(readLines(log, warn = FALSE), 15), collapse = "\n")

  proc <- callr::r_bg(
    function(root, port, envs) {
      do.call(Sys.setenv, as.list(envs))
      setwd(root)
      plumber::pr_run(plumber::plumb(file = "plumber/api.R"),
                      host = "127.0.0.1", port = port, quiet = TRUE)
    },
    args = list(root = here::here(), port = port, envs = envs),
    stdout = log, stderr = "2>&1"
  )

  url <- paste0("http://127.0.0.1:", port)
  status <- NA_integer_
  for (i in 1:60) {
    if (!proc$is_alive()) stop("API arrêtée au démarrage :\n", log_tail())

    resp <- tryCatch(
      httr2::request(paste0(url, "/health")) |>
        httr2::req_timeout(2) |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_perform(),
      error = function(e) NULL)
    status <- if (is.null(resp)) NA_integer_ else httr2::resp_status(resp)

    if (identical(status, 200L)) break
    if (!is.na(status) && status >= 500) {
      proc$kill()
      stop("L'API répond ", status, " sur /health :\n", log_tail())
    }
    Sys.sleep(1)
  }
  if (!identical(status, 200L)) {
    proc$kill()
    stop("API injoignable après 60 s :\n", log_tail())
  }
  list(proc = proc, url = url)
}

api_req <- function(api, method, path, body = NULL, raw_body = NULL, token = NULL) {
  r <- httr2::request(paste0(api$url, path)) |>
    httr2::req_method(method) |>
    httr2::req_timeout(30) |>
    httr2::req_error(is_error = function(resp) FALSE)
  if (!is.null(token))    r <- httr2::req_auth_bearer_token(r, token)
  if (!is.null(body))     r <- httr2::req_body_json(r, body, auto_unbox = TRUE)
  if (!is.null(raw_body)) r <- httr2::req_body_raw(r, raw_body, type = "application/json")

  resp <- httr2::req_perform(r)
  list(status = httr2::resp_status(resp),
       body = tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE),
                       error = function(e) NULL),
       www = httr2::resp_header(resp, "WWW-Authenticate"))
}

# --- Mise en place du fichier : API stricte + comptes de test -------------------
strict <- NULL
users  <- NULL
strict_error <- NULL
if (api_prereqs_ok()) {
  strict <- tryCatch(start_test_api("true"),
                     error = function(e) { strict_error <<- conditionMessage(e); NULL })
  if (!is.null(strict)) {
    withr::defer(strict$proc$kill(), testthat::teardown_env())
    con <- db_connect()
    users <- list(admin   = new_test_user(con, "admin"),
                  auditor = new_test_user(con, "auditor"),
                  data    = new_test_user(con, "data_ai"),
                  sales   = new_test_user(con, "sales_agent"))
    DBI::dbDisconnect(con)
  }
}

skip_if_no_api <- function() {
  if (is.null(strict) || is.null(users)) {
    testthat::skip(paste0("API de test indisponible. ",
                          if (!is.null(strict_error)) strict_error
                          else "(base de test, modèle, callr ou httr2 ?)"))
  }
}

token_for <- function(user) {
  api_req(strict, "POST", "/auth/login",
          list(identifier = user$username, password = user$password))$body$token
}

sample_client_json <- function() {
  paste(readLines(here::here("tests", "sample_customer.json"), warn = FALSE), collapse = "")
}

# Compte créé directement en base (validé), pour les tests qui le modifient.
fresh_user <- function(role) {
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con))
  new_test_user(con, role)
}

rand_name <- function(prefix = "api") {
  paste0(prefix, "_", paste(sample(c(letters, 0:9), 8, TRUE), collapse = ""))
}

# --- Tests : service, authentification, prédiction ------------------------------

test_that("/health est public et décrit l'état du service", {
  skip_if_no_api()
  r <- api_req(strict, "GET", "/health")
  expect_equal(r$status, 200)
  expect_equal(r$body$status, "ok")
  expect_true(r$body$database)
  expect_true(r$body$model_loaded)
  expect_equal(r$body$threshold, 0.3)
  expect_true(r$body$auth_required)
})

test_that("login : succès, échec générique, JSON invalide", {
  skip_if_no_api()
  ok <- api_req(strict, "POST", "/auth/login",
                list(identifier = users$admin$username, password = users$admin$password))
  expect_equal(ok$status, 200)
  expect_equal(ok$body$token_type, "Bearer")
  expect_equal(ok$body$user$role, "admin")
  expect_true("users.manage" %in% unlist(ok$body$permissions))

  bad  <- api_req(strict, "POST", "/auth/login",
                  list(identifier = users$admin$username, password = "Mauvais-Mot2Passe-9"))
  none <- api_req(strict, "POST", "/auth/login",
                  list(identifier = "inconnu_xyz_123", password = "Mauvais-Mot2Passe-9"))
  expect_equal(bad$status, 401)
  expect_equal(none$status, 401)
  expect_equal(bad$body$error, none$body$error)

  expect_equal(api_req(strict, "POST", "/auth/login", raw_body = "pas du json")$status, 400)
})

test_that("login : verrouillage après 5 échecs (429)", {
  skip_if_no_api()
  u <- fresh_user("auditor")

  for (i in 1:5) {
    expect_equal(api_req(strict, "POST", "/auth/login",
                         list(identifier = u$username, password = "Mauvais-Mot2Passe-9"))$status, 401)
  }
  r <- api_req(strict, "POST", "/auth/login",
               list(identifier = u$username, password = u$password))
  expect_equal(r$status, 429)
})

test_that("/auth/me exige un jeton valide", {
  skip_if_no_api()
  none <- api_req(strict, "GET", "/auth/me")
  expect_equal(none$status, 401)
  expect_equal(none$www, "Bearer")

  expect_equal(api_req(strict, "GET", "/auth/me", token = "pas.un.jeton")$status, 401)

  me <- api_req(strict, "GET", "/auth/me", token = token_for(users$auditor))
  expect_equal(me$status, 200)
  expect_equal(me$body$user$username, users$auditor$username)
  expect_equal(me$body$user$role, "auditor")
  expect_true("audit.read" %in% unlist(me$body$permissions))
})

test_that("/predict : 401 sans jeton, 403 sans permission, 200 avec permission", {
  skip_if_no_api()
  body <- sample_client_json()

  expect_equal(api_req(strict, "POST", "/predict", raw_body = body)$status, 401)
  expect_equal(api_req(strict, "POST", "/predict", raw_body = body,
                       token = token_for(users$auditor))$status, 403)
  expect_equal(api_req(strict, "POST", "/predict", raw_body = body,
                       token = token_for(users$sales))$status, 403)

  ok <- api_req(strict, "POST", "/predict", raw_body = body,
                token = token_for(users$admin))
  expect_equal(ok$status, 200)
  expect_equal(ok$body$n, 1)
  expect_equal(ok$body$threshold, 0.3)
  expect_equal(ok$body$model_version, "unversioned")
  # Non-régression : même résultat qu'avant l'authentification
  expect_equal(ok$body$predictions[[1]]$churn_probability, 0.6891, tolerance = 1e-4)
  expect_equal(ok$body$predictions[[1]]$prediction, "Yes")
})

test_that("/predict par lot : permission dédiée et journal d'audit", {
  skip_if_no_api()
  one <- jsonlite::fromJSON(sample_client_json(), simplifyDataFrame = TRUE)
  two <- jsonlite::toJSON(rbind(one, one), dataframe = "rows", auto_unbox = TRUE)

  r <- api_req(strict, "POST", "/predict", raw_body = as.character(two),
               token = token_for(users$data))
  expect_equal(r$status, 200)
  expect_equal(r$body$n, 2)

  # data_ai a predictions.batch ; sales_agent non
  expect_equal(api_req(strict, "POST", "/predict", raw_body = as.character(two),
                       token = token_for(users$sales))$status, 403)

  con <- db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  n_audit <- DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n FROM audit_log
    WHERE action = 'predict.batch' AND user_id = $1::uuid",
                             params = list(users$data$id))$n
  expect_gte(n_audit, 1)
})

test_that("/predict : validations d'entrée", {
  skip_if_no_api()
  tok <- token_for(users$admin)

  incomplete <- api_req(strict, "POST", "/predict", raw_body = '[{"gender":"Female"}]', token = tok)
  expect_equal(incomplete$status, 400)
  expect_equal(incomplete$body$error, "Colonnes manquantes")

  expect_equal(api_req(strict, "POST", "/predict", raw_body = "{pas du json", token = tok)$status, 400)

  trop_gros <- paste0("[", strrep(" ", 3000), "]")
  expect_equal(api_req(strict, "POST", "/predict", raw_body = trop_gros, token = tok)$status, 413)
})

test_that("déconnexion : le jeton est rejeté ensuite", {
  skip_if_no_api()
  tok <- token_for(users$admin)
  expect_equal(api_req(strict, "GET", "/auth/me", token = tok)$status, 200)
  expect_equal(api_req(strict, "POST", "/auth/logout", token = tok)$status, 200)
  expect_equal(api_req(strict, "GET", "/auth/me", token = tok)$status, 401)
})

test_that("route inconnue : 404 en JSON, sans détail interne", {
  skip_if_no_api()
  r <- api_req(strict, "GET", "/inconnu", token = token_for(users$admin))
  expect_equal(r$status, 404)
  expect_equal(r$body$error, "Ressource introuvable")
})

# --- Tests : gestion des utilisateurs -------------------------------------------

test_that("GET /users : droits, recherche, pagination, pas de hachage", {
  skip_if_no_api()
  expect_equal(api_req(strict, "GET", "/users")$status, 401)
  expect_equal(api_req(strict, "GET", "/users", token = token_for(users$auditor))$status, 403)
  expect_equal(api_req(strict, "GET", "/users", token = token_for(users$sales))$status, 403)

  tok <- token_for(users$admin)
  r <- api_req(strict, "GET", paste0("/users?q=", users$admin$username, "&page_size=5"), token = tok)
  expect_equal(r$status, 200)
  expect_gte(r$body$total, 1)
  expect_equal(r$body$page, 1)
  expect_equal(r$body$items[[1]]$username, users$admin$username)
  expect_equal(r$body$items[[1]]$role, "admin")
  expect_null(r$body$items[[1]]$password_hash)

  expect_equal(api_req(strict, "GET", "/users?page_size=500", token = tok)$status, 400)
  expect_equal(api_req(strict, "GET", "/users?status=autre", token = tok)$status, 400)

  one <- api_req(strict, "GET", paste0("/users/", users$data$id), token = tok)
  expect_equal(one$status, 200)
  expect_equal(one$body$user$username, users$data$username)
  expect_equal(api_req(strict, "GET", "/users/pas-un-uuid", token = tok)$status, 404)

  # Le refus est journalisé
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  n <- DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n FROM audit_log
    WHERE action = 'users.list' AND result = 'denied' AND user_id = $1::uuid",
                       params = list(users$auditor$id))$n
  expect_gte(n, 1)
})

test_that("POST /users : création, hiérarchie des rôles, doublons, validations", {
  skip_if_no_api()
  tok <- token_for(users$admin)
  name <- rand_name()
  body <- list(username = name, email = paste0(name, "@example.test"),
               full_name = "Créé par API", password = "Mot2Passe-Solide-42",
               role = "auditor")

  r <- api_req(strict, "POST", "/users", body = body, token = tok)
  expect_equal(r$status, 201)
  expect_equal(r$body$user$username, name)
  expect_equal(r$body$user$role, "auditor")
  expect_null(r$body$user$password)
  expect_null(r$body$user$password_hash)
  expect_equal(api_req(strict, "POST", "/auth/login",
                       list(identifier = name, password = body$password))$status, 200)

  expect_equal(api_req(strict, "POST", "/users", body = body, token = tok)$status, 409)

  other <- rand_name()
  as_admin <- modifyList(body, list(username = other, email = paste0(other, "@example.test"),
                                    role = "admin"))
  expect_equal(api_req(strict, "POST", "/users", body = as_admin, token = tok)$status, 403)

  expect_equal(api_req(strict, "POST", "/users", body = body,
                       token = token_for(users$auditor))$status, 403)
  expect_equal(api_req(strict, "POST", "/users", body = body)$status, 401)

  sans_mail <- body[setdiff(names(body), "email")]
  sans_mail$username <- rand_name()
  expect_equal(api_req(strict, "POST", "/users", body = sans_mail, token = tok)$status, 400)

  faible <- modifyList(body, list(username = rand_name(), email = "faible@example.test",
                                  password = "court1"))
  expect_equal(api_req(strict, "POST", "/users", body = faible, token = tok)$status, 400)

  expect_equal(api_req(strict, "POST", "/users", raw_body = "{pas du json", token = tok)$status, 400)
})

test_that("PATCH /users/<id> : modification, droits et validations", {
  skip_if_no_api()
  tok <- token_for(users$admin)
  target <- fresh_user("sales_agent")
  path <- paste0("/users/", target$id)

  r <- api_req(strict, "PATCH", path, body = list(full_name = "Nom modifié"), token = tok)
  expect_equal(r$status, 200)
  expect_equal(r$body$user$full_name, "Nom modifié")

  peer <- fresh_user("admin")
  expect_equal(api_req(strict, "PATCH", paste0("/users/", peer$id),
                       body = list(full_name = "X"), token = tok)$status, 403)
  expect_equal(api_req(strict, "PATCH", path, body = list(full_name = "X"),
                       token = token_for(users$auditor))$status, 403)
  expect_equal(api_req(strict, "PATCH", path, body = list(role = "admin"), token = tok)$status, 403)
  expect_equal(api_req(strict, "PATCH", path, body = list(mot = "x"), token = tok)$status, 400)
  expect_equal(api_req(strict, "PATCH", path, body = list(role = "inconnu"), token = tok)$status, 400)
  expect_equal(api_req(strict, "PATCH", "/users/00000000-0000-0000-0000-000000000000",
                       body = list(full_name = "X"), token = tok)$status, 404)
  expect_equal(api_req(strict, "PATCH", path, raw_body = "{pas du json", token = tok)$status, 400)
})

test_that("désactivation et réactivation : jetons révoqués, protections", {
  skip_if_no_api()
  tok <- token_for(users$admin)
  target <- fresh_user("sales_agent")
  target_tok <- token_for(target)
  expect_equal(api_req(strict, "GET", "/auth/me", token = target_tok)$status, 200)

  d <- api_req(strict, "POST", paste0("/users/", target$id, "/disable"), token = tok)
  expect_equal(d$status, 200)
  expect_equal(d$body$user$status, "disabled")
  expect_equal(api_req(strict, "GET", "/auth/me", token = target_tok)$status, 401)
  expect_equal(api_req(strict, "POST", "/auth/login",
                       list(identifier = target$username, password = target$password))$status, 401)

  e <- api_req(strict, "POST", paste0("/users/", target$id, "/enable"), token = tok)
  expect_equal(e$status, 200)
  expect_equal(e$body$user$status, "active")
  expect_equal(api_req(strict, "POST", "/auth/login",
                       list(identifier = target$username, password = target$password))$status, 200)

  # Interdits : se désactiver soi-même, désactiver un pair, agir sans droit
  expect_equal(api_req(strict, "POST", paste0("/users/", users$admin$id, "/disable"),
                       token = tok)$status, 403)
  peer <- fresh_user("admin")
  expect_equal(api_req(strict, "POST", paste0("/users/", peer$id, "/disable"),
                       token = tok)$status, 403)
  expect_equal(api_req(strict, "POST", paste0("/users/", target$id, "/disable"),
                       token = token_for(users$auditor))$status, 403)
})

test_that("changement de mot de passe : contrôles et révocation des autres sessions", {
  skip_if_no_api()
  u <- fresh_user("auditor")
  t1 <- token_for(u)
  t2 <- token_for(u)
  new_pw <- "Nouveau-Mot2Passe-77"

  expect_equal(api_req(strict, "POST", "/auth/change-password", token = t1,
                       body = list(current_password = "Mauvais-Mot2Passe-9",
                                   new_password = new_pw))$status, 403)
  expect_equal(api_req(strict, "POST", "/auth/change-password", token = t1,
                       body = list(current_password = u$password,
                                   new_password = "court1"))$status, 400)
  expect_equal(api_req(strict, "POST", "/auth/change-password",
                       body = list(current_password = u$password,
                                   new_password = new_pw))$status, 401)

  ok <- api_req(strict, "POST", "/auth/change-password", token = t1,
                body = list(current_password = u$password, new_password = new_pw))
  expect_equal(ok$status, 200)

  expect_equal(api_req(strict, "GET", "/auth/me", token = t1)$status, 200)   # session courante
  expect_equal(api_req(strict, "GET", "/auth/me", token = t2)$status, 401)   # autre session
  expect_equal(api_req(strict, "POST", "/auth/login",
                       list(identifier = u$username, password = u$password))$status, 401)
  expect_equal(api_req(strict, "POST", "/auth/login",
                       list(identifier = u$username, password = new_pw))$status, 200)
})

# --- Test : mode transitoire ----------------------------------------------------

test_that("mode transitoire AUTH_REQUIRED=false : /predict ouvert, le reste protégé", {
  skip_if_no_api()
  legacy <- tryCatch(start_test_api("false"), error = function(e) NULL)
  skip_if(is.null(legacy), "API (mode transitoire) indisponible")
  withr::defer(legacy$proc$kill())

  r <- api_req(legacy, "POST", "/predict", raw_body = sample_client_json())
  expect_equal(r$status, 200)
  expect_equal(r$body$predictions[[1]]$churn_probability, 0.6891, tolerance = 1e-4)
  expect_equal(api_req(legacy, "GET", "/auth/me")$status, 401)
  expect_equal(api_req(legacy, "GET", "/users")$status, 401)
  expect_false(api_req(legacy, "GET", "/health")$body$auth_required)
})
