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
  status <- NA
  for (i in 1:90) {
    if (!proc$is_alive()) {
      stop("API arrêtée au démarrage :\n",
           paste(utils::tail(readLines(log, warn = FALSE), 20), collapse = "\n"))
    }
    status <- tryCatch(
      httr2::request(paste0(url, "/health")) |>
        httr2::req_timeout(2) |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_perform() |> httr2::resp_status(),
      error = function(e) NA)
    if (identical(status, 200L)) break
    Sys.sleep(1)
  }
  if (!identical(status, 200L)) {
    proc$kill()
    stop("API injoignable après 90 s")
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
if (api_prereqs_ok()) {
  strict <- tryCatch(start_test_api("true"),
                     error = function(e) { message(conditionMessage(e)); NULL })
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
  testthat::skip_if(is.null(strict) || is.null(users),
                    "API de test indisponible (base de test, modèle, callr ou httr2)")
}

token_for <- function(user) {
  api_req(strict, "POST", "/auth/login",
          list(identifier = user$username, password = user$password))$body$token
}

sample_client_json <- function() {
  paste(readLines(here::here("tests", "sample_customer.json"), warn = FALSE), collapse = "")
}

# --- Tests ----------------------------------------------------------------------

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
  con <- db_connect()
  u <- new_test_user(con)
  DBI::dbDisconnect(con)

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
  one   <- jsonlite::fromJSON(sample_client_json(), simplifyDataFrame = TRUE)
  two   <- jsonlite::toJSON(rbind(one, one), dataframe = "rows", auto_unbox = TRUE)

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

test_that("mode transitoire AUTH_REQUIRED=false : /predict ouvert, le reste protégé", {
  skip_if_no_api()
  legacy <- tryCatch(start_test_api("false"), error = function(e) NULL)
  skip_if(is.null(legacy), "API (mode transitoire) indisponible")
  withr::defer(legacy$proc$kill())

  r <- api_req(legacy, "POST", "/predict", raw_body = sample_client_json())
  expect_equal(r$status, 200)
  expect_equal(r$body$predictions[[1]]$churn_probability, 0.6891, tolerance = 1e-4)
  expect_equal(api_req(legacy, "GET", "/auth/me")$status, 401)
  expect_false(api_req(legacy, "GET", "/health")$body$auth_required)
})
