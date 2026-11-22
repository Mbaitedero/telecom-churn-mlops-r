# Aides pour les tests qui utilisent PostgreSQL. Tous visent la base de TEST,
# jamais la base de développement. Ignorés si la base de test est absente.
if (file.exists(here::here(".env"))) readRenviron(here::here(".env"))
Sys.setenv(DB_HOST = Sys.getenv("DB_HOST", "127.0.0.1"))
Sys.setenv(DB_NAME = Sys.getenv("TEST_DB_NAME", "telecom_platform_test"))
if (nchar(Sys.getenv("JWT_SECRET")) < 32) {
  Sys.setenv(JWT_SECRET = "secret-de-test-uniquement-0123456789abcdef")
}

source(here::here("R", "db.R"))
source(here::here("R", "auth.R"))

skip_if_no_db <- function() {
  testthat::skip_if_not(
    db_available(),
    "Base de test injoignable : lance docker compose up -d db puis bash scripts/reset_test_db.sh"
  )
}

# Connexion dans une transaction annulée à la fin du test : aucun résidu.
local_db_txn <- function(env = parent.frame()) {
  con <- db_connect()
  DBI::dbBegin(con)
  withr::defer({
    try(DBI::dbRollback(con), silent = TRUE)
    DBI::dbDisconnect(con)
  }, envir = env)
  con
}

new_test_user <- function(con, role = "auditor", password = "Mot2Passe-Solide-42") {
  name <- paste0("t_", paste(sample(c(letters, 0:9), 10, TRUE), collapse = ""))
  id <- create_user(con, username = name, email = paste0(name, "@example.test"),
                    full_name = "Utilisateur Test", password = password, role_code = role)
  list(id = id, username = name, password = password)
}
