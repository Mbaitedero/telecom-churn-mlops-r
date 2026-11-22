# ==============================================================================
# Fichier : R/db.R
# Rôle    : Connexion à PostgreSQL et lecture des paramètres. Toutes les
#           requêtes sont paramétrées ($1, $2...) : aucune valeur n'est collée
#           dans le SQL.
# Config  : DB_HOST, DB_PORT, DB_NAME, DB_USER, DB_PASSWORD (environnement).
#           À défaut : POSTGRES_DB, APP_DB_USER, APP_DB_PASSWORD (fichier .env).
# ==============================================================================


# db_connect()
#   Input  : paramètres de connexion (par défaut : variables d'environnement)
#   Output : connexion DBI (à fermer avec DBI::dbDisconnect)
db_connect <- function(host     = Sys.getenv("DB_HOST", "db"),
                       port     = as.integer(Sys.getenv("DB_PORT", "5432")),
                       dbname   = Sys.getenv("DB_NAME", Sys.getenv("POSTGRES_DB")),
                       user     = Sys.getenv("DB_USER", Sys.getenv("APP_DB_USER")),
                       password = Sys.getenv("DB_PASSWORD", Sys.getenv("APP_DB_PASSWORD"))) {
  stopifnot("Configuration de base manquante (voir .env)" =
              nzchar(dbname) && nzchar(user) && nzchar(password))
  DBI::dbConnect(RPostgres::Postgres(), host = host, port = port,
                 dbname = dbname, user = user, password = password)
}


# db_available()
#   Output : TRUE si la base répond avec la configuration courante
db_available <- function() {
  tryCatch({
    con <- db_connect()
    DBI::dbDisconnect(con)
    TRUE
  }, error = function(e) FALSE)
}


# get_setting()
#   Lit un paramètre de la table settings (valeur JSON décodée).
#   Input  : con, key, default = valeur si la clé n'existe pas
get_setting <- function(con, key, default = NULL) {
  r <- DBI::dbGetQuery(con, "SELECT value::text AS value FROM settings WHERE key = $1",
                       params = list(key))
  if (!nrow(r)) return(default)
  jsonlite::fromJSON(r$value[[1]])
}
