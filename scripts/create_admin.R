# ==============================================================================
# Script  : scripts/create_admin.R
# Rôle    : Crée le PREMIER super administrateur (refuse s'il en existe déjà un).
# Usage   : ADMIN_PASSWORD=... Rscript scripts/create_admin.R <identifiant> <email> "<Nom complet>"
#           Le mot de passe est lu dans la variable d'environnement ADMIN_PASSWORD,
#           jamais dans les arguments (donc absent de l'historique du terminal).
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  stop("Usage : Rscript scripts/create_admin.R <identifiant> <email> \"<Nom complet>\"")
}
password <- Sys.getenv("ADMIN_PASSWORD")
if (!nzchar(password)) stop("Variable ADMIN_PASSWORD absente")

if (file.exists(".env")) readRenviron(".env")
Sys.setenv(DB_HOST = Sys.getenv("DB_HOST", "127.0.0.1"))   # depuis la machine hôte

source("R/db.R")
source("R/auth.R")

con <- db_connect()
on.exit(DBI::dbDisconnect(con), add = TRUE)

existing <- DBI::dbGetQuery(con, "
  SELECT count(*)::int AS n FROM users u JOIN roles r ON r.id = u.role_id
  WHERE r.code = 'super_admin'")$n
if (existing > 0) stop("Un super administrateur existe déjà : création refusée.")

id <- create_user(con, username = args[1], email = args[2], full_name = args[3],
                  password = password, role_code = "super_admin")
audit_write(con, "user.bootstrap_admin", "success", user_id = id,
            object_type = "user", object_id = id)

cat("Super administrateur créé : ", args[1], " (", id, ")\n", sep = "")
