# ==============================================================================
# Fichier : R/auth.R
# Rôle    : Authentification : mots de passe (scrypt (libsodium)), jetons JWT liés à une
#           session en base, connexion avec verrouillage, déconnexion,
#           permissions et journal d'audit.
#
# Input   : une connexion DBI (con) et des données utilisateur
# Output  : listes R (jamais de mot de passe ni de hachage renvoyés au client)
# Config  : JWT_SECRET (32 caractères minimum), paramètres "auth_policy" en base
# ==============================================================================

AUTH_ISSUER   <- "telecom-platform"
AUTH_AUDIENCE <- "telecom-api"
GENERIC_LOGIN_ERROR <- "Identifiants invalides ou compte indisponible."

.auth_cache <- new.env(parent = emptyenv())


# --- Mots de passe -------------------------------------------------------------

# validate_password()
#   Output : vecteur de messages d'erreur (vide si le mot de passe est accepté)
validate_password <- function(password, username = "") {
  if (!is.character(password) || length(password) != 1 || is.na(password)) {
    return("Mot de passe invalide")
  }
  errs <- character()
  if (nchar(password) < 12)  errs <- c(errs, "12 caractères minimum")
  if (nchar(password) > 128) errs <- c(errs, "128 caractères maximum")
  if (!grepl("[A-Za-z]", password) || !grepl("[0-9]", password)) {
    errs <- c(errs, "doit contenir au moins une lettre et un chiffre")
  }
  if (nzchar(username) && grepl(tolower(username), tolower(password), fixed = TRUE)) {
    errs <- c(errs, "ne doit pas contenir l'identifiant")
  }
  errs
}

hash_password <- function(password) sodium::password_store(password)

verify_password <- function(hash, password) {
  isTRUE(tryCatch(sodium::password_verify(hash, password), error = function(e) FALSE))
}

# Hachage factice vérifié quand le compte n'existe pas : le temps de réponse
# ne révèle pas si l'identifiant existe.
dummy_hash <- function() {
  if (is.null(.auth_cache$dummy)) .auth_cache$dummy <- hash_password("mot-de-passe-factice-0")
  .auth_cache$dummy
}


# --- Jetons JWT ----------------------------------------------------------------

jwt_secret <- function() {
  s <- Sys.getenv("JWT_SECRET")
  if (nchar(s) < 32) stop("JWT_SECRET manquant ou trop court (32 caractères minimum)")
  charToRaw(s)
}

# issue_token()
#   Input  : user_id, session_id (devient le champ jti), expires_at (POSIXct)
issue_token <- function(user_id, session_id, expires_at) {
  claim <- jose::jwt_claim(
    iss = AUTH_ISSUER, sub = as.character(user_id), aud = AUTH_AUDIENCE,
    jti = as.character(session_id),
    iat = as.numeric(Sys.time()), exp = as.numeric(expires_at)
  )
  jose::jwt_encode_hmac(claim, secret = jwt_secret())
}

# decode_token()
#   Vérifie signature, expiration, émetteur et audience.
#   (jose vérifie la signature mais PAS l'expiration : on la contrôle ici.)
#   Output : liste des revendications, ou NULL si le jeton est invalide
decode_token <- function(token) {
  tryCatch({
    if (!is.character(token) || length(token) != 1 || is.na(token)) return(NULL)
    claims <- jose::jwt_decode_hmac(token, secret = jwt_secret())
    if (!identical(claims$iss, AUTH_ISSUER)) return(NULL)
    if (!identical(as.character(claims$aud), AUTH_AUDIENCE)) return(NULL)
    if (is.null(claims$sub) || is.null(claims$jti)) return(NULL)

    exp <- suppressWarnings(as.numeric(claims$exp))
    if (length(exp) != 1 || is.na(exp) || exp <= as.numeric(Sys.time())) return(NULL)

    claims
  }, error = function(e) NULL)
}


# --- Journal d'audit -----------------------------------------------------------

# sanitize_metadata()
#   Retire toute clé qui ressemble à un secret (mot de passe, jeton...).
sanitize_metadata <- function(metadata) {
  if (!length(metadata)) return(list())
  keep <- !grepl("pass|token|secret|authorization|cookie", names(metadata),
                 ignore.case = TRUE)
  metadata[keep]
}

# audit_write()
#   Ajoute une ligne au journal d'audit (table en ajout seul).
audit_write <- function(con, action, result = "success",
                        user_id = NA_character_, object_type = NA_character_,
                        object_id = NA_character_, metadata = list()) {
  stopifnot(result %in% c("success", "failure", "denied"))
  meta <- sanitize_metadata(metadata)
  meta_json <- if (length(meta)) {
    as.character(jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null"))
  } else "{}"

  DBI::dbExecute(con, "
    INSERT INTO audit_log (user_id, action, object_type, object_id, result, metadata)
    VALUES ($1::uuid, $2, $3, $4, $5, $6::jsonb)",
                 params = list(as.character(user_id), action, as.character(object_type),
                               as.character(object_id), result, meta_json))
  invisible(TRUE)
}


# --- Utilisateurs --------------------------------------------------------------

# find_user()
#   Recherche par nom d'utilisateur ou e-mail (insensible à la casse).
find_user <- function(con, identifier) {
  DBI::dbGetQuery(con, "
    SELECT u.id::text AS id, u.username::text AS username, u.email::text AS email,
           u.full_name, u.password_hash, u.status, r.code AS role
    FROM users u JOIN roles r ON r.id = u.role_id
    WHERE u.username = $1::citext OR u.email = $1::citext
    LIMIT 1", params = list(identifier))
}

# user_permissions()
#   Output : vecteur de codes de permission du rôle de l'utilisateur
user_permissions <- function(con, user_id) {
  DBI::dbGetQuery(con, "
    SELECT p.code
    FROM users u
    JOIN role_permissions rp ON rp.role_id = u.role_id
    JOIN permissions p ON p.id = rp.permission_id
    WHERE u.id = $1::uuid
    ORDER BY p.code", params = list(as.character(user_id)))$code
}

# create_user()
#   Crée un utilisateur (mot de passe haché). Le contrôle des droits de
#   l'appelant se fait dans l'API, pas ici.
#   Output : identifiant (uuid) du nouvel utilisateur
create_user <- function(con, username, email, full_name, password, role_code,
                        created_by = NA_character_, must_change_password = FALSE) {
  if (!grepl("^[A-Za-z0-9._-]{3,50}$", username)) {
    stop("Nom d'utilisateur invalide (3 à 50 caractères : lettres, chiffres, . _ -)")
  }
  if (!grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email)) {
    stop("Adresse e-mail invalide")
  }
  if (!nzchar(trimws(full_name))) stop("Nom complet requis")

  pw_errors <- validate_password(password, username)
  if (length(pw_errors)) stop("Mot de passe refusé : ", paste(pw_errors, collapse = " ; "))

  dup <- DBI::dbGetQuery(con, "
    SELECT 1 FROM users WHERE username = $1::citext OR email = $2::citext LIMIT 1",
                         params = list(username, email))
  if (nrow(dup)) stop("Nom d'utilisateur ou e-mail déjà utilisé")

  res <- DBI::dbGetQuery(con, "
    INSERT INTO users (username, email, full_name, password_hash, role_id,
                       created_by, must_change_password)
    SELECT $1::citext, $2::citext, $3, $4, r.id, $5::uuid, $6
    FROM roles r WHERE r.code = $7
    RETURNING id::text AS id",
                         params = list(username, email, trimws(full_name), hash_password(password),
                                       as.character(created_by), isTRUE(must_change_password), role_code))
  if (!nrow(res)) stop("Rôle inconnu : ", role_code)
  res$id[[1]]
}


# --- Connexion, session, déconnexion -------------------------------------------

default_auth_policy <- list(max_failed_attempts = 5, lockout_minutes = 15,
                            session_idle_minutes = 30, session_max_hours = 8)

auth_policy <- function(con) {
  utils::modifyList(default_auth_policy, get_setting(con, "auth_policy", list()))
}

# Échecs récents d'un identifiant, depuis sa dernière connexion réussie.
recent_failures <- function(con, identifier, window_minutes) {
  DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n FROM login_attempts
    WHERE identifier = $1::citext AND success = false
      AND attempted_at > now() - make_interval(mins => $2::int)
      AND attempted_at > COALESCE(
            (SELECT max(attempted_at) FROM login_attempts
              WHERE identifier = $1::citext AND success = true),
            '-infinity'::timestamptz)",
                  params = list(identifier, as.integer(window_minutes)))$n
}

record_attempt <- function(con, identifier, success) {
  DBI::dbExecute(con, "
    INSERT INTO login_attempts (identifier, success, attempted_at)
    VALUES ($1::citext, $2, clock_timestamp())",
                 params = list(identifier, success))
}

# login()
#   Output : list(ok, status = "ok" | "invalid" | "locked", message,
#                 token, expires_at, user, permissions)
#   Le message est le même que le compte existe ou non.
login <- function(con, identifier, password) {
  invalid <- list(ok = FALSE, status = "invalid", message = GENERIC_LOGIN_ERROR)

  identifier <- tolower(trimws(as.character(identifier)))
  if (length(identifier) != 1 || is.na(identifier) || !nzchar(identifier) ||
      nchar(identifier) > 254 || !is.character(password) || length(password) != 1 ||
      is.na(password) || !nzchar(password) || nchar(password) > 1024) {
    return(invalid)
  }

  policy   <- auth_policy(con)
  id_hash  <- substr(digest::digest(identifier, algo = "sha256", serialize = FALSE), 1, 12)

  if (recent_failures(con, identifier, policy$lockout_minutes) >=
      policy$max_failed_attempts) {
    audit_write(con, "auth.login", "denied", metadata = list(reason = "locked",
                                                             identifier_hash = id_hash))
    return(list(ok = FALSE, status = "locked",
                message = "Trop de tentatives. Réessaie plus tard."))
  }

  user    <- find_user(con, identifier)
  exists  <- nrow(user) == 1
  pw_ok   <- verify_password(if (exists) user$password_hash else dummy_hash(), password)

  if (!exists || !pw_ok || user$status != "active") {
    record_attempt(con, identifier, FALSE)
    audit_write(con, "auth.login", "failure",
                user_id = if (exists) user$id else NA_character_,
                metadata = list(identifier_hash = id_hash))
    return(invalid)
  }

  record_attempt(con, identifier, TRUE)
  session <- DBI::dbGetQuery(con, "
    INSERT INTO auth_sessions (user_id, expires_at)
    VALUES ($1::uuid, now() + make_interval(hours => $2::int))
    RETURNING id::text AS id, expires_at",
                             params = list(user$id, as.integer(policy$session_max_hours)))
  DBI::dbExecute(con, "UPDATE users SET last_login_at = now() WHERE id = $1::uuid",
                 params = list(user$id))

  audit_write(con, "auth.login", "success", user_id = user$id,
              object_type = "session", object_id = session$id)

  list(
    ok = TRUE, status = "ok",
    token = issue_token(user$id, session$id, session$expires_at),
    expires_at = session$expires_at,
    user = list(id = user$id, username = user$username,
                full_name = user$full_name, role = user$role),
    permissions = user_permissions(con, user$id)
  )
}

# authenticate_token()
#   Jeton valide ET session active (non révoquée, non expirée, non inactive)
#   ET compte actif. Met à jour la dernière activité.
#   Output : list(user_id, username, full_name, role, session_id, permissions) ou NULL
authenticate_token <- function(con, token) {
  claims <- decode_token(token)
  if (is.null(claims)) return(NULL)

  idle <- auth_policy(con)$session_idle_minutes
  row <- tryCatch(DBI::dbGetQuery(con, "
    SELECT s.id::text AS session_id, u.id::text AS user_id,
           u.username::text AS username, u.full_name, r.code AS role
    FROM auth_sessions s
    JOIN users u ON u.id = s.user_id
    JOIN roles r ON r.id = u.role_id
    WHERE s.id = $1::uuid AND s.user_id = $2::uuid
      AND s.revoked_at IS NULL AND s.expires_at > now()
      AND s.last_seen_at > now() - make_interval(mins => $3::int)
      AND u.status = 'active'",
                                  params = list(as.character(claims$jti), as.character(claims$sub), as.integer(idle))),
                  error = function(e) NULL)
  if (is.null(row) || !nrow(row)) return(NULL)

  DBI::dbExecute(con, "UPDATE auth_sessions SET last_seen_at = now() WHERE id = $1::uuid",
                 params = list(row$session_id))

  list(user_id = row$user_id, username = row$username, full_name = row$full_name,
       role = row$role, session_id = row$session_id,
       permissions = user_permissions(con, row$user_id))
}

# logout()
#   Révoque la session ; le jeton devient inutilisable immédiatement.
logout <- function(con, session_id, user_id = NA_character_) {
  n <- DBI::dbExecute(con, "
    UPDATE auth_sessions SET revoked_at = now()
    WHERE id = $1::uuid AND revoked_at IS NULL", params = list(session_id))
  audit_write(con, "auth.logout", if (n > 0) "success" else "failure",
              user_id = user_id, object_type = "session", object_id = session_id)
  n > 0
}

# has_permission()
#   Output : TRUE si l'utilisateur authentifié possède la permission
has_permission <- function(auth, permission) {
  !is.null(auth) && permission %in% auth$permissions
}
