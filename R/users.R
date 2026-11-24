# ==============================================================================
# Fichier : R/users.R
# Rôle    : Gestion des utilisateurs : liste, création, modification,
#           activation/désactivation, changement de mot de passe, avec les
#           règles métier (hiérarchie des rôles, dernier super administrateur,
#           pas de modification de son propre rôle, pas d'auto-désactivation).
#
# Input   : con (DBI), actor = list(user_id, role) issu de authenticate_token()
# Output  : data.frames / listes SANS hachage de mot de passe ;
#           erreurs de classe "app_error" portant un code HTTP (champ status)
# Dépend  : R/db.R et R/auth.R
# ==============================================================================


# --- Erreurs applicatives -------------------------------------------------------

# app_error()
#   Erreur "attendue" (validation, droits, conflit) : l'API la traduit en code
#   HTTP et renvoie son message. Toute autre erreur reste une erreur interne.
app_error <- function(message, status = 400) {
  structure(class = c("app_error", "error", "condition"),
            list(message = message, call = NULL, status = status))
}

is_uuid <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) &&
    grepl("^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$", tolower(x))
}

is_text <- function(x, max_chars = 254) {
  is.character(x) && length(x) == 1 && !is.na(x) && nchar(x) <= max_chars
}


# --- Hiérarchie des rôles -------------------------------------------------------

ROLE_MANAGEMENT <- list(
  super_admin = c("super_admin", "admin", "data_ai", "sales_agent", "auditor"),
  admin       = c("data_ai", "sales_agent", "auditor")
)

# can_manage_role()
#   Output : TRUE si un utilisateur de ce rôle peut gérer (créer, modifier,
#            désactiver) un compte de ce rôle, ou attribuer ce rôle.
can_manage_role <- function(actor_role, target_role) {
  isTRUE(is_text(actor_role) && is_text(target_role) &&
           target_role %in% ROLE_MANAGEMENT[[actor_role]])
}

known_roles <- function(con) DBI::dbGetQuery(con, "SELECT code FROM roles")$code

active_super_admin_count <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n FROM users u JOIN roles r ON r.id = u.role_id
    WHERE r.code = 'super_admin' AND u.status = 'active'")$n
}

# Refuse de retirer (désactiver ou rétrograder) le dernier super administrateur actif.
guard_last_super_admin <- function(con, target) {
  if (identical(target$role, "super_admin") && identical(target$status, "active") &&
      active_super_admin_count(con) <= 1) {
    stop(app_error("Impossible de retirer le dernier super administrateur actif", 409))
  }
}


# --- Lecture --------------------------------------------------------------------

USER_SELECT <- "
  SELECT u.id::text AS id, u.username::text AS username, u.email::text AS email,
         u.full_name, r.code AS role, u.status, u.last_login_at, u.created_at
  FROM users u JOIN roles r ON r.id = u.role_id"

format_user_dates <- function(df) {
  iso <- function(x) format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  df$last_login_at <- iso(df$last_login_at)
  df$created_at    <- iso(df$created_at)
  df
}

# get_user()
#   Output : data.frame d'une ligne, ou NULL si l'identifiant est invalide/inconnu
get_user <- function(con, id) {
  if (!is_uuid(id)) return(NULL)
  row <- DBI::dbGetQuery(con, paste(USER_SELECT, "WHERE u.id = $1::uuid"),
                         params = list(tolower(id)))
  if (!nrow(row)) return(NULL)
  format_user_dates(row)
}

require_user <- function(con, id) {
  u <- get_user(con, id)
  if (is.null(u)) stop(app_error("Utilisateur introuvable", 404))
  u
}

# Échappe les jokers LIKE (\ % _) pour une recherche littérale
escape_like <- function(x) gsub("([\\\\%_])", "\\\\\\1", x)

# list_users()
#   Liste paginée avec recherche (nom d'utilisateur, e-mail, nom complet) et
#   filtres par rôle et statut. Tri : plus récents d'abord.
#   Output : list(items, page, page_size, total, pages)
list_users <- function(con, q = NULL, role = NULL, status = NULL,
                       page = 1, page_size = 20) {
  page      <- suppressWarnings(as.integer(page))
  page_size <- suppressWarnings(as.integer(page_size))
  if (length(page) != 1 || is.na(page) || page < 1) {
    stop(app_error("Paramètre page invalide"))
  }
  if (length(page_size) != 1 || is.na(page_size) || page_size < 1 || page_size > 100) {
    stop(app_error("page_size doit être compris entre 1 et 100"))
  }

  clean <- function(x) {
    if (is.null(x) || length(x) != 1 || is.na(x)) return(NA_character_)
    x <- trimws(as.character(x))
    if (nzchar(x)) x else NA_character_
  }
  q <- clean(q); role <- clean(role); status <- clean(status)
  if (!is.na(q) && nchar(q) > 100) stop(app_error("Recherche trop longue (100 caractères maximum)"))
  if (!is.na(status) && !status %in% c("active", "disabled")) {
    stop(app_error("status invalide"))
  }
  pattern <- if (is.na(q)) NA_character_ else paste0("%", escape_like(q), "%")

  where <- "
    WHERE ($1::text IS NULL OR u.username::text ILIKE $1::text
           OR u.email::text ILIKE $1::text OR u.full_name ILIKE $1::text)
      AND ($2::text IS NULL OR r.code = $2::text)
      AND ($3::text IS NULL OR u.status = $3::text)"
  filter_params <- list(pattern, role, status)

  total <- DBI::dbGetQuery(
    con, paste("SELECT count(*)::int AS n FROM users u JOIN roles r ON r.id = u.role_id", where),
    params = filter_params)$n

  items <- DBI::dbGetQuery(
    con, paste(USER_SELECT, where, "ORDER BY u.created_at DESC, u.id LIMIT $4 OFFSET $5"),
    params = c(filter_params, list(page_size, (page - 1L) * page_size)))

  list(items = format_user_dates(items), page = page, page_size = page_size,
       total = total, pages = as.integer(ceiling(total / page_size)))
}


# --- Création -------------------------------------------------------------------

# create_user_as()
#   Crée un utilisateur au nom de actor (contrôle de la hiérarchie des rôles).
#   Output : data.frame d'une ligne (sans hachage)
create_user_as <- function(con, actor, username, email, full_name, password, role) {
  fields <- list(username = username, email = email, full_name = full_name,
                 password = password, role = role)
  ok <- vapply(fields, function(x) is_text(x, 1024), logical(1))
  if (!all(ok)) {
    stop(app_error(paste("Champs invalides ou manquants :",
                         paste(names(fields)[!ok], collapse = ", "))))
  }
  if (!role %in% known_roles(con)) stop(app_error(paste("Rôle inconnu :", role)))
  if (!can_manage_role(actor$role, role)) {
    stop(app_error("Accès refusé : tu ne peux pas attribuer ce rôle", 403))
  }

  id <- tryCatch(
    create_user(con, username, email, full_name, password, role,
                created_by = actor$user_id),
    error = function(e) {
      msg <- conditionMessage(e)
      # create_user() lève des erreurs de validation en clair : on les traduit
      if (grepl("^(Nom d'utilisateur|Adresse e-mail|Nom complet|Mot de passe refusé|Rôle inconnu)", msg)) {
        stop(app_error(msg, if (grepl("déjà utilisé", msg)) 409 else 400))
      }
      stop(e)
    })

  audit_write(con, "user.create", "success", user_id = actor$user_id,
              object_type = "user", object_id = id, metadata = list(role = role))
  require_user(con, id)
}


# --- Modification ---------------------------------------------------------------

# update_user()
#   Modifie email, full_name et/ou role (liste nommée `fields`).
#   Le changement de rôle prend effet immédiatement (les permissions sont lues
#   en base à chaque requête).
update_user <- function(con, actor, id, fields) {
  target <- require_user(con, id)
  if (!can_manage_role(actor$role, target$role)) stop(app_error("Accès refusé", 403))

  if (!is.list(fields) || !length(fields)) stop(app_error("Aucun champ à modifier"))
  unknown <- setdiff(names(fields), c("email", "full_name", "role"))
  if (length(unknown)) {
    stop(app_error(paste("Champs non modifiables :", paste(unknown, collapse = ", "))))
  }

  email_new <- name_new <- role_new <- NA_character_

  if ("email" %in% names(fields)) {
    e <- fields$email
    if (!is_text(e) || !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", e)) {
      stop(app_error("Adresse e-mail invalide"))
    }
    dup <- DBI::dbGetQuery(con, "SELECT 1 FROM users WHERE email = $1::citext AND id <> $2::uuid",
                           params = list(e, target$id))
    if (nrow(dup)) stop(app_error("Adresse e-mail déjà utilisée", 409))
    email_new <- e
  }

  if ("full_name" %in% names(fields)) {
    n <- fields$full_name
    if (!is_text(n, 200) || !nzchar(trimws(n))) stop(app_error("Nom complet invalide"))
    name_new <- trimws(n)
  }

  if ("role" %in% names(fields)) {
    r <- fields$role
    if (!is_text(r) || !r %in% known_roles(con)) stop(app_error("Rôle inconnu"))
    if (!identical(r, target$role)) {
      if (identical(tolower(target$id), tolower(actor$user_id))) {
        stop(app_error("Tu ne peux pas modifier ton propre rôle", 403))
      }
      if (!can_manage_role(actor$role, r)) {
        stop(app_error("Accès refusé : tu ne peux pas attribuer ce rôle", 403))
      }
      guard_last_super_admin(con, target)
      role_new <- r
    }
  }

  DBI::dbExecute(con, "
    UPDATE users SET
      email     = COALESCE($2::citext, email),
      full_name = COALESCE($3::text, full_name),
      role_id   = COALESCE((SELECT id FROM roles WHERE code = $4::text), role_id)
    WHERE id = $1::uuid",
                 params = list(target$id, email_new, name_new, role_new))

  meta <- list(fields = names(fields))
  if (!is.na(role_new)) meta <- c(meta, list(role_from = target$role, role_to = role_new))
  audit_write(con, "user.update", "success", user_id = actor$user_id,
              object_type = "user", object_id = target$id, metadata = meta)
  require_user(con, target$id)
}


# --- Activation / désactivation -------------------------------------------------

# set_user_status()
#   status = "active" | "disabled". La désactivation révoque toutes les sessions.
#   Idempotent : renvoie l'utilisateur tel quel s'il a déjà ce statut.
set_user_status <- function(con, actor, id, status) {
  if (!is_text(status) || !status %in% c("active", "disabled")) {
    stop(app_error("status invalide"))
  }
  target <- require_user(con, id)
  if (!can_manage_role(actor$role, target$role)) stop(app_error("Accès refusé", 403))

  if (status == "disabled") {
    if (identical(tolower(target$id), tolower(actor$user_id))) {
      stop(app_error("Tu ne peux pas désactiver ton propre compte", 403))
    }
    guard_last_super_admin(con, target)
  }
  if (identical(target$status, status)) return(target)

  DBI::dbExecute(con, "
    UPDATE users SET status = $2::text,
      disabled_at = CASE WHEN $2::text = 'disabled' THEN now() ELSE NULL END
    WHERE id = $1::uuid", params = list(target$id, status))

  if (status == "disabled") {
    DBI::dbExecute(con, "
      UPDATE auth_sessions SET revoked_at = now()
      WHERE user_id = $1::uuid AND revoked_at IS NULL", params = list(target$id))
  }

  audit_write(con, if (status == "disabled") "user.disable" else "user.enable",
              "success", user_id = actor$user_id,
              object_type = "user", object_id = target$id)
  require_user(con, target$id)
}


# --- Mot de passe ---------------------------------------------------------------

# change_password()
#   L'utilisateur authentifié change son mot de passe : l'ancien est vérifié, la
#   politique s'applique, les AUTRES sessions sont révoquées (la session
#   courante reste valide).
change_password <- function(con, auth, current_password, new_password) {
  if (!is_text(current_password, 1024) || !is_text(new_password, 1024)) {
    stop(app_error("Champs invalides ou manquants"))
  }
  row <- DBI::dbGetQuery(con, "
    SELECT password_hash, username::text AS username FROM users
    WHERE id = $1::uuid AND status = 'active'", params = list(auth$user_id))

  if (!nrow(row) || !verify_password(row$password_hash, current_password)) {
    audit_write(con, "auth.change_password", "failure", user_id = auth$user_id,
                object_type = "user", object_id = auth$user_id)
    stop(app_error("Mot de passe actuel incorrect", 403))
  }

  errs <- validate_password(new_password, row$username)
  if (length(errs)) {
    stop(app_error(paste("Mot de passe refusé :", paste(errs, collapse = " ; "))))
  }
  if (identical(current_password, new_password)) {
    stop(app_error("Le nouveau mot de passe doit être différent de l'ancien"))
  }

  DBI::dbExecute(con, "
    UPDATE users SET password_hash = $2, must_change_password = false
    WHERE id = $1::uuid", params = list(auth$user_id, hash_password(new_password)))
  DBI::dbExecute(con, "
    UPDATE auth_sessions SET revoked_at = now()
    WHERE user_id = $1::uuid AND id <> $2::uuid AND revoked_at IS NULL",
                 params = list(auth$user_id, auth$session_id))

  audit_write(con, "auth.change_password", "success", user_id = auth$user_id,
              object_type = "user", object_id = auth$user_id)
  invisible(TRUE)
}
