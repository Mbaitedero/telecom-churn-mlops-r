-- ==============================================================================
-- 001 : RBAC, utilisateurs, sessions, tentatives de connexion, audit, paramètres
-- Variable psql : app_user (rôle applicatif)
-- Convention : une migration appliquée ne se modifie jamais.
-- ==============================================================================

CREATE EXTENSION IF NOT EXISTS citext;

CREATE FUNCTION set_updated_at() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  NEW.updated_at = now();
  RETURN NEW;
END $$;

-- --- RBAC ---------------------------------------------------------------------
CREATE TABLE roles (
  id         smallserial PRIMARY KEY,
  code       text NOT NULL UNIQUE CHECK (code ~ '^[a-z][a-z_]{1,49}$'),
  label      text NOT NULL,
  is_system  boolean NOT NULL DEFAULT false,
  created_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE permissions (
  id          smallserial PRIMARY KEY,
  code        text NOT NULL UNIQUE CHECK (code ~ '^[a-z_]+\.[a-z_]+$'),
  description text NOT NULL
);

CREATE TABLE role_permissions (
  role_id       smallint NOT NULL REFERENCES roles(id) ON DELETE CASCADE,
  permission_id smallint NOT NULL REFERENCES permissions(id) ON DELETE CASCADE,
  PRIMARY KEY (role_id, permission_id)
);

-- --- Utilisateurs -------------------------------------------------------------
CREATE TABLE users (
  id                   uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  username             citext NOT NULL UNIQUE CHECK (username ~ '^[A-Za-z0-9._-]{3,50}$'),
  email                citext NOT NULL UNIQUE CHECK (email ~ '^[^@\s]+@[^@\s]+\.[^@\s]+$'),
  full_name            text NOT NULL CHECK (length(btrim(full_name)) > 0),
  password_hash        text NOT NULL,
  role_id              smallint NOT NULL REFERENCES roles(id),
  status               text NOT NULL DEFAULT 'active' CHECK (status IN ('active', 'disabled')),
  failed_attempts      integer NOT NULL DEFAULT 0 CHECK (failed_attempts >= 0),
  locked_until         timestamptz,
  last_login_at        timestamptz,
  must_change_password boolean NOT NULL DEFAULT false,
  disabled_at          timestamptz,
  created_by           uuid REFERENCES users(id),
  created_at           timestamptz NOT NULL DEFAULT now(),
  updated_at           timestamptz NOT NULL DEFAULT now(),
  CHECK ((status = 'disabled') = (disabled_at IS NOT NULL))
);
CREATE INDEX idx_users_role   ON users(role_id);
CREATE INDEX idx_users_status ON users(status);

CREATE TRIGGER trg_users_updated_at BEFORE UPDATE ON users
  FOR EACH ROW EXECUTE FUNCTION set_updated_at();

-- Garde-fou en base : le dernier super administrateur actif ne peut être ni
-- désactivé, ni rétrogradé, ni supprimé (en plus du contrôle côté API).
CREATE FUNCTION prevent_last_super_admin() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  sa_id     smallint;
  remaining integer;
BEGIN
  SELECT id INTO sa_id FROM roles WHERE code = 'super_admin';

  IF sa_id IS NOT NULL AND OLD.role_id = sa_id AND OLD.status = 'active' THEN
    IF TG_OP = 'DELETE' OR NEW.status <> 'active' OR NEW.role_id <> sa_id THEN
      -- Verrouille les super administrateurs actifs pour éviter une course
      PERFORM 1 FROM users
       WHERE role_id = sa_id AND status = 'active' FOR UPDATE;

      SELECT count(*) INTO remaining FROM users
       WHERE role_id = sa_id AND status = 'active' AND id <> OLD.id;

      IF remaining = 0 THEN
        RAISE EXCEPTION 'Impossible de retirer le dernier super administrateur actif'
          USING ERRCODE = 'integrity_constraint_violation';
      END IF;
    END IF;
  END IF;

  IF TG_OP = 'DELETE' THEN RETURN OLD; END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_users_last_super_admin BEFORE UPDATE OR DELETE ON users
  FOR EACH ROW EXECUTE FUNCTION prevent_last_super_admin();

-- --- Sessions (la déconnexion et l'expiration d'inactivité s'appuient dessus) --
CREATE TABLE auth_sessions (
  id           uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id      uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  issued_at    timestamptz NOT NULL DEFAULT now(),
  last_seen_at timestamptz NOT NULL DEFAULT now(),
  expires_at   timestamptz NOT NULL,
  revoked_at   timestamptz,
  CHECK (expires_at > issued_at)
);
CREATE INDEX idx_sessions_user   ON auth_sessions(user_id);
CREATE INDEX idx_sessions_active ON auth_sessions(expires_at) WHERE revoked_at IS NULL;

-- --- Tentatives de connexion (limitation sans révéler l'existence d'un compte) -
CREATE TABLE login_attempts (
  id           bigserial PRIMARY KEY,
  identifier   citext NOT NULL,
  attempted_at timestamptz NOT NULL DEFAULT now(),
  success      boolean NOT NULL
);
CREATE INDEX idx_login_attempts ON login_attempts(identifier, attempted_at DESC);

-- --- Journal d'audit : ajout seul ----------------------------------------------
-- user_id n'a pas de ON DELETE : un utilisateur ayant une activité se désactive,
-- il ne se supprime pas.
CREATE TABLE audit_log (
  id          bigserial PRIMARY KEY,
  occurred_at timestamptz NOT NULL DEFAULT now(),
  user_id     uuid REFERENCES users(id),
  action      text NOT NULL,
  object_type text,
  object_id   text,
  result      text NOT NULL CHECK (result IN ('success', 'failure', 'denied')),
  metadata    jsonb NOT NULL DEFAULT '{}'::jsonb
);
CREATE INDEX idx_audit_time   ON audit_log(occurred_at DESC);
CREATE INDEX idx_audit_user   ON audit_log(user_id, occurred_at DESC);
CREATE INDEX idx_audit_action ON audit_log(action);

CREATE FUNCTION forbid_audit_change() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  RAISE EXCEPTION 'audit_log est en ajout seul : modification et suppression interdites'
    USING ERRCODE = 'insufficient_privilege';
END $$;

CREATE TRIGGER trg_audit_no_change BEFORE UPDATE OR DELETE ON audit_log
  FOR EACH ROW EXECUTE FUNCTION forbid_audit_change();
CREATE TRIGGER trg_audit_no_truncate BEFORE TRUNCATE ON audit_log
  FOR EACH STATEMENT EXECUTE FUNCTION forbid_audit_change();

-- --- Paramètres ----------------------------------------------------------------
CREATE TABLE settings (
  key         text PRIMARY KEY,
  value       jsonb NOT NULL,
  description text,
  updated_by  uuid REFERENCES users(id),
  updated_at  timestamptz NOT NULL DEFAULT now()
);

-- --- Droits du compte applicatif (moindre privilège) ----------------------------
REVOKE CREATE ON SCHEMA public FROM PUBLIC;
GRANT USAGE ON SCHEMA public TO :"app_user";

GRANT SELECT, INSERT, UPDATE         ON roles            TO :"app_user";
GRANT SELECT                         ON permissions      TO :"app_user";
GRANT SELECT, INSERT, DELETE         ON role_permissions TO :"app_user";
GRANT SELECT, INSERT, UPDATE         ON users            TO :"app_user";  -- pas de DELETE
GRANT SELECT, INSERT, UPDATE, DELETE ON auth_sessions    TO :"app_user";
GRANT SELECT, INSERT, DELETE         ON login_attempts   TO :"app_user";
GRANT SELECT, INSERT                 ON audit_log        TO :"app_user";  -- ajout seul
GRANT SELECT, INSERT, UPDATE         ON settings         TO :"app_user";
GRANT USAGE ON ALL SEQUENCES IN SCHEMA public TO :"app_user";
