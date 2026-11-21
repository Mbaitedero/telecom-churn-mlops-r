-- ==============================================================================
-- db/bootstrap.sql : rôle applicatif (exécuté à chaque migration, idempotent).
-- Variables psql : app_user, app_password (viennent de l'environnement).
-- Le rôle applicatif ne peut ni créer de rôles, ni de bases, ni de tables.
-- ==============================================================================

SELECT format('CREATE ROLE %I LOGIN PASSWORD %L NOSUPERUSER NOCREATEDB NOCREATEROLE',
              :'app_user', :'app_password')
WHERE NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = :'app_user')
\gexec

-- Garde le mot de passe synchronisé avec .env
SELECT format('ALTER ROLE %I PASSWORD %L', :'app_user', :'app_password')
\gexec

SELECT format('GRANT CONNECT ON DATABASE %I TO %I', current_database(), :'app_user')
\gexec
