#!/bin/sh
# ==============================================================================
# db/migrate.sh : applique db/migrations/*.sql dans l'ordre alphabétique.
#  - chaque migration s'exécute dans une transaction et est enregistrée dans
#    schema_migrations avec son empreinte SHA-256 ;
#  - une migration déjà appliquée puis modifiée provoque une erreur :
#    on ne modifie jamais une migration, on en crée une nouvelle.
# ==============================================================================
set -eu

: "${PGHOST:?PGHOST manquant}"
: "${PGDATABASE:?PGDATABASE manquant}"
: "${PGUSER:?PGUSER manquant}"
: "${PGPASSWORD:?PGPASSWORD manquant}"
: "${APP_DB_USER:?APP_DB_USER manquant}"
: "${APP_DB_PASSWORD:?APP_DB_PASSWORD manquant}"

MIGRATIONS_DIR="${MIGRATIONS_DIR:-/db/migrations}"

run_psql() {
  psql -X -q -v ON_ERROR_STOP=1 \
       -v app_user="$APP_DB_USER" -v app_password="$APP_DB_PASSWORD" "$@"
}

i=0
until pg_isready -q; do
  i=$((i + 1))
  if [ "$i" -gt 30 ]; then echo "Base injoignable" >&2; exit 1; fi
  sleep 2
done

echo "==> Rôle applicatif"
run_psql -f /db/bootstrap.sql

run_psql -c "CREATE TABLE IF NOT EXISTS schema_migrations (
  version    text PRIMARY KEY,
  checksum   text NOT NULL,
  applied_at timestamptz NOT NULL DEFAULT now()
);"

for file in $(ls "$MIGRATIONS_DIR"/*.sql 2>/dev/null | sort); do
  version=$(basename "$file" .sql)
  case "$version" in
    *[!A-Za-z0-9_]*) echo "Nom de migration invalide : $version" >&2; exit 1 ;;
  esac

  checksum=$(sha256sum "$file" | cut -d' ' -f1)
  applied=$(run_psql -At -c "SELECT checksum FROM schema_migrations WHERE version = '$version'")

  if [ -n "$applied" ]; then
    if [ "$applied" != "$checksum" ]; then
      echo "ERREUR : $version a été modifiée après son application. Crée une nouvelle migration." >&2
      exit 1
    fi
    echo "==> $version : déjà appliquée"
    continue
  fi

  echo "==> $version : application"
  run_psql --single-transaction -f "$file" \
           -c "INSERT INTO schema_migrations (version, checksum) VALUES ('$version', '$checksum')"
done

echo "Migrations terminées."
