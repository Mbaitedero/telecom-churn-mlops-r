#!/usr/bin/env bash
# ==============================================================================
# Recrée la base de TEST (telecom_platform_test) puis y applique les migrations.
# Ne touche jamais à la base de développement (telecom_platform).
# ==============================================================================
set -euo pipefail

TEST_DB="telecom_platform_test"

echo "==> Recréation de la base de test : ${TEST_DB}"
docker compose exec -T db sh -c "psql -v ON_ERROR_STOP=1 -U \"\$POSTGRES_USER\" -d postgres -c 'DROP DATABASE IF EXISTS ${TEST_DB} WITH (FORCE)' -c 'CREATE DATABASE ${TEST_DB}'"

docker compose run --rm -e PGDATABASE="${TEST_DB}" migrate
echo "Base de test prête : ${TEST_DB}"
