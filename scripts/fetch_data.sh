#!/usr/bin/env bash
# ==============================================================================
# Télécharge data/raw/churn.csv depuis Google Drive et vérifie son empreinte.
# Usage : bash scripts/fetch_data.sh
# (l'identifiant peut être remplacé : DATA_FILE_ID=<id> bash scripts/fetch_data.sh)
# ==============================================================================
set -euo pipefail

FILE_ID="${DATA_FILE_ID:-1oGUoaZA8AJwF3SZxOwNffosfiqHlmJFu}"

mkdir -p data/raw
curl -fL --retry 5 --retry-delay 3 \
  -o data/raw/churn.csv \
  "https://drive.google.com/uc?export=download&id=${FILE_ID}"

(cd data/raw && sha256sum -c churn.csv.sha256)
echo "Données téléchargées et vérifiées."
