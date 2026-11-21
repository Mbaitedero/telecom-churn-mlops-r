# ==============================================================================
# Image de l'API de prédiction du churn (R 4.5.2 + renv + plumber)
# Build  : docker build -t telecom-churn-api .
# Run    : docker run -p 8000:8000 telecom-churn-api
# ==============================================================================
FROM rocker/r-ver:4.5.2

# Bibliothèques système nécessaires aux packages R (binaires Posit PM)
RUN apt-get update && apt-get install -y --no-install-recommends \
      curl ca-certificates \
      libcurl4-openssl-dev libssl-dev libxml2-dev libsodium-dev libgit2-dev \
      libuv1-dev libglpk-dev libgmp-dev \
      libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
      libpng-dev libjpeg-dev libtiff-dev libicu-dev \
      zlib1g-dev libbz2-dev liblzma-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# 1) Dépendances R : couche mise en cache tant que renv.lock ne change pas
COPY .Rprofile .Rprofile
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Binaires précompilés (Posit Package Manager) pour éviter de tout compiler
RUN CODENAME=$(. /etc/os-release && echo "$VERSION_CODENAME") && \
    RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/${CODENAME}/latest" \
    R -e "renv::restore(prompt = FALSE)"

# 2) Code et modèle
COPY R/ R/
COPY plumber/ plumber/
COPY models/final_model.rds models/final_model.rds

EXPOSE 8000

CMD ["R", "-e", "plumber::plumb(file = 'plumber/api.R')$run(host = '0.0.0.0', port = 8000)"]
