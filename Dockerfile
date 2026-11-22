# ==============================================================================
# Image unique : API plumber (port 8000) et frontend Shiny (port 3838),
# lancés par deux services docker-compose avec des commandes différentes.
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
      libuv1-dev libglpk-dev libgmp-dev libpq-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# 1) Dépendances R : couche mise en cache tant que renv.lock ne change pas
COPY .Rprofile .Rprofile
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

RUN CODENAME=$(. /etc/os-release && echo "$VERSION_CODENAME") && \
    RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/${CODENAME}/latest" \
    R -e "renv::restore(prompt = FALSE)"

# 2) Code, frontend et modèle
COPY R/ R/
COPY plumber/ plumber/
COPY app/ app/
COPY models/final_model.rds models/final_model.rds

EXPOSE 8000 3838

CMD ["R", "-e", "plumber::plumb(file = 'plumber/api.R')$run(host = '0.0.0.0', port = 8000)"]
