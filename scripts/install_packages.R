options(timeout = 1200,
        repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

pkgs <- c(
  # Données
  "dplyr", "tidyr", "purrr", "readr", "stringr", "lubridate", "janitor",
  "here", "yaml", "config", "logger", "jsonlite",
  # Validation
  "validate", "assertr",
  # Modélisation
  "tidymodels", "themis", "xgboost", "ranger", "glmnet", "vip", "probably",
  # Déploiement & API
  "vetiver", "pins", "bundle", "butcher", "plumber", "httr2",
  # Pipeline
  "targets", "tarchetypes",
  # Tests & qualité
  "testthat", "lintr", "styler", "withr", "usethis",
  # Rapports
  "rmarkdown", "knitr"
)

for (p in pkgs) {
  tryCatch(renv::install(p, prompt = FALSE),
           error = function(e) message("ÉCHEC : ", p))
}

manquants <- setdiff(pkgs, rownames(installed.packages()))
if (length(manquants)) {
  message("Manquants, relance le script : ", paste(manquants, collapse = ", "))
} else {
  message("Tous les packages sont installés.")
}
