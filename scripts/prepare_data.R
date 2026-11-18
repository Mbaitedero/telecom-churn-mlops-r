# Point d'entrée : ingestion puis validation / EDA
source("R/data_ingestion.R")
source("R/data_validation.R")

run_ingestion()
res <- run_validation()

res$plots$contract   # pour afficher un graphique dans RStudio
