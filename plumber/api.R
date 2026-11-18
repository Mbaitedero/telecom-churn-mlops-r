# ==============================================================================
# Fichier : plumber/api.R
# Rôle    : API REST de prédiction du churn télécom.
#           GET  /health  : état du service
#           POST /predict : risque de churn pour un ou plusieurs clients (JSON)
#
# Lancer  : plumber::plumb(file = "plumber/api.R")$run(port = 8000)
# Docs    : http://127.0.0.1:8000/__docs__/
# ==============================================================================

library(plumber)
library(parsnip)
library(workflows)
library(recipes)

# --- Racine du projet ---------------------------------------------------------
# plumb() se place dans le dossier plumber/ : on retrouve donc la racine en
# cherchant R/preprocessing.R ici, puis dans le dossier parent.
root <- if (file.exists("R/preprocessing.R")) {
  "."
} else if (file.exists("../R/preprocessing.R")) {
  ".."
} else {
  stop("Racine du projet introuvable : lance l'API depuis telecom-churn-mlops-r/")
}

source(file.path(root, "R", "preprocessing.R"))
source(file.path(root, "R", "predict.R"))

# Le modèle est chargé une seule fois, au démarrage de l'API
final <- read_final_model(file.path(root, "models", "final_model.rds"))


#* Vérifie que l'API et le modèle sont disponibles
#* @get /health
#* @serializer json list(auto_unbox = TRUE)
function() {
  list(status = "ok", model_loaded = !is.null(final$model),
       threshold = final$threshold)
}


#* Prédit le risque de churn. Corps JSON : un client (objet) ou plusieurs (liste)
#* @post /predict
#* @serializer json list(auto_unbox = TRUE)
function(req, res) {
  new_data <- tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyDataFrame = TRUE),
    error = function(e) NULL
  )

  if (is.null(new_data)) {
    res$status <- 400
    return(list(error = "Corps de requête JSON invalide"))
  }
  new_data <- as.data.frame(new_data)

  absentes <- missing_columns(new_data)
  if (length(absentes) > 0) {
    res$status <- 400
    return(list(error = "Colonnes manquantes",
                colonnes = absentes))
  }

  tryCatch(
    {
      preds <- predict_churn(final, new_data)
      list(threshold = final$threshold,
           n = nrow(preds),
           predictions = preds)
    },
    error = function(e) {
      res$status <- 500
      list(error = "Erreur de prédiction", detail = conditionMessage(e))
    }
  )
}
