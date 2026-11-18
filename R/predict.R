# ==============================================================================
# Fichier : R/predict.R
# Rôle    : Sauvegarde / chargement du modèle final, contrôle des données
#           reçues et prédiction du churn sur de nouveaux clients
#           (utilisé par l'API plumber).
#
# Input   : modèle final (workflow + seuil), nouveaux clients (data.frame brut)
# Output  : models/final_model.rds ; tibble de prédictions
# ==============================================================================


# required_columns()
#   Colonnes que le client doit fournir (customerID et Churn non requis).
#   Output : vecteur de noms de colonnes
required_columns <- function() {
  c("gender", "SeniorCitizen", "Partner", "Dependents", "tenure",
    "PhoneService", "MultipleLines", "InternetService", "OnlineSecurity",
    "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV",
    "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod",
    "MonthlyCharges", "TotalCharges")
}


# missing_columns()
#   Input  : new_data = data.frame reçu
#   Output : noms des colonnes manquantes (vecteur vide si tout est présent)
missing_columns <- function(new_data) {
  setdiff(required_columns(), names(new_data))
}


# save_final_model()
#   Input  : final = list(model, threshold), path = fichier de sortie
#   Output : chemin du fichier (utilisable comme cible "file" de targets)
save_final_model <- function(final, path = "models/final_model.rds") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(final, path)
  path
}


# read_final_model()
#   Input  : path = fichier du modèle final
#   Output : list(model, threshold)
read_final_model <- function(path = "models/final_model.rds") {
  stopifnot("Modèle final introuvable : lance targets::tar_make()" =
              file.exists(path))
  readRDS(path)
}


# predict_churn()
#   Prédit le risque de churn de nouveaux clients. Applique le même nettoyage
#   que pendant l'entraînement (TotalCharges en numérique, SeniorCitizen...).
#   Input  : final = list(model, threshold), new_data = clients (colonnes
#            brutes, sans Churn ; customerID facultatif)
#   Output : tibble(churn_probability, prediction = "Yes"/"No")
predict_churn <- function(final, new_data) {
  clean <- new_data |>
    dplyr::select(-dplyr::any_of(c("customerID", "Churn"))) |>
    clean_data()

  prob <- predict(final$model, clean, type = "prob")$.pred_Yes
  tibble::tibble(
    churn_probability = round(prob, 4),
    prediction        = ifelse(prob >= final$threshold, "Yes", "No")
  )
}
