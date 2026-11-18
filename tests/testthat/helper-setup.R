# ==============================================================================
# Fichier : tests/testthat/helper-setup.R
# Rôle    : Charge les fonctions du projet et fournit des données de test
#           synthétiques (aucune dépendance aux vrais fichiers de data/).
# ==============================================================================

library(testthat)
library(parsnip)
library(workflows)
library(recipes)

source(here::here("R", "data_ingestion.R"))
source(here::here("R", "data_validation.R"))
source(here::here("R", "preprocessing.R"))
source(here::here("R", "predict.R"))
source(here::here("R", "monitoring.R"))


# make_fake_customers()
#   Génère n clients fictifs avec les 21 colonnes du jeu Telco (≈ 26,5 % de churn).
make_fake_customers <- function(n = 1000, seed = 1) {
  set.seed(seed)
  pick <- function(x, p = NULL) sample(x, n, replace = TRUE, prob = p)

  tenure  <- sample(0:72, n, replace = TRUE)
  monthly <- round(stats::runif(n, 20, 120), 2)

  tibble::tibble(
    customerID       = paste0("ID-", seq_len(n)),
    gender           = pick(c("Female", "Male")),
    SeniorCitizen    = pick(c(0, 1), c(0.84, 0.16)),
    Partner          = pick(c("Yes", "No")),
    Dependents       = pick(c("Yes", "No")),
    tenure           = tenure,
    PhoneService     = pick(c("Yes", "No")),
    MultipleLines    = pick(c("Yes", "No", "No phone service")),
    InternetService  = pick(c("DSL", "Fiber optic", "No")),
    OnlineSecurity   = pick(c("Yes", "No", "No internet service")),
    OnlineBackup     = pick(c("Yes", "No", "No internet service")),
    DeviceProtection = pick(c("Yes", "No", "No internet service")),
    TechSupport      = pick(c("Yes", "No", "No internet service")),
    StreamingTV      = pick(c("Yes", "No", "No internet service")),
    StreamingMovies  = pick(c("Yes", "No", "No internet service")),
    Contract         = pick(c("Month-to-month", "One year", "Two year")),
    PaperlessBilling = pick(c("Yes", "No")),
    PaymentMethod    = pick(c("Electronic check", "Mailed check",
                              "Bank transfer (automatic)",
                              "Credit card (automatic)")),
    MonthlyCharges   = monthly,
    TotalCharges     = round(monthly * pmax(tenure, 1), 2),
    Churn            = pick(c("Yes", "No"), c(0.265, 0.735))
  )
}


# example_client()
#   Un client à haut risque, tel que l'API le reçoit (sans Churn ni customerID).
example_client <- function() {
  data.frame(
    gender = "Female", SeniorCitizen = 0, Partner = "No", Dependents = "No",
    tenure = 2, PhoneService = "Yes", MultipleLines = "No",
    InternetService = "Fiber optic", OnlineSecurity = "No", OnlineBackup = "No",
    DeviceProtection = "No", TechSupport = "No", StreamingTV = "No",
    StreamingMovies = "No", Contract = "Month-to-month", PaperlessBilling = "Yes",
    PaymentMethod = "Electronic check", MonthlyCharges = 70.7,
    TotalCharges = 151.65
  )
}
