test_that("required_columns exclut customerID et Churn", {
  expect_length(required_columns(), 19)
  expect_false(any(c("customerID", "Churn") %in% required_columns()))
})

test_that("missing_columns repère les colonnes absentes", {
  expect_length(missing_columns(example_client()), 0)

  partial <- example_client()[, c("gender", "tenure")]
  expect_setequal(missing_columns(partial),
                  setdiff(required_columns(), c("gender", "tenure")))
})

test_that("predict_churn renvoie une probabilité et une classe cohérentes", {
  path <- here::here("models", "final_model.rds")
  skip_if_not(file.exists(path), "models/final_model.rds absent : lance tar_make()")

  final <- read_final_model(path)
  preds <- predict_churn(final, example_client())

  expect_equal(nrow(preds), 1)
  expect_gte(preds$churn_probability, 0)
  expect_lte(preds$churn_probability, 1)
  expect_equal(preds$prediction,
               ifelse(preds$churn_probability >= final$threshold, "Yes", "No"))
})

test_that("predict_churn accepte customerID et une modalité inconnue", {
  path <- here::here("models", "final_model.rds")
  skip_if_not(file.exists(path), "models/final_model.rds absent : lance tar_make()")

  final  <- read_final_model(path)
  client <- cbind(customerID = "X-1", example_client())
  client$InternetService <- "Satellite"   # modalité jamais vue à l'entraînement

  expect_no_error(preds <- predict_churn(final, client))
  expect_equal(nrow(preds), 1)
})
