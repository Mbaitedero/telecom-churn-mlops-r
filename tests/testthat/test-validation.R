test_that("validate_data accepte des données conformes", {
  df <- make_fake_customers(300) |> drop_id()
  expect_no_error(suppressMessages(validate_data(df)))
})

test_that("validate_data rejette une modalité de Churn invalide", {
  df <- make_fake_customers(300) |> drop_id()
  df$Churn[1] <- "Maybe"
  expect_error(validate_data(df), "Validation échouée")
})

test_that("validate_data rejette une colonne manquante", {
  df <- make_fake_customers(300) |> drop_id() |> dplyr::select(-tenure)
  expect_error(validate_data(df), "Colonnes manquantes")
})

test_that("validate_data rejette un tenure négatif", {
  df <- make_fake_customers(300) |> drop_id()
  df$tenure[1] <- -5
  expect_error(validate_data(df), "tenure")
})
