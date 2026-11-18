test_that("convert_total_charges convertit le texte en numérique", {
  df  <- data.frame(TotalCharges = c("29.85", " ", "1889.5"))
  out <- convert_total_charges(df)

  expect_type(out$TotalCharges, "double")
  expect_equal(out$TotalCharges, c(29.85, NA, 1889.5))
})

test_that("clean_data corrige TotalCharges, SeniorCitizen et Churn", {
  df <- data.frame(
    tenure = c(0, 5), TotalCharges = c(NA, 100),
    SeniorCitizen = c(0, 1), Churn = c("No", "Yes")
  )
  out <- clean_data(df)

  expect_equal(out$TotalCharges, c(0, 100))          # NA -> 0 si tenure = 0
  expect_equal(out$SeniorCitizen, c("No", "Yes"))
  expect_equal(levels(out$Churn), c("Yes", "No"))    # "Yes" = classe positive
})

test_that("clean_data fonctionne sans colonne Churn (cas de l'API)", {
  df <- data.frame(tenure = 3, TotalCharges = "50.5", SeniorCitizen = 0)
  expect_no_error(out <- clean_data(df))
  expect_false("Churn" %in% names(out))
})

test_that("split_columns sépare numériques et catégorielles", {
  df   <- make_fake_customers(200) |> drop_id() |> clean_data()
  cols <- split_columns(df)

  expect_setequal(cols$numeric, c("tenure", "MonthlyCharges", "TotalCharges"))
  expect_length(cols$categorical, 16)
  expect_false("Churn" %in% c(cols$numeric, cols$categorical))
})

test_that("split_xy sépare X et y", {
  df <- make_fake_customers(100) |> drop_id() |> clean_data()
  xy <- split_xy(df)

  expect_false("Churn" %in% names(xy$X))
  expect_length(xy$y, nrow(df))
})
