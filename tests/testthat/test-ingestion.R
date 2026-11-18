test_that("drop_id supprime customerID", {
  out <- drop_id(make_fake_customers(50))
  expect_false("customerID" %in% names(out))
})

test_that("split_three produit 3 jeux disjoints aux bonnes proportions", {
  df <- make_fake_customers(1000)
  sp <- split_three(df, seed = 123)

  expect_named(sp, c("train", "valid", "test"))
  expect_equal(sum(vapply(sp, nrow, integer(1))), nrow(df))
  expect_equal(nrow(sp$train), 700, tolerance = 0.02)

  expect_length(intersect(sp$train$customerID, sp$valid$customerID), 0)
  expect_length(intersect(sp$train$customerID, sp$test$customerID), 0)
  expect_length(intersect(sp$valid$customerID, sp$test$customerID), 0)
})

test_that("split_three conserve la proportion de churn (stratification)", {
  sp <- split_three(make_fake_customers(1000), seed = 123)
  expect_no_error(capture.output(check_split_balance(sp, tol = 2)))
})

test_that("check_split_balance détecte un déséquilibre", {
  bad <- list(a = data.frame(Churn = rep("No", 10)),
              b = data.frame(Churn = rep("Yes", 10)))
  expect_error(capture.output(check_split_balance(bad)), "déséquilibrées")
})

test_that("split_three refuse des proportions qui ne font pas 1", {
  expect_error(split_three(make_fake_customers(50),
                           prop = c(train = 0.5, valid = 0.3, test = 0.3)))
})
