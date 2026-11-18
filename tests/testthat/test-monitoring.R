test_that("PSI numérique : ~0 sans dérive, élevé avec dérive", {
  set.seed(1)
  x <- rnorm(4000)

  expect_equal(psi_numeric(x, x), 0)
  expect_lt(psi_numeric(x[1:2000], x[2001:4000]), 0.10)
  expect_gt(psi_numeric(x, x + 2), 0.25)
})

test_that("PSI catégoriel détecte un changement de proportions", {
  ref <- c(rep("a", 90), rep("b", 10))
  cur <- c(rep("a", 50), rep("b", 50))

  expect_equal(psi_categorical(ref, ref), 0)
  expect_gt(psi_categorical(ref, cur), 0.25)
})

test_that("psi_status applique les seuils", {
  expect_equal(psi_status(c(0.05, 0.15, 0.30)), c("stable", "warning", "alert"))
})

test_that("detect_drift : stable sur même distribution, alerte si décalage", {
  ref  <- make_fake_customers(2000, seed = 1) |> drop_id() |> clean_data()
  cur  <- make_fake_customers(2000, seed = 2) |> drop_id() |> clean_data()
  cols <- split_columns(ref)

  stable <- detect_drift(ref, cur, cols$numeric, cols$categorical)
  expect_true(all(stable$status == "stable"))

  cur$MonthlyCharges <- cur$MonthlyCharges * 1.5
  shifted <- detect_drift(ref, cur, cols$numeric, cols$categorical)
  expect_equal(shifted$status[shifted$variable == "MonthlyCharges"], "alert")
  expect_equal(shifted$variable[1], "MonthlyCharges")   # trié par PSI décroissant
})
