test_that("validate_password applique la politique", {
  expect_length(validate_password("Solide-Mot2Passe", "bob"), 0)
  expect_true(length(validate_password("court1", "bob")) > 0)
  expect_true(length(validate_password("seulement-des-lettres", "bob")) > 0)
  expect_true(length(validate_password("123456789012345", "bob")) > 0)
  expect_true(length(validate_password("Bob-et-moi-2026-ok", "bob")) > 0)
  expect_true(length(validate_password(NA_character_)) > 0)
})

test_that("hachage scrypt (libsodium) : vérification correcte et sel aléatoire", {
  h1 <- hash_password("Solide-Mot2Passe")
  h2 <- hash_password("Solide-Mot2Passe")
  expect_true(grepl("^\\$[0-9a-z]+\\$", h1))   # format $<algorithme>$...
  expect_gt(nchar(h1), 50)
  expect_false(identical(h1, h2))
  expect_true(verify_password(h1, "Solide-Mot2Passe"))
  expect_false(verify_password(h1, "Autre-Mot2Passe"))
  expect_false(verify_password("pas-un-hachage", "Solide-Mot2Passe"))
})

test_that("sanitize_metadata retire les secrets", {
  m <- sanitize_metadata(list(password = "x", Token = "y", api_secret = "z", ok = 1))
  expect_named(m, "ok")
  expect_equal(sanitize_metadata(list()), list())
})

test_that("jeton JWT : aller-retour, falsification, mauvaise clé, expiration", {
  tok <- issue_token("11111111-1111-1111-1111-111111111111",
                     "22222222-2222-2222-2222-222222222222", Sys.time() + 3600)
  claims <- decode_token(tok)
  expect_equal(claims$sub, "11111111-1111-1111-1111-111111111111")
  expect_equal(claims$jti, "22222222-2222-2222-2222-222222222222")

  # Falsification de la charge utile
  parts <- strsplit(tok, ".", fixed = TRUE)[[1]]
  parts[2] <- paste0(substr(parts[2], 1, nchar(parts[2]) - 2), "AA")
  expect_null(decode_token(paste(parts, collapse = ".")))

  # Mauvaise clé
  withr::with_envvar(c(JWT_SECRET = "une-autre-cle-de-test-0123456789abcdef"),
                     expect_null(decode_token(tok)))

  # Expiré
  old <- issue_token("11111111-1111-1111-1111-111111111111",
                     "22222222-2222-2222-2222-222222222222", Sys.time() - 60)
  expect_null(decode_token(old))

  expect_null(decode_token(NA_character_))
  expect_null(decode_token("n'importe quoi"))
})

test_that("JWT_SECRET trop court est refusé", {
  withr::with_envvar(c(JWT_SECRET = "court"), expect_error(jwt_secret(), "JWT_SECRET"))
})
