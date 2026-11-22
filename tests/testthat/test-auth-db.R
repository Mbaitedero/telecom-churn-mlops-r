test_that("connexion réussie : session, jeton valide et permissions", {
  skip_if_no_db()
  con <- local_db_txn()
  u <- new_test_user(con, role = "auditor")

  res <- login(con, u$username, u$password)
  expect_true(res$ok)
  expect_equal(res$user$role, "auditor")
  expect_true("audit.read" %in% res$permissions)
  expect_false("users.manage" %in% res$permissions)

  auth <- authenticate_token(con, res$token)
  expect_equal(auth$user_id, u$id)
  expect_true(has_permission(auth, "audit.read"))
  expect_false(has_permission(auth, "users.manage"))
  expect_false(has_permission(NULL, "audit.read"))

  # Connexion par e-mail, sans tenir compte de la casse
  res2 <- login(con, toupper(paste0(u$username, "@example.test")), u$password)
  expect_true(res2$ok)
})

test_that("mauvais mot de passe et compte inconnu donnent la même réponse", {
  skip_if_no_db()
  con <- local_db_txn()
  u <- new_test_user(con)

  bad  <- login(con, u$username, "Mauvais-Mot2Passe-9")
  none <- login(con, "inconnu_xyz_123", "Mauvais-Mot2Passe-9")
  expect_false(bad$ok)
  expect_equal(bad$status, "invalid")
  expect_equal(bad$message, none$message)
  expect_equal(bad$status, none$status)
})

test_that("verrouillage après 5 échecs, même avec le bon mot de passe", {
  skip_if_no_db()
  con <- local_db_txn()
  u <- new_test_user(con)

  for (i in 1:5) expect_equal(login(con, u$username, "Mauvais-Mot2Passe-9")$status, "invalid")
  locked <- login(con, u$username, u$password)
  expect_false(locked$ok)
  expect_equal(locked$status, "locked")

  # Un identifiant inexistant est verrouillé de la même façon (aucune fuite d'existence)
  for (i in 1:5) login(con, "fantome_abc_987", "x1y2z3-Mot2Passe")
  expect_equal(login(con, "fantome_abc_987", "x1y2z3-Mot2Passe")$status, "locked")
})

test_that("compte désactivé : connexion refusée et jeton existant rejeté", {
  skip_if_no_db()
  con <- local_db_txn()
  u <- new_test_user(con)

  res <- login(con, u$username, u$password)
  expect_true(res$ok)

  DBI::dbExecute(con, "UPDATE users SET status = 'disabled', disabled_at = now()
                       WHERE id = $1::uuid", params = list(u$id))
  expect_null(authenticate_token(con, res$token))
  expect_equal(login(con, u$username, u$password)$status, "invalid")
})

test_that("déconnexion : le jeton est révoqué immédiatement", {
  skip_if_no_db()
  con <- local_db_txn()
  u <- new_test_user(con)

  res <- login(con, u$username, u$password)
  claims <- decode_token(res$token)
  expect_false(is.null(authenticate_token(con, res$token)))

  expect_true(logout(con, claims$jti, u$id))
  expect_null(authenticate_token(con, res$token))
  expect_false(logout(con, claims$jti, u$id))   # déjà révoquée
})

test_that("inactivité : la session expire", {
  skip_if_no_db()
  con <- local_db_txn()
  u <- new_test_user(con)

  res <- login(con, u$username, u$password)
  claims <- decode_token(res$token)
  DBI::dbExecute(con, "UPDATE auth_sessions SET last_seen_at = now() - interval '2 hours'
                       WHERE id = $1::uuid", params = list(claims$jti))
  expect_null(authenticate_token(con, res$token))
})

test_that("create_user refuse doublons, mots de passe faibles et rôles inconnus", {
  skip_if_no_db()
  con <- local_db_txn()
  u <- new_test_user(con)

  expect_error(create_user(con, toupper(u$username), "autre@example.test", "X Y",
                           "Mot2Passe-Solide-42", "auditor"), "déjà utilisé")
  expect_error(create_user(con, "nouvel_utilisateur", "faible@example.test", "X Y",
                           "court1", "auditor"), "Mot de passe refusé")
  expect_error(create_user(con, "autre_utilisateur", "role@example.test", "X Y",
                           "Mot2Passe-Solide-42", "role_inexistant"), "Rôle inconnu")
})

test_that("permissions par rôle conformes à la matrice", {
  skip_if_no_db()
  con <- local_db_txn()
  sales <- new_test_user(con, role = "sales_agent")
  admin <- new_test_user(con, role = "admin")
  sa    <- new_test_user(con, role = "super_admin")

  expect_true("campaigns.update_own" %in% user_permissions(con, sales$id))
  expect_false("customers.write" %in% user_permissions(con, sales$id))
  expect_true("users.manage" %in% user_permissions(con, admin$id))
  expect_false("audit.read" %in% user_permissions(con, admin$id))
  expect_length(user_permissions(con, sa$id), 19)
})

test_that("le journal d'audit n'enregistre jamais le mot de passe", {
  skip_if_no_db()
  con <- local_db_txn()
  u <- new_test_user(con)
  bad_pw <- "Mauvais-Mot2Passe-9"

  login(con, u$username, bad_pw)
  last <- DBI::dbGetQuery(con, "
    SELECT result, metadata::text AS meta FROM audit_log
    WHERE action = 'auth.login' ORDER BY id DESC LIMIT 1")
  expect_equal(last$result, "failure")
  expect_false(grepl(bad_pw, last$meta, fixed = TRUE))
  expect_false(grepl(u$password, last$meta, fixed = TRUE))
})
