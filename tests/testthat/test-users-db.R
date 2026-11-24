rand_tag <- function() paste(sample(letters, 8, TRUE), collapse = "")

test_that("list_users : pagination, recherche, filtres, jokers et absence de hachage", {
  skip_if_no_db()
  con <- local_db_txn()
  tag <- rand_tag()
  mk <- function(role, s) {
    n <- paste0("lst_", tag, "_", s)
    create_user(con, n, paste0(n, "@example.test"), "Liste Test", "Mot2Passe-Solide-42", role)
  }
  mk("auditor", "a"); mk("auditor", "b"); mk("sales_agent", "c")
  q <- paste0("lst_", tag)

  all <- list_users(con, q = q)
  expect_equal(all$total, 3)
  expect_false("password_hash" %in% names(all$items))
  expect_equal(list_users(con, q = q, role = "auditor")$total, 2)
  expect_equal(list_users(con, q = q, status = "disabled")$total, 0)

  p1 <- list_users(con, q = q, page = 1, page_size = 2)
  p2 <- list_users(con, q = q, page = 2, page_size = 2)
  expect_equal(nrow(p1$items), 2)
  expect_equal(nrow(p2$items), 1)
  expect_equal(p1$pages, 2)
  expect_length(intersect(p1$items$id, p2$items$id), 0)

  expect_equal(list_users(con, q = "%")$total, 0)   # joker traité comme un caractère

  expect_equal(app_status(list_users(con, page_size = 500)), 400)
  expect_equal(app_status(list_users(con, page = 0)), 400)
  expect_equal(app_status(list_users(con, status = "autre")), 400)
})

test_that("création : hiérarchie des rôles, doublons et validations", {
  skip_if_no_db()
  con <- local_db_txn()
  tag <- rand_tag()
  sa <- new_test_user(con, "super_admin")
  ad <- new_test_user(con, "admin")
  act_sa <- list(user_id = sa$id, role = "super_admin")
  act_ad <- list(user_id = ad$id, role = "admin")

  mk <- function(actor, s, role = "auditor", password = "Mot2Passe-Solide-42") {
    n <- paste0("cr_", tag, "_", s)
    create_user_as(con, actor, n, paste0(n, "@example.test"), "Créé Test", password, role)
  }

  u <- mk(act_ad, "a")
  expect_equal(u$role, "auditor")
  expect_equal(u$status, "active")
  expect_false("password_hash" %in% names(u))

  expect_equal(app_status(mk(act_ad, "b", "admin")), 403)         # pair
  expect_equal(app_status(mk(act_ad, "c", "super_admin")), 403)   # supérieur
  expect_true(is.na(app_status(mk(act_sa, "d", "admin"))))        # autorisé
  expect_equal(app_status(mk(act_sa, "e", "role_inconnu")), 400)
  expect_equal(app_status(mk(act_ad, "a")), 409)                  # doublon
  expect_equal(app_status(mk(act_ad, "f", password = "court1")), 400)
  expect_equal(app_status(create_user_as(con, act_ad, NULL, "x@example.test", "X",
                                         "Mot2Passe-Solide-42", "auditor")), 400)
  expect_equal(app_status(create_user_as(con, act_ad, "cr_mail", "pas-un-mail", "X",
                                         "Mot2Passe-Solide-42", "auditor")), 400)

  a <- DBI::dbGetQuery(con, "
    SELECT result, metadata::text AS meta FROM audit_log
    WHERE action = 'user.create' AND object_id = $1", params = list(u$id))
  expect_equal(nrow(a), 1)
  expect_equal(a$result, "success")
  expect_false(grepl("Mot2Passe", a$meta, fixed = TRUE))
})

test_that("modification : hiérarchie, rôle propre interdit, doublons, effet immédiat", {
  skip_if_no_db()
  con <- local_db_txn()
  tag <- rand_tag()
  sa    <- new_test_user(con, "super_admin")
  ad    <- new_test_user(con, "admin")
  ad2   <- new_test_user(con, "admin")
  sales <- new_test_user(con, "sales_agent")
  act_sa <- list(user_id = sa$id, role = "super_admin")
  act_ad <- list(user_id = ad$id, role = "admin")

  new_mail <- paste0("maj_", tag, "@example.test")
  u <- update_user(con, act_ad, sales$id, list(full_name = "  Nouveau Nom ", email = new_mail))
  expect_equal(u$full_name, "Nouveau Nom")
  expect_equal(u$email, new_mail)

  expect_equal(app_status(update_user(con, act_ad, ad2$id, list(full_name = "X"))), 403)
  expect_equal(app_status(update_user(con, act_ad, sa$id, list(full_name = "X"))), 403)
  expect_equal(app_status(update_user(con, act_ad, sales$id, list(role = "admin"))), 403)
  expect_equal(app_status(update_user(con, act_sa, sa$id, list(role = "admin"))), 403)  # son propre rôle

  # Un super administrateur rétrograde un admin : les permissions changent aussitôt
  expect_true("users.manage" %in% user_permissions(con, ad2$id))
  expect_equal(update_user(con, act_sa, ad2$id, list(role = "auditor"))$role, "auditor")
  expect_false("users.manage" %in% user_permissions(con, ad2$id))

  expect_equal(app_status(update_user(con, act_ad, sales$id, list(role = "inconnu"))), 400)
  expect_equal(app_status(update_user(con, act_ad, sales$id, list(mot = "x"))), 400)
  expect_equal(app_status(update_user(con, act_ad, sales$id, list())), 400)
  expect_equal(app_status(update_user(con, act_ad, sales$id, list(email = "pas-un-mail"))), 400)
  expect_equal(app_status(update_user(con, act_ad, sales$id,
                                      list(email = paste0(ad$username, "@example.test")))), 409)
  expect_equal(app_status(update_user(con, act_ad, "00000000-0000-0000-0000-000000000000",
                                      list(full_name = "X"))), 404)
  expect_equal(app_status(update_user(con, act_ad, "pas-un-uuid", list(full_name = "X"))), 404)
})

test_that("désactivation : sessions révoquées, réactivation, protections, audit", {
  skip_if_no_db()
  con <- local_db_txn()
  ad    <- new_test_user(con, "admin")
  ad2   <- new_test_user(con, "admin")
  sales <- new_test_user(con, "sales_agent")
  act_ad <- list(user_id = ad$id, role = "admin")

  res <- login(con, sales$username, sales$password)
  expect_true(res$ok)
  expect_false(is.null(authenticate_token(con, res$token)))

  expect_equal(set_user_status(con, act_ad, sales$id, "disabled")$status, "disabled")
  expect_null(authenticate_token(con, res$token))
  expect_equal(login(con, sales$username, sales$password)$status, "invalid")

  expect_equal(set_user_status(con, act_ad, sales$id, "active")$status, "active")
  expect_true(login(con, sales$username, sales$password)$ok)
  expect_equal(set_user_status(con, act_ad, sales$id, "active")$status, "active")  # idempotent

  expect_equal(app_status(set_user_status(con, act_ad, ad$id, "disabled")), 403)   # soi-même
  expect_equal(app_status(set_user_status(con, act_ad, ad2$id, "disabled")), 403)  # pair
  expect_equal(app_status(set_user_status(con, act_ad, sales$id, "autre")), 400)

  n <- DBI::dbGetQuery(con, "
    SELECT count(*)::int AS n FROM audit_log
    WHERE action = 'user.disable' AND object_id = $1", params = list(sales$id))$n
  expect_equal(n, 1)
})

test_that("le dernier super administrateur actif est protégé (409)", {
  skip_if_no_db()
  con <- local_db_txn()
  sa <- new_test_user(con, "super_admin")
  ad <- new_test_user(con, "admin")
  skip_if(active_super_admin_count(con) != 1,
          "d'autres super administrateurs actifs existent dans la base de test")

  fake <- list(user_id = ad$id, role = "super_admin")   # acteur fictif, distinct de la cible
  expect_equal(app_status(set_user_status(con, fake, sa$id, "disabled")), 409)
  expect_equal(app_status(update_user(con, fake, sa$id, list(role = "admin"))), 409)

  sa2 <- new_test_user(con, "super_admin")
  expect_equal(set_user_status(con, fake, sa$id, "disabled")$status, "disabled")
  expect_equal(app_status(set_user_status(con, fake, sa2$id, "disabled")), 409)
})

test_that("changement de mot de passe : contrôles et révocation des autres sessions", {
  skip_if_no_db()
  con <- local_db_txn()
  u <- new_test_user(con, "auditor")
  s1 <- login(con, u$username, u$password)
  s2 <- login(con, u$username, u$password)
  auth1 <- authenticate_token(con, s1$token)
  new_pw <- "Nouveau-Mot2Passe-77"

  expect_equal(app_status(change_password(con, auth1, "Mauvais-Mot2Passe-9", new_pw)), 403)
  expect_equal(app_status(change_password(con, auth1, u$password, "court1")), 400)
  expect_equal(app_status(change_password(con, auth1, u$password, u$password)), 400)

  expect_true(change_password(con, auth1, u$password, new_pw))
  expect_false(is.null(authenticate_token(con, s1$token)))   # session courante conservée
  expect_null(authenticate_token(con, s2$token))             # autre session révoquée
  expect_equal(login(con, u$username, u$password)$status, "invalid")
  expect_true(login(con, u$username, new_pw)$ok)
})
