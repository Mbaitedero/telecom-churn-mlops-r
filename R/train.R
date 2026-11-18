# ==============================================================================
# Fichier : R/train.R
# Rôle    : Entraînement et comparaison de 3 modèles (régression logistique,
#           Random Forest, XGBoost), puis amélioration de la régression
#           logistique (régularisation glmnet, SMOTE, seuil de décision).
#           Entraînement sur train, évaluation sur valid.
#
# Input   : train et valid nettoyés (load_train_valid), recipe (make_recipe)
# Output  : models/model.rds (meilleur modèle de la comparaison),
#           reports/metrics/validation_metrics.csv
# ==============================================================================


# define_models()
#   Input  : seed = graine aléatoire
#   Output : liste nommée de spécifications parsnip
define_models <- function(seed = 123) {
  list(
    logistic = parsnip::logistic_reg() |>
      parsnip::set_engine("glm") |>
      parsnip::set_mode("classification"),

    random_forest = parsnip::rand_forest(trees = 500) |>
      parsnip::set_engine("ranger", seed = seed, importance = "impurity") |>
      parsnip::set_mode("classification"),

    xgboost = parsnip::boost_tree(trees = 300, tree_depth = 4,
                                  learn_rate = 0.05) |>
      parsnip::set_engine("xgboost") |>
      parsnip::set_mode("classification")
  )
}


# build_workflow()
#   Input  : spec = modèle parsnip, rec = recipe
#   Output : workflow (recette + modèle)
build_workflow <- function(spec, rec) {
  workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(spec)
}


# score_model()
#   Calcule les métriques d'un workflow entraîné sur un jeu de données.
#   Input  : fit = workflow entraîné, data = jeu à évaluer
#   Output : tibble des métriques (AUC, accuracy, recall, precision, F1)
score_model <- function(fit, data) {
  preds <- dplyr::bind_cols(
    data["Churn"],
    predict(fit, data, type = "prob"),
    predict(fit, data, type = "class")
  )

  yardstick::metric_set(
    yardstick::roc_auc, yardstick::accuracy, yardstick::recall,
    yardstick::precision, yardstick::f_meas
  )(preds, truth = Churn, .pred_Yes, estimate = .pred_class)
}


# compare_models()
#   Entraîne chaque modèle sur train et l'évalue sur valid.
#   Input  : train, valid, rec, seed
#   Output : liste(metrics = tableau large trié par AUC, fits = modèles)
compare_models <- function(train, valid, rec, seed = 123) {
  specs <- define_models(seed)
  fits  <- list()
  rows  <- list()

  for (nm in names(specs)) {
    message("Entraînement : ", nm)
    set.seed(seed)
    fits[[nm]] <- generics::fit(build_workflow(specs[[nm]], rec), data = train)

    rows[[nm]] <- score_model(fits[[nm]], valid) |>
      dplyr::select(.metric, .estimate) |>
      dplyr::mutate(model = nm)
  }

  metrics <- dplyr::bind_rows(rows) |>
    tidyr::pivot_wider(names_from = .metric, values_from = .estimate) |>
    dplyr::arrange(dplyr::desc(roc_auc))

  list(metrics = metrics, fits = fits)
}


# save_best_model()
#   Sauvegarde le meilleur modèle (AUC) et le tableau des métriques.
#   Input  : result = sortie de compare_models, model_dir, metrics_dir
#   Output : nom du meilleur modèle (invisible)
save_best_model <- function(result,
                            model_dir = "models",
                            metrics_dir = "reports/metrics") {
  dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(metrics_dir, showWarnings = FALSE, recursive = TRUE)

  best <- result$metrics$model[1]
  saveRDS(result$fits[[best]], file.path(model_dir, "model.rds"))
  readr::write_csv(result$metrics,
                   file.path(metrics_dir, "validation_metrics.csv"))

  message("Meilleur modèle (AUC sur valid) : ", best)
  invisible(best)
}


# ------------------------------------------------------------------------------
# Amélioration de la régression logistique
# ------------------------------------------------------------------------------

# tune_logistic()
#   Régression logistique régularisée (glmnet). Cherche penalty (force de la
#   régularisation) et mixture (0 = ridge, 1 = lasso) par validation croisée
#   stratifiée sur train (critère : AUC).
#   Input  : train_df, rec, v = plis, seed, penalty_range = exposants (log10),
#            levels = nombre de valeurs testées par paramètre
#   Output : list(tuned, best, fit) ; fit = workflow final entraîné sur train
tune_logistic <- function(train_df, rec, v = 5, seed = 123,
                          penalty_range = c(-4, -0.5),
                          levels = c(penalty = 15, mixture = 5)) {
  spec <- parsnip::logistic_reg(penalty = tune::tune(), mixture = tune::tune()) |>
    parsnip::set_engine("glmnet") |>
    parsnip::set_mode("classification")

  wf   <- build_workflow(spec, rec)
  grid <- dials::grid_regular(dials::penalty(range = penalty_range),
                              dials::mixture(range = c(0, 1)),
                              levels = levels)
  set.seed(seed)
  folds <- rsample::vfold_cv(train_df, v = v, strata = Churn)

  tuned <- tune::tune_grid(
    wf, resamples = folds, grid = grid,
    metrics = yardstick::metric_set(yardstick::roc_auc),
    control = tune::control_grid(save_pred = TRUE)
  )

  best <- tune::select_best(tuned, metric = "roc_auc")
  fit  <- generics::fit(tune::finalize_workflow(wf, best), data = train_df)

  list(tuned = tuned, best = best, fit = fit)
}


# find_threshold()
#   Cherche le seuil qui maximise le F-score (beta = 1 : F1 ; beta = 2 donne
#   plus de poids au recall).
#   Input  : truth = facteur Churn, prob_yes = probabilité de churn,
#            beta, grid = seuils testés
#   Output : seuil optimal (numérique)
find_threshold <- function(truth, prob_yes, beta = 1,
                           grid = seq(0.10, 0.70, by = 0.01)) {
  scores <- vapply(grid, function(t) {
    cls <- factor(ifelse(prob_yes >= t, "Yes", "No"), levels = c("Yes", "No"))
    yardstick::f_meas_vec(truth, cls, beta = beta)
  }, numeric(1))
  grid[which.max(scores)]
}


# eval_at_threshold()
#   Métriques d'un modèle sur un jeu de données pour un seuil donné.
#   Input  : fit, data (avec Churn), threshold
#   Output : tibble d'une ligne
eval_at_threshold <- function(fit, data, threshold = 0.5) {
  prob  <- predict(fit, data, type = "prob")$.pred_Yes
  truth <- data$Churn
  cls   <- factor(ifelse(prob >= threshold, "Yes", "No"), levels = c("Yes", "No"))

  tibble::tibble(
    threshold = threshold,
    accuracy  = yardstick::accuracy_vec(truth, cls),
    recall    = yardstick::recall_vec(truth, cls),
    precision = yardstick::precision_vec(truth, cls),
    f_meas    = yardstick::f_meas_vec(truth, cls),
    roc_auc   = yardstick::roc_auc_vec(truth, prob)
  )
}


# run_variant()
#   Règle la régression logistique pour une recette donnée, choisit le seuil
#   sur les prédictions hors-pli de train, puis évalue sur valid (seuil 0.5 et
#   seuil optimal).
#   Input  : name = libellé, train_df, valid_df, rec, seed
#   Output : list(res, thr, table)
run_variant <- function(name, train_df, valid_df, rec, seed = 123) {
  res <- tune_logistic(train_df, rec, seed = seed)
  oof <- tune::collect_predictions(res$tuned, parameters = res$best)
  thr <- find_threshold(oof$Churn, oof$.pred_Yes)

  tab <- dplyr::bind_rows(
    dplyr::mutate(eval_at_threshold(res$fit, valid_df, 0.5),
                  version = paste(name, "- seuil 0.5")),
    dplyr::mutate(eval_at_threshold(res$fit, valid_df, thr),
                  version = paste(name, "- seuil optimal"))
  )
  list(res = res, thr = thr, table = tab)
}
