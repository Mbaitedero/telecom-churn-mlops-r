# ==============================================================================
# Fichier : _targets.R
# Rôle    : Pipeline reproductible (package targets) :
#           ingestion -> validation -> prétraitement -> entraînement ->
#           modèle final -> évaluation sur test
# Lancer  : targets::tar_make()
# ==============================================================================

library(targets)

# Charge toutes les fonctions de R/
tar_source("R")

tar_option_set(
  packages = c("dplyr", "ggplot2", "recipes", "parsnip", "workflows",
               "tune", "yardstick")
)

list(
  # --- 1. Ingestion -----------------------------------------------------------
  tar_target(raw_file, "data/raw/churn.csv", format = "file"),
  tar_target(raw_data, ingest_data(dirname(raw_file))),
  tar_target(splits,   raw_data |> drop_id() |> split_three(seed = 123)),
  tar_target(split_balance, check_split_balance(splits)),
  tar_target(split_files, save_splits(splits), format = "file"),

  # --- 2. Validation et exploration (jeu train uniquement) --------------------
  tar_target(data_check, {
    validate_data(splits$train)
    TRUE
  }),
  tar_target(eda_figures, {
    data_check
    plot_eda(splits$train)
    list.files("reports/figures",
               pattern = "^(churn_|tenure_|monthly_)", full.names = TRUE)
  }, format = "file"),

  # --- 3. Prétraitement -------------------------------------------------------
  tar_target(train_df, {
    data_check
    clean_data(splits$train)
  }),
  tar_target(valid_df, clean_data(splits$valid)),
  tar_target(test_df,  clean_data(splits$test)),
  tar_target(cols, split_columns(train_df)),
  tar_target(rec,  make_recipe(train_df, cols$numeric, cols$categorical)),

  # --- 4. Entraînement --------------------------------------------------------
  tar_target(model_comparison, compare_models(train_df, valid_df, rec)),
  tar_target(model_comparison_file, {
    dir.create("reports/metrics", showWarnings = FALSE, recursive = TRUE)
    path <- "reports/metrics/validation_metrics.csv"
    readr::write_csv(model_comparison$metrics, path)
    path
  }, format = "file"),

  tar_target(logistic_tuned,
             run_variant("glmnet sans SMOTE", train_df, valid_df, rec)),
  tar_target(final_model, list(model     = logistic_tuned$res$fit,
                               threshold = logistic_tuned$thr)),
  tar_target(final_model_file, save_final_model(final_model), format = "file"),

  # --- 5. Évaluation finale sur test ------------------------------------------
  tar_target(test_eval, evaluate_on_test(final_model, test_df)),
  tar_target(test_metrics, test_eval$metrics),
  tar_target(test_files, test_eval$files, format = "file")
)
