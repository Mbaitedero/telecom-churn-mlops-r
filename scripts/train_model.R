# ==============================================================================
# Script  : scripts/train_model.R
# Rôle    : 1) compare 3 modèles (logistic, random forest, xgboost) sur valid
#           2) règle la régression logistique (glmnet), avec et sans SMOTE,
#              et choisit le seuil de décision
# Lancer  : source("scripts/train_model.R")   (projet ouvert, racine du projet)
# ==============================================================================

source("R/preprocessing.R")
source("R/train.R")

# --- Données : X / y, colonnes numériques et catégorielles --------------------
d <- load_train_valid()

cat("Colonnes numériques   :", length(d$num_cols), "->",
    paste(d$num_cols, collapse = ", "), "\n")
cat("Colonnes catégorielles:", length(d$cat_cols), "\n")

train_df <- dplyr::bind_cols(d$X_train, Churn = d$y_train)
valid_df <- dplyr::bind_cols(d$X_val,   Churn = d$y_val)

# --- 1) Comparaison des 3 modèles ---------------------------------------------
rec    <- make_recipe(train_df, d$num_cols, d$cat_cols)
result <- compare_models(train_df, valid_df, rec)
print(result$metrics)

save_best_model(result)

# --- 2) Régression logistique : sans / avec SMOTE, paramètres réglés ----------
rec_smote <- make_recipe(train_df, d$num_cols, d$cat_cols, smote = TRUE)

base <- eval_at_threshold(result$fits$logistic, valid_df, 0.5) |>
  dplyr::mutate(version = "glm de base - seuil 0.5")

a <- run_variant("glmnet sans SMOTE", train_df, valid_df, rec)
b <- run_variant("glmnet avec SMOTE", train_df, valid_df, rec_smote)

message("Paramètres retenus sans SMOTE : penalty = ", signif(a$res$best$penalty, 3),
        ", mixture = ", a$res$best$mixture, ", seuil = ", a$thr)
message("Paramètres retenus avec SMOTE : penalty = ", signif(b$res$best$penalty, 3),
        ", mixture = ", b$res$best$mixture, ", seuil = ", b$thr)

comparaison <- dplyr::bind_rows(base, a$table, b$table) |>
  dplyr::select(version, dplyr::everything())
print(comparaison, width = Inf)

# --- Sauvegardes --------------------------------------------------------------
dir.create("models", showWarnings = FALSE)
dir.create("reports/metrics", showWarnings = FALSE, recursive = TRUE)

saveRDS(list(sans_smote = list(model = a$res$fit, threshold = a$thr),
             avec_smote = list(model = b$res$fit, threshold = b$thr)),
        "models/logistic_variants.rds")
readr::write_csv(comparaison, "reports/metrics/validation_metrics_logistic.csv")
