# ==============================================================================
# Fichier : R/preprocessing.R
# Rôle    : Nettoyage des données, séparation numérique / catégoriel,
#           séparation X / y, et construction du pipeline de prétraitement
#           (partie numérique + partie catégorielle, SMOTE en option).
#
# Input   : data/processed/train.csv et valid.csv (le jeu test n'est pas
#           chargé ici)
# Output  : X_train, y_train, X_val, y_val, noms des colonnes numériques
#           et catégorielles, recipe (pipeline de prétraitement)
# ==============================================================================


# convert_total_charges()
#   Convertit TotalCharges en numérique (les valeurs vides ou invalides
#   deviennent NA). Utile aussi à l'inférence, où l'API reçoit du texte.
#   Input  : df avec une colonne TotalCharges (texte ou numérique)
#   Output : df avec TotalCharges numérique
convert_total_charges <- function(df) {
  dplyr::mutate(
    df,
    TotalCharges = suppressWarnings(as.numeric(trimws(as.character(TotalCharges))))
  )
}


# clean_data()
#   - TotalCharges en numérique, 0 si tenure = 0 (client pas encore facturé)
#   - SeniorCitizen (0/1) en catégorie "No"/"Yes"
#   - Churn (si présent) en facteur, "Yes" = classe positive (1er niveau)
#   Input  : df brut
#   Output : df nettoyé
clean_data <- function(df) {
  df |>
    convert_total_charges() |>
    dplyr::mutate(
      TotalCharges  = dplyr::if_else(is.na(TotalCharges) & tenure == 0,
                                     0, TotalCharges),
      SeniorCitizen = dplyr::if_else(
        as.character(SeniorCitizen) %in% c("1", "Yes"), "Yes", "No"
      ),
      dplyr::across(dplyr::any_of("Churn"),
                    ~ factor(.x, levels = c("Yes", "No")))
    )
}


# split_columns()
#   Sépare les variables explicatives en numériques et catégorielles.
#   Input  : df, target = nom de la cible
#   Output : list(numeric = noms, categorical = noms)
split_columns <- function(df, target = "Churn") {
  predictors <- setdiff(names(df), target)
  is_num     <- vapply(df[predictors], is.numeric, logical(1))
  list(numeric = predictors[is_num], categorical = predictors[!is_num])
}


# split_xy()
#   Sépare les variables explicatives (X) de la cible (y).
#   Input  : df, target = nom de la cible
#   Output : list(X = data.frame, y = vecteur)
split_xy <- function(df, target = "Churn") {
  list(X = df[setdiff(names(df), target)], y = df[[target]])
}


# load_split()
#   Charge un jeu enregistré et applique clean_data().
#   Input  : name = "train" | "valid" | "test", dir = dossier des CSV
#   Output : df nettoyé
load_split <- function(name, dir = "data/processed") {
  path <- file.path(dir, paste0(name, ".csv"))
  stopifnot("Fichier introuvable : lance d'abord run_ingestion()" =
              file.exists(path))
  clean_data(readr::read_csv(path, show_col_types = FALSE))
}


# load_train_valid()
#   Charge train et valid, puis sépare X / y et les types de colonnes.
#   Input  : dir = dossier des CSV, target = nom de la cible
#   Output : list(X_train, y_train, X_val, y_val, num_cols, cat_cols)
load_train_valid <- function(dir = "data/processed", target = "Churn") {
  train <- load_split("train", dir)
  valid <- load_split("valid", dir)

  cols <- split_columns(train, target)
  tr   <- split_xy(train, target)
  va   <- split_xy(valid, target)

  list(
    X_train = tr$X, y_train = tr$y,
    X_val   = va$X, y_val   = va$y,
    num_cols = cols$numeric,
    cat_cols = cols$categorical
  )
}


# make_recipe()
#   Pipeline de prétraitement en deux parties, appris sur train uniquement :
#     1) numérique    : imputation par la médiane, normalisation
#     2) catégoriel   : conversion en facteur, imputation par le mode,
#                       modalités inconnues, encodage one-hot
#   Option SMOTE : suréchantillonne la classe minoritaire (Churn = Yes) sur le
#   jeu d'entraînement seulement ; l'étape est ignorée à la prédiction.
#   Input  : train_df = X_train + colonne Churn, num_cols, cat_cols,
#            smote = TRUE/FALSE, seed
#   Output : recipe (non entraînée)
make_recipe <- function(train_df, num_cols, cat_cols, smote = FALSE, seed = 123) {
  rec <- recipes::recipe(Churn ~ ., data = train_df) |>
    # --- Partie numérique ---
    recipes::step_impute_median(tidyselect::all_of(num_cols)) |>
    recipes::step_normalize(tidyselect::all_of(num_cols)) |>
    # --- Partie catégorielle ---
    recipes::step_string2factor(tidyselect::all_of(cat_cols)) |>
    recipes::step_impute_mode(tidyselect::all_of(cat_cols)) |>
    recipes::step_novel(tidyselect::all_of(cat_cols)) |>
    recipes::step_dummy(tidyselect::all_of(cat_cols)) |>
    recipes::step_zv(recipes::all_predictors())

  if (smote) {
    rec <- rec |> themis::step_smote(Churn, over_ratio = 1, seed = seed)
  }
  rec
}
