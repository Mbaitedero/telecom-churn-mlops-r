# ==============================================================================
# Fichier : R/data_validation.R
# Rôle    : Chargement du jeu d'entraînement, exploration, contrôles de
#           qualité et graphiques (EDA).
#
# Input   : data/processed/train.csv
# Output  : message de validation, graphiques dans reports/figures/
#           (+ liste list(data, plots) retournée par run_validation())
# ==============================================================================


# load_train()
#   Input  : path = chemin du jeu d'entraînement
#   Output : data.frame (tibble)
load_train <- function(path = "data/processed/train.csv") {
  stopifnot("train.csv introuvable : lance d'abord run_ingestion()" =
              file.exists(path))
  readr::read_csv(path, show_col_types = FALSE)
}


# explore_train()
#   Affiche structure, résumé statistique, valeurs manquantes, part de churn.
#   Input  : df = jeu d'entraînement
#   Output : df (invisible)
explore_train <- function(df) {
  dplyr::glimpse(df)

  cat("\n--- Résumé statistique ---\n")
  print(summary(df))

  na_counts <- colSums(is.na(df))
  cat("\n--- Valeurs manquantes ---\n")
  if (any(na_counts > 0)) print(na_counts[na_counts > 0]) else cat("aucune\n")

  cat("\n--- Répartition de Churn ---\n")
  print(df |> dplyr::count(Churn) |> dplyr::mutate(prop = round(n / sum(n), 3)))

  invisible(df)
}


# validate_data()
#   Contrôles de qualité : colonnes attendues, modalités, bornes, NA.
#   Stoppe avec une erreur explicite si un contrôle échoue.
#   Input  : df = jeu d'entraînement
#   Output : df (invisible)
validate_data <- function(df) {
  expected_cols <- c(
    "gender", "SeniorCitizen", "Partner", "Dependents", "tenure",
    "PhoneService", "MultipleLines", "InternetService", "OnlineSecurity",
    "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV",
    "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod",
    "MonthlyCharges", "TotalCharges", "Churn"
  )

  missing_cols <- setdiff(expected_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Colonnes manquantes : ", paste(missing_cols, collapse = ", "))
  }

  checks <- c(
    "Churn sans NA"             = !anyNA(df$Churn),
    "Churn dans {Yes, No}"      = all(df$Churn %in% c("Yes", "No")),
    "SeniorCitizen dans {0, 1}" = all(df$SeniorCitizen %in% c(0, 1)),
    "tenure >= 0"               = all(df$tenure >= 0, na.rm = TRUE),
    "MonthlyCharges > 0"        = all(df$MonthlyCharges > 0, na.rm = TRUE),
    "TotalCharges numérique"    = is.numeric(df$TotalCharges),
    "TotalCharges >= 0"         = all(df$TotalCharges >= 0, na.rm = TRUE),
    "NA TotalCharges <= 1 %"    = mean(is.na(df$TotalCharges)) <= 0.01,
    "Contract valide"           = all(df$Contract %in%
                                        c("Month-to-month", "One year", "Two year"))
  )

  failed <- names(checks)[!checks]
  if (length(failed) > 0) {
    stop("Validation échouée : ", paste(failed, collapse = " | "))
  }

  message("Validation OK (", length(checks), " contrôles)")
  invisible(df)
}


# plot_eda()
#   Crée et enregistre 4 graphiques dans out_dir.
#   Input  : df = jeu d'entraînement, out_dir = dossier de sortie
#   Output : liste de graphiques ggplot (invisible)
plot_eda <- function(df, out_dir = "reports/figures") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  save_plot <- function(p, name) {
    ggplot2::ggsave(file.path(out_dir, name), p,
                    width = 7, height = 4.5, dpi = 150)
  }

  p_churn <- ggplot2::ggplot(df, ggplot2::aes(x = Churn, fill = Churn)) +
    ggplot2::geom_bar(show.legend = FALSE) +
    ggplot2::labs(title = "Distribution de la cible Churn",
                  x = NULL, y = "Clients")

  p_tenure <- ggplot2::ggplot(df, ggplot2::aes(x = tenure, fill = Churn)) +
    ggplot2::geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
    ggplot2::labs(title = "Ancienneté (tenure) selon le churn",
                  x = "Mois", y = "Clients")

  p_charges <- ggplot2::ggplot(df, ggplot2::aes(x = Churn, y = MonthlyCharges,
                                                fill = Churn)) +
    ggplot2::geom_boxplot(show.legend = FALSE) +
    ggplot2::labs(title = "Facture mensuelle selon le churn",
                  x = NULL, y = "MonthlyCharges")

  p_contract <- df |>
    dplyr::group_by(Contract) |>
    dplyr::summarise(taux = mean(Churn == "Yes"), .groups = "drop") |>
    ggplot2::ggplot(ggplot2::aes(x = Contract, y = taux)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(title = "Taux de churn par type de contrat",
                  x = NULL, y = "Taux de churn")

  save_plot(p_churn,    "churn_distribution.png")
  save_plot(p_tenure,   "tenure_by_churn.png")
  save_plot(p_charges,  "monthly_charges_by_churn.png")
  save_plot(p_contract, "churn_by_contract.png")

  invisible(list(churn = p_churn, tenure = p_tenure,
                 charges = p_charges, contract = p_contract))
}


# run_validation()
#   Enchaîne chargement, exploration, validation et graphiques.
#   Input  : path = jeu d'entraînement, out_dir = dossier des figures
#   Output : list(data, plots) (invisible)
run_validation <- function(path = "data/processed/train.csv",
                           out_dir = "reports/figures") {
  train <- load_train(path)
  explore_train(train)
  validate_data(train)
  plots <- plot_eda(train, out_dir)

  invisible(list(data = train, plots = plots))
}
