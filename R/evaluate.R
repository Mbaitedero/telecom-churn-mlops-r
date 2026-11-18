# ==============================================================================
# Fichier : R/evaluate.R
# Rôle    : Évaluation finale du modèle retenu sur le jeu test (jamais utilisé
#           avant) : métriques, matrice de confusion, courbe ROC, coefficients.
#
# Input   : modèle final + seuil, jeu test nettoyé
#           (utilise eval_at_threshold() défini dans R/train.R)
# Output  : reports/metrics/test_metrics.csv
#           reports/figures/{confusion_matrix,roc_curve,coefficients}.png
# ==============================================================================


# load_final_model()
#   Input  : path = fichier des modèles, variant = "sans_smote" | "avec_smote"
#   Output : list(model = workflow entraîné, threshold = seuil de décision)
load_final_model <- function(path = "models/logistic_variants.rds",
                             variant = "sans_smote") {
  stopifnot("Modèle introuvable : lance d'abord scripts/train_model.R" =
              file.exists(path))
  readRDS(path)[[variant]]
}


# predict_with_threshold()
#   Input  : fit = workflow entraîné, data = jeu avec Churn, threshold
#   Output : tibble(Churn, .pred_Yes, .pred_class)
predict_with_threshold <- function(fit, data, threshold) {
  prob <- predict(fit, data, type = "prob")$.pred_Yes
  tibble::tibble(
    Churn       = data$Churn,
    .pred_Yes   = prob,
    .pred_class = factor(ifelse(prob >= threshold, "Yes", "No"),
                         levels = c("Yes", "No"))
  )
}


# plot_confusion()
#   Input  : preds = sortie de predict_with_threshold
#   Output : graphique ggplot (matrice de confusion)
plot_confusion <- function(preds) {
  cm <- yardstick::conf_mat(preds, truth = Churn, estimate = .pred_class)
  ggplot2::autoplot(cm, type = "heatmap") +
    ggplot2::labs(title = "Matrice de confusion (jeu test)")
}


# plot_roc()
#   Input  : preds
#   Output : graphique ggplot (courbe ROC)
plot_roc <- function(preds) {
  yardstick::roc_curve(preds, truth = Churn, .pred_Yes) |>
    ggplot2::autoplot() +
    ggplot2::labs(title = "Courbe ROC (jeu test)")
}


# plot_coefficients()
#   Affiche les coefficients non nuls les plus importants (en valeur absolue).
#   Positif = augmente le risque de churn ; négatif = le diminue.
#   Input  : fit = workflow glmnet entraîné, n = nombre de variables
#   Output : graphique ggplot
plot_coefficients <- function(fit, n = 15) {
  coefs <- workflows::extract_fit_parsnip(fit) |>
    broom::tidy() |>
    dplyr::filter(term != "(Intercept)", estimate != 0) |>
    dplyr::slice_max(abs(estimate), n = n)

  ggplot2::ggplot(coefs, ggplot2::aes(x = estimate, y = stats::reorder(term, estimate),
                                      fill = estimate > 0)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::labs(title = "Variables les plus influentes",
                  x = "Coefficient (échelle logit)", y = NULL)
}


# evaluate_on_test()
#   Évalue le modèle final sur le jeu test, écrit métriques et graphiques.
#   Input  : final = list(model, threshold), test = jeu test nettoyé,
#            fig_dir, metrics_dir
#   Output : list(metrics = tibble, files = chemins des fichiers écrits)
evaluate_on_test <- function(final, test,
                             fig_dir = "reports/figures",
                             metrics_dir = "reports/metrics") {
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(metrics_dir, showWarnings = FALSE, recursive = TRUE)

  metrics      <- eval_at_threshold(final$model, test, final$threshold)
  metrics_path <- file.path(metrics_dir, "test_metrics.csv")
  readr::write_csv(metrics, metrics_path)

  preds <- predict_with_threshold(final$model, test, final$threshold)
  figs  <- file.path(fig_dir, c("confusion_matrix.png", "roc_curve.png",
                                "coefficients.png"))
  save_plot <- function(p, path) {
    ggplot2::ggsave(path, p, width = 7, height = 4.5, dpi = 150)
  }
  save_plot(plot_confusion(preds),          figs[1])
  save_plot(plot_roc(preds),                figs[2])
  save_plot(plot_coefficients(final$model), figs[3])

  list(metrics = metrics, files = c(metrics_path, figs))
}


# run_evaluation()
#   Version manuelle (scripts/evaluate_model.R) : charge le modèle sauvegardé
#   et le jeu test, puis appelle evaluate_on_test().
#   Input  : variant, model_path, fig_dir, metrics_dir
#   Output : tableau des métriques (invisible)
run_evaluation <- function(variant = "sans_smote",
                           model_path = "models/logistic_variants.rds",
                           fig_dir = "reports/figures",
                           metrics_dir = "reports/metrics") {
  final <- load_final_model(model_path, variant)
  test  <- load_split("test")

  out <- evaluate_on_test(final, test, fig_dir, metrics_dir)
  print(out$metrics, width = Inf)

  invisible(out$metrics)
}
