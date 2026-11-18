# ==============================================================================
# Fichier : R/monitoring.R
# Rôle    : Surveillance du modèle en production : dérive des variables et des
#           scores (PSI, test KS) et dégradation de la performance.
#           Seuils PSI : < 0.10 stable ; 0.10-0.25 warning ; >= 0.25 alert.
#
# Input   : jeu de référence (train nettoyé), jeu courant (nouvelles données
#           nettoyées), modèle final + seuil, métriques de référence (facultatif)
# Output  : reports/metrics/drift_report_<tag>.csv
#           reports/figures/drift_psi_<tag>.png
#           reports/metrics/monitoring_log.csv (une ligne ajoutée par exécution)
# Dépend  : split_columns() (R/preprocessing.R), eval_at_threshold() (R/train.R)
# ==============================================================================


# psi_numeric()
#   PSI d'une variable numérique : on découpe en déciles de la référence puis
#   on compare les proportions de la référence et des données courantes.
#   Input  : expected = référence, actual = courant, bins, eps (évite log(0))
#   Output : PSI (numérique)
psi_numeric <- function(expected, actual, bins = 10, eps = 1e-4) {
  expected <- expected[!is.na(expected)]
  actual   <- actual[!is.na(actual)]

  breaks <- unique(stats::quantile(expected,
                                   probs = seq(0, 1, length.out = bins + 1),
                                   names = FALSE))
  if (length(breaks) < 3) return(0)          # variable (quasi) constante
  breaks[1] <- -Inf
  breaks[length(breaks)] <- Inf

  e <- as.numeric(table(cut(expected, breaks, include.lowest = TRUE))) / length(expected)
  a <- as.numeric(table(cut(actual,   breaks, include.lowest = TRUE))) / length(actual)
  e <- pmax(e, eps)
  a <- pmax(a, eps)

  sum((a - e) * log(a / e))
}


# psi_categorical()
#   PSI d'une variable catégorielle (proportions par modalité).
#   Input  : expected, actual, eps
#   Output : PSI (numérique)
psi_categorical <- function(expected, actual, eps = 1e-4) {
  expected <- as.character(expected[!is.na(expected)])
  actual   <- as.character(actual[!is.na(actual)])

  lv <- union(unique(expected), unique(actual))
  e  <- as.numeric(table(factor(expected, levels = lv))) / length(expected)
  a  <- as.numeric(table(factor(actual,   levels = lv))) / length(actual)
  e  <- pmax(e, eps)
  a  <- pmax(a, eps)

  sum((a - e) * log(a / e))
}


# psi_status()
#   Input  : psi (vecteur), warn, alert = seuils
#   Output : "stable" | "warning" | "alert"
psi_status <- function(psi, warn = 0.10, alert = 0.25) {
  ifelse(psi >= alert, "alert", ifelse(psi >= warn, "warning", "stable"))
}


# detect_drift()
#   Calcule PSI (et test KS pour les variables numériques) de chaque variable.
#   Input  : reference, current = data.frames nettoyés,
#            num_cols, cat_cols = noms des colonnes
#   Output : tibble(variable, type, psi, ks_stat, ks_p_value, status),
#            trié par PSI décroissant
detect_drift <- function(reference, current, num_cols, cat_cols) {
  num <- lapply(num_cols, function(v) {
    # exact = FALSE : p-value asymptotique, beaucoup plus rapide avec des ex æquo
    ks <- suppressWarnings(stats::ks.test(reference[[v]], current[[v]],
                                          exact = FALSE))
    tibble::tibble(
      variable   = v, type = "numeric",
      psi        = psi_numeric(reference[[v]], current[[v]]),
      ks_stat    = unname(ks$statistic),
      ks_p_value = ks$p.value
    )
  })

  cat <- lapply(cat_cols, function(v) {
    tibble::tibble(
      variable   = v, type = "categorical",
      psi        = psi_categorical(reference[[v]], current[[v]]),
      ks_stat    = NA_real_, ks_p_value = NA_real_
    )
  })

  dplyr::bind_rows(num, cat) |>
    dplyr::mutate(status = psi_status(psi)) |>
    dplyr::arrange(dplyr::desc(psi))
}

# score_drift()
#   PSI des probabilités de churn prédites (dérive du score du modèle).
#   Input  : final = list(model, threshold), reference, current
#   Output : tibble d'une ligne (variable = "score_churn")
score_drift <- function(final, reference, current) {
  ref_prob <- predict(final$model, reference, type = "prob")$.pred_Yes
  cur_prob <- predict(final$model, current,   type = "prob")$.pred_Yes
  psi      <- psi_numeric(ref_prob, cur_prob)

  tibble::tibble(variable = "score_churn", type = "score",
                 psi = psi, status = psi_status(psi))
}


# monitor_performance()
#   Compare la performance courante à la référence (nécessite la colonne
#   Churn : en pratique, les vrais résultats arrivent avec du retard).
#   Alerte si le recall, la precision ou l'AUC baisse au-delà d'un seuil.
#   Input  : final, current (avec Churn), reference_metrics (recall, precision,
#            roc_auc), max_recall_drop, max_precision_drop, max_auc_drop
#   Output : tibble d'une ligne (métriques, baisses, alert = TRUE/FALSE)
monitor_performance <- function(final, current, reference_metrics,
                                max_recall_drop = 0.05,
                                max_precision_drop = 0.10,
                                max_auc_drop = 0.03) {
  now <- eval_at_threshold(final$model, current, final$threshold)

  recall_drop    <- reference_metrics$recall    - now$recall
  precision_drop <- reference_metrics$precision - now$precision
  auc_drop       <- reference_metrics$roc_auc   - now$roc_auc

  dplyr::mutate(
    now,
    recall_drop    = recall_drop,
    precision_drop = precision_drop,
    auc_drop       = auc_drop,
    alert = recall_drop > max_recall_drop |
      precision_drop > max_precision_drop |
      auc_drop > max_auc_drop
  )
}
# plot_drift()
#   Diagramme du PSI par variable avec les seuils 0.10 et 0.25.
#   Input  : drift = sortie de detect_drift (éventuellement + score_drift)
#   Output : graphique ggplot
plot_drift <- function(drift) {
  ggplot2::ggplot(drift, ggplot2::aes(x = psi, y = stats::reorder(variable, psi),
                                      fill = status)) +
    ggplot2::geom_col() +
    ggplot2::geom_vline(xintercept = c(0.10, 0.25), linetype = "dashed") +
    ggplot2::scale_fill_manual(values = c(stable = "seagreen",
                                          warning = "orange",
                                          alert = "firebrick")) +
    ggplot2::labs(title = "Dérive des variables (PSI)", x = "PSI", y = NULL,
                  fill = NULL)
}


# run_monitoring()
#   Enchaîne dérive des variables, dérive du score, performance (si les
#   Churn sont présents et reference_metrics fourni), écrit le rapport, le
#   graphique et ajoute une ligne au journal de monitoring.
#   Input  : reference, current, final, reference_metrics (facultatif),
#            tag = nom du scénario, out_dir, fig_dir
#   Output : list(drift, performance, summary) (invisible)
run_monitoring <- function(reference, current, final, reference_metrics = NULL,
                           tag = "courant",
                           out_dir = "reports/metrics",
                           fig_dir = "reports/figures") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

  cols  <- split_columns(reference)
  drift <- dplyr::bind_rows(
    detect_drift(reference, current, cols$numeric, cols$categorical),
    score_drift(final, reference, current)
  ) |>
    dplyr::arrange(dplyr::desc(psi))

  readr::write_csv(drift, file.path(out_dir, paste0("drift_report_", tag, ".csv")))
  ggplot2::ggsave(file.path(fig_dir, paste0("drift_psi_", tag, ".png")),
                  plot_drift(drift), width = 7, height = 6, dpi = 150)

  perf <- NULL
  if ("Churn" %in% names(current) && !is.null(reference_metrics)) {
    perf <- monitor_performance(final, current, reference_metrics)
  }

  summary <- tibble::tibble(
    date        = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    scenario    = tag,
    n_current   = nrow(current),
    n_warning   = sum(drift$status == "warning"),
    n_alert     = sum(drift$status == "alert"),
    score_psi   = drift$psi[drift$variable == "score_churn"],
    recall      = if (!is.null(perf)) perf$recall  else NA_real_,
    roc_auc     = if (!is.null(perf)) perf$roc_auc else NA_real_,
    perf_alert  = if (!is.null(perf)) perf$alert   else NA
  )

  log_path <- file.path(out_dir, "monitoring_log.csv")
  readr::write_csv(summary, log_path, append = file.exists(log_path))

  message(sprintf("[%s] %d alertes, %d warnings (PSI du score : %.3f)",
                  tag, summary$n_alert, summary$n_warning, summary$score_psi))

  invisible(list(drift = drift, performance = perf, summary = summary))
}
