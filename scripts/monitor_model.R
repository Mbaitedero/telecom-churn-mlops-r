# ==============================================================================
# Script  : scripts/monitor_model.R
# Rôle    : Démonstration du monitoring. Compare le jeu train (référence) à
#           1) le jeu test (aucune dérive attendue)
#           2) une version du jeu test volontairement modifiée (dérive simulée)
# Prérequis : targets::tar_make() déjà exécuté
# ==============================================================================

library(parsnip); library(workflows); library(recipes)

source("R/preprocessing.R")
source("R/train.R")        # eval_at_threshold()
source("R/monitoring.R")

reference <- targets::tar_read(train_df)
current   <- targets::tar_read(test_df)
final     <- targets::tar_read(final_model)
ref_perf  <- targets::tar_read(test_metrics)


# simulate_drift()
#   Simule un changement de population : factures plus élevées, clients plus
#   récents, davantage de contrats mensuels.
simulate_drift <- function(df, seed = 42) {
  set.seed(seed)
  df$MonthlyCharges <- df$MonthlyCharges * 1.3
  df$tenure         <- round(df$tenure * 0.6)
  df$TotalCharges   <- df$MonthlyCharges * pmax(df$tenure, 1)

  flip <- df$Contract != "Month-to-month" & runif(nrow(df)) < 0.5
  df$Contract[flip] <- "Month-to-month"
  df
}

# 1) Pas de dérive attendue
ok <- run_monitoring(reference, current, final, ref_perf, tag = "reference_vs_test")
print(utils::head(ok$drift, 6))

# 2) Dérive simulée
ko <- run_monitoring(reference, simulate_drift(current), final, ref_perf,
                     tag = "derive_simulee")
print(utils::head(ko$drift, 6))
print(ko$performance, width = Inf)

print(readr::read_csv("reports/metrics/monitoring_log.csv", show_col_types = FALSE),
      width = Inf)
