# ==============================================================================
# Fichier : R/data_ingestion.R
# Rôle    : Chargement des données brutes, exploration rapide, suppression de
#           l'identifiant, découpage en 3 jeux (train / valid / test) et
#           sauvegarde dans data/processed/.
#
# Input   : dossier data/raw/ contenant le CSV brut
# Output  : data/processed/train.csv, valid.csv, test.csv
#           (+ liste de 3 data.frames retournée par run_ingestion())
# ==============================================================================


# ingest_data()
#   Input  : dir = dossier contenant le CSV brut
#   Output : data.frame (tibble) des données brutes
ingest_data <- function(dir = "data/raw") {
  csv_files <- list.files(dir, pattern = "\\.csv$",
                          full.names = TRUE, ignore.case = TRUE)
  stopifnot("Aucun CSV trouvé dans data/raw" = length(csv_files) >= 1)

  readr::read_csv(csv_files[1], show_col_types = FALSE)
}


# explore_raw()
#   Affiche les dimensions, les premières lignes et les valeurs manquantes.
#   Input  : df = données brutes, n = nombre de lignes à afficher
#   Output : df (invisible)
explore_raw <- function(df, n = 5) {
  cat("Dimensions :", nrow(df), "lignes x", ncol(df), "colonnes\n\n")
  print(utils::head(df, n))

  na_counts <- colSums(is.na(df))
  cat("\nValeurs manquantes par colonne :\n")
  if (any(na_counts > 0)) print(na_counts[na_counts > 0]) else cat("aucune\n")

  invisible(df)
}


# drop_id()
#   Supprime la colonne identifiant (inutile pour la modélisation).
#   Input  : df, col = nom de la colonne identifiant
#   Output : df sans cette colonne
drop_id <- function(df, col = "customerID") {
  dplyr::select(df, -dplyr::any_of(col))
}


# split_three()
#   Découpe en train / valid / test, stratifié sur Churn
#   (même proportion de churn dans chaque jeu).
#   Input  : df, prop = proportions (somme = 1), seed = graine aléatoire
#   Output : liste nommée list(train, valid, test)
split_three <- function(df,
                        prop = c(train = 0.70, valid = 0.15, test = 0.15),
                        seed = 123) {
  stopifnot(abs(sum(prop) - 1) < 1e-8)
  set.seed(seed)

  first <- rsample::initial_split(df, prop = prop[["train"]], strata = Churn)
  rest  <- rsample::testing(first)

  second <- rsample::initial_split(
    rest,
    prop   = prop[["valid"]] / (prop[["valid"]] + prop[["test"]]),
    strata = Churn
  )

  list(
    train = rsample::training(first),
    valid = rsample::training(second),
    test  = rsample::testing(second)
  )
}
# check_split_balance()
#   Vérifie que la part de churn est quasi identique dans chaque jeu.
#   Input  : splits = liste nommée de data.frames, tol = écart max toléré
#            (en points de pourcentage)
#   Output : tableau récapitulatif (invisible) ; erreur si l'écart > tol
check_split_balance <- function(splits, target = "Churn",
                                positive = "Yes", tol = 1) {
  tab <- data.frame(
    jeu       = names(splits),
    lignes    = vapply(splits, nrow, integer(1)),
    churn_pct = vapply(splits, function(d) 100 * mean(d[[target]] == positive),
                       numeric(1)),
    row.names = NULL
  )
  print(tab)

  ecart <- diff(range(tab$churn_pct))
  if (ecart > tol) {
    stop(sprintf("Proportions de churn déséquilibrées (écart = %.2f points)",
                 ecart))
  }

  message(sprintf("Proportions de churn équilibrées (écart = %.2f point)", ecart))
  invisible(tab)
}

# save_splits()
#   Enregistre chaque jeu en CSV dans le dossier de sortie.
#   Input  : splits = liste nommée de data.frames, dir = dossier de sortie
#   Output : chemins des fichiers écrits (invisible)
save_splits <- function(splits, dir = "data/processed") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  paths <- file.path(dir, paste0(names(splits), ".csv"))
  for (i in seq_along(splits)) readr::write_csv(splits[[i]], paths[i])
  invisible(paths)
}


# run_ingestion()
#   Enchaîne toutes les étapes ci-dessus.
#   Input  : raw_dir, out_dir, prop, seed
#   Output : liste list(train, valid, test) (invisible)
run_ingestion <- function(raw_dir = "data/raw",
                          out_dir = "data/processed",
                          prop = c(train = 0.70, valid = 0.15, test = 0.15),
                          seed = 123) {
  raw <- ingest_data(raw_dir)
  explore_raw(raw)

  splits <- raw |> drop_id() |> split_three(prop = prop, seed = seed)
  save_splits(splits, out_dir)

  cat("\nJeux enregistrés dans", out_dir, "\n")
  check_split_balance(splits)



  invisible(splits)
}
