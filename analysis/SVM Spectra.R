
data.analysis <- readRDS("data/intermediate/data analysis.rds")

pattern <- "^[[:digit:]]*$"
data.spectra <- data.analysis |> dplyr::select(grep(pattern, colnames(data.analysis), value = TRUE))

data.svm <- bind_cols(data.spectra, class = data.analysis$class)

task <- TaskClassif$new(
  id = "SVM",
  backend = data.svm,
  target = "class",
  positive = "fresco"
)

task$col_roles$stratum <- "class"

tune_svm(task, resampling_folds = 5, n_evals = 30, measure_prob = "classif.acc")

