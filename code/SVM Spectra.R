
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

res1 <- tune_svm(task, resampling_folds = 5, n_evals = 50, measure_prob = "classif.acc")
res1$accuracy
res1$confusion_matrix
res1$best_params

head(res1$probability)

svm.undersample <- undersample(data.svm, "class", "deteriorato")

task <- TaskClassif$new(
  id = "SVM",
  backend = svm.undersample,
  target = "class",
  positive = "fresco"
)

task$col_roles$stratum <- "class"

res2 <- tune_svm(task, resampling_folds = 5, n_evals = 50, measure_prob = "classif.acc")
res2$accuracy
res2$confusion_matrix
res2$best_params

head(res2$probability)
