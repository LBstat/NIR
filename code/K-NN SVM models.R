
# Function that trains the support vector machine algortihm.
tune_svm <- function(task, resampling_folds = 5, resampling_instance = NULL, n_evals = 100, measure_prob = "classif.auc",
  measures = c("classif.acc", "classif.auc", "classif.ce", "classif.sensitivity", "classif.specificity"), filter = TRUE,
  filter_type = "auc", pca = FALSE) {

  # Assertions
  assertNumber(resampling_folds, lower = 3)
  assertNumber(n_evals, lower = 15)
  assertString(measure_prob)
  assertSubset(measure_prob, choices = grep("^classif\\..*", measure_prob, value = TRUE))
  assertCharacter(measures)
  assertSubset(measures, choices = grep("^classif\\..*", measures, value = TRUE))
  assertFlag(filter)
  assertString(filter_type)
  assertSubset(filter_type, choices = c("anova", "auc", "mrmr"))
  assertFlag(pca)

  if (all(c(filter, pca))) {
    stop("Choose either feature selection or pca")
  }

  # 1. Define mandatory start of the pipeline
  pipeline <- list(
    po("scale"),
    po("classbalancing", adjust = "minor", reference = "major", id = "balance")
  )

  # 2. Hyperparameters
  # 2.1 Conditionally add filter
  if (filter) {
    pipeline <- c(pipeline, po("filter", filter = flt(filter_type), filter.nfeat = 1, id = "feat_select"))

    param_space <- ps(
      feat_select.filter.nfeat = p_int(lower = 1, upper = 10),
      classif.svm.kernel = p_fct(c("radial", "polynomial", "sigmoid")),
      classif.svm.cost = p_dbl(lower = 0.1, upper = 10, logscale = TRUE),
      classif.svm.gamma = p_dbl(lower = 0.001, upper = 1, logscale = TRUE)
      )
  }

  # 2.2 Conditionally add PCA
  if (pca) {
    pipeline <- c(pipeline, po("pca", rank. = 1, id = "rank_select"))

    param_space <- ps(
      rank_select.rank. = p_int(lower = 1, upper = 8),
      classif.svm.kernel = p_fct(c("radial", "polynomial", "sigmoid")),
      classif.svm.cost = p_dbl(lower = 0.1, upper = 10, logscale = TRUE),
      classif.svm.gamma = p_dbl(lower = 0.001, upper = 1, logscale = TRUE)
      )
  }

  # 3. Add the Learner
  svm_lrn <- lrn("classif.svm", type = "C-classification", predict_type = "prob")
  pipeline <- c(pipeline, svm_lrn)

  # 4. This connects everything in the list regardless of length
  my_graph <- Reduce(`%>>%`, pipeline)

  # Visualize for debugging
  my_graph$plot()

  glrn <- GraphLearner$new(my_graph)

  # 5. AutoTuner
  at_svm <- AutoTuner$new(
    learner = glrn,
    resampling = rsmp("cv", folds = resampling_folds),
    measure = msr(measure_prob),
    search_space = param_space,
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("random_search")
  )

  # 6. Resampling (using the function argument for folds)
  set.seed(123)
  if (is.null(resampling_instance)) {
    rr_svm <- mlr3::resample(task, at_svm, rsmp("cv", folds = resampling_folds), store_models = TRUE)
  } else {
    rr_svm <- mlr3::resample(task, at_svm, resampling_instance, store_models = TRUE)
  }

  # Final model
  at_svm$train(task)

  # 7. Output Extraction
  list(
    performance = rr_svm$aggregate(msrs(measures)),
    confusion_matrix = rr_svm$prediction()$confusion,
    resampling_results = rr_svm,
    final_model_object = at_svm
  )
}

# Function that trains the knn algortihm.
tune_knn <- function(task, resampling_folds = 5, resampling_instance = NULL, n_evals = 50, measure_prob = "classif.auc",
  measures = c("classif.acc", "classif.auc", "classif.ce", "classif.sensitivity", "classif.specificity"), filter = TRUE,
  filter_type = "auc", pca = FALSE) {

  # Assertions
  assertNumber(resampling_folds, lower = 3)
  assertNumber(n_evals, lower = 15)
  assertString(measure_prob)
  assertSubset(measure_prob, choices = grep("^classif\\..*", measure_prob, value = TRUE))
  assertCharacter(measures)
  assertSubset(measures, choices = grep("^classif\\..*", measures, value = TRUE))
  assertFlag(filter)
  assertString(filter_type)
  assertSubset(filter_type, choices = c("anova", "auc", "mrmr"))
  assertFlag(pca)

  if (all(c(filter, pca))) {
    stop("Choose either feature selection or pca")
  }

  # 1. Define mandatory start of the pipeline
  pipeline <- list(
    po("scale"),
    po("classbalancing", adjust = "minor", reference = "major", id = "balance")
  )

  # 2. Hyperparameters
  # 2.1 Conditionally add filter
  if (filter) {
    pipeline <- c(pipeline, po("filter", filter = flt(filter_type), filter.nfeat = 1, id = "feat_select"))

    param_space <- ps(
      feat_select.filter.nfeat = p_int(lower = 1, upper = 10),
      classif.kknn.k = p_int(lower = 2, upper = 15),
      classif.kknn.distance = p_dbl(lower = 1, upper = 2)
      )
  }
  
  # 2.2 Conditionally add PCA
  if (pca) {
    pipeline <- c(pipeline, po("pca", rank. = 1, id = "rank_select"))
    param_space <- ps(
      rank_select.rank. = p_int(lower = 1, upper = 8),
      classif.kknn.k = p_int(lower = 2, upper = 15),
      classif.kknn.distance = p_dbl(lower = 1, upper = 2)
      )
  }

  # 3. Add the Learner
  knn_lrn <- lrn("classif.kknn", predict_type = "prob")
  pipeline <- c(pipeline, knn_lrn)

  # 4. This connects everything in the list regardless of length
  my_graph <- Reduce(`%>>%`, pipeline)

  # Visualize for debugging
  my_graph$plot()

  glrn <- GraphLearner$new(my_graph)

  # 5. AutoTuner
  at_knn <- AutoTuner$new(
    learner = glrn,
    resampling = rsmp("cv", folds = resampling_folds),
    measure = msr(measure_prob),
    search_space = param_space,
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("random_search")
  )

  # 6. Resampling (using the function argument for folds)
  set.seed(123)
  if (is.null(resampling_instance)) {
    rr_knn <- mlr3::resample(task, at_knn, rsmp("cv", folds = resampling_folds), store_models = TRUE)
  } else {
    rr_knn <- mlr3::resample(task, at_knn, resampling_instance, store_models = TRUE)
  }

  # Final model
  at_knn$train(task)

  # 7. Output Extraction
  list(
    performance = rr_knn$aggregate(msrs(measures)),
    confusion_matrix = rr_knn$prediction()$confusion,
    resampling_results = rr_knn,
    final_model_object = at_knn
  )
}
