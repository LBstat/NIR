
# Function that trains the support vector machine algortihm.
tune_svm <- function(task, resampling_folds = 5, n_evals = 30, measure_prob = "classif.auc",
  measures = c("classif.acc", "classif.auc", "classif.ce", "classif.sensitivity", "classif.specificity")) {

  # Assertions
  assert_task(task, task_type = "classif")
  assertNumber(resampling_folds, lower = 3)
  assertNumber(n_evals, lower = 15)
  assertString(measure_prob)
  assertSubset(measure_prob, choices = grep("^classif\\..*", measure_prob, value = TRUE))
  assertCharacter(measures)
  assertSubset(measures, choices = grep("^classif\\..*", measures, value = TRUE))

  # Pipeline with scaling and class balancing
  po_scale <- po("scale")
  po_balance <- po("classbalancing", adjust = "minor", reference = "major", id = "balance")

  learner_svm <- lrn("classif.svm", type = "C-classification", predict_type = "prob")
  graph <- po_scale %>>% po_balance %>>% learner_svm
  glrn <- GraphLearner$new(graph)

  # Hyperparameters
  param_space <- ps(
    classif.svm.kernel = p_fct(c("radial", "polynomial", "sigmoid")),
    classif.svm.cost = p_dbl(lower = 0.1, upper = 10, logscale = TRUE),
    classif.svm.gamma = p_dbl(lower = 0.001, upper = 1, logscale = TRUE)
  )

  # AutoTuner
  at_svm <- AutoTuner$new(
    learner = glrn,
    resampling = rsmp("cv", folds = resampling_folds),
    measure = msr(measure_prob),
    search_space = param_space,
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("random_search")
  )

  # Nested Cross-Validation
  set.seed(123)
  outer_resampling <- rsmp("cv", folds = resampling_folds)
  rr_svm <- mlr3::resample(task, at_svm, outer_resampling, store_models = TRUE)

  # Results over all folds
  performance_reale <- rr_svm$aggregate(lapply(measures, function(x) msr(x)))

  # Final model
  at_svm$train(task)
  best_params = at_svm$archive$best()$x_domain[[1]]

  # Output
  list(
    nested_performance = performance_reale,
    resampling_result = rr_svm, # Results for each fold
    model = at_svm,             # Final model
    best_params = best_params,
    confusion_matrix = rr_svm$prediction()$confusion
  )
}

# Function that trains the knn algortihm.
tune_knn <- function(task, resampling_folds = 5, n_evals = 30, measure_prob = "classif.auc",
  measures = c("classif.acc", "classif.auc", "classif.ce", "classif.sensitivity", "classif.specificity")) {

  # Assertions
  assert_task(task, task_type = "classif")
  assertNumber(resampling_folds, lower = 3)
  assertNumber(n_evals, lower = 15)
  assertString(measure_prob)
  assertSubset(measure_prob, choices = grep("^classif\\..*", measure_prob, value = TRUE))
  assertCharacter(measures)
  assertSubset(measures, choices = grep("^classif\\..*", measures, value = TRUE))

  # Pipeline with scaling and class balancing
  po_scale <- po("scale")
  po_balance <- po("classbalancing", adjust = "minor", reference = "major", id = "balance")

  learner_knn <- lrn("classif.kknn", predict_type = "prob")
  graph <- po_scale %>>% po_balance %>>% learner_knn
  glrn <- GraphLearner$new(graph)

  # Hyperparameters
  param_space <- ps(
    classif.kknn.k = p_int(lower = 3, upper = 15),
    classif.kknn.distance = p_dbl(lower = 1, upper = 2)
  )

  # AutoTuner
  at_knn <- AutoTuner$new(
    learner = glrn,
    resampling = rsmp("cv", folds = resampling_folds),
    measure = msr(measure_prob),
    search_space = param_space,
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("random_search")
  )

  # Nested Cross-Validation
  set.seed(123)
  outer_resampling <- rsmp("cv", folds = resampling_folds)
  rr_knn <- mlr3::resample(task, at_knn, outer_resampling, store_models = TRUE)

  # Results over all folds
  performance_reale <- rr_knn$aggregate(lapply(measures, function(x) msr(x)))
  
  # Final model
  at_knn$train(task)
  best_params = at_knn$archive$best()$x_domain[[1]]
  
  # Output
  list(
    nested_performance = performance_reale,
    resampling_result = rr_knn, # Results for each fold
    model = at_knn,             # Final model
    best_params = best_params,
    confusion_matrix = rr_knn$prediction()$confusion
  )
}

# Function for lda model
model_lda <- function(task, resampling_folds = 5,
  measures = c("classif.acc", "classif.auc", "classif.ce", "classif.sensitivity", "classif.specificity")) {

  # Assertions
  assert_task(task, task_type = "classif")
  assertNumber(resampling_folds, lower = 3)
  assertCharacter(measures)
  assertSubset(measures, choices = grep("^classif\\..*", measures, value = TRUE))

  # Pipeline with scaling and class balancing
  po_scale <- po("scale")
  po_balance <- po("classbalancing", adjust = "minor", reference = "major", id = "balance")
  
  learner_lda <- lrn("classif.lda", predict_type = "prob")
  graph <- po_scale %>>% po_balance %>>% learner_lda
  glrn <- GraphLearner$new(graph)
  
  # Cross Validation
  set.seed(123)
  rr_lda <- mlr3::resample(task, glrn, rsmp("cv", folds = resampling_folds))
  
  # Final model
  glrn$train(task)
  
  # Output
  list(
    performance = rr_lda$aggregate(lapply(measures, function(x) msr(x))),
    confusion_matrix = rr_lda$prediction()$confusion,
    resampling_results = rr_lda,
    final_model_object = glrn,
    native_model = glrn$model$classif.lda$model
  )
}

# Function for qda model
model_qda <- function(task, resampling_folds = 5,
  measures = c("classif.acc", "classif.auc", "classif.ce", "classif.sensitivity", "classif.specificity")) {

  # Assertions
  assert_task(task, task_type = "classif")
  assertNumber(resampling_folds, lower = 3)
  assertCharacter(measures)
  assertSubset(measures, choices = grep("^classif\\..*", measures, value = TRUE))
  
  # Pipeline with scaling and class balancing
  po_scale <- po("scale")
  po_balance <- po("classbalancing", adjust = "minor", reference = "major", id = "balance")
  
  learner_qda <- lrn("classif.qda", predict_type = "prob")
  graph <- po_scale %>>% po_balance %>>% learner_qda
  glrn <- GraphLearner$new(graph)
  
  # Cross Validation
  set.seed(123)
  rr_qda <- mlr3::resample(task, glrn, rsmp("cv", folds = resampling_folds))

  # Final model
  glrn$train(task)

  # Output
  list(
    performance = rr_qda$aggregate(lapply(measures, function(x) msr(x))),
    confusion_matrix = rr_qda$prediction()$confusion,
    resampling_results = rr_qda,
    final_model_object = glrn,
    native_model = glrn$model$classif.qda$model
  )
}
