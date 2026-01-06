
# Function for lda model
model_lda <- function(task, resampling_folds = 5, resampling_instance = NULL, resolution = 10, measure_prob = "classif.auc",
  measures = c("classif.acc", "classif.auc", "classif.ce", "classif.sensitivity", "classif.specificity"), filter = TRUE,
  filter_type = "auc", pca = FALSE) {

  # Assertions
  assertNumber(resampling_folds, lower = 3)
  assertNumber(resolution, lower = 10)
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

  # 2 Hyperparameters
  # 2.1 Conditionally add filter
  if (filter) {
    pipeline <- c(pipeline, po("filter", filter = flt(filter_type), filter.nfeat = 1, id = "feat_select"))
    
    param_space <- ps(
      feat_select.filter.nfeat = p_int(lower = 1, upper = 10)
      )
  }
  
  # 2.2 Conditionally add PCA
  if (pca) {
    pipeline <- c(pipeline, po("pca", rank. = 1, id = "rank_select"))
    
    param_space <- ps(
      rank_select.rank. = p_int(lower = 1, upper = 12)
      )
  }

  # 3. Add the Learner
  lda_lrn <- lrn("classif.lda", predict_type = "prob")
  pipeline <- c(pipeline, lda_lrn)

  # 4. This connects everything in the list regardless of length
  my_graph <- Reduce(`%>>%`, pipeline)

  # Visualize for debugging
  my_graph$plot()

  glrn <- GraphLearner$new(my_graph)

  # 5. AutoTuner
  at_lda <- AutoTuner$new(
    learner = glrn,
    resampling = rsmp("cv", folds = resampling_folds),
    measure = msr(measure_prob),
    search_space = param_space,
    terminator = trm("none"),
    tuner = tnr("grid_search", resolution = resolution)
    )

  # 6. Resampling (using the function argument for folds)
  set.seed(123)
  if (is.null(resampling_instance)) {
    rr_lda <- mlr3::resample(task, at_lda, rsmp("cv", folds = resampling_folds), store_models = TRUE)
  } else {
    rr_lda <- mlr3::resample(task, at_lda, resampling_instance, store_models = TRUE)
  }

  # Final model
  at_lda$train(task)

  # 7. Output Extraction
  list(
    performance = rr_lda$aggregate(msrs(measures)),
    confusion_matrix = rr_lda$prediction()$confusion,
    resampling_results = rr_lda,
    final_model_object = at_lda
    )
}

# Function for qda model
model_qda <- function(task, resampling_folds = 5, resampling_instance = NULL, resolution = 10, measure_prob = "classif.auc",
  measures = c("classif.acc", "classif.auc", "classif.ce", "classif.sensitivity", "classif.specificity"), filter = TRUE,
  filter_type = "auc", pca = FALSE) {

  # Assertions
  assertNumber(resampling_folds, lower = 3)
  assertNumber(resolution, lower = 10)
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

  # 2 Hyperparameters
  # 2.1 Conditionally add filter
  if (filter) {
    pipeline <- c(pipeline, po("filter", filter = flt(filter_type), filter.nfeat = 1, id = "feat_select"))

    param_space <- ps(
      feat_select.filter.nfeat = p_int(lower = 1, upper = 5)
      )
  }
  
  # 2.2 Conditionally add PCA
  if (pca) {
    pipeline <- c(pipeline, po("pca", rank. = 1, id = "rank_select"))

    param_space <- ps(
      rank_select.rank. = p_int(lower = 1, upper = 5)
      )
  }

  # 3. Add the Learner
  qda_lrn <- lrn("classif.qda", predict_type = "prob")
  pipeline_main <- c(pipeline, qda_lrn)

  # 4. This connects everything in the list regardless of length
  my_graph <- Reduce(`%>>%`, pipeline_main)

  # Visualize for debugging
  my_graph$plot()

  glrn <- GraphLearner$new(my_graph)

  # 5. AutoTuner
  at_qda <- AutoTuner$new(
    learner = glrn,
    resampling = rsmp("cv", folds = resampling_folds),
    measure = msr(measure_prob),
    search_space = param_space,
    terminator = trm("none"),
    tuner = tnr("grid_search", resolution = resolution)
    )

  # 6. Resampling (using the function argument for folds)
  set.seed(123)
  if (is.null(resampling_instance)) {
    rr_qda <- mlr3::resample(task, at_qda, rsmp("cv", folds = resampling_folds), store_models = TRUE)
  } else {
    rr_qda <- mlr3::resample(task, at_qda, resampling_instance, store_models = TRUE)
  }

  # Final model
  at_qda$train(task)

  # 7. Output Extraction
  list(
    performance = rr_qda$aggregate(msrs(measures)),
    confusion_matrix = rr_qda$prediction()$confusion,
    resampling_results = rr_qda,
    final_model_object = at_qda
    )
}
