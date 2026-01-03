
# Function for lda model
model_lda <- function(task, resampling_folds = 5, resampling_instance = NULL,
                      measures = c("classif.acc", "classif.auc", "classif.ce", "classif.sensitivity", "classif.specificity"),
                      filter = "auc", filter_nfeat = 5, pca = FALSE, pca_rank = NULL) {
  
  # Assertions
  assertNumber(resampling_folds, lower = 3)
  assertCharacter(measures)
  assertSubset(measures, choices = grep("^classif\\..*", measures, value = TRUE))
  assertNumber(resampling_folds, lower = 3)
  assertString(filter)
  assertSubset(filter, choices = c("anova", "auc", "importance", "information_gain"))
  assertNumber(filter_nfeat, lower = 1)
  assertFlag(pca)
  assertNumber(pca_rank, null.ok = TRUE, lower = 2)
  
  # 1. Define mandatory start of the pipeline
  pipeline <- list(
    po("scale"),
    po("classbalancing", adjust = "minor", reference = "major", id = "balance"),
    po("filter", filter = flt(filter), filter.nfeat = filter_nfeat)
  )
  
  # 2. Conditionally add PCA
  if (pca) {
    p_rank <- if (is.null(pca_rank)) min(task$ncol, 5) else pca_rank
    pipeline <- c(pipeline, po("pca", rank. = p_rank))
  }
  
  # 3. Add the Learner
  lda_lrn <- lrn("classif.lda", predict_type = "prob")
  pipeline <- c(pipeline, lda_lrn)
  
  # 4. This connects everything in the list regardless of length
  my_graph <- Reduce(`%>>%`, pipeline)
  
  # Visualize for debugging
  my_graph$plot()
  
  glrn <- GraphLearner$new(my_graph)
  
  # 5. Resampling (using the function argument for folds)
  set.seed(123)
  if (is.null(resampling_instance)) {
    rr_lda <- mlr3::resample(task, glrn, rsmp("cv", folds = resampling_folds), store_models = TRUE)
  } else {
    rr_lda <- mlr3::resample(task, glrn, resampling_instance, store_models = TRUE)
  }
  
  # Final model
  glrn$train(task)
  
  # 6. Safe Output Extraction
  list(
    performance = rr_lda$aggregate(msrs(measures)),
    confusion_matrix = rr_lda$prediction()$confusion,
    resampling_results = rr_lda,
    final_model_object = glrn
  )
}

# Function for qda model
model_qda <- function(task, resampling_folds = 5, resampling_instance = NULL,
                      measures = c("classif.acc", "classif.auc", "classif.ce", "classif.sensitivity", "classif.specificity"),
                      filter = "auc", filter_nfeat = 5, pca = FALSE, pca_rank = NULL) {
  
  # Assertions
  assertNumber(resampling_folds, lower = 3)
  assertCharacter(measures)
  assertSubset(measures, choices = grep("^classif\\..*", measures, value = TRUE))
  assertNumber(resampling_folds, lower = 3)
  assertString(filter)
  assertSubset(filter, choices = c("anova", "auc", "importance", "information_gain"))
  assertNumber(filter_nfeat, lower = 1)
  assertFlag(pca)
  assertNumber(pca_rank, null.ok = TRUE, lower = 2)
  
  # 1. Define mandatory start of the pipeline
  pipeline <- list(
    po("scale"),
    po("classbalancing", adjust = "minor", reference = "major", id = "balance"),
    po("filter", filter = flt(filter), filter.nfeat = filter_nfeat)
  )
  
  # 2. Conditionally add PCA
  if (pca) {
    p_rank <- if (is.null(pca_rank)) min(task$ncol, 5) else pca_rank
    pipeline <- c(pipeline, po("pca", rank. = p_rank))
  }
  
  # 3. Add the Learner
  qda_lrn <- lrn("classif.qda", predict_type = "prob")
  pipeline <- c(pipeline, qda_lrn)
  
  # 4. This connects everything in the list regardless of length
  my_graph <- Reduce(`%>>%`, pipeline)
  
  # Visualize for debugging
  my_graph$plot()
  
  glrn <- GraphLearner$new(my_graph)
  
  # 5. Resampling (using the function argument for folds)
  set.seed(123)
  if (is.null(resampling_instance)) {
    rr_qda <- mlr3::resample(task, glrn, rsmp("cv", folds = resampling_folds), store_models = TRUE)
  } else {
    rr_qda <- mlr3::resample(task, glrn, resampling_instance, store_models = TRUE)
  }
  
  # Final model
  glrn$train(task)
  
  # 6. Safe Output Extraction
  list(
    performance = rr_qda$aggregate(msrs(measures)),
    confusion_matrix = rr_qda$prediction()$confusion,
    resampling_results = rr_qda,
    final_model_object = glrn
  )
}