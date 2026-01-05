
feature_analysis <- function(task, resampling_folds = 5, resampling_instance = NULL, resolution = 10, n_evals_knn = 50, n_evals_svm = 100,
  measure_prob = "classif.auc", measures = c("classif.acc", "classif.auc", "classif.ce", "classif.sensitivity", "classif.specificity"),
  filter = TRUE, filter_type = "auc", pca = FALSE) {
  
  results.lda <- model_lda(task, resampling_folds, resampling_instance, resolution, measure_prob, measures, filter, filter_type, pca)
  results.qda <- model_qda(task, resampling_folds, resampling_instance, resolution, measure_prob, measures, filter, filter_type, pca)
  results.knn <- tune_knn(task, resampling_folds, resampling_instance, n_evals_knn, measure_prob, measures, filter, filter_type, pca)
  results.svm <- tune_svm(task, resampling_folds, resampling_instance, n_evals_svm, measure_prob, measures, filter, filter_type, pca)

  # Performance
  performance <- rbind(
    results.lda$performance,
    results.qda$performance,
    results.knn$performance,
    results.svm$performance
  )

  rownames(performance) <- c("LDA", "QDA", "k-NN", "SVM")

  # Confusion matrix
  confusion <- list(
    LDA  = results.lda$confusion_matrix,
    QDA  = results.qda$confusion_matrix,
    kNN  = results.knn$confusion_matrix,
    SVM = results.svm$confusion_matrix
  )

  # Learners and parameters
  learners <- list(
    LDA = results.lda$final_model_object,
    QDA = results.qda$final_model_object,
    kNN = results.knn$final_model_object,
    SVM = results.svm$final_model_object
  )

  # Output
  results <- list(
    performance = performance,
    confusion = confusion,
    learners = learners
  )

  pattern <- "results([0-9]+)\\.rds"

  existing.files <- list.files("data/results/", pattern = pattern)
  if (length(existing.files) == 0) {
    saveRDS(results, file = "data/results/results1.rds")
  } else {
    value <- vapply(existing.files, function(x) {
      as.numeric(regmatches(x, regexec(pattern, x, perl = TRUE))[[1]][[2]])
    }, numeric(1))
    saveRDS(results, file = paste0("data/results/results", max(value) + 1, ".rds"))
  }

  invisible(results)
}