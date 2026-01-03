
feature_analysis <- function(task) {
  
  results.lda <- model_lda(task)
  results.qda <- model_qda(task)
  results.knn <- tune_knn(task)

  # Performance
  performance <- rbind(
    results.lda$performance,
    results.qda$performance,
    results.knn$nested_performance
  )

  rownames(performance) <- c("LDA", "QDA", "k-NN")

  # Confusion matrix
  confusion <- list(
    LDA  = results.lda$confusion_matrix,
    QDA  = results.qda$confusion_matrix,
    kNN  = results.knn$confusion_matrix
  )

  # Learners and parameters
  learners <- list(
    LDA = results.lda$final_model_object,
    QDA = results.qda$final_model_object,
    kNN = results.knn$model
  )

  # Output
  results <- list(
    performance = performance,
    confusion = confusion,
    learners = learners
  )

  saveRDS(results, file = "data/results/results auc.rds")

  invisible(results)
}