
cor.analysis <- function(task) {

  results1.lda <- model_lda(task)
  results1.qda <- model_qda(task)
  results1.knn <- tune_knn(task)

  # Performance
  performance <- rbind(
    results1.lda$performance,
    results1.qda$performance,
    results1.knn$nested_performance
  )

  rownames(performance) <- c("LDA", "QDA", "k-NN")

  # Confusion matrix
  confusion <- list(
    LDA  = results1.lda$confusion_matrix,
    QDA  = results1.qda$confusion_matrix,
    kNN  = results1.knn$confusion_matrix
  )

  # Learners and parameters
  learners <- list(
    LDA = results1.lda$final_model_object,
    QDA = results1.qda$final_model_object,
    kNN = results1.knn$model
  )

  # Output
  results <- list(
    performance = performance,
    confusion   = confusion,
    learners    = learners
  )

  saveRDS(results, file = "data/results/results correlation.rds")

  invisible(results)
}

auc.analysis <- function(task) {
  
  results2.lda <- model_lda(task)
  results2.qda <- model_qda(task)
  results2.knn <- tune_knn(task)

  # Performance
  performance <- rbind(
    results2.lda$performance,
    results2.qda$performance,
    results2.knn$nested_performance
  )

  rownames(performance) <- c("LDA", "QDA", "k-NN")

  # Confusion matrix
  confusion <- list(
    LDA  = results2.lda$confusion_matrix,
    QDA  = results2.qda$confusion_matrix,
    kNN  = results2.knn$confusion_matrix
  )

  # Learners and parameters
  learners <- list(
    LDA = results2.lda$final_model_object,
    QDA = results2.qda$final_model_object,
    kNN = results2.knn$model
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