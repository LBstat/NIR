
# Function for qda model
model_qda_deep_dive <- function(task, resampling_folds = 5, resampling_instance = NULL, resolution = 10,
  measure_prob = "classif.auc", measures = c("classif.acc", "classif.auc", "classif.ce", "classif.sensitivity", "classif.specificity"),
  filter_type = "auc", pca_fix = TRUE, pca_selected = NULL, pca_rank = 12) {
  
  # Assertions
  assertNumber(resampling_folds, lower = 3)
  assertNumber(resolution, lower = 10)
  assertString(measure_prob)
  assertSubset(measure_prob, choices = grep("^classif\\..*", measure_prob, value = TRUE))
  assertCharacter(measures)
  assertSubset(measures, choices = grep("^classif\\..*", measures, value = TRUE))
  assertString(filter_type)
  assertSubset(filter_type, choices = c("anova", "auc", "mrmr"))
  assertFlag(pca_fix)
  assertNumber(pca_rank, lower = 2)

  # 1. Define mandatory start of the pipeline
  pipeline <- list(
    po("scale"),
    po("classbalancing", adjust = "minor", reference = "major", id = "balance"),
    po("pca", rank. = pca_rank, id = "pca_gen")
    )

  # 2. Hyperparameters
  if (pca_fix) {
    if (is.null(pca_selected)) stop("No PCA components selected")
    pipeline <- c(pipeline, po("select", selector = selector_name(pca_selected)))

    # 3. Add the Learner
    qda_lrn <- lrn("classif.qda", predict_type = "prob")
    pipeline_main <- c(pipeline, qda_lrn)

    # 4. This connects everything in the list regardless of length
    my_graph <- Reduce(`%>>%`, pipeline_main)

    # Visualize for debugging
    my_graph$plot()

    glrn <- GraphLearner$new(my_graph)

    # 6. Resampling (using the function argument for folds)
    set.seed(123)
    if (is.null(resampling_instance)) {
      rr_qda <- mlr3::resample(task, glrn, rsmp("cv", folds = resampling_folds), store_models = TRUE)
    } else {
      rr_qda <- mlr3::resample(task, glrn, resampling_instance, store_models = TRUE)
    }

    # Final model
    glrn$train(task)

    # 7. Output Extraction
    list(
      performance = rr_qda$aggregate(msrs(measures)),
      confusion_matrix = rr_qda$prediction()$confusion,
      resampling_results = rr_qda,
      final_model_object = glrn
    )
  } else {
    pipeline <- c(pipeline, po("filter", filter = flt(filter_type), id = "best_pc_select"))

    param_space <- ps(
      best_pc_select.filter.nfeat = p_int(lower = 1, upper = 4)
      )

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
}
