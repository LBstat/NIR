
data.analysis <- readRDS("data/intermediate/data analysis.rds")

cor.data <- data.analysis |> dplyr::select(!c(sample, class))
corr_Kvalue <- cor(cor.data)["K value", ]

sort(abs(corr_Kvalue), decreasing = TRUE)

data.cluster <- data.analysis |> dplyr::select(`1373`, class)

task <- TaskClassif$new(
  id = "Classification original data",
  backend = data.cluster,
  target = "class",
  positive = "fresco"
)

task$col_roles$stratum <- "class"

lda <- lrn("classif.lda", predict_type = "prob")

qda <- lrn("classif.qda", predict_type = "prob")

knn <- lrn(
  "classif.kknn",
  predict_type = "prob"
)

search_space <- ps(
  k = p_int(3, 10)
)

at_knn <- AutoTuner$new(
  learner = knn,
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.auc"),
  search_space = search_space,
  tuner = tnr("random_search"),
  terminator = trm("evals", n_evals = 30)
)

resampling_outer <- rsmp("cv", folds = 10)
resampling_outer$instantiate(task)

# k-NN tuned
rr_knn <- mlr3::resample(task, at_knn, resampling_outer, store_models = TRUE)

# LDA e QDA
rr_lda <- mlr3::resample(task, lda, resampling_outer, store_models = TRUE)
rr_qda <- mlr3::resample(task, qda, resampling_outer, store_models = TRUE)

measures <- list(
  msr("classif.acc"),
  msr("classif.auc"),
  msr("classif.ce"),
  msr("classif.sensitivity"),
  msr("classif.specificity")
)

# k-NN
rr_knn$aggregate(measures)

# LDA
rr_lda$aggregate(measures)

# QDA
rr_qda$aggregate(measures)

rr_knn$score()
rr_lda$score()
rr_qda$score()

data.cluster.undersample <- undersample(data.cluster, "class", "deteriorato")

table(data.cluster.undersample$class)


task <- TaskClassif$new(
  id = "Classification original data",
  backend = data.cluster.undersample,
  target = "class",
  positive = "fresco"
)

task
task$col_roles$stratum <- "class"

lda <- lrn("classif.lda", predict_type = "prob")

qda <- lrn("classif.qda", predict_type = "prob")

knn <- lrn(
  "classif.kknn",
  predict_type = "prob"
)

search_space <- ps(
  k = p_int(3, 10)
)

at_knn <- AutoTuner$new(
  learner = knn,
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.auc"),
  search_space = search_space,
  tuner = tnr("random_search"),
  terminator = trm("evals", n_evals = 30)
)

resampling_outer <- rsmp("cv", folds = 10)
resampling_outer$instantiate(task)

# k-NN tuned
rr_knn <- mlr3::resample(task, at_knn, resampling_outer, store_models = TRUE)

# LDA e QDA
rr_lda <- mlr3::resample(task, lda, resampling_outer, store_models = TRUE)
rr_qda <- mlr3::resample(task, qda, resampling_outer, store_models = TRUE)

measures <- list(
  msr("classif.acc"),
  msr("classif.auc"),
  msr("classif.ce"),
  msr("classif.sensitivity"),
  msr("classif.specificity")
)

# k-NN
rr_knn$aggregate(measures)

# LDA
rr_lda$aggregate(measures)

# QDA
rr_qda$aggregate(measures)

rr_knn$score()
rr_lda$score()
rr_qda$score()