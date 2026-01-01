
data.analysis <- readRDS("data/intermediate/data analysis.rds")

pattern <- "^[[:digit:]]*$"
data.pca <- data.analysis |> dplyr::select(grep(pattern, colnames(data.analysis), value = TRUE))

pca.res <- prcomp(data.pca, center = TRUE, scale. = TRUE)
summary(pca.res)

pca.df <- data.frame(
  PC1 = pca.res$x[, 1],
  PC2 = pca.res$x[, 2],
  class = data.analysis$class
)

p1 <- ggplot(pca.df, aes(x = PC1, y = PC2, colour = class)) +
  geom_point(alpha = 0.8, size = 2.5) +
  scale_color_manual(values = c("fresco" = "#003366", "deteriorato" = "#9B1B30")) +
  labs(
    title = "PCA sugli spettri",
    x = paste0("PC1 (", round(summary(pca.res)$importance[2, 1] *100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca.res)$importance[2, 2] *100, 1), "%)"),
    color = "Freschezza"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
    panel.grid.minor = element_blank())

p1

ggsave("plot/scatterplotPCA.png", plot = p1, width = 8, height = 6, units = "in")

loadings <- pca.res$rotation
contrib <- loadings^2
round(contrib[, 1:2],4)

table(pca.df$class)

task <- TaskClassif$new(
  id = "Classification on PCA",
  backend = pca.df,
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

pca.oversample <- oversample.with.noise(pca.df, "class", "fresco")

table(pca.oversample$class)

task <- TaskClassif$new(
  id = "Classification on PCA oversample",
  backend = pca.oversample,
  target = "class",
  positive = "fresco"
)

task$col_roles$stratum <- "class"
task

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

resampling_outer <- rsmp("cv", folds = 5)
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

pca.undersample <- undersample(pca.df, "class", "deteriorato")

table(pca.undersample$class)

task <- TaskClassif$new(
  id = "Classification on PCA undersample",
  backend = pca.undersample,
  target = "class",
  positive = "fresco"
)

task$col_roles$stratum <- "class"
task

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

resampling_outer <- rsmp("cv", folds = 5)
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
