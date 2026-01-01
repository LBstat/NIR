
data.analysis <- readRDS("data/intermediate/data analysis.rds")

data.cluster <- data.analysis |> dplyr::select(grep("^[[:digit:]]*$", colnames(data.analysis), value = TRUE))
colnames(data.cluster) <- paste0("V", colnames(data.cluster))
data.cluster <- bind_cols(data.cluster, class = data.analysis$class)

set.seed(123)

task <- TaskClassif$new(
  id = "Classification original data",
  backend = data.cluster,
  target = "class",
  positive = "fresco"
)

task$col_roles$stratum <- "class"

# Usiamo il filtro AUC direttamente sul task
filter_auc = flt("auc")
filter_auc$calculate(task)

# Estraiamo i punteggi e creiamo una tabella ordinata
df_scores = as.data.frame(filter_auc$scores)
colnames(df_scores) = "Score_AUC_Raw"

# Calcoliamo l'AUC Reale (Punteggio + 0.5)
df_scores$AUC_Real = round(df_scores$Score_AUC_Raw + 0.5, 3)

# Vediamo le migliori 10
print(head(df_scores, 10))

# Salviamo i nomi delle migliori 5 feature per i modelli successivi
top_features <- rownames(df_scores[1,])

data.cluster <- data.cluster |> dplyr::select(top_features, class)

task <- TaskClassif$new(
  id = "Classification original data",
  backend = data.cluster,
  target = "class",
  positive = "fresco"
)

task$col_roles$stratum <- "class"

measures <- list(
  msr("classif.acc"),
  msr("classif.auc"),
  msr("classif.ce"),
  msr("classif.sensitivity"),
  msr("classif.specificity")
)

po_us <- po("classbalancing", adjust = "minor", reference = "major", id = "balance")

knn <- lrn("classif.kknn", predict_type = "prob")

graph_knn <- po_us %>>% knn
learner_knn <- GraphLearner$new(graph_knn)

search_space <- ps(
  classif.kknn.k = p_int(3, 10)
)

at_knn <- AutoTuner$new(
  learner = learner_knn,
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.auc"),
  search_space = search_space,
  tuner = tnr("random_search"),
  terminator = trm("evals", n_evals = 30)
)

resampling_outer <- rsmp("cv", folds = 5)

rr_knn <- mlr3::resample(task, at_knn, resampling_outer, store_models = TRUE)

rr_knn$aggregate(measures)

lda <- lrn("classif.lda", predict_type = "prob")
qda <- lrn("classif.qda", predict_type = "prob")

graph_lda <- po_us %>>% lda
graph_qda <- po_us %>>% qda

rr_lda <- mlr3::resample(task, GraphLearner$new(graph_lda), resampling_outer, store_models = TRUE)
rr_qda <- mlr3::resample(task, GraphLearner$new(graph_qda), resampling_outer, store_models = TRUE)

# LDA
rr_lda$aggregate(measures)

# QDA
rr_qda$aggregate(measures)
