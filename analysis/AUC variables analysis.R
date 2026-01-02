
data.analysis <- readRDS("data/intermediate/data analysis.rds")

data.cluster <- data.analysis |> dplyr::select(grep("^[[:digit:]]*$", colnames(data.analysis), value = TRUE))
colnames(data.cluster) <- paste0("V", colnames(data.cluster))
data.cluster <- bind_cols(data.cluster, class = data.analysis$class)

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

# Salviamo i nomi delle migliori 3 feature per i modelli successivi
top_features <- rownames(df_scores[1:3,])

data.cluster <- data.cluster |> dplyr::select(top_features, class)

task <- TaskClassif$new(
  id = "Classification original data",
  backend = data.cluster,
  target = "class",
  positive = "fresco"
)

task$col_roles$stratum <- "class"

model_lda(task)
model_qda(task)

tune_knn(task)