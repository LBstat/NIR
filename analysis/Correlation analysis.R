
data.analysis <- readRDS("data/intermediate/data analysis.rds")

cor.data <- data.analysis |> dplyr::select(!c(sample, class))
corr_Kvalue <- cor(cor.data)["K value",]

top.cor <- sort(abs(corr_Kvalue), decreasing = TRUE)
top.cor <- head(top.cor, 10)

data.cluster <- data.analysis |> dplyr::select(grep("^[[:digit:]]*$", names(top.cor), value = TRUE), class)

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
