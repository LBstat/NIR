
data.analysis <- readRDS("data/intermediate/data analysis.rds")

cor.data <- data.analysis |> dplyr::select(!c(sample, class))
corr_Kvalue <- cor(cor.data)["K value", ]

sort(abs(corr_Kvalue), decreasing = TRUE)
