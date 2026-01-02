
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

loadings <- pca.res$rotation
contrib <- loadings^2
round(contrib[, 1:2],4)

task <- TaskClassif$new(
  id = "Classification on PCA",
  backend = pca.df,
  target = "class",
  positive = "fresco"
)

task$col_roles$stratum <- "class"

model_lda(task)
model_qda(task)

tune_knn(task)
