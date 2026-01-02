
build_data_main <- function() {
  data <- read_excel("data/raw/Dati filetti.xlsx")
  data.lightness <- read_excel("data/raw/Dati luminosità.xlsx")
  spectral.measures <- read_excel("data/raw/Spettri.xlsx")

  pattern <- "^[[:alpha:]]\\*?$"

  data.for.compare <- data.lightness |> dplyr::select(grep(pattern, colnames(data.lightness), value = TRUE))

  data.norm <- as_tibble(apply(data.for.compare[1:5, ], MARGIN = 2, FUN = normalization))

  data.long <- data.norm |> pivot_longer(cols = everything(), names_to = "variabile", values_to = "valore") 

  density.comparation(data.long)

  data.lightness <- data.lightness |> dplyr::select(campione, grep(pattern, colnames(data.lightness), value = TRUE)) |>
    group_by(campione) |>
    summarize(L.mediana = median(`L*`),
              a.mediana = median(`a*`),
              b.mediana = median(`b*`),
              C.mediana = median(`C*`),
              h.mediana = median(h)) |>
    mutate(sample = gsub("campione_", "sample_", campione), sample.number = as.numeric(gsub("sample_", "", sample))) |>
    arrange(sample.number)

  data.lightness <- data.lightness |> dplyr::select(!campione) |>
    mutate(day = ceiling(row_number() / 5))

  data <- cbind(data, data.lightness)

  saveRDS(data, file = "data/intermediate/data.rds")

  data.for.imputation <- data[, c("IMP", "ATP", "ADP", "ipoxantina", "AMP", "Inosina", "day", "K value")]
  cor(data.for.imputation, use = "complete.obs")

  multivariable.normality.test(data.for.imputation)

  imp <- mice(data.for.imputation, m = 10, maxit = 50, seed = 123, method = "pmm", printFlag = FALSE)

  complete.data <- complete(imp, "long")

  cor(complete.data[, c("IMP", "ATP", "ADP", "ipoxantina", "AMP", "Inosina", "day", "K value")], use = "complete.obs")
  cor(data.for.imputation, use = "complete.obs")
  # correlation structure preserved for strong correlated variables, slight shrinkage for less correlated variables

  imputed.df <- complete(imp, "all")
  imputed.pos <- which(is.na(data.for.imputation$`K value`))

  graphics <- lapply(seq_along(imputed.df), function(i) {
    graphical.comparation(imputed.df[[i]], imputed_pos = imputed.pos, x = "day", y = "K value", x_ignore = TRUE, dataset_id = i)
  })

  data.imputated <- imputed.df[[7]]
  saveRDS(data.imputated, file = "data/intermediate/data imputated.rds")

  spectral.measures <- spectral.measures[-which(spectral.measures$spettro == 6),]

  pattern <- "^[[:digit:]]*$"
  spectral.measures <- spectral.measures |> dplyr::select(grep(pattern, colnames(spectral.measures), value = TRUE))

  data.long <- spectral.measures[1:5, sample(length(colnames(spectral.measures)), 10)] |> pivot_longer(cols = everything(), names_to = "variabile", values_to = "valore") 
  
  density.comparation(data.long)
  
  spectral.measures <- spectral.measures |>
    mutate(sample = ((row_number() - 1) %/% 5) + 1) |>
    group_by(sample) |>
    summarize(across(everything(), ~ median(.x, na.rm = TRUE)))

  data.analysis <- bind_cols(data.imputated, spectral.measures)

  data.analysis$class <- ifelse(data.analysis$`K value` < 0.2, "very fresh",
                                ifelse(data.analysis$`K value` <= 0.4, "fresh",
                                       ifelse(data.analysis$`K value` <= 0.6, "deteriorated", "very deteriorated")))

  data.analysis$class <- as.factor(data.analysis$class)
  saveRDS(data.analysis, file = "data/intermediate/data analysis.rds")
}

feature.selection <- function(task) {

  filter_auc = flt("auc")
  filter_auc$calculate(task)

  df_scores = as.data.frame(filter_auc$scores)
  colnames(df_scores) = "Score_AUC_Raw"

  df_scores$AUC_Real = round(df_scores$Score_AUC_Raw + 0.5, 3)
  
  print(head(df_scores, 10))

  top_features <- rownames(df_scores[1:3,])
  top_features
}