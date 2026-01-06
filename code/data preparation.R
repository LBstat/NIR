
# Data building function
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
  data$ADP[[31]] <- 0.0247096

  saveRDS(data, file = "data/intermediate/data.rds")

  data.for.imputation <- data |> dplyr::select(!c(sample, sample.number))
  cor(data.for.imputation, use = "complete.obs")

  multivariable.normality.test(data.for.imputation)

  m <- 10
  imp <- mice(data.for.imputation[, -7], m = m, maxit = 50, seed = 123, method = "pmm", printFlag = FALSE)

  calculate.k <- function(df) {
    num <- df$Inosina + df$ipoxantina
    den <- df$ATP + df$ADP + df$AMP + df$IMP + df$Inosina + df$ipoxantina
    num / den
  }

  imputed.df <- lapply(seq_len(m), function(i) {
    df <- complete(imp, i)
    df$`K value` <- calculate.k(df)
    df
  })

  imputed.pos <- which(is.na(data.for.imputation$`K value`))
  real.vals <- data.for.imputation$`K value`[-imputed.pos]

  ks.distances <- sapply(imputed.df, function(k.vector) {
    imputed.vals <- k.vector[imputed_pos]

    ks.test(real.vals, imputed.vals)$statistic
  })

  best_idx <- which.min(unname(ks.distances))
  cat("Il dataset statisticamente più coerente è il numero:", best_idx)

  trend.errors <- sapply(imputed.df, function(k.vector) {

    df.temp <- data.frame(k = k.vector, day = data.for.imputation$day)
    mod <- lm(k ~ day, data = df.temp[-imputed.pos, ])

    preds <- predict(mod, newdata = df.temp[imputed.pos, ])
    mean((k.vector[imputed.pos] - preds)^2)
  })

  best_idx <- which.min(trend.errors)
  cat("Il dataset statisticamente più coerente è il numero:", best_idx)

  complete.data <- complete(imp, "long")

  cor(complete.data[, -c(14, 15)], use = "complete.obs")
  cor(data.for.imputation[, -7], use = "complete.obs")
  # correlation structure preserved for strong correlated variables, slight shrinkage for less correlated variables

  graphics <- lapply(seq_along(imputed.df), function(i) {
    graphical.comparation(imputed.df[[i]], imputed_pos = imputed.pos, x = "day", y = "K value", x_ignore = TRUE, dataset_id = i)
  })

  data.imputated <- imputed.df[[5]]
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

# Function that explores different method for feature selection
feature_selection <- function(task, filter = "auc") {

  # Assertions
  assertString(filter)
  assertSubset(filter, choices = c("anova", "auc", "importance", "information_gain", "mrmr"))

  # Selection
  set.seed(123)
  filter_selection = flt(filter)
  filter_selection$calculate(task)

  filter_selection$scores
}