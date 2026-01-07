
# Data building function
build_data_main <- function() {
  data <- read_excel("data/raw/Dati filetti.xlsx")
  data_lightness <- read_excel("data/raw/Dati luminosità.xlsx")
  spectral_measures <- read_excel("data/raw/Spettri.xlsx")

  pattern <- "^[[:alpha:]]\\*?$"
  data_for_compare <- data_lightness |> dplyr::select(grep(pattern, colnames(data.lightness), value = TRUE))

  data_norm <- as_tibble(apply(data_for_compare[1:5, ], MARGIN = 2, FUN = normalization))
  data_long <- data_norm |> pivot_longer(cols = everything(), names_to = "variabile", values_to = "valore") 

  density_comparation(data.long)

  # Lightness data aggregation
  data_lightness <- data_lightness |> dplyr::select(campione, grep(pattern, colnames(data.lightness), value = TRUE)) |>
    group_by(campione) |>
    summarize(L.mediana = median(`L*`),
              a.mediana = median(`a*`),
              b.mediana = median(`b*`),
              C.mediana = median(`C*`),
              h.mediana = median(h)) |>
    mutate(sample = as.numeric(gsub("campione_", "", campione))) |>
    arrange(sample)

  data_lightness <- data_lightness |> dplyr::select(!campione) |>
    mutate(day = ceiling(row_number() / 5))

  data <- cbind(data, data.lightness)

  # Data entry mistake correction
  data$ADP[[31]] <- 0.0247096

  # Spectral data aggregation
  spectral_measures <- spectral_measures[-which(spectral_measures$spettro == 6),]

  pattern <- "^[[:digit:]]*$"
  spectral_measures <- spectral_measures |> dplyr::select(grep(pattern, colnames(spectral_measures), value = TRUE))

  data_long <- spectral_measures[1:5, sample(length(colnames(spectral_measures)), 10)] |>
    pivot_longer(cols = everything(), names_to = "variabile", values_to = "valore") 

  density_comparation(data_long)

  spectral_measures <- spectral_measures |>
    mutate(sample = ((row_number() - 1) %/% 5) + 1) |>
    group_by(sample) |>
    summarize(across(everything(), ~ median(.x, na.rm = TRUE)))

  data_analysis <- merge(data, spectral_measures)

  saveRDS(data_analysis, file = "data/intermediate/data analysis.rds")
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