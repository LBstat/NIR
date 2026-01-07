
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
    mutate(sample = as.numeric(gsub("campione_", "", campione))) |>
    arrange(sample)

  data.lightness <- data.lightness |> dplyr::select(!campione) |>
    mutate(day = ceiling(row_number() / 5))

  data <- cbind(data, data.lightness)
  data$ADP[[31]] <- 0.0247096

  spectral.measures <- spectral.measures[-which(spectral.measures$spettro == 6),]
  
  pattern <- "^[[:digit:]]*$"
  spectral.measures <- spectral.measures |> dplyr::select(grep(pattern, colnames(spectral.measures), value = TRUE))
  
  data.long <- spectral.measures[1:5, sample(length(colnames(spectral.measures)), 10)] |> pivot_longer(cols = everything(), names_to = "variabile", values_to = "valore") 
  
  density.comparation(data.long)
  
  spectral.measures <- spectral.measures |>
    mutate(sample = ((row_number() - 1) %/% 5) + 1) |>
    group_by(sample) |>
    summarize(across(everything(), ~ median(.x, na.rm = TRUE)))
  
  data.analysis <- merge(data, spectral.measures)

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