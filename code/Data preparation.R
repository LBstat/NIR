
data <- read_excel("data/raw/Dati filetti.xlsx")
data.lightness <- read_excel("data/raw/Dati luminosità.xlsx")
spectral.measures <- read_excel("data/raw/Spettri.xlsx")

str(data)
str(data.lightness)

pattern <- "^[[:alpha:]]\\*?$"

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

lapply(imputed.df, function(i) {
  graphical.comparation(i, imputed.pos = imputed.pos, x = "day", y = "K value")
})
# dataset 7 seems to overlap the original data best 

data.imputated <- imputed.df[[7]]
saveRDS(data.imputated, file = "data/intermediate/data imputated.rds")

table(spectral.measures$spettro)
spectral.measures <- spectral.measures[-which(spectral.measures$spettro == 6),]

pattern <- "^[[:digit:]]*$"
spectral.measures <- spectral.measures |> dplyr::select(grep(pattern, colnames(spectral.measures), value = TRUE))

spectral.measures <- spectral.measures |>
  mutate(sample = ((row_number() - 1) %/% 5) + 1) |>
  group_by(sample) |>
  summarize(across(everything(), ~ median(.x, na.rm = TRUE)))

data.analysis <- bind_cols(data.imputated, spectral.measures)
str(data.analysis)

data.analysis$class <- ifelse(data.analysis$`K value` < 0.2, "molto fresco",
                              ifelse(data.analysis$`K value` <= 0.4, "fresco",
                                     ifelse(data.analysis$`K value` <= 0.6, "deteriorato", "non commerciabile")))

data.analysis$class <- as.factor(data.analysis$class)
saveRDS(data.analysis, file = "data/intermediate/data analysis.rds")
