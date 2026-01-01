
# Carico i dati
data.analysis <- readRDS("data/intermediate/data analysis.rds")

# Seleziono solo le colonne numeriche (spettrali)
pattern <- "^[[:digit:]]*$"
data.spectra <- data.analysis |> dplyr::select(grep(pattern, colnames(data.analysis), value = TRUE))


X <- data.spectra
y <- (data.analysis$class)

set.seed(123)

cost.grid  <- 2^(-1:5)
gamma.grid <- 2^(-10:-2)

tune.res <- tune(
  svm,
  train.x = X,
  train.y = y,
  kernel = "radial",
  ranges = list(cost = cost.grid, gamma = gamma.grid),
  tunecontrol = tune.control(cross = 5)
)

# Miglior modello
best.svm <- tune.res$best.model

# Riepilogo tuning
summary(tune.res)

# Predizioni sul dataset completo
svm.pred <- predict(best.svm, X)

# Confusion matrix
conf.mat <- table(Predicted = svm.pred, True = y)
conf.mat

# Accuracy
accuracy <- sum(diag(conf.mat)) / sum(conf.mat)
accuracy

# Parametri ottimali
best.params <- tune.res$best.parameters
best.params

smoothed.spectra <- smoothing(data.spectra)

snv.spectra <- apply(smoothed.spectra, MARGIN = 1, FUN = normalization)

X <- snv.spectra

set.seed(123)

cost.grid  <- 2^(-1:5)
gamma.grid <- 2^(-10:-2)

tune.res <- tune(
  svm,
  train.x = X,
  train.y = y,
  kernel = "radial",
  ranges = list(cost = cost.grid, gamma = gamma.grid),
  tunecontrol = tune.control(cross = 5)
)

# Miglior modello
best.svm <- tune.res$best.model

# Riepilogo tuning
summary(tune.res)

# Predizioni sul dataset completo
svm.pred <- predict(best.svm, X)

# Confusion matrix
conf.mat <- table(Predicted = svm.pred, True = y)
conf.mat

# Accuracy
accuracy <- sum(diag(conf.mat)) / sum(conf.mat)
accuracy

# Parametri ottimali
best.params <- tune.res$best.parameters
best.params


