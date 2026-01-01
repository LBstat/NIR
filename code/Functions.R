
# Function that performs Shapiro Wilk Normality Test on each numerical column of a given
# data frame. The histogram of the data and the theoretical normality density curve are plotted.
multivariable.normality.test <- function(data) {
  assertDataFrame(data, types = "numeric")

  for (i in seq_len(ncol(data))) {
    var.name <- colnames(data)[i]
    vec <- data[, i]

    p_val <- shapiro.test(vec)$p.value

    if (p_val <= 0.05) {
      cat("H0 rejected for variable", var.name, "\n")
    } else {
      cat("H0 not rejected for variable", var.name, "\n")
    }

    # Compute mean and standard deviation
    mu <- mean(vec, na.rm = TRUE)
    sigma <- sd(vec, na.rm = TRUE)

    df.plot <- tibble(value = vec)

    # Plot histogram with theoretical normal curve
    p <- ggplot(df.plot, aes(x = value)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "#69b3a2", color = "black", alpha = 0.6) +
      stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), color = "#9B1B30", size = 1) +
      labs(title = "Histogram with Normal Density Curve",
           x = paste0(var.name),
           y = "Density") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
        panel.grid.minor = element_blank())

    print(p)
  }
}

# Function that compares the imputed values with the original distributions
graphical.comparation <- function(imputed.data, imputed.pos, x, y) {
  assertDataFrame(imputed.data, types = "numeric")
  assertInteger(imputed.pos, lower = 1, any.missing = FALSE)
  assertString(x)
  assertString(y)
  assertSubset(x, colnames(imputed.data))
  assertSubset(y, colnames(imputed.data))

  imputed.data$type <- "Real"
  imputed.data$type[imputed.pos] <- "Imputed"

  p1 <- ggplot(imputed.data , aes(x = .data[[x]], y = .data[[y]], color = type)) +
    geom_point(alpha = 0.8, size = 2.5) +
    scale_color_manual(values = c("Real" = "#003366", "Imputed" = "#9B1B30")) +
    geom_smooth(method = "gam", se = TRUE, color = "black") +
    labs(title = "Imputed dataset",
         x = paste0(x),
         y = paste0(y),
         color = "Type") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank())

  p2 <- ggplot(imputed.data, aes(x = .data[[x]], fill = type)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("Real" = "#003366", "Imputed" = "#9B1B30"))

  p3 <- ggplot(imputed.data, aes(x = .data[[y]], fill = type)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("Real" = "#003366", "Imputed" = "#9B1B30"))

  c(p1/(p2|p3))
}

# Function that oversamples form the unbalanced class adding noise to the numerical columns
oversample.with.noise <- function(data, target.col, minority.class, factor = 1, noise.sd = 0.01) {
  assertDataFrame(data)
  assertString(target.col)
  assertSubset(target.col, choices = colnames(data))
  assertString(minority.class)
  assertSubset(minority.class, choices = levels(data[[target.col]]))
  assertNumber(factor, lower = 1)
  assertNumber(noise.sd, lower = 0)

  minority <- data[data[[target.col]] == minority.class,]
  majority <- data[data[[target.col]] != minority.class,]

  # Number of observations to oversample
  n.new <- (nrow(majority) - nrow(minority)) * factor

  # Bootstraping form minority class
  set.seed(123)
  sampled <- minority[sample(1:nrow(minority), n.new, replace = TRUE), ]

  # Noise added to numeric columns
  num.cols <- sapply(sampled, is.numeric)
  sampled[, num.cols] <- sampled[, num.cols] + matrix(rnorm(n.new * sum(num.cols), mean = 0, sd = noise.sd), nrow = n.new)

  # New data = old data + oversampled rows
  new.data <- rbind(majority, minority, sampled)
  new.data
}

# Function that undersamples from the class with more observations
undersample <- function(data, target.col, majority.class) {
  assertDataFrame(data)
  assertString(target.col)
  assertSubset(target.col, choices = colnames(data))
  assertString(majority.class)
  assertSubset(majority.class, choices = levels(data[[target.col]]))

  majority <- data[data[[target.col]] == majority.class, ]
  minority <- data[data[[target.col]] != majority.class, ]

  # Sample majority to match minority
  set.seed(123)
  majority.sampled <- majority[sample(1:nrow(majority), nrow(minority)), ]

  # Combine with minority
  new.data <- rbind(minority, majority.sampled)

  # Shuffle rows
  new.data <- new.data[sample(1:nrow(new.data)), ]
  new.data
}

# Function that performs linear smoothing. Derivatives obtained by setting m != 0
smoothing <- function(data, window = 11, poly = 2, m = 0) {
  assertDataFrame(data, types = "numeric")
  assertNumber(window)
  assertNumber(poly)
  assertNumber(m, lower = 0)

  if (window %% 2 != 1) stop("window must be odd")

  smoothed <- t(apply(data, 1, function(x) sgolayfilt(x, p = poly, n = window, m = m)))
  as.data.frame(smoothed)
}

# Function to normalize single numeric vectors
normalization <- function(x) {
  assertNumeric(x, any.missing = FALSE)

  (x - mean(x)) / sd(x)
}

tune_svm <- function(task, resampling.folds = 5, n.evals = 50, measure.prob = "classif.auc") {
  assert_task(task, task_type = "classif")
  assertNumber(resampling.folds, lower = 5)
  assertNumber(n.evals, lower = 30)
  assertString(measure.prob)
  assertSubset(measure.prob, choices = grep("^classif\\..*", measure.prob, value = TRUE))

  # Definisco il learner SVM con probabilità
  learner = lrn("classif.svm", type = "C-classification", predict_type = "prob")

  # Spazio di iperparametri
  param.space <- ps(
    kernel = p_fct(c("radial", "polynomial", "sigmoid")),
    cost = p_dbl(lower = 0.1, upper = 10, logscale = TRUE),
    gamma = p_dbl(lower = 0.001, upper = 1, logscale = TRUE)
  )

  # Tuner (ricerca casuale)
  tuner = tnr("random_search", batch_size = 10)

  # Resampling cross-validation
  resampling = rsmp("cv", folds = resampling.folds)

  # AutoTuner
  set.seed(123)
  at = AutoTuner$new(
    learner = learner,
    resampling = resampling,
    measure = msr(measure.prob),
    search_space = param.space,
    terminator = trm("evals", n_evals = n.evals),
    tuner = tuner
  )

  # Addestramento
  at$train(task)

  # Predizioni
  prediction = at$predict(task)

  # Calcolo probabilità e classi predette
  prob <- prediction$prob
  pred.class <- prediction$response

  # Metriche classiche
  conf.mat <- table(Predicted = pred_class, True = task$truth())
  accuracy <- sum(diag(conf_mat)) / sum(conf_mat)

  # Performance probabilistica
  prob.measure <- prediction$score(msr(measure.prob))

  # Parametri ottimali
  best.params <- at$archive$best()$x_domain[[1]]

  # Output completo
  list(
    model = at,
    prediction = prediction,
    probability = prob,
    predicted_class = pred.class,
    confusion_matrix = conf.mat,
    accuracy = accuracy,
    prob_performance = prob.measure,
    best_params = best.params
  )
}

