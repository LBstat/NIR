
# Function that performs Shapiro Wilk Normality Test on each numerical column of a given
# data frame. The histogram of the data and the theoretical normality density curve are plotted.
multivariable.normality.test <- function(data, out_dir = "plot/", save = TRUE, print = FALSE) {
  assertDataFrame(data, types = "numeric")

  for (i in seq_len(ncol(data))) {
    var.name <- colnames(data)[i]
    vec <- data[, i]

    p_val <- shapiro.test(vec)$p.value

    if (print) {
      if (p_val <= 0.05) {
        cat("H0 rejected for variable", var.name, "\n")
        } else {
          cat("H0 not rejected for variable", var.name, "\n")
        }
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
           y = "density") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
        panel.grid.minor = element_blank())

    if (save) {
      filename <- paste0("normality_check_col", i, ".png")
      ggsave(
        filename = file.path(out_dir, filename),
        plot = p,
        width = 12,
        height = 6,
        dpi = 300
      )
    }
    invisible(p)
  }
}

# Function that performs linear smoothing. Derivatives obtained by setting m != 0
smoothing <- function(data, window = 11, poly = 2, m = 0) {
  assertDataFrame(data, types = "numeric")
  assertNumber(window)
  assertNumber(poly)
  assertNumber(m, lower = 0)

  if (window %% 2 != 1) stop("window must be odd")

  smoothed <- t(apply(data, MARGIN = 1, function(x) sgolayfilt(x, p = poly, n = window, m = m)))
  as.data.frame(smoothed)
}

# Function to normalize single numeric vectors
normalization <- function(x) {
  assertNumeric(x, any.missing = FALSE)

  (x - mean(x)) / sd(x)
}