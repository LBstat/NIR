
# Function that compares the imputed values with the original distributions
graphical.comparation <- function(imputed_data, imputed_pos, x, y, x_ignore = FALSE, y_ignore = FALSE, dataset_id = NULL, 
  out_dir = "plot/", save = TRUE) {

  assertDataFrame(imputed_data, types = "numeric")
  assertInteger(imputed_pos, lower = 1, any.missing = FALSE)
  assertString(x)
  assertString(y)
  assertSubset(x, colnames(imputed_data))
  assertSubset(y, colnames(imputed_data))

  imputed_data$type <- "real"
  imputed_data$type[imputed_pos] <- "imputed"

  p1 <- ggplot(imputed_data , aes(x = .data[[x]], y = .data[[y]], color = type)) +
    geom_point(alpha = 0.8, size = 2.5) +
    scale_color_manual(values = c("real" = "#003366", "imputed" = "#9B1B30")) +
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

  if (!x_ignore) {
  p2 <- ggplot(imputed_data, aes(x = .data[[x]], fill = type)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("real" = "#003366", "imputed" = "#9B1B30")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank())
  }

  if (!y_ignore) {
  p3 <- ggplot(imputed_data, aes(x = .data[[y]], fill = type)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("real" = "#003366", "imputed" = "#9B1B30")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank())
  }

  if (x_ignore) {
    p <- p1 / p3
  }
  if (y_ignore) {
    p <- p1 / p2
  }
  if (!x_ignore & !y_ignore) {
    p <- p1 / (p2 | p3)
  }

  if (save) {
    if (is.null(dataset_id)) dataset_id <- format(Sys.time(), "%H%M%S")
    filename <- paste0("imputation_", dataset_id, "_", y, ".png")
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

# Function that compares density distributions. DataFrame must be in long format.
density.comparation <- function(data, out_dir = "plot/", save = TRUE) {

  assertDataFrame(data)

  p <- ggplot(data, aes(x = valore, color = variabile, fill = variabile)) +
    geom_density(alpha = 0.3) +
    labs(title = "Variable density",
         x = "value",
         y = "density") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank())

  pattern <- "densities([0-9]+)\\.png"

  existing.files <- list.files("plot/", pattern = pattern)
  if (save) {
    if (length(existing.files) == 0) {
      ggsave(
        filename = file.path(out_dir, "densities1.png"),
        plot = p,
        width = 12,
        height = 6,
        dpi = 300
      )
    } else {
      value <- vapply(existing.files, function(x) {
        as.numeric(regmatches(x, regexec(pattern, x, perl = TRUE))[[1]][[2]])
      }, numeric(1))
      ggsave(
        filename = file.path(out_dir, paste0("densities", max(value) + 1, ".png")),
        plot = p,
        width = 12,
        height = 6,
        dpi = 300
      )
    }
  }
  invisible(p)
}
