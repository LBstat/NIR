
# Function that compares the imputed values with the original distributions
graphical_comparation <- function(imputed_data, imputed_pos, x, y, x_ignore = FALSE, y_ignore = FALSE, dataset_id = NULL, 
  out_dir = "plot/", save = TRUE) {

  # Assertions
  assertDataFrame(imputed_data, types = "numeric")
  assertInteger(imputed_pos, lower = 1, any.missing = FALSE)
  assertString(x)
  assertString(y)
  assertSubset(x, colnames(imputed_data))
  assertSubset(y, colnames(imputed_data))
  assertString(out_dir)
  assertFlag(save)

  imputed_data$type <- "real"
  imputed_data$type[imputed_pos] <- "imputed"

  p1 <- ggplot(imputed_data , aes(x = .data[[x]], y = .data[[y]], color = type)) +
    geom_point(alpha = 0.8, size = 2.5) +
    scale_color_manual(values = c("real" = "#003366", "imputed" = "#9B1B30")) +
    geom_smooth(method = "gam", se = TRUE, color = "black") +
    labs(title = paste0("Imputed dataset ", dataset_id),
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
    filename <- paste0("imputation_", dataset_id, "_", paste0(strsplit(y, split = " ")[[1]], collapse = ""), ".png")
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
density_comparation <- function(data, out_dir = "plot/", save = TRUE) {

  # Assertions
  assertDataFrame(data)
  assertString(out_dir)
  assertFlag(save)

  p <- ggplot(data, aes(x = valore, color = variabile, fill = variabile)) +
    geom_density(alpha = 0.3) +
    labs(title = "Variable density",
         x = "Value",
         y = "Density") +
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

plot_spectral_comparison <- function(raw_data, smoothed_data, n_samples = 6, out_dir = "plot/", save = TRUE) {

  # Assertions
  assertDataFrame(raw_data)
  assertDataFrame(smoothed_data)
  assertString(out_dir)
  assertFlag(save)

  # Random selecting rows
  set.seed(123)
  sample_ids <- sample(seq_len(nrow(raw_data)), n_samples)
  
  # Add an ID column and pivot both to long format
  prepare_long <- function(df, type_label) {
    df |> mutate(sample_id = as.factor(row_number())) |>
      dplyr::filter(sample_id %in% sample_ids) |>
      pivot_longer(cols = -sample_id, names_to = "wavelength", values_to = "intensity") |>
      mutate(wavelength = as.numeric(str_extract(wavelength, pattern = "[0-9]+")), type = type_label)
  }

  df_raw <- prepare_long(raw_data, "raw")
  df_smooth <- prepare_long(smoothed_data, "smoothed")
  
  # Combine datasets
  df_plot <- bind_rows(df_raw, df_smooth)
  
  # Create the plot
  p <- ggplot(df_plot, aes(x = wavelength, y = intensity, color = type, alpha = type)) +
    geom_line(aes(group = interaction(sample_id, type)), size = 0.8) +
    facet_wrap(~sample_id, scales = "free_y") +
    scale_alpha_manual(values = c("raw" = 0.4, "smoothed" = 1)) +
    scale_color_manual(values = c("raw" = "#003366", "smoothed" = "#9B1B30")) +
    labs(
      title = "Spectral Smoothing/SNV Comparison",
      subtitle = paste("Showing", n_samples, "randomly selected spectra"),
      x = "Wavelength / wavenumber",
      y = "Absorbance / intensity",
      color = "data state",
      alpha = "data state"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank())

  if (save) {
    ggsave(
      filename = file.path(out_dir, "spectral_smoothing_comparison.png"),
      plot = p,
      width = 12,
      height = 6,
      dpi = 300
    )
  }

  invisible(p)
}

pca_representation_facet <- function(pca.res, class, j, out_dir = "plot/", save = TRUE) {

  # Assertions
  assertClass(pca.res, "prcomp")
  assertFactor(class, any.missing = FALSE)
  assertNumber(j, lower = 1)
  assertString(out_dir)
  assertFlag(save)

  pca.df <- as.data.frame(pca.res$x[, 1:(2*j)])
  pca.df$class <- class
  pca.df$id <- seq_len(nrow(pca.df))

  var_exp <- summary(pca.res)$importance[2, ]

  data_list <- lapply(seq_len(j), function(i) {
    tibble(
      PCx = pca.df[[paste0("PC", 2*i - 1)]],
      PCy = pca.df[[paste0("PC", 2*i)]],
      class = class,
      pair = paste0("PC", 2*i - 1, " vs PC", 2*i),
      xlab = paste0("PC", 2*i - 1, " (", round(var_exp[2*i - 1]*100,1), "%)"),
      ylab = paste0("PC", 2*i, " (", round(var_exp[2*i]*100,1), "%)")
    )
  })

  long.df <- bind_rows(data_list)
  long.df$pair <- factor(long.df$pair, levels = unique(long.df$pair))

  p <- ggplot(long.df, aes(PCx, PCy, colour = class)) +
    geom_point(alpha = 0.8, size = 2.2) +
    facet_wrap(~ pair, scales = "free") +
    scale_color_manual(values = c("good/acceptable" = "#003366", "impending spoilage/spoiled" = "#9B1B30")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank())

  if (save) {
    ggsave(
      filename = file.path(out_dir, "pca_scatterplot.png"),
      plot = p,
      width = 12,
      height = 6,
      dpi = 300
    )
  }

  invisible(p)
}
