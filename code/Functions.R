
# Auto-increment filename function
save_plot <- function(plot_obj, base_name, out_dir = "plot/") {
  pattern <- paste0(base_name, "([0-9]+)\\.png")
  existing_files <- list.files(out_dir, pattern = pattern)
  
  if (length(existing_files) == 0) {
    index <- 1
  } else {
    value <- vapply(existing_files, function(x) {
      as.numeric(regmatches(x, regexec(pattern, x, perl = TRUE))[[1]][2])
    }, numeric(1))
    index <- max(value) + 1
  }
  
  ggsave(
    filename = file.path(out_dir, paste0(base_name, index, ".png")),
    plot = plot_obj,
    width = 12,
    height = 6,
    dpi = 300
  )
}

# Function that computes Kvalue index for imputed data
calculate_kvalue <- function(df) {

  # Assertions
  assertDataFrame(df)

  num <- df$Inosina + df$ipoxantina
  den <- df$ATP + df$ADP + df$AMP + df$IMP + df$Inosina + df$ipoxantina
  num / den
}

# Function that performs linear smoothing. Derivatives obtained by setting m != 0
smoothing <- function(data, window = 11, poly = 2, m = 0) {

  # Assertions
  assertDataFrame(data, types = "numeric")
  assertNumber(window)
  assertNumber(poly, lower = 1)
  assertNumber(m, lower = 0)

  if (window %% 2 != 1) stop("window must be odd")

  # Store names
  orig_names <- colnames(data)
  
  # Perform smoothing
  smoothed <- t(apply(data, 1, function(x) sgolayfilt(x, p = poly, n = window, m = m)))

  # Convert back to data frame and RE-ASSIGN NAMES
  smoothed_df <- as.data.frame(smoothed)
  colnames(smoothed_df) <- orig_names
  smoothed_df
}

# Function to normalize single numeric vectors
normalization <- function(x) {

  # Assertions
  assertNumeric(x, any.missing = FALSE)

  (x - mean(x)) / sd(x)
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

# Function that extracts data from lapply list results
extract_metrics <- function(res_item, index) {

  # Assertions
  assertList(res_item, null.ok = FALSE)
  assertNumber(index)

  current_pcs <- paste(combinations_list[[index]], collapse = ", ")

  df <- data.frame(
    rank = index,
    pcs = current_pcs,
    acc = res_item$performance["classif.acc"],
    auc = res_item$performance["classif.auc"],
    sens = res_item$performance["classif.sensitivity"],
    spec = res_item$performance["classif.specificity"],
    stringsAsFactors = FALSE
  )
}
