
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