# Function to calculate aggregated data
calculate_agg <- function(data, period) {
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) == 0) {
    stop("No numeric columns found in the data")
  }
  data_numeric <- data[, numeric_cols, drop = FALSE]
  means <- colMeans(data_numeric, na.rm = TRUE)
  agg <- as.data.frame(t(means), stringsAsFactors = FALSE)
  agg$Period <- period
  
  # Ensure Period is the first column and only select existing columns
  existing_columns <- intersect(c("Period", names(means)), colnames(agg))
  agg <- agg[, existing_columns, drop = FALSE]
  return(agg)
}
