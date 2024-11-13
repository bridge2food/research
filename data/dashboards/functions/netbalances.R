# Function to calculate net balances
calculate_nb <- function(data, period) {
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) == 0) {
    stop("No numeric columns found in the data")
  }
  data_numeric <- data[, numeric_cols, drop = FALSE]
  means <- colMeans(data_numeric, na.rm = TRUE)
  net_balances <- as.data.frame(t(means * 100), stringsAsFactors = FALSE)
  net_balances$Period <- period
  
  # Ensure Period is the first column and only select existing columns
  existing_columns <- intersect(c("Period", names(means)), colnames(net_balances))
  net_balances <- net_balances[, existing_columns, drop = FALSE]
  return(net_balances)
}

