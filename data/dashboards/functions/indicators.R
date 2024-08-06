# Function to calculate Industry Confidence Indicator
calculate_industry_confidence <- function(data) {
  orders <- data$po.orders_curr.q
  stocks <- data$po.stocks_curr.q
  prod_exp <- data$po.prod_next_3.q
  ic <- (sum(orders, na.rm = TRUE) - sum(stocks, na.rm = TRUE) + sum(prod_exp, na.rm = TRUE)) / 3
  return(ic)
}

# Function for Industry Uncertainty Indicator
calculate_industry_uncertainty <- function(data) {
  uncertainty <- data$po.uncertainty.q
  iu <- sum(uncertainty, na.rm = T) # Placeholder value, replace with actual calculation
  return(iu)
}

# Function for Employment Outlook Indicator
calculate_employment_outlook <- function(data) {
  emp_exp <- data$lab.emp_next_3.q
  eo <- sum(emp_exp, na.rm = T) # Placeholder value, replace with actual calculation
  return(eo)
}

# Top-level function to calculate all indicators
calculate_indicators <- function(data, period) {
  
  # Calculate each indicator
  ic <- calculate_industry_confidence(data)
  iu <- calculate_industry_uncertainty(data)
  eo <- calculate_employment_outlook(data)
  
  # Combine indicators into a data frame
  indicators <- data.frame(
    Period = period,
    ic = ic,
    iu = iu,
    eo = eo
  )
  return(indicators)
}



# Function to calculate quarterly and annual changes and percent changes of indicators
# In dq, pdq, dy, pdy, d = delta, p = percent, q = quarter, y = year.
delta_indicators <- function(survey_name, dir_path) {
  # Define file paths
  indicators_file_path <- paste0(dir_path, "/", tolower(survey_name), "-indicators.rds")
  delta_indicators_file_path <- paste0(dir_path, "/", tolower(survey_name), "-indicators-delta.rds")
  
  # Check if the indicators file exists
  if (!file.exists(indicators_file_path)) {
    stop("Indicators file not found")
  }
  
  # Read the indicators data
  indicators_df <- read_rds(indicators_file_path)
  
  # Ensure the Period column is sorted
  indicators_df <- indicators_df %>% arrange(Period)
  
  # Extract year and quarter from Period
  indicators_df <- indicators_df %>%
    mutate(
      Year = as.numeric(str_extract(Period, "^\\d{4}")),
      Quarter = str_extract(Period, "Q\\d")
    )
  
  # Get the column names excluding the first column (Period) and the temporary Year and Quarter columns
  cols <- colnames(indicators_df)[-c(1, ncol(indicators_df)-1, ncol(indicators_df))]
  
  # Calculate quarterly and yearly deltas and percent deltas
  for (col in cols) {
    delta_q_col <- paste0(col, "_dq")
    percent_delta_q_col <- paste0(col, "_pdq")
    delta_y_col <- paste0(col, "_dy")
    percent_delta_y_col <- paste0(col, "_pdy")
    
    indicators_df <- indicators_df %>%
      mutate(
        !!delta_q_col := .data[[col]] - lag(.data[[col]]),
        !!percent_delta_q_col := (ifelse(!is.na(lag(.data[[col]])), ((.data[[col]] - lag(.data[[col]])) / abs(lag(.data[[col]]))) * 100, NA)),
        !!delta_y_col := .data[[col]] - lag(.data[[col]], n = 4),
        !!percent_delta_y_col := (ifelse(!is.na(lag(.data[[col]], n = 4)), ((.data[[col]] - lag(.data[[col]], n = 4)) / abs(lag(.data[[col]], n = 4))) * 100, NA))
      )
  }
  
  # Reorder columns to place delta and percent delta columns immediately after the original columns
  new_order <- c("Period")
  for (col in cols) {
    delta_q_col <- paste0(col, "_dq")
    percent_delta_q_col <- paste0(col, "_pdq")
    delta_y_col <- paste0(col, "_dy")
    percent_delta_y_col <- paste0(col, "_pdy")
    new_order <- append(new_order, c(col, delta_q_col, percent_delta_q_col, delta_y_col, percent_delta_y_col))
  }
  
  # Select columns in the new order
  indicators_df <- indicators_df %>% select(all_of(new_order), everything())
  
  # Remove the temporary Year and Quarter columns
  indicators_df <- indicators_df %>% select(-Year, -Quarter)
  
  # Save the updated indicators data
  write_rds(indicators_df, delta_indicators_file_path)
}
