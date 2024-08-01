

#####################

# Helper function to list files for a given survey name
list_survey_files <- function(survey_name) {
  list.files(dir_path, pattern = paste0("^", survey_name, "-\\d{4}-Q\\d\\.sav$"), full.names = TRUE)
}

# Helper function to extract year and quarter from file name(s)
extract_time_info <- function(files) {
  time_info <- data.frame(
    file = files,
    year = as.numeric(str_extract(files, "(?<=-)(\\d{4})(?=-Q\\d)")),
    quarter = str_extract(files, "Q\\d")
  )
  time_info$period <- paste0(time_info$year, "-", time_info$quarter)
  return(time_info)
}

# Function to get the latest period (year and quarter) for a given survey name
latest_period <- function(survey_name) {
  files <- list_survey_files(survey_name)
  if (length(files) == 0) stop("No files found for the given survey name")
  time_info <- extract_time_info(files)
  latest_year <- max(time_info$year, na.rm = TRUE)
  latest_quarter <- max(time_info$quarter[time_info$year == latest_year], na.rm = TRUE)
  list(year = latest_year, quarter = latest_quarter)
}

# Function to get the previous period (year and quarter) for a given survey name
previous_period <- function(survey_name) {
  period <- latest_period(survey_name)
  latest_year <- period$year
  latest_quarter <- period$quarter
  quarter_order <- c("Q1", "Q2", "Q3", "Q4")
  latest_quarter_index <- match(latest_quarter, quarter_order)
  if (latest_quarter_index == 1) {
    prev_year <- latest_year - 1
    prev_quarter <- "Q4"
  } else {
    prev_year <- latest_year
    prev_quarter <- quarter_order[latest_quarter_index - 1]
  }
  list(year = prev_year, quarter = prev_quarter)
}

# Function to get data for a specified year and quarter
get_data <- function(survey_name, year, quarter) {
  files <- list_survey_files(survey_name)
  if (length(files) == 0) stop("No files found for the given survey name")
  time_info <- extract_time_info(files)
  specific_file <- time_info$file[time_info$year == year & time_info$quarter == quarter]
  if (length(specific_file) == 0) stop("No file found for the specified quarter and year")
  read_sav(specific_file)
}

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

# Function to calculate indicators
calculate_indicators <- function(data, period) {
  # Check if required columns exist
  required_cols <- c("Q2.3", "Q2.4", "Q2.5")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    warning(paste("Missing columns for indicators calculation:", paste(missing_cols, collapse = ", ")))
    return(data.frame(Period = period, ic = NA))
  }
  
  # Industry confidence indicator
  orders <- data$Q2.3
  stocks <- data$Q2.4
  prod_exp <- data$Q2.5
  ic <- mean(orders, na.rm = TRUE) - mean(stocks, na.rm = TRUE) + mean(prod_exp, na.rm = TRUE)
  
  indicators <- data.frame(
    Period = period,
    ic = ic
  )
  return(indicators)
}

# General function to process data for a given survey and function (agg or indicators)
process_data <- function(survey_name, dir_path, calc_func, file_suffix) {
  file_path <- paste0(dir_path, "/", tolower(survey_name), "-", file_suffix, ".rds")
  
  df <- data.frame()
  
  # List and process all files
  files <- list_survey_files(survey_name)
  
  for (file in files) {
    time_info <- extract_time_info(file)
    period <- time_info$period
    data <- read_sav(file)
    result <- calc_func(data, period)
    df <- bind_rows(df, result)
  }
  
  # Check the data frame before arranging to ensure Period exists
  if (!"Period" %in% colnames(df)) {
    stop("Period column not found in the data frame")
  }
  
  # Remove duplicates and sort
  df <- df %>% distinct() %>% arrange(Period)
  
  # Save the updated data
  write_rds(df, file_path)
}

# Main processing functions
process_agg_data <- function(survey_name, dir_path) {
  process_data(survey_name, dir_path, calculate_agg, "agg")
}

process_indicators_data <- function(survey_name, dir_path) {
  process_data(survey_name, dir_path, calculate_indicators, "indicators")
}

# Specific period data retrieval functions
latest_data <- function(survey_name) {
  period <- latest_period(survey_name)
  get_data(survey_name, period$year, period$quarter)
}

prev_q_data <- function(survey_name) {
  period <- previous_period(survey_name)
  get_data(survey_name, period$year, period$quarter)
}

prev_y_data <- function(survey_name) {
  period <- latest_period(survey_name)
  prev_year <- period$year - 1
  get_data(survey_name, prev_year, period$quarter)
}

# Function to get data for a specified period
specified_period_data <- function(survey_name, period) {
  parts <- strsplit(period, "-Q")[[1]]
  year <- as.numeric(parts[1])
  quarter <- paste0("Q", parts[2])
  get_data(survey_name, year, quarter)
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
        !!percent_delta_q_col := (ifelse(!is.na(lag(.data[[col]])), ((.data[[col]] - lag(.data[[col]])) / lag(.data[[col]])) * 100, NA)),
        !!delta_y_col := .data[[col]] - lag(.data[[col]], n = 4),
        !!percent_delta_y_col := (ifelse(!is.na(lag(.data[[col]], n = 4)), ((.data[[col]] - lag(.data[[col]], n = 4)) / lag(.data[[col]], n = 4)) * 100, NA))
      )
  }
  
  # Reorder columns to place delta and percent delta columns immediately after the original columns
  new_order <- c("Period", cols)
  for (col in cols) {
    delta_q_col <- paste0(col, "_dq")
    percent_delta_q_col <- paste0(col, "_pdq")
    delta_y_col <- paste0(col, "_dy")
    percent_delta_y_col <- paste0(col, "_pdy")
    new_order <- append(new_order, c(delta_q_col, percent_delta_q_col, delta_y_col, percent_delta_y_col))
  }
  
  # Select columns in the new order
  indicators_df <- indicators_df %>% select(all_of(new_order), everything())
  
  # Remove the temporary Year and Quarter columns
  indicators_df <- indicators_df %>% select(-Year, -Quarter)
  
  # Save the updated indicators data
  write_rds(indicators_df, delta_indicators_file_path)
}

###################

# Function to create a bar chart of frequencies of values in a column
v_bar_chart <- function(df, column_name, title) {
  # Check if the column exists in the dataframe
  if (!column_name %in% colnames(df)) {
    stop("The column does not exist in the dataframe")
  }
  
  # Create a summary of frequencies of values in the column
  summary_df <- as.data.frame(table(as_factor(df[[column_name]]))) %>%
    rename(Value = Var1)
  
  # Create the bar chart using plotly
  fig <- plot_ly(
    data = summary_df,
    x = ~Value,
    y = ~Freq,
    type = 'bar'
  )
  
  # Customize the layout
  fig <- fig %>% layout(
    title = title,
    xaxis = list(title = ""),
    yaxis = list(title = "Count")
  )
  
  # Remove the plotly toolbar
  fig <- fig %>% config(displayModeBar = FALSE)
}


