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