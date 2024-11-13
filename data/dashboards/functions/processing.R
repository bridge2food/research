#################### Processing functions

# General function to process data for a given survey and function (agg or indicators)
process_data <- function(survey_name, dir_path, calc_func) {
  df <- data.frame()
  
  # List and process all files
  files <- list_survey_files(survey_name)
  
  for (file in files) {
    time_info <- extract_time_info(file)
    period <- time_info$period
    data <- read_sav(file) %>%
      filter(Finished == 1) # Only include completed surveys
    result <- calc_func(data, period)
    df <- bind_rows(df, result)
  }
  
  # Check the data frame before arranging to ensure Period exists
  if (!"Period" %in% colnames(df)) {
    stop("Period column not found in the data frame")
  }
  
  # Remove duplicates and sort
  df <- df %>% distinct() %>% arrange(Period)
  
  return(df)
}

# Main processing functions
process_nb_data <- function(survey_name, dir_path) {
  process_data(survey_name, dir_path, calculate_nb)
}

process_indicators_data <- function(survey_name, dir_path) {
  process_data(survey_name, dir_path, calculate_indicators)
}
