##################### Helper functions

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



