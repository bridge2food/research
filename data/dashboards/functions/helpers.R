library(lubridate)

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

# Helper function to convert YYYY-QN period data to date format YYYY-MM
convert_period_to_date <- function(period) {
  year <- as.numeric(substring(period, 1, 4))
  quarter <- substring(period, 6, 7)
  month <- switch(quarter,
                  "Q1" = "01",
                  "Q2" = "04",
                  "Q3" = "07",
                  "Q4" = "10")
  return(as.Date(paste(year, month, "01", sep = "-")))
}

# Helper function to convert Mon-YY to date
convert_mon_yy_to_date <- function(period) {
  return(as.Date(paste0("01-", period), format = "%d-%b-%y"))
}

# Helper function to display an up or down caret icon
caret <- function(value) {
  if (value >= 0) {
    return("caret-up")
  } else if (value < 0) {
    return("caret-down")
  }
}

# Helper function to set color for caret icon
caret_color <- function(value) {
  if (value >= 0) {
    return("text-success")
  } else if (value < 0) {
    return("text-danger")
  }
}

