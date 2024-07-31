# Function to get the latest year for a given survey name
latest_y <- function(survey_name, directory) {
  # List all files in the directory
  files <- list.files(directory, pattern = paste0("^", survey_name), full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No files found for the given survey name")
  }
  
  # Extract the year information from the file names
  years <- as.numeric(str_extract(files, "(?<=-)(\\d{4})(?=-Q\\d)"))
  
  # Get the latest year
  latest_year <- max(years, na.rm = TRUE)
  
  return(latest_year)
}

# Function to get the latest quarter for a given survey name
latest_q <- function(survey_name, directory) {
  # List all files in the directory
  files <- list.files(directory, pattern = paste0("^", survey_name), full.names = TRUE)
  
  # Check if there are any files
  if (length(files) == 0) {
    stop("No files found for the given survey name")
  }
  
  # Extract the quarter and year information from the file names
  quarters <- str_extract(files, "Q\\d")
  years <- as.numeric(str_extract(files, "(?<=-)(\\d{4})(?=-Q\\d)"))
  
  # Get the latest year
  latest_year <- latest_y(survey_name, directory)
  
  # Get the latest quarter within the latest year
  latest_quarter <- max(quarters[years == latest_year], na.rm = TRUE)
  
  return(latest_quarter)
}

# Function to get the previous year for a given survey name
previous_y <- function(survey_name, directory) {
  # Get the latest year
  latest_year <- latest_y(survey_name, directory)
  
  # Determine previous year
  prev_year <- latest_year - 1
  
  return(prev_year)
}

# Function to get the previous quarter for a given survey name
previous_q <- function(survey_name, directory) {
  # Get the latest year and quarter
  latest_year <- latest_y(survey_name, directory)
  latest_quarter <- latest_q(survey_name, directory)
  
  # Define quarter order
  quarter_order <- c("Q1", "Q2", "Q3", "Q4")
  
  # Determine previous quarter
  latest_quarter_index <- match(latest_quarter, quarter_order)
  if (latest_quarter_index == 1) {
    prev_quarter <- "Q4"
    prev_year <- latest_year - 1
  } else {
    prev_quarter <- quarter_order[latest_quarter_index - 1]
    prev_year <- latest_year
  }
  
  return(list(year = prev_year, quarter = prev_quarter))
}

# Function to get the data from the latest quarter
latest_data <- function(survey_name, directory) {
  # Get the latest year and quarter
  latest_year <- latest_y(survey_name, directory)
  latest_quarter <- latest_q(survey_name, directory)
  
  # List all files in the directory
  files <- list.files(directory, pattern = paste0("^", survey_name), full.names = TRUE)
  
  # Extract the quarter and year information from the file names
  quarters <- str_extract(files, "Q\\d")
  years <- as.numeric(str_extract(files, "(?<=-)(\\d{4})(?=-Q\\d)"))
  
  # Get the latest quarter file
  latest_file <- files[years == latest_year & quarters == latest_quarter]
  
  # Check if the latest file is found
  if (length(latest_file) == 0) {
    stop("No file found for the latest quarter")
  }
  
  # Read the file into a dataframe
  latest_data <- read_sav(latest_file)
  
  return(latest_data)
}

# Function to get the data from the previous quarter
prev_q_data <- function(survey_name, directory) {
  # Get the previous quarter and its year
  prev <- previous_q(survey_name, directory)
  prev_year <- prev$year
  prev_quarter <- prev$quarter
  
  # List all files in the directory
  files <- list.files(directory, pattern = paste0("^", survey_name), full.names = TRUE)
  
  # Extract the quarter and year information from the file names
  quarters <- str_extract(files, "Q\\d")
  years <- as.numeric(str_extract(files, "(?<=-)(\\d{4})(?=-Q\\d)"))
  
  # Get the previous quarter file
  prev_file <- files[years == prev_year & quarters == prev_quarter]
  
  # Check if the previous file is found
  if (length(prev_file) == 0) {
    stop("No file found for the previous quarter")
  }
  
  # Read the file into a dataframe
  prev_data <- read_sav(prev_file)
  
  return(prev_data)
}

# Function to get the data from the same quarter in the previous year
prev_y_data <- function(survey_name, directory) {
  # Get the latest year and quarter
  latest_year <- latest_y(survey_name, directory)
  latest_quarter <- latest_q(survey_name, directory)
  
  # Get the previous year
  prev_year <- previous_y(survey_name, directory)
  
  # List all files in the directory
  files <- list.files(directory, pattern = paste0("^", survey_name), full.names = TRUE)
  
  # Extract the quarter and year information from the file names
  quarters <- str_extract(files, "Q\\d")
  years <- as.numeric(str_extract(files, "(?<=-)(\\d{4})(?=-Q\\d)"))
  
  # Get the same quarter file in the previous year
  prev_file <- files[years == prev_year & quarters == latest_quarter]
  
  # Check if the previous file is found
  if (length(prev_file) == 0) {
    stop("No file found for the same quarter in the previous year")
  }
  
  # Read the file into a dataframe
  prev_data <- read_sav(prev_file)
  
  return(prev_data)
}

# Function to get the data for a specific quarter and year
get_data <- function(survey_name, directory, quarter, year) {
  # List all files in the directory with .sav extension
  files <- list.files(directory, pattern = paste0("^", survey_name, ".*\\.sav$"), full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No files found for the given survey name")
  }
  
  # Extract the quarter and year information from the file names
  quarters <- str_extract(files, "Q\\d")
  years <- as.numeric(str_extract(files, "(?<=-)(\\d{4})(?=-Q\\d)"))
  
  # Filter out NA years
  valid_indices <- !is.na(years)
  files <- files[valid_indices]
  quarters <- quarters[valid_indices]
  years <- years[valid_indices]
  
  # Get the specific quarter and year file
  specific_file <- files[years == year & quarters == quarter]
  
  # Check if the specific file is found
  if (length(specific_file) == 0) {
    stop("No file found for the specified quarter and year")
  }
  
  # Ensure the file has a .sav extension
  specific_file <- specific_file[file_ext(specific_file) == "sav"]
  
  if (length(specific_file) == 0) {
    stop("The file found is not a .sav file")
  }
  
  # Read the file into a dataframe
  data <- read_sav(specific_file)
  
  return(data)
}

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


