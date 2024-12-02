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

# Function to create HTML snippet for delta values given a dataframe and a column name
# Note, this only works with the dataframes with naming convention created by
# the calc_deltas() function

create_delta_html <- function(data, column, show_na = FALSE) {
  # Ensure curr_period is defined in the environment
  if (!exists("curr_period")) {
    stop("The variable 'curr_period' is not defined in the environment")
  }
  
  # Filter data for the latest period
  latest_data <- data %>% filter(Period == curr_period)
  
  # Construct column names based on the naming conventions
  delta_col <- paste0(column, "_dq")
  percent_delta_col <- paste0(column, "_pdq")
  
  # Check if the required columns exist in the dataframe
  if (!(delta_col %in% colnames(data)) || !(percent_delta_col %in% colnames(data))) {
    stop("Delta columns not found in the dataframe")
  }
  
  # Get the delta and percent delta values for the latest period
  delta_value <- latest_data[[delta_col]]
  percent_delta_value <- latest_data[[percent_delta_col]]
  
  # Check if delta values are numeric and not NA
  if (!is.na(delta_value) && is.numeric(delta_value) && !is.na(percent_delta_value) && is.numeric(percent_delta_value)) {
    # Determine caret icon and color based on delta_value
    icon <- caret(delta_value)
    color_class <- caret_color(delta_value)
    
    # Format the values to two decimal places
    formatted_delta <- sprintf("%.2f", round(delta_value, 2))
    formatted_percent_delta <- sprintf("%.2f%%", round(percent_delta_value, 2))  # Include percent sign
    
    # Create the HTML string
    html_snippet <- sprintf(
      '<i class="bi bi-%s %s"></i> %s (%s)',
      icon,
      color_class,
      formatted_delta,
      formatted_percent_delta
    )
    return(html_snippet)
  } else {
    # If show_na = TRUE, return "NA" with an up caret icon; otherwise, return an empty string
    if (show_na) {
      icon <- caret(1)  # Default to up caret for NA case
      color_class <- caret_color(1)  # Default to success color
      html_snippet <- sprintf(
        '<i class="bi bi-%s %s"></i> NA (NA%%)',
        icon,
        color_class
      )
      return(html_snippet)
    } else {
      return('')
    }
  }
}


# Helper function to wrap text by inserting <br> tags
wrap_text <- function(text, max_char = 15) {
  # Split the text into words
  words <- unlist(strsplit(text, " "))
  
  # Initialize variables
  wrapped_text <- ""
  current_line <- ""
  
  for (word in words) {
    # Check if adding the next word exceeds the max_char limit
    if (nchar(current_line) + nchar(word) + 1 <= max_char) {
      if (current_line == "") {
        current_line <- word
      } else {
        current_line <- paste(current_line, word, sep = " ")
      }
    } else {
      # Add the current line to wrapped_text and start a new line
      if (wrapped_text == "") {
        wrapped_text <- current_line
      } else {
        wrapped_text <- paste(wrapped_text, current_line, sep = "<br>")
      }
      current_line <- word
    }
  }
  
  # Add any remaining text
  if (current_line != "") {
    if (wrapped_text == "") {
      wrapped_text <- current_line
    } else {
      wrapped_text <- paste(wrapped_text, current_line, sep = "<br>")
    }
  }
  
  return(wrapped_text)
}


get_curr_nb <- function(column_name) {
  # Check if the column exists in the dataframe
  if (!column_name %in% colnames(netbalances)) {
    stop("The specified column does not exist in the netbalances dataframe")
  }
  
  # Check if the curr_period variable exists in the environment
  if (!exists("curr_period")) {
    stop("The variable 'curr_period' is not defined in the environment")
  }
  
  # Retrieve the value for the current period and round to 2 decimal places
  result <- netbalances %>%
    filter(Period == curr_period) %>%
    pull(column_name) %>%
    round(2)
  
  return(result)
}

# Function to calculate quarterly and annual changes and percent changes
# of each column in a dataframe, creating variables with the following
# naming convention:
# In dq, pdq, dy, pdy, d = delta, p = percent, q = quarter, y = year.

calc_deltas <- function(dataframe) {
  # Ensure the Period column is sorted
  dataframe <- dataframe %>% arrange(Period)
  
  # Extract year and quarter from Period
  dataframe <- dataframe %>%
    mutate(
      Year = as.numeric(str_extract(Period, "^\\d{4}")),
      Quarter = str_extract(Period, "Q\\d")
    )
  
  # Get the column names excluding the first column (Period) and the temporary Year and Quarter columns
  cols <- colnames(dataframe)[-c(1, ncol(dataframe)-1, ncol(dataframe))]
  
  # Calculate quarterly and yearly deltas and percent deltas
  for (col in cols) {
    delta_q_col <- paste0(col, "_dq")
    percent_delta_q_col <- paste0(col, "_pdq")
    delta_y_col <- paste0(col, "_dy")
    percent_delta_y_col <- paste0(col, "_pdy")
    
    dataframe <- dataframe %>%
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
  dataframe <- dataframe %>% select(all_of(new_order), everything())
  
  # Remove the temporary Year and Quarter columns
  dataframe <- dataframe %>% select(-Year, -Quarter)
  
  return(dataframe)
}

# Function to generate html displaying a variable's net balance and delta figures
nb_html <- function(variable_name) {
  # Dynamically retrieve variable values
  nb <- get(paste0(variable_name, "_nb"))
  nb_delta_html <- get(paste0(variable_name, "_nb_delta_html"))
  text <- get(paste0(variable_name, "_text"))
  
  # Generate HTML code
  html_code <- paste0(
    '<div class="mt-5 text-center">\n',
    '<span class="display-1">', nb, '</span>\n\n',
    nb_delta_html, '\n\n',
    '<span class="text-muted small m-4 p-4">', text, '</span>\n',
    '</div>'
  )
  
  return(html_code)
}

# Function to generate username:password combos for basic-auth
generate_credentials <- function(dataframe) {
  # Load required library
  library(stringi)
  
  # Check if the Company column exists
  if (!"Company" %in% colnames(dataframe)) {
    stop("The dataframe must have a 'Company' column.")
  }
  
  # Create a function to generate usernames
  generate_username <- function(company) {
    username <- tolower(gsub("[^a-zA-Z0-9]", "", company)) # Remove non-alphanumeric characters
    substring(username, 1, 20) # Limit to 20 characters
  }
  
  # Improved password generation function
  generate_password <- function(length = 20) {
    if(length < 4) {
      stop("Password length should be at least 4 to include all character types.")
    }
    
    # Define character sets
    uppercase_letters <- LETTERS
    lowercase_letters <- letters
    digits <- 0:9
    special_chars <- c("!", "@", "#", "$", "%", "^", "&", "*")
    
    # Ensure at least one character from each set
    password_chars <- c(
      sample(uppercase_letters, 1),
      sample(lowercase_letters, 1),
      sample(digits, 1),
      sample(special_chars, 1)
    )
    
    # Fill the remaining length with a random mix of all characters
    all_chars <- c(uppercase_letters, lowercase_letters, digits, special_chars)
    if(length > 4) {
      password_chars <- c(
        password_chars,
        sample(all_chars, length - 4, replace = TRUE)
      )
    }
    
    # Shuffle the characters to prevent predictable sequences
    password <- paste(sample(password_chars), collapse = "")
    
    return(password)
  }
  
  # Generate usernames and passwords
  dataframe$username <- sapply(dataframe$Company, generate_username)
  dataframe$password <- replicate(nrow(dataframe), generate_password(20))
  
  # Combine usernames and passwords with a colon
  credentials <- paste(dataframe$username, dataframe$password, sep = ":")
  
  # Join all credentials with a space
  single_line_output <- paste(credentials, collapse = " ")
  
  # Return the single line output
  return(single_line_output)
}


