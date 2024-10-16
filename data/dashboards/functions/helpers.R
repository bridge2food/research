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

# Function to create HTML snippet for a delta indicator
create_delta_html <- function(delta_value, is_percent = FALSE) {
  # Check if delta_value is numeric and not NA
  if (!is.na(delta_value) && is.numeric(delta_value)) {
    # Determine caret icon and color
    icon <- caret(delta_value)
    color_class <- caret_color(delta_value)
    
    # Format delta value to two decimal places
    formatted_delta <- sprintf("%.2f", round(delta_value, 2))
    
    # Append percent sign if needed
    if (is_percent) {
      formatted_delta <- paste0(formatted_delta, "%")
    }
    
    # Create HTML string
    html_snippet <- sprintf(
      '<i class="bi bi-%s %s"></i> %s',
      icon,
      color_class,
      formatted_delta
    )
    return(html_snippet)
  } else {
    # Return empty string if delta_value is NA or not numeric
    return('')
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
