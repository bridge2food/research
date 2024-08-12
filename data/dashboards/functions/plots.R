################### Chart functions

# Function to create a vertical bar chart of frequencies of values in a column
v_bar_chart <- function(df, column_name) {
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
    title = "",
    xaxis = list(title = ""),
    yaxis = list(title = "Count")
  )
  
  # Remove the plotly toolbar
  fig <- fig %>% config(displayModeBar = FALSE)
}

# Function to create a pie chart of frequencies of values in a column
pie_chart <- function(df, column_name) {
  # Check if the column exists in the dataframe
  if (!column_name %in% colnames(df)) {
    stop("The column does not exist in the dataframe")
  }
  
  # Create a summary of frequencies of values in the column
  summary_df <- as.data.frame(table(as_factor(df[[column_name]]))) %>%
    rename(Value = Var1)
  
  # Create the pie chart using plotly
  fig <- plot_ly(
    data = summary_df,
    labels = ~Value,
    values = ~Freq,
    type = 'pie'
  )
  
  # Customize the layout
  fig <- fig %>% layout(
    title = ""
  )
  
  # Remove the plotly toolbar
  fig <- fig %>% config(displayModeBar = FALSE)
  
  # Return the figure
  return(fig)
}

# Function to create a pie chart of frequencies of values across columns with a given naming structure, which excludes suffixes "_n"
pie_chart_cols <- function(data, base_col_name) {
  # Use select and starts_with to get columns that match the pattern
  matching_cols <- data %>%
    select(starts_with(base_col_name)) 
  
  # Filter the columns that follow the pattern "base_col_name_number"
  matching_cols <- matching_cols %>%
    select(matches(paste0("^", base_col_name, "_\\d+$")))
  
  if (ncol(matching_cols) == 0) {
    stop("No columns match the given base column name structure.")
  }
  
  # Retrieve the value labels using attr(), but only keep the label names
  factor_labels <- sapply(matching_cols, function(col) {
    val_labels <- attr(col, "labels")
    if (!is.null(val_labels)) {
      names(val_labels)  # Use only the label names
    } else {
      colnames(data)
    }
  }, simplify = TRUE, USE.NAMES = FALSE)
  
  # Summarize the values of the matching columns
  column_sums <- colSums(matching_cols, na.rm = TRUE)
  
  # Set the names of the sums to the corresponding factor labels
  names(column_sums) <- unlist(factor_labels)
  
  # Create a pie chart using Plotly
  pie_chart <- plot_ly(
    labels = names(column_sums),
    values = column_sums,
    type = 'pie'
  ) %>%
    layout(title = "") %>%
    config(displayModeBar = FALSE)
  
  return(pie_chart)
}
