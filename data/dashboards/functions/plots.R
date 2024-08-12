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
pie_chart <- function(df, column_name, order = "descending") {
  # Check if the column exists in the dataframe
  if (!column_name %in% colnames(df)) {
    stop("The column does not exist in the dataframe.")
  }
  
  # Ensure the column is treated as a factor
  df[[column_name]] <- as_factor(df[[column_name]])
  
  # Determine the correct order of factor levels
  if (order == "descending") {
    ordered_levels <- sort(unique(df[[column_name]]), decreasing = TRUE)
  } else if (order == "ascending") {
    ordered_levels <- sort(unique(df[[column_name]]), decreasing = FALSE)
  }
  
  # Apply the ordered levels to the factor
  df[[column_name]] <- factor(df[[column_name]], levels = ordered_levels)
  
  # Create a summary of frequencies of values in the column, preserving factor order
  summary_df <- df %>%
    group_by(Value = df[[column_name]]) %>%
    summarize(Freq = n()) %>%
    ungroup()
  
  # Ensure the summary dataframe maintains the correct factor level order
  summary_df$Value <- factor(summary_df$Value, levels = ordered_levels)
  
  # Create the pie chart using plotly
  fig <- plot_ly(
    data = summary_df,
    labels = ~Value,
    values = ~Freq,
    type = 'pie',
    sort = FALSE  # Prevent Plotly from re-sorting the slices
  )
  
  # Customize the layout to ensure the legend order matches the factor levels
  fig <- fig %>% layout(
    title = "",
    legend = list(traceorder = "normal")  # Keep the legend order as defined by the factor levels
  )
  
  # Remove the plotly toolbar for a cleaner presentation
  fig <- fig %>% config(displayModeBar = FALSE)
  
  return(fig)
}

# Function to create a pie chart of frequencies of values across columns with a given naming structure
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
  
  # Retrieve the value labels using attr(), and ensure they are in the correct order
  factor_labels <- sapply(matching_cols, function(col) {
    val_labels <- attr(col, "labels")
    if (!is.null(val_labels)) {
      val_labels <- val_labels[order(names(val_labels))]  # Ensure labels are ordered correctly
      return(names(val_labels))
    } else {
      return(colnames(data))
    }
  }, simplify = TRUE, USE.NAMES = FALSE)
  
  # Summarize the values of the matching columns
  column_sums <- colSums(matching_cols, na.rm = TRUE)
  
  # Set the names of the sums to the corresponding factor labels
  names(column_sums) <- unlist(factor_labels)
  
  # Ensure the order of factor labels is maintained
  ordered_factor_labels <- unique(unlist(factor_labels))
  
  # Create a pie chart using Plotly
  pie_chart <- plot_ly(
    labels = ordered_factor_labels,
    values = column_sums[ordered_factor_labels],  # Ensure the values follow the correct order
    type = 'pie',
    sort = FALSE  # Prevent Plotly from sorting the slices
  ) %>%
    layout(
      title = "",
      legend = list(traceorder = "normal")  # Keep the legend order as defined by the factor levels
    ) %>%
    config(displayModeBar = FALSE)
  
  return(pie_chart)
}

# Function to create a pie chart of frequencies of values across columns with a given naming structure, which excludes suffixes "_n"
# Also cleans variable labels to only keep text after "- "
pie_chart_cols_pct <- function(data, base_col_name) {
  # Use select and starts_with to get columns that match the pattern
  matching_cols <- data %>%
    select(starts_with(base_col_name)) 
  
  # Filter the columns that follow the pattern "base_col_name_number"
  matching_cols <- matching_cols %>%
    select(matches(paste0("^", base_col_name, "_\\d+$")))
  
  if (ncol(matching_cols) == 0) {
    stop("No columns match the given base column name structure.")
  }
  
  # Retrieve the variable labels for each column and remove text before "- "
  variable_labels <- sapply(matching_cols, function(col) {
    var_label <- attr(col, "label")
    if (!is.null(var_label)) {
      cleaned_label <- str_remove(var_label, ".*- ")  # Remove text before "- "
      return(cleaned_label)
    } else {
      return(colnames(data))
    }
  }, simplify = TRUE, USE.NAMES = FALSE)
  
  # Summarize the values of the matching columns
  column_means <- colMeans(matching_cols, na.rm = TRUE)
  
  # Set the names of the means to the corresponding cleaned variable labels
  names(column_means) <- unlist(variable_labels)
  
  # Create a pie chart using Plotly
  pie_chart <- plot_ly(
    labels = names(column_means),
    values = column_means,
    type = 'pie'
  ) %>%
    layout(title = "") %>%
    config(displayModeBar = FALSE)
  
  return(pie_chart)
}
