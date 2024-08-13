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
  
  # Calculate percentages
  summary_df <- summary_df %>%
    mutate(Percentage = Freq / sum(Freq) * 100)
  
  # Create the bar chart using plotly with custom hover tooltips
  fig <- plot_ly(
    data = summary_df,
    x = ~Value,
    y = ~Freq,
    type = 'bar',
    hovertext = ~paste0(Value, '<br>', Freq, '<br>', round(Percentage, 2), '%'),
    hoverinfo = 'hovertext',
    hovertemplate = '%{hovertext}<extra></extra>'  # Custom hover text with no additional trace info
  )
  
  # Customize the layout
  fig <- fig %>% layout(
    title = "",
    xaxis = list(title = ""),
    yaxis = list(title = ""),
    showlegend = FALSE  # Remove legend (if not needed)
  )
  
  # Remove the plotly toolbar
  fig <- fig %>% config(displayModeBar = FALSE)
  
  return(fig)
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
  
  # Modify the factor labels to keep only the text before the hyphen
  summary_df$Value <- str_remove(summary_df$Value, "\\s*[-–—].*")
  
  # Reapply the correct order to the factor levels in summary_df
  summary_df$Value <- factor(summary_df$Value, levels = unique(summary_df$Value))
  
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

# Function to create a donut chart of frequencies of values across columns with a given naming structure, which excludes suffixes "_n"
# Also cleans variable labels to only keep text after "- "
donut_chart_cols_pct <- function(data, base_col_name) {
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
  donut_chart <- plot_ly(
    labels = names(column_means),
    values = column_means,
    type = 'pie',
    hole = 0.4
  ) %>%
    layout(title = "") %>%
    config(displayModeBar = FALSE)
  
  return(donut_chart)
}


# Function to create a box plot of values in a column
v_box_plot <- function(df, column_name) {
  # Check if the column exists in the dataframe
  if (!column_name %in% colnames(df)) {
    stop("The column does not exist in the dataframe")
  }
  
  # Ensure the column is numeric
  if (!is.numeric(df[[column_name]])) {
    stop("The column must be numeric to create a box plot")
  }
  
  # Create the box plot using plotly
  fig <- plot_ly(
    data = df,
    y = df[[column_name]],
    type = 'box',
    boxpoints = 'all',  # Show all points, including outliers
    jitter = 0.3,  # Add some jitter to avoid overlapping points
    pointpos = -1.8,  # Position of points relative to box
    hoverinfo = 'y'  # Show the value on hover
  )
  
  # Customize the layout
  fig <- fig %>% layout(
    title = "",
    yaxis = list(title = "%"),
    xaxis = list(title = "", showticklabels = FALSE)
  )
  
  # Remove the plotly toolbar
  fig <- fig %>% config(displayModeBar = FALSE)
  
  return(fig)
}

# Function to create a stacked bar chart of frequencies of values across columns with a given naming structure
stacked_v_bar_chart_cols <- function(data, base_col_name, wrap_width = 30) {
  # Use select and starts_with to get columns that match the pattern
  matching_cols <- data %>%
    select(starts_with(base_col_name)) 
  
  # Filter the columns that follow the pattern "base_col_name_number"
  matching_cols <- matching_cols %>%
    select(matches(paste0("^", base_col_name, "_\\d+$")))
  
  if (ncol(matching_cols) == 0) {
    stop("No columns match the given base column name structure.")
  }
  
  # Retrieve the variable labels for each column
  variable_labels <- sapply(matching_cols, function(col) {
    var_label <- attr(col, "label")
    if (!is.null(var_label)) {
      # Remove text before "- "
      cleaned_label <- str_remove(var_label, ".*- ")
      # Move the text inside parentheses to a new line and wrap the text
      cleaned_label <- str_replace(cleaned_label, "\\s*\\((.*)\\)", function(match) {
        wrapped_text <- str_wrap(str_match(match, "\\((.*)\\)")[2], width = wrap_width)
        return(paste0("<br>(", str_replace_all(wrapped_text, "\n", "<br>"), ")"))
      })
      return(cleaned_label)
    } else {
      return(colnames(data))
    }
  }, simplify = TRUE, USE.NAMES = FALSE)
  
  # Retrieve the factor labels from the first column (assuming all columns have the same labels)
  value_labels <- attr(matching_cols[[1]], "labels")
  
  if (is.null(value_labels)) {
    stop("No value labels found in the data.")
  }
  
  # Convert factor labels to a named vector and reverse the order
  value_labels <- setNames(names(value_labels), as.character(value_labels))
  value_labels <- rev(value_labels)  # Reverse the order of labels
  
  # Reshape the data for plotting (long format)
  df_long <- matching_cols %>%
    pivot_longer(cols = everything(), names_to = "Category", values_to = "Value")
  
  # Map the cleaned variable labels to the categories
  df_long$Category <- factor(df_long$Category, levels = colnames(matching_cols), labels = variable_labels)
  
  # Convert the values to their corresponding labels, using reversed levels
  df_long$Value <- factor(df_long$Value, levels = names(value_labels), labels = value_labels)
  
  # Summarize the counts for each value by category and calculate percentages
  summary_df <- df_long %>%
    group_by(Category, Value) %>%
    summarize(Count = n(), .groups = 'drop') %>%
    group_by(Category) %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  # Create a stacked bar chart using plotly
  bar_chart <- plot_ly(
    data = summary_df,
    x = ~Category,
    y = ~Count,
    color = ~Value,  # Color the bars by the response label
    type = 'bar',
    text = ~paste0(Value, "<br>", Count, "<br>", round(Percentage, 2), "%"),
    hoverinfo = 'text',
    hovertemplate = '%{text}<extra></extra>'
  ) %>%
    layout(
      title = "",
      barmode = 'stack',  # Stack the bars
      xaxis = list(title = ""),
      yaxis = list(title = "")
    ) %>%
    config(displayModeBar = FALSE)
  
  return(bar_chart)
}

# Function to create a horizontal stacked bar chart of frequencies of values across columns with a given naming structure
stacked_h_bar_chart_cols <- function(data, base_col_name, wrap_width = 30) {
  # Use select and starts_with to get columns that match the pattern
  matching_cols <- data %>%
    select(starts_with(base_col_name)) 
  
  # Filter the columns that follow the pattern "base_col_name_number"
  matching_cols <- matching_cols %>%
    select(matches(paste0("^", base_col_name, "_\\d+$")))
  
  if (ncol(matching_cols) == 0) {
    stop("No columns match the given base column name structure.")
  }
  
  # Retrieve the variable labels for each column
  variable_labels <- sapply(matching_cols, function(col) {
    var_label <- attr(col, "label")
    if (!is.null(var_label)) {
      # Remove text before "- "
      cleaned_label <- str_remove(var_label, ".*- ")
      # Move the text inside parentheses to a new line and wrap the text
      cleaned_label <- str_replace(cleaned_label, "\\s*\\((.*)\\)", function(match) {
        wrapped_text <- str_wrap(str_match(match, "\\((.*)\\)")[2], width = wrap_width)
        return(paste0("<br>(", str_replace_all(wrapped_text, "\n", "<br>"), ")"))
      })
      return(cleaned_label)
    } else {
      return(colnames(data))
    }
  }, simplify = TRUE, USE.NAMES = FALSE)
  
  # Retrieve the factor labels from the first column (assuming all columns have the same labels)
  value_labels <- attr(matching_cols[[1]], "labels")
  
  if (is.null(value_labels)) {
    stop("No value labels found in the data.")
  }
  
  # Convert factor labels to a named vector and reverse the order
  value_labels <- setNames(names(value_labels), as.character(value_labels))
  value_labels <- rev(value_labels)  # Reverse the order of labels
  
  # Reshape the data for plotting (long format)
  df_long <- matching_cols %>%
    pivot_longer(cols = everything(), names_to = "Category", values_to = "Value")
  
  # Map the cleaned variable labels to the categories and reverse the order of levels
  df_long$Category <- factor(df_long$Category, levels = rev(colnames(matching_cols)), labels = rev(variable_labels))
  
  # Convert the values to their corresponding labels, using reversed levels
  df_long$Value <- factor(df_long$Value, levels = names(value_labels), labels = value_labels)
  
  # Summarize the counts for each value by category and calculate percentages
  summary_df <- df_long %>%
    group_by(Category, Value) %>%
    summarize(Count = n(), .groups = 'drop') %>%
    group_by(Category) %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  # Create a horizontal stacked bar chart using plotly
  bar_chart <- plot_ly(
    data = summary_df,
    y = ~Category,  # Flip x and y to make it horizontal
    x = ~Count,     # Count on the x-axis now
    color = ~Value, # Color the bars by the response label
    type = 'bar',
    orientation = 'h',  # Horizontal orientation
    text = ~paste0(Value, "<br>", Count, "<br>", round(Percentage, 2), "%"),
    hoverinfo = 'text',
    hovertemplate = '%{text}<extra></extra>'
  ) %>%
    layout(
      title = "",
      barmode = 'stack',  # Stack the bars
      yaxis = list(title = ""),  # Adjust the y-axis title
      xaxis = list(title = "")   # Adjust the x-axis title
    ) %>%
    config(displayModeBar = FALSE)
  
  return(bar_chart)
}
