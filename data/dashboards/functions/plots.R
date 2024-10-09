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

#####

# Function to create a stacked vertical bar chart across columns with naming structure name_n_k
# where n represents categories and k represents choices within each category

stacked_v_bar_chart_cols_nk <- function(data, 
                                          base_col_name, 
                                          label_sep = " - ",    # Separator between name_prefix and the rest
                                          k_label_words = 2,    # Number of words representing k_label
                                          wrap_width = 30) {     # Width for text wrapping
  
  # 1. Construct the regex pattern to match columns like base_col_name_n_k
  pattern <- paste0("^", base_col_name, "_\\d+_\\d+$")
  
  # 2. Select columns matching the pattern
  matching_cols <- data %>%
    select(matches(pattern))
  
  # 3. Check if any columns match
  if (ncol(matching_cols) == 0) {
    stop("No columns match the given base column name structure.")
  }
  
  # 4. Extract 'n' and 'k' from column names using regex
  col_names <- colnames(matching_cols)
  col_info <- str_match(col_names, paste0("^", base_col_name, "_(\\d+)_(\\d+)$"))
  
  if (any(is.na(col_info))) {
    stop("Some columns do not match the expected pattern 'base_col_name_n_k'.")
  }
  
  # 5. Create a data frame with column information
  col_info_df <- data.frame(
    full_name = col_names,
    n = col_info[,2],
    k = col_info[,3],
    stringsAsFactors = FALSE
  )
  
  # 6. Count the number of 1s in each sub-category (column)
  counts_df <- matching_cols %>%
    summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "full_name", values_to = "Count") %>%
    left_join(col_info_df, by = "full_name")
  
  # 7. Extract 'n_label' and 'k_label'
  
  # Helper function to extract n_label and k_label from a label string
  extract_labels <- function(label, label_sep, k_label_words) {
    if (is.null(label) || is.na(label)) {
      return(list(n_label = NA, k_label = NA))
    }
    
    # Split the label using label_sep
    parts <- str_split_fixed(label, fixed(label_sep), 2)
    
    if (ncol(parts) < 2) {
      # If label_sep is not found, assign default labels
      return(list(n_label = paste0("Group ", n), k_label = paste0("Option ", k)))
    }
    
    # parts[1] is name_prefix, parts[2] is "n_part k_part"
    n_part_k_part <- parts[2]
    
    # Split n_part_k_part into n_part and k_part based on k_label_words
    words <- str_split(n_part_k_part, "\\s+")[[1]]
    
    if (length(words) < k_label_words) {
      # Not enough words to split
      n_label <- n_part_k_part
      k_label <- paste0("Option ", k)
    } else {
      n_label <- paste(head(words, length(words) - k_label_words), collapse = " ")
      k_label <- paste(tail(words, k_label_words), collapse = " ")
    }
    
    return(list(n_label = n_label, k_label = k_label))
  }
  
  # 7a. Extract unique n_labels
  n_labels_df <- col_info_df %>%
    group_by(n) %>%
    slice(1) %>%  # Take the first k for each n
    ungroup() %>%
    rowwise() %>%
    mutate(labels = list(extract_labels(attr(data[[full_name]], "label"), label_sep, k_label_words))) %>%
    mutate(n_label = labels$n_label) %>%
    select(n, n_label)
  
  # 7b. Extract unique k_labels
  k_labels_df <- col_info_df %>%
    group_by(k) %>%
    slice(1) %>%  # Take the first n for each k
    ungroup() %>%
    rowwise() %>%
    mutate(labels = list(extract_labels(attr(data[[full_name]], "label"), label_sep, k_label_words))) %>%
    mutate(k_label = labels$k_label) %>%
    select(k, k_label)
  
  # 8. Assign 'n_label' and 'k_label' back to counts_df
  counts_df <- counts_df %>%
    left_join(n_labels_df, by = "n") %>%
    left_join(k_labels_df, by = "k")
  
  # 9. Clean labels by handling text inside parentheses and adding line breaks
  clean_label <- function(label) {
    if (is.na(label)) return(label)
    # Handle text inside parentheses by moving it to a new line and wrapping
    # Match text inside parentheses
    label_clean <- str_replace(label, "\\((.*)\\)", function(x) {
      inner_text <- str_match(x, "\\((.*)\\)")[2]
      wrapped_text <- str_wrap(inner_text, width = wrap_width)
      paste0("<br>(", str_replace_all(wrapped_text, "\n", "<br>"), ")")
    })
    return(label_clean)
  }
  
  # Apply cleaning to 'n_label' and 'k_label'
  counts_df <- counts_df %>%
    mutate(
      n_label = sapply(n_label, clean_label),
      k_label = sapply(k_label, clean_label)
    )
  
  # 10. Ensure 'n_label' and 'k_label' are factors with proper ordering
  # Sort n_labels_df by numeric 'n' and set factor levels
  n_labels_df_sorted <- n_labels_df %>%
    mutate(n_numeric = as.numeric(n)) %>%
    arrange(n_numeric) %>%
    mutate(n_label = factor(clean_label(n_label), levels = unique(clean_label(n_label))))
  
  # Sort k_labels_df by numeric 'k' and set factor levels
  k_labels_df_sorted <- k_labels_df %>%
    mutate(k_numeric = as.numeric(k)) %>%
    arrange(k_numeric) %>%
    mutate(k_label = factor(clean_label(k_label), levels = unique(clean_label(k_label))))
  
  # Assign factor levels to counts_df based on the sorted labels
  counts_df <- counts_df %>%
    mutate(
      n_label = factor(n_label, levels = levels(n_labels_df_sorted$n_label)),
      k_label = factor(k_label, levels = levels(k_labels_df_sorted$k_label))
    )
  
  # 11. Check for duplicated levels in 'n_label' and 'k_label'
  if (any(duplicated(levels(counts_df$n_label)))) {
    stop("Duplicated levels found in 'n_label'. Please ensure that each group has a unique label.")
  }
  
  if (any(duplicated(levels(counts_df$k_label)))) {
    stop("Duplicated levels found in 'k_label'. Please ensure that each choice has a unique label.")
  }
  
  # 12. Create the stacked vertical bar chart using plotly
  bar_chart <- plot_ly(
    data = counts_df,
    x = ~n_label,
    y = ~Count,
    color = ~k_label,
    type = 'bar',
    text = ~paste0(k_label, ": ", Count),
    hoverinfo = 'text',
    hovertemplate = '%{text}<extra></extra>'
  ) %>%
    layout(
      title = '',
      barmode = 'stack',
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      legend = list(title = list(text = ""))
    ) %>%
    config(displayModeBar = FALSE)
  
  return(bar_chart)
}
