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
