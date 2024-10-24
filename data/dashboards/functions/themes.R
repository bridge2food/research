# Load necessary packages
library(plotly)
library(RColorBrewer)

# Define the custom theme function
static_theme <- function(p, num_colors = 9) {
  # Generate a color palette with the specified number of colors
  color_palette <- brewer.pal(min(num_colors, 9), "Set2")
  
  # Apply the theme
  p %>% layout(
    # Set the font for the entire plot
    font = list(
      family = "Helvetica, Arial, sans-serif",
      size = 12,
      color = "#333333"
    ),
    # Customize the plot title
    title = list(
      font = list(
        size = 20,
        color = "#333333"
      ),
      x = 0.5,  # Center the title
      xanchor = 'center'
    ),
    # Customize the legend
    legend = list(
      font = list(
        size = 12
      ),
      bgcolor = 'rgba(0,0,0,0)',  # Transparent background
      bordercolor = 'rgba(0,0,0,0)'
    ),
    # Customize axes (useful for bar charts)
    xaxis = list(
      titlefont = list(
        size = 14,
        color = "#333333"
      ),
      tickfont = list(
        size = 12,
        color = "#333333"
      ),
      gridcolor = '#E2E2E2',
      zerolinecolor = '#E2E2E2'
    ),
    yaxis = list(
      titlefont = list(
        size = 14,
        color = "#333333"
      ),
      tickfont = list(
        size = 12,
        color = "#333333"
      ),
      gridcolor = '#E2E2E2',
      zerolinecolor = '#E2E2E2'
    ),
    # Set the plot's background and margins
    plot_bgcolor = '#FFFFFF',
    paper_bgcolor = '#FFFFFF',
    margin = list(
      l = 80,
      r = 50,
      b = 80,
      t = 100,
      pad = 0
    )
  ) %>%
    # Update the marker colors if applicable
    plotly::style(marker = list(colors = color_palette))
}

# Define the custom darker theme function
darker_plotly_theme <- function(p) {
  p %>% layout(
    # Set the font for the entire plot
    font = list(
      family = "Arial, sans-serif",
      size = 12,
      color = "#E5E5E5"  # Light gray font color
    ),
    # Customize the plot title
    title = list(
      font = list(
        size = 18,
        color = "#FFFFFF"  # White title color
      ),
      x = 0.5,
      xanchor = 'center'
    ),
    # Customize the legend
    legend = list(
      font = list(
        size = 12,
        color = "#E5E5E5"
      ),
      bgcolor = 'rgba(0,0,0,0)',  # Transparent background
      bordercolor = 'rgba(0,0,0,0)'
    ),
    # Customize axes (useful for bar charts)
    xaxis = list(
      titlefont = list(
        size = 14,
        color = "#FFFFFF"
      ),
      tickfont = list(
        size = 12,
        color = "#E5E5E5"
      ),
      gridcolor = '#444444',
      zerolinecolor = '#444444',
      showline = TRUE,
      linecolor = '#666666'
    ),
    yaxis = list(
      titlefont = list(
        size = 14,
        color = "#FFFFFF"
      ),
      tickfont = list(
        size = 12,
        color = "#E5E5E5"
      ),
      gridcolor = '#444444',
      zerolinecolor = '#444444',
      showline = TRUE,
      linecolor = '#666666'
    ),
    # Set the plot's background and margins
    plot_bgcolor = '#1A1A1A',  # Darker gray plot background
    paper_bgcolor = '#1A1A1A',  # Darker gray paper background
    margin = list(
      l = 70,
      r = 50,
      b = 70,
      t = 80,
      pad = 0
    )
  )
}
