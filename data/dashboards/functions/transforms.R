# transforms.R

# Define the global transforms (applied to all periods and surveys)
global_transforms <- function(data, period, survey_name) {
  data %>%
    filter(Finished == 1) %>%
    mutate(Period = period, Survey = survey_name)
}

# Define period-specific transforms
period_specific_transforms <- list(
  "2024-Q4" = function(data) {
    data %>%
      filter(Company != "Magnificent Proteins") # not currently in production
  }
  # Add more periods as needed
)

# Main function to apply all transforms
apply_transforms <- function(data, period, survey_name) {
  # Apply the global filter
  data <- global_transforms(data, period, survey_name)
  
  # Check if there is a transform for the specific period and apply it if available
  if (period %in% names(period_specific_transforms)) {
    data <- period_specific_transforms[[period]](data)
  }
  
  return(data)
}
