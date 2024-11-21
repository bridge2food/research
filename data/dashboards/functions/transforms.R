# transforms.R

# Define the global transforms (applied to all periods and surveys)
global_transforms <- function(data, period, survey_name) {
  data %>%
    filter(Finished == 1) %>%
    mutate(Period = period, Survey = survey_name)
}

# Define period-specific transforms
period_specific_transforms <- list(
  "2024-Q4" = function(data, survey_name) {
    if (survey_name == "PPUS") {
      # Load the segment mapping CSV for Q4
      segment_mapping <- read_csv(paste0(base_dir,"PPUS-2024-Q4-segment-mapping.csv"))
      data <- data %>%
        filter(Company != "Magnificent Proteins") %>%  # not currently in production
        left_join(segment_mapping, by = "Company") %>%  # Join to add Segment information
        filter(Segment != "Equipment & Technology")
    }
    if (survey_name == "PMP") {
      data <- data %>%
        mutate(Segment = "Plant-Based Meat")
    }
    if (survey_name == "PDP") {
      data <- data %>%
        mutate(Segment = "Plant-Based Dairy")
    }
    
    data <- data %>%
      filter(po.prod_lvl.q_1 > 0)
    
    return(data)
  }
  # Add more periods as needed
)

# Main function to apply all transforms
apply_transforms <- function(data, period, survey_name) {
  # Apply the global transforms
  data <- global_transforms(data, period, survey_name)
  
  # Check if there are transforms for the specific period and apply them if available
  if (period %in% names(period_specific_transforms)) {
    data <- period_specific_transforms[[period]](data, survey_name)
  }
  
  return(data)
}
