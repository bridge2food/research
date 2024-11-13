#################### Indicators functions
# Functions to calculate composite indicators, i.e., those that are not
# merely the net balance of a column's values

# Function to calculate Industry Confidence Indicator
calculate_industry_confidence <- function(data) {
  
  orders_nb <- mean(data$po.orders_curr.q, na.rm = TRUE)*100
  stocks_nb <- mean(data$po.stocks_curr.q, na.rm = TRUE)*100
  prod_exp_nb <- mean(data$po.prod_next_3.q, na.rm = TRUE)*100
  
  ic <- (orders_nb - stocks_nb + prod_exp_nb) / 3
  return(ic)
}

# Business uncertainty indicator
calculate_business_uncertainty <- function(data) {
  bu <- mean(data$po.uncertainty.q, na.rm = TRUE) * (-100)
  return(bu)
}


# Function for Employment Outlook Indicator
calculate_employment_outlook <- function(data) {
  emp_exp <- data$lab.emp_next_3.q
  eo <- mean(emp_exp, na.rm = T)*100 
  return(eo)
}

# Top-level function to calculate all indicators
calculate_indicators <- function(data, period) {
  
  # Calculate each indicator
  ic <- calculate_industry_confidence(data)
  bu <- calculate_business_uncertainty(data)
  eo <- calculate_employment_outlook(data)
  
  # Combine indicators into a data frame
  indicators <- data.frame(
    Period = period,
    ic = ic,
    bu = bu,
    eo = eo
  )
  return(indicators)
}

