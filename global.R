# global.R
# Load all required libraries and define global functions

# Load required packages
library(shiny)
library(shinydashboard)
library(readxl)
library(writexl)
library(DT)
library(dplyr)
library(lubridate)
library(shinyjs)

# Function to get first day of current month
get_first_day_of_month <- function() {
  floor_date(Sys.Date(), "month")
}

# Function to generate monthly dates
generate_monthly_dates <- function(start_date, num_months = 12) {
  seq.Date(from = start_date, by = "month", length.out = num_months)
}

# Function to calculate shortage warnings
calculate_shortage_warnings <- function(monthly_data, forecast_type, months = 6) {
  # Extract forecast row
  forecast_row <- monthly_data %>% 
    filter(Category == forecast_type)
  
  # Calculate total supply
  supply_categories <- c("Scheduled Receipts", "CM #1 Inventory", "CM #2 Inventory", 
                         "OEM Inventory", "Supplier Capacity")
  
  supply_rows <- monthly_data %>%
    filter(Category %in% supply_categories)
  
  # Get column names for months
  month_cols <- names(monthly_data)[2:(months+1)]
  
  # Calculate totals
  shortages <- data.frame(
    Month = month_cols,
    Forecast = as.numeric(forecast_row[1, month_cols]),
    Supply = colSums(supply_rows[, month_cols], na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Calculate shortage/surplus
  shortages$Shortage <- shortages$Supply - shortages$Forecast
  
  # Add warning flag
  shortages$Status <- ifelse(shortages$Shortage < 0, "SHORTAGE", "OK")
  
  return(shortages)
}

# Function to sort part numbers
sort_part_numbers <- function(part_numbers) {
  # Sort based on numeric and alpha components
  mixed_sort <- function(x) {
    # Extract numeric parts and character parts
    num_part <- as.numeric(gsub("[^0-9]", "", x))
    char_part <- gsub("[0-9]", "", x)
    
    # Order by num_part then by char_part
    order(num_part, char_part)
  }
  
  part_numbers[mixed_sort(part_numbers)]
}

# Define JavaScript function for client-side use
JS <- function(x) {
  shiny:::JS(x)
}