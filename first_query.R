# Load necessary libraries
library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)

# Sample data
employee_data_sample <- data.frame(
  employee_id = c(21, 1, 76),
  available_date = as.Date(c("2024-05-07", "2024-08-29", "2024-08-29")),
  points = c(0, 999, 499)
)

open_position_data_sample <- data.frame(
  position_ID = c(1, 2, 3),
  start_date = as.Date(c("2024-08-04", "2025-01-06", "2025-03-17"))
)

employee_data <- read_excel("assignment_data.xlsx", sheet = "employees")
open_position_data <- read_excel("assignment_data.xlsx", sheet = "open positions")

# Convert dates to Date format
employee_data$available_date <- as.Date(employee_data$available_date, format = "%m/%d/%Y")
open_position_data$start_date <- as.Date(open_position_data$start_date, format = "%m/%d/%Y")

# Calculate date difference and adjust points
employee_data <- employee_data %>%
  mutate(date_diff = as.numeric(start_date - available_date),
         new_points = ifelse(date_diff > 30, points + 500, points + date_diff))

# Join employee and position data, matching on the lowest points
matched_data <- open_position_data %>%
  left_join(employee_data, by = character()) %>%
  group_by(position_ID) %>%
  arrange(new_points) %>%
  slice_head(n = 1) %>%
  ungroup()

# Handle cases where no match is possible
unmatched_positions <- open_position_data %>%
  anti_join(matched_data, by = "position_ID")

unmatched_positions <- unmatched_positions %>%
  mutate(employee_id = NA, points = 1000, date_diff = NA, new_points = 1000)

# Combine matched and unmatched data
final_data <- bind_rows(matched_data, unmatched_positions)

# Display the final data
print(final_data)
