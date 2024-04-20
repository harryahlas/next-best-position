library(dplyr)
library(lubridate)

# Simulating the data
employee_data <- data.frame(
  employee_id = c(1067, 1134, 1201, 1268, 1335, 1402),
  available_date = as.Date(c("2024-08-29", "2024-07-02", "2024-06-03", "2024-10-05", "2024-07-20", "2024-08-29")),
  work_skill = c("A", "A", "B", "A", "B", "A"),
  region = c(1, 1, 1, 1, 2, 1)
)

open_positions <- data.frame(
  position_ID = c(1016, 1037, 1033, 1013, 1032),
  start_date = as.Date(c("2024-11-23", "2024-12-21", "2024-08-25", "2025-03-25", "2025-01-02")),
  work_skill = c("B", "A", "A", "A", "A"),
  region = c(1, 1, 1, 1, 2)
)


# Step 3: Define the Matching Logic
# We'll define a function to calculate the points for each potential match.

calculate_points <- function(employee, position) {
  days_diff <- as.numeric(position$start_date - employee$available_date)
  points <- 0
  
  if (days_diff > 30) {
    points <- points + 500
  } else {
    points <- points + days_diff
  }
  
  if (employee$work_skill != position$work_skill) {
    points <- points + 500
  }
  
  if (employee$region != position$region) {
    points <- points + 250
  }
  
  return(points)
}


# Step 4: Implement the Matching Algorithm
# This step involves looping through all possible combinations of employees and positions, calculating the points for each combination, and keeping track of the combination with the lowest total points.

lowest_total_points <- Inf
best_match <- NULL

for (i in seq_len(nrow(employee_data))) {
  employee <- employee_data[i, ]
  for (j in seq_len(nrow(open_positions))) {
    position <- open_positions[j, ]
    if (employee$available_date < position$start_date) {
      points <- calculate_points(employee, position)
      if (points < lowest_total_points) {
        lowest_total_points <- points
        best_match <- data.frame(employee_id = employee$employee_id, position_ID = position$position_ID)
      }
    }
  }
}


# Add unmatched employees and positions
unmatched_employees <- setdiff(employee_data$employee_id, best_match$employee_id)
unmatched_positions <- setdiff(open_positions$position_ID, best_match$position_ID)

for (employee_id in unmatched_employees) {
  best_match <- rbind(best_match, data.frame(employee_id = employee_id, position_ID = NA))
}

for (position_ID in unmatched_positions) {
  best_match <- rbind(best_match, data.frame(employee_id = NA, position_ID = position_ID))
}

# Add 1000 points for unmatched rows
best_match$points <- ifelse(is.na(best_match$position_ID), 1000, calculate_points(employee_data[employee_data$employee_id == best_match$employee_id,], open_positions[open_positions$position_ID == best_match$position_ID,]))

# Summarize the total points
total_points <- sum(best_match$points)

print(best_match)
print(paste("Total Points:", total_points))
