

# Step 1: Create the Datasets
# First, we'll create the employee_data and open_positions datasets using the data.frame function.
employee_data <- data.frame(
  employee_id = c(1067, 1134, 1201, 1268, 1335, 1402, 1469, 1536, 1603, 1670, 1737, 1804, 1871, 1938, 2005, 2072, 2139, 2206, 2273),
  available_date = as.Date(c("2024-08-29", "2024-07-02", "2024-06-03", "2024-10-05", "2024-07-20", "2024-08-29", "2024-06-12", "2024-06-11", "2024-05-06", "2024-09-21", "2024-09-21", "2024-07-10", "2024-09-20", "2024-08-22", "2024-07-31", "2024-06-06", "2024-06-20", "2024-06-30", "2024-08-29")),
  work_skill = c("A", "A", "B", "A", "B", "A", "A", "A", "A", "A", "A", "B", "A", "A", "A", "A", "A", "B", "B"),
  region = c(1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 1, 2, 2, 2, 1, 1, 2, 2)
)

open_positions <- data.frame(
  position_ID = c(1016, 1037, 1033, 1013, 1032, 1028, 1065, 1021, 1020, 1097, 1076, 1001, 1042, 1085, 1060, 1095, 1067, 1061, 1011),
  start_date = as.Date(c("2024-11-23", "2024-12-21", "2024-08-25", "2025-03-25", "2025-01-02", "2024-08-23", "2025-04-01", "2024-11-29", "2024-10-24", "2024-06-05", "2025-03-12", "2025-01-17", "2024-11-15", "2025-03-16", "2024-01-02", "2024-10-08", "2024-05-19", "2025-02-03", "2025-03-05")),
  work_skill = c("B", "A", "A", "A", "A", "B", "B", "B", "A", "B", "A", "A", "B", "B", "B", "B", "A", "A", "A"),
  region = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)

# Step 2: Define the calculate_points Function
# This function calculates the points based on the given criteria.
calculate_points <- function(available_date, start_date, work_skill, region) {
 points <- 0
 days_diff <- as.numeric(start_date - available_date)
  
 if (days_diff > 30) {
    points <- points + 500
 } else {
    points <- points + days_diff
 }
  
 if (work_skill != open_positions$work_skill[1]) {
    points <- points + 500
 }
  
 if (region != open_positions$region[1]) {
    points <- points + 250
 }
  
 if (available_date > start_date) {
    points <- points + 1000
 }
  
 return(points)
}



library(dplyr)


# Step 3: Implement the Matching Logic
# This part involves looping through all possible combinations of employee_data and open_positions to find the best matches.
lowest_points <- Inf
best_matches <- data.frame()

# Generate all possible combinations
combinations <- expand.grid(employee_data$employee_id, open_positions$position_ID)

# Loop through each combination
for (i in 1:nrow(combinations)) {
  total_loop_points <- 0
  
  # Calculate points for the current pair
  points <- calculate_points(combinations$available_date[i], combinations$start_date[i], combinations$work_skill[i], combinations$region[i])
  total_loop_points <- total_loop_points + points
  
  # Update lowest_points and best_matches if necessary
  if (total_loop_points < lowest_points) {
    lowest_points <- total_loop_points
    best_matches <- combinations[i, ]
  }
}

# Print the best matches
print(best_matches)


# Print the best matches
print(best_matches)
