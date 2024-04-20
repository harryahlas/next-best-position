# Load necessary libraries
library(dplyr)
library(gtools)

# Your data
data <- data.frame(
  employee_id = c(1067, 1134, 1201, 1268, 1067, 1134, 1201, 1268, 1067, 1134, 1201, 1268, 1067, 1134, 1201, 1268),
  position_id = c(1016, 1016, 1016, 1016, 1037, 1037, 1037, 1037, 1033, 1033, 1033, 1033, 1013, 1013, 1013, 1013),
  points = c(100, 500, 500, 500, 1000, 100, 250, 750, 1000, 250, 0, 0, 100, 250, 400, 100)
)

number_of_employees <- 4
number_of_openings <- 4 # must be less or equal to employees number

# Generate all possible combinations of employee-position assignments
combinations <- permutations(n = number_of_employees, r = number_of_openings, v = unique(data$employee_id), repeats.allowed = FALSE)

head(combinations)

sum(data)

# Function to calculate total score for a given combination
calculate_score <- function(combination) {
  
  total_score = 
  
  total_score <- sum(data$points[data$employee_id == combination[1] & data$position_id == combination[2]] +
                       data$points[data$employee_id == combination[3] & data$position_id == combination[4]] +
                       data$points[data$employee_id == combination[5] & data$position_id == combination[6]] +
                       data$points[data$employee_id == combination[7] & data$position_id == combination[8]])
  return(total_score)
}


# Calculate the total score for each combination
scores <- sapply(combinations, calculate_score)

# Find the combination with the lowest score
lowest_score <- min(scores)
lowest_score_combination <- combinations[which.min(scores)]

# Print the result
cat("The combination with the lowest score is:", lowest_score_combination, "\n")
cat("The total score for this combination is:", lowest_score, "\n")
