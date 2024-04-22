# Install and load the lpSolve package
if (!require(lpSolve)) install.packages("lpSolve", dependencies=TRUE)
library(lpSolve)
library(readxl)

# Load necessary library
library(lubridate)
library(tidyverse)

# Define employee data
employee_data <- read.table(text = "
employee_id available_date work_skill region
1067 8/29/2024 A 1
1134 7/2/2024 A 1
1201 6/3/2024 B 1
1268 10/5/2024 A 1
1335 7/20/2024 B 2
1402 8/29/2024 A 1
1469 6/12/2024 A 2
1536 6/11/2024 A 2
1603 5/6/2024 A 2
1670 9/21/2024 A 2
1737 9/21/2024 A 2
1804 7/10/2024 B 1
1871 9/20/2024 A 2
1938 8/22/2024 A 2
2005 7/31/2024 A 2
2072 6/6/2024 A 1
2139 6/20/2024 A 1
2206 6/30/2024 A 2
2273 8/29/2024 B 2
", header = TRUE, stringsAsFactors = FALSE)

employee_data <- read_excel("assignment_data_5b.xlsx", sheet = "employees")


# Define open positions data
open_positions <- read.table(text = "
position_ID start_date work_skill region
1016 11/23/2024 B 1
1037 12/21/2024 A 1
1033 8/25/2024 A 1
1013 3/25/2025 A 1
1032 1/2/2025 A 1
1028 8/23/2024 B 1
1065 4/1/2025 B 1
1021 11/29/2024 B 1
1020 10/24/2024 A 1
1097 6/5/2024 B 1
1076 3/12/2025 A 1
1001 1/17/2025 A 1
1042 11/15/2024 B 1
1085 3/16/2025 B 1
1060 1/2/2025 B 1
1095 10/8/2024 B 1
1067 5/19/2024 A 1
1061 2/3/2025 A 1
1011 3/5/2025 A 1
", header = TRUE, stringsAsFactors = FALSE)

open_positions <- read_excel("assignment_data_5b.xlsx", sheet = "open positions")

# mdy(as.character(as.Date(employee_data$available_date[1])))
# mdy(format(employee_data$available_date[1], "%m/%d/%Y"))
# Function to calculate points
calculate_points <- function(employee, position) {
  # Convert dates
  available_date <- mdy(format(employee$available_date, "%m/%d/%Y"))
  start_date <- mdy(format(position$start_date, "%m/%d/%Y"))
  
  # Calculate day difference
  day_diff <- as.integer(difftime(start_date, available_date, units = "days"))
  
  # Initialize points
  points <- 0
  
  # Calculate points based on conditions
  if (day_diff > 30) {
    points <- points + 500
  } else {
    points <- points + abs(day_diff)
  }
  
  if (employee$work_skill != position$work_skill) {
    points <- points + 500
  }
  
  if (employee$region != position$region) {
    points <- points + 250
  }
  
  if (available_date > start_date) {
    points <- points + 1000
  }
  
  return(points)
}

# Compute the matrix of points
score_matrix <- matrix(nrow = nrow(employee_data), ncol = nrow(open_positions))

# Fill the matrix with points
for (i in 1:nrow(employee_data)) {
  for (j in 1:nrow(open_positions)) {
    score_matrix[i, j] <- calculate_points(employee_data[i,], open_positions[j,])
  }
}

# Assign row and column names for clarity
rownames(score_matrix) <- employee_data$employee_id
colnames(score_matrix) <- open_positions$position_ID

# Print the matrix
print(score_matrix)
#write.csv(score_matrix, "score_matrix.csv")

# Example data: matrix of productivity scores
# Rows: People, Columns: Jobs
# score_matrix <- matrix(c(
#   10, 19, 8, 15, 10,
#   10, 18, 7, 17, 9,
#   13, 1, 9, 14, 9,
#   12, 19, 8, 19, 10,
#   14, 17, 10, 16, 9,
#   14, 1, 10, 16, 10,
#   13, 18, 9, 14, 8,
#   12, 17, 9, 18, 10,
#   14, 15, 10, 15, 10,
#   13, 16, 9, 17, 9,
#   10, 19, 8, 15, 10,
#   10, 18, 7, 17, 9,
#   13, 1, 9, 14, 9,
#   12, 19, 8, 19, 10,
#   14, 17, 10, 16, 9,
#   14, 1, 10, 16, 10,
#   13, 18, 9, 14, 8,
#   12, 17, 9, 18, 10,
#   14, 15, 10, 15, 10,
#   13, 16, 9, 17, 9
# ), nrow=10, byrow=TRUE)

# Convert productivity to "cost" by negating (for maximization)
# cost_matrix <- score_matrix

# Define the linear program model
lp_model <- lp.assign(score_matrix, "min")

# Solution: Assigned jobs for each person
solution <- lp_model$solution
rownames(solution) <- employee_data$employee_id
colnames(solution) <- open_positions$position_ID
solution %>% 
  data.frame() %>% 
  rownames_to_column("employee_id") %>% 
  pivot_longer(cols = -employee_id, names_to = "position_id") %>% 
  mutate(position_id = str_replace(position_id, "X", "")) %>% 
  mutate(employee_id = as.numeric(employee_id),
         position_ID = as.numeric(position_id)) %>% 
  filter(value == 1) %>% 
  left_join(employee_data) %>% 
  rename(work_skill_employee = work_skill,
         region_employee = region) %>% 
  left_join(open_positions) %>% 
  rename(work_skill_position = work_skill,
         region_position = region) %>% 
  write.csv("solution5b.csv")

# Print results
cat("The optimal assignment matrix is:\n")
print(lp_model$solution)
cat("The maximum total productivity is:", -lp_model$objval, "\n")




