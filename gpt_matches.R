library(clue)
library(lubridate)

# Function to calculate the cost for an employee to start a job based on the rules provided
calculate_cost <- function(employee, job) {
  # Initialize cost
  cost <- 0
  
  # Convert dates to Date objects for comparison
  emp_date <- mdy(employee$available_date)
  job_date <- mdy(job$start_date)
  
  # Check if the available date is on or after the start date
  if (emp_date > job_date) {
    # Add high cost for starting before availability
    cost <- cost + 500
  } else {
    # Add cost for each day the start date is greater than the available date
    cost <- cost + as.integer(job_date - emp_date)
  }
  
  # Add cost for work skill mismatch
  if (employee$work_skill != job$work_skill) {
    cost <- cost + 500
  }
  
  # Add cost for region mismatch
  if (employee$region != job$region) {
    cost <- cost + 500
  }
  
  return(cost)
}

# Read the data from files
employees <- read.delim('D:\\github\\next-best-position\\employees.txt', header=TRUE, sep='\t')
jobs <- read.delim('D:\\github\\next-best-position\\jobs.txt', header=TRUE, sep='\t')

# Create a cost matrix based on the rules provided
cost_matrix <- matrix(0, nrow=nrow(employees), ncol=nrow(jobs))
for (i in 1:nrow(employees)) {
  for (j in 1:nrow(jobs)) {
    cost_matrix[i, j] <- calculate_cost(employees[i, ], jobs[j, ])
  }
}

# Apply the Hungarian algorithm to find the minimum cost assignment
solution <- solve_LSAP(cost_matrix, maximum=FALSE)

# Get the total cost
total_cost <- sum(cost_matrix[cbind(1:nrow(employees), solution)])

# Create a data frame for the matches
matches <- data.frame(employee_id = employees$employee_id,
                      position_ID = jobs$position_ID[solution])

# Print the total cost and the first few matches
print(total_cost)
print(head(matches))

# Write the matches to a CSV file
write.csv(matches, 'gpt_employee_job_matches_R.csv', row.names=FALSE)
