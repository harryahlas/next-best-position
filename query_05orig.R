# Install and load the lpSolve package
if (!require(lpSolve)) install.packages("lpSolve", dependencies=TRUE)
library(lpSolve)

# Example data: matrix of productivity scores
# Rows: People, Columns: Jobs
productivity_matrix <- matrix(c(
  10, 19, 8, 15, 10,
  10, 18, 7, 17, 9,
  13, 1, 9, 14, 9,
  12, 19, 8, 19, 10,
  14, 17, 10, 16, 9,
  14, 1, 10, 16, 10,
  13, 18, 9, 14, 8,
  12, 17, 9, 18, 10,
  14, 15, 10, 15, 10,
  13, 16, 9, 17, 9,
  10, 19, 8, 15, 10,
  10, 18, 7, 17, 9,
  13, 1, 9, 14, 9,
  12, 19, 8, 19, 10,
  14, 17, 10, 16, 9,
  14, 1, 10, 16, 10,
  13, 18, 9, 14, 8,
  12, 17, 9, 18, 10,
  14, 15, 10, 15, 10,
  13, 16, 9, 17, 9
), nrow=10, byrow=TRUE)

# Convert productivity to "cost" by negating (for maximization)
cost_matrix <- productivity_matrix

# Define the linear program model
lp_model <- lp.assign(cost_matrix, "min")

# Solution: Assigned jobs for each person
lp_model$solution

# Print results
cat("The optimal assignment matrix is:\n")
print(lp_model$solution)
cat("The maximum total productivity is:", -lp_model$objval, "\n")




