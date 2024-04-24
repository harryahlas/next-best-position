import pandas as pd
from scipy.optimize import linear_sum_assignment
from datetime import datetime
import numpy as np

# Function to calculate the cost for an employee to start a job based on the rules provided
def calculate_cost(employee, job):
    # Initialize cost
    cost = 0

    # Convert dates to datetime objects for comparison
    emp_date = datetime.strptime(employee['available_date'], '%m/%d/%Y')
    job_date = datetime.strptime(job['start_date'], '%m/%d/%Y')

    # Check if the available date is on or after the start date
    if emp_date > job_date:
        # Add high cost for starting before availability
        cost += 500
    else:
        # Add cost for each day the start date is greater than the available date
        cost += (job_date - emp_date).days

    # Add cost for work skill mismatch
    if employee['work_skill'] != job['work_skill']:
        cost += 500

    # Add cost for region mismatch
    if employee['region'] != job['region']:
        cost += 500

    return cost

# Path to the files
employees_path = "D:\\github\\next-best-position\\employees.txt"
jobs_path = "D:\\github\\next-best-position\\jobs.txt"

# Reading the data into pandas dataframes
employees_df = pd.read_csv(employees_path, sep='\t')
jobs_df = pd.read_csv(jobs_path, sep='\t')

# Create a cost matrix based on the rules provided
cost_matrix = []
for _, employee in employees_df.iterrows():
    employee_costs = []
    for _, job in jobs_df.iterrows():
        employee_costs.append(calculate_cost(employee, job))
    cost_matrix.append(employee_costs)

# Convert the cost matrix to a numpy array for the Hungarian algorithm
cost_matrix = np.array(cost_matrix)

# Apply the Hungarian algorithm to find the minimum cost assignment
row_ind, col_ind = linear_sum_assignment(cost_matrix)

# Create a DataFrame for the matches
matches_df = pd.DataFrame({
    'employee_id': employees_df['employee_id'][row_ind].values,
    'position_ID': jobs_df['position_ID'][col_ind].values
})

# Save the matches to a CSV file
matches_file_path = 'D:\\github\\next-best-position\\employee_job_matches2.csv'
matches_df.to_csv(matches_file_path, index=False)
