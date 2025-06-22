# tests/testthat/helper-overview-module.R
# Helper functions and utilities for Overview Module testing

library(testthat)
library(mockery)
library(shiny)

# Test Configuration Constants
TEST_CONFIG <- list(
  PERFORMANCE_THRESHOLD_SECONDS = 2.0,
  MEMORY_THRESHOLD_MB = 50,
  LARGE_DATASET_SIZE = 5000,
  MEDIUM_DATASET_SIZE = 1000,
  SMALL_DATASET_SIZE = 100,
  TOLERANCE = 0.001
)

# Enhanced Mock Data Generator with Realistic Distributions
create_realistic_employee_data <- function(n = 100, seed = 123, include_nulls = FALSE) {
  set.seed(seed)
  
  # Realistic department distribution
  dept_weights <- c("Sales" = 0.25, "IT" = 0.20, "HR" = 0.10, 
                    "Finance" = 0.15, "Marketing" = 0.15, "Operations" = 0.15)
  
  # Realistic age distribution (normal distribution)
  ages <- pmax(22, pmin(65, round(rnorm(n, mean = 38, sd = 10))))
  
  # Salary based on department and age
  base_salaries <- c("Sales" = 55000, "IT" = 75000, "HR" = 50000,
                     "Finance" = 65000, "Marketing" = 60000, "Operations" = 48000)
  
  departments <- sample(names(dept_weights), n, replace = TRUE, prob = dept_weights)
  
  # Calculate realistic salaries with age and department factors
  salaries <- sapply(seq_len(n), function(i) {
    base <- base_salaries[departments[i]]
    age_factor <- (ages[i] - 22) * 1000  # $1k per year of age beyond 22
    experience_bonus <- sample(0:20000, 1)  # Random experience bonus
    pmax(35000, base + age_factor + experience_bonus)
  })
  
  # Attrition based on realistic factors
  attrition_prob <- pmin(0.4, pmax(0.05, 
    0.15 - (ages - 25) * 0.002 +  # Younger employees more likely to leave
    ifelse(salaries < 50000, 0.05, -0.02) +  # Lower salary increases risk
    ifelse(departments == "IT", -0.03, 0)  # IT has lower attrition
  ))
  
  data <- data.frame(
    EmployeeID = 1:n,
    FirstName = paste0("Employee", sprintf("%04d", 1:n)),
    LastName = paste0("Surname", sprintf("%04d", 1:n)),
    Age = ages,
    Gender = sample(c("Male", "Female", "Other"), n, replace = TRUE, 
                    prob = c(0.48, 0.48, 0.04)),
    Department = departments,
    Salary = salaries,
    Attrition = ifelse(runif(n) < attrition_prob, "Yes", "No"),
    YearsAtCompany = pmax(1, pmin(ages - 22, rpois(n, lambda = 5))),
    JobSatisfaction = sample(1:5, n, replace = TRUE, prob = c(0.05, 0.15, 0.30, 0