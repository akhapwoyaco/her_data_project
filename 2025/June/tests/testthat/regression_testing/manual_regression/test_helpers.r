# ================================================================================
# Atlas Labs HR Analytics - Test Helpers and Directory Structure
# Author: akhapwoyaco
# Description: Helper functions and utilities for comprehensive testing
# ================================================================================

# ================================================================================
# TEST DIRECTORY STRUCTURE
# ================================================================================

# tests/
# ├── testthat.R                    # Main test runner
# ├── testthat/                     # testthat configuration
# │   └── setup.R                   # Test setup and teardown
# ├── unit/                         # Unit tests
# │   ├── test-data-loader.R
# │   ├── test-logger.R
# │   ├── test-modules.R
# │   └── test-utils.R
# ├── integration/                  # Integration tests
# │   ├── test-module-communication.R
# │   ├── test-data-flow.R
# │   └── test-ui-integration.R
# ├── performance/                  # Performance tests
# │   ├── test-load-testing.R
# │   ├── test-memory-usage.R
# │   └── test-response-times.R
# ├── security/                     # Security tests
# │   ├── test-input-validation.R
# │   ├── test-xss-prevention.R
# │   └── test-data-exposure.R
# ├── accessibility/                # Accessibility tests
# │   ├── test-wcag-compliance.R
# │   ├── test-keyboard-navigation.R
# │   └── test-screen-reader.R
# ├── edge_cases/                   # Edge case tests
# │   ├── test-boundary-values.R
# │   ├── test-data-corruption.R
# │   └── test-extreme-scenarios.R
# ├── visual/                       # Visual regression tests
# │   ├── test-plot-rendering.R
# │   └── test-ui-components.R
# ├── benchmarks/                   # Performance benchmarks
# │   └── run_benchmarks.R
# └── helpers/                      # Test helper functions
#     ├── create_test_data.R
#     ├── mock_functions.R
#     └── test_utilities.R

# ================================================================================
# MAIN TEST RUNNER (tests/testthat.R)
# ================================================================================

create_main_test_runner <- function() {
  test_runner_content <- '
# Atlas Labs HR Analytics - Main Test Runner
library(testthat)
library(shiny)

# Set test environment
Sys.setenv("TESTTHAT" = "true")
Sys.setenv("ATLAS_TEST_MODE" = "true")

# Run all tests
test_check("atlas_labs_hr_analytics")
'
  writeLines(test_runner_content, "tests/testthat.R")
}

# ================================================================================
# TEST SETUP AND CONFIGURATION (tests/testthat/setup.R)
# ================================================================================

create_test_setup <- function() {
  setup_content <- '
# Atlas Labs HR Analytics - Test Setup
library(testthat)
library(shiny)
library(mockery)
library(DT)
library(plotly)

# Global test configuration
options(
  testthat.default_check_reporter = "summary",
  testthat.default_reporter = "summary",
  shiny.testmode = TRUE,
  warn = 1
)

# Test data directory
TEST_DATA_DIR <- file.path("tests", "data")
if (!dir.exists(TEST_DATA_DIR)) {
  dir.create(TEST_DATA_DIR, recursive = TRUE)
}

# Helper functions for all tests
source("tests/helpers/test_utilities.R")
source("tests/helpers/create_test_data.R")
source("tests/helpers/mock_functions.R")

# Setup test environment
setup_test_environment <- function() {
  # Create temporary directories for testing
  temp_dirs <- c(
    "temp_data", "temp_logs", "temp_reports", "temp_exports"
  )
  
  for (dir in temp_dirs) {
    temp_path <- file.path(tempdir(), dir)
    if (!dir.exists(temp_path)) {
      dir.create(temp_path, recursive = TRUE)
    }
  }
  
  # Set test-specific options
  options(
    atlas.test.data.dir = file.path(tempdir(), "temp_data"),
    atlas.test.log.dir = file.path(tempdir(), "temp_logs"),
    atlas.test.report.dir = file.path(tempdir(), "temp_reports")
  )
}

# Cleanup test environment
cleanup_test_environment <- function() {
  # Remove temporary directories
  temp_base <- tempdir()
  temp_dirs <- c("temp_data", "temp_logs", "temp_reports", "temp_exports")
  
  for (dir in temp_dirs) {
    temp_path <- file.path(temp_base, dir)
    if (dir.exists(temp_path)) {
      unlink(temp_path, recursive = TRUE)
    }
  }
  
  # Reset options
  options(
    atlas.test.data.dir = NULL,
    atlas.test.log.dir = NULL,
    atlas.test.report.dir = NULL
  )
}

# Run setup
setup_test_environment()

# Ensure cleanup on exit
reg.finalizer(globalenv(), function(e) cleanup_test_environment(), onexit = TRUE)
'
  
  if (!dir.exists("tests/testthat")) {
    dir.create("tests/testthat", recursive = TRUE)
  }
  writeLines(setup_content, "tests/testthat/setup.R")
}

# ================================================================================
# TEST DATA CREATION HELPERS (tests/helpers/create_test_data.R)
# ================================================================================

create_test_data_helpers <- function() {
  helper_content <- '
# Atlas Labs HR Analytics - Test Data Creation Helpers

#\' Create standard test dataset
#\' @param n Number of employees to create
#\' @param seed Random seed for reproducible data
#\' @return data.frame with employee data
create_test_employee_data <- function(n = 1000, seed = 12345) {
  set.seed(seed)
  
  # Define realistic value ranges and distributions
  departments <- c("Sales", "Engineering", "HR", "Marketing", "Finance", "Operations")
  job_roles <- c("Manager", "Analyst", "Specialist", "Coordinator", "Director", "Associate")
  education_levels <- c("High School", "Bachelor", "Master", "PhD")
  states <- c("CA", "NY", "TX", "FL", "IL", "PA", "OH", "GA", "NC", "MI")
  ethnicities <- c("White", "Black", "Hispanic", "Asian", "Other")
  
  data.frame(
    EmployeeID = 1:n,
    FirstName = sample(c("John", "Jane", "Michael", "Sarah", "David", "Lisa", 
                        "Robert", "Emily", "William", "Ashley"), n, replace = TRUE),
    LastName = sample(c("Smith", "Johnson", "Williams", "Brown", "Jones", 
                       "Garcia", "Miller", "Davis", "Rodriguez", "Martinez"), n, replace = TRUE),
    Gender = sample(c("Male", "Female", "Non-binary"), n, replace = TRUE, 
                   prob = c(0.45, 0.45, 0.1)),
    Age = pmax(18, pmin(65, round(rnorm(n, mean = 35, sd = 10)))),
    BusinessTravel = sample(c("Travel_Rarely", "Travel_Frequently", "Non-Travel"), 
                           n, replace = TRUE, prob = c(0.6, 0.2, 0.2)),
    Department = sample(departments, n, replace = TRUE),
    DistanceFromHome = pmax(1, round(rgamma(n, shape = 2, rate = 0.1))),
    State = sample(states, n, replace = TRUE),
    Ethnicity = sample(ethnicities, n, replace = TRUE, 
                      prob = c(0.6, 0.15, 0.15, 0.08, 0.02)),
    Education = sample(1:4, n, replace = TRUE, prob = c(0.2, 0.5, 0.25, 0.05)),
    EducationField = sample(c("Life Sciences", "Medical", "Marketing", "Technical Degree", 
                             "Other", "Human Resources"), n, replace = TRUE),
    JobRole = sample(job_roles, n, replace = TRUE),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), n, replace = TRUE, 
                          prob = c(0.3, 0.6, 0.1)),
    Salary = pmax(30000, round(rnorm(n, mean = 65000, sd = 20000))),
    StockOptionLevel = sample(0:3, n, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
    OverTime = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.3, 0.7)),
    HireDate = sample(seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = "day"), 
                     n, replace = TRUE),
    Attrition = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.16, 0.84)),
    YearsAtCompany = pmax(0, round(runif(n, 0, 15))),
    YearsInMostRecentRole = pmax(0, round(runif(n, 0, 8))),
    YearsSinceLastPromotion = pmax(0, round(runif(n, 0, 10))),
    YearsWithCurrManager = pmax(0, round(runif(n, 0, 8))),
    stringsAsFactors = FALSE
  )
}

#\' Create performance rating test data
#\' @param employee_data Employee data to create ratings for
#\' @param ratings_per_employee Average number of ratings per employee
#\' @return data.frame with performance rating data
create_test_performance_data <- function(employee_data, ratings_per_employee = 2) {
  n_employees <- nrow(employee_data)
  n_ratings <- round(n_employees * ratings_per_employee)
  
  data.frame(
    PerformanceID = 1:n_ratings,
    EmployeeID = sample(employee_data$EmployeeID, n_ratings, replace = TRUE),
    ReviewDate = sample(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"), 
                       n_ratings, replace = TRUE),
    EnvironmentSatisfaction = sample(1:4, n_ratings, replace = TRUE, 
                                   prob = c(0.1, 0.2, 0.4, 0.3)),
    JobSatisfaction = sample(1:4, n_ratings, replace = TRUE, 
                           prob = c(0.1, 0.2, 0.4, 0.3)),
    RelationshipSatisfaction = sample(1:4, n_ratings, replace = TRUE, 
                                    prob = c(0.1, 0.2, 0.4, 0.3)),
    WorkLifeBalance = sample(1:4, n_ratings, replace = TRUE, 
                           prob = c(0.15, 0.25, 0.35, 0.25)),
    SelfRating = sample(1:5, n_ratings, replace = TRUE, 
                       prob = c(0.05, 0.15, 0.3, 0.35, 0.15)),
    ManagerRating = sample(1:5, n_ratings, replace = TRUE, 
                          prob = c(0.05, 0.15, 0.3, 0.35, 0.15)),
    TrainingOpportunitiesWithinYear = sample(0:6, n_ratings, replace = TRUE, 
                                           prob = c(0.1, 0.2, 0.3, 0.2, 0.1, 0.05, 0.05)),
    TrainingOpportunitiesTaken = sample(0:4, n_ratings, replace = TRUE, 
                                      prob = c(0.2, 0.3, 0.3, 0.15, 0.05)),
    stringsAsFactors = FALSE
  )
}

#\' Create education level lookup data
#\' @return data.frame with education level mappings
create_test_education_data <- function