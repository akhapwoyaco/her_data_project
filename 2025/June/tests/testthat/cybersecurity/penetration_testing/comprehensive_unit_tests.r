# =============================================================================
# COMPREHENSIVE UNIT TESTS FOR ATLAS LABS HR ANALYTICS DASHBOARD
# =============================================================================
# Author: akhapwoyaco
# Purpose: Extensive testing framework covering all app components
# Excludes: Cybersecurity testing and third-party integration security
# =============================================================================

# Required Libraries for Testing
library(testthat)
library(shiny)
library(shinycssloaders)
library(DT)
library(plotly)
library(tidyverse)
library(R6)
library(mockery)
library(htmltools)
library(reactlog)
library(profvis)
library(bench)
library(withr)

# =============================================================================
# TEST CONFIGURATION AND SETUP
# =============================================================================

# Test Configuration
test_config <- list(
  test_data_size = 1000,
  performance_threshold_ms = 5000,
  memory_threshold_mb = 100,
  ui_response_timeout = 30,
  large_dataset_size = 10000,
  stress_test_iterations = 100
)

# Mock Data Generator
generate_mock_employee_data <- function(n = 100) {
  set.seed(123)
  tibble(
    EmployeeID = 1:n,
    FirstName = sample(c("John", "Jane", "Mike", "Sarah", "David"), n, replace = TRUE),
    LastName = sample(c("Smith", "Johnson", "Williams", "Brown", "Jones"), n, replace = TRUE),
    Gender = sample(c("Male", "Female", "Non-binary"), n, replace = TRUE, prob = c(0.45, 0.45, 0.1)),
    Age = sample(22:65, n, replace = TRUE),
    BusinessTravel = sample(c("Travel_Rarely", "Travel_Frequently", "Non-Travel"), n, replace = TRUE),
    Department = sample(c("Sales", "R&D", "HR", "Finance", "IT"), n, replace = TRUE),
    DistanceFromHome = sample(1:50, n, replace = TRUE),
    State = sample(c("CA", "NY", "TX", "FL", "WA"), n, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), n, replace = TRUE),
    Education = sample(1:5, n, replace = TRUE),
    EducationField = sample(c("Engineering", "Business", "Marketing", "HR", "Finance"), n, replace = TRUE),
    JobRole = sample(c("Manager", "Developer", "Analyst", "Specialist", "Director"), n, replace = TRUE),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), n, replace = TRUE),
    Salary = sample(40000:150000, n, replace = TRUE),
    StockOptionLevel = sample(0:3, n, replace = TRUE),
    OverTime = sample(c("Yes", "No"), n, replace = TRUE),
    HireDate = sample(seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = "day"), n, replace = TRUE),
    Attrition = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.16, 0.84)),
    YearsAtCompany = sample(0:40, n, replace = TRUE),
    YearsInMostRecentRole = sample(0:20, n, replace = TRUE),
    YearsSinceLastPromotion = sample(0:15, n, replace = TRUE),
    YearsWithCurrManager = sample(0:20, n, replace = TRUE)
  )
}

generate_mock_performance_data <- function(n = 100) {
  set.seed(123)
  tibble(
    PerformanceID = 1:n,
    EmployeeID = sample(1:100, n, replace = TRUE),
    ReviewDate = sample(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"), n, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, n, replace = TRUE),
    JobSatisfaction = sample(1:5, n, replace = TRUE),
    RelationshipSatisfaction = sample(1:5, n, replace = TRUE),
    WorkLifeBalance = sample(1:5, n, replace = TRUE),
    SelfRating = sample(1:5, n, replace = TRUE),
    ManagerRating = sample(1:5, n, replace = TRUE),
    TrainingOpportunitiesWithinYear = sample(0:10, n, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:8, n, replace = TRUE)
  )
}

generate_mock_education_data <- function() {
  tibble(
    EducationLevelID = 1:5,
    EducationLevel = c("High School", "Bachelor's", "Master's", "PhD", "Professional")
  )
}

# =============================================================================
# 1. DATA LOADER MODULE TESTS
# =============================================================================

test_that("Data Loader Module - Basic Functionality", {
  
  # Test data loading with valid files
  test_that("loads valid CSV files correctly", {
    temp_dir <- tempdir()
    
    # Create temporary test files
    employee_file <- file.path(temp_dir, "employee.csv")
    performance_file <- file.path(temp_dir, "performance_rating.csv")
    education_file <- file.path(temp_dir, "education_level.csv")
    
    write_csv(generate_mock_employee_data(), employee_file)
    write_csv(generate_mock_performance_data(), performance_file)
    write_csv(generate_mock_education_data(), education_file)
    
    # Test loading function
    expect_true(file.exists(employee_file))
    expect_true(file.exists(performance_file))
    expect_true(file.exists(education_file))
    
    # Clean up
    unlink(c(employee_file, performance_file, education_file))
  })
  
  # Test error handling for missing files
  test_that("handles missing files gracefully", {
    expect_error(read_csv("nonexistent_file.csv"))
  })
  
  # Test data validation
  test_that("validates data structure and types", {
    mock_data <- generate_mock_employee_data()
    
    # Required columns exist
    required_cols <- c("EmployeeID", "FirstName", "LastName", "Gender", "Age", "Department", "Salary", "Attrition")
    expect_true(all(required_cols %in% names(mock_data)))
    
    # Data types are correct
    expect_true(is.numeric(mock_data$EmployeeID))
    expect_true(is.character(mock_data$FirstName))
    expect_true(is.numeric(mock_data$Age))
    expect_true(is.numeric(mock_data$Salary))
  })
  
  # Test data integrity checks
  test_that("performs data integrity validations", {
    mock_data <- generate_mock_employee_data()
    
    # No duplicate employee IDs
    expect_equal(length(unique(mock_data$EmployeeID)), nrow(mock_data))
    
    # Age within reasonable range
    expect_true(all(mock_data$Age >= 18 & mock_data$Age <= 70))
    
    # Salary within reasonable range
    expect_true(all(mock_data$Salary >= 20000 & mock_data$Salary <= 300000))
    
    # Required fields not missing
    expect_true(all(!is.na(mock_data$EmployeeID)))
    expect_true(all(!is.na(mock_data$Department)))
  })
})

test_that("Data Loader Module - Edge Cases", {
  
  # Test empty dataset
  test_that("handles empty datasets", {
    empty_data <- tibble()
    expect_equal(nrow(empty_data), 0)
    expect_warning(validate_data_structure(empty_data))
  })
  
  # Test datasets with missing columns
  test_that("handles missing required columns", {
    incomplete_data <- tibble(
      EmployeeID = 1:5,
      FirstName = c("John", "Jane", "Mike", "Sarah", "David")
      # Missing required columns
    )
    expect_error(validate_required_columns(incomplete_data))
  })
  
  # Test datasets with invalid data types
  test_that("handles invalid data types", {
    invalid_data <- generate_mock_employee_data()
    invalid_data$Age <- as.character(invalid_data$Age)  # Should be numeric
    
    expect_warning(validate_data_types(invalid_data))
  })
  
  # Test extremely large datasets
  test_that("handles large datasets efficiently", {
    large_data <- generate_mock_employee_data(n = test_config$large_dataset_size)
    
    start_time <- Sys.time()
    processed_data <- process_employee_data(large_data)
    end_time <- Sys.time()
    
    processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    expect_lt(processing_time, test_config$performance_threshold_ms / 1000)
  })
  
  # Test datasets with special characters and encoding
  test_that("handles special characters and encoding", {
    special_char_data <- generate_mock_employee_data()
    special_char_data$FirstName[1] <- "José"
    special_char_data$LastName[1] <- "Müller"
    special_char_data$Department[1] <- "R&D"
    
    expect_no_error(process_employee_data(special_char_data))
  })
  
  # Test datasets with extreme values
  test_that("handles extreme values appropriately", {
    extreme_data <- generate_mock_employee_data()
    extreme_data$Age[1] <- 100  # Extreme age
    extreme_data$Salary[1] <- 1000000  # Extreme salary
    extreme_data$YearsAtCompany[1] <- 50  # Extreme tenure
    
    validated_data <- validate_data_ranges(extreme_data)
    expect_true(all(validated_data$Age <= 70))  # Should be capped or flagged
  })
})

# =============================================================================
# 2. LOGGER MODULE TESTS (R6 CLASS)
# =============================================================================

test_that("Logger Module - R6 Class Functionality", {
  
  # Test logger initialization
  test_that("initializes logger correctly", {
    logger <- AtlasLogger$new()
    expect_r6(logger, "AtlasLogger")
    expect_true(is.list(logger$logs))
    expect_true(length(logger$logs) == 0)
  })
  
  # Test basic logging functions
  test_that("logs messages with different levels", {
    logger <- AtlasLogger$new()
    
    logger$log_info("Test info message", "test_module")
    logger$log_warning("Test warning message", "test_module")
    logger$log_error("Test error message", "test_module")
    
    expect_equal(length(logger$logs), 3)
    expect_equal(logger$logs[[1]]$level, "INFO")
    expect_equal(logger$logs[[2]]$level, "WARNING")
    expect_equal(logger$logs[[3]]$level, "ERROR")
  })
  
  # Test performance tracking
  test_that("tracks performance metrics", {
    logger <- AtlasLogger$new()
    
    performance_data <- list(
      execution_time = 1.5,
      memory_usage = 50,
      cpu_usage = 25
    )
    
    logger$log_info("Performance test", "test_module", performance_data)
    
    expect_true("performance_data" %in% names(logger$logs[[1]]))
    expect_equal(logger$logs[[1]]$performance_data$execution_time, 1.5)
  })
  
  # Test memory usage tracking
  test_that("tracks memory usage accurately", {
    logger <- AtlasLogger$new()
    
    initial_memory <- logger$get_memory_usage()
    
    # Create some data to increase memory usage
    large_data <- generate_mock_employee_data(n = 1000)
    
    current_memory <- logger$get_memory_usage()
    expect_gte(current_memory, initial_memory)
    
    logger$track_memory_usage("test_module")
    expect_true(length(logger$logs) > 0)
  })
  
  # Test execution time tracking
  test_that("tracks execution time accurately", {
    logger <- AtlasLogger$new()
    
    start_time <- Sys.time()
    Sys.sleep(0.1)  # Simulate some work
    end_time <- Sys.time()
    
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    logger$track_execution_time("test_function", execution_time, "test_module")
    
    expect_true(length(logger$logs) > 0)
    expect_true(logger$logs[[1]]$performance_data$execution_time >= 0.1)
  })
  
  # Test log filtering and retrieval
  test_that("filters and retrieves logs correctly", {
    logger <- AtlasLogger$new()
    
    logger$log_info("Info message 1", "module_a")
    logger$log_warning("Warning message 1", "module_b")
    logger$log_error("Error message 1", "module_a")
    logger$log_info("Info message 2", "module_b")
    
    # Filter by level
    error_logs <- logger$get_logs_by_level("ERROR")
    expect_equal(length(error_logs), 1)
    
    # Filter by module
    module_a_logs <- logger$get_logs_by_module("module_a")
    expect_equal(length(module_a_logs), 2)
    
    # Get performance summary
    performance_summary <- logger$get_performance_summary()
    expect_true(is.list(performance_summary))
  })
})

test_that("Logger Module - Edge Cases and Stress Testing", {
  
  # Test high-volume logging
  test_that("handles high-volume logging efficiently", {
    logger <- AtlasLogger$new()
    
    start_time <- Sys.time()
    
    for (i in 1:test_config$stress_test_iterations) {
      logger$log_info(paste("Message", i), "stress_test_module")
    }
    
    end_time <- Sys.time()
    logging_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    expect_equal(length(logger$logs), test_config$stress_test_iterations)
    expect_lt(logging_time, 10)  # Should complete within 10 seconds
  })
  
  # Test memory management with large logs
  test_that("manages memory efficiently with large logs", {
    logger <- AtlasLogger$new()
    
    initial_memory <- logger$get_memory_usage()
    
    # Generate many large log entries
    for (i in 1:100) {
      large_message <- paste(rep("Large log entry", 100), collapse = " ")
      logger$log_info(large_message, "memory_test_module")
    }
    
    current_memory <- logger$get_memory_usage()
    memory_increase <- current_memory - initial_memory
    
    expect_lt(memory_increase, test_config$memory_threshold_mb)
  })
  
  # Test concurrent logging (simulated)
  test_that("handles concurrent logging operations", {
    logger <- AtlasLogger$new()
    
    # Simulate concurrent logging
    concurrent_logs <- function(prefix) {
      for (i in 1:10) {
        logger$log_info(paste(prefix, "Message", i), "concurrent_module")
      }
    }
    
    concurrent_logs("Thread1")
    concurrent_logs("Thread2")
    concurrent_logs("Thread3")
    
    expect_equal(length(logger$logs), 30)
  })
  
  # Test log rotation and cleanup
  test_that("performs log rotation and cleanup", {
    logger <- AtlasLogger$new()
    
    # Fill logger with many entries
    for (i in 1:1000) {
      logger$log_info(paste("Log entry", i), "rotation_test")
    }
    
    initial_count <- length(logger$logs)
    
    # Test log rotation
    logger$rotate_logs(max_entries = 500)
    
    expect_lte(length(logger$logs), 500)
    expect_lt(length(logger$logs), initial_count)
  })
})

# =============================================================================
# 3. UI MODULE TESTS
# =============================================================================

test_that("UI Module Tests - Component Rendering", {
  
  # Test sidebar module UI
  test_that("renders sidebar module correctly", {
    ui_output <- sidebarUI("test_sidebar")
    
    expect_s3_class(ui_output, "shiny.tag")
    expect_true(length(ui_output$children) > 0)
  })
  
  # Test overview module UI
  test_that("renders overview module correctly", {
    ui_output <- overviewUI("test_overview")
    
    expect_s3_class(ui_output, "shiny.tag")
    expect_true(any(grepl("overview", ui_output$attribs$class)))
  })
  
  # Test all module UIs
  test_that("renders all module UIs without errors", {
    modules <- c("attrition", "demographics", "performance", "compensation", "satisfaction")
    
    for (module in modules) {
      ui_function <- get(paste0(module, "UI"))
      ui_output <- ui_function(paste0("test_", module))
      
      expect_s3_class(ui_output, "shiny.tag")
      expect_true(length(ui_output$children) > 0)
    }
  })
  
  # Test responsive design elements
  test_that("includes responsive design classes", {
    ui_output <- overviewUI("test_responsive")
    ui_html <- as.character(ui_output)
    
    # Check for Bootstrap responsive classes
    expect_true(grepl("col-", ui_html))
    expect_true(grepl("row", ui_html))
  })
  
  # Test accessibility attributes
  test_that("includes accessibility attributes", {
    ui_output <- sidebarUI("test_accessibility")
    ui_html <- as.character(ui_output)
    
    # Check for accessibility attributes
    expect_true(grepl("aria-", ui_html) || grepl("role=", ui_html))
  })
})

test_that("UI Module Tests - Interactive Elements", {
  
  # Test input validation
  test_that("validates user inputs correctly", {
    # Test date range validation
    start_date <- as.Date("2023-01-01")
    end_date <- as.Date("2022-12-31")  # Invalid: end before start
    
    expect_error(validate_date_range(start_date, end_date))
    
    # Test numeric input validation
    expect_error(validate_numeric_input("not_a_number"))
    expect_true(validate_numeric_input(42))
  })
  
  # Test filter combinations
  test_that("handles complex filter combinations", {
    filters <- list(
      department = c("Sales", "R&D"),
      age_range = c(25, 45),
      salary_range = c(50000, 100000),
      attrition = "No"
    )
    
    mock_data <- generate_mock_employee_data()
    filtered_data <- apply_filters(mock_data, filters)
    
    expect_lt(nrow(filtered_data), nrow(mock_data))
    expect_true(all(filtered_data$Department %in% filters$department))
    expect_true(all(filtered_data$Attrition == filters$attrition))
  })
  
  # Test dynamic UI updates
  test_that("updates UI dynamically based on data", {
    mock_data <- generate_mock_employee_data()
    
    # Test department choices update
    departments <- unique(mock_data$Department)
    expect_true(length(departments) > 0)
    
    # Test age range updates
    age_range <- range(mock_data$Age)
    expect_true(age_range[2] > age_range[1])
  })
})

# =============================================================================
# 4. VISUALIZATION TESTS
# =============================================================================

test_that("Visualization Tests - Chart Generation", {
  
  # Test ggplot2 chart generation
  test_that("generates ggplot2 charts correctly", {
    mock_data <- generate_mock_employee_data()
    
    # Test attrition chart
    attrition_chart <- create_attrition_chart(mock_data)
    expect_s3_class(attrition_chart, "ggplot")
    
    # Test demographics chart
    demographics_chart <- create_demographics_chart(mock_data)
    expect_s3_class(demographics_chart, "ggplot")
    
    # Test salary distribution chart
    salary_chart <- create_salary_distribution_chart(mock_data)
    expect_s3_class(salary_chart, "ggplot")
  })
  
  # Test plotly interactive charts
  test_that("generates plotly interactive charts", {
    mock_data <- generate_mock_employee_data()
    
    # Test interactive scatter plot
    scatter_plot <- create_interactive_scatter_plot(mock_data)
    expect_s3_class(scatter_plot, "plotly")
    
    # Test interactive bar chart
    bar_chart <- create_interactive_bar_chart(mock_data)
    expect_s3_class(bar_chart, "plotly")
  })
  
  # Test custom themes
  test_that("applies custom themes correctly", {
    mock_data <- generate_mock_employee_data()
    
    chart_with_theme <- create_attrition_chart(mock_data) + atlas_theme()
    expect_s3_class(chart_with_theme, "ggplot")
    
    # Check if theme is applied
    theme_elements <- chart_with_theme$theme
    expect_true(length(theme_elements) > 0)
  })
  
  # Test chart data validation
  test_that("validates chart data before visualization", {
    # Test with empty data
    empty_data <- tibble()
    expect_error(create_attrition_chart(empty_data))
    
    # Test with missing required columns
    incomplete_data <- tibble(EmployeeID = 1:5)
    expect_error(create_demographics_chart(incomplete_data))
  })
})

test_that("Visualization Tests - Edge Cases", {
  
  # Test with single data point
  test_that("handles single data point gracefully", {
    single_point_data <- generate_mock_employee_data(n = 1)
    
    chart <- create_attrition_chart(single_point_data)
    expect_s3_class(chart, "ggplot")
  })
  
  # Test with extreme values
  test_that("handles extreme values in visualizations", {
    extreme_data <- generate_mock_employee_data()
    extreme_data$Salary[1] <- 1000000  # Extreme salary
    extreme_data$Age[1] <- 100  # Extreme age
    
    salary_chart <- create_salary_distribution_chart(extreme_data)
    expect_s3_class(salary_chart, "ggplot")
  })
  
  # Test with all identical values
  test_that("handles identical values across dataset", {
    identical_data <- generate_mock_employee_data()
    identical_data$Department <- "Sales"  # All same department
    identical_data$Attrition <- "No"  # All same attrition
    
    chart <- create_attrition_chart(identical_data)
    expect_s3_class(chart, "ggplot")
  })
  
  # Test with missing/NA values
  test_that("handles missing values in visualizations", {
    na_data <- generate_mock_employee_data()
    na_data$Salary[1:5] <- NA
    na_data$Age[6:10] <- NA
    
    salary_chart <- create_salary_distribution_chart(na_data)
    expect_s3_class(salary_chart, "ggplot")
  })
})

# =============================================================================
# 5. DATA ANALYSIS MODULE TESTS
# =============================================================================

test_that("Data Analysis Tests - Statistical Calculations", {
  
  # Test attrition analysis
  test_that("calculates attrition metrics correctly", {
    mock_data <- generate_mock_employee_data()
    
    attrition_rate <- calculate_attrition_rate(mock_data)
    expect_true(is.numeric(attrition_rate))
    expect_true(attrition_rate >= 0 && attrition_rate <= 1)
    
    # Test attrition by department
    dept_attrition <- calculate_attrition_by_department(mock_data)
    expect_true(is.data.frame(dept_attrition))
    expect_true("Department" %in% names(dept_attrition))
    expect_true("AttritionRate" %in% names(dept_attrition))
  })
  
  # Test satisfaction analysis
  test_that("calculates satisfaction metrics correctly", {
    mock_performance <- generate_mock_performance_data()
    
    avg_satisfaction <- calculate_average_satisfaction(mock_performance)
    expect_true(is.numeric(avg_satisfaction))
    expect_true(avg_satisfaction >= 1 && avg_satisfaction <= 5)
    
    # Test satisfaction correlation
    satisfaction_corr <- calculate_satisfaction_correlation(mock_performance)
    expect_true(is.matrix(satisfaction_corr))
  })
  
  # Test performance analysis
  test_that("analyzes performance metrics correctly", {
    mock_performance <- generate_mock_performance_data()
    
    performance_summary <- analyze_performance_ratings(mock_performance)
    expect_true(is.list(performance_summary))
    expect_true("self_rating_avg" %in% names(performance_summary))
    expect_true("manager_rating_avg" %in% names(performance_summary))
  })
  
  # Test compensation analysis
  test_that("analyzes compensation correctly", {
    mock_data <- generate_mock_employee_data()
    
    salary_stats <- analyze_salary_distribution(mock_data)
    expect_true(is.list(salary_stats))
    expect_true("mean" %in% names(salary_stats))
    expect_true("median" %in% names(salary_stats))
    expect_true("sd" %in% names(salary_stats))
    
    # Test pay equity analysis
    pay_equity <- analyze_pay_equity(mock_data)
    expect_true(is.data.frame(pay_equity))
  })
})

test_that("Data Analysis Tests - Advanced Analytics", {
  
  # Test predictive modeling
  test_that("performs predictive modeling correctly", {
    mock_data <- generate_mock_employee_data(n = 500)  # Larger dataset for modeling
    
    # Test attrition prediction model
    attrition_model <- build_attrition_model(mock_data)
    expect_true(class(attrition_model)[1] %in% c("glm", "randomForest", "lm"))
    
    # Test model predictions
    predictions <- predict_attrition_risk(attrition_model, mock_data)
    expect_equal(length(predictions), nrow(mock_data))
    expect_true(all(predictions >= 0 & predictions <= 1))
  })
  
  # Test clustering analysis
  test_that("performs clustering analysis correctly", {
    mock_data <- generate_mock_employee_data()
    
    # Test employee clustering
    clusters <- perform_employee_clustering(mock_data)
    expect_true(is.numeric(clusters))
    expect_equal(length(clusters), nrow(mock_data))
    expect_true(all(clusters > 0))
  })
  
  # Test time series analysis
  test_that("performs time series analysis correctly", {
    mock_data <- generate_mock_employee_data()
    
    # Test hiring trends
    hiring_trends <- analyze_hiring_trends(mock_data)
    expect_true(is.data.frame(hiring_trends))
    expect_true("HireDate" %in% names(hiring_trends))
    expect_true("HiringCount" %in% names(hiring_trends))
  })
})

# =============================================================================
# 6. PERFORMANCE TESTS
# =============================================================================

test_that("Performance Tests - Execution Time", {
  
  # Test data loading performance
  test_that("loads data within acceptable time limits", {
    large_data <- generate_mock_employee_data(n = test_config$large_dataset_size)
    
    execution_time <- system.time({
      processed_data <- process_employee_data(large_data)
    })
    
    expect_lt(execution_time["elapsed"], test_config$performance_threshold_ms / 1000)
  })
  
  # Test visualization performance
  test_that("generates visualizations within time limits", {
    mock_data <- generate_mock_employee_data(n = 1000)
    
    chart_time <- system.time({
      chart <- create_attrition_chart(mock_data)
    })
    
    expect_lt(chart_time["elapsed"], 5)  # Should complete within 5 seconds
  })
  
  # Test analysis performance
  test_that("performs analysis within time limits", {
    mock_data <- generate_mock_employee_data(n = 5000)
    
    analysis_time <- system.time({
      results <- analyze_attrition_factors(mock_data)
    })
    
    expect_lt(analysis_time["elapsed"], 10)  # Should complete within 10 seconds
  })
})

test_that("Performance Tests - Memory Usage", {
  
  # Test memory efficiency
  test_that("manages memory efficiently", {
    initial_memory <- pryr::mem_used()
    
    # Create and process large dataset
    large_data <- generate_mock_employee_data(n = 10000)
    processed_data <- process_employee_data(large_data)
    
    peak_memory <- pryr::mem_used()
    memory_increase <- as.numeric(peak_memory - initial_memory)
    
    # Clean up
    rm(large_data, processed_data)
    gc()
    
    expect_lt(memory_increase, test_config$memory_threshold_mb * 1024^2)
  })
  
  # Test memory leaks
  test_that("prevents memory leaks", {
    initial_memory <- pryr::mem_used()
    
    # Perform repeated operations
    for (i in 1:50) {
      temp_data <- generate_mock_employee_data(n = 100)
      temp_chart <- create_attrition_chart(temp_data)
      rm(temp_data, temp_chart)
    }
    
    gc()  # Force garbage collection
    final_memory <- pryr::mem_used()
    
    memory_difference <- as.numeric(final_memory - initial_memory)
    expect_lt(memory_difference, 50 * 1024^2)  # Should not increase significantly
  })
})

# =============================================================================
# 7. REACTIVE SYSTEM TESTS
# =============================================================================

test_that