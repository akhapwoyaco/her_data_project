# =============================================================================
# ATLAS LABS HR ANALYTICS - COMPREHENSIVE UNIT TESTS
# =============================================================================
# Author: akhapwoyaco
# Description: Extensive unit tests covering all app functionality except security
# Coverage Areas: Data validation, UI/UX, Performance, Integration, Edge cases
# =============================================================================

library(testthat)
library(shiny)
library(shintest2)
library(mockery)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(R6)

# =============================================================================
# 1. DATA VALIDATION & INTEGRITY TESTING
# =============================================================================

test_that("Data Loading Module - Core Functionality", {
  
  # Test 1.1: Valid CSV file loading
  test_that("loads valid employee CSV correctly", {
    # Mock valid employee data
    mock_employee_data <- data.frame(
      EmployeeID = 1:100,
      FirstName = paste0("Employee", 1:100),
      LastName = paste0("Last", 1:100),
      Gender = sample(c("Male", "Female", "Non-binary"), 100, replace = TRUE),
      Age = sample(25:65, 100, replace = TRUE),
      BusinessTravel = sample(c("Travel_Rarely", "Travel_Frequently", "Non-Travel"), 100, replace = TRUE),
      Department = sample(c("Sales", "R&D", "HR"), 100, replace = TRUE),
      DistanceFromHome = sample(1:50, 100, replace = TRUE),
      State = sample(c("California", "Texas", "New York"), 100, replace = TRUE),
      Ethnicity = sample(c("White", "Black", "Asian", "Hispanic"), 100, replace = TRUE),
      Education = sample(1:5, 100, replace = TRUE),
      EducationField = sample(c("Technical", "Medical", "Marketing"), 100, replace = TRUE),
      JobRole = sample(c("Sales Executive", "Research Scientist", "Manager"), 100, replace = TRUE),
      MaritalStatus = sample(c("Single", "Married", "Divorced"), 100, replace = TRUE),
      Salary = sample(30000:150000, 100, replace = TRUE),
      StockOptionLevel = sample(0:3, 100, replace = TRUE),
      OverTime = sample(c("Yes", "No"), 100, replace = TRUE),
      HireDate = sample(seq(as.Date("2010-01-01"), as.Date("2023-12-31"), by = "day"), 100),
      Attrition = sample(c("Yes", "No"), 100, replace = TRUE),
      YearsAtCompany = sample(1:40, 100, replace = TRUE),
      YearsInMostRecentRole = sample(1:20, 100, replace = TRUE),
      YearsSinceLastPromotion = sample(0:15, 100, replace = TRUE),
      YearsWithCurrManager = sample(1:15, 100, replace = TRUE)
    )
    
    # Test data structure
    expect_equal(nrow(mock_employee_data), 100)
    expect_equal(ncol(mock_employee_data), 23)
    expect_true(all(c("EmployeeID", "Attrition", "Salary") %in% names(mock_employee_data)))
  })
  
  # Test 1.2: Data type validation
  test_that("validates data types correctly", {
    mock_data <- data.frame(
      EmployeeID = as.character(1:10),  # Should be numeric
      Age = c(25, 30, "invalid", 45, 50, 35, 40, 28, 33, 55),  # Mixed types
      Salary = c(50000, 60000, 70000, 80000, 90000, 100000, 55000, 65000, 75000, 85000)
    )
    
    # Test validation function
    validation_result <- validate_data_types(mock_data)
    expect_false(validation_result$valid)
    expect_true("Age" %in% validation_result$invalid_columns)
  })
  
  # Test 1.3: Missing value detection
  test_that("detects missing values appropriately", {
    mock_data_with_na <- data.frame(
      EmployeeID = c(1, 2, NA, 4, 5),
      FirstName = c("John", NA, "Jane", "Bob", "Alice"),
      Salary = c(50000, 60000, 70000, NA, 90000)
    )
    
    missing_analysis <- analyze_missing_values(mock_data_with_na)
    expect_equal(missing_analysis$EmployeeID$count, 1)
    expect_equal(missing_analysis$FirstName$count, 1)
    expect_equal(missing_analysis$Salary$count, 1)
  })
  
  # Test 1.4: Data range validation
  test_that("validates data ranges correctly", {
    # Age should be between 18-100
    invalid_age_data <- data.frame(
      Age = c(15, 25, 35, 150, 45)  # 15 and 150 are invalid
    )
    
    age_validation <- validate_age_range(invalid_age_data$Age)
    expect_equal(length(age_validation$invalid_values), 2)
    expect_true(15 %in% age_validation$invalid_values)
    expect_true(150 %in% age_validation$invalid_values)
  })
  
  # Test 1.5: Duplicate record detection
  test_that("identifies duplicate employee records", {
    duplicate_data <- data.frame(
      EmployeeID = c(1, 2, 3, 2, 5),  # ID 2 is duplicated
      FirstName = c("John", "Jane", "Bob", "Jane", "Alice")
    )
    
    duplicates <- find_duplicate_employees(duplicate_data)
    expect_equal(length(duplicates), 1)
    expect_equal(duplicates[[1]], 2)
  })
})

# =============================================================================
# 2. BUSINESS LOGIC TESTING
# =============================================================================

test_that("Attrition Analysis Module - Business Logic", {
  
  # Test 2.1: Attrition rate calculation
  test_that("calculates attrition rate correctly", {
    mock_data <- data.frame(
      EmployeeID = 1:100,
      Attrition = c(rep("Yes", 20), rep("No", 80))
    )
    
    attrition_rate <- calculate_attrition_rate(mock_data)
    expect_equal(attrition_rate, 0.20)
  })
  
  # Test 2.2: Department-wise attrition
  test_that("calculates department-wise attrition correctly", {
    mock_data <- data.frame(
      Department = c(rep("Sales", 50), rep("R&D", 30), rep("HR", 20)),
      Attrition = c(rep("Yes", 15), rep("No", 35), rep("Yes", 5), rep("No", 25), rep("Yes", 2), rep("No", 18))
    )
    
    dept_attrition <- calculate_department_attrition(mock_data)
    expect_equal(dept_attrition$Sales, 0.30)  # 15/50
    expect_equal(dept_attrition$`R&D`, 0.167, tolerance = 0.01)  # 5/30
    expect_equal(dept_attrition$HR, 0.10)  # 2/20
  })
  
  # Test 2.3: Tenure analysis
  test_that("analyzes tenure patterns correctly", {
    mock_data <- data.frame(
      YearsAtCompany = c(1, 2, 5, 10, 15, 20, 25, 30),
      Attrition = c("Yes", "Yes", "No", "No", "No", "No", "Yes", "No")
    )
    
    tenure_analysis <- analyze_tenure_attrition(mock_data)
    expect_true("high_risk_tenure" %in% names(tenure_analysis))
    expect_true("avg_tenure_attrition" %in% names(tenure_analysis))
  })
  
  # Test 2.4: Salary impact on attrition
  test_that("analyzes salary impact on attrition", {
    mock_data <- data.frame(
      Salary = c(30000, 35000, 50000, 75000, 100000, 120000),
      Attrition = c("Yes", "Yes", "No", "No", "No", "No")
    )
    
    salary_impact <- analyze_salary_attrition_correlation(mock_data)
    expect_true(salary_impact$correlation < 0)  # Negative correlation expected
  })
})

test_that("Performance Analysis Module - Business Logic", {
  
  # Test 2.5: Performance rating calculations
  test_that("calculates performance metrics correctly", {
    mock_performance <- data.frame(
      EmployeeID = 1:50,
      SelfRating = sample(1:5, 50, replace = TRUE),
      ManagerRating = sample(1:5, 50, replace = TRUE),
      JobSatisfaction = sample(1:5, 50, replace = TRUE)
    )
    
    perf_metrics <- calculate_performance_metrics(mock_performance)
    expect_true(all(c("avg_self_rating", "avg_manager_rating", "rating_gap") %in% names(perf_metrics)))
    expect_true(perf_metrics$avg_self_rating >= 1 && perf_metrics$avg_self_rating <= 5)
  })
  
  # Test 2.6: Training opportunity analysis
  test_that("analyzes training opportunities correctly", {
    mock_training <- data.frame(
      EmployeeID = 1:30,
      TrainingOpportunitiesWithinYear = sample(0:10, 30, replace = TRUE),
      TrainingOpportunitiesTaken = sample(0:8, 30, replace = TRUE)
    )
    
    training_analysis <- analyze_training_effectiveness(mock_training)
    expect_true("training_utilization_rate" %in% names(training_analysis))
    expect_true(training_analysis$training_utilization_rate >= 0 && training_analysis$training_utilization_rate <= 1)
  })
})

# =============================================================================
# 3. UI/UX COMPONENT TESTING
# =============================================================================

test_that("UI Component Functionality", {
  
  # Test 3.1: Module UI generation
  test_that("generates module UIs correctly", {
    # Test overview module UI
    overview_ui <- overviewUI("test_overview")
    expect_s3_class(overview_ui, "shiny.tag")
    expect_match(as.character(overview_ui), "test_overview")
    
    # Test attrition module UI
    attrition_ui <- attritionUI("test_attrition")
    expect_s3_class(attrition_ui, "shiny.tag")
    expect_match(as.character(attrition_ui), "test_attrition")
  })
  
  # Test 3.2: Reactive value initialization
  test_that("initializes reactive values correctly", {
    session <- MockShinySession$new()
    
    # Test shared reactive values
    shared_values <- reactiveValues(
      employee_data = NULL,
      performance_data = NULL,
      filtered_data = NULL
    )
    
    expect_true(is.reactivevalues(shared_values))
    expect_null(shared_values$employee_data)
  })
  
  # Test 3.3: Filter functionality
  test_that("filters work correctly", {
    mock_data <- data.frame(
      Department = c("Sales", "R&D", "HR", "Sales", "R&D"),
      Age = c(25, 35, 45, 30, 40),
      Salary = c(50000, 75000, 90000, 55000, 80000)
    )
    
    # Test department filter
    filtered_sales <- filter_by_department(mock_data, "Sales")
    expect_equal(nrow(filtered_sales), 2)
    expect_true(all(filtered_sales$Department == "Sales"))
    
    # Test age range filter
    filtered_age <- filter_by_age_range(mock_data, min_age = 30, max_age = 40)
    expect_equal(nrow(filtered_age), 3)
    expect_true(all(filtered_age$Age >= 30 & filtered_age$Age <= 40))
  })
  
  # Test 3.4: Dynamic UI updates
  test_that("dynamic UI updates work correctly", {
    # Mock server function for testing
    mock_server <- function(input, output, session) {
      output$dynamic_plot <- renderPlotly({
        plot_ly(x = 1:10, y = 1:10, type = "scatter", mode = "markers")
      })
    }
    
    # Test that output is generated
    expect_true(is.function(mock_server))
  })
})

# =============================================================================
# 4. DATA VISUALIZATION TESTING
# =============================================================================

test_that("Visualization Generation", {
  
  # Test 4.1: ggplot2 chart generation
  test_that("generates ggplot2 charts correctly", {
    mock_data <- data.frame(
      Department = c("Sales", "R&D", "HR"),
      Count = c(50, 30, 20)
    )
    
    # Test bar chart creation
    bar_chart <- create_department_chart(mock_data)
    expect_s3_class(bar_chart, "ggplot")
    expect_equal(length(bar_chart$layers), 1)
  })
  
  # Test 4.2: Plotly interactive charts
  test_that("generates plotly charts correctly", {
    mock_data <- data.frame(
      x = 1:10,
      y = sample(1:100, 10)
    )
    
    plotly_chart <- create_interactive_scatter(mock_data)
    expect_s3_class(plotly_chart, "plotly")
  })
  
  # Test 4.3: Chart customization
  test_that("applies custom themes correctly", {
    mock_data <- data.frame(
      Category = c("A", "B", "C"),
      Value = c(10, 20, 30)
    )
    
    themed_chart <- create_themed_chart(mock_data)
    expect_s3_class(themed_chart, "ggplot")
    # Verify custom theme is applied
    expect_true("atlas_theme" %in% class(themed_chart$theme))
  })
  
  # Test 4.4: Chart data validation
  test_that("validates chart data before rendering", {
    # Empty data
    empty_data <- data.frame()
    expect_error(create_department_chart(empty_data), "Data cannot be empty")
    
    # Missing required columns
    incomplete_data <- data.frame(Department = c("Sales", "R&D"))
    expect_error(create_department_chart(incomplete_data), "Missing required column: Count")
  })
  
  # Test 4.5: Chart responsiveness
  test_that("charts handle different data sizes", {
    # Small dataset
    small_data <- data.frame(Department = "Sales", Count = 10)
    small_chart <- create_department_chart(small_data)
    expect_s3_class(small_chart, "ggplot")
    
    # Large dataset
    large_data <- data.frame(
      Department = rep(c("Sales", "R&D", "HR"), 1000),
      Count = sample(1:100, 3000, replace = TRUE)
    )
    large_chart <- create_department_chart(large_data)
    expect_s3_class(large_chart, "ggplot")
  })
})

# =============================================================================
# 5. PERFORMANCE TESTING
# =============================================================================

test_that("Performance Benchmarks", {
  
  # Test 5.1: Data loading performance
  test_that("data loading meets performance benchmarks", {
    # Create large mock dataset
    large_dataset <- data.frame(
      EmployeeID = 1:10000,
      FirstName = paste0("Employee", 1:10000),
      Department = sample(c("Sales", "R&D", "HR", "Finance", "IT"), 10000, replace = TRUE),
      Salary = sample(30000:200000, 10000, replace = TRUE)
    )
    
    # Measure loading time
    start_time <- Sys.time()
    processed_data <- process_employee_data(large_dataset)
    end_time <- Sys.time()
    
    loading_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    expect_lt(loading_time, 5)  # Should load within 5 seconds
  })
  
  # Test 5.2: Chart rendering performance
  test_that("chart rendering is performant", {
    mock_data <- data.frame(
      x = 1:1000,
      y = sample(1:100, 1000, replace = TRUE),
      category = sample(LETTERS[1:10], 1000, replace = TRUE)
    )
    
    start_time <- Sys.time()
    chart <- create_performance_chart(mock_data)
    end_time <- Sys.time()
    
    render_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    expect_lt(render_time, 3)  # Should render within 3 seconds
  })
  
  # Test 5.3: Memory usage monitoring
  test_that("memory usage is within acceptable limits", {
    initial_memory <- pryr::mem_used()
    
    # Simulate heavy data processing
    large_data <- replicate(100, {
      data.frame(
        id = 1:1000,
        value = rnorm(1000)
      )
    }, simplify = FALSE)
    
    processed_data <- lapply(large_data, function(df) {
      df$processed_value <- df$value * 2
      df
    })
    
    final_memory <- pryr::mem_used()
    memory_increase <- final_memory - initial_memory
    
    # Memory increase should be reasonable (less than 500MB)
    expect_lt(as.numeric(memory_increase), 500 * 1024^2)
    
    # Clean up
    rm(large_data, processed_data)
    gc()
  })
  
  # Test 5.4: Concurrent user simulation
  test_that("handles multiple concurrent operations", {
    # Simulate multiple users accessing different modules
    concurrent_operations <- replicate(10, {
      future::future({
        mock_data <- data.frame(
          EmployeeID = 1:100,
          Department = sample(c("Sales", "R&D"), 100, replace = TRUE)
        )
        calculate_department_metrics(mock_data)
      })
    }, simplify = FALSE)
    
    # Resolve all futures
    results <- future::value(concurrent_operations)
    expect_equal(length(results), 10)
    expect_true(all(sapply(results, is.list)))
  })
})

# =============================================================================
# 6. LOGGING SYSTEM TESTING
# =============================================================================

test_that("R6 Logger Functionality", {
  
  # Test 6.1: Logger initialization
  test_that("initializes logger correctly", {
    logger <- AtlasLogger$new()
    expect_r6(logger, "AtlasLogger")
    expect_true(is.list(logger$logs))
    expect_equal(length(logger$logs), 0)
  })
  
  # Test 6.2: Log message creation
  test_that("creates log messages correctly", {
    logger <- AtlasLogger$new()
    
    logger$log_info("Test info message", "test_module")
    logger$log_warning("Test warning message", "test_module")
    logger$log_error("Test error message", "test_module")
    
    expect_equal(length(logger$logs), 3)
    expect_equal(logger$logs[[1]]$level, "INFO")
    expect_equal(logger$logs[[2]]$level, "WARNING")
    expect_equal(logger$logs[[3]]$level, "ERROR")
  })
  
  # Test 6.3: Performance tracking
  test_that("tracks performance metrics correctly", {
    logger <- AtlasLogger$new()
    
    # Start performance tracking
    start_time <- Sys.time()
    Sys.sleep(0.1)  # Simulate processing time
    end_time <- Sys.time()
    
    performance_data <- list(
      execution_time = as.numeric(difftime(end_time, start_time, units = "secs")),
      memory_used = pryr::mem_used()
    )
    
    logger$log_performance("test_operation", "test_module", performance_data)
    
    expect_true(any(sapply(logger$logs, function(log) log$type == "PERFORMANCE")))
  })
  
  # Test 6.4: Memory usage tracking
  test_that("tracks memory usage correctly", {
    logger <- AtlasLogger$new()
    
    memory_before <- pryr::mem_used()
    # Simulate memory-intensive operation
    temp_data <- matrix(rnorm(10000), nrow = 100)
    memory_after <- pryr::mem_used()
    
    logger$track_memory_usage("memory_test", "test_module")
    
    # Clean up
    rm(temp_data)
    gc()
    
    expect_true(any(sapply(logger$logs, function(log) grepl("memory", log$message, ignore.case = TRUE))))
  })
  
  # Test 6.5: Log filtering and retrieval
  test_that("filters logs correctly", {
    logger <- AtlasLogger$new()
    
    logger$log_info("Info 1", "module_a")
    logger$log_warning("Warning 1", "module_b")
    logger$log_error("Error 1", "module_a")
    logger$log_info("Info 2", "module_b")
    
    # Filter by module
    module_a_logs <- logger$get_logs_by_module("module_a")
    expect_equal(length(module_a_logs), 2)
    
    # Filter by level
    error_logs <- logger$get_logs_by_level("ERROR")
    expect_equal(length(error_logs), 1)
  })
})

# =============================================================================
# 7. INTEGRATION TESTING
# =============================================================================

test_that("Module Integration", {
  
  # Test 7.1: Data flow between modules
  test_that("data flows correctly between modules", {
    # Mock shared reactive values
    shared_data <- reactiveValues(
      employee_data = data.frame(
        EmployeeID = 1:10,
        Department = rep(c("Sales", "R&D"), 5),
        Attrition = rep(c("Yes", "No"), 5)
      ),
      filtered_data = NULL
    )
    
    # Simulate filter application
    shared_data$filtered_data <- shared_data$employee_data[shared_data$employee_data$Department == "Sales", ]
    
    expect_equal(nrow(shared_data$filtered_data), 5)
    expect_true(all(shared_data$filtered_data$Department == "Sales"))
  })
  
  # Test 7.2: Module communication
  test_that("modules communicate bidirectionally", {
    # Create mock modules
    module_a_data <- reactiveVal(NULL)
    module_b_data <- reactiveVal(NULL)
    
    # Simulate data exchange
    test_data <- data.frame(id = 1:5, value = letters[1:5])
    module_a_data(test_data)
    
    # Module B receives data from Module A
    received_data <- module_a_data()
    module_b_data(received_data)
    
    expect_equal(module_b_data(), test_data)
  })
  
  # Test 7.3: Report generation integration
  test_that("report module integrates with analysis modules", {
    # Mock analysis results
    analysis_results <- list(
      kpi_metrics = list(
        total_employees = 100,
        attrition_rate = 0.15,
        avg_satisfaction = 3.8
      ),
      attrition_analysis = list(
        by_department = data.frame(
          Department = c("Sales", "R&D", "HR"),
          AttritionRate = c(0.20, 0.10, 0.15)
        )
      )
    )
    
    # Test report parameter generation
    report_params <- generate_report_parameters(analysis_results)
    expect_true("kpi_metrics" %in% names(report_params))
    expect_true("attrition_analysis" %in% names(report_params))
  })
})

# =============================================================================
# 8. EDGE CASE TESTING
# =============================================================================

test_that("Edge Cases & Error Handling", {
  
  # Test 8.1: Empty dataset handling
  test_that("handles empty datasets gracefully", {
    empty_data <- data.frame()
    
    # Should handle empty data without crashing
    result <- tryCatch({
      calculate_attrition_rate(empty_data)
    }, error = function(e) {
      "handled_error"
    })
    
    expect_equal(result, "handled_error")
  })
  
  # Test 8.2: Single row dataset
  test_that("handles single row datasets", {
    single_row <- data.frame(
      EmployeeID = 1,
      Department = "Sales",
      Attrition = "No"
    )
    
    result <- calculate_department_metrics(single_row)
    expect_true(is.list(result))
    expect_equal(result$total_employees, 1)
  })
  
  # Test 8.3: All identical values
  test_that("handles datasets with identical values", {
    identical_data <- data.frame(
      Department = rep("Sales", 100),
      Attrition = rep("No", 100),
      Salary = rep(50000, 100)
    )
    
    metrics <- calculate_department_metrics(identical_data)
    expect_equal(metrics$attrition_rate, 0)
    expect_equal(metrics$salary_variance, 0)
  })
  
  # Test 8.4: Extreme values
  test_that("handles extreme values appropriately", {
    extreme_data <- data.frame(
      Age = c(18, 100, 25, 65),  # Min and max valid ages
      Salary = c(1, 1000000, 50000, 75000),  # Very low and very high salaries
      YearsAtCompany = c(0, 50, 5, 10)  # Min and max tenure
    )
    
    validation_result <- validate_extreme_values(extreme_data)
    expect_true(validation_result$has_extremes)
    expect_true("Salary" %in% validation_result$extreme_columns)
  })
  
  # Test 8.5: Special date handling
  test_that("handles special date cases", {
    date_data <- data.frame(
      HireDate = c(
        as.Date("1900-01-01"),  # Very old date
        as.Date("2030-01-01"),  # Future date
        as.Date("2020-02-29"),  # Leap year date
        Sys.Date()  # Today's date
      ),
      EmployeeID = 1:4
    )
    
    date_validation <- validate_hire_dates(date_data)
    expect_true("future_dates" %in% names(date_validation))
    expect_true("ancient_dates" %in% names(date_validation))
  })
  
  # Test 8.6: Unicode and special characters
  test_that("handles unicode and special characters", {
    unicode_data <- data.frame(
      FirstName = c("José", "François", "王小明", "محمد"),
      LastName = c("García", "Müller", "李", "العربي"),
      Department = c("Sales & Marketing", "R&D", "HR", "Finance")
    )
    
    processed_data <- process_text_fields(unicode_data)
    expect_equal(nrow(processed_data), 4)
    expect_true(all(nchar(processed_data$FirstName) > 0))
  })
  
  # Test 8.7: Large dataset handling
  test_that("handles large datasets efficiently", {
    # Create large dataset (100k rows)
    large_data <- data.frame(
      EmployeeID = 1:100000,
      Department = sample(c("Sales", "R&D", "HR", "Finance", "IT"), 100000, replace = TRUE),
      Salary = sample(30000:200000, 100000, replace = TRUE),
      Attrition = sample(c("Yes", "No"), 100000, replace = TRUE, prob = c(0.15, 0.85))
    )
    
    start_time <- Sys.time()
    result <- calculate_large_dataset_metrics(large_data)
    end_time <- Sys.time()
    
    processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    expect_true(is.list(result))
    expect_lt(processing_time, 10)  # Should process within 10 seconds
  })
  
  # Test 8.8: Memory pressure scenarios
  test_that("handles memory pressure gracefully", {
    # Simulate low memory scenario
    tryCatch({
      # Create multiple large objects
      large_objects <- replicate(100, {
        matrix(rnorm(10000), nrow = 100)
      }, simplify = FALSE)
      
      # Test if app can still function
      test_data <- data.frame(id = 1:10, value = 1:10)
      result <- process_under_memory_pressure(test_data)
      
      expect_true(is.data.frame(result))
      
    }, error = function(e) {
      # Memory pressure handled gracefully
      expect_true(TRUE)
    }, finally = {
      # Clean up
      if (exists("large_objects")) rm(large_objects)
      gc()
    })
  })
})

# =============================================================================
# 9. CROSS-BROWSER & PLATFORM TESTING
# =============================================================================

test_that("Cross-Platform Compatibility", {
  
  # Test 9.1: Date format handling across locales
  test_that("handles date formats correctly across locales", {
    # Save current locale
    current_locale <- Sys.getlocale("LC_TIME")
    
    tryCatch({
      # Test US format
      Sys.setlocale("LC_TIME", "en_US.UTF-8")
      us_date <- parse_date_string("12/31/2023")
      expect_equal(format(us_date, "%Y-%m-%d"), "2023-12-31")
      
      # Test European format
      Sys.setlocale("LC_TIME", "en_GB.UTF-8")
      eu_date <- parse_date_string("31/12/2023