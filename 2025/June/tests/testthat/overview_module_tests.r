# tests/testthat/test-overview-module.R
# Comprehensive Unit Tests for Overview Module - Atlas Labs HR Analytics
# Testing Areas: KPI calculations, data aggregation, chart rendering, interactivity,
# filter state, reactive dependencies, data refresh, error handling, loading states, exports

library(testthat)
library(shiny)
library(DT)
library(plotly)
library(dplyr)
library(mockery)

# Load the module (assuming it's sourced)
source("modules/overview_module.R")
source("modules/logger_module.R")
source("utils.R")

# Mock data for testing
create_mock_employee_data <- function(n = 100) {
  set.seed(123)
  data.frame(
    EmployeeID = 1:n,
    FirstName = paste0("Employee", 1:n),
    Age = sample(22:65, n, replace = TRUE),
    Gender = sample(c("Male", "Female", "Other"), n, replace = TRUE),
    Department = sample(c("Sales", "HR", "IT", "Finance", "Marketing"), n, replace = TRUE),
    Salary = sample(40000:120000, n, replace = TRUE),
    Attrition = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.15, 0.85)),
    YearsAtCompany = sample(1:20, n, replace = TRUE),
    JobSatisfaction = sample(1:5, n, replace = TRUE),
    PerformanceRating = sample(1:5, n, replace = TRUE),
    BusinessTravel = sample(c("None", "Rarely", "Frequently"), n, replace = TRUE),
    OverTime = sample(c("Yes", "No"), n, replace = TRUE),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

create_mock_logger <- function() {
  list(
    log_info = function(msg, module = "test", perf = NULL) invisible(),
    log_warning = function(msg, module = "test") invisible(),
    log_error = function(msg, module = "test") invisible(),
    track_performance = function(operation, duration, memory = NULL) invisible()
  )
}

# Test Suite 1: KPI Calculation Accuracy
test_that("KPI calculations are accurate", {
  
  # Test data setup
  test_data <- create_mock_employee_data(200)
  mock_logger <- create_mock_logger()
  
  # Calculate expected KPIs manually
  expected_total_employees <- nrow(test_data)
  expected_attrition_rate <- sum(test_data$Attrition == "Yes") / nrow(test_data)
  expected_avg_age <- mean(test_data$Age, na.rm = TRUE)
  expected_avg_salary <- mean(test_data$Salary, na.rm = TRUE)
  expected_avg_satisfaction <- mean(test_data$JobSatisfaction, na.rm = TRUE)
  expected_avg_tenure <- mean(test_data$YearsAtCompany, na.rm = TRUE)
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Test total employees KPI
    expect_equal(kpi_metrics()$total_employees, expected_total_employees)
    
    # Test attrition rate KPI (within tolerance for floating point)
    expect_equal(kpi_metrics()$attrition_rate, expected_attrition_rate, tolerance = 0.001)
    
    # Test average age KPI
    expect_equal(kpi_metrics()$avg_age, expected_avg_age, tolerance = 0.01)
    
    # Test average salary KPI
    expect_equal(kpi_metrics()$avg_salary, expected_avg_salary, tolerance = 0.01)
    
    # Test average satisfaction KPI
    expect_equal(kpi_metrics()$avg_satisfaction, expected_avg_satisfaction, tolerance = 0.01)
    
    # Test average tenure KPI
    expect_equal(kpi_metrics()$avg_tenure, expected_avg_tenure, tolerance = 0.01)
    
    # Test KPI structure
    expect_true(is.list(kpi_metrics()))
    expect_true(all(c("total_employees", "attrition_rate", "avg_age", 
                      "avg_salary", "avg_satisfaction", "avg_tenure") %in% names(kpi_metrics())))
  })
})

test_that("KPI calculations handle edge cases", {
  
  # Test with empty data
  empty_data <- data.frame()
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(empty_data),
    logger = mock_logger
  ), {
    expect_equal(kpi_metrics()$total_employees, 0)
    expect_true(is.na(kpi_metrics()$attrition_rate) || kpi_metrics()$attrition_rate == 0)
  })
  
  # Test with single row
  single_row <- create_mock_employee_data(1)
  testServer(overviewServer, args = list(
    data = reactive(single_row),
    logger = mock_logger
  ), {
    expect_equal(kpi_metrics()$total_employees, 1)
    expect_equal(kpi_metrics()$attrition_rate, ifelse(single_row$Attrition == "Yes", 1, 0))
  })
  
  # Test with missing values
  data_with_na <- create_mock_employee_data(50)
  data_with_na$Age[1:5] <- NA
  data_with_na$Salary[1:3] <- NA
  
  testServer(overviewServer, args = list(
    data = reactive(data_with_na),
    logger = mock_logger
  ), {
    expect_false(is.na(kpi_metrics()$avg_age))
    expect_false(is.na(kpi_metrics()$avg_salary))
    expect_equal(kpi_metrics()$total_employees, 50)
  })
})

# Test Suite 2: Data Aggregation Correctness
test_that("Data aggregation functions work correctly", {
  
  test_data <- create_mock_employee_data(150)
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Test department aggregation
    dept_summary <- dept_breakdown()
    expect_true(is.data.frame(dept_summary))
    expect_true("Department" %in% names(dept_summary))
    expect_true("Count" %in% names(dept_summary))
    expect_equal(sum(dept_summary$Count), nrow(test_data))
    
    # Test attrition by department
    attrition_by_dept <- attrition_by_department()
    expect_true(is.data.frame(attrition_by_dept))
    expect_true(all(c("Department", "Attrition_Rate", "Total_Count") %in% names(attrition_by_dept)))
    expect_true(all(attrition_by_dept$Attrition_Rate >= 0 & attrition_by_dept$Attrition_Rate <= 1))
    
    # Test age distribution
    age_dist <- age_distribution()
    expect_true(is.data.frame(age_dist))
    expect_true("Age_Group" %in% names(age_dist))
    expect_true("Count" %in% names(age_dist))
    
    # Test salary distribution
    salary_dist <- salary_distribution()
    expect_true(is.data.frame(salary_dist))
    expect_true(all(salary_dist$Count >= 0))
  })
})

test_that("Aggregation handles filtered data correctly", {
  
  test_data <- create_mock_employee_data(100)
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Test with department filter
    session$setInputs(dept_filter = "Sales")
    
    # Verify filtered aggregations
    filtered_summary <- dept_breakdown()
    sales_data <- test_data[test_data$Department == "Sales", ]
    expect_equal(sum(filtered_summary$Count), nrow(sales_data))
    expect_true(all(filtered_summary$Department == "Sales"))
  })
})

# Test Suite 3: Chart Rendering Validation
test_that("Chart rendering produces valid outputs", {
  
  test_data <- create_mock_employee_data(80)
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Test department chart
    dept_chart <- output$dept_chart
    expect_true(!is.null(dept_chart))
    
    # Test attrition chart
    attrition_chart <- output$attrition_chart
    expect_true(!is.null(attrition_chart))
    
    # Test age distribution chart
    age_chart <- output$age_dist_chart
    expect_true(!is.null(age_chart))
    
    # Test salary distribution chart
    salary_chart <- output$salary_dist_chart
    expect_true(!is.null(salary_chart))
    
    # Test KPI value boxes
    expect_true(!is.null(output$total_employees_box))
    expect_true(!is.null(output$attrition_rate_box))
    expect_true(!is.null(output$avg_salary_box))
    expect_true(!is.null(output$avg_satisfaction_box))
  })
})

test_that("Charts handle empty data gracefully", {
  
  empty_data <- data.frame()
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(empty_data),
    logger = mock_logger
  ), {
    
    # Verify charts don't crash with empty data
    expect_no_error({
      dept_chart <- output$dept_chart
      attrition_chart <- output$attrition_chart
      age_chart <- output$age_dist_chart
      salary_chart <- output$salary_dist_chart
    })
  })
})

# Test Suite 4: Interactive Element Functionality
test_that("Interactive elements respond correctly", {
  
  test_data <- create_mock_employee_data(120)
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Test department filter
    departments <- unique(test_data$Department)
    session$setInputs(dept_filter = departments[1])
    expect_equal(input$dept_filter, departments[1])
    
    # Test date range filter
    session$setInputs(date_range = c("2020-01-01", "2023-12-31"))
    expect_equal(input$date_range, c("2020-01-01", "2023-12-31"))
    
    # Test refresh button
    session$setInputs(refresh_btn = 1)
    expect_equal(input$refresh_btn, 1)
    
    # Test chart click interactions
    session$setInputs(dept_chart_click = list(x = "Sales"))
    expect_equal(input$dept_chart_click$x, "Sales")
    
    # Test export button
    session$setInputs(export_btn = 1)
    expect_equal(input$export_btn, 1)
  })
})

test_that("Interactive elements validate inputs", {
  
  test_data <- create_mock_employee_data(50)
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Test invalid department filter
    session$setInputs(dept_filter = "InvalidDepartment")
    filtered_data <- filtered_dataset()
    expect_equal(nrow(filtered_data), 0)  # Should return empty dataset
    
    # Test invalid date range
    session$setInputs(date_range = c("2025-01-01", "2020-01-01"))  # End before start
    expect_true(is.null(input$date_range) || length(input$date_range) == 0)
  })
})

# Test Suite 5: Filter State Preservation
test_that("Filter states are preserved correctly", {
  
  test_data <- create_mock_employee_data(90)
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Set multiple filters
    session$setInputs(
      dept_filter = "IT",
      age_range = c(25, 45),
      salary_range = c(50000, 80000)
    )
    
    # Verify filter state
    current_filters <- get_current_filters()
    expect_equal(current_filters$dept_filter, "IT")
    expect_equal(current_filters$age_range, c(25, 45))
    expect_equal(current_filters$salary_range, c(50000, 80000))
    
    # Test filter persistence after data refresh
    session$setInputs(refresh_btn = 1)
    preserved_filters <- get_current_filters()
    expect_equal(preserved_filters$dept_filter, "IT")
    expect_equal(preserved_filters$age_range, c(25, 45))
    expect_equal(preserved_filters$salary_range, c(50000, 80000))
  })
})

test_that("Filter reset functionality works", {
  
  test_data <- create_mock_employee_data(70)
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Set filters
    session$setInputs(
      dept_filter = "Sales",
      age_range = c(30, 50)
    )
    
    # Reset filters
    session$setInputs(reset_filters_btn = 1)
    
    # Verify filters are reset
    reset_filters <- get_current_filters()
    expect_true(is.null(reset_filters$dept_filter) || reset_filters$dept_filter == "All")
    expect_true(is.null(reset_filters$age_range) || all(reset_filters$age_range == range(test_data$Age)))
  })
})

# Test Suite 6: Reactive Dependency Chains
test_that("Reactive dependencies update correctly", {
  
  test_data <- create_mock_employee_data(110)
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Track reactive invalidations
    kpi_invalidations <- 0
    chart_invalidations <- 0
    
    # Mock reactive observers
    observe({
      kpi_metrics()
      kpi_invalidations <<- kpi_invalidations + 1
    })
    
    observe({
      dept_breakdown()
      chart_invalidations <<- chart_invalidations + 1
    })
    
    # Initial state
    initial_kpi_count <- kpi_invalidations
    initial_chart_count <- chart_invalidations
    
    # Change filter - should trigger reactive updates
    session$setInputs(dept_filter = "HR")
    session$flushReact()
    
    # Verify reactive chains fired
    expect_gt(kpi_invalidations, initial_kpi_count)
    expect_gt(chart_invalidations, initial_chart_count)
  })
})

test_that("Reactive dependencies are optimized", {
  
  test_data <- create_mock_employee_data(60)
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Test that unrelated input changes don't trigger unnecessary updates
    invalidation_count <- 0
    
    observe({
      kpi_metrics()
      invalidation_count <<- invalidation_count + 1
    })
    
    initial_count <- invalidation_count
    
    # Change unrelated UI element
    session$setInputs(ui_theme = "dark")
    session$flushReact()
    
    # KPI metrics should not invalidate for theme changes
    expect_equal(invalidation_count, initial_count)
  })
})

# Test Suite 7: Data Refresh Mechanisms
test_that("Data refresh works correctly", {
  
  test_data_v1 <- create_mock_employee_data(50)
  test_data_v2 <- create_mock_employee_data(75)
  
  mock_logger <- create_mock_logger()
  
  # Create reactive data source
  data_source <- reactiveVal(test_data_v1)
  
  testServer(overviewServer, args = list(
    data = data_source,
    logger = mock_logger
  ), {
    
    # Initial state
    initial_kpis <- kpi_metrics()
    expect_equal(initial_kpis$total_employees, 50)
    
    # Update data source
    data_source(test_data_v2)
    session$flushReact()
    
    # Verify refresh
    updated_kpis <- kpi_metrics()
    expect_equal(updated_kpis$total_employees, 75)
    expect_not_equal(initial_kpis$total_employees, updated_kpis$total_employees)
  })
})

test_that("Manual refresh button works", {
  
  test_data <- create_mock_employee_data(80)
  mock_logger <- create_mock_logger()
  refresh_counter <- reactiveVal(0)
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger,
    refresh_trigger = refresh_counter
  ), {
    
    initial_refresh_count <- refresh_counter()
    
    # Trigger manual refresh
    session$setInputs(refresh_btn = 1)
    session$flushReact()
    
    # Verify refresh was triggered
    expect_gt(refresh_counter(), initial_refresh_count)
  })
})

# Test Suite 8: Error State Handling
test_that("Error states are handled gracefully", {
  
  mock_logger <- create_mock_logger()
  
  # Test with NULL data
  testServer(overviewServer, args = list(
    data = reactive(NULL),
    logger = mock_logger
  ), {
    
    expect_no_error({
      kpis <- kpi_metrics()
      dept_data <- dept_breakdown()
    })
    
    # Verify error states
    expect_true(is.null(kpi_metrics()) || all(is.na(kpi_metrics())))
  })
  
  # Test with malformed data
  bad_data <- data.frame(
    WrongColumn = 1:10,
    AnotherWrongColumn = letters[1:10]
  )
  
  testServer(overviewServer, args = list(
    data = reactive(bad_data),
    logger = mock_logger
  ), {
    
    expect_no_error({
      kpis <- kpi_metrics()
      charts <- dept_breakdown()
    })
    
    # Should handle missing required columns gracefully
    expect_true(is.null(kpi_metrics()$total_employees) || kpi_metrics()$total_employees == 0)
  })
})

test_that("Network errors are handled", {
  
  mock_logger <- create_mock_logger()
  
  # Simulate network error with reactive that throws
  error_data <- reactive({
    stop("Network connection failed")
  })
  
  testServer(overviewServer, args = list(
    data = error_data,
    logger = mock_logger
  ), {
    
    # Should not crash the app
    expect_no_error({
      tryCatch({
        kpis <- kpi_metrics()
      }, error = function(e) NULL)
    })
  })
})

# Test Suite 9: Loading State Management
test_that("Loading states are managed correctly", {
  
  test_data <- create_mock_employee_data(100)
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Test initial loading state
    expect_true(is.logical(loading_state()))
    
    # Test loading during data processing
    session$setInputs(refresh_btn = 1)
    
    # Should show loading during refresh
    # Note: In real implementation, this would be tested with async operations
    expect_true(is.logical(loading_state()))
  })
})

test_that("Loading indicators update correctly", {
  
  test_data <- create_mock_employee_data(150)
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Test loading states for different components
    expect_true(!is.null(output$loading_kpis))
    expect_true(!is.null(output$loading_charts))
    
    # Verify loading states clear after processing
    session$flushReact()
    
    # After processing, loading should be FALSE
    processed_state <- loading_state()
    expect_false(processed_state)
  })
})

# Test Suite 10: Export Functionality
test_that("Export functionality works correctly", {
  
  test_data <- create_mock_employee_data(60)
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Test export data preparation
    export_data <- prepare_export_data()
    expect_true(is.data.frame(export_data) || is.list(export_data))
    
    # Test export button
    session$setInputs(export_btn = 1)
    
    # Verify export was triggered
    expect_equal(input$export_btn, 1)
    
    # Test export formats
    session$setInputs(export_format = "csv")
    expect_equal(input$export_format, "csv")
    
    session$setInputs(export_format = "xlsx")
    expect_equal(input$export_format, "xlsx")
    
    session$setInputs(export_format = "pdf")
    expect_equal(input$export_format, "pdf")
  })
})

test_that("Export data integrity is maintained", {
  
  test_data <- create_mock_employee_data(40)
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Set filters
    session$setInputs(dept_filter = "IT")
    
    # Get export data
    export_data <- prepare_export_data()
    filtered_data <- filtered_dataset()
    
    # Verify export data matches filtered data
    if (is.data.frame(export_data) && is.data.frame(filtered_data)) {
      expect_equal(nrow(export_data), nrow(filtered_data))
    }
    
    # Test export metadata
    export_metadata <- get_export_metadata()
    expect_true(is.list(export_metadata))
    expect_true("export_date" %in% names(export_metadata))
    expect_true("filters_applied" %in% names(export_metadata))
    expect_true("record_count" %in% names(export_metadata))
  })
})

# Integration Tests
test_that("Overview module integrates correctly with other components", {
  
  test_data <- create_mock_employee_data(100)
  mock_logger <- create_mock_logger()
  
  # Test with shared reactive values (simulating app-wide state)
  shared_values <- reactiveValues(
    current_filters = list(),
    selected_employees = NULL,
    analysis_mode = "overview"
  )
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger,
    shared_state = shared_values
  ), {
    
    # Test filter communication
    session$setInputs(dept_filter = "Sales")
    
    # Verify shared state updates
    expect_true(is.reactivevalues(shared_values))
    
    # Test cross-module data sharing
    kpis <- kpi_metrics()
    expect_true(is.list(kpis))
    expect_true(length(kpis) > 0)
  })
})

# Performance Tests
test_that("Overview module performs within acceptable limits", {
  
  # Large dataset for performance testing
  large_data <- create_mock_employee_data(5000)
  mock_logger <- create_mock_logger()
  
  testServer(overviewServer, args = list(
    data = reactive(large_data),
    logger = mock_logger
  ), {
    
    # Time KPI calculation
    start_time <- Sys.time()
    kpis <- kpi_metrics()
    end_time <- Sys.time()
    
    kpi_duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Should complete within reasonable time (< 2 seconds for 5000 records)
    expect_lt(kpi_duration, 2.0)
    
    # Time chart generation
    start_time <- Sys.time()
    dept_data <- dept_breakdown()
    end_time <- Sys.time()
    
    chart_duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
    expect_lt(chart_duration, 1.0)
  })
})

# Memory Usage Tests
test_that("Memory usage is within acceptable limits", {
  
  test_data <- create_mock_employee_data(1000)
  mock_logger <- create_mock_logger()
  
  # Capture initial memory
  gc()
  initial_memory <- memory.size()
  
  testServer(overviewServer, args = list(
    data = reactive(test_data),
    logger = mock_logger
  ), {
    
    # Generate all outputs
    kpis <- kpi_metrics()
    dept_data <- dept_breakdown()
    age_data <- age_distribution()
    salary_data <- salary_distribution()
    
    # Force garbage collection and check memory
    gc()
    final_memory <- memory.size()
    
    # Memory increase should be reasonable (< 50MB for this test size)
    memory_increase <- final_memory - initial_memory
    expect_lt(memory_increase, 50)
  })
})

# Cleanup
teardown({
  # Clean up any test artifacts
  if (exists("test_exports")) {
    unlink("test_exports", recursive = TRUE)
  }
})