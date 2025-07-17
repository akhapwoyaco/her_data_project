# =============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD - COMPREHENSIVE UNIT TESTS
# =============================================================================
# 
# Test Categories:
# 1. Data Loading & Validation Tests
# 2. Logger System Tests (R6 Class)
# 3. Module Functionality Tests
# 4. UI/UX Component Tests
# 5. Performance & Memory Tests
# 6. Data Processing & Analysis Tests
# 7. Visualization Tests
# 8. Report Generation Tests
# 9. Cross-Module Communication Tests
# 10. Edge Cases & Error Handling Tests
# 11. Integration Tests
# 12. Accessibility Tests
# 13. Browser Compatibility Tests
# 14. Load Testing
# 15. Data Quality & Integrity Tests
#
# =============================================================================

# Required libraries for testing
library(testthat)
library(shiny)
library(shinymeta)
library(shinytest2)
library(mockery)
library(httr)
library(jsonlite)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(R6)
library(benchmarkme)
library(profvis)
library(pryr)
library(webdriver)

# Source application files
source("global.R")
source("utils.R")
source("custom_theme.R")

# Load all modules
purrr::walk(list.files("modules", full.names = TRUE), source)

# =============================================================================
# 1. DATA LOADING & VALIDATION TESTS
# =============================================================================

test_that("Data Loader Module - File Loading", {
  
  # Test 1.1: Valid CSV file loading
  test_data <- data.frame(
    EmployeeID = 1:5,
    FirstName = c("John", "Jane", "Bob", "Alice", "Tom"),
    LastName = c("Doe", "Smith", "Johnson", "Brown", "Wilson"),
    Salary = c(50000, 60000, 55000, 65000, 58000)
  )
  
  temp_file <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_file, row.names = FALSE)
  
  # Mock the data loader
  mock_loader <- dataLoaderServer("test")
  
  expect_silent(load_employee_data(temp_file))
  expect_is(load_employee_data(temp_file), "data.frame")
  expect_equal(nrow(load_employee_data(temp_file)), 5)
  
  unlink(temp_file)
})

test_that("Data Loader Module - File Validation Edge Cases", {
  
  # Test 1.2: Empty file
  empty_file <- tempfile(fileext = ".csv")
  writeLines("", empty_file)
  
  expect_error(load_employee_data(empty_file), "File is empty")
  unlink(empty_file)
  
  # Test 1.3: Corrupted CSV
  corrupted_file <- tempfile(fileext = ".csv")
  writeLines(c("col1,col2", "val1,val2,val3,val4"), corrupted_file)
  
  expect_warning(load_employee_data(corrupted_file), "Inconsistent columns")
  unlink(corrupted_file)
  
  # Test 1.4: Missing required columns
  incomplete_data <- data.frame(
    Name = c("John", "Jane"),
    Age = c(30, 25)
  )
  
  incomplete_file <- tempfile(fileext = ".csv")
  write.csv(incomplete_data, incomplete_file, row.names = FALSE)
  
  expect_error(validate_data_integrity(incomplete_data), "Missing required columns")
  unlink(incomplete_file)
  
  # Test 1.5: Invalid data types
  invalid_data <- data.frame(
    EmployeeID = c("ABC", "DEF"),
    Salary = c("NotANumber", "AlsoNotANumber")
  )
  
  expect_error(validate_data_integrity(invalid_data), "Invalid data types")
  
  # Test 1.6: Duplicate employee IDs
  duplicate_data <- data.frame(
    EmployeeID = c(1, 1, 2),
    FirstName = c("John", "Jane", "Bob")
  )
  
  expect_error(validate_data_integrity(duplicate_data), "Duplicate employee IDs")
  
  # Test 1.7: File size limits
  large_data <- data.frame(
    EmployeeID = 1:1000000,
    FirstName = rep("Test", 1000000)
  )
  
  expect_warning(validate_data_integrity(large_data), "Large dataset detected")
})

test_that("Data Loader Module - Data Merging", {
  
  # Test 1.8: Successful data merging
  employee_data <- data.frame(
    EmployeeID = 1:3,
    FirstName = c("John", "Jane", "Bob")
  )
  
  performance_data <- data.frame(
    EmployeeID = 1:3,
    JobSatisfaction = c(4, 5, 3)
  )
  
  merged_data <- merge_datasets(employee_data, performance_data)
  
  expect_equal(nrow(merged_data), 3)
  expect_true("FirstName" %in% names(merged_data))
  expect_true("JobSatisfaction" %in% names(merged_data))
  
  # Test 1.9: Partial merge with missing records
  partial_performance <- data.frame(
    EmployeeID = 1:2,
    JobSatisfaction = c(4, 5)
  )
  
  partial_merged <- merge_datasets(employee_data, partial_performance)
  expect_equal(nrow(partial_merged), 2)
  expect_warning(merge_datasets(employee_data, partial_performance), "Partial merge")
})

# =============================================================================
# 2. LOGGER SYSTEM TESTS (R6 CLASS)
# =============================================================================

test_that("AtlasLogger R6 Class - Basic Functionality", {
  
  # Test 2.1: Logger initialization
  logger <- AtlasLogger$new()
  
  expect_is(logger, "AtlasLogger")
  expect_is(logger, "R6")
  expect_true(exists("log_info", envir = logger))
  expect_true(exists("log_warning", envir = logger))
  expect_true(exists("log_error", envir = logger))
  
  # Test 2.2: Basic logging functionality
  expect_silent(logger$log_info("Test info message", "test_module"))
  expect_silent(logger$log_warning("Test warning", "test_module"))
  expect_silent(logger$log_error("Test error", "test_module"))
  
  # Test 2.3: Log level filtering
  logger$set_log_level("WARNING")
  expect_silent(logger$log_info("Should be ignored", "test_module"))
  expect_output(logger$log_warning("Should appear", "test_module"))
})

test_that("AtlasLogger R6 Class - Performance Tracking", {
  
  logger <- AtlasLogger$new()
  
  # Test 2.4: Memory usage tracking
  initial_memory <- logger$track_memory_usage()
  expect_is(initial_memory, "numeric")
  expect_true(initial_memory > 0)
  
  # Test 2.5: Execution time tracking
  start_time <- logger$start_timer("test_operation")
  Sys.sleep(0.1)
  end_time <- logger$end_timer("test_operation")
  
  expect_true(end_time >= 0.1)
  expect_true(end_time < 1.0)
  
  # Test 2.6: Performance summary
  summary <- logger$get_performance_summary()
  expect_is(summary, "list")
  expect_true("memory_usage" %in% names(summary))
  expect_true("execution_times" %in% names(summary))
})

test_that("AtlasLogger R6 Class - Edge Cases", {
  
  logger <- AtlasLogger$new()
  
  # Test 2.7: Null/empty messages
  expect_error(logger$log_info(NULL, "test_module"))
  expect_error(logger$log_info("", "test_module"))
  
  # Test 2.8: Invalid module names
  expect_error(logger$log_info("Test", NULL))
  expect_error(logger$log_info("Test", ""))
  
  # Test 2.9: Memory pressure handling
  # Simulate high memory usage
  big_data <- rep(1:1000000, 100)
  logger$track_memory_usage()
  
  expect_warning(logger$check_memory_pressure(), "High memory usage detected")
  
  # Test 2.10: Timer edge cases
  expect_error(logger$end_timer("nonexistent_timer"))
  expect_error(logger$start_timer(""))
  
  # Test 2.11: Log buffer overflow
  for (i in 1:10000) {
    logger$log_info(paste("Message", i), "stress_test")
  }
  
  expect_warning(logger$check_log_buffer_size(), "Log buffer approaching limit")
})

# =============================================================================
# 3. MODULE FUNCTIONALITY TESTS
# =============================================================================

test_that("Overview Module - KPI Calculations", {
  
  # Test 3.1: Basic KPI calculations
  test_data <- data.frame(
    EmployeeID = 1:100,
    Attrition = c(rep("Yes", 20), rep("No", 80)),
    Salary = rnorm(100, 50000, 10000),
    JobSatisfaction = sample(1:5, 100, replace = TRUE)
  )
  
  kpis <- calculate_kpis(test_data)
  
  expect_equal(kpis$total_employees, 100)
  expect_equal(kpis$attrition_rate, 0.2)
  expect_true(kpis$avg_salary > 0)
  expect_true(kpis$avg_satisfaction >= 1 && kpis$avg_satisfaction <= 5)
  
  # Test 3.2: Edge case - empty dataset
  empty_data <- data.frame()
  empty_kpis <- calculate_kpis(empty_data)
  
  expect_equal(empty_kpis$total_employees, 0)
  expect_true(is.na(empty_kpis$attrition_rate))
  
  # Test 3.3: Edge case - single employee
  single_employee <- data.frame(
    EmployeeID = 1,
    Attrition = "No",
    Salary = 50000,
    JobSatisfaction = 4
  )
  
  single_kpis <- calculate_kpis(single_employee)
  expect_equal(single_kpis$total_employees, 1)
  expect_equal(single_kpis$attrition_rate, 0)
})

test_that("Attrition Module - Analysis Functions", {
  
  # Test 3.4: Attrition by department
  dept_data <- data.frame(
    Department = c(rep("IT", 50), rep("HR", 30), rep("Finance", 20)),
    Attrition = c(rep(c("Yes", "No"), c(10, 40)), 
                  rep(c("Yes", "No"), c(5, 25)),
                  rep(c("Yes", "No"), c(2, 18)))
  )
  
  attrition_by_dept <- analyze_attrition_by_department(dept_data)
  
  expect_equal(nrow(attrition_by_dept), 3)
  expect_true(all(c("Department", "AttritionRate", "TotalEmployees") %in% names(attrition_by_dept)))
  
  # Test 3.5: Attrition correlation analysis
  correlation_data <- data.frame(
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE),
    Salary = rnorm(100, 50000, 10000),
    YearsAtCompany = sample(1:20, 100, replace = TRUE),
    JobSatisfaction = sample(1:5, 100, replace = TRUE)
  )
  
  correlations <- analyze_attrition_correlations(correlation_data)
  
  expect_is(correlations, "data.frame")
  expect_true(all(abs(correlations$correlation) <= 1))
  
  # Test 3.6: Survival analysis
  survival_data <- data.frame(
    EmployeeID = 1:100,
    HireDate = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), length.out = 100),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE),
    AttritionDate = sample(seq(as.Date("2020-06-01"), as.Date("2023-06-01"), length.out = 50), 100, replace = TRUE)
  )
  
  survival_analysis <- perform_survival_analysis(survival_data)
  
  expect_is(survival_analysis, "list")
  expect_true("survival_curve" %in% names(survival_analysis))
  expect_true("median_tenure" %in% names(survival_analysis))
})

test_that("Performance Module - Rating Analysis", {
  
  # Test 3.7: Performance rating distribution
  performance_data <- data.frame(
    EmployeeID = 1:100,
    ManagerRating = sample(1:5, 100, replace = TRUE),
    SelfRating = sample(1:5, 100, replace = TRUE),
    Department = sample(c("IT", "HR", "Finance"), 100, replace = TRUE)
  )
  
  rating_distribution <- analyze_performance_distribution(performance_data)
  
  expect_is(rating_distribution, "data.frame")
  expect_true(all(rating_distribution$count >= 0))
  expect_equal(sum(rating_distribution$count), 100)
  
  # Test 3.8: Manager vs Self-rating comparison
  rating_comparison <- compare_manager_self_ratings(performance_data)
  
  expect_is(rating_comparison, "data.frame")
  expect_true("rating_difference" %in% names(rating_comparison))
  expect_true(all(abs(rating_comparison$rating_difference) <= 4))
  
  # Test 3.9: Performance trend analysis
  trend_data <- data.frame(
    EmployeeID = rep(1:20, each = 5),
    ReviewDate = rep(seq(as.Date("2019-01-01"), as.Date("2023-01-01"), by = "year"), 20),
    ManagerRating = sample(1:5, 100, replace = TRUE)
  )
  
  performance_trends <- analyze_performance_trends(trend_data)
  
  expect_is(performance_trends, "data.frame")
  expect_true("trend_slope" %in% names(performance_trends))
})

test_that("Compensation Module - Pay Analysis", {
  
  # Test 3.10: Pay equity analysis
  compensation_data <- data.frame(
    EmployeeID = 1:100,
    Gender = sample(c("Male", "Female"), 100, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian"), 100, replace = TRUE),
    Department = sample(c("IT", "HR", "Finance"), 100, replace = TRUE),
    JobRole = sample(c("Analyst", "Manager", "Director"), 100, replace = TRUE),
    Salary = rnorm(100, 60000, 15000)
  )
  
  pay_equity <- analyze_pay_equity(compensation_data)
  
  expect_is(pay_equity, "list")
  expect_true("gender_gaps" %in% names(pay_equity))
  expect_true("ethnicity_gaps" %in% names(pay_equity))
  
  # Test 3.11: Salary band analysis
  salary_bands <- create_salary_bands(compensation_data)
  
  expect_is(salary_bands, "data.frame")
  expect_true(all(c("band", "min_salary", "max_salary", "employee_count") %in% names(salary_bands)))
  
  # Test 3.12: Compensation vs performance correlation
  comp_performance_data <- merge(compensation_data, 
                                data.frame(EmployeeID = 1:100, 
                                          ManagerRating = sample(1:5, 100, replace = TRUE)))
  
  comp_perf_correlation <- analyze_compensation_performance(comp_performance_data)
  
  expect_is(comp_perf_correlation, "numeric")
  expect_true(abs(comp_perf_correlation) <= 1)
})

# =============================================================================
# 4. UI/UX COMPONENT TESTS
# =============================================================================

test_that("UI Component Rendering", {
  
  # Test 4.1: Module UI generation
  overview_ui <- overviewUI("test_overview")
  expect_is(overview_ui, "shiny.tag")
  expect_true(grepl("test_overview", as.character(overview_ui)))
  
  attrition_ui <- attritionUI("test_attrition")
  expect_is(attrition_ui, "shiny.tag")
  
  # Test 4.2: Dynamic UI updates
  test_server <- function(input, output, session) {
    callModule(overviewServer, "test_overview", reactive(test_data))
  }
  
  testServer(test_server, {
    session$setInputs(`test_overview-refresh` = 1)
    expect_true(exists("output", envir = session))
  })
  
  # Test 4.3: Input validation
  test_inputs <- list(
    date_range = c(as.Date("2020-01-01"), as.Date("2023-01-01")),
    department_filter = c("IT", "HR"),
    salary_range = c(40000, 80000)
  )
  
  validated_inputs <- validate_ui_inputs(test_inputs)
  expect_is(validated_inputs, "list")
  expect_true(validated_inputs$valid)
  
  # Test 4.4: Invalid input handling
  invalid_inputs <- list(
    date_range = c(as.Date("2023-01-01"), as.Date("2020-01-01")), # Invalid range
    department_filter = c(), # Empty selection
    salary_range = c(-1000, 1000000) # Invalid salary range
  )
  
  invalid_validation <- validate_ui_inputs(invalid_inputs)
  expect_false(invalid_validation$valid)
  expect_true(length(invalid_validation$errors) > 0)
})

test_that("Responsive Design Tests", {
  
  # Test 4.5: Mobile responsiveness
  mobile_viewport <- list(width = 375, height = 667)
  tablet_viewport <- list(width = 768, height = 1024)
  desktop_viewport <- list(width = 1920, height = 1080)
  
  # These would be tested with actual browser automation
  expect_true(check_responsive_design(mobile_viewport))
  expect_true(check_responsive_design(tablet_viewport))
  expect_true(check_responsive_design(desktop_viewport))
  
  # Test 4.6: CSS class validation
  css_classes <- c("atlas-kpi-card", "atlas-chart-container", "atlas-filter-panel")
  
  for (class in css_classes) {
    expect_true(validate_css_class(class))
  }
  
  # Test 4.7: JavaScript functionality
  js_functions <- c("updateChartData", "toggleFilterPanel", "exportChart")
  
  for (func in js_functions) {
    expect_true(validate_js_function(func))
  }
})

# =============================================================================
# 5. PERFORMANCE & MEMORY TESTS
# =============================================================================

test_that("Performance Benchmarks", {
  
  # Test 5.1: Data loading performance
  large_dataset <- data.frame(
    EmployeeID = 1:10000,
    FirstName = sample(letters, 10000, replace = TRUE),
    Salary = rnorm(10000, 50000, 10000),
    Department = sample(c("IT", "HR", "Finance", "Sales"), 10000, replace = TRUE)
  )
  
  load_time <- system.time({
    processed_data <- process_employee_data(large_dataset)
  })
  
  expect_true(load_time[["elapsed"]] < 5.0) # Should load within 5 seconds
  
  # Test 5.2: Visualization rendering performance
  chart_render_time <- system.time({
    chart <- create_attrition_chart(large_dataset)
  })
  
  expect_true(chart_render_time[["elapsed"]] < 2.0) # Should render within 2 seconds
  
  # Test 5.3: Memory usage limits
  initial_memory <- pryr::mem_used()
  
  # Process large dataset
  processed_large <- process_employee_data(large_dataset)
  
  final_memory <- pryr::mem_used()
  memory_increase <- final_memory - initial_memory
  
  expect_true(memory_increase < 100 * 1024^2) # Should use less than 100MB
  
  # Test 5.4: Concurrent user simulation
  concurrent_load_test <- function(n_users = 5) {
    results <- parallel::mclapply(1:n_users, function(i) {
      start_time <- Sys.time()
      test_data <- process_employee_data(large_dataset)
      end_time <- Sys.time()
      
      list(
        user_id = i,
        processing_time = as.numeric(end_time - start_time),
        memory_used = pryr::mem_used()
      )
    }, mc.cores = min(n_users, parallel::detectCores()))
    
    avg_time <- mean(sapply(results, function(x) x$processing_time))
    expect_true(avg_time < 10.0) # Average processing time should be under 10 seconds
    
    return(results)
  }
  
  concurrent_results <- concurrent_load_test(3)
  expect_length(concurrent_results, 3)
})

test_that("Memory Management", {
  
  # Test 5.5: Memory leak detection
  initial_objects <- ls()
  
  for (i in 1:100) {
    temp_data <- data.frame(x = rnorm(1000), y = rnorm(1000))
    temp_result <- process_employee_data(temp_data)
    rm(temp_data, temp_result)
  }
  
  gc() # Force garbage collection
  final_objects <- ls()
  
  # Should not have significantly more objects
  expect_true(length(final_objects) - length(initial_objects) < 10)
  
  # Test 5.6: Large dataset handling
  stress_test_data <- data.frame(
    EmployeeID = 1:100000,
    RandomData = rnorm(100000)
  )
  
  expect_error(process_employee_data(stress_test_data), NA) # Should not error
  
  # Test 5.7: Memory pressure response
  memory_monitor <- function() {
    current_memory <- pryr::mem_used()
    threshold <- 500 * 1024^2 # 500MB threshold
    
    if (current_memory > threshold) {
      warning("Memory usage exceeds threshold")
      return(FALSE)
    }
    return(TRUE)
  }
  
  expect_true(memory_monitor())
})

# =============================================================================
# 6. DATA PROCESSING & ANALYSIS TESTS
# =============================================================================

test_that("Data Transformation Functions", {
  
  # Test 6.1: Data type conversions
  raw_data <- data.frame(
    EmployeeID = c("1", "2", "3"),
    Salary = c("50000", "60000", "55000"),
    HireDate = c("2020-01-01", "2021-06-15", "2019-03-10"),
    Attrition = c("Yes", "No", "Yes")
  )
  
  transformed_data <- transform_data_types(raw_data)
  
  expect_is(transformed_data$EmployeeID, "integer")
  expect_is(transformed_data$Salary, "numeric")
  expect_is(transformed_data$HireDate, "Date")
  expect_is(transformed_data$Attrition, "logical")
  
  # Test 6.2: Missing value handling
  data_with_missing <- data.frame(
    EmployeeID = 1:5,
    Salary = c(50000, NA, 60000, 55000, NA),
    Department = c("IT", "HR", NA, "Finance", "IT")
  )
  
  cleaned_data <- handle_missing_values(data_with_missing)
  
  expect_false(any(is.na(cleaned_data$Salary)))
  expect_false(any(is.na(cleaned_data$Department)))
  
  # Test 6.3: Outlier detection
  outlier_data <- data.frame(
    EmployeeID = 1:100,
    Salary = c(rnorm(95, 50000, 10000), rep(200000, 5)) # 5 outliers
  )
  
  outliers <- detect_outliers(outlier_data, "Salary")
  
  expect_equal(length(outliers), 5)
  expect_true(all(outlier_data$Salary[outliers] > 150000))
  
  # Test 6.4: Data aggregation
  aggregation_data <- data.frame(
    Department = rep(c("IT", "HR", "Finance"), each = 10),
    Salary = rnorm(30, 50000, 10000),
    Attrition = sample(c("Yes", "No"), 30, replace = TRUE)
  )
  
  dept_summary <- aggregate_by_department(aggregation_data)
  
  expect_equal(nrow(dept_summary), 3)
  expect_true(all(c("Department", "AvgSalary", "AttritionRate", "EmployeeCount") %in% names(dept_summary)))
})

test_that("Statistical Analysis Functions", {
  
  # Test 6.5: Correlation analysis
  correlation_data <- data.frame(
    Salary = rnorm(100, 50000, 10000),
    YearsExperience = sample(1:20, 100, replace = TRUE),
    JobSatisfaction = sample(1:5, 100, replace = TRUE),
    Age = sample(25:65, 100, replace = TRUE)
  )
  
  correlation_matrix <- calculate_correlation_matrix(correlation_data)
  
  expect_is(correlation_matrix, "matrix")
  expect_equal(nrow(correlation_matrix), 4)
  expect_equal(ncol(correlation_matrix), 4)
  expect_true(all(abs(correlation_matrix) <= 1))
  expect_true(all(diag(correlation_matrix) == 1))
  
  # Test 6.6: Hypothesis testing
  group_a <- rnorm(50, 50000, 10000)
  group_b <- rnorm(50, 55000, 10000)
  
  t_test_result <- perform_t_test(group_a, group_b)
  
  expect_is(t_test_result, "htest")
  expect_true("p.value" %in% names(t_test_result))
  expect_true("statistic" %in% names(t_test_result))
  
  # Test 6.7: Regression analysis
  regression_data <- data.frame(
    salary = rnorm(100, 50000, 10000),
    experience = sample(1:20, 100, replace = TRUE),
    education = sample(1:5, 100, replace = TRUE)
  )
  
  regression_model <- build_regression_model(regression_data, "salary")
  
  expect_is(regression_model, "lm")
  expect_true(summary(regression_model)$r.squared >= 0)
  expect_true(summary(regression_model)$r.squared <= 1)
})

# =============================================================================
# 7. VISUALIZATION TESTS
# =============================================================================

test_that("Chart Generation", {
  
  # Test 7.1: Basic chart creation
  chart_data <- data.frame(
    Department = c("IT", "HR", "Finance"),
    EmployeeCount = c(50, 30, 20),
    AttritionRate = c(0.15, 0.10, 0.12)
  )
  
  bar_chart <- create_bar_chart(chart_data, "Department", "EmployeeCount")
  
  expect_is(bar_chart, "ggplot")
  expect_true("data" %in% names(bar_chart))
  expect_true("layers" %in% names(bar_chart))
  
  # Test 7.2: Interactive plotly charts
  interactive_chart <- create_interactive_chart(chart_data, "Department", "AttritionRate")
  
  expect_is(interactive_chart, "plotly")
  expect_true("x" %in% names(interactive_chart))
  expect_true("data" %in% names(interactive_chart))
  
  # Test 7.3: Heatmap generation
  heatmap_data <- matrix(rnorm(100), nrow = 10, ncol = 10)
  colnames(heatmap_data) <- paste0("Var", 1:10)
  rownames(heatmap_data) <- paste0("Emp", 1:10)
  
  heatmap <- create_heatmap(heatmap_data)
  
  expect_is(heatmap, "ggplot")
  
  # Test 7.4: Time series visualization
  time_series_data <- data.frame(
    Date = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "month"),
    AttritionRate = cumsum(rnorm(37, 0, 0.01)) + 0.15
  )
  
  time_series_chart <- create_time_series_chart(time_series_data, "Date", "AttritionRate")
  
  expect_is(time_series_chart, "ggplot")
  expect_true("Date" %in% names(time_series_chart$data))
})

test_that("Chart Customization", {
  
  # Test 7.5: Custom theme application
  basic_chart <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  themed_chart <- basic_chart + atlas_theme()
  
  expect_is(themed_chart, "ggplot")
  expect_true("theme" %in% names(themed_chart))
  
  # Test 7.6: Color palette consistency
  colors <- get_atlas_colors()
  
  expect_is(colors, "character")
  expect_true(length(colors) >= 5)
  expect_true(all(grepl("^#[0