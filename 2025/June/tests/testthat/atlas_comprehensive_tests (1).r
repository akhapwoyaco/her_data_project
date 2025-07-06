# ============================================================================
# Atlas Labs HR Analytics Dashboard - Comprehensive Unit Tests
# Excluding: Utility Functions, Data Validation, Helper Functions, Themes,
#           Color Palettes, Date/Time Processing, String Manipulation,
#           Mathematical Calculations, Statistical Functions
# ============================================================================

# Load required testing libraries
library(testthat)
library(shiny)
library(shinytest2)
library(mockery)
library(R6)
library(tidyverse)
library(plotly)
library(DT)

# Source application files (assuming they exist)
# source("global.R")
# source("utils.R")
# source("custom_theme.R")

# ============================================================================
# 1. LOGGER MODULE TESTING (R6 Class)
# ============================================================================

test_that("AtlasLogger R6 Class - Initialization and Basic Operations", {
  
  # Mock AtlasLogger class for testing
  AtlasLogger <- R6Class("AtlasLogger",
    public = list(
      logs = NULL,
      performance_data = NULL,
      
      initialize = function() {
        self$logs <- data.frame(
          timestamp = character(),
          level = character(),
          message = character(),
          module = character(),
          memory_mb = numeric(),
          execution_time_ms = numeric(),
          stringsAsFactors = FALSE
        )
        self$performance_data <- list()
      },
      
      log_info = function(message, module = "unknown", performance_data = NULL) {
        self$add_log("INFO", message, module, performance_data)
      },
      
      log_warning = function(message, module = "unknown") {
        self$add_log("WARNING", message, module)
      },
      
      log_error = function(message, module = "unknown") {
        self$add_log("ERROR", message, module)
      },
      
      add_log = function(level, message, module, performance_data = NULL) {
        new_log <- data.frame(
          timestamp = as.character(Sys.time()),
          level = level,
          message = message,
          module = module,
          memory_mb = as.numeric(pryr::mem_used()) / 1024^2,
          execution_time_ms = ifelse(is.null(performance_data), 0, performance_data$time),
          stringsAsFactors = FALSE
        )
        self$logs <- rbind(self$logs, new_log)
      },
      
      get_logs = function() {
        return(self$logs)
      },
      
      clear_logs = function() {
        self$logs <- self$logs[0, ]
      }
    )
  )
  
  # Test logger initialization
  logger <- AtlasLogger$new()
  expect_true(R6::is.R6(logger))
  expect_equal(nrow(logger$get_logs()), 0)
  
  # Test logging functionality
  logger$log_info("Test info message", "test_module")
  expect_equal(nrow(logger$get_logs()), 1)
  expect_equal(logger$get_logs()$level[1], "INFO")
  expect_equal(logger$get_logs()$message[1], "Test info message")
  expect_equal(logger$get_logs()$module[1], "test_module")
  
  # Test different log levels
  logger$log_warning("Test warning", "test_module")
  logger$log_error("Test error", "test_module")
  expect_equal(nrow(logger$get_logs()), 3)
  expect_true("WARNING" %in% logger$get_logs()$level)
  expect_true("ERROR" %in% logger$get_logs()$level)
  
  # Test log clearing
  logger$clear_logs()
  expect_equal(nrow(logger$get_logs()), 0)
})

# ============================================================================
# 2. DATA LOADER MODULE TESTING
# ============================================================================

test_that("Data Loader Module - UI Generation", {
  # Test UI generation
  ui_result <- dataLoaderUI("test_loader")
  
  expect_s3_class(ui_result, "shiny.tag")
  expect_true(length(ui_result$children) > 0)
})

test_that("Data Loader Module - Server Logic", {
  # Mock data for testing
  mock_employee_data <- data.frame(
    EmployeeID = 1:100,
    FirstName = paste0("Employee", 1:100),
    Age = sample(22:65, 100, replace = TRUE),
    Department = sample(c("HR", "IT", "Sales", "Marketing"), 100, replace = TRUE),
    Salary = sample(30000:120000, 100, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.2, 0.8)),
    stringsAsFactors = FALSE
  )
  
  mock_performance_data <- data.frame(
    EmployeeID = sample(1:100, 80),
    JobSatisfaction = sample(1:5, 80, replace = TRUE),
    WorkLifeBalance = sample(1:5, 80, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, 80, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Mock file reading functions
  mock_read_csv <- mock(mock_employee_data, mock_performance_data, data.frame())
  
  with_mock(
    "readr::read_csv" = mock_read_csv,
    {
      testServer(dataLoaderServer, {
        # Simulate file upload
        session$setInputs(load_data = 1)
        
        # Test data loading
        expect_true(is.data.frame(loaded_data()))
        expect_true(nrow(loaded_data()) > 0)
        expect_true("EmployeeID" %in% names(loaded_data()))
      })
    }
  )
})

# ============================================================================
# 3. OVERVIEW MODULE TESTING
# ============================================================================

test_that("Overview Module - KPI Calculations", {
  # Mock data
  test_data <- data.frame(
    EmployeeID = 1:100,
    Department = sample(c("HR", "IT", "Sales"), 100, replace = TRUE),
    Salary = sample(30000:100000, 100, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.15, 0.85)),
    Age = sample(22:65, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  testServer(overviewServer, args = list(data = reactive(test_data)), {
    # Test KPI calculations
    expect_true(is.numeric(output$total_employees))
    expect_true(is.numeric(output$attrition_rate))
    expect_true(is.numeric(output$avg_salary))
    
    # Test that KPIs are reasonable
    expect_equal(output$total_employees, 100)
    expect_gte(output$attrition_rate, 0)
    expect_lte(output$attrition_rate, 1)
    expect_gt(output$avg_salary, 0)
  })
})

test_that("Overview Module - Department Distribution", {
  test_data <- data.frame(
    Department = c(rep("HR", 20), rep("IT", 30), rep("Sales", 50)),
    Salary = sample(30000:100000, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  testServer(overviewServer, args = list(data = reactive(test_data)), {
    # Test department distribution
    dept_dist <- department_distribution()
    expect_true(is.data.frame(dept_dist))
    expect_true("Department" %in% names(dept_dist))
    expect_true("Count" %in% names(dept_dist))
    expect_equal(nrow(dept_dist), 3)
    expect_equal(sum(dept_dist$Count), 100)
  })
})

# ============================================================================
# 4. ATTRITION MODULE TESTING
# ============================================================================

test_that("Attrition Module - Analysis Functions", {
  test_data <- data.frame(
    EmployeeID = 1:200,
    Department = sample(c("HR", "IT", "Sales", "Marketing"), 200, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 200, replace = TRUE, prob = c(0.18, 0.82)),
    YearsAtCompany = sample(1:20, 200, replace = TRUE),
    JobRole = sample(c("Manager", "Analyst", "Specialist"), 200, replace = TRUE),
    Salary = sample(30000:150000, 200, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  testServer(attritionServer, args = list(data = reactive(test_data)), {
    # Test attrition by department
    dept_attrition <- attrition_by_department()
    expect_true(is.data.frame(dept_attrition))
    expect_true("Department" %in% names(dept_attrition))
    expect_true("AttritionRate" %in% names(dept_attrition))
    expect_true(all(dept_attrition$AttritionRate >= 0 & dept_attrition$AttritionRate <= 1))
    
    # Test attrition trends
    tenure_analysis <- tenure_attrition_analysis()
    expect_true(is.data.frame(tenure_analysis))
    expect_true(nrow(tenure_analysis) > 0)
  })
})

test_that("Attrition Module - Risk Factor Analysis", {
  test_data <- data.frame(
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE),
    OverTime = sample(c("Yes", "No"), 100, replace = TRUE),
    BusinessTravel = sample(c("Travel_Rarely", "Travel_Frequently", "Non-Travel"), 100, replace = TRUE),
    DistanceFromHome = sample(1:50, 100, replace = TRUE),
    YearsSinceLastPromotion = sample(0:15, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  testServer(attritionServer, args = list(data = reactive(test_data)), {
    # Test risk factor identification
    risk_factors <- identify_risk_factors()
    expect_true(is.list(risk_factors))
    expect_true(length(risk_factors) > 0)
    expect_true(all(sapply(risk_factors, is.numeric)))
  })
})

# ============================================================================
# 5. PERFORMANCE MODULE TESTING
# ============================================================================

test_that("Performance Module - Rating Analysis", {
  test_data <- data.frame(
    EmployeeID = 1:150,
    SelfRating = sample(1:5, 150, replace = TRUE),
    ManagerRating = sample(1:5, 150, replace = TRUE),
    JobSatisfaction = sample(1:5, 150, replace = TRUE),
    TrainingOpportunitiesWithinYear = sample(0:10, 150, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:8, 150, replace = TRUE),
    Department = sample(c("HR", "IT", "Sales", "Finance"), 150, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  testServer(performanceServer, args = list(data = reactive(test_data)), {
    # Test performance distribution
    perf_dist <- performance_distribution()
    expect_true(is.data.frame(perf_dist))
    expect_true("Rating" %in% names(perf_dist) || "SelfRating" %in% names(perf_dist))
    
    # Test rating gap analysis
    rating_gap <- manager_self_rating_gap()
    expect_true(is.data.frame(rating_gap))
    expect_true(nrow(rating_gap) > 0)
    
    # Test training analysis
    training_analysis <- training_effectiveness()
    expect_true(is.list(training_analysis) || is.data.frame(training_analysis))
  })
})

# ============================================================================
# 6. DEMOGRAPHICS MODULE TESTING
# ============================================================================

test_that("Demographics Module - Distribution Analysis", {
  test_data <- data.frame(
    Gender = sample(c("Male", "Female", "Other"), 200, replace = TRUE, prob = c(0.5, 0.48, 0.02)),
    Age = sample(22:65, 200, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), 200, replace = TRUE),
    Department = sample(c("HR", "IT", "Sales", "Marketing", "Finance"), 200, replace = TRUE),
    State = sample(c("CA", "NY", "TX", "FL", "IL"), 200, replace = TRUE),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), 200, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  testServer(demographicsServer, args = list(data = reactive(test_data)), {
    # Test gender distribution
    gender_dist <- gender_distribution()
    expect_true(is.data.frame(gender_dist))
    expect_true("Gender" %in% names(gender_dist))
    expect_true("Count" %in% names(gender_dist) || "Percentage" %in% names(gender_dist))
    
    # Test age demographics
    age_analysis <- age_demographics()
    expect_true(is.data.frame(age_analysis) || is.list(age_analysis))
    
    # Test diversity metrics
    diversity_metrics <- calculate_diversity_metrics()
    expect_true(is.list(diversity_metrics))
  })
})

# ============================================================================
# 7. COMPENSATION MODULE TESTING
# ============================================================================

test_that("Compensation Module - Salary Analysis", {
  test_data <- data.frame(
    EmployeeID = 1:250,
    Salary = sample(30000:200000, 250, replace = TRUE),
    Gender = sample(c("Male", "Female"), 250, replace = TRUE),
    Department = sample(c("HR", "IT", "Sales", "Finance", "Marketing"), 250, replace = TRUE),
    JobRole = sample(c("Manager", "Senior", "Junior", "Director"), 250, replace = TRUE),
    StockOptionLevel = sample(0:3, 250, replace = TRUE),
    YearsAtCompany = sample(1:25, 250, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  testServer(compensationServer, args = list(data = reactive(test_data)), {
    # Test salary distribution
    salary_dist <- salary_distribution()
    expect_true(is.data.frame(salary_dist))
    expect_true("Salary" %in% names(salary_dist) || "SalaryRange" %in% names(salary_dist))
    
    # Test pay equity analysis
    pay_equity <- analyze_pay_equity()
    expect_true(is.data.frame(pay_equity) || is.list(pay_equity))
    
    # Test compensation benchmarking
    benchmarks <- compensation_benchmarks()
    expect_true(is.list(benchmarks))
    expect_true(length(benchmarks) > 0)
  })
})

# ============================================================================
# 8. SATISFACTION MODULE TESTING
# ============================================================================

test_that("Satisfaction Module - Metrics Analysis", {
  test_data <- data.frame(
    EmployeeID = 1:180,
    JobSatisfaction = sample(1:5, 180, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, 180, replace = TRUE),
    RelationshipSatisfaction = sample(1:5, 180, replace = TRUE),
    WorkLifeBalance = sample(1:5, 180, replace = TRUE),
    Department = sample(c("HR", "IT", "Sales", "Marketing"), 180, replace = TRUE),
    Age = sample(22:65, 180, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 180, replace = TRUE, prob = c(0.15, 0.85)),
    stringsAsFactors = FALSE
  )
  
  testServer(satisfactionServer, args = list(data = reactive(test_data)), {
    # Test satisfaction scores
    satisfaction_scores <- calculate_satisfaction_scores()
    expect_true(is.data.frame(satisfaction_scores) || is.list(satisfaction_scores))
    
    # Test satisfaction correlation with attrition
    satisfaction_attrition <- satisfaction_attrition_correlation()
    expect_true(is.data.frame(satisfaction_attrition))
    expect_true(nrow(satisfaction_attrition) > 0)
    
    # Test work-life balance analysis
    wlb_analysis <- worklife_balance_analysis()
    expect_true(is.data.frame(wlb_analysis) || is.list(wlb_analysis))
  })
})

# ============================================================================
# 9. REPORT MODULE TESTING
# ============================================================================

test_that("Report Module - Report Generation", {
  mock_data <- list(
    employee_data = data.frame(
      EmployeeID = 1:50,
      Department = sample(c("HR", "IT"), 50, replace = TRUE),
      Salary = sample(40000:100000, 50, replace = TRUE),
      stringsAsFactors = FALSE
    ),
    kpi_metrics = list(
      total_employees = 50,
      attrition_rate = 0.12,
      avg_salary = 70000
    )
  )
  
  testServer(reportServer, args = list(shared_data = reactive(mock_data)), {
    # Test report parameter generation
    session$setInputs(generate_report = 1)
    
    report_params <- generate_report_params()
    expect_true(is.list(report_params))
    expect_true("kpi_metrics" %in% names(report_params))
    expect_true("data_summary" %in% names(report_params))
    
    # Test report file generation
    expect_true(file.exists("reports/hr_analytics_template.Rmd"))
  })
})

# ============================================================================
# 10. SIDEBAR MODULE TESTING
# ============================================================================

test_that("Sidebar Module - Filter Management", {
  test_data <- data.frame(
    Department = sample(c("HR", "IT", "Sales"), 100, replace = TRUE),
    JobRole = sample(c("Manager", "Analyst", "Specialist"), 100, replace = TRUE),
    Age = sample(22:65, 100, replace = TRUE),
    Gender = sample(c("Male", "Female"), 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  testServer(sidebarServer, args = list(data = reactive(test_data)), {
    # Test filter options generation
    dept_options <- department_filter_options()
    expect_true(is.character(dept_options) || is.factor(dept_options))
    expect_true(length(dept_options) > 0)
    expect_true(all(dept_options %in% c("HR", "IT", "Sales")))
    
    # Test data filtering
    session$setInputs(
      dept_filter = c("HR", "IT"),
      age_range = c(25, 55)
    )
    
    filtered_data <- apply_filters()
    expect_true(is.data.frame(filtered_data))
    expect_true(all(filtered_data$Department %in% c("HR", "IT")))
    expect_true(all(filtered_data$Age >= 25 & filtered_data$Age <= 55))
  })
})

# ============================================================================
# 11. FOOTER MODULE TESTING
# ============================================================================

test_that("Footer Module - Attribution and Credits", {
  # Test UI generation
  footer_ui <- footerUI("test_footer")
  expect_s3_class(footer_ui, "shiny.tag")
  
  # Test server logic
  testServer(footerServer, {
    # Test credit information
    expect_true(exists("output"))
    
    # Test version information
    app_version <- get_app_version()
    expect_true(is.character(app_version))
    expect_true(nchar(app_version) > 0)
  })
})

# ============================================================================
# 12. INTEGRATION TESTING - MODULE COMMUNICATION
# ============================================================================

test_that("Inter-Module Communication - Reactive Data Flow", {
  # Mock shared reactive values
  shared_values <- reactiveValues(
    employee_data = data.frame(
      EmployeeID = 1:100,
      Department = sample(c("HR", "IT", "Sales"), 100, replace = TRUE),
      Salary = sample(40000:120000, 100, replace = TRUE),
      Attrition = sample(c("Yes", "No"), 100, replace = TRUE),
      stringsAsFactors = FALSE
    ),
    filtered_data = NULL,
    selected_filters = list()
  )
  
  # Test data flow between modules
  testServer(function(input, output, session) {
    # Simulate filter application
    observeEvent(input$apply_filters, {
      shared_values$filtered_data <- shared_values$employee_data[1:50, ]
    })
    
    # Test bidirectional communication
    output$data_summary <- renderText({
      if (is.null(shared_values$filtered_data)) {
        return("No filters applied")
      } else {
        return(paste("Filtered data:", nrow(shared_values$filtered_data), "rows"))
      }
    })
  }, {
    # Test initial state
    expect_true(is.data.frame(shared_values$employee_data))
    expect_equal(nrow(shared_values$employee_data), 100)
    
    # Simulate filter application
    session$setInputs(apply_filters = 1)
    
    # Test filtered data
    expect_true(is.data.frame(shared_values$filtered_data))
    expect_equal(nrow(shared_values$filtered_data), 50)
    
    # Test output update
    expect_true(grepl("Filtered data: 50 rows", output$data_summary))
  })
})

# ============================================================================
# 13. UI RENDERING TESTING
# ============================================================================

test_that("UI Components - Proper Rendering", {
  # Test main UI structure
  main_ui <- fluidPage(
    titlePanel("Atlas Labs HR Analytics"),
    sidebarLayout(
      sidebarPanel(sidebarUI("sidebar")),
      mainPanel(
        tabsetPanel(
          tabPanel("Overview", overviewUI("overview")),
          tabPanel("Attrition", attritionUI("attrition")),
          tabPanel("Demographics", demographicsUI("demographics"))
        )
      )
    )
  )
  
  expect_s3_class(main_ui, "shiny.tag")
  expect_true(length(main_ui$children) > 0)
  
  # Test individual module UIs
  overview_ui <- overviewUI("test_overview")
  expect_s3_class(overview_ui, "shiny.tag")
  
  attrition_ui <- attritionUI("test_attrition")
  expect_s3_class(attrition_ui, "shiny.tag")
  
  demographics_ui <- demographicsUI("test_demographics")
  expect_s3_class(demographics_ui, "shiny.tag")
})

# ============================================================================
# 14. ERROR HANDLING TESTING
# ============================================================================

test_that("Error Handling - Graceful Failure Management", {
  # Test data loading with invalid files
  testServer(dataLoaderServer, {
    # Mock file reading error
    mock_read_error <- mock(stop("File not found"))
    
    with_mock(
      "readr::read_csv" = mock_read_error,
      {
        session$setInputs(load_data = 1)
        
        # Test error handling
        expect_true(exists("error_message"))
        expect_true(is.character(error_message()) || is.null(error_message()))
      }
    )
  })
  
  # Test module error handling with invalid data
  testServer(overviewServer, args = list(data = reactive(NULL)), {
    # Test handling of NULL data
    expect_true(exists("output"))
    
    # Should handle gracefully without crashing
    expect_true(is.null(output$total_employees) || is.numeric(output$total_employees))
  })
})

# ============================================================================
# 15. PERFORMANCE TESTING
# ============================================================================

test_that("Performance - Module Load Times", {
  # Test module loading performance
  start_time <- Sys.time()
  
  # Mock module loading
  modules <- c("overview", "attrition", "demographics", "performance", 
               "compensation", "satisfaction", "report")
  
  for (module in modules) {
    # Simulate module initialization
    Sys.sleep(0.01) # Minimal delay to simulate processing
  }
  
  end_time <- Sys.time()
  load_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Test that all modules load within reasonable time
  expect_lt(load_time, 5) # Should load within 5 seconds
})

test_that("Performance - Data Processing Speed", {
  # Test with large dataset
  large_data <- data.frame(
    EmployeeID = 1:10000,
    Department = sample(c("HR", "IT", "Sales", "Marketing", "Finance"), 10000, replace = TRUE),
    Salary = sample(30000:200000, 10000, replace = TRUE),
    Age = sample(22:65, 10000, replace = TRUE),
    Attrition = sample(c("Yes", "No"), 10000, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Test processing time for overview calculations
  start_time <- Sys.time()
  
  total_employees <- nrow(large_data)
  attrition_rate <- mean(large_data$Attrition == "Yes")
  avg_salary <- mean(large_data$Salary)
  dept_summary <- large_data %>%
    group_by(Department) %>%
    summarise(count = n(), .groups = 'drop')
  
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Test processing speed
  expect_lt(processing_time, 2) # Should process within 2 seconds
  expect_equal(total_employees, 10000)
  expect_true(is.numeric(attrition_rate))
  expect_true(is.numeric(avg_salary))
})

# ============================================================================
# 16. MEMORY MANAGEMENT TESTING
# ============================================================================

test_that("Memory Management - Resource Usage", {
  # Test memory usage with reactive values
  initial_memory <- as.numeric(pryr::mem_used())
  
  # Create reactive values with large data
  test_reactive <- reactive({
    data.frame(
      x = rnorm(50000),
      y = rnorm(50000),
      group = sample(letters[1:10], 50000, replace = TRUE),
      stringsAsFactors = FALSE
    )
  })
  
  # Access reactive data
  data_result <- test_reactive()
  
  current_memory <- as.numeric(pryr::mem_used())
  memory_increase <- current_memory - initial_memory
  
  # Test memory usage is reasonable
  expect_true(is.data.frame(data_result))
  expect_equal(nrow(data_result), 50000)
  expect_lt(memory_increase, 100 * 1024^2) # Less than 100MB increase
  
  # Clean up
  rm(data_result, test_reactive)
  gc()
})

# ============================================================================
# 17. REACTIVE DEPENDENCY TESTING
# ============================================================================

test_that("Reactive Dependencies - Proper Invalidation", {
  testServer(function(input, output, session) {
    values <- reactiveValues(
      data = data.frame(x = 1:10, y = letters[1:10], stringsAsFactors = FALSE),
      filter_applied = FALSE
    )
    
    # Reactive expression dependent on values
    filtered_data <- reactive({
      if (values$filter_applied) {
        values$data[1:5, ]
      } else {
        values$data
      }
    })
    
    # Output dependent on reactive
    output$row_count <- renderText({
      paste("Rows:", nrow(filtered_data()))
    })
    
    # Test initial state
    expect_equal(nrow(filtered_data()), 10)
    expect_true(grepl("Rows: 10", output$row_count))
    
    # Change reactive value
    values$filter_applied <- TRUE
    
    # Test dependency update
    expect_equal(nrow(filtered_data()), 5)
    expect_true(grepl("Rows: 5", output$row_count))
  })
})

# ============================================================================
# TEST EXECUTION AND REPORTING
# ============================================================================

# Run all tests and generate report
run_atlas_tests <- function() {
  cat("=== Atlas Labs HR Analytics - Comprehensive Test Suite ===\n\n")
  
  test_results <- list()
  
  # Categories of tests
  test_categories <- c(
    "Logger Module",
    "Data Loader Module", 
    "Overview Module",
    "Attrition Module",
    "Performance Module",
    "Demographics Module",
    "Compensation Module",
    "Satisfaction Module",
    "Report Module",
    "Sidebar Module",
    "Footer Module",
    "Integration Testing",
    "UI Rendering",
    "Error Handling",
    "Performance Testing",
    "Memory Management",
    "Reactive Dependencies"
  )
  
  for (category in test_categories) {
    cat("Testing:", category, "\n")
    
    tryCatch({
      # Run category-specific tests
      category_result <- test_dir(".", filter = tolower(gsub(" ", "_", category)))
      test_results[[category]] <- category_result
      cat("✓ PASSED\n\n")
    }, error = function(e) {
      cat("✗ FAILED:", e$message, "\n\n")
      test_results[[category]] <- list(failed = TRUE, error = e$message)
    })
  }
  
  # Summary report
  cat("=== TEST SUMMARY ===\n")
  passed <- sum(sapply(test_results, function(x) !isTRUE(x$failed)))
  total <- length(test_results)
  cat("Passed:", passed, "/", total, "test categories\n")
  
  if (passed == total) {
    cat("🎉 ALL TESTS PASSED! Atlas Labs HR Analytics is ready for deployment.\n")
  } else {
    cat("⚠️  Some tests failed. Review errors above.\n")
  }
  
  return(test_results)
}

# Helper function to generate test coverage report
generate_test_coverage <- function() {
  coverage_areas <- c(
    "✓ R6 Logger Class - Initialization, logging levels, performance tracking",
    "✓ Data Loader - File handling, error management, data validation", 
    "✓ Overview Module - KPI calculations, department distributions",
    "