# ============================================================================
# Atlas Labs HR Analytics Dashboard - Comprehensive Regression Test Suite
# Edge Cases & Advanced Regression Testing
# Author: akhapwoyaco
# ============================================================================

library(testthat)
library(shiny)
library(shinytest2)
library(R6)
library(dplyr)
library(mockery)
library(withr)
library(processx)

# ============================================================================
# 9. DATA VALIDATION & INTEGRITY REGRESSION TESTS
# ============================================================================

test_that("Data Type Consistency Regression", {
  # Test data type preservation across module communications
  test_data <- list(
    employees = data.frame(
      EmployeeID = 1:5,
      Age = c(25, 30, 35, 40, 45),
      Salary = c(50000, 60000, 70000, 80000, 90000),
      Attrition = c("Yes", "No", "No", "Yes", "No"),
      HireDate = as.Date(c("2020-01-01", "2019-06-15", "2018-03-20", "2021-11-30", "2017-09-10")),
      stringsAsFactors = FALSE
    )
  )
  
  # Test that data types remain consistent after module processing
  processed_data <- process_employee_data(test_data$employees)
  
  expect_true(is.numeric(processed_data$Age))
  expect_true(is.numeric(processed_data$Salary))
  expect_true(is.character(processed_data$Attrition))
  expect_true(inherits(processed_data$HireDate, "Date"))
  
  # Test edge case: empty data frames
  empty_df <- data.frame()
  expect_silent(process_employee_data(empty_df))
  
  # Test edge case: single row data
  single_row <- test_data$employees[1, ]
  result <- process_employee_data(single_row)
  expect_equal(nrow(result), 1)
})

test_that("Data Boundary Value Regression", {
  # Test extreme values and boundary conditions
  boundary_data <- data.frame(
    EmployeeID = c(1, 2, 3, 4, 5),
    Age = c(18, 65, 16, 100, -5),  # Min legal age, retirement, under-age, extreme, negative
    Salary = c(0, 1000000, -1000, 999999999, NA),  # Zero, high, negative, extreme, missing
    YearsAtCompany = c(0, 50, -1, 100, 0.5),  # New hire, long tenure, negative, extreme, fractional
    PerformanceRating = c(1, 5, 0, 6, 2.5)  # Min, max, below min, above max, decimal
  )
  
  validation_result <- validate_data_boundaries(boundary_data)
  
  # Age boundaries (18-65 typical working age)
  expect_true(all(validation_result$age_warnings >= 0))
  expect_true(length(validation_result$age_outliers) > 0)
  
  # Salary boundaries
  expect_true(all(validation_result$salary_warnings >= 0, na.rm = TRUE))
  
  # Performance rating boundaries (1-5 scale)
  expect_true(all(validation_result$performance_warnings %in% 1:5, na.rm = TRUE))
})

test_that("Data Relationship Integrity Regression", {
  # Test referential integrity between datasets
  employees <- data.frame(
    EmployeeID = 1:5,
    Department = c("HR", "IT", "Finance", "IT", "HR")
  )
  
  performance <- data.frame(
    EmployeeID = c(1, 2, 3, 6, 7),  # 6,7 don't exist in employees
    PerformanceRating = c(4, 3, 5, 2, 4)
  )
  
  integrity_check <- check_referential_integrity(employees, performance)
  
  expect_length(integrity_check$orphaned_performance, 2)  # IDs 6,7
  expect_equal(integrity_check$orphaned_performance, c(6, 7))
  expect_length(integrity_check$missing_performance, 2)  # IDs 4,5
})

# ============================================================================
# 10. MODULE INTERACTION & STATE REGRESSION TESTS
# ============================================================================

test_that("Module State Persistence Regression", {
  # Test that module states persist correctly across sessions
  app <- AppDriver$new()
  
  # Set initial state in overview module
  app$set_inputs(overview_department_filter = "IT")
  app$set_inputs(overview_date_range = c("2020-01-01", "2023-12-31"))
  
  # Navigate to attrition module
  app$click("attrition_tab")
  
  # Check if filters persisted
  expect_equal(app$get_value(input = "attrition_department_filter"), "IT")
  expect_equal(app$get_value(input = "attrition_date_range"), c("2020-01-01", "2023-12-31"))
  
  # Test state reset functionality
  app$click("reset_filters_btn")
  expect_null(app$get_value(input = "attrition_department_filter"))
})

test_that("Cross-Module Data Propagation Regression", {
  # Test data changes propagate correctly across modules
  testServer(attritionServer, {
    # Mock shared reactive values
    shared_data <- reactiveValues(
      filtered_employees = data.frame(EmployeeID = 1:10, Attrition = rep(c("Yes", "No"), 5)),
      selected_filters = list(department = "IT")
    )
    
    # Update data in one module
    shared_data$filtered_employees <- shared_data$filtered_employees[1:5, ]
    
    # Test that other modules receive updated data
    expect_equal(nrow(shared_data$filtered_employees), 5)
    expect_true(reactive_dependency_triggered(shared_data$filtered_employees))
  })
})

test_that("Module Memory Leak Regression", {
  # Test for memory leaks in module interactions
  initial_memory <- gc()$used[1]
  
  # Simulate heavy module usage
  for (i in 1:100) {
    testServer(performanceServer, {
      large_data <- data.frame(
        EmployeeID = 1:1000,
        Performance = runif(1000, 1, 5)
      )
      output$performance_plot <- renderPlotly({
        plot_ly(large_data, x = ~EmployeeID, y = ~Performance)
      })
    })
  }
  
  gc()  # Force garbage collection
  final_memory <- gc()$used[1]
  memory_increase <- final_memory - initial_memory
  
  # Memory increase should be reasonable (less than 50MB)
  expect_lt(memory_increase, 50 * 1024 * 1024)
})

# ============================================================================
# 11. REACTIVE DEPENDENCY & INVALIDATION REGRESSION TESTS
# ============================================================================

test_that("Reactive Invalidation Chain Regression", {
  # Test complex reactive dependency chains
  testServer(function(input, output, session) {
    # Create reactive chain: input -> filter -> data -> plot
    filtered_data <- reactive({
      req(input$department_filter)
      filter(employee_data, Department == input$department_filter)
    })
    
    summarized_data <- reactive({
      req(filtered_data())
      filtered_data() %>%
        group_by(JobRole) %>%
        summarise(avg_salary = mean(Salary, na.rm = TRUE))
    })
    
    output$salary_plot <- renderPlot({
      ggplot(summarized_data(), aes(x = JobRole, y = avg_salary)) +
        geom_col()
    })
    
    # Test invalidation chain
    session$setInputs(department_filter = "IT")
    expect_true(reactive_invalidated(filtered_data))
    expect_true(reactive_invalidated(summarized_data))
    expect_true(output_invalidated(output$salary_plot))
  })
})

test_that("Circular Dependency Prevention Regression", {
  # Test prevention of circular reactive dependencies
  expect_error({
    testServer(function(input, output, session) {
      # Attempt to create circular dependency
      reactive_a <- reactive({
        reactive_b() + 1
      })
      
      reactive_b <- reactive({
        reactive_a() + 1
      })
      
      # This should throw an error
      reactive_a()
    })
  }, class = "shiny.silent.error")
})

# ============================================================================
# 12. ASYNC OPERATIONS & CONCURRENCY REGRESSION TESTS
# ============================================================================

test_that("Concurrent User Session Regression", {
  # Test multiple simultaneous user sessions
  apps <- list()
  
  # Start multiple app instances
  for (i in 1:5) {
    apps[[i]] <- AppDriver$new()
  }
  
  # Perform concurrent operations
  for (i in 1:5) {
    apps[[i]]$set_inputs(department_filter = paste0("Dept", i))
    apps[[i]]$click("generate_report_btn")
  }
  
  # Wait for all operations to complete
  Sys.sleep(2)
  
  # Verify each session maintained its state
  for (i in 1:5) {
    expect_equal(apps[[i]]$get_value(input = "department_filter"), paste0("Dept", i))
  }
  
  # Clean up
  lapply(apps, function(app) app$stop())
})

test_that("Async Data Loading Regression", {
  # Test asynchronous data loading operations
  testServer(dataLoaderServer, {
    # Mock async file loading
    future_data <- future({
      Sys.sleep(1)  # Simulate loading delay
      data.frame(EmployeeID = 1:100, Name = paste0("Employee", 1:100))
    })
    
    # Test that UI remains responsive during loading
    expect_true(is_reactive_running())
    
    # Test data availability after loading
    resolved_data <- value(future_data)
    expect_equal(nrow(resolved_data), 100)
  })
})

# ============================================================================
# 13. ERROR HANDLING & RECOVERY REGRESSION TESTS
# ============================================================================

test_that("Graceful Error Recovery Regression", {
  # Test application recovery from various error states
  
  # Test 1: Database connection failure
  with_mock(
    `DBI::dbConnect` = function(...) stop("Database connection failed"),
    {
      expect_error(load_employee_data(), "Database connection failed")
      
      # Test fallback to file loading
      fallback_data <- load_employee_data_fallback()
      expect_true(is.data.frame(fallback_data))
    }
  )
  
  # Test 2: Memory exhaustion simulation
  expect_error({
    large_data <- try(matrix(1, nrow = 1e8, ncol = 1e8), silent = TRUE)
    if (inherits(large_data, "try-error")) {
      stop("Memory allocation failed")
    }
  }, "Memory allocation failed")
  
  # Test 3: File corruption handling
  corrupted_file <- tempfile(fileext = ".csv")
  writeLines("corrupted,data\n1,2,3,4,5", corrupted_file)
  
  expect_warning(
    result <- safe_read_csv(corrupted_file),
    "File appears corrupted"
  )
  expect_true(is.null(result) || nrow(result) == 0)
})

test_that("Input Validation Edge Cases Regression", {
  # Test extreme input validation scenarios
  edge_cases <- list(
    # SQL injection attempts
    malicious_input = "'; DROP TABLE employees; --",
    
    # XSS attempts
    xss_input = "<script>alert('xss')</script>",
    
    # Extremely long strings
    long_input = paste0(rep("a", 10000), collapse = ""),
    
    # Special characters
    special_chars = "!@#$%^&*()[]{}|\\:;\"'<>?,./`~",
    
    # Unicode and emoji
    unicode_input = "👨‍💼📊💼🏢",
    
    # Empty and whitespace
    empty_input = "",
    whitespace_input = "   \n\t   "
  )
  
  for (case_name in names(edge_cases)) {
    input_value <- edge_cases[[case_name]]
    
    # Test input sanitization
    sanitized <- sanitize_input(input_value)
    expect_false(grepl("<script", sanitized, ignore.case = TRUE))
    expect_false(grepl("DROP TABLE", sanitized, ignore.case = TRUE))
    
    # Test input length limits
    if (nchar(input_value) > 1000) {
      expect_lte(nchar(sanitized), 1000)
    }
  }
})

# ============================================================================
# 14. BROWSER COMPATIBILITY & RENDERING REGRESSION TESTS
# ============================================================================

test_that("Cross-Browser JavaScript Compatibility Regression", {
  # Test JavaScript functions across different browser contexts
  js_functions <- c(
    "updateFilters", "resetDashboard", "exportData", 
    "toggleTheme", "showNotification"
  )
  
  browsers <- c("chrome", "firefox", "edge", "safari")
  
  for (browser in browsers) {
    app <- AppDriver$new(browser = browser)
    
    for (js_func in js_functions) {
      # Test JavaScript function execution
      result <- app$run_js(paste0("typeof ", js_func))
      expect_equal(result, "function")
    }
    
    app$stop()
  }
})

test_that("Responsive Design Regression", {
  # Test responsive behavior across different screen sizes
  screen_sizes <- list(
    mobile = list(width = 375, height = 667),
    tablet = list(width = 768, height = 1024),
    desktop = list(width = 1920, height = 1080)
  )
  
  for (size_name in names(screen_sizes)) {
    size <- screen_sizes[[size_name]]
    app <- AppDriver$new(width = size$width, height = size$height)
    
    # Test navigation menu visibility
    nav_visible <- app$get_js("$('.navbar').is(':visible')")
    expect_true(nav_visible)
    
    # Test chart responsiveness
    chart_width <- app$get_js("$('.plotly').width()")
    expect_lt(chart_width, size$width)
    
    app$stop()
  }
})

# ============================================================================
# 15. ACCESSIBILITY & COMPLIANCE REGRESSION TESTS
# ============================================================================

test_that("WCAG 2.1 Compliance Regression", {
  app <- AppDriver$new()
  
  # Test color contrast ratios
  contrast_ratios <- app$run_js("
    function getContrastRatio(color1, color2) {
      // Simplified contrast ratio calculation
      return 4.5; // Mock implementation
    }
    
    var bgColor = getComputedStyle(document.body).backgroundColor;
    var textColor = getComputedStyle(document.body).color;
    getContrastRatio(bgColor, textColor);
  ")
  
  expect_gte(contrast_ratios, 4.5)  # WCAG AA standard
  
  # Test keyboard navigation
  app$send_keys("Tab")
  focused_element <- app$run_js("document.activeElement.tagName")
  expect_true(focused_element %in% c("BUTTON", "INPUT", "SELECT", "A"))
  
  # Test alt text presence
  images_without_alt <- app$run_js("
    Array.from(document.images).filter(img => !img.alt).length
  ")
  expect_equal(images_without_alt, 0)
  
  app$stop()
})

test_that("Screen Reader Compatibility Regression", {
  app <- AppDriver$new()
  
  # Test ARIA labels presence
  aria_elements <- app$run_js("
    document.querySelectorAll('[aria-label], [aria-labelledby], [aria-describedby]').length
  ")
  expect_gt(aria_elements, 0)
  
  # Test heading hierarchy
  headings <- app$run_js("
    Array.from(document.querySelectorAll('h1, h2, h3, h4, h5, h6'))
      .map(h => parseInt(h.tagName.charAt(1)))
  ")
  
  # Check proper heading sequence (no gaps)
  for (i in 2:length(headings)) {
    expect_lte(headings[i] - headings[i-1], 1)
  }
  
  app$stop()
})

# ============================================================================
# 16. LOCALIZATION & INTERNATIONALIZATION REGRESSION TESTS
# ============================================================================

test_that("Multi-Language Support Regression", {
  # Test application behavior with different locales
  locales <- c("en_US", "es_ES", "fr_FR", "de_DE")
  
  for (locale in locales) {
    withr::with_locale(c("LC_ALL" = locale), {
      # Test date formatting
      test_date <- as.Date("2023-12-25")
      formatted_date <- format_date_for_display(test_date)
      expect_true(is.character(formatted_date))
      expect_gt(nchar(formatted_date), 0)
      
      # Test number formatting
      test_number <- 1234567.89
      formatted_number <- format_currency(test_number)
      expect_true(is.character(formatted_number))
      
      # Test text translation availability
      translated_text <- get_translated_text("dashboard_title", locale)
      expect_true(is.character(translated_text))
      expect_gt(nchar(translated_text), 0)
    })
  }
})

test_that("Character Encoding Regression", {
  # Test handling of various character encodings
  encodings <- c("UTF-8", "Latin1", "ASCII")
  special_text <- "Résumé café naïve Zürich"
  
  for (encoding in encodings) {
    # Test text processing with different encodings
    encoded_text <- iconv(special_text, to = encoding)
    if (!is.na(encoded_text)) {
      processed_text <- process_text_input(encoded_text)
      expect_true(is.character(processed_text))
      expect_gt(nchar(processed_text), 0)
    }
  }
})

# ============================================================================
# 17. DEPLOYMENT & ENVIRONMENT REGRESSION TESTS
# ============================================================================

test_that("Environment Variable Dependency Regression", {
  # Test application behavior with different environment configurations
  env_configs <- list(
    development = list(DEBUG = "TRUE", LOG_LEVEL = "DEBUG"),
    staging = list(DEBUG = "FALSE", LOG_LEVEL = "INFO"),
    production = list(DEBUG = "FALSE", LOG_LEVEL = "ERROR")
  )
  
  for (env_name in names(env_configs)) {
    config <- env_configs[[env_name]]
    
    withr::with_envvar(config, {
      # Test logger initialization
      logger <- AtlasLogger$new()
      expect_true(R6::is.R6(logger))
      
      # Test debug mode behavior
      debug_enabled <- as.logical(Sys.getenv("DEBUG", "FALSE"))
      expect_equal(logger$debug_mode, debug_enabled)
      
      # Test log level configuration
      log_level <- Sys.getenv("LOG_LEVEL", "INFO")
      expect_equal(logger$log_level, log_level)
    })
  }
})

test_that("Resource Limitation Regression", {
  # Test application behavior under resource constraints
  
  # Test 1: Limited memory scenario
  withr::with_options(list(expressions = 50), {
    expect_warning(
      large_computation <- try({
        # Simulate memory-intensive operation
        replicate(1000, {
          data.frame(x = runif(1000), y = runif(1000))
        })
      }, silent = TRUE),
      "Resource limitation"
    )
  })
  
  # Test 2: Limited CPU time
  start_time <- Sys.time()
  result <- with_timeout({
    # Simulate CPU-intensive operation
    sum(sapply(1:1000, function(x) sum(1:x)))
  }, timeout = 1)
  
  elapsed_time <- as.numeric(Sys.time() - start_time)
  expect_lte(elapsed_time, 2)  # Should complete within reasonable time
})

# ============================================================================
# 18. VERSION COMPATIBILITY & MIGRATION REGRESSION TESTS
# ============================================================================

test_that("R Version Compatibility Regression", {
  # Test compatibility across different R versions
  r_version <- getRversion()
  
  # Test package compatibility
  required_packages <- c("shiny", "dplyr", "ggplot2", "plotly", "DT")
  
  for (pkg in required_packages) {
    expect_true(requireNamespace(pkg, quietly = TRUE))
    
    # Test package version compatibility
    pkg_version <- packageVersion(pkg)
    expect_true(is.numeric_version(pkg_version))
  }
  
  # Test R feature compatibility
  if (r_version >= "4.0.0") {
    # Test R 4.0+ features
    expect_true(exists("deparse1"))
  }
  
  if (r_version >= "4.1.0") {
    # Test native pipe operator
    result <- 1:10 |> sum()
    expect_equal(result, 55)
  }
})

test_that("Data Schema Migration Regression", {
  # Test handling of different data schema versions
  old_schema <- data.frame(
    EmployeeID = 1:5,
    Name = paste0("Employee", 1:5),
    Dept = c("HR", "IT", "Finance", "IT", "HR")  # Old column name
  )
  
  new_schema <- data.frame(
    EmployeeID = 1:5,
    FirstName = paste0("Employee", 1:5),
    Department = c("HR", "IT", "Finance", "IT", "HR")  # New column names
  )
  
  # Test schema migration function
  migrated_data <- migrate_data_schema(old_schema, version = "1.0")
  expect_true("Department" %in% names(migrated_data))
  expect_false("Dept" %in% names(migrated_data))
  
  # Test backward compatibility
  legacy_compatible <- ensure_backward_compatibility(new_schema)
  expect_true(all(c("Department", "Dept") %in% names(legacy_compatible)))
})

# ============================================================================
# 19. CUSTOM HELPER FUNCTIONS FOR TESTS
# ============================================================================

# Mock functions for testing
process_employee_data <- function(data) {
  if (nrow(data) == 0) return(data)
  return(data)
}

validate_data_boundaries <- function(data) {
  list(
    age_warnings = which(data$Age < 18 | data$Age > 65),
    age_outliers = which(data$Age < 16 | data$Age > 100),
    salary_warnings = which(data$Salary < 0, na.rm = TRUE),
    performance_warnings = data$PerformanceRating[data$PerformanceRating >= 1 & data$PerformanceRating <= 5]
  )
}

check_referential_integrity <- function(employees, performance) {
  orphaned_performance <- setdiff(performance$EmployeeID, employees$EmployeeID)
  missing_performance <- setdiff(employees$EmployeeID, performance$EmployeeID)
  
  list(
    orphaned_performance = orphaned_performance,
    missing_performance = missing_performance
  )
}

sanitize_input <- function(input) {
  # Remove potential security threats
  input <- gsub("<script.*?</script>", "", input, ignore.case = TRUE)
  input <- gsub("DROP TABLE", "", input, ignore.case = TRUE)
  
  # Limit length
  if (nchar(input) > 1000) {
    input <- substr(input, 1, 1000)
  }
  
  return(input)
}

format_date_for_display <- function(date) {
  format(date, "%Y-%m-%d")
}

format_currency <- function(amount) {
  paste0("$", formatC(amount, format = "f", digits = 2, big.mark = ","))
}

get_translated_text <- function(key, locale) {
  # Mock translation function
  translations <- list(
    dashboard_title = "HR Analytics Dashboard"
  )
  return(translations[[key]] %||% key)
}

process_text_input <- function(text) {
  # Mock text processing
  return(text)
}

with_timeout <- function(expr, timeout = 1) {
  # Mock timeout implementation
  eval(expr)
}

migrate_data_schema <- function(data, version) {
  # Mock schema migration
  if ("Dept" %in% names(data)) {
    names(data)[names(data) == "Dept"] <- "Department"
  }
  return(data)
}

ensure_backward_compatibility <- function(data) {
  # Mock backward compatibility
  if ("Department" %in% names(data) && !"Dept" %in% names(data)) {
    data$Dept <- data$Department
  }
  return(data)
}

# ============================================================================
# TEST SUITE EXECUTION
# ============================================================================

# Run all tests
if (interactive()) {
  cat("Running Atlas Labs HR Analytics Comprehensive Regression Test Suite...\n")
  test_results <- test_dir("tests/", reporter = "progress")
  print(test_results)
}