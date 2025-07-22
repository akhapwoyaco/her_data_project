# =============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD - COMPREHENSIVE UNIT TESTS
# =============================================================================
# Author: akhapwoyaco
# Description: Extensive unit testing covering all app functionality except
#              data integrity/ETL testing
# =============================================================================

# Required libraries for testing
library(testthat)
library(shiny)
library(shinytest2)
library(mockery)
library(R6)
library(tidyverse)
library(plotly)
library(DT)
library(profvis)
library(bench)
library(parallel)

# =============================================================================
# 1. R6 LOGGER SYSTEM TESTING
# =============================================================================

test_that("AtlasLogger R6 Class - Core Functionality", {
  
  # Test logger instantiation
  logger <- AtlasLogger$new()
  expect_r6(logger, "AtlasLogger")
  expect_true(is.environment(logger))
  
  # Test basic logging methods exist
  expect_true(exists("log_info", envir = logger))
  expect_true(exists("log_warning", envir = logger))
  expect_true(exists("log_error", envir = logger))
  expect_true(exists("track_memory_usage", envir = logger))
  expect_true(exists("track_execution_time", envir = logger))
  
  # Test log level validation
  expect_error(logger$log_info("", level = "INVALID"))
  expect_no_error(logger$log_info("Test message", module = "test"))
  
  # Test memory tracking
  memory_before <- logger$track_memory_usage()
  expect_type(memory_before, "double")
  expect_gte(memory_before, 0)
  
  # Test performance tracking initialization
  logger$start_performance_tracking("test_operation")
  expect_true("test_operation" %in% names(logger$active_timers))
  
  logger$end_performance_tracking("test_operation")
  expect_true("test_operation" %in% names(logger$performance_log))
})

test_that("AtlasLogger - Color Output and Formatting", {
  
  logger <- AtlasLogger$new()
  
  # Mock console output capture
  output <- capture_output(logger$log_info("Test message", module = "test"))
  expect_true(grepl("Test message", output$out))
  
  # Test color codes in output
  colored_output <- capture_output(logger$log_error("Error message"))
  expect_true(grepl("\\033\\[31m", colored_output$out)) # Red color code
  
  # Test timestamp formatting
  log_entry <- logger$format_log_entry("INFO", "Test", "module")
  expect_match(log_entry, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}")
})

test_that("AtlasLogger - Performance Metrics", {
  
  logger <- AtlasLogger$new()
  
  # Test execution time tracking
  logger$start_performance_tracking("slow_operation")
  Sys.sleep(0.1)
  result <- logger$end_performance_tracking("slow_operation")
  
  expect_gte(result$execution_time, 0.1)
  expect_type(result$execution_time, "double")
  
  # Test memory usage tracking
  initial_memory <- logger$track_memory_usage()
  large_object <- rep(1:1000000, 10)
  final_memory <- logger$track_memory_usage()
  
  expect_gt(final_memory, initial_memory)
  
  # Test performance summary generation
  summary <- logger$get_performance_summary()
  expect_type(summary, "list")
  expect_true("total_operations" %in% names(summary))
  expect_true("average_execution_time" %in% names(summary))
})

test_that("AtlasLogger - Edge Cases and Error Handling", {
  
  logger <- AtlasLogger$new()
  
  # Test NULL message handling
  expect_warning(logger$log_info(NULL))
  expect_warning(logger$log_info(""))
  
  # Test extremely long messages
  long_message <- paste(rep("A", 10000), collapse = "")
  expect_no_error(logger$log_info(long_message))
  
  # Test concurrent logging
  if (parallel::detectCores() > 1) {
    results <- parallel::mclapply(1:10, function(i) {
      logger$log_info(paste("Concurrent message", i), module = paste("module", i))
    }, mc.cores = 2)
    
    expect_length(results, 10)
  }
  
  # Test timer edge cases
  expect_error(logger$end_performance_tracking("nonexistent_timer"))
  
  # Test memory tracking under extreme conditions
  expect_no_error(logger$track_memory_usage())
  
  # Test log rotation when log gets too large
  for (i in 1:1000) {
    logger$log_info(paste("Log entry", i))
  }
  expect_lt(length(logger$log_entries), 1000) # Should have rotated
})

# =============================================================================
# 2. SHINY MODULE TESTING
# =============================================================================

test_that("Data Loader Module - UI Generation", {
  
  # Test UI function exists and returns proper structure
  ui_output <- dataLoaderUI("test")
  expect_s3_class(ui_output, "shiny.tag")
  
  # Test namespace handling
  expect_true(grepl("test-", htmltools::renderTags(ui_output)$html))
  
  # Test required UI elements present
  ui_html <- as.character(ui_output)
  expect_true(grepl("fileInput", ui_html))
  expect_true(grepl("actionButton", ui_html))
})

test_that("Data Loader Module - Server Logic", {
  
  # Create mock session and reactive values
  testServer(dataLoaderServer, {
    
    # Test initial state
    expect_null(output$data_status)
    
    # Mock file input
    session$setInputs(
      employee_file = data.frame(
        name = "employee.csv",
        datapath = tempfile(fileext = ".csv")
      )
    )
    
    # Test file validation
    expect_true(is.reactive(validate_file_upload))
    
    # Test error handling for invalid files
    session$setInputs(
      employee_file = data.frame(
        name = "invalid.txt",
        datapath = tempfile(fileext = ".txt")
      )
    )
    
    expect_true(grepl("Invalid file", validation_message()))
  })
})

test_that("Overview Module - KPI Calculations", {
  
  # Create sample data for testing
  sample_data <- data.frame(
    EmployeeID = 1:100,
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.2, 0.8)),
    Salary = rnorm(100, 50000, 10000),
    JobSatisfaction = sample(1:5, 100, replace = TRUE),
    Department = sample(c("HR", "IT", "Finance"), 100, replace = TRUE)
  )
  
  testServer(overviewServer, args = list(data = reactive(sample_data)), {
    
    # Test KPI calculations
    kpis <- calculate_kpis()
    
    expect_type(kpis, "list")
    expect_true("total_employees" %in% names(kpis))
    expect_true("attrition_rate" %in% names(kpis))
    expect_equal(kpis$total_employees, 100)
    expect_gte(kpis$attrition_rate, 0)
    expect_lte(kpis$attrition_rate, 1)
    
    # Test department breakdown
    dept_summary <- department_breakdown()
    expect_s3_class(dept_summary, "data.frame")
    expect_true("Department" %in% names(dept_summary))
  })
})

test_that("Attrition Module - Analysis Functions", {
  
  sample_data <- data.frame(
    EmployeeID = 1:200,
    Attrition = sample(c("Yes", "No"), 200, replace = TRUE),
    Age = sample(22:65, 200, replace = TRUE),
    YearsAtCompany = sample(0:40, 200, replace = TRUE),
    Department = sample(c("HR", "IT", "Finance", "Sales"), 200, replace = TRUE),
    JobSatisfaction = sample(1:5, 200, replace = TRUE)
  )
  
  testServer(attritionServer, args = list(data = reactive(sample_data)), {
    
    # Test attrition analysis
    analysis <- analyze_attrition_factors()
    
    expect_type(analysis, "list")
    expect_true("by_department" %in% names(analysis))
    expect_true("by_age_group" %in% names(analysis))
    
    # Test correlation analysis
    correlation_matrix <- calculate_attrition_correlations()
    expect_s3_class(correlation_matrix, "matrix")
    expect_equal(nrow(correlation_matrix), ncol(correlation_matrix))
    
    # Test predictive model
    model <- build_attrition_model()
    expect_s3_class(model, c("glm", "lm"))
  })
})

test_that("Performance Module - Rating Analysis", {
  
  performance_data <- data.frame(
    EmployeeID = 1:150,
    ManagerRating = sample(1:5, 150, replace = TRUE),
    SelfRating = sample(1:5, 150, replace = TRUE),
    EnvironmentSatisfaction = sample(1:5, 150, replace = TRUE),
    TrainingOpportunitiesWithinYear = sample(0:10, 150, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:8, 150, replace = TRUE)
  )
  
  testServer(performanceServer, args = list(data = reactive(performance_data)), {
    
    # Test performance metrics calculation
    metrics <- calculate_performance_metrics()
    
    expect_type(metrics, "list")
    expect_true("avg_manager_rating" %in% names(metrics))
    expect_true("avg_self_rating" %in% names(metrics))
    expect_true("rating_gap" %in% names(metrics))
    
    # Test training analysis
    training_analysis <- analyze_training_effectiveness()
    expect_s3_class(training_analysis, "data.frame")
    
    # Test performance distribution
    distribution <- performance_distribution_analysis()
    expect_type(distribution, "list")
  })
})

# =============================================================================
# 3. VISUALIZATION TESTING
# =============================================================================

test_that("Custom ggplot2 Themes - Application and Consistency", {
  
  # Test theme function exists
  expect_true(exists("atlas_theme"))
  
  # Test theme application
  sample_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) +
    geom_point() +
    atlas_theme()
  
  expect_s3_class(sample_plot, "ggplot")
  
  # Test theme elements
  theme_elements <- sample_plot$theme
  expect_true("plot.title" %in% names(theme_elements))
  expect_true("axis.text" %in% names(theme_elements))
  
  # Test color palette consistency
  atlas_colors <- get_atlas_colors()
  expect_type(atlas_colors, "character")
  expect_gte(length(atlas_colors), 5)
  
  # Test color validation
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", atlas_colors)))
})

test_that("Plotly Integration - Interactivity and Performance", {
  
  # Create sample plotly visualization
  sample_data <- data.frame(
    x = 1:100,
    y = rnorm(100),
    category = sample(letters[1:5], 100, replace = TRUE)
  )
  
  # Test plotly creation
  p <- plot_ly(sample_data, x = ~x, y = ~y, color = ~category, type = "scatter")
  expect_s3_class(p, "plotly")
  
  # Test plotly customization
  customized_plot <- p %>% 
    layout(
      title = "Test Plot",
      xaxis = list(title = "X Axis"),
      yaxis = list(title = "Y Axis")
    )
  
  expect_s3_class(customized_plot, "plotly")
  
  # Test plotly performance with large datasets
  large_data <- data.frame(
    x = 1:10000,
    y = rnorm(10000)
  )
  
  start_time <- Sys.time()
  large_plot <- plot_ly(large_data, x = ~x, y = ~y, type = "scatter", mode = "markers")
  end_time <- Sys.time()
  
  expect_lt(as.numeric(end_time - start_time), 2) # Should complete within 2 seconds
})

test_that("ggrepel Label Positioning - Edge Cases", {
  
  # Test with overlapping points
  overlapping_data <- data.frame(
    x = c(1, 1.01, 1.02, 1.03, 1.04),
    y = c(1, 1.01, 1.02, 1.03, 1.04),
    label = paste("Point", 1:5)
  )
  
  p <- ggplot(overlapping_data, aes(x = x, y = y, label = label)) +
    geom_point() +
    geom_text_repel()
  
  expect_s3_class(p, "ggplot")
  
  # Test with extreme values
  extreme_data <- data.frame(
    x = c(-1000, 0, 1000),
    y = c(-1000, 0, 1000),
    label = c("Min", "Center", "Max")
  )
  
  expect_no_error({
    ggplot(extreme_data, aes(x = x, y = y, label = label)) +
      geom_point() +
      geom_text_repel()
  })
  
  # Test with many labels
  many_labels_data <- data.frame(
    x = runif(100),
    y = runif(100),
    label = paste("Label", 1:100)
  )
  
  expect_no_error({
    ggplot(many_labels_data, aes(x = x, y = y, label = label)) +
      geom_point() +
      geom_text_repel(max.overlaps = 50)
  })
})

# =============================================================================
# 4. REACTIVE PROGRAMMING TESTING
# =============================================================================

test_that("Reactive Value Management - Complex Dependencies", {
  
  testServer(function(input, output, session) {
    
    # Create complex reactive dependencies
    base_data <- reactive({
      data.frame(
        id = 1:100,
        value = rnorm(100),
        category = sample(letters[1:5], 100, replace = TRUE)
      )
    })
    
    filtered_data <- reactive({
      req(input$filter_category)
      base_data() %>% filter(category %in% input$filter_category)
    })
    
    summary_stats <- reactive({
      data <- filtered_data()
      list(
        count = nrow(data),
        mean_value = mean(data$value),
        categories = unique(data$category)
      )
    })
    
    # Test reactive chain
    session$setInputs(filter_category = c("a", "b"))
    
    expect_s3_class(base_data(), "data.frame")
    expect_lte(nrow(filtered_data()), nrow(base_data()))
    expect_type(summary_stats(), "list")
    expect_gte(summary_stats()$count, 0)
  })
})

test_that("Reactive Invalidation - Performance Optimization", {
  
  # Test reactive invalidation patterns
  invalidation_count <- 0
  
  testServer(function(input, output, session) {
    
    expensive_computation <- reactive({
      invalidation_count <<- invalidation_count + 1
      Sys.sleep(0.01) # Simulate expensive operation
      input$trigger_value * 2
    })
    
    cached_result <- reactive({
      expensive_computation()
    })
    
    # Test that computation only runs when necessary
    session$setInputs(trigger_value = 1)
    result1 <- cached_result()
    result2 <- cached_result() # Should use cached value
    
    expect_equal(result1, result2)
    expect_equal(invalidation_count, 1) # Should only run once
    
    # Test invalidation on input change
    session$setInputs(trigger_value = 2)
    result3 <- cached_result()
    
    expect_equal(invalidation_count, 2) # Should run again
    expect_equal(result3, 4)
  })
})

test_that("Cross-Module Communication - Bidirectional Flow", {
  
  # Create shared reactive values
  shared_values <- reactiveValues(
    selected_filters = list(),
    current_data = NULL,
    analysis_results = NULL
  )
  
  testServer(function(input, output, session) {
    
    # Module A updates shared values
    observe({
      req(input$filter_department)
      shared_values$selected_filters$department <- input$filter_department
    })
    
    # Module B reacts to shared values
    filtered_data <- reactive({
      base_data <- data.frame(
        id = 1:100,
        department = sample(c("HR", "IT", "Finance"), 100, replace = TRUE)
      )
      
      if (!is.null(shared_values$selected_filters$department)) {
        base_data <- base_data[base_data$department %in% 
                              shared_values$selected_filters$department, ]
      }
      
      shared_values$current_data <- base_data
      base_data
    })
    
    # Test bidirectional communication
    session$setInputs(filter_department = "HR")
    
    expect_true("department" %in% names(shared_values$selected_filters))
    expect_equal(shared_values$selected_filters$department, "HR")
    expect_true(all(filtered_data()$department == "HR"))
  })
})

# =============================================================================
# 5. UI/UX TESTING
# =============================================================================

test_that("Responsive Design - Multiple Screen Sizes", {
  
  # Test CSS responsiveness
  css_content <- read_lines("www/custom_styles.css")
  
  # Check for responsive breakpoints
  expect_true(any(grepl("@media", css_content)))
  expect_true(any(grepl("max-width", css_content)))
  expect_true(any(grepl("min-width", css_content)))
  
  # Test Bootstrap classes usage
  ui_elements <- fluidPage(
    fluidRow(
      column(12, class = "col-lg-6 col-md-12",
        h1("Responsive Header")
      )
    )
  )
  
  ui_html <- as.character(ui_elements)
  expect_true(grepl("col-lg-6", ui_html))
  expect_true(grepl("col-md-12", ui_html))
})

test_that("Accessibility Compliance - WCAG Guidelines", {
  
  # Test semantic HTML structure
  accessible_ui <- tagList(
    tags$nav(role = "navigation", aria_label = "Main navigation"),
    tags$main(role = "main",
      tags$h1("Dashboard Title"),
      tags$section(aria_labelledby = "charts-heading",
        tags$h2(id = "charts-heading", "Charts Section")
      )
    )
  )
  
  ui_html <- as.character(accessible_ui)
  
  # Test ARIA attributes
  expect_true(grepl('role="navigation"', ui_html))
  expect_true(grepl('aria-label="Main navigation"', ui_html))
  expect_true(grepl('role="main"', ui_html))
  
  # Test color contrast (mock test)
  atlas_colors <- get_atlas_colors()
  expect_true(check_color_contrast(atlas_colors[1], "#FFFFFF")) # Mock function
})

test_that("Interactive Elements - Event Handling", {
  
  # Test button interactions
  testServer(function(input, output, session) {
    
    click_count <- reactiveVal(0)
    
    observeEvent(input$test_button, {
      click_count(click_count() + 1)
    })
    
    output$click_display <- renderText({
      paste("Clicked", click_count(), "times")
    })
    
    # Simulate button clicks
    session$setInputs(test_button = 1)
    expect_equal(click_count(), 1)
    
    session$setInputs(test_button = 2)
    expect_equal(click_count(), 2)
    
    expect_match(output$click_display, "Clicked 2 times")
  })
})

test_that("Custom CSS and JavaScript Integration", {
  
  # Test CSS file loading
  expect_true(file.exists("www/custom_styles.css"))
  
  # Test JavaScript file loading
  expect_true(file.exists("www/scripts.js"))
  
  # Test CSS parsing
  css_content <- readLines("www/custom_styles.css")
  expect_true(any(grepl("atlas-theme", css_content)))
  expect_true(any(grepl("\\{.*\\}", css_content))) # CSS rules
  
  # Test JavaScript syntax (basic)
  js_content <- readLines("www/scripts.js")
  expect_true(any(grepl("function", js_content)))
})

# =============================================================================
# 6. PERFORMANCE TESTING
# =============================================================================

test_that("Memory Usage - Large Dataset Handling", {
  
  # Test memory efficiency with large datasets
  large_dataset <- data.frame(
    id = 1:100000,
    value1 = rnorm(100000),
    value2 = sample(letters, 100000, replace = TRUE),
    value3 = runif(100000)
  )
  
  # Monitor memory usage
  initial_memory <- pryr::mem_used()
  
  # Process large dataset
  processed_data <- large_dataset %>%
    filter(value1 > 0) %>%
    group_by(value2) %>%
    summarize(mean_value = mean(value1), .groups = "drop")
  
  final_memory <- pryr::mem_used()
  memory_used <- final_memory - initial_memory
  
  # Memory usage should be reasonable (less than 100MB for this operation)
  expect_lt(as.numeric(memory_used), 100 * 1024^2)
  
  # Clean up
  rm(large_dataset, processed_data)
  gc()
})

test_that("Rendering Performance - Complex Visualizations", {
  
  # Test plotly rendering performance
  complex_data <- data.frame(
    x = rep(1:100, 50),
    y = rep(1:50, each = 100),
    z = rnorm(5000),
    category = sample(LETTERS[1:10], 5000, replace = TRUE)
  )
  
  # Benchmark plotly creation
  benchmark_result <- bench::mark(
    simple_scatter = plot_ly(complex_data, x = ~x, y = ~y, type = "scatter"),
    complex_3d = plot_ly(complex_data, x = ~x, y = ~y, z = ~z, 
                        color = ~category, type = "scatter3d"),
    min_time = "100ms",
    max_iterations = 10
  )
  
  # Both should complete reasonably quickly
  expect_lt(median(benchmark_result$total_time[1]), 1) # Less than 1 second
})

test_that("Reactive Performance - Complex Dependencies", {
  
  # Test performance with many reactive dependencies
  testServer(function(input, output, session) {
    
    # Create multiple reactive chains
    reactive_chain <- list()
    
    for (i in 1:20) {
      reactive_chain[[i]] <- reactive({
        if (i == 1) {
          input$base_value %||% 1
        } else {
          reactive_chain[[i-1]]() * 1.01
        }
      })
    }
    
    # Measure update time
    start_time <- Sys.time()
    session$setInputs(base_value = 10)
    
    # Force evaluation of all reactives
    final_result <- reactive_chain[[20]]()
    end_time <- Sys.time()
    
    execution_time <- as.numeric(end_time - start_time)
    
    # Should complete within reasonable time
    expect_lt(execution_time, 0.5) # Less than 500ms
    expect_gt(final_result, 10) # Should have propagated through chain
  })
})

# =============================================================================
# 7. ERROR HANDLING AND EDGE CASES
# =============================================================================

test_that("Input Validation - Malformed Data", {
  
  # Test with missing required columns
  incomplete_data <- data.frame(
    EmployeeID = 1:10
    # Missing other required columns
  )
  
  expect_error(validate_employee_data(incomplete_data))
  
  # Test with wrong data types
  wrong_types_data <- data.frame(
    EmployeeID = as.character(1:10), # Should be numeric
    Salary = as.character(rnorm(10, 50000)), # Should be numeric
    Attrition = sample(0:1, 10, replace = TRUE) # Should be Yes/No
  )
  
  expect_warning(validate_employee_data(wrong_types_data))
  
  # Test with extreme values
  extreme_data <- data.frame(
    EmployeeID = 1:5,
    Age = c(-5, 200, 25, NA, 35), # Invalid ages
    Salary = c(-1000, 1e10, 50000, NA, 60000), # Invalid salaries
    Attrition = c("Yes", "No", "Maybe", "Yes", "No") # Invalid attrition value
  )
  
  validation_result <- validate_employee_data(extreme_data)
  expect_true(any(validation_result$warnings))
})

test_that("Network and File System Errors", {
  
  # Test file reading errors
  expect_error(read_employee_data("nonexistent_file.csv"))
  
  # Test with corrupted file (empty file)
  temp_file <- tempfile(fileext = ".csv")
  writeLines("", temp_file)
  expect_error(read_employee_data(temp_file))
  
  # Test with permission issues (mock)
  # This would need to be platform-specific
  
  # Clean up
  unlink(temp_file)
})

test_that("Concurrent User Scenarios", {
  
  # Simulate multiple users accessing the app
  if (parallel::detectCores() > 1) {
    
    # Create multiple test sessions
    test_results <- parallel::mclapply(1:5, function(user_id) {
      
      testServer(function(input, output, session) {
        
        # Each user loads different data subset
        user_data <- reactive({
          data.frame(
            id = 1:100,
            user_id = user_id,
            value = rnorm(100)
          )
        })
        
        # Simulate user interactions
        session$setInputs(filter_value = user_id * 10)
        
        # Return results
        list(
          user_id = user_id,
          data_count = nrow(user_data()),
          filter_applied = input$filter_value
        )
      })
      
    }, mc.cores = 2)
    
    # Verify all users got correct results
    expect_length(test_results, 5)
    expect_true(all(sapply(test_results, function(x) x$data_count == 100)))
  }
})

# =============================================================================
# 8. SECURITY TESTING
# =============================================================================

test_that("Input Sanitization - XSS Prevention", {
  
  # Test malicious input handling
  malicious_inputs <- c(
    "<script>alert('XSS')</script>",
    "'; DROP TABLE users; --",
    "<img src='x' onerror='alert(1)'>",
    "javascript:alert('XSS')"
  )
  
  for (input in malicious_inputs) {
    sanitized <- sanitize_user_input(input)
    
    # Should not contain script tags or javascript
    expect_false(grepl("<script", sanitized, ignore.case = TRUE))
    expect_false(grepl("javascript:", sanitized, ignore.case = TRUE))
    expect_false(grepl("onerror", sanitized, ignore.case = TRUE))
  }
})

test_that("File Upload Security - Malicious Files", {
  
  # Test file extension validation
  malicious_files <- c(
    "malware.exe",
    "script.js",
    "payload.php",
    "virus.bat"
  )
  
  for (filename in malicious_files) {
    expect_false(is_valid_file_extension(filename))
  }
  
  # Test valid file extensions
  valid_files <- c(
    "data.csv",
    "employees.xlsx",
    "report.txt"
  )
  
  for (filename in valid_files) {
    expect_true(is_valid_file_extension(filename))
  }
})

test_that("Session Management - Timeout and Cleanup", {
  
  testServer(function(input, output, session) {
    
    # Test session timeout handling
    session_timeout <- 30 * 60 # 30 minutes
    
    # Simulate session activity
    last_activity <- Sys.time()
    
    # Mock session cleanup
    cleanup_triggered <- FALSE
    
    # This would normally be handled by Shiny's built-in session management
    if (difftime(Sys.time(), last_activity, units = "secs") > session_timeout) {
      cleanup_triggered <- TRUE
    }
    
    # For testing, we'll just verify the logic
    expect_false(cleanup_triggered) # Should not timeout immediately
  })
})

# =============================================================================
# 9. INTEGRATION TESTING
# =============================================================================

test_that("End-to-End Workflow - Complete Analysis Pipeline", {
  
  # Create comprehensive test data
  employee_data <- data.frame(
    EmployeeID = 1:200,
    FirstName = paste("Employee", 1:200),
    Age = sample(22:65, 200, replace = TRUE),
    Department = sample(c("HR", "IT", "Finance", "Sales"), 200, replace = TRUE),
    Salary = rnorm(200, 50000, 15000),
    Attrition = sample(c("Yes", "No"), 200, replace = TRUE, prob = c(0.15, 0.85))
  )
  
  performance_data <- data.frame(
    