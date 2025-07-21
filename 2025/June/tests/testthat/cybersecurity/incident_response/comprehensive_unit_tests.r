# =====================================================
# Atlas Labs HR Analytics Dashboard - Comprehensive Unit Tests
# Developer: akhapwoyaco
# Coverage: All areas except cybersecurity incident response
# =====================================================

library(testthat)
library(shiny)
library(shinytest2)
library(mockery)
library(DT)
library(plotly)
library(R6)
library(tidyverse)
library(lubridate)

# =====================================================
# 1. DATA LOADER MODULE TESTS
# =====================================================

test_that("Data Loader - File Validation", {
  
  # Test valid CSV file detection
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(x = 1:3, y = letters[1:3]), temp_csv, row.names = FALSE)
  
  expect_true(file.exists(temp_csv))
  expect_match(temp_csv, "\\.csv$")
  
  # Test invalid file extension handling
  temp_txt <- tempfile(fileext = ".txt")
  writeLines("invalid,data", temp_txt)
  
  expect_false(grepl("\\.csv$", temp_txt))
  
  # Test empty file handling
  temp_empty <- tempfile(fileext = ".csv")
  file.create(temp_empty)
  
  expect_equal(file.size(temp_empty), 0)
  
  # Cleanup
  unlink(c(temp_csv, temp_txt, temp_empty))
})

test_that("Data Loader - Data Validation Edge Cases", {
  
  # Test malformed CSV data
  malformed_data <- "Name,Age,Salary\nJohn,30,\nJane,,50000\n,,\n"
  temp_file <- tempfile(fileext = ".csv")
  writeLines(malformed_data, temp_file)
  
  # Should handle missing values gracefully
  df <- tryCatch({
    read.csv(temp_file, stringsAsFactors = FALSE)
  }, error = function(e) NULL)
  
  expect_false(is.null(df))
  expect_true(any(is.na(df)))
  
  # Test duplicate headers
  duplicate_header_data <- "Name,Name,Age\nJohn,John,30\n"
  writeLines(duplicate_header_data, temp_file)
  
  df_dup <- read.csv(temp_file, stringsAsFactors = FALSE)
  expect_true(ncol(df_dup) >= 2)
  
  # Test special characters in data
  special_char_data <- "Name,Description\n\"O'Connor\",\"Data with \"quotes\" and commas, here\"\n"
  writeLines(special_char_data, temp_file)
  
  df_special <- read.csv(temp_file, stringsAsFactors = FALSE)
  expect_equal(nrow(df_special), 1)
  
  unlink(temp_file)
})

test_that("Data Loader - Data Type Conversion", {
  
  # Test date conversion edge cases
  test_dates <- c("2023-01-01", "31/12/2023", "Dec 31, 2023", "invalid_date", NA)
  
  converted_dates <- sapply(test_dates, function(x) {
    tryCatch({
      if (is.na(x)) return(NA)
      as.Date(x, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%b %d, %Y"))
    }, error = function(e) NA)
  })
  
  expect_true(is.na(converted_dates[4])) # Invalid date
  expect_true(is.na(converted_dates[5])) # NA value
  expect_false(is.na(converted_dates[1])) # Valid ISO date
  
  # Test numeric conversion with various formats
  test_numbers <- c("123", "123.45", "$1,234.56", "1.23e5", "invalid", NA)
  
  converted_numbers <- sapply(test_numbers, function(x) {
    if (is.na(x)) return(NA)
    # Remove currency symbols and commas
    clean_x <- gsub("[\\$,]", "", x)
    as.numeric(clean_x)
  })
  
  expect_true(is.na(converted_numbers[5])) # Invalid number
  expect_equal(converted_numbers[1], 123)
  expect_equal(converted_numbers[3], 1234.56)
})

# =====================================================
# 2. LOGGER MODULE TESTS (R6 CLASS)
# =====================================================

test_that("AtlasLogger R6 Class - Initialization", {
  
  # Mock AtlasLogger class for testing
  AtlasLogger <- R6Class("AtlasLogger",
    private = list(
      .logs = list(),
      .performance_data = list(),
      .memory_usage = list()
    ),
    public = list(
      initialize = function() {
        private$.logs <- list()
        private$.performance_data <- list()
        private$.memory_usage <- list()
      },
      
      log_info = function(message, module = "unknown", performance_data = NULL) {
        timestamp <- Sys.time()
        log_entry <- list(
          level = "INFO",
          message = message,
          module = module,
          timestamp = timestamp,
          performance = performance_data
        )
        private$.logs <- append(private$.logs, list(log_entry))
        return(invisible(self))
      },
      
      get_logs = function() {
        return(private$.logs)
      },
      
      clear_logs = function() {
        private$.logs <- list()
        return(invisible(self))
      },
      
      track_memory = function(module) {
        mem_usage <- as.numeric(object.size(ls(envir = .GlobalEnv)))
        private$.memory_usage[[module]] <- mem_usage
        return(mem_usage)
      }
    )
  )
  
  logger <- AtlasLogger$new()
  
  expect_true(R6::is.R6(logger))
  expect_equal(length(logger$get_logs()), 0)
  
  # Test logging functionality
  logger$log_info("Test message", "test_module")
  logs <- logger$get_logs()
  
  expect_equal(length(logs), 1)
  expect_equal(logs[[1]]$level, "INFO")
  expect_equal(logs[[1]]$message, "Test message")
  expect_equal(logs[[1]]$module, "test_module")
})

test_that("AtlasLogger - Performance Tracking Edge Cases", {
  
  AtlasLogger <- R6Class("AtlasLogger",
    private = list(.performance_data = list()),
    public = list(
      initialize = function() {
        private$.performance_data <- list()
      },
      
      track_execution_time = function(func, module = "unknown") {
        start_time <- Sys.time()
        
        result <- tryCatch({
          func()
        }, error = function(e) {
          list(error = e$message, result = NULL)
        })
        
        end_time <- Sys.time()
        execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
        
        private$.performance_data[[module]] <- list(
          execution_time = execution_time,
          start_time = start_time,
          end_time = end_time,
          status = if (is.list(result) && !is.null(result$error)) "error" else "success"
        )
        
        return(result)
      },
      
      get_performance_data = function() {
        return(private$.performance_data)
      }
    )
  )
  
  logger <- AtlasLogger$new()
  
  # Test successful execution tracking
  result1 <- logger$track_execution_time(function() {
    Sys.sleep(0.01) # Small delay for testing
    return("success")
  }, "fast_function")
  
  perf_data <- logger$get_performance_data()
  expect_true("fast_function" %in% names(perf_data))
  expect_true(perf_data$fast_function$execution_time > 0)
  expect_equal(perf_data$fast_function$status, "success")
  
  # Test error handling in execution tracking
  result2 <- logger$track_execution_time(function() {
    stop("Intentional error")
  }, "error_function")
  
  perf_data <- logger$get_performance_data()
  expect_equal(perf_data$error_function$status, "error")
})

# =====================================================
# 3. UI MODULE TESTS
# =====================================================

test_that("UI Module - Input Validation", {
  
  # Mock UI module functions
  validate_date_input <- function(date_str) {
    if (is.null(date_str) || is.na(date_str) || date_str == "") {
      return(list(valid = FALSE, message = "Date cannot be empty"))
    }
    
    parsed_date <- tryCatch({
      as.Date(date_str)
    }, error = function(e) NA)
    
    if (is.na(parsed_date)) {
      return(list(valid = FALSE, message = "Invalid date format"))
    }
    
    # Check if date is in reasonable range
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    date_year <- as.numeric(format(parsed_date, "%Y"))
    
    if (date_year < 1900 || date_year > current_year + 1) {
      return(list(valid = FALSE, message = "Date out of reasonable range"))
    }
    
    return(list(valid = TRUE, date = parsed_date))
  }
  
  # Test valid dates
  valid_result <- validate_date_input("2023-01-01")
  expect_true(valid_result$valid)
  expect_equal(class(valid_result$date), "Date")
  
  # Test invalid dates
  invalid_results <- list(
    validate_date_input(""),
    validate_date_input(NA),
    validate_date_input("invalid"),
    validate_date_input("1800-01-01"), # Too old
    validate_date_input("2030-01-01")  # Too future
  )
  
  sapply(invalid_results, function(result) {
    expect_false(result$valid)
    expect_true(nchar(result$message) > 0)
  })
})

test_that("UI Module - Filter Validation", {
  
  # Mock filter validation function
  validate_filter_inputs <- function(filters) {
    errors <- list()
    
    # Validate department filter
    if (!is.null(filters$departments)) {
      valid_departments <- c("HR", "IT", "Finance", "Sales", "Marketing")
      invalid_depts <- setdiff(filters$departments, valid_departments)
      if (length(invalid_depts) > 0) {
        errors$departments <- paste("Invalid departments:", paste(invalid_depts, collapse = ", "))
      }
    }
    
    # Validate salary range
    if (!is.null(filters$salary_range)) {
      if (length(filters$salary_range) != 2) {
        errors$salary_range <- "Salary range must have exactly 2 values"
      } else if (filters$salary_range[1] > filters$salary_range[2]) {
        errors$salary_range <- "Minimum salary cannot be greater than maximum"
      } else if (any(filters$salary_range < 0)) {
        errors$salary_range <- "Salary values cannot be negative"
      }
    }
    
    # Validate age range
    if (!is.null(filters$age_range)) {
      if (length(filters$age_range) != 2) {
        errors$age_range <- "Age range must have exactly 2 values"
      } else if (any(filters$age_range < 18 | filters$age_range > 100)) {
        errors$age_range <- "Age must be between 18 and 100"
      }
    }
    
    return(list(
      valid = length(errors) == 0,
      errors = errors
    ))
  }
  
  # Test valid filters
  valid_filters <- list(
    departments = c("HR", "IT"),
    salary_range = c(30000, 80000),
    age_range = c(25, 65)
  )
  
  valid_result <- validate_filter_inputs(valid_filters)
  expect_true(valid_result$valid)
  expect_equal(length(valid_result$errors), 0)
  
  # Test invalid filters
  invalid_filters <- list(
    departments = c("InvalidDept", "HR"),
    salary_range = c(80000, 30000), # Min > Max
    age_range = c(15, 105) # Out of range
  )
  
  invalid_result <- validate_filter_inputs(invalid_filters)
  expect_false(invalid_result$valid)
  expect_true(length(invalid_result$errors) > 0)
})

# =====================================================
# 4. DATA PROCESSING TESTS
# =====================================================

test_that("Data Processing - Aggregation Functions", {
  
  # Create sample data
  sample_data <- data.frame(
    employee_id = 1:10,
    department = c("HR", "IT", "HR", "Finance", "IT", "HR", "Finance", "IT", "HR", "Finance"),
    salary = c(50000, 75000, 55000, 60000, 80000, 52000, 65000, 78000, 54000, 62000),
    age = c(25, 30, 28, 35, 32, 27, 40, 29, 26, 38),
    attrition = c("No", "Yes", "No", "No", "Yes", "No", "No", "Yes", "No", "No"),
    stringsAsFactors = FALSE
  )
  
  # Test department aggregation
  dept_summary <- sample_data %>%
    group_by(department) %>%
    summarise(
      count = n(),
      avg_salary = mean(salary, na.rm = TRUE),
      avg_age = mean(age, na.rm = TRUE),
      attrition_rate = mean(attrition == "Yes", na.rm = TRUE),
      .groups = 'drop'
    )
  
  expect_equal(nrow(dept_summary), 3) # HR, IT, Finance
  expect_true(all(dept_summary$count > 0))
  expect_true(all(dept_summary$avg_salary > 0))
  expect_true(all(dept_summary$attrition_rate >= 0 & dept_summary$attrition_rate <= 1))
  
  # Test edge case: empty data
  empty_summary <- data.frame() %>%
    group_by() %>%
    summarise(
      count = n(),
      avg_salary = mean(numeric(0), na.rm = TRUE),
      .groups = 'drop'
    )
  
  expect_equal(nrow(empty_summary), 1)
  expect_equal(empty_summary$count, 0)
  expect_true(is.nan(empty_summary$avg_salary) || is.na(empty_summary$avg_salary))
})

test_that("Data Processing - Missing Value Handling", {
  
  # Create data with missing values
  data_with_na <- data.frame(
    id = 1:5,
    salary = c(50000, NA, 60000, 55000, NA),
    age = c(25, 30, NA, 35, 28),
    department = c("HR", NA, "IT", "Finance", "HR"),
    stringsAsFactors = FALSE
  )
  
  # Test complete cases
  complete_data <- data_with_na[complete.cases(data_with_na), ]
  expect_equal(nrow(complete_data), 2) # Only rows 1 and 4 are complete
  
  # Test column-wise missing value summary
  na_summary <- data_with_na %>%
    summarise(
      across(everything(), ~sum(is.na(.x))),
      .groups = 'drop'
    )
  
  expect_equal(na_summary$salary, 2)
  expect_equal(na_summary$age, 1)
  expect_equal(na_summary$department, 1)
  expect_equal(na_summary$id, 0)
  
  # Test imputation strategies
  # Mean imputation for numeric columns
  salary_mean <- mean(data_with_na$salary, na.rm = TRUE)
  age_mean <- mean(data_with_na$age, na.rm = TRUE)
  
  imputed_data <- data_with_na %>%
    mutate(
      salary = ifelse(is.na(salary), salary_mean, salary),
      age = ifelse(is.na(age), age_mean, age)
    )
  
  expect_equal(sum(is.na(imputed_data$salary)), 0)
  expect_equal(sum(is.na(imputed_data$age)), 0)
  expect_equal(imputed_data$salary[2], salary_mean)
})

# =====================================================
# 5. VISUALIZATION TESTS
# =====================================================

test_that("Visualization - Plot Generation", {
  
  # Sample data for plotting
  plot_data <- data.frame(
    department = c("HR", "IT", "Finance", "Sales"),
    count = c(25, 40, 30, 35),
    avg_salary = c(55000, 75000, 65000, 60000)
  )
  
  # Test ggplot creation
  p1 <- ggplot(plot_data, aes(x = department, y = count)) +
    geom_col() +
    theme_minimal()
  
  expect_s3_class(p1, "ggplot")
  expect_equal(length(p1$layers), 1)
  
  # Test plotly conversion
  p1_plotly <- plotly::ggplotly(p1)
  expect_s3_class(p1_plotly, "plotly")
  
  # Test empty data handling
  empty_plot_data <- data.frame(
    department = character(0),
    count = numeric(0)
  )
  
  p_empty <- ggplot(empty_plot_data, aes(x = department, y = count)) +
    geom_col() +
    theme_minimal()
  
  expect_s3_class(p_empty, "ggplot")
  
  # Test data with special characters
  special_data <- data.frame(
    department = c("R&D", "Sales & Marketing", "IT/Tech"),
    count = c(10, 20, 15)
  )
  
  p_special <- ggplot(special_data, aes(x = department, y = count)) +
    geom_col() +
    theme_minimal()
  
  expect_s3_class(p_special, "ggplot")
})

test_that("Visualization - Color and Theme Consistency", {
  
  # Test custom color palette
  atlas_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")
  
  expect_true(length(atlas_colors) >= 3)
  expect_true(all(grepl("^#[0-9a-fA-F]{6}$", atlas_colors))) # Valid hex colors
  
  # Test theme application
  sample_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) +
    geom_point(color = atlas_colors[1]) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial"),
      plot.title = element_text(size = 14, face = "bold")
    )
  
  expect_s3_class(sample_plot, "ggplot")
  
  # Test accessibility - color contrast
  # Simple test for dark vs light colors
  hex_to_rgb <- function(hex) {
    hex <- gsub("#", "", hex)
    list(
      r = strtoi(substr(hex, 1, 2), 16L),
      g = strtoi(substr(hex, 3, 4), 16L),
      b = strtoi(substr(hex, 5, 6), 16L)
    )
  }
  
  # Calculate luminance for first color
  rgb <- hex_to_rgb(atlas_colors[1])
  luminance <- (0.299 * rgb$r + 0.587 * rgb$g + 0.114 * rgb$b) / 255
  
  expect_true(luminance >= 0 && luminance <= 1)
})

# =====================================================
# 6. PERFORMANCE TESTS
# =====================================================

test_that("Performance - Memory Usage", {
  
  # Test memory usage with large dataset
  large_data <- data.frame(
    id = 1:10000,
    value = rnorm(10000),
    category = sample(letters[1:5], 10000, replace = TRUE)
  )
  
  # Memory before operation
  mem_before <- as.numeric(object.size(large_data))
  
  # Perform aggregation
  aggregated <- large_data %>%
    group_by(category) %>%
    summarise(
      mean_value = mean(value),
      count = n(),
      .groups = 'drop'
    )
  
  # Memory after operation
  mem_after <- as.numeric(object.size(aggregated))
  
  expect_true(mem_after < mem_before) # Aggregated data should be smaller
  expect_equal(nrow(aggregated), 5) # Should have 5 categories
  
  # Clean up
  rm(large_data, aggregated)
  gc()
})

test_that("Performance - Execution Time", {
  
  # Test function execution time
  measure_execution_time <- function(func) {
    start_time <- Sys.time()
    result <- func()
    end_time <- Sys.time()
    
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    list(
      result = result,
      execution_time = execution_time
    )
  }
  
  # Test fast operation
  fast_op <- measure_execution_time(function() {
    sum(1:1000)
  })
  
  expect_true(fast_op$execution_time >= 0)
  expect_equal(fast_op$result, sum(1:1000))
  
  # Test slower operation
  slow_op <- measure_execution_time(function() {
    Sys.sleep(0.01) # 10ms delay
    return("completed")
  })
  
  expect_true(slow_op$execution_time >= 0.01)
  expect_equal(slow_op$result, "completed")
})

# =====================================================
# 7. ERROR HANDLING TESTS
# =====================================================

test_that("Error Handling - Graceful Failures", {
  
  # Test division by zero
  safe_divide <- function(a, b) {
    tryCatch({
      if (b == 0) {
        warning("Division by zero attempted")
        return(Inf)
      }
      return(a / b)
    }, error = function(e) {
      return(NA)
    })
  }
  
  expect_equal(safe_divide(10, 2), 5)
  expect_equal(safe_divide(10, 0), Inf)
  expect_warning(safe_divide(10, 0))
  
  # Test invalid data type handling
  safe_mean <- function(x) {
    tryCatch({
      if (!is.numeric(x)) {
        stop("Input must be numeric")
      }
      return(mean(x, na.rm = TRUE))
    }, error = function(e) {
      return(list(error = e$message, result = NA))
    })
  }
  
  numeric_result <- safe_mean(c(1, 2, 3, 4, 5))
  expect_equal(numeric_result, 3)
  
  error_result <- safe_mean(c("a", "b", "c"))
  expect_true(is.list(error_result))
  expect_true("error" %in% names(error_result))
})

test_that("Error Handling - Input Validation", {
  
  # Comprehensive input validation function
  validate_inputs <- function(data, required_columns) {
    errors <- list()
    
    # Check if data is provided
    if (is.null(data) || nrow(data) == 0) {
      errors$data <- "Data cannot be null or empty"
    }
    
    # Check required columns
    if (!is.null(data)) {
      missing_columns <- setdiff(required_columns, names(data))
      if (length(missing_columns) > 0) {
        errors$columns <- paste("Missing required columns:", paste(missing_columns, collapse = ", "))
      }
    }
    
    # Check data types
    if (!is.null(data) && "salary" %in% names(data)) {
      if (!is.numeric(data$salary)) {
        errors$salary_type <- "Salary column must be numeric"
      } else if (any(data$salary < 0, na.rm = TRUE)) {
        errors$salary_values <- "Salary values cannot be negative"
      }
    }
    
    return(list(
      valid = length(errors) == 0,
      errors = errors
    ))
  }
  
  # Test valid input
  valid_data <- data.frame(
    employee_id = 1:3,
    name = c("John", "Jane", "Bob"),
    salary = c(50000, 60000, 55000)
  )
  
  valid_result <- validate_inputs(valid_data, c("employee_id", "name", "salary"))
  expect_true(valid_result$valid)
  
  # Test missing columns
  invalid_data <- data.frame(
    employee_id = 1:3,
    name = c("John", "Jane", "Bob")
    # Missing salary column
  )
  
  invalid_result <- validate_inputs(invalid_data, c("employee_id", "name", "salary"))
  expect_false(invalid_result$valid)
  expect_true("columns" %in% names(invalid_result$errors))
  
  # Test null data
  null_result <- validate_inputs(NULL, c("employee_id"))
  expect_false(null_result$valid)
  expect_true("data" %in% names(null_result$errors))
})

# =====================================================
# 8. INTEGRATION TESTS
# =====================================================

test_that("Integration - Module Communication", {
  
  # Mock reactive values for testing inter-module communication
  mock_shared_values <- list(
    employee_data = data.frame(
      employee_id = 1:5,
      department = c("HR", "IT", "Finance", "HR", "IT"),
      salary = c(50000, 75000, 60000, 52000, 78000)
    ),
    selected_filters = list(
      departments = c("HR", "IT"),
      salary_range = c(50000, 80000)
    )
  )
  
  # Function to apply filters (simulating module communication)
  apply_filters <- function(data, filters) {
    filtered_data <- data
    
    if (!is.null(filters$departments)) {
      filtered_data <- filtered_data[filtered_data$department %in% filters$departments, ]
    }
    
    if (!is.null(filters$salary_range)) {
      filtered_data <- filtered_data[
        filtered_data$salary >= filters$salary_range[1] & 
        filtered_data$salary <= filters$salary_range[2], 
      ]
    }
    
    return(filtered_data)
  }
  
  filtered_result <- apply_filters(
    mock_shared_values$employee_data, 
    mock_shared_values$selected_filters
  )
  
  expect_true(nrow(filtered_result) > 0)
  expect_true(all(filtered_result$department %in% c("HR", "IT")))
  expect_true(all(filtered_result$salary >= 50000 & filtered_result$salary <= 80000))
})

test_that("Integration - Report Generation", {
  
  # Mock report data preparation
  prepare_report_data <- function(employee_data) {
    if (is.null(employee_data) || nrow(employee_data) == 0) {
      return(list(
        success = FALSE,
        error = "No data available for report generation"
      ))
    }
    
    # Calculate summary statistics
    summary_stats <- list(
      total_employees = nrow(employee_data),
      avg_salary = mean(employee_data$salary, na.rm = TRUE),
      departments = unique(employee_data$department),
      salary_range = range(employee_data$salary, na.rm = TRUE)
    )
    
    return(list(
      success = TRUE,
      data = summary_stats
    ))
  }
  
  # Test with valid data
  test_data <- data.frame(
    employee_id = 1:10,
    department = rep(c("HR", "IT"), 5),
    salary = seq(50000, 95000, 5000)
  )
  
  report_result <- prepare_report_data(test_data)
  expect_true(report_result$success)
  expect_equal(report_result$data$total_employees, 10)
  expect_true(length(report_result$data$departments) > 0)
  
  # Test with empty data
  empty_result <- prepare_report_data(data.frame())
  expect_false(empty_result$success)
  expect_true(nchar(empty_result$error) > 0)
})

# =====================================================
# 9. ACCESSIBILITY TESTS
# =====================================================

test_that("Accessibility - UI Elements", {
  
  # Test ARIA label generation
  generate_aria_label <- function(chart_type, data_summary) {
    base_label <- paste("Interactive", chart_type, "chart")
    
    if (!is.null(data_summary)) {
      if ("total_points" %in% names(data_summary)) {
        base_label <- paste(base_label, "with", data_summary$total_points, "data points")
      }
      
      if ("categories" %in% names(data_summary)) {
        base_label <- paste(base_label, "showing", length(data_summary$categories), "categories")
      }
    }
    
    return(base_label)
  }
  
  test_summary <- list(
    total_points = 100,
    categories = c("A", "B", "C")
  )
  
  aria_label <- generate_aria_label("bar", test_summary)
  expect_true(grepl("Interactive bar chart", aria_label))
  expect_true(grepl("100 data points", aria_label))
  expect_true(grepl("3 categories", aria_label))
  
  # Test color contrast validation
  validate_color_contrast <- function(foreground, background) {
    # Simple contrast ratio calculation (simplified)
    # In real implementation, would use proper WCAG contrast calculation
    
    hex_to_luminance <- function(hex_color) {
      # Remove # if present
      hex_color <- gsub("#", "", hex_color)