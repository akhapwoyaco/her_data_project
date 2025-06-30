# =============================================================================
# ATLAS LABS HR ANALYTICS - REPORT MODULE UNIT TESTS
# Comprehensive test suite focusing on:
# 1. Data serialization safety
# 2. Cross-reference validation  
# 3. Formatting consistency
# 4. Multi-language support
# =============================================================================

library(testthat)
library(shiny)
library(rmarkdown)
library(jsonlite)
library(lubridate)
library(stringr)
library(purrr)

# Mock Report Module Functions (for testing purposes)
source("modules/report_module.R", local = TRUE)

# =============================================================================
# TEST SUITE 1: DATA SERIALIZATION SAFETY
# =============================================================================

context("Report Module - Data Serialization Safety")

test_that("serialize_report_data handles NULL values safely", {
  # Test NULL input
  result <- serialize_report_data(NULL)
  expect_equal(result, list(status = "empty", data = NULL))
  
  # Test empty list
  result <- serialize_report_data(list())
  expect_equal(result$status, "empty")
  
  # Test list with NULL values
  test_data <- list(
    employees = NULL,
    performance = data.frame(id = 1:3, rating = c(4, 5, NA)),
    metrics = NULL
  )
  
  result <- serialize_report_data(test_data)
  expect_true("serialization_safe" %in% names(result))
  expect_true(result$serialization_safe)
})

test_that("serialize_report_data handles special characters and encoding", {
  # Test data with special characters
  special_data <- list(
    employee_names = c("José García", "François Müller", "北京张", "أحمد محمد"),
    departments = c("R&D", "Sales & Marketing", "IT/Operations"),
    comments = c("Performance review: 'Excellent'", 'Comments with "quotes"', "Multi\nline\ntext")
  )
  
  result <- serialize_report_data(special_data)
  
  # Verify UTF-8 encoding preservation
  expect_true(all(Encoding(unlist(result$data$employee_names)) %in% c("UTF-8", "unknown")))
  
  # Verify special characters are preserved
  expect_true(any(grepl("José", result$data$employee_names)))
  expect_true(any(grepl("北京", result$data$employee_names)))
  expect_true(any(grepl("أحمد", result$data$employee_names)))
})

test_that("serialize_report_data handles large datasets efficiently", {
  # Create large test dataset
  large_data <- list(
    employees = data.frame(
      id = 1:10000,
      name = paste("Employee", 1:10000),
      salary = runif(10000, 30000, 150000),
      performance = sample(1:5, 10000, replace = TRUE)
    ),
    performance_history = expand.grid(
      employee_id = 1:1000, 
      year = 2020:2024,
      rating = sample(1:5, 5000, replace = TRUE)
    )
  )
  
  # Measure serialization time
  start_time <- Sys.time()
  result <- serialize_report_data(large_data)
  end_time <- Sys.time()
  
  serialization_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Verify performance (should complete within 5 seconds)
  expect_lt(serialization_time, 5)
  expect_true(result$serialization_safe)
  expect_true("data_size_mb" %in% names(result$metadata))
})

test_that("serialize_report_data validates data types correctly", {
  # Test mixed data types
  mixed_data <- list(
    numeric_col = c(1, 2, 3.5, Inf, -Inf),
    date_col = as.Date(c("2024-01-01", "2024-12-31")),
    datetime_col = as.POSIXct(c("2024-01-01 10:30:00", "2024-12-31 23:59:59")),
    logical_col = c(TRUE, FALSE, NA),
    factor_col = factor(c("Low", "Medium", "High")),
    character_col = c("Valid", NA_character_, "")
  )
  
  result <- serialize_report_data(mixed_data)
  
  # Verify type preservation
  expect_equal(result$metadata$data_types$numeric_col, "numeric")
  expect_equal(result$metadata$data_types$date_col, "Date")
  expect_equal(result$metadata$data_types$datetime_col, "POSIXct")
  expect_equal(result$metadata$data_types$logical_col, "logical")
  expect_equal(result$metadata$data_types$factor_col, "factor")
  expect_equal(result$metadata$data_types$character_col, "character")
})

test_that("serialize_report_data handles circular references", {
  # Create circular reference
  circular_data <- list(a = 1, b = 2)
  circular_data$self_ref <- circular_data
  
  # Should handle gracefully without infinite recursion
  expect_warning(
    result <- serialize_report_data(circular_data),
    "circular|recursive"
  )
  
  expect_false(result$serialization_safe)
  expect_true("error_message" %in% names(result))
})

# =============================================================================
# TEST SUITE 2: CROSS-REFERENCE VALIDATION
# =============================================================================

context("Report Module - Cross-Reference Validation")

test_that("validate_cross_references detects missing employee IDs", {
  # Setup test data with missing references
  employee_data <- data.frame(
    employee_id = c(1, 2, 3, 5),  # Missing ID 4
    name = c("John", "Jane", "Bob", "Alice"),
    department = c("IT", "HR", "Sales", "IT")
  )
  
  performance_data <- data.frame(
    employee_id = c(1, 2, 3, 4, 6),  # References missing employees 4 and 6
    rating = c(4, 5, 3, 4, 5),
    review_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04", "2024-01-05"))
  )
  
  validation_result <- validate_cross_references(
    list(employees = employee_data, performance = performance_data)
  )
  
  expect_false(validation_result$is_valid)
  expect_true(4 %in% validation_result$missing_employee_refs$performance)
  expect_true(6 %in% validation_result$missing_employee_refs$performance)
  expect_equal(length(validation_result$orphaned_records), 2)
})

test_that("validate_cross_references handles multiple table relationships", {
  # Setup complex relational data
  employees <- data.frame(
    employee_id = 1:5,
    manager_id = c(NA, 1, 1, 2, 2),  # Self-referencing
    department_id = c(1, 1, 2, 2, 3)
  )
  
  departments <- data.frame(
    department_id = c(1, 2),  # Missing department 3
    department_name = c("IT", "HR")
  )
  
  performance <- data.frame(
    employee_id = c(1, 2, 3, 7),  # Employee 7 doesn't exist
    manager_id = c(1, 1, 2, 8),   # Manager 8 doesn't exist
    rating = c(4, 5, 3, 4)
  )
  
  test_data <- list(
    employees = employees,
    departments = departments, 
    performance = performance
  )
  
  validation_result <- validate_cross_references(test_data)
  
  expect_false(validation_result$is_valid)
  
  # Check department reference validation
  expect_true(3 %in% validation_result$missing_department_refs$employees)
  
  # Check employee reference validation
  expect_true(7 %in% validation_result$missing_employee_refs$performance)
  
  # Check manager reference validation  
  expect_true(8 %in% validation_result$missing_manager_refs$performance)
})

test_that("validate_cross_references handles date range consistency", {
  # Test temporal consistency
  employees <- data.frame(
    employee_id = 1:3,
    hire_date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
    termination_date = as.Date(c(NA, "2023-12-31", NA))
  )
  
  performance <- data.frame(
    employee_id = c(1, 2, 2, 3),
    review_date = as.Date(c("2024-01-01", "2021-06-01", "2024-06-01", "2021-01-01")),
    rating = c(4, 5, 3, 4)
  )
  
  validation_result <- validate_cross_references(
    list(employees = employees, performance = performance),
    check_date_consistency = TRUE
  )
  
  expect_false(validation_result$is_valid)
  
  # Performance review after termination
  expect_true(any(validation_result$date_inconsistencies$type == "review_after_termination"))
  
  # Performance review before hire date
  expect_true(any(validation_result$date_inconsistencies$type == "review_before_hire"))
})

test_that("validate_cross_references validates data completeness", {
  # Test data with missing critical fields
  incomplete_data <- list(
    employees = data.frame(
      employee_id = 1:3,
      name = c("John", NA, "Bob"),  # Missing name
      department = c("IT", "HR", NA)  # Missing department
    ),
    performance = data.frame(
      employee_id = c(1, 2),
      rating = c(4, NA),  # Missing rating
      review_date = c(as.Date("2024-01-01"), NA)  # Missing date
    )
  )
  
  validation_result <- validate_cross_references(incomplete_data)
  
  expect_false(validation_result$is_valid)
  expect_true("completeness_issues" %in% names(validation_result))
  expect_gt(length(validation_result$completeness_issues), 0)
})

# =============================================================================
# TEST SUITE 3: FORMATTING CONSISTENCY
# =============================================================================

context("Report Module - Formatting Consistency")

test_that("format_report_values applies consistent number formatting", {
  # Test various numeric formats
  test_values <- list(
    currency = c(50000, 75000.50, 100000.99),
    percentages = c(0.1234, 0.5678, 1.0),
    ratings = c(3.4, 4.2, 4.87),
    counts = c(100, 1500, 25000)
  )
  
  formatted <- format_report_values(test_values, 
    currency_fields = "currency",
    percentage_fields = "percentages", 
    decimal_places = list(ratings = 1, default = 0)
  )
  
  # Verify currency formatting
  expect_true(all(str_detect(formatted$currency, "\\$")))
  expect_true(all(str_detect(formatted$currency, ",")))
  
  # Verify percentage formatting
  expect_true(all(str_detect(formatted$percentages, "%")))
  expect_equal(formatted$percentages[1], "12.3%")
  
  # Verify decimal consistency
  expect_equal(formatted$ratings[1], "3.4")
  expect_equal(formatted$counts[1], "100")
})

test_that("format_report_values handles date formatting consistently", {
  # Test various date formats
  test_dates <- list(
    hire_dates = as.Date(c("2020-01-15", "2021-12-31", "2024-06-01")),
    review_dates = as.POSIXct(c("2024-01-01 09:00:00", "2024-06-15 14:30:00")),
    quarters = c("2024-Q1", "2024-Q2", "2024-Q3")
  )
  
  formatted <- format_report_values(test_dates,
    date_format = "%B %d, %Y",
    datetime_format = "%b %d, %Y %I:%M %p"
  )
  
  # Verify date formatting consistency
  expect_equal(formatted$hire_dates[1], "January 15, 2020")
  expect_equal(formatted$hire_dates[2], "December 31, 2021")
  
  # Verify datetime formatting
  expect_true(str_detect(formatted$review_dates[1], "Jan 01, 2024"))
  expect_true(str_detect(formatted$review_dates[1], "AM|PM"))
})

test_that("format_report_values maintains consistent text formatting", {
  # Test text formatting consistency
  test_text <- list(
    names = c("john doe", "JANE SMITH", "Bob Johnson"),
    departments = c("human resources", "INFORMATION TECHNOLOGY", "Sales & Marketing"),
    job_titles = c("senior developer", "HR MANAGER", "sales representative")
  )
  
  formatted <- format_report_values(test_text,
    title_case_fields = c("names", "job_titles"),
    upper_case_fields = "departments"
  )
  
  # Verify title case formatting
  expect_equal(formatted$names[1], "John Doe")
  expect_equal(formatted$names[2], "Jane Smith")
  expect_equal(formatted$job_titles[1], "Senior Developer")
  
  # Verify upper case formatting
  expect_equal(formatted$departments[1], "HUMAN RESOURCES")
  expect_equal(formatted$departments[3], "SALES & MARKETING")
})

test_that("format_report_values handles edge cases gracefully", {
  # Test with edge cases
  edge_cases <- list(
    numbers_with_na = c(1000, NA, Inf, -Inf, 0),
    empty_strings = c("", " ", "Valid Text", NA),
    dates_with_na = c(as.Date("2024-01-01"), NA, as.Date("1900-01-01"))
  )
  
  formatted <- format_report_values(edge_cases)
  
  # Verify NA handling
  expect_equal(formatted$numbers_with_na[2], "N/A")
  expect_equal(formatted$empty_strings[4], "N/A")
  expect_equal(formatted$dates_with_na[2], "N/A")
  
  # Verify infinite value handling
  expect_equal(formatted$numbers_with_na[3], "∞")
  expect_equal(formatted$numbers_with_na[4], "-∞")
  
  # Verify empty string handling
  expect_equal(formatted$empty_strings[1], "N/A")
  expect_equal(formatted$empty_strings[2], "N/A")
})

test_that("format_report_tables maintains consistent table styling", {
  # Test table formatting consistency
  test_table <- data.frame(
    Employee = c("John Doe", "Jane Smith", "Bob Johnson"),
    Department = c("IT", "HR", "Sales"),
    Salary = c(75000, 85000, 65000),
    Performance = c(4.2, 4.8, 3.9),
    Hire_Date = as.Date(c("2020-01-01", "2019-06-15", "2021-03-01"))
  )
  
  formatted_table <- format_report_table(test_table,
    currency_cols = "Salary",
    decimal_cols = list(Performance = 1),
    date_cols = "Hire_Date"
  )
  
  # Verify column formatting consistency
  expect_true(all(str_detect(formatted_table$Salary, "\\$")))
  expect_equal(formatted_table$Performance[1], "4.2")
  expect_true(all(str_detect(formatted_table$Hire_Date, "Jan|Jun|Mar")))
  
  # Verify table structure preservation
  expect_equal(nrow(formatted_table), nrow(test_table))
  expect_equal(ncol(formatted_table), ncol(test_table))
})

# =============================================================================
# TEST SUITE 4: MULTI-LANGUAGE SUPPORT
# =============================================================================

context("Report Module - Multi-Language Support")

test_that("translate_report_labels supports multiple languages", {
  # Test label translation
  labels_to_translate <- c(
    "Employee", "Department", "Salary", "Performance", 
    "Attrition Rate", "Job Satisfaction", "Work-Life Balance"
  )
  
  # Test Spanish translation
  spanish_labels <- translate_report_labels(labels_to_translate, target_lang = "es")
  expect_equal(spanish_labels["Employee"], "Empleado")
  expect_equal(spanish_labels["Department"], "Departamento")
  expect_equal(spanish_labels["Salary"], "Salario")
  
  # Test French translation
  french_labels <- translate_report_labels(labels_to_translate, target_lang = "fr")
  expect_equal(french_labels["Employee"], "Employé")
  expect_equal(french_labels["Department"], "Département")
  expect_equal(french_labels["Performance"], "Performance")
})

test_that("format_numbers_by_locale handles different number formats", {
  test_numbers <- c(1234.56, 10000, 0.1234)
  
  # Test US locale (default)
  us_format <- format_numbers_by_locale(test_numbers, locale = "en_US")
  expect_equal(us_format[1], "1,234.56")
  expect_equal(us_format[2], "10,000")
  
  # Test German locale (uses comma for decimal, period for thousands)
  de_format <- format_numbers_by_locale(test_numbers, locale = "de_DE")
  expect_equal(de_format[1], "1.234,56")
  expect_equal(de_format[2], "10.000")
  
  # Test French locale
  fr_format <- format_numbers_by_locale(test_numbers, locale = "fr_FR")
  expect_equal(fr_format[1], "1 234,56")
  expect_equal(fr_format[2], "10 000")
})

test_that("format_dates_by_locale handles different date formats", {
  test_dates <- as.Date(c("2024-01-15", "2024-12-31"))
  
  # Test US date format
  us_dates <- format_dates_by_locale(test_dates, locale = "en_US")
  expect_equal(us_dates[1], "January 15, 2024")
  expect_equal(us_dates[2], "December 31, 2024")
  
  # Test UK date format  
  uk_dates <- format_dates_by_locale(test_dates, locale = "en_GB")
  expect_equal(uk_dates[1], "15 January 2024")
  expect_equal(uk_dates[2], "31 December 2024")
  
  # Test German date format
  de_dates <- format_dates_by_locale(test_dates, locale = "de_DE")
  expect_equal(de_dates[1], "15. Januar 2024")
  expect_equal(de_dates[2], "31. Dezember 2024")
})

test_that("generate_localized_report creates language-specific content", {
  # Sample report data
  sample_data <- list(
    summary = list(
      total_employees = 150,
      attrition_rate = 0.12,
      avg_satisfaction = 4.2
    ),
    employees = data.frame(
      name = c("John Doe", "Jane Smith"),
      department = c("IT", "HR"),
      salary = c(75000, 85000)
    )
  )
  
  # Generate English report
  en_report <- generate_localized_report(sample_data, locale = "en_US")
  expect_true(str_detect(en_report$content, "Total Employees"))
  expect_true(str_detect(en_report$content, "Department"))
  
  # Generate Spanish report
  es_report <- generate_localized_report(sample_data, locale = "es_ES")
  expect_true(str_detect(es_report$content, "Empleados Totales|Total de Empleados"))
  expect_true(str_detect(es_report$content, "Departamento"))
  
  # Verify locale-specific number formatting
  expect_true(str_detect(en_report$content, "75,000"))  # US format
  expect_true(str_detect(es_report$content, "75.000"))  # Spanish format
})

test_that("validate_locale_support checks available translations", {
  # Test supported locales
  supported_locales <- get_supported_locales()
  expect_true("en_US" %in% supported_locales)
  expect_true("es_ES" %in% supported_locales)
  expect_true("fr_FR" %in% supported_locales)
  expect_true("de_DE" %in% supported_locales)
  
  # Test locale validation
  expect_true(validate_locale("en_US"))
  expect_true(validate_locale("es_ES"))
  expect_false(validate_locale("invalid_locale"))
  expect_false(validate_locale("xx_XX"))
})

test_that("handle_rtl_languages supports right-to-left text", {
  # Test RTL language detection
  expect_true(is_rtl_locale("ar_SA"))  # Arabic
  expect_true(is_rtl_locale("he_IL"))  # Hebrew
  expect_false(is_rtl_locale("en_US")) # English
  expect_false(is_rtl_locale("es_ES")) # Spanish
  
  # Test RTL content formatting
  rtl_content <- "اسم الموظف: أحمد محمد"
  formatted_rtl <- format_rtl_content(rtl_content)
  expect_true(str_detect(formatted_rtl, 'dir="rtl"'))
  expect_true(str_detect(formatted_rtl, 'text-align: right'))
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

context("Report Module - Integration Tests")

test_that("full report generation pipeline works end-to-end", {
  # Complete test data
  complete_data <- list(
    employees = data.frame(
      employee_id = 1:5,
      name = c("John Doe", "Jane Smith", "José García", "François Müller", "Bob Johnson"),
      department = c("IT", "HR", "Sales", "IT", "Marketing"),
      salary = c(75000, 85000, 65000, 90000, 70000),
      hire_date = as.Date(c("2020-01-01", "2019-06-15", "2021-03-01", "2018-09-01", "2022-01-15"))
    ),
    performance = data.frame(
      employee_id = 1:5,
      rating = c(4.2, 4.8, 3.9, 4.5, 4.1),
      review_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04", "2024-01-05"))
    )
  )
  
  # Test full pipeline
  # 1. Serialize data
  serialized <- serialize_report_data(complete_data)
  expect_true(serialized$serialization_safe)
  
  # 2. Validate cross-references
  validation <- validate_cross_references(complete_data)
  expect_true(validation$is_valid)
  
  # 3. Format data
  formatted <- format_report_values(complete_data$employees, 
    currency_fields = "salary",
    title_case_fields = "name"
  )
  expect_true(all(str_detect(formatted$salary, "\\$")))
  
  # 4. Generate localized report
  localized <- generate_localized_report(complete_data, locale = "en_US")
  expect_true(nchar(localized$content) > 0)
  expect_true(localized$generation_successful)
})

test_that("error handling works across all components", {
  # Test with corrupted data
  corrupted_data <- list(
    employees = "not_a_dataframe",
    performance = data.frame(invalid = "structure")
  )
  
  # Each component should handle errors gracefully
  expect_warning(serialized <- serialize_report_data(corrupted_data))
  expect_false(serialized$serialization_safe)
  
  expect_warning(validation <- validate_cross_references(corrupted_data))
  expect_false(validation$is_valid)
  
  expect_error(formatted <- format_report_values(corrupted_data))
  
  expect_warning(localized <- generate_localized_report(corrupted_data))
  expect_false(localized$generation_successful)
})

# =============================================================================
# PERFORMANCE TESTS
# =============================================================================

context("Report Module - Performance Tests")

test_that("large dataset processing meets performance requirements", {
  # Create large dataset
  large_dataset <- list(
    employees = data.frame(
      employee_id = 1:50000,
      name = paste("Employee", 1:50000),
      department = sample(c("IT", "HR", "Sales", "Marketing", "Finance"), 50000, replace = TRUE),
      salary = runif(50000, 30000, 150000)
    )
  )
  
  # Test serialization performance
  start_time <- Sys.time()
  serialized <- serialize_report_data(large_dataset)
  serialization_time <- difftime(Sys.time(), start_time, units = "secs")
  
  expect_lt(as.numeric(serialization_time), 10)  # Should complete within 10 seconds
  
  # Test validation performance
  start_time <- Sys.time()
  validation <- validate_cross_references(large_dataset)
  validation_time <- difftime(Sys.time(), start_time, units = "secs")
  
  expect_lt(as.numeric(validation_time), 5)  # Should complete within 5 seconds
})

# =============================================================================
# MOCK FUNCTIONS FOR TESTING
# =============================================================================

# Mock implementations of the report module functions for testing
serialize_report_data <- function(data) {
  if (is.null(data) || length(data) == 0) {
    return(list(status = "empty", data = NULL))
  }
  
  tryCatch({
    # Check for circular references
    if (any(sapply(data, function(x) identical(x, data)))) {
      warning("Circular reference detected")
      return(list(serialization_safe = FALSE, error_message = "Circular reference"))
    }
    
    # Get data types
    data_types <- lapply(data, function(x) {
      if (is.data.frame(x)) {
        sapply(x, class)[1,]
      } else {
        class(x)[1]
      }
    })
    
    # Calculate size
    data_size <- object.size(data)
    
    list(
      serialization_safe = TRUE,
      data = data,
      metadata = list(
        data_types = data_types,
        data_size_mb = as.numeric(data_size) / 1024^2
      )
    )
  }, error = function(e) {
    list(serialization_safe = FALSE, error_message = e$message)
  })
}

validate_cross_references <- function(data, check_date_consistency = FALSE) {
  result <- list(is_valid = TRUE, issues = list())
  
  if ("employees" %in% names(data) && "performance" %in% names(data)) {
    emp_ids <- data$employees$employee_id
    perf_ids <- data$performance$employee_id
    
    missing_refs <- setdiff(perf_ids, emp_ids)
    if (length(missing_refs) > 0) {
      result$is_valid <- FALSE
      result$missing_employee_refs <- list(performance = missing_refs)
      result$orphaned_records <- length(missing_refs)
    }
  }
  
  # Additional validation logic would go here
  result
}

format_report_values <- function(data, ...) {
  args <- list(...)
  
  if (is.data.frame(data)) {
    result <- data
    
    # Format currency fields
    if ("currency_fields" %in% names(args)) {
      for (field in args$currency_fields) {
        if (field %in% names(result)) {
          result[[field]] <- paste0("$", format(result[[field]], big.mark = ",", scientific = FALSE))
        }
      }
    }
    
    # Format percentage fields
    if ("percentage_fields" %in% names(args)) {
      for (field in args$percentage_fields) {
        if (field %in% names(result)) {
          result[[field]] <- paste0(round(result[[field]] * 100, 1), "%")
        }
      }
    }
    
    # Handle title case
    if ("title_case_fields" %in% names(args)) {
      for (field in args$title_case_fields) {
        if (field %in% names(result)) {
          result[[field]] <- str_to_title(result[[field]])
        }
      }
    }
    
    return(result)
  }
  
  # Handle list data
  result <- data
  for (name in names(result)) {
    if (is.numeric(result[[name]])) {
      result[[name]][is.na(result[[name]])] <- "N/A"
      result[[name]][is.infinite(result[[name]]) & result[[name]] > 0] <- "∞"
      result[[name]][is.infinite(result[[name]]) & result[[name]] < 0] <- "-∞"
    }
    if (is.character(result[[name]])) {
      result[[name]][is.na(result[[name]]) | result[[name]] == "" | str_trim(result[[name]]) == ""] <- "N/A"
    }
  }
  
  result
}

translate_report_labels <- function(labels, target_lang = "en") {
  translations <- list(
    es = list(
      "Employee" = "Empleado",
      "Department" = "Departamento", 
      "Salary" = "Salario",
      "Performance" = "Rendimiento"
    ),
    fr = list(
      "Employee" = "Employé",
      "Department" = "Département",
      "Salary" = "Salaire",
      "Performance" = "Performance"
    )
  )
  
  if (target_lang %in% names(translations)) {
    result <- character(length(labels))
    