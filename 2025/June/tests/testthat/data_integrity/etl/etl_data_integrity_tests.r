# ===============================================================================
# ATLAS LABS HR ANALYTICS - ETL DATA INTEGRITY TESTING SUITE
# Comprehensive unit tests for Extract, Transform, Load processes
# Focus: Data integrity, error handling, and robustness validation
# ===============================================================================

library(testthat)
library(tidyverse)
library(lubridate)
library(digest)
library(R6)

# Mock Atlas Logger for testing
MockAtlasLogger <- R6Class("MockAtlasLogger",
  private = list(
    .logs = list(),
    .performance_data = list()
  ),
  public = list(
    log_info = function(message, module = "test", performance_data = NULL) {
      private$.logs <- append(private$.logs, list(list(
        level = "INFO", message = message, module = module, 
        timestamp = Sys.time(), performance = performance_data
      )))
    },
    log_warning = function(message, module = "test") {
      private$.logs <- append(private$.logs, list(list(
        level = "WARNING", message = message, module = module, 
        timestamp = Sys.time()
      )))
    },
    log_error = function(message, module = "test") {
      private$.logs <- append(private$.logs, list(list(
        level = "ERROR", message = message, module = module, 
        timestamp = Sys.time()
      )))
    },
    get_logs = function() private$.logs,
    clear_logs = function() private$.logs <- list()
  )
)

# ===============================================================================
# 5.2.1 EXTRACT ACCURACY VALIDATION TESTS
# ===============================================================================

test_that("Extract Accuracy - Valid CSV file extraction", {
  # Setup test data
  temp_file <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    EmployeeID = c(1001, 1002, 1003),
    FirstName = c("John", "Jane", "Bob"),
    LastName = c("Doe", "Smith", "Wilson"),
    Age = c(25, 30, 35),
    Salary = c(50000, 60000, 70000),
    HireDate = c("2020-01-15", "2019-06-01", "2018-03-10"),
    Attrition = c("No", "Yes", "No")
  )
  write.csv(test_data, temp_file, row.names = FALSE)
  
  # Mock extraction function
  extract_employee_data <- function(file_path, logger = NULL) {
    tryCatch({
      if (!file.exists(file_path)) {
        stop("File does not exist: ", file_path)
      }
      
      data <- read.csv(file_path, stringsAsFactors = FALSE)
      
      if (nrow(data) == 0) {
        stop("Empty dataset detected")
      }
      
      # Log extraction success
      if (!is.null(logger)) {
        logger$log_info(paste("Successfully extracted", nrow(data), "records"), 
                       "extract_validation")
      }
      
      return(list(
        data = data,
        records_extracted = nrow(data),
        columns_extracted = ncol(data),
        file_size = file.info(file_path)$size,
        extraction_timestamp = Sys.time(),
        file_hash = digest(file = file_path, algo = "md5")
      ))
    }, error = function(e) {
      if (!is.null(logger)) {
        logger$log_error(paste("Extraction failed:", e$message), "extract_validation")
      }
      stop(e)
    })
  }
  
  logger <- MockAtlasLogger$new()
  result <- extract_employee_data(temp_file, logger)
  
  # Assertions
  expect_equal(result$records_extracted, 3)
  expect_equal(result$columns_extracted, 7)
  expect_true(result$file_size > 0)
  expect_true(!is.null(result$file_hash))
  expect_equal(nrow(result$data), 3)
  expect_true(all(c("EmployeeID", "FirstName", "Salary") %in% names(result$data)))
  
  # Cleanup
  unlink(temp_file)
})

test_that("Extract Accuracy - Corrupted file handling", {
  # Create corrupted CSV file
  temp_file <- tempfile(fileext = ".csv")
  writeLines(c(
    "EmployeeID,FirstName,LastName,Age",
    "1001,John,Doe,25",
    "1002,Jane,Smith", # Missing column
    "1003,Bob,Wilson,35,Extra" # Extra column
  ), temp_file)
  
  extract_with_validation <- function(file_path, logger = NULL) {
    tryCatch({
      data <- read.csv(file_path, stringsAsFactors = FALSE)
      
      # Check for inconsistent columns
      expected_cols <- 4
      if (any(lengths(strsplit(readLines(file_path)[-1], ",")) != expected_cols)) {
        warning("Inconsistent column count detected in file")
        if (!is.null(logger)) {
          logger$log_warning("Corrupted file structure detected", "extract_validation")
        }
      }
      
      return(list(data = data, status = "success_with_warnings"))
    }, error = function(e) {
      if (!is.null(logger)) {
        logger$log_error(paste("File corruption detected:", e$message), "extract_validation")
      }
      return(list(data = NULL, status = "failed", error = e$message))
    })
  }
  
  logger <- MockAtlasLogger$new()
  result <- extract_with_validation(temp_file, logger)
  
  # Should handle corruption gracefully
  expect_true(!is.null(result))
  logs <- logger$get_logs()
  expect_true(any(sapply(logs, function(x) grepl("Corrupted|corruption", x$message))))
  
  unlink(temp_file)
})

test_that("Extract Accuracy - Missing file error handling", {
  non_existent_file <- "/path/that/does/not/exist.csv"
  
  extract_employee_data <- function(file_path, logger = NULL) {
    if (!file.exists(file_path)) {
      if (!is.null(logger)) {
        logger$log_error(paste("File not found:", file_path), "extract_validation")
      }
      stop("File does not exist: ", file_path)
    }
    read.csv(file_path)
  }
  
  logger <- MockAtlasLogger$new()
  
  expect_error(extract_employee_data(non_existent_file, logger))
  logs <- logger$get_logs()
  expect_true(any(sapply(logs, function(x) x$level == "ERROR")))
})

test_that("Extract Accuracy - Large file memory management", {
  # Create large test file
  temp_file <- tempfile(fileext = ".csv")
  large_data <- data.frame(
    EmployeeID = 1:10000,
    FirstName = rep(c("John", "Jane", "Bob", "Alice"), 2500),
    Salary = sample(30000:120000, 10000, replace = TRUE),
    HireDate = sample(seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = "day"), 
                     10000, replace = TRUE)
  )
  write.csv(large_data, temp_file, row.names = FALSE)
  
  extract_large_file <- function(file_path, chunk_size = 1000, logger = NULL) {
    file_size <- file.info(file_path)$size
    memory_before <- as.numeric(object.size(.GlobalEnv))
    
    if (file_size > 1024^2) { # > 1MB
      # Chunked reading for large files
      con <- file(file_path, "r")
      header <- readLines(con, n = 1)
      close(con)
      
      chunks <- list()
      skip_lines <- 1
      
      repeat {
        chunk <- tryCatch({
          read.csv(file_path, skip = skip_lines, nrows = chunk_size, 
                  header = FALSE, col.names = strsplit(header, ",")[[1]])
        }, error = function(e) NULL)
        
        if (is.null(chunk) || nrow(chunk) == 0) break
        
        chunks <- append(chunks, list(chunk))
        skip_lines <- skip_lines + chunk_size
        
        # Memory check
        memory_current <- as.numeric(object.size(.GlobalEnv))
        if (memory_current > memory_before * 2) {
          if (!is.null(logger)) {
            logger$log_warning("High memory usage detected during extraction", 
                             "extract_validation")
          }
        }
      }
      
      data <- do.call(rbind, chunks)
    } else {
      data <- read.csv(file_path)
    }
    
    if (!is.null(logger)) {
      logger$log_info(paste("Large file extraction completed:", nrow(data), "records"), 
                     "extract_validation")
    }
    
    return(data)
  }
  
  logger <- MockAtlasLogger$new()
  result <- extract_large_file(temp_file, logger = logger)
  
  expect_equal(nrow(result), 10000)
  expect_true(all(c("EmployeeID", "FirstName", "Salary") %in% names(result)))
  
  unlink(temp_file)
})

# ===============================================================================
# 5.2.2 TRANSFORMATION LOGIC VERIFICATION TESTS
# ===============================================================================

test_that("Transformation Logic - Date format standardization", {
  # Test data with various date formats
  test_data <- data.frame(
    EmployeeID = 1:5,
    HireDate = c("2020-01-15", "15/01/2020", "Jan 15, 2020", "2020.01.15", "15-Jan-2020"),
    stringsAsFactors = FALSE
  )
  
  standardize_dates <- function(data, date_column, logger = NULL) {
    original_format_counts <- table(nchar(data[[date_column]]))
    
    # Try multiple date formats
    date_formats <- c("%Y-%m-%d", "%d/%m/%Y", "%b %d, %Y", "%Y.%m.%d", "%d-%b-%Y")
    
    standardized_dates <- rep(as.Date(NA), nrow(data))
    
    for (i in seq_len(nrow(data))) {
      date_str <- data[[date_column]][i]
      
      for (format in date_formats) {
        parsed_date <- tryCatch(
          as.Date(date_str, format = format),
          error = function(e) NA
        )
        
        if (!is.na(parsed_date)) {
          standardized_dates[i] <- parsed_date
          break
        }
      }
    }
    
    # Check for parsing failures
    failed_parsing <- sum(is.na(standardized_dates))
    if (failed_parsing > 0 && !is.null(logger)) {
      logger$log_warning(paste("Failed to parse", failed_parsing, "dates"), 
                        "transformation_validation")
    }
    
    data[[paste0(date_column, "_standardized")]] <- standardized_dates
    
    if (!is.null(logger)) {
      logger$log_info(paste("Date standardization completed. Success rate:", 
                           round((nrow(data) - failed_parsing) / nrow(data) * 100, 2), "%"), 
                     "transformation_validation")
    }
    
    return(list(
      data = data,
      success_count = nrow(data) - failed_parsing,
      failure_count = failed_parsing,
      original_formats = names(original_format_counts)
    ))
  }
  
  logger <- MockAtlasLogger$new()
  result <- standardize_dates(test_data, "HireDate", logger)
  
  expect_equal(result$success_count, 5)
  expect_equal(result$failure_count, 0)
  expect_true("HireDate_standardized" %in% names(result$data))
  expect_true(all(!is.na(result$data$HireDate_standardized)))
  expect_true(all(class(result$data$HireDate_standardized) == "Date"))
})

test_that("Transformation Logic - Salary data cleaning and validation", {
  # Test data with various salary formats and edge cases
  test_data <- data.frame(
    EmployeeID = 1:8,
    Salary = c("$50,000", "60000", "$75,000.50", "0", "-5000", "NULL", "", "1000000"),
    stringsAsFactors = FALSE
  )
  
  clean_salary_data <- function(data, salary_column, logger = NULL) {
    original_data <- data[[salary_column]]
    
    # Remove currency symbols and commas
    cleaned_salary <- gsub("[\\$,]", "", data[[salary_column]])
    
    # Convert to numeric
    numeric_salary <- suppressWarnings(as.numeric(cleaned_salary))
    
    # Define business rules
    min_salary <- 15000  # Minimum wage threshold
    max_salary <- 500000 # Maximum reasonable salary
    
    # Validation checks
    validation_results <- list(
      invalid_format = sum(is.na(numeric_salary) & cleaned_salary != "" & 
                          cleaned_salary != "NULL"),
      negative_values = sum(numeric_salary < 0, na.rm = TRUE),
      zero_values = sum(numeric_salary == 0, na.rm = TRUE),
      below_minimum = sum(numeric_salary < min_salary & numeric_salary > 0, na.rm = TRUE),
      above_maximum = sum(numeric_salary > max_salary, na.rm = TRUE),
      null_values = sum(is.na(numeric_salary) | cleaned_salary == "" | 
                       cleaned_salary == "NULL")
    )
    
    # Apply business rules
    validated_salary <- numeric_salary
    validated_salary[numeric_salary < 0] <- NA  # Remove negative values
    validated_salary[numeric_salary == 0] <- NA  # Remove zero values
    validated_salary[numeric_salary > max_salary] <- max_salary  # Cap at maximum
    
    # Flag records that need review
    flags <- rep("VALID", length(validated_salary))
    flags[is.na(validated_salary)] <- "INVALID"
    flags[numeric_salary > max_salary] <- "CAPPED"
    flags[numeric_salary < min_salary & numeric_salary > 0] <- "BELOW_MIN"
    
    data[[paste0(salary_column, "_cleaned")]] <- validated_salary
    data[[paste0(salary_column, "_flag")]] <- flags
    
    # Logging
    if (!is.null(logger)) {
      for (check_name in names(validation_results)) {
        if (validation_results[[check_name]] > 0) {
          logger$log_warning(paste("Salary validation:", check_name, "-", 
                                 validation_results[[check_name]], "records"), 
                           "transformation_validation")
        }
      }
      
      valid_records <- sum(flags == "VALID")
      logger$log_info(paste("Salary cleaning completed:", valid_records, "valid records"), 
                     "transformation_validation")
    }
    
    return(list(
      data = data,
      validation_results = validation_results,
      valid_count = sum(flags == "VALID"),
      flagged_count = sum(flags != "VALID")
    ))
  }
  
  logger <- MockAtlasLogger$new()
  result <- clean_salary_data(test_data, "Salary", logger)
  
  expect_true("Salary_cleaned" %in% names(result$data))
  expect_true("Salary_flag" %in% names(result$data))
  expect_equal(result$validation_results$negative_values, 1)
  expect_equal(result$validation_results$zero_values, 1)
  expect_true(result$valid_count > 0)
  
  # Check specific transformations
  expect_equal(result$data$Salary_cleaned[1], 50000)  # $50,000 -> 50000
  expect_equal(result$data$Salary_cleaned[2], 60000)  # 60000 -> 60000
  expect_true(is.na(result$data$Salary_cleaned[5]))   # -5000 -> NA
})

test_that("Transformation Logic - Employee ID validation and deduplication", {
  # Test data with duplicate and invalid employee IDs
  test_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002", "EMP001", "INVALID", "", "EMP003", "EMP002"),
    FirstName = c("John", "Jane", "John", "Invalid", "Empty", "Bob", "Jane2"),
    LastName = c("Doe", "Smith", "Doe", "User", "User", "Wilson", "Smith"),
    stringsAsFactors = FALSE
  )
  
  validate_employee_ids <- function(data, logger = NULL) {
    original_count <- nrow(data)
    
    # Employee ID format validation (should be EMP followed by 3 digits)
    id_pattern <- "^EMP\\d{3}$"
    valid_format <- grepl(id_pattern, data$EmployeeID)
    
    # Check for empty IDs
    empty_ids <- data$EmployeeID == "" | is.na(data$EmployeeID)
    
    # Check for duplicates
    duplicate_ids <- duplicated(data$EmployeeID) | duplicated(data$EmployeeID, fromLast = TRUE)
    
    # Create validation flags
    data$ID_ValidationFlag <- "VALID"
    data$ID_ValidationFlag[!valid_format] <- "INVALID_FORMAT"
    data$ID_ValidationFlag[empty_ids] <- "EMPTY_ID"
    data$ID_ValidationFlag[duplicate_ids & valid_format] <- "DUPLICATE"
    
    # Summary statistics
    validation_summary <- table(data$ID_ValidationFlag)
    
    # Handle duplicates by keeping first occurrence and flagging others
    if (sum(duplicate_ids) > 0) {
      duplicate_employee_ids <- data$EmployeeID[duplicate_ids & !duplicated(data$EmployeeID)]
      
      for (emp_id in duplicate_employee_ids) {
        duplicate_indices <- which(data$EmployeeID == emp_id)
        if (length(duplicate_indices) > 1) {
          # Keep first occurrence, mark others for removal
          data$ID_ValidationFlag[duplicate_indices[-1]] <- "DUPLICATE_REMOVE"
        }
      }
    }
    
    # Clean dataset (remove invalid records)
    clean_data <- data[data$ID_ValidationFlag %in% c("VALID", "DUPLICATE"), ]
    removed_data <- data[!data$ID_ValidationFlag %in% c("VALID", "DUPLICATE"), ]
    
    # Logging
    if (!is.null(logger)) {
      logger$log_info(paste("Employee ID validation completed:", nrow(clean_data), 
                           "valid records retained from", original_count, "original records"), 
                     "transformation_validation")
      
      if (nrow(removed_data) > 0) {
        logger$log_warning(paste("Removed", nrow(removed_data), "records with invalid Employee IDs"), 
                          "transformation_validation")
      }
    }
    
    return(list(
      clean_data = clean_data,
      removed_data = removed_data,
      validation_summary = validation_summary,
      duplicate_count = sum(grepl("DUPLICATE", data$ID_ValidationFlag)),
      invalid_format_count = sum(data$ID_ValidationFlag == "INVALID_FORMAT"),
      empty_id_count = sum(data$ID_ValidationFlag == "EMPTY_ID")
    ))
  }
  
  logger <- MockAtlasLogger$new()
  result <- validate_employee_ids(test_data, logger)
  
  expect_equal(result$duplicate_count, 2)  # EMP001 and EMP002 appear twice
  expect_equal(result$invalid_format_count, 1)  # "INVALID"
  expect_equal(result$empty_id_count, 1)  # ""
  expect_true(nrow(result$clean_data) < nrow(test_data))
  expect_true(all(grepl("^EMP\\d{3}$", result$clean_data$EmployeeID)))
})

# ===============================================================================
# 5.2.3 LOAD PROCESS INTEGRITY TESTS
# ===============================================================================

test_that("Load Process - Database integrity constraints", {
  # Mock database connection and operations
  mock_db <- list(
    employees = data.frame(),
    performance = data.frame(),
    education = data.frame()
  )
  
  load_to_database <- function(data, table_name, db_connection, logger = NULL) {
    tryCatch({
      # Simulate pre-load validation
      if (nrow(data) == 0) {
        stop("Cannot load empty dataset")
      }
      
      # Check for required columns based on table
      required_columns <- switch(table_name,
        "employees" = c("EmployeeID", "FirstName", "LastName"),
        "performance" = c("EmployeeID", "PerformanceID"),
        "education" = c("EducationID", "EducationLevel"),
        character(0)
      )
      
      missing_columns <- setdiff(required_columns, names(data))
      if (length(missing_columns) > 0) {
        stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
      }
      
      # Simulate referential integrity check
      if (table_name == "performance" && "employees" %in% names(db_connection)) {
        existing_emp_ids <- db_connection$employees$EmployeeID
        invalid_refs <- setdiff(data$EmployeeID, existing_emp_ids)
        
        if (length(invalid_refs) > 0) {
          if (!is.null(logger)) {
            logger$log_warning(paste("Found", length(invalid_refs), 
                                   "invalid employee references"), "load_validation")
          }
          # Remove invalid references
          data <- data[data$EmployeeID %in% existing_emp_ids, ]
        }
      }
      
      # Simulate unique constraint check
      if (table_name == "employees" && nrow(db_connection$employees) > 0) {
        existing_ids <- db_connection$employees$EmployeeID
        duplicate_ids <- intersect(data$EmployeeID, existing_ids)
        
        if (length(duplicate_ids) > 0) {
          if (!is.null(logger)) {
            logger$log_warning(paste("Found", length(duplicate_ids), 
                                   "duplicate employee IDs, skipping"), "load_validation")
          }
          data <- data[!data$EmployeeID %in% duplicate_ids, ]
        }
      }
      
      # Simulate the load operation
      db_connection[[table_name]] <- rbind(db_connection[[table_name]], data)
      
      if (!is.null(logger)) {
        logger$log_info(paste("Successfully loaded", nrow(data), "records to", table_name), 
                       "load_validation")
      }
      
      return(list(
        success = TRUE,
        records_loaded = nrow(data),
        table = table_name,
        timestamp = Sys.time()
      ))
      
    }, error = function(e) {
      if (!is.null(logger)) {
        logger$log_error(paste("Load failed for table", table_name, ":", e$message), 
                        "load_validation")
      }
      return(list(
        success = FALSE,
        error = e$message,
        table = table_name,
        timestamp = Sys.time()
      ))
    })
  }
  
  # Test data
  employee_data <- data.frame(
    EmployeeID = c("EMP001", "EMP002", "EMP003"),
    FirstName = c("John", "Jane", "Bob"),
    LastName = c("Doe", "Smith", "Wilson")
  )
  
  performance_data <- data.frame(
    PerformanceID = c("PERF001", "PERF002"),
    EmployeeID = c("EMP001", "EMP999"),  # EMP999 doesn't exist
    JobSatisfaction = c(4, 5)
  )
  
  logger <- MockAtlasLogger$new()
  
  # Load employees first
  result1 <- load_to_database(employee_data, "employees", mock_db, logger)
  expect_true(result1$success)
  expect_equal(result1$records_loaded, 3)
  
  # Load performance (should handle referential integrity)
  result2 <- load_to_database(performance_data, "performance", mock_db, logger)
  expect_true(result2$success)
  expect_equal(result2$records_loaded, 1)  # Only EMP001 should be loaded
  
  # Check logs for referential integrity warning
  logs <- logger$get_logs()
  ref_integrity_logs <- logs[sapply(logs, function(x) grepl("invalid employee references", x$message))]
  expect_length(ref_integrity_logs, 1)
})

test_that("Load Process - Transaction rollback capability", {
  # Mock database with transaction support
  mock_db_with_transactions <- R6Class("MockDB",
    private = list(
      .data = list(employees = data.frame()),
      .transaction_active = FALSE,
      .backup_data = NULL
    ),
    public = list(
      begin_transaction = function() {
        private$.backup_data <- private$.data
        private$.transaction_active <- TRUE
      },
      
      commit_transaction = function() {
        private$.transaction_active <- FALSE
        private$.backup_data <- NULL
      },
      
      rollback_transaction = function() {
        if (private$.transaction_active) {
          private$.data <- private$.backup_data
          private$.transaction_active <- FALSE
          private$.backup_data <- NULL
        }
      },
      
      insert_data = function(table_name, data) {
        if (!table_name %in% names(private$.data)) {
          stop("Table does not exist: ", table_name)
        }
        private$.data[[table_name]] <- rbind(private$.data[[table_name]], data)
      },
      
      get_data = function(table_name) {
        private$.data[[table_name]]
      },
      
      is_transaction_active = function() {
        private$.transaction_active
      }
    )
  )
  
  transactional_load <- function(data_list, db, logger = NULL) {
    tryCatch({
      # Begin transaction
      db$begin_transaction()
      
      if (!is.null(logger)) {
        logger$log_info("Transaction started", "load_validation")
      }
      
      loaded_tables <- character(0)
      
      for (table_name in names(data_list)) {
        data <- data_list[[table_name]]
        
        # Simulate validation that might fail
        if (table_name == "employees" && any(is.na(data$EmployeeID))) {
          stop("NULL employee IDs not allowed")
        }
        
        if (table_name == "performance" && any(data$JobSatisfaction > 5)) {
          stop("Job satisfaction must be between 1 and 5")
        }
        
        db$insert_data(table_name, data)
        loaded_tables <- c(loaded_tables, table_name)
        
        if (!is.null(logger)) {
          logger$log_info(paste("Loaded", nrow(data), "records to", table_name), 
                         "load_validation")
        }
      }
      
      # Commit transaction
      db$commit_transaction()
      
      if (!is.null(logger)) {
        logger$log_info("Transaction committed successfully", "load_validation")
      }
      
      return(list(
        success = TRUE,
        tables_loaded = loaded_tables,
        transaction_committed = TRUE
      ))
      
    }, error = function(e) {
      # Rollback transaction
      if (db$is_transaction_active()) {
        db$rollback_transaction()
        
        if (!is.null(logger)) {
          logger$log_error(paste("Transaction rolled back due to error:", e$message), 
                          "load_validation")
        }
      }
      
      return(list(
        success = FALSE,
        error = e$message,
        transaction_rolled_back = TRUE
      ))
    })
  }
  
  # Test successful transaction
  db <- mock_db_with_transactions$new()
  logger <- MockAtlasLogger$new()
  
  valid_data <- list(
    employees = data.frame(
      EmployeeID = c("EMP001", "EMP002"),
      FirstName = c("John", "Jane"),
      LastName = c("Doe", "Smith")
    )
  )
  
  result <- transactional_load(valid_data, db, logger)
  expect_true(result$success)
  expect_true(result$transaction_committed)
  expect_equal(nrow(db$get_data("employees")), 2)
  
  # Test failed transaction with rollback
  logger$clear_logs()
  invalid_data <- list(
    employees = data.frame(
      EmployeeID = c("EMP003", NA),  # NA will cause failure
      FirstName = c("Bob", "Invalid"),
      LastName = c("Wilson", "User")
    )
  )
  
  result2 <- transactional_load(invalid_data, db, logger)
  expect_false(result2$success)
  expect_true(result2$transaction_rolled_back)
  expect_equal(nrow(db$get_data("employees")), 2)  # Should still be 2, not 4
  
  # Check rollback was logged
  logs <- logger$get_logs()
  rollback_logs <- logs[sapply(logs, function(x) grepl("rolled back", x$message))]
  expect_length(rollback_logs, 1)
})

# ===============================================================================
# 5.2.4 ERROR HANDLING ROBUSTNESS TESTS
# ===============================================================================

test_that("Error Handling - Network connectivity issues", {
  simulate_network_load <- function(data, endpoint, retry_count = 3, logger = NULL) {
    attempt <- 1
    
    while (attempt <= retry_count) {
      tryCatch({
        # Simulate network call
        if (endpoint == "unreliable_server" && attempt < 3) {
          stop("Connection timeout")
        }
        
        if (endpoint == "always_fails") {
          stop("Server unavailable")
        }
        
        # Simulate successful upload
        Sys.sleep(0.1)  # Simulate network delay
        
        if (!is.null(logger)) {
          logger$log_info(paste("Data loaded successfully on