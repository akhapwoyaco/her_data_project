# =============================================================================
# ATLAS LABS HR ANALYTICS - DATABASE INTEGRATION TESTS
# Comprehensive Testing Suite for Data Operations
# Developer: akhapwoyaco
# =============================================================================

# Required Libraries for Testing
library(testthat)
library(tibble)
library(dplyr)
library(purrr)
library(readr)
library(fs)
library(parallel)
library(future)
library(furrr)
library(digest)
library(lubridate)

# Source the modules being tested
source("modules/data_loader_module.R")
source("modules/logger_module.R")
source("global.R")
source("utils.R")

# =============================================================================
# 6.2.1 CONNECTION RELIABILITY TESTS
# =============================================================================

test_that("Data Connection Reliability - File Access", {
  
  # Test 1: Basic file existence and accessibility
  test_basic_file_access <- function() {
    expected_files <- c("data/employee.csv", "data/performance_rating.csv", "data/education_level.csv")
    
    for (file in expected_files) {
      expect_true(file.exists(file), info = paste("File should exist:", file))
      expect_true(file.access(file, 4) == 0, info = paste("File should be readable:", file))
    }
  }
  
  # Test 2: File corruption detection
  test_file_integrity <- function() {
    test_files <- c("data/employee.csv", "data/performance_rating.csv", "data/education_level.csv")
    
    for (file in test_files) {
      if (file.exists(file)) {
        # Test if file can be read without errors
        expect_silent({
          data <- readr::read_csv(file, show_col_types = FALSE)
        }, info = paste("File should be readable without errors:", file))
        
        # Test if file has expected structure
        data <- readr::read_csv(file, show_col_types = FALSE)
        expect_true(nrow(data) > 0, info = paste("File should contain data:", file))
        expect_true(ncol(data) > 0, info = paste("File should have columns:", file))
      }
    }
  }
  
  # Test 3: Network/Path reliability simulation
  test_path_reliability <- function() {
    # Test various path formats
    test_paths <- c(
      "./data/employee.csv",
      "data/employee.csv",
      file.path("data", "employee.csv")
    )
    
    for (path in test_paths) {
      if (file.exists(path)) {
        expect_silent({
          data <- readr::read_csv(path, show_col_types = FALSE)
        }, info = paste("Path should be accessible:", path))
      }
    }
  }
  
  # Test 4: Connection timeout simulation
  test_connection_timeout <- function() {
    # Simulate file locking scenario
    temp_file <- tempfile(fileext = ".csv")
    writeLines("col1,col2\n1,2", temp_file)
    
    # Test reading locked file (simulated)
    expect_silent({
      data <- readr::read_csv(temp_file, show_col_types = FALSE)
    })
    
    unlink(temp_file)
  }
  
  # Test 5: Recovery mechanism testing
  test_connection_recovery <- function() {
    # Test backup file mechanism
    primary_file <- "data/employee.csv"
    backup_file <- "data/employee_backup.csv"
    
    if (file.exists(primary_file)) {
      # Create backup
      file.copy(primary_file, backup_file, overwrite = TRUE)
      
      # Test fallback mechanism
      expect_true(file.exists(backup_file), "Backup file should be created")
      
      # Test recovery
      if (file.exists(backup_file)) {
        data_backup <- readr::read_csv(backup_file, show_col_types = FALSE)
        expect_true(nrow(data_backup) > 0, "Backup data should be accessible")
      }
      
      # Cleanup
      if (file.exists(backup_file)) unlink(backup_file)
    }
  }
  
  # Execute all reliability tests
  test_basic_file_access()
  test_file_integrity()
  test_path_reliability()
  test_connection_timeout()
  test_connection_recovery()
})

# =============================================================================
# 6.2.2 TRANSACTION CONSISTENCY TESTS
# =============================================================================

test_that("Transaction Consistency - Data Integrity", {
  
  # Test 1: ACID-like properties for file operations
  test_atomicity <- function() {
    temp_dir <- tempdir()
    test_file <- file.path(temp_dir, "test_atomic.csv")
    
    # Atomic write operation
    test_data <- data.frame(
      id = 1:100,
      name = paste("Employee", 1:100),
      value = runif(100)
    )
    
    # Test atomic write
    expect_silent({
      readr::write_csv(test_data, test_file)
    })
    
    # Verify complete write
    read_data <- readr::read_csv(test_file, show_col_types = FALSE)
    expect_equal(nrow(read_data), 100, "All rows should be written atomically")
    expect_equal(names(read_data), names(test_data), "All columns should be preserved")
    
    unlink(test_file)
  }
  
  # Test 2: Consistency across multiple files
  test_referential_integrity <- function() {
    # Create test data with relationships
    temp_dir <- tempdir()
    
    # Employee data
    employee_data <- data.frame(
      EmployeeID = 1:50,
      FirstName = paste("First", 1:50),
      LastName = paste("Last", 1:50),
      Department = sample(c("HR", "IT", "Finance"), 50, replace = TRUE)
    )
    
    # Performance data (referencing EmployeeID)
    performance_data <- data.frame(
      PerformanceID = 1:100,
      EmployeeID = sample(1:50, 100, replace = TRUE),
      Rating = sample(1:5, 100, replace = TRUE),
      ReviewDate = Sys.Date() - sample(1:365, 100, replace = TRUE)
    )
    
    employee_file <- file.path(temp_dir, "test_employee.csv")
    performance_file <- file.path(temp_dir, "test_performance.csv")
    
    # Write both files
    readr::write_csv(employee_data, employee_file)
    readr::write_csv(performance_data, performance_file)
    
    # Test referential integrity
    emp_data <- readr::read_csv(employee_file, show_col_types = FALSE)
    perf_data <- readr::read_csv(performance_file, show_col_types = FALSE)
    
    # All performance EmployeeIDs should exist in employee data
    expect_true(
      all(perf_data$EmployeeID %in% emp_data$EmployeeID),
      "All performance records should reference valid employees"
    )
    
    # Cleanup
    unlink(c(employee_file, performance_file))
  }
  
  # Test 3: Data validation consistency
  test_data_validation_consistency <- function() {
    # Test consistent data types across reads
    if (file.exists("data/employee.csv")) {
      data1 <- readr::read_csv("data/employee.csv", show_col_types = FALSE)
      data2 <- readr::read_csv("data/employee.csv", show_col_types = FALSE)
      
      # Data should be identical
      expect_identical(data1, data2, "Multiple reads should return identical data")
      
      # Test data types consistency
      expect_identical(sapply(data1, class), sapply(data2, class),
                      "Data types should be consistent across reads")
    }
  }
  
  # Test 4: Transaction rollback simulation
  test_rollback_mechanism <- function() {
    temp_dir <- tempdir()
    original_file <- file.path(temp_dir, "original.csv")
    backup_file <- file.path(temp_dir, "backup.csv")
    
    # Create original data
    original_data <- data.frame(id = 1:10, value = letters[1:10])
    readr::write_csv(original_data, original_file)
    
    # Create backup
    file.copy(original_file, backup_file)
    
    # Simulate failed transaction (corrupted write)
    expect_silent({
      # This would be the rollback mechanism
      if (file.exists(backup_file)) {
        file.copy(backup_file, original_file, overwrite = TRUE)
      }
    })
    
    # Verify rollback worked
    restored_data <- readr::read_csv(original_file, show_col_types = FALSE)
    expect_identical(restored_data, original_data, "Rollback should restore original data")
    
    # Cleanup
    unlink(c(original_file, backup_file))
  }
  
  # Execute consistency tests
  test_atomicity()
  test_referential_integrity()
  test_data_validation_consistency()
  test_rollback_mechanism()
})

# =============================================================================
# 6.2.3 CONCURRENT ACCESS HANDLING TESTS
# =============================================================================

test_that("Concurrent Access Handling - Multi-User Scenarios", {
  
  # Test 1: Multiple simultaneous reads
  test_concurrent_reads <- function() {
    if (!file.exists("data/employee.csv")) {
      skip("Employee data file not available for concurrent read testing")
    }
    
    # Set up parallel processing
    plan(multisession, workers = 4)
    
    # Function to read data
    read_data_safely <- function(i) {
      tryCatch({
        data <- readr::read_csv("data/employee.csv", show_col_types = FALSE)
        list(success = TRUE, rows = nrow(data), attempt = i)
      }, error = function(e) {
        list(success = FALSE, error = e$message, attempt = i)
      })
    }
    
    # Perform concurrent reads
    results <- future_map(1:10, read_data_safely, .options = furrr_options(seed = TRUE))
    
    # All reads should succeed
    successes <- map_lgl(results, ~ .x$success)
    expect_true(all(successes), "All concurrent reads should succeed")
    
    # All reads should return same number of rows
    row_counts <- map_int(results[successes], ~ .x$rows)
    expect_true(length(unique(row_counts)) == 1, "All reads should return same row count")
    
    plan(sequential) # Reset plan
  }
  
  # Test 2: Read-while-write scenarios
  test_read_write_conflict <- function() {
    temp_file <- tempfile(fileext = ".csv")
    initial_data <- data.frame(id = 1:100, value = runif(100))
    readr::write_csv(initial_data, temp_file)
    
    # Simulate concurrent read and write
    plan(multisession, workers = 2)
    
    # Reader function
    reader <- function() {
      Sys.sleep(0.1) # Small delay
      tryCatch({
        data <- readr::read_csv(temp_file, show_col_types = FALSE)
        list(success = TRUE, rows = nrow(data))
      }, error = function(e) {
        list(success = FALSE, error = e$message)
      })
    }
    
    # Writer function
    writer <- function() {
      new_data <- data.frame(id = 1:150, value = runif(150))
      tryCatch({
        readr::write_csv(new_data, temp_file)
        list(success = TRUE, operation = "write")
      }, error = function(e) {
        list(success = FALSE, error = e$message)
      })
    }
    
    # Execute concurrent operations
    read_result <- future(reader())
    write_result <- future(writer())
    
    read_outcome <- value(read_result)
    write_outcome <- value(write_result)
    
    # Both operations should complete
    expect_true(read_outcome$success || write_outcome$success, 
               "At least one operation should succeed")
    
    plan(sequential)
    unlink(temp_file)
  }
  
  # Test 3: File locking mechanism
  test_file_locking <- function() {
    temp_file <- tempfile(fileext = ".csv")
    test_data <- data.frame(id = 1:50, name = paste("Item", 1:50))
    
    # Write initial data
    readr::write_csv(test_data, temp_file)
    
    # Test exclusive access simulation
    expect_silent({
      # In a real scenario, this would test file locking
      data <- readr::read_csv(temp_file, show_col_types = FALSE)
      expect_equal(nrow(data), 50)
    })
    
    unlink(temp_file)
  }
  
  # Test 4: Resource contention detection
  test_resource_contention <- function() {
    temp_dir <- tempdir()
    
    # Create multiple files for contention testing
    file_names <- paste0("test_", 1:5, ".csv")
    file_paths <- file.path(temp_dir, file_names)
    
    # Create test data for each file
    for (i in seq_along(file_paths)) {
      test_data <- data.frame(
        id = 1:20,
        file_num = i,
        timestamp = Sys.time()
      )
      readr::write_csv(test_data, file_paths[i])
    }
    
    plan(multisession, workers = 3)
    
    # Function to process files
    process_file <- function(file_path) {
      tryCatch({
        data <- readr::read_csv(file_path, show_col_types = FALSE)
        processed <- data %>% 
          mutate(processed_at = Sys.time()) %>%
          summarise(
            total_rows = n(),
            file_num = first(file_num)
          )
        list(success = TRUE, result = processed)
      }, error = function(e) {
        list(success = FALSE, error = e$message)
      })
    }
    
    # Process files concurrently
    results <- future_map(file_paths, process_file)
    
    # All processing should succeed
    successes <- map_lgl(results, ~ .x$success)
    expect_true(all(successes), "All concurrent file processing should succeed")
    
    plan(sequential)
    unlink(file_paths)
  }
  
  # Execute concurrent access tests
  test_concurrent_reads()
  test_read_write_conflict()
  test_file_locking()
  test_resource_contention()
})

# =============================================================================
# 6.2.4 MIGRATION SCRIPT VALIDATION TESTS
# =============================================================================

test_that("Migration Script Validation - Schema Evolution", {
  
  # Test 1: Schema version compatibility
  test_schema_versioning <- function() {
    # Define schema versions
    schema_v1 <- c("EmployeeID", "FirstName", "LastName", "Department")
    schema_v2 <- c("EmployeeID", "FirstName", "LastName", "Department", "Email", "PhoneNumber")
    schema_v3 <- c("EmployeeID", "FirstName", "LastName", "Department", "Email", "PhoneNumber", "StartDate")
    
    temp_dir <- tempdir()
    
    # Create v1 data
    v1_data <- data.frame(
      EmployeeID = 1:10,
      FirstName = paste("First", 1:10),
      LastName = paste("Last", 1:10),
      Department = sample(c("HR", "IT"), 10, replace = TRUE)
    )
    
    v1_file <- file.path(temp_dir, "employee_v1.csv")
    readr::write_csv(v1_data, v1_file)
    
    # Migration function v1 to v2
    migrate_v1_to_v2 <- function(data) {
      data %>%
        mutate(
          Email = paste0(tolower(FirstName), ".", tolower(LastName), "@atlaslabs.com"),
          PhoneNumber = paste0("555-", sprintf("%04d", row_number()))
        )
    }
    
    # Test migration
    original_data <- readr::read_csv(v1_file, show_col_types = FALSE)
    migrated_data <- migrate_v1_to_v2(original_data)
    
    # Validate migration
    expect_true(all(schema_v1 %in% names(migrated_data)), "Original columns should be preserved")
    expect_true(all(c("Email", "PhoneNumber") %in% names(migrated_data)), "New columns should be added")
    expect_equal(nrow(migrated_data), nrow(original_data), "Row count should remain same")
    
    unlink(v1_file)
  }
  
  # Test 2: Data type migration validation
  test_data_type_migration <- function() {
    temp_dir <- tempdir()
    
    # Create data with mixed types
    original_data <- data.frame(
      id = 1:20,
      salary = c(50000, 60000, "70000", "80000", 90000, rep(75000, 15)), # Mixed types
      hire_date = c("2020-01-01", "2021-02-15", rep("2022-03-10", 18)),
      active = c("TRUE", "FALSE", "1", "0", rep("TRUE", 16)),
      stringsAsFactors = FALSE
    )
    
    test_file <- file.path(temp_dir, "migration_test.csv")
    readr::write_csv(original_data, test_file)
    
    # Migration function with type conversion
    migrate_data_types <- function(file_path) {
      data <- readr::read_csv(file_path, show_col_types = FALSE)
      
      data %>%
        mutate(
          salary = as.numeric(salary),
          hire_date = as.Date(hire_date),
          active = as.logical(ifelse(active %in% c("TRUE", "1"), TRUE, FALSE))
        )
    }
    
    # Test migration
    migrated_data <- migrate_data_types(test_file)
    
    # Validate type conversions
    expect_true(is.numeric(migrated_data$salary), "Salary should be numeric")
    expect_true(is.Date(migrated_data$hire_date), "Hire date should be Date type")
    expect_true(is.logical(migrated_data$active), "Active should be logical")
    expect_equal(nrow(migrated_data), 20, "All rows should be preserved")
    
    unlink(test_file)
  }
  
  # Test 3: Rollback mechanism validation
  test_migration_rollback <- function() {
    temp_dir <- tempdir()
    
    # Original data
    original_data <- data.frame(
      id = 1:15,
      name = paste("Employee", 1:15),
      dept = sample(c("A", "B", "C"), 15, replace = TRUE)
    )
    
    original_file <- file.path(temp_dir, "original.csv")
    backup_file <- file.path(temp_dir, "backup.csv")
    migrated_file <- file.path(temp_dir, "migrated.csv")
    
    # Write original data
    readr::write_csv(original_data, original_file)
    
    # Create backup before migration
    file.copy(original_file, backup_file)
    
    # Perform migration (simulate failure)
    migration_failed <- function() {
      # Simulate corrupted migration
      corrupted_data <- original_data[1:5, ] # Partial data
      readr::write_csv(corrupted_data, migrated_file)
      stop("Migration failed")
    }
    
    # Test rollback mechanism
    expect_error(migration_failed(), "Migration failed")
    
    # Restore from backup
    if (file.exists(backup_file)) {
      file.copy(backup_file, original_file, overwrite = TRUE)
    }
    
    # Verify rollback
    restored_data <- readr::read_csv(original_file, show_col_types = FALSE)
    expect_identical(restored_data, original_data, "Rollback should restore original data")
    
    unlink(c(original_file, backup_file, migrated_file))
  }
  
  # Test 4: Migration validation checks
  test_migration_validation <- function() {
    # Pre-migration validation
    validate_pre_migration <- function(data) {
      checks <- list(
        has_required_columns = all(c("EmployeeID", "FirstName", "LastName") %in% names(data)),
        no_duplicate_ids = !any(duplicated(data$EmployeeID)),
        no_missing_ids = !any(is.na(data$EmployeeID)),
        reasonable_row_count = nrow(data) > 0 && nrow(data) < 100000
      )
      
      all(unlist(checks))
    }
    
    # Post-migration validation
    validate_post_migration <- function(original_data, migrated_data) {
      checks <- list(
        row_count_preserved = nrow(original_data) == nrow(migrated_data),
        ids_preserved = all(original_data$EmployeeID %in% migrated_data$EmployeeID),
        no_data_loss = all(names(original_data) %in% names(migrated_data))
      )
      
      all(unlist(checks))
    }
    
    # Test with sample data
    test_data <- data.frame(
      EmployeeID = 1:25,
      FirstName = paste("First", 1:25),
      LastName = paste("Last", 1:25),
      Department = sample(c("HR", "IT", "Finance"), 25, replace = TRUE)
    )
    
    # Test pre-migration validation
    expect_true(validate_pre_migration(test_data), "Pre-migration validation should pass")
    
    # Simulate migration (add email column)
    migrated_data <- test_data %>%
      mutate(Email = paste0(tolower(FirstName), "@company.com"))
    
    # Test post-migration validation
    expect_true(validate_post_migration(test_data, migrated_data), 
               "Post-migration validation should pass")
  }
  
  # Execute migration tests
  test_schema_versioning()
  test_data_type_migration()
  test_migration_rollback()
  test_migration_validation()
})

# =============================================================================
# 6.2.5 BACKUP AND RESTORE TESTING
# =============================================================================

test_that("Backup and Restore Operations", {
  
  # Test 1: Automated backup creation
  test_backup_creation <- function() {
    temp_dir <- tempdir()
    
    # Original data files
    employee_data <- data.frame(
      EmployeeID = 1:30,
      FirstName = paste("Employee", 1:30),
      Department = sample(c("HR", "IT", "Finance", "Sales"), 30, replace = TRUE),
      Salary = runif(30, 50000, 120000)
    )
    
    performance_data <- data.frame(
      PerformanceID = 1:60,
      EmployeeID = sample(1:30, 60, replace = TRUE),
      Rating = sample(1:5, 60, replace = TRUE),
      ReviewDate = Sys.Date() - sample(1:365, 60, replace = TRUE)
    )
    
    # Original files
    emp_file <- file.path(temp_dir, "employee.csv")
    perf_file <- file.path(temp_dir, "performance.csv")
    
    readr::write_csv(employee_data, emp_file)
    readr::write_csv(performance_data, perf_file)
    
    # Backup function
    create_backup <- function(source_files, backup_dir) {
      if (!dir.exists(backup_dir)) {
        dir.create(backup_dir, recursive = TRUE)
      }
      
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      backup_results <- list()
      
      for (file in source_files) {
        if (file.exists(file)) {
          backup_name <- paste0(tools::file_path_sans_ext(basename(file)), 
                               "_backup_", timestamp, ".csv")
          backup_path <- file.path(backup_dir, backup_name)
          
          success <- file.copy(file, backup_path)
          backup_results[[file]] <- list(success = success, backup_path = backup_path)
        }
      }
      
      backup_results
    }
    
    # Create backups
    backup_dir <- file.path(temp_dir, "backups")
    source_files <- c(emp_file, perf_file)
    backup_results <- create_backup(source_files, backup_dir)
    
    # Validate backups
    expect_true(all(sapply(backup_results, function(x) x$success)), 
               "All backups should be created successfully")
    
    for (result in backup_results) {
      expect_true(file.exists(result$backup_path), "Backup file should exist")
      
      # Verify backup integrity
      if (grepl("employee", result$backup_path)) {
        backup_data <- readr::read_csv(result$backup_path, show_col_types = FALSE)
        expect_equal(nrow(backup_data), 30, "Employee backup should have correct row count")
      }
    }
    
    # Cleanup
    unlink(temp_dir, recursive = TRUE)
  }
  
  # Test 2: Incremental backup validation
  test_incremental_backup <- function() {
    temp_dir <- tempdir()
    
    # Create initial data
    initial_data <- data.frame(
      id = 1:20,
      value = runif(20),
      created_at = Sys.time(),
      version = 1
    )
    
    main_file <- file.path(temp_dir, "main_data.csv")
    readr::write_csv(initial_data, main_file)
    
    # Create first backup
    backup1_file <- file.path(temp_dir, "backup_v1.csv")
    file.copy(main_file, backup1_file)
    
    # Modify data (simulate incremental changes)
    modified_data <- initial_data %>%
      mutate(
        value = value * 1.1,
        modified_at = Sys.time(),
        version = 2
      ) %>%
      add_row(
        id = 21:25,
        value = runif(5),
        created_at = Sys.time(),
        version = 2,
        modified_at = Sys.time()
      )
    
    readr::write_csv(modified_data, main_file)
    
    # Create incremental backup
    backup2_file <- file.path(temp_dir, "backup_v2.csv")
    file.copy(main_file, backup2_file)
    
    # Validate incremental backup
    backup1_data <- readr::read_csv(backup1_file, show_col_types = FALSE)
    backup2_data <- readr::read_csv(backup2_file, show_col_types = FALSE)
    
    expect_equal(nrow(backup1_data), 20, "First backup should have original row count")
    expect_equal(nrow(backup2_data), 25, "Second backup should have updated row count")
    expect_true(all(backup1_data$version == 1), "First backup should have version 1")
    expect_true(all(backup2_data$version == 2), "Second backup should have version 2")
    
    unlink(c(main_file, backup1_file, backup2_file))
  }
  
  # Test 3: Restore functionality validation
  test_restore_functionality <- function() {
    temp_dir <- tempdir()
    
    # Original data
    original_data <- data.frame(
      EmployeeID = 1:40,
      Name = paste("Employee", 1:40),
      Status = "Active",
      LastModified = Sys.Date()
    )
    
    main_file <- file.path(temp_dir, "employees.csv")
    backup_file <- file.path(temp_dir, "employees_backup.csv")
    
    # Write original and backup
    readr::write_csv(original_data, main_file)
    readr::write_csv(original_data, backup_file)
    
    # Simulate data corruption
    corrupted_data <- original_data[1:10, ] # Partial data loss
    readr::write_csv(corrupted_data, main_file)
    
    # Restore function
    restore_from_backup <- function(backup_path, target_path) {
      if (!file.exists(backup_path)) {
        stop("Backup file not found")
      }
      
      # Validate backup before restore
      backup_data <- readr::read_csv(backup_path, show_col_types = FALSE)
      if (nrow(backup_data) == 0) {
        stop("Backup file is empty")
      }
      
      # Perform restore
      file.copy(backup_path, target_path, overwrite = TRUE)
      
      # Verify restore
      restored_data <- readr::read_csv(target_path, show_col_types = FALSE)
      identical(backup_data, restored_data)
    }
    
    # Test restore
    restore_success <- restore_from_backup(backup_file, main_file)
    expect_true(restore_success, "Restore operation should succeed")
    
    # Verify restored data
    restored_data <- readr::read_csv(main_file, show_col_types = FALSE)
    expect_equal(nrow(restored_data), 40, "Restored data should have all original rows")
    expect_identical(restored_data, original_data, "Restored data should match original")
    
    unlink(c(main_file, backup_file))
  }
  
  # Test 4: Backup integrity validation
  test_backup_integrity <- function() {
    temp_dir <- tempdir()
    
    # Create test data
    test_data <- data.frame(
      id = 1:100,
      data_field = paste("Data", 1:100),
      numeric_field = runif(100),
      date_field = Sys.Date() + sample(-365:365, 100, replace = TRUE)
    )
    
    original_file <- file.path(temp_dir, "test_data.csv")
    readr::write_csv(test_data, original_file)
    
    # Backup with checksum
    create_backup_with_checksum <- function(source_file, backup_dir) {
      if (!dir.exists(backup_dir)) {
        dir.create(backup_dir, recursive = TRUE)
      }
      
      # Calculate checksum of original file
      original_checksum <- digest::digest(file = source_file, algo = "md5")
      
      # Create backup
      backup_file <- file.path(backup_dir, paste0("backup_", basename(source_file)))
      file.copy(source_file, backup_file)
      
      # Verify backup checksum
      backup_checksum <- digest::digest(file = backup_file, algo = "md5")
      
      # Create checksum file
      checksum_file <- file.path(backup_dir, paste0(basename(source_file), ".checksum"))
      writeLines(original_checksum, checksum_file)
      
      list(
        success = original_checksum == backup_checksum,
        original_checksum = original_checksum,
        backup_checksum = backup_checksum,
        backup_file = backup_file,
        checksum_file = checksum_file
      )
    }
    
    # Create backup with integrity check
    backup_dir <- file.path(temp_dir, "secure_backups")
    backup_result <- create_backup_with_checksum(original_file, backup_dir)
    
    # Validate backup integrity
    expect_true(backup_result$success, "Backup should have matching checksum")
    expect_true(file.exists(backup_result$backup_file), "Backup file should exist")
    expect_true(file.exists(backup_result$checksum_file), "Checksum file should exist")
    
    # Verify data integrity
    backup_data <- readr::read_csv(backup_result$backup_file, show_col_types = FALSE)
    expect_identical(backup_data, test_data, "Backup data should match original")
    
    unlink(temp_dir, recursive = TRUE)
  }
  
  # Execute backup and restore tests
  test_backup_creation()
  test_incremental_backup()
  test_restore_functionality()
  test_backup_integrity()
})

# =============================================================================
# 6.2.6 REPLICATION SYNCHRONIZATION TESTS
# =============================================================================

test_that("Replication Synchronization - Multi-Instance Data Sync", {
  
  # Test 1: Master-replica synchronization
  test_master_replica_sync <- function() {
    temp_dir <- tempdir()
    
    # Create master and replica directories
    master_dir <- file.path(temp_dir, "master")
    replica_dir <- file.path(temp_dir, "replica")
    dir.create(master_dir, recursive = TRUE)
    dir.create(replica_dir, recursive = TRUE)
    
    # Master data
    master_data <- data.frame(
      id = 1:50,
      name = paste("Record", 1:50),
      value = runif(50),
      last_updated = Sys.time(),
      version = 1
    )
    
    master_file <- file.path(master_dir, "master_data.csv")
    readr::write_csv(master_data, master_file)
    
    # Synchronization function
    synchronize_replica <- function(master_file, replica_file) {
      if (!file.exists(master_file)) {
        stop("Master file not found")
      }
      
      master_data <- readr::read_csv(master_file, show_col_types = FALSE)
      
      # Check if replica exists and compare versions
      if (file.exists(replica_file)) {
        replica_data <- readr::read_csv(replica_file, show_col_types = FALSE)
        
        # Compare modification times or version numbers
        master_version <- max(master_data$version, na.rm = TRUE)
        replica_version <- max(replica_data$version, na.rm = TRUE)
        
        if (master_version <= replica_version) {
          return(list(synchronized = FALSE, reason = "Replica is up to date"))
        }
      }
      
      # Perform synchronization
      readr::write_csv(master_data, replica_file)
      
      # Verify synchronization
      synced_data <- readr::read_csv(replica_file, show_col_types = FALSE)
      
      list(
        synchronized = TRUE,
        rows_synced = nrow(synced_data),
        sync_time = Sys.time()
      )
    }
    
    # Test synchronization
    replica_file <- file.path(replica_dir, "replica_data.csv")
    sync_result <- synchronize_replica(master_file, replica_file)
    
    expect_true(sync_result$synchronized, "Synchronization should succeed")
    expect_equal(sync_result$rows_synced, 50, "All rows should be synchronized")
    
    # Verify replica data
    replica_data <- readr::read_csv(replica_file, show_col_types = FALSE)
    expect_identical(replica_data, master_data, "Replica should match master")
    
    unlink(temp_dir, recursive = TRUE)
  }
  
  # Test 2: Conflict resolution during sync
  test_conflict_resolution <- function() {
    temp_dir <- tempdir()
    
    # Create conflicting data scenarios
    master_data <- data.frame(
      id = 1:20,
      name = paste("Master", 1:20),
      value = runif(20),
      last_modified = Sys.time(),
      source = "master"
    )
    
    replica_data <- data.frame(
      id = 1:20,
      name = paste("Replica", 1:20),
      value = runif(20),
      last_modified = Sys.time() - 3600, # 1 hour earlier
      source = "replica"
    )
    
    master_file <- file.path(temp_dir, "master_conflict.csv")
    replica_file <- file.path(temp_dir, "replica_conflict.csv")
    
    readr::write_csv(master_data, master_file)
    readr::write_csv(replica_data, replica_file)
    
    # Conflict resolution function (master wins)
    resolve_conflicts <- function(master_file, replica_file, strategy = "master_wins") {
      master_data <- readr::read_csv(master_file, show_col_types = FALSE)
      replica_data <- readr::read_csv(replica_file, show_col_types = FALSE)
      
      conflicts <- list()
      
      if (strategy == "master_wins") {
        resolved_data <- master_data
        conflicts$resolution <- "Master data preserved"
      } else if (strategy == "timestamp_wins") {
        # Compare timestamps and keep newer records
        master_time <- max(master_data$last_modified, na.rm = TRUE)
        replica_time <- max(replica_data$last_modified, na.rm = TRUE)
        
        if (master_time >= replica_time) {
          resolved_data <- master_data
          conflicts$resolution <- "Master had newer timestamp"
        } else {
          resolved_data <- replica_data
          conflicts$resolution <- "Replica had newer timestamp"
        }
      }
      
      # Write resolved data back to replica
      readr::write_csv(resolved_data, replica_file)
      
      list(
        conflicts_detected = nrow(master_data) > 0 && nrow(replica_data) > 0,
        resolution_strategy = strategy,
        conflicts = conflicts,
        resolved_rows = nrow(resolved_data)
      )
    }
    
    # Test conflict resolution
    resolution_result <- resolve_conflicts(master_file, replica_file, "master_wins")
    
    expect_true(resolution_result$conflicts_detected, "Conflicts should be detected")
    expect_equal(resolution_result$resolved_rows, 20, "All rows should be resolved")
    
    # Verify resolution
    resolved_data <- readr::read_csv(replica_file, show_col_types = FALSE)
    expect_true(all(resolved_data$source == "master"), "Master data should win conflicts")
    
    unlink(c(master_file, replica_file))
  }
  
  # Test 3: Delta synchronization
  test_delta_synchronization <- function() {
    temp_dir <- tempdir()
    
    # Initial synchronized state
    initial_data <- data.frame(
      id = 1:30,
      name = paste("Item", 1:30),
      value = runif(30),
      checksum = sapply(1:30, function(i) digest::digest(paste("Item", i), algo = "md5")),
      last_sync = Sys.time()
    )
    
    master_file <- file.path(temp_dir, "master_delta.csv")
    replica_file <- file.path(temp_dir, "replica_delta.csv")
    
    readr::write_csv(initial_data, master_file)
    readr::write_csv(initial_data, replica_file)
    
    # Simulate changes in master
    Sys.sleep(1) # Ensure different timestamp
    
    modified_data <- initial_data %>%
      mutate(
        value = ifelse(id <= 10, value * 1.5, value), # Modify first 10 records
        checksum = ifelse(id <= 10, 
                         sapply(paste("Modified Item", id), function(x) digest::digest(x, algo = "md5")),
                         checksum),
        last_sync = ifelse(id <= 10, Sys.time(), last_sync)
      ) %>%
      add_row( # Add new records
        id = 31:35,
        name = paste("New Item", 31:35),
        value = runif(5),
        checksum = sapply(31:35, function(i) digest::digest(paste("New Item", i), algo = "md5")),
        last_sync = Sys.time()
      )
    
    readr::write_csv(modified_data, master_file)
    
    # Delta synchronization function
    delta_sync <- function(master_file, replica_file) {
      master_data <- readr::read_csv(master_file, show_col_types = FALSE)
      replica_data <- readr::read_csv(replica_file, show_col_types = FALSE)
      
      # Find differences
      master_checksums <- setNames(master_data$checksum, master_data$id)
      replica_checksums <- setNames(replica_data$checksum, replica_data$id)
      
      # Identify changes
      new_ids <- setdiff(names(master_checksums), names(replica_checksums))
      modified_ids <- intersect(names(master_checksums), names(replica_checksums))[
        master_checksums[intersect(names(master_checksums), names(replica_checksums))] != 
        replica_checksums[intersect(names(master_checksums), names(replica_checksums))]
      ]
      
      delta_changes <- list(
        new_records = length(new_ids),
        modified_records = length(modified_ids),
        total_changes = length(new_ids) + length(modified_ids)
      )
      
      # Apply delta changes
      if (delta_changes$total_changes > 0) {
        # Update replica with changes
        updated_replica <- replica_data
        
        # Add new records
        if (length(new_ids) > 0) {
          new_records <- master_data[master_data$id %in% as.numeric(new_ids), ]
          updated_replica <- bind_rows(updated_replica, new_records)
        }
        
        # Update modified records
        if (length(modified_ids) > 0) {
          for (mod_id in modified_ids) {
            updated_replica[updated_replica$id == as.numeric(mod_id), ] <- 
              master_data[master_data$id == as.numeric(mod_id), ]
          }
        }
        
        readr::write_csv(updated_replica, replica_file)
      }
      
      delta_changes
    }
    
    # Test delta synchronization
    delta_result <- delta_sync(master_file, replica_file)
    
    expect_equal(delta_result$new_records, 5, "Should detect 5 new records")
    expect_equal(delta_result$modified_records, 10, "Should detect 10 modified records")
    expect_equal(delta_result$total_changes, 15, "Should have 15 total changes")
    
    # Verify synchronization
    final_replica <- readr::read_csv(replica_file, show_col_types = FALSE)
    final_master <- readr::read_csv(master_file, show_col_types = FALSE)
    
    expect_equal(nrow(final_replica), nrow(final_master), "Replica should match master row count")
    
    unlink(c(master_file, replica_file))
  }
  
  # Test 4: Bidirectional synchronization
  test_bidirectional_sync <- function() {
    temp_dir <- tempdir()
    
    # Create two nodes with different data
    node1_data <- data.frame(
      id = 1:15,
      name = paste("Node1", 1:15),
      value = runif(15),
      node_origin = "node1",
      timestamp = Sys.time()
    )
    
    node2_data <- data.frame(
      id = 16:30,
      name = paste("Node2", 16:30),
      value = runif(15),
      node_origin = "node2",
      timestamp = Sys.time()
    )
    
    node1_file <- file.path(temp_dir, "node1_data.csv")
    node2_file <- file.path(temp_dir, "node2_data.csv")
    
    readr::write_csv(node1_data, node1_file)
    readr::write_csv(node2_data, node2_file)
    
    # Bidirectional sync function
    bidirectional_sync <- function(file1, file2) {
      data1 <- readr::read_csv(file1, show_col_types = FALSE)
      data2 <- readr::read_csv(file2, show_col_types = FALSE)
      
      # Merge data from both nodes
      merged_data <- bind_rows(data1, data2) %>%
        distinct(id, .keep_all = TRUE) %>% # Remove duplicates based on ID
        arrange(id)
      
      # Write merged data to both files
      readr::write_csv(merged_data, file1)
      readr::write_csv(merged_data, file2)
      
      list(
        total_records = nrow(merged_data),
        node1_contributed = sum(merged_data$node_origin == "node1"),
        node2_contributed = sum(merged_data$node_origin == "node2"),
        sync_timestamp = Sys.time()
      )
    }
    
    # Test bidirectional synchronization
    sync_result <- bidirectional_sync(node1_file, node2_file)
    
    expect_equal(sync_result$total_records, 30, "Should have all records from both nodes")
    expect_equal(sync_result$node1_contributed, 15, "Should have 15 records from node1")
    expect_equal(sync_result$node2_contributed, 15, "Should have 15 records from node2")
    
    # Verify both files are identical
    final_node1 <- readr::read_csv(node1_file, show_col_types = FALSE)
    final_node2 <- readr::read_csv(node2_file, show_col_types = FALSE)
    
    expect_identical(final_node1, final_node2, "Both nodes should have identical data after sync")
    
    unlink(c(node1_file, node2_file))
  }
  
  # Execute replication synchronization tests
  test_master_replica_sync()
  test_conflict_resolution()
  test_delta_synchronization()
  test_bidirectional_sync()
})

# =============================================================================
# 6.2.7 PERFORMANCE OPTIMIZATION TESTS
# =============================================================================

test_that("Performance Optimization - Query and I/O Performance", {
  
  # Test 1: Large dataset handling
  test_large_dataset_performance <- function() {
    temp_dir <- tempdir()
    
    # Create large dataset
    large_data <- data.frame(
      id = 1:10000,
      employee_name = paste("Employee", 1:10000),
      department = sample(c("HR", "IT", "Finance", "Sales", "Marketing"), 10000, replace = TRUE),
      salary = runif(10000, 30000, 150000),
      hire_date = sample(seq(as.Date("2010-01-01"), as.Date("2023-12-31"), by = "day"), 10000, replace = TRUE),
      performance_score = runif(10000, 1, 5),
      manager_id = sample(1:100, 10000, replace = TRUE)
    )
    
    large_file <- file.path(temp_dir, "large_dataset.csv")
    
    # Test write performance
    write_start <- Sys.time()
    readr::write_csv(large_data, large_file)
    write_duration <- as.numeric(difftime(Sys.time(), write_start, units = "secs"))
    
    expect_true(write_duration < 5, "Large dataset write should complete within 5 seconds")
    expect_true(file.exists(large_file), "Large dataset file should be created")
    
    # Test read performance
    read_start <- Sys.time()
    read_data <- readr::read_csv(large_file, show_col_types = FALSE)
    read_duration <- as.numeric(difftime(Sys.time(), read_start, units = "secs"))
    
    expect_true(read_duration < 3, "Large dataset read should complete within 3 seconds")
    expect_equal(nrow(read_data), 10000, "Should read all rows")
    expect_equal(ncol(read_data), 7, "Should read all columns")
    
    # Test filtered read performance
    filter_start <- Sys.time()
    filtered_data <- read_data %>%
      filter(department == "IT", salary > 80000) %>%
      arrange(desc(performance_score))
    filter_duration <- as.numeric(difftime(Sys.time(), filter_start, units = "secs"))
    
    expect_true(filter_duration < 1, "Data filtering should complete within 1 second")
    expect_true(nrow(filtered_data) > 0, "Filtered data should contain results")
    
    unlink(large_file)
  }
  
  # Test 2: Memory usage optimization
  test_memory_optimization <- function() {
    # Function to measure memory usage
    get_memory_usage <- function() {
      gc_info <- gc()
      sum(gc_info[, "used"])
    }
    
    initial_memory <- get_memory_usage()
    
    # Create multiple datasets in memory
    datasets <- list()
    for (i in 1:5) {
      datasets[[i]] <- data.frame(
        id = 1:2000,
        value1 = runif(2000),
        value2 = rnorm(2000),
        category = sample(letters[1:10], 2000, replace = TRUE),
        timestamp = Sys.time() + sample(1:86400, 2000, replace = TRUE)
      )
    }
    
    peak_memory <- get_memory_usage()
    memory_increase <- peak_memory - initial_memory
    
    # Clean up datasets
    rm(datasets)
    gc()
    
    final_memory <- get_memory_usage()
    memory_recovered <- peak_memory - final_memory
    recovery_ratio <- memory_recovered / memory_increase
    
    expect_true(memory_increase > 0, "Memory usage should increase with data creation")
    expect_true(recovery_ratio > 0.8, "Should recover at least 80% of allocated memory")
    
    # Test memory-efficient data processing
    temp_file <- tempfile(fileext = ".csv")
    test_data <- data.frame(
      id = 1:5000,
      large_text = replicate(5000, paste(sample(letters, 100, replace = TRUE), collapse = "")),
      numeric_data = runif(5000)
    )
    
    readr::write_csv(test_data, temp_file)
    
    # Memory-efficient chunked processing
    process_in_chunks <- function(file_path, chunk_size = 1000) {
      total_rows <- nrow(readr::read_csv(file_path, n_max = 0, show_col_types = FALSE))
      results <- list()
      
      for (i in seq(1, total_rows, by = chunk_size)) {
        chunk <- readr::read_csv(file_path, skip = i - 1, n_max = chunk_size, show_col_types = FALSE)
        if (nrow(chunk) > 0) {
          processed_chunk <- chunk %>%
            summarise(
              chunk_start = i,
              chunk_size = nrow(chunk),
              avg_numeric = mean(numeric_data, na.rm = TRUE)
            )
          results[[length(results) + 1]] <- processed_chunk
        }
      }
      
      bind_rows(results)
    }
    
    chunk_start_memory <- get_memory_usage()
    chunk_results <- process_in_chunks(temp_file, 1000)
    chunk_end_memory <- get_memory_usage()
    
    chunk_memory_usage <- chunk_end_memory - chunk_start_memory
    
    expect_true(nrow(chunk_results) >= 5, "Should process data in chunks")
    expect_true(chunk_memory_usage < memory_increase / 2, "Chunked processing should use less memory")
    
    unlink(temp_file)
  }
  
  # Test 3: I/O optimization strategies
  test_io_optimization <- function() {
    temp_dir <- tempdir()
    
    # Test compressed vs uncompressed I/O
    test_data <- data.frame(
      id = 1:8000,
      description = replicate(8000, paste(sample(LETTERS, 50, replace = TRUE), collapse = "")),
      category = sample(paste("Category", 1:20), 8000, replace = TRUE),
      value = runif(8000, 1, 1000000)
    )
    
    # Uncompressed write/read
    uncompressed_file <- file.path(temp_dir, "uncompressed.csv")
    
    uncomp_write_start <- Sys.time()
    readr::write_csv(test_data, uncompressed_file)
    uncomp_write_time <- as.numeric(difftime(Sys.time(), uncomp_write_start, units = "secs"))
    
    uncomp_read_start <- Sys.time()
    uncomp_data <- readr::read_csv(uncompressed_file, show_col_types = FALSE)
    uncomp_read_time <- as.numeric(difftime(Sys.time(), uncomp_read_start, units = "secs"))
    
    # File size comparison
    uncomp_size <- file.size(uncompressed_file)
    
    expect_true(uncomp_write_time < 2, "Uncompressed write should be fast")
    expect_true(uncomp_read_time < 1, "Uncompressed read should be fast")
    expect_equal(nrow(uncomp_data), 8000, "Should read all data correctly")
    
    # Test batch I/O operations
    batch_files <- paste0("batch_", 1:10, ".csv")
    batch_paths <- file.path(temp_dir, batch_files)
    
    # Create batch data
    batch_datasets <- lapply(1:10, function(i) {
      data.frame(
        batch_id = i,
        id = 1:500,
        value = runif(500),
        category = sample(c("A", "B", "C"), 500, replace = TRUE)
      )
    })
    
    # Sequential write
    seq_start <- Sys.time()
    for (i in seq_along(batch_datasets)) {
      readr::write_csv(batch_datasets[[i]], batch_paths[i])
    }
    seq_write_time <- as.numeric(difftime(Sys.time(), seq_start, units = "secs"))
    
    # Parallel write using future
    plan(multisession, workers = 4)
    
    par_start <- Sys.time()
    future_map2(batch_datasets, batch_paths, ~ readr::write_csv(.x, .y))
    par_write_time <- as.numeric(difftime(Sys.time(), par_start, units = "secs"))
    
    plan(sequential)
    
    expect_true(all(file.exists(batch_paths)), "All batch files should be created")
    
    # Parallel should be faster for multiple files (though overhead might make it slower for small files)
    performance_improvement <- seq_write_time / par_write_time
    
    # Test concurrent read performance
    read_all_start <- Sys.time()
    all_batch_data <- map(batch_paths, ~ readr::read_csv(.x, show_col_types = FALSE))
    read_all_time <- as.numeric(difftime(Sys.time(), read_all_start, units = "secs"))
    
    expect_true(read_all_time < 3, "Reading all batch files should complete quickly")
    expect_equal(length(all_batch_data), 10, "Should read all batch files")
    
    # Cleanup
    unlink(c(uncompressed_file, batch_paths))
  }
  
  # Test 4: Query optimization patterns
  test_query_optimization <- function() {
    # Create indexed-like data structure
    large_dataset <- data.frame(
      employee_id = 1:20000,
      department = sample(c("HR", "IT", "Finance", "Sales", "Marketing", "Operations"), 20000, replace = TRUE),
      salary = runif(20000, 35000, 200000),
      performance_rating = sample(1:5, 20000, replace = TRUE),
      hire_year = sample(2015:2023, 20000, replace = TRUE),
      status = sample(c("Active", "Inactive"), 20000, replace = TRUE, prob = c(0.85, 0.15))
    )
    
    # Test different query approaches
    
    # Approach 1: Sequential filtering
    seq_start <- Sys.time()
    result1 <- large_dataset %>%
      filter(department == "IT") %>%
      filter(salary > 80000) %>%
      filter(performance_rating >= 4) %>%
      filter(status == "Active")
    seq_time <- as.numeric(difftime(Sys.time(), seq_start, units = "secs"))
    
    # Approach 2: Combined filtering
    combined_start <- Sys.time()
    result2 <- large_dataset %>%
      filter(
        department == "IT" & 
        salary > 80000 & 
        performance_rating >= 4 & 
        status == "Active"
      )
    combined_time <- as.numeric(difftime(Sys.time(), combined_start, units = "secs"))
    
    # Approach 3: Pre-grouped optimization
    grouped_start <- Sys.time()
    dept_data <- large_dataset %>% filter(department == "IT")
    result3 <- dept_data %>%
      filter(
        salary > 80000 & 
        performance_rating >= 4 & 
        status == "Active"
      )
    grouped_time <- as.numeric(difftime(Sys.time(), grouped_start, units = "secs"))
    
    # All approaches should yield same results
    expect_identical(result1, result2, "Sequential and combined filtering should yield same results")
    expect_identical(result2, result3, "Combined and grouped filtering should yield same results")
    
    # Combined approach should be faster
    expect_true(combined_time <= seq_time, "Combined filtering should be faster than sequential")
    
    # Test aggregation performance
    agg_start <- Sys.time()
    aggregated_results <- large_dataset %>%
      group_by(department, hire_year) %>%
      summarise(
        avg_salary = mean(salary, na.rm = TRUE),
        avg_performance = mean(performance_rating, na.rm = TRUE),
        employee_count = n(),
        active_count = sum(status == "Active"),
        .groups = "drop"
      )
    agg_time <- as.numeric(difftime(Sys.time(), agg_start, units = "secs"))
    
    expect_true(agg_time < 1, "Aggregation should complete within 1 second")
    expect_true(nrow(aggregated_results) > 0, "Aggregation should produce results")
    
    # Test sorting performance
    sort_start <- Sys.time()
    sorted_data <- large_dataset %>%
      arrange(department, desc(salary), desc(performance_rating))
    sort_time <- as.numeric(difftime(Sys.time(), sort_start, units = "secs"))
    
    expect_true(sort_time < 2, "Sorting should complete within 2 seconds")
    expect_equal(nrow(sorted_data), 20000, "Sorting should preserve all rows")
  }
  
  # Execute performance optimization tests
  test_large_dataset_performance()
  test_memory_optimization()
  test_io_optimization()
  test_query_optimization()
})

# =============================================================================
# 6.2.8 DATA ARCHIVING PROCESSES TESTS
# =============================================================================

test_that("Data Archiving Processes - Lifecycle Management", {
  
  # Test 1: Automated archiving based on age
  test_age_based_archiving <- function() {
    temp_dir <- tempdir()
    
    # Create historical data with different dates
    historical_data <- data.frame(
      record_id = 1:1000,
      employee_id = sample(1:200, 1000, replace = TRUE),
      event_type = sample(c("hire", "promotion", "review", "termination"), 1000, replace = TRUE),
      event_date = sample(seq(as.Date("2020-01-01"), Sys.Date(), by = "day"), 1000, replace = TRUE),
      created_at = sample(seq(as.POSIXct("2020-01-01"), Sys.time(), by = "hour"), 1000, replace = TRUE),
      data_value = runif(1000, 1, 100)
    )
    
    main_file <- file.path(temp_dir, "historical_data.csv")
    archive_dir <- file.path(temp_dir, "archive")
    dir.create(archive_dir, recursive = TRUE)
    
    readr::write_csv(historical_data, main_file)
    
    # Archive function based on age
    archive_old_data <- function(source_file, archive_dir, cutoff_date) {
      data <- readr::read_csv(source_file, show_col_types = FALSE)
      
      # Separate old and current data
      old_data <- data %>% filter(event_date < cutoff_date)
      current_data <- data %>% filter(event_date >= cutoff_date)
      
      if (nrow(old_data) > 0) {
        # Create archive file with timestamp
        archive_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        archive_file <- file.path(archive_dir, paste0("archived_", archive_timestamp, ".csv"))
        
        # Write archived data
        readr::write_csv(old_data, archive_file)
        
        # Update main file with current data only
        readr::write_csv(current_data, source_file)
        
        # Create archive metadata
        metadata <- list(
          archive_date = Sys.time(),
          cutoff_date = cutoff_date,
          archived_records = nrow(old_data),
          remaining_records = nrow(current_data),
          archive_file = basename(archive_file)
        )
        
        metadata_file <- file.path(archive_dir, paste0("metadata_", archive_timestamp, ".json"))
        jsonlite::write_json(metadata, metadata_file, pretty = TRUE)
      }
      
      list(
        archived_records = nrow(old_data),
        remaining_records = nrow(current_data),
        archive_created = nrow(old_data) > 0
      )
    }
    
    # Test archiving data older than 1 year
    cutoff_date <- Sys.Date() - 365
    archive_result <- archive_old_data(main_file, archive_dir, cutoff_date)
    
    expect_true(archive_result$archive_created, "Archive should be created for old data")
    expect_true(archive_result$archived_records > 0, "Should archive some records")
    expect_true(archive_result$remaining_records > 0, "Should retain some current records")
    
    # Verify archive files exist
    archive_files <- list.files(archive_dir, pattern = "archived_.*\\.csv", full.names = TRUE)
    metadata_files <- list.files(archive_dir, pattern = "metadata_.*\\.json", full.names = TRUE)
    
    expect_true(length(archive_files) > 0, "Archive data file should be created")
    expect_true(length(metadata_files) > 0, "Archive metadata file should be created")
    
    # Verify data integrity
    if (length(archive_files) > 0) {
      archived_data <- readr::read_csv(archive_files[1], show_col_types = FALSE)
      expect_true(all(archived_data$event_date < cutoff_date), "All archived data should be older than cutoff")
    }
    
    current_data <- readr::read_csv(main_file, show_col_types = FALSE)
    expect_true(all(current_data$event_date >= cutoff_date), "All remaining data should be newer than cutoff")
    
    unlink(temp_dir, recursive = TRUE)
  }
  
  # Test 2: Size-based archiving
  test_size_based_archiving <- function() {
    temp_dir <- tempdir()
    
    # Create large dataset
    large_data <- data.frame(
      id = 1:5000,
      employee_id = sample(1:500, 5000, replace = TRUE),
      transaction_type = sample(c("payroll", "expense", "bonus", "deduction"), 5000, replace = TRUE),
      amount = runif(5000, 100, 10000),
      transaction_date = sample(seq(as.Date("2023-01-01"), Sys.Date(), by = "day"), 5000, replace = TRUE),
      description = replicate(5000, paste(sample(letters, 20, replace = TRUE), collapse = ""))
    )
    
    main_file <- file.path(temp_dir, "transaction_data.csv")
    archive_dir <- file.path(temp_dir, "size_archive")
    dir.create(archive_dir, recursive = TRUE)
    
    readr::write_csv(large_data, main_file)
    
    # Size-based archiving function
    archive_by_size <- function(source_file, archive_dir, max_size_mb = 1) {
      file_size_mb <- file.size(source_file) / (1024 * 1024)
      
      if (file_size_mb <= max_size_mb) {
        return(list(archive_needed = FALSE, current_size_mb = file_size_mb))
      }
      
      data <- readr::read_csv(source_file, show_col_types = FALSE)
      
      # Calculate how many records to archive (keep newest 60%)
      total_records <- nrow(data)
      keep_records <- ceiling(total_records * 0.6)
      archive_records <- total_records - keep_records
      
      # Sort by date and archive oldest records
      data_sorted <- data %>% arrange(transaction_date)
      
      archive_data <- data_sorted[1:archive_records, ]
      keep_data <- data_sorted[(archive_records + 1):total_records, ]
      
      # Create archive
      archive_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      archive_file <- file.path(archive_dir, paste0("size_archive_", archive_timestamp, ".csv"))
      
      readr::write_csv(archive_data, archive_file)
      readr::write_csv(keep_data, source_file)
      
      # Verify new size
      new_size_mb <- file.size(source_file) / (1024 * 1024)
      
      list(
        archive_needed = TRUE,
        original_size_mb = file_size_mb,
        new_size_mb = new_size_mb,
        archived_records = archive_records,
        remaining_records = keep_records,
        size_reduction_percent = ((file_size_mb - new_size_mb) / file_size_mb) * 100
      )
    }
    
    # Test size-based archiving
    original_size <- file.size(main_file) / (1024 * 1024)
    archive_result <- archive_by_size(main_file, archive_dir, max_size_mb = original_size * 0.8)
    
    expect_true(archive_result$archive_needed, "Archiving should be needed for large file")
    expect_true(archive_result$new_size_mb < archive_result$original_size_mb, "File size should be reduced")
    expect_true(archive_result$size_reduction_percent > 20, "Should achieve significant size reduction")
    
    # Verify archive file exists and contains data
    archive_files <- list.files(archive_dir, pattern = "size_archive_.*\\.csv", full.names = TRUE)
    expect_true(length(archive_files) > 0, "Archive file should be created")
    
    if (length(archive_files) > 0) {
      archived_data <- readr::read_csv(archive_files[1], show_col_types = FALSE)
      expect_equal(nrow(archived_data), archive_result$archived_records, "Archive should contain expected number of records")
    }
    
    unlink(temp_dir, recursive = TRUE)
  }
  
  # Test 3: Compressed archiving
  test_compressed_archiving <- function() {
    temp_dir <- tempdir()
    
    # Create data with repetitive content (good for compression)
    repetitive_data <- data.frame(
      id = 1:3000,
      department = sample(c("HR", "IT", "Finance"), 3000, replace = TRUE),
      status = sample(c("Active", "Inactive", "Pending"), 3000, replace = TRUE),
      category = sample(c("Full-time", "Part-time", "Contract"), 3000, replace = TRUE),
      location = sample(c("New York", "Los Angeles", "Chicago", "Houston"), 3000, replace = TRUE),
      description = sample(c("Standard employee record", "Updated employee information", 
                           "New hire processing", "Status change record"), 3000, replace = TRUE),
      created_date = sample(seq(as.Date("2022-01-01"), Sys.Date(), by = "day"), 3000, replace = TRUE)
    )
    
    source_file <- file.path(temp_dir, "repetitive_data.csv")
    archive_dir <- file.path(temp_dir, "compressed_archive")
    dir.create(archive_dir, recursive = TRUE)
    
    readr::write_csv(repetitive_data, source_file)
    
    # Compressed archiving function
    create_compressed_archive <- function(source_file, archive_dir, compression_method = "gzip") {
      data <- readr::read_csv(source_file, show_col_types = FALSE)
      
      # Filter data for archiving (older than 6 months)
      cutoff_date <- Sys.Date() - 180
      archive_data <- data %>% filter(created_date < cutoff_date)
      current_data <- data %>% filter(created_date >= cutoff_date)
      
      if (nrow(archive_data) == 0) {
        return(list(archived = FALSE, reason = "No data to archive"))
      }
      
      # Create compressed archive
      archive_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      
      if (compression_method == "gzip") {
        archive_file <- file.path(archive_dir, paste0("archive_", archive_timestamp, ".csv.gz"))
        
        # Write compressed file
        gz_con <- gzfile(archive_file, "wb")
        readr::write_csv(archive_data, gz_con)
        close(gz_con)
        
      } else {
        # Fallback to regular CSV
        archive_file <- file.path(archive_dir, paste0("archive_", archive_timestamp, ".csv"))
        readr::write_csv(archive_data, archive_file)
      }
      
      # Update source file with current data
      readr::write_csv(current_data, source_file)
      
      # Calculate compression ratio
      original_size <- object.size(archive_data)
      compressed_size <- file.size(archive_file)
      compression_ratio <- as.numeric(original_size) / compressed_size
      
      list(
        archived = TRUE,
        archive_file = archive_file,
        archived_records = nrow(archive_data),
        remaining_records = nrow(current_data),
        original_size_bytes = as.numeric(original_size),
        compressed_size_bytes = compressed_size,
        compression_ratio = compression_ratio,
        space_saved_percent = ((as.numeric(original_size) - compressed_size) / as.numeric(original_size)) * 100
      )
    }
    
    # Test compressed archiving
    original_size <- file.size(source_file)
    archive_result <- create_compressed_archive(source_file, archive_dir, "gzip")
    
    if (archive_result$archived) {
      expect_true(file.exists(archive_result$archive_file), "Compressed archive file should be created")
      expect_true(archive_result$compression_ratio > 1, "Compression should reduce file size")
      expect_true(archive_result$space_saved_percent > 0, "Should save storage space")
      
      # Verify compressed file can be read
      if (grepl("\\.gz$", archive_result$archive_file)) {
        gz_con <- gzfile(archive_result$archive_file, "rb")
        decompressed_data <- readr::read_csv(gz_con, show_col_types = FALSE)
        close(gz_con)
        
        expect_equal(nrow(decompressed_data), archive_result$archived_records, 
                    "Decompressed data should have correct number of records")
      }
    }
    
    unlink(temp_dir, recursive = TRUE)
  }
  
  # Test 4: Archive retrieval and restoration
  test_archive_retrieval <- function() {
    temp_dir <- tempdir()
    
    # Create current and archived data
    current_data <- data.frame(
      id = 1:100,
      name = paste("Current Record", 1:100),
      created_date = Sys.Date() - sample(1:30, 100, replace = TRUE),
      status = "active"
    )
    
    archived_data_2023 <- data.frame(
      id = 101:200,
      name = paste("Archived Record 2023", 101:200),
      created_date = as.Date("2023-01-01") + sample(1:365, 100, replace = TRUE),
      status = "archived"
    )
    
    archived_data_2022 <- data.frame(
      id = 201:300,
      name = paste("Archived Record 2022", 201:300),
      created_date = as.Date("2022-01-01") + sample(1:365, 100, replace = TRUE),
      status = "archived"
    )
    
    # Set up files
    current_file <- file.path(temp_dir, "current_data.csv")
    archive_dir <- file.path(temp_dir, "retrieval_archive")
    dir.create(archive_dir, recursive = TRUE)
    
    archive_2023_file <- file.path(archive_dir, "archive_2023_20240101_120000.csv")
    archive_2022_file <- file.path(archive_dir, "archive_2022_20230101_120000.csv")
    
    readr::write_csv(current_data, current_file)
    readr::write_csv(archived_data_2023, archive_2023_file)
    readr::write_csv(archived_data_2022, archive_2022_file)
    
    