# =============================================================================
# Atlas Labs HR Analytics Dashboard
# Data Loader Module - Comprehensive Unit Tests
# Developer: akhapwoyaco
# =============================================================================

# Load required libraries for testing
library(testthat)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(fs)
library(withr)

# Source the data loader module (assuming it exists)
# source("modules/data_loader_module.R")

# =============================================================================
# TEST SETUP AND UTILITIES
# =============================================================================

# Create test data directory
setup_test_environment <- function() {
  test_dir <- tempdir()
  test_data_dir <- file.path(test_dir, "test_data")
  dir.create(test_data_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Store original working directory
  original_wd <- getwd()
  
  list(
    test_dir = test_dir,
    test_data_dir = test_data_dir,
    original_wd = original_wd
  )
}

# Clean up test environment
cleanup_test_environment <- function(test_env) {
  unlink(test_env$test_data_dir, recursive = TRUE, force = TRUE)
  setwd(test_env$original_wd)
  gc() # Force garbage collection
}

# Create sample valid CSV data
create_sample_employee_data <- function() {
  data.frame(
    EmployeeID = 1:100,
    FirstName = paste0("Employee", 1:100),
    LastName = paste0("Surname", 1:100),
    Gender = sample(c("Male", "Female", "Non-binary"), 100, replace = TRUE),
    Age = sample(22:65, 100, replace = TRUE),
    BusinessTravel = sample(c("Travel_Rarely", "Travel_Frequently", "Non-Travel"), 100, replace = TRUE),
    Department = sample(c("Sales", "Research & Development", "Human Resources"), 100, replace = TRUE),
    DistanceFromHome = sample(1:50, 100, replace = TRUE),
    State = sample(c("California", "Texas", "New York", "Florida"), 100, replace = TRUE),
    Ethnicity = sample(c("White", "Black", "Hispanic", "Asian", "Other"), 100, replace = TRUE),
    Education = sample(1:5, 100, replace = TRUE),
    EducationField = sample(c("Life Sciences", "Medical", "Marketing", "Technical Degree"), 100, replace = TRUE),
    JobRole = sample(c("Sales Executive", "Research Scientist", "Laboratory Technician"), 100, replace = TRUE),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), 100, replace = TRUE),
    Salary = sample(30000:150000, 100, replace = TRUE),
    StockOptionLevel = sample(0:3, 100, replace = TRUE),
    OverTime = sample(c("Yes", "No"), 100, replace = TRUE),
    HireDate = sample(seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = "day"), 100),
    Attrition = sample(c("Yes", "No"), 100, replace = TRUE),
    YearsAtCompany = sample(1:20, 100, replace = TRUE),
    YearsInMostRecentRole = sample(1:15, 100, replace = TRUE),
    YearsSinceLastPromotion = sample(0:10, 100, replace = TRUE),
    YearsWithCurrManager = sample(1:10, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Create sample performance rating data
create_sample_performance_data <- function() {
  data.frame(
    PerformanceID = 1:200,
    EmployeeID = sample(1:100, 200, replace = TRUE),
    ReviewDate = sample(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"), 200),
    EnvironmentSatisfaction = sample(1:5, 200, replace = TRUE),
    JobSatisfaction = sample(1:5, 200, replace = TRUE),
    RelationshipSatisfaction = sample(1:5, 200, replace = TRUE),
    WorkLifeBalance = sample(1:5, 200, replace = TRUE),
    SelfRating = sample(1:5, 200, replace = TRUE),
    ManagerRating = sample(1:5, 200, replace = TRUE),
    TrainingOpportunitiesWithinYear = sample(0:10, 200, replace = TRUE),
    TrainingOpportunitiesTaken = sample(0:8, 200, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Create sample education level data
create_sample_education_data <- function() {
  data.frame(
    `Education Level ID` = 1:5,
    `Education Level` = c("Below College", "College", "Bachelor", "Master", "Doctor"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# =============================================================================
# MOCK DATA LOADER MODULE FUNCTIONS
# =============================================================================

# Mock implementation of the data loader module for testing
AtlasDataLoader <- R6::R6Class("AtlasDataLoader",
  public = list(
    initialize = function() {
      private$logger <- list(
        log_info = function(msg, module = "DataLoader") cat("[INFO]", module, ":", msg, "\n"),
        log_warning = function(msg, module = "DataLoader") cat("[WARNING]", module, ":", msg, "\n"),
        log_error = function(msg, module = "DataLoader") cat("[ERROR]", module, ":", msg, "\n")
      )
    },
    
    load_csv_file = function(file_path, expected_columns = NULL) {
      tryCatch({
        # Validate file exists
        if (!file.exists(file_path)) {
          stop("File does not exist: ", file_path)
        }
        
        # Check file permissions
        if (!file.access(file_path, 4) == 0) {
          stop("File is not readable: ", file_path)
        }
        
        # Check file size
        file_size <- file.size(file_path)
        if (file_size > 100 * 1024 * 1024) { # 100MB
          private$logger$log_warning("Large file detected, processing may take time")
        }
        
        # Check if file is empty
        if (file_size == 0) {
          stop("File is empty: ", file_path)
        }
        
        # Detect file encoding
        encoding <- private$detect_encoding(file_path)
        private$logger$log_info(paste("Detected encoding:", encoding))
        
        # Read CSV with proper encoding
        data <- readr::read_csv(
          file_path,
          locale = readr::locale(encoding = encoding),
          show_col_types = FALSE,
          lazy = FALSE
        )
        
        # Validate columns if expected columns provided
        if (!is.null(expected_columns)) {
          private$validate_columns(data, expected_columns)
        }
        
        # Convert data types
        data <- private$convert_data_types(data)
        
        # Validate data integrity
        private$validate_data_integrity(data)
        
        private$logger$log_info(paste("Successfully loaded", nrow(data), "rows from", basename(file_path)))
        
        return(data)
      }, error = function(e) {
        private$logger$log_error(paste("Failed to load file:", e$message))
        stop(e)
      })
    },
    
    validate_file_format = function(file_path) {
      if (!tools::file_ext(file_path) %in% c("csv", "CSV")) {
        stop("Invalid file format. Only CSV files are supported.")
      }
      
      # Additional validation for CSV structure
      tryCatch({
        # Read first few lines to check CSV structure
        first_lines <- readLines(file_path, n = 5, warn = FALSE)
        if (length(first_lines) == 0) {
          stop("File appears to be empty or corrupted")
        }
        
        # Check for consistent delimiter
        comma_counts <- sapply(first_lines, function(x) stringr::str_count(x, ","))
        if (length(unique(comma_counts)) > 2) { # Allow some variation for headers
          stop("Inconsistent CSV delimiter structure detected")
        }
        
        return(TRUE)
      }, error = function(e) {
        stop("File appears to be corrupted or not a valid CSV: ", e$message)
      })
    },
    
    get_memory_usage = function() {
      gc_info <- gc()
      total_memory <- sum(gc_info[, "used"]) * 1024 * 1024 # Convert to bytes
      return(total_memory)
    },
    
    cleanup_memory = function() {
      gc(verbose = FALSE)
      private$logger$log_info("Memory cleanup completed")
    }
  ),
  
  private = list(
    logger = NULL,
    
    detect_encoding = function(file_path) {
      # Simple encoding detection
      tryCatch({
        # Try UTF-8 first
        test_read <- readr::read_lines(file_path, n_max = 10, locale = readr::locale(encoding = "UTF-8"))
        return("UTF-8")
      }, error = function(e) {
        # Fallback to system default
        return("latin1")
      })
    },
    
    validate_columns = function(data, expected_columns) {
      actual_columns <- names(data)
      missing_columns <- setdiff(expected_columns, actual_columns)
      extra_columns <- setdiff(actual_columns, expected_columns)
      
      if (length(missing_columns) > 0) {
        stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
      }
      
      if (length(extra_columns) > 0) {
        private$logger$log_warning(paste("Extra columns found:", paste(extra_columns, collapse = ", ")))
      }
    },
    
    convert_data_types = function(data) {
      # Convert date columns
      date_columns <- c("HireDate", "ReviewDate")
      for (col in date_columns) {
        if (col %in% names(data)) {
          data[[col]] <- as.Date(data[[col]])
        }
      }
      
      # Convert numeric columns
      numeric_columns <- c("Age", "DistanceFromHome", "Salary", "YearsAtCompany", 
                          "EnvironmentSatisfaction", "JobSatisfaction")
      for (col in numeric_columns) {
        if (col %in% names(data)) {
          data[[col]] <- as.numeric(data[[col]])
        }
      }
      
      # Convert factor columns
      factor_columns <- c("Gender", "Department", "Attrition", "OverTime")
      for (col in factor_columns) {
        if (col %in% names(data)) {
          data[[col]] <- as.factor(data[[col]])
        }
      }
      
      return(data)
    },
    
    validate_data_integrity = function(data) {
      # Check for completely empty rows
      empty_rows <- rowSums(is.na(data)) == ncol(data)
      if (any(empty_rows)) {
        private$logger$log_warning(paste("Found", sum(empty_rows), "completely empty rows"))
      }
      
      # Check for duplicate employee IDs
      if ("EmployeeID" %in% names(data)) {
        duplicates <- sum(duplicated(data$EmployeeID))
        if (duplicates > 0) {
          private$logger$log_warning(paste("Found", duplicates, "duplicate Employee IDs"))
        }
      }
    }
  )
)

# =============================================================================
# UNIT TESTS
# =============================================================================

test_that("Data Loader Module - Valid CSV File Loading and Parsing", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  # Create valid CSV file
  sample_data <- create_sample_employee_data()
  csv_file <- file.path(test_env$test_data_dir, "valid_employee.csv")
  write_csv(sample_data, csv_file)
  
  # Test loading
  loader <- AtlasDataLoader$new()
  result <- loader$load_csv_file(csv_file)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 100)
  expect_true(all(c("EmployeeID", "FirstName", "LastName") %in% names(result)))
  expect_true(is.numeric(result$Age))
  expect_true(is.factor(result$Gender))
})

test_that("Data Loader Module - Invalid File Format Rejection", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  # Create non-CSV files
  txt_file <- file.path(test_env$test_data_dir, "invalid.txt")
  xlsx_file <- file.path(test_env$test_data_dir, "invalid.xlsx")
  json_file <- file.path(test_env$test_data_dir, "invalid.json")
  
  writeLines("This is not a CSV", txt_file)
  writeLines("This is not a CSV", xlsx_file)
  writeLines('{"key": "value"}', json_file)
  
  loader <- AtlasDataLoader$new()
  
  expect_error(loader$validate_file_format(txt_file), "Invalid file format")
  expect_error(loader$validate_file_format(xlsx_file), "Invalid file format")
  expect_error(loader$validate_file_format(json_file), "Invalid file format")
})

test_that("Data Loader Module - Missing File Error Handling", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  loader <- AtlasDataLoader$new()
  non_existent_file <- file.path(test_env$test_data_dir, "does_not_exist.csv")
  
  expect_error(loader$load_csv_file(non_existent_file), "File does not exist")
})

test_that("Data Loader Module - Large File Processing (>100MB)", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  skip_if(Sys.getenv("ATLAS_SKIP_LARGE_FILE_TEST") == "TRUE", "Large file test skipped")
  
  # Create a large dataset (simulate >100MB)
  large_data <- do.call(rbind, replicate(5000, create_sample_employee_data(), simplify = FALSE))
  large_csv_file <- file.path(test_env$test_data_dir, "large_employee.csv")
  write_csv(large_data, large_csv_file)
  
  # Check if file is actually large enough
  if (file.size(large_csv_file) > 50 * 1024 * 1024) { # At least 50MB
    loader <- AtlasDataLoader$new()
    
    # Capture warnings
    expect_warning(
      result <- loader$load_csv_file(large_csv_file),
      "Large file detected"
    )
    
    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 100000)
  } else {
    skip("Could not create sufficiently large test file")
  }
})

test_that("Data Loader Module - Corrupted File Detection", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  # Create corrupted CSV files
  corrupted_file1 <- file.path(test_env$test_data_dir, "corrupted1.csv")
  corrupted_file2 <- file.path(test_env$test_data_dir, "corrupted2.csv")
  corrupted_file3 <- file.path(test_env$test_data_dir, "corrupted3.csv")
  
  # File with inconsistent delimiters
  writeLines(c(
    "col1,col2,col3",
    "val1,val2,val3",
    "val1;val2;val3",  # Different delimiter
    "val1,val2,val3,val4,val5"  # Too many columns
  ), corrupted_file1)
  
  # File with binary data
  writeBin(as.raw(c(0x00, 0x01, 0x02, 0xFF, 0xFE)), corrupted_file2)
  
  # File with incomplete lines
  writeLines(c(
    "col1,col2,col3",
    "val1,val2",  # Missing column
    "val1,val2,val3",
    "val1,val2"   # Missing column again
  ), corrupted_file3)
  
  loader <- AtlasDataLoader$new()
  
  expect_error(loader$validate_file_format(corrupted_file1), "corrupted|Invalid")
  expect_error(loader$load_csv_file(corrupted_file2), "corrupted|parsing")
  # The third case might not always fail depending on readr's tolerance
})

test_that("Data Loader Module - Empty File Handling", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  # Create empty file
  empty_file <- file.path(test_env$test_data_dir, "empty.csv")
  file.create(empty_file)
  
  loader <- AtlasDataLoader$new()
  
  expect_error(loader$load_csv_file(empty_file), "File is empty")
})

test_that("Data Loader Module - Special Characters in Data", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  # Create data with special characters
  special_data <- data.frame(
    EmployeeID = 1:5,
    FirstName = c("José", "François", "Müller", "O'Connor", "Smith & Jones"),
    LastName = c("García", "Dubois", "Schmidt", "MacDonald", "Johnson-Brown"),
    Department = c("R&D", "Sales & Marketing", "HR/Admin", "IT/Support", "Finance"),
    Notes = c("Café worker", "Naïve approach", "£100 bonus", "50% increase", "€500 budget"),
    stringsAsFactors = FALSE
  )
  
  special_csv_file <- file.path(test_env$test_data_dir, "special_chars.csv")
  write_csv(special_data, special_csv_file)
  
  loader <- AtlasDataLoader$new()
  result <- loader$load_csv_file(special_csv_file)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_true("José" %in% result$FirstName)
  expect_true("García" %in% result$LastName)
  expect_true(any(grepl("&", result$Department)))
})

test_that("Data Loader Module - Unicode/Encoding Issues", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  # Create data with various Unicode characters
  unicode_data <- data.frame(
    EmployeeID = 1:4,
    FirstName = c("王", "田中", "Αλέξανδρος", "Владимир"),
    LastName = c("小明", "太郎", "Παπαδόπουλος", "Иванов"),
    Department = c("研发部", "営業部", "Διοίκηση", "Отдел продаж"),
    City = c("北京", "東京", "Αθήνα", "Москва"),
    stringsAsFactors = FALSE
  )
  
  unicode_csv_file <- file.path(test_env$test_data_dir, "unicode_data.csv")
  
  # Write with UTF-8 encoding
  write_csv(unicode_data, unicode_csv_file)
  
  loader <- AtlasDataLoader$new()
  result <- loader$load_csv_file(unicode_csv_file)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
  # Check if Unicode characters are preserved
  expect_true(any(grepl("王", result$FirstName, fixed = TRUE)))
  expect_true(any(grepl("太郎", result$LastName, fixed = TRUE)))
})

test_that("Data Loader Module - Column Name Validation", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  # Create CSV with expected columns
  valid_data <- create_sample_employee_data()
  valid_csv_file <- file.path(test_env$test_data_dir, "valid_columns.csv")
  write_csv(valid_data, valid_csv_file)
  
  # Create CSV with missing columns
  invalid_data <- valid_data[, 1:10]  # Remove some columns
  invalid_csv_file <- file.path(test_env$test_data_dir, "invalid_columns.csv")
  write_csv(invalid_data, invalid_csv_file)
  
  loader <- AtlasDataLoader$new()
  expected_columns <- c("EmployeeID", "FirstName", "LastName", "Gender", "Age", "Salary", "Attrition")
  
  # Test valid columns
  expect_silent(loader$load_csv_file(valid_csv_file, expected_columns))
  
  # Test missing columns
  expect_error(
    loader$load_csv_file(invalid_csv_file, expected_columns),
    "Missing required columns"
  )
})

test_that("Data Loader Module - Data Type Conversion Accuracy", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  # Create test data with mixed types
  mixed_data <- data.frame(
    EmployeeID = c("1", "2", "3"),
    Age = c("25", "30", "35"),
    Salary = c("50000", "60000", "70000"),
    HireDate = c("2020-01-01", "2021-06-15", "2022-12-31"),
    Gender = c("Male", "Female", "Male"),
    Attrition = c("Yes", "No", "No"),
    EnvironmentSatisfaction = c("4", "3", "5"),
    stringsAsFactors = FALSE
  )
  
  mixed_csv_file <- file.path(test_env$test_data_dir, "mixed_types.csv")
  write_csv(mixed_data, mixed_csv_file)
  
  loader <- AtlasDataLoader$new()
  result <- loader$load_csv_file(mixed_csv_file)
  
  # Check data type conversions
  expect_true(is.numeric(result$Age))
  expect_true(is.numeric(result$Salary))
  expect_true(is.Date(result$HireDate))
  expect_true(is.factor(result$Gender))
  expect_true(is.factor(result$Attrition))
  expect_true(is.numeric(result$EnvironmentSatisfaction))
  
  # Check values are preserved
  expect_equal(result$Age, c(25, 30, 35))
  expect_equal(result$Salary, c(50000, 60000, 70000))
  expect_equal(as.character(result$HireDate), c("2020-01-01", "2021-06-15", "2022-12-31"))
})

test_that("Data Loader Module - Memory Cleanup After Loading", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  # Create moderately large dataset
  large_data <- do.call(rbind, replicate(1000, create_sample_employee_data(), simplify = FALSE))
  large_csv_file <- file.path(test_env$test_data_dir, "memory_test.csv")
  write_csv(large_data, large_csv_file)
  
  loader <- AtlasDataLoader$new()
  
  # Get initial memory usage
  initial_memory <- loader$get_memory_usage()
  
  # Load data
  result <- loader$load_csv_file(large_csv_file)
  
  # Get memory after loading
  after_load_memory <- loader$get_memory_usage()
  
  # Clean up memory
  loader$cleanup_memory()
  
  # Get memory after cleanup
  after_cleanup_memory <- loader$get_memory_usage()
  
  # Verify data was loaded
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 50000)
  
  # Memory should increase after loading
  expect_gte(after_load_memory, initial_memory)
  
  # Memory should decrease after cleanup (though not always significantly)
  expect_lte(after_cleanup_memory, after_load_memory)
})

test_that("Data Loader Module - File Permission Validation", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  skip_on_os("windows")  # File permissions work differently on Windows
  
  # Create a file and remove read permissions
  restricted_file <- file.path(test_env$test_data_dir, "restricted.csv")
  sample_data <- create_sample_employee_data()
  write_csv(sample_data, restricted_file)
  
  # Remove read permissions
  Sys.chmod(restricted_file, mode = "000")
  
  loader <- AtlasDataLoader$new()
  
  expect_error(
    loader$load_csv_file(restricted_file),
    "not readable|permission"
  )
  
  # Restore permissions for cleanup
  Sys.chmod(restricted_file, mode = "644")
})

test_that("Data Loader Module - Performance Benchmarking", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  # Create test data of different sizes
  small_data <- create_sample_employee_data()
  medium_data <- do.call(rbind, replicate(10, small_data, simplify = FALSE))
  
  small_csv <- file.path(test_env$test_data_dir, "small.csv")
  medium_csv <- file.path(test_env$test_data_dir, "medium.csv")
  
  write_csv(small_data, small_csv)
  write_csv(medium_data, medium_csv)
  
  loader <- AtlasDataLoader$new()
  
  # Benchmark small file
  small_time <- system.time({
    small_result <- loader$load_csv_file(small_csv)
  })
  
  # Benchmark medium file
  medium_time <- system.time({
    medium_result <- loader$load_csv_file(medium_csv)
  })
  
  # Verify results
  expect_equal(nrow(small_result), 100)
  expect_equal(nrow(medium_result), 1000)
  
  # Medium file should take longer but not excessively so
  expect_gt(medium_time[["elapsed"]], small_time[["elapsed"]])
  expect_lt(medium_time[["elapsed"]], small_time[["elapsed"]] * 20)  # Not more than 20x slower
})

test_that("Data Loader Module - Error Recovery and Graceful Degradation", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  # Create partially corrupted CSV
  partial_data <- c(
    "EmployeeID,FirstName,LastName,Age,Salary",
    "1,John,Doe,25,50000",
    "2,Jane,Smith,invalid_age,60000",  # Invalid age
    "3,Bob,Johnson,35,invalid_salary", # Invalid salary
    "4,Alice,Brown,30,70000"
  )
  
  partial_csv <- file.path(test_env$test_data_dir, "partial_corruption.csv")
  writeLines(partial_data, partial_csv)
  
  loader <- AtlasDataLoader$new()
  
  # Should load but with warnings/issues handled
  result <- loader$load_csv_file(partial_csv)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)  # All rows should be present
  
  # Check that invalid values are handled (converted to NA)
  expect_true(is.na(result$Age[2]))  # Invalid age should be NA
  expect_true(is.na(result$Salary[3]))  # Invalid salary should be NA
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("Data Loader Module - Full Integration Test with All HR Files", {
  test_env <- setup_test_environment()
  on.exit(cleanup_test_environment(test_env))
  
  # Create all three HR data files
  employee_data <- create_sample_employee_data()
  performance_data <- create_sample_performance_data()
  education_data <- create_sample_education_data()
  
  employee_csv <- file.path(test_env$test_data_dir, "employee.csv")
  performance_csv <- file.path(test_env$test_data_dir, "performance_rating.csv")
  education_csv <- file.path(test_env$test_data_dir, "education_level.csv")
  
  write_csv(employee_data, employee_csv)
  write_csv(performance_data, performance_csv)
  write_csv(education_data, education_csv)
  
  loader <- AtlasDataLoader$new()
  
  # Load all files
  emp_result <- loader$load_csv_file(employee_csv)
  perf_result <- loader$load_csv_file(performance_csv)
  edu_result <- loader$load_csv_file(education_csv)
  
  # Verify all files loaded correctly
  expect_s3_class(emp_result, "data.frame")
  expect_s3_class(perf_result, "data.frame")
  expect_s3_class(edu_result, "data.frame")
  
  expect_equal(nrow(emp_result), 100)
  expect_equal(nrow(perf_result), 200)
  expect_equal(nrow(edu_result), 5)
  
  # Test data relationships
  expect_true(all(perf_result$EmployeeID %in% emp_result$EmployeeID))
  expect_true(all(emp_result$Education %in% edu_result$`Education Level ID`))
})

# =============================================================================
# PERFORMANCE AND STRESS TESTS
# =============================================================================

test_that("Data Loader Module - Stress Test with Multiple Concurrent Loads", {
  test_env <- setup_test_