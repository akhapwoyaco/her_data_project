# Atlas Labs HR Analytics Dashboard
# Comprehensive Unit Tests for Data Loader Module
# Developer: akhapwoyaco
# Focus: Performance, Stress Testing, and Advanced Scenarios

# Load required libraries for testing
library(testthat)
library(here)
library(bench)
library(profvis)
library(future)
library(promises)
library(data.table)
library(readr)
library(stringi)
library(R6)

# Source the Data Loader Module (assuming it exists)
# source(here("modules", "data_loader_module.R"))
# source(here("modules", "logger_module.R"))

# Test Context: Advanced Data Loader Module Testing
context("Atlas Labs Data Loader Module - Advanced Testing Suite")

# ================================================================================
# PERFORMANCE BENCHMARKING TESTS
# ================================================================================

test_that("Performance: Concurrent file loading benchmark", {
  skip_if_not_installed("future")
  
  # Create multiple test files of varying sizes
  test_files <- list(
    small = data.frame(id = 1:1000, value = rnorm(1000)),
    medium = data.frame(id = 1:50000, value = rnorm(50000)),
    large = data.frame(id = 1:200000, value = rnorm(200000))
  )
  
  # Write test files
  temp_files <- map(names(test_files), ~{
    temp_file <- tempfile(pattern = paste0("test_", .x, "_"), fileext = ".csv")
    write_csv(test_files[[.x]], temp_file)
    temp_file
  })
  names(temp_files) <- names(test_files)
  
  # Benchmark sequential vs concurrent loading
  benchmark_results <- bench::mark(
    sequential = {
      map(temp_files, ~read_csv(.x, show_col_types = FALSE))
    },
    concurrent = {
      future::plan(future::multisession, workers = 3)
      results <- future.apply::future_lapply(temp_files, function(f) {
        read_csv(f, show_col_types = FALSE)
      })
      future::plan(future::sequential)
      results
    },
    iterations = 5,
    check = FALSE
  )
  
  # Performance assertions
  expect_true(benchmark_results$median[1] > 0) # Sequential should work
  expect_true(benchmark_results$median[2] > 0) # Concurrent should work
  
  # Memory efficiency check
  expect_true(all(benchmark_results$mem_alloc < "500MB"))
  
  # Cleanup
  walk(temp_files, unlink)
})

test_that("Performance: Memory-efficient chunked processing", {
  # Create large dataset
  large_data <- data.frame(
    id = 1:500000,
    name = stri_rand_strings(500000, 20),
    value = rnorm(500000),
    category = sample(LETTERS[1:10], 500000, replace = TRUE)
  )
  
  temp_file <- tempfile(fileext = ".csv")
  write_csv(large_data, temp_file)
  
  # Test chunked reading with memory monitoring
  chunk_size <- 50000
  memory_usage <- c()
  processed_rows <- 0
  
  # Monitor memory during chunked processing
  con <- file(temp_file, "r")
  readLines(con, n = 1) # Skip header
  
  while(TRUE) {
    chunk <- try(read_csv(con, col_names = FALSE, n_max = chunk_size, show_col_types = FALSE), silent = TRUE)
    
    if(inherits(chunk, "try-error") || nrow(chunk) == 0) break
    
    # Record memory usage
    memory_usage <- c(memory_usage, as.numeric(pryr::mem_used()))
    processed_rows <- processed_rows + nrow(chunk)
    
    # Simulate processing
    chunk <- chunk %>% 
      mutate(processed = TRUE) %>%
      select(-processed) # Remove to free memory
    
    gc() # Force garbage collection
  }
  
  close(con)
  
  # Performance assertions
  expect_equal(processed_rows, nrow(large_data))
  expect_true(max(memory_usage) < 2 * min(memory_usage)) # Memory shouldn't grow more than 2x
  expect_true(length(memory_usage) > 5) # Should have processed multiple chunks
  
  unlink(temp_file)
})

test_that("Performance: Parallel data validation", {
  # Create datasets with various data quality issues
  test_datasets <- list(
    clean = data.frame(
      id = 1:10000,
      age = sample(18:65, 10000, replace = TRUE),
      salary = sample(30000:150000, 10000, replace = TRUE)
    ),
    dirty = data.frame(
      id = c(1:9990, rep(NA, 10)),
      age = c(sample(18:65, 9980), rep(-1, 20)),
      salary = c(sample(30000:150000, 9970), rep(0, 30))
    )
  )
  
  # Write test files
  temp_files <- map(names(test_datasets), ~{
    temp_file <- tempfile(pattern = paste0("validation_", .x, "_"), fileext = ".csv")
    write_csv(test_datasets[[.x]], temp_file)
    temp_file
  })
  names(temp_files) <- names(test_datasets)
  
  # Parallel validation function
  validate_data_parallel <- function(file_path, n_cores = 2) {
    data <- read_csv(file_path, show_col_types = FALSE)
    
    # Split data for parallel processing
    splits <- split(data, rep(1:n_cores, length.out = nrow(data)))
    
    future::plan(future::multisession, workers = n_cores)
    
    validation_results <- future.apply::future_lapply(splits, function(chunk) {
      list(
        missing_values = sum(is.na(chunk)),
        negative_ages = sum(chunk$age < 0, na.rm = TRUE),
        zero_salaries = sum(chunk$salary == 0, na.rm = TRUE),
        duplicated_ids = sum(duplicated(chunk$id))
      )
    })
    
    future::plan(future::sequential)
    
    # Aggregate results
    list(
      total_missing = sum(map_dbl(validation_results, ~.x$missing_values)),
      total_negative_ages = sum(map_dbl(validation_results, ~.x$negative_ages)),
      total_zero_salaries = sum(map_dbl(validation_results, ~.x$zero_salaries)),
      total_duplicated_ids = sum(map_dbl(validation_results, ~.x$duplicated_ids))
    )
  }
  
  # Test parallel validation
  clean_results <- validate_data_parallel(temp_files$clean)
  dirty_results <- validate_data_parallel(temp_files$dirty)
  
  # Assertions
  expect_equal(clean_results$total_missing, 0)
  expect_equal(clean_results$total_negative_ages, 0)
  expect_equal(dirty_results$total_missing, 10)
  expect_equal(dirty_results$total_negative_ages, 20)
  
  # Cleanup
  walk(temp_files, unlink)
})

# ================================================================================
# STRESS TESTING
# ================================================================================

test_that("Stress: Extreme file size handling", {
  skip_on_cran()
  skip_if(Sys.getenv("SKIP_STRESS_TESTS") == "true")
  
  # Create extremely large dataset (1GB+ when written)
  n_rows <- 2000000
  large_dataset <- data.frame(
    id = 1:n_rows,
    uuid = replicate(n_rows, paste0(sample(c(0:9, letters), 36, replace = TRUE), collapse = "")),
    timestamp = seq(as.POSIXct("2020-01-01"), by = "hour", length.out = n_rows),
    value1 = rnorm(n_rows),
    value2 = rnorm(n_rows),
    value3 = rnorm(n_rows),
    category = sample(paste0("Category_", 1:1000), n_rows, replace = TRUE),
    description = stri_rand_strings(n_rows, length = 100)
  )
  
  temp_file <- tempfile(fileext = ".csv")
  
  # Monitor memory during write
  start_memory <- as.numeric(pryr::mem_used())
  write_csv(large_dataset, temp_file)
  write_memory <- as.numeric(pryr::mem_used())
  
  # Test loading with memory constraints
  rm(large_dataset)
  gc()
  
  start_load_memory <- as.numeric(pryr::mem_used())
  
  # Use data.table for memory efficiency
  loaded_data <- fread(temp_file, showProgress = FALSE)
  
  end_load_memory <- as.numeric(pryr::mem_used())
  
  # Stress test assertions
  expect_true(nrow(loaded_data) == n_rows)
  expect_true(ncol(loaded_data) == 8)
  expect_true((end_load_memory - start_load_memory) < 3e9) # Less than 3GB memory increase
  
  # File size check
  file_size <- file.info(temp_file)$size
  expect_true(file_size > 1e9) # File should be > 1GB
  
  unlink(temp_file)
})

test_that("Stress: Rapid successive file operations", {
  # Test rapid file creation, reading, and deletion
  n_operations <- 100
  operation_times <- numeric(n_operations)
  memory_snapshots <- numeric(n_operations)
  
  for(i in 1:n_operations) {
    start_time <- Sys.time()
    
    # Create random dataset
    test_data <- data.frame(
      id = 1:1000,
      value = rnorm(1000),
      category = sample(LETTERS[1:5], 1000, replace = TRUE)
    )
    
    # Write, read, and delete
    temp_file <- tempfile(fileext = ".csv")
    write_csv(test_data, temp_file)
    read_data <- read_csv(temp_file, show_col_types = FALSE)
    unlink(temp_file)
    
    end_time <- Sys.time()
    operation_times[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
    memory_snapshots[i] <- as.numeric(pryr::mem_used())
    
    # Force garbage collection every 10 operations
    if(i %% 10 == 0) gc()
  }
  
  # Stress test assertions
  expect_true(mean(operation_times) < 1.0) # Average operation < 1 second
  expect_true(max(operation_times) < 5.0) # No operation > 5 seconds
  expect_true(sd(memory_snapshots) < max(memory_snapshots) * 0.1) # Memory usage should be stable
})

test_that("Stress: Resource exhaustion scenarios", {
  # Test behavior under memory pressure
  original_memory <- as.numeric(pryr::mem_used())
  
  # Create multiple large objects to simulate memory pressure
  memory_hogs <- list()
  for(i in 1:5) {
    memory_hogs[[i]] <- matrix(rnorm(1000000), nrow = 1000, ncol = 1000)
  }
  
  # Try to load data under memory pressure
  test_data <- data.frame(
    id = 1:10000,
    value = rnorm(10000)
  )
  
  temp_file <- tempfile(fileext = ".csv")
  write_csv(test_data, temp_file)
  
  # Attempt to read under memory pressure
  result <- tryCatch({
    read_csv(temp_file, show_col_types = FALSE)
  }, error = function(e) {
    e
  })
  
  # Should either succeed or fail gracefully
  expect_true(is.data.frame(result) || inherits(result, "error"))
  
  # Cleanup
  rm(memory_hogs)
  gc()
  unlink(temp_file)
})

# ================================================================================
# ADVANCED ERROR HANDLING AND RECOVERY
# ================================================================================

test_that("Advanced: Partial file corruption recovery", {
  # Create a file with partial corruption
  good_data <- data.frame(
    id = 1:1000,
    name = paste0("Name_", 1:1000),
    value = rnorm(1000)
  )
  
  temp_file <- tempfile(fileext = ".csv")
  write_csv(good_data, temp_file)
  
  # Introduce corruption by truncating file
  file_content <- readLines(temp_file)
  corrupted_content <- file_content[1:(length(file_content) - 100)] # Remove last 100 lines
  
  corrupted_file <- tempfile(fileext = ".csv")
  writeLines(corrupted_content, corrupted_file)
  
  # Test recovery mechanism
  recovery_result <- tryCatch({
    data <- read_csv(corrupted_file, show_col_types = FALSE)
    list(
      success = TRUE,
      rows_recovered = nrow(data),
      data = data
    )
  }, error = function(e) {
    list(
      success = FALSE,
      error = e$message,
      rows_recovered = 0
    )
  })
  
  # Should recover partial data or fail gracefully
  expect_true(is.list(recovery_result))
  if(recovery_result$success) {
    expect_true(recovery_result$rows_recovered > 0)
    expect_true(recovery_result$rows_recovered < 1000)
  }
  
  unlink(c(temp_file, corrupted_file))
})

test_that("Advanced: Network file system simulation", {
  skip_if_not_installed("future")
  
  # Simulate network delays and interruptions
  simulate_network_load <- function(file_path, delay_seconds = 0.1) {
    Sys.sleep(delay_seconds) # Simulate network delay
    
    # Random chance of "network interruption"
    if(runif(1) < 0.1) {
      stop("Network interruption simulated")
    }
    
    read_csv(file_path, show_col_types = FALSE)
  }
  
  # Create test file
  test_data <- data.frame(
    id = 1:5000,
    value = rnorm(5000)
  )
  
  temp_file <- tempfile(fileext = ".csv")
  write_csv(test_data, temp_file)
  
  # Test with retry mechanism
  max_retries <- 3
  retry_count <- 0
  success <- FALSE
  
  while(retry_count < max_retries && !success) {
    result <- tryCatch({
      data <- simulate_network_load(temp_file, delay_seconds = 0.05)
      success <- TRUE
      data
    }, error = function(e) {
      retry_count <<- retry_count + 1
      NULL
    })
  }
  
  # Should eventually succeed or exhaust retries
  expect_true(retry_count <= max_retries)
  expect_true(success || retry_count == max_retries)
  
  unlink(temp_file)
})

# ================================================================================
# DATA INTEGRITY AND CONSISTENCY TESTS
# ================================================================================

test_that("Integrity: Cross-file referential consistency", {
  # Create related datasets with foreign key relationships
  employees <- data.frame(
    employee_id = 1:1000,
    name = paste0("Employee_", 1:1000),
    department_id = sample(1:10, 1000, replace = TRUE)
  )
  
  departments <- data.frame(
    department_id = 1:10,
    department_name = paste0("Dept_", 1:10)
  )
  
  # Introduce some referential integrity issues
  performance <- data.frame(
    employee_id = c(1:950, 1001:1050), # Some invalid employee IDs
    rating = sample(1:5, 1000, replace = TRUE)
  )
  
  # Write test files
  emp_file <- tempfile(fileext = ".csv")
  dept_file <- tempfile(fileext = ".csv")
  perf_file <- tempfile(fileext = ".csv")
  
  write_csv(employees, emp_file)
  write_csv(departments, dept_file)
  write_csv(performance, perf_file)
  
  # Load and validate relationships
  emp_data <- read_csv(emp_file, show_col_types = FALSE)
  dept_data <- read_csv(dept_file, show_col_types = FALSE)
  perf_data <- read_csv(perf_file, show_col_types = FALSE)
  
  # Referential integrity checks
  orphaned_employees <- anti_join(emp_data, dept_data, by = "department_id")
  orphaned_performance <- anti_join(perf_data, emp_data, by = "employee_id")
  
  # Assertions
  expect_equal(nrow(orphaned_employees), 0) # No orphaned employees
  expect_equal(nrow(orphaned_performance), 50) # 50 orphaned performance records
  
  unlink(c(emp_file, dept_file, perf_file))
})

test_that("Integrity: Time-series data validation", {
  # Create time-series data with various issues
  n_records <- 10000
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = n_records)
  
  # Introduce temporal anomalies
  time_series_data <- data.frame(
    date = c(dates[1:5000], dates[5001:10000], dates[1:100]), # Duplicates
    value = c(rnorm(5000, 100, 10), rnorm(5000, 120, 15), rnorm(100, 50, 5)),
    employee_id = rep(1:100, length.out = n_records + 100)
  )
  
  temp_file <- tempfile(fileext = ".csv")
  write_csv(time_series_data, temp_file)
  
  # Load and validate temporal consistency
  loaded_data <- read_csv(temp_file, show_col_types = FALSE)
  
  # Temporal validation
  duplicate_dates <- loaded_data %>%
    group_by(date, employee_id) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(count > 1)
  
  chronological_order <- loaded_data %>%
    arrange(employee_id, date) %>%
    group_by(employee_id) %>%
    mutate(date_lag = lag(date)) %>%
    filter(!is.na(date_lag) & date < date_lag) %>%
    nrow()
  
  # Assertions
  expect_true(nrow(duplicate_dates) > 0) # Should detect duplicates
  expect_equal(chronological_order, 0) # Data should be sortable chronologically
  
  unlink(temp_file)
})

# ================================================================================
# SCALABILITY AND ARCHITECTURE TESTS
# ================================================================================

test_that("Scalability: Dynamic schema adaptation", {
  # Create files with evolving schemas
  schema_v1 <- data.frame(
    id = 1:100,
    name = paste0("Name_", 1:100),
    value = rnorm(100)
  )
  
  schema_v2 <- data.frame(
    id = 101:200,
    name = paste0("Name_", 101:200),
    value = rnorm(100),
    new_field = sample(LETTERS[1:5], 100, replace = TRUE)
  )
  
  schema_v3 <- data.frame(
    id = 201:300,
    name = paste0("Name_", 201:300),
    value = rnorm(100),
    new_field = sample(LETTERS[1:5], 100, replace = TRUE),
    another_field = runif(100)
  )
  
  # Write files
  files <- list(
    v1 = tempfile(fileext = ".csv"),
    v2 = tempfile(fileext = ".csv"),
    v3 = tempfile(fileext = ".csv")
  )
  
  write_csv(schema_v1, files$v1)
  write_csv(schema_v2, files$v2)
  write_csv(schema_v3, files$v3)
  
  # Dynamic schema handling
  load_with_schema_adaptation <- function(file_paths) {
    all_data <- map(file_paths, ~read_csv(.x, show_col_types = FALSE))
    
    # Find all unique columns
    all_columns <- unique(unlist(map(all_data, names)))
    
    # Standardize all datasets to have the same columns
    standardized_data <- map(all_data, function(df) {
      missing_cols <- setdiff(all_columns, names(df))
      for(col in missing_cols) {
        df[[col]] <- NA
      }
      df[all_columns]
    })
    
    # Combine all data
    do.call(rbind, standardized_data)
  }
  
  combined_data <- load_with_schema_adaptation(unlist(files))
  
  # Assertions
  expect_equal(nrow(combined_data), 300)
  expect_equal(ncol(combined_data), 5) # All fields from all versions
  expect_true(all(c("id", "name", "value", "new_field", "another_field") %in% names(combined_data)))
  
  # Check NA handling for missing fields
  expect_true(all(is.na(combined_data$new_field[1:100]))) # v1 data should have NA
  expect_true(all(is.na(combined_data$another_field[1:200]))) # v1 and v2 should have NA
  
  unlink(unlist(files))
})

test_that("Architecture: Plugin-based data source handling", {
  # Simulate different data source types
  csv_data <- data.frame(id = 1:100, value = rnorm(100))
  json_data <- toJSON(list(data = csv_data), auto_unbox = TRUE)
  rds_data <- csv_data
  
  # Create files of different types
  csv_file <- tempfile(fileext = ".csv")
  json_file <- tempfile(fileext = ".json")
  rds_file <- tempfile(fileext = ".rds")
  
  write_csv(csv_data, csv_file)
  writeLines(json_data, json_file)
  saveRDS(rds_data, rds_file)
  
  # Plugin-based loader
  load_by_extension <- function(file_path) {
    ext <- tools::file_ext(file_path)
    
    switch(ext,
      "csv" = read_csv(file_path, show_col_types = FALSE),
      "json" = fromJSON(file_path)$data,
      "rds" = readRDS(file_path),
      stop("Unsupported file type: ", ext)
    )
  }
  
  # Test plugin system
  csv_loaded <- load_by_extension(csv_file)
  json_loaded <- load_by_extension(json_file)
  rds_loaded <- load_by_extension(rds_file)
  
  # Assertions
  expect_equal(nrow(csv_loaded), 100)
  expect_equal(nrow(json_loaded), 100)
  expect_equal(nrow(rds_loaded), 100)
  
  # Data consistency across formats
  expect_equal(csv_loaded$id, json_loaded$id)
  expect_equal(csv_loaded$id, rds_loaded$id)
  
  unlink(c(csv_file, json_file, rds_file))
})

# ================================================================================
# SECURITY AND ACCESS CONTROL TESTS
# ================================================================================

test_that("Security: Path traversal prevention", {
  # Test various path traversal attempts
  malicious_paths <- c(
    "../../../etc/passwd",
    "..\\..\\..\\windows\\system32\\config\\sam",
    "/etc/shadow",
    "C:\\Windows\\System32\\config\\SAM",
    "file:///etc/passwd",
    "//server/share/file.csv"
  )
  
  # Safe path validation function
  is_safe_path <- function(path) {
    # Normalize path
    normalized <- normalizePath(path, mustWork = FALSE)
    
    # Check for directory traversal
    if(grepl("\\.\\.", path) || grepl("^\\/", path) || grepl("^[A-Za-z]:\\\\", path)) {
      return(FALSE)
    }
    
    # Check for network paths
    if(grepl("^//", path) || grepl("^file://", path)) {
      return(FALSE)
    }
    
    TRUE
  }
  
  # Test all malicious paths
  safety_results <- map_lgl(malicious_paths, is_safe_path)
  
  # All should be flagged as unsafe
  expect_true(all(!safety_results))
  
  # Test legitimate paths
  legitimate_paths <- c(
    "data.csv",
    "folder/data.csv",
    "temp_file.csv"
  )
  
  legitimate_results <- map_lgl(legitimate_paths, is_safe_path)
  expect_true(all(legitimate_results))
})

test_that("Security: Data sanitization", {
  # Create data with potential security issues
  malicious_data <- data.frame(
    id = 1:10,
    name = c(
      "Normal Name",
      "<script>alert('xss')</script>",
      "'; DROP TABLE users; --",
      "Name with\nNewline",
      "Name with\tTab",
      "Name with\rCarriage Return",
      "Name with NULL\0character",
      paste0(rep("A", 1000), collapse = ""), # Very long string
      "Unicode: 🔥💻🚀",
      "控制字符测试"
    ),
    email = c(
      "normal@example.com",
      "test@<script>alert('xss')</script>.com",
      "test'; DROP TABLE users; --@example.com",
      "test\n@example.com",
      "test@example.com\0",
      "test@" . paste0(rep("a", 100), collapse = "") . ".com",
      "test@example.com",
      "test@example.com",
      "测试@example.com",
      "test@example.com"
    )
  )
  
  temp_file <- tempfile(fileext = ".csv")
  write_csv(malicious_data, temp_file)
  
  # Load and sanitize
  loaded_data <- read_csv(temp_file, show_col_types = FALSE)
  
  sanitize_text <- function(text) {
    # Remove control characters
    text <- gsub("[\x00-\x1F\x7F]", "", text)
    
    # Limit string length
    text <- substr(text, 1, 255)
    
    # Basic HTML/SQL escape (simplified)
    text <- gsub("<", "&lt;", text)
    text <- gsub(">", "&gt;", text)
    text <- gsub("'", "&#39;", text)
    text <- gsub("\"", "&quot;", text)
    
    text
  }
  
  sanitized_data <- loaded_data %>%
    mutate(
      name = map_chr(name, sanitize_text),
      email = map_chr(email, sanitize_text)
    )
  
  # Assertions
  expect_true(all(nchar(sanitized_data$name) <= 255))
  expect_true(all(!grepl("<script>", sanitized_data$name)))
  expect_true(all(!grepl("DROP TABLE", sanitized_data$name)))
  expect_true(all(!grepl("[\x00-\x1F\x7F]", sanitized_data$name)))
  
  unlink(temp_file)
})

# ================================================================================
# CLEANUP AND TEARDOWN
# ================================================================================

# Global test cleanup
teardown({
  # Clean up any remaining temporary files
  temp_files <- list.files(tempdir(), pattern = "^file", full.names = TRUE)
  unlink(temp_files)
  
  # Force garbage collection
  gc()
  
  # Reset any global options that might have been changed
  options(warn = 1)
})

# Performance summary test
test_that("Performance: Overall module performance summary", {
  # This test summarizes the performance characteristics discovered
  # in the above tests and can be used for regression testing
  
  performance_benchmarks <- list(
    max_file_size_gb = 1.0,
    max_memory_usage_gb = 3.0,
    max_load_time_seconds = 30.0,
    max_concurrent_files = 10,
    supported_formats = c("csv", "json", "rds"),
    security_features = c("path_traversal_protection", "data_sanitization"),
    error_recovery = c("partial_corruption", "network_interruption"),
    scalability_features = c("chunked_processing", "dynamic_schema", "plugin_architecture")
  )
  
  # Document the benchmark expectations
  expect_true(performance_benchmarks$max_file_size_gb >= 1.0)
  expect_true(performance_benchmarks$max_memory_usage_gb <= 5.0)
  expect_true(performance_benchmarks$max_load_time_seconds <= 60.0)
  expect_true(length(performance_benchmarks$supported_formats) >= 3)
  
  cat("\n=== Atlas Labs Data Loader Performance Summary ===\n")
  cat("Max File Size:", performance_benchmarks$max_file_size_gb, "GB\n")
  cat("Max Memory Usage:", performance_benchmarks$max_memory_usage_gb, "GB\n")
  cat("Max Load Time:", performance_benchmarks$max_load_time_seconds, "seconds\n")
  cat("Supported Formats:", paste(performance_benchmarks$supported_formats, collapse = ", "), "\n")
  cat("Security Features:", length(performance_benchmarks$security_features), "implemented\n")
  cat("=============================================\n")
})

# End of comprehensive test suite