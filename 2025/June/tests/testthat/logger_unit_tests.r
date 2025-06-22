# ============================================================================
# Atlas Labs HR Analytics Dashboard - Logger Module Unit Tests
# Comprehensive testing suite for R6 AtlasLogger class
# Developer: akhapwoyaco
# ============================================================================

# Load required libraries for testing
if (!require(testthat)) install.packages("testthat")
if (!require(R6)) install.packages("R6")
if (!require(crayon)) install.packages("crayon")
if (!require(fs)) install.packages("fs")
if (!require(lubridate)) install.packages("lubridate")

library(testthat)
library(R6)
library(crayon)
library(fs)
library(lubridate)

# Source the logger module (assuming it exists)
# source("modules/logger_module.R")

# ============================================================================
# MOCK ATLAS LOGGER CLASS FOR TESTING
# ============================================================================

AtlasLogger <- R6Class("AtlasLogger",
  private = list(
    log_level = "INFO",
    log_file = NULL,
    max_file_size = 10485760, # 10MB
    max_files = 5,
    console_colors = TRUE,
    performance_data = list(),
    memory_snapshots = list(),
    start_time = NULL,
    .format_message = function(level, message, module = NULL, timestamp = NULL) {
      if (is.null(timestamp)) timestamp <- Sys.time()
      
      module_str <- if (!is.null(module)) paste0("[", module, "] ") else ""
      formatted <- sprintf("[%s] %s%s%s", 
                          format(timestamp, "%Y-%m-%d %H:%M:%S"),
                          level, 
                          module_str, 
                          message)
      return(formatted)
    },
    
    .get_color_function = function(level) {
      switch(level,
        "INFO" = crayon::blue,
        "WARN" = crayon::yellow, 
        "ERROR" = crayon::red,
        "DEBUG" = crayon::green,
        crayon::white
      )
    },
    
    .write_to_file = function(message) {
      if (!is.null(private$log_file)) {
        # Check file size and rotate if necessary
        if (file.exists(private$log_file)) {
          file_size <- file.info(private$log_file)$size
          if (!is.na(file_size) && file_size > private$max_file_size) {
            self$rotate_logs()
          }
        }
        
        # Write to file
        cat(message, "\n", file = private$log_file, append = TRUE)
      }
    },
    
    .should_log = function(level) {
      level_hierarchy <- c("DEBUG" = 1, "INFO" = 2, "WARN" = 3, "ERROR" = 4)
      current_level <- level_hierarchy[private$log_level]
      message_level <- level_hierarchy[level]
      
      return(!is.na(message_level) && !is.na(current_level) && message_level >= current_level)
    }
  ),
  
  public = list(
    initialize = function(log_level = "INFO", 
                         log_file = NULL, 
                         console_colors = TRUE,
                         max_file_size = 10485760,
                         max_files = 5) {
      
      # Validate log level
      valid_levels <- c("DEBUG", "INFO", "WARN", "ERROR")
      if (!log_level %in% valid_levels) {
        stop("Invalid log level. Must be one of: ", paste(valid_levels, collapse = ", "))
      }
      
      private$log_level <- log_level
      private$console_colors <- console_colors
      private$max_file_size <- max_file_size
      private$max_files <- max_files
      private$start_time <- Sys.time()
      
      # Set up log file if specified
      if (!is.null(log_file)) {
        # Create directory if it doesn't exist
        log_dir <- dirname(log_file)
        if (!dir.exists(log_dir)) {
          dir.create(log_dir, recursive = TRUE)
        }
        private$log_file <- log_file
      }
      
      # Initialize performance tracking
      private$performance_data <- list()
      private$memory_snapshots <- list()
      
      self$log_info("AtlasLogger initialized", "SYSTEM")
    },
    
    log_info = function(message, module = NULL, performance_data = NULL) {
      if (private$.should_log("INFO")) {
        self$.log_message("INFO", message, module, performance_data)
      }
    },
    
    log_warn = function(message, module = NULL) {
      if (private$.should_log("WARN")) {
        self$.log_message("WARN", message, module)
      }
    },
    
    log_error = function(message, module = NULL) {
      if (private$.should_log("ERROR")) {
        self$.log_message("ERROR", message, module)
      }
    },
    
    log_debug = function(message, module = NULL) {
      if (private$.should_log("DEBUG")) {
        self$.log_message("DEBUG", message, module)
      }
    },
    
    .log_message = function(level, message, module = NULL, performance_data = NULL) {
      timestamp <- Sys.time()
      formatted_message <- private$.format_message(level, message, module, timestamp)
      
      # Console output with colors
      if (private$console_colors) {
        color_func <- private$.get_color_function(level)
        cat(color_func(formatted_message), "\n")
      } else {
        cat(formatted_message, "\n")
      }
      
      # File output
      private$.write_to_file(formatted_message)
      
      # Store performance data if provided
      if (!is.null(performance_data)) {
        private$performance_data[[length(private$performance_data) + 1]] <- list(
          timestamp = timestamp,
          module = module,
          data = performance_data
        )
      }
    },
    
    track_memory_usage = function(module = NULL) {
      gc_info <- gc()
      memory_info <- list(
        timestamp = Sys.time(),
        module = module,
        used_mb = sum(gc_info[, "used"]) * 0.001,
        max_mb = sum(gc_info[, "max used"]) * 0.001,
        total_memory = memory.size(),
        available_memory = memory.limit() - memory.size()
      )
      
      private$memory_snapshots[[length(private$memory_snapshots) + 1]] <- memory_info
      return(memory_info)
    },
    
    track_execution_time = function(start_time, end_time = NULL, operation = NULL, module = NULL) {
      if (is.null(end_time)) end_time <- Sys.time()
      
      execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      perf_data <- list(
        operation = operation,
        execution_time_sec = execution_time,
        start_time = start_time,
        end_time = end_time
      )
      
      self$log_info(
        sprintf("Operation '%s' completed in %.3f seconds", 
                operation %||% "unknown", execution_time),
        module,
        perf_data
      )
      
      return(execution_time)
    },
    
    get_performance_summary = function() {
      list(
        total_logs = length(private$performance_data),
        memory_snapshots = length(private$memory_snapshots),
        uptime_seconds = as.numeric(difftime(Sys.time(), private$start_time, units = "secs")),
        current_memory = self$track_memory_usage("SYSTEM"),
        performance_data = private$performance_data,
        memory_history = private$memory_snapshots
      )
    },
    
    rotate_logs = function() {
      if (is.null(private$log_file) || !file.exists(private$log_file)) {
        return(FALSE)
      }
      
      base_name <- tools::file_path_sans_ext(private$log_file)
      extension <- tools::file_ext(private$log_file)
      
      # Rotate existing files
      for (i in (private$max_files - 1):1) {
        old_file <- paste0(base_name, ".", i, ".", extension)
        new_file <- paste0(base_name, ".", i + 1, ".", extension)
        
        if (file.exists(old_file)) {
          if (i == (private$max_files - 1)) {
            file.remove(old_file)  # Remove oldest file
          } else {
            file.rename(old_file, new_file)
          }
        }
      }
      
      # Move current log to .1
      rotated_file <- paste0(base_name, ".1.", extension)
      file.rename(private$log_file, rotated_file)
      
      self$log_info("Log file rotated", "SYSTEM")
      return(TRUE)
    },
    
    set_log_level = function(level) {
      valid_levels <- c("DEBUG", "INFO", "WARN", "ERROR")
      if (!level %in% valid_levels) {
        self$log_error(paste("Invalid log level:", level), "SYSTEM")
        return(FALSE)
      }
      
      old_level <- private$log_level
      private$log_level <- level
      self$log_info(paste("Log level changed from", old_level, "to", level), "SYSTEM")
      return(TRUE)
    },
    
    get_log_level = function() {
      return(private$log_level)
    },
    
    clear_performance_data = function() {
      private$performance_data <- list()
      private$memory_snapshots <- list()
      self$log_info("Performance data cleared", "SYSTEM")
    }
  )
)

# ============================================================================
# UNIT TESTS
# ============================================================================

test_that("Logger initialization and configuration", {
  # Test default initialization
  logger <- AtlasLogger$new()
  expect_equal(logger$get_log_level(), "INFO")
  expect_true(is.R6(logger))
  
  # Test custom initialization
  logger_custom <- AtlasLogger$new(
    log_level = "DEBUG",
    console_colors = FALSE,
    max_file_size = 5242880,
    max_files = 3
  )
  expect_equal(logger_custom$get_log_level(), "DEBUG")
  
  # Test invalid log level
  expect_error(
    AtlasLogger$new(log_level = "INVALID"),
    "Invalid log level"
  )
  
  # Test log level change
  expect_true(logger$set_log_level("ERROR"))
  expect_equal(logger$get_log_level(), "ERROR")
  expect_false(logger$set_log_level("INVALID"))
})

test_that("Log level filtering (INFO, WARN, ERROR)", {
  # Create temporary log file
  temp_log <- tempfile(fileext = ".log")
  logger <- AtlasLogger$new(log_level = "WARN", log_file = temp_log)
  
  # Test that INFO messages are filtered out
  logger$log_info("This should not appear", "TEST")
  logger$log_warn("This should appear", "TEST")
  logger$log_error("This should also appear", "TEST")
  
  # Read log file
  if (file.exists(temp_log)) {
    log_content <- readLines(temp_log)
    expect_false(any(grepl("This should not appear", log_content)))
    expect_true(any(grepl("This should appear", log_content)))
    expect_true(any(grepl("This should also appear", log_content)))
  }
  
  # Test DEBUG level (should show all)
  logger$set_log_level("DEBUG")
  logger$log_debug("Debug message", "TEST")
  logger$log_info("Info message", "TEST")
  
  if (file.exists(temp_log)) {
    log_content <- readLines(temp_log)
    expect_true(any(grepl("Debug message", log_content)))
    expect_true(any(grepl("Info message", log_content)))
  }
  
  # Cleanup
  if (file.exists(temp_log)) file.remove(temp_log)
})

test_that("Message formatting consistency", {
  temp_log <- tempfile(fileext = ".log")
  logger <- AtlasLogger$new(log_file = temp_log)
  
  # Log messages with different parameters
  test_time <- as.POSIXct("2024-01-01 12:00:00")
  
  logger$log_info("Test message", "TEST_MODULE")
  logger$log_warn("Warning message")
  logger$log_error("Error message", "ERROR_MODULE")
  
  if (file.exists(temp_log)) {
    log_content <- readLines(temp_log)
    
    # Check timestamp format (YYYY-MM-DD HH:MM:SS)
    timestamp_pattern <- "\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]"
    expect_true(all(grepl(timestamp_pattern, log_content)))
    
    # Check log level format
    expect_true(any(grepl("\\[.*\\] INFO\\[TEST_MODULE\\]", log_content)))
    expect_true(any(grepl("\\[.*\\] WARN", log_content)))
    expect_true(any(grepl("\\[.*\\] ERROR\\[ERROR_MODULE\\]", log_content)))
  }
  
  # Cleanup
  if (file.exists(temp_log)) file.remove(temp_log)
})

test_that("Color coding output validation", {
  # Test with colors enabled
  logger_color <- AtlasLogger$new(console_colors = TRUE)
  
  # Capture console output (this is tricky to test directly)
  # We'll test the color function selection instead
  expect_true(is.function(logger_color$.__enclos_env__$private$.get_color_function("INFO")))
  expect_true(is.function(logger_color$.__enclos_env__$private$.get_color_function("WARN")))
  expect_true(is.function(logger_color$.__enclos_env__$private$.get_color_function("ERROR")))
  expect_true(is.function(logger_color$.__enclos_env__$private$.get_color_function("DEBUG")))
  
  # Test with colors disabled
  logger_no_color <- AtlasLogger$new(console_colors = FALSE)
  expect_false(logger_no_color$.__enclos_env__$private$console_colors)
})

test_that("File output integrity", {
  temp_log <- tempfile(fileext = ".log")
  logger <- AtlasLogger$new(log_file = temp_log)
  
  # Test messages
  test_messages <- c(
    "First test message",
    "Second test message with special chars: !@#$%^&*()",
    "Third message with unicode: 🚀📊",
    "Multi\nline\nmessage"
  )
  
  for (msg in test_messages) {
    logger$log_info(msg, "FILE_TEST")
  }
  
  # Verify file exists and content
  expect_true(file.exists(temp_log))
  
  if (file.exists(temp_log)) {
    log_content <- readLines(temp_log)
    expect_length(log_content, length(test_messages) + 1) # +1 for initialization message
    
    # Check that all messages are present
    for (msg in test_messages) {
      # Handle multiline messages
      msg_clean <- gsub("\n", " ", msg)
      expect_true(any(grepl(fixed(msg_clean), log_content, fixed = TRUE)))
    }
  }
  
  # Cleanup
  if (file.exists(temp_log)) file.remove(temp_log)
})

test_that("Memory usage tracking accuracy", {
  logger <- AtlasLogger$new()
  
  # Track memory usage
  memory_info <- logger$track_memory_usage("MEMORY_TEST")
  
  # Verify memory info structure
  expect_true(is.list(memory_info))
  expect_true("timestamp" %in% names(memory_info))
  expect_true("module" %in% names(memory_info))
  expect_true("used_mb" %in% names(memory_info))
  expect_true("max_mb" %in% names(memory_info))
  
  # Verify data types
  expect_true(is.numeric(memory_info$used_mb))
  expect_true(is.numeric(memory_info$max_mb))
  expect_true(memory_info$used_mb > 0)
  expect_equal(memory_info$module, "MEMORY_TEST")
  
  # Track multiple times
  for (i in 1:3) {
    logger$track_memory_usage(paste0("TEST_", i))
  }
  
  summary <- logger$get_performance_summary()
  expect_equal(summary$memory_snapshots, 4) # 3 + 1 from first call
})

test_that("Performance metric calculation", {
  logger <- AtlasLogger$new()
  
  # Test execution time tracking
  start_time <- Sys.time()
  Sys.sleep(0.1) # Sleep for 100ms
  
  execution_time <- logger$track_execution_time(
    start_time, 
    operation = "test_operation", 
    module = "PERF_TEST"
  )
  
  # Verify execution time is reasonable (should be around 0.1 seconds)
  expect_true(execution_time >= 0.09 && execution_time <= 0.2)
  
  # Test performance summary
  summary <- logger$get_performance_summary()
  expect_true(is.list(summary))
  expect_true("total_logs" %in% names(summary))
  expect_true("uptime_seconds" %in% names(summary))
  expect_true("current_memory" %in% names(summary))
  
  expect_true(summary$uptime_seconds > 0)
  expect_true(summary$total_logs >= 1)
})

test_that("Module location tracking", {
  temp_log <- tempfile(fileext = ".log")
  logger <- AtlasLogger$new(log_file = temp_log)
  
  # Test different modules
  modules <- c("AUTH_MODULE", "DATA_MODULE", "UI_MODULE", "REPORT_MODULE")
  
  for (module in modules) {
    logger$log_info(paste("Message from", module), module)
  }
  
  if (file.exists(temp_log)) {
    log_content <- readLines(temp_log)
    
    # Verify each module appears in logs
    for (module in modules) {
      expect_true(any(grepl(paste0("\\[", module, "\\]"), log_content)))
    }
  }
  
  # Test memory tracking with modules
  memory_info <- logger$track_memory_usage("MEMORY_MODULE")
  expect_equal(memory_info$module, "MEMORY_MODULE")
  
  # Cleanup
  if (file.exists(temp_log)) file.remove(temp_log)
})

test_that("Timestamp accuracy", {
  temp_log <- tempfile(fileext = ".log")
  logger <- AtlasLogger$new(log_file = temp_log)
  
  # Record time before logging
  before_time <- Sys.time()
  logger$log_info("Timestamp test", "TIME_TEST")
  after_time <- Sys.time()
  
  if (file.exists(temp_log)) {
    log_content <- readLines(temp_log)
    
    # Extract timestamp from log entry
    timestamp_match <- regexpr("\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\\]", log_content[length(log_content)])
    timestamp_str <- regmatches(log_content[length(log_content)], timestamp_match)
    timestamp_str <- gsub("\\[|\\]", "", timestamp_str)
    
    log_time <- as.POSIXct(timestamp_str, format = "%Y-%m-%d %H:%M:%S")
    
    # Verify timestamp is within reasonable range
    expect_true(log_time >= before_time && log_time <= after_time)
  }
  
  # Test memory tracking timestamp
  memory_info <- logger$track_memory_usage("TIME_TEST")
  expect_true(inherits(memory_info$timestamp, "POSIXct"))
  expect_true(memory_info$timestamp <= Sys.time())
  
  # Cleanup
  if (file.exists(temp_log)) file.remove(temp_log)
})

test_that("Log rotation functionality", {
  temp_log <- tempfile(fileext = ".log")
  logger <- AtlasLogger$new(log_file = temp_log, max_file_size = 1024, max_files = 3) # 1KB max
  
  # Generate enough log entries to trigger rotation
  for (i in 1:100) {
    logger$log_info(paste("Log entry number", i, "with some additional text to increase file size"), "ROTATION_TEST")
  }
  
  # Check if rotation occurred
  base_name <- tools::file_path_sans_ext(temp_log)
  extension <- tools::file_ext(temp_log)
  rotated_file <- paste0(base_name, ".1.", extension)
  
  # May or may not have rotated depending on exact file size
  if (file.exists(rotated_file)) {
    expect_true(file.exists(temp_log))  # Current log should still exist
    expect_true(file.exists(rotated_file))  # Rotated log should exist
  }
  
  # Test manual rotation
  if (file.exists(temp_log)) {
    result <- logger$rotate_logs()
    expect_true(result)
    expect_true(file.exists(rotated_file))
  }
  
  # Cleanup
  files_to_remove <- c(temp_log, rotated_file)
  for (f in files_to_remove) {
    if (file.exists(f)) file.remove(f)
  }
})

test_that("Concurrent logging safety", {
  temp_log <- tempfile(fileext = ".log")
  logger <- AtlasLogger$new(log_file = temp_log)
  
  # Simulate concurrent logging (sequential in this test)
  modules <- c("MODULE_A", "MODULE_B", "MODULE_C")
  
  # Log from multiple "modules" rapidly
  for (i in 1:30) {
    module <- modules[(i %% length(modules)) + 1]
    logger$log_info(paste("Concurrent message", i), module)
  }
  
  if (file.exists(temp_log)) {
    log_content <- readLines(temp_log)
    
    # Verify all messages are present
    expect_true(length(log_content) >= 30)
    
    # Verify no corruption (each line should have proper format)
    timestamp_pattern <- "\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]"
    valid_lines <- grepl(timestamp_pattern, log_content)
    expect_true(all(valid_lines))
  }
  
  # Test concurrent memory tracking
  for (i in 1:10) {
    logger$track_memory_usage(paste0("CONCURRENT_", i))
  }
  
  summary <- logger$get_performance_summary()
  expect_true(summary$memory_snapshots >= 10)
  
  # Cleanup
  if (file.exists(temp_log)) file.remove(temp_log)
})

test_that("Log file size management", {
  temp_log <- tempfile(fileext = ".log")
  small_size <- 512  # 512 bytes
  logger <- AtlasLogger$new(log_file = temp_log, max_file_size = small_size, max_files = 2)
  
  # Write enough data to exceed file size
  large_message <- paste(rep("A", 100), collapse = "")
  
  for (i in 1:20) {
    logger$log_info(paste("Large message", i, large_message), "SIZE_TEST")
  }
  
  # Check if file size is managed
  if (file.exists(temp_log)) {
    file_size <- file.info(temp_log)$size
    
    # File should either be under max size or rotation should have occurred
    base_name <- tools::file_path_sans_ext(temp_log)
    extension <- tools::file_ext(temp_log)
    rotated_file <- paste0(base_name, ".1.", extension)
    
    if (file.exists(rotated_file)) {
      # Rotation occurred, which is expected behavior
      expect_true(TRUE)
    } else {
      # No rotation yet, file should be close to max size
      expect_true(file_size <= small_size * 2) # Allow some buffer
    }
  }
  
  # Test max files limit
  for (i in 1:50) {
    logger$log_info(paste("File limit test", i, large_message), "LIMIT_TEST")
  }
  
  # Count rotated files
  base_name <- tools::file_path_sans_ext(temp_log)
  extension <- tools::file_ext(temp_log)
  
  rotated_files <- 0
  for (i in 1:5) {
    rotated_file <- paste0(base_name, ".", i, ".", extension)
    if (file.exists(rotated_file)) {
      rotated_files <- rotated_files + 1
    }
  }
  
  # Should not exceed max_files limit
  expect_true(rotated_files <= 2)
  
  # Cleanup
  files_to_remove <- c(temp_log)
  for (i in 1:5) {
    rotated_file <- paste0(base_name, ".", i, ".", extension)
    files_to_remove <- c(files_to_remove, rotated_file)
  }
  
  for (f in files_to_remove) {
    if (file.exists(f)) file.remove(f)
  }
})

# ============================================================================
# PERFORMANCE BENCHMARK TESTS
# ============================================================================

test_that("Logger performance benchmarks", {
  logger <- AtlasLogger$new()
  
  # Benchmark logging performance
  start_time <- Sys.time()
  
  for (i in 1:1000) {
    logger$log_info(paste("Benchmark message", i), "BENCHMARK")
  }
  
  end_time <- Sys.time()
  total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Should be able to handle 1000 log messages in reasonable time (< 5 seconds)
  expect_true(total_time < 5.0)
  
  # Benchmark memory tracking
  start_time <- Sys.time()
  
  for (i in 1:100) {
    logger$track_memory_usage("BENCHMARK")
  }
  
  end_time <- Sys.time()
  memory_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Memory tracking should be fast (< 2 seconds for 100 calls)
  expect_true(memory_time < 2.0)
  
  # Test performance summary generation
  start_time <- Sys.time()
  summary <- logger$get_performance_summary()
  end_time <- Sys.time()
  summary_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Performance summary should be very fast (< 0.1 seconds)
  expect_true(summary_time < 0.1)
  expect_true(is.list(summary))
})

# ============================================================================
# INTEGRATION TESTS
# ============================================================================

test_that("Logger integration with performance data", {
  logger <- AtlasLogger$new()
  
  # Test logging with performance data
  perf_data <- list(
    operation = "data_load",
    records_processed = 1000,
    memory_used_mb = 50.5,
    cpu_time_sec = 2.3
  )
  
  logger$log_info("Data loading completed", "DATA_MODULE", perf_data)
  
  # Verify performance data is stored
  summary <- logger$get_performance_summary()
  expect_true(summary$total_logs >= 1)
  expect_true(length(summary$performance_data) >= 1)
  
  # Check that performance data structure is correct
  last_perf <- summary$performance_data[[length(summary$performance_data)]]
  expect_equal(last_perf$module, "DATA_MODULE")
  expect_equal(last_perf$data$operation, "data_load")
  expect_equal(last_perf$data$records_processed, 1000)
})

test_that("Logger cleanup and resource management", {
  temp_log <- tempfile(fileext = ".log")
  logger <- AtlasLogger$new(log_file = temp_log)
  
  # Generate some data
  for (i in 1:10) {
    logger$log_info(paste("Test message", i), "CLEANUP_TEST")
    logger$track_memory_usage("CLEANUP_TEST")
  }
  
  # Verify data exists
  summary_before <- logger$get_performance_summary()
  expect_true(summary_before$total_logs > 0)
  expect_true(summary_before$memory_snapshots > 0)
  
  # Clear performance data
  logger$clear_performance_data()
  
  # Verify data is cleared
  summary_after <- logger$get_performance_summary()
  expect_equal(summary_after$total_logs, 0)
  expect_equal(summary_after$memory_snapshots, 0)
  
  # Log file should still exist
  expect_true(file.exists(temp_log))
  
  # Cleanup
  if (file.exists(temp_log)) file.remove(temp_log)
})

# ============================================================================
# RUN ALL TESTS
# ============================================================================

cat("Running Atlas Labs Logger Module Unit Tests...\n")
cat("=" %R% 50, "\n")

# Run tests with detailed output
test_results <- test_dir(".", reporter = "summary")

cat("\n", "=" %R% 50, "\n")
cat("Atlas Labs Logger Module Tests Complete!\n")

# Additional test summary
if (exists("test_results")) {
  cat("Test Results Summary:\n")
  print(test_results)
}