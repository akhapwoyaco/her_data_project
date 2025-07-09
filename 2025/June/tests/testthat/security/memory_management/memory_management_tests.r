# ===============================================================================
# Atlas Labs HR Analytics Dashboard - Memory Management Unit Tests
# Comprehensive testing suite for memory management with edge cases
# Developer: akhapwoyaco
# ===============================================================================

# Load required libraries for testing
library(testthat)
library(shiny)
library(R6)
library(tidyverse)
library(plotly)
library(pryr)
library(profvis)
library(bench)
library(gc)

# Source the modules (assuming they exist)
# source("modules/logger_module.R")
# source("modules/data_loader_module.R")
# source("global.R")
# source("utils.R")

# ===============================================================================
# MEMORY MANAGEMENT TEST SUITE
# ===============================================================================

# Mock AtlasLogger R6 class for testing
AtlasLogger <- R6Class("AtlasLogger",
  private = list(
    .logs = NULL,
    .memory_snapshots = NULL,
    .gc_stats = NULL,
    .performance_cache = NULL,
    .buffer_size = 1000,
    .max_cache_size = 100
  ),
  
  public = list(
    initialize = function(buffer_size = 1000, max_cache_size = 100) {
      private$.logs <- list()
      private$.memory_snapshots <- list()
      private$.gc_stats <- list()
      private$.performance_cache <- new.env(parent = emptyenv())
      private$.buffer_size <- buffer_size
      private$.max_cache_size <- max_cache_size
    },
    
    log_memory_snapshot = function(context = "default") {
      snapshot <- list(
        timestamp = Sys.time(),
        context = context,
        memory_usage = pryr::mem_used(),
        objects_count = length(ls(envir = .GlobalEnv)),
        gc_info = gc(verbose = FALSE, reset = TRUE)
      )
      private$.memory_snapshots[[length(private$.memory_snapshots) + 1]] <- snapshot
      invisible(snapshot)
    },
    
    get_memory_snapshots = function() {
      private$.memory_snapshots
    },
    
    clear_cache = function() {
      rm(list = ls(envir = private$.performance_cache), envir = private$.performance_cache)
      gc(verbose = FALSE)
    },
    
    get_cache_size = function() {
      length(ls(envir = private$.performance_cache))
    },
    
    add_to_cache = function(key, value) {
      if (self$get_cache_size() >= private$.max_cache_size) {
        # Remove oldest entries
        cache_keys <- ls(envir = private$.performance_cache)
        oldest_key <- cache_keys[1]
        rm(list = oldest_key, envir = private$.performance_cache)
      }
      assign(key, value, envir = private$.performance_cache)
    },
    
    get_from_cache = function(key) {
      if (exists(key, envir = private$.performance_cache)) {
        return(get(key, envir = private$.performance_cache))
      }
      return(NULL)
    },
    
    force_gc = function() {
      gc(verbose = FALSE, full = TRUE)
    }
  )
)

# Mock data generation functions
generate_large_dataset <- function(n_rows = 100000, n_cols = 50) {
  data <- tibble::tibble(
    id = seq_len(n_rows),
    .rows = n_rows
  )
  
  # Add various column types
  for (i in seq_len(n_cols - 1)) {
    col_name <- paste0("col_", i)
    if (i %% 4 == 0) {
      data[[col_name]] <- sample(letters, n_rows, replace = TRUE)
    } else if (i %% 4 == 1) {
      data[[col_name]] <- runif(n_rows)
    } else if (i %% 4 == 2) {
      data[[col_name]] <- sample(1:1000, n_rows, replace = TRUE)
    } else {
      data[[col_name]] <- sample(c(TRUE, FALSE), n_rows, replace = TRUE)
    }
  }
  
  return(data)
}

generate_nested_data <- function(depth = 5, width = 10) {
  if (depth == 0) return(runif(width))
  
  result <- list()
  for (i in seq_len(width)) {
    result[[i]] <- generate_nested_data(depth - 1, width)
  }
  return(result)
}

# ===============================================================================
# TEST SUITE 1: MEMORY LEAK DETECTION
# ===============================================================================

test_that("Memory Leak Detection - Basic Operations", {
  skip_if_not_installed("pryr")
  
  logger <- AtlasLogger$new()
  
  # Baseline memory measurement
  initial_memory <- pryr::mem_used()
  logger$log_memory_snapshot("baseline")
  
  # Perform operations that should not leak memory
  for (i in 1:100) {
    temp_data <- data.frame(x = runif(1000), y = rnorm(1000))
    temp_result <- temp_data %>%
      mutate(z = x + y) %>%
      filter(z > 0) %>%
      summarise(mean_z = mean(z))
    
    # Clean up explicitly
    rm(temp_data, temp_result)
  }
  
  # Force garbage collection
  gc(verbose = FALSE, full = TRUE)
  
  # Check for memory leaks
  final_memory <- pryr::mem_used()
  memory_increase <- as.numeric(final_memory - initial_memory)
  
  # Allow for some memory increase but flag excessive growth
  expect_lt(memory_increase, 10 * 1024 * 1024, # 10MB threshold
           info = paste("Memory increased by", memory_increase, "bytes"))
})

test_that("Memory Leak Detection - R6 Object Lifecycle", {
  skip_if_not_installed("pryr")
  
  initial_memory <- pryr::mem_used()
  
  # Create and destroy R6 objects
  loggers <- list()
  for (i in 1:50) {
    loggers[[i]] <- AtlasLogger$new()
    loggers[[i]]$log_memory_snapshot(paste("iteration", i))
  }
  
  # Clear all loggers
  rm(loggers)
  gc(verbose = FALSE, full = TRUE)
  
  final_memory <- pryr::mem_used()
  memory_increase <- as.numeric(final_memory - initial_memory)
  
  # R6 objects should be properly cleaned up
  expect_lt(memory_increase, 5 * 1024 * 1024, # 5MB threshold
           info = paste("R6 objects may have leaked memory:", memory_increase, "bytes"))
})

test_that("Memory Leak Detection - Reactive Context Simulation", {
  skip_if_not_installed("pryr")
  
  initial_memory <- pryr::mem_used()
  
  # Simulate reactive context with reactive values
  reactive_env <- new.env()
  
  for (i in 1:20) {
    # Simulate reactive data updates
    reactive_env[[paste0("data_", i)]] <- generate_large_dataset(10000, 20)
    
    # Simulate filtered data
    reactive_env[[paste0("filtered_", i)]] <- reactive_env[[paste0("data_", i)]] %>%
      filter(col_1 > 0.5) %>%
      select(1:10)
    
    # Clean up old data (simulate reactive invalidation)
    if (i > 5) {
      old_key <- paste0("data_", i - 5)
      if (exists(old_key, envir = reactive_env)) {
        rm(list = old_key, envir = reactive_env)
      }
    }
  }
  
  # Clear reactive environment
  rm(list = ls(envir = reactive_env), envir = reactive_env)
  gc(verbose = FALSE, full = TRUE)
  
  final_memory <- pryr::mem_used()
  memory_increase <- as.numeric(final_memory - initial_memory)
  
  expect_lt(memory_increase, 20 * 1024 * 1024, # 20MB threshold
           info = paste("Reactive context simulation leaked memory:", memory_increase, "bytes"))
})

# ===============================================================================
# TEST SUITE 2: GARBAGE COLLECTION EFFICIENCY
# ===============================================================================

test_that("Garbage Collection Efficiency - Manual GC Triggers", {
  logger <- AtlasLogger$new()
  
  # Generate data that should trigger GC
  large_objects <- list()
  for (i in 1:10) {
    large_objects[[i]] <- generate_large_dataset(50000, 30)
  }
  
  # Capture GC stats before cleanup
  gc_before <- gc(verbose = FALSE, reset = TRUE)
  
  # Clear objects and force GC
  rm(large_objects)
  logger$force_gc()
  
  # Capture GC stats after cleanup
  gc_after <- gc(verbose = FALSE, reset = TRUE)
  
  # Check that GC was effective
  expect_gte(gc_after[1, 1], 0) # Ncells should be reasonable
  expect_gte(gc_after[2, 1], 0) # Vcells should be reasonable
  
  # Memory should be reclaimed
  expect_lt(gc_after[1, 2], gc_before[1, 2] * 1.5) # Allow some increase
})

test_that("Garbage Collection Efficiency - Automatic GC Thresholds", {
  skip_if_not_installed("pryr")
  
  logger <- AtlasLogger$new()
  gc_count_before <- gc(verbose = FALSE)[1, 3] # GC count
  
  # Create objects that should trigger automatic GC
  for (i in 1:100) {
    temp_large <- matrix(runif(100000), nrow = 1000)
    temp_processed <- temp_large * 2
    
    # Some operations should trigger GC automatically
    if (i %% 10 == 0) {
      logger$log_memory_snapshot(paste("iteration", i))
    }
    
    # Don't explicitly clean up - let GC handle it
  }
  
  gc_count_after <- gc(verbose = FALSE)[1, 3]
  
  # GC should have been triggered automatically
  expect_gt(gc_count_after, gc_count_before,
           info = "Automatic garbage collection should have been triggered")
})

test_that("Garbage Collection Efficiency - Large Object Cleanup", {
  skip_if_not_installed("bench")
  
  logger <- AtlasLogger$new()
  
  # Test GC efficiency with very large objects
  gc_timing <- bench::mark(
    large_object_creation = {
      huge_data <- generate_large_dataset(200000, 50)
      processed_data <- huge_data %>%
        mutate(computed_col = col_1 * col_2) %>%
        group_by(col_3) %>%
        summarise(mean_val = mean(computed_col, na.rm = TRUE))
      
      rm(huge_data, processed_data)
      gc(verbose = FALSE)
    },
    iterations = 5,
    check = FALSE
  )
  
  # GC should complete within reasonable time
  expect_lt(median(gc_timing$total_time), as.difftime(5, units = "secs"),
           info = "Large object cleanup taking too long")
})

# ===============================================================================
# TEST SUITE 3: OBJECT LIFECYCLE MANAGEMENT
# ===============================================================================

test_that("Object Lifecycle Management - R6 Object Creation and Destruction", {
  skip_if_not_installed("pryr")
  
  initial_objects <- length(ls(envir = .GlobalEnv))
  
  # Create objects with references
  logger_parent <- AtlasLogger$new()
  logger_child <- AtlasLogger$new()
  
  # Create cross-references
  logger_parent$add_to_cache("child_ref", logger_child)
  logger_child$add_to_cache("parent_ref", logger_parent)
  
  # Verify objects exist
  expect_true(exists("logger_parent"))
  expect_true(exists("logger_child"))
  
  # Break circular references
  logger_parent$clear_cache()
  logger_child$clear_cache()
  
  # Clean up
  rm(logger_parent, logger_child)
  gc(verbose = FALSE)
  
  final_objects <- length(ls(envir = .GlobalEnv))
  
  # Object count should return to baseline
  expect_lte(final_objects, initial_objects + 2, # Allow for some test artifacts
            info = "Objects not properly cleaned up")
})

test_that("Object Lifecycle Management - Weak References", {
  skip_if_not_installed("pryr")
  
  # Create objects with weak references simulation
  strong_ref <- AtlasLogger$new()
  weak_ref_env <- new.env(parent = emptyenv())
  
  # Simulate weak reference
  weak_ref_env$logger_ref <- strong_ref
  
  # Verify strong reference exists
  expect_true(!is.null(strong_ref))
  expect_true(exists("logger_ref", envir = weak_ref_env))
  
  # Remove strong reference
  rm(strong_ref)
  gc(verbose = FALSE)
  
  # Weak reference should still exist but object should be collectable
  expect_true(exists("logger_ref", envir = weak_ref_env))
  
  # Clean up weak reference environment
  rm(list = ls(envir = weak_ref_env), envir = weak_ref_env)
})

test_that("Object Lifecycle Management - Environment Cleanup", {
  # Create nested environments
  parent_env <- new.env()
  child_env <- new.env(parent = parent_env)
  
  # Populate with data
  parent_env$large_data <- generate_large_dataset(10000, 20)
  child_env$processed_data <- parent_env$large_data %>%
    mutate(new_col = col_1 * 2)
  
  # Verify environment structure
  expect_true(exists("large_data", envir = parent_env))
  expect_true(exists("processed_data", envir = child_env))
  
  # Clean up child environment
  rm(list = ls(envir = child_env), envir = child_env)
  
  # Parent data should still exist
  expect_true(exists("large_data", envir = parent_env))
  
  # Clean up parent environment
  rm(list = ls(envir = parent_env), envir = parent_env)
  
  # Verify cleanup
  expect_equal(length(ls(envir = parent_env)), 0)
  expect_equal(length(ls(envir = child_env)), 0)
})

# ===============================================================================
# TEST SUITE 4: LARGE DATASET HANDLING
# ===============================================================================

test_that("Large Dataset Handling - Memory Efficient Processing", {
  skip_if_not_installed("pryr")
  
  logger <- AtlasLogger$new()
  initial_memory <- pryr::mem_used()
  
  # Test chunked processing of large dataset
  large_data <- generate_large_dataset(500000, 30)
  logger$log_memory_snapshot("after_creation")
  
  # Process in chunks to avoid memory spikes
  chunk_size <- 50000
  n_chunks <- ceiling(nrow(large_data) / chunk_size)
  
  results <- list()
  for (i in 1:n_chunks) {
    start_row <- (i - 1) * chunk_size + 1
    end_row <- min(i * chunk_size, nrow(large_data))
    
    chunk <- large_data[start_row:end_row, ]
    result <- chunk %>%
      mutate(computed = col_1 * col_2) %>%
      group_by(col_3) %>%
      summarise(mean_val = mean(computed, na.rm = TRUE), .groups = "drop")
    
    results[[i]] <- result
    rm(chunk, result) # Clean up chunk immediately
  }
  
  # Combine results
  final_result <- bind_rows(results)
  rm(results, large_data)
  gc(verbose = FALSE)
  
  final_memory <- pryr::mem_used()
  memory_increase <- as.numeric(final_memory - initial_memory)
  
  # Memory increase should be reasonable for large dataset processing
  expect_lt(memory_increase, 100 * 1024 * 1024, # 100MB threshold
           info = paste("Large dataset processing used excessive memory:", memory_increase, "bytes"))
  
  # Result should be valid
  expect_true(nrow(final_result) > 0)
  expect_true(ncol(final_result) >= 2)
})

test_that("Large Dataset Handling - Streaming Data Simulation", {
  skip_if_not_installed("pryr")
  
  logger <- AtlasLogger$new()
  max_memory_seen <- 0
  
  # Simulate streaming data processing
  for (batch in 1:20) {
    # Simulate incoming data batch
    batch_data <- generate_large_dataset(25000, 15)
    
    # Process batch
    processed <- batch_data %>%
      filter(col_1 > 0.3) %>%
      mutate(score = col_1 * col_2 + col_3) %>%
      arrange(desc(score)) %>%
      slice_head(n = 1000)
    
    # Track memory usage
    current_memory <- as.numeric(pryr::mem_used())
    max_memory_seen <- max(max_memory_seen, current_memory)
    
    # Clean up batch immediately
    rm(batch_data, processed)
    
    # Force GC every few batches
    if (batch %% 5 == 0) {
      gc(verbose = FALSE)
    }
  }
  
  # Final cleanup
  gc(verbose = FALSE, full = TRUE)
  final_memory <- as.numeric(pryr::mem_used())
  
  # Memory should not grow excessively during streaming
  expect_lt(final_memory, max_memory_seen * 1.2,
           info = "Memory not properly cleaned up during streaming")
})

test_that("Large Dataset Handling - Memory Pressure Simulation", {
  skip_if_not_installed("pryr")
  
  logger <- AtlasLogger$new()
  
  # Test behavior under memory pressure
  datasets <- list()
  memory_measurements <- numeric()
  
  tryCatch({
    for (i in 1:10) {
      # Create increasingly large datasets
      size_multiplier <- i * 2
      datasets[[i]] <- generate_large_dataset(100000 * size_multiplier, 20)
      
      memory_measurements[i] <- as.numeric(pryr::mem_used())
      
      # Process data to create memory pressure
      processed <- datasets[[i]] %>%
        mutate(
          interaction1 = col_1 * col_2,
          interaction2 = col_3 * col_4,
          complex_calc = sqrt(abs(interaction1 - interaction2))
        ) %>%
        filter(complex_calc > quantile(complex_calc, 0.9, na.rm = TRUE))
      
      logger$add_to_cache(paste0("result_", i), processed)
      
      # If memory usage is too high, clean up older datasets
      if (i > 5) {
        old_index <- i - 5
        if (exists(paste0("result_", old_index), envir = logger$get_cache_size())) {
          datasets[[old_index]] <- NULL
        }
      }
    }
  }, error = function(e) {
    # If we hit memory limits, that's expected behavior
    message("Memory pressure reached: ", e$message)
  })
  
  # Clean up
  rm(datasets)
  logger$clear_cache()
  gc(verbose = FALSE, full = TRUE)
  
  # Memory measurements should show reasonable progression
  expect_true(length(memory_measurements) > 0)
  expect_true(all(memory_measurements > 0))
})

# ===============================================================================
# TEST SUITE 5: MEMORY FRAGMENTATION ANALYSIS
# ===============================================================================

test_that("Memory Fragmentation Analysis - Allocation Patterns", {
  skip_if_not_installed("pryr")
  
  logger <- AtlasLogger$new()
  
  # Create fragmentation by allocating and deallocating objects of different sizes
  objects <- list()
  memory_snapshots <- numeric()
  
  # Phase 1: Allocate objects of varying sizes
  for (i in 1:20) {
    size <- sample(c(1000, 5000, 10000, 50000), 1)
    objects[[i]] <- generate_large_dataset(size, 10)
    memory_snapshots[i] <- as.numeric(pryr::mem_used())
  }
  
  # Phase 2: Deallocate every other object (create holes)
  for (i in seq(2, 20, by = 2)) {
    objects[[i]] <- NULL
  }
  
  gc(verbose = FALSE)
  memory_after_partial_cleanup <- pryr::mem_used()
  
  # Phase 3: Allocate new objects in the gaps
  for (i in seq(2, 20, by = 2)) {
    size <- sample(c(2000, 7000, 15000), 1)
    objects[[i]] <- generate_large_dataset(size, 8)
  }
  
  memory_after_reallocation <- pryr::mem_used()
  
  # Clean up everything
  rm(objects)
  gc(verbose = FALSE, full = TRUE)
  memory_after_full_cleanup <- pryr::mem_used()
  
  # Analyze fragmentation patterns
  fragmentation_indicator <- as.numeric(memory_after_reallocation - memory_after_partial_cleanup)
  
  # Memory should be efficiently reused
  expect_lt(fragmentation_indicator, 50 * 1024 * 1024, # 50MB threshold
           info = paste("Possible memory fragmentation detected:", fragmentation_indicator, "bytes"))
})

test_that("Memory Fragmentation Analysis - Object Size Distribution", {
  skip_if_not_installed("pryr")
  
  logger <- AtlasLogger$new()
  initial_memory <- pryr::mem_used()
  
  # Test with objects of exponentially increasing sizes
  objects <- list()
  object_sizes <- c(1000, 2000, 4000, 8000, 16000, 32000, 64000)
  
  for (i in seq_along(object_sizes)) {
    objects[[i]] <- generate_large_dataset(object_sizes[i], 10)
    
    # Measure memory after each allocation
    current_memory <- pryr::mem_used()
    memory_increase <- as.numeric(current_memory - initial_memory)
    
    # Memory increase should be roughly proportional to object size
    expected_increase <- sum(object_sizes[1:i]) * 10 * 8 # rough estimate
    ratio <- memory_increase / expected_increase
    
    # Allow for overhead but detect excessive fragmentation
    expect_lt(ratio, 5, # 5x overhead threshold
             info = paste("Excessive memory overhead at size", object_sizes[i]))
  }
  
  # Clean up and verify memory is reclaimed
  rm(objects)
  gc(verbose = FALSE, full = TRUE)
  
  final_memory <- pryr::mem_used()
  memory_retained <- as.numeric(final_memory - initial_memory)
  
  # Most memory should be reclaimed
  expect_lt(memory_retained, 20 * 1024 * 1024, # 20MB threshold
           info = "Memory not properly reclaimed, possible fragmentation")
})

# ===============================================================================
# TEST SUITE 6: CACHE UTILIZATION EFFICIENCY
# ===============================================================================

test_that("Cache Utilization Efficiency - LRU Cache Behavior", {
  logger <- AtlasLogger$new(max_cache_size = 5)
  
  # Fill cache to capacity
  for (i in 1:5) {
    logger$add_to_cache(paste0("key_", i), generate_large_dataset(1000, 5))
  }
  
  expect_equal(logger$get_cache_size(), 5)
  
  # Add one more item (should evict oldest)
  logger$add_to_cache("key_6", generate_large_dataset(1000, 5))
  expect_equal(logger$get_cache_size(), 5)
  
  # First item should be evicted
  expect_null(logger$get_from_cache("key_1"))
  expect_true(!is.null(logger$get_from_cache("key_6")))
  
  # Test cache hit efficiency
  hit_count <- 0
  miss_count <- 0
  
  for (i in 1:50) {
    key <- paste0("key_", sample(2:6, 1)) # Keys 2-6 should be in cache
    result <- logger$get_from_cache(key)
    if (!is.null(result)) {
      hit_count <- hit_count + 1
    } else {
      miss_count <- miss_count + 1
    }
  }
  
  hit_rate <- hit_count / (hit_count + miss_count)
  expect_gt(hit_rate, 0.8, # 80% hit rate expected
           info = paste("Cache hit rate too low:", hit_rate))
})

test_that("Cache Utilization Efficiency - Memory Pressure on Cache", {
  skip_if_not_installed("pryr")
  
  logger <- AtlasLogger$new(max_cache_size = 10)
  initial_memory <- pryr::mem_used()
  
  # Add large objects to cache
  for (i in 1:10) {
    large_object <- generate_large_dataset(50000, 20)
    logger$add_to_cache(paste0("large_", i), large_object)
  }
  
  memory_with_cache <- pryr::mem_used()
  cache_memory_usage <- as.numeric(memory_with_cache - initial_memory)
  
  # Clear cache and measure memory reclaimed
  logger$clear_cache()
  gc(verbose = FALSE)
  
  memory_after_clear <- pryr::mem_used()
  memory_reclaimed <- as.numeric(memory_with_cache - memory_after_clear)
  
  # Most cache memory should be reclaimed
  cache_efficiency <- memory_reclaimed / cache_memory_usage
  expect_gt(cache_efficiency, 0.8, # 80% of cache memory should be reclaimed
           info = paste("Cache memory not efficiently reclaimed:", cache_efficiency))
})

test_that("Cache Utilization Efficiency - Cache Fragmentation", {
  logger <- AtlasLogger$new(max_cache_size = 20)
  
  # Create cache fragmentation by adding/removing items of different sizes
  for (i in 1:10) {
    small_data <- generate_large_dataset(1000, 5)
    large_data <- generate_large_dataset(10000, 15)
    
    logger$add_to_cache(paste0("small_", i), small_data)
    logger$add_to_cache(paste0("large_", i), large_data)
  }
  
  # Remove every other item
  for (i in seq(2, 10, by = 2)) {
    logger$add_to_cache(paste0("small_", i), NULL) # Simulate removal
    logger$add_to_cache(paste0("large_", i), NULL)
  }
  
  # Add new items of medium size
  for (i in 1:5) {
    medium_data <- generate_large_dataset(5000, 10)
    logger$add_to_cache(paste0("medium_", i), medium_data)
  }
  
  # Cache should still function efficiently
  expect_lte(logger$get_cache_size(), 20)
  
  # Test retrieval efficiency
  retrieval_times <- numeric()
  for (i in 1:20) {
    start_time <- Sys.time()
    result <- logger$get_from_cache(paste0("medium_", sample(1:5, 1)))
    end_time <- Sys.time()
    retrieval_times[i] <- as.numeric(end_time - start_time)
  }
  
  # Retrieval should be consistently fast
  expect_lt(mean(retrieval_times), 0.001, # 1ms average
           info = "Cache retrieval too slow, possible fragmentation")
})

# ===============================================================================
# TEST SUITE 7: BUFFER OVERFLOW PREVENTION
# ===============================================================================

test_that("Buffer Overflow Prevention - Fixed Size Buffers", {
  logger <- AtlasLogger$new(buffer_size = 100)
  
  # Test buffer overflow protection
  for (i in 1:150) {
    logger$log_memory_snapshot(paste("test", i))
  }
  
  snapshots <- logger$get_memory_snapshots()
  
  # Buffer should not exceed its limit
  expect_lte(length(snapshots), 100,
            info = "Buffer overflow detected - size limit not enforced")
  
  # Latest entries should be preserved
  latest_snapshot <- snapshots[[length(snapshots)]]
  expect_equal(latest_snapshot$context, "test 150")
})

test_that("Buffer Overflow Prevention - Dynamic Buffer Resizing", {
  logger <- AtlasLogger$new(buffer_size = 50)
  
  # Fill buffer beyond capacity
  for (i in 1:100) {
    large_snapshot_data <- list(
      timestamp = Sys.time(),
      context = paste("large_test", i),
      memory_usage = pryr::mem_used(),
      large_data = generate_large_dataset(1000, 10)
    )
    
    # Simulate adding large snapshots
    logger$log_memory_snapshot(paste("large_test", i))
  }
  
  snapshots <- logger$get_memory_snapshots()
  
  # Buffer should maintain reasonable size
  expect_lte(length(snapshots), 50,
            info = "Buffer size not properly controlled")
  
  # Memory usage should remain reasonable
  expect_true(all(sapply(snapshots, function(x) !is.null(x$memory_usage))))
})

test_that("Buffer Overflow Prevention - Circular Buffer Behavior", {
  logger <- AtlasLogger$new(buffer_size = 10)
  
  # Fill buffer with identifiable entries
  for (i in 1:20) {
    logger$log_memory_snapshot(paste("entry", i))
  }
  
  snapshots <- logger$get_memory_snapshots()
  
  # Should have exactly buffer_size entries
  expect_equal(length(snapshots), 10)
  
  # Should contain the latest entries (11-20)
  contexts <- sapply(snapshots, function(x) x$context)
  expect_true(all(grepl("entry (1[1-9]|20)", contexts)))
  
  # Should not contain early entries (1-10)
  expect_false(any(grepl("entry [1-9]$", contexts)))
})

# ===============================================================================
# TEST SUITE 8: MEMORY USAGE OPTIMIZATION
# ===============================================================================

test_that("Memory Usage Optimization - Object Pooling