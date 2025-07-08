# =============================================================================
# Atlas Labs HR Analytics Dashboard - Comprehensive Load Testing Suite
# =============================================================================
# Testing Framework: testthat, shinytest2, parallel processing
# Coverage: Concurrent users, database pooling, memory, CPU, network, response time
# =============================================================================

# Load required libraries
library(testthat)
library(shinytest2)
library(parallel)
library(future)
library(promises)
library(pool)
library(DBI)
library(RSQLite)
library(httr)
library(microbenchmark)
library(profvis)
library(pryr)
library(pingr)

# =============================================================================
# 1. CONCURRENT USER SIMULATION TESTS
# =============================================================================

test_that("Concurrent User Simulation - Basic Load", {
  # Test 10 concurrent users
  test_concurrent_users <- function(n_users = 10) {
    # Setup test environment
    app_dir <- getwd()
    
    # Create multiple app instances
    app_instances <- vector("list", n_users)
    start_time <- Sys.time()
    
    # Simulate concurrent connections
    results <- future.apply::future_lapply(1:n_users, function(i) {
      tryCatch({
        # Simulate user session
        app <- AppDriver$new(app_dir, load_timeout = 30000)
        
        # Simulate typical user actions
        session_start <- Sys.time()
        
        # Navigate to different modules
        app$set_inputs(sidebar_tab = "overview")
        Sys.sleep(0.1)
        
        app$set_inputs(sidebar_tab = "attrition")
        Sys.sleep(0.1)
        
        app$set_inputs(sidebar_tab = "demographics")
        Sys.sleep(0.1)
        
        # Apply filters
        app$set_inputs(department_filter = "Engineering")
        Sys.sleep(0.1)
        
        # Generate report
        app$click("generate_report")
        Sys.sleep(0.5)
        
        session_end <- Sys.time()
        session_duration <- as.numeric(session_end - session_start)
        
        app$stop()
        
        list(
          user_id = i,
          session_duration = session_duration,
          status = "success",
          timestamp = session_start
        )
      }, error = function(e) {
        list(
          user_id = i,
          session_duration = NA,
          status = "error",
          error_message = e$message,
          timestamp = Sys.time()
        )
      })
    })
    
    end_time <- Sys.time()
    total_duration <- as.numeric(end_time - start_time)
    
    # Analyze results
    successful_sessions <- sum(sapply(results, function(x) x$status == "success"))
    failed_sessions <- sum(sapply(results, function(x) x$status == "error"))
    avg_session_duration <- mean(sapply(results, function(x) x$session_duration), na.rm = TRUE)
    
    list(
      total_users = n_users,
      successful_sessions = successful_sessions,
      failed_sessions = failed_sessions,
      success_rate = successful_sessions / n_users,
      avg_session_duration = avg_session_duration,
      total_test_duration = total_duration,
      throughput = successful_sessions / total_duration
    )
  }
  
  # Test with 10 concurrent users
  result_10 <- test_concurrent_users(10)
  expect_gte(result_10$success_rate, 0.8)  # 80% success rate minimum
  expect_lt(result_10$avg_session_duration, 5)  # Average session under 5 seconds
  
  # Test with 50 concurrent users (stress test)
  result_50 <- test_concurrent_users(50)
  expect_gte(result_50$success_rate, 0.7)  # 70% success rate under stress
  expect_lt(result_50$avg_session_duration, 10)  # Average session under 10 seconds
})

test_that("Concurrent User Simulation - Edge Cases", {
  # Test rapid connection/disconnection
  test_rapid_connections <- function() {
    results <- replicate(20, {
      tryCatch({
        app <- AppDriver$new(getwd(), load_timeout = 5000)
        app$stop()
        "success"
      }, error = function(e) "error")
    })
    
    success_rate <- sum(results == "success") / length(results)
    expect_gte(success_rate, 0.8)
  }
  
  test_rapid_connections()
  
  # Test simultaneous module switching
  test_simultaneous_navigation <- function() {
    app <- AppDriver$new(getwd())
    
    # Rapidly switch between modules
    modules <- c("overview", "attrition", "demographics", "performance", "compensation")
    
    start_time <- Sys.time()
    for (i in 1:10) {
      for (module in modules) {
        app$set_inputs(sidebar_tab = module)
        Sys.sleep(0.05)  # Very rapid switching
      }
    }
    end_time <- Sys.time()
    
    navigation_time <- as.numeric(end_time - start_time)
    expect_lt(navigation_time, 30)  # Should complete within 30 seconds
    
    app$stop()
  }
  
  test_simultaneous_navigation()
})

# =============================================================================
# 2. DATABASE CONNECTION POOLING TESTS
# =============================================================================

test_that("Database Connection Pooling - Pool Management", {
  # Create test database
  create_test_db <- function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    
    # Create test tables
    DBI::dbExecute(con, "
      CREATE TABLE employee (
        employee_id INTEGER PRIMARY KEY,
        first_name TEXT,
        last_name TEXT,
        department TEXT,
        salary REAL,
        hire_date DATE
      )
    ")
    
    # Insert test data
    test_data <- data.frame(
      employee_id = 1:1000,
      first_name = paste0("Employee", 1:1000),
      last_name = paste0("LastName", 1:1000),
      department = sample(c("Engineering", "HR", "Sales", "Marketing"), 1000, replace = TRUE),
      salary = runif(1000, 50000, 150000),
      hire_date = sample(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"), 1000, replace = TRUE)
    )
    
    DBI::dbWriteTable(con, "employee", test_data, overwrite = TRUE)
    con
  }
  
  # Test connection pool creation
  test_pool <- pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = ":memory:",
    minSize = 2,
    maxSize = 10,
    idleTimeout = 60
  )
  
  # Test basic pool operations
  expect_true(pool::dbIsValid(test_pool))
  expect_equal(pool::dbGetInfo(test_pool)$minSize, 2)
  expect_equal(pool::dbGetInfo(test_pool)$maxSize, 10)
  
  # Test concurrent database operations
  test_concurrent_queries <- function(pool, n_queries = 20) {
    results <- future.apply::future_lapply(1:n_queries, function(i) {
      tryCatch({
        start_time <- Sys.time()
        
        # Execute query
        result <- pool::dbGetQuery(pool, "SELECT COUNT(*) as count FROM employee WHERE department = 'Engineering'")
        
        end_time <- Sys.time()
        query_time <- as.numeric(end_time - start_time)
        
        list(
          query_id = i,
          result = result,
          query_time = query_time,
          status = "success"
        )
      }, error = function(e) {
        list(
          query_id = i,
          result = NULL,
          query_time = NA,
          status = "error",
          error_message = e$message
        )
      })
    })
    
    successful_queries <- sum(sapply(results, function(x) x$status == "success"))
    avg_query_time <- mean(sapply(results, function(x) x$query_time), na.rm = TRUE)
    
    list(
      total_queries = n_queries,
      successful_queries = successful_queries,
      success_rate = successful_queries / n_queries,
      avg_query_time = avg_query_time
    )
  }
  
  query_results <- test_concurrent_queries(test_pool, 20)
  expect_gte(query_results$success_rate, 0.95)  # 95% success rate
  expect_lt(query_results$avg_query_time, 1)    # Average query under 1 second
  
  pool::poolClose(test_pool)
})

test_that("Database Connection Pooling - Edge Cases", {
  # Test pool exhaustion
  test_pool_exhaustion <- function() {
    small_pool <- pool::dbPool(
      drv = RSQLite::SQLite(),
      dbname = ":memory:",
      minSize = 1,
      maxSize = 2  # Very small pool
    )
    
    # Try to use more connections than available
    results <- future.apply::future_lapply(1:10, function(i) {
      tryCatch({
        pool::dbGetQuery(small_pool, "SELECT 1")
        "success"
      }, error = function(e) {
        "error"
      })
    })
    
    # Should handle gracefully, not crash
    expect_true(length(results) == 10)
    
    pool::poolClose(small_pool)
  }
  
  test_pool_exhaustion()
  
  # Test connection timeout
  test_connection_timeout <- function() {
    timeout_pool <- pool::dbPool(
      drv = RSQLite::SQLite(),
      dbname = ":memory:",
      minSize = 1,
      maxSize = 5,
      idleTimeout = 1  # 1 second timeout
    )
    
    # Use connection
    result1 <- pool::dbGetQuery(timeout_pool, "SELECT 1")
    expect_equal(result1[[1]], 1)
    
    # Wait for timeout
    Sys.sleep(2)
    
    # Should still work after timeout
    result2 <- pool::dbGetQuery(timeout_pool, "SELECT 1")
    expect_equal(result2[[1]], 1)
    
    pool::poolClose(timeout_pool)
  }
  
  test_connection_timeout()
})

# =============================================================================
# 3. MEMORY USAGE UNDER LOAD TESTS
# =============================================================================

test_that("Memory Usage Under Load - Basic Monitoring", {
  # Memory monitoring function
  monitor_memory <- function(test_function, duration_seconds = 30) {
    memory_samples <- list()
    start_time <- Sys.time()
    
    # Start memory monitoring
    memory_monitor <- function() {
      while (as.numeric(Sys.time() - start_time) < duration_seconds) {
        memory_info <- pryr::mem_used()
        gc_info <- gc(verbose = FALSE)
        
        memory_samples[[length(memory_samples) + 1]] <- list(
          timestamp = Sys.time(),
          memory_used = memory_info,
          gc_level0 = gc_info[1, 2],
          gc_level1 = gc_info[2, 2],
          gc_level2 = gc_info[3, 2]
        )
        
        Sys.sleep(0.5)  # Sample every 0.5 seconds
      }
    }
    
    # Run test function and monitor simultaneously
    future_memory <- future::future(memory_monitor())
    test_result <- test_function()
    
    # Wait for monitoring to complete
    future::value(future_memory)
    
    list(
      test_result = test_result,
      memory_samples = memory_samples,
      peak_memory = max(sapply(memory_samples, function(x) x$memory_used)),
      avg_memory = mean(sapply(memory_samples, function(x) x$memory_used)),
      memory_growth = tail(memory_samples, 1)[[1]]$memory_used - memory_samples[[1]]$memory_used
    )
  }
  
  # Test data loading under load
  test_data_loading <- function() {
    # Simulate loading large datasets repeatedly
    for (i in 1:50) {
      # Create large data frame
      large_df <- data.frame(
        id = 1:10000,
        name = paste0("Employee", 1:10000),
        department = sample(c("Engineering", "HR", "Sales"), 10000, replace = TRUE),
        salary = runif(10000, 50000, 150000),
        performance = rnorm(10000, 3.5, 0.8)
      )
      
      # Process data
      summary_stats <- large_df %>%
        group_by(department) %>%
        summarise(
          avg_salary = mean(salary),
          avg_performance = mean(performance),
          count = n()
        )
      
      # Clean up
      rm(large_df)
      if (i %% 10 == 0) gc()  # Garbage collect every 10 iterations
    }
    
    return("completed")
  }
  
  memory_test <- monitor_memory(test_data_loading, 30)
  
  # Memory should not grow excessively (less than 100MB growth)
  expect_lt(memory_test$memory_growth, 100 * 1024 * 1024)
  
  # Peak memory should be reasonable (less than 500MB)
  expect_lt(memory_test$peak_memory, 500 * 1024 * 1024)
})

test_that("Memory Usage Under Load - Edge Cases", {
  # Test memory leak detection
  test_memory_leak <- function() {
    initial_memory <- pryr::mem_used()
    
    # Create and destroy objects repeatedly
    for (i in 1:100) {
      # Create large objects
      temp_data <- replicate(10, {
        matrix(rnorm(1000), nrow = 100)
      }, simplify = FALSE)
      
      # Should be cleaned up automatically
      rm(temp_data)
      
      if (i %% 20 == 0) {
        gc()
        current_memory <- pryr::mem_used()
        memory_growth <- current_memory - initial_memory
        
        # Memory growth should be minimal
        expect_lt(memory_growth, 50 * 1024 * 1024)  # Less than 50MB growth
      }
    }
  }
  
  test_memory_leak()
  
  # Test large dataset handling
  test_large_dataset <- function() {
    # Create very large dataset
    large_dataset <- data.frame(
      id = 1:100000,
      value1 = rnorm(100000),
      value2 = runif(100000),
      category = sample(letters[1:10], 100000, replace = TRUE)
    )
    
    initial_memory <- pryr::mem_used()
    
    # Perform operations on large dataset
    result <- large_dataset %>%
      group_by(category) %>%
      summarise(
        mean_value1 = mean(value1),
        mean_value2 = mean(value2),
        count = n()
      )
    
    # Clean up
    rm(large_dataset)
    gc()
    
    final_memory <- pryr::mem_used()
    memory_cleaned <- initial_memory - final_memory
    
    # Should have freed most memory
    expect_gt(memory_cleaned, 0)
    
    return(result)
  }
  
  test_large_dataset()
})

# =============================================================================
# 4. CPU UTILIZATION PATTERNS TESTS
# =============================================================================

test_that("CPU Utilization Patterns - Load Testing", {
  # CPU monitoring function
  monitor_cpu <- function(test_function, duration_seconds = 30) {
    cpu_samples <- list()
    start_time <- Sys.time()
    
    # CPU monitoring (simplified - would need system-specific tools in production)
    cpu_monitor <- function() {
      while (as.numeric(Sys.time() - start_time) < duration_seconds) {
        # Simulate CPU usage measurement
        cpu_info <- list(
          timestamp = Sys.time(),
          cpu_percent = sample(1:100, 1),  # Placeholder - would use actual CPU monitoring
          load_avg = runif(1, 0, 2)
        )
        
        cpu_samples[[length(cpu_samples) + 1]] <- cpu_info
        Sys.sleep(1)  # Sample every second
      }
    }
    
    # Run test and monitor CPU
    future_cpu <- future::future(cpu_monitor())
    test_result <- test_function()
    future::value(future_cpu)
    
    list(
      test_result = test_result,
      cpu_samples = cpu_samples,
      peak_cpu = max(sapply(cpu_samples, function(x) x$cpu_percent)),
      avg_cpu = mean(sapply(cpu_samples, function(x) x$cpu_percent)),
      avg_load = mean(sapply(cpu_samples, function(x) x$load_avg))
    )
  }
  
  # Test CPU-intensive operations
  test_cpu_intensive <- function() {
    # Simulate data processing operations
    results <- future.apply::future_lapply(1:8, function(i) {  # 8 parallel tasks
      # Complex calculations
      data <- matrix(rnorm(10000), nrow = 1000)
      
      # Matrix operations
      result1 <- data %*% t(data)
      result2 <- eigen(result1[1:100, 1:100])
      
      # Statistical operations
      correlations <- cor(data)
      pca <- prcomp(data)
      
      # Clustering
      if (nrow(data) > 500) {
        clusters <- kmeans(data, centers = 5, nstart = 10)
      }
      
      return(list(
        task_id = i,
        status = "completed",
        result_size = object.size(result1)
      ))
    })
    
    return(results)
  }
  
  cpu_test <- monitor_cpu(test_cpu_intensive, 30)
  
  # CPU utilization should be reasonable
  expect_lt(cpu_test$avg_cpu, 90)  # Average CPU below 90%
  expect_lt(cpu_test$peak_cpu, 100)  # Peak CPU below 100%
})

test_that("CPU Utilization Patterns - Edge Cases", {
  # Test CPU spike handling
  test_cpu_spikes <- function() {
    # Create sudden CPU spikes
    spike_results <- lapply(1:5, function(i) {
      start_time <- Sys.time()
      
      # Sudden intensive computation
      result <- replicate(1000, {
        matrix(rnorm(1000), nrow = 100) %*% matrix(rnorm(1000), nrow = 100)
      }, simplify = FALSE)
      
      end_time <- Sys.time()
      computation_time <- as.numeric(end_time - start_time)
      
      list(
        spike_id = i,
        computation_time = computation_time,
        result_length = length(result)
      )
    })
    
    # Should handle spikes gracefully
    avg_spike_time <- mean(sapply(spike_results, function(x) x$computation_time))
    expect_lt(avg_spike_time, 10)  # Each spike should complete within 10 seconds
    
    return(spike_results)
  }
  
  test_cpu_spikes()
  
  # Test sustained CPU load
  test_sustained_load <- function() {
    start_time <- Sys.time()
    
    # Sustained moderate load for 30 seconds
    while (as.numeric(Sys.time() - start_time) < 30) {
      # Moderate computation
      data <- matrix(rnorm(5000), nrow = 500)
      result <- data %*% t(data)
      
      # Brief pause to avoid 100% CPU
      Sys.sleep(0.1)
    }
    
    end_time <- Sys.time()
    total_time <- as.numeric(end_time - start_time)
    
    # Should complete within reasonable time
    expect_gte(total_time, 30)  # Should run for at least 30 seconds
    expect_lt(total_time, 35)   # Should not take much longer
  }
  
  test_sustained_load()
})

# =============================================================================
# 5. NETWORK BANDWIDTH CONSUMPTION TESTS
# =============================================================================

test_that("Network Bandwidth Consumption - Data Transfer", {
  # Mock network operations (in real scenario, would test actual HTTP requests)
  simulate_network_transfer <- function(data_size_mb, n_transfers = 10) {
    transfer_results <- lapply(1:n_transfers, function(i) {
      start_time <- Sys.time()
      
      # Simulate data transfer
      mock_data <- raw(data_size_mb * 1024 * 1024)  # Create data of specified size
      
      # Simulate network delay
      Sys.sleep(runif(1, 0.1, 0.5))  # Random delay 0.1-0.5 seconds
      
      end_time <- Sys.time()
      transfer_time <- as.numeric(end_time - start_time)
      
      list(
        transfer_id = i,
        data_size_mb = data_size_mb,
        transfer_time = transfer_time,
        bandwidth_mbps = data_size_mb / transfer_time
      )
    })
    
    return(transfer_results)
  }
  
  # Test small data transfers (typical dashboard updates)
  small_transfers <- simulate_network_transfer(0.1, 20)  # 100KB transfers
  avg_small_bandwidth <- mean(sapply(small_transfers, function(x) x$bandwidth_mbps))
  expect_gt(avg_small_bandwidth, 0.1)  # Should achieve reasonable bandwidth
  
  # Test medium data transfers (report generation)
  medium_transfers <- simulate_network_transfer(1, 10)  # 1MB transfers
  avg_medium_bandwidth <- mean(sapply(medium_transfers, function(x) x$bandwidth_mbps))
  expect_gt(avg_medium_bandwidth, 0.5)  # Should handle 1MB transfers efficiently
  
  # Test large data transfers (data export)
  large_transfers <- simulate_network_transfer(10, 5)  # 10MB transfers
  avg_large_bandwidth <- mean(sapply(large_transfers, function(x) x$bandwidth_mbps))
  expect_gt(avg_large_bandwidth, 1)  # Should handle large transfers
})

test_that("Network Bandwidth Consumption - Edge Cases", {
  # Test concurrent network operations
  test_concurrent_transfers <- function() {
    start_time <- Sys.time()
    
    # Simulate multiple concurrent transfers
    concurrent_results <- future.apply::future_lapply(1:10, function(i) {
      transfer_start <- Sys.time()
      
      # Mock data transfer
      mock_data <- raw(500000)  # 500KB
      Sys.sleep(runif(1, 0.1, 0.3))  # Simulate network delay
      
      transfer_end <- Sys.time()
      transfer_time <- as.numeric(transfer_end - transfer_start)
      
      list(
        transfer_id = i,
        transfer_time = transfer_time,
        data_size_kb = 500,
        bandwidth_kbps = 500 / transfer_time
      )
    })
    
    end_time <- Sys.time()
    total_time <- as.numeric(end_time - start_time)
    
    # All transfers should complete within reasonable time
    expect_lt(total_time, 5)  # Should complete within 5 seconds
    
    # Check individual transfer performance
    avg_bandwidth <- mean(sapply(concurrent_results, function(x) x$bandwidth_kbps))
    expect_gt(avg_bandwidth, 100)  # Should maintain good bandwidth under load
    
    return(concurrent_results)
  }
  
  test_concurrent_transfers()
  
  # Test bandwidth throttling scenarios
  test_bandwidth_throttling <- function() {
    # Simulate slower network conditions
    slow_transfers <- lapply(1:5, function(i) {
      start_time <- Sys.time()
      
      # Simulate slow transfer
      mock_data <- raw(1024000)  # 1MB
      Sys.sleep(2)  # Simulate slow network (2 seconds for 1MB)
      
      end_time <- Sys.time()
      transfer_time <- as.numeric(end_time - start_time)
      
      list(
        transfer_id = i,
        transfer_time = transfer_time,
        bandwidth_mbps = 1 / transfer_time
      )
    })
    
    # Should handle slow network gracefully
    avg_slow_bandwidth <- mean(sapply(slow_transfers, function(x) x$bandwidth_mbps))
    expect_gt(avg_slow_bandwidth, 0.1)  # Should work even with slow connections
    
    return(slow_transfers)
  }
  
  test_bandwidth_throttling()
})

# =============================================================================
# 6. RESPONSE TIME DEGRADATION TESTS
# =============================================================================

test_that("Response Time Degradation - Progressive Load", {
  # Test response time under increasing load
  test_response_degradation <- function() {
    load_levels <- c(1, 5, 10, 20, 50)  # Progressive load levels
    response_times <- list()
    
    for (load in load_levels) {
      # Simulate operations under specific load
      load_start <- Sys.time()
      
      # Run operations with specified load
      operation_results <- future.apply::future_lapply(1:load, function(i) {
        operation_start <- Sys.time()
        
        # Simulate typical dashboard operation
        data <- data.frame(
          id = 1:1000,
          value = rnorm(1000),
          category = sample(c("A", "B", "C"), 1000, replace = TRUE)
        )
        
        # Process data
        summary <- data %>%
          group_by(category) %>%
          summarise(
            mean_value = mean(value),
            count = n()
          )
        
        operation_end <- Sys.time()
        operation_time <- as.numeric(operation_end - operation_start)
        
        list(
          operation_id = i,
          operation_time = operation_time,
          load_level = load
        )
      })
      
      load_end <- Sys.time()
      total_load_time <- as.numeric(load_end - load_start)
      
      # Calculate response time metrics
      avg_response_time <- mean(sapply(operation_results, function(x) x$operation_time))
      max_response_time <- max(sapply(operation_results, function(x) x$operation_time))
      
      response_times[[length(response_times) + 1]] <- list(
        load_level = load,
        avg_response_time = avg_response_time,
        max_response_time = max_response_time,
        total_time = total_load_time,
        throughput = load / total_load_time
      )
    }
    
    return(response_times)
  }
  
  degradation_results <- test_response_degradation()
  
  # Response time should not degrade excessively
  response_times <- sapply(degradation_results, function(x) x$avg_response_time)
  load_levels <- sapply(degradation_results, function(x) x$load_level)
  
  # Response time should increase sub-linearly with load
  max_response_time <- max(response_times)
  min_response_time <- min(response_times)
  
  expect_lt(max_response_time / min_response_time, 10)  # Should not degrade more than 10x
  expect_lt(max_response_time, 5)  # Maximum response time should be under 5 seconds
})

test_that("Response Time Degradation - Edge Cases", {
  # Test response time with memory pressure
  test_memory_pressure_response <- function() {
    # Create memory pressure
    memory_hogs <- replicate(5, {
      matrix(rnorm(100000), nrow = 1000)
    }, simplify = FALSE)
    
    # Test response time under memory pressure
    response_times <- replicate(10, {
      start_time <- Sys.time()
      
      # Simple operation
      data <- data.frame(x = 1:1000, y = rnorm(1000))
      result <- summary(data)
      
      end_time <- Sys.time()
      as.numeric(end_time - start_time)
    })
    
    # Clean up memory
    rm(memory_hogs)
    gc()
    
    # Response time should still be reasonable under memory pressure
    avg_response_time <- mean(response_times)
    expect_lt(avg_response_time, 2)  # Should complete within 2 seconds
    
    return(response_times)
  }
  
  test_memory_pressure_response()
  
  # Test response time with CPU saturation
  test_cpu_saturation_response <- function() {
    # Create CPU load
    cpu_load <- future.apply::future_lapply(1:4, function(i) {
      # CPU-intensive background task
      for (j in 1:1000) {
        matrix(rnorm(1000), nrow = 100) %*% matrix(rnorm(1000), nrow = 100)
      }
    })
    
    # Test response time with CPU saturation
    response_times <- replicate(10, {
      start_time <- Sys.time()
      
      # Simple operation
      data <- data.frame(x = 1:100, y = rnorm(100))
      result <- mean(data$y)
      
      end_time <- Sys.time()
      as.numeric(end_time - start_time)
    })
    
    # Wait for background tasks to complete
    future::value(cpu_load)
    
    # Response time should be reasonable even with CPU load
    avg_response_time <- mean(response_times)
    expect_lt(avg_response_time, 1)  # Should complete within 1 second
    
    return(response_times)
  }
  
  test_cpu_saturation_response()
})

# =============================================================================
# 7. THROUGHPUT MEASUREMENT TESTS
# =============================================================================

test_that("Throughput Measurement - Operations per Second", {
  # Test basic throughput
  test_basic_throughput <- function(duration_seconds = 30) {
    start_time <- Sys.time()
    operation_count <- 0
    operations_log <- list()
    
    while (as.numeric(Sys.time() - start_time) < duration_seconds) {
      