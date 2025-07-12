# ===================================================================
# ATLAS LABS HR ANALYTICS - SCALABILITY UNIT TESTS
# ===================================================================
# Focus Areas:
# 1. Module Communication (Microservices equivalent)
# 2. Reactive Rate Limiting (API rate limiting equivalent)
# 3. CDN Performance Validation (Asset delivery performance)
# ===================================================================

# Required Libraries
library(testthat)
library(shiny)
library(mockery)
library(httr)
library(microbenchmark)
library(future)
library(promises)
library(profvis)
library(curl)
library(jsonlite)

# ===================================================================
# 1. MODULE COMMUNICATION TESTING (Microservices Communication)
# ===================================================================

test_that("Module Communication - Basic Inter-Module Data Flow", {
  # Test Setup
  source("modules/data_loader_module.R")
  source("modules/attrition_module.R")
  source("modules/logger_module.R")
  
  # Mock session and input
  session <- MockShinySession$new()
  
  # Create test data
  test_employee_data <- data.frame(
    EmployeeID = 1:1000,
    Attrition = sample(c("Yes", "No"), 1000, replace = TRUE),
    Salary = rnorm(1000, 70000, 15000),
    Department = sample(c("IT", "HR", "Sales", "Finance"), 1000, replace = TRUE)
  )
  
  # Test data loader module output
  data_loader_output <- testServer(
    dataLoaderServer,
    args = list(id = "test_loader"),
    {
      # Simulate successful data load
      session$setInputs(file_upload = list(
        datapath = "test_data.csv",
        type = "text/csv"
      ))
      
      # Verify data structure
      expect_true(is.data.frame(loaded_data()))
      expect_gt(nrow(loaded_data()), 0)
      expect_true("EmployeeID" %in% names(loaded_data()))
    }
  )
  
  # Test attrition module receiving data
  attrition_output <- testServer(
    attritionServer,
    args = list(id = "test_attrition", data = reactive(test_employee_data)),
    {
      # Verify module receives and processes data
      expect_true(is.data.frame(data()))
      expect_equal(nrow(data()), 1000)
      
      # Test attrition calculations
      attrition_rate <- sum(data()$Attrition == "Yes") / nrow(data())
      expect_true(attrition_rate >= 0 && attrition_rate <= 1)
    }
  )
})

test_that("Module Communication - Bidirectional Data Exchange", {
  # Test bidirectional communication between modules
  shared_values <- reactiveValues(
    filtered_data = NULL,
    selected_filters = list(),
    analysis_results = NULL
  )
  
  # Mock filter updates from sidebar to analysis modules
  testServer(
    function(input, output, session) {
      # Simulate filter change
      observeEvent(input$department_filter, {
        shared_values$selected_filters$department <- input$department_filter
        shared_values$filtered_data <- filter_data(
          base_data(), 
          shared_values$selected_filters
        )
      })
      
      # Simulate analysis results back to report module
      observeEvent(shared_values$filtered_data, {
        if (!is.null(shared_values$filtered_data)) {
          shared_values$analysis_results <- calculate_metrics(
            shared_values$filtered_data
          )
        }
      })
    },
    {
      # Test filter propagation
      session$setInputs(department_filter = c("IT", "HR"))
      
      expect_equal(
        shared_values$selected_filters$department,
        c("IT", "HR")
      )
      
      # Verify filtered data is updated
      expect_true(!is.null(shared_values$filtered_data))
      expect_true(all(shared_values$filtered_data$Department %in% c("IT", "HR")))
    }
  )
})

test_that("Module Communication - Concurrent Module Operations", {
  # Test multiple modules processing simultaneously
  n_modules <- 5
  processing_times <- numeric(n_modules)
  
  # Simulate concurrent module operations
  test_data <- data.frame(
    EmployeeID = 1:10000,
    Salary = rnorm(10000, 70000, 15000),
    Performance = sample(1:5, 10000, replace = TRUE)
  )
  
  for (i in 1:n_modules) {
    processing_times[i] <- system.time({
      # Simulate heavy computation in module
      result <- test_data %>%
        dplyr::group_by(Performance) %>%
        dplyr::summarise(
          avg_salary = mean(Salary),
          count = n(),
          .groups = "drop"
        )
    })["elapsed"]
  }
  
  # Test that processing times are reasonable
  expect_true(all(processing_times < 1.0))  # Should complete in under 1 second
  expect_true(sd(processing_times) < 0.5)   # Consistent performance
})

test_that("Module Communication - Error Propagation and Recovery", {
  # Test error handling in module communication chain
  
  # Mock failing data loader
  failing_loader <- function() {
    stop("Simulated data loading error")
  }
  
  # Mock error-tolerant attrition module
  safe_attrition_processor <- function(data) {
    tryCatch({
      if (is.null(data) || nrow(data) == 0) {
        return(list(error = "No data available", fallback = TRUE))
      }
      return(list(attrition_rate = 0.15, error = NULL))
    }, error = function(e) {
      return(list(error = e$message, fallback = TRUE))
    })
  }
  
  # Test error propagation
  expect_error(failing_loader(), "Simulated data loading error")
  
  # Test graceful degradation
  result <- safe_attrition_processor(NULL)
  expect_true(result$fallback)
  expect_true(!is.null(result$error))
})

test_that("Module Communication - Message Queue Simulation", {
  # Simulate message queue for module communication
  message_queue <- list()
  
  # Mock message sender
  send_message <- function(from_module, to_module, message_type, payload) {
    message <- list(
      timestamp = Sys.time(),
      from = from_module,
      to = to_module,
      type = message_type,
      payload = payload,
      id = paste0("msg_", sample(1000:9999, 1))
    )
    message_queue <<- append(message_queue, list(message))
    return(message$id)
  }
  
  # Mock message receiver
  get_messages <- function(to_module, message_type = NULL) {
    messages <- Filter(function(msg) {
      msg$to == to_module && 
        (is.null(message_type) || msg$type == message_type)
    }, message_queue)
    return(messages)
  }
  
  # Test message sending
  msg_id <- send_message("data_loader", "attrition", "data_update", 
                        list(rows = 1000, status = "success"))
  
  expect_true(length(message_queue) == 1)
  expect_equal(message_queue[[1]]$from, "data_loader")
  expect_equal(message_queue[[1]]$to, "attrition")
  
  # Test message retrieval
  received_messages <- get_messages("attrition", "data_update")
  expect_equal(length(received_messages), 1)
  expect_equal(received_messages[[1]]$payload$rows, 1000)
})

# ===================================================================
# 2. REACTIVE RATE LIMITING TESTING (API Rate Limiting)
# ===================================================================

test_that("Rate Limiting - Basic Reactive Throttling", {
  # Create rate limiter for reactive expressions
  create_rate_limiter <- function(max_calls = 10, window_seconds = 60) {
    call_log <- list()
    
    function(reactive_expr) {
      current_time <- Sys.time()
      
      # Clean old calls outside window
      call_log <<- Filter(function(timestamp) {
        as.numeric(current_time - timestamp) <= window_seconds
      }, call_log)
      
      # Check rate limit
      if (length(call_log) >= max_calls) {
        warning("Rate limit exceeded. Skipping reactive update.")
        return(NULL)
      }
      
      # Log this call and execute
      call_log <<- append(call_log, list(current_time))
      return(reactive_expr())
    }
  }
  
  # Test rate limiter
  rate_limiter <- create_rate_limiter(max_calls = 5, window_seconds = 10)
  
  # Simulate rapid reactive updates
  results <- list()
  for (i in 1:8) {
    result <- rate_limiter(function() paste("Update", i))
    results <- append(results, list(result))
  }
  
  # Verify rate limiting worked
  non_null_results <- Filter(function(x) !is.null(x), results)
  expect_true(length(non_null_results) <= 5)
  
  # Test warning generation
  expect_warning(
    rate_limiter(function() "Should be rate limited"),
    "Rate limit exceeded"
  )
})

test_that("Rate Limiting - Adaptive Rate Control", {
  # Advanced rate limiter with adaptive control
  adaptive_rate_limiter <- function(initial_rate = 10, performance_threshold = 0.5) {
    call_log <- list()
    current_rate <- initial_rate
    performance_history <- numeric(0)
    
    function(reactive_expr) {
      start_time <- Sys.time()
      
      # Execute and measure performance
      result <- tryCatch({
        reactive_expr()
      }, error = function(e) {
        list(error = e$message, execution_time = NA)
      })
      
      execution_time <- as.numeric(Sys.time() - start_time)
      performance_history <<- tail(c(performance_history, execution_time), 20)
      
      # Adapt rate based on performance
      avg_performance <- mean(performance_history, na.rm = TRUE)
      if (!is.na(avg_performance)) {
        if (avg_performance > performance_threshold) {
          current_rate <<- max(1, current_rate * 0.8)  # Reduce rate
        } else {
          current_rate <<- min(50, current_rate * 1.1)  # Increase rate
        }
      }
      
      # Apply current rate limit
      current_time <- Sys.time()
      call_log <<- Filter(function(timestamp) {
        as.numeric(current_time - timestamp) <= 60
      }, call_log)
      
      if (length(call_log) >= current_rate) {
        return(list(
          result = NULL, 
          rate_limited = TRUE, 
          current_rate = current_rate
        ))
      }
      
      call_log <<- append(call_log, list(current_time))
      return(list(
        result = result, 
        rate_limited = FALSE, 
        current_rate = current_rate
      ))
    }
  }
  
  # Test adaptive rate control
  limiter <- adaptive_rate_limiter(initial_rate = 5, performance_threshold = 0.1)
  
  # Simulate slow operations
  slow_results <- replicate(3, {
    limiter(function() {
      Sys.sleep(0.2)  # Simulate slow operation
      return("slow_result")
    })
  }, simplify = FALSE)
  
  # Verify rate was reduced due to poor performance
  final_rate <- slow_results[[length(slow_results)]]$current_rate
  expect_true(final_rate < 5)
})

test_that("Rate Limiting - Burst Control", {
  # Test burst detection and control
  burst_controller <- function(max_burst = 3, burst_window = 1) {
    burst_log <- list()
    
    function(reactive_expr) {
      current_time <- Sys.time()
      
      # Clean old burst entries
      burst_log <<- Filter(function(timestamp) {
        as.numeric(current_time - timestamp) <= burst_window
      }, burst_log)
      
      # Check for burst
      if (length(burst_log) >= max_burst) {
        return(list(
          result = NULL,
          burst_detected = TRUE,
          burst_count = length(burst_log)
        ))
      }
      
      # Log this call
      burst_log <<- append(burst_log, list(current_time))
      
      return(list(
        result = reactive_expr(),
        burst_detected = FALSE,
        burst_count = length(burst_log)
      ))
    }
  }
  
  # Test burst control
  controller <- burst_controller(max_burst = 3, burst_window = 2)
  
  # Simulate burst of requests
  burst_results <- replicate(5, {
    controller(function() "burst_test")
  }, simplify = FALSE)
  
  # Verify burst was detected
  burst_detected <- any(sapply(burst_results, function(x) x$burst_detected))
  expect_true(burst_detected)
  
  # Verify some requests were blocked
  blocked_requests <- sum(sapply(burst_results, function(x) x$burst_detected))
  expect_true(blocked_requests >= 2)
})

test_that("Rate Limiting - User-Specific Rate Control", {
  # Test user-specific rate limiting
  user_rate_limiter <- function(rates_per_user = list(default = 10)) {
    user_logs <- list()
    
    function(user_id, reactive_expr) {
      user_rate <- rates_per_user[[user_id]] %||% rates_per_user$default
      
      if (is.null(user_logs[[user_id]])) {
        user_logs[[user_id]] <<- list()
      }
      
      current_time <- Sys.time()
      user_logs[[user_id]] <<- Filter(function(timestamp) {
        as.numeric(current_time - timestamp) <= 60
      }, user_logs[[user_id]])
      
      if (length(user_logs[[user_id]]) >= user_rate) {
        return(list(
          result = NULL,
          rate_limited = TRUE,
          user_rate = user_rate,
          user_calls = length(user_logs[[user_id]])
        ))
      }
      
      user_logs[[user_id]] <<- append(user_logs[[user_id]], list(current_time))
      
      return(list(
        result = reactive_expr(),
        rate_limited = FALSE,
        user_rate = user_rate,
        user_calls = length(user_logs[[user_id]])
      ))
    }
  }
  
  # Test user-specific limits
  limiter <- user_rate_limiter(rates_per_user = list(
    admin = 50,
    premium = 25,
    default = 10
  ))
  
  # Test different user types
  admin_results <- replicate(12, {
    limiter("admin", function() "admin_query")
  }, simplify = FALSE)
  
  regular_results <- replicate(12, {
    limiter("regular_user", function() "regular_query")
  }, simplify = FALSE)
  
  # Verify admin has higher limits
  admin_blocked <- sum(sapply(admin_results, function(x) x$rate_limited))
  regular_blocked <- sum(sapply(regular_results, function(x) x$rate_limited))
  
  expect_true(admin_blocked < regular_blocked)
})

# ===================================================================
# 3. CDN PERFORMANCE VALIDATION TESTING
# ===================================================================

test_that("CDN Performance - Asset Loading Speed", {
  # Test CDN asset loading performance
  cdn_base_url <- "https://cdnjs.cloudflare.com/ajax/libs/"
  
  # Test critical assets
  critical_assets <- c(
    "jquery/3.6.0/jquery.min.js",
    "bootstrap/5.1.3/js/bootstrap.bundle.min.js",
    "bootstrap/5.1.3/css/bootstrap.min.css",
    "plotly.js/2.18.2/plotly.min.js"
  )
  
  # Function to test asset loading
  test_asset_loading <- function(asset_path) {
    url <- paste0(cdn_base_url, asset_path)
    
    start_time <- Sys.time()
    response <- tryCatch({
      httr::GET(url, httr::timeout(10))
    }, error = function(e) {
      return(list(error = e$message, status_code = 0))
    })
    
    load_time <- as.numeric(Sys.time() - start_time)
    
    if ("error" %in% names(response)) {
      return(list(
        asset = asset_path,
        success = FALSE,
        error = response$error,
        load_time = load_time
      ))
    }
    
    return(list(
      asset = asset_path,
      success = httr::status_code(response) == 200,
      status_code = httr::status_code(response),
      load_time = load_time,
      size_bytes = length(httr::content(response, "raw"))
    ))
  }
  
  # Test all critical assets
  asset_results <- lapply(critical_assets, test_asset_loading)
  
  # Verify all assets loaded successfully
  all_successful <- all(sapply(asset_results, function(x) x$success))
  expect_true(all_successful, info = "All CDN assets should load successfully")
  
  # Verify reasonable loading times (under 5 seconds)
  load_times <- sapply(asset_results, function(x) x$load_time)
  expect_true(all(load_times < 5), info = "All assets should load within 5 seconds")
  
  # Test average loading time
  avg_load_time <- mean(load_times)
  expect_true(avg_load_time < 2, info = "Average load time should be under 2 seconds")
})

test_that("CDN Performance - Geographic Distribution", {
  # Test CDN performance from different geographic locations
  # This simulates testing from different regions
  
  test_regions <- list(
    us_east = list(latency_sim = 0.05, bandwidth_sim = 1000),
    us_west = list(latency_sim = 0.08, bandwidth_sim = 800),
    europe = list(latency_sim = 0.12, bandwidth_sim = 600),
    asia = list(latency_sim = 0.15, bandwidth_sim = 500)
  )
  
  simulate_regional_performance <- function(region_config, asset_size_mb = 1) {
    # Simulate network conditions
    base_load_time <- asset_size_mb / (region_config$bandwidth_sim / 1000)  # MB/s
    network_latency <- region_config$latency_sim
    
    # Add some randomness to simulate real-world conditions
    jitter <- runif(1, -0.1, 0.1)
    
    total_time <- base_load_time + network_latency + jitter
    
    return(list(
      load_time = total_time,
      effective_bandwidth = asset_size_mb / total_time,
      latency = network_latency
    ))
  }
  
  # Test performance across regions
  regional_results <- lapply(names(test_regions), function(region) {
    config <- test_regions[[region]]
    result <- simulate_regional_performance(config, asset_size_mb = 0.5)
    result$region <- region
    return(result)
  })
  
  # Verify performance consistency
  load_times <- sapply(regional_results, function(x) x$load_time)
  
  # All regions should have reasonable performance
  expect_true(all(load_times < 3), info = "All regions should load within 3 seconds")
  
  # Performance variation should be reasonable
  performance_cv <- sd(load_times) / mean(load_times)
  expect_true(performance_cv < 0.5, info = "Performance variation should be reasonable")
})

test_that("CDN Performance - Cache Effectiveness", {
  # Test CDN cache hit rates and effectiveness
  
  # Simulate cache testing
  simulate_cache_test <- function(n_requests = 100, cache_hit_rate = 0.85) {
    cache_hits <- rbinom(n_requests, 1, cache_hit_rate)
    
    # Simulate response times (cache hits are much faster)
    response_times <- ifelse(cache_hits, 
                            runif(n_requests, 0.01, 0.05),  # Cache hit: 10-50ms
                            runif(n_requests, 0.2, 0.8))    # Cache miss: 200-800ms
    
    return(list(
      cache_hits = sum(cache_hits),
      cache_misses = sum(1 - cache_hits),
      cache_hit_rate = mean(cache_hits),
      avg_response_time = mean(response_times),
      median_response_time = median(response_times)
    ))
  }
  
  # Test cache performance
  cache_results <- simulate_cache_test(n_requests = 1000, cache_hit_rate = 0.9)
  
  # Verify cache hit rate
  expect_true(cache_results$cache_hit_rate > 0.8, 
              info = "Cache hit rate should be above 80%")
  
  # Verify response times
  expect_true(cache_results$avg_response_time < 0.2, 
              info = "Average response time should be under 200ms")
  
  # Test cache warming
  cache_warm_test <- function(popular_assets = 10, warm_rate = 0.95) {
    # Simulate popular assets being pre-warmed in cache
    warm_assets <- rbinom(popular_assets, 1, warm_rate)
    
    return(list(
      warm_assets = sum(warm_assets),
      warm_rate = mean(warm_assets)
    ))
  }
  
  warm_results <- cache_warm_test(popular_assets = 20, warm_rate = 0.9)
  expect_true(warm_results$warm_rate > 0.85, 
              info = "Cache warm rate should be above 85%")
})

test_that("CDN Performance - Load Testing", {
  # Test CDN performance under load
  
  simulate_load_test <- function(concurrent_users = 100, duration_seconds = 60) {
    # Simulate concurrent requests
    request_times <- seq(0, duration_seconds, length.out = concurrent_users)
    
    # Simulate response times with load-based degradation
    base_response_time <- 0.1  # 100ms base
    load_factor <- pmax(1, concurrent_users / 50)  # Degradation factor
    
    response_times <- base_response_time * load_factor * runif(concurrent_users, 0.8, 1.2)
    
    # Calculate throughput
    requests_per_second <- concurrent_users / duration_seconds
    
    # Simulate error rate under load
    error_rate <- pmax(0, (concurrent_users - 100) / 1000)  # Errors start at 100+ users
    errors <- rbinom(concurrent_users, 1, error_rate)
    
    return(list(
      concurrent_users = concurrent_users,
      avg_response_time = mean(response_times),
      p95_response_time = quantile(response_times, 0.95),
      p99_response_time = quantile(response_times, 0.99),
      throughput = requests_per_second,
      error_rate = mean(errors),
      total_errors = sum(errors)
    ))
  }
  
  # Test different load levels
  load_levels <- c(10, 50, 100, 200, 500)
  load_results <- lapply(load_levels, function(users) {
    simulate_load_test(concurrent_users = users, duration_seconds = 30)
  })
  
  # Verify performance under load
  for (i in seq_along(load_results)) {
    result <- load_results[[i]]
    
    # Response times should remain reasonable
    expect_true(result$avg_response_time < 2, 
                info = paste("Average response time should be under 2s for", 
                           result$concurrent_users, "users"))
    
    # Error rate should be manageable
    expect_true(result$error_rate < 0.1, 
                info = paste("Error rate should be under 10% for", 
                           result$concurrent_users, "users"))
  }
  
  # Test performance degradation is gradual
  response_times <- sapply(load_results, function(x) x$avg_response_time)
  
  # Response times should increase gradually, not spike
  for (i in 2:length(response_times)) {
    degradation_factor <- response_times[i] / response_times[i-1]
    expect_true(degradation_factor < 3, 
                info = "Response time degradation should be gradual")
  }
})

test_that("CDN Performance - Failover and Redundancy", {
  # Test CDN failover scenarios
  
  simulate_failover_test <- function(primary_availability = 0.99, 
                                   failover_time = 0.5, 
                                   secondary_availability = 0.95) {
    
    n_requests <- 1000
    
    # Simulate primary CDN availability
    primary_up <- rbinom(n_requests, 1, primary_availability)
    
    # Simulate failover for failed requests
    failover_requests <- which(primary_up == 0)
    secondary_success <- rbinom(length(failover_requests), 1, secondary_availability)
    
    # Calculate response times
    response_times <- numeric(n_requests)
    response_times[primary_up == 1] <- runif(sum(primary_up), 0.05, 0.15)  # Primary fast
    
    # Failover requests take longer
    failover_successful <- failover_requests[secondary_success == 1]
    failover_failed <- failover_requests[secondary_success == 0]
    
    response_times[failover_successful] <- failover_time + runif(length(failover_successful), 0.1, 0.3)
    response_times[failover_failed] <- NA  # Failed requests
    
    return(list(
      total_requests = n_requests,
      primary_success = sum(primary_up),
      failover_success = sum(secondary_success),
      total_failures = length(failover_failed),
      overall_success_rate = (sum(primary_up) + sum(secondary_success)) / n_requests,
      avg_response_time = mean(response_times, na.rm = TRUE),
      failover_rate = length(failover_requests) / n_requests
    ))
  }
  
  # Test failover scenarios
  failover_results <- simulate_failover_test(
    primary_availability = 0.98,
    failover_time = 0.3,
    secondary_availability = 0.95
  )
  
  # Verify failover effectiveness
  expect_true(failover_results$overall_success_rate > 0.95, 
              info = "Overall success rate should be above 95% with failover")
  
  expect_true(failover_results$avg_response_time < 1, 
              info = "Average response time should remain under 1s with failover")
  
  # Test multiple failover scenarios
  scenarios <- list(
    high_availability = list(primary = 0.99, secondary = 0.98),
    medium_availability = list(primary = 0.95, secondary = 0.93),
    low_availability = list(primary = 0.90, secondary = 0.88)
  )
  
  scenario_results <- lapply(scenarios, function(scenario) {
    simulate_failover_test(
      primary_availability = scenario$primary,
      secondary_availability = scenario$secondary
    )
  })
  
  # All scenarios should maintain reasonable success rates
  success_rates <- sapply(scenario_results, function(x) x$overall_success_rate)
  expect_true(all(success_rates > 0.9), 
              info = "All failover scenarios should maintain 90%+ success rate")
})

# ===================================================================
# INTEGRATION TESTS - Combined Scalability Scenarios
# ===================================================================

test_that("Integration - High Load with Rate Limiting and CDN", {
  # Comprehensive test combining all three areas
  
  # Setup integrated test environment
  integrated_test <- function(concurrent_users = 100, 
                             rate_limit = 50, 
                             cdn_latency = 0.1) {
    
    # Simulate user requests with rate limiting
    rate_limiter <- create_rate_limiter(max_calls = rate_limit, window_seconds = 60)
    
    # Simulate CDN performance
    cdn_performance <- function() {
      # Simulate CDN response time
      base_latency <- cdn_latency
      load_factor <- pmax(1, concurrent_users / 50)
      return(base_latency * load_factor * runif(1, 0.8, 1.2))
    }
    
    # Simulate module communication under load
    module_performance <- function() {
      # Simulate inter-module communication delay
      base_delay <- 0.02  # 20ms base
      load_factor <- pmax(1, concurrent_users / 100)
      return(base_delay * load_factor * runif(1, 0.9, 1.1))
    }
    
    # Run integrated test
    results <- list()
    
    for (i in 1:concurrent_users) {
      # Test rate limiting
      rate_limited <- length(Filter(function(x) !is.null(x), results)) >= rate_limit
      
      if (!rate_limited) {
        # Simulate full request cycle
        cdn_time <- cdn_performance()
        module_time <- module_performance()
        total_time <- cdn_time + module_time
        
        results[[i]] <- list(
          user_id = i,
          cdn_time = cdn_time,
          module_time = module_time,
          total_time = total_time,
          rate_limited = FALSE
        )
      } else {
        results[[i]] <- list(
          user_id = i,
          rate_limited = TRUE
        )
      }
    }
    
    return(results)
  }
  
  # Run integrated test
  integrated_results <- integrated_test(
    concurrent_users = 150,
    rate_limit = 100,
    cdn_latency = 0.08
  )
  
  # Analyze results
  successful_requests <- Filter(function(x) !x$