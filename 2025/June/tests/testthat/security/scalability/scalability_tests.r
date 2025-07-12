# =============================================================================
# ATLAS LABS HR ANALYTICS - SCALABILITY UNIT TESTS
# Comprehensive testing for horizontal/vertical scaling, auto-scaling, 
# resource optimization, container orchestration, microservices, API rate limiting, CDN
# =============================================================================

# Required Libraries
library(testthat)
library(shiny)
library(httr)
library(jsonlite)
library(parallel)
library(future)
library(promises)
library(R6)
library(microbenchmark)
library(pryr)
library(curl)
library(mockery)

# =============================================================================
# 1. HORIZONTAL SCALING VALIDATION TESTS
# =============================================================================

test_that("Horizontal Scaling - Load Distribution", {
  
  # Test concurrent user handling across multiple instances
  test_concurrent_users <- function(num_users = 100, num_instances = 3) {
    instance_urls <- paste0("http://localhost:", 3838:(3838 + num_instances - 1))
    
    # Simulate concurrent requests
    results <- future_map(1:num_users, function(user_id) {
      instance_url <- instance_urls[((user_id - 1) %% num_instances) + 1]
      
      tryCatch({
        response <- GET(paste0(instance_url, "/health"))
        list(
          user_id = user_id,
          instance = instance_url,
          status_code = status_code(response),
          response_time = response$times[["total"]]
        )
      }, error = function(e) {
        list(user_id = user_id, instance = instance_url, error = e$message)
      })
    })
    
    # Validate load distribution
    successful_requests <- results[sapply(results, function(x) !is.null(x$status_code))]
    expect_true(length(successful_requests) >= num_users * 0.95) # 95% success rate
    
    # Check load balancing effectiveness
    instance_counts <- table(sapply(successful_requests, function(x) x$instance))
    expect_true(max(instance_counts) - min(instance_counts) <= 2) # Even distribution
  }
  
  # Edge Cases
  test_concurrent_users(10, 2)    # Low load
  test_concurrent_users(500, 5)   # High load
  test_concurrent_users(1000, 10) # Extreme load
  
  # Test session affinity
  test_session_affinity <- function() {
    user_sessions <- replicate(50, {
      session_id <- paste0("session_", sample(1000:9999, 1))
      
      # Make multiple requests with same session
      responses <- map(1:5, function(i) {
        GET("http://localhost:3838/data", 
            add_headers("X-Session-ID" = session_id))
      })
      
      # All requests should hit same instance
      instances <- map_chr(responses, function(r) headers(r)$`X-Instance-ID`)
      length(unique(instances)) == 1
    })
    
    expect_true(mean(user_sessions) >= 0.95) # 95% session affinity
  }
  
  test_session_affinity()
  
  # Test failover scenarios
  test_failover <- function() {
    # Simulate instance failure
    instance_down <- "http://localhost:3839"
    
    # Health check should detect failure
    expect_error(GET(paste0(instance_down, "/health")), timeout = 5)
    
    # Traffic should redirect to healthy instances
    healthy_responses <- map(1:20, function(i) {
      GET("http://localhost:3838/dashboard")
    })
    
    success_rate <- mean(map_dbl(healthy_responses, status_code) == 200)
    expect_true(success_rate >= 0.95)
  }
  
  test_failover()
})

test_that("Horizontal Scaling - State Management", {
  
  # Test shared state across instances
  test_shared_state <- function() {
    # Create shared data on instance 1
    shared_data <- list(
      user_id = "test_user",
      filters = list(department = "HR", year = 2024),
      timestamp = Sys.time()
    )
    
    POST("http://localhost:3838/api/state", 
         body = toJSON(shared_data), 
         content_type("application/json"))
    
    # Retrieve from instance 2
    response <- GET("http://localhost:3839/api/state?user_id=test_user")
    retrieved_data <- fromJSON(content(response, "text"))
    
    expect_equal(retrieved_data$user_id, shared_data$user_id)
    expect_equal(retrieved_data$filters, shared_data$filters)
  }
  
  test_shared_state()
  
  # Test cache synchronization
  test_cache_sync <- function() {
    # Update cache on instance 1
    cache_data <- list(
      key = "employee_summary",
      data = list(total = 1000, active = 950),
      ttl = 3600
    )
    
    PUT("http://localhost:3838/api/cache", 
        body = toJSON(cache_data),
        content_type("application/json"))
    
    # Check cache consistency across instances
    instances <- c("http://localhost:3838", "http://localhost:3839", "http://localhost:3840")
    
    cache_responses <- map(instances, function(url) {
      response <- GET(paste0(url, "/api/cache?key=employee_summary"))
      fromJSON(content(response, "text"))
    })
    
    # All instances should have same cached data
    expect_true(all(map_dbl(cache_responses, ~ .$data$total) == 1000))
  }
  
  test_cache_sync()
})

# =============================================================================
# 2. VERTICAL SCALING EFFICIENCY TESTS
# =============================================================================

test_that("Vertical Scaling - Resource Utilization", {
  
  # Test CPU scaling efficiency
  test_cpu_scaling <- function() {
    # Baseline performance with 2 cores
    baseline_time <- system.time({
      result <- mclapply(1:100, function(x) {
        # Simulate CPU-intensive task
        sum(rnorm(10000))
      }, mc.cores = 2)
    })
    
    # Performance with 4 cores
    scaled_time <- system.time({
      result <- mclapply(1:100, function(x) {
        sum(rnorm(10000))
      }, mc.cores = 4)
    })
    
    # Should see near-linear scaling
    efficiency <- (baseline_time[3] / scaled_time[3]) / 2
    expect_true(efficiency >= 0.8) # 80% efficiency threshold
  }
  
  test_cpu_scaling()
  
  # Test memory scaling
  test_memory_scaling <- function() {
    # Test with increasing memory allocation
    memory_tests <- map(c(1, 2, 4, 8), function(gb) {
      gc() # Clean up before test
      
      # Allocate memory
      data_size <- gb * 1024^3 / 8 # Convert GB to number of doubles
      
      start_time <- Sys.time()
      start_mem <- pryr::mem_used()
      
      # Create large dataset
      large_data <- matrix(rnorm(sqrt(data_size)), nrow = sqrt(data_size))
      
      # Perform operations
      result <- apply(large_data, 1, mean)
      
      end_time <- Sys.time()
      end_mem <- pryr::mem_used()
      
      list(
        gb = gb,
        duration = as.numeric(end_time - start_time),
        memory_used = as.numeric(end_mem - start_mem),
        efficiency = data_size / as.numeric(end_time - start_time)
      )
    })
    
    # Memory efficiency should improve with more RAM
    efficiencies <- map_dbl(memory_tests, ~ .$efficiency)
    expect_true(all(diff(efficiencies) >= 0)) # Non-decreasing efficiency
  }
  
  test_memory_scaling()
  
  # Test I/O scaling with SSD vs HDD
  test_io_scaling <- function() {
    # Create test data
    test_data <- data.frame(
      employee_id = 1:100000,
      salary = rnorm(100000, 50000, 15000),
      department = sample(c("HR", "IT", "Finance", "Sales"), 100000, replace = TRUE)
    )
    
    # Test write performance
    ssd_write_time <- system.time({
      write.csv(test_data, "/tmp/ssd_test.csv", row.names = FALSE)
    })
    
    # Test read performance
    ssd_read_time <- system.time({
      read_data <- read.csv("/tmp/ssd_test.csv")
    })
    
    # SSD should outperform HDD (if available)
    expect_true(ssd_write_time[3] < 5) # Should complete within 5 seconds
    expect_true(ssd_read_time[3] < 3)  # Should read within 3 seconds
  }
  
  test_io_scaling()
})

test_that("Vertical Scaling - Resource Monitoring", {
  
  # Test real-time resource monitoring
  test_resource_monitoring <- function() {
    # Monitor CPU usage during load
    cpu_monitor <- function(duration = 10) {
      start_time <- Sys.time()
      cpu_readings <- list()
      
      while (as.numeric(Sys.time() - start_time) < duration) {
        # Simulate CPU load
        temp <- sum(rnorm(1000))
        
        # Record CPU usage (simplified)
        cpu_readings <- append(cpu_readings, list(list(
          time = Sys.time(),
          usage = runif(1, 0.5, 0.9) # Simulated CPU usage
        )))
        
        Sys.sleep(0.1)
      }
      
      cpu_readings
    }
    
    readings <- cpu_monitor(5)
    expect_true(length(readings) > 40) # Should have multiple readings
    
    # Check for CPU usage spikes
    usage_values <- map_dbl(readings, ~ .$usage)
    expect_true(max(usage_values) < 1.0) # Should not exceed 100%
  }
  
  test_resource_monitoring()
  
  # Test memory leak detection
  test_memory_leak_detection <- function() {
    initial_memory <- pryr::mem_used()
    
    # Simulate operations that might cause memory leaks
    for (i in 1:100) {
      # Create and destroy objects
      temp_data <- matrix(rnorm(10000), nrow = 100)
      temp_result <- apply(temp_data, 1, sum)
      
      # Force garbage collection periodically
      if (i %% 10 == 0) gc()
    }
    
    final_memory <- pryr::mem_used()
    memory_growth <- as.numeric(final_memory - initial_memory)
    
    # Memory growth should be minimal
    expect_true(memory_growth < 1024^2) # Less than 1MB growth
  }
  
  test_memory_leak_detection()
})

# =============================================================================
# 3. AUTO-SCALING TRIGGER ACCURACY TESTS
# =============================================================================

test_that("Auto-scaling - Trigger Mechanisms", {
  
  # Test CPU-based auto-scaling
  test_cpu_triggers <- function() {
    # Simulate CPU threshold monitoring
    cpu_thresholds <- list(
      scale_up = 0.8,
      scale_down = 0.3,
      min_instances = 2,
      max_instances = 10
    )
    
    # Test scale-up trigger
    high_cpu_readings <- rep(0.85, 5) # 5 consecutive high readings
    
    trigger_decision <- function(readings, thresholds) {
      if (all(readings >= thresholds$scale_up)) {
        return("scale_up")
      } else if (all(readings <= thresholds$scale_down)) {
        return("scale_down")
      } else {
        return("no_action")
      }
    }
    
    decision <- trigger_decision(high_cpu_readings, cpu_thresholds)
    expect_equal(decision, "scale_up")
    
    # Test scale-down trigger
    low_cpu_readings <- rep(0.25, 5)
    decision <- trigger_decision(low_cpu_readings, cpu_thresholds)
    expect_equal(decision, "scale_down")
    
    # Test no action zone
    moderate_cpu_readings <- rep(0.5, 5)
    decision <- trigger_decision(moderate_cpu_readings, cpu_thresholds)
    expect_equal(decision, "no_action")
  }
  
  test_cpu_triggers()
  
  # Test memory-based auto-scaling
  test_memory_triggers <- function() {
    memory_thresholds <- list(
      scale_up = 0.85,
      scale_down = 0.4,
      min_instances = 2,
      max_instances = 8
    )
    
    # Simulate memory pressure scenarios
    memory_scenarios <- list(
      high_pressure = rep(0.90, 3),
      low_pressure = rep(0.35, 3),
      moderate_pressure = rep(0.60, 3)
    )
    
    results <- map(memory_scenarios, function(readings) {
      if (all(readings >= memory_thresholds$scale_up)) {
        "scale_up"
      } else if (all(readings <= memory_thresholds$scale_down)) {
        "scale_down"
      } else {
        "no_action"
      }
    })
    
    expect_equal(results$high_pressure, "scale_up")
    expect_equal(results$low_pressure, "scale_down")
    expect_equal(results$moderate_pressure, "no_action")
  }
  
  test_memory_triggers()
  
  # Test response time-based triggers
  test_response_time_triggers <- function() {
    response_thresholds <- list(
      scale_up_ms = 2000,
      scale_down_ms = 500,
      consecutive_readings = 3
    )
    
    # Test slow response trigger
    slow_responses <- c(2100, 2200, 2050) # 3 consecutive slow responses
    should_scale_up <- all(slow_responses >= response_thresholds$scale_up_ms)
    expect_true(should_scale_up)
    
    # Test fast response trigger
    fast_responses <- c(400, 450, 380)
    should_scale_down <- all(fast_responses <= response_thresholds$scale_down_ms)
    expect_true(should_scale_down)
  }
  
  test_response_time_triggers()
})

test_that("Auto-scaling - Edge Cases", {
  
  # Test flapping prevention
  test_flapping_prevention <- function() {
    # Simulate oscillating metrics
    cpu_readings <- c(0.85, 0.25, 0.82, 0.28, 0.87, 0.22) # Oscillating pattern
    
    # Implement cooldown mechanism
    cooldown_seconds <- 300 # 5 minutes
    last_scale_action <- Sys.time() - 100 # 100 seconds ago
    
    can_scale <- function(last_action, cooldown) {
      as.numeric(Sys.time() - last_action) >= cooldown
    }
    
    # Should prevent scaling due to recent action
    expect_false(can_scale(last_scale_action, cooldown_seconds))
    
    # Should allow scaling after cooldown
    old_action <- Sys.time() - 400
    expect_true(can_scale(old_action, cooldown_seconds))
  }
  
  test_flapping_prevention()
  
  # Test burst traffic handling
  test_burst_traffic <- function() {
    # Simulate sudden traffic spike
    normal_requests <- rep(100, 10)    # Normal: 100 req/min
    burst_requests <- rep(1000, 5)     # Burst: 1000 req/min
    
    # Auto-scaling should handle burst
    scaling_decision <- function(requests) {
      if (any(requests > 500)) {
        instances_needed <- ceiling(max(requests) / 200) # 200 req/instance
        return(min(instances_needed, 10)) # Cap at 10 instances
      }
      return(2) # Minimum instances
    }
    
    normal_instances <- scaling_decision(normal_requests)
    burst_instances <- scaling_decision(burst_requests)
    
    expect_equal(normal_instances, 2)
    expect_equal(burst_instances, 5) # Should scale up for burst
  }
  
  test_burst_traffic()
  
  # Test resource constraint scenarios
  test_resource_constraints <- function() {
    # Test scaling with resource limits
    resource_limits <- list(
      max_cpu_cores = 32,
      max_memory_gb = 128,
      max_instances = 8
    )
    
    # Calculate optimal scaling
    calculate_scaling <- function(demand, limits) {
      cpu_needed <- ceiling(demand$cpu_usage * demand$instances)
      memory_needed <- ceiling(demand$memory_usage * demand$instances)
      
      # Check constraints
      max_by_cpu <- floor(limits$max_cpu_cores / demand$cpu_usage)
      max_by_memory <- floor(limits$max_memory_gb / demand$memory_usage)
      
      min(demand$instances, max_by_cpu, max_by_memory, limits$max_instances)
    }
    
    # High demand scenario
    high_demand <- list(
      cpu_usage = 4,    # 4 cores per instance
      memory_usage = 16, # 16GB per instance
      instances = 12     # Desired instances
    )
    
    optimal_instances <- calculate_scaling(high_demand, resource_limits)
    expect_equal(optimal_instances, 8) # Limited by max_instances
  }
  
  test_resource_constraints()
})

# =============================================================================
# 4. RESOURCE ALLOCATION OPTIMIZATION TESTS
# =============================================================================

test_that("Resource Allocation - CPU Optimization", {
  
  # Test CPU affinity optimization
  test_cpu_affinity <- function() {
    # Test different CPU binding strategies
    strategies <- list(
      round_robin = function(processes, cpus) {
        map(seq_along(processes), function(i) {
          (i - 1) %% length(cpus) + 1
        })
      },
      
      load_balanced = function(processes, cpus) {
        # Simulate load-based assignment
        cpu_loads <- rep(0, length(cpus))
        assignments <- numeric(length(processes))
        
        for (i in seq_along(processes)) {
          min_load_cpu <- which.min(cpu_loads)
          assignments[i] <- min_load_cpu
          cpu_loads[min_load_cpu] <- cpu_loads[min_load_cpu] + processes[i]
        }
        
        assignments
      }
    )
    
    processes <- c(0.3, 0.5, 0.2, 0.8, 0.4) # Process CPU requirements
    cpus <- 1:4 # Available CPUs
    
    # Test strategies
    rr_assignment <- strategies$round_robin(processes, cpus)
    lb_assignment <- strategies$load_balanced(processes, cpus)
    
    # Load balanced should distribute better
    rr_loads <- sapply(1:4, function(cpu) {
      sum(processes[rr_assignment == cpu])
    })
    
    lb_loads <- sapply(1:4, function(cpu) {
      sum(processes[lb_assignment == cpu])
    })
    
    # Load balanced should have lower variance
    expect_true(var(lb_loads) <= var(rr_loads))
  }
  
  test_cpu_affinity()
  
  # Test dynamic CPU allocation
  test_dynamic_cpu_allocation <- function() {
    # Simulate workload changes
    workload_patterns <- list(
      morning_rush = c(0.9, 0.8, 0.7, 0.6),
      afternoon_lull = c(0.3, 0.2, 0.4, 0.3),
      evening_peak = c(0.8, 0.9, 0.85, 0.9)
    )
    
    # Dynamic allocation function
    allocate_cpu <- function(pattern, total_cpus = 8) {
      normalized_pattern <- pattern / sum(pattern)
      allocation <- pmax(1, round(normalized_pattern * total_cpus))
      
      # Ensure total doesn't exceed available CPUs
      while (sum(allocation) > total_cpus) {
        max_idx <- which.max(allocation)
        allocation[max_idx] <- allocation[max_idx] - 1
      }
      
      allocation
    }
    
    allocations <- map(workload_patterns, allocate_cpu)
    
    # Each allocation should use available CPUs efficiently
    expect_true(all(map_dbl(allocations, sum) <= 8))
    expect_true(all(map_dbl(allocations, min) >= 1))
  }
  
  test_dynamic_cpu_allocation()
})

test_that("Resource Allocation - Memory Optimization", {
  
  # Test memory pool management
  test_memory_pools <- function() {
    # Simulate memory pool allocation
    memory_pools <- list(
      small_objects = list(size = 1024^2, count = 0, max_count = 1000),
      medium_objects = list(size = 10 * 1024^2, count = 0, max_count = 100),
      large_objects = list(size = 100 * 1024^2, count = 0, max_count = 10)
    )
    
    # Memory allocation function
    allocate_memory <- function(request_size, pools) {
      # Find appropriate pool
      suitable_pools <- pools[map_dbl(pools, ~ .$size) >= request_size]
      
      if (length(suitable_pools) == 0) {
        return(NULL) # No suitable pool
      }
      
      # Choose smallest suitable pool
      best_pool <- suitable_pools[[which.min(map_dbl(suitable_pools, ~ .$size))]]
      
      # Check availability
      if (best_pool$count < best_pool$max_count) {
        return(best_pool$size)
      }
      
      NULL # Pool exhausted
    }
    
    # Test allocations
    small_alloc <- allocate_memory(512 * 1024, memory_pools)
    medium_alloc <- allocate_memory(5 * 1024^2, memory_pools)
    large_alloc <- allocate_memory(50 * 1024^2, memory_pools)
    
    expect_equal(small_alloc, 1024^2)
    expect_equal(medium_alloc, 10 * 1024^2)
    expect_equal(large_alloc, 100 * 1024^2)
  }
  
  test_memory_pools()
  
  # Test garbage collection optimization
  test_gc_optimization <- function() {
    # Test different GC strategies
    gc_strategies <- list(
      aggressive = function() {
        gc(verbose = FALSE)
        gc(verbose = FALSE) # Double GC
      },
      
      lazy = function() {
        # Only GC when memory usage is high
        if (pryr::mem_used() > 1024^3) { # 1GB threshold
          gc(verbose = FALSE)
        }
      },
      
      adaptive = function(memory_pressure) {
        if (memory_pressure > 0.8) {
          gc(verbose = FALSE)
        }
      }
    )
    
    # Measure GC impact
    initial_memory <- pryr::mem_used()
    
    # Create garbage
    garbage <- replicate(100, {
      matrix(rnorm(1000), nrow = 100)
    }, simplify = FALSE)
    
    before_gc <- pryr::mem_used()
    
    # Test aggressive GC
    gc_strategies$aggressive()
    after_aggressive_gc <- pryr::mem_used()
    
    # Aggressive GC should free significant memory
    memory_freed <- as.numeric(before_gc - after_aggressive_gc)
    expect_true(memory_freed > 0)
  }
  
  test_gc_optimization()
})

# =============================================================================
# 5. CONTAINER ORCHESTRATION TESTING
# =============================================================================

test_that("Container Orchestration - Kubernetes Integration", {
  
  # Mock Kubernetes API responses
  mock_k8s_api <- function() {
    list(
      get_pods = function(namespace = "default") {
        list(
          items = list(
            list(
              metadata = list(name = "atlas-hr-pod-1", namespace = namespace),
              status = list(phase = "Running"),
              spec = list(containers = list(list(name = "atlas-hr-app")))
            ),
            list(
              metadata = list(name = "atlas-hr-pod-2", namespace = namespace),
              status = list(phase = "Running"),
              spec = list(containers = list(list(name = "atlas-hr-app")))
            )
          )
        )
      },
      
      scale_deployment = function(deployment, replicas) {
        list(
          spec = list(replicas = replicas),
          status = list(availableReplicas = replicas)
        )
      },
      
      get_service = function(service_name) {
        list(
          spec = list(
            ports = list(list(port = 3838, targetPort = 3838)),
            selector = list(app = "atlas-hr")
          )
        )
      }
    )
  }
  
  k8s <- mock_k8s_api()
  
  # Test pod management
  test_pod_management <- function() {
    pods <- k8s$get_pods("atlas-hr-namespace")
    
    # Should have multiple pods
    expect_true(length(pods$items) >= 2)
    
    # All pods should be running
    pod_phases <- map_chr(pods$items, ~ .$status$phase)
    expect_true(all(pod_phases == "Running"))
  }
  
  test_pod_management()
  
  # Test deployment scaling
  test_deployment_scaling <- function() {
    # Scale up deployment
    scale_result <- k8s$scale_deployment("atlas-hr-deployment", 5)
    
    expect_equal(scale_result$spec$replicas, 5)
    expect_equal(scale_result$status$availableReplicas, 5)
  }
  
  test_deployment_scaling()
  
  # Test service discovery
  test_service_discovery <- function() {
    service <- k8s$get_service("atlas-hr-service")
    
    # Service should expose correct port
    expect_equal(service$spec$ports[[1]]$port, 3838)
    expect_equal(service$spec$selector$app, "atlas-hr")
  }
  
  test_service_discovery()
})

test_that("Container Orchestration - Health Checks", {
  
  # Test readiness probes
  test_readiness_probes <- function() {
    # Simulate readiness check
    readiness_check <- function(app_state) {
      checks <- list(
        database_connected = app_state$db_status == "connected",
        cache_available = app_state$cache_status == "available",
        memory_ok = app_state$memory_usage < 0.9
      )
      
      all(unlist(checks))
    }
    
    # Healthy state
    healthy_state <- list(
      db_status = "connected",
      cache_status = "available",
      memory_usage = 0.6
    )
    
    # Unhealthy state
    unhealthy_state <- list(
      db_status = "disconnected",
      cache_status = "available",
      memory_usage = 0.6
    )
    
    expect_true(readiness_check(healthy_state))
    expect_false(readiness_check(unhealthy_state))
  }
  
  test_readiness_probes()
  
  # Test liveness probes
  test_liveness_probes <- function() {
    # Simulate liveness check
    liveness_check <- function(app_state) {
      checks <- list(
        process_running = app_state$process_status == "running",
        responding = app_state$last_response_time < 30,
        not_deadlocked = app_state$active_threads < 100
      )
      
      all(unlist(checks))
    }
    
    # Alive state
    alive_state <- list(
      process_status = "running",
      last_response_time = 15,
      active_threads = 25
    )
    
    # Dead state
    dead_state <- list(
      process_status = "running",
      last_response_time = 45, # Too slow
      active_threads = 25
    )
    
    expect_true(liveness_check(alive_state))
    expect_false(liveness_check(dead_state))
  }
  
  test_liveness_probes()
  
  # Test rolling updates
  test_rolling_updates <- function() {
    # Simulate rolling update strategy
    rolling_update <- function(current_pods, new_version) {
      update_steps <- list()
      
      for (i in seq_along(current_pods)) {
        # Update one pod at a time
        updated_pods <- current_pods
        updated_pods[[i]]$version <- new_version
        updated_pods[[i]]$status <- "updating"
        
        update_steps[[i]] <- list(
          step = i,
          pods = updated_pods,
          healthy_pods = sum(map_chr(updated_pods, ~ .$status) == "running")
        )
        
        # Mark as running after update
        updated_pods[[i]]$status <- "running"
      }
      
      update_steps
    }
    
    initial_pods <- replicate(3, list(
      version = "1.0.0",
      status = "running"
    ), simplify = FALSE)
    
    update_steps <- rolling_update(initial_pods, "1.1.0")
    
    # Should maintain minimum healthy pods during update
    healthy_counts <- map_dbl(update_steps, ~ .$healthy_pods)
    expect_true(all(healthy_counts >= 2)) # At least 2 healthy pods
  }
  
  test_rolling_updates()
})

# =============================================================================
# 6. MICROSERVICES COMMUNICATION TESTS
# =============================================================================

test_that("Microservices Communication - Service Mesh", {
  
  # Test service discovery
  test_service_discovery <- function() {
    # Mock service registry
    service_registry <- list(
      "atlas-hr-api" = list(
        host = "atlas-hr-api.default.svc.cluster.local",
        port = 8080,
        health_endpoint = "/health"
      ),
      "atlas-hr-cache" = list(
        