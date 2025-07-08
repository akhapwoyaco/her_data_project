# ============================================================================
# Atlas Labs HR Analytics Dashboard - Load Testing Suite
# Focus: Throughput Measurement & Stress Testing Limits
# Author: akhapwoyaco
# ============================================================================

# Required Libraries for Load Testing
library(testthat)
library(shiny)
library(shinytest2)
library(parallel)
library(foreach)
library(doParallel)
library(microbenchmark)
library(profvis)
library(bench)
library(future)
library(promises)
library(memuse)
library(pryr)
library(R6)

# ============================================================================
# LOAD TESTING FRAMEWORK
# ============================================================================

#' Load Testing Framework for Atlas Labs HR Dashboard
#' 
#' This R6 class provides comprehensive load testing capabilities including
#' throughput measurement, stress testing, and performance monitoring
AtlasLoadTester <- R6Class("AtlasLoadTester",
  public = list(
    app_dir = NULL,
    results = NULL,
    stress_limits = NULL,
    
    initialize = function(app_directory = ".") {
      self$app_dir <- app_directory
      self$results <- list()
      self$stress_limits <- list(
        max_concurrent_users = 100,
        max_requests_per_second = 1000,
        max_memory_mb = 2048,
        max_response_time_ms = 5000,
        max_cpu_percent = 80
      )
      
      # Setup parallel processing
      if (!foreach::getDoParRegistered()) {
        cores <- parallel::detectCores() - 1
        doParallel::registerDoParallel(cores)
      }
    },
    
    #' Execute comprehensive load tests
    run_load_tests = function() {
      cat("🚀 Starting Atlas Labs Load Testing Suite...\n")
      
      # 1. Throughput Measurement Tests
      self$test_throughput_measurement()
      
      # 2. Stress Testing Limits
      self$test_stress_limits()
      
      # 3. Generate comprehensive report
      self$generate_load_report()
      
      return(self$results)
    },
    
    #' Throughput measurement tests
    test_throughput_measurement = function() {
      cat("📊 Testing Throughput Measurement...\n")
      
      # Test 1: Single user throughput
      self$results$single_user_throughput <- self$measure_single_user_throughput()
      
      # Test 2: Multiple user throughput
      self$results$multi_user_throughput <- self$measure_multi_user_throughput()
      
      # Test 3: Module-specific throughput
      self$results$module_throughput <- self$measure_module_throughput()
      
      # Test 4: Data processing throughput
      self$results$data_processing_throughput <- self$measure_data_processing_throughput()
      
      # Test 5: Visualization rendering throughput
      self$results$visualization_throughput <- self$measure_visualization_throughput()
    },
    
    #' Stress testing limits
    test_stress_limits = function() {
      cat("💪 Testing Stress Limits...\n")
      
      # Test 1: Concurrent user limits
      self$results$concurrent_user_limits <- self$test_concurrent_user_limits()
      
      # Test 2: Memory stress testing
      self$results$memory_stress <- self$test_memory_stress()
      
      # Test 3: CPU stress testing
      self$results$cpu_stress <- self$test_cpu_stress()
      
      # Test 4: Data volume stress testing
      self$results$data_volume_stress <- self$test_data_volume_stress()
      
      # Test 5: Network stress testing
      self$results$network_stress <- self$test_network_stress()
    },
    
    #' Measure single user throughput
    measure_single_user_throughput = function() {
      cat("  📈 Testing single user throughput...\n")
      
      results <- list()
      
      # Start Shiny app for testing
      app <- shinytest2::AppDriver$new(self$app_dir)
      
      tryCatch({
        # Test various user actions
        actions <- list(
          "app_load" = function() app$wait_for_idle(),
          "data_load" = function() app$click("load_data"),
          "filter_change" = function() app$set_inputs(dept_filter = "HR"),
          "chart_render" = function() app$click("refresh_charts"),
          "export_report" = function() app$click("export_report")
        )
        
        # Measure throughput for each action
        for (action_name in names(actions)) {
          cat("    Testing:", action_name, "\n")
          
          # Warm up
          actions[[action_name]]()
          app$wait_for_idle()
          
          # Measure throughput
          benchmark_result <- microbenchmark::microbenchmark(
            {
              actions[[action_name]]()
              app$wait_for_idle()
            },
            times = 20,
            unit = "ms"
          )
          
          results[[action_name]] <- list(
            mean_time_ms = mean(benchmark_result$time) / 1e6,
            median_time_ms = median(benchmark_result$time) / 1e6,
            min_time_ms = min(benchmark_result$time) / 1e6,
            max_time_ms = max(benchmark_result$time) / 1e6,
            throughput_per_second = 1000 / (mean(benchmark_result$time) / 1e6)
          )
        }
        
        app$stop()
        
      }, error = function(e) {
        cat("    Error in single user throughput test:", e$message, "\n")
        if (exists("app")) app$stop()
      })
      
      return(results)
    },
    
    #' Measure multi-user throughput
    measure_multi_user_throughput = function() {
      cat("  👥 Testing multi-user throughput...\n")
      
      results <- list()
      user_counts <- c(2, 5, 10, 20, 50)
      
      for (users in user_counts) {
        cat("    Testing with", users, "concurrent users...\n")
        
        # Create multiple app instances
        start_time <- Sys.time()
        
        # Use parallel processing for concurrent users
        tryCatch({
          cluster_results <- parallel::mclapply(1:users, function(user_id) {
            
            # Simulate user session
            user_start <- Sys.time()
            
            # Create app instance
            app <- shinytest2::AppDriver$new(self$app_dir)
            
            # Simulate user actions
            actions_performed <- 0
            
            # Load data
            app$click("load_data")
            app$wait_for_idle()
            actions_performed <- actions_performed + 1
            
            # Change filters
            app$set_inputs(dept_filter = sample(c("HR", "IT", "Sales"), 1))
            app$wait_for_idle()
            actions_performed <- actions_performed + 1
            
            # Refresh charts
            app$click("refresh_charts")
            app$wait_for_idle()
            actions_performed <- actions_performed + 1
            
            app$stop()
            
            user_end <- Sys.time()
            
            return(list(
              user_id = user_id,
              session_duration = as.numeric(user_end - user_start),
              actions_performed = actions_performed,
              throughput = actions_performed / as.numeric(user_end - user_start)
            ))
            
          }, mc.cores = min(users, parallel::detectCores() - 1))
          
          end_time <- Sys.time()
          total_duration <- as.numeric(end_time - start_time)
          
          # Calculate aggregate metrics
          successful_sessions <- length(cluster_results)
          total_actions <- sum(sapply(cluster_results, function(x) x$actions_performed))
          
          results[[paste0("users_", users)]] <- list(
            concurrent_users = users,
            successful_sessions = successful_sessions,
            total_duration_seconds = total_duration,
            total_actions = total_actions,
            aggregate_throughput = total_actions / total_duration,
            average_session_duration = mean(sapply(cluster_results, function(x) x$session_duration)),
            success_rate = successful_sessions / users
          )
          
        }, error = function(e) {
          cat("    Error with", users, "users:", e$message, "\n")
          results[[paste0("users_", users)]] <- list(
            error = e$message,
            concurrent_users = users,
            success_rate = 0
          )
        })
      }
      
      return(results)
    },
    
    #' Measure module-specific throughput
    measure_module_throughput = function() {
      cat("  🔧 Testing module-specific throughput...\n")
      
      results <- list()
      
      # Test individual modules
      modules <- c("overview", "attrition", "demographics", "performance", 
                   "compensation", "satisfaction", "report")
      
      app <- shinytest2::AppDriver$new(self$app_dir)
      
      tryCatch({
        for (module in modules) {
          cat("    Testing module:", module, "\n")
          
          # Navigate to module
          module_selector <- paste0(module, "_tab")
          
          benchmark_result <- microbenchmark::microbenchmark(
            {
              app$click(module_selector)
              app$wait_for_idle()
            },
            times = 10,
            unit = "ms"
          )
          
          results[[module]] <- list(
            mean_load_time_ms = mean(benchmark_result$time) / 1e6,
            median_load_time_ms = median(benchmark_result$time) / 1e6,
            min_load_time_ms = min(benchmark_result$time) / 1e6,
            max_load_time_ms = max(benchmark_result$time) / 1e6,
            throughput_per_second = 1000 / (mean(benchmark_result$time) / 1e6)
          )
        }
        
        app$stop()
        
      }, error = function(e) {
        cat("    Error in module throughput test:", e$message, "\n")
        if (exists("app")) app$stop()
      })
      
      return(results)
    },
    
    #' Measure data processing throughput
    measure_data_processing_throughput = function() {
      cat("  💾 Testing data processing throughput...\n")
      
      results <- list()
      
      # Generate test datasets of various sizes
      dataset_sizes <- c(100, 1000, 10000, 50000, 100000)
      
      for (size in dataset_sizes) {
        cat("    Testing with", size, "records...\n")
        
        # Create test dataset
        test_data <- data.frame(
          EmployeeID = 1:size,
          Department = sample(c("HR", "IT", "Sales", "Marketing"), size, replace = TRUE),
          Salary = sample(30000:150000, size, replace = TRUE),
          Attrition = sample(c("Yes", "No"), size, replace = TRUE),
          JobSatisfaction = sample(1:5, size, replace = TRUE),
          Age = sample(22:65, size, replace = TRUE),
          stringsAsFactors = FALSE
        )
        
        # Test data processing operations
        processing_tests <- list(
          "filter_operation" = function() {
            filtered <- test_data[test_data$Department == "HR", ]
            return(nrow(filtered))
          },
          "aggregate_operation" = function() {
            aggregated <- aggregate(Salary ~ Department, test_data, mean)
            return(nrow(aggregated))
          },
          "join_operation" = function() {
            lookup <- data.frame(
              Department = c("HR", "IT", "Sales", "Marketing"),
              Manager = c("John", "Jane", "Bob", "Alice")
            )
            joined <- merge(test_data, lookup, by = "Department")
            return(nrow(joined))
          },
          "sort_operation" = function() {
            sorted <- test_data[order(test_data$Salary, decreasing = TRUE), ]
            return(nrow(sorted))
          }
        )
        
        size_results <- list()
        
        for (test_name in names(processing_tests)) {
          benchmark_result <- microbenchmark::microbenchmark(
            processing_tests[[test_name]](),
            times = 5,
            unit = "ms"
          )
          
          size_results[[test_name]] <- list(
            mean_time_ms = mean(benchmark_result$time) / 1e6,
            records_per_second = size / (mean(benchmark_result$time) / 1e9),
            throughput_mb_per_second = (object.size(test_data) / 1024^2) / (mean(benchmark_result$time) / 1e9)
          )
        }
        
        results[[paste0("records_", size)]] <- size_results
      }
      
      return(results)
    },
    
    #' Measure visualization rendering throughput
    measure_visualization_throughput = function() {
      cat("  📊 Testing visualization rendering throughput...\n")
      
      results <- list()
      
      # Test different chart types with varying data sizes
      chart_types <- c("bar_chart", "line_chart", "scatter_plot", "heatmap", "histogram")
      data_sizes <- c(100, 1000, 5000, 10000)
      
      for (chart_type in chart_types) {
        cat("    Testing chart type:", chart_type, "\n")
        
        chart_results <- list()
        
        for (size in data_sizes) {
          # Generate test data
          test_data <- data.frame(
            x = 1:size,
            y = runif(size, 0, 100),
            category = sample(letters[1:10], size, replace = TRUE),
            value = sample(1:1000, size, replace = TRUE)
          )
          
          # Mock chart rendering function
          render_chart <- function(data, type) {
            switch(type,
              "bar_chart" = {
                # Simulate bar chart rendering
                Sys.sleep(0.001 * nrow(data) / 1000)
                return(nrow(data))
              },
              "line_chart" = {
                # Simulate line chart rendering
                Sys.sleep(0.0005 * nrow(data) / 1000)
                return(nrow(data))
              },
              "scatter_plot" = {
                # Simulate scatter plot rendering
                Sys.sleep(0.002 * nrow(data) / 1000)
                return(nrow(data))
              },
              "heatmap" = {
                # Simulate heatmap rendering
                Sys.sleep(0.005 * nrow(data) / 1000)
                return(nrow(data))
              },
              "histogram" = {
                # Simulate histogram rendering
                Sys.sleep(0.001 * nrow(data) / 1000)
                return(nrow(data))
              }
            )
          }
          
          benchmark_result <- microbenchmark::microbenchmark(
            render_chart(test_data, chart_type),
            times = 5,
            unit = "ms"
          )
          
          chart_results[[paste0("size_", size)]] <- list(
            data_points = size,
            mean_render_time_ms = mean(benchmark_result$time) / 1e6,
            points_per_second = size / (mean(benchmark_result$time) / 1e9),
            fps_equivalent = 1000 / (mean(benchmark_result$time) / 1e6)
          )
        }
        
        results[[chart_type]] <- chart_results
      }
      
      return(results)
    },
    
    #' Test concurrent user limits
    test_concurrent_user_limits = function() {
      cat("  🏋️ Testing concurrent user limits...\n")
      
      results <- list()
      
      # Test increasing number of concurrent users until failure
      max_users <- self$stress_limits$max_concurrent_users
      user_increments <- c(1, 5, 10, 20, 50, 100, 200, 500)
      
      for (users in user_increments) {
        if (users > max_users) break
        
        cat("    Testing", users, "concurrent users...\n")
        
        start_time <- Sys.time()
        memory_before <- as.numeric(pryr::mem_used())
        
        tryCatch({
          # Create concurrent user sessions
          futures <- list()
          
          for (i in 1:users) {
            futures[[i]] <- future::future({
              # Simulate user session
              app <- shinytest2::AppDriver$new(self$app_dir)
              
              # Perform basic actions
              app$click("load_data")
              app$wait_for_idle()
              
              app$set_inputs(dept_filter = "HR")
              app$wait_for_idle()
              
              app$click("refresh_charts")
              app$wait_for_idle()
              
              app$stop()
              
              return(TRUE)
            })
          }
          
          # Wait for all sessions to complete
          session_results <- future::value(futures)
          
          end_time <- Sys.time()
          memory_after <- as.numeric(pryr::mem_used())
          
          successful_sessions <- sum(sapply(session_results, function(x) isTRUE(x)))
          
          results[[paste0("users_", users)]] <- list(
            concurrent_users = users,
            successful_sessions = successful_sessions,
            success_rate = successful_sessions / users,
            total_duration_seconds = as.numeric(end_time - start_time),
            memory_increase_mb = (memory_after - memory_before) / 1024^2,
            memory_per_user_mb = (memory_after - memory_before) / (1024^2 * users),
            status = ifelse(successful_sessions / users >= 0.9, "PASS", "FAIL")
          )
          
          # If success rate drops below 90%, we've found the limit
          if (successful_sessions / users < 0.9) {
            results$concurrent_user_limit <- users - 1
            break
          }
          
        }, error = function(e) {
          cat("    Error with", users, "users:", e$message, "\n")
          results[[paste0("users_", users)]] <- list(
            concurrent_users = users,
            error = e$message,
            status = "ERROR"
          )
          results$concurrent_user_limit <- users - 1
        })
      }
      
      return(results)
    },
    
    #' Test memory stress limits
    test_memory_stress = function() {
      cat("  🧠 Testing memory stress limits...\n")
      
      results <- list()
      
      # Test with increasing data sizes
      data_sizes <- c(1000, 10000, 100000, 500000, 1000000)
      
      for (size in data_sizes) {
        cat("    Testing with", size, "records...\n")
        
        memory_before <- as.numeric(pryr::mem_used())
        
        tryCatch({
          # Create large dataset
          large_data <- data.frame(
            id = 1:size,
            name = paste0("Employee_", 1:size),
            department = sample(c("HR", "IT", "Sales", "Marketing"), size, replace = TRUE),
            salary = sample(30000:150000, size, replace = TRUE),
            rating = sample(1:5, size, replace = TRUE),
            notes = paste0("Note_", 1:size, "_", sample(letters, size, replace = TRUE))
          )
          
          memory_after_creation <- as.numeric(pryr::mem_used())
          
          # Process the data
          processed_data <- large_data %>%
            dplyr::group_by(department) %>%
            dplyr::summarise(
              count = n(),
              avg_salary = mean(salary),
              avg_rating = mean(rating),
              .groups = "drop"
            )
          
          memory_after_processing <- as.numeric(pryr::mem_used())
          
          # Clean up
          rm(large_data, processed_data)
          gc()
          
          memory_after_cleanup <- as.numeric(pryr::mem_used())
          
          results[[paste0("records_", size)]] <- list(
            record_count = size,
            memory_before_mb = memory_before / 1024^2,
            memory_after_creation_mb = memory_after_creation / 1024^2,
            memory_after_processing_mb = memory_after_processing / 1024^2,
            memory_after_cleanup_mb = memory_after_cleanup / 1024^2,
            peak_memory_usage_mb = (memory_after_processing - memory_before) / 1024^2,
            memory_per_record_bytes = (memory_after_creation - memory_before) / size,
            status = ifelse((memory_after_processing - memory_before) / 1024^2 < self$stress_limits$max_memory_mb, "PASS", "FAIL")
          )
          
          # If memory usage exceeds limit, we've found the limit
          if ((memory_after_processing - memory_before) / 1024^2 >= self$stress_limits$max_memory_mb) {
            results$memory_limit_records <- size
            break
          }
          
        }, error = function(e) {
          cat("    Memory error with", size, "records:", e$message, "\n")
          results[[paste0("records_", size)]] <- list(
            record_count = size,
            error = e$message,
            status = "ERROR"
          )
          results$memory_limit_records <- size
        })
      }
      
      return(results)
    },
    
    #' Test CPU stress limits
    test_cpu_stress = function() {
      cat("  🔥 Testing CPU stress limits...\n")
      
      results <- list()
      
      # Test CPU-intensive operations
      cpu_tests <- list(
        "complex_aggregation" = function(data) {
          data %>%
            dplyr::group_by(department, job_role) %>%
            dplyr::summarise(
              count = n(),
              avg_salary = mean(salary),
              median_salary = median(salary),
              salary_sd = sd(salary),
              attrition_rate = mean(attrition == "Yes"),
              .groups = "drop"
            ) %>%
            dplyr::arrange(desc(avg_salary))
        },
        "statistical_modeling" = function(data) {
          # Simulate statistical modeling
          if (nrow(data) > 100) {
            model <- lm(salary ~ age + job_satisfaction + years_at_company, data = data)
            return(summary(model))
          }
          return(NULL)
        },
        "complex_joins" = function(data) {
          # Create multiple datasets for joining
          dept_data <- data.frame(
            department = unique(data$department),
            budget = sample(100000:1000000, length(unique(data$department)))
          )
          
          role_data <- data.frame(
            job_role = sample(c("Manager", "Senior", "Junior"), nrow(data), replace = TRUE),
            level = sample(1:5, nrow(data), replace = TRUE)
          )
          
          # Perform complex joins
          result <- data %>%
            dplyr::left_join(dept_data, by = "department") %>%
            dplyr::left_join(role_data, by = "job_role") %>%
            dplyr::group_by(department, job_role) %>%
            dplyr::summarise(
              avg_salary = mean(salary),
              budget_per_employee = first(budget) / n(),
              .groups = "drop"
            )
          
          return(result)
        }
      )
      
      data_sizes <- c(1000, 5000, 10000, 25000, 50000)
      
      for (size in data_sizes) {
        cat("    Testing CPU with", size, "records...\n")
        
        # Create test data
        test_data <- data.frame(
          id = 1:size,
          department = sample(c("HR", "IT", "Sales", "Marketing"), size, replace = TRUE),
          job_role = sample(c("Manager", "Senior", "Junior"), size, replace = TRUE),
          salary = sample(30000:150000, size, replace = TRUE),
          age = sample(22:65, size, replace = TRUE),
          job_satisfaction = sample(1:5, size, replace = TRUE),
          years_at_company = sample(1:30, size, replace = TRUE),
          attrition = sample(c("Yes", "No"), size, replace = TRUE, prob = c(0.2, 0.8))
        )
        
        size_results <- list()
        
        for (test_name in names(cpu_tests)) {
          cat("      Testing", test_name, "...\n")
          
          # Monitor CPU usage (approximated by execution time)
          start_time <- Sys.time()
          
          benchmark_result <- microbenchmark::microbenchmark(
            cpu_tests[[test_name]](test_data),
            times = 3,
            unit = "ms"
          )
          
          end_time <- Sys.time()
          
          size_results[[test_name]] <- list(
            mean_execution_time_ms = mean(benchmark_result$time) / 1e6,
            median_execution_time_ms = median(benchmark_result$time) / 1e6,
            max_execution_time_ms = max(benchmark_result$time) / 1e6,
            records_per_second = size / (mean(benchmark_result$time) / 1e9),
            cpu_efficiency_score = size / (mean(benchmark_result$time) / 1e6),
            status = ifelse(mean(benchmark_result$time) / 1e6 < self$stress_limits$max_response_time_ms, "PASS", "FAIL")
          )
        }
        
        results[[paste0("records_", size)]] <- size_results
      }
      
      return(results)
    },
    
    #' Test data volume stress limits
    test_data_volume_stress = function() {
      cat("  📚 Testing data volume stress limits...\n")
      
      results <- list()
      
      # Test with increasing data volumes
      volume_tests <- c(1000, 10000, 50000, 100000, 250000, 500000, 1000000)
      
      for (volume in volume_tests) {
        cat("    Testing with", volume, "records...\n")
        
        start_time <- Sys.time()
        memory_before <- as.numeric(pryr::mem_used())
        
        tryCatch({
          # Create large dataset
          large_dataset <- data.frame(
            employee_id = 1:volume,
            first_name = paste0("First_", 1:volume),
            last_name = paste0("Last_", 1:volume),
            department = sample(c("HR", "IT", "Sales", "Marketing", "Finance"), volume, replace = TRUE),
            job_role = sample(c("Manager", "Senior", "Junior", "Intern"), volume, replace = TRUE),
            salary = sample(30000:200000, volume, replace = TRUE),
            age = sample(22:65, volume, replace = TRUE),
            hire_date = sample(seq(as.Date("2010-01-01"), as.Date("2023-12-31"), by = "day"), volume, replace = TRUE),
            performance_rating = sample(1:5, volume, replace = TRUE),
            job_satisfaction = sample(1:5, volume, replace = TRUE),
            attrition = sample(c("Yes", "No"), volume, replace = TRUE, prob = c(0.15, 0.85))
          )
          
          memory_after_creation <- as.numeric(pryr::mem_used())
          
          # Perform typical analytics operations
          analytics_start <- Sys.time()
          
          # Aggregation
          agg_result <- large_dataset %>%
            dplyr::group_by(department, job_role) %>%
            dplyr::summarise(
              count = n(),
              avg_salary = mean(salary),
              attrition_rate = mean(attrition == "Yes"),
              avg_performance = mean(performance_rating),
              .groups = "drop"
            )
          
          # Filtering
          filtered_result <- large_dataset %>%
            dplyr::filter(
              department %in% c("HR", "IT"),
              salary > 50000,
              performance_rating >= 3
            )
          
          # Sorting
          sorted_result <- large_dataset %>%
            dplyr::arrange(desc(salary), desc(performance_rating))
          
          analytics_end <- Sys.time()
          memory_after_analytics <- as.numeric(pryr::mem_used())
          
          # Clean up
          rm(large_dataset, agg_result, filtered_result, sorted_result)
          gc()
          
          end_time <- Sys.time()
          memory_after_cleanup <- as.numeric(pryr::mem_used())
          
          results[[paste0("volume_", volume)]] <- list(
            record_count = volume,
            total_duration_seconds = as.numeric(end_time - start_time),
            creation_duration_seconds = as.numeric(analytics_start - start_time),
            analytics_duration_seconds = as.numeric(analytics_end - analytics_start),
            memory_usage_mb = (memory_after_analytics - memory_before) / 1024^2,
            peak_memory_mb = (memory_after_analytics - memory_before) / 1024^2,
            memory_per_record_bytes = (memory_after_creation - memory_before) / volume,
            processing_rate_records_per_second = volume / as.numeric(analytics_end - analytics_start),
            status = ifelse(
              (memory_after_analytics - memory_before) / 1024^2 < self$stress_limits$max_memory_mb &&
              as.numeric(analytics_end - analytics_start) < self$stress_limits$max_response_time_ms / 1000,
              "PASS", "FAIL"
            )
          )
          
        }, error = function(e) {
          cat("    Error with volume", volume, ":", e$message, "\n")
          results[[paste0("volume_", volume)]] <- list(
            record_count = volume,
            error = e$message,
            status = "ERROR"
          )
        })
      }
      
      return(results)
    },
    
    #' Test network stress limits
    test_network_stress = function() {
      cat("  🌐 Testing network stress limits...\n")
      
      results <- list()
      
      # Simulate network stress by testing data transfer sizes
      transfer_sizes <- c(1, 10, 50, 100, 500, 1000) # MB
      
      for (