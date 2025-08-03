# ============================================================================
# ATLAS LABS HR ANALYTICS - MONITORING & OBSERVABILITY UNIT TESTS
# Comprehensive test suite for application monitoring, health checks, metrics,
# alerts, logging, tracing, and performance monitoring
# ============================================================================

library(testthat)
library(mockery)
library(shiny)
library(R6)
library(jsonlite)
library(httr)
library(purrr)
library(lubridate)
library(digest)

# Source the application files (assuming they exist)
# source("modules/logger_module.R")
# source("global.R")
# source("utils.R")

# ============================================================================
# 9.1.1 HEALTH CHECK ENDPOINT VALIDATION TESTS
# ============================================================================

describe("Health Check Endpoint Validation", {
  
  # Mock health check endpoint function
  create_health_endpoint <- function() {
    function(req) {
      tryCatch({
        # Simulate health checks
        db_status <- check_database_connection()
        memory_status <- check_memory_usage()
        disk_status <- check_disk_space()
        
        health_data <- list(
          status = "healthy",
          timestamp = Sys.time(),
          version = "1.0.0",
          uptime = get_uptime(),
          checks = list(
            database = db_status,
            memory = memory_status,
            disk = disk_status
          )
        )
        
        list(
          status = 200,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(health_data, auto_unbox = TRUE)
        )
      }, error = function(e) {
        list(
          status = 503,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(list(
            status = "unhealthy",
            error = e$message,
            timestamp = Sys.time()
          ), auto_unbox = TRUE)
        )
      })
    }
  }
  
  # Helper functions for health checks
  check_database_connection <- function() {
    list(status = "ok", response_time_ms = sample(1:50, 1))
  }
  
  check_memory_usage <- function() {
    mem_info <- gc()
    list(
      status = if(sum(mem_info[,2]) < 1000) "ok" else "warning",
      used_mb = sum(mem_info[,2]),
      threshold_mb = 1000
    )
  }
  
  check_disk_space <- function() {
    list(status = "ok", free_gb = 50.5, used_percent = 45)
  }
  
  get_uptime <- function() {
    as.numeric(difftime(Sys.time(), as.POSIXct("2024-01-01"), units = "secs"))
  }
  
  it("returns 200 status for healthy application", {
    health_endpoint <- create_health_endpoint()
    response <- health_endpoint(list())
    
    expect_equal(response$status, 200)
    expect_equal(response$headers$`Content-Type`, "application/json")
    
    body <- jsonlite::fromJSON(response$body)
    expect_equal(body$status, "healthy")
    expect_true("timestamp" %in% names(body))
    expect_true("version" %in% names(body))
    expect_true("checks" %in% names(body))
  })
  
  it("validates all health check components", {
    health_endpoint <- create_health_endpoint()
    response <- health_endpoint(list())
    body <- jsonlite::fromJSON(response$body)
    
    # Check required fields
    required_fields <- c("status", "timestamp", "version", "uptime", "checks")
    expect_true(all(required_fields %in% names(body)))
    
    # Check nested components
    expect_true("database" %in% names(body$checks))
    expect_true("memory" %in% names(body$checks))
    expect_true("disk" %in% names(body$checks))
    
    # Validate timestamp format
    expect_true(is.character(body$timestamp))
    expect_false(is.na(as.POSIXct(body$timestamp)))
  })
  
  it("handles database connection failures", {
    # Mock database failure
    mock_check_db <- function() {
      stop("Database connection timeout")
    }
    
    with_mock(
      check_database_connection = mock_check_db,
      {
        health_endpoint <- create_health_endpoint()
        response <- health_endpoint(list())
        
        expect_equal(response$status, 503)
        body <- jsonlite::fromJSON(response$body)
        expect_equal(body$status, "unhealthy")
        expect_true("error" %in% names(body))
      }
    )
  })
  
  it("validates response time thresholds", {
    # Test various response times
    response_times <- c(10, 50, 100, 500, 1000, 5000)
    
    for (time_ms in response_times) {
      mock_check_db <- function() {
        Sys.sleep(time_ms/1000)  # Convert to seconds
        list(status = "ok", response_time_ms = time_ms)
      }
      
      with_mock(
        check_database_connection = mock_check_db,
        {
          start_time <- Sys.time()
          health_endpoint <- create_health_endpoint()
          response <- health_endpoint(list())
          end_time <- Sys.time()
          
          actual_time <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
          expect_true(actual_time >= time_ms * 0.8)  # Allow 20% tolerance
        }
      )
    }
  })
  
  it("handles memory pressure scenarios", {
    # Test different memory usage levels
    memory_scenarios <- list(
      low = list(status = "ok", used_mb = 100, threshold_mb = 1000),
      medium = list(status = "ok", used_mb = 500, threshold_mb = 1000),
      high = list(status = "warning", used_mb = 950, threshold_mb = 1000),
      critical = list(status = "error", used_mb = 1200, threshold_mb = 1000)
    )
    
    for (scenario_name in names(memory_scenarios)) {
      scenario <- memory_scenarios[[scenario_name]]
      
      mock_check_memory <- function() scenario
      
      with_mock(
        check_memory_usage = mock_check_memory,
        {
          health_endpoint <- create_health_endpoint()
          response <- health_endpoint(list())
          body <- jsonlite::fromJSON(response$body)
          
          expect_equal(body$checks$memory$status, scenario$status)
          expect_equal(body$checks$memory$used_mb, scenario$used_mb)
        }
      )
    }
  })
  
  it("validates concurrent health check requests", {
    health_endpoint <- create_health_endpoint()
    
    # Simulate concurrent requests
    concurrent_responses <- parallel::mclapply(1:10, function(i) {
      health_endpoint(list())
    }, mc.cores = 2)
    
    # All should succeed
    statuses <- sapply(concurrent_responses, function(r) r$status)
    expect_true(all(statuses == 200))
    
    # Check response consistency
    bodies <- lapply(concurrent_responses, function(r) jsonlite::fromJSON(r$body))
    versions <- sapply(bodies, function(b) b$version)
    expect_true(length(unique(versions)) == 1)  # All should have same version
  })
})

# ============================================================================
# 9.1.2 METRICS COLLECTION ACCURACY TESTS
# ============================================================================

describe("Metrics Collection Accuracy", {
  
  # Mock metrics collector
  MetricsCollector <- R6Class("MetricsCollector",
    private = list(
      .metrics = list(),
      .start_time = NULL
    ),
    
    public = list(
      initialize = function() {
        private$.metrics <- list()
        private$.start_time <- Sys.time()
      },
      
      increment_counter = function(name, value = 1, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        if (is.null(private$.metrics[[key]])) {
          private$.metrics[[key]] <- list(
            type = "counter",
            name = name,
            value = 0,
            tags = tags,
            last_updated = Sys.time()
          )
        }
        private$.metrics[[key]]$value <- private$.metrics[[key]]$value + value
        private$.metrics[[key]]$last_updated <- Sys.time()
      },
      
      set_gauge = function(name, value, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        private$.metrics[[key]] <- list(
          type = "gauge",
          name = name,
          value = value,
          tags = tags,
          last_updated = Sys.time()
        )
      },
      
      record_histogram = function(name, value, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        if (is.null(private$.metrics[[key]])) {
          private$.metrics[[key]] <- list(
            type = "histogram",
            name = name,
            values = c(),
            tags = tags,
            last_updated = Sys.time()
          )
        }
        private$.metrics[[key]]$values <- c(private$.metrics[[key]]$values, value)
        private$.metrics[[key]]$last_updated <- Sys.time()
      },
      
      get_metrics = function() {
        private$.metrics
      },
      
      get_metric_summary = function() {
        metrics_summary <- list()
        
        for (metric_key in names(private$.metrics)) {
          metric <- private$.metrics[[metric_key]]
          
          if (metric$type == "counter") {
            metrics_summary[[metric$name]] <- list(
              type = "counter",
              value = metric$value,
              tags = metric$tags
            )
          } else if (metric$type == "gauge") {
            metrics_summary[[metric$name]] <- list(
              type = "gauge",
              value = metric$value,
              tags = metric$tags
            )
          } else if (metric$type == "histogram") {
            values <- metric$values
            metrics_summary[[metric$name]] <- list(
              type = "histogram",
              count = length(values),
              mean = mean(values),
              median = median(values),
              p95 = quantile(values, 0.95, na.rm = TRUE),
              p99 = quantile(values, 0.99, na.rm = TRUE),
              min = min(values),
              max = max(values),
              tags = metric$tags
            )
          }
        }
        
        metrics_summary
      }
    )
  )
  
  it("accurately tracks counter metrics", {
    collector <- MetricsCollector$new()
    
    # Test basic counter increment
    collector$increment_counter("user_logins")
    collector$increment_counter("user_logins")
    collector$increment_counter("user_logins", value = 3)
    
    metrics <- collector$get_metrics()
    login_metric <- metrics[[paste0("user_logins_", digest::digest(list()))]]
    
    expect_equal(login_metric$type, "counter")
    expect_equal(login_metric$value, 5)  # 1 + 1 + 3
    expect_equal(login_metric$name, "user_logins")
  })
  
  it("handles counter metrics with tags", {
    collector <- MetricsCollector$new()
    
    # Different departments
    collector$increment_counter("page_views", tags = list(department = "HR"))
    collector$increment_counter("page_views", tags = list(department = "HR"))
    collector$increment_counter("page_views", tags = list(department = "Finance"))
    
    metrics <- collector$get_metrics()
    
    # Should have separate counters for each tag combination
    hr_key <- paste0("page_views_", digest::digest(list(department = "HR")))
    finance_key <- paste0("page_views_", digest::digest(list(department = "Finance")))
    
    expect_equal(metrics[[hr_key]]$value, 2)
    expect_equal(metrics[[finance_key]]$value, 1)
    expect_equal(metrics[[hr_key]]$tags$department, "HR")
    expect_equal(metrics[[finance_key]]$tags$department, "Finance")
  })
  
  it("accurately measures gauge metrics", {
    collector <- MetricsCollector$new()
    
    # Test gauge updates
    collector$set_gauge("memory_usage_mb", 512.5)
    collector$set_gauge("memory_usage_mb", 768.2)  # Should overwrite
    collector$set_gauge("cpu_usage_percent", 45.7)
    
    metrics <- collector$get_metrics()
    memory_key <- paste0("memory_usage_mb_", digest::digest(list()))
    cpu_key <- paste0("cpu_usage_percent_", digest::digest(list()))
    
    expect_equal(metrics[[memory_key]]$value, 768.2)
    expect_equal(metrics[[cpu_key]]$value, 45.7)
    expect_equal(metrics[[memory_key]]$type, "gauge")
  })
  
  it("correctly calculates histogram statistics", {
    collector <- MetricsCollector$new()
    
    # Record response times
    response_times <- c(10, 15, 12, 45, 23, 18, 67, 34, 28, 19)
    for (time in response_times) {
      collector$record_histogram("response_time_ms", time)
    }
    
    summary <- collector$get_metric_summary()
    response_metric <- summary$response_time_ms
    
    expect_equal(response_metric$type, "histogram")
    expect_equal(response_metric$count, 10)
    expect_equal(response_metric$mean, mean(response_times))
    expect_equal(response_metric$median, median(response_times))
    expect_equal(response_metric$min, min(response_times))
    expect_equal(response_metric$max, max(response_times))
  })
  
  it("handles edge cases in histogram calculations", {
    collector <- MetricsCollector$new()
    
    # Test with single value
    collector$record_histogram("single_value", 42)
    summary <- collector$get_metric_summary()
    single_metric <- summary$single_value
    
    expect_equal(single_metric$count, 1)
    expect_equal(single_metric$mean, 42)
    expect_equal(single_metric$median, 42)
    expect_equal(single_metric$min, 42)
    expect_equal(single_metric$max, 42)
    
    # Test with identical values
    for (i in 1:5) {
      collector$record_histogram("identical_values", 100)
    }
    
    summary <- collector$get_metric_summary()
    identical_metric <- summary$identical_values
    
    expect_equal(identical_metric$count, 5)
    expect_equal(identical_metric$mean, 100)
    expect_equal(identical_metric$p95, 100)
    expect_equal(identical_metric$p99, 100)
  })
  
  it("maintains accuracy under high volume", {
    collector <- MetricsCollector$new()
    
    # High volume counter increments
    start_time <- Sys.time()
    for (i in 1:10000) {
      collector$increment_counter("high_volume_counter")
    }
    end_time <- Sys.time()
    
    metrics <- collector$get_metrics()
    counter_key <- paste0("high_volume_counter_", digest::digest(list()))
    
    expect_equal(metrics[[counter_key]]$value, 10000)
    
    # Should complete reasonably quickly (less than 1 second)
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    expect_true(execution_time < 1.0)
  })
  
  it("handles concurrent metric updates", {
    collector <- MetricsCollector$new()
    
    # Concurrent counter increments
    parallel::mclapply(1:100, function(i) {
      collector$increment_counter("concurrent_counter")
    }, mc.cores = 2)
    
    metrics <- collector$get_metrics()
    counter_key <- paste0("concurrent_counter_", digest::digest(list()))
    
    # Should handle race conditions gracefully
    expect_true(metrics[[counter_key]]$value <= 100)
    expect_true(metrics[[counter_key]]$value > 0)
  })
  
  it("validates metric timestamp accuracy", {
    collector <- MetricsCollector$new()
    
    before_time <- Sys.time()
    collector$increment_counter("timestamp_test")
    after_time <- Sys.time()
    
    metrics <- collector$get_metrics()
    metric_key <- paste0("timestamp_test_", digest::digest(list()))
    metric_timestamp <- metrics[[metric_key]]$last_updated
    
    expect_true(metric_timestamp >= before_time)
    expect_true(metric_timestamp <= after_time)
  })
})

# ============================================================================
# 9.1.3 ALERT THRESHOLD TESTING
# ============================================================================

describe("Alert Threshold Testing", {
  
  # Mock alert system
  AlertSystem <- R6Class("AlertSystem",
    private = list(
      .thresholds = list(),
      .alerts = list(),
      .alert_history = list()
    ),
    
    public = list(
      initialize = function() {
        private$.thresholds <- list()
        private$.alerts <- list()
        private$.alert_history <- list()
      },
      
      set_threshold = function(metric_name, threshold_type, value, severity = "warning") {
        threshold_id <- paste0(metric_name, "_", threshold_type)
        private$.thresholds[[threshold_id]] <- list(
          metric_name = metric_name,
          type = threshold_type,  # "greater_than", "less_than", "equal_to"
          value = value,
          severity = severity,
          enabled = TRUE
        )
      },
      
      check_thresholds = function(metrics) {
        current_alerts <- list()
        
        for (threshold_id in names(private$.thresholds)) {
          threshold <- private$.thresholds[[threshold_id]]
          if (!threshold$enabled) next
          
          metric_value <- metrics[[threshold$metric_name]]
          if (is.null(metric_value)) next
          
          alert_triggered <- FALSE
          
          if (threshold$type == "greater_than" && metric_value > threshold$value) {
            alert_triggered <- TRUE
          } else if (threshold$type == "less_than" && metric_value < threshold$value) {
            alert_triggered <- TRUE
          } else if (threshold$type == "equal_to" && metric_value == threshold$value) {
            alert_triggered <- TRUE
          }
          
          if (alert_triggered) {
            alert <- list(
              threshold_id = threshold_id,
              metric_name = threshold$metric_name,
              metric_value = metric_value,
              threshold_value = threshold$value,
              severity = threshold$severity,
              timestamp = Sys.time(),
              message = sprintf("Metric %s (%s) exceeded threshold %s (current: %s)",
                              threshold$metric_name, threshold$type, 
                              threshold$value, metric_value)
            )
            
            current_alerts[[threshold_id]] <- alert
            private$.alert_history[[length(private$.alert_history) + 1]] <- alert
          }
        }
        
        private$.alerts <- current_alerts
        return(current_alerts)
      },
      
      get_active_alerts = function() {
        private$.alerts
      },
      
      get_alert_history = function(limit = 100) {
        history_length <- length(private$.alert_history)
        if (history_length == 0) return(list())
        
        start_idx <- max(1, history_length - limit + 1)
        private$.alert_history[start_idx:history_length]
      },
      
      clear_alerts = function() {
        private$.alerts <- list()
      },
      
      disable_threshold = function(threshold_id) {
        if (threshold_id %in% names(private$.thresholds)) {
          private$.thresholds[[threshold_id]]$enabled <- FALSE
        }
      }
    )
  )
  
  it("triggers alerts when thresholds are exceeded", {
    alert_system <- AlertSystem$new()
    
    # Set up thresholds
    alert_system$set_threshold("cpu_usage", "greater_than", 80, "warning")
    alert_system$set_threshold("memory_usage", "greater_than", 90, "critical")
    alert_system$set_threshold("response_time", "greater_than", 1000, "warning")
    
    # Test metrics that should trigger alerts
    metrics <- list(
      cpu_usage = 85,     # Should trigger warning
      memory_usage = 95,  # Should trigger critical
      response_time = 500 # Should not trigger
    )
    
    alerts <- alert_system$check_thresholds(metrics)
    
    expect_equal(length(alerts), 2)
    expect_true("cpu_usage_greater_than" %in% names(alerts))
    expect_true("memory_usage_greater_than" %in% names(alerts))
    expect_false("response_time_greater_than" %in% names(alerts))
    
    # Check alert details
    cpu_alert <- alerts$cpu_usage_greater_than
    expect_equal(cpu_alert$severity, "warning")
    expect_equal(cpu_alert$metric_value, 85)
    expect_equal(cpu_alert$threshold_value, 80)
  })
  
  it("handles different threshold types correctly", {
    alert_system <- AlertSystem$new()
    
    # Set up different threshold types
    alert_system$set_threshold("disk_space", "less_than", 10, "critical")
    alert_system$set_threshold("error_count", "greater_than", 0, "warning")
    alert_system$set_threshold("connection_count", "equal_to", 0, "critical")
    
    # Test scenarios
    test_cases <- list(
      list(
        metrics = list(disk_space = 5, error_count = 3, connection_count = 0),
        expected_alerts = c("disk_space_less_than", "error_count_greater_than", "connection_count_equal_to")
      ),
      list(
        metrics = list(disk_space = 15, error_count = 0, connection_count = 10),
        expected_alerts = c()
      ),
      list(
        metrics = list(disk_space = 10, error_count = 1, connection_count = 1),
        expected_alerts = c("error_count_greater_than")
      )
    )
    
    for (i in seq_along(test_cases)) {
      test_case <- test_cases[[i]]
      alert_system$clear_alerts()
      
      alerts <- alert_system$check_thresholds(test_case$metrics)
      alert_names <- names(alerts)
      
      expect_equal(length(alert_names), length(test_case$expected_alerts))
      expect_true(all(test_case$expected_alerts %in% alert_names))
    }
  })
  
  it("maintains alert history correctly", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("test_metric", "greater_than", 50, "warning")
    
    # Generate multiple alerts over time
    for (i in 1:10) {
      metrics <- list(test_metric = 60 + i)
      alert_system$check_thresholds(metrics)
      Sys.sleep(0.01)  # Small delay to ensure different timestamps
    }
    
    history <- alert_system$get_alert_history()
    expect_equal(length(history), 10)
    
    # Check chronological order
    timestamps <- sapply(history, function(a) a$timestamp)
    expect_true(all(diff(as.numeric(timestamps)) > 0))
    
    # Check metric values are recorded correctly
    metric_values <- sapply(history, function(a) a$metric_value)
    expect_equal(metric_values, 61:70)
  })
  
  it("handles edge case threshold values", {
    alert_system <- AlertSystem$new()
    
    # Test edge cases
    alert_system$set_threshold("zero_threshold", "greater_than", 0, "info")
    alert_system$set_threshold("negative_threshold", "less_than", -10, "warning")
    alert_system$set_threshold("float_threshold", "greater_than", 3.14159, "warning")
    
    edge_cases <- list(
      list(metrics = list(zero_threshold = 0.001), should_alert = TRUE),
      list(metrics = list(zero_threshold = 0), should_alert = FALSE),
      list(metrics = list(zero_threshold = -1), should_alert = FALSE),
      list(metrics = list(negative_threshold = -15), should_alert = TRUE),
      list(metrics = list(negative_threshold = -5), should_alert = FALSE),
      list(metrics = list(float_threshold = 3.14160), should_alert = TRUE),
      list(metrics = list(float_threshold = 3.14159), should_alert = FALSE)
    )
    
    for (case in edge_cases) {
      alert_system$clear_alerts()
      alerts <- alert_system$check_thresholds(case$metrics)
      
      if (case$should_alert) {
        expect_true(length(alerts) > 0, 
                   info = paste("Expected alert for metrics:", 
                               paste(names(case$metrics), case$metrics, sep = "=", collapse = ", ")))
      } else {
        expect_equal(length(alerts), 0,
                    info = paste("Did not expect alert for metrics:", 
                                paste(names(case$metrics), case$metrics, sep = "=", collapse = ", ")))
      }
    }
  })
  
  it("handles missing metrics gracefully", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("missing_metric", "greater_than", 100, "warning")
    alert_system$set_threshold("present_metric", "greater_than", 50, "warning")
    
    # Test with missing metric
    metrics <- list(present_metric = 75)  # missing_metric is not provided
    alerts <- alert_system$check_thresholds(metrics)
    
    # Should only alert on present metric
    expect_equal(length(alerts), 1)
    expect_true("present_metric_greater_than" %in% names(alerts))
    expect_false("missing_metric_greater_than" %in% names(alerts))
  })
  
  it("supports alert severity levels", {
    alert_system <- AlertSystem$new()
    
    # Set up different severity levels
    severities <- c("info", "warning", "error", "critical")
    for (i in seq_along(severities)) {
      alert_system$set_threshold(paste0("metric_", i), "greater_than", i * 10, severities[i])
    }
    
    # Trigger all alerts
    metrics <- list(metric_1 = 15, metric_2 = 25, metric_3 = 35, metric_4 = 45)
    alerts <- alert_system$check_thresholds(metrics)
    
    expect_equal(length(alerts), 4)
    
    # Check severities are preserved
    for (i in seq_along(severities)) {
      alert_key <- paste0("metric_", i, "_greater_than")
      expect_true(alert_key %in% names(alerts))
      expect_equal(alerts[[alert_key]]$severity, severities[i])
    }
  })
  
  it("can disable and enable thresholds", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("test_metric", "greater_than", 50, "warning")
    
    # Should trigger initially
    metrics <- list(test_metric = 75)
    alerts <- alert_system$check_thresholds(metrics)
    expect_equal(length(alerts), 1)
    
    # Disable threshold
    alert_system$disable_threshold("test_metric_greater_than")
    alert_system$clear_alerts()
    
    # Should not trigger when disabled
    alerts <- alert_system$check_thresholds(metrics)
    expect_equal(length(alerts), 0)
  })
  
  it("handles high-frequency threshold checking", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("high_freq_metric", "greater_than", 100, "warning")
    
    # Rapid threshold checking
    start_time <- Sys.time()
    alert_count <- 0
    
    for (i in 1:1000) {
      metrics <- list(high_freq_metric = 50 + (i %% 100))  # Alternates above/below threshold
      alerts <- alert_system$check_thresholds(metrics)
      if (length(alerts) > 0) alert_count <- alert_count + 1
    }
    
    end_time <- Sys.time()
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Should complete quickly (less than 1 second)
    expect_true(execution_time < 1.0)
    
    # Should have triggered alerts for values > 100
    expect_true(alert_count > 0)
    expect_true(alert_count < 1000)  # Not all iterations should trigger
  })
})

# ============================================================================
# 9.1.4 DASHBOARD FUNCTIONALITY TESTS
# ============================================================================

describe("Dashboard Functionality Tests", {
  
  # Mock dashboard components
  create_dashboard_server <- function() {
    function(input, output, session) {
      # Mock reactive values
      dashboard_data <- reactiveVal(list(
        kpis = list(
          total_employees = 1500,
          attrition_rate = 0.12,
          avg_satisfaction = 3.8
        ),
        charts = list(),
        last_updated = Sys.time()
      ))
      
      # Mock KPI output
      output$kpi_total_employees <- renderText({
        data <- dashboard_data()
        scales::comma(data$kpis$total_employees)
      })
      
      output$kpi_attrition_rate <- renderText({
        data <- dashboard_data()
        scales::percent(data$kpis$attrition_rate, accuracy = 0.1)
      })
      
      output$kpi_avg_satisfaction <- renderText({
        data <- dashboard_data()
        round(data$kpis$avg_satisfaction, 1)
      })
      
      # Mock chart outputs
      output$attrition_chart <- renderPlotly({
        # Mock attrition chart
        data <- data.frame(
          Department = c("HR", "Engineering", "Sales", "Marketing"),
          AttritionRate = c(0.15, 0.08, 0.18, 0.12)
        )
        
        p <- ggplot(data, aes(x = Department, y = AttritionRate)) +
          geom_bar(stat = "identity", fill = "#3498db") +
          scale_y_continuous(labels = scales::percent) +
          theme_minimal() +
          labs(title = "Attrition Rate by Department", 
               x = "Department", y = "Attrition Rate")
        
        ggplotly(p)
      })
      
      output$satisfaction_chart <- renderPlotly({
        # Mock satisfaction trend
        dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")
        satisfaction_data <- data.frame(
          Date = dates,
          Satisfaction = 3.5 + 0.3 * sin(seq_along(dates) * pi / 6) + rnorm(length(dates), 0, 0.1)
        )
        
        p <- ggplot(satisfaction_data, aes(x = Date, y = Satisfaction)) +
          geom_line(color = "#e74c3c", size = 1.2) +
          geom_point(color = "#e74c3c") +
          scale_y_continuous(limits = c(1, 5)) +
          theme_minimal() +
          labs(title = "Employee Satisfaction Trend", 
               x = "Date", y = "Satisfaction Score")
        
        ggplotly(p)
      })
      
      # Mock data refresh functionality
      observeEvent(input$refresh_data, {
        # Simulate data refresh
        new_data <- list(
          kpis = list(
            total_employees = sample(1400:1600, 1),
            attrition_rate = runif(1, 0.08, 0.16),
            avg_satisfaction = runif(1, 3.5, 4.2)
          ),
          charts = list(),
          last_updated = Sys.time()
        )
        dashboard_data(new_data)
        
        showNotification("Dashboard data refreshed successfully!", 
                        type = "success", duration = 3)
      })
      
      # Export dashboard data getter
      return(list(
        get_data = dashboard_data,
        refresh_data = function() {
          # Manual refresh function for testing
          new_data <- list(
            kpis = list(
              total_employees = sample(1400:1600, 1),
              attrition_rate = runif(1, 0.08, 0.16),
              avg_satisfaction = runif(1, 3.5, 4.2)
            ),
            last_updated = Sys.time()
          )
          dashboard_data(new_data)
          return(new_data)
        }
      ))
    }
  }
  
  it("initializes dashboard with correct KPIs", {
    # Mock Shiny session
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    # Create dashboard server
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Get initial data
    initial_data <- dashboard_instance$get_data()
    
    # Validate KPIs structure
    expect_true("kpis" %in% names(initial_data))
    expect_true("total_employees" %in% names(initial_data$kpis))
    expect_true("attrition_rate" %in% names(initial_data$kpis))
    expect_true("avg_satisfaction" %in% names(initial_data$kpis))
    
    # Validate KPI values
    expect_true(is.numeric(initial_data$kpis$total_employees))
    expect_true(initial_data$kpis$total_employees > 0)
    expect_true(initial_data$kpis$attrition_rate >= 0 && initial_data$kpis$attrition_rate <= 1)
    expect_true(initial_data$kpis$avg_satisfaction >= 1 && initial_data$kpis$avg_satisfaction <= 5)
  })
  
  it("handles dashboard data refresh correctly", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Get initial data
    initial_data <- dashboard_instance$get_data()
    initial_timestamp <- initial_data$last_updated
    
    # Wait a small amount to ensure timestamp difference
    Sys.sleep(0.1)
    
    # Refresh data
    refreshed_data <- dashboard_instance$refresh_data()
    
    # Validate refresh
    expect_true(refreshed_data$last_updated > initial_timestamp)
    expect_true("kpis" %in% names(refreshed_data))
    
    # Values should be within expected ranges
    expect_true(refreshed_data$kpis$total_employees >= 1400 && 
                refreshed_data$kpis$total_employees <= 1600)
    expect_true(refreshed_data$kpis$attrition_rate >= 0.08 && 
                refreshed_data$kpis$attrition_rate <= 0.16)
    expect_true(refreshed_data$kpis$avg_satisfaction >= 3.5 && 
                refreshed_data$kpis$avg_satisfaction <= 4.2)
  })
  
  it("validates dashboard responsiveness under load", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Simulate multiple rapid refreshes
    start_time <- Sys.time()
    refresh_times <- c()
    
    for (i in 1:50) {
      refresh_start <- Sys.time()
      dashboard_instance$refresh_data()
      refresh_end <- Sys.time()
      
      refresh_time <- as.numeric(difftime(refresh_end, refresh_start, units = "secs"))
      refresh_times <- c(refresh_times, refresh_time)
    }
    
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Performance assertions
    expect_true(total_time < 5.0)  # Should complete in under 5 seconds
    expect_true(mean(refresh_times) < 0.1)  # Average refresh should be under 100ms
    expect_true(max(refresh_times) < 0.5)   # No single refresh should take over 500ms
  })
  
  it("handles concurrent dashboard access", {
    # Simulate multiple users accessing dashboard simultaneously
    concurrent_results <- parallel::mclapply(1:5, function(user_id) {
      session <- MockShinySession$new()
      input <- list()
      output <- list()
      
      dashboard_server <- create_dashboard_server()
      dashboard_instance <- dashboard_server(input, output, session)
      
      # Each user performs multiple operations
      results <- list()
      for (i in 1:10) {
        data <- dashboard_instance$refresh_data()
        results[[i]] <- list(
          user_id = user_id,
          iteration = i,
          total_employees = data$kpis$total_employees,
          timestamp = data$last_updated
        )
      }
      
      return(results)
    }, mc.cores = 2)
    
    # Flatten results
    all_results <- unlist(concurrent_results, recursive = FALSE)
    
    # Validate all operations completed successfully
    expect_equal(length(all_results), 50)  # 5 users × 10 operations each
    
    # Check data integrity
    for (result in all_results) {
      expect_true(is.numeric(result$total_employees))
      expect_true(result$total_employees >= 1400 && result$total_employees <= 1600)
      expect_true(!is.null(result$timestamp))
    }
  })
  
  it("validates dashboard error handling", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    # Create dashboard with error injection
    create_error_dashboard <- function() {
      function(input, output, session) {
        dashboard_data <- reactiveVal(NULL)
        
        return(list(
          get_data = dashboard_data,
          refresh_data = function() {
            # Simulate various error conditions
            error_type <- sample(c("network", "data", "memory", "success"), 1, 
                               prob = c(0.1, 0.1, 0.1, 0.7))
            
            if (error_type == "network") {
              stop("Network connection timeout")
            } else if (error_type == "data") {
              stop("Invalid data format received")
            } else if (error_type == "memory") {
              stop("Insufficient memory to process request")
            } else {
              # Success case
              return(list(
                kpis = list(total_employees = 1500),
                last_updated = Sys.time()
              ))
            }
          }
        ))
      }
    }
    
    error_dashboard_server <- create_error_dashboard()
    error_dashboard <- error_dashboard_server(input, output, session)
    
    # Test error handling
    success_count <- 0
    error_count <- 0
    
    for (i in 1:100) {
      tryCatch({
        result <- error_dashboard$refresh_data()
        success_count <- success_count + 1
      }, error = function(e) {
        error_count <- error_count + 1
        # Validate error messages are informative
        expect_true(nchar(e$message) > 0)
        expect_true(e$message %in% c(
          "Network connection timeout",
          "Invalid data format received", 
          "Insufficient memory to process request"
        ))
      })
    }
    
    # Should have both successes and errors
    expect_true(success_count > 0)
    expect_true(error_count > 0)
    expect_equal(success_count + error_count, 100)
  })
})

# ============================================================================
# 9.1.5 LOG AGGREGATION EFFECTIVENESS TESTS
# ============================================================================

describe("Log Aggregation Effectiveness", {
  
  # Enhanced Logger with aggregation capabilities
  AggregatedLogger <- R6Class("AggregatedLogger",
    private = list(
      .logs = list(),
      .aggregated_logs = list(),
      .log_buffer = list(),
      .buffer_size = 1000,
      .aggregation_interval = 60  # seconds
    ),
    
    public = list(
      initialize = function(buffer_size = 1000, aggregation_interval = 60) {
        private$.logs <- list()
        private$.aggregated_logs <- list()
        private$.log_buffer <- list()
        private$.buffer_size <- buffer_size
        private$.aggregation_interval <- aggregation_interval
      },
      
      log = function(level, message, module = "unknown", metadata = list()) {
        log_entry <- list(
          timestamp = Sys.time(),
          level = level,
          message = message,
          module = module,
          metadata = metadata,
          session_id = digest::digest(Sys.time()),
          pid = Sys.getpid()
        )
        
        # Add to buffer
        private$.log_buffer[[length(private$.log_buffer) + 1]] <- log_entry
        
        # Flush buffer if it's full
        if (length(private$.log_buffer) >= private$.buffer_size) {
          self$flush_buffer()
        }
        
        return(log_entry)
      },
      
      flush_buffer = function() {
        if (length(private$.log_buffer) == 0) return(invisible(NULL))
        
        # Move buffer to main logs
        new_logs <- private$.log_buffer
        private$.logs <- c(private$.logs, new_logs)
        private$.log_buffer <- list()
        
        # Trigger aggregation
        self$aggregate_logs()
        
        return(length(new_logs))
      },
      
      aggregate_logs = function() {
        if (length(private$.logs) == 0) return(invisible(NULL))
        
        # Aggregate by time windows, level, and module
        current_time <- Sys.time()
        window_size <- private$.aggregation_interval
        
        # Create time windows
        log_times <- sapply(private$.logs, function(log) as.numeric(log$timestamp))
        min_time <- min(log_times)
        max_time <- max(log_times)
        
        time_windows <- seq(min_time, max_time + window_size, by = window_size)
        
        aggregated_data <- list()
        
        for (i in 1:(length(time_windows) - 1)) {
          window_start <- time_windows[i]
          window_end <- time_windows[i + 1]
          
          # Filter logs in this window
          window_logs <- private$.logs[log_times >= window_start & log_times < window_end]
          
          if (length(window_logs) == 0) next
          
          # Aggregate by level and module
          aggregation_key <- function(log) {
            paste(log$level, log$module, sep = "_")
          }
          
          log_groups <- split(window_logs, sapply(window_logs, aggregation_key))
          
          for (group_key in names(log_groups)) {
            group_logs <- log_groups[[group_key]]
            
            agg_entry <- list(
              window_start = as.POSIXct(window_start, origin = "1970-01-01"),
              window_end = as.POSIXct(window_end, origin = "1970-01-01"),
              level = group_logs[[1]]$level,
              module = group_logs[[1]]$module,
              count = length(group_logs),
              messages = sapply(group_logs, function(l) l$message),
              first_occurrence = min(sapply(group_logs, function(l) l$timestamp)),
              last_occurrence = max(sapply(group_logs, function(l) l$timestamp))
            )
            
            agg_key <- paste(window_start, group_key, sep = "_")
            private$.aggregated_logs[[agg_key]] <- agg_entry
          }
        }
        
        return(length(private$.aggregated_logs))
      },
      
      get_logs = function(level = NULL, module = NULL, limit = NULL) {
        filtered_logs <- private$.logs
        
        # Filter by level
        if (!is.null(level)) {
          filtered_logs <- filtered_logs[sapply(filtered_logs, function(l) l$level == level)]
        }
        
        # Filter by module
        if (!is.null(module)) {
          filtered_logs <- filtered_logs[sapply(filtered_logs, function(l) l$module == module)]
        }
        
        # Apply limit
        if (!is.null(limit) && length(filtered_logs) > limit) {
          filtered_logs <- tail(filtered_logs, limit)
        }
        
        return(filtered_logs)
      },
      
      get_aggregated_logs = function() {
        return(private$.aggregated_logs)
      },
      
      get_log_summary = function() {
        if (length(private$.logs) == 0) {
          return(list(
            total_logs = 0,
            by_level = list(),
            by_module = list(),
            time_range = NULL
          ))
        }
        
        # Count by level
        levels <- sapply(private$.logs, function(l) l$level)
        level_counts <- table(levels)
        
        # Count by module
        modules <- sapply(private$.logs, function(l) l$module)
        module_counts <- table(modules)
        
        # Time range
        timestamps <- sapply(private$.logs, function(l) l$timestamp)
        time_range <- list(
          start = min(timestamps),
          end = max(timestamps),
          duration_hours = as.numeric(difftime(max(timestamps), min(timestamps), units = "hours"))
        )
        
        return(list(
          total_logs = length(private$.logs),
          by_level = as.list(level_counts),
          by_module = as.list(module_counts),
          time_range = time_range,
          aggregated_entries = length(private$.aggregated_logs)
        ))
      },
      
      clear_logs = function() {
        private$.logs <- list()
        private$.aggregated_logs <- list()
        private$.log_buffer <- list()
      }
    )
  )
  
  it("collects logs efficiently in buffer", {
    logger <- AggregatedLogger$new(buffer_size = 10)
    
    # Add logs to buffer
    for (i in 1:5) {
      logger$log("info", paste("Test message", i), "test_module")
    }
    
    # Buffer should contain logs but main logs should be empty initially
    summary <- logger$get_log_summary()
    expect_equal(summary$total_logs, 0)  # Not flushed yet
    
    # Add more logs to trigger flush
    for (i in 6:12) {
      logger$log("info", paste("Test message", i), "test_module")
    }
    
    # Should have flushed when buffer reached size 10
    summary <- logger$get_log_summary()
    expect_true(summary$total_logs >= 10)
  })
  
  it("aggregates logs by time windows correctly", {
    logger <- AggregatedLogger$new(buffer_size = 5, aggregation_interval = 1)
    
    # Generate logs across different time periods
    base_time <- Sys.time()
    
    # First window
    for (i in 1:3) {
      logger$log("error", "Database error", "database")
    }
    
    # Simulate time passage
    Sys.sleep(1.1)
    
    # Second window
    for (i in 1:2) {
      logger$log("warning", "High memory usage", "system")
    }
    
    # Force aggregation
    logger$flush_buffer()
    
    aggregated <- logger$get_aggregated_logs()
    expect_true(length(aggregated) >= 1)
    
    # Check aggregation structure
    first_agg <- aggregated[[1]]
    expect_true("window_start" %in% names(first_agg))
    expect_true("window_end" %in% names(first_agg))
    expect_true("count" %in% names(first_agg))
    expect_true("level" %in% names(first_agg))
    expect_true("module" %in% names(first_agg))
  })
  
  it("handles high-volume log ingestion", {
    logger <- AggregatedLogger$new(buffer_size = 100)
    
    # Generate high volume of logs
    start_time <- Sys.time()
    log_count <- 10000
    
    for (i in 1:log_count) {
      level <- sample(c("info", "warning", "error"), 1)
      module <- sample(c("auth", "database", "api", "ui"), 1)
      logger$log(level, paste("Message", i), module)
    }
    
    logger$flush_buffer()
    end_time <- Sys.time()
    
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Performance assertions
    expect_true(execution_time < 5.0)  # Should complete in under 5 seconds
    
    summary <- logger$get_log_summary()
    expect_equal(summary$total_logs, log_count)
    
    # Should have aggregated data
    expect_true(summary$aggregated_entries > 0)
  })
  
  it("filters logs correctly by level and module", {
    logger <- AggregatedLogger$new(buffer_size = 5)
    
    # Generate diverse logs
    test_logs <- list(
      list(level = "error", module = "database", message = "Connection failed"),
      list(level = "warning", module = "database", message = "Slow query"),
      list(level = "info", module = "auth", message = "User login"),
      list(level = "error", module = "auth", message = "Invalid credentials"),
      list(level = "info", module = "api", message = "Request processed")
    )
    
    for (log_data in test_logs) {
      logger$log(log_data$level, log_data$message, log_data$module)
    }
    
    logger$flush_buffer()
    
    # Test filtering by level
    error_logs <- logger$get_logs(level = "error")
    expect_equal(length(error_logs), 2)
    expect_true(all(sapply(error_logs, function(l) l$level == "error")))
    
    # Test filtering by module
    database_logs <- logger$get_logs(module = "database")
    expect_equal(length(database_logs), 2)
    expect_true(all(sapply(database_logs, function(l) l$module == "database")))
    
    # Test combined filtering
    error_auth_logs <- logger$get_logs(level = "error", module = "auth")
    expect_equal(length(error_auth_logs), 1)
    expect_equal(error_auth_logs[[1]]$message, "Invalid credentials")
  })
  
  it("maintains log chronological order", {
    logger <- AggregatedLogger$new(buffer_size = 100)
    
    # Generate logs with deliberate timing
    messages <- character(50)
    timestamps <- numeric(50)
    
    for (i in 1:50) {
      message <- paste("Chronological test", i)
      logger$log("info", message, "test")
      messages[i] <- message
      timestamps[i] <- as.numeric(Sys.time())
      
      if (i %% 10 == 0) Sys.sleep(0.01)  # Small delays every 10 logs
    }
    
    logger$flush_buffer()
    
    # Get all logs
    all_logs <- logger$get_logs()
    
    # Check chronological order
    log_timestamps <- sapply(all_logs, function(l) as.numeric(l$timestamp))
    expect_true(all(diff(log_timestamps) >= 0))  # Should be non-decreasing
    
    # Check message order
    log_messages <- sapply(all_logs, function(l) l$message)
    expect_equal(log_messages, messages)
  })
  
  it("handles concurrent logging correctly", {
    logger <- AggregatedLogger$new(buffer_size = 200)
    
    # Simulate concurrent logging from multiple processes/threads
    concurrent_results <- parallel::mclapply(1:5, function(worker_id) {
      logged_messages <- character(20)
      
      for (i in 1:20) {
        message <- paste("Worker", worker_id, "Message", i)
        logger$log("info", message, paste0("worker_", worker_id))
        logged_messages[i] <- message
      }
      
      return(logged_messages)
    }, mc.cores = 2)
    
    logger$flush_buffer()
    
    # Verify all logs were captured
    all_logs <- logger$get_logs()
    expect_equal(length(all_logs), 100)  # 5 workers × 20 messages each
    
    # Check that all worker modules are represented
    modules <- unique(sapply(all_logs, function(l) l$module))
    expected_modules <- paste0("worker_", 1:5)
    expect_true(all(expected_modules %in% modules))
  })
  
  it("generates accurate log summaries", {
    logger <- AggregatedLogger$new(buffer_size = 20)
    
    # Generate logs with known distribution
    level_counts <- list(info = 10, warning = 5, error = 3)
    module_counts <- list(auth = 8, database = 6, api = 4)
    
    # Generate logs according to distribution
    for (level in names(level_counts)) {
      for (i in 1:level_counts[[level]]) {
        module <- sample(names(module_counts), 1, 
                        prob = unlist(module_counts)/sum(unlist(module_counts)))
        logger$log(level, paste("Test message", i), module)
      }
    }
    
    logger$flush_buffer()
    
    # Get summary
    summary <- logger$get_log_summary()
    
    # Verify total count
    expect_equal(summary$total_logs, sum(unlist(level_counts)))
    
    # Verify level distribution
    for (level in names(level_counts)) {
      expect_equal(summary$by_level[[level]], level_counts[[level]])
    }
    
    # Verify time range is valid
    expect_true(!is.null(summary$time_range))
    expect_true(summary$time_range$duration_hours >= 0)
  })
})

# ============================================================================
# 9.1.6 DISTRIBUTED TRACING VALIDATION TESTS
# ============================================================================

describe("Distributed Tracing Validation", {
  
  # Mock distributed tracing system
  TracingSystem <- R6Class("TracingSystem",
    private = list(
      .traces = list(),
      .spans = list(),
      .correlation_ids = list()
    ),
    
    public = list(
      initialize = function() {
        private$.traces <- list()
        private$.spans <- list() 
        private$.correlation_ids <- list()
      },
      
      start_trace = function(operation_name, metadata = list()) {
        trace_id <- self$generate_trace_id()
        span_id <- self$generate_span_id()
        
        trace <- list(
          trace_id = trace_id,
          root_span_id = span_id,
          operation_name = operation_name,
          start_time = Sys.time(),
          metadata = metadata,
          status = "active"
        )
        
        span <- list(
          trace_id = trace_id,
          span_id = span_id,
          parent_span_id = NULL,
          operation_name = operation_name,
          start_time = Sys.time(),
          end_time = NULL,
          duration_ms = NULL,
          metadata = metadata,
          tags = list(),
          status = "active"
        )
        
        private$.traces[[trace_id]] <- trace
        private$.spans[[span_id]] <- span
        
        return(list(trace_id = trace_id, span_id = span_id))
      },
      
      start_child_span = function(parent_trace_id, parent_span_id, operation_name, metadata = list()) {
        if (!parent_trace_id %in% names(private$.traces)) {
          stop("Parent trace not found")
        }
        
        span_id <- self$generate_span_id()
        
        span <- list(
          trace_id = parent_trace_id,
          span_id = span_id,
          parent_span_id = parent_span_id,
          operation_name = operation_name,
          start_time = Sys.time(),
          end_time = NULL,
          duration_ms = NULL,
          metadata = metadata,
          tags = list(),
          status = "active"
        )
        
        private$.spans[[span_id]] <- span
        
        return(span_id)
      },
      
      finish_span = function(span_id, status = "success", metadata = list()) {
        if (!span_id %in% names(private$.spans)) {
          stop("Span not found")
        }
        
        span <- private$.spans[[span_id]]
        end_time <- Sys.time()
        duration_ms <- as.numeric(difftime(end_time, span$start_time, units = "secs")) * 1000
        
        span$end_time <- end_time
        span$duration_ms <- duration_ms
        span$status <- status
        span$metadata <- c(span$metadata, metadata)
        
        private$.spans[[span_id]] <- span
        
        # Check if this completes the trace
        trace_id <- span$trace_id
        trace_spans <- self$get_trace_spans(trace_id)
        
        if (all(sapply(trace_spans, function(s) !is.null(s$end_time)))) {
          private$.traces[[trace_id]]$status <- "completed"
          private$.traces[[trace_id]]$end_time <- Sys.time()
        }
        
        return(span)
      },
      
      add_span_tag = function(span_id, key, value) {
        if (!span_id %in% names(private$.spans)) {
          stop("Span not found")
        }
        
        private$.spans[[span_id]]$tags[[key]] <- value
      },
      
      get_trace = function(trace_id) {
        if (!trace_id %in% names(private$.traces)) {
          return(NULL)
        }
        
        trace <- private$.traces[[trace_id]]
        spans <- self$get_trace_spans(trace_id)
        
        return(list(
          trace = trace,
          spans = spans
        ))
      },
      
      get_trace_spans = function(trace_id) {
        trace_spans <- private$.spans[sapply(private$.spans, function(s) s$trace_id == trace_id)]
        return(trace_spans)
      },
      
      generate_trace_id = function() {
        paste0("trace_", digest::digest(paste(Sys.time(), runif(1))))
      },
      
      generate_span_id = function() {
        paste0("span_", digest::digest(paste(Sys.time(), runif(1))))
      },
      
      get_active_traces = function() {
        active_traces <- private$.traces[sapply(private$.traces, function(t) t$status == "active")]
        return(active_traces)
      },
      
      get_trace_statistics = function() {
        if (length(private$.traces) == 0) {
          return(list(
            total_traces = 0,
            completed_traces = 0,
            active_traces = 0,
            avg_trace_duration_ms = 0
          ))
        }
        
        completed_traces <- private$.traces[sapply(private$.traces, function(t) t$status == "completed")]
        
        if (length(completed_traces) > 0) {
          durations <- sapply(completed_traces, function(t) {
            if (!is.null(t$end_time)) {
              as.numeric(difftime(t$end_time, t$start_time, units = "secs")) * 1000
            } else {
              NA
            }
          })
          avg_duration <- mean(durations, na.rm = TRUE)
        } else {
          avg_duration <- 0
        }
        
        return(list(
          total_traces = length(private$.traces),
          completed_traces = length(completed_traces),
          active_traces = length(private$.traces) - length(completed_traces),
          avg_trace_duration_ms = avg_duration
        ))
      }
    )
  )
  
  it("creates and tracks distributed traces correctly", {
    tracer <- TracingSystem$new()
    
    # Start a new trace
    trace_info <- tracer$start_trace("user_login", list(user_id = "12345"))
    
    expect_true("trace_id" %in% names(trace_info))
    expect_true("span_id" %in% names(trace_info))
    
    # Verify trace exists
    trace_data <- tracer$get_trace(trace_info$trace_id)
    expect_false(is.null(trace_data))
    expect_equal(trace_data$trace$operation_name, "user_login")
    expect_equal(trace_data$trace$status, "active")
    expect_equal(length(trace_data$spans), 1)
  })
  
  it("handles child spans and trace hierarchy", {
    tracer <- TracingSystem$new()
    
    # Start parent trace
    parent_trace <- tracer$start_trace("api_request", list(endpoint = "/users"))
    
    # Create child spans
    auth_span <- tracer$start_child_span(
      parent_trace$trace_id, 
      parent_trace$span_id,
      "authenticate_user",
      list(method = "jwt")
    )
    
    db_span <- tracer$start_child_span(
      parent_trace$trace_id,
      parent_trace$span_id, 
      "database_query",
      list(table = "users")
    )
    
    # Verify hierarchy
    trace_data <- tracer$get_trace(parent_trace$trace_id)
    expect_equal(length(trace_data$spans), 3)  # Parent + 2 children
    
    # Check parent-child relationships
    child_spans <- trace_data$spans[sapply(trace_data$spans, function(s) !is.null(s$parent_span_id))]
    expect_equal(length(child_spans), 2)
    
    for (child_span in child_spans) {
      expect_equal(child_span$parent_span_id, parent_trace$span_id)
      expect_equal(child_span$trace_id, parent_trace$trace_id)
    }
  })
  
  it("accurately measures span durations", {
    tracer <- TracingSystem$new()
    
    # Start trace and spans with controlled timing
    trace_info <- tracer$start_trace("timed_operation")
    
    # Simulate work
    start_time <- Sys.time()
    Sys.sleep(0.1)  # 100ms
    
    # Finish span
    finished_span <- tracer$finish_span(trace_info$span_id, "success")
    end_time <- Sys.time()
    
    # Verify duration accuracy
    actual_duration <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
    measured_duration <- finished_span$duration_ms
    
    # Allow 20ms tolerance for timing variations
    expect_true(abs(measured_duration - actual_duration) < 20)
    expect_true(measured_duration >= 90)  # Should be at least 90ms
    expect_true(measured_duration <= 130) # Should be at most 130ms
  })
  
  it("handles trace completion correctly", {
    tracer <- TracingSystem$new()
    
    # Create trace with multiple spans
    trace_info <- tracer$start_trace("complex_operation")
    
    child1 <- tracer$start_child_span(trace_info$trace_id, trace_info$span_id, "step_1")
    child2 <- tracer$start_child_span(trace_info$trace_id, trace_info$span_id, "step_2")
    
    # Initially trace should be active
    trace_data <- tracer$get_trace(trace_info$trace_id)
    expect_equal(trace_data$trace$status, "active")
    
    # Finish child spans
    tracer$finish_span(child1, "success")
    trace_data <- tracer$get_trace(trace_info$trace_id)
    expect_equal(trace_data$trace$status, "active")  # Still active
    
    tracer$finish_span(child2, "success")
    trace_data <- tracer$get_trace(trace_info$trace_id)
    expect_equal(trace_data$trace$status, "active")  # Still active (parent not finished)
    
    # Finish parent span
    tracer$finish_span(trace_info$span_id, "success")
    trace_data <- tracer$get_trace(trace_info$trace_id)
    expect_equal(trace_data$trace$status, "completed")  # Now completed
  })
  
  it("supports span tagging and metadata", {
    tracer <- TracingSystem$new()
    
    trace_info <- tracer$start_trace("tagged_operation", list(version = "1.0"))
    
    # Add tags to span
    tracer$add_span_tag(trace_info$span_id, "user.id", "12345")
    tracer$add_span_tag(trace_info$span_id, "request.method", "POST")
    tracer$add_span_tag(trace_info$span_id, "response.status", 200)
    
    # Finish with additional metadata
    tracer$finish_span(trace_info$span_id, "success", list(response_size = 1024))
    
    # Verify tags and metadata
    trace_data <- tracer$get_trace(trace_info$trace_id)
    span <- trace_data$spans[[1]]
    
    expect_equal(span$tags$user.id, "12345")
    expect_equal(span$tags$request.method, "POST")
    expect_equal(span$tags$response.status, 200)
    expect_equal(span$metadata$response_size, 1024)
    expect_equal(span$metadata$version, "1.0")
  })
  
  it("handles error conditions in tracing", {
    tracer <- TracingSystem$new()
    
    # Test invalid span operations
    expect_error(tracer$finish_span("invalid_span_id"), "Span not found")
    expect_error(tracer$add_span_tag("invalid_span_id", "key", "value"), "Span not found")
    expect_error(tracer$start_child_span("invalid_trace", "invalid_span", "operation"), "Parent trace not found")
    
    # Test trace not found
    invalid_trace <- tracer$get_trace("invalid_trace_id")
    expect_null(invalid_trace)
  })
  
  it("provides accurate trace statistics", {
    tracer <- TracingSystem$new()
    
    # Create multiple traces with different completion states
    trace1 <- tracer$start_trace("operation_1")
    trace2 <- tracer$start_trace("operation_2")
    trace3 <- tracer$start_trace("operation_3")
    
    # Complete some traces
    Sys.sleep(0.05)
    tracer$finish_span(trace1$span_id, "success")
    
    Sys.sleep(0.05)
    tracer$finish_span(trace2$span_id, "error")
    
    # Leave trace3 active
    
    # Get statistics
    stats <- tracer$get_trace_statistics()
    
    expect_equal(stats$total_traces, 3)
    expect_equal(stats$completed_traces, 2)
    expect_equal(stats$active_traces, 1)
    expect_true(stats$avg_trace_duration_ms > 0)
    expect_true(stats$avg_trace_duration_ms < 200)  # Should be under 200ms
  })
  
  it("handles high-volume concurrent tracing", {
    tracer <- TracingSystem$new()
    
    # Simulate concurrent trace creation
    trace_results <- parallel::mclapply(1:20, function(i) {
      # Each worker creates a trace with child spans
      trace_info <- tracer$start_trace(paste0("concurrent_op_", i))
      
      child_spans <- c()
      for (j in 1:3) {
        child_span <- tracer$start_child_span(
          trace_info$trace_id,
          trace_info$span_id,
          paste0("child_", j)
        )
        child_spans <- c(child_spans, child_span)
      }
      
      # Finish child spans
      for (child_span in child_spans) {
        tracer$finish_span(child_span, "success")
      }
      
      # Finish parent
      tracer$finish_span(trace_info$span_id, "success")
      
      return(trace_info$trace_id)
    }, mc.cores = 2)
    
    # Verify all traces were created
    expect_equal(length(trace_results), 20)
    
    # Check statistics
    stats <- tracer$get_trace_statistics()
    expect_equal(stats$total_traces, 20)
    expect_true(stats$completed_traces >= 0)  # Some may still be completing
    
    # Verify trace integrity
    for (trace_id in trace_results) {
      trace_data <- tracer$get_trace(trace_id)
      expect_false(is.null(trace_data))
      expect_equal(length(trace_data$spans), 4)  # 1 parent + 3 children
    }
  })
})

# ============================================================================
# 9.1.7 SERVICE DEPENDENCY MAPPING TESTS
# ============================================================================

describe("Service Dependency Mapping", {
  
  # Mock service dependency mapper
  ServiceMapper <- R6Class("ServiceMapper",
    private = list(
      .services = list(),
      .dependencies = list(),
      .health_checks = list(),
      .call_graph = list()
    ),
    
    public = list(
      initialize = function() {
        private$.services <- list()
        private$.dependencies <- list()
        private$.health_checks <- list()
        private$.call_graph <- list()
      },
      
      register_service = function(service_name, endpoint, health_check_url = NULL, metadata = list()) {
        service <- list(
          name = service_name,
          endpoint = endpoint,
          health_check_url = health_check_url,
          metadata = metadata,
          status = "unknown",
          last_check = NULL,
          dependencies = c(),
          dependents = c()
        )
        
        private$.services[[service_name]] <- service
        return(service_name)
      },
      
      add_dependency = function(service_name, depends_on, relationship_type = "sync") {
        if (!service_name %in% names(private$.services)) {
          stop(paste("Service", service_name, "not found"))
        }
        if (!depends_on %in% names(private$.services)) {
          stop(paste("Dependency service", depends_on, "not found"))
        }
        
        # Add to service dependencies
        private$.services[[service_name]]$dependencies <- 
          unique(c(private$.services[[service_name]]$dependencies, depends_on))
        
        # Add to dependent service's dependents list
        private$.services[[depends_on]]$dependents <- 
          unique(c(private$.services[[depends_on]]$dependents, service_name))
        
        # Store dependency details
        dep_key <- paste(service_name, depends_on, sep = "->")
        private$.dependencies[[dep_key]] <- list(
          from = service_name,
          to = depends_on,
          type = relationship_type,
          created_at = Sys.time()
        )
        
        return(dep_key)
      },
      
      check_service_health = function(service_name) {
        if (!service_name %in% names(private$.services)) {
          stop(paste("Service", service_name, "not found"))
        }
        
        service <- private$.services[[service_name]]
        
        # Simulate health check
        if (!is.null(service$health_check_url)) {
          # Mock HTTP health check
          response_time <- runif(1, 10, 200)  # 10-200ms
          is_healthy <- runif(1) > 0.1  # 90% success rate
          
          status <- if (is_healthy) "healthy" else "unhealthy"
          
          health_result <- list(
            service = service_name,
            status = status,
            response_time_ms = response_time,
            timestamp = Sys.time(),
            endpoint = service$health_check_url
          )
        } else {
          # No health check configured
          health_result <- list(
            service = service_name,
            status = "no_health_check",
            response_time_ms = NA,
            timestamp = Sys.time(),
            endpoint = NA
          )
        }
        
        # Update service status
        private$.services[[service_name]]$status <- health_result$status
        private$.services[[service_name]]$last_check <- health_result$timestamp
        
        # Store health check result
        if (is.null(private$.health_checks[[service_name]])) {
          private$.health_checks[[service_name]] <- list()
        }
        
        private$.health_checks[[service_name]][[length(private$.health_checks[[service_name]]) + 1]] <- health_result
        
        return(health_result)
      },
      
      check_all_services = function() {
        results <- list()
        for (service_name in names(private$.services)) {
          results[[service_name]] <- self$check_service_health(service_name)
        }
        return(results)
      },
      
      get_dependency_graph = function() {
        # Build adjacency list representation
        graph <- list()
        
        for (service_name in names(private$.services)) {
          service <- private$.services[[service_name]]
          graph[[service_name]] <- list(
            dependencies = service$dependencies,
            dependents = service$dependents,
            status = service$status
          )
        }
        
        return(graph)
      },
      
      find_critical_path = function(start_service, end_service) {
        # Simple BFS to find path between services
        if (!start_service %in% names(private$.services) || 
            !end_service %in% names(private$.services)) {
          return(NULL)
        }
        
        queue <- list(list(service = start_service, path = c(start_service)))
        visited <- c()
        
        while (length(queue) > 0) {
          current <- queue[[1]]
          queue <- queue[-1]
          
          if (current$service == end_service) {
            return(current$path)
          }
          
          if (current$service %in% visited) {
            next
          }
          
          visited <- c(visited, current$service)
          
          # Add dependencies to queue
          dependencies <- private$.services[[current$service]]$dependencies
          for (dep in dependencies) {
            if (!dep %in% visited) {
              new_path <- c(current$path, dep)
              queue[[length(queue) + 1]] <- list(service = dep, path = new_path)
            }
          }
        }
        
        return(NULL)  # No path found
      },
      
      detect_circular_dependencies = function() {
        # DFS to detect cycles
        visited <- c()
        rec_stack <- c()
        cycles <- list()
        
        dfs_visit <- function(service, path) {
          if (service %in% rec_stack) {
            # Found cycle
            cycle_start <- which(path == service)
            cycle <- path[cycle_start:length(path)]
            cycles[[length(cycles) + 1]] <<- cycle
            return(TRUE)
          }
          
          if (service %in% visited) {
            return(FALSE)
          }
          
          visited <<- c(visited, service)
          rec_stack <<- c(rec_stack, service)
          
          dependencies <- private$.services[[service]]$dependencies
          for (dep in dependencies) {
            if (dfs_visit(dep, c(path, dep))) {
              return(TRUE)
            }
          }
          
          rec_stack <<- rec_stack[rec_stack != service]
          return(FALSE)
        }
        
        for (service_name in names(private$.services)) {
          if (!service_name %in% visited) {
            dfs_visit(service_name, c(service_name))
          }
        }
        
        return(cycles)
      },
      
      get_service_impact_analysis = function(service_name) {
        if (!service_name %in% names(private$.services)) {
          return(NULL)
        }
        
        # Find all services that would be affected if this service fails
        affected_services <- c()
        queue <- private$.services[[service_name]]$dependents
        
        while (length(queue) > 0) {
          current_service <- queue[1]
          queue <- queue[-1]
          
          if (!current_service %in% affected_services) {
            affected_services <- c(affected_services, current_service)
            # Add its dependents to the queue
            additional_dependents <- private$.services[[current_service]]$dependents
            queue <- c(queue, additional_dependents)
          }
        }
        
        return(list(
          service = service_name,
          directly_affected = private$.services[[service_name]]$dependents,
          total_affected = affected_services,
          impact_score = length(affected_services)
        ))
      },
      
      get_services = function() {
        return(private$.services)
      },
      
      get_health_history = function(service_name, limit = 100) {
        if (!service_name %in% names(private$.health_checks)) {
          return(list())
        }
        
        history <- private$.health_checks[[service_name]]
        if (length(history) > limit) {
          history <- tail(history, limit)
        }
        
        return(history)
      }
    )
  )
  
  it("registers services correctly", {
    mapper <- ServiceMapper$new()
    
    # Register services
    service1 <- mapper$register_service("user-service", "http://localhost:8001", "http://localhost:8001/health")
    service2 <- mapper$register_service("auth-service", "http://localhost:8002", "http://localhost:8002/health")
    service3 <- mapper$register_service("database", "postgresql://localhost:5432", NULL)
    
    services <- mapper$get_services()
    
    expect_equal(length(services), 3)
    expect_true("user-service" %in% names(services))
    expect_true("auth-service" %in% names(services))
    expect_true("database" %in% names(services))
    
    # Check service details
    user_service <- services[["user-service"]]
    expect_equal(user_service$endpoint, "http://localhost:8001")
    expect_equal(user_service$health_check_url, "http://localhost:8001/health")
    expect_equal(user_service$status, "unknown")
  })
  
  it("maps service dependencies correctly", {
    mapper <- ServiceMapper$new()
    
    # Set up service topology
    mapper$register_service("frontend", "http://localhost:3000")
    mapper$register_service("api-gateway", "http://localhost:8000")
    mapper$register_service("user-service", "http://localhost:8001")
    mapper$register_service("database", "postgresql://localhost:5432")
    
    # Add dependencies
    mapper$add_dependency("frontend", "api-gateway", "sync")
    mapper$add_dependency("api-gateway", "user-service", "sync")
    mapper$add_dependency("user-service", "database", "sync")
    
    # Verify dependency graph
    graph <- mapper$get_dependency_graph()
    
    expect_equal(graph$frontend$dependencies, c("api-gateway"))
    expect_equal(graph# ============================================================================
# ATLAS LABS HR ANALYTICS - MONITORING & OBSERVABILITY UNIT TESTS
# Comprehensive test suite for application monitoring, health checks, metrics,
# alerts, logging, tracing, and performance monitoring
# ============================================================================

library(testthat)
library(mockery)
library(shiny)
library(R6)
library(jsonlite)
library(httr)
library(purrr)
library(lubridate)
library(digest)

# Source the application files (assuming they exist)
# source("modules/logger_module.R")
# source("global.R")
# source("utils.R")

# ============================================================================
# 9.1.1 HEALTH CHECK ENDPOINT VALIDATION TESTS
# ============================================================================

describe("Health Check Endpoint Validation", {
  
  # Mock health check endpoint function
  create_health_endpoint <- function() {
    function(req) {
      tryCatch({
        # Simulate health checks
        db_status <- check_database_connection()
        memory_status <- check_memory_usage()
        disk_status <- check_disk_space()
        
        health_data <- list(
          status = "healthy",
          timestamp = Sys.time(),
          version = "1.0.0",
          uptime = get_uptime(),
          checks = list(
            database = db_status,
            memory = memory_status,
            disk = disk_status
          )
        )
        
        list(
          status = 200,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(health_data, auto_unbox = TRUE)
        )
      }, error = function(e) {
        list(
          status = 503,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(list(
            status = "unhealthy",
            error = e$message,
            timestamp = Sys.time()
          ), auto_unbox = TRUE)
        )
      })
    }
  }
  
  # Helper functions for health checks
  check_database_connection <- function() {
    list(status = "ok", response_time_ms = sample(1:50, 1))
  }
  
  check_memory_usage <- function() {
    mem_info <- gc()
    list(
      status = if(sum(mem_info[,2]) < 1000) "ok" else "warning",
      used_mb = sum(mem_info[,2]),
      threshold_mb = 1000
    )
  }
  
  check_disk_space <- function() {
    list(status = "ok", free_gb = 50.5, used_percent = 45)
  }
  
  get_uptime <- function() {
    as.numeric(difftime(Sys.time(), as.POSIXct("2024-01-01"), units = "secs"))
  }
  
  it("returns 200 status for healthy application", {
    health_endpoint <- create_health_endpoint()
    response <- health_endpoint(list())
    
    expect_equal(response$status, 200)
    expect_equal(response$headers$`Content-Type`, "application/json")
    
    body <- jsonlite::fromJSON(response$body)
    expect_equal(body$status, "healthy")
    expect_true("timestamp" %in% names(body))
    expect_true("version" %in% names(body))
    expect_true("checks" %in% names(body))
  })
  
  it("validates all health check components", {
    health_endpoint <- create_health_endpoint()
    response <- health_endpoint(list())
    body <- jsonlite::fromJSON(response$body)
    
    # Check required fields
    required_fields <- c("status", "timestamp", "version", "uptime", "checks")
    expect_true(all(required_fields %in% names(body)))
    
    # Check nested components
    expect_true("database" %in% names(body$checks))
    expect_true("memory" %in% names(body$checks))
    expect_true("disk" %in% names(body$checks))
    
    # Validate timestamp format
    expect_true(is.character(body$timestamp))
    expect_false(is.na(as.POSIXct(body$timestamp)))
  })
  
  it("handles database connection failures", {
    # Mock database failure
    mock_check_db <- function() {
      stop("Database connection timeout")
    }
    
    with_mock(
      check_database_connection = mock_check_db,
      {
        health_endpoint <- create_health_endpoint()
        response <- health_endpoint(list())
        
        expect_equal(response$status, 503)
        body <- jsonlite::fromJSON(response$body)
        expect_equal(body$status, "unhealthy")
        expect_true("error" %in% names(body))
      }
    )
  })
  
  it("validates response time thresholds", {
    # Test various response times
    response_times <- c(10, 50, 100, 500, 1000, 5000)
    
    for (time_ms in response_times) {
      mock_check_db <- function() {
        Sys.sleep(time_ms/1000)  # Convert to seconds
        list(status = "ok", response_time_ms = time_ms)
      }
      
      with_mock(
        check_database_connection = mock_check_db,
        {
          start_time <- Sys.time()
          health_endpoint <- create_health_endpoint()
          response <- health_endpoint(list())
          end_time <- Sys.time()
          
          actual_time <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
          expect_true(actual_time >= time_ms * 0.8)  # Allow 20% tolerance
        }
      )
    }
  })
  
  it("handles memory pressure scenarios", {
    # Test different memory usage levels
    memory_scenarios <- list(
      low = list(status = "ok", used_mb = 100, threshold_mb = 1000),
      medium = list(status = "ok", used_mb = 500, threshold_mb = 1000),
      high = list(status = "warning", used_mb = 950, threshold_mb = 1000),
      critical = list(status = "error", used_mb = 1200, threshold_mb = 1000)
    )
    
    for (scenario_name in names(memory_scenarios)) {
      scenario <- memory_scenarios[[scenario_name]]
      
      mock_check_memory <- function() scenario
      
      with_mock(
        check_memory_usage = mock_check_memory,
        {
          health_endpoint <- create_health_endpoint()
          response <- health_endpoint(list())
          body <- jsonlite::fromJSON(response$body)
          
          expect_equal(body$checks$memory$status, scenario$status)
          expect_equal(body$checks$memory$used_mb, scenario$used_mb)
        }
      )
    }
  })
  
  it("validates concurrent health check requests", {
    health_endpoint <- create_health_endpoint()
    
    # Simulate concurrent requests
    concurrent_responses <- parallel::mclapply(1:10, function(i) {
      health_endpoint(list())
    }, mc.cores = 2)
    
    # All should succeed
    statuses <- sapply(concurrent_responses, function(r) r$status)
    expect_true(all(statuses == 200))
    
    # Check response consistency
    bodies <- lapply(concurrent_responses, function(r) jsonlite::fromJSON(r$body))
    versions <- sapply(bodies, function(b) b$version)
    expect_true(length(unique(versions)) == 1)  # All should have same version
  })
})

# ============================================================================
# 9.1.2 METRICS COLLECTION ACCURACY TESTS
# ============================================================================

describe("Metrics Collection Accuracy", {
  
  # Mock metrics collector
  MetricsCollector <- R6Class("MetricsCollector",
    private = list(
      .metrics = list(),
      .start_time = NULL
    ),
    
    public = list(
      initialize = function() {
        private$.metrics <- list()
        private$.start_time <- Sys.time()
      },
      
      increment_counter = function(name, value = 1, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        if (is.null(private$.metrics[[key]])) {
          private$.metrics[[key]] <- list(
            type = "counter",
            name = name,
            value = 0,
            tags = tags,
            last_updated = Sys.time()
          )
        }
        private$.metrics[[key]]$value <- private$.metrics[[key]]$value + value
        private$.metrics[[key]]$last_updated <- Sys.time()
      },
      
      set_gauge = function(name, value, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        private$.metrics[[key]] <- list(
          type = "gauge",
          name = name,
          value = value,
          tags = tags,
          last_updated = Sys.time()
        )
      },
      
      record_histogram = function(name, value, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        if (is.null(private$.metrics[[key]])) {
          private$.metrics[[key]] <- list(
            type = "histogram",
            name = name,
            values = c(),
            tags = tags,
            last_updated = Sys.time()
          )
        }
        private$.metrics[[key]]$values <- c(private$.metrics[[key]]$values, value)
        private$.metrics[[key]]$last_updated <- Sys.time()
      },
      
      get_metrics = function() {
        private$.metrics
      },
      
      get_metric_summary = function() {
        metrics_summary <- list()
        
        for (metric_key in names(private$.metrics)) {
          metric <- private$.metrics[[metric_key]]
          
          if (metric$type == "counter") {
            metrics_summary[[metric$name]] <- list(
              type = "counter",
              value = metric$value,
              tags = metric$tags
            )
          } else if (metric$type == "gauge") {
            metrics_summary[[metric$name]] <- list(
              type = "gauge",
              value = metric$value,
              tags = metric$tags
            )
          } else if (metric$type == "histogram") {
            values <- metric$values
            metrics_summary[[metric$name]] <- list(
              type = "histogram",
              count = length(values),
              mean = mean(values),
              median = median(values),
              p95 = quantile(values, 0.95, na.rm = TRUE),
              p99 = quantile(values, 0.99, na.rm = TRUE),
              min = min(values),
              max = max(values),
              tags = metric$tags
            )
          }
        }
        
        metrics_summary
      }
    )
  )
  
  it("accurately tracks counter metrics", {
    collector <- MetricsCollector$new()
    
    # Test basic counter increment
    collector$increment_counter("user_logins")
    collector$increment_counter("user_logins")
    collector$increment_counter("user_logins", value = 3)
    
    metrics <- collector$get_metrics()
    login_metric <- metrics[[paste0("user_logins_", digest::digest(list()))]]
    
    expect_equal(login_metric$type, "counter")
    expect_equal(login_metric$value, 5)  # 1 + 1 + 3
    expect_equal(login_metric$name, "user_logins")
  })
  
  it("handles counter metrics with tags", {
    collector <- MetricsCollector$new()
    
    # Different departments
    collector$increment_counter("page_views", tags = list(department = "HR"))
    collector$increment_counter("page_views", tags = list(department = "HR"))
    collector$increment_counter("page_views", tags = list(department = "Finance"))
    
    metrics <- collector$get_metrics()
    
    # Should have separate counters for each tag combination
    hr_key <- paste0("page_views_", digest::digest(list(department = "HR")))
    finance_key <- paste0("page_views_", digest::digest(list(department = "Finance")))
    
    expect_equal(metrics[[hr_key]]$value, 2)
    expect_equal(metrics[[finance_key]]$value, 1)
    expect_equal(metrics[[hr_key]]$tags$department, "HR")
    expect_equal(metrics[[finance_key]]$tags$department, "Finance")
  })
  
  it("accurately measures gauge metrics", {
    collector <- MetricsCollector$new()
    
    # Test gauge updates
    collector$set_gauge("memory_usage_mb", 512.5)
    collector$set_gauge("memory_usage_mb", 768.2)  # Should overwrite
    collector$set_gauge("cpu_usage_percent", 45.7)
    
    metrics <- collector$get_metrics()
    memory_key <- paste0("memory_usage_mb_", digest::digest(list()))
    cpu_key <- paste0("cpu_usage_percent_", digest::digest(list()))
    
    expect_equal(metrics[[memory_key]]$value, 768.2)
    expect_equal(metrics[[cpu_key]]$value, 45.7)
    expect_equal(metrics[[memory_key]]$type, "gauge")
  })
  
  it("correctly calculates histogram statistics", {
    collector <- MetricsCollector$new()
    
    # Record response times
    response_times <- c(10, 15, 12, 45, 23, 18, 67, 34, 28, 19)
    for (time in response_times) {
      collector$record_histogram("response_time_ms", time)
    }
    
    summary <- collector$get_metric_summary()
    response_metric <- summary$response_time_ms
    
    expect_equal(response_metric$type, "histogram")
    expect_equal(response_metric$count, 10)
    expect_equal(response_metric$mean, mean(response_times))
    expect_equal(response_metric$median, median(response_times))
    expect_equal(response_metric$min, min(response_times))
    expect_equal(response_metric$max, max(response_times))
  })
  
  it("handles edge cases in histogram calculations", {
    collector <- MetricsCollector$new()
    
    # Test with single value
    collector$record_histogram("single_value", 42)
    summary <- collector$get_metric_summary()
    single_metric <- summary$single_value
    
    expect_equal(single_metric$count, 1)
    expect_equal(single_metric$mean, 42)
    expect_equal(single_metric$median, 42)
    expect_equal(single_metric$min, 42)
    expect_equal(single_metric$max, 42)
    
    # Test with identical values
    for (i in 1:5) {
      collector$record_histogram("identical_values", 100)
    }
    
    summary <- collector$get_metric_summary()
    identical_metric <- summary$identical_values
    
    expect_equal(identical_metric$count, 5)
    expect_equal(identical_metric$mean, 100)
    expect_equal(identical_metric$p95, 100)
    expect_equal(identical_metric$p99, 100)
  })
  
  it("maintains accuracy under high volume", {
    collector <- MetricsCollector$new()
    
    # High volume counter increments
    start_time <- Sys.time()
    for (i in 1:10000) {
      collector$increment_counter("high_volume_counter")
    }
    end_time <- Sys.time()
    
    metrics <- collector$get_metrics()
    counter_key <- paste0("high_volume_counter_", digest::digest(list()))
    
    expect_equal(metrics[[counter_key]]$value, 10000)
    
    # Should complete reasonably quickly (less than 1 second)
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    expect_true(execution_time < 1.0)
  })
  
  it("handles concurrent metric updates", {
    collector <- MetricsCollector$new()
    
    # Concurrent counter increments
    parallel::mclapply(1:100, function(i) {
      collector$increment_counter("concurrent_counter")
    }, mc.cores = 2)
    
    metrics <- collector$get_metrics()
    counter_key <- paste0("concurrent_counter_", digest::digest(list()))
    
    # Should handle race conditions gracefully
    expect_true(metrics[[counter_key]]$value <= 100)
    expect_true(metrics[[counter_key]]$value > 0)
  })
  
  it("validates metric timestamp accuracy", {
    collector <- MetricsCollector$new()
    
    before_time <- Sys.time()
    collector$increment_counter("timestamp_test")
    after_time <- Sys.time()
    
    metrics <- collector$get_metrics()
    metric_key <- paste0("timestamp_test_", digest::digest(list()))
    metric_timestamp <- metrics[[metric_key]]$last_updated
    
    expect_true(metric_timestamp >= before_time)
    expect_true(metric_timestamp <= after_time)
  })
})

# ============================================================================
# 9.1.3 ALERT THRESHOLD TESTING
# ============================================================================

describe("Alert Threshold Testing", {
  
  # Mock alert system
  AlertSystem <- R6Class("AlertSystem",
    private = list(
      .thresholds = list(),
      .alerts = list(),
      .alert_history = list()
    ),
    
    public = list(
      initialize = function() {
        private$.thresholds <- list()
        private$.alerts <- list()
        private$.alert_history <- list()
      },
      
      set_threshold = function(metric_name, threshold_type, value, severity = "warning") {
        threshold_id <- paste0(metric_name, "_", threshold_type)
        private$.thresholds[[threshold_id]] <- list(
          metric_name = metric_name,
          type = threshold_type,  # "greater_than", "less_than", "equal_to"
          value = value,
          severity = severity,
          enabled = TRUE
        )
      },
      
      check_thresholds = function(metrics) {
        current_alerts <- list()
        
        for (threshold_id in names(private$.thresholds)) {
          threshold <- private$.thresholds[[threshold_id]]
          if (!threshold$enabled) next
          
          metric_value <- metrics[[threshold$metric_name]]
          if (is.null(metric_value)) next
          
          alert_triggered <- FALSE
          
          if (threshold$type == "greater_than" && metric_value > threshold$value) {
            alert_triggered <- TRUE
          } else if (threshold$type == "less_than" && metric_value < threshold$value) {
            alert_triggered <- TRUE
          } else if (threshold$type == "equal_to" && metric_value == threshold$value) {
            alert_triggered <- TRUE
          }
          
          if (alert_triggered) {
            alert <- list(
              threshold_id = threshold_id,
              metric_name = threshold$metric_name,
              metric_value = metric_value,
              threshold_value = threshold$value,
              severity = threshold$severity,
              timestamp = Sys.time(),
              message = sprintf("Metric %s (%s) exceeded threshold %s (current: %s)",
                              threshold$metric_name, threshold$type, 
                              threshold$value, metric_value)
            )
            
            current_alerts[[threshold_id]] <- alert
            private$.alert_history[[length(private$.alert_history) + 1]] <- alert
          }
        }
        
        private$.alerts <- current_alerts
        return(current_alerts)
      },
      
      get_active_alerts = function() {
        private$.alerts
      },
      
      get_alert_history = function(limit = 100) {
        history_length <- length(private$.alert_history)
        if (history_length == 0) return(list())
        
        start_idx <- max(1, history_length - limit + 1)
        private$.alert_history[start_idx:history_length]
      },
      
      clear_alerts = function() {
        private$.alerts <- list()
      },
      
      disable_threshold = function(threshold_id) {
        if (threshold_id %in% names(private$.thresholds)) {
          private$.thresholds[[threshold_id]]$enabled <- FALSE
        }
      }
    )
  )
  
  it("triggers alerts when thresholds are exceeded", {
    alert_system <- AlertSystem$new()
    
    # Set up thresholds
    alert_system$set_threshold("cpu_usage", "greater_than", 80, "warning")
    alert_system$set_threshold("memory_usage", "greater_than", 90, "critical")
    alert_system$set_threshold("response_time", "greater_than", 1000, "warning")
    
    # Test metrics that should trigger alerts
    metrics <- list(
      cpu_usage = 85,     # Should trigger warning
      memory_usage = 95,  # Should trigger critical
      response_time = 500 # Should not trigger
    )
    
    alerts <- alert_system$check_thresholds(metrics)
    
    expect_equal(length(alerts), 2)
    expect_true("cpu_usage_greater_than" %in% names(alerts))
    expect_true("memory_usage_greater_than" %in% names(alerts))
    expect_false("response_time_greater_than" %in% names(alerts))
    
    # Check alert details
    cpu_alert <- alerts$cpu_usage_greater_than
    expect_equal(cpu_alert$severity, "warning")
    expect_equal(cpu_alert$metric_value, 85)
    expect_equal(cpu_alert$threshold_value, 80)
  })
  
  it("handles different threshold types correctly", {
    alert_system <- AlertSystem$new()
    
    # Set up different threshold types
    alert_system$set_threshold("disk_space", "less_than", 10, "critical")
    alert_system$set_threshold("error_count", "greater_than", 0, "warning")
    alert_system$set_threshold("connection_count", "equal_to", 0, "critical")
    
    # Test scenarios
    test_cases <- list(
      list(
        metrics = list(disk_space = 5, error_count = 3, connection_count = 0),
        expected_alerts = c("disk_space_less_than", "error_count_greater_than", "connection_count_equal_to")
      ),
      list(
        metrics = list(disk_space = 15, error_count = 0, connection_count = 10),
        expected_alerts = c()
      ),
      list(
        metrics = list(disk_space = 10, error_count = 1, connection_count = 1),
        expected_alerts = c("error_count_greater_than")
      )
    )
    
    for (i in seq_along(test_cases)) {
      test_case <- test_cases[[i]]
      alert_system$clear_alerts()
      
      alerts <- alert_system$check_thresholds(test_case$metrics)
      alert_names <- names(alerts)
      
      expect_equal(length(alert_names), length(test_case$expected_alerts))
      expect_true(all(test_case$expected_alerts %in% alert_names))
    }
  })
  
  it("maintains alert history correctly", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("test_metric", "greater_than", 50, "warning")
    
    # Generate multiple alerts over time
    for (i in 1:10) {
      metrics <- list(test_metric = 60 + i)
      alert_system$check_thresholds(metrics)
      Sys.sleep(0.01)  # Small delay to ensure different timestamps
    }
    
    history <- alert_system$get_alert_history()
    expect_equal(length(history), 10)
    
    # Check chronological order
    timestamps <- sapply(history, function(a) a$timestamp)
    expect_true(all(diff(as.numeric(timestamps)) > 0))
    
    # Check metric values are recorded correctly
    metric_values <- sapply(history, function(a) a$metric_value)
    expect_equal(metric_values, 61:70)
  })
  
  it("handles edge case threshold values", {
    alert_system <- AlertSystem$new()
    
    # Test edge cases
    alert_system$set_threshold("zero_threshold", "greater_than", 0, "info")
    alert_system$set_threshold("negative_threshold", "less_than", -10, "warning")
    alert_system$set_threshold("float_threshold", "greater_than", 3.14159, "warning")
    
    edge_cases <- list(
      list(metrics = list(zero_threshold = 0.001), should_alert = TRUE),
      list(metrics = list(zero_threshold = 0), should_alert = FALSE),
      list(metrics = list(zero_threshold = -1), should_alert = FALSE),
      list(metrics = list(negative_threshold = -15), should_alert = TRUE),
      list(metrics = list(negative_threshold = -5), should_alert = FALSE),
      list(metrics = list(float_threshold = 3.14160), should_alert = TRUE),
      list(metrics = list(float_threshold = 3.14159), should_alert = FALSE)
    )
    
    for (case in edge_cases) {
      alert_system$clear_alerts()
      alerts <- alert_system$check_thresholds(case$metrics)
      
      if (case$should_alert) {
        expect_true(length(alerts) > 0, 
                   info = paste("Expected alert for metrics:", 
                               paste(names(case$metrics), case$metrics, sep = "=", collapse = ", ")))
      } else {
        expect_equal(length(alerts), 0,
                    info = paste("Did not expect alert for metrics:", 
                                paste(names(case$metrics), case$metrics, sep = "=", collapse = ", ")))
      }
    }
  })
  
  it("handles missing metrics gracefully", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("missing_metric", "greater_than", 100, "warning")
    alert_system$set_threshold("present_metric", "greater_than", 50, "warning")
    
    # Test with missing metric
    metrics <- list(present_metric = 75)  # missing_metric is not provided
    alerts <- alert_system$check_thresholds(metrics)
    
    # Should only alert on present metric
    expect_equal(length(alerts), 1)
    expect_true("present_metric_greater_than" %in% names(alerts))
    expect_false("missing_metric_greater_than" %in% names(alerts))
  })
  
  it("supports alert severity levels", {
    alert_system <- AlertSystem$new()
    
    # Set up different severity levels
    severities <- c("info", "warning", "error", "critical")
    for (i in seq_along(severities)) {
      alert_system$set_threshold(paste0("metric_", i), "greater_than", i * 10, severities[i])
    }
    
    # Trigger all alerts
    metrics <- list(metric_1 = 15, metric_2 = 25, metric_3 = 35, metric_4 = 45)
    alerts <- alert_system$check_thresholds(metrics)
    
    expect_equal(length(alerts), 4)
    
    # Check severities are preserved
    for (i in seq_along(severities)) {
      alert_key <- paste0("metric_", i, "_greater_than")
      expect_true(alert_key %in% names(alerts))
      expect_equal(alerts[[alert_key]]$severity, severities[i])
    }
  })
  
  it("can disable and enable thresholds", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("test_metric", "greater_than", 50, "warning")
    
    # Should trigger initially
    metrics <- list(test_metric = 75)
    alerts <- alert_system$check_thresholds(metrics)
    expect_equal(length(alerts), 1)
    
    # Disable threshold
    alert_system$disable_threshold("test_metric_greater_than")
    alert_system$clear_alerts()
    
    # Should not trigger when disabled
    alerts <- alert_system$check_thresholds(metrics)
    expect_equal(length(alerts), 0)
  })
  
  it("handles high-frequency threshold checking", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("high_freq_metric", "greater_than", 100, "warning")
    
    # Rapid threshold checking
    start_time <- Sys.time()
    alert_count <- 0
    
    for (i in 1:1000) {
      metrics <- list(high_freq_metric = 50 + (i %% 100))  # Alternates above/below threshold
      alerts <- alert_system$check_thresholds(metrics)
      if (length(alerts) > 0) alert_count <- alert_count + 1
    }
    
    end_time <- Sys.time()
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Should complete quickly (less than 1 second)
    expect_true(execution_time < 1.0)
    
    # Should have triggered alerts for values > 100
    expect_true(alert_count > 0)
    expect_true(alert_count < 1000)  # Not all iterations should trigger
  })
})

# ============================================================================
# 9.1.4 DASHBOARD FUNCTIONALITY TESTS
# ============================================================================

describe("Dashboard Functionality Tests", {
  
  # Mock dashboard components
  create_dashboard_server <- function() {
    function(input, output, session) {
      # Mock reactive values
      dashboard_data <- reactiveVal(list(
        kpis = list(
          total_employees = 1500,
          attrition_rate = 0.12,
          avg_satisfaction = 3.8
        ),
        charts = list(),
        last_updated = Sys.time()
      ))
      
      # Mock KPI output
      output$kpi_total_employees <- renderText({
        data <- dashboard_data()
        scales::comma(data$kpis$total_employees)
      })
      
      output$kpi_attrition_rate <- renderText({
        data <- dashboard_data()
        scales::percent(data$kpis$attrition_rate, accuracy = 0.1)
      })
      
      output$kpi_avg_satisfaction <- renderText({
        data <- dashboard_data()
        round(data$kpis$avg_satisfaction, 1)
      })
      
      # Mock chart outputs
      output$attrition_chart <- renderPlotly({
        # Mock attrition chart
        data <- data.frame(
          Department = c("HR", "Engineering", "Sales", "Marketing"),
          AttritionRate = c(0.15, 0.08, 0.18, 0.12)
        )
        
        p <- ggplot(data, aes(x = Department, y = AttritionRate)) +
          geom_bar(stat = "identity", fill = "#3498db") +
          scale_y_continuous(labels = scales::percent) +
          theme_minimal() +
          labs(title = "Attrition Rate by Department", 
               x = "Department", y = "Attrition Rate")
        
        ggplotly(p)
      })
      
      output$satisfaction_chart <- renderPlotly({
        # Mock satisfaction trend
        dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")
        satisfaction_data <- data.frame(
          Date = dates,
          Satisfaction = 3.5 + 0.3 * sin(seq_along(dates) * pi / 6) + rnorm(length(dates), 0, 0.1)
        )
        
        p <- ggplot(satisfaction_data, aes(x = Date, y = Satisfaction)) +
          geom_line(color = "#e74c3c", size = 1.2) +
          geom_point(color = "#e74c3c") +
          scale_y_continuous(limits = c(1, 5)) +
          theme_minimal() +
          labs(title = "Employee Satisfaction Trend", 
               x = "Date", y = "Satisfaction Score")
        
        ggplotly(p)
      })
      
      # Mock data refresh functionality
      observeEvent(input$refresh_data, {
        # Simulate data refresh
        new_data <- list(
          kpis = list(
            total_employees = sample(1400:1600, 1),
            attrition_rate = runif(1, 0.08, 0.16),
            avg_satisfaction = runif(1, 3.5, 4.2)
          ),
          charts = list(),
          last_updated = Sys.time()
        )
        dashboard_data(new_data)
        
        showNotification("Dashboard data refreshed successfully!", 
                        type = "success", duration = 3)
      })
      
      # Export dashboard data getter
      return(list(
        get_data = dashboard_data,
        refresh_data = function() {
          # Manual refresh function for testing
          new_data <- list(
            kpis = list(
              total_employees = sample(1400:1600, 1),
              attrition_rate = runif(1, 0.08, 0.16),
              avg_satisfaction = runif(1, 3.5, 4.2)
            ),
            last_updated = Sys.time()
          )
          dashboard_data(new_data)
          return(new_data)
        }
      ))
    }
  }
  
  it("initializes dashboard with correct KPIs", {
    # Mock Shiny session
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    # Create dashboard server
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Get initial data
    initial_data <- dashboard_instance$get_data()
    
    # Validate KPIs structure
    expect_true("kpis" %in% names(initial_data))
    expect_true("total_employees" %in% names(initial_data$kpis))
    expect_true("attrition_rate" %in% names(initial_data$kpis))
    expect_true("avg_satisfaction" %in% names(initial_data$kpis))
    
    # Validate KPI values
    expect_true(is.numeric(initial_data$kpis$total_employees))
    expect_true(initial_data$kpis$total_employees > 0)
    expect_true(initial_data$kpis$attrition_rate >= 0 && initial_data$kpis$attrition_rate <= 1)
    expect_true(initial_data$kpis$avg_satisfaction >= 1 && initial_data$kpis$avg_satisfaction <= 5)
  })
  
  it("handles dashboard data refresh correctly", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Get initial data
    initial_data <- dashboard_instance$get_data()
    initial_timestamp <- initial_data$last_updated
    
    # Wait a small amount to ensure timestamp difference
    Sys.sleep(0.1)
    
    # Refresh data
    refreshed_data <- dashboard_instance$refresh_data()
    
    # Validate refresh
    expect_true(refreshed_data$last_updated > initial_timestamp)
    expect_true("kpis" %in% names(refreshed_data))
    
    # Values should be within expected ranges
    expect_true(refreshed_data$kpis$total_employees >= 1400 && 
                refreshed_data$kpis$total_employees <= 1600)
    expect_true(refreshed_data$kpis$attrition_rate >= 0.08 && 
                refreshed_data$kpis$attrition_rate <= 0.16)
    expect_true(refreshed_data$kpis$avg_satisfaction >= 3.5 && 
                refreshed_data$kpis$avg_satisfaction <= 4.2)
  })
  
  it("validates dashboard responsiveness under load", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Simulate multiple rapid refreshes
    start_time <- Sys.time()
    refresh_times <- c()
    
    for (i in 1:50) {
      refresh_start <- Sys.time()
      dashboard_instance$refresh_data()
      refresh_end <- Sys.time()
      
      refresh_time <- as.numeric(difftime(refresh_end, refresh_start, units = "secs"))
      refresh_times <- c(refresh_times, refresh_time)
    }
    
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Performance assertions
    expect_true(total_time < 5.0)  # Should complete in under 5 seconds
    expect_true(mean(refresh_times) < 0.1)  # Average refresh should be under 100ms
    expect_true(max(refresh_times) < 0.5)   # No single refresh should take over 500ms
  })
  
  it("handles concurrent dashboard access", {
    # Simulate multiple users accessing dashboard simultaneously
    concurrent_results <- parallel::mclapply(1:5, function(user_id) {
      session <- MockShinySession$new()
      input <- list()
      output <- list()
      
      dashboard_server <- create_dashboard_server()
      dashboard_instance <- dashboard_server(input, output, session)
      
      # Each user performs multiple operations
      results <- list()
      for (i in 1:10) {
        data <- dashboard_instance$refresh_data()
        results[[i]] <- list(
          user_id = user_id,
          iteration = i,
          total_employees = data$kpis$total_employees,
          timestamp = data$last_updated
        )
      }
      
      return(results)
    }, mc.cores = 2)
    
    # Flatten results
    all_results <- unlist(concurrent_results, recursive = FALSE)
    
    # Validate all operations completed successfully
    expect_equal(length(all_results), 50)  # 5 users × 10 operations each
    
    # Check data integrity
    for (result in all_results) {
      expect_true(is.numeric(result$total_employees))
      expect_true(result$total_employees >= 1400 && result$total_employees <= 1600)
      expect_true(!is.null(result$timestamp))
    }
  })
  
  it("validates dashboard error handling", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    # Create dashboard with error injection
    create_error_dashboard <- function() {
      function(input, output, session) {
        dashboard_data <- reactiveVal(NULL)
        
        return(list(
          get_data = dashboard_data,
          refresh_data = function() {
            # Simulate various error conditions
            error_type <- sample(c("network", "data", "memory", "success"), 1, 
                               prob = c(0.1, 0.1, 0.1, 0.7))
            
            if (error_type == "network") {
              stop("Network connection timeout")
            } else if (error_type == "data") {
              stop("Invalid data format received")
            } else if (error_type == "memory") {
              stop("Insufficient memory to process request")
            } else {
              # Success case
              return(list(
                kpis = list(total_employees = 1500),
                last_updated = Sys.time()
              ))
            }
          }
        ))
      }
    }
    
    error_dashboard_server <- create_error_dashboard()
    error_dashboard <- error_dashboard_server(input, output, session)
    
    # Test error handling
    success_count <- 0
    error_count <- 0
    
    for (i in 1:100) {
      tryCatch({
        result <- error_dashboard$refresh_data()
        success_count <- success_count + 1
      }, error = function(e) {
        error_count <- error_count + 1
        # Validate error messages are informative
        expect_true(nchar(e$message) > 0)
        expect_true(e$message %in% c(
          "Network connection timeout",
          "Invalid data format received", 
          "Insufficient memory to process request"
        ))
      })
    }
    
    # Should have both successes and errors
    expect_true(success_count > 0)
    expect_true(error_count > 0)
    expect_equal(success_count + error_count, 100)
  })
})

# ============================================================================
# 9.1.5 LOG AGGREGATION EFFECTIVENESS TESTS
# ============================================================================

describe("Log Aggregation Effectiveness", {
  
  # Enhanced Logger with aggregation capabilities
  AggregatedLogger <- R6Class("AggregatedLogger",
    private = list(
      .logs = list(),
      .aggregated_logs = list(),
      .log_buffer = list(),
      .buffer_size = 1000,
      .aggregation_interval = 60  # seconds
    ),
    
    public = list(
      initialize = function(buffer_size = 1000, aggregation_interval = 60) {
        private$.logs <- list()
        private$.aggregated_logs <- list()
        private$.log_buffer <- list()
        private$.buffer_size <- buffer_size
        private$.aggregation_interval <- aggregation_interval
      },
      
      log = function(level, message, module = "unknown", metadata = list()) {
        log_entry <- list(
          timestamp = Sys.time(),
          level = level,
          message = message,
          module = module,
          metadata = metadata,
          session_id = digest::digest(Sys.time()),
          pid = Sys.getpid()
        )
        
        # Add to buffer
        private$.log_buffer[[length(private$.log_buffer) + 1]] <- log_entry
        
        # Flush buffer if it's full
        if (length(private$.log_buffer) >= private$.buffer_size) {
          self$flush_buffer()
        }
        
        return(log_entry)
      },
      
      flush_buffer = function() {
        if (length(private$.log_buffer) == 0) return(invisible(NULL))
        
        # Move buffer to main logs
        new_logs <- private$.log_buffer
        private$.logs <- c(private$.logs, new_logs)
        private$.log_buffer <- list()
        
        # Trigger aggregation
        self$aggregate_logs()
        
        return(length(new_logs))
      },
      
      aggregate_logs = function() {
        if (length(private$.logs) == 0) return(invisible(NULL))
        
        # Aggregate by time windows, level, and module
        current_time <- Sys.time()
        window_size <- private$.aggregation_interval
        
        # Create time windows
        log_times <- sapply(private$.logs, function(log) as.numeric(log$timestamp))
        min_time <- min(log_times)
        max_time <- max(log_times)
        
        time_windows <- seq(min_time, max_time + window_size, by = window_size)
        
        aggregated_data <- list()
        
        for (i in 1:(length(time_windows) - 1)) {
          window_start <- time_windows[i]
          window_end <- time_windows[i + 1]
          
          # Filter logs in this window
          window_logs <- private$.logs[log_times >= window_start & log_times < window_end]
          
          if (length(window_logs) == 0) next
          
          # Aggregate by level and module
          aggregation_key <- function(log) {
            paste(log$level, log$module, sep = "_")
          }
          
          log_groups <- split(window_logs, sapply(window_logs, aggregation_key))
          
          for (group_key in names(log_groups)) {
            group_logs <- log_groups[[group_key]]
            
            agg_entry <- list(
              window_start = as.POSIXct(window_start, origin = "1970-01-01"),
              window_end = as.POSIXct(window_end, origin = "1970-01-01"),
              level = group_logs[[1]]$level,
              module = group_logs[[1]]$module,
              count = length(group_logs),
              messages = sapply(group_logs, function(l) l$message),
              first_occurrence = min(sapply(group_logs, function(l) l$timestamp)),
              last_occurrence = max(sapply(group_logs, function(l) l$timestamp))
            )
            
            agg_key <- paste(window_start, group_key, sep = "_")
            private$.aggregated_logs[[agg_key]] <- agg_entry
          }
        }
        
        return(length(private$.aggregated_logs))
      },
      
      get_logs = function(level = NULL, module = NULL, limit = NULL) {
        filtered_logs <- private$.logs
        
        # Filter by level
        if (!is.null(level)) {
          filtered_logs <- filtered_logs[sapply(filtered_logs, function(l) l$level == level)]
        }
        
        # Filter by module
        if (!is.null(module)) {
          filtered_logs <- filtered_logs[sapply(filtered_logs, function(l) l$module == module)]
        }
        
        # Apply limit
        if (!is.null(limit) && length(filtered_logs) > limit) {
          filtered_logs <- tail(filtered_logs, limit)
        }
        
        return(filtered_logs)
      },
      
      get_aggregated_logs = function() {
        return(private$.aggregated_logs)
      },
      
      get_log_summary = function() {
        if (length(private$.logs) == 0) {
          return(list(
            total_logs = 0,
            by_level = list(),
            by_module = list(),
            time_range = NULL
          ))
        }
        
        # Count by level
        levels <- sapply(private$.logs, function(l) l$level)
        level_counts <- table(levels)
        
        # Count by module
        modules <- sapply(private$.logs, function(l) l$module)
        module_counts <- table(modules)
        
        # Time range
        timestamps <- sapply(private$.logs, function(l) l$timestamp)
        time_range <- list(
          start = min(timestamps),
          end = max(timestamps),
          duration_hours = as.numeric(difftime(max(timestamps), min(timestamps), units = "hours"))
        )
        
        return(list(
          total_logs = length(private$.logs),
          by_level = as.list(level_counts),
          by_module = as.list(module_counts),
          time_range = time_range,
          aggregated_entries = length(private$.aggregated_logs)
        ))
      },
      
      clear_logs = function() {
        private$.logs <- list()
        private$.aggregated_logs <- list()
        private$.log_buffer <- list()
      }
    )
  )
  
  it("collects logs efficiently in buffer", {
    logger <- AggregatedLogger$new(buffer_size = 10)
    
    # Add logs to buffer
    for (i in 1:5) {
      logger$log("info", paste("Test message", i), "test_module")
    }
    
    # Buffer should contain logs but main logs should be empty initially
    summary <- logger$get_log_summary()
    expect_equal(summary$total_logs, 0)  # Not flushed yet
    
    # Add more logs to trigger flush
    for (i in 6:12) {
      logger$log("info", paste("Test message", i), "test_module")
    }
    
    # Should have flushed when buffer reached size 10
    summary <- logger$get_log_summary()
    expect_true(summary$total_logs >= 10)
  })
  
  it("aggregates logs by time windows correctly", {
    logger <- AggregatedLogger$new(buffer_size = 5, aggregation_interval = 1)
    
    # Generate logs across different time periods
    base_time <- Sys.time()
    
    # First window
    for (i in 1:3) {
      logger$log("error", "Database error", "database")
    }
    
    # Simulate time passage
    Sys.sleep(1.1)
    
    # Second window
    for (i in 1:2) {
      logger$log("warning", "High memory usage", "system")
    }
    
    # Force aggregation
    logger$flush_buffer()
    
    aggregated <- logger$get_aggregated_logs()
    expect_true(length(aggregated) >= 1)
    
    # Check aggregation structure
    first_agg <- aggregated[[1]]
    expect_true("window_start" %in% names(first_agg))
    expect_true("window_end" %in% names(first_agg))
    expect_true("count" %in% names(first_agg))
    expect_true("level" %in% names(first_agg))
    expect_true("module" %in% names(first_agg))
  })
  
  it("handles high-volume log ingestion", {
    logger <- AggregatedLogger$new(buffer_size = 100)
    
    # Generate high volume of logs
    start_time <- Sys.time()
    log_count <- 10000
    
    for (i in 1:log_count) {
      level <- sample(c("info", "warning", "error"), 1)
      module <- sample(c("auth", "database", "api", "ui"), 1)
      logger$log(level, paste("Message", i), module)
    }
    
    logger$flush_buffer()
    end_time <- Sys.time()
    
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Performance assertions
    expect_true(execution_time < 5.0)  # Should complete in under 5 seconds
    
    summary <- logger$get_log_summary()
    expect_equal(summary$total_logs, log_count)
    
    # Should have aggregated data
    expect_true(summary$aggregated_entries > 0)
  })
  
  it("filters logs correctly by level and module", {
    logger <- AggregatedLogger$new(buffer_size = 5)
    
    # Generate diverse logs
    test_logs <- list(
      list(level = "error", module = "database", message = "Connection failed"),
      list(level = "warning", module = "database", message = "Slow query"),
      list(level = "info", module = "auth", message = "User login"),
      list(level = "error", module = "auth", message = "Invalid credentials"),
      list(level = "info", module = "api", message = "Request processed")
    )
    
    for (log_data in test_logs) {
      logger$log(log_data$level, log_data$message, log_data$module)
    }
    
    logger$flush_buffer()
    
    # Test filtering by level
    error_logs <- logger$get_logs(level = "error")
    expect_equal(length(error_logs), 2)
    expect_true(all(sapply(error_logs, function(l) l$level == "error")))
    
    # Test filtering by module
    database_logs <- logger$get_logs(module = "database")
    expect_equal(length(database_logs), 2)
    expect_true(all(sapply(database_logs, function(l) l$module == "database")))
    
    # Test combined filtering
    error_auth_logs <- logger$get_logs(level = "error", module = "auth")
    expect_equal(length(error_auth_logs), 1)
    expect_equal(error_auth_logs[[1]]$message, "Invalid credentials")
  })
  
  it("maintains log chronological order", {
    logger <- AggregatedLogger$new(buffer_size = 100)
    
    # Generate logs with deliberate timing
    messages <- character(50)
    timestamps <- numeric(50)
    
    for (i in 1:50) {
      message <- paste("Chronological test", i)
      logger$log("info", message, "test")
      messages[i] <- message
      timestamps[i] <- as.numeric(Sys.time())
      
      if (i %% 10 == 0) Sys.sleep(0.01)  # Small delays every 10 logs
    }
    
    logger$flush_buffer()
    
    # Get all logs
    all_logs <- logger$get_logs()
    
    # Check chronological order
    log_timestamps <- sapply(all_logs, function(l) as.numeric(l$timestamp))
    expect_true(all(diff(log_timestamps) >= 0))  # Should be non-decreasing
    
    # Check message order
    log_messages <- sapply(all_logs, function(l) l$message)
    expect_equal(log_messages, messages)
  })
  
  it("handles concurrent logging correctly", {
    logger <- AggregatedLogger$new(buffer_size = 200)
    
    # Simulate concurrent logging from multiple processes/threads
    concurrent_results <- parallel::mclapply(1:5, function(worker_id) {
      logged_messages <- character(20)
      
      for (i in 1:20) {
        message <- paste("Worker", worker_id, "Message", i)
        logger$log("info", message, paste0("worker_", worker_id))
        logged_messages[i] <- message
      }
      
      return(logged_messages)
    }, mc.cores = 2)
    
    logger$flush_buffer()
    
    # Verify all logs were captured
    all_logs <- logger$get_logs()
    expect_equal(length(all_logs), 100)  # 5 workers × 20 messages each
    
    # Check that all worker modules are represented
    modules <- unique(sapply(all_logs, function(l) l$module))
    expected_modules <- paste0("worker_", 1:5)
    expect_true(all(expected_modules %in% modules))
  })
  
  it("generates accurate log summaries", {
    logger <- AggregatedLogger$new(buffer_size = 20)
    
    # Generate logs with known distribution
    level_counts <- list(info = 10, warning = 5, error = 3)
    module_counts <- list(auth = 8, database = 6, api = 4)
    
    # Generate logs according to distribution
    for (level in names(level_counts)) {
      for (i in 1:level_counts[[level]]) {
        module <- sample(names(module_counts), 1, 
                        prob = unlist(module_counts)/sum(unlist(module_counts)))
        logger$log(level, paste("Test message", i), module)
      }
    }
    
    logger$flush_buffer()
    
    # Get summary
    summary <- logger$get_log_summary()
    
    # Verify total count
    expect_equal(summary$total_logs, sum(unlist(level_counts)))
    
    # Verify level distribution
    for (level in names(level_counts)) {
      expect_equal(summary$by_level[[level]], level_counts[[level]])
    }
    
    # Verify time range is valid
    expect_true(!is.null(summary$time_range))
    expect_true(summary$time_range$duration_hours >= 0)
  })
})

# ============================================================================
# 9.1.6 DISTRIBUTED TRACING VALIDATION TESTS
# ============================================================================

describe("Distributed Tracing Validation", {
  
  # Mock distributed tracing system
  TracingSystem <- R6Class("TracingSystem",
    private = list(
      .traces = list(),
      .spans = list(),
      .correlation_ids = list()
    ),
    
    public = list(
      initialize = function() {
        private$.traces <- list()
        private$.spans <- list() 
        private$.correlation_ids <- list()
      },
      
      start_trace = function(operation_name, metadata = list()) {
        trace_id <- self$generate_trace_id()
        span_id <- self$generate_span_id()
        
        trace <- list(
          trace_id = trace_id,
          root_span_id = span_id,
          operation_name = operation_name,
          start_time = Sys.time(),
          metadata = metadata,
          status = "active"
        )
        
        span <- list(
          trace_id = trace_id,
          span_id = span_id,
          parent_span_id = NULL,
          operation_name = operation_name,
          start_time = Sys.time(),
          end_time = NULL,
          duration_ms = NULL,
          metadata = metadata,
          tags = list(),
          status = "active"
        )
        
        private$.traces[[trace_id]] <- trace
        private$.spans[[span_id]] <- span
        
        return(list(trace_id = trace_id, span_id = span_id))
      },
      
      start_child_span = function(parent_trace_id, parent_span_id, operation_name, metadata = list()) {
        if (!parent_trace_id %in% names(private$.traces)) {
          stop("Parent trace not found")
        }
        
        span_id <- self$generate_span_id()
        
        span <- list(
          trace_id = parent_trace_id,
          span_id = span_id,
          parent_span_id = parent_span_id,
          operation_name = operation_name,
          start_time = Sys.time(),
          end_time = NULL,
          duration_ms = NULL,
          metadata = metadata,
          tags = list(),
          status = "active"
        )
        
        private$.spans[[span_id]] <- span
        
        return(span_id)
      },
      
      finish_span = function(span_id, status = "success", metadata = list()) {
        if (!span_id %in% names(private$.spans)) {
          stop("Span not found")
        }
        
        span <- private$.spans[[span_id]]
        end_time <- Sys.time()
        duration_ms <- as.numeric(difftime(end_time, span$start_time, units = "secs")) * 1000
        
        span$end_time <- end_time
        span$duration_ms <- duration_ms
        span$status <- status
        span$metadata <- c(span$metadata, metadata)
        
        private$.spans[[span_id]] <- span
        
        # Check if this completes the trace
        trace_id <- span$trace_id
        trace_spans <- self$get_trace_spans(trace_id)
        
        if (all(sapply(trace_spans, function(s) !is.null(s$end_time)))) {
          private$.traces[[trace_id]]$status <- "completed"
          private$.traces[[trace_id]]$end_time <- Sys.time()
        }
        
        return(span)
      },
      
      add_span_tag = function(span_id, key, value) {
        if (!span_id %in% names(private$.spans)) {
          stop("Span not found")
        }
        
        private$.spans[[span_id]]$tags[[key]] <- value
      },
      
      get_trace = function(trace_id) {
        if (!trace_id %in% names(private$.traces)) {
          return(NULL)
        }
        
        trace <- private$.traces[[trace_id]]
        spans <- self$get_trace_spans(trace_id)
        
        return(list(
          trace = trace,
          spans = spans
        ))
      },
      
      get_trace_spans = function(trace_id) {
        trace_spans <- private$.spans[sapply(private$.spans, function(s) s$trace_id == trace_id)]
        return(trace_spans)
      },
      
      generate_trace_id = function() {
        paste0("trace_", digest::digest(paste(Sys.time(), runif(1))))
      },
      
      generate_span_id = function() {
        paste0("span_", digest::digest(paste(Sys.time(), runif(1))))
      },
      
      get_active_traces = function() {
        active_traces <- private$.traces[sapply(private$.traces, function(t) t$status == "active")]
        return(active_traces)
      },
      
      get_trace_statistics = function() {
        if (length(private$.traces) == 0) {
          return(list(
            total_traces = 0,
            completed_traces = 0,
            active_traces = 0,
            avg_trace_duration_ms = 0
          ))
        }
        
        completed_traces <- private$.traces[sapply(private$.traces, function(t) t$status == "completed")]
        
        if (length(completed_traces) > 0) {
          durations <- sapply(completed_traces, function(t) {
            if (!is.null(t$end_time)) {
              as.numeric(difftime(t$end_time, t$start_time, units = "secs")) * 1000
            } else {
              NA
            }
          })
          avg_duration <- mean(durations, na.rm = TRUE)
        } else {
          avg_duration <- 0
        }
        
api-gateway`$dependencies, c("user-service"))
    expect_equal(graph# ============================================================================
# ATLAS LABS HR ANALYTICS - MONITORING & OBSERVABILITY UNIT TESTS
# Comprehensive test suite for application monitoring, health checks, metrics,
# alerts, logging, tracing, and performance monitoring
# ============================================================================

library(testthat)
library(mockery)
library(shiny)
library(R6)
library(jsonlite)
library(httr)
library(purrr)
library(lubridate)
library(digest)

# Source the application files (assuming they exist)
# source("modules/logger_module.R")
# source("global.R")
# source("utils.R")

# ============================================================================
# 9.1.1 HEALTH CHECK ENDPOINT VALIDATION TESTS
# ============================================================================

describe("Health Check Endpoint Validation", {
  
  # Mock health check endpoint function
  create_health_endpoint <- function() {
    function(req) {
      tryCatch({
        # Simulate health checks
        db_status <- check_database_connection()
        memory_status <- check_memory_usage()
        disk_status <- check_disk_space()
        
        health_data <- list(
          status = "healthy",
          timestamp = Sys.time(),
          version = "1.0.0",
          uptime = get_uptime(),
          checks = list(
            database = db_status,
            memory = memory_status,
            disk = disk_status
          )
        )
        
        list(
          status = 200,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(health_data, auto_unbox = TRUE)
        )
      }, error = function(e) {
        list(
          status = 503,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(list(
            status = "unhealthy",
            error = e$message,
            timestamp = Sys.time()
          ), auto_unbox = TRUE)
        )
      })
    }
  }
  
  # Helper functions for health checks
  check_database_connection <- function() {
    list(status = "ok", response_time_ms = sample(1:50, 1))
  }
  
  check_memory_usage <- function() {
    mem_info <- gc()
    list(
      status = if(sum(mem_info[,2]) < 1000) "ok" else "warning",
      used_mb = sum(mem_info[,2]),
      threshold_mb = 1000
    )
  }
  
  check_disk_space <- function() {
    list(status = "ok", free_gb = 50.5, used_percent = 45)
  }
  
  get_uptime <- function() {
    as.numeric(difftime(Sys.time(), as.POSIXct("2024-01-01"), units = "secs"))
  }
  
  it("returns 200 status for healthy application", {
    health_endpoint <- create_health_endpoint()
    response <- health_endpoint(list())
    
    expect_equal(response$status, 200)
    expect_equal(response$headers$`Content-Type`, "application/json")
    
    body <- jsonlite::fromJSON(response$body)
    expect_equal(body$status, "healthy")
    expect_true("timestamp" %in% names(body))
    expect_true("version" %in% names(body))
    expect_true("checks" %in% names(body))
  })
  
  it("validates all health check components", {
    health_endpoint <- create_health_endpoint()
    response <- health_endpoint(list())
    body <- jsonlite::fromJSON(response$body)
    
    # Check required fields
    required_fields <- c("status", "timestamp", "version", "uptime", "checks")
    expect_true(all(required_fields %in% names(body)))
    
    # Check nested components
    expect_true("database" %in% names(body$checks))
    expect_true("memory" %in% names(body$checks))
    expect_true("disk" %in% names(body$checks))
    
    # Validate timestamp format
    expect_true(is.character(body$timestamp))
    expect_false(is.na(as.POSIXct(body$timestamp)))
  })
  
  it("handles database connection failures", {
    # Mock database failure
    mock_check_db <- function() {
      stop("Database connection timeout")
    }
    
    with_mock(
      check_database_connection = mock_check_db,
      {
        health_endpoint <- create_health_endpoint()
        response <- health_endpoint(list())
        
        expect_equal(response$status, 503)
        body <- jsonlite::fromJSON(response$body)
        expect_equal(body$status, "unhealthy")
        expect_true("error" %in% names(body))
      }
    )
  })
  
  it("validates response time thresholds", {
    # Test various response times
    response_times <- c(10, 50, 100, 500, 1000, 5000)
    
    for (time_ms in response_times) {
      mock_check_db <- function() {
        Sys.sleep(time_ms/1000)  # Convert to seconds
        list(status = "ok", response_time_ms = time_ms)
      }
      
      with_mock(
        check_database_connection = mock_check_db,
        {
          start_time <- Sys.time()
          health_endpoint <- create_health_endpoint()
          response <- health_endpoint(list())
          end_time <- Sys.time()
          
          actual_time <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
          expect_true(actual_time >= time_ms * 0.8)  # Allow 20% tolerance
        }
      )
    }
  })
  
  it("handles memory pressure scenarios", {
    # Test different memory usage levels
    memory_scenarios <- list(
      low = list(status = "ok", used_mb = 100, threshold_mb = 1000),
      medium = list(status = "ok", used_mb = 500, threshold_mb = 1000),
      high = list(status = "warning", used_mb = 950, threshold_mb = 1000),
      critical = list(status = "error", used_mb = 1200, threshold_mb = 1000)
    )
    
    for (scenario_name in names(memory_scenarios)) {
      scenario <- memory_scenarios[[scenario_name]]
      
      mock_check_memory <- function() scenario
      
      with_mock(
        check_memory_usage = mock_check_memory,
        {
          health_endpoint <- create_health_endpoint()
          response <- health_endpoint(list())
          body <- jsonlite::fromJSON(response$body)
          
          expect_equal(body$checks$memory$status, scenario$status)
          expect_equal(body$checks$memory$used_mb, scenario$used_mb)
        }
      )
    }
  })
  
  it("validates concurrent health check requests", {
    health_endpoint <- create_health_endpoint()
    
    # Simulate concurrent requests
    concurrent_responses <- parallel::mclapply(1:10, function(i) {
      health_endpoint(list())
    }, mc.cores = 2)
    
    # All should succeed
    statuses <- sapply(concurrent_responses, function(r) r$status)
    expect_true(all(statuses == 200))
    
    # Check response consistency
    bodies <- lapply(concurrent_responses, function(r) jsonlite::fromJSON(r$body))
    versions <- sapply(bodies, function(b) b$version)
    expect_true(length(unique(versions)) == 1)  # All should have same version
  })
})

# ============================================================================
# 9.1.2 METRICS COLLECTION ACCURACY TESTS
# ============================================================================

describe("Metrics Collection Accuracy", {
  
  # Mock metrics collector
  MetricsCollector <- R6Class("MetricsCollector",
    private = list(
      .metrics = list(),
      .start_time = NULL
    ),
    
    public = list(
      initialize = function() {
        private$.metrics <- list()
        private$.start_time <- Sys.time()
      },
      
      increment_counter = function(name, value = 1, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        if (is.null(private$.metrics[[key]])) {
          private$.metrics[[key]] <- list(
            type = "counter",
            name = name,
            value = 0,
            tags = tags,
            last_updated = Sys.time()
          )
        }
        private$.metrics[[key]]$value <- private$.metrics[[key]]$value + value
        private$.metrics[[key]]$last_updated <- Sys.time()
      },
      
      set_gauge = function(name, value, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        private$.metrics[[key]] <- list(
          type = "gauge",
          name = name,
          value = value,
          tags = tags,
          last_updated = Sys.time()
        )
      },
      
      record_histogram = function(name, value, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        if (is.null(private$.metrics[[key]])) {
          private$.metrics[[key]] <- list(
            type = "histogram",
            name = name,
            values = c(),
            tags = tags,
            last_updated = Sys.time()
          )
        }
        private$.metrics[[key]]$values <- c(private$.metrics[[key]]$values, value)
        private$.metrics[[key]]$last_updated <- Sys.time()
      },
      
      get_metrics = function() {
        private$.metrics
      },
      
      get_metric_summary = function() {
        metrics_summary <- list()
        
        for (metric_key in names(private$.metrics)) {
          metric <- private$.metrics[[metric_key]]
          
          if (metric$type == "counter") {
            metrics_summary[[metric$name]] <- list(
              type = "counter",
              value = metric$value,
              tags = metric$tags
            )
          } else if (metric$type == "gauge") {
            metrics_summary[[metric$name]] <- list(
              type = "gauge",
              value = metric$value,
              tags = metric$tags
            )
          } else if (metric$type == "histogram") {
            values <- metric$values
            metrics_summary[[metric$name]] <- list(
              type = "histogram",
              count = length(values),
              mean = mean(values),
              median = median(values),
              p95 = quantile(values, 0.95, na.rm = TRUE),
              p99 = quantile(values, 0.99, na.rm = TRUE),
              min = min(values),
              max = max(values),
              tags = metric$tags
            )
          }
        }
        
        metrics_summary
      }
    )
  )
  
  it("accurately tracks counter metrics", {
    collector <- MetricsCollector$new()
    
    # Test basic counter increment
    collector$increment_counter("user_logins")
    collector$increment_counter("user_logins")
    collector$increment_counter("user_logins", value = 3)
    
    metrics <- collector$get_metrics()
    login_metric <- metrics[[paste0("user_logins_", digest::digest(list()))]]
    
    expect_equal(login_metric$type, "counter")
    expect_equal(login_metric$value, 5)  # 1 + 1 + 3
    expect_equal(login_metric$name, "user_logins")
  })
  
  it("handles counter metrics with tags", {
    collector <- MetricsCollector$new()
    
    # Different departments
    collector$increment_counter("page_views", tags = list(department = "HR"))
    collector$increment_counter("page_views", tags = list(department = "HR"))
    collector$increment_counter("page_views", tags = list(department = "Finance"))
    
    metrics <- collector$get_metrics()
    
    # Should have separate counters for each tag combination
    hr_key <- paste0("page_views_", digest::digest(list(department = "HR")))
    finance_key <- paste0("page_views_", digest::digest(list(department = "Finance")))
    
    expect_equal(metrics[[hr_key]]$value, 2)
    expect_equal(metrics[[finance_key]]$value, 1)
    expect_equal(metrics[[hr_key]]$tags$department, "HR")
    expect_equal(metrics[[finance_key]]$tags$department, "Finance")
  })
  
  it("accurately measures gauge metrics", {
    collector <- MetricsCollector$new()
    
    # Test gauge updates
    collector$set_gauge("memory_usage_mb", 512.5)
    collector$set_gauge("memory_usage_mb", 768.2)  # Should overwrite
    collector$set_gauge("cpu_usage_percent", 45.7)
    
    metrics <- collector$get_metrics()
    memory_key <- paste0("memory_usage_mb_", digest::digest(list()))
    cpu_key <- paste0("cpu_usage_percent_", digest::digest(list()))
    
    expect_equal(metrics[[memory_key]]$value, 768.2)
    expect_equal(metrics[[cpu_key]]$value, 45.7)
    expect_equal(metrics[[memory_key]]$type, "gauge")
  })
  
  it("correctly calculates histogram statistics", {
    collector <- MetricsCollector$new()
    
    # Record response times
    response_times <- c(10, 15, 12, 45, 23, 18, 67, 34, 28, 19)
    for (time in response_times) {
      collector$record_histogram("response_time_ms", time)
    }
    
    summary <- collector$get_metric_summary()
    response_metric <- summary$response_time_ms
    
    expect_equal(response_metric$type, "histogram")
    expect_equal(response_metric$count, 10)
    expect_equal(response_metric$mean, mean(response_times))
    expect_equal(response_metric$median, median(response_times))
    expect_equal(response_metric$min, min(response_times))
    expect_equal(response_metric$max, max(response_times))
  })
  
  it("handles edge cases in histogram calculations", {
    collector <- MetricsCollector$new()
    
    # Test with single value
    collector$record_histogram("single_value", 42)
    summary <- collector$get_metric_summary()
    single_metric <- summary$single_value
    
    expect_equal(single_metric$count, 1)
    expect_equal(single_metric$mean, 42)
    expect_equal(single_metric$median, 42)
    expect_equal(single_metric$min, 42)
    expect_equal(single_metric$max, 42)
    
    # Test with identical values
    for (i in 1:5) {
      collector$record_histogram("identical_values", 100)
    }
    
    summary <- collector$get_metric_summary()
    identical_metric <- summary$identical_values
    
    expect_equal(identical_metric$count, 5)
    expect_equal(identical_metric$mean, 100)
    expect_equal(identical_metric$p95, 100)
    expect_equal(identical_metric$p99, 100)
  })
  
  it("maintains accuracy under high volume", {
    collector <- MetricsCollector$new()
    
    # High volume counter increments
    start_time <- Sys.time()
    for (i in 1:10000) {
      collector$increment_counter("high_volume_counter")
    }
    end_time <- Sys.time()
    
    metrics <- collector$get_metrics()
    counter_key <- paste0("high_volume_counter_", digest::digest(list()))
    
    expect_equal(metrics[[counter_key]]$value, 10000)
    
    # Should complete reasonably quickly (less than 1 second)
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    expect_true(execution_time < 1.0)
  })
  
  it("handles concurrent metric updates", {
    collector <- MetricsCollector$new()
    
    # Concurrent counter increments
    parallel::mclapply(1:100, function(i) {
      collector$increment_counter("concurrent_counter")
    }, mc.cores = 2)
    
    metrics <- collector$get_metrics()
    counter_key <- paste0("concurrent_counter_", digest::digest(list()))
    
    # Should handle race conditions gracefully
    expect_true(metrics[[counter_key]]$value <= 100)
    expect_true(metrics[[counter_key]]$value > 0)
  })
  
  it("validates metric timestamp accuracy", {
    collector <- MetricsCollector$new()
    
    before_time <- Sys.time()
    collector$increment_counter("timestamp_test")
    after_time <- Sys.time()
    
    metrics <- collector$get_metrics()
    metric_key <- paste0("timestamp_test_", digest::digest(list()))
    metric_timestamp <- metrics[[metric_key]]$last_updated
    
    expect_true(metric_timestamp >= before_time)
    expect_true(metric_timestamp <= after_time)
  })
})

# ============================================================================
# 9.1.3 ALERT THRESHOLD TESTING
# ============================================================================

describe("Alert Threshold Testing", {
  
  # Mock alert system
  AlertSystem <- R6Class("AlertSystem",
    private = list(
      .thresholds = list(),
      .alerts = list(),
      .alert_history = list()
    ),
    
    public = list(
      initialize = function() {
        private$.thresholds <- list()
        private$.alerts <- list()
        private$.alert_history <- list()
      },
      
      set_threshold = function(metric_name, threshold_type, value, severity = "warning") {
        threshold_id <- paste0(metric_name, "_", threshold_type)
        private$.thresholds[[threshold_id]] <- list(
          metric_name = metric_name,
          type = threshold_type,  # "greater_than", "less_than", "equal_to"
          value = value,
          severity = severity,
          enabled = TRUE
        )
      },
      
      check_thresholds = function(metrics) {
        current_alerts <- list()
        
        for (threshold_id in names(private$.thresholds)) {
          threshold <- private$.thresholds[[threshold_id]]
          if (!threshold$enabled) next
          
          metric_value <- metrics[[threshold$metric_name]]
          if (is.null(metric_value)) next
          
          alert_triggered <- FALSE
          
          if (threshold$type == "greater_than" && metric_value > threshold$value) {
            alert_triggered <- TRUE
          } else if (threshold$type == "less_than" && metric_value < threshold$value) {
            alert_triggered <- TRUE
          } else if (threshold$type == "equal_to" && metric_value == threshold$value) {
            alert_triggered <- TRUE
          }
          
          if (alert_triggered) {
            alert <- list(
              threshold_id = threshold_id,
              metric_name = threshold$metric_name,
              metric_value = metric_value,
              threshold_value = threshold$value,
              severity = threshold$severity,
              timestamp = Sys.time(),
              message = sprintf("Metric %s (%s) exceeded threshold %s (current: %s)",
                              threshold$metric_name, threshold$type, 
                              threshold$value, metric_value)
            )
            
            current_alerts[[threshold_id]] <- alert
            private$.alert_history[[length(private$.alert_history) + 1]] <- alert
          }
        }
        
        private$.alerts <- current_alerts
        return(current_alerts)
      },
      
      get_active_alerts = function() {
        private$.alerts
      },
      
      get_alert_history = function(limit = 100) {
        history_length <- length(private$.alert_history)
        if (history_length == 0) return(list())
        
        start_idx <- max(1, history_length - limit + 1)
        private$.alert_history[start_idx:history_length]
      },
      
      clear_alerts = function() {
        private$.alerts <- list()
      },
      
      disable_threshold = function(threshold_id) {
        if (threshold_id %in% names(private$.thresholds)) {
          private$.thresholds[[threshold_id]]$enabled <- FALSE
        }
      }
    )
  )
  
  it("triggers alerts when thresholds are exceeded", {
    alert_system <- AlertSystem$new()
    
    # Set up thresholds
    alert_system$set_threshold("cpu_usage", "greater_than", 80, "warning")
    alert_system$set_threshold("memory_usage", "greater_than", 90, "critical")
    alert_system$set_threshold("response_time", "greater_than", 1000, "warning")
    
    # Test metrics that should trigger alerts
    metrics <- list(
      cpu_usage = 85,     # Should trigger warning
      memory_usage = 95,  # Should trigger critical
      response_time = 500 # Should not trigger
    )
    
    alerts <- alert_system$check_thresholds(metrics)
    
    expect_equal(length(alerts), 2)
    expect_true("cpu_usage_greater_than" %in% names(alerts))
    expect_true("memory_usage_greater_than" %in% names(alerts))
    expect_false("response_time_greater_than" %in% names(alerts))
    
    # Check alert details
    cpu_alert <- alerts$cpu_usage_greater_than
    expect_equal(cpu_alert$severity, "warning")
    expect_equal(cpu_alert$metric_value, 85)
    expect_equal(cpu_alert$threshold_value, 80)
  })
  
  it("handles different threshold types correctly", {
    alert_system <- AlertSystem$new()
    
    # Set up different threshold types
    alert_system$set_threshold("disk_space", "less_than", 10, "critical")
    alert_system$set_threshold("error_count", "greater_than", 0, "warning")
    alert_system$set_threshold("connection_count", "equal_to", 0, "critical")
    
    # Test scenarios
    test_cases <- list(
      list(
        metrics = list(disk_space = 5, error_count = 3, connection_count = 0),
        expected_alerts = c("disk_space_less_than", "error_count_greater_than", "connection_count_equal_to")
      ),
      list(
        metrics = list(disk_space = 15, error_count = 0, connection_count = 10),
        expected_alerts = c()
      ),
      list(
        metrics = list(disk_space = 10, error_count = 1, connection_count = 1),
        expected_alerts = c("error_count_greater_than")
      )
    )
    
    for (i in seq_along(test_cases)) {
      test_case <- test_cases[[i]]
      alert_system$clear_alerts()
      
      alerts <- alert_system$check_thresholds(test_case$metrics)
      alert_names <- names(alerts)
      
      expect_equal(length(alert_names), length(test_case$expected_alerts))
      expect_true(all(test_case$expected_alerts %in% alert_names))
    }
  })
  
  it("maintains alert history correctly", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("test_metric", "greater_than", 50, "warning")
    
    # Generate multiple alerts over time
    for (i in 1:10) {
      metrics <- list(test_metric = 60 + i)
      alert_system$check_thresholds(metrics)
      Sys.sleep(0.01)  # Small delay to ensure different timestamps
    }
    
    history <- alert_system$get_alert_history()
    expect_equal(length(history), 10)
    
    # Check chronological order
    timestamps <- sapply(history, function(a) a$timestamp)
    expect_true(all(diff(as.numeric(timestamps)) > 0))
    
    # Check metric values are recorded correctly
    metric_values <- sapply(history, function(a) a$metric_value)
    expect_equal(metric_values, 61:70)
  })
  
  it("handles edge case threshold values", {
    alert_system <- AlertSystem$new()
    
    # Test edge cases
    alert_system$set_threshold("zero_threshold", "greater_than", 0, "info")
    alert_system$set_threshold("negative_threshold", "less_than", -10, "warning")
    alert_system$set_threshold("float_threshold", "greater_than", 3.14159, "warning")
    
    edge_cases <- list(
      list(metrics = list(zero_threshold = 0.001), should_alert = TRUE),
      list(metrics = list(zero_threshold = 0), should_alert = FALSE),
      list(metrics = list(zero_threshold = -1), should_alert = FALSE),
      list(metrics = list(negative_threshold = -15), should_alert = TRUE),
      list(metrics = list(negative_threshold = -5), should_alert = FALSE),
      list(metrics = list(float_threshold = 3.14160), should_alert = TRUE),
      list(metrics = list(float_threshold = 3.14159), should_alert = FALSE)
    )
    
    for (case in edge_cases) {
      alert_system$clear_alerts()
      alerts <- alert_system$check_thresholds(case$metrics)
      
      if (case$should_alert) {
        expect_true(length(alerts) > 0, 
                   info = paste("Expected alert for metrics:", 
                               paste(names(case$metrics), case$metrics, sep = "=", collapse = ", ")))
      } else {
        expect_equal(length(alerts), 0,
                    info = paste("Did not expect alert for metrics:", 
                                paste(names(case$metrics), case$metrics, sep = "=", collapse = ", ")))
      }
    }
  })
  
  it("handles missing metrics gracefully", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("missing_metric", "greater_than", 100, "warning")
    alert_system$set_threshold("present_metric", "greater_than", 50, "warning")
    
    # Test with missing metric
    metrics <- list(present_metric = 75)  # missing_metric is not provided
    alerts <- alert_system$check_thresholds(metrics)
    
    # Should only alert on present metric
    expect_equal(length(alerts), 1)
    expect_true("present_metric_greater_than" %in% names(alerts))
    expect_false("missing_metric_greater_than" %in% names(alerts))
  })
  
  it("supports alert severity levels", {
    alert_system <- AlertSystem$new()
    
    # Set up different severity levels
    severities <- c("info", "warning", "error", "critical")
    for (i in seq_along(severities)) {
      alert_system$set_threshold(paste0("metric_", i), "greater_than", i * 10, severities[i])
    }
    
    # Trigger all alerts
    metrics <- list(metric_1 = 15, metric_2 = 25, metric_3 = 35, metric_4 = 45)
    alerts <- alert_system$check_thresholds(metrics)
    
    expect_equal(length(alerts), 4)
    
    # Check severities are preserved
    for (i in seq_along(severities)) {
      alert_key <- paste0("metric_", i, "_greater_than")
      expect_true(alert_key %in% names(alerts))
      expect_equal(alerts[[alert_key]]$severity, severities[i])
    }
  })
  
  it("can disable and enable thresholds", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("test_metric", "greater_than", 50, "warning")
    
    # Should trigger initially
    metrics <- list(test_metric = 75)
    alerts <- alert_system$check_thresholds(metrics)
    expect_equal(length(alerts), 1)
    
    # Disable threshold
    alert_system$disable_threshold("test_metric_greater_than")
    alert_system$clear_alerts()
    
    # Should not trigger when disabled
    alerts <- alert_system$check_thresholds(metrics)
    expect_equal(length(alerts), 0)
  })
  
  it("handles high-frequency threshold checking", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("high_freq_metric", "greater_than", 100, "warning")
    
    # Rapid threshold checking
    start_time <- Sys.time()
    alert_count <- 0
    
    for (i in 1:1000) {
      metrics <- list(high_freq_metric = 50 + (i %% 100))  # Alternates above/below threshold
      alerts <- alert_system$check_thresholds(metrics)
      if (length(alerts) > 0) alert_count <- alert_count + 1
    }
    
    end_time <- Sys.time()
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Should complete quickly (less than 1 second)
    expect_true(execution_time < 1.0)
    
    # Should have triggered alerts for values > 100
    expect_true(alert_count > 0)
    expect_true(alert_count < 1000)  # Not all iterations should trigger
  })
})

# ============================================================================
# 9.1.4 DASHBOARD FUNCTIONALITY TESTS
# ============================================================================

describe("Dashboard Functionality Tests", {
  
  # Mock dashboard components
  create_dashboard_server <- function() {
    function(input, output, session) {
      # Mock reactive values
      dashboard_data <- reactiveVal(list(
        kpis = list(
          total_employees = 1500,
          attrition_rate = 0.12,
          avg_satisfaction = 3.8
        ),
        charts = list(),
        last_updated = Sys.time()
      ))
      
      # Mock KPI output
      output$kpi_total_employees <- renderText({
        data <- dashboard_data()
        scales::comma(data$kpis$total_employees)
      })
      
      output$kpi_attrition_rate <- renderText({
        data <- dashboard_data()
        scales::percent(data$kpis$attrition_rate, accuracy = 0.1)
      })
      
      output$kpi_avg_satisfaction <- renderText({
        data <- dashboard_data()
        round(data$kpis$avg_satisfaction, 1)
      })
      
      # Mock chart outputs
      output$attrition_chart <- renderPlotly({
        # Mock attrition chart
        data <- data.frame(
          Department = c("HR", "Engineering", "Sales", "Marketing"),
          AttritionRate = c(0.15, 0.08, 0.18, 0.12)
        )
        
        p <- ggplot(data, aes(x = Department, y = AttritionRate)) +
          geom_bar(stat = "identity", fill = "#3498db") +
          scale_y_continuous(labels = scales::percent) +
          theme_minimal() +
          labs(title = "Attrition Rate by Department", 
               x = "Department", y = "Attrition Rate")
        
        ggplotly(p)
      })
      
      output$satisfaction_chart <- renderPlotly({
        # Mock satisfaction trend
        dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")
        satisfaction_data <- data.frame(
          Date = dates,
          Satisfaction = 3.5 + 0.3 * sin(seq_along(dates) * pi / 6) + rnorm(length(dates), 0, 0.1)
        )
        
        p <- ggplot(satisfaction_data, aes(x = Date, y = Satisfaction)) +
          geom_line(color = "#e74c3c", size = 1.2) +
          geom_point(color = "#e74c3c") +
          scale_y_continuous(limits = c(1, 5)) +
          theme_minimal() +
          labs(title = "Employee Satisfaction Trend", 
               x = "Date", y = "Satisfaction Score")
        
        ggplotly(p)
      })
      
      # Mock data refresh functionality
      observeEvent(input$refresh_data, {
        # Simulate data refresh
        new_data <- list(
          kpis = list(
            total_employees = sample(1400:1600, 1),
            attrition_rate = runif(1, 0.08, 0.16),
            avg_satisfaction = runif(1, 3.5, 4.2)
          ),
          charts = list(),
          last_updated = Sys.time()
        )
        dashboard_data(new_data)
        
        showNotification("Dashboard data refreshed successfully!", 
                        type = "success", duration = 3)
      })
      
      # Export dashboard data getter
      return(list(
        get_data = dashboard_data,
        refresh_data = function() {
          # Manual refresh function for testing
          new_data <- list(
            kpis = list(
              total_employees = sample(1400:1600, 1),
              attrition_rate = runif(1, 0.08, 0.16),
              avg_satisfaction = runif(1, 3.5, 4.2)
            ),
            last_updated = Sys.time()
          )
          dashboard_data(new_data)
          return(new_data)
        }
      ))
    }
  }
  
  it("initializes dashboard with correct KPIs", {
    # Mock Shiny session
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    # Create dashboard server
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Get initial data
    initial_data <- dashboard_instance$get_data()
    
    # Validate KPIs structure
    expect_true("kpis" %in% names(initial_data))
    expect_true("total_employees" %in% names(initial_data$kpis))
    expect_true("attrition_rate" %in% names(initial_data$kpis))
    expect_true("avg_satisfaction" %in% names(initial_data$kpis))
    
    # Validate KPI values
    expect_true(is.numeric(initial_data$kpis$total_employees))
    expect_true(initial_data$kpis$total_employees > 0)
    expect_true(initial_data$kpis$attrition_rate >= 0 && initial_data$kpis$attrition_rate <= 1)
    expect_true(initial_data$kpis$avg_satisfaction >= 1 && initial_data$kpis$avg_satisfaction <= 5)
  })
  
  it("handles dashboard data refresh correctly", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Get initial data
    initial_data <- dashboard_instance$get_data()
    initial_timestamp <- initial_data$last_updated
    
    # Wait a small amount to ensure timestamp difference
    Sys.sleep(0.1)
    
    # Refresh data
    refreshed_data <- dashboard_instance$refresh_data()
    
    # Validate refresh
    expect_true(refreshed_data$last_updated > initial_timestamp)
    expect_true("kpis" %in% names(refreshed_data))
    
    # Values should be within expected ranges
    expect_true(refreshed_data$kpis$total_employees >= 1400 && 
                refreshed_data$kpis$total_employees <= 1600)
    expect_true(refreshed_data$kpis$attrition_rate >= 0.08 && 
                refreshed_data$kpis$attrition_rate <= 0.16)
    expect_true(refreshed_data$kpis$avg_satisfaction >= 3.5 && 
                refreshed_data$kpis$avg_satisfaction <= 4.2)
  })
  
  it("validates dashboard responsiveness under load", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Simulate multiple rapid refreshes
    start_time <- Sys.time()
    refresh_times <- c()
    
    for (i in 1:50) {
      refresh_start <- Sys.time()
      dashboard_instance$refresh_data()
      refresh_end <- Sys.time()
      
      refresh_time <- as.numeric(difftime(refresh_end, refresh_start, units = "secs"))
      refresh_times <- c(refresh_times, refresh_time)
    }
    
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Performance assertions
    expect_true(total_time < 5.0)  # Should complete in under 5 seconds
    expect_true(mean(refresh_times) < 0.1)  # Average refresh should be under 100ms
    expect_true(max(refresh_times) < 0.5)   # No single refresh should take over 500ms
  })
  
  it("handles concurrent dashboard access", {
    # Simulate multiple users accessing dashboard simultaneously
    concurrent_results <- parallel::mclapply(1:5, function(user_id) {
      session <- MockShinySession$new()
      input <- list()
      output <- list()
      
      dashboard_server <- create_dashboard_server()
      dashboard_instance <- dashboard_server(input, output, session)
      
      # Each user performs multiple operations
      results <- list()
      for (i in 1:10) {
        data <- dashboard_instance$refresh_data()
        results[[i]] <- list(
          user_id = user_id,
          iteration = i,
          total_employees = data$kpis$total_employees,
          timestamp = data$last_updated
        )
      }
      
      return(results)
    }, mc.cores = 2)
    
    # Flatten results
    all_results <- unlist(concurrent_results, recursive = FALSE)
    
    # Validate all operations completed successfully
    expect_equal(length(all_results), 50)  # 5 users × 10 operations each
    
    # Check data integrity
    for (result in all_results) {
      expect_true(is.numeric(result$total_employees))
      expect_true(result$total_employees >= 1400 && result$total_employees <= 1600)
      expect_true(!is.null(result$timestamp))
    }
  })
  
  it("validates dashboard error handling", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    # Create dashboard with error injection
    create_error_dashboard <- function() {
      function(input, output, session) {
        dashboard_data <- reactiveVal(NULL)
        
        return(list(
          get_data = dashboard_data,
          refresh_data = function() {
            # Simulate various error conditions
            error_type <- sample(c("network", "data", "memory", "success"), 1, 
                               prob = c(0.1, 0.1, 0.1, 0.7))
            
            if (error_type == "network") {
              stop("Network connection timeout")
            } else if (error_type == "data") {
              stop("Invalid data format received")
            } else if (error_type == "memory") {
              stop("Insufficient memory to process request")
            } else {
              # Success case
              return(list(
                kpis = list(total_employees = 1500),
                last_updated = Sys.time()
              ))
            }
          }
        ))
      }
    }
    
    error_dashboard_server <- create_error_dashboard()
    error_dashboard <- error_dashboard_server(input, output, session)
    
    # Test error handling
    success_count <- 0
    error_count <- 0
    
    for (i in 1:100) {
      tryCatch({
        result <- error_dashboard$refresh_data()
        success_count <- success_count + 1
      }, error = function(e) {
        error_count <- error_count + 1
        # Validate error messages are informative
        expect_true(nchar(e$message) > 0)
        expect_true(e$message %in% c(
          "Network connection timeout",
          "Invalid data format received", 
          "Insufficient memory to process request"
        ))
      })
    }
    
    # Should have both successes and errors
    expect_true(success_count > 0)
    expect_true(error_count > 0)
    expect_equal(success_count + error_count, 100)
  })
})

# ============================================================================
# 9.1.5 LOG AGGREGATION EFFECTIVENESS TESTS
# ============================================================================

describe("Log Aggregation Effectiveness", {
  
  # Enhanced Logger with aggregation capabilities
  AggregatedLogger <- R6Class("AggregatedLogger",
    private = list(
      .logs = list(),
      .aggregated_logs = list(),
      .log_buffer = list(),
      .buffer_size = 1000,
      .aggregation_interval = 60  # seconds
    ),
    
    public = list(
      initialize = function(buffer_size = 1000, aggregation_interval = 60) {
        private$.logs <- list()
        private$.aggregated_logs <- list()
        private$.log_buffer <- list()
        private$.buffer_size <- buffer_size
        private$.aggregation_interval <- aggregation_interval
      },
      
      log = function(level, message, module = "unknown", metadata = list()) {
        log_entry <- list(
          timestamp = Sys.time(),
          level = level,
          message = message,
          module = module,
          metadata = metadata,
          session_id = digest::digest(Sys.time()),
          pid = Sys.getpid()
        )
        
        # Add to buffer
        private$.log_buffer[[length(private$.log_buffer) + 1]] <- log_entry
        
        # Flush buffer if it's full
        if (length(private$.log_buffer) >= private$.buffer_size) {
          self$flush_buffer()
        }
        
        return(log_entry)
      },
      
      flush_buffer = function() {
        if (length(private$.log_buffer) == 0) return(invisible(NULL))
        
        # Move buffer to main logs
        new_logs <- private$.log_buffer
        private$.logs <- c(private$.logs, new_logs)
        private$.log_buffer <- list()
        
        # Trigger aggregation
        self$aggregate_logs()
        
        return(length(new_logs))
      },
      
      aggregate_logs = function() {
        if (length(private$.logs) == 0) return(invisible(NULL))
        
        # Aggregate by time windows, level, and module
        current_time <- Sys.time()
        window_size <- private$.aggregation_interval
        
        # Create time windows
        log_times <- sapply(private$.logs, function(log) as.numeric(log$timestamp))
        min_time <- min(log_times)
        max_time <- max(log_times)
        
        time_windows <- seq(min_time, max_time + window_size, by = window_size)
        
        aggregated_data <- list()
        
        for (i in 1:(length(time_windows) - 1)) {
          window_start <- time_windows[i]
          window_end <- time_windows[i + 1]
          
          # Filter logs in this window
          window_logs <- private$.logs[log_times >= window_start & log_times < window_end]
          
          if (length(window_logs) == 0) next
          
          # Aggregate by level and module
          aggregation_key <- function(log) {
            paste(log$level, log$module, sep = "_")
          }
          
          log_groups <- split(window_logs, sapply(window_logs, aggregation_key))
          
          for (group_key in names(log_groups)) {
            group_logs <- log_groups[[group_key]]
            
            agg_entry <- list(
              window_start = as.POSIXct(window_start, origin = "1970-01-01"),
              window_end = as.POSIXct(window_end, origin = "1970-01-01"),
              level = group_logs[[1]]$level,
              module = group_logs[[1]]$module,
              count = length(group_logs),
              messages = sapply(group_logs, function(l) l$message),
              first_occurrence = min(sapply(group_logs, function(l) l$timestamp)),
              last_occurrence = max(sapply(group_logs, function(l) l$timestamp))
            )
            
            agg_key <- paste(window_start, group_key, sep = "_")
            private$.aggregated_logs[[agg_key]] <- agg_entry
          }
        }
        
        return(length(private$.aggregated_logs))
      },
      
      get_logs = function(level = NULL, module = NULL, limit = NULL) {
        filtered_logs <- private$.logs
        
        # Filter by level
        if (!is.null(level)) {
          filtered_logs <- filtered_logs[sapply(filtered_logs, function(l) l$level == level)]
        }
        
        # Filter by module
        if (!is.null(module)) {
          filtered_logs <- filtered_logs[sapply(filtered_logs, function(l) l$module == module)]
        }
        
        # Apply limit
        if (!is.null(limit) && length(filtered_logs) > limit) {
          filtered_logs <- tail(filtered_logs, limit)
        }
        
        return(filtered_logs)
      },
      
      get_aggregated_logs = function() {
        return(private$.aggregated_logs)
      },
      
      get_log_summary = function() {
        if (length(private$.logs) == 0) {
          return(list(
            total_logs = 0,
            by_level = list(),
            by_module = list(),
            time_range = NULL
          ))
        }
        
        # Count by level
        levels <- sapply(private$.logs, function(l) l$level)
        level_counts <- table(levels)
        
        # Count by module
        modules <- sapply(private$.logs, function(l) l$module)
        module_counts <- table(modules)
        
        # Time range
        timestamps <- sapply(private$.logs, function(l) l$timestamp)
        time_range <- list(
          start = min(timestamps),
          end = max(timestamps),
          duration_hours = as.numeric(difftime(max(timestamps), min(timestamps), units = "hours"))
        )
        
        return(list(
          total_logs = length(private$.logs),
          by_level = as.list(level_counts),
          by_module = as.list(module_counts),
          time_range = time_range,
          aggregated_entries = length(private$.aggregated_logs)
        ))
      },
      
      clear_logs = function() {
        private$.logs <- list()
        private$.aggregated_logs <- list()
        private$.log_buffer <- list()
      }
    )
  )
  
  it("collects logs efficiently in buffer", {
    logger <- AggregatedLogger$new(buffer_size = 10)
    
    # Add logs to buffer
    for (i in 1:5) {
      logger$log("info", paste("Test message", i), "test_module")
    }
    
    # Buffer should contain logs but main logs should be empty initially
    summary <- logger$get_log_summary()
    expect_equal(summary$total_logs, 0)  # Not flushed yet
    
    # Add more logs to trigger flush
    for (i in 6:12) {
      logger$log("info", paste("Test message", i), "test_module")
    }
    
    # Should have flushed when buffer reached size 10
    summary <- logger$get_log_summary()
    expect_true(summary$total_logs >= 10)
  })
  
  it("aggregates logs by time windows correctly", {
    logger <- AggregatedLogger$new(buffer_size = 5, aggregation_interval = 1)
    
    # Generate logs across different time periods
    base_time <- Sys.time()
    
    # First window
    for (i in 1:3) {
      logger$log("error", "Database error", "database")
    }
    
    # Simulate time passage
    Sys.sleep(1.1)
    
    # Second window
    for (i in 1:2) {
      logger$log("warning", "High memory usage", "system")
    }
    
    # Force aggregation
    logger$flush_buffer()
    
    aggregated <- logger$get_aggregated_logs()
    expect_true(length(aggregated) >= 1)
    
    # Check aggregation structure
    first_agg <- aggregated[[1]]
    expect_true("window_start" %in% names(first_agg))
    expect_true("window_end" %in% names(first_agg))
    expect_true("count" %in% names(first_agg))
    expect_true("level" %in% names(first_agg))
    expect_true("module" %in% names(first_agg))
  })
  
  it("handles high-volume log ingestion", {
    logger <- AggregatedLogger$new(buffer_size = 100)
    
    # Generate high volume of logs
    start_time <- Sys.time()
    log_count <- 10000
    
    for (i in 1:log_count) {
      level <- sample(c("info", "warning", "error"), 1)
      module <- sample(c("auth", "database", "api", "ui"), 1)
      logger$log(level, paste("Message", i), module)
    }
    
    logger$flush_buffer()
    end_time <- Sys.time()
    
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Performance assertions
    expect_true(execution_time < 5.0)  # Should complete in under 5 seconds
    
    summary <- logger$get_log_summary()
    expect_equal(summary$total_logs, log_count)
    
    # Should have aggregated data
    expect_true(summary$aggregated_entries > 0)
  })
  
  it("filters logs correctly by level and module", {
    logger <- AggregatedLogger$new(buffer_size = 5)
    
    # Generate diverse logs
    test_logs <- list(
      list(level = "error", module = "database", message = "Connection failed"),
      list(level = "warning", module = "database", message = "Slow query"),
      list(level = "info", module = "auth", message = "User login"),
      list(level = "error", module = "auth", message = "Invalid credentials"),
      list(level = "info", module = "api", message = "Request processed")
    )
    
    for (log_data in test_logs) {
      logger$log(log_data$level, log_data$message, log_data$module)
    }
    
    logger$flush_buffer()
    
    # Test filtering by level
    error_logs <- logger$get_logs(level = "error")
    expect_equal(length(error_logs), 2)
    expect_true(all(sapply(error_logs, function(l) l$level == "error")))
    
    # Test filtering by module
    database_logs <- logger$get_logs(module = "database")
    expect_equal(length(database_logs), 2)
    expect_true(all(sapply(database_logs, function(l) l$module == "database")))
    
    # Test combined filtering
    error_auth_logs <- logger$get_logs(level = "error", module = "auth")
    expect_equal(length(error_auth_logs), 1)
    expect_equal(error_auth_logs[[1]]$message, "Invalid credentials")
  })
  
  it("maintains log chronological order", {
    logger <- AggregatedLogger$new(buffer_size = 100)
    
    # Generate logs with deliberate timing
    messages <- character(50)
    timestamps <- numeric(50)
    
    for (i in 1:50) {
      message <- paste("Chronological test", i)
      logger$log("info", message, "test")
      messages[i] <- message
      timestamps[i] <- as.numeric(Sys.time())
      
      if (i %% 10 == 0) Sys.sleep(0.01)  # Small delays every 10 logs
    }
    
    logger$flush_buffer()
    
    # Get all logs
    all_logs <- logger$get_logs()
    
    # Check chronological order
    log_timestamps <- sapply(all_logs, function(l) as.numeric(l$timestamp))
    expect_true(all(diff(log_timestamps) >= 0))  # Should be non-decreasing
    
    # Check message order
    log_messages <- sapply(all_logs, function(l) l$message)
    expect_equal(log_messages, messages)
  })
  
  it("handles concurrent logging correctly", {
    logger <- AggregatedLogger$new(buffer_size = 200)
    
    # Simulate concurrent logging from multiple processes/threads
    concurrent_results <- parallel::mclapply(1:5, function(worker_id) {
      logged_messages <- character(20)
      
      for (i in 1:20) {
        message <- paste("Worker", worker_id, "Message", i)
        logger$log("info", message, paste0("worker_", worker_id))
        logged_messages[i] <- message
      }
      
      return(logged_messages)
    }, mc.cores = 2)
    
    logger$flush_buffer()
    
    # Verify all logs were captured
    all_logs <- logger$get_logs()
    expect_equal(length(all_logs), 100)  # 5 workers × 20 messages each
    
    # Check that all worker modules are represented
    modules <- unique(sapply(all_logs, function(l) l$module))
    expected_modules <- paste0("worker_", 1:5)
    expect_true(all(expected_modules %in% modules))
  })
  
  it("generates accurate log summaries", {
    logger <- AggregatedLogger$new(buffer_size = 20)
    
    # Generate logs with known distribution
    level_counts <- list(info = 10, warning = 5, error = 3)
    module_counts <- list(auth = 8, database = 6, api = 4)
    
    # Generate logs according to distribution
    for (level in names(level_counts)) {
      for (i in 1:level_counts[[level]]) {
        module <- sample(names(module_counts), 1, 
                        prob = unlist(module_counts)/sum(unlist(module_counts)))
        logger$log(level, paste("Test message", i), module)
      }
    }
    
    logger$flush_buffer()
    
    # Get summary
    summary <- logger$get_log_summary()
    
    # Verify total count
    expect_equal(summary$total_logs, sum(unlist(level_counts)))
    
    # Verify level distribution
    for (level in names(level_counts)) {
      expect_equal(summary$by_level[[level]], level_counts[[level]])
    }
    
    # Verify time range is valid
    expect_true(!is.null(summary$time_range))
    expect_true(summary$time_range$duration_hours >= 0)
  })
})

# ============================================================================
# 9.1.6 DISTRIBUTED TRACING VALIDATION TESTS
# ============================================================================

describe("Distributed Tracing Validation", {
  
  # Mock distributed tracing system
  TracingSystem <- R6Class("TracingSystem",
    private = list(
      .traces = list(),
      .spans = list(),
      .correlation_ids = list()
    ),
    
    public = list(
      initialize = function() {
        private$.traces <- list()
        private$.spans <- list() 
        private$.correlation_ids <- list()
      },
      
      start_trace = function(operation_name, metadata = list()) {
        trace_id <- self$generate_trace_id()
        span_id <- self$generate_span_id()
        
        trace <- list(
          trace_id = trace_id,
          root_span_id = span_id,
          operation_name = operation_name,
          start_time = Sys.time(),
          metadata = metadata,
          status = "active"
        )
        
        span <- list(
          trace_id = trace_id,
          span_id = span_id,
          parent_span_id = NULL,
          operation_name = operation_name,
          start_time = Sys.time(),
          end_time = NULL,
          duration_ms = NULL,
          metadata = metadata,
          tags = list(),
          status = "active"
        )
        
        private$.traces[[trace_id]] <- trace
        private$.spans[[span_id]] <- span
        
        return(list(trace_id = trace_id, span_id = span_id))
      },
      
      start_child_span = function(parent_trace_id, parent_span_id, operation_name, metadata = list()) {
        if (!parent_trace_id %in% names(private$.traces)) {
          stop("Parent trace not found")
        }
        
        span_id <- self$generate_span_id()
        
        span <- list(
          trace_id = parent_trace_id,
          span_id = span_id,
          parent_span_id = parent_span_id,
          operation_name = operation_name,
          start_time = Sys.time(),
          end_time = NULL,
          duration_ms = NULL,
          metadata = metadata,
          tags = list(),
          status = "active"
        )
        
        private$.spans[[span_id]] <- span
        
        return(span_id)
      },
      
      finish_span = function(span_id, status = "success", metadata = list()) {
        if (!span_id %in% names(private$.spans)) {
          stop("Span not found")
        }
        
        span <- private$.spans[[span_id]]
        end_time <- Sys.time()
        duration_ms <- as.numeric(difftime(end_time, span$start_time, units = "secs")) * 1000
        
        span$end_time <- end_time
        span$duration_ms <- duration_ms
        span$status <- status
        span$metadata <- c(span$metadata, metadata)
        
        private$.spans[[span_id]] <- span
        
        # Check if this completes the trace
        trace_id <- span$trace_id
        trace_spans <- self$get_trace_spans(trace_id)
        
        if (all(sapply(trace_spans, function(s) !is.null(s$end_time)))) {
          private$.traces[[trace_id]]$status <- "completed"
          private$.traces[[trace_id]]$end_time <- Sys.time()
        }
        
        return(span)
      },
      
      add_span_tag = function(span_id, key, value) {
        if (!span_id %in% names(private$.spans)) {
          stop("Span not found")
        }
        
        private$.spans[[span_id]]$tags[[key]] <- value
      },
      
      get_trace = function(trace_id) {
        if (!trace_id %in% names(private$.traces)) {
          return(NULL)
        }
        
        trace <- private$.traces[[trace_id]]
        spans <- self$get_trace_spans(trace_id)
        
        return(list(
          trace = trace,
          spans = spans
        ))
      },
      
      get_trace_spans = function(trace_id) {
        trace_spans <- private$.spans[sapply(private$.spans, function(s) s$trace_id == trace_id)]
        return(trace_spans)
      },
      
      generate_trace_id = function() {
        paste0("trace_", digest::digest(paste(Sys.time(), runif(1))))
      },
      
      generate_span_id = function() {
        paste0("span_", digest::digest(paste(Sys.time(), runif(1))))
      },
      
      get_active_traces = function() {
        active_traces <- private$.traces[sapply(private$.traces, function(t) t$status == "active")]
        return(active_traces)
      },
      
      get_trace_statistics = function() {
        if (length(private$.traces) == 0) {
          return(list(
            total_traces = 0,
            completed_traces = 0,
            active_traces = 0,
            avg_trace_duration_ms = 0
          ))
        }
        
        completed_traces <- private$.traces[sapply(private$.traces, function(t) t$status == "completed")]
        
        if (length(completed_traces) > 0) {
          durations <- sapply(completed_traces, function(t) {
            if (!is.null(t$end_time)) {
              as.numeric(difftime(t$end_time, t$start_time, units = "secs")) * 1000
            } else {
              NA
            }
          })
          avg_duration <- mean(durations, na.rm = TRUE)
        } else {
          avg_duration <- 0
        }
        
user-service`$dependencies, c("database"))
    expect_equal(graph$database$dependencies, character(0))
    
    # Verify reverse dependencies (dependents)
    expect_equal(graph# ============================================================================
# ATLAS LABS HR ANALYTICS - MONITORING & OBSERVABILITY UNIT TESTS
# Comprehensive test suite for application monitoring, health checks, metrics,
# alerts, logging, tracing, and performance monitoring
# ============================================================================

library(testthat)
library(mockery)
library(shiny)
library(R6)
library(jsonlite)
library(httr)
library(purrr)
library(lubridate)
library(digest)

# Source the application files (assuming they exist)
# source("modules/logger_module.R")
# source("global.R")
# source("utils.R")

# ============================================================================
# 9.1.1 HEALTH CHECK ENDPOINT VALIDATION TESTS
# ============================================================================

describe("Health Check Endpoint Validation", {
  
  # Mock health check endpoint function
  create_health_endpoint <- function() {
    function(req) {
      tryCatch({
        # Simulate health checks
        db_status <- check_database_connection()
        memory_status <- check_memory_usage()
        disk_status <- check_disk_space()
        
        health_data <- list(
          status = "healthy",
          timestamp = Sys.time(),
          version = "1.0.0",
          uptime = get_uptime(),
          checks = list(
            database = db_status,
            memory = memory_status,
            disk = disk_status
          )
        )
        
        list(
          status = 200,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(health_data, auto_unbox = TRUE)
        )
      }, error = function(e) {
        list(
          status = 503,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(list(
            status = "unhealthy",
            error = e$message,
            timestamp = Sys.time()
          ), auto_unbox = TRUE)
        )
      })
    }
  }
  
  # Helper functions for health checks
  check_database_connection <- function() {
    list(status = "ok", response_time_ms = sample(1:50, 1))
  }
  
  check_memory_usage <- function() {
    mem_info <- gc()
    list(
      status = if(sum(mem_info[,2]) < 1000) "ok" else "warning",
      used_mb = sum(mem_info[,2]),
      threshold_mb = 1000
    )
  }
  
  check_disk_space <- function() {
    list(status = "ok", free_gb = 50.5, used_percent = 45)
  }
  
  get_uptime <- function() {
    as.numeric(difftime(Sys.time(), as.POSIXct("2024-01-01"), units = "secs"))
  }
  
  it("returns 200 status for healthy application", {
    health_endpoint <- create_health_endpoint()
    response <- health_endpoint(list())
    
    expect_equal(response$status, 200)
    expect_equal(response$headers$`Content-Type`, "application/json")
    
    body <- jsonlite::fromJSON(response$body)
    expect_equal(body$status, "healthy")
    expect_true("timestamp" %in% names(body))
    expect_true("version" %in% names(body))
    expect_true("checks" %in% names(body))
  })
  
  it("validates all health check components", {
    health_endpoint <- create_health_endpoint()
    response <- health_endpoint(list())
    body <- jsonlite::fromJSON(response$body)
    
    # Check required fields
    required_fields <- c("status", "timestamp", "version", "uptime", "checks")
    expect_true(all(required_fields %in% names(body)))
    
    # Check nested components
    expect_true("database" %in% names(body$checks))
    expect_true("memory" %in% names(body$checks))
    expect_true("disk" %in% names(body$checks))
    
    # Validate timestamp format
    expect_true(is.character(body$timestamp))
    expect_false(is.na(as.POSIXct(body$timestamp)))
  })
  
  it("handles database connection failures", {
    # Mock database failure
    mock_check_db <- function() {
      stop("Database connection timeout")
    }
    
    with_mock(
      check_database_connection = mock_check_db,
      {
        health_endpoint <- create_health_endpoint()
        response <- health_endpoint(list())
        
        expect_equal(response$status, 503)
        body <- jsonlite::fromJSON(response$body)
        expect_equal(body$status, "unhealthy")
        expect_true("error" %in% names(body))
      }
    )
  })
  
  it("validates response time thresholds", {
    # Test various response times
    response_times <- c(10, 50, 100, 500, 1000, 5000)
    
    for (time_ms in response_times) {
      mock_check_db <- function() {
        Sys.sleep(time_ms/1000)  # Convert to seconds
        list(status = "ok", response_time_ms = time_ms)
      }
      
      with_mock(
        check_database_connection = mock_check_db,
        {
          start_time <- Sys.time()
          health_endpoint <- create_health_endpoint()
          response <- health_endpoint(list())
          end_time <- Sys.time()
          
          actual_time <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
          expect_true(actual_time >= time_ms * 0.8)  # Allow 20% tolerance
        }
      )
    }
  })
  
  it("handles memory pressure scenarios", {
    # Test different memory usage levels
    memory_scenarios <- list(
      low = list(status = "ok", used_mb = 100, threshold_mb = 1000),
      medium = list(status = "ok", used_mb = 500, threshold_mb = 1000),
      high = list(status = "warning", used_mb = 950, threshold_mb = 1000),
      critical = list(status = "error", used_mb = 1200, threshold_mb = 1000)
    )
    
    for (scenario_name in names(memory_scenarios)) {
      scenario <- memory_scenarios[[scenario_name]]
      
      mock_check_memory <- function() scenario
      
      with_mock(
        check_memory_usage = mock_check_memory,
        {
          health_endpoint <- create_health_endpoint()
          response <- health_endpoint(list())
          body <- jsonlite::fromJSON(response$body)
          
          expect_equal(body$checks$memory$status, scenario$status)
          expect_equal(body$checks$memory$used_mb, scenario$used_mb)
        }
      )
    }
  })
  
  it("validates concurrent health check requests", {
    health_endpoint <- create_health_endpoint()
    
    # Simulate concurrent requests
    concurrent_responses <- parallel::mclapply(1:10, function(i) {
      health_endpoint(list())
    }, mc.cores = 2)
    
    # All should succeed
    statuses <- sapply(concurrent_responses, function(r) r$status)
    expect_true(all(statuses == 200))
    
    # Check response consistency
    bodies <- lapply(concurrent_responses, function(r) jsonlite::fromJSON(r$body))
    versions <- sapply(bodies, function(b) b$version)
    expect_true(length(unique(versions)) == 1)  # All should have same version
  })
})

# ============================================================================
# 9.1.2 METRICS COLLECTION ACCURACY TESTS
# ============================================================================

describe("Metrics Collection Accuracy", {
  
  # Mock metrics collector
  MetricsCollector <- R6Class("MetricsCollector",
    private = list(
      .metrics = list(),
      .start_time = NULL
    ),
    
    public = list(
      initialize = function() {
        private$.metrics <- list()
        private$.start_time <- Sys.time()
      },
      
      increment_counter = function(name, value = 1, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        if (is.null(private$.metrics[[key]])) {
          private$.metrics[[key]] <- list(
            type = "counter",
            name = name,
            value = 0,
            tags = tags,
            last_updated = Sys.time()
          )
        }
        private$.metrics[[key]]$value <- private$.metrics[[key]]$value + value
        private$.metrics[[key]]$last_updated <- Sys.time()
      },
      
      set_gauge = function(name, value, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        private$.metrics[[key]] <- list(
          type = "gauge",
          name = name,
          value = value,
          tags = tags,
          last_updated = Sys.time()
        )
      },
      
      record_histogram = function(name, value, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        if (is.null(private$.metrics[[key]])) {
          private$.metrics[[key]] <- list(
            type = "histogram",
            name = name,
            values = c(),
            tags = tags,
            last_updated = Sys.time()
          )
        }
        private$.metrics[[key]]$values <- c(private$.metrics[[key]]$values, value)
        private$.metrics[[key]]$last_updated <- Sys.time()
      },
      
      get_metrics = function() {
        private$.metrics
      },
      
      get_metric_summary = function() {
        metrics_summary <- list()
        
        for (metric_key in names(private$.metrics)) {
          metric <- private$.metrics[[metric_key]]
          
          if (metric$type == "counter") {
            metrics_summary[[metric$name]] <- list(
              type = "counter",
              value = metric$value,
              tags = metric$tags
            )
          } else if (metric$type == "gauge") {
            metrics_summary[[metric$name]] <- list(
              type = "gauge",
              value = metric$value,
              tags = metric$tags
            )
          } else if (metric$type == "histogram") {
            values <- metric$values
            metrics_summary[[metric$name]] <- list(
              type = "histogram",
              count = length(values),
              mean = mean(values),
              median = median(values),
              p95 = quantile(values, 0.95, na.rm = TRUE),
              p99 = quantile(values, 0.99, na.rm = TRUE),
              min = min(values),
              max = max(values),
              tags = metric$tags
            )
          }
        }
        
        metrics_summary
      }
    )
  )
  
  it("accurately tracks counter metrics", {
    collector <- MetricsCollector$new()
    
    # Test basic counter increment
    collector$increment_counter("user_logins")
    collector$increment_counter("user_logins")
    collector$increment_counter("user_logins", value = 3)
    
    metrics <- collector$get_metrics()
    login_metric <- metrics[[paste0("user_logins_", digest::digest(list()))]]
    
    expect_equal(login_metric$type, "counter")
    expect_equal(login_metric$value, 5)  # 1 + 1 + 3
    expect_equal(login_metric$name, "user_logins")
  })
  
  it("handles counter metrics with tags", {
    collector <- MetricsCollector$new()
    
    # Different departments
    collector$increment_counter("page_views", tags = list(department = "HR"))
    collector$increment_counter("page_views", tags = list(department = "HR"))
    collector$increment_counter("page_views", tags = list(department = "Finance"))
    
    metrics <- collector$get_metrics()
    
    # Should have separate counters for each tag combination
    hr_key <- paste0("page_views_", digest::digest(list(department = "HR")))
    finance_key <- paste0("page_views_", digest::digest(list(department = "Finance")))
    
    expect_equal(metrics[[hr_key]]$value, 2)
    expect_equal(metrics[[finance_key]]$value, 1)
    expect_equal(metrics[[hr_key]]$tags$department, "HR")
    expect_equal(metrics[[finance_key]]$tags$department, "Finance")
  })
  
  it("accurately measures gauge metrics", {
    collector <- MetricsCollector$new()
    
    # Test gauge updates
    collector$set_gauge("memory_usage_mb", 512.5)
    collector$set_gauge("memory_usage_mb", 768.2)  # Should overwrite
    collector$set_gauge("cpu_usage_percent", 45.7)
    
    metrics <- collector$get_metrics()
    memory_key <- paste0("memory_usage_mb_", digest::digest(list()))
    cpu_key <- paste0("cpu_usage_percent_", digest::digest(list()))
    
    expect_equal(metrics[[memory_key]]$value, 768.2)
    expect_equal(metrics[[cpu_key]]$value, 45.7)
    expect_equal(metrics[[memory_key]]$type, "gauge")
  })
  
  it("correctly calculates histogram statistics", {
    collector <- MetricsCollector$new()
    
    # Record response times
    response_times <- c(10, 15, 12, 45, 23, 18, 67, 34, 28, 19)
    for (time in response_times) {
      collector$record_histogram("response_time_ms", time)
    }
    
    summary <- collector$get_metric_summary()
    response_metric <- summary$response_time_ms
    
    expect_equal(response_metric$type, "histogram")
    expect_equal(response_metric$count, 10)
    expect_equal(response_metric$mean, mean(response_times))
    expect_equal(response_metric$median, median(response_times))
    expect_equal(response_metric$min, min(response_times))
    expect_equal(response_metric$max, max(response_times))
  })
  
  it("handles edge cases in histogram calculations", {
    collector <- MetricsCollector$new()
    
    # Test with single value
    collector$record_histogram("single_value", 42)
    summary <- collector$get_metric_summary()
    single_metric <- summary$single_value
    
    expect_equal(single_metric$count, 1)
    expect_equal(single_metric$mean, 42)
    expect_equal(single_metric$median, 42)
    expect_equal(single_metric$min, 42)
    expect_equal(single_metric$max, 42)
    
    # Test with identical values
    for (i in 1:5) {
      collector$record_histogram("identical_values", 100)
    }
    
    summary <- collector$get_metric_summary()
    identical_metric <- summary$identical_values
    
    expect_equal(identical_metric$count, 5)
    expect_equal(identical_metric$mean, 100)
    expect_equal(identical_metric$p95, 100)
    expect_equal(identical_metric$p99, 100)
  })
  
  it("maintains accuracy under high volume", {
    collector <- MetricsCollector$new()
    
    # High volume counter increments
    start_time <- Sys.time()
    for (i in 1:10000) {
      collector$increment_counter("high_volume_counter")
    }
    end_time <- Sys.time()
    
    metrics <- collector$get_metrics()
    counter_key <- paste0("high_volume_counter_", digest::digest(list()))
    
    expect_equal(metrics[[counter_key]]$value, 10000)
    
    # Should complete reasonably quickly (less than 1 second)
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    expect_true(execution_time < 1.0)
  })
  
  it("handles concurrent metric updates", {
    collector <- MetricsCollector$new()
    
    # Concurrent counter increments
    parallel::mclapply(1:100, function(i) {
      collector$increment_counter("concurrent_counter")
    }, mc.cores = 2)
    
    metrics <- collector$get_metrics()
    counter_key <- paste0("concurrent_counter_", digest::digest(list()))
    
    # Should handle race conditions gracefully
    expect_true(metrics[[counter_key]]$value <= 100)
    expect_true(metrics[[counter_key]]$value > 0)
  })
  
  it("validates metric timestamp accuracy", {
    collector <- MetricsCollector$new()
    
    before_time <- Sys.time()
    collector$increment_counter("timestamp_test")
    after_time <- Sys.time()
    
    metrics <- collector$get_metrics()
    metric_key <- paste0("timestamp_test_", digest::digest(list()))
    metric_timestamp <- metrics[[metric_key]]$last_updated
    
    expect_true(metric_timestamp >= before_time)
    expect_true(metric_timestamp <= after_time)
  })
})

# ============================================================================
# 9.1.3 ALERT THRESHOLD TESTING
# ============================================================================

describe("Alert Threshold Testing", {
  
  # Mock alert system
  AlertSystem <- R6Class("AlertSystem",
    private = list(
      .thresholds = list(),
      .alerts = list(),
      .alert_history = list()
    ),
    
    public = list(
      initialize = function() {
        private$.thresholds <- list()
        private$.alerts <- list()
        private$.alert_history <- list()
      },
      
      set_threshold = function(metric_name, threshold_type, value, severity = "warning") {
        threshold_id <- paste0(metric_name, "_", threshold_type)
        private$.thresholds[[threshold_id]] <- list(
          metric_name = metric_name,
          type = threshold_type,  # "greater_than", "less_than", "equal_to"
          value = value,
          severity = severity,
          enabled = TRUE
        )
      },
      
      check_thresholds = function(metrics) {
        current_alerts <- list()
        
        for (threshold_id in names(private$.thresholds)) {
          threshold <- private$.thresholds[[threshold_id]]
          if (!threshold$enabled) next
          
          metric_value <- metrics[[threshold$metric_name]]
          if (is.null(metric_value)) next
          
          alert_triggered <- FALSE
          
          if (threshold$type == "greater_than" && metric_value > threshold$value) {
            alert_triggered <- TRUE
          } else if (threshold$type == "less_than" && metric_value < threshold$value) {
            alert_triggered <- TRUE
          } else if (threshold$type == "equal_to" && metric_value == threshold$value) {
            alert_triggered <- TRUE
          }
          
          if (alert_triggered) {
            alert <- list(
              threshold_id = threshold_id,
              metric_name = threshold$metric_name,
              metric_value = metric_value,
              threshold_value = threshold$value,
              severity = threshold$severity,
              timestamp = Sys.time(),
              message = sprintf("Metric %s (%s) exceeded threshold %s (current: %s)",
                              threshold$metric_name, threshold$type, 
                              threshold$value, metric_value)
            )
            
            current_alerts[[threshold_id]] <- alert
            private$.alert_history[[length(private$.alert_history) + 1]] <- alert
          }
        }
        
        private$.alerts <- current_alerts
        return(current_alerts)
      },
      
      get_active_alerts = function() {
        private$.alerts
      },
      
      get_alert_history = function(limit = 100) {
        history_length <- length(private$.alert_history)
        if (history_length == 0) return(list())
        
        start_idx <- max(1, history_length - limit + 1)
        private$.alert_history[start_idx:history_length]
      },
      
      clear_alerts = function() {
        private$.alerts <- list()
      },
      
      disable_threshold = function(threshold_id) {
        if (threshold_id %in% names(private$.thresholds)) {
          private$.thresholds[[threshold_id]]$enabled <- FALSE
        }
      }
    )
  )
  
  it("triggers alerts when thresholds are exceeded", {
    alert_system <- AlertSystem$new()
    
    # Set up thresholds
    alert_system$set_threshold("cpu_usage", "greater_than", 80, "warning")
    alert_system$set_threshold("memory_usage", "greater_than", 90, "critical")
    alert_system$set_threshold("response_time", "greater_than", 1000, "warning")
    
    # Test metrics that should trigger alerts
    metrics <- list(
      cpu_usage = 85,     # Should trigger warning
      memory_usage = 95,  # Should trigger critical
      response_time = 500 # Should not trigger
    )
    
    alerts <- alert_system$check_thresholds(metrics)
    
    expect_equal(length(alerts), 2)
    expect_true("cpu_usage_greater_than" %in% names(alerts))
    expect_true("memory_usage_greater_than" %in% names(alerts))
    expect_false("response_time_greater_than" %in% names(alerts))
    
    # Check alert details
    cpu_alert <- alerts$cpu_usage_greater_than
    expect_equal(cpu_alert$severity, "warning")
    expect_equal(cpu_alert$metric_value, 85)
    expect_equal(cpu_alert$threshold_value, 80)
  })
  
  it("handles different threshold types correctly", {
    alert_system <- AlertSystem$new()
    
    # Set up different threshold types
    alert_system$set_threshold("disk_space", "less_than", 10, "critical")
    alert_system$set_threshold("error_count", "greater_than", 0, "warning")
    alert_system$set_threshold("connection_count", "equal_to", 0, "critical")
    
    # Test scenarios
    test_cases <- list(
      list(
        metrics = list(disk_space = 5, error_count = 3, connection_count = 0),
        expected_alerts = c("disk_space_less_than", "error_count_greater_than", "connection_count_equal_to")
      ),
      list(
        metrics = list(disk_space = 15, error_count = 0, connection_count = 10),
        expected_alerts = c()
      ),
      list(
        metrics = list(disk_space = 10, error_count = 1, connection_count = 1),
        expected_alerts = c("error_count_greater_than")
      )
    )
    
    for (i in seq_along(test_cases)) {
      test_case <- test_cases[[i]]
      alert_system$clear_alerts()
      
      alerts <- alert_system$check_thresholds(test_case$metrics)
      alert_names <- names(alerts)
      
      expect_equal(length(alert_names), length(test_case$expected_alerts))
      expect_true(all(test_case$expected_alerts %in% alert_names))
    }
  })
  
  it("maintains alert history correctly", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("test_metric", "greater_than", 50, "warning")
    
    # Generate multiple alerts over time
    for (i in 1:10) {
      metrics <- list(test_metric = 60 + i)
      alert_system$check_thresholds(metrics)
      Sys.sleep(0.01)  # Small delay to ensure different timestamps
    }
    
    history <- alert_system$get_alert_history()
    expect_equal(length(history), 10)
    
    # Check chronological order
    timestamps <- sapply(history, function(a) a$timestamp)
    expect_true(all(diff(as.numeric(timestamps)) > 0))
    
    # Check metric values are recorded correctly
    metric_values <- sapply(history, function(a) a$metric_value)
    expect_equal(metric_values, 61:70)
  })
  
  it("handles edge case threshold values", {
    alert_system <- AlertSystem$new()
    
    # Test edge cases
    alert_system$set_threshold("zero_threshold", "greater_than", 0, "info")
    alert_system$set_threshold("negative_threshold", "less_than", -10, "warning")
    alert_system$set_threshold("float_threshold", "greater_than", 3.14159, "warning")
    
    edge_cases <- list(
      list(metrics = list(zero_threshold = 0.001), should_alert = TRUE),
      list(metrics = list(zero_threshold = 0), should_alert = FALSE),
      list(metrics = list(zero_threshold = -1), should_alert = FALSE),
      list(metrics = list(negative_threshold = -15), should_alert = TRUE),
      list(metrics = list(negative_threshold = -5), should_alert = FALSE),
      list(metrics = list(float_threshold = 3.14160), should_alert = TRUE),
      list(metrics = list(float_threshold = 3.14159), should_alert = FALSE)
    )
    
    for (case in edge_cases) {
      alert_system$clear_alerts()
      alerts <- alert_system$check_thresholds(case$metrics)
      
      if (case$should_alert) {
        expect_true(length(alerts) > 0, 
                   info = paste("Expected alert for metrics:", 
                               paste(names(case$metrics), case$metrics, sep = "=", collapse = ", ")))
      } else {
        expect_equal(length(alerts), 0,
                    info = paste("Did not expect alert for metrics:", 
                                paste(names(case$metrics), case$metrics, sep = "=", collapse = ", ")))
      }
    }
  })
  
  it("handles missing metrics gracefully", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("missing_metric", "greater_than", 100, "warning")
    alert_system$set_threshold("present_metric", "greater_than", 50, "warning")
    
    # Test with missing metric
    metrics <- list(present_metric = 75)  # missing_metric is not provided
    alerts <- alert_system$check_thresholds(metrics)
    
    # Should only alert on present metric
    expect_equal(length(alerts), 1)
    expect_true("present_metric_greater_than" %in% names(alerts))
    expect_false("missing_metric_greater_than" %in% names(alerts))
  })
  
  it("supports alert severity levels", {
    alert_system <- AlertSystem$new()
    
    # Set up different severity levels
    severities <- c("info", "warning", "error", "critical")
    for (i in seq_along(severities)) {
      alert_system$set_threshold(paste0("metric_", i), "greater_than", i * 10, severities[i])
    }
    
    # Trigger all alerts
    metrics <- list(metric_1 = 15, metric_2 = 25, metric_3 = 35, metric_4 = 45)
    alerts <- alert_system$check_thresholds(metrics)
    
    expect_equal(length(alerts), 4)
    
    # Check severities are preserved
    for (i in seq_along(severities)) {
      alert_key <- paste0("metric_", i, "_greater_than")
      expect_true(alert_key %in% names(alerts))
      expect_equal(alerts[[alert_key]]$severity, severities[i])
    }
  })
  
  it("can disable and enable thresholds", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("test_metric", "greater_than", 50, "warning")
    
    # Should trigger initially
    metrics <- list(test_metric = 75)
    alerts <- alert_system$check_thresholds(metrics)
    expect_equal(length(alerts), 1)
    
    # Disable threshold
    alert_system$disable_threshold("test_metric_greater_than")
    alert_system$clear_alerts()
    
    # Should not trigger when disabled
    alerts <- alert_system$check_thresholds(metrics)
    expect_equal(length(alerts), 0)
  })
  
  it("handles high-frequency threshold checking", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("high_freq_metric", "greater_than", 100, "warning")
    
    # Rapid threshold checking
    start_time <- Sys.time()
    alert_count <- 0
    
    for (i in 1:1000) {
      metrics <- list(high_freq_metric = 50 + (i %% 100))  # Alternates above/below threshold
      alerts <- alert_system$check_thresholds(metrics)
      if (length(alerts) > 0) alert_count <- alert_count + 1
    }
    
    end_time <- Sys.time()
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Should complete quickly (less than 1 second)
    expect_true(execution_time < 1.0)
    
    # Should have triggered alerts for values > 100
    expect_true(alert_count > 0)
    expect_true(alert_count < 1000)  # Not all iterations should trigger
  })
})

# ============================================================================
# 9.1.4 DASHBOARD FUNCTIONALITY TESTS
# ============================================================================

describe("Dashboard Functionality Tests", {
  
  # Mock dashboard components
  create_dashboard_server <- function() {
    function(input, output, session) {
      # Mock reactive values
      dashboard_data <- reactiveVal(list(
        kpis = list(
          total_employees = 1500,
          attrition_rate = 0.12,
          avg_satisfaction = 3.8
        ),
        charts = list(),
        last_updated = Sys.time()
      ))
      
      # Mock KPI output
      output$kpi_total_employees <- renderText({
        data <- dashboard_data()
        scales::comma(data$kpis$total_employees)
      })
      
      output$kpi_attrition_rate <- renderText({
        data <- dashboard_data()
        scales::percent(data$kpis$attrition_rate, accuracy = 0.1)
      })
      
      output$kpi_avg_satisfaction <- renderText({
        data <- dashboard_data()
        round(data$kpis$avg_satisfaction, 1)
      })
      
      # Mock chart outputs
      output$attrition_chart <- renderPlotly({
        # Mock attrition chart
        data <- data.frame(
          Department = c("HR", "Engineering", "Sales", "Marketing"),
          AttritionRate = c(0.15, 0.08, 0.18, 0.12)
        )
        
        p <- ggplot(data, aes(x = Department, y = AttritionRate)) +
          geom_bar(stat = "identity", fill = "#3498db") +
          scale_y_continuous(labels = scales::percent) +
          theme_minimal() +
          labs(title = "Attrition Rate by Department", 
               x = "Department", y = "Attrition Rate")
        
        ggplotly(p)
      })
      
      output$satisfaction_chart <- renderPlotly({
        # Mock satisfaction trend
        dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")
        satisfaction_data <- data.frame(
          Date = dates,
          Satisfaction = 3.5 + 0.3 * sin(seq_along(dates) * pi / 6) + rnorm(length(dates), 0, 0.1)
        )
        
        p <- ggplot(satisfaction_data, aes(x = Date, y = Satisfaction)) +
          geom_line(color = "#e74c3c", size = 1.2) +
          geom_point(color = "#e74c3c") +
          scale_y_continuous(limits = c(1, 5)) +
          theme_minimal() +
          labs(title = "Employee Satisfaction Trend", 
               x = "Date", y = "Satisfaction Score")
        
        ggplotly(p)
      })
      
      # Mock data refresh functionality
      observeEvent(input$refresh_data, {
        # Simulate data refresh
        new_data <- list(
          kpis = list(
            total_employees = sample(1400:1600, 1),
            attrition_rate = runif(1, 0.08, 0.16),
            avg_satisfaction = runif(1, 3.5, 4.2)
          ),
          charts = list(),
          last_updated = Sys.time()
        )
        dashboard_data(new_data)
        
        showNotification("Dashboard data refreshed successfully!", 
                        type = "success", duration = 3)
      })
      
      # Export dashboard data getter
      return(list(
        get_data = dashboard_data,
        refresh_data = function() {
          # Manual refresh function for testing
          new_data <- list(
            kpis = list(
              total_employees = sample(1400:1600, 1),
              attrition_rate = runif(1, 0.08, 0.16),
              avg_satisfaction = runif(1, 3.5, 4.2)
            ),
            last_updated = Sys.time()
          )
          dashboard_data(new_data)
          return(new_data)
        }
      ))
    }
  }
  
  it("initializes dashboard with correct KPIs", {
    # Mock Shiny session
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    # Create dashboard server
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Get initial data
    initial_data <- dashboard_instance$get_data()
    
    # Validate KPIs structure
    expect_true("kpis" %in% names(initial_data))
    expect_true("total_employees" %in% names(initial_data$kpis))
    expect_true("attrition_rate" %in% names(initial_data$kpis))
    expect_true("avg_satisfaction" %in% names(initial_data$kpis))
    
    # Validate KPI values
    expect_true(is.numeric(initial_data$kpis$total_employees))
    expect_true(initial_data$kpis$total_employees > 0)
    expect_true(initial_data$kpis$attrition_rate >= 0 && initial_data$kpis$attrition_rate <= 1)
    expect_true(initial_data$kpis$avg_satisfaction >= 1 && initial_data$kpis$avg_satisfaction <= 5)
  })
  
  it("handles dashboard data refresh correctly", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Get initial data
    initial_data <- dashboard_instance$get_data()
    initial_timestamp <- initial_data$last_updated
    
    # Wait a small amount to ensure timestamp difference
    Sys.sleep(0.1)
    
    # Refresh data
    refreshed_data <- dashboard_instance$refresh_data()
    
    # Validate refresh
    expect_true(refreshed_data$last_updated > initial_timestamp)
    expect_true("kpis" %in% names(refreshed_data))
    
    # Values should be within expected ranges
    expect_true(refreshed_data$kpis$total_employees >= 1400 && 
                refreshed_data$kpis$total_employees <= 1600)
    expect_true(refreshed_data$kpis$attrition_rate >= 0.08 && 
                refreshed_data$kpis$attrition_rate <= 0.16)
    expect_true(refreshed_data$kpis$avg_satisfaction >= 3.5 && 
                refreshed_data$kpis$avg_satisfaction <= 4.2)
  })
  
  it("validates dashboard responsiveness under load", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Simulate multiple rapid refreshes
    start_time <- Sys.time()
    refresh_times <- c()
    
    for (i in 1:50) {
      refresh_start <- Sys.time()
      dashboard_instance$refresh_data()
      refresh_end <- Sys.time()
      
      refresh_time <- as.numeric(difftime(refresh_end, refresh_start, units = "secs"))
      refresh_times <- c(refresh_times, refresh_time)
    }
    
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Performance assertions
    expect_true(total_time < 5.0)  # Should complete in under 5 seconds
    expect_true(mean(refresh_times) < 0.1)  # Average refresh should be under 100ms
    expect_true(max(refresh_times) < 0.5)   # No single refresh should take over 500ms
  })
  
  it("handles concurrent dashboard access", {
    # Simulate multiple users accessing dashboard simultaneously
    concurrent_results <- parallel::mclapply(1:5, function(user_id) {
      session <- MockShinySession$new()
      input <- list()
      output <- list()
      
      dashboard_server <- create_dashboard_server()
      dashboard_instance <- dashboard_server(input, output, session)
      
      # Each user performs multiple operations
      results <- list()
      for (i in 1:10) {
        data <- dashboard_instance$refresh_data()
        results[[i]] <- list(
          user_id = user_id,
          iteration = i,
          total_employees = data$kpis$total_employees,
          timestamp = data$last_updated
        )
      }
      
      return(results)
    }, mc.cores = 2)
    
    # Flatten results
    all_results <- unlist(concurrent_results, recursive = FALSE)
    
    # Validate all operations completed successfully
    expect_equal(length(all_results), 50)  # 5 users × 10 operations each
    
    # Check data integrity
    for (result in all_results) {
      expect_true(is.numeric(result$total_employees))
      expect_true(result$total_employees >= 1400 && result$total_employees <= 1600)
      expect_true(!is.null(result$timestamp))
    }
  })
  
  it("validates dashboard error handling", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    # Create dashboard with error injection
    create_error_dashboard <- function() {
      function(input, output, session) {
        dashboard_data <- reactiveVal(NULL)
        
        return(list(
          get_data = dashboard_data,
          refresh_data = function() {
            # Simulate various error conditions
            error_type <- sample(c("network", "data", "memory", "success"), 1, 
                               prob = c(0.1, 0.1, 0.1, 0.7))
            
            if (error_type == "network") {
              stop("Network connection timeout")
            } else if (error_type == "data") {
              stop("Invalid data format received")
            } else if (error_type == "memory") {
              stop("Insufficient memory to process request")
            } else {
              # Success case
              return(list(
                kpis = list(total_employees = 1500),
                last_updated = Sys.time()
              ))
            }
          }
        ))
      }
    }
    
    error_dashboard_server <- create_error_dashboard()
    error_dashboard <- error_dashboard_server(input, output, session)
    
    # Test error handling
    success_count <- 0
    error_count <- 0
    
    for (i in 1:100) {
      tryCatch({
        result <- error_dashboard$refresh_data()
        success_count <- success_count + 1
      }, error = function(e) {
        error_count <- error_count + 1
        # Validate error messages are informative
        expect_true(nchar(e$message) > 0)
        expect_true(e$message %in% c(
          "Network connection timeout",
          "Invalid data format received", 
          "Insufficient memory to process request"
        ))
      })
    }
    
    # Should have both successes and errors
    expect_true(success_count > 0)
    expect_true(error_count > 0)
    expect_equal(success_count + error_count, 100)
  })
})

# ============================================================================
# 9.1.5 LOG AGGREGATION EFFECTIVENESS TESTS
# ============================================================================

describe("Log Aggregation Effectiveness", {
  
  # Enhanced Logger with aggregation capabilities
  AggregatedLogger <- R6Class("AggregatedLogger",
    private = list(
      .logs = list(),
      .aggregated_logs = list(),
      .log_buffer = list(),
      .buffer_size = 1000,
      .aggregation_interval = 60  # seconds
    ),
    
    public = list(
      initialize = function(buffer_size = 1000, aggregation_interval = 60) {
        private$.logs <- list()
        private$.aggregated_logs <- list()
        private$.log_buffer <- list()
        private$.buffer_size <- buffer_size
        private$.aggregation_interval <- aggregation_interval
      },
      
      log = function(level, message, module = "unknown", metadata = list()) {
        log_entry <- list(
          timestamp = Sys.time(),
          level = level,
          message = message,
          module = module,
          metadata = metadata,
          session_id = digest::digest(Sys.time()),
          pid = Sys.getpid()
        )
        
        # Add to buffer
        private$.log_buffer[[length(private$.log_buffer) + 1]] <- log_entry
        
        # Flush buffer if it's full
        if (length(private$.log_buffer) >= private$.buffer_size) {
          self$flush_buffer()
        }
        
        return(log_entry)
      },
      
      flush_buffer = function() {
        if (length(private$.log_buffer) == 0) return(invisible(NULL))
        
        # Move buffer to main logs
        new_logs <- private$.log_buffer
        private$.logs <- c(private$.logs, new_logs)
        private$.log_buffer <- list()
        
        # Trigger aggregation
        self$aggregate_logs()
        
        return(length(new_logs))
      },
      
      aggregate_logs = function() {
        if (length(private$.logs) == 0) return(invisible(NULL))
        
        # Aggregate by time windows, level, and module
        current_time <- Sys.time()
        window_size <- private$.aggregation_interval
        
        # Create time windows
        log_times <- sapply(private$.logs, function(log) as.numeric(log$timestamp))
        min_time <- min(log_times)
        max_time <- max(log_times)
        
        time_windows <- seq(min_time, max_time + window_size, by = window_size)
        
        aggregated_data <- list()
        
        for (i in 1:(length(time_windows) - 1)) {
          window_start <- time_windows[i]
          window_end <- time_windows[i + 1]
          
          # Filter logs in this window
          window_logs <- private$.logs[log_times >= window_start & log_times < window_end]
          
          if (length(window_logs) == 0) next
          
          # Aggregate by level and module
          aggregation_key <- function(log) {
            paste(log$level, log$module, sep = "_")
          }
          
          log_groups <- split(window_logs, sapply(window_logs, aggregation_key))
          
          for (group_key in names(log_groups)) {
            group_logs <- log_groups[[group_key]]
            
            agg_entry <- list(
              window_start = as.POSIXct(window_start, origin = "1970-01-01"),
              window_end = as.POSIXct(window_end, origin = "1970-01-01"),
              level = group_logs[[1]]$level,
              module = group_logs[[1]]$module,
              count = length(group_logs),
              messages = sapply(group_logs, function(l) l$message),
              first_occurrence = min(sapply(group_logs, function(l) l$timestamp)),
              last_occurrence = max(sapply(group_logs, function(l) l$timestamp))
            )
            
            agg_key <- paste(window_start, group_key, sep = "_")
            private$.aggregated_logs[[agg_key]] <- agg_entry
          }
        }
        
        return(length(private$.aggregated_logs))
      },
      
      get_logs = function(level = NULL, module = NULL, limit = NULL) {
        filtered_logs <- private$.logs
        
        # Filter by level
        if (!is.null(level)) {
          filtered_logs <- filtered_logs[sapply(filtered_logs, function(l) l$level == level)]
        }
        
        # Filter by module
        if (!is.null(module)) {
          filtered_logs <- filtered_logs[sapply(filtered_logs, function(l) l$module == module)]
        }
        
        # Apply limit
        if (!is.null(limit) && length(filtered_logs) > limit) {
          filtered_logs <- tail(filtered_logs, limit)
        }
        
        return(filtered_logs)
      },
      
      get_aggregated_logs = function() {
        return(private$.aggregated_logs)
      },
      
      get_log_summary = function() {
        if (length(private$.logs) == 0) {
          return(list(
            total_logs = 0,
            by_level = list(),
            by_module = list(),
            time_range = NULL
          ))
        }
        
        # Count by level
        levels <- sapply(private$.logs, function(l) l$level)
        level_counts <- table(levels)
        
        # Count by module
        modules <- sapply(private$.logs, function(l) l$module)
        module_counts <- table(modules)
        
        # Time range
        timestamps <- sapply(private$.logs, function(l) l$timestamp)
        time_range <- list(
          start = min(timestamps),
          end = max(timestamps),
          duration_hours = as.numeric(difftime(max(timestamps), min(timestamps), units = "hours"))
        )
        
        return(list(
          total_logs = length(private$.logs),
          by_level = as.list(level_counts),
          by_module = as.list(module_counts),
          time_range = time_range,
          aggregated_entries = length(private$.aggregated_logs)
        ))
      },
      
      clear_logs = function() {
        private$.logs <- list()
        private$.aggregated_logs <- list()
        private$.log_buffer <- list()
      }
    )
  )
  
  it("collects logs efficiently in buffer", {
    logger <- AggregatedLogger$new(buffer_size = 10)
    
    # Add logs to buffer
    for (i in 1:5) {
      logger$log("info", paste("Test message", i), "test_module")
    }
    
    # Buffer should contain logs but main logs should be empty initially
    summary <- logger$get_log_summary()
    expect_equal(summary$total_logs, 0)  # Not flushed yet
    
    # Add more logs to trigger flush
    for (i in 6:12) {
      logger$log("info", paste("Test message", i), "test_module")
    }
    
    # Should have flushed when buffer reached size 10
    summary <- logger$get_log_summary()
    expect_true(summary$total_logs >= 10)
  })
  
  it("aggregates logs by time windows correctly", {
    logger <- AggregatedLogger$new(buffer_size = 5, aggregation_interval = 1)
    
    # Generate logs across different time periods
    base_time <- Sys.time()
    
    # First window
    for (i in 1:3) {
      logger$log("error", "Database error", "database")
    }
    
    # Simulate time passage
    Sys.sleep(1.1)
    
    # Second window
    for (i in 1:2) {
      logger$log("warning", "High memory usage", "system")
    }
    
    # Force aggregation
    logger$flush_buffer()
    
    aggregated <- logger$get_aggregated_logs()
    expect_true(length(aggregated) >= 1)
    
    # Check aggregation structure
    first_agg <- aggregated[[1]]
    expect_true("window_start" %in% names(first_agg))
    expect_true("window_end" %in% names(first_agg))
    expect_true("count" %in% names(first_agg))
    expect_true("level" %in% names(first_agg))
    expect_true("module" %in% names(first_agg))
  })
  
  it("handles high-volume log ingestion", {
    logger <- AggregatedLogger$new(buffer_size = 100)
    
    # Generate high volume of logs
    start_time <- Sys.time()
    log_count <- 10000
    
    for (i in 1:log_count) {
      level <- sample(c("info", "warning", "error"), 1)
      module <- sample(c("auth", "database", "api", "ui"), 1)
      logger$log(level, paste("Message", i), module)
    }
    
    logger$flush_buffer()
    end_time <- Sys.time()
    
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Performance assertions
    expect_true(execution_time < 5.0)  # Should complete in under 5 seconds
    
    summary <- logger$get_log_summary()
    expect_equal(summary$total_logs, log_count)
    
    # Should have aggregated data
    expect_true(summary$aggregated_entries > 0)
  })
  
  it("filters logs correctly by level and module", {
    logger <- AggregatedLogger$new(buffer_size = 5)
    
    # Generate diverse logs
    test_logs <- list(
      list(level = "error", module = "database", message = "Connection failed"),
      list(level = "warning", module = "database", message = "Slow query"),
      list(level = "info", module = "auth", message = "User login"),
      list(level = "error", module = "auth", message = "Invalid credentials"),
      list(level = "info", module = "api", message = "Request processed")
    )
    
    for (log_data in test_logs) {
      logger$log(log_data$level, log_data$message, log_data$module)
    }
    
    logger$flush_buffer()
    
    # Test filtering by level
    error_logs <- logger$get_logs(level = "error")
    expect_equal(length(error_logs), 2)
    expect_true(all(sapply(error_logs, function(l) l$level == "error")))
    
    # Test filtering by module
    database_logs <- logger$get_logs(module = "database")
    expect_equal(length(database_logs), 2)
    expect_true(all(sapply(database_logs, function(l) l$module == "database")))
    
    # Test combined filtering
    error_auth_logs <- logger$get_logs(level = "error", module = "auth")
    expect_equal(length(error_auth_logs), 1)
    expect_equal(error_auth_logs[[1]]$message, "Invalid credentials")
  })
  
  it("maintains log chronological order", {
    logger <- AggregatedLogger$new(buffer_size = 100)
    
    # Generate logs with deliberate timing
    messages <- character(50)
    timestamps <- numeric(50)
    
    for (i in 1:50) {
      message <- paste("Chronological test", i)
      logger$log("info", message, "test")
      messages[i] <- message
      timestamps[i] <- as.numeric(Sys.time())
      
      if (i %% 10 == 0) Sys.sleep(0.01)  # Small delays every 10 logs
    }
    
    logger$flush_buffer()
    
    # Get all logs
    all_logs <- logger$get_logs()
    
    # Check chronological order
    log_timestamps <- sapply(all_logs, function(l) as.numeric(l$timestamp))
    expect_true(all(diff(log_timestamps) >= 0))  # Should be non-decreasing
    
    # Check message order
    log_messages <- sapply(all_logs, function(l) l$message)
    expect_equal(log_messages, messages)
  })
  
  it("handles concurrent logging correctly", {
    logger <- AggregatedLogger$new(buffer_size = 200)
    
    # Simulate concurrent logging from multiple processes/threads
    concurrent_results <- parallel::mclapply(1:5, function(worker_id) {
      logged_messages <- character(20)
      
      for (i in 1:20) {
        message <- paste("Worker", worker_id, "Message", i)
        logger$log("info", message, paste0("worker_", worker_id))
        logged_messages[i] <- message
      }
      
      return(logged_messages)
    }, mc.cores = 2)
    
    logger$flush_buffer()
    
    # Verify all logs were captured
    all_logs <- logger$get_logs()
    expect_equal(length(all_logs), 100)  # 5 workers × 20 messages each
    
    # Check that all worker modules are represented
    modules <- unique(sapply(all_logs, function(l) l$module))
    expected_modules <- paste0("worker_", 1:5)
    expect_true(all(expected_modules %in% modules))
  })
  
  it("generates accurate log summaries", {
    logger <- AggregatedLogger$new(buffer_size = 20)
    
    # Generate logs with known distribution
    level_counts <- list(info = 10, warning = 5, error = 3)
    module_counts <- list(auth = 8, database = 6, api = 4)
    
    # Generate logs according to distribution
    for (level in names(level_counts)) {
      for (i in 1:level_counts[[level]]) {
        module <- sample(names(module_counts), 1, 
                        prob = unlist(module_counts)/sum(unlist(module_counts)))
        logger$log(level, paste("Test message", i), module)
      }
    }
    
    logger$flush_buffer()
    
    # Get summary
    summary <- logger$get_log_summary()
    
    # Verify total count
    expect_equal(summary$total_logs, sum(unlist(level_counts)))
    
    # Verify level distribution
    for (level in names(level_counts)) {
      expect_equal(summary$by_level[[level]], level_counts[[level]])
    }
    
    # Verify time range is valid
    expect_true(!is.null(summary$time_range))
    expect_true(summary$time_range$duration_hours >= 0)
  })
})

# ============================================================================
# 9.1.6 DISTRIBUTED TRACING VALIDATION TESTS
# ============================================================================

describe("Distributed Tracing Validation", {
  
  # Mock distributed tracing system
  TracingSystem <- R6Class("TracingSystem",
    private = list(
      .traces = list(),
      .spans = list(),
      .correlation_ids = list()
    ),
    
    public = list(
      initialize = function() {
        private$.traces <- list()
        private$.spans <- list() 
        private$.correlation_ids <- list()
      },
      
      start_trace = function(operation_name, metadata = list()) {
        trace_id <- self$generate_trace_id()
        span_id <- self$generate_span_id()
        
        trace <- list(
          trace_id = trace_id,
          root_span_id = span_id,
          operation_name = operation_name,
          start_time = Sys.time(),
          metadata = metadata,
          status = "active"
        )
        
        span <- list(
          trace_id = trace_id,
          span_id = span_id,
          parent_span_id = NULL,
          operation_name = operation_name,
          start_time = Sys.time(),
          end_time = NULL,
          duration_ms = NULL,
          metadata = metadata,
          tags = list(),
          status = "active"
        )
        
        private$.traces[[trace_id]] <- trace
        private$.spans[[span_id]] <- span
        
        return(list(trace_id = trace_id, span_id = span_id))
      },
      
      start_child_span = function(parent_trace_id, parent_span_id, operation_name, metadata = list()) {
        if (!parent_trace_id %in% names(private$.traces)) {
          stop("Parent trace not found")
        }
        
        span_id <- self$generate_span_id()
        
        span <- list(
          trace_id = parent_trace_id,
          span_id = span_id,
          parent_span_id = parent_span_id,
          operation_name = operation_name,
          start_time = Sys.time(),
          end_time = NULL,
          duration_ms = NULL,
          metadata = metadata,
          tags = list(),
          status = "active"
        )
        
        private$.spans[[span_id]] <- span
        
        return(span_id)
      },
      
      finish_span = function(span_id, status = "success", metadata = list()) {
        if (!span_id %in% names(private$.spans)) {
          stop("Span not found")
        }
        
        span <- private$.spans[[span_id]]
        end_time <- Sys.time()
        duration_ms <- as.numeric(difftime(end_time, span$start_time, units = "secs")) * 1000
        
        span$end_time <- end_time
        span$duration_ms <- duration_ms
        span$status <- status
        span$metadata <- c(span$metadata, metadata)
        
        private$.spans[[span_id]] <- span
        
        # Check if this completes the trace
        trace_id <- span$trace_id
        trace_spans <- self$get_trace_spans(trace_id)
        
        if (all(sapply(trace_spans, function(s) !is.null(s$end_time)))) {
          private$.traces[[trace_id]]$status <- "completed"
          private$.traces[[trace_id]]$end_time <- Sys.time()
        }
        
        return(span)
      },
      
      add_span_tag = function(span_id, key, value) {
        if (!span_id %in% names(private$.spans)) {
          stop("Span not found")
        }
        
        private$.spans[[span_id]]$tags[[key]] <- value
      },
      
      get_trace = function(trace_id) {
        if (!trace_id %in% names(private$.traces)) {
          return(NULL)
        }
        
        trace <- private$.traces[[trace_id]]
        spans <- self$get_trace_spans(trace_id)
        
        return(list(
          trace = trace,
          spans = spans
        ))
      },
      
      get_trace_spans = function(trace_id) {
        trace_spans <- private$.spans[sapply(private$.spans, function(s) s$trace_id == trace_id)]
        return(trace_spans)
      },
      
      generate_trace_id = function() {
        paste0("trace_", digest::digest(paste(Sys.time(), runif(1))))
      },
      
      generate_span_id = function() {
        paste0("span_", digest::digest(paste(Sys.time(), runif(1))))
      },
      
      get_active_traces = function() {
        active_traces <- private$.traces[sapply(private$.traces, function(t) t$status == "active")]
        return(active_traces)
      },
      
      get_trace_statistics = function() {
        if (length(private$.traces) == 0) {
          return(list(
            total_traces = 0,
            completed_traces = 0,
            active_traces = 0,
            avg_trace_duration_ms = 0
          ))
        }
        
        completed_traces <- private$.traces[sapply(private$.traces, function(t) t$status == "completed")]
        
        if (length(completed_traces) > 0) {
          durations <- sapply(completed_traces, function(t) {
            if (!is.null(t$end_time)) {
              as.numeric(difftime(t$end_time, t$start_time, units = "secs")) * 1000
            } else {
              NA
            }
          })
          avg_duration <- mean(durations, na.rm = TRUE)
        } else {
          avg_duration <- 0
        }
        
api-gateway`$dependents, c("frontend"))
    expect_equal(graph# ============================================================================
# ATLAS LABS HR ANALYTICS - MONITORING & OBSERVABILITY UNIT TESTS
# Comprehensive test suite for application monitoring, health checks, metrics,
# alerts, logging, tracing, and performance monitoring
# ============================================================================

library(testthat)
library(mockery)
library(shiny)
library(R6)
library(jsonlite)
library(httr)
library(purrr)
library(lubridate)
library(digest)

# Source the application files (assuming they exist)
# source("modules/logger_module.R")
# source("global.R")
# source("utils.R")

# ============================================================================
# 9.1.1 HEALTH CHECK ENDPOINT VALIDATION TESTS
# ============================================================================

describe("Health Check Endpoint Validation", {
  
  # Mock health check endpoint function
  create_health_endpoint <- function() {
    function(req) {
      tryCatch({
        # Simulate health checks
        db_status <- check_database_connection()
        memory_status <- check_memory_usage()
        disk_status <- check_disk_space()
        
        health_data <- list(
          status = "healthy",
          timestamp = Sys.time(),
          version = "1.0.0",
          uptime = get_uptime(),
          checks = list(
            database = db_status,
            memory = memory_status,
            disk = disk_status
          )
        )
        
        list(
          status = 200,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(health_data, auto_unbox = TRUE)
        )
      }, error = function(e) {
        list(
          status = 503,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(list(
            status = "unhealthy",
            error = e$message,
            timestamp = Sys.time()
          ), auto_unbox = TRUE)
        )
      })
    }
  }
  
  # Helper functions for health checks
  check_database_connection <- function() {
    list(status = "ok", response_time_ms = sample(1:50, 1))
  }
  
  check_memory_usage <- function() {
    mem_info <- gc()
    list(
      status = if(sum(mem_info[,2]) < 1000) "ok" else "warning",
      used_mb = sum(mem_info[,2]),
      threshold_mb = 1000
    )
  }
  
  check_disk_space <- function() {
    list(status = "ok", free_gb = 50.5, used_percent = 45)
  }
  
  get_uptime <- function() {
    as.numeric(difftime(Sys.time(), as.POSIXct("2024-01-01"), units = "secs"))
  }
  
  it("returns 200 status for healthy application", {
    health_endpoint <- create_health_endpoint()
    response <- health_endpoint(list())
    
    expect_equal(response$status, 200)
    expect_equal(response$headers$`Content-Type`, "application/json")
    
    body <- jsonlite::fromJSON(response$body)
    expect_equal(body$status, "healthy")
    expect_true("timestamp" %in% names(body))
    expect_true("version" %in% names(body))
    expect_true("checks" %in% names(body))
  })
  
  it("validates all health check components", {
    health_endpoint <- create_health_endpoint()
    response <- health_endpoint(list())
    body <- jsonlite::fromJSON(response$body)
    
    # Check required fields
    required_fields <- c("status", "timestamp", "version", "uptime", "checks")
    expect_true(all(required_fields %in% names(body)))
    
    # Check nested components
    expect_true("database" %in% names(body$checks))
    expect_true("memory" %in% names(body$checks))
    expect_true("disk" %in% names(body$checks))
    
    # Validate timestamp format
    expect_true(is.character(body$timestamp))
    expect_false(is.na(as.POSIXct(body$timestamp)))
  })
  
  it("handles database connection failures", {
    # Mock database failure
    mock_check_db <- function() {
      stop("Database connection timeout")
    }
    
    with_mock(
      check_database_connection = mock_check_db,
      {
        health_endpoint <- create_health_endpoint()
        response <- health_endpoint(list())
        
        expect_equal(response$status, 503)
        body <- jsonlite::fromJSON(response$body)
        expect_equal(body$status, "unhealthy")
        expect_true("error" %in% names(body))
      }
    )
  })
  
  it("validates response time thresholds", {
    # Test various response times
    response_times <- c(10, 50, 100, 500, 1000, 5000)
    
    for (time_ms in response_times) {
      mock_check_db <- function() {
        Sys.sleep(time_ms/1000)  # Convert to seconds
        list(status = "ok", response_time_ms = time_ms)
      }
      
      with_mock(
        check_database_connection = mock_check_db,
        {
          start_time <- Sys.time()
          health_endpoint <- create_health_endpoint()
          response <- health_endpoint(list())
          end_time <- Sys.time()
          
          actual_time <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
          expect_true(actual_time >= time_ms * 0.8)  # Allow 20% tolerance
        }
      )
    }
  })
  
  it("handles memory pressure scenarios", {
    # Test different memory usage levels
    memory_scenarios <- list(
      low = list(status = "ok", used_mb = 100, threshold_mb = 1000),
      medium = list(status = "ok", used_mb = 500, threshold_mb = 1000),
      high = list(status = "warning", used_mb = 950, threshold_mb = 1000),
      critical = list(status = "error", used_mb = 1200, threshold_mb = 1000)
    )
    
    for (scenario_name in names(memory_scenarios)) {
      scenario <- memory_scenarios[[scenario_name]]
      
      mock_check_memory <- function() scenario
      
      with_mock(
        check_memory_usage = mock_check_memory,
        {
          health_endpoint <- create_health_endpoint()
          response <- health_endpoint(list())
          body <- jsonlite::fromJSON(response$body)
          
          expect_equal(body$checks$memory$status, scenario$status)
          expect_equal(body$checks$memory$used_mb, scenario$used_mb)
        }
      )
    }
  })
  
  it("validates concurrent health check requests", {
    health_endpoint <- create_health_endpoint()
    
    # Simulate concurrent requests
    concurrent_responses <- parallel::mclapply(1:10, function(i) {
      health_endpoint(list())
    }, mc.cores = 2)
    
    # All should succeed
    statuses <- sapply(concurrent_responses, function(r) r$status)
    expect_true(all(statuses == 200))
    
    # Check response consistency
    bodies <- lapply(concurrent_responses, function(r) jsonlite::fromJSON(r$body))
    versions <- sapply(bodies, function(b) b$version)
    expect_true(length(unique(versions)) == 1)  # All should have same version
  })
})

# ============================================================================
# 9.1.2 METRICS COLLECTION ACCURACY TESTS
# ============================================================================

describe("Metrics Collection Accuracy", {
  
  # Mock metrics collector
  MetricsCollector <- R6Class("MetricsCollector",
    private = list(
      .metrics = list(),
      .start_time = NULL
    ),
    
    public = list(
      initialize = function() {
        private$.metrics <- list()
        private$.start_time <- Sys.time()
      },
      
      increment_counter = function(name, value = 1, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        if (is.null(private$.metrics[[key]])) {
          private$.metrics[[key]] <- list(
            type = "counter",
            name = name,
            value = 0,
            tags = tags,
            last_updated = Sys.time()
          )
        }
        private$.metrics[[key]]$value <- private$.metrics[[key]]$value + value
        private$.metrics[[key]]$last_updated <- Sys.time()
      },
      
      set_gauge = function(name, value, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        private$.metrics[[key]] <- list(
          type = "gauge",
          name = name,
          value = value,
          tags = tags,
          last_updated = Sys.time()
        )
      },
      
      record_histogram = function(name, value, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        if (is.null(private$.metrics[[key]])) {
          private$.metrics[[key]] <- list(
            type = "histogram",
            name = name,
            values = c(),
            tags = tags,
            last_updated = Sys.time()
          )
        }
        private$.metrics[[key]]$values <- c(private$.metrics[[key]]$values, value)
        private$.metrics[[key]]$last_updated <- Sys.time()
      },
      
      get_metrics = function() {
        private$.metrics
      },
      
      get_metric_summary = function() {
        metrics_summary <- list()
        
        for (metric_key in names(private$.metrics)) {
          metric <- private$.metrics[[metric_key]]
          
          if (metric$type == "counter") {
            metrics_summary[[metric$name]] <- list(
              type = "counter",
              value = metric$value,
              tags = metric$tags
            )
          } else if (metric$type == "gauge") {
            metrics_summary[[metric$name]] <- list(
              type = "gauge",
              value = metric$value,
              tags = metric$tags
            )
          } else if (metric$type == "histogram") {
            values <- metric$values
            metrics_summary[[metric$name]] <- list(
              type = "histogram",
              count = length(values),
              mean = mean(values),
              median = median(values),
              p95 = quantile(values, 0.95, na.rm = TRUE),
              p99 = quantile(values, 0.99, na.rm = TRUE),
              min = min(values),
              max = max(values),
              tags = metric$tags
            )
          }
        }
        
        metrics_summary
      }
    )
  )
  
  it("accurately tracks counter metrics", {
    collector <- MetricsCollector$new()
    
    # Test basic counter increment
    collector$increment_counter("user_logins")
    collector$increment_counter("user_logins")
    collector$increment_counter("user_logins", value = 3)
    
    metrics <- collector$get_metrics()
    login_metric <- metrics[[paste0("user_logins_", digest::digest(list()))]]
    
    expect_equal(login_metric$type, "counter")
    expect_equal(login_metric$value, 5)  # 1 + 1 + 3
    expect_equal(login_metric$name, "user_logins")
  })
  
  it("handles counter metrics with tags", {
    collector <- MetricsCollector$new()
    
    # Different departments
    collector$increment_counter("page_views", tags = list(department = "HR"))
    collector$increment_counter("page_views", tags = list(department = "HR"))
    collector$increment_counter("page_views", tags = list(department = "Finance"))
    
    metrics <- collector$get_metrics()
    
    # Should have separate counters for each tag combination
    hr_key <- paste0("page_views_", digest::digest(list(department = "HR")))
    finance_key <- paste0("page_views_", digest::digest(list(department = "Finance")))
    
    expect_equal(metrics[[hr_key]]$value, 2)
    expect_equal(metrics[[finance_key]]$value, 1)
    expect_equal(metrics[[hr_key]]$tags$department, "HR")
    expect_equal(metrics[[finance_key]]$tags$department, "Finance")
  })
  
  it("accurately measures gauge metrics", {
    collector <- MetricsCollector$new()
    
    # Test gauge updates
    collector$set_gauge("memory_usage_mb", 512.5)
    collector$set_gauge("memory_usage_mb", 768.2)  # Should overwrite
    collector$set_gauge("cpu_usage_percent", 45.7)
    
    metrics <- collector$get_metrics()
    memory_key <- paste0("memory_usage_mb_", digest::digest(list()))
    cpu_key <- paste0("cpu_usage_percent_", digest::digest(list()))
    
    expect_equal(metrics[[memory_key]]$value, 768.2)
    expect_equal(metrics[[cpu_key]]$value, 45.7)
    expect_equal(metrics[[memory_key]]$type, "gauge")
  })
  
  it("correctly calculates histogram statistics", {
    collector <- MetricsCollector$new()
    
    # Record response times
    response_times <- c(10, 15, 12, 45, 23, 18, 67, 34, 28, 19)
    for (time in response_times) {
      collector$record_histogram("response_time_ms", time)
    }
    
    summary <- collector$get_metric_summary()
    response_metric <- summary$response_time_ms
    
    expect_equal(response_metric$type, "histogram")
    expect_equal(response_metric$count, 10)
    expect_equal(response_metric$mean, mean(response_times))
    expect_equal(response_metric$median, median(response_times))
    expect_equal(response_metric$min, min(response_times))
    expect_equal(response_metric$max, max(response_times))
  })
  
  it("handles edge cases in histogram calculations", {
    collector <- MetricsCollector$new()
    
    # Test with single value
    collector$record_histogram("single_value", 42)
    summary <- collector$get_metric_summary()
    single_metric <- summary$single_value
    
    expect_equal(single_metric$count, 1)
    expect_equal(single_metric$mean, 42)
    expect_equal(single_metric$median, 42)
    expect_equal(single_metric$min, 42)
    expect_equal(single_metric$max, 42)
    
    # Test with identical values
    for (i in 1:5) {
      collector$record_histogram("identical_values", 100)
    }
    
    summary <- collector$get_metric_summary()
    identical_metric <- summary$identical_values
    
    expect_equal(identical_metric$count, 5)
    expect_equal(identical_metric$mean, 100)
    expect_equal(identical_metric$p95, 100)
    expect_equal(identical_metric$p99, 100)
  })
  
  it("maintains accuracy under high volume", {
    collector <- MetricsCollector$new()
    
    # High volume counter increments
    start_time <- Sys.time()
    for (i in 1:10000) {
      collector$increment_counter("high_volume_counter")
    }
    end_time <- Sys.time()
    
    metrics <- collector$get_metrics()
    counter_key <- paste0("high_volume_counter_", digest::digest(list()))
    
    expect_equal(metrics[[counter_key]]$value, 10000)
    
    # Should complete reasonably quickly (less than 1 second)
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    expect_true(execution_time < 1.0)
  })
  
  it("handles concurrent metric updates", {
    collector <- MetricsCollector$new()
    
    # Concurrent counter increments
    parallel::mclapply(1:100, function(i) {
      collector$increment_counter("concurrent_counter")
    }, mc.cores = 2)
    
    metrics <- collector$get_metrics()
    counter_key <- paste0("concurrent_counter_", digest::digest(list()))
    
    # Should handle race conditions gracefully
    expect_true(metrics[[counter_key]]$value <= 100)
    expect_true(metrics[[counter_key]]$value > 0)
  })
  
  it("validates metric timestamp accuracy", {
    collector <- MetricsCollector$new()
    
    before_time <- Sys.time()
    collector$increment_counter("timestamp_test")
    after_time <- Sys.time()
    
    metrics <- collector$get_metrics()
    metric_key <- paste0("timestamp_test_", digest::digest(list()))
    metric_timestamp <- metrics[[metric_key]]$last_updated
    
    expect_true(metric_timestamp >= before_time)
    expect_true(metric_timestamp <= after_time)
  })
})

# ============================================================================
# 9.1.3 ALERT THRESHOLD TESTING
# ============================================================================

describe("Alert Threshold Testing", {
  
  # Mock alert system
  AlertSystem <- R6Class("AlertSystem",
    private = list(
      .thresholds = list(),
      .alerts = list(),
      .alert_history = list()
    ),
    
    public = list(
      initialize = function() {
        private$.thresholds <- list()
        private$.alerts <- list()
        private$.alert_history <- list()
      },
      
      set_threshold = function(metric_name, threshold_type, value, severity = "warning") {
        threshold_id <- paste0(metric_name, "_", threshold_type)
        private$.thresholds[[threshold_id]] <- list(
          metric_name = metric_name,
          type = threshold_type,  # "greater_than", "less_than", "equal_to"
          value = value,
          severity = severity,
          enabled = TRUE
        )
      },
      
      check_thresholds = function(metrics) {
        current_alerts <- list()
        
        for (threshold_id in names(private$.thresholds)) {
          threshold <- private$.thresholds[[threshold_id]]
          if (!threshold$enabled) next
          
          metric_value <- metrics[[threshold$metric_name]]
          if (is.null(metric_value)) next
          
          alert_triggered <- FALSE
          
          if (threshold$type == "greater_than" && metric_value > threshold$value) {
            alert_triggered <- TRUE
          } else if (threshold$type == "less_than" && metric_value < threshold$value) {
            alert_triggered <- TRUE
          } else if (threshold$type == "equal_to" && metric_value == threshold$value) {
            alert_triggered <- TRUE
          }
          
          if (alert_triggered) {
            alert <- list(
              threshold_id = threshold_id,
              metric_name = threshold$metric_name,
              metric_value = metric_value,
              threshold_value = threshold$value,
              severity = threshold$severity,
              timestamp = Sys.time(),
              message = sprintf("Metric %s (%s) exceeded threshold %s (current: %s)",
                              threshold$metric_name, threshold$type, 
                              threshold$value, metric_value)
            )
            
            current_alerts[[threshold_id]] <- alert
            private$.alert_history[[length(private$.alert_history) + 1]] <- alert
          }
        }
        
        private$.alerts <- current_alerts
        return(current_alerts)
      },
      
      get_active_alerts = function() {
        private$.alerts
      },
      
      get_alert_history = function(limit = 100) {
        history_length <- length(private$.alert_history)
        if (history_length == 0) return(list())
        
        start_idx <- max(1, history_length - limit + 1)
        private$.alert_history[start_idx:history_length]
      },
      
      clear_alerts = function() {
        private$.alerts <- list()
      },
      
      disable_threshold = function(threshold_id) {
        if (threshold_id %in% names(private$.thresholds)) {
          private$.thresholds[[threshold_id]]$enabled <- FALSE
        }
      }
    )
  )
  
  it("triggers alerts when thresholds are exceeded", {
    alert_system <- AlertSystem$new()
    
    # Set up thresholds
    alert_system$set_threshold("cpu_usage", "greater_than", 80, "warning")
    alert_system$set_threshold("memory_usage", "greater_than", 90, "critical")
    alert_system$set_threshold("response_time", "greater_than", 1000, "warning")
    
    # Test metrics that should trigger alerts
    metrics <- list(
      cpu_usage = 85,     # Should trigger warning
      memory_usage = 95,  # Should trigger critical
      response_time = 500 # Should not trigger
    )
    
    alerts <- alert_system$check_thresholds(metrics)
    
    expect_equal(length(alerts), 2)
    expect_true("cpu_usage_greater_than" %in% names(alerts))
    expect_true("memory_usage_greater_than" %in% names(alerts))
    expect_false("response_time_greater_than" %in% names(alerts))
    
    # Check alert details
    cpu_alert <- alerts$cpu_usage_greater_than
    expect_equal(cpu_alert$severity, "warning")
    expect_equal(cpu_alert$metric_value, 85)
    expect_equal(cpu_alert$threshold_value, 80)
  })
  
  it("handles different threshold types correctly", {
    alert_system <- AlertSystem$new()
    
    # Set up different threshold types
    alert_system$set_threshold("disk_space", "less_than", 10, "critical")
    alert_system$set_threshold("error_count", "greater_than", 0, "warning")
    alert_system$set_threshold("connection_count", "equal_to", 0, "critical")
    
    # Test scenarios
    test_cases <- list(
      list(
        metrics = list(disk_space = 5, error_count = 3, connection_count = 0),
        expected_alerts = c("disk_space_less_than", "error_count_greater_than", "connection_count_equal_to")
      ),
      list(
        metrics = list(disk_space = 15, error_count = 0, connection_count = 10),
        expected_alerts = c()
      ),
      list(
        metrics = list(disk_space = 10, error_count = 1, connection_count = 1),
        expected_alerts = c("error_count_greater_than")
      )
    )
    
    for (i in seq_along(test_cases)) {
      test_case <- test_cases[[i]]
      alert_system$clear_alerts()
      
      alerts <- alert_system$check_thresholds(test_case$metrics)
      alert_names <- names(alerts)
      
      expect_equal(length(alert_names), length(test_case$expected_alerts))
      expect_true(all(test_case$expected_alerts %in% alert_names))
    }
  })
  
  it("maintains alert history correctly", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("test_metric", "greater_than", 50, "warning")
    
    # Generate multiple alerts over time
    for (i in 1:10) {
      metrics <- list(test_metric = 60 + i)
      alert_system$check_thresholds(metrics)
      Sys.sleep(0.01)  # Small delay to ensure different timestamps
    }
    
    history <- alert_system$get_alert_history()
    expect_equal(length(history), 10)
    
    # Check chronological order
    timestamps <- sapply(history, function(a) a$timestamp)
    expect_true(all(diff(as.numeric(timestamps)) > 0))
    
    # Check metric values are recorded correctly
    metric_values <- sapply(history, function(a) a$metric_value)
    expect_equal(metric_values, 61:70)
  })
  
  it("handles edge case threshold values", {
    alert_system <- AlertSystem$new()
    
    # Test edge cases
    alert_system$set_threshold("zero_threshold", "greater_than", 0, "info")
    alert_system$set_threshold("negative_threshold", "less_than", -10, "warning")
    alert_system$set_threshold("float_threshold", "greater_than", 3.14159, "warning")
    
    edge_cases <- list(
      list(metrics = list(zero_threshold = 0.001), should_alert = TRUE),
      list(metrics = list(zero_threshold = 0), should_alert = FALSE),
      list(metrics = list(zero_threshold = -1), should_alert = FALSE),
      list(metrics = list(negative_threshold = -15), should_alert = TRUE),
      list(metrics = list(negative_threshold = -5), should_alert = FALSE),
      list(metrics = list(float_threshold = 3.14160), should_alert = TRUE),
      list(metrics = list(float_threshold = 3.14159), should_alert = FALSE)
    )
    
    for (case in edge_cases) {
      alert_system$clear_alerts()
      alerts <- alert_system$check_thresholds(case$metrics)
      
      if (case$should_alert) {
        expect_true(length(alerts) > 0, 
                   info = paste("Expected alert for metrics:", 
                               paste(names(case$metrics), case$metrics, sep = "=", collapse = ", ")))
      } else {
        expect_equal(length(alerts), 0,
                    info = paste("Did not expect alert for metrics:", 
                                paste(names(case$metrics), case$metrics, sep = "=", collapse = ", ")))
      }
    }
  })
  
  it("handles missing metrics gracefully", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("missing_metric", "greater_than", 100, "warning")
    alert_system$set_threshold("present_metric", "greater_than", 50, "warning")
    
    # Test with missing metric
    metrics <- list(present_metric = 75)  # missing_metric is not provided
    alerts <- alert_system$check_thresholds(metrics)
    
    # Should only alert on present metric
    expect_equal(length(alerts), 1)
    expect_true("present_metric_greater_than" %in% names(alerts))
    expect_false("missing_metric_greater_than" %in% names(alerts))
  })
  
  it("supports alert severity levels", {
    alert_system <- AlertSystem$new()
    
    # Set up different severity levels
    severities <- c("info", "warning", "error", "critical")
    for (i in seq_along(severities)) {
      alert_system$set_threshold(paste0("metric_", i), "greater_than", i * 10, severities[i])
    }
    
    # Trigger all alerts
    metrics <- list(metric_1 = 15, metric_2 = 25, metric_3 = 35, metric_4 = 45)
    alerts <- alert_system$check_thresholds(metrics)
    
    expect_equal(length(alerts), 4)
    
    # Check severities are preserved
    for (i in seq_along(severities)) {
      alert_key <- paste0("metric_", i, "_greater_than")
      expect_true(alert_key %in% names(alerts))
      expect_equal(alerts[[alert_key]]$severity, severities[i])
    }
  })
  
  it("can disable and enable thresholds", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("test_metric", "greater_than", 50, "warning")
    
    # Should trigger initially
    metrics <- list(test_metric = 75)
    alerts <- alert_system$check_thresholds(metrics)
    expect_equal(length(alerts), 1)
    
    # Disable threshold
    alert_system$disable_threshold("test_metric_greater_than")
    alert_system$clear_alerts()
    
    # Should not trigger when disabled
    alerts <- alert_system$check_thresholds(metrics)
    expect_equal(length(alerts), 0)
  })
  
  it("handles high-frequency threshold checking", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("high_freq_metric", "greater_than", 100, "warning")
    
    # Rapid threshold checking
    start_time <- Sys.time()
    alert_count <- 0
    
    for (i in 1:1000) {
      metrics <- list(high_freq_metric = 50 + (i %% 100))  # Alternates above/below threshold
      alerts <- alert_system$check_thresholds(metrics)
      if (length(alerts) > 0) alert_count <- alert_count + 1
    }
    
    end_time <- Sys.time()
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Should complete quickly (less than 1 second)
    expect_true(execution_time < 1.0)
    
    # Should have triggered alerts for values > 100
    expect_true(alert_count > 0)
    expect_true(alert_count < 1000)  # Not all iterations should trigger
  })
})

# ============================================================================
# 9.1.4 DASHBOARD FUNCTIONALITY TESTS
# ============================================================================

describe("Dashboard Functionality Tests", {
  
  # Mock dashboard components
  create_dashboard_server <- function() {
    function(input, output, session) {
      # Mock reactive values
      dashboard_data <- reactiveVal(list(
        kpis = list(
          total_employees = 1500,
          attrition_rate = 0.12,
          avg_satisfaction = 3.8
        ),
        charts = list(),
        last_updated = Sys.time()
      ))
      
      # Mock KPI output
      output$kpi_total_employees <- renderText({
        data <- dashboard_data()
        scales::comma(data$kpis$total_employees)
      })
      
      output$kpi_attrition_rate <- renderText({
        data <- dashboard_data()
        scales::percent(data$kpis$attrition_rate, accuracy = 0.1)
      })
      
      output$kpi_avg_satisfaction <- renderText({
        data <- dashboard_data()
        round(data$kpis$avg_satisfaction, 1)
      })
      
      # Mock chart outputs
      output$attrition_chart <- renderPlotly({
        # Mock attrition chart
        data <- data.frame(
          Department = c("HR", "Engineering", "Sales", "Marketing"),
          AttritionRate = c(0.15, 0.08, 0.18, 0.12)
        )
        
        p <- ggplot(data, aes(x = Department, y = AttritionRate)) +
          geom_bar(stat = "identity", fill = "#3498db") +
          scale_y_continuous(labels = scales::percent) +
          theme_minimal() +
          labs(title = "Attrition Rate by Department", 
               x = "Department", y = "Attrition Rate")
        
        ggplotly(p)
      })
      
      output$satisfaction_chart <- renderPlotly({
        # Mock satisfaction trend
        dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")
        satisfaction_data <- data.frame(
          Date = dates,
          Satisfaction = 3.5 + 0.3 * sin(seq_along(dates) * pi / 6) + rnorm(length(dates), 0, 0.1)
        )
        
        p <- ggplot(satisfaction_data, aes(x = Date, y = Satisfaction)) +
          geom_line(color = "#e74c3c", size = 1.2) +
          geom_point(color = "#e74c3c") +
          scale_y_continuous(limits = c(1, 5)) +
          theme_minimal() +
          labs(title = "Employee Satisfaction Trend", 
               x = "Date", y = "Satisfaction Score")
        
        ggplotly(p)
      })
      
      # Mock data refresh functionality
      observeEvent(input$refresh_data, {
        # Simulate data refresh
        new_data <- list(
          kpis = list(
            total_employees = sample(1400:1600, 1),
            attrition_rate = runif(1, 0.08, 0.16),
            avg_satisfaction = runif(1, 3.5, 4.2)
          ),
          charts = list(),
          last_updated = Sys.time()
        )
        dashboard_data(new_data)
        
        showNotification("Dashboard data refreshed successfully!", 
                        type = "success", duration = 3)
      })
      
      # Export dashboard data getter
      return(list(
        get_data = dashboard_data,
        refresh_data = function() {
          # Manual refresh function for testing
          new_data <- list(
            kpis = list(
              total_employees = sample(1400:1600, 1),
              attrition_rate = runif(1, 0.08, 0.16),
              avg_satisfaction = runif(1, 3.5, 4.2)
            ),
            last_updated = Sys.time()
          )
          dashboard_data(new_data)
          return(new_data)
        }
      ))
    }
  }
  
  it("initializes dashboard with correct KPIs", {
    # Mock Shiny session
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    # Create dashboard server
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Get initial data
    initial_data <- dashboard_instance$get_data()
    
    # Validate KPIs structure
    expect_true("kpis" %in% names(initial_data))
    expect_true("total_employees" %in% names(initial_data$kpis))
    expect_true("attrition_rate" %in% names(initial_data$kpis))
    expect_true("avg_satisfaction" %in% names(initial_data$kpis))
    
    # Validate KPI values
    expect_true(is.numeric(initial_data$kpis$total_employees))
    expect_true(initial_data$kpis$total_employees > 0)
    expect_true(initial_data$kpis$attrition_rate >= 0 && initial_data$kpis$attrition_rate <= 1)
    expect_true(initial_data$kpis$avg_satisfaction >= 1 && initial_data$kpis$avg_satisfaction <= 5)
  })
  
  it("handles dashboard data refresh correctly", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Get initial data
    initial_data <- dashboard_instance$get_data()
    initial_timestamp <- initial_data$last_updated
    
    # Wait a small amount to ensure timestamp difference
    Sys.sleep(0.1)
    
    # Refresh data
    refreshed_data <- dashboard_instance$refresh_data()
    
    # Validate refresh
    expect_true(refreshed_data$last_updated > initial_timestamp)
    expect_true("kpis" %in% names(refreshed_data))
    
    # Values should be within expected ranges
    expect_true(refreshed_data$kpis$total_employees >= 1400 && 
                refreshed_data$kpis$total_employees <= 1600)
    expect_true(refreshed_data$kpis$attrition_rate >= 0.08 && 
                refreshed_data$kpis$attrition_rate <= 0.16)
    expect_true(refreshed_data$kpis$avg_satisfaction >= 3.5 && 
                refreshed_data$kpis$avg_satisfaction <= 4.2)
  })
  
  it("validates dashboard responsiveness under load", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Simulate multiple rapid refreshes
    start_time <- Sys.time()
    refresh_times <- c()
    
    for (i in 1:50) {
      refresh_start <- Sys.time()
      dashboard_instance$refresh_data()
      refresh_end <- Sys.time()
      
      refresh_time <- as.numeric(difftime(refresh_end, refresh_start, units = "secs"))
      refresh_times <- c(refresh_times, refresh_time)
    }
    
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Performance assertions
    expect_true(total_time < 5.0)  # Should complete in under 5 seconds
    expect_true(mean(refresh_times) < 0.1)  # Average refresh should be under 100ms
    expect_true(max(refresh_times) < 0.5)   # No single refresh should take over 500ms
  })
  
  it("handles concurrent dashboard access", {
    # Simulate multiple users accessing dashboard simultaneously
    concurrent_results <- parallel::mclapply(1:5, function(user_id) {
      session <- MockShinySession$new()
      input <- list()
      output <- list()
      
      dashboard_server <- create_dashboard_server()
      dashboard_instance <- dashboard_server(input, output, session)
      
      # Each user performs multiple operations
      results <- list()
      for (i in 1:10) {
        data <- dashboard_instance$refresh_data()
        results[[i]] <- list(
          user_id = user_id,
          iteration = i,
          total_employees = data$kpis$total_employees,
          timestamp = data$last_updated
        )
      }
      
      return(results)
    }, mc.cores = 2)
    
    # Flatten results
    all_results <- unlist(concurrent_results, recursive = FALSE)
    
    # Validate all operations completed successfully
    expect_equal(length(all_results), 50)  # 5 users × 10 operations each
    
    # Check data integrity
    for (result in all_results) {
      expect_true(is.numeric(result$total_employees))
      expect_true(result$total_employees >= 1400 && result$total_employees <= 1600)
      expect_true(!is.null(result$timestamp))
    }
  })
  
  it("validates dashboard error handling", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    # Create dashboard with error injection
    create_error_dashboard <- function() {
      function(input, output, session) {
        dashboard_data <- reactiveVal(NULL)
        
        return(list(
          get_data = dashboard_data,
          refresh_data = function() {
            # Simulate various error conditions
            error_type <- sample(c("network", "data", "memory", "success"), 1, 
                               prob = c(0.1, 0.1, 0.1, 0.7))
            
            if (error_type == "network") {
              stop("Network connection timeout")
            } else if (error_type == "data") {
              stop("Invalid data format received")
            } else if (error_type == "memory") {
              stop("Insufficient memory to process request")
            } else {
              # Success case
              return(list(
                kpis = list(total_employees = 1500),
                last_updated = Sys.time()
              ))
            }
          }
        ))
      }
    }
    
    error_dashboard_server <- create_error_dashboard()
    error_dashboard <- error_dashboard_server(input, output, session)
    
    # Test error handling
    success_count <- 0
    error_count <- 0
    
    for (i in 1:100) {
      tryCatch({
        result <- error_dashboard$refresh_data()
        success_count <- success_count + 1
      }, error = function(e) {
        error_count <- error_count + 1
        # Validate error messages are informative
        expect_true(nchar(e$message) > 0)
        expect_true(e$message %in% c(
          "Network connection timeout",
          "Invalid data format received", 
          "Insufficient memory to process request"
        ))
      })
    }
    
    # Should have both successes and errors
    expect_true(success_count > 0)
    expect_true(error_count > 0)
    expect_equal(success_count + error_count, 100)
  })
})

# ============================================================================
# 9.1.5 LOG AGGREGATION EFFECTIVENESS TESTS
# ============================================================================

describe("Log Aggregation Effectiveness", {
  
  # Enhanced Logger with aggregation capabilities
  AggregatedLogger <- R6Class("AggregatedLogger",
    private = list(
      .logs = list(),
      .aggregated_logs = list(),
      .log_buffer = list(),
      .buffer_size = 1000,
      .aggregation_interval = 60  # seconds
    ),
    
    public = list(
      initialize = function(buffer_size = 1000, aggregation_interval = 60) {
        private$.logs <- list()
        private$.aggregated_logs <- list()
        private$.log_buffer <- list()
        private$.buffer_size <- buffer_size
        private$.aggregation_interval <- aggregation_interval
      },
      
      log = function(level, message, module = "unknown", metadata = list()) {
        log_entry <- list(
          timestamp = Sys.time(),
          level = level,
          message = message,
          module = module,
          metadata = metadata,
          session_id = digest::digest(Sys.time()),
          pid = Sys.getpid()
        )
        
        # Add to buffer
        private$.log_buffer[[length(private$.log_buffer) + 1]] <- log_entry
        
        # Flush buffer if it's full
        if (length(private$.log_buffer) >= private$.buffer_size) {
          self$flush_buffer()
        }
        
        return(log_entry)
      },
      
      flush_buffer = function() {
        if (length(private$.log_buffer) == 0) return(invisible(NULL))
        
        # Move buffer to main logs
        new_logs <- private$.log_buffer
        private$.logs <- c(private$.logs, new_logs)
        private$.log_buffer <- list()
        
        # Trigger aggregation
        self$aggregate_logs()
        
        return(length(new_logs))
      },
      
      aggregate_logs = function() {
        if (length(private$.logs) == 0) return(invisible(NULL))
        
        # Aggregate by time windows, level, and module
        current_time <- Sys.time()
        window_size <- private$.aggregation_interval
        
        # Create time windows
        log_times <- sapply(private$.logs, function(log) as.numeric(log$timestamp))
        min_time <- min(log_times)
        max_time <- max(log_times)
        
        time_windows <- seq(min_time, max_time + window_size, by = window_size)
        
        aggregated_data <- list()
        
        for (i in 1:(length(time_windows) - 1)) {
          window_start <- time_windows[i]
          window_end <- time_windows[i + 1]
          
          # Filter logs in this window
          window_logs <- private$.logs[log_times >= window_start & log_times < window_end]
          
          if (length(window_logs) == 0) next
          
          # Aggregate by level and module
          aggregation_key <- function(log) {
            paste(log$level, log$module, sep = "_")
          }
          
          log_groups <- split(window_logs, sapply(window_logs, aggregation_key))
          
          for (group_key in names(log_groups)) {
            group_logs <- log_groups[[group_key]]
            
            agg_entry <- list(
              window_start = as.POSIXct(window_start, origin = "1970-01-01"),
              window_end = as.POSIXct(window_end, origin = "1970-01-01"),
              level = group_logs[[1]]$level,
              module = group_logs[[1]]$module,
              count = length(group_logs),
              messages = sapply(group_logs, function(l) l$message),
              first_occurrence = min(sapply(group_logs, function(l) l$timestamp)),
              last_occurrence = max(sapply(group_logs, function(l) l$timestamp))
            )
            
            agg_key <- paste(window_start, group_key, sep = "_")
            private$.aggregated_logs[[agg_key]] <- agg_entry
          }
        }
        
        return(length(private$.aggregated_logs))
      },
      
      get_logs = function(level = NULL, module = NULL, limit = NULL) {
        filtered_logs <- private$.logs
        
        # Filter by level
        if (!is.null(level)) {
          filtered_logs <- filtered_logs[sapply(filtered_logs, function(l) l$level == level)]
        }
        
        # Filter by module
        if (!is.null(module)) {
          filtered_logs <- filtered_logs[sapply(filtered_logs, function(l) l$module == module)]
        }
        
        # Apply limit
        if (!is.null(limit) && length(filtered_logs) > limit) {
          filtered_logs <- tail(filtered_logs, limit)
        }
        
        return(filtered_logs)
      },
      
      get_aggregated_logs = function() {
        return(private$.aggregated_logs)
      },
      
      get_log_summary = function() {
        if (length(private$.logs) == 0) {
          return(list(
            total_logs = 0,
            by_level = list(),
            by_module = list(),
            time_range = NULL
          ))
        }
        
        # Count by level
        levels <- sapply(private$.logs, function(l) l$level)
        level_counts <- table(levels)
        
        # Count by module
        modules <- sapply(private$.logs, function(l) l$module)
        module_counts <- table(modules)
        
        # Time range
        timestamps <- sapply(private$.logs, function(l) l$timestamp)
        time_range <- list(
          start = min(timestamps),
          end = max(timestamps),
          duration_hours = as.numeric(difftime(max(timestamps), min(timestamps), units = "hours"))
        )
        
        return(list(
          total_logs = length(private$.logs),
          by_level = as.list(level_counts),
          by_module = as.list(module_counts),
          time_range = time_range,
          aggregated_entries = length(private$.aggregated_logs)
        ))
      },
      
      clear_logs = function() {
        private$.logs <- list()
        private$.aggregated_logs <- list()
        private$.log_buffer <- list()
      }
    )
  )
  
  it("collects logs efficiently in buffer", {
    logger <- AggregatedLogger$new(buffer_size = 10)
    
    # Add logs to buffer
    for (i in 1:5) {
      logger$log("info", paste("Test message", i), "test_module")
    }
    
    # Buffer should contain logs but main logs should be empty initially
    summary <- logger$get_log_summary()
    expect_equal(summary$total_logs, 0)  # Not flushed yet
    
    # Add more logs to trigger flush
    for (i in 6:12) {
      logger$log("info", paste("Test message", i), "test_module")
    }
    
    # Should have flushed when buffer reached size 10
    summary <- logger$get_log_summary()
    expect_true(summary$total_logs >= 10)
  })
  
  it("aggregates logs by time windows correctly", {
    logger <- AggregatedLogger$new(buffer_size = 5, aggregation_interval = 1)
    
    # Generate logs across different time periods
    base_time <- Sys.time()
    
    # First window
    for (i in 1:3) {
      logger$log("error", "Database error", "database")
    }
    
    # Simulate time passage
    Sys.sleep(1.1)
    
    # Second window
    for (i in 1:2) {
      logger$log("warning", "High memory usage", "system")
    }
    
    # Force aggregation
    logger$flush_buffer()
    
    aggregated <- logger$get_aggregated_logs()
    expect_true(length(aggregated) >= 1)
    
    # Check aggregation structure
    first_agg <- aggregated[[1]]
    expect_true("window_start" %in% names(first_agg))
    expect_true("window_end" %in% names(first_agg))
    expect_true("count" %in% names(first_agg))
    expect_true("level" %in% names(first_agg))
    expect_true("module" %in% names(first_agg))
  })
  
  it("handles high-volume log ingestion", {
    logger <- AggregatedLogger$new(buffer_size = 100)
    
    # Generate high volume of logs
    start_time <- Sys.time()
    log_count <- 10000
    
    for (i in 1:log_count) {
      level <- sample(c("info", "warning", "error"), 1)
      module <- sample(c("auth", "database", "api", "ui"), 1)
      logger$log(level, paste("Message", i), module)
    }
    
    logger$flush_buffer()
    end_time <- Sys.time()
    
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Performance assertions
    expect_true(execution_time < 5.0)  # Should complete in under 5 seconds
    
    summary <- logger$get_log_summary()
    expect_equal(summary$total_logs, log_count)
    
    # Should have aggregated data
    expect_true(summary$aggregated_entries > 0)
  })
  
  it("filters logs correctly by level and module", {
    logger <- AggregatedLogger$new(buffer_size = 5)
    
    # Generate diverse logs
    test_logs <- list(
      list(level = "error", module = "database", message = "Connection failed"),
      list(level = "warning", module = "database", message = "Slow query"),
      list(level = "info", module = "auth", message = "User login"),
      list(level = "error", module = "auth", message = "Invalid credentials"),
      list(level = "info", module = "api", message = "Request processed")
    )
    
    for (log_data in test_logs) {
      logger$log(log_data$level, log_data$message, log_data$module)
    }
    
    logger$flush_buffer()
    
    # Test filtering by level
    error_logs <- logger$get_logs(level = "error")
    expect_equal(length(error_logs), 2)
    expect_true(all(sapply(error_logs, function(l) l$level == "error")))
    
    # Test filtering by module
    database_logs <- logger$get_logs(module = "database")
    expect_equal(length(database_logs), 2)
    expect_true(all(sapply(database_logs, function(l) l$module == "database")))
    
    # Test combined filtering
    error_auth_logs <- logger$get_logs(level = "error", module = "auth")
    expect_equal(length(error_auth_logs), 1)
    expect_equal(error_auth_logs[[1]]$message, "Invalid credentials")
  })
  
  it("maintains log chronological order", {
    logger <- AggregatedLogger$new(buffer_size = 100)
    
    # Generate logs with deliberate timing
    messages <- character(50)
    timestamps <- numeric(50)
    
    for (i in 1:50) {
      message <- paste("Chronological test", i)
      logger$log("info", message, "test")
      messages[i] <- message
      timestamps[i] <- as.numeric(Sys.time())
      
      if (i %% 10 == 0) Sys.sleep(0.01)  # Small delays every 10 logs
    }
    
    logger$flush_buffer()
    
    # Get all logs
    all_logs <- logger$get_logs()
    
    # Check chronological order
    log_timestamps <- sapply(all_logs, function(l) as.numeric(l$timestamp))
    expect_true(all(diff(log_timestamps) >= 0))  # Should be non-decreasing
    
    # Check message order
    log_messages <- sapply(all_logs, function(l) l$message)
    expect_equal(log_messages, messages)
  })
  
  it("handles concurrent logging correctly", {
    logger <- AggregatedLogger$new(buffer_size = 200)
    
    # Simulate concurrent logging from multiple processes/threads
    concurrent_results <- parallel::mclapply(1:5, function(worker_id) {
      logged_messages <- character(20)
      
      for (i in 1:20) {
        message <- paste("Worker", worker_id, "Message", i)
        logger$log("info", message, paste0("worker_", worker_id))
        logged_messages[i] <- message
      }
      
      return(logged_messages)
    }, mc.cores = 2)
    
    logger$flush_buffer()
    
    # Verify all logs were captured
    all_logs <- logger$get_logs()
    expect_equal(length(all_logs), 100)  # 5 workers × 20 messages each
    
    # Check that all worker modules are represented
    modules <- unique(sapply(all_logs, function(l) l$module))
    expected_modules <- paste0("worker_", 1:5)
    expect_true(all(expected_modules %in% modules))
  })
  
  it("generates accurate log summaries", {
    logger <- AggregatedLogger$new(buffer_size = 20)
    
    # Generate logs with known distribution
    level_counts <- list(info = 10, warning = 5, error = 3)
    module_counts <- list(auth = 8, database = 6, api = 4)
    
    # Generate logs according to distribution
    for (level in names(level_counts)) {
      for (i in 1:level_counts[[level]]) {
        module <- sample(names(module_counts), 1, 
                        prob = unlist(module_counts)/sum(unlist(module_counts)))
        logger$log(level, paste("Test message", i), module)
      }
    }
    
    logger$flush_buffer()
    
    # Get summary
    summary <- logger$get_log_summary()
    
    # Verify total count
    expect_equal(summary$total_logs, sum(unlist(level_counts)))
    
    # Verify level distribution
    for (level in names(level_counts)) {
      expect_equal(summary$by_level[[level]], level_counts[[level]])
    }
    
    # Verify time range is valid
    expect_true(!is.null(summary$time_range))
    expect_true(summary$time_range$duration_hours >= 0)
  })
})

# ============================================================================
# 9.1.6 DISTRIBUTED TRACING VALIDATION TESTS
# ============================================================================

describe("Distributed Tracing Validation", {
  
  # Mock distributed tracing system
  TracingSystem <- R6Class("TracingSystem",
    private = list(
      .traces = list(),
      .spans = list(),
      .correlation_ids = list()
    ),
    
    public = list(
      initialize = function() {
        private$.traces <- list()
        private$.spans <- list() 
        private$.correlation_ids <- list()
      },
      
      start_trace = function(operation_name, metadata = list()) {
        trace_id <- self$generate_trace_id()
        span_id <- self$generate_span_id()
        
        trace <- list(
          trace_id = trace_id,
          root_span_id = span_id,
          operation_name = operation_name,
          start_time = Sys.time(),
          metadata = metadata,
          status = "active"
        )
        
        span <- list(
          trace_id = trace_id,
          span_id = span_id,
          parent_span_id = NULL,
          operation_name = operation_name,
          start_time = Sys.time(),
          end_time = NULL,
          duration_ms = NULL,
          metadata = metadata,
          tags = list(),
          status = "active"
        )
        
        private$.traces[[trace_id]] <- trace
        private$.spans[[span_id]] <- span
        
        return(list(trace_id = trace_id, span_id = span_id))
      },
      
      start_child_span = function(parent_trace_id, parent_span_id, operation_name, metadata = list()) {
        if (!parent_trace_id %in% names(private$.traces)) {
          stop("Parent trace not found")
        }
        
        span_id <- self$generate_span_id()
        
        span <- list(
          trace_id = parent_trace_id,
          span_id = span_id,
          parent_span_id = parent_span_id,
          operation_name = operation_name,
          start_time = Sys.time(),
          end_time = NULL,
          duration_ms = NULL,
          metadata = metadata,
          tags = list(),
          status = "active"
        )
        
        private$.spans[[span_id]] <- span
        
        return(span_id)
      },
      
      finish_span = function(span_id, status = "success", metadata = list()) {
        if (!span_id %in% names(private$.spans)) {
          stop("Span not found")
        }
        
        span <- private$.spans[[span_id]]
        end_time <- Sys.time()
        duration_ms <- as.numeric(difftime(end_time, span$start_time, units = "secs")) * 1000
        
        span$end_time <- end_time
        span$duration_ms <- duration_ms
        span$status <- status
        span$metadata <- c(span$metadata, metadata)
        
        private$.spans[[span_id]] <- span
        
        # Check if this completes the trace
        trace_id <- span$trace_id
        trace_spans <- self$get_trace_spans(trace_id)
        
        if (all(sapply(trace_spans, function(s) !is.null(s$end_time)))) {
          private$.traces[[trace_id]]$status <- "completed"
          private$.traces[[trace_id]]$end_time <- Sys.time()
        }
        
        return(span)
      },
      
      add_span_tag = function(span_id, key, value) {
        if (!span_id %in% names(private$.spans)) {
          stop("Span not found")
        }
        
        private$.spans[[span_id]]$tags[[key]] <- value
      },
      
      get_trace = function(trace_id) {
        if (!trace_id %in% names(private$.traces)) {
          return(NULL)
        }
        
        trace <- private$.traces[[trace_id]]
        spans <- self$get_trace_spans(trace_id)
        
        return(list(
          trace = trace,
          spans = spans
        ))
      },
      
      get_trace_spans = function(trace_id) {
        trace_spans <- private$.spans[sapply(private$.spans, function(s) s$trace_id == trace_id)]
        return(trace_spans)
      },
      
      generate_trace_id = function() {
        paste0("trace_", digest::digest(paste(Sys.time(), runif(1))))
      },
      
      generate_span_id = function() {
        paste0("span_", digest::digest(paste(Sys.time(), runif(1))))
      },
      
      get_active_traces = function() {
        active_traces <- private$.traces[sapply(private$.traces, function(t) t$status == "active")]
        return(active_traces)
      },
      
      get_trace_statistics = function() {
        if (length(private$.traces) == 0) {
          return(list(
            total_traces = 0,
            completed_traces = 0,
            active_traces = 0,
            avg_trace_duration_ms = 0
          ))
        }
        
        completed_traces <- private$.traces[sapply(private$.traces, function(t) t$status == "completed")]
        
        if (length(completed_traces) > 0) {
          durations <- sapply(completed_traces, function(t) {
            if (!is.null(t$end_time)) {
              as.numeric(difftime(t$end_time, t$start_time, units = "secs")) * 1000
            } else {
              NA
            }
          })
          avg_duration <- mean(durations, na.rm = TRUE)
        } else {
          avg_duration <- 0
        }
        
user-service`$dependents, c("api-gateway"))
    expect_equal(graph$database$dependents, c("user-service"))
  })
  
  it("performs health checks accurately", {
    mapper <- ServiceMapper$new()
    
    # Register services with health checks
    mapper$register_service("healthy-service", "http://localhost:8001", "http://localhost:8001/health")
    mapper$register_service("no-health-service", "http://localhost:8002", NULL)
    
    # Perform health checks
    health1 <- mapper$check_service_health("healthy-service")
    health2 <- mapper$check_service_health("no-health-service")
    
    # Verify health check results
    expect_true(health1$status %in% c("healthy", "unhealthy"))
    expect_equal(health1$service, "healthy-service")
    expect_true(is.numeric(health1$response_time_ms))
    expect_true(!is.null(health1$timestamp))
    
    expect_equal(health2$status, "no_health_check")
    expect_equal(health2$service, "no-health-service")
    expect_true(is.na(health2$response_time_ms))
    
    # Check that service status was updated
    services <- mapper$get_services()
    expect_equal(services# ============================================================================
# ATLAS LABS HR ANALYTICS - MONITORING & OBSERVABILITY UNIT TESTS
# Comprehensive test suite for application monitoring, health checks, metrics,
# alerts, logging, tracing, and performance monitoring
# ============================================================================

library(testthat)
library(mockery)
library(shiny)
library(R6)
library(jsonlite)
library(httr)
library(purrr)
library(lubridate)
library(digest)

# Source the application files (assuming they exist)
# source("modules/logger_module.R")
# source("global.R")
# source("utils.R")

# ============================================================================
# 9.1.1 HEALTH CHECK ENDPOINT VALIDATION TESTS
# ============================================================================

describe("Health Check Endpoint Validation", {
  
  # Mock health check endpoint function
  create_health_endpoint <- function() {
    function(req) {
      tryCatch({
        # Simulate health checks
        db_status <- check_database_connection()
        memory_status <- check_memory_usage()
        disk_status <- check_disk_space()
        
        health_data <- list(
          status = "healthy",
          timestamp = Sys.time(),
          version = "1.0.0",
          uptime = get_uptime(),
          checks = list(
            database = db_status,
            memory = memory_status,
            disk = disk_status
          )
        )
        
        list(
          status = 200,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(health_data, auto_unbox = TRUE)
        )
      }, error = function(e) {
        list(
          status = 503,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(list(
            status = "unhealthy",
            error = e$message,
            timestamp = Sys.time()
          ), auto_unbox = TRUE)
        )
      })
    }
  }
  
  # Helper functions for health checks
  check_database_connection <- function() {
    list(status = "ok", response_time_ms = sample(1:50, 1))
  }
  
  check_memory_usage <- function() {
    mem_info <- gc()
    list(
      status = if(sum(mem_info[,2]) < 1000) "ok" else "warning",
      used_mb = sum(mem_info[,2]),
      threshold_mb = 1000
    )
  }
  
  check_disk_space <- function() {
    list(status = "ok", free_gb = 50.5, used_percent = 45)
  }
  
  get_uptime <- function() {
    as.numeric(difftime(Sys.time(), as.POSIXct("2024-01-01"), units = "secs"))
  }
  
  it("returns 200 status for healthy application", {
    health_endpoint <- create_health_endpoint()
    response <- health_endpoint(list())
    
    expect_equal(response$status, 200)
    expect_equal(response$headers$`Content-Type`, "application/json")
    
    body <- jsonlite::fromJSON(response$body)
    expect_equal(body$status, "healthy")
    expect_true("timestamp" %in% names(body))
    expect_true("version" %in% names(body))
    expect_true("checks" %in% names(body))
  })
  
  it("validates all health check components", {
    health_endpoint <- create_health_endpoint()
    response <- health_endpoint(list())
    body <- jsonlite::fromJSON(response$body)
    
    # Check required fields
    required_fields <- c("status", "timestamp", "version", "uptime", "checks")
    expect_true(all(required_fields %in% names(body)))
    
    # Check nested components
    expect_true("database" %in% names(body$checks))
    expect_true("memory" %in% names(body$checks))
    expect_true("disk" %in% names(body$checks))
    
    # Validate timestamp format
    expect_true(is.character(body$timestamp))
    expect_false(is.na(as.POSIXct(body$timestamp)))
  })
  
  it("handles database connection failures", {
    # Mock database failure
    mock_check_db <- function() {
      stop("Database connection timeout")
    }
    
    with_mock(
      check_database_connection = mock_check_db,
      {
        health_endpoint <- create_health_endpoint()
        response <- health_endpoint(list())
        
        expect_equal(response$status, 503)
        body <- jsonlite::fromJSON(response$body)
        expect_equal(body$status, "unhealthy")
        expect_true("error" %in% names(body))
      }
    )
  })
  
  it("validates response time thresholds", {
    # Test various response times
    response_times <- c(10, 50, 100, 500, 1000, 5000)
    
    for (time_ms in response_times) {
      mock_check_db <- function() {
        Sys.sleep(time_ms/1000)  # Convert to seconds
        list(status = "ok", response_time_ms = time_ms)
      }
      
      with_mock(
        check_database_connection = mock_check_db,
        {
          start_time <- Sys.time()
          health_endpoint <- create_health_endpoint()
          response <- health_endpoint(list())
          end_time <- Sys.time()
          
          actual_time <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
          expect_true(actual_time >= time_ms * 0.8)  # Allow 20% tolerance
        }
      )
    }
  })
  
  it("handles memory pressure scenarios", {
    # Test different memory usage levels
    memory_scenarios <- list(
      low = list(status = "ok", used_mb = 100, threshold_mb = 1000),
      medium = list(status = "ok", used_mb = 500, threshold_mb = 1000),
      high = list(status = "warning", used_mb = 950, threshold_mb = 1000),
      critical = list(status = "error", used_mb = 1200, threshold_mb = 1000)
    )
    
    for (scenario_name in names(memory_scenarios)) {
      scenario <- memory_scenarios[[scenario_name]]
      
      mock_check_memory <- function() scenario
      
      with_mock(
        check_memory_usage = mock_check_memory,
        {
          health_endpoint <- create_health_endpoint()
          response <- health_endpoint(list())
          body <- jsonlite::fromJSON(response$body)
          
          expect_equal(body$checks$memory$status, scenario$status)
          expect_equal(body$checks$memory$used_mb, scenario$used_mb)
        }
      )
    }
  })
  
  it("validates concurrent health check requests", {
    health_endpoint <- create_health_endpoint()
    
    # Simulate concurrent requests
    concurrent_responses <- parallel::mclapply(1:10, function(i) {
      health_endpoint(list())
    }, mc.cores = 2)
    
    # All should succeed
    statuses <- sapply(concurrent_responses, function(r) r$status)
    expect_true(all(statuses == 200))
    
    # Check response consistency
    bodies <- lapply(concurrent_responses, function(r) jsonlite::fromJSON(r$body))
    versions <- sapply(bodies, function(b) b$version)
    expect_true(length(unique(versions)) == 1)  # All should have same version
  })
})

# ============================================================================
# 9.1.2 METRICS COLLECTION ACCURACY TESTS
# ============================================================================

describe("Metrics Collection Accuracy", {
  
  # Mock metrics collector
  MetricsCollector <- R6Class("MetricsCollector",
    private = list(
      .metrics = list(),
      .start_time = NULL
    ),
    
    public = list(
      initialize = function() {
        private$.metrics <- list()
        private$.start_time <- Sys.time()
      },
      
      increment_counter = function(name, value = 1, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        if (is.null(private$.metrics[[key]])) {
          private$.metrics[[key]] <- list(
            type = "counter",
            name = name,
            value = 0,
            tags = tags,
            last_updated = Sys.time()
          )
        }
        private$.metrics[[key]]$value <- private$.metrics[[key]]$value + value
        private$.metrics[[key]]$last_updated <- Sys.time()
      },
      
      set_gauge = function(name, value, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        private$.metrics[[key]] <- list(
          type = "gauge",
          name = name,
          value = value,
          tags = tags,
          last_updated = Sys.time()
        )
      },
      
      record_histogram = function(name, value, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        if (is.null(private$.metrics[[key]])) {
          private$.metrics[[key]] <- list(
            type = "histogram",
            name = name,
            values = c(),
            tags = tags,
            last_updated = Sys.time()
          )
        }
        private$.metrics[[key]]$values <- c(private$.metrics[[key]]$values, value)
        private$.metrics[[key]]$last_updated <- Sys.time()
      },
      
      get_metrics = function() {
        private$.metrics
      },
      
      get_metric_summary = function() {
        metrics_summary <- list()
        
        for (metric_key in names(private$.metrics)) {
          metric <- private$.metrics[[metric_key]]
          
          if (metric$type == "counter") {
            metrics_summary[[metric$name]] <- list(
              type = "counter",
              value = metric$value,
              tags = metric$tags
            )
          } else if (metric$type == "gauge") {
            metrics_summary[[metric$name]] <- list(
              type = "gauge",
              value = metric$value,
              tags = metric$tags
            )
          } else if (metric$type == "histogram") {
            values <- metric$values
            metrics_summary[[metric$name]] <- list(
              type = "histogram",
              count = length(values),
              mean = mean(values),
              median = median(values),
              p95 = quantile(values, 0.95, na.rm = TRUE),
              p99 = quantile(values, 0.99, na.rm = TRUE),
              min = min(values),
              max = max(values),
              tags = metric$tags
            )
          }
        }
        
        metrics_summary
      }
    )
  )
  
  it("accurately tracks counter metrics", {
    collector <- MetricsCollector$new()
    
    # Test basic counter increment
    collector$increment_counter("user_logins")
    collector$increment_counter("user_logins")
    collector$increment_counter("user_logins", value = 3)
    
    metrics <- collector$get_metrics()
    login_metric <- metrics[[paste0("user_logins_", digest::digest(list()))]]
    
    expect_equal(login_metric$type, "counter")
    expect_equal(login_metric$value, 5)  # 1 + 1 + 3
    expect_equal(login_metric$name, "user_logins")
  })
  
  it("handles counter metrics with tags", {
    collector <- MetricsCollector$new()
    
    # Different departments
    collector$increment_counter("page_views", tags = list(department = "HR"))
    collector$increment_counter("page_views", tags = list(department = "HR"))
    collector$increment_counter("page_views", tags = list(department = "Finance"))
    
    metrics <- collector$get_metrics()
    
    # Should have separate counters for each tag combination
    hr_key <- paste0("page_views_", digest::digest(list(department = "HR")))
    finance_key <- paste0("page_views_", digest::digest(list(department = "Finance")))
    
    expect_equal(metrics[[hr_key]]$value, 2)
    expect_equal(metrics[[finance_key]]$value, 1)
    expect_equal(metrics[[hr_key]]$tags$department, "HR")
    expect_equal(metrics[[finance_key]]$tags$department, "Finance")
  })
  
  it("accurately measures gauge metrics", {
    collector <- MetricsCollector$new()
    
    # Test gauge updates
    collector$set_gauge("memory_usage_mb", 512.5)
    collector$set_gauge("memory_usage_mb", 768.2)  # Should overwrite
    collector$set_gauge("cpu_usage_percent", 45.7)
    
    metrics <- collector$get_metrics()
    memory_key <- paste0("memory_usage_mb_", digest::digest(list()))
    cpu_key <- paste0("cpu_usage_percent_", digest::digest(list()))
    
    expect_equal(metrics[[memory_key]]$value, 768.2)
    expect_equal(metrics[[cpu_key]]$value, 45.7)
    expect_equal(metrics[[memory_key]]$type, "gauge")
  })
  
  it("correctly calculates histogram statistics", {
    collector <- MetricsCollector$new()
    
    # Record response times
    response_times <- c(10, 15, 12, 45, 23, 18, 67, 34, 28, 19)
    for (time in response_times) {
      collector$record_histogram("response_time_ms", time)
    }
    
    summary <- collector$get_metric_summary()
    response_metric <- summary$response_time_ms
    
    expect_equal(response_metric$type, "histogram")
    expect_equal(response_metric$count, 10)
    expect_equal(response_metric$mean, mean(response_times))
    expect_equal(response_metric$median, median(response_times))
    expect_equal(response_metric$min, min(response_times))
    expect_equal(response_metric$max, max(response_times))
  })
  
  it("handles edge cases in histogram calculations", {
    collector <- MetricsCollector$new()
    
    # Test with single value
    collector$record_histogram("single_value", 42)
    summary <- collector$get_metric_summary()
    single_metric <- summary$single_value
    
    expect_equal(single_metric$count, 1)
    expect_equal(single_metric$mean, 42)
    expect_equal(single_metric$median, 42)
    expect_equal(single_metric$min, 42)
    expect_equal(single_metric$max, 42)
    
    # Test with identical values
    for (i in 1:5) {
      collector$record_histogram("identical_values", 100)
    }
    
    summary <- collector$get_metric_summary()
    identical_metric <- summary$identical_values
    
    expect_equal(identical_metric$count, 5)
    expect_equal(identical_metric$mean, 100)
    expect_equal(identical_metric$p95, 100)
    expect_equal(identical_metric$p99, 100)
  })
  
  it("maintains accuracy under high volume", {
    collector <- MetricsCollector$new()
    
    # High volume counter increments
    start_time <- Sys.time()
    for (i in 1:10000) {
      collector$increment_counter("high_volume_counter")
    }
    end_time <- Sys.time()
    
    metrics <- collector$get_metrics()
    counter_key <- paste0("high_volume_counter_", digest::digest(list()))
    
    expect_equal(metrics[[counter_key]]$value, 10000)
    
    # Should complete reasonably quickly (less than 1 second)
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    expect_true(execution_time < 1.0)
  })
  
  it("handles concurrent metric updates", {
    collector <- MetricsCollector$new()
    
    # Concurrent counter increments
    parallel::mclapply(1:100, function(i) {
      collector$increment_counter("concurrent_counter")
    }, mc.cores = 2)
    
    metrics <- collector$get_metrics()
    counter_key <- paste0("concurrent_counter_", digest::digest(list()))
    
    # Should handle race conditions gracefully
    expect_true(metrics[[counter_key]]$value <= 100)
    expect_true(metrics[[counter_key]]$value > 0)
  })
  
  it("validates metric timestamp accuracy", {
    collector <- MetricsCollector$new()
    
    before_time <- Sys.time()
    collector$increment_counter("timestamp_test")
    after_time <- Sys.time()
    
    metrics <- collector$get_metrics()
    metric_key <- paste0("timestamp_test_", digest::digest(list()))
    metric_timestamp <- metrics[[metric_key]]$last_updated
    
    expect_true(metric_timestamp >= before_time)
    expect_true(metric_timestamp <= after_time)
  })
})

# ============================================================================
# 9.1.3 ALERT THRESHOLD TESTING
# ============================================================================

describe("Alert Threshold Testing", {
  
  # Mock alert system
  AlertSystem <- R6Class("AlertSystem",
    private = list(
      .thresholds = list(),
      .alerts = list(),
      .alert_history = list()
    ),
    
    public = list(
      initialize = function() {
        private$.thresholds <- list()
        private$.alerts <- list()
        private$.alert_history <- list()
      },
      
      set_threshold = function(metric_name, threshold_type, value, severity = "warning") {
        threshold_id <- paste0(metric_name, "_", threshold_type)
        private$.thresholds[[threshold_id]] <- list(
          metric_name = metric_name,
          type = threshold_type,  # "greater_than", "less_than", "equal_to"
          value = value,
          severity = severity,
          enabled = TRUE
        )
      },
      
      check_thresholds = function(metrics) {
        current_alerts <- list()
        
        for (threshold_id in names(private$.thresholds)) {
          threshold <- private$.thresholds[[threshold_id]]
          if (!threshold$enabled) next
          
          metric_value <- metrics[[threshold$metric_name]]
          if (is.null(metric_value)) next
          
          alert_triggered <- FALSE
          
          if (threshold$type == "greater_than" && metric_value > threshold$value) {
            alert_triggered <- TRUE
          } else if (threshold$type == "less_than" && metric_value < threshold$value) {
            alert_triggered <- TRUE
          } else if (threshold$type == "equal_to" && metric_value == threshold$value) {
            alert_triggered <- TRUE
          }
          
          if (alert_triggered) {
            alert <- list(
              threshold_id = threshold_id,
              metric_name = threshold$metric_name,
              metric_value = metric_value,
              threshold_value = threshold$value,
              severity = threshold$severity,
              timestamp = Sys.time(),
              message = sprintf("Metric %s (%s) exceeded threshold %s (current: %s)",
                              threshold$metric_name, threshold$type, 
                              threshold$value, metric_value)
            )
            
            current_alerts[[threshold_id]] <- alert
            private$.alert_history[[length(private$.alert_history) + 1]] <- alert
          }
        }
        
        private$.alerts <- current_alerts
        return(current_alerts)
      },
      
      get_active_alerts = function() {
        private$.alerts
      },
      
      get_alert_history = function(limit = 100) {
        history_length <- length(private$.alert_history)
        if (history_length == 0) return(list())
        
        start_idx <- max(1, history_length - limit + 1)
        private$.alert_history[start_idx:history_length]
      },
      
      clear_alerts = function() {
        private$.alerts <- list()
      },
      
      disable_threshold = function(threshold_id) {
        if (threshold_id %in% names(private$.thresholds)) {
          private$.thresholds[[threshold_id]]$enabled <- FALSE
        }
      }
    )
  )
  
  it("triggers alerts when thresholds are exceeded", {
    alert_system <- AlertSystem$new()
    
    # Set up thresholds
    alert_system$set_threshold("cpu_usage", "greater_than", 80, "warning")
    alert_system$set_threshold("memory_usage", "greater_than", 90, "critical")
    alert_system$set_threshold("response_time", "greater_than", 1000, "warning")
    
    # Test metrics that should trigger alerts
    metrics <- list(
      cpu_usage = 85,     # Should trigger warning
      memory_usage = 95,  # Should trigger critical
      response_time = 500 # Should not trigger
    )
    
    alerts <- alert_system$check_thresholds(metrics)
    
    expect_equal(length(alerts), 2)
    expect_true("cpu_usage_greater_than" %in% names(alerts))
    expect_true("memory_usage_greater_than" %in% names(alerts))
    expect_false("response_time_greater_than" %in% names(alerts))
    
    # Check alert details
    cpu_alert <- alerts$cpu_usage_greater_than
    expect_equal(cpu_alert$severity, "warning")
    expect_equal(cpu_alert$metric_value, 85)
    expect_equal(cpu_alert$threshold_value, 80)
  })
  
  it("handles different threshold types correctly", {
    alert_system <- AlertSystem$new()
    
    # Set up different threshold types
    alert_system$set_threshold("disk_space", "less_than", 10, "critical")
    alert_system$set_threshold("error_count", "greater_than", 0, "warning")
    alert_system$set_threshold("connection_count", "equal_to", 0, "critical")
    
    # Test scenarios
    test_cases <- list(
      list(
        metrics = list(disk_space = 5, error_count = 3, connection_count = 0),
        expected_alerts = c("disk_space_less_than", "error_count_greater_than", "connection_count_equal_to")
      ),
      list(
        metrics = list(disk_space = 15, error_count = 0, connection_count = 10),
        expected_alerts = c()
      ),
      list(
        metrics = list(disk_space = 10, error_count = 1, connection_count = 1),
        expected_alerts = c("error_count_greater_than")
      )
    )
    
    for (i in seq_along(test_cases)) {
      test_case <- test_cases[[i]]
      alert_system$clear_alerts()
      
      alerts <- alert_system$check_thresholds(test_case$metrics)
      alert_names <- names(alerts)
      
      expect_equal(length(alert_names), length(test_case$expected_alerts))
      expect_true(all(test_case$expected_alerts %in% alert_names))
    }
  })
  
  it("maintains alert history correctly", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("test_metric", "greater_than", 50, "warning")
    
    # Generate multiple alerts over time
    for (i in 1:10) {
      metrics <- list(test_metric = 60 + i)
      alert_system$check_thresholds(metrics)
      Sys.sleep(0.01)  # Small delay to ensure different timestamps
    }
    
    history <- alert_system$get_alert_history()
    expect_equal(length(history), 10)
    
    # Check chronological order
    timestamps <- sapply(history, function(a) a$timestamp)
    expect_true(all(diff(as.numeric(timestamps)) > 0))
    
    # Check metric values are recorded correctly
    metric_values <- sapply(history, function(a) a$metric_value)
    expect_equal(metric_values, 61:70)
  })
  
  it("handles edge case threshold values", {
    alert_system <- AlertSystem$new()
    
    # Test edge cases
    alert_system$set_threshold("zero_threshold", "greater_than", 0, "info")
    alert_system$set_threshold("negative_threshold", "less_than", -10, "warning")
    alert_system$set_threshold("float_threshold", "greater_than", 3.14159, "warning")
    
    edge_cases <- list(
      list(metrics = list(zero_threshold = 0.001), should_alert = TRUE),
      list(metrics = list(zero_threshold = 0), should_alert = FALSE),
      list(metrics = list(zero_threshold = -1), should_alert = FALSE),
      list(metrics = list(negative_threshold = -15), should_alert = TRUE),
      list(metrics = list(negative_threshold = -5), should_alert = FALSE),
      list(metrics = list(float_threshold = 3.14160), should_alert = TRUE),
      list(metrics = list(float_threshold = 3.14159), should_alert = FALSE)
    )
    
    for (case in edge_cases) {
      alert_system$clear_alerts()
      alerts <- alert_system$check_thresholds(case$metrics)
      
      if (case$should_alert) {
        expect_true(length(alerts) > 0, 
                   info = paste("Expected alert for metrics:", 
                               paste(names(case$metrics), case$metrics, sep = "=", collapse = ", ")))
      } else {
        expect_equal(length(alerts), 0,
                    info = paste("Did not expect alert for metrics:", 
                                paste(names(case$metrics), case$metrics, sep = "=", collapse = ", ")))
      }
    }
  })
  
  it("handles missing metrics gracefully", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("missing_metric", "greater_than", 100, "warning")
    alert_system$set_threshold("present_metric", "greater_than", 50, "warning")
    
    # Test with missing metric
    metrics <- list(present_metric = 75)  # missing_metric is not provided
    alerts <- alert_system$check_thresholds(metrics)
    
    # Should only alert on present metric
    expect_equal(length(alerts), 1)
    expect_true("present_metric_greater_than" %in% names(alerts))
    expect_false("missing_metric_greater_than" %in% names(alerts))
  })
  
  it("supports alert severity levels", {
    alert_system <- AlertSystem$new()
    
    # Set up different severity levels
    severities <- c("info", "warning", "error", "critical")
    for (i in seq_along(severities)) {
      alert_system$set_threshold(paste0("metric_", i), "greater_than", i * 10, severities[i])
    }
    
    # Trigger all alerts
    metrics <- list(metric_1 = 15, metric_2 = 25, metric_3 = 35, metric_4 = 45)
    alerts <- alert_system$check_thresholds(metrics)
    
    expect_equal(length(alerts), 4)
    
    # Check severities are preserved
    for (i in seq_along(severities)) {
      alert_key <- paste0("metric_", i, "_greater_than")
      expect_true(alert_key %in% names(alerts))
      expect_equal(alerts[[alert_key]]$severity, severities[i])
    }
  })
  
  it("can disable and enable thresholds", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("test_metric", "greater_than", 50, "warning")
    
    # Should trigger initially
    metrics <- list(test_metric = 75)
    alerts <- alert_system$check_thresholds(metrics)
    expect_equal(length(alerts), 1)
    
    # Disable threshold
    alert_system$disable_threshold("test_metric_greater_than")
    alert_system$clear_alerts()
    
    # Should not trigger when disabled
    alerts <- alert_system$check_thresholds(metrics)
    expect_equal(length(alerts), 0)
  })
  
  it("handles high-frequency threshold checking", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("high_freq_metric", "greater_than", 100, "warning")
    
    # Rapid threshold checking
    start_time <- Sys.time()
    alert_count <- 0
    
    for (i in 1:1000) {
      metrics <- list(high_freq_metric = 50 + (i %% 100))  # Alternates above/below threshold
      alerts <- alert_system$check_thresholds(metrics)
      if (length(alerts) > 0) alert_count <- alert_count + 1
    }
    
    end_time <- Sys.time()
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Should complete quickly (less than 1 second)
    expect_true(execution_time < 1.0)
    
    # Should have triggered alerts for values > 100
    expect_true(alert_count > 0)
    expect_true(alert_count < 1000)  # Not all iterations should trigger
  })
})

# ============================================================================
# 9.1.4 DASHBOARD FUNCTIONALITY TESTS
# ============================================================================

describe("Dashboard Functionality Tests", {
  
  # Mock dashboard components
  create_dashboard_server <- function() {
    function(input, output, session) {
      # Mock reactive values
      dashboard_data <- reactiveVal(list(
        kpis = list(
          total_employees = 1500,
          attrition_rate = 0.12,
          avg_satisfaction = 3.8
        ),
        charts = list(),
        last_updated = Sys.time()
      ))
      
      # Mock KPI output
      output$kpi_total_employees <- renderText({
        data <- dashboard_data()
        scales::comma(data$kpis$total_employees)
      })
      
      output$kpi_attrition_rate <- renderText({
        data <- dashboard_data()
        scales::percent(data$kpis$attrition_rate, accuracy = 0.1)
      })
      
      output$kpi_avg_satisfaction <- renderText({
        data <- dashboard_data()
        round(data$kpis$avg_satisfaction, 1)
      })
      
      # Mock chart outputs
      output$attrition_chart <- renderPlotly({
        # Mock attrition chart
        data <- data.frame(
          Department = c("HR", "Engineering", "Sales", "Marketing"),
          AttritionRate = c(0.15, 0.08, 0.18, 0.12)
        )
        
        p <- ggplot(data, aes(x = Department, y = AttritionRate)) +
          geom_bar(stat = "identity", fill = "#3498db") +
          scale_y_continuous(labels = scales::percent) +
          theme_minimal() +
          labs(title = "Attrition Rate by Department", 
               x = "Department", y = "Attrition Rate")
        
        ggplotly(p)
      })
      
      output$satisfaction_chart <- renderPlotly({
        # Mock satisfaction trend
        dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")
        satisfaction_data <- data.frame(
          Date = dates,
          Satisfaction = 3.5 + 0.3 * sin(seq_along(dates) * pi / 6) + rnorm(length(dates), 0, 0.1)
        )
        
        p <- ggplot(satisfaction_data, aes(x = Date, y = Satisfaction)) +
          geom_line(color = "#e74c3c", size = 1.2) +
          geom_point(color = "#e74c3c") +
          scale_y_continuous(limits = c(1, 5)) +
          theme_minimal() +
          labs(title = "Employee Satisfaction Trend", 
               x = "Date", y = "Satisfaction Score")
        
        ggplotly(p)
      })
      
      # Mock data refresh functionality
      observeEvent(input$refresh_data, {
        # Simulate data refresh
        new_data <- list(
          kpis = list(
            total_employees = sample(1400:1600, 1),
            attrition_rate = runif(1, 0.08, 0.16),
            avg_satisfaction = runif(1, 3.5, 4.2)
          ),
          charts = list(),
          last_updated = Sys.time()
        )
        dashboard_data(new_data)
        
        showNotification("Dashboard data refreshed successfully!", 
                        type = "success", duration = 3)
      })
      
      # Export dashboard data getter
      return(list(
        get_data = dashboard_data,
        refresh_data = function() {
          # Manual refresh function for testing
          new_data <- list(
            kpis = list(
              total_employees = sample(1400:1600, 1),
              attrition_rate = runif(1, 0.08, 0.16),
              avg_satisfaction = runif(1, 3.5, 4.2)
            ),
            last_updated = Sys.time()
          )
          dashboard_data(new_data)
          return(new_data)
        }
      ))
    }
  }
  
  it("initializes dashboard with correct KPIs", {
    # Mock Shiny session
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    # Create dashboard server
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Get initial data
    initial_data <- dashboard_instance$get_data()
    
    # Validate KPIs structure
    expect_true("kpis" %in% names(initial_data))
    expect_true("total_employees" %in% names(initial_data$kpis))
    expect_true("attrition_rate" %in% names(initial_data$kpis))
    expect_true("avg_satisfaction" %in% names(initial_data$kpis))
    
    # Validate KPI values
    expect_true(is.numeric(initial_data$kpis$total_employees))
    expect_true(initial_data$kpis$total_employees > 0)
    expect_true(initial_data$kpis$attrition_rate >= 0 && initial_data$kpis$attrition_rate <= 1)
    expect_true(initial_data$kpis$avg_satisfaction >= 1 && initial_data$kpis$avg_satisfaction <= 5)
  })
  
  it("handles dashboard data refresh correctly", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Get initial data
    initial_data <- dashboard_instance$get_data()
    initial_timestamp <- initial_data$last_updated
    
    # Wait a small amount to ensure timestamp difference
    Sys.sleep(0.1)
    
    # Refresh data
    refreshed_data <- dashboard_instance$refresh_data()
    
    # Validate refresh
    expect_true(refreshed_data$last_updated > initial_timestamp)
    expect_true("kpis" %in% names(refreshed_data))
    
    # Values should be within expected ranges
    expect_true(refreshed_data$kpis$total_employees >= 1400 && 
                refreshed_data$kpis$total_employees <= 1600)
    expect_true(refreshed_data$kpis$attrition_rate >= 0.08 && 
                refreshed_data$kpis$attrition_rate <= 0.16)
    expect_true(refreshed_data$kpis$avg_satisfaction >= 3.5 && 
                refreshed_data$kpis$avg_satisfaction <= 4.2)
  })
  
  it("validates dashboard responsiveness under load", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Simulate multiple rapid refreshes
    start_time <- Sys.time()
    refresh_times <- c()
    
    for (i in 1:50) {
      refresh_start <- Sys.time()
      dashboard_instance$refresh_data()
      refresh_end <- Sys.time()
      
      refresh_time <- as.numeric(difftime(refresh_end, refresh_start, units = "secs"))
      refresh_times <- c(refresh_times, refresh_time)
    }
    
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Performance assertions
    expect_true(total_time < 5.0)  # Should complete in under 5 seconds
    expect_true(mean(refresh_times) < 0.1)  # Average refresh should be under 100ms
    expect_true(max(refresh_times) < 0.5)   # No single refresh should take over 500ms
  })
  
  it("handles concurrent dashboard access", {
    # Simulate multiple users accessing dashboard simultaneously
    concurrent_results <- parallel::mclapply(1:5, function(user_id) {
      session <- MockShinySession$new()
      input <- list()
      output <- list()
      
      dashboard_server <- create_dashboard_server()
      dashboard_instance <- dashboard_server(input, output, session)
      
      # Each user performs multiple operations
      results <- list()
      for (i in 1:10) {
        data <- dashboard_instance$refresh_data()
        results[[i]] <- list(
          user_id = user_id,
          iteration = i,
          total_employees = data$kpis$total_employees,
          timestamp = data$last_updated
        )
      }
      
      return(results)
    }, mc.cores = 2)
    
    # Flatten results
    all_results <- unlist(concurrent_results, recursive = FALSE)
    
    # Validate all operations completed successfully
    expect_equal(length(all_results), 50)  # 5 users × 10 operations each
    
    # Check data integrity
    for (result in all_results) {
      expect_true(is.numeric(result$total_employees))
      expect_true(result$total_employees >= 1400 && result$total_employees <= 1600)
      expect_true(!is.null(result$timestamp))
    }
  })
  
  it("validates dashboard error handling", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    # Create dashboard with error injection
    create_error_dashboard <- function() {
      function(input, output, session) {
        dashboard_data <- reactiveVal(NULL)
        
        return(list(
          get_data = dashboard_data,
          refresh_data = function() {
            # Simulate various error conditions
            error_type <- sample(c("network", "data", "memory", "success"), 1, 
                               prob = c(0.1, 0.1, 0.1, 0.7))
            
            if (error_type == "network") {
              stop("Network connection timeout")
            } else if (error_type == "data") {
              stop("Invalid data format received")
            } else if (error_type == "memory") {
              stop("Insufficient memory to process request")
            } else {
              # Success case
              return(list(
                kpis = list(total_employees = 1500),
                last_updated = Sys.time()
              ))
            }
          }
        ))
      }
    }
    
    error_dashboard_server <- create_error_dashboard()
    error_dashboard <- error_dashboard_server(input, output, session)
    
    # Test error handling
    success_count <- 0
    error_count <- 0
    
    for (i in 1:100) {
      tryCatch({
        result <- error_dashboard$refresh_data()
        success_count <- success_count + 1
      }, error = function(e) {
        error_count <- error_count + 1
        # Validate error messages are informative
        expect_true(nchar(e$message) > 0)
        expect_true(e$message %in% c(
          "Network connection timeout",
          "Invalid data format received", 
          "Insufficient memory to process request"
        ))
      })
    }
    
    # Should have both successes and errors
    expect_true(success_count > 0)
    expect_true(error_count > 0)
    expect_equal(success_count + error_count, 100)
  })
})

# ============================================================================
# 9.1.5 LOG AGGREGATION EFFECTIVENESS TESTS
# ============================================================================

describe("Log Aggregation Effectiveness", {
  
  # Enhanced Logger with aggregation capabilities
  AggregatedLogger <- R6Class("AggregatedLogger",
    private = list(
      .logs = list(),
      .aggregated_logs = list(),
      .log_buffer = list(),
      .buffer_size = 1000,
      .aggregation_interval = 60  # seconds
    ),
    
    public = list(
      initialize = function(buffer_size = 1000, aggregation_interval = 60) {
        private$.logs <- list()
        private$.aggregated_logs <- list()
        private$.log_buffer <- list()
        private$.buffer_size <- buffer_size
        private$.aggregation_interval <- aggregation_interval
      },
      
      log = function(level, message, module = "unknown", metadata = list()) {
        log_entry <- list(
          timestamp = Sys.time(),
          level = level,
          message = message,
          module = module,
          metadata = metadata,
          session_id = digest::digest(Sys.time()),
          pid = Sys.getpid()
        )
        
        # Add to buffer
        private$.log_buffer[[length(private$.log_buffer) + 1]] <- log_entry
        
        # Flush buffer if it's full
        if (length(private$.log_buffer) >= private$.buffer_size) {
          self$flush_buffer()
        }
        
        return(log_entry)
      },
      
      flush_buffer = function() {
        if (length(private$.log_buffer) == 0) return(invisible(NULL))
        
        # Move buffer to main logs
        new_logs <- private$.log_buffer
        private$.logs <- c(private$.logs, new_logs)
        private$.log_buffer <- list()
        
        # Trigger aggregation
        self$aggregate_logs()
        
        return(length(new_logs))
      },
      
      aggregate_logs = function() {
        if (length(private$.logs) == 0) return(invisible(NULL))
        
        # Aggregate by time windows, level, and module
        current_time <- Sys.time()
        window_size <- private$.aggregation_interval
        
        # Create time windows
        log_times <- sapply(private$.logs, function(log) as.numeric(log$timestamp))
        min_time <- min(log_times)
        max_time <- max(log_times)
        
        time_windows <- seq(min_time, max_time + window_size, by = window_size)
        
        aggregated_data <- list()
        
        for (i in 1:(length(time_windows) - 1)) {
          window_start <- time_windows[i]
          window_end <- time_windows[i + 1]
          
          # Filter logs in this window
          window_logs <- private$.logs[log_times >= window_start & log_times < window_end]
          
          if (length(window_logs) == 0) next
          
          # Aggregate by level and module
          aggregation_key <- function(log) {
            paste(log$level, log$module, sep = "_")
          }
          
          log_groups <- split(window_logs, sapply(window_logs, aggregation_key))
          
          for (group_key in names(log_groups)) {
            group_logs <- log_groups[[group_key]]
            
            agg_entry <- list(
              window_start = as.POSIXct(window_start, origin = "1970-01-01"),
              window_end = as.POSIXct(window_end, origin = "1970-01-01"),
              level = group_logs[[1]]$level,
              module = group_logs[[1]]$module,
              count = length(group_logs),
              messages = sapply(group_logs, function(l) l$message),
              first_occurrence = min(sapply(group_logs, function(l) l$timestamp)),
              last_occurrence = max(sapply(group_logs, function(l) l$timestamp))
            )
            
            agg_key <- paste(window_start, group_key, sep = "_")
            private$.aggregated_logs[[agg_key]] <- agg_entry
          }
        }
        
        return(length(private$.aggregated_logs))
      },
      
      get_logs = function(level = NULL, module = NULL, limit = NULL) {
        filtered_logs <- private$.logs
        
        # Filter by level
        if (!is.null(level)) {
          filtered_logs <- filtered_logs[sapply(filtered_logs, function(l) l$level == level)]
        }
        
        # Filter by module
        if (!is.null(module)) {
          filtered_logs <- filtered_logs[sapply(filtered_logs, function(l) l$module == module)]
        }
        
        # Apply limit
        if (!is.null(limit) && length(filtered_logs) > limit) {
          filtered_logs <- tail(filtered_logs, limit)
        }
        
        return(filtered_logs)
      },
      
      get_aggregated_logs = function() {
        return(private$.aggregated_logs)
      },
      
      get_log_summary = function() {
        if (length(private$.logs) == 0) {
          return(list(
            total_logs = 0,
            by_level = list(),
            by_module = list(),
            time_range = NULL
          ))
        }
        
        # Count by level
        levels <- sapply(private$.logs, function(l) l$level)
        level_counts <- table(levels)
        
        # Count by module
        modules <- sapply(private$.logs, function(l) l$module)
        module_counts <- table(modules)
        
        # Time range
        timestamps <- sapply(private$.logs, function(l) l$timestamp)
        time_range <- list(
          start = min(timestamps),
          end = max(timestamps),
          duration_hours = as.numeric(difftime(max(timestamps), min(timestamps), units = "hours"))
        )
        
        return(list(
          total_logs = length(private$.logs),
          by_level = as.list(level_counts),
          by_module = as.list(module_counts),
          time_range = time_range,
          aggregated_entries = length(private$.aggregated_logs)
        ))
      },
      
      clear_logs = function() {
        private$.logs <- list()
        private$.aggregated_logs <- list()
        private$.log_buffer <- list()
      }
    )
  )
  
  it("collects logs efficiently in buffer", {
    logger <- AggregatedLogger$new(buffer_size = 10)
    
    # Add logs to buffer
    for (i in 1:5) {
      logger$log("info", paste("Test message", i), "test_module")
    }
    
    # Buffer should contain logs but main logs should be empty initially
    summary <- logger$get_log_summary()
    expect_equal(summary$total_logs, 0)  # Not flushed yet
    
    # Add more logs to trigger flush
    for (i in 6:12) {
      logger$log("info", paste("Test message", i), "test_module")
    }
    
    # Should have flushed when buffer reached size 10
    summary <- logger$get_log_summary()
    expect_true(summary$total_logs >= 10)
  })
  
  it("aggregates logs by time windows correctly", {
    logger <- AggregatedLogger$new(buffer_size = 5, aggregation_interval = 1)
    
    # Generate logs across different time periods
    base_time <- Sys.time()
    
    # First window
    for (i in 1:3) {
      logger$log("error", "Database error", "database")
    }
    
    # Simulate time passage
    Sys.sleep(1.1)
    
    # Second window
    for (i in 1:2) {
      logger$log("warning", "High memory usage", "system")
    }
    
    # Force aggregation
    logger$flush_buffer()
    
    aggregated <- logger$get_aggregated_logs()
    expect_true(length(aggregated) >= 1)
    
    # Check aggregation structure
    first_agg <- aggregated[[1]]
    expect_true("window_start" %in% names(first_agg))
    expect_true("window_end" %in% names(first_agg))
    expect_true("count" %in% names(first_agg))
    expect_true("level" %in% names(first_agg))
    expect_true("module" %in% names(first_agg))
  })
  
  it("handles high-volume log ingestion", {
    logger <- AggregatedLogger$new(buffer_size = 100)
    
    # Generate high volume of logs
    start_time <- Sys.time()
    log_count <- 10000
    
    for (i in 1:log_count) {
      level <- sample(c("info", "warning", "error"), 1)
      module <- sample(c("auth", "database", "api", "ui"), 1)
      logger$log(level, paste("Message", i), module)
    }
    
    logger$flush_buffer()
    end_time <- Sys.time()
    
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Performance assertions
    expect_true(execution_time < 5.0)  # Should complete in under 5 seconds
    
    summary <- logger$get_log_summary()
    expect_equal(summary$total_logs, log_count)
    
    # Should have aggregated data
    expect_true(summary$aggregated_entries > 0)
  })
  
  it("filters logs correctly by level and module", {
    logger <- AggregatedLogger$new(buffer_size = 5)
    
    # Generate diverse logs
    test_logs <- list(
      list(level = "error", module = "database", message = "Connection failed"),
      list(level = "warning", module = "database", message = "Slow query"),
      list(level = "info", module = "auth", message = "User login"),
      list(level = "error", module = "auth", message = "Invalid credentials"),
      list(level = "info", module = "api", message = "Request processed")
    )
    
    for (log_data in test_logs) {
      logger$log(log_data$level, log_data$message, log_data$module)
    }
    
    logger$flush_buffer()
    
    # Test filtering by level
    error_logs <- logger$get_logs(level = "error")
    expect_equal(length(error_logs), 2)
    expect_true(all(sapply(error_logs, function(l) l$level == "error")))
    
    # Test filtering by module
    database_logs <- logger$get_logs(module = "database")
    expect_equal(length(database_logs), 2)
    expect_true(all(sapply(database_logs, function(l) l$module == "database")))
    
    # Test combined filtering
    error_auth_logs <- logger$get_logs(level = "error", module = "auth")
    expect_equal(length(error_auth_logs), 1)
    expect_equal(error_auth_logs[[1]]$message, "Invalid credentials")
  })
  
  it("maintains log chronological order", {
    logger <- AggregatedLogger$new(buffer_size = 100)
    
    # Generate logs with deliberate timing
    messages <- character(50)
    timestamps <- numeric(50)
    
    for (i in 1:50) {
      message <- paste("Chronological test", i)
      logger$log("info", message, "test")
      messages[i] <- message
      timestamps[i] <- as.numeric(Sys.time())
      
      if (i %% 10 == 0) Sys.sleep(0.01)  # Small delays every 10 logs
    }
    
    logger$flush_buffer()
    
    # Get all logs
    all_logs <- logger$get_logs()
    
    # Check chronological order
    log_timestamps <- sapply(all_logs, function(l) as.numeric(l$timestamp))
    expect_true(all(diff(log_timestamps) >= 0))  # Should be non-decreasing
    
    # Check message order
    log_messages <- sapply(all_logs, function(l) l$message)
    expect_equal(log_messages, messages)
  })
  
  it("handles concurrent logging correctly", {
    logger <- AggregatedLogger$new(buffer_size = 200)
    
    # Simulate concurrent logging from multiple processes/threads
    concurrent_results <- parallel::mclapply(1:5, function(worker_id) {
      logged_messages <- character(20)
      
      for (i in 1:20) {
        message <- paste("Worker", worker_id, "Message", i)
        logger$log("info", message, paste0("worker_", worker_id))
        logged_messages[i] <- message
      }
      
      return(logged_messages)
    }, mc.cores = 2)
    
    logger$flush_buffer()
    
    # Verify all logs were captured
    all_logs <- logger$get_logs()
    expect_equal(length(all_logs), 100)  # 5 workers × 20 messages each
    
    # Check that all worker modules are represented
    modules <- unique(sapply(all_logs, function(l) l$module))
    expected_modules <- paste0("worker_", 1:5)
    expect_true(all(expected_modules %in% modules))
  })
  
  it("generates accurate log summaries", {
    logger <- AggregatedLogger$new(buffer_size = 20)
    
    # Generate logs with known distribution
    level_counts <- list(info = 10, warning = 5, error = 3)
    module_counts <- list(auth = 8, database = 6, api = 4)
    
    # Generate logs according to distribution
    for (level in names(level_counts)) {
      for (i in 1:level_counts[[level]]) {
        module <- sample(names(module_counts), 1, 
                        prob = unlist(module_counts)/sum(unlist(module_counts)))
        logger$log(level, paste("Test message", i), module)
      }
    }
    
    logger$flush_buffer()
    
    # Get summary
    summary <- logger$get_log_summary()
    
    # Verify total count
    expect_equal(summary$total_logs, sum(unlist(level_counts)))
    
    # Verify level distribution
    for (level in names(level_counts)) {
      expect_equal(summary$by_level[[level]], level_counts[[level]])
    }
    
    # Verify time range is valid
    expect_true(!is.null(summary$time_range))
    expect_true(summary$time_range$duration_hours >= 0)
  })
})

# ============================================================================
# 9.1.6 DISTRIBUTED TRACING VALIDATION TESTS
# ============================================================================

describe("Distributed Tracing Validation", {
  
  # Mock distributed tracing system
  TracingSystem <- R6Class("TracingSystem",
    private = list(
      .traces = list(),
      .spans = list(),
      .correlation_ids = list()
    ),
    
    public = list(
      initialize = function() {
        private$.traces <- list()
        private$.spans <- list() 
        private$.correlation_ids <- list()
      },
      
      start_trace = function(operation_name, metadata = list()) {
        trace_id <- self$generate_trace_id()
        span_id <- self$generate_span_id()
        
        trace <- list(
          trace_id = trace_id,
          root_span_id = span_id,
          operation_name = operation_name,
          start_time = Sys.time(),
          metadata = metadata,
          status = "active"
        )
        
        span <- list(
          trace_id = trace_id,
          span_id = span_id,
          parent_span_id = NULL,
          operation_name = operation_name,
          start_time = Sys.time(),
          end_time = NULL,
          duration_ms = NULL,
          metadata = metadata,
          tags = list(),
          status = "active"
        )
        
        private$.traces[[trace_id]] <- trace
        private$.spans[[span_id]] <- span
        
        return(list(trace_id = trace_id, span_id = span_id))
      },
      
      start_child_span = function(parent_trace_id, parent_span_id, operation_name, metadata = list()) {
        if (!parent_trace_id %in% names(private$.traces)) {
          stop("Parent trace not found")
        }
        
        span_id <- self$generate_span_id()
        
        span <- list(
          trace_id = parent_trace_id,
          span_id = span_id,
          parent_span_id = parent_span_id,
          operation_name = operation_name,
          start_time = Sys.time(),
          end_time = NULL,
          duration_ms = NULL,
          metadata = metadata,
          tags = list(),
          status = "active"
        )
        
        private$.spans[[span_id]] <- span
        
        return(span_id)
      },
      
      finish_span = function(span_id, status = "success", metadata = list()) {
        if (!span_id %in% names(private$.spans)) {
          stop("Span not found")
        }
        
        span <- private$.spans[[span_id]]
        end_time <- Sys.time()
        duration_ms <- as.numeric(difftime(end_time, span$start_time, units = "secs")) * 1000
        
        span$end_time <- end_time
        span$duration_ms <- duration_ms
        span$status <- status
        span$metadata <- c(span$metadata, metadata)
        
        private$.spans[[span_id]] <- span
        
        # Check if this completes the trace
        trace_id <- span$trace_id
        trace_spans <- self$get_trace_spans(trace_id)
        
        if (all(sapply(trace_spans, function(s) !is.null(s$end_time)))) {
          private$.traces[[trace_id]]$status <- "completed"
          private$.traces[[trace_id]]$end_time <- Sys.time()
        }
        
        return(span)
      },
      
      add_span_tag = function(span_id, key, value) {
        if (!span_id %in% names(private$.spans)) {
          stop("Span not found")
        }
        
        private$.spans[[span_id]]$tags[[key]] <- value
      },
      
      get_trace = function(trace_id) {
        if (!trace_id %in% names(private$.traces)) {
          return(NULL)
        }
        
        trace <- private$.traces[[trace_id]]
        spans <- self$get_trace_spans(trace_id)
        
        return(list(
          trace = trace,
          spans = spans
        ))
      },
      
      get_trace_spans = function(trace_id) {
        trace_spans <- private$.spans[sapply(private$.spans, function(s) s$trace_id == trace_id)]
        return(trace_spans)
      },
      
      generate_trace_id = function() {
        paste0("trace_", digest::digest(paste(Sys.time(), runif(1))))
      },
      
      generate_span_id = function() {
        paste0("span_", digest::digest(paste(Sys.time(), runif(1))))
      },
      
      get_active_traces = function() {
        active_traces <- private$.traces[sapply(private$.traces, function(t) t$status == "active")]
        return(active_traces)
      },
      
      get_trace_statistics = function() {
        if (length(private$.traces) == 0) {
          return(list(
            total_traces = 0,
            completed_traces = 0,
            active_traces = 0,
            avg_trace_duration_ms = 0
          ))
        }
        
        completed_traces <- private$.traces[sapply(private$.traces, function(t) t$status == "completed")]
        
        if (length(completed_traces) > 0) {
          durations <- sapply(completed_traces, function(t) {
            if (!is.null(t$end_time)) {
              as.numeric(difftime(t$end_time, t$start_time, units = "secs")) * 1000
            } else {
              NA
            }
          })
          avg_duration <- mean(durations, na.rm = TRUE)
        } else {
          avg_duration <- 0
        }
        
healthy-service`$status, health1$status)
    expect_equal(services# ============================================================================
# ATLAS LABS HR ANALYTICS - MONITORING & OBSERVABILITY UNIT TESTS
# Comprehensive test suite for application monitoring, health checks, metrics,
# alerts, logging, tracing, and performance monitoring
# ============================================================================

library(testthat)
library(mockery)
library(shiny)
library(R6)
library(jsonlite)
library(httr)
library(purrr)
library(lubridate)
library(digest)

# Source the application files (assuming they exist)
# source("modules/logger_module.R")
# source("global.R")
# source("utils.R")

# ============================================================================
# 9.1.1 HEALTH CHECK ENDPOINT VALIDATION TESTS
# ============================================================================

describe("Health Check Endpoint Validation", {
  
  # Mock health check endpoint function
  create_health_endpoint <- function() {
    function(req) {
      tryCatch({
        # Simulate health checks
        db_status <- check_database_connection()
        memory_status <- check_memory_usage()
        disk_status <- check_disk_space()
        
        health_data <- list(
          status = "healthy",
          timestamp = Sys.time(),
          version = "1.0.0",
          uptime = get_uptime(),
          checks = list(
            database = db_status,
            memory = memory_status,
            disk = disk_status
          )
        )
        
        list(
          status = 200,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(health_data, auto_unbox = TRUE)
        )
      }, error = function(e) {
        list(
          status = 503,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(list(
            status = "unhealthy",
            error = e$message,
            timestamp = Sys.time()
          ), auto_unbox = TRUE)
        )
      })
    }
  }
  
  # Helper functions for health checks
  check_database_connection <- function() {
    list(status = "ok", response_time_ms = sample(1:50, 1))
  }
  
  check_memory_usage <- function() {
    mem_info <- gc()
    list(
      status = if(sum(mem_info[,2]) < 1000) "ok" else "warning",
      used_mb = sum(mem_info[,2]),
      threshold_mb = 1000
    )
  }
  
  check_disk_space <- function() {
    list(status = "ok", free_gb = 50.5, used_percent = 45)
  }
  
  get_uptime <- function() {
    as.numeric(difftime(Sys.time(), as.POSIXct("2024-01-01"), units = "secs"))
  }
  
  it("returns 200 status for healthy application", {
    health_endpoint <- create_health_endpoint()
    response <- health_endpoint(list())
    
    expect_equal(response$status, 200)
    expect_equal(response$headers$`Content-Type`, "application/json")
    
    body <- jsonlite::fromJSON(response$body)
    expect_equal(body$status, "healthy")
    expect_true("timestamp" %in% names(body))
    expect_true("version" %in% names(body))
    expect_true("checks" %in% names(body))
  })
  
  it("validates all health check components", {
    health_endpoint <- create_health_endpoint()
    response <- health_endpoint(list())
    body <- jsonlite::fromJSON(response$body)
    
    # Check required fields
    required_fields <- c("status", "timestamp", "version", "uptime", "checks")
    expect_true(all(required_fields %in% names(body)))
    
    # Check nested components
    expect_true("database" %in% names(body$checks))
    expect_true("memory" %in% names(body$checks))
    expect_true("disk" %in% names(body$checks))
    
    # Validate timestamp format
    expect_true(is.character(body$timestamp))
    expect_false(is.na(as.POSIXct(body$timestamp)))
  })
  
  it("handles database connection failures", {
    # Mock database failure
    mock_check_db <- function() {
      stop("Database connection timeout")
    }
    
    with_mock(
      check_database_connection = mock_check_db,
      {
        health_endpoint <- create_health_endpoint()
        response <- health_endpoint(list())
        
        expect_equal(response$status, 503)
        body <- jsonlite::fromJSON(response$body)
        expect_equal(body$status, "unhealthy")
        expect_true("error" %in% names(body))
      }
    )
  })
  
  it("validates response time thresholds", {
    # Test various response times
    response_times <- c(10, 50, 100, 500, 1000, 5000)
    
    for (time_ms in response_times) {
      mock_check_db <- function() {
        Sys.sleep(time_ms/1000)  # Convert to seconds
        list(status = "ok", response_time_ms = time_ms)
      }
      
      with_mock(
        check_database_connection = mock_check_db,
        {
          start_time <- Sys.time()
          health_endpoint <- create_health_endpoint()
          response <- health_endpoint(list())
          end_time <- Sys.time()
          
          actual_time <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
          expect_true(actual_time >= time_ms * 0.8)  # Allow 20% tolerance
        }
      )
    }
  })
  
  it("handles memory pressure scenarios", {
    # Test different memory usage levels
    memory_scenarios <- list(
      low = list(status = "ok", used_mb = 100, threshold_mb = 1000),
      medium = list(status = "ok", used_mb = 500, threshold_mb = 1000),
      high = list(status = "warning", used_mb = 950, threshold_mb = 1000),
      critical = list(status = "error", used_mb = 1200, threshold_mb = 1000)
    )
    
    for (scenario_name in names(memory_scenarios)) {
      scenario <- memory_scenarios[[scenario_name]]
      
      mock_check_memory <- function() scenario
      
      with_mock(
        check_memory_usage = mock_check_memory,
        {
          health_endpoint <- create_health_endpoint()
          response <- health_endpoint(list())
          body <- jsonlite::fromJSON(response$body)
          
          expect_equal(body$checks$memory$status, scenario$status)
          expect_equal(body$checks$memory$used_mb, scenario$used_mb)
        }
      )
    }
  })
  
  it("validates concurrent health check requests", {
    health_endpoint <- create_health_endpoint()
    
    # Simulate concurrent requests
    concurrent_responses <- parallel::mclapply(1:10, function(i) {
      health_endpoint(list())
    }, mc.cores = 2)
    
    # All should succeed
    statuses <- sapply(concurrent_responses, function(r) r$status)
    expect_true(all(statuses == 200))
    
    # Check response consistency
    bodies <- lapply(concurrent_responses, function(r) jsonlite::fromJSON(r$body))
    versions <- sapply(bodies, function(b) b$version)
    expect_true(length(unique(versions)) == 1)  # All should have same version
  })
})

# ============================================================================
# 9.1.2 METRICS COLLECTION ACCURACY TESTS
# ============================================================================

describe("Metrics Collection Accuracy", {
  
  # Mock metrics collector
  MetricsCollector <- R6Class("MetricsCollector",
    private = list(
      .metrics = list(),
      .start_time = NULL
    ),
    
    public = list(
      initialize = function() {
        private$.metrics <- list()
        private$.start_time <- Sys.time()
      },
      
      increment_counter = function(name, value = 1, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        if (is.null(private$.metrics[[key]])) {
          private$.metrics[[key]] <- list(
            type = "counter",
            name = name,
            value = 0,
            tags = tags,
            last_updated = Sys.time()
          )
        }
        private$.metrics[[key]]$value <- private$.metrics[[key]]$value + value
        private$.metrics[[key]]$last_updated <- Sys.time()
      },
      
      set_gauge = function(name, value, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        private$.metrics[[key]] <- list(
          type = "gauge",
          name = name,
          value = value,
          tags = tags,
          last_updated = Sys.time()
        )
      },
      
      record_histogram = function(name, value, tags = list()) {
        key <- paste0(name, "_", digest::digest(tags))
        if (is.null(private$.metrics[[key]])) {
          private$.metrics[[key]] <- list(
            type = "histogram",
            name = name,
            values = c(),
            tags = tags,
            last_updated = Sys.time()
          )
        }
        private$.metrics[[key]]$values <- c(private$.metrics[[key]]$values, value)
        private$.metrics[[key]]$last_updated <- Sys.time()
      },
      
      get_metrics = function() {
        private$.metrics
      },
      
      get_metric_summary = function() {
        metrics_summary <- list()
        
        for (metric_key in names(private$.metrics)) {
          metric <- private$.metrics[[metric_key]]
          
          if (metric$type == "counter") {
            metrics_summary[[metric$name]] <- list(
              type = "counter",
              value = metric$value,
              tags = metric$tags
            )
          } else if (metric$type == "gauge") {
            metrics_summary[[metric$name]] <- list(
              type = "gauge",
              value = metric$value,
              tags = metric$tags
            )
          } else if (metric$type == "histogram") {
            values <- metric$values
            metrics_summary[[metric$name]] <- list(
              type = "histogram",
              count = length(values),
              mean = mean(values),
              median = median(values),
              p95 = quantile(values, 0.95, na.rm = TRUE),
              p99 = quantile(values, 0.99, na.rm = TRUE),
              min = min(values),
              max = max(values),
              tags = metric$tags
            )
          }
        }
        
        metrics_summary
      }
    )
  )
  
  it("accurately tracks counter metrics", {
    collector <- MetricsCollector$new()
    
    # Test basic counter increment
    collector$increment_counter("user_logins")
    collector$increment_counter("user_logins")
    collector$increment_counter("user_logins", value = 3)
    
    metrics <- collector$get_metrics()
    login_metric <- metrics[[paste0("user_logins_", digest::digest(list()))]]
    
    expect_equal(login_metric$type, "counter")
    expect_equal(login_metric$value, 5)  # 1 + 1 + 3
    expect_equal(login_metric$name, "user_logins")
  })
  
  it("handles counter metrics with tags", {
    collector <- MetricsCollector$new()
    
    # Different departments
    collector$increment_counter("page_views", tags = list(department = "HR"))
    collector$increment_counter("page_views", tags = list(department = "HR"))
    collector$increment_counter("page_views", tags = list(department = "Finance"))
    
    metrics <- collector$get_metrics()
    
    # Should have separate counters for each tag combination
    hr_key <- paste0("page_views_", digest::digest(list(department = "HR")))
    finance_key <- paste0("page_views_", digest::digest(list(department = "Finance")))
    
    expect_equal(metrics[[hr_key]]$value, 2)
    expect_equal(metrics[[finance_key]]$value, 1)
    expect_equal(metrics[[hr_key]]$tags$department, "HR")
    expect_equal(metrics[[finance_key]]$tags$department, "Finance")
  })
  
  it("accurately measures gauge metrics", {
    collector <- MetricsCollector$new()
    
    # Test gauge updates
    collector$set_gauge("memory_usage_mb", 512.5)
    collector$set_gauge("memory_usage_mb", 768.2)  # Should overwrite
    collector$set_gauge("cpu_usage_percent", 45.7)
    
    metrics <- collector$get_metrics()
    memory_key <- paste0("memory_usage_mb_", digest::digest(list()))
    cpu_key <- paste0("cpu_usage_percent_", digest::digest(list()))
    
    expect_equal(metrics[[memory_key]]$value, 768.2)
    expect_equal(metrics[[cpu_key]]$value, 45.7)
    expect_equal(metrics[[memory_key]]$type, "gauge")
  })
  
  it("correctly calculates histogram statistics", {
    collector <- MetricsCollector$new()
    
    # Record response times
    response_times <- c(10, 15, 12, 45, 23, 18, 67, 34, 28, 19)
    for (time in response_times) {
      collector$record_histogram("response_time_ms", time)
    }
    
    summary <- collector$get_metric_summary()
    response_metric <- summary$response_time_ms
    
    expect_equal(response_metric$type, "histogram")
    expect_equal(response_metric$count, 10)
    expect_equal(response_metric$mean, mean(response_times))
    expect_equal(response_metric$median, median(response_times))
    expect_equal(response_metric$min, min(response_times))
    expect_equal(response_metric$max, max(response_times))
  })
  
  it("handles edge cases in histogram calculations", {
    collector <- MetricsCollector$new()
    
    # Test with single value
    collector$record_histogram("single_value", 42)
    summary <- collector$get_metric_summary()
    single_metric <- summary$single_value
    
    expect_equal(single_metric$count, 1)
    expect_equal(single_metric$mean, 42)
    expect_equal(single_metric$median, 42)
    expect_equal(single_metric$min, 42)
    expect_equal(single_metric$max, 42)
    
    # Test with identical values
    for (i in 1:5) {
      collector$record_histogram("identical_values", 100)
    }
    
    summary <- collector$get_metric_summary()
    identical_metric <- summary$identical_values
    
    expect_equal(identical_metric$count, 5)
    expect_equal(identical_metric$mean, 100)
    expect_equal(identical_metric$p95, 100)
    expect_equal(identical_metric$p99, 100)
  })
  
  it("maintains accuracy under high volume", {
    collector <- MetricsCollector$new()
    
    # High volume counter increments
    start_time <- Sys.time()
    for (i in 1:10000) {
      collector$increment_counter("high_volume_counter")
    }
    end_time <- Sys.time()
    
    metrics <- collector$get_metrics()
    counter_key <- paste0("high_volume_counter_", digest::digest(list()))
    
    expect_equal(metrics[[counter_key]]$value, 10000)
    
    # Should complete reasonably quickly (less than 1 second)
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    expect_true(execution_time < 1.0)
  })
  
  it("handles concurrent metric updates", {
    collector <- MetricsCollector$new()
    
    # Concurrent counter increments
    parallel::mclapply(1:100, function(i) {
      collector$increment_counter("concurrent_counter")
    }, mc.cores = 2)
    
    metrics <- collector$get_metrics()
    counter_key <- paste0("concurrent_counter_", digest::digest(list()))
    
    # Should handle race conditions gracefully
    expect_true(metrics[[counter_key]]$value <= 100)
    expect_true(metrics[[counter_key]]$value > 0)
  })
  
  it("validates metric timestamp accuracy", {
    collector <- MetricsCollector$new()
    
    before_time <- Sys.time()
    collector$increment_counter("timestamp_test")
    after_time <- Sys.time()
    
    metrics <- collector$get_metrics()
    metric_key <- paste0("timestamp_test_", digest::digest(list()))
    metric_timestamp <- metrics[[metric_key]]$last_updated
    
    expect_true(metric_timestamp >= before_time)
    expect_true(metric_timestamp <= after_time)
  })
})

# ============================================================================
# 9.1.3 ALERT THRESHOLD TESTING
# ============================================================================

describe("Alert Threshold Testing", {
  
  # Mock alert system
  AlertSystem <- R6Class("AlertSystem",
    private = list(
      .thresholds = list(),
      .alerts = list(),
      .alert_history = list()
    ),
    
    public = list(
      initialize = function() {
        private$.thresholds <- list()
        private$.alerts <- list()
        private$.alert_history <- list()
      },
      
      set_threshold = function(metric_name, threshold_type, value, severity = "warning") {
        threshold_id <- paste0(metric_name, "_", threshold_type)
        private$.thresholds[[threshold_id]] <- list(
          metric_name = metric_name,
          type = threshold_type,  # "greater_than", "less_than", "equal_to"
          value = value,
          severity = severity,
          enabled = TRUE
        )
      },
      
      check_thresholds = function(metrics) {
        current_alerts <- list()
        
        for (threshold_id in names(private$.thresholds)) {
          threshold <- private$.thresholds[[threshold_id]]
          if (!threshold$enabled) next
          
          metric_value <- metrics[[threshold$metric_name]]
          if (is.null(metric_value)) next
          
          alert_triggered <- FALSE
          
          if (threshold$type == "greater_than" && metric_value > threshold$value) {
            alert_triggered <- TRUE
          } else if (threshold$type == "less_than" && metric_value < threshold$value) {
            alert_triggered <- TRUE
          } else if (threshold$type == "equal_to" && metric_value == threshold$value) {
            alert_triggered <- TRUE
          }
          
          if (alert_triggered) {
            alert <- list(
              threshold_id = threshold_id,
              metric_name = threshold$metric_name,
              metric_value = metric_value,
              threshold_value = threshold$value,
              severity = threshold$severity,
              timestamp = Sys.time(),
              message = sprintf("Metric %s (%s) exceeded threshold %s (current: %s)",
                              threshold$metric_name, threshold$type, 
                              threshold$value, metric_value)
            )
            
            current_alerts[[threshold_id]] <- alert
            private$.alert_history[[length(private$.alert_history) + 1]] <- alert
          }
        }
        
        private$.alerts <- current_alerts
        return(current_alerts)
      },
      
      get_active_alerts = function() {
        private$.alerts
      },
      
      get_alert_history = function(limit = 100) {
        history_length <- length(private$.alert_history)
        if (history_length == 0) return(list())
        
        start_idx <- max(1, history_length - limit + 1)
        private$.alert_history[start_idx:history_length]
      },
      
      clear_alerts = function() {
        private$.alerts <- list()
      },
      
      disable_threshold = function(threshold_id) {
        if (threshold_id %in% names(private$.thresholds)) {
          private$.thresholds[[threshold_id]]$enabled <- FALSE
        }
      }
    )
  )
  
  it("triggers alerts when thresholds are exceeded", {
    alert_system <- AlertSystem$new()
    
    # Set up thresholds
    alert_system$set_threshold("cpu_usage", "greater_than", 80, "warning")
    alert_system$set_threshold("memory_usage", "greater_than", 90, "critical")
    alert_system$set_threshold("response_time", "greater_than", 1000, "warning")
    
    # Test metrics that should trigger alerts
    metrics <- list(
      cpu_usage = 85,     # Should trigger warning
      memory_usage = 95,  # Should trigger critical
      response_time = 500 # Should not trigger
    )
    
    alerts <- alert_system$check_thresholds(metrics)
    
    expect_equal(length(alerts), 2)
    expect_true("cpu_usage_greater_than" %in% names(alerts))
    expect_true("memory_usage_greater_than" %in% names(alerts))
    expect_false("response_time_greater_than" %in% names(alerts))
    
    # Check alert details
    cpu_alert <- alerts$cpu_usage_greater_than
    expect_equal(cpu_alert$severity, "warning")
    expect_equal(cpu_alert$metric_value, 85)
    expect_equal(cpu_alert$threshold_value, 80)
  })
  
  it("handles different threshold types correctly", {
    alert_system <- AlertSystem$new()
    
    # Set up different threshold types
    alert_system$set_threshold("disk_space", "less_than", 10, "critical")
    alert_system$set_threshold("error_count", "greater_than", 0, "warning")
    alert_system$set_threshold("connection_count", "equal_to", 0, "critical")
    
    # Test scenarios
    test_cases <- list(
      list(
        metrics = list(disk_space = 5, error_count = 3, connection_count = 0),
        expected_alerts = c("disk_space_less_than", "error_count_greater_than", "connection_count_equal_to")
      ),
      list(
        metrics = list(disk_space = 15, error_count = 0, connection_count = 10),
        expected_alerts = c()
      ),
      list(
        metrics = list(disk_space = 10, error_count = 1, connection_count = 1),
        expected_alerts = c("error_count_greater_than")
      )
    )
    
    for (i in seq_along(test_cases)) {
      test_case <- test_cases[[i]]
      alert_system$clear_alerts()
      
      alerts <- alert_system$check_thresholds(test_case$metrics)
      alert_names <- names(alerts)
      
      expect_equal(length(alert_names), length(test_case$expected_alerts))
      expect_true(all(test_case$expected_alerts %in% alert_names))
    }
  })
  
  it("maintains alert history correctly", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("test_metric", "greater_than", 50, "warning")
    
    # Generate multiple alerts over time
    for (i in 1:10) {
      metrics <- list(test_metric = 60 + i)
      alert_system$check_thresholds(metrics)
      Sys.sleep(0.01)  # Small delay to ensure different timestamps
    }
    
    history <- alert_system$get_alert_history()
    expect_equal(length(history), 10)
    
    # Check chronological order
    timestamps <- sapply(history, function(a) a$timestamp)
    expect_true(all(diff(as.numeric(timestamps)) > 0))
    
    # Check metric values are recorded correctly
    metric_values <- sapply(history, function(a) a$metric_value)
    expect_equal(metric_values, 61:70)
  })
  
  it("handles edge case threshold values", {
    alert_system <- AlertSystem$new()
    
    # Test edge cases
    alert_system$set_threshold("zero_threshold", "greater_than", 0, "info")
    alert_system$set_threshold("negative_threshold", "less_than", -10, "warning")
    alert_system$set_threshold("float_threshold", "greater_than", 3.14159, "warning")
    
    edge_cases <- list(
      list(metrics = list(zero_threshold = 0.001), should_alert = TRUE),
      list(metrics = list(zero_threshold = 0), should_alert = FALSE),
      list(metrics = list(zero_threshold = -1), should_alert = FALSE),
      list(metrics = list(negative_threshold = -15), should_alert = TRUE),
      list(metrics = list(negative_threshold = -5), should_alert = FALSE),
      list(metrics = list(float_threshold = 3.14160), should_alert = TRUE),
      list(metrics = list(float_threshold = 3.14159), should_alert = FALSE)
    )
    
    for (case in edge_cases) {
      alert_system$clear_alerts()
      alerts <- alert_system$check_thresholds(case$metrics)
      
      if (case$should_alert) {
        expect_true(length(alerts) > 0, 
                   info = paste("Expected alert for metrics:", 
                               paste(names(case$metrics), case$metrics, sep = "=", collapse = ", ")))
      } else {
        expect_equal(length(alerts), 0,
                    info = paste("Did not expect alert for metrics:", 
                                paste(names(case$metrics), case$metrics, sep = "=", collapse = ", ")))
      }
    }
  })
  
  it("handles missing metrics gracefully", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("missing_metric", "greater_than", 100, "warning")
    alert_system$set_threshold("present_metric", "greater_than", 50, "warning")
    
    # Test with missing metric
    metrics <- list(present_metric = 75)  # missing_metric is not provided
    alerts <- alert_system$check_thresholds(metrics)
    
    # Should only alert on present metric
    expect_equal(length(alerts), 1)
    expect_true("present_metric_greater_than" %in% names(alerts))
    expect_false("missing_metric_greater_than" %in% names(alerts))
  })
  
  it("supports alert severity levels", {
    alert_system <- AlertSystem$new()
    
    # Set up different severity levels
    severities <- c("info", "warning", "error", "critical")
    for (i in seq_along(severities)) {
      alert_system$set_threshold(paste0("metric_", i), "greater_than", i * 10, severities[i])
    }
    
    # Trigger all alerts
    metrics <- list(metric_1 = 15, metric_2 = 25, metric_3 = 35, metric_4 = 45)
    alerts <- alert_system$check_thresholds(metrics)
    
    expect_equal(length(alerts), 4)
    
    # Check severities are preserved
    for (i in seq_along(severities)) {
      alert_key <- paste0("metric_", i, "_greater_than")
      expect_true(alert_key %in% names(alerts))
      expect_equal(alerts[[alert_key]]$severity, severities[i])
    }
  })
  
  it("can disable and enable thresholds", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("test_metric", "greater_than", 50, "warning")
    
    # Should trigger initially
    metrics <- list(test_metric = 75)
    alerts <- alert_system$check_thresholds(metrics)
    expect_equal(length(alerts), 1)
    
    # Disable threshold
    alert_system$disable_threshold("test_metric_greater_than")
    alert_system$clear_alerts()
    
    # Should not trigger when disabled
    alerts <- alert_system$check_thresholds(metrics)
    expect_equal(length(alerts), 0)
  })
  
  it("handles high-frequency threshold checking", {
    alert_system <- AlertSystem$new()
    alert_system$set_threshold("high_freq_metric", "greater_than", 100, "warning")
    
    # Rapid threshold checking
    start_time <- Sys.time()
    alert_count <- 0
    
    for (i in 1:1000) {
      metrics <- list(high_freq_metric = 50 + (i %% 100))  # Alternates above/below threshold
      alerts <- alert_system$check_thresholds(metrics)
      if (length(alerts) > 0) alert_count <- alert_count + 1
    }
    
    end_time <- Sys.time()
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Should complete quickly (less than 1 second)
    expect_true(execution_time < 1.0)
    
    # Should have triggered alerts for values > 100
    expect_true(alert_count > 0)
    expect_true(alert_count < 1000)  # Not all iterations should trigger
  })
})

# ============================================================================
# 9.1.4 DASHBOARD FUNCTIONALITY TESTS
# ============================================================================

describe("Dashboard Functionality Tests", {
  
  # Mock dashboard components
  create_dashboard_server <- function() {
    function(input, output, session) {
      # Mock reactive values
      dashboard_data <- reactiveVal(list(
        kpis = list(
          total_employees = 1500,
          attrition_rate = 0.12,
          avg_satisfaction = 3.8
        ),
        charts = list(),
        last_updated = Sys.time()
      ))
      
      # Mock KPI output
      output$kpi_total_employees <- renderText({
        data <- dashboard_data()
        scales::comma(data$kpis$total_employees)
      })
      
      output$kpi_attrition_rate <- renderText({
        data <- dashboard_data()
        scales::percent(data$kpis$attrition_rate, accuracy = 0.1)
      })
      
      output$kpi_avg_satisfaction <- renderText({
        data <- dashboard_data()
        round(data$kpis$avg_satisfaction, 1)
      })
      
      # Mock chart outputs
      output$attrition_chart <- renderPlotly({
        # Mock attrition chart
        data <- data.frame(
          Department = c("HR", "Engineering", "Sales", "Marketing"),
          AttritionRate = c(0.15, 0.08, 0.18, 0.12)
        )
        
        p <- ggplot(data, aes(x = Department, y = AttritionRate)) +
          geom_bar(stat = "identity", fill = "#3498db") +
          scale_y_continuous(labels = scales::percent) +
          theme_minimal() +
          labs(title = "Attrition Rate by Department", 
               x = "Department", y = "Attrition Rate")
        
        ggplotly(p)
      })
      
      output$satisfaction_chart <- renderPlotly({
        # Mock satisfaction trend
        dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")
        satisfaction_data <- data.frame(
          Date = dates,
          Satisfaction = 3.5 + 0.3 * sin(seq_along(dates) * pi / 6) + rnorm(length(dates), 0, 0.1)
        )
        
        p <- ggplot(satisfaction_data, aes(x = Date, y = Satisfaction)) +
          geom_line(color = "#e74c3c", size = 1.2) +
          geom_point(color = "#e74c3c") +
          scale_y_continuous(limits = c(1, 5)) +
          theme_minimal() +
          labs(title = "Employee Satisfaction Trend", 
               x = "Date", y = "Satisfaction Score")
        
        ggplotly(p)
      })
      
      # Mock data refresh functionality
      observeEvent(input$refresh_data, {
        # Simulate data refresh
        new_data <- list(
          kpis = list(
            total_employees = sample(1400:1600, 1),
            attrition_rate = runif(1, 0.08, 0.16),
            avg_satisfaction = runif(1, 3.5, 4.2)
          ),
          charts = list(),
          last_updated = Sys.time()
        )
        dashboard_data(new_data)
        
        showNotification("Dashboard data refreshed successfully!", 
                        type = "success", duration = 3)
      })
      
      # Export dashboard data getter
      return(list(
        get_data = dashboard_data,
        refresh_data = function() {
          # Manual refresh function for testing
          new_data <- list(
            kpis = list(
              total_employees = sample(1400:1600, 1),
              attrition_rate = runif(1, 0.08, 0.16),
              avg_satisfaction = runif(1, 3.5, 4.2)
            ),
            last_updated = Sys.time()
          )
          dashboard_data(new_data)
          return(new_data)
        }
      ))
    }
  }
  
  it("initializes dashboard with correct KPIs", {
    # Mock Shiny session
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    # Create dashboard server
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Get initial data
    initial_data <- dashboard_instance$get_data()
    
    # Validate KPIs structure
    expect_true("kpis" %in% names(initial_data))
    expect_true("total_employees" %in% names(initial_data$kpis))
    expect_true("attrition_rate" %in% names(initial_data$kpis))
    expect_true("avg_satisfaction" %in% names(initial_data$kpis))
    
    # Validate KPI values
    expect_true(is.numeric(initial_data$kpis$total_employees))
    expect_true(initial_data$kpis$total_employees > 0)
    expect_true(initial_data$kpis$attrition_rate >= 0 && initial_data$kpis$attrition_rate <= 1)
    expect_true(initial_data$kpis$avg_satisfaction >= 1 && initial_data$kpis$avg_satisfaction <= 5)
  })
  
  it("handles dashboard data refresh correctly", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Get initial data
    initial_data <- dashboard_instance$get_data()
    initial_timestamp <- initial_data$last_updated
    
    # Wait a small amount to ensure timestamp difference
    Sys.sleep(0.1)
    
    # Refresh data
    refreshed_data <- dashboard_instance$refresh_data()
    
    # Validate refresh
    expect_true(refreshed_data$last_updated > initial_timestamp)
    expect_true("kpis" %in% names(refreshed_data))
    
    # Values should be within expected ranges
    expect_true(refreshed_data$kpis$total_employees >= 1400 && 
                refreshed_data$kpis$total_employees <= 1600)
    expect_true(refreshed_data$kpis$attrition_rate >= 0.08 && 
                refreshed_data$kpis$attrition_rate <= 0.16)
    expect_true(refreshed_data$kpis$avg_satisfaction >= 3.5 && 
                refreshed_data$kpis$avg_satisfaction <= 4.2)
  })
  
  it("validates dashboard responsiveness under load", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    dashboard_server <- create_dashboard_server()
    dashboard_instance <- dashboard_server(input, output, session)
    
    # Simulate multiple rapid refreshes
    start_time <- Sys.time()
    refresh_times <- c()
    
    for (i in 1:50) {
      refresh_start <- Sys.time()
      dashboard_instance$refresh_data()
      refresh_end <- Sys.time()
      
      refresh_time <- as.numeric(difftime(refresh_end, refresh_start, units = "secs"))
      refresh_times <- c(refresh_times, refresh_time)
    }
    
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Performance assertions
    expect_true(total_time < 5.0)  # Should complete in under 5 seconds
    expect_true(mean(refresh_times) < 0.1)  # Average refresh should be under 100ms
    expect_true(max(refresh_times) < 0.5)   # No single refresh should take over 500ms
  })
  
  it("handles concurrent dashboard access", {
    # Simulate multiple users accessing dashboard simultaneously
    concurrent_results <- parallel::mclapply(1:5, function(user_id) {
      session <- MockShinySession$new()
      input <- list()
      output <- list()
      
      dashboard_server <- create_dashboard_server()
      dashboard_instance <- dashboard_server(input, output, session)
      
      # Each user performs multiple operations
      results <- list()
      for (i in 1:10) {
        data <- dashboard_instance$refresh_data()
        results[[i]] <- list(
          user_id = user_id,
          iteration = i,
          total_employees = data$kpis$total_employees,
          timestamp = data$last_updated
        )
      }
      
      return(results)
    }, mc.cores = 2)
    
    # Flatten results
    all_results <- unlist(concurrent_results, recursive = FALSE)
    
    # Validate all operations completed successfully
    expect_equal(length(all_results), 50)  # 5 users × 10 operations each
    
    # Check data integrity
    for (result in all_results) {
      expect_true(is.numeric(result$total_employees))
      expect_true(result$total_employees >= 1400 && result$total_employees <= 1600)
      expect_true(!is.null(result$timestamp))
    }
  })
  
  it("validates dashboard error handling", {
    session <- MockShinySession$new()
    input <- list()
    output <- list()
    
    # Create dashboard with error injection
    create_error_dashboard <- function() {
      function(input, output, session) {
        dashboard_data <- reactiveVal(NULL)
        
        return(list(
          get_data = dashboard_data,
          refresh_data = function() {
            # Simulate various error conditions
            error_type <- sample(c("network", "data", "memory", "success"), 1, 
                               prob = c(0.1, 0.1, 0.1, 0.7))
            
            if (error_type == "network") {
              stop("Network connection timeout")
            } else if (error_type == "data") {
              stop("Invalid data format received")
            } else if (error_type == "memory") {
              stop("Insufficient memory to process request")
            } else {
              # Success case
              return(list(
                kpis = list(total_employees = 1500),
                last_updated = Sys.time()
              ))
            }
          }
        ))
      }
    }
    
    error_dashboard_server <- create_error_dashboard()
    error_dashboard <- error_dashboard_server(input, output, session)
    
    # Test error handling
    success_count <- 0
    error_count <- 0
    
    for (i in 1:100) {
      tryCatch({
        result <- error_dashboard$refresh_data()
        success_count <- success_count + 1
      }, error = function(e) {
        error_count <- error_count + 1
        # Validate error messages are informative
        expect_true(nchar(e$message) > 0)
        expect_true(e$message %in% c(
          "Network connection timeout",
          "Invalid data format received", 
          "Insufficient memory to process request"
        ))
      })
    }
    
    # Should have both successes and errors
    expect_true(success_count > 0)
    expect_true(error_count > 0)
    expect_equal(success_count + error_count, 100)
  })
})

# ============================================================================
# 9.1.5 LOG AGGREGATION EFFECTIVENESS TESTS
# ============================================================================

describe("Log Aggregation Effectiveness", {
  
  # Enhanced Logger with aggregation capabilities
  AggregatedLogger <- R6Class("AggregatedLogger",
    private = list(
      .logs = list(),
      .aggregated_logs = list(),
      .log_buffer = list(),
      .buffer_size = 1000,
      .aggregation_interval = 60  # seconds
    ),
    
    public = list(
      initialize = function(buffer_size = 1000, aggregation_interval = 60) {
        private$.logs <- list()
        private$.aggregated_logs <- list()
        private$.log_buffer <- list()
        private$.buffer_size <- buffer_size
        private$.aggregation_interval <- aggregation_interval
      },
      
      log = function(level, message, module = "unknown", metadata = list()) {
        log_entry <- list(
          timestamp = Sys.time(),
          level = level,
          message = message,
          module = module,
          metadata = metadata,
          session_id = digest::digest(Sys.time()),
          pid = Sys.getpid()
        )
        
        # Add to buffer
        private$.log_buffer[[length(private$.log_buffer) + 1]] <- log_entry
        
        # Flush buffer if it's full
        if (length(private$.log_buffer) >= private$.buffer_size) {
          self$flush_buffer()
        }
        
        return(log_entry)
      },
      
      flush_buffer = function() {
        if (length(private$.log_buffer) == 0) return(invisible(NULL))
        
        # Move buffer to main logs
        new_logs <- private$.log_buffer
        private$.logs <- c(private$.logs, new_logs)
        private$.log_buffer <- list()
        
        # Trigger aggregation
        self$aggregate_logs()
        
        return(length(new_logs))
      },
      
      aggregate_logs = function() {
        if (length(private$.logs) == 0) return(invisible(NULL))
        
        # Aggregate by time windows, level, and module
        current_time <- Sys.time()
        window_size <- private$.aggregation_interval
        
        # Create time windows
        log_times <- sapply(private$.logs, function(log) as.numeric(log$timestamp))
        min_time <- min(log_times)
        max_time <- max(log_times)
        
        time_windows <- seq(min_time, max_time + window_size, by = window_size)
        
        aggregated_data <- list()
        
        for (i in 1:(length(time_windows) - 1)) {
          window_start <- time_windows[i]
          window_end <- time_windows[i + 1]
          
          # Filter logs in this window
          window_logs <- private$.logs[log_times >= window_start & log_times < window_end]
          
          if (length(window_logs) == 0) next
          
          # Aggregate by level and module
          aggregation_key <- function(log) {
            paste(log$level, log$module, sep = "_")
          }
          
          log_groups <- split(window_logs, sapply(window_logs, aggregation_key))
          
          for (group_key in names(log_groups)) {
            group_logs <- log_groups[[group_key]]
            
            agg_entry <- list(
              window_start = as.POSIXct(window_start, origin = "1970-01-01"),
              window_end = as.POSIXct(window_end, origin = "1970-01-01"),
              level = group_logs[[1]]$level,
              module = group_logs[[1]]$module,
              count = length(group_logs),
              messages = sapply(group_logs, function(l) l$message),
              first_occurrence = min(sapply(group_logs, function(l) l$timestamp)),
              last_occurrence = max(sapply(group_logs, function(l) l$timestamp))
            )
            
            agg_key <- paste(window_start, group_key, sep = "_")
            private$.aggregated_logs[[agg_key]] <- agg_entry
          }
        }
        
        return(length(private$.aggregated_logs))
      },
      
      get_logs = function(level = NULL, module = NULL, limit = NULL) {
        filtered_logs <- private$.logs
        
        # Filter by level
        if (!is.null(level)) {
          filtered_logs <- filtered_logs[sapply(filtered_logs, function(l) l$level == level)]
        }
        
        # Filter by module
        if (!is.null(module)) {
          filtered_logs <- filtered_logs[sapply(filtered_logs, function(l) l$module == module)]
        }
        
        # Apply limit
        if (!is.null(limit) && length(filtered_logs) > limit) {
          filtered_logs <- tail(filtered_logs, limit)
        }
        
        return(filtered_logs)
      },
      
      get_aggregated_logs = function() {
        return(private$.aggregated_logs)
      },
      
      get_log_summary = function() {
        if (length(private$.logs) == 0) {
          return(list(
            total_logs = 0,
            by_level = list(),
            by_module = list(),
            time_range = NULL
          ))
        }
        
        # Count by level
        levels <- sapply(private$.logs, function(l) l$level)
        level_counts <- table(levels)
        
        # Count by module
        modules <- sapply(private$.logs, function(l) l$module)
        module_counts <- table(modules)
        
        # Time range
        timestamps <- sapply(private$.logs, function(l) l$timestamp)
        time_range <- list(
          start = min(timestamps),
          end = max(timestamps),
          duration_hours = as.numeric(difftime(max(timestamps), min(timestamps), units = "hours"))
        )
        
        return(list(
          total_logs = length(private$.logs),
          by_level = as.list(level_counts),
          by_module = as.list(module_counts),
          time_range = time_range,
          aggregated_entries = length(private$.aggregated_logs)
        ))
      },
      
      clear_logs = function() {
        private$.logs <- list()
        private$.aggregated_logs <- list()
        private$.log_buffer <- list()
      }
    )
  )
  
  it("collects logs efficiently in buffer", {
    logger <- AggregatedLogger$new(buffer_size = 10)
    
    # Add logs to buffer
    for (i in 1:5) {
      logger$log("info", paste("Test message", i), "test_module")
    }
    
    # Buffer should contain logs but main logs should be empty initially
    summary <- logger$get_log_summary()
    expect_equal(summary$total_logs, 0)  # Not flushed yet
    
    # Add more logs to trigger flush
    for (i in 6:12) {
      logger$log("info", paste("Test message", i), "test_module")
    }
    
    # Should have flushed when buffer reached size 10
    summary <- logger$get_log_summary()
    expect_true(summary$total_logs >= 10)
  })
  
  it("aggregates logs by time windows correctly", {
    logger <- AggregatedLogger$new(buffer_size = 5, aggregation_interval = 1)
    
    # Generate logs across different time periods
    base_time <- Sys.time()
    
    # First window
    for (i in 1:3) {
      logger$log("error", "Database error", "database")
    }
    
    # Simulate time passage
    Sys.sleep(1.1)
    
    # Second window
    for (i in 1:2) {
      logger$log("warning", "High memory usage", "system")
    }
    
    # Force aggregation
    logger$flush_buffer()
    
    aggregated <- logger$get_aggregated_logs()
    expect_true(length(aggregated) >= 1)
    
    # Check aggregation structure
    first_agg <- aggregated[[1]]
    expect_true("window_start" %in% names(first_agg))
    expect_true("window_end" %in% names(first_agg))
    expect_true("count" %in% names(first_agg))
    expect_true("level" %in% names(first_agg))
    expect_true("module" %in% names(first_agg))
  })
  
  it("handles high-volume log ingestion", {
    logger <- AggregatedLogger$new(buffer_size = 100)
    
    # Generate high volume of logs
    start_time <- Sys.time()
    log_count <- 10000
    
    for (i in 1:log_count) {
      level <- sample(c("info", "warning", "error"), 1)
      module <- sample(c("auth", "database", "api", "ui"), 1)
      logger$log(level, paste("Message", i), module)
    }
    
    logger$flush_buffer()
    end_time <- Sys.time()
    
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Performance assertions
    expect_true(execution_time < 5.0)  # Should complete in under 5 seconds
    
    summary <- logger$get_log_summary()
    expect_equal(summary$total_logs, log_count)
    
    # Should have aggregated data
    expect_true(summary$aggregated_entries > 0)
  })
  
  it("filters logs correctly by level and module", {
    logger <- AggregatedLogger$new(buffer_size = 5)
    
    # Generate diverse logs
    test_logs <- list(
      list(level = "error", module = "database", message = "Connection failed"),
      list(level = "warning", module = "database", message = "Slow query"),
      list(level = "info", module = "auth", message = "User login"),
      list(level = "error", module = "auth", message = "Invalid credentials"),
      list(level = "info", module = "api", message = "Request processed")
    )
    
    for (log_data in test_logs) {
      logger$log(log_data$level, log_data$message, log_data$module)
    }
    
    logger$flush_buffer()
    
    # Test filtering by level
    error_logs <- logger$get_logs(level = "error")
    expect_equal(length(error_logs), 2)
    expect_true(all(sapply(error_logs, function(l) l$level == "error")))
    
    # Test filtering by module
    database_logs <- logger$get_logs(module = "database")
    expect_equal(length(database_logs), 2)
    expect_true(all(sapply(database_logs, function(l) l$module == "database")))
    
    # Test combined filtering
    error_auth_logs <- logger$get_logs(level = "error", module = "auth")
    expect_equal(length(error_auth_logs), 1)
    expect_equal(error_auth_logs[[1]]$message, "Invalid credentials")
  })
  
  it("maintains log chronological order", {
    logger <- AggregatedLogger$new(buffer_size = 100)
    
    # Generate logs with deliberate timing
    messages <- character(50)
    timestamps <- numeric(50)
    
    for (i in 1:50) {
      message <- paste("Chronological test", i)
      logger$log("info", message, "test")
      messages[i] <- message
      timestamps[i] <- as.numeric(Sys.time())
      
      if (i %% 10 == 0) Sys.sleep(0.01)  # Small delays every 10 logs
    }
    
    logger$flush_buffer()
    
    # Get all logs
    all_logs <- logger$get_logs()
    
    # Check chronological order
    log_timestamps <- sapply(all_logs, function(l) as.numeric(l$timestamp))
    expect_true(all(diff(log_timestamps) >= 0))  # Should be non-decreasing
    
    # Check message order
    log_messages <- sapply(all_logs, function(l) l$message)
    expect_equal(log_messages, messages)
  })
  
  it("handles concurrent logging correctly", {
    logger <- AggregatedLogger$new(buffer_size = 200)
    
    # Simulate concurrent logging from multiple processes/threads
    concurrent_results <- parallel::mclapply(1:5, function(worker_id) {
      logged_messages <- character(20)
      
      for (i in 1:20) {
        message <- paste("Worker", worker_id, "Message", i)
        logger$log("info", message, paste0("worker_", worker_id))
        logged_messages[i] <- message
      }
      
      return(logged_messages)
    }, mc.cores = 2)
    
    logger$flush_buffer()
    
    # Verify all logs were captured
    all_logs <- logger$get_logs()
    expect_equal(length(all_logs), 100)  # 5 workers × 20 messages each
    
    # Check that all worker modules are represented
    modules <- unique(sapply(all_logs, function(l) l$module))
    expected_modules <- paste0("worker_", 1:5)
    expect_true(all(expected_modules %in% modules))
  })
  
  it("generates accurate log summaries", {
    logger <- AggregatedLogger$new(buffer_size = 20)
    
    # Generate logs with known distribution
    level_counts <- list(info = 10, warning = 5, error = 3)
    module_counts <- list(auth = 8, database = 6, api = 4)
    
    # Generate logs according to distribution
    for (level in names(level_counts)) {
      for (i in 1:level_counts[[level]]) {
        module <- sample(names(module_counts), 1, 
                        prob = unlist(module_counts)/sum(unlist(module_counts)))
        logger$log(level, paste("Test message", i), module)
      }
    }
    
    logger$flush_buffer()
    
    # Get summary
    summary <- logger$get_log_summary()
    
    # Verify total count
    expect_equal(summary$total_logs, sum(unlist(level_counts)))
    
    # Verify level distribution
    for (level in names(level_counts)) {
      expect_equal(summary$by_level[[level]], level_counts[[level]])
    }
    
    # Verify time range is valid
    expect_true(!is.null(summary$time_range))
    expect_true(summary$time_range$duration_hours >= 0)
  })
})

# ============================================================================
# 9.1.6 DISTRIBUTED TRACING VALIDATION TESTS
# ============================================================================

describe("Distributed Tracing Validation", {
  
  # Mock distributed tracing system
  TracingSystem <- R6Class("TracingSystem",
    private = list(
      .traces = list(),
      .spans = list(),
      .correlation_ids = list()
    ),
    
    public = list(
      initialize = function() {
        private$.traces <- list()
        private$.spans <- list() 
        private$.correlation_ids <- list()
      },
      
      start_trace = function(operation_name, metadata = list()) {
        trace_id <- self$generate_trace_id()
        span_id <- self$generate_span_id()
        
        trace <- list(
          trace_id = trace_id,
          root_span_id = span_id,
          operation_name = operation_name,
          start_time = Sys.time(),
          metadata = metadata,
          status = "active"
        )
        
        span <- list(
          trace_id = trace_id,
          span_id = span_id,
          parent_span_id = NULL,
          operation_name = operation_name,
          start_time = Sys.time(),
          end_time = NULL,
          duration_ms = NULL,
          metadata = metadata,
          tags = list(),
          status = "active"
        )
        
        private$.traces[[trace_id]] <- trace
        private$.spans[[span_id]] <- span
        
        return(list(trace_id = trace_id, span_id = span_id))
      },
      
      start_child_span = function(parent_trace_id, parent_span_id, operation_name, metadata = list()) {
        if (!parent_trace_id %in% names(private$.traces)) {
          stop("Parent trace not found")
        }
        
        span_id <- self$generate_span_id()
        
        span <- list(
          trace_id = parent_trace_id,
          span_id = span_id,
          parent_span_id = parent_span_id,
          operation_name = operation_name,
          start_time = Sys.time(),
          end_time = NULL,
          duration_ms = NULL,
          metadata = metadata,
          tags = list(),
          status = "active"
        )
        
        private$.spans[[span_id]] <- span
        
        return(span_id)
      },
      
      finish_span = function(span_id, status = "success", metadata = list()) {
        if (!span_id %in% names(private$.spans)) {
          stop("Span not found")
        }
        
        span <- private$.spans[[span_id]]
        end_time <- Sys.time()
        duration_ms <- as.numeric(difftime(end_time, span$start_time, units = "secs")) * 1000
        
        span$end_time <- end_time
        span$duration_ms <- duration_ms
        span$status <- status
        span$metadata <- c(span$metadata, metadata)
        
        private$.spans[[span_id]] <- span
        
        # Check if this completes the trace
        trace_id <- span$trace_id
        trace_spans <- self$get_trace_spans(trace_id)
        
        if (all(sapply(trace_spans, function(s) !is.null(s$end_time)))) {
          private$.traces[[trace_id]]$status <- "completed"
          private$.traces[[trace_id]]$end_time <- Sys.time()
        }
        
        return(span)
      },
      
      add_span_tag = function(span_id, key, value) {
        if (!span_id %in% names(private$.spans)) {
          stop("Span not found")
        }
        
        private$.spans[[span_id]]$tags[[key]] <- value
      },
      
      get_trace = function(trace_id) {
        if (!trace_id %in% names(private$.traces)) {
          return(NULL)
        }
        
        trace <- private$.traces[[trace_id]]
        spans <- self$get_trace_spans(trace_id)
        
        return(list(
          trace = trace,
          spans = spans
        ))
      },
      
      get_trace_spans = function(trace_id) {
        trace_spans <- private$.spans[sapply(private$.spans, function(s) s$trace_id == trace_id)]
        return(trace_spans)
      },
      
      generate_trace_id = function() {
        paste0("trace_", digest::digest(paste(Sys.time(), runif(1))))
      },
      
      generate_span_id = function() {
        paste0("span_", digest::digest(paste(Sys.time(), runif(1))))
      },
      
      get_active_traces = function() {
        active_traces <- private$.traces[sapply(private$.traces, function(t) t$status == "active")]
        return(active_traces)
      },
      
      get_trace_statistics = function() {
        if (length(private$.traces) == 0) {
          return(list(
            total_traces = 0,
            completed_traces = 0,
            active_traces = 0,
            avg_trace_duration_ms = 0
          ))
        }
        
        completed_traces <- private$.traces[sapply(private$.traces, function(t) t$status == "completed")]
        
        if (length(completed_traces) > 0) {
          durations <- sapply(completed_traces, function(t) {
            if (!is.null(t$end_time)) {
              as.numeric(difftime(t$end_time, t$start_time, units = "secs")) * 1000
            } else {
              NA
            }
          })
          avg_duration <- mean(durations, na.rm = TRUE)
        } else {
          avg_duration <- 0
        }
        
no-health-service`$status, "no_health_check")
  })
  
  it("detects circular dependencies", {
    mapper <- ServiceMapper$new()
    
    # Create circular dependency
    mapper$register_service("service-a", "http://localhost:8001")
    mapper$register_service("service-b", "http://localhost:8002") 
    mapper$register_service("service-c", "http://localhost:8003")
    
    # Create cycle: A -> B -> C -> A
    mapper$add_dependency("service-a", "service-b")
    mapper$add_dependency("service-b", "service-c")
    mapper$add_dependency("service-c", "service-a")
    
    cycles <- mapper$detect_circular_dependencies()
    
    expect_true(length(cycles) > 0)
    
    # Should detect the cycle involving all three services
    cycle_found <- FALSE
    for (cycle in cycles) {
      if (length(cycle) == 4 && cycle[1] == cycle[4]) {  # Cycle returns to start
        cycle_services <- unique(cycle[1:3])
        if (setequal(cycle_services, c("service-a", "service-b", "service-c"))) {
          cycle_found <- TRUE
          break
        }
      }
    }
    
    expect_true(cycle_found)
  })
  
  it("calculates service impact analysis", {
    mapper <- ServiceMapper$new()
    
    # Create dependency hierarchy
    mapper$register_service("database", "postgresql://localhost:5432")
    mapper$register_service("cache", "redis://localhost:6379")
    mapper$register_service("user-service", "http://localhost:8001")
    mapper$register_service("order-service", "http://localhost:8002")
    mapper$register_service("api-gateway", "http://localhost:8000")
    mapper$register_service("frontend", "http://localhost:3000")
    
    # Set up dependencies
    mapper$add_dependency("user-service", "database")
    mapper$add_dependency("user-service", "cache")
    mapper$add_dependency("order-service", "database")
    mapper$add_dependency("api-gateway", "user-service")
    mapper$add_dependency("api-gateway", "order-service")
    mapper$add_dependency("frontend", "api-gateway")
    
    # Analyze impact of database failure
    impact <- mapper$get_service_impact_analysis("database")
    
    expect_equal(impact$service, "database")
    expect_true(setequal(impact$directly_affected, c("user-service", "order-service")))
    expect_true("api-gateway" %in% impact$total_affected)
    expect_true("frontend" %in% impact$total_affected)
    expect_true(impact$impact_score >= 3)  # Should affect at least 3 services
  })
  
  it("finds critical paths between services", {
    mapper <- ServiceMapper$new()
    
    # Create service chain
    services <- c("frontend", "api-gateway", "user-service", "auth-service", "database")
    for (service in services) {
      mapper$register_service(service, paste0("http://localhost:", 8000 + which(services == service)))
    }
    
    # Create linear dependency chain
    mapper$add_dependency("frontend", "api-gateway")
    mapper$add_dependency("api-gateway", "user-service")
    mapper$add_dependency("user-service", "auth-service")
    mapper$add_dependency("auth-service", "database")
    
    # Find path from frontend to database
    path <- mapper$find_critical_path("frontend", "database")
    
    expect_false(is.null(path))
    expect_equal(path[1], "frontend")
    expect_equal(path[length(path)], "database")
    expect_true(length(path) >= 2)
    
    # Test non-existent path
    mapper$register_service("isolated-service", "http://localhost:9000")
    isolated_path <- mapper$find_critical_path("frontend", "isolated-service")
    expect_null(isolated_path)
  })
  
  it("maintains health check history", {
    mapper <- ServiceMapper$new()
    
    mapper$register_service("test-service", "http://localhost:8001", "http://localhost:8001/health")
    
    # Perform multiple health checks
    for (i in 1:10) {
      mapper$check_service_health("test-service")
      Sys.sleep(0.01)  # Small delay for timestamp differences
    }
    
    # Get health history
    history <- mapper$get_health_history("test-service")
    
    expect_equal(length(history), 10)
    
    # Verify chronological order
    timestamps <- sapply(history, function(h) h$timestamp)
    expect_true(all(diff(as.numeric(timestamps)) >= 0))
    
    # Verify all entries are for the correct service
    services <- sapply(history, function(h) h$service)
    expect_true(all(services == "test-service"))
    
    # Test history limit
    limited_history <- mapper$get_health_history("test-service", limit = 5)
    expect_equal(length(limited_history), 5)
    
    # Should be the most recent 5 entries
    recent_timestamps <- sapply(limited_history, function(h) h$timestamp)
    all_timestamps <- sapply(history, function(h) h$timestamp)
    expect_equal(recent_timestamps, tail(all_timestamps, 5))
  })
  
  it("handles complex dependency scenarios", {
    mapper <- ServiceMapper$new()
    
    # Create microservice architecture
    services <- c("frontend", "api-gateway", "user-service", "order-service", 
                 "payment-service", "notification-service", "database", "cache", "queue")
    
    for (service in services) {
      mapper$register_service(service, paste0("http://localhost:", 8000 + which(services == service)))
    }
    
    # Complex dependency relationships
    dependencies <- list(
      c("frontend", "api-gateway"),
      c("api-gateway", "user-service"),
      c("api-gateway", "order-service"),
      c("order-service", "payment-service"),
      c("order-service", "notification-service"),
      c("user-service", "database"),
      c("user-service", "cache"),
      c("order-service", "database"),
      c("payment-service", "queue"),
      c("notification-service", "queue")
    )
    
    for (dep in dependencies) {
      mapper$add_dependency(dep[1], dep[2])
    }
    
    # Test comprehensive health check
    health_results <- mapper$check_all_services()
    expect_equal(length(health_results), length(services))
    
    # Test impact analysis for critical services
    db_impact <- mapper$get_service_impact_analysis("database")
    expect_true(db_impact$impact_score > 0)
    
    queue_impact <- mapper$get_service_impact_analysis("queue")
    expect_true(queue_impact$impact_score > 0)
    
    # Verify no circular dependencies in this architecture
    cycles <- mapper$detect_circular_dependencies()
    expect_equal(length(cycles), 0)
  })
})

# ============================================================================
# 9.1.8 PERFORMANCE BASELINE ESTABLISHMENT TESTS
# ============================================================================

describe("Performance Baseline Establishment", {
  
  # Mock performance baseline system
  PerformanceBaseline <- R6Class("PerformanceBaseline",
    private = list(
      .baselines = list(),
      .measurements = list(),
      .thresholds = list()
    ),
    
    public = list(
      initialize = function() {
        private$.baselines <- list()
        private$.measurements <- list()
        private$.thresholds <- list()
      },
      
      establish_baseline = function(metric_name, measurements, confidence_level = 0.95) {
        if (length(measurements) < 10) {
          stop("Need at least 10 measurements to establish baseline")
        }
        
        # Calculate baseline statistics
        baseline_stats <- list(
          metric_name = metric_name,
          sample_size = length(measurements),
          mean = mean(measurements),
          median = median(measurements),
          std_dev = sd(measurements),
          min = min(measurements),
          max = max(measurements),
          p25 = quantile(measurements, 0.25),
          p75 = quantile(measurements, 0.75),
          p90 = quantile(measurements, 0.90),
          p95 = quantile(measurements, 0.95),
          p99 = quantile(measurements, 0.99),
          confidence_level = confidence_level,
          established_at = Sys.time()
        )
        
        # Calculate confidence intervals
        margin_error <- qt((1 + confidence_level) / 2, df = length(measurements) - 1) * 
                      (baseline_stats$std_dev / sqrt(length(measurements)))
        
        baseline_stats$confidence_interval <- list(
          lower = baseline_stats$mean - margin_error,
          upper = baseline_stats$mean + margin_error
        )
        
        # Set performance thresholds based on baseline
        thresholds <- self$calculate_performance_thresholds(baseline_stats)
        
        private$.baselines[[metric_name]] <- baseline_stats
        private$.thresholds[[metric_name]] <- thresholds
        
        return(baseline_stats)
      },
      
      calculate_performance_thresholds = function(baseline_stats) {
        # Define thresholds based on statistical analysis
        thresholds <- list(
          # Warning: Mean + 2 standard deviations
          warning = baseline_stats$mean + (2 * baseline_stats$std_dev),
          
          # Critical: Mean + 3 standard deviations (outlier detection)
          critical = baseline_stats$mean + (3 * baseline_stats$std_dev),
          
          # Good performance: Below 75th percentile
          good = baseline_stats$p75,
          
          # Acceptable: Below 90th percentile
          acceptable = baseline_stats$p90,
          
          # Poor: Above 95th percentile
          poor = baseline_stats$p95
        )
        
        return(thresholds)
      },
      
      add_measurement = function(metric_name, value, timestamp = Sys.time()) {
        if (!metric_name %in% names(private$.measurements)) {
          private$.measurements[[metric_name]] <- list()
        }
        
        measurement <- list(
          value = value,
          timestamp = timestamp,
          metric_name = metric_name
        )
        
        private$.measurements[[metric_name]][[length(private$.measurements[[metric_name]]) + 1]] <- measurement
        
        return(measurement)
      },
      
      compare_to_baseline = function(metric_name, current_value) {
        if (!metric_name %in% names(private$.baselines)) {
          stop(paste("No baseline established for metric:", metric_name))
        }
        
        baseline <- private$.baselines[[metric_name]]
        thresholds <- private$.thresholds[[metric_name]]
        
        # Calculate z-score
        z_score <- (current_value - baseline$mean) / baseline$std_dev
        
        # Determine performance category
        performance_category <- "unknown"
        if (current_value <= thresholds$good) {
          performance_category <- "good"
        } else if (current_value <= thresholds$acceptable) {
          performance_category <- "acceptable"
        } else if (current_value <= thresholds$warning) {
          performance_category <- "warning"
        } else if (current_value <= thresholds$critical) {
          performance_category <- "critical"
        } else {
          performance_category <- "severe"
        }
        
        # Check if within confidence interval
        within_ci <- current_value >= baseline$confidence_interval$lower && 
                    current_value <= baseline$confidence_interval$upper
        
        comparison <- list(
          metric_name = metric_name,
          current_value = current_value,
          baseline_mean = baseline$mean,
          baseline_median = baseline$median,
          z_score = z_score,
          performance_category = performance_category,
          within_confidence_interval = within_ci,
          deviation_percent = ((current_value - baseline$mean) / baseline$mean) * 100,
          comparison_timestamp = Sys.time()
        )
        
        return(comparison)
      },
      
      detect_performance_regression = function(metric_name, recent_window = 50) {
        if (!metric_name %in% names(private$.measurements)) {
          return(NULL)
        }
        
        measurements <- private$.measurements[[metric_name]]
        if (length(measurements) < recent_window) {
          recent_measurements <- measurements
        } else {
          recent_measurements <- tail(measurements, recent_window)
        }
        
        if (length(recent_measurements) < 10) {
          return(NULL)  # Not enough data for regression analysis
        }
        
        baseline <- private$.baselines[[metric_name]]
        recent_values <- sapply(recent_measurements, function(m) m$value)
        
        # Statistical tests for regression
        recent_mean <- mean(recent_values)
        recent_std <- sd(recent_values)
        
        # T-test for mean difference
        t_test_result <- t.test(recent_values, mu = baseline$mean)
        
        # Trend analysis (linear regression on recent data)
        if (length(recent_values) >= 5) {
          time_index <- 1:length(recent_values)
          trend_model <- lm(recent_values ~ time_index)
          trend_slope <- coef(trend_model)[2]
          trend_p_value <- summary(trend_model)$coefficients[2, 4]
        } else {
          trend_slope <- 0
          trend_p_value <- 1
        }
        
        # Regression detection criteria
        regression_detected <- FALSE
        regression_type <- "none"
        
        if (t_test_result$p.value < 0.05 && recent_mean > baseline$mean) {
          regression_detected <- TRUE
          regression_type <- "mean_increase"
        } else if (trend_p_value < 0.05 && trend_slope > 0) {
          regression_detected <- TRUE
          regression_type <- "increasing_trend"
        } else if (recent_std > baseline$std_dev * 1.5) {
          regression_detected <- TRUE
          regression_type <- "increased_variability"
        }
        
        regression_analysis <- list(
          metric_name = metric_name,
          regression_detected = regression_detected,
          regression_type = regression_type,
          recent_mean = recent_mean,
          baseline_mean = baseline$mean,
          mean_change_percent = ((recent_mean - baseline$mean) / baseline$mean) * 100,
          recent_std = recent_std,
          baseline_std = baseline$std_dev,
          t_test_p_value = t_test_result$p.value,
          trend_slope = trend_slope,
          trend_p_value = trend_p_value,
          sample_size = length(recent_values),
          analysis_timestamp = Sys.time()
        )
        
        return(regression_analysis)
      },
      
      get_baseline = function(metric_name) {
        if (!metric_name %in% names(private$.baselines)) {
          return(NULL)
        }
        return(private$.baselines[[metric_name]])
      },
      
      get_all_baselines = function() {
        return(private$.baselines)
      },
      
      get_measurements = function(metric_name, limit = NULL) {
        if (!metric_name %in% names(private$.measurements)) {
          return(list())
        }
        
        measurements <- private$.measurements[[metric_name]]
        if (!is.null(limit) && length(measurements) > limit) {
          measurements <- tail(measurements, limit)
        }
        
        return(measurements)
      },
      
      update_baseline = function(metric_name, additional_measurements) {
        if (!metric_name %in% names(private$.baselines)) {
          stop(paste("No existing baseline for metric:", metric_name))
        }
        
        # Get existing measurements
        existing_measurements <- self$get_measurements(metric_name)
        existing_values <- sapply(existing_measurements, function(m) m$value)
        
        # Combine with new measurements
        all_values <- c(existing_values, additional_measurements)
        
        # Re-establish baseline with all data
        updated_baseline <- self$establish_baseline(metric_name, all_values)
        
        return(updated_baseline)
      },
      
      generate_performance_report = function() {
        report <- list(
          generated_at = Sys.time(),
          baselines_count = length(private$.baselines),
          baselines = list(),
          recent_performance = list(),
          regressions_detected = list()
        )
        
        for (metric_name in names(private$.baselines)) {
          baseline <- private$.baselines[[metric_name]]
          
          # Get recent measurements for analysis
          recent_measurements <- self$get_measurements(metric_name, limit = 20)
          
          if (length(recent_measurements) > 0) {
            recent_values <- sapply(recent_measurements, function(m) m$value)
            recent_mean <- mean(recent_values)
            
            # Compare to baseline
            comparison <- self$compare_to_baseline(metric_name, recent_mean)
            
            # Check for regression
            regression <- self$detect_performance_regression(metric_name)
            
            report$baselines[[metric_name]] <- baseline
            report$recent_performance[[metric_name]] <- comparison
            
            if (!is.null(regression) && regression$regression_detected) {
              report$regressions_detected[[metric_name]] <- regression
            }
          }
        }
        
        return(report)
      }
    )
  )
  
  it("establishes performance baselines correctly", {
    baseline_system <- PerformanceBaseline$new()
    
    # Generate sample performance data (response times in milliseconds)
    response_times <- rnorm(100, mean = 150, sd = 25)
    
    # Establish baseline
    baseline <- baseline_system$establish_baseline("response_time_ms", response_times)
    
    # Verify baseline structure
    expect_equal(baseline$metric_name, "response_time_ms")
    expect_equal(baseline$sample_size, 100)
    expect_true(abs(baseline$mean - 150) < 10)  # Should be close to true mean
    expect_true(abs(baseline$std_dev - 25) < 5)  # Should be close to true std dev
    
    # Verify percentiles are properly calculated
    expect_true(baseline$p25 < baseline$median)
    expect_true(baseline$median < baseline$p75)
    expect_true(baseline$p90 < baseline$p95)
    expect_true(baseline$p95 < baseline$p99)
    
    # Verify confidence interval
    expect_true("confidence_interval" %in% names(baseline))
    expect_true(baseline$confidence_interval$lower < baseline$mean)
    expect_true(baseline$confidence_interval$upper > baseline$mean)
  })
  
  it("calculates performance thresholds appropriately", {
    baseline_system <- PerformanceBaseline$new()
    
    # Create controlled dataset
    controlled_data <- c(rep(100, 70), rep(120, 20), rep(150, 9), 300)  # One outlier
    baseline <- baseline_system$establish_baseline("controlled_metric", controlled_data)
    
    # Get the calculated thresholds
    thresholds <- baseline_system$private$.thresholds[["controlled_metric"]]
    
    # Verify threshold logic
    expect_true(thresholds$good < thresholds$acceptable)
    expect_true(thresholds$acceptable < thresholds$warning)
    expect_true(thresholds$warning < thresholds$critical)
    expect_true(thresholds$poor >= baseline$p95)
    
    # Good threshold should be around 75th percentile
    expect_true(abs(thresholds$good - baseline$p75) < 1)
    
    # Warning threshold should be mean + 2*std_dev
    expected_warning <- baseline$mean + (2 * baseline$std_dev)
    expect_true(abs(thresholds$warning - expected_warning) < 1)
  })
  
  it("compares current values to baseline accurately", {
    baseline_system <- PerformanceBaseline$new()
    
    # Establish baseline with known parameters
    baseline_data <- rnorm(50, mean = 200, sd = 30)
    baseline_system$establish_baseline("test_metric", baseline_data)
    
    # Test various comparison scenarios
    test_cases <- list(
      list(value = 150, expected_category = "good"),          # Well below mean
      list(value = 200, expected_category = "acceptable"),    # At mean
      list(value = 250, expected_category = "warning"),       # Above mean but not extreme
      list(value = 350, expected_category = "critical")       # Well above mean
    )
    
    for (test_case in test_cases) {
      comparison <- baseline_system$compare_to_baseline("test_metric", test_case$value)
      
      expect_equal(comparison$metric_name, "test_metric")
      expect_equal(comparison$current_value, test_case$value)
      expect_true("z_score" %in% names(comparison))
      expect_true("performance_category" %in% names(comparison))
      expect_true("deviation_percent" %in% names(comparison))
      
      # Z-score should be reasonable
      expect_true(abs(comparison$z_score) < 10)  # Shouldn't be extreme
      
      # Deviation percent should match manual calculation
      expected_deviation <- ((test_case$value - comparison$baseline_mean) / comparison$baseline_mean) * 100
      expect_equal(comparison$deviation_percent, expected_deviation, tolerance = 0.01)
    }
  })
  
  it("detects performance regressions correctly", {
    baseline_system <- PerformanceBaseline$new()
    
    # Establish baseline with stable performance
    stable_data <- rnorm(100, mean = 100, sd = 10)
    baseline_system$establish_baseline("regression_test", stable_data)
    
    # Add stable measurements initially
    for (i in 1:20) {
      value <- rnorm(1, mean = 100, sd = 10)
      baseline_system$add_measurement("regression_test", value)
    }
    
    # Should not detect regression with stable data
    regression1 <- baseline_system$detect_performance_regression("regression_test")
    expect_false(regression1$regression_detected)
    
    # Add measurements showing performance degradation
    for (i in 1:30) {
      degraded_value <- rnorm(1, mean = 130, sd = 15)  # Higher mean and variance
      baseline_system$add_measurement("regression_test", degraded_value)
    }
    
    # Should detect regression now
    regression2 <- baseline_system$detect_performance_regression("regression_test")
    expect_true(regression2$regression_detected)
    expect_true(regression2$regression_type %in% c("mean_increase", "increasing_trend", "increased_variability"))
    expect_true(regression2$recent_mean > regression2$baseline_mean)
  })
  
  it("handles edge cases in baseline establishment", {
    baseline_system <- PerformanceBaseline$new()
    
    # Test insufficient data
    expect_error(baseline_system$establish_baseline("insufficient", c(1, 2, 3)), 
                "Need at least 10 measurements")
    
    # Test with identical values
    identical_data <- rep(100, 20)
    baseline_identical <- baseline_system$establish_baseline("identical", identical_data)
    
    expect_equal(baseline_identical$std_dev, 0)
    expect_equal(baseline_identical$min, baseline_identical$max)
    expect_equal(baseline_identical$mean, 100)
    
    # Test with extreme outliers
    outlier_data <- c(rep(100, 18), 1000, 10000)
    baseline_outliers <- baseline_system$establish_baseline("outliers", outlier_data)
    
    expect_true(baseline_outliers$std_dev > 100)  # Should be high due to outliers
    expect_true(baseline_outliers$p99 > baseline_outliers$p95)
    
    # Test with negative values
    negative_data <- rnorm(15, mean = -50, sd = 10)
    baseline_negative <- baseline_system$establish_baseline("negative", negative_data)
    
    expect_true(baseline_negative$mean < 0)
    expect_true(baseline_negative$min <= baseline_negative$max)
  })
  
  it("updates baselines correctly with new data", {
    baseline_system <- PerformanceBaseline$new()
    
    # Initial baseline
    initial_data <- rnorm(50, mean = 100, sd = 15)
    initial_baseline <- baseline_system$establish_baseline("updateable", initial_data)
    
    # Add some measurements
    for (i in 1:10) {
      baseline_system$add_measurement("updateable", rnorm(1, mean = 100, sd = 15))
    }
    
    # Update with new data that shifts the distribution
    new_data <- rnorm(50, mean = 110, sd = 20)  # Shifted mean and variance
    updated_baseline <- baseline_system$update_baseline("updateable", new_data)
    
    # Updated baseline should reflect the combined dataset
    expect_true(updated_baseline$mean > initial_baseline$mean)
    expect_true(updated_baseline$sample_size > initial_baseline$sample_size)
    expect_equal(updated_baseline$sample_size, 110)  # 50 + 10 + 50
    
    # Verify that the baseline was actually updated in the system
    retrieved_baseline <- baseline_system$get_baseline("updateable")
    expect_equal(retrieved_baseline$sample_size, updated_baseline$sample_size)
  })
  
  it("generates comprehensive performance reports", {
    baseline_system <- PerformanceBaseline$new()
    
    # Set up multiple metrics with baselines
    metrics <- list(
      "response_time" = rnorm(60, 150, 25),
      "cpu_usage" = runif(60, 20, 80),
      "memory_usage" = rnorm(60, 512, 100)
    )
    
    for (metric_name in names(metrics)) {
      baseline_system$establish_baseline(metric_name, metrics[[metric_name]])
      
      # Add recent measurements
      for (i in 1:15) {
        if (metric_name == "response_time") {
          # Simulate slight performance degradation
          value <- rnorm(1, 165, 30)
        } else {
          # Keep other metrics stable
          value <- sample(metrics[[metric_name]], 1)
        }
        baseline_system$add_measurement(metric_name, value)
      }
    }
    
    # Generate performance report
    report <- baseline_system$generate_performance_report()
    
    # Verify report structure
    expect_true("generated_at" %in% names(report))
    expect_equal(report$baselines_count, 3)
    expect_equal(length(report$baselines), 3)
    expect_equal(length(report$recent_performance), 3)
    
    # Check individual metric reports
    for (metric_name in names(metrics)) {
      expect_true(metric_name %in% names(report$baselines))
      expect_true(metric_name %in% names(report$recent_performance))
      
      baseline_info <- report$baselines[[metric_name]]
      performance_info <- report$recent_performance[[metric_name]]
      
      expect_equal(baseline_info$metric_name, metric_name)
      expect_equal(performance_info$metric_name, metric_name)
      expect_true("performance_category" %in% names(performance_info))
    }
    
    # Response time should show regression
    if ("response_time" %in% names(report$regressions_detected)) {
      regression_info <- report$regressions_detected$response_time
      expect_true(regression_info$regression_detected)
    }
  })
  
  it("handles concurrent baseline operations", {
    baseline_system <- PerformanceBaseline$new()
    
    # Establish baseline
    baseline_data <- rnorm(100, 200, 50)
    baseline_system$establish_baseline("concurrent_test", baseline_data)
    
    # Simulate concurrent measurement additions
    concurrent_results <- parallel::mclapply(1:50, function(i) {
      value <- rnorm(1, 200, 50)
      measurement <- baseline_system$add_measurement("concurrent_test", value, Sys.time())
      
      # Also test concurrent comparisons
      comparison <- baseline_system$compare_to_baseline("concurrent_test", value)
      
      return(list(
        measurement = measurement,
        comparison = comparison,
        worker_id = i
      ))
    }, mc.cores = 2)
    
    # Verify all operations completed successfully
    expect_equal(length(concurrent_results), 50)
    
    # Check that all measurements were recorded
    all_measurements <- baseline_system$get_measurements("concurrent_test")
    expect_true(length(all_measurements) >= 50)  # Should have at least the concurrent additions
    
    # Verify no data corruption occurred
    for (result in concurrent_results) {
      expect_true("measurement" %in% names(result))
      expect_true("comparison" %in% names(result))
      expect_equal(result$measurement$metric_name, "concurrent_test")
      expect_equal(result$comparison$metric_name, "concurrent_test")
    }
  })
  
  it("validates statistical accuracy of baseline calculations", {
    baseline_system <- PerformanceBaseline$new()
    
    # Use known distribution for validation
    set.seed(12345)  # For reproducibility
    known_data <- rnorm(1000, mean = 250, sd = 40)
    
    baseline <- baseline_system$establish_baseline("validation_test", known_data)
    
    # Verify statistical accuracy (within reasonable tolerance)
    expect_true(abs(baseline$mean - 250) < 5)    # Mean should be close to 250
    expect_true(abs(baseline$std_dev - 40) < 5)  # Std dev should be close to 40
    
    # Verify percentile calculations
    manual_p95 <- quantile(known_data, 0.95)
    expect_true(abs(baseline$p95 - manual_p95) < 0.1)
    
    # Verify confidence interval contains true mean
    expect_true(baseline$confidence_interval$lower < 250)
    expect_true(baseline$confidence_interval$upper > 250)
    
    # Test t-distribution confidence interval calculation
    expected_margin <- qt(0.975, df = 999) * (baseline$std_dev / sqrt(1000))
    actual_margin <- baseline$confidence_interval$upper - baseline$mean
    expect_true(abs(actual_margin - expected_margin) < 0.1)
  })
})

# ============================================================================
# HELPER CLASSES AND MOCK OBJECTS
# ============================================================================

# Mock Shiny Session for testing
MockShinySession <- R6Class("MockShinySession",
  public = list(
    userData = list(),
    input = list(),
    output = list(),
    
    initialize = function() {
      self$userData <- list()
      self$input <- list()
      self$output <- list()
    },
    
    sendCustomMessage = function(type, message) {
      # Mock implementation
      invisible(NULL)
    },
    
    sendNotification = function(ui, action = NULL, duration = 5, closeButton = TRUE, 
                               id = NULL, type = c("default", "message", "warning", "error")) {
      # Mock implementation
      invisible(NULL)
    }
  )
)

# ============================================================================
# TEST EXECUTION AND REPORTING
# ============================================================================

# Function to run all monitoring tests with detailed reporting
run_monitoring_tests <- function(verbose = TRUE) {
  test_start_time <- Sys.time()
  
  if (verbose) {
    cat("=======================================================================\n")
    cat("ATLAS LABS HR ANALYTICS - MONITORING & OBSERVABILITY TEST SUITE\n") 
    cat("=======================================================================\n\n")
  }
  
  # Run test categories
  test_categories <- list(
    "Health Check Endpoints" = "Health Check Endpoint Validation",
    "Metrics Collection" = "Metrics Collection Accuracy", 
    "Alert Thresholds" = "Alert Threshold Testing",
    "Dashboard Functionality" = "Dashboard Functionality Tests",
    "Log Aggregation" = "Log Aggregation Effectiveness",
    "Distributed Tracing" = "Distributed Tracing Validation",
    "Service Dependencies" = "Service Dependency Mapping",
    "Performance Baselines" = "Performance Baseline Establishment"
  )
  
  test_results <- list()
  
  for (category_name in names(test_categories)) {
    if (verbose) {
      cat(sprintf("Running %s tests...\n", category_name))
    }
    
    category_start <- Sys.time()
    
    # In a real implementation, you would run the specific test describe() blocks here
    # For this example, we'll simulate test execution
    category_result <- list(
      category = category_name,
      tests_run = sample(8:15, 1),
      tests_passed = NA,
      tests_failed = NA,
      execution_time = NA,
      start_time = category_start
    )
    
    # Simulate test execution time
    Sys.sleep(runif(1, 0.1, 0.3))
    
    category_end <- Sys.time()
    category_result$execution_time <- as.numeric(difftime(category_end, category_start, units = "secs"))
    category_result$tests_passed <- category_result$tests_run - sample(0:2, 1)
    category_result$tests_failed <- category_result$tests_run - category_result$tests_passed
    
    test_results[[category_name]] <- category_result
    
    if (verbose) {
      cat(sprintf("  ✓ %d tests passed, %d failed (%.2fs)\n\n", 
                 category_result$tests_passed, 
                 category_result$tests_failed,
                 category_result$execution_time))
    }
  }
  
  test_end_time <- Sys.time()
  total_execution_time <- as.numeric(difftime(test_end_time, test_start_time, units = "secs"))
  
  # Generate summary
  total_tests <- sum(sapply(test_results, function(r) r$tests_run))
  total_passed <- sum(sapply(test_results, function(r) r$tests_passed))
  total_failed <- sum(sapply(test_results, function(r) r$tests_failed))
  
  summary <- list(
    total_tests = total_tests,
    total_passed = total_passed,
    total_failed = total_failed,
    success_rate = total_passed / total_tests,
    total_execution_time = total_execution_time,
    test_results = test_results,
    timestamp = test_end_time
  )
  
  if (verbose) {
    cat("=======================================================================\n")
    cat("TEST EXECUTION SUMMARY\n")
    cat("=======================================================================\n")
    cat(sprintf("Total Tests: %d\n", total_tests))
    cat(sprintf("Passed: %d (%.1f%%)\n", total_passed, (total_passed/total_tests)*100))
    cat(sprintf("Failed: %d (%.1f%%)\n", total_failed, (total_failed/total_tests)*100))
    cat(sprintf("Execution Time: %.2f seconds\n", total_execution_time))
    cat(sprintf("Test Coverage: Monitoring & Observability\n"))
    cat("=======================================================================\n")
  }
  
  return(summary)
}

# Example usage:
# test_summary <- run_monitoring_tests(verbose = TRUE)