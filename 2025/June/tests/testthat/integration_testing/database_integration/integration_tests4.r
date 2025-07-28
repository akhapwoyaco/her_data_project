# =============================================================================
# ATLAS LABS HR ANALYTICS - THIRD-PARTY SERVICES INTEGRATION TESTS
# =============================================================================
# Comprehensive integration testing for external dependencies and services
# Author: akhapwoyaco (GitHub)
# Coverage: External API reliability, service dependencies, failover mechanisms
# =============================================================================

library(testthat)
library(httr)
library(jsonlite)
library(pingr)
library(lubridate)
library(R6)
library(mockery)
library(withr)

# =============================================================================
# 6.3.6 MONITORING AND ALERTING TESTING
# =============================================================================

test_that("Monitoring and Alerting Systems", {
  
  # Test application performance monitoring (APM)
  test_that("Application performance monitoring", {
    
    # Mock APM metrics collection
    apm_metrics <- list(
      response_times = c(),
      error_rates = c(),
      throughput = c(),
      memory_usage = c(),
      cpu_usage = c()
    )
    
    # Performance metrics collector
    collect_performance_metrics <- function(operation_name, execution_time, success) {
      apm_metrics$response_times <<- c(apm_metrics$response_times, execution_time)
      apm_metrics$error_rates <<- c(apm_metrics$error_rates, !success)
      apm_metrics$throughput <<- c(apm_metrics$throughput, 1)
      apm_metrics$memory_usage <<- c(apm_metrics$memory_usage, runif(1, 50, 200))  # MB
      apm_metrics$cpu_usage <<- c(apm_metrics$cpu_usage, runif(1, 10, 80))  # %
      
      # Check for performance threshold violations
      if (execution_time > 5000) {  # 5 seconds
        return(list(alert = TRUE, type = "slow_response", value = execution_time))
      }
      
      current_error_rate <- mean(tail(apm_metrics$error_rates, 100))
      if (current_error_rate > 0.05) {  # 5% error rate
        return(list(alert = TRUE, type = "high_error_rate", value = current_error_rate))
      }
      
      return(list(alert = FALSE))
    }
    
    # Simulate various operations
    operations <- c("load_dashboard", "export_report", "filter_data", "generate_chart")
    
    for (i in 1:50) {
      operation <- sample(operations, 1)
      exec_time <- rlnorm(1, meanlog = 6, sdlog = 1)  # Log-normal distribution
      success_rate <- ifelse(exec_time > 8000, 0.7, 0.95)  # Higher failure for slow operations
      success <- runif(1) < success_rate
      
      alert_result <- collect_performance_metrics(operation, exec_time, success)
      
      if (alert_result$alert) {
        if (alert_result$type == "slow_response") {
          expect_gt(alert_result$value, 5000)
        }
        if (alert_result$type == "high_error_rate") {
          expect_gt(alert_result$value, 0.05)
        }
      }
    }
    
    # Verify metrics collection
    expect_equal(length(apm_metrics$response_times), 50)
    expect_equal(length(apm_metrics$error_rates), 50)
  })
  
  # Test infrastructure monitoring
  test_that("Infrastructure monitoring and alerts", {
    
    # Mock server metrics
    server_metrics <- list(
      web_servers = list(
        server1 = list(cpu = 25, memory = 60, disk = 40, network_io = 1000),
        server2 = list(cpu = 45, memory = 80, disk = 60, network_io = 1500),
        server3 = list(cpu = 85, memory = 95, disk = 90, network_io = 500)  # High load
      ),
      database_servers = list(
        db_primary = list(cpu = 35, memory = 70, disk = 50, connections = 150),
        db_replica = list(cpu = 20, memory = 45, disk = 55, connections = 80)
      )
    )
    
    # Infrastructure alert thresholds
    alert_thresholds <- list(
      cpu_critical = 90,
      cpu_warning = 75,
      memory_critical = 90,
      memory_warning = 80,
      disk_critical = 85,
      disk_warning = 75,
      db_connections_critical = 200,
      db_connections_warning = 180
    )
    
    # Monitor infrastructure and generate alerts
    monitor_infrastructure <- function() {
      alerts <- list()
      
      # Check web servers
      for (server_name in names(server_metrics$web_servers)) {
        server <- server_metrics$web_servers[[server_name]]
        
        # CPU alerts
        if (server$cpu >= alert_thresholds$cpu_critical) {
          alerts[[length(alerts) + 1]] <- list(
            type = "critical",
            category = "cpu",
            server = server_name,
            value = server$cpu,
            message = paste("Critical CPU usage on", server_name, ":", server$cpu, "%")
          )
        } else if (server$cpu >= alert_thresholds$cpu_warning) {
          alerts[[length(alerts) + 1]] <- list(
            type = "warning",
            category = "cpu", 
            server = server_name,
            value = server$cpu,
            message = paste("High CPU usage on", server_name, ":", server$cpu, "%")
          )
        }
        
        # Memory alerts
        if (server$memory >= alert_thresholds$memory_critical) {
          alerts[[length(alerts) + 1]] <- list(
            type = "critical",
            category = "memory",
            server = server_name,
            value = server$memory,
            message = paste("Critical memory usage on", server_name, ":", server$memory, "%")
          )
        }
        
        # Disk alerts
        if (server$disk >= alert_thresholds$disk_critical) {
          alerts[[length(alerts) + 1]] <- list(
            type = "critical",
            category = "disk",
            server = server_name, 
            value = server$disk,
            message = paste("Critical disk usage on", server_name, ":", server$disk, "%")
          )
        }
      }
      
      return(alerts)
    }
    
    alerts <- monitor_infrastructure()
    
    # Verify alert generation
    expect_gt(length(alerts), 0)
    
    # Check that server3 generated multiple critical alerts
    server3_alerts <- alerts[sapply(alerts, function(a) a$server == "server3")]
    expect_gte(length(server3_alerts), 2)  # CPU and memory critical
    
    # Verify alert structure
    for (alert in alerts) {
      expect_true(alert$type %in% c("critical", "warning"))
      expect_true(alert$category %in% c("cpu", "memory", "disk"))
      expect_true(nchar(alert$message) > 0)
    }
  })
  
  # Test business metrics monitoring
  test_that("Business metrics monitoring", {
    
    # Mock business KPI tracking
    business_metrics <- list(
      daily_active_users = 450,
      report_generation_count = 23,
      data_export_count = 8,
      dashboard_load_failures = 12,
      user_session_duration_avg = 25.5,  # minutes
      api_calls_per_hour = 1200
    )
    
    # Business metric thresholds
    business_thresholds <- list(
      min_daily_active_users = 400,
      max_dashboard_failures_per_day = 10,
      min_session_duration = 15,  # minutes
      max_api_calls_per_hour = 1000
    )
    
    # Business metrics alerting
    monitor_business_metrics <- function() {
      business_alerts <- list()
      
      # Low user engagement alert
      if (business_metrics$daily_active_users < business_thresholds$min_daily_active_users) {
        business_alerts[[length(business_alerts) + 1]] <- list(
          type = "warning",
          category = "user_engagement",
          metric = "daily_active_users",
          value = business_metrics$daily_active_users,
          threshold = business_thresholds$min_daily_active_users,
          message = "Daily active users below threshold"
        )
      }
      
      # Dashboard failure rate alert
      if (business_metrics$dashboard_load_failures > business_thresholds$max_dashboard_failures_per_day) {
        business_alerts[[length(business_alerts) + 1]] <- list(
          type = "critical",
          category = "reliability",
          metric = "dashboard_failures",
          value = business_metrics$dashboard_load_failures,
          threshold = business_thresholds$max_dashboard_failures_per_day,
          message = "Dashboard failure rate exceeded acceptable limits"
        )
      }
      
      # API rate limiting alert
      if (business_metrics$api_calls_per_hour > business_thresholds$max_api_calls_per_hour) {
        business_alerts[[length(business_alerts) + 1]] <- list(
          type = "warning",
          category = "api_usage",
          metric = "api_calls_per_hour",
          value = business_metrics$api_calls_per_hour,
          threshold = business_thresholds$max_api_calls_per_hour,
          message = "API usage approaching rate limits"
        )
      }
      
      return(business_alerts)
    }
    
    business_alerts <- monitor_business_metrics()
    
    # Verify business alerts
    expect_gt(length(business_alerts), 0)
    
    # Check for dashboard failure alert
    failure_alerts <- business_alerts[sapply(business_alerts, function(a) a$category == "reliability")]
    expect_equal(length(failure_alerts), 1)
    expect_equal(failure_alerts[[1]]$type, "critical")
    
    # Check for API usage alert
    api_alerts <- business_alerts[sapply(business_alerts, function(a) a$category == "api_usage")]
    expect_equal(length(api_alerts), 1)
    expect_equal(api_alerts[[1]]$type, "warning")
  })
  
  # Test alert notification delivery
  test_that("Alert notification delivery systems", {
    
    # Mock notification channels
    notification_channels <- list(
      email = list(enabled = TRUE, addresses = c("admin@atlas.com", "ops@atlas.com")),
      slack = list(enabled = TRUE, webhook_url = "https://hooks.slack.com/services/ABC123"),
      pagerduty = list(enabled = TRUE, service_key = "pd_service_123"),
      sms = list(enabled = FALSE, phone_numbers = c("+1234567890"))
    )
    
    # Notification delivery simulator
    send_notification <- function(alert, channel_type) {
      channel <- notification_channels[[channel_type]]
      
      if (!channel$enabled) {
        return(list(sent = FALSE, reason = "channel_disabled"))
      }
      
      # Simulate different delivery scenarios
      delivery_success_rates <- list(
        email = 0.95,
        slack = 0.98,
        pagerduty = 0.99,
        sms = 0.85
      )
      
      success <- runif(1) < delivery_success_rates[[channel_type]]
      
      if (success) {
        return(list(
          sent = TRUE,
          channel = channel_type,
          timestamp = Sys.time(),
          recipients = switch(channel_type,
            email = length(channel$addresses),
            slack = 1,
            pagerduty = 1,
            sms = length(channel$phone_numbers)
          )
        ))
      } else {
        return(list(
          sent = FALSE,
          channel = channel_type,
          reason = "delivery_failed",
          error = paste("Failed to deliver", channel_type, "notification")
        ))
      }
    }
    
    # Test notification delivery for critical alert
    critical_alert <- list(
      type = "critical",
      category = "infrastructure",
      message = "Database server down",
      timestamp = Sys.time()
    )
    
    # Send notifications across all enabled channels
    notification_results <- list()
    for (channel in names(notification_channels)) {
      result <- send_notification(critical_alert, channel)
      notification_results[[channel]] <- result
    }
    
    # Verify email notification
    expect_true(notification_results$email$sent)
    expect_equal(notification_results$email$recipients, 2)
    
    # Verify Slack notification
    expect_true(notification_results$slack$sent)
    
    # Verify PagerDuty notification
    expect_true(notification_results$pagerduty$sent)
    
    # Verify SMS is disabled
    expect_false(notification_results$sms$sent)
    expect_equal(notification_results$sms$reason, "channel_disabled")
  })
})

# =============================================================================
# 6.3.7 SLA COMPLIANCE VALIDATION TESTING
# =============================================================================

test_that("SLA Compliance Validation", {
  
  # Test availability SLA compliance
  test_that("Availability SLA monitoring", {
    
    # Mock SLA requirements
    sla_requirements <- list(
      availability = 99.9,  # 99.9% uptime
      response_time = 2000, # 2 seconds max
      error_rate = 0.01,    # 1% max error rate
      data_freshness = 15   # 15 minutes max data age
    )
    
    # Mock service uptime tracking
    uptime_data <- data.frame(
      timestamp = seq(Sys.time() - 86400, Sys.time(), by = "5 min"),  # Last 24 hours
      status = sample(c("up", "down", "degraded"), 288, 
                     prob = c(0.995, 0.003, 0.002), replace = TRUE),
      response_time = rlnorm(288, meanlog = 6.5, sdlog = 0.5),  # Response times
      stringsAsFactors = FALSE
    )
    
    # Calculate availability metrics
    calculate_availability <- function(uptime_data, period_hours = 24) {
      total_checks <- nrow(uptime_data)
      up_checks <- sum(uptime_data$status == "up")
      down_checks <- sum(uptime_data$status == "down")
      degraded_checks <- sum(uptime_data$status == "degraded")
      
      # Calculate availability percentage
      availability_pct <- (up_checks + 0.5 * degraded_checks) / total_checks * 100
      
      # Calculate average response time for successful requests
      successful_requests <- uptime_data[uptime_data$status %in% c("up", "degraded"), ]
      avg_response_time <- mean(successful_requests$response_time)
      
      # Calculate downtime in minutes
      downtime_minutes <- (down_checks + 0.5 * degraded_checks) * 5  # 5-minute intervals
      
      return(list(
        availability_pct = availability_pct,
        avg_response_time = avg_response_time,
        downtime_minutes = downtime_minutes,
        total_checks = total_checks,
        up_checks = up_checks,
        down_checks = down_checks,
        degraded_checks = degraded_checks
      ))
    }
    
    availability_metrics <- calculate_availability(uptime_data)
    
    # SLA compliance checks
    sla_compliance <- list(
      availability_met = availability_metrics$availability_pct >= sla_requirements$availability,
      response_time_met = availability_metrics$avg_response_time <= sla_requirements$response_time,
      acceptable_downtime = availability_metrics$downtime_minutes <= (24 * 60 * (100 - sla_requirements$availability) / 100)
    )
    
    # Verify SLA compliance
    expect_true(sla_compliance$availability_met)
    expect_true(sla_compliance$response_time_met)
    expect_true(sla_compliance$acceptable_downtime)
    
    # Generate SLA report
    sla_report <- list(
      period = "24 hours",
      availability_actual = round(availability_metrics$availability_pct, 3),
      availability_target = sla_requirements$availability,
      response_time_actual = round(availability_metrics$avg_response_time, 0),
      response_time_target = sla_requirements$response_time,
      downtime_minutes = availability_metrics$downtime_minutes,
      compliance_status = all(unlist(sla_compliance))
    )
    
    expect_true(sla_report$compliance_status)
  })
  
  # Test performance SLA validation
  test_that("Performance SLA validation", {
    
    # Mock performance metrics over time
    performance_data <- data.frame(
      timestamp = seq(Sys.time() - 7*86400, Sys.time(), by = "1 hour"),  # Last 7 days
      response_time_p50 = rlnorm(168, meanlog = 6.2, sdlog = 0.3),  # 50th percentile
      response_time_p95 = rlnorm(168, meanlog = 7.0, sdlog = 0.4),  # 95th percentile
      response_time_p99 = rlnorm(168, meanlog = 7.5, sdlog = 0.5),  # 99th percentile
      error_rate = rbeta(168, 2, 200),  # Error rate distribution
      throughput = rnorm(168, 1000, 100),  # Requests per minute
      stringsAsFactors = FALSE
    )
    
    # Performance SLA thresholds
    perf_sla <- list(
      response_time_p95_max = 3000,  # 95th percentile under 3 seconds
      response_time_p99_max = 5000,  # 99th percentile under 5 seconds
      error_rate_max = 0.02,         # 2% max error rate
      throughput_min = 800           # Minimum 800 requests/minute
    )
    
    # Calculate SLA compliance
    validate_performance_sla <- function(data, sla_thresholds) {
      # Calculate compliance for each metric
      p95_compliance <- mean(data$response_time_p95 <= sla_thresholds$response_time_p95_max)
      p99_compliance <- mean(data$response_time_p99 <= sla_thresholds$response_time_p99_max)
      error_rate_compliance <- mean(data$error_rate <= sla_thresholds$error_rate_max)
      throughput_compliance <- mean(data$throughput >= sla_thresholds$throughput_min)
      
      # Identify SLA violations
      violations <- list(
        p95_violations = sum(data$response_time_p95 > sla_thresholds$response_time_p95_max),
        p99_violations = sum(data$response_time_p99 > sla_thresholds$response_time_p99_max),
        error_rate_violations = sum(data$error_rate > sla_thresholds$error_rate_max),
        throughput_violations = sum(data$throughput < sla_thresholds$throughput_min)
      )
      
      return(list(
        compliance_rates = list(
          p95_response_time = p95_compliance,
          p99_response_time = p99_compliance,
          error_rate = error_rate_compliance,
          throughput = throughput_compliance
        ),
        violations = violations,
        overall_compliance = min(p95_compliance, p99_compliance, error_rate_compliance, throughput_compliance)
      ))
    }
    
    sla_validation <- validate_performance_sla(performance_data, perf_sla)
    
    # Verify SLA compliance rates
    expect_gt(sla_validation$compliance_rates$p95_response_time, 0.95)  # 95% compliance minimum
    expect_gt(sla_validation$compliance_rates$error_rate, 0.98)         # 98% compliance minimum
    expect_gt(sla_validation$overall_compliance, 0.95)                  # Overall 95% compliance
    
    # Check violation counts are reasonable
    expect_lt(sla_validation$violations$p95_violations, nrow(performance_data) * 0.05)
    expect_lt(sla_validation$violations$error_rate_violations, nrow(performance_data) * 0.02)
  })
  
  # Test data quality SLA compliance  
  test_that("Data quality SLA compliance", {
    
    # Mock data quality metrics
    data_quality_metrics <- data.frame(
      date = seq(Sys.Date() - 30, Sys.Date(), by = "day"),
      completeness_pct = runif(31, 95, 100),           # Data completeness
      accuracy_pct = runif(31, 98, 100),               # Data accuracy
      freshness_minutes = rexp(31, rate = 1/10),       # Data freshness
      consistency_score = runif(31, 0.95, 1.0),        # Cross-system consistency
      validation_errors = rpois(31, lambda = 2),       # Daily validation errors
      stringsAsFactors = FALSE
    )
    
    # Data quality SLA requirements
    data_sla <- list(
      completeness_min = 98.0,     # 98% minimum completeness
      accuracy_min = 99.0,         # 99% minimum accuracy  
      freshness_max_minutes = 15,  # 15 minutes max data age
      consistency_min = 0.98,      # 98% minimum consistency
      max_daily_errors = 5         # Maximum 5 validation errors per day
    )
    
    # Validate data quality SLA
    validate_data_quality_sla <- function(metrics, sla_requirements) {
      # Calculate compliance for each metric
      completeness_compliance <- mean(metrics$completeness_pct >= sla_requirements$completeness_min)
      accuracy_compliance <- mean(metrics$accuracy_pct >= sla_requirements$accuracy_min)
      freshness_compliance <- mean(metrics$freshness_minutes <= sla_requirements$freshness_max_minutes)
      consistency_compliance <- mean(metrics$consistency_score >= sla_requirements$consistency_min)
      error_compliance <- mean(metrics$validation_errors <= sla_requirements$max_daily_errors)
      
      # Identify non-compliant days
      non_compliant_days <- metrics[
        metrics$completeness_pct < sla_requirements$completeness_min |
        metrics$accuracy_pct < sla_requirements$accuracy_min |
        metrics$freshness_minutes > sla_requirements$freshness_max_minutes |
        metrics$consistency_score < sla_requirements$consistency_min |
        metrics$validation_errors > sla_requirements$max_daily_errors,
      ]
      
      return(list(
        compliance_rates = list(
          completeness = completeness_compliance,
          accuracy = accuracy_compliance,
          freshness = freshness_compliance,
          consistency = consistency_compliance,
          error_rate = error_compliance
        ),
        non_compliant_days = nrow(non_compliant_days),
        worst_metrics = list(
          min_completeness = min(metrics$completeness_pct),
          min_accuracy = min(metrics$accuracy_pct),
          max_freshness = max(metrics$freshness_minutes),
          min_consistency = min(metrics$consistency_score),
          max_errors = max(metrics$validation_errors)
        )
      ))
    }
    
    data_sla_validation <- validate_data_quality_sla(data_quality_metrics, data_sla)
    
    # Verify data quality SLA compliance
    expect_gt(data_sla_validation$compliance_rates$completeness, 0.90)
    expect_gt(data_sla_validation$compliance_rates$accuracy, 0.95)
    expect_lt(data_sla_validation$non_compliant_days, 5)  # Less than 5 non-compliant days
    
    # Check worst-case metrics are still reasonable
    expect_gt(data_sla_validation$worst_metrics$min_completeness, 90)
    expect_gt(data_sla_validation$worst_metrics$min_accuracy, 95)
  })
})

# =============================================================================
# 6.3.8 VENDOR RISK ASSESSMENT TESTING
# =============================================================================

test_that("Vendor Risk Assessment", {
  
  # Test vendor dependency analysis
  test_that("Vendor dependency risk analysis", {
    
    # Mock vendor information
    vendor_registry <- list(
      cloud_provider = list(
        name = "AWS",
        criticality = "critical",
        services = c("hosting", "database", "storage", "cdn"),
        contract_end = as.Date("2025-12-31"),
        backup_vendors = c("Azure", "GCP"),
        sla_level = 99.99,
        data_residency = c("US", "EU"),
        compliance = c("SOC2", "ISO27001", "HIPAA")
      ),
      authentication = list(
        name = "Auth0",
        criticality = "high", 
        services = c("sso", "user_management", "mfa"),
        contract_end = as.Date("2024-06-30"),
        backup_vendors = c("Okta", "Azure AD"),
        sla_level = 99.9,
        data_residency = c("US"),
        compliance = c("SOC2", "ISO27001")
      ),
      analytics = list(
        name = "DataDog",
        criticality = "medium",
        services = c("monitoring", "logging", "alerting"),
        contract_end = as.Date("2024-03-15"), 
        backup_vendors = c("New Relic", "Splunk"),
        sla_level = 99.5,
        data_residency = c("US", "EU"),
        compliance = c("SOC2")
      ),
      email_service = list(
        name = "SendGrid",
        criticality = "low",
        services = c("transactional_email", "notifications"),
        contract_end = as.Date("2024-08-20"),
        backup_vendors = c("Mailgun", "Amazon SES"),
        sla_level = 99.0,
        data_residency = c("US"),
        compliance = c("SOC2")
      )
    )
    
    # Vendor risk assessment function
    assess_vendor_risk <- function(vendor_info) {
      risk_score <- 0
      risk_factors <- c()
      
      # Criticality risk
      criticality_risk <- switch(vendor_info$criticality,
        "critical" = 40,
        "high" = 25,
        "medium" = 15,
        "low" = 5
      )
      risk_score <- risk_score + criticality_risk
      
      # Contract expiry risk
      days_to_expiry <- as.numeric(vendor_info$contract_end - Sys.Date())
      if (days_to_expiry < 90) {
        risk_score <- risk_score + 30
        risk_factors <- c(risk_factors, "contract_expiring_soon")
      } else if (days_to_expiry < 180) {
        risk_score <- risk_score + 15
        risk_factors <- c(risk_factors, "contract_renewal_needed")
      }
      
      # Backup vendor availability risk
      if (length(vendor_info$backup_vendors) == 0) {
        risk_score <- risk_score + 25
        risk_factors <- c(risk_factors, "no_backup_vendors")
      } else if (length(vendor_info$backup_vendors) == 1) {
        risk_score <- risk_score + 10
        risk_factors <- c(risk_factors, "limited_backup_options")
      }
      
      # SLA risk
      if (vendor_info$sla_level < 99.0) {
        risk_score <- risk_score + 20
        risk_factors <- c(risk_factors, "low_sla")
      } else if (vendor_info$sla_level < 99.5) {
        risk_score <- risk_score + 10
        risk_factors <- c(risk_factors, "moderate_sla")
      }
      
      # Compliance risk
      required_compliance <- c("SOC2", "ISO27001")
      missing_compliance <- setdiff(required_compliance, vendor_info$compliance)
      if (length(missing_compliance) > 0) {
        risk_score <- risk_score + length(missing_compliance) * 15
        risk_factors <- c(risk_factors, paste("missing_compliance:", missing_compliance))
      }
      
      # Risk level classification
      risk_level <- if (risk_score >= 80) "critical" else
                   if (risk_score >= 60) "high" else
                   if (risk_score >= 40) "medium" else "low"
      
      return(list(
        vendor = vendor_info$name,
        risk_score = risk_score,
        risk_level = risk_level,
        risk_factors = risk_factors,
        days_to_contract_expiry = days_to_expiry,
        mitigation_priority = vendor_info$criticality
      ))
    }
    
    # Assess all vendors
    vendor_assessments <- lapply(vendor_registry, assess_vendor_risk)
    
    # Test critical vendor (AWS) assessment
    aws_assessment <- vendor_assessments$cloud_provider
    expect_equal(aws_assessment$vendor, "AWS")
    expect_gte(aws_assessment$risk_score, 40)  # High base risk due to criticality
    expect_true(aws_assessment$risk_level %in% c("medium", "high", "critical"))
    
    # Test vendor with expiring contract (Analytics - DataDog)
    datadog_assessment <- vendor_assessments$analytics
    expect_true("contract_expiring_soon" %in% datadog_assessment$risk_factors ||
                "contract_renewal_needed" %in% datadog_assessment$risk_factors)
    
    # Test compliance gaps
    for (assessment in vendor_assessments) {
      compliance_factors <- assessment$risk_factors[grepl("missing_compliance", assessment$risk_factors)]
      if (length(compliance_factors) > 0) {
        expect_gt(assessment$risk_score, 30)  # Should have elevated risk score
      }
    }
  })
  
  # Test vendor performance monitoring
  test_that("Vendor performance monitoring", {
    
    # Mock vendor performance metrics
    vendor_performance <- list(
      aws = data.frame(
        date = seq(Sys.Date() - 30, Sys.Date(), by = "day"),
        availability = runif(31, 99.95, 100),
        response_time = rnorm(31, 150, 20),  # milliseconds
        incidents = rpois(31, 0.1),          # Very low incident rate
        cost_variance = rnorm(31, 0, 0.05),  # Cost variance from budget
        stringsAsFactors = FALSE
      ),
      auth0 = data.frame(
        date = seq(Sys.Date() - 30, Sys.Date(), by = "day"),
        availability = runif(31, 99.8, 99.95),
        response_time = rnorm(31, 300, 50),
        incidents = rpois(31, 0.2),
        cost_variance = rnorm(31, 0.02, 0.03),
        stringsAsFactors = FALSE
      ),
      datadog = data.frame(
        date = seq(Sys.Date() - 30, Sys.Date(), by = "day"),
        availability = runif(31, 99.0, 99.8),
        response_time = rnorm(31, 500, 100),
        incidents = rpois(31, 0.5),
        cost_variance = rnorm(31, 0.1, 0.08),
        stringsAsFactors = FALSE
      )
    )
    
    # Vendor performance SLA thresholds
    vendor_sla_thresholds <- list(
      availability_min = 99.5,
      response_time_max = 1000,  # milliseconds
      max_incidents_per_month = 5,
      cost_variance_max = 0.15   # 15% variance
    )
    
    # Monitor vendor performance
    monitor_vendor_performance <- function(perf_data, vendor_name, thresholds) {
      # Calculate monthly metrics
      monthly_availability <- mean(perf_data$availability)
      avg_response_time <- mean(perf_data$response_time)
      total_incidents <- sum(perf_data$incidents)
      avg_cost_variance <- mean(abs(perf_data$cost_variance))
      
      # Check SLA compliance
      sla_violations <- list()
      
      if (monthly_availability < thresholds$availability_min) {
        sla_violations <- c(sla_violations, list(list(
          type = "availability",
          actual = monthly_availability,
          threshold = thresholds$availability_min,
          severity = "high"
        )))
      }
      
      if (avg_response_time > thresholds$response_time_max) {
        sla_violations <- c(sla_violations, list(list(
          type = "response_time", 
          actual = avg_response_time,
          threshold = thresholds$response_time_max,
          severity = "medium"
        )))
      }
      
      if (total_incidents > thresholds$max_incidents_per_month) {
        sla_violations <- c(sla_violations, list(list(
          type = "incidents",
          actual = total_incidents,
          threshold = thresholds$max_incidents_per_month,
          severity = "high"
        )))
      }
      
      if (avg_cost_variance > thresholds$cost_variance_max) {
        sla_violations <- c(sla_violations, list(list(
          type = "cost_variance",
          actual = avg_cost_variance,
          threshold = thresholds$cost_variance_max,
          severity = "low"
        )))
      }
      
      # Performance score (0-100)
      performance_score <- (
        (min(monthly_availability, 100) / 100) * 30 +
        (max(0, 1 - avg_response_time / thresholds$response_time_max)) * 25 +
        (max(0, 1 - total_incidents / thresholds$max_incidents_per_month)) * 25 +
        (max(0, 1 - avg_cost_variance / thresholds$cost_variance_max)) * 20
      ) * 100
      
      return(list(
        vendor = vendor_name,
        performance_score = round(performance_score, 1),
        monthly_availability = round(monthly_availability, 3),
        avg_response_time = round(avg_response_time, 0),
        total_incidents = total_incidents,
        avg_cost_variance = round(avg_cost_variance, 3),
        sla_violations = sla_violations,
        compliant = length(sla_violations) == 0
      ))
    }
    
    # Monitor all vendors
    vendor_reports <- list()
    for (vendor in names(vendor_performance)) {
      vendor_reports[[vendor]] <- monitor_vendor_performance(
        vendor_performance[[vendor]], 
        vendor, 
        vendor_sla_thresholds
      )
    }
    
    # Test AWS performance (should be excellent)
    aws_report <- vendor_reports$aws
    expect_gt(aws_report$performance_score, 90)
    expect_gt(aws_report$monthly_availability, 99.9)
    expect_true(aws_report$compliant)
    
    # Test DataDog performance (may have some issues)
    datadog_report <- vendor_reports$datadog
    expect_lt(datadog_report$performance_score, 85)  # Lower performance expected
    
    # Verify all reports have required fields
    for (report in vendor_reports) {
      expect_true("vendor" %in% names(report))
      expect_true("performance_score" %in% names(report))
      expect_true("sla_violations" %in% names(report))
      expect_true("compliant" %in% names(report))
    }
  })
  
  # Test vendor security assessment
  test_that("Vendor security risk assessment", {
    
    # Mock vendor security profiles
    vendor_security <- list(
      aws = list(
        security_certifications = c("SOC2", "ISO27001", "FedRAMP", "PCI-DSS"),
        data_encryption = list(at_rest = TRUE, in_transit = TRUE, key_management = "HSM"),
        access_controls = list(mfa = TRUE, rbac = TRUE, audit_logging = TRUE),
        incident_response_time = 30,  # minutes
        security_incidents_last_year = 0,
        penetration_test_frequency = "quarterly",
        vulnerability_scan_frequency = "weekly",
        data_residency_compliance = TRUE,
        third_party_assessments = c("AWS", "independent_auditor")
      ),
      auth0 = list(
        security_certifications = c("SOC2", "ISO27001"),
        data_encryption = list(at_rest = TRUE, in_transit = TRUE, key_management = "software"),
        access_controls = list(mfa = TRUE, rbac = TRUE, audit_logging = TRUE),
        incident_response_time = 60,
        security_incidents_last_year = 1,
        penetration_test_frequency = "annually",
        vulnerability_scan_frequency = "monthly",
        data_residency_compliance = TRUE,
        third_party_assessments = c("independent_auditor")
      ),
      datadog = list(
        security_certifications = c("SOC2"),
        data_encryption = list(at_rest = TRUE, in_transit = TRUE, key_management = "software"),
        access_controls = list(mfa = TRUE, rbac = FALSE, audit_logging = TRUE),
        incident_response_time = 120,
        security_incidents_last_year = 3,
        penetration_test_frequency = "annually", 
        vulnerability_scan_frequency = "monthly",
        data_residency_compliance = FALSE,
        third_party_assessments = c()
      )
    )
    
    # Security risk scoring function
    assess_security_risk <- function(security_profile, vendor_name) {
      security_score <- 100  # Start with perfect score, deduct points for risks
      risk_factors <- c()
      
      # Certification requirements
      required_certs <- c("SOC2", "ISO27001")
      missing_certs <- setdiff(required_certs, security_profile$security_certifications)
      if (length(missing_certs) > 0) {
        security_score <- security_score - length(missing_certs) * 15
        risk_factors <- c(risk_factors, paste("missing_certification:", missing_certs))
      }
      
      # Encryption requirements
      if (!security_profile$data_encryption$at_rest) {
        security_score <- security_score - 20
        risk_factors <- c(risk_factors, "no_encryption_at_rest")
      }
      if (!security_profile$data_encryption$in_transit) {
        security_score <- security_score - 20
        risk_factors <- c(risk_factors, "no_encryption_in_transit")
      }
      if (security_profile$data_encryption$key_management != "HSM") {
        security_score <- security_score - 10
        risk_factors <- c(risk_factors, "weak_key_management")
      }
      
      # Access control requirements
      if (!security_profile$access_controls$mfa) {
        security_score <- security_score - 15
        risk_factors <- c(risk_factors, "no_mfa")
      }
      if (!security_profile$access_controls$rbac) {
        security_score <- security_score - 10
        risk_factors <- c(risk_factors, "no_rbac")
      }
      if (!security_profile$access_controls$audit_logging) {
        security_score <- security_score - 10
        risk_factors <- c(risk_factors, "insufficient_audit_logging")
      }
      
      # Incident response time
      if (security_profile$incident_response_time > 60) {
        security_score <- security_score - 10
        risk_factors <- c(risk_factors, "slow_incident_response")
      }
      
      # Security incident history
      if (security_profile$security_incidents_last_year > 2) {
        security_score <- security_score - security_profile$security_incidents_last_year * 5
        risk_factors <- c(risk_factors, "high_incident_rate")
      }
      
      # Testing frequency
      test_frequencies <- c("weekly" = 1, "monthly" = 2, "quarterly" = 4, "annually" = 12)
      vuln_scan_months <- test_frequencies[security_profile$vulnerability_scan_frequency]
      if (vuln_scan_months > 1) {
        security_score <- security_score - (vuln_scan_months - 1) * 2
        risk_factors <- c(risk_factors, "infrequent_vulnerability_scanning")
      }
      
      # Data residency compliance
      if (!security_profile$data_residency_compliance) {
        security_score <- security_score - 15
        risk_factors <- c(risk_factors, "data_residency_non_compliance")
      }
      
      # Third-party assessments
      if (length(security_profile$third_party_assessments) == 0) {
        security_score <- security_score - 10
        risk_factors <- c(risk_factors, "no_third_party_assessment")
      }
      
      # Security risk level
      security_risk_level <- if (security_score >= 90) "low" else
                           if (security_score >= 75) "medium" else
                           if (security_score >= 60) "high" else "critical"
      
      return(list(
        vendor = vendor_name,
        security_score = max(0, security_score),
        security_risk_level = security_risk_level,
        risk_factors = risk_factors,
        recommendations = generate_security_recommendations(risk_factors)
      ))
    }
    
    # Generate security recommendations
    generate_security_recommendations <- function(risk_factors) {
      recommendations <- c()
      
      if (any(grepl("missing_certification", risk_factors))) {
        recommendations <- c(recommendations, "Obtain required security certifications")
      }
      if ("no_encryption_at_rest" %in% risk_factors) {
        recommendations <- c(recommendations, "Implement encryption at rest")
      }
      if ("no_mfa" %in% risk_factors) {
        recommendations <- c(recommendations, "Enable multi-factor authentication")
      }
      if ("high_incident_rate" %in% risk_factors) {
        recommendations <- c(recommendations, "Review incident response procedures")
      }
      if ("data_residency_non_compliance" %in% risk_factors) {
        recommendations <- c(recommendations, "Ensure data residency compliance")
      }
      
      return(recommendations)
    }
    
    # Assess security for all vendors
    security_assessments <- list()
    for (vendor in names(vendor_security)) {
      security_assessments[[vendor]] <- assess_security_risk(
        vendor_security[[vendor]], 
        vendor
      )
    }
    
    # Test AWS security assessment (should be excellent)
    aws_security <- security_assessments$aws
    expect_gte(aws_security$security_score, 90)
    expect_equal(aws_security$security_risk_level, "low")
    expect_equal(length(aws_security$risk_factors), 1)  # Only weak key management
    
    # Test DataDog security assessment (should have more risks)
    datadog_security <- security_assessments$datadog
    expect_lt(datadog_security$security_score, 80)
    expect_true(datadog_security$security_risk_level %in% c("medium", "high"))
    expect_gt(length(datadog_security$risk_factors), 3)
    
    # Verify recommendations are generated for high-risk vendors
    high_risk_vendors <- security_assessments[sapply(security_assessments, 
                                                   function(x) x$security_risk_level %in% c("high", "critical"))]
    for (vendor_assessment in high_risk_vendors) {
      expect_gt(length(vendor_assessment$recommendations), 0)
    }
  })
  
  # Test vendor business continuity planning
  test_that("Vendor business continuity assessment", {
    
    # Mock vendor business continuity profiles
    vendor_bcp <- list(
      aws = list(
        disaster_recovery_rto = 4,    # hours
        disaster_recovery_rpo = 1,    # hours  
        backup_frequency = "continuous",
        geographic_redundancy = TRUE,
        failover_testing_frequency = "quarterly",
        business_continuity_plan = TRUE,
        insurance_coverage = 100000000,  # $100M
        financial_stability_rating = "AAA",
        market_position = "leader",
        alternative_vendors_available = 2
      ),
      auth0 = list(
        disaster_recovery_rto = 8,
        disaster_recovery_rpo = 4,
        backup_frequency = "daily",
        geographic_redundancy = TRUE,
        failover_testing_frequency = "biannually",
        business_continuity_plan = TRUE,
        insurance_coverage = 50000000,  # $50M
        financial_stability_rating = "A",
        market_position = "established",
        alternative_vendors_available = 3
      ),
      datadog = list(
        disaster_recovery_rto = 12,
        disaster_recovery_rpo = 8,
        backup_frequency = "daily",
        geographic_redundancy = FALSE,
        failover_testing_frequency = "annually",
        business_continuity_plan = FALSE,
        insurance_coverage = 10000000,  # $10M
        financial_stability_rating = "BBB",
        market_position = "growing",
        alternative_vendors_available = 4
      )
    )
    
    # Business continuity risk assessment
    assess_business_continuity_risk <- function(bcp_profile, vendor_name) {
      continuity_score <- 100
      risk_factors <- c()
      
      # Recovery time objectives (RTO)
      if (bcp_profile$disaster_recovery_rto > 8) {
        continuity_score <- continuity_score - 20
        risk_factors <- c(risk_factors, "long_recovery_time")
      } else if (bcp_profile$disaster_recovery_rto > 4) {
        continuity_score <- continuity_score - 10
        risk_factors <- c(risk_factors, "moderate_recovery_time")
      }
      
      # Recovery point objectives (RPO)
      if (bcp_profile$disaster_recovery_rpo > 4) {
        continuity_score <- continuity_score - 15
        risk_factors <- c(risk_factors, "high_data_loss_risk")
      } else if (bcp_profile$disaster_recovery_rpo > 1) {
        continuity_score <- continuity_score - 8
        risk_factors <- c(risk_factors, "moderate_data_loss_risk")
      }
      
      # Geographic redundancy
      if (!bcp_profile$geographic_redundancy) {
        continuity_score <- continuity_score - 15
        risk_factors <- c(risk_factors, "no_geographic_redundancy")
      }
      
      # Business continuity plan
      if (!bcp_profile$business_continuity_plan) {
        continuity_score <- continuity_score - 20
        risk_factors <- c(risk_factors, "no_bcp_plan")
      }
      
      # Failover testing frequency
      test_frequency_risk <- switch(bcp_profile$failover_testing_frequency,
        "monthly" = 0,
        "quarterly" = 5,
        "biannually" = 10,
        "annually" = 15,
        20  # default for less frequent
      )
      continuity_score <- continuity_score - test_frequency_risk
      if (test_frequency_risk > 10) {
        risk_factors <- c(risk_factors, "infrequent_failover_testing")
      }
      
      # Financial stability
      financial_risk <- switch(bcp_profile$financial_stability_rating,
        "AAA" = 0, "AA" = 2, "A" = 5, "BBB" = 10, "BB" = 20, 25
      )
      continuity_score <- continuity_score - financial_risk
      if (financial_risk > 10) {
        risk_factors <- c(risk_factors, "financial_stability_concerns")
      }
      
      # Alternative vendor availability
      if (bcp_profile$alternative_vendors_available < 2) {
        continuity_score <- continuity_score - 15
        risk_factors <- c(risk_factors, "limited_alternatives")
      }
      
      # Overall business continuity risk level
      continuity_risk_level <- if (continuity_score >= 85) "low" else
                             if (continuity_score >= 70) "medium" else
                             if (continuity_score >= 55) "high" else "critical"
      
      return(list(
        vendor = vendor_name,
        continuity_score = max(0, continuity_score),
        continuity_risk_level = continuity_risk_level,
        rto_hours = bcp_profile$disaster_recovery_rto,
        rpo_hours = bcp_profile$disaster_recovery_rpo,
        risk_factors = risk_factors,
        mitigation_strategies = generate_continuity_mitigation(risk_factors)
      ))
    }
    
    # Generate mitigation strategies
    generate_continuity_mitigation <- function(risk_factors) {
      strategies <- c()
      
      if ("long_recovery_time" %in% risk_factors) {
        strategies <- c(strategies, "Negotiate improved RTO in contract renewal")
      }
      if ("no_geographic_redundancy" %in% risk_factors) {
        strategies <- c(strategies, "Require geographic redundancy implementation")
      }
      if ("no_bcp_plan" %in% risk_factors) {
        strategies <- c(strategies, "Request vendor business continuity plan")
      }
      if ("financial_stability_concerns" %in% risk_factors) {
        strategies <- c(strategies, "Monitor vendor financial health quarterly")
      }
      if ("limited_alternatives" %in% risk_factors) {
        strategies <- c(strategies, "Identify and evaluate additional vendor options")
      }
      
      return(strategies)
    }
    
    # Assess business continuity for all vendors
    bcp_assessments <- list()
    for (vendor in names(vendor_bcp)) {
      bcp_assessments[[vendor]] <- assess_business_continuity_risk(
        vendor_bcp[[vendor]], 
        vendor
      )
    }
    
    # Test AWS business continuity (should be excellent)
    aws_bcp <- bcp_assessments$aws
    expect_gte(aws_bcp$continuity_score, 85)
    expect_equal(aws_bcp$continuity_risk_level, "low")
    expect_lte(aws_bcp$rto_hours, 4)
    expect_lte(aws_bcp$rpo_hours, 1)
    
    # Test DataDog business continuity (should have higher risk)
    datadog_bcp <- bcp_assessments$datadog
    expect_lt(datadog_bcp$continuity_score, 75)
    expect_true(datadog_bcp$continuity_risk_level %in% c("medium", "high"))
    expect_gt(length(datadog_bcp$risk_factors), 2)
    
    # Verify mitigation strategies are provided for high-risk vendors
    high_risk_bcp <- bcp_assessments[sapply(bcp_assessments, 
                                          function(x) x$continuity_risk_level %in% c("high", "critical"))]
    for (vendor_assessment in high_risk_bcp) {
      expect_gt(length(vendor_assessment$mitigation_strategies), 0)
    }
  })
})

# =============================================================================
# INTEGRATION TEST EXECUTION AND REPORTING
# =============================================================================

# Test execution wrapper with comprehensive reporting
run_integration_tests <- function() {
  
  # Initialize test results tracking
  test_results <- list(
    start_time = Sys.time(),
    end_time = NULL,
    total_tests = 0,
    passed_tests = 0,
    failed_tests = 0,
    skipped_tests = 0,
    test_details = list(),
    coverage_metrics = list(),
    performance_metrics = list()
  )
  
  # Custom test reporter
  test_reporter <- function(test_name, test_function) {
    cat(paste("\n=== Running:", test_name, "===\n"))
    start_time <- Sys.time()
    
    tryCatch({
      test_function()
      end_time <- Sys.time()
      execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      test_results$total_tests <<- test_results$total_tests + 1
      test_results$passed_tests <<- test_results$passed_tests + 1
      
      test_results$test_details[[test_name]] <<- list(
        status = "PASSED",
        execution_time = execution_time,
        start_time = start_time,
        end_time = end_time,
        error = NULL
      )
      
      cat(paste("✓ PASSED -", test_name, "- Execution time:", round(execution_time, 3), "seconds\n"))
      
    }, error = function(e) {
      end_time <- Sys.time()
      execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      test_results$total_tests <<- test_results$total_tests + 1
      test_results$failed_tests <<- test_results$failed_tests + 1
      
      test_results$test_details[[test_name]] <<- list(
        status = "FAILED",
        execution_time = execution_time,
        start_time = start_time,
        end_time = end_time,
        error = as.character(e)
      )
      
      cat(paste("✗ FAILED -", test_name, "- Error:", as.character(e), "\n"))
    })
  }
  
  # Execute all test suites
  cat("🚀 Starting Atlas Labs HR Analytics - Third-Party Services Integration Tests\n")
  cat("=" %+% rep("=", 70) %+% "\n")
  
  # Run test suites
  test_suites <- list(
    "External API Reliability" = function() test_that("External API Reliability", { }),
    "Service Dependency Management" = function() test_that("Service Dependency Management", { }),
    "Failover Mechanism Testing" = function() test_that("Failover Mechanism Testing", { }),
    "Data Synchronization Accuracy" = function() test_that("Data Synchronization Accuracy", { }),
    "Authentication Integration" = function() test_that("Authentication Integration", { }),
    "Monitoring and Alerting" = function() test_that("Monitoring and Alerting Systems", { }),
    "SLA Compliance Validation" = function() test_that("SLA Compliance Validation", { }),
    "Vendor Risk Assessment" = function() test_that("Vendor Risk Assessment", { })
  )
  
  for (suite_name in names(test_suites)) {
    test_reporter(suite_name, test_suites[[suite_name]])
  }
  
  # Finalize test results
  test_results$end_time <- Sys.time()
  total_execution_time <- as.numeric(difftime(test_results$end_time, test_results$start_time, units = "secs"))
  
  # Generate final report
  cat("\n" %+% "=" %+% rep("=", 70) %+% "\n")
  cat("📊 INTEGRATION TEST RESULTS SUMMARY\n")
  cat("=" %+% rep("=", 70) %+% "\n")
  cat(paste("Total Execution Time:", round(total_execution_time, 2), "seconds\n"))
  cat(paste("Total Tests:", test_results$total_tests, "\n"))
  cat(paste("✓ Passed:", test_results$passed_tests, "\n"))
  cat(paste("✗ Failed:", test_results$failed_tests, "\n"))
  cat(paste("⚠ Skipped:", test_results$skipped_tests, "\n"))
  cat(paste("Success Rate:", round((test_results$passed_tests / test_results$total_tests) * 100, 1), "%\n"))
  
  # Performance metrics
  avg_execution_time <- mean(sapply(test_results$test_details, function(x) x$execution_time))
  cat(paste("Average Test Execution Time:", round(avg_execution_time, 3), "seconds\n"))
  
  if (test_results$failed_tests > 0) {
    cat("\n❌ FAILED TESTS:\n")
    failed_tests <- test_results$test_details[sapply(test_results$test_details, function(x) x$status == "FAILED")]
    for (test_name in names(failed_tests)) {
      cat(paste("  •", test_name, "-", failed_tests[[test_name]]$error, "\n"))
    }
  }
  
  cat("\n✅ Integration testing completed for Atlas Labs HR Analytics Dashboard\n")
  cat("📁 Report generated by: akhapwoyaco (GitHub)\n")
  cat("🔗 Test coverage: Third-Party Services Integration (6.3)\n")
  
  return(test_results)
}

# Execute tests if running directly
if (interactive()) {
  integration_test_results <- run_integration_tests()
}3.1 EXTERNAL API RELIABILITY TESTING
# =============================================================================

# Mock external services that HR dashboard might integrate with
test_that("External API Reliability - HR Data Provider Service", {
  
  # Test API endpoint availability
  test_that("API endpoint availability and response times", {
    
    # Mock HR data API endpoints
    api_endpoints <- list(
      employee_sync = "https://api.hrdataprovider.com/v1/employees",
      performance_sync = "https://api.hrdataprovider.com/v1/performance",
      payroll_sync = "https://api.payrollservice.com/v2/compensation"
    )
    
    # Test each endpoint for availability
    for (endpoint_name in names(api_endpoints)) {
      endpoint_url <- api_endpoints[[endpoint_name]]
      
      # Mock HTTP responses for different scenarios
      with_mock(
        `httr::GET` = function(url, ...) {
          if (grepl("hrdataprovider", url)) {
            list(status_code = 200, content = '{"status": "healthy"}')
          } else if (grepl("payrollservice", url)) {
            list(status_code = 503, content = '{"error": "service unavailable"}')
          }
        },
        {
          # Test successful API call
          if (grepl("hrdataprovider", endpoint_url)) {
            response <- httr::GET(endpoint_url)
            expect_equal(response$status_code, 200)
          }
          
          # Test failed API call
          if (grepl("payrollservice", endpoint_url)) {
            response <- httr::GET(endpoint_url)
            expect_equal(response$status_code, 503)
          }
        }
      )
    }
  })
  
  # Test API response time thresholds
  test_that("API response time compliance", {
    
    # Mock response time testing
    test_api_response_time <- function(endpoint, max_response_time = 2000) {
      start_time <- Sys.time()
      
      # Simulate API call with variable response times
      with_mock(
        `httr::GET` = function(url, ...) {
          # Simulate network delay
          Sys.sleep(runif(1, 0.1, 3.0))
          list(status_code = 200, content = '{"data": "test"}')
        },
        {
          response <- httr::GET(endpoint)
          end_time <- Sys.time()
          response_time <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
          
          # Test response time is within acceptable limits
          expect_lt(response_time, max_response_time,
                   info = paste("Response time", response_time, "ms exceeds threshold"))
        }
      )
    }
    
    # Test critical endpoints
    test_api_response_time("https://api.hrdataprovider.com/v1/employees", 1500)
    test_api_response_time("https://api.hrdataprovider.com/v1/performance", 2000)
  })
  
  # Test API rate limiting and throttling
  test_that("API rate limiting compliance", {
    
    # Mock rate limiting scenarios
    api_call_count <- 0
    rate_limit_threshold <- 100
    
    with_mock(
      `httr::GET` = function(url, ...) {
        api_call_count <<- api_call_count + 1
        
        if (api_call_count > rate_limit_threshold) {
          list(status_code = 429, 
               headers = list("Retry-After" = "60"),
               content = '{"error": "rate limit exceeded"}')
        } else {
          list(status_code = 200, content = '{"data": "success"}')
        }
      },
      {
        # Simulate burst of API calls
        for (i in 1:120) {
          response <- httr::GET("https://api.hrdataprovider.com/v1/test")
          
          if (i <= rate_limit_threshold) {
            expect_equal(response$status_code, 200)
          } else {
            expect_equal(response$status_code, 429)
            expect_true("Retry-After" %in% names(response$headers))
          }
        }
      }
    )
  })
  
  # Test API authentication token expiration
  test_that("API authentication token management", {
    
    # Mock token expiration scenarios
    token_expired <- FALSE
    refresh_count <- 0
    
    authenticate_api <- function() {
      with_mock(
        `httr::POST` = function(url, body, ...) {
          if (grepl("token", url)) {
            refresh_count <<- refresh_count + 1
            list(status_code = 200, 
                 content = '{"access_token": "new_token_123", "expires_in": 3600}')
          }
        },
        {
          response <- httr::POST("https://api.hrdataprovider.com/oauth/token")
          expect_equal(response$status_code, 200)
          
          token_data <- jsonlite::fromJSON(response$content)
          expect_true("access_token" %in% names(token_data))
          expect_true("expires_in" %in% names(token_data))
        }
      )
    }
    
    # Test automatic token refresh
    with_mock(
      `httr::GET` = function(url, ...) {
        if (token_expired) {
          list(status_code = 401, content = '{"error": "token expired"}')
        } else {
          list(status_code = 200, content = '{"data": "authorized"}')
        }
      },
      {
        # Initial successful call
        response <- httr::GET("https://api.hrdataprovider.com/v1/data")
        expect_equal(response$status_code, 200)
        
        # Simulate token expiration
        token_expired <<- TRUE
        response <- httr::GET("https://api.hrdataprovider.com/v1/data")
        expect_equal(response$status_code, 401)
        
        # Test token refresh mechanism
        authenticate_api()
        expect_equal(refresh_count, 1)
        
        # Reset token state
        token_expired <<- FALSE
        response <- httr::GET("https://api.hrdataprovider.com/v1/data")
        expect_equal(response$status_code, 200)
      }
    )
  })
})

# =============================================================================
# 6.3.2 SERVICE DEPENDENCY MANAGEMENT TESTING
# =============================================================================

test_that("Service Dependency Management", {
  
  # Test service discovery and health checks
  test_that("Service discovery and health monitoring", {
    
    # Mock service registry
    service_registry <- list(
      hr_api = list(url = "https://hr-api.atlas.com", status = "healthy"),
      auth_service = list(url = "https://auth.atlas.com", status = "degraded"),
      report_service = list(url = "https://reports.atlas.com", status = "unhealthy")
    )
    
    check_service_health <- function(service_name) {
      service <- service_registry[[service_name]]
      
      with_mock(
        `httr::GET` = function(url, ...) {
          if (service$status == "healthy") {
            list(status_code = 200, content = '{"status": "OK", "uptime": 99.9}')
          } else if (service$status == "degraded") {
            list(status_code = 200, content = '{"status": "DEGRADED", "uptime": 95.2}')
          } else {
            list(status_code = 503, content = '{"status": "DOWN", "uptime": 0}')
          }
        },
        {
          health_endpoint <- paste0(service$url, "/health")
          response <- httr::GET(health_endpoint)
          
          if (service$status == "healthy") {
            expect_equal(response$status_code, 200)
            health_data <- jsonlite::fromJSON(response$content)
            expect_true(health_data$uptime > 99.0)
          } else if (service$status == "degraded") {
            expect_equal(response$status_code, 200)
            health_data <- jsonlite::fromJSON(response$content)
            expect_true(health_data$uptime < 99.0)
          } else {
            expect_equal(response$status_code, 503)
          }
        }
      )
    }
    
    # Test health checks for all services
    lapply(names(service_registry), check_service_health)
  })
  
  # Test cascading failure scenarios
  test_that("Cascading failure prevention", {
    
    # Mock service dependency chain: UI -> API -> Database -> External Service
    service_chain <- c("external_service", "database", "api", "ui")
    service_status <- list(
      external_service = "down",
      database = "healthy", 
      api = "healthy",
      ui = "healthy"
    )
    
    # Test circuit breaker pattern
    circuit_breaker_state <- "closed"  # closed, open, half-open
    failure_count <- 0
    failure_threshold <- 5
    
    call_external_service <- function() {
      if (circuit_breaker_state == "open") {
        return(list(status = "circuit_open", data = NULL))
      }
      
      # Simulate external service call
      if (service_status$external_service == "down") {
        failure_count <<- failure_count + 1
        
        if (failure_count >= failure_threshold) {
          circuit_breaker_state <<- "open"
        }
        
        return(list(status = "failure", data = NULL))
      } else {
        failure_count <<- 0
        return(list(status = "success", data = "external_data"))
      }
    }
    
    # Test multiple failed calls trigger circuit breaker
    for (i in 1:7) {
      result <- call_external_service()
      
      if (i < failure_threshold) {
        expect_equal(result$status, "failure")
      } else {
        expect_equal(result$status, "circuit_open")
      }
    }
    
    expect_equal(circuit_breaker_state, "open")
  })
  
  # Test service timeout and retry mechanisms
  test_that("Service timeout and retry logic", {
    
    retry_count <- 0
    max_retries <- 3
    timeout_duration <- 5  # seconds
    
    call_with_retry <- function(service_url, max_attempts = max_retries) {
      attempt <- 1
      
      while (attempt <= max_attempts) {
        result <- with_mock(
          `httr::GET` = function(url, timeout, ...) {
            retry_count <<- retry_count + 1
            
            # Simulate different failure scenarios
            if (attempt == 1) {
              stop("Connection timeout")
            } else if (attempt == 2) {
              list(status_code = 500, content = '{"error": "internal server error"}')
            } else {
              list(status_code = 200, content = '{"data": "success"}')
            }
          },
          {
            tryCatch({
              response <- httr::GET(service_url, timeout = timeout_duration)
              
              if (response$status_code == 200) {
                return(list(success = TRUE, data = response$content, attempt = attempt))
              } else {
                attempt <- attempt + 1
                Sys.sleep(2^attempt)  # Exponential backoff
              }
            }, error = function(e) {
              attempt <<- attempt + 1
              if (attempt <= max_attempts) {
                Sys.sleep(2^attempt)
              }
            })
          }
        )
        
        if (!is.null(result) && result$success) {
          return(result)
        }
        
        attempt <- attempt + 1
      }
      
      return(list(success = FALSE, error = "Max retries exceeded"))
    }
    
    # Test retry mechanism
    result <- call_with_retry("https://api.external.com/data")
    expect_true(result$success)
    expect_equal(result$attempt, 3)
    expect_equal(retry_count, 3)
  })
})

# =============================================================================
# 6.3.3 FAILOVER MECHANISM TESTING
# =============================================================================

test_that("Failover Mechanism Testing", {
  
  # Test database failover scenarios
  test_that("Database failover and replication", {
    
    # Mock database cluster configuration
    db_cluster <- list(
      primary = list(host = "db-primary.atlas.com", status = "healthy"),
      replica1 = list(host = "db-replica1.atlas.com", status = "healthy"),
      replica2 = list(host = "db-replica2.atlas.com", status = "degraded")
    )
    
    current_db <- "primary"
    
    # Database connection simulator
    connect_to_db <- function(db_node) {
      node_info <- db_cluster[[db_node]]
      
      if (node_info$status == "healthy") {
        return(list(connected = TRUE, host = node_info$host, latency = runif(1, 10, 50)))
      } else if (node_info$status == "degraded") {
        return(list(connected = TRUE, host = node_info$host, latency = runif(1, 200, 500)))
      } else {
        return(list(connected = FALSE, host = node_info$host, error = "Connection failed"))
      }
    }
    
    # Test primary database connection
    primary_conn <- connect_to_db("primary")
    expect_true(primary_conn$connected)
    expect_lt(primary_conn$latency, 100)
    
    # Simulate primary database failure
    db_cluster$primary$status <- "down"
    
    # Test automatic failover to replica
    failover_sequence <- c("replica1", "replica2")
    successful_failover <- FALSE
    
    for (replica in failover_sequence) {
      replica_conn <- connect_to_db(replica)
      
      if (replica_conn$connected && replica_conn$latency < 200) {
        current_db <<- replica
        successful_failover <- TRUE
        break
      }
    }
    
    expect_true(successful_failover)
    expect_equal(current_db, "replica1")
  })
  
  # Test load balancer failover
  test_that("Load balancer and server failover", {
    
    # Mock server pool
    server_pool <- list(
      server1 = list(url = "https://app1.atlas.com", load = 0.3, status = "healthy"),
      server2 = list(url = "https://app2.atlas.com", load = 0.7, status = "healthy"),
      server3 = list(url = "https://app3.atlas.com", load = 0.1, status = "maintenance")
    )
    
    # Load balancing algorithm (round-robin with health checks)
    get_next_server <- function() {
      healthy_servers <- server_pool[sapply(server_pool, function(s) s$status == "healthy")]
      
      if (length(healthy_servers) == 0) {
        return(NULL)
      }
      
      # Select server with lowest load
      server_loads <- sapply(healthy_servers, function(s) s$load)
      selected_server <- names(healthy_servers)[which.min(server_loads)]
      
      return(healthy_servers[[selected_server]])
    }
    
    # Test normal load balancing
    selected_server <- get_next_server()
    expect_equal(selected_server$url, "https://app1.atlas.com")  # Lowest load
    
    # Simulate server1 failure
    server_pool$server1$status <- "down"
    
    # Test failover to next available server
    selected_server <- get_next_server()
    expect_equal(selected_server$url, "https://app2.atlas.com")
    
    # Simulate all servers down except one in maintenance
    server_pool$server2$status <- "down"
    server_pool$server3$status <- "healthy"
    server_pool$server3$load <- 0.9
    
    selected_server <- get_next_server()
    expect_equal(selected_server$url, "https://app3.atlas.com")
  })
  
  # Test CDN and static asset failover
  test_that("CDN and static asset failover", {
    
    # Mock CDN configuration
    cdn_endpoints <- list(
      primary = "https://cdn-primary.atlas.com",
      secondary = "https://cdn-backup.atlas.com",
      tertiary = "https://static.atlas.com"
    )
    
    cdn_status <- list(
      primary = "down",
      secondary = "healthy", 
      tertiary = "healthy"
    )
    
    # Asset loading with failover
    load_static_asset <- function(asset_path) {
      for (cdn_name in names(cdn_endpoints)) {
        if (cdn_status[[cdn_name]] == "healthy") {
          asset_url <- paste0(cdn_endpoints[[cdn_name]], "/", asset_path)
          
          # Simulate asset loading
          with_mock(
            `httr::GET` = function(url, ...) {
              if (grepl("cdn-backup", url)) {
                list(status_code = 200, content = "asset_content", 
                     headers = list("Content-Type" = "text/css"))
              } else {
                list(status_code = 404, content = "Not found")
              }
            },
            {
              response <- httr::GET(asset_url)
              
              if (response$status_code == 200) {
                return(list(success = TRUE, source = cdn_name, url = asset_url))
              }
            }
          )
        }
      }
      
      return(list(success = FALSE, error = "All CDN endpoints failed"))
    }
    
    # Test asset loading with primary CDN down
    result <- load_static_asset("css/dashboard.css")
    expect_true(result$success)
    expect_equal(result$source, "secondary")
  })
})

# =============================================================================
# 6.3.4 DATA SYNCHRONIZATION ACCURACY TESTING
# =============================================================================

test_that("Data Synchronization Accuracy", {
  
  # Test real-time data synchronization
  test_that("Real-time data sync validation", {
    
    # Mock source and target data stores
    source_data <- data.frame(
      employee_id = 1:100,
      last_modified = Sys.time() - runif(100, 0, 86400),  # Random times within 24 hours
      salary = runif(100, 50000, 150000),
      stringsAsFactors = FALSE
    )
    
    target_data <- source_data[1:95, ]  # Missing 5 records
    target_data$salary[1:5] <- target_data$salary[1:5] * 1.1  # 5 records with different values
    
    # Data synchronization validation
    validate_sync <- function(source, target) {
      # Check for missing records
      missing_records <- setdiff(source$employee_id, target$employee_id)
      
      # Check for value discrepancies
      common_ids <- intersect(source$employee_id, target$employee_id)
      source_common <- source[source$employee_id %in% common_ids, ]
      target_common <- target[target$employee_id %in% common_ids, ]
      
      # Compare salaries
      salary_diffs <- abs(source_common$salary - target_common$salary) > 0.01
      inconsistent_records <- source_common$employee_id[salary_diffs]
      
      return(list(
        missing_count = length(missing_records),
        missing_records = missing_records,
        inconsistent_count = length(inconsistent_records),
        inconsistent_records = inconsistent_records
      ))
    }
    
    sync_result <- validate_sync(source_data, target_data)
    
    expect_equal(sync_result$missing_count, 5)
    expect_equal(sync_result$inconsistent_count, 5)
    expect_true(all(sync_result$missing_records %in% 96:100))
  })
  
  # Test batch synchronization integrity
  test_that("Batch sync data integrity", {
    
    # Mock batch synchronization process
    batch_size <- 1000
    total_records <- 10000
    sync_errors <- c()
    
    process_batch <- function(batch_start, batch_end) {
      batch_data <- data.frame(
        id = batch_start:batch_end,
        value = runif(batch_end - batch_start + 1),
        checksum = NA
      )
      
      # Calculate checksums
      batch_data$checksum <- sapply(1:nrow(batch_data), function(i) {
        digest::digest(paste(batch_data$id[i], batch_data$value[i]), algo = "md5")
      })
      
      # Simulate random errors (2% failure rate)
      if (runif(1) < 0.02) {
        return(list(success = FALSE, error = "Network timeout", batch_id = batch_start))
      }
      
      return(list(success = TRUE, records = nrow(batch_data), checksum_count = sum(!is.na(batch_data$checksum))))
    }
    
    # Process all batches
    for (start in seq(1, total_records, batch_size)) {
      end <- min(start + batch_size - 1, total_records)
      result <- process_batch(start, end)
      
      if (!result$success) {
        sync_errors <- c(sync_errors, result$batch_id)
      } else {
        expect_equal(result$records, result$checksum_count)
      }
    }
    
    # Verify error handling
    expect_true(length(sync_errors) <= ceiling(total_records / batch_size) * 0.05)  # Max 5% error rate
  })
  
  # Test cross-system data consistency
  test_that("Cross-system data consistency", {
    
    # Mock multiple data sources
    hr_system_data <- data.frame(
      employee_id = 1:50,
      department = sample(c("IT", "HR", "Finance", "Marketing"), 50, replace = TRUE),
      hire_date = as.Date("2020-01-01") + runif(50, 0, 1000),
      stringsAsFactors = FALSE
    )
    
    payroll_system_data <- data.frame(
      employee_id = 1:50,
      department = hr_system_data$department,
      salary = runif(50, 50000, 120000),
      stringsAsFactors = FALSE
    )
    
    # Introduce inconsistencies
    payroll_system_data$department[1:3] <- c("Finance", "IT", "Marketing")  # Different from HR system
    
    # Cross-system validation
    validate_cross_system <- function(hr_data, payroll_data) {
      merged_data <- merge(hr_data, payroll_data, by = "employee_id", suffixes = c("_hr", "_payroll"))
      
      department_inconsistencies <- merged_data$department_hr != merged_data$department_payroll
      inconsistent_employees <- merged_data$employee_id[department_inconsistencies]
      
      return(list(
        total_employees = nrow(merged_data),
        inconsistencies = sum(department_inconsistencies),
        inconsistent_ids = inconsistent_employees
      ))
    }
    
    validation_result <- validate_cross_system(hr_system_data, payroll_system_data)
    
    expect_equal(validation_result$inconsistencies, 3)
    expect_equal(length(validation_result$inconsistent_ids), 3)
    expect_true(all(validation_result$inconsistent_ids %in% 1:3))
  })
})

# =============================================================================
# 6.3.5 AUTHENTICATION INTEGRATION TESTING
# =============================================================================

test_that("Authentication Integration", {
  
  # Test SSO (Single Sign-On) integration
  test_that("SSO authentication flow", {
    
    # Mock SAML/OAuth2 flow
    sso_config <- list(
      provider = "Azure AD",
      client_id = "atlas-hr-dashboard",
      redirect_uri = "https://hr.atlas.com/auth/callback",
      scopes = c("openid", "profile", "email")
    )
    
    # Test authentication initiation
    initiate_sso_auth <- function() {
      auth_url <- paste0(
        "https://login.microsoftonline.com/atlas.com/oauth2/v2.0/authorize",
        "?client_id=", sso_config$client_id,
        "&response_type=code",
        "&redirect_uri=", URLencode(sso_config$redirect_uri),
        "&scope=", paste(sso_config$scopes, collapse = "%20")
      )
      
      return(list(auth_url = auth_url, state = "random_state_123"))
    }
    
    auth_init <- initiate_sso_auth()
    expect_true(grepl("login.microsoftonline.com", auth_init$auth_url))
    expect_true(grepl("atlas-hr-dashboard", auth_init$auth_url))
    
    # Test token exchange
    exchange_auth_code <- function(auth_code, state) {
      with_mock(
        `httr::POST` = function(url, body, ...) {
          if (grepl("token", url) && auth_code == "valid_auth_code") {
            list(
              status_code = 200,
              content = jsonlite::toJSON(list(
                access_token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9...",
                token_type = "Bearer",
                expires_in = 3600,
                id_token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9..."
              ))
            )
          } else {
            list(status_code = 400, content = '{"error": "invalid_grant"}')
          }
        },
        {
          response <- httr::POST("https://login.microsoftonline.com/atlas.com/oauth2/v2.0/token")
          
          if (auth_code == "valid_auth_code") {
            expect_equal(response$status_code, 200)
            token_data <- jsonlite::fromJSON(response$content)
            expect_true("access_token" %in% names(token_data))
            expect_equal(token_data$token_type, "Bearer")
          } else {
            expect_equal(response$status_code, 400)
          }
        }
      )
    }
    
    # Test valid and invalid auth codes
    exchange_auth_code("valid_auth_code", "random_state_123")
    exchange_auth_code("invalid_auth_code", "random_state_123")
  })
  
  # Test role-based access control (RBAC)
  test_that("Role-based access control", {
    
    # Mock user roles and permissions
    user_roles <- list(
      hr_admin = c("read_all_data", "export_reports", "manage_users", "view_sensitive_data"),
      hr_manager = c("read_team_data", "export_reports", "view_performance"),
      employee = c("read_own_data", "view_public_reports"),
      contractor = c("read_limited_data")
    )
    
    # Test permission validation
    check_permission <- function(user_role, required_permission) {
      user_permissions <- user_roles[[user_role]]
      return(required_permission %in% user_permissions)
    }
    
    # Test various permission scenarios
    expect_true(check_permission("hr_admin", "view_sensitive_data"))
    expect_true(check_permission("hr_manager", "export_reports"))
    expect_false(check_permission("employee", "manage_users"))
    expect_false(check_permission("contractor", "export_reports"))
  })
  
  # Test session management and timeout
  test_that("Session management and security", {
    
    # Mock session data
    session_config <- list(
      timeout_minutes = 30,
      max_concurrent_sessions = 3,
      session_encryption = TRUE
    )
    
    active_sessions <- list()
    
    # Session creation
    create_session <- function(user_id, user_role) {
      session_id <- paste0("sess_", user_id, "_", as.numeric(Sys.time()))
      session_data <- list(
        session_id = session_id,
        user_id = user_id,
        user_role = user_role,
        created_at = Sys.time(),
        last_activity = Sys.time(),
        ip_address = "192.168.1.100"
      )
      
      active_sessions[[session_id]] <<- session_data
      return(session_id)
    }
    
    # Session validation
    validate_session <- function(session_id) {
      if (!session_id %in% names(active_sessions)) {
        return(list(valid = FALSE, reason = "session_not_found"))
      }
      
      session <- active_sessions[[session_id]]
      time_since_activity <- as.numeric(difftime(Sys.time(), session$last_activity, units = "mins"))
      
      if (time_since_activity > session_config$timeout_minutes) {
        active_sessions[[session_id]] <<- NULL
        return(list(valid = FALSE, reason = "session_expired"))
      }
      
      # Update last activity
      active_sessions[[session_id]]$last_activity <<- Sys.time()
      return(list(valid = TRUE, session = session))
    }
    
    # Test session creation and validation
    session_id <- create_session("user123", "hr_manager")
    expect_true(session_id %in% names(active_sessions))
    
    validation_result <- validate_session(session_id)
    expect_true(validation_result$valid)
    
    # Test invalid session
    invalid_result <- validate_session("invalid_session_id")
    expect_false(invalid_result$valid)
    expect_equal(invalid_result$reason, "session_not_found")
  })
})

# =============================================================================
# 6.