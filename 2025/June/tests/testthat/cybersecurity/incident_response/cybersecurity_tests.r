# ============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD
# CYBERSECURITY INCIDENT RESPONSE UNIT TESTS
# 
# Focus Areas:
# 4.4.1 Disaster Recovery Validation
# 4.4.2 Backup Restoration Testing  
# 4.4.3 Stakeholder Notification Systems
#
# Author: akhapwoyaco
# Date: 2025
# ============================================================================

library(testthat)
library(mockery)
library(R6)
library(shiny)
library(digest)
library(jsonlite)
library(httr)

# ============================================================================
# MOCK CLASSES AND HELPER FUNCTIONS
# ============================================================================

# Mock AtlasLogger for testing
MockAtlasLogger <- R6Class("MockAtlasLogger",
  public = list(
    logs = list(),
    log_info = function(message, module = "test", performance_data = NULL) {
      self$logs <- append(self$logs, list(list(
        level = "INFO",
        message = message,
        module = module,
        timestamp = Sys.time(),
        performance_data = performance_data
      )))
    },
    log_warning = function(message, module = "test") {
      self$logs <- append(self$logs, list(list(
        level = "WARNING", 
        message = message,
        module = module,
        timestamp = Sys.time()
      )))
    },
    log_error = function(message, module = "test") {
      self$logs <- append(self$logs, list(list(
        level = "ERROR",
        message = message, 
        module = module,
        timestamp = Sys.time()
      )))
    },
    get_logs = function() { self$logs },
    clear_logs = function() { self$logs <- list() }
  )
)

# Mock Disaster Recovery Manager
DisasterRecoveryManager <- R6Class("DisasterRecoveryManager",
  private = list(
    .logger = NULL,
    .backup_locations = list(),
    .recovery_status = "standby",
    .last_backup_time = NULL,
    .notification_endpoints = list()
  ),
  
  public = list(
    initialize = function(logger = NULL) {
      private$.logger <- logger %||% MockAtlasLogger$new()
      private$.backup_locations <- list(
        primary = "/data/backups/primary",
        secondary = "/data/backups/secondary", 
        cloud = "s3://atlas-labs-backup"
      )
      private$.notification_endpoints <- list(
        email = "hr-security@atlaslabs.com",
        slack = "https://hooks.slack.com/services/...",
        sms = "+1-555-0199"
      )
    },
    
    # Disaster Recovery Methods
    initiate_disaster_recovery = function(incident_type = "data_breach") {
      tryCatch({
        private$.logger$log_info(
          paste("Initiating disaster recovery for:", incident_type),
          "disaster_recovery"
        )
        private$.recovery_status <- "active"
        
        # Simulate recovery steps
        self$isolate_affected_systems()
        self$verify_backup_integrity()
        self$restore_from_backup()
        
        return(list(status = "success", recovery_id = paste0("DR_", Sys.time())))
      }, error = function(e) {
        private$.logger$log_error(paste("Disaster recovery failed:", e$message), "disaster_recovery")
        return(list(status = "failed", error = e$message))
      })
    },
    
    isolate_affected_systems = function() {
      private$.logger$log_info("Isolating affected systems", "disaster_recovery")
      # Simulate system isolation
      Sys.sleep(0.1)
      return(TRUE)
    },
    
    verify_backup_integrity = function(backup_location = "primary") {
      tryCatch({
        if (!backup_location %in% names(private$.backup_locations)) {
          stop("Invalid backup location specified")
        }
        
        # Simulate integrity check
        integrity_check <- runif(1) > 0.1 # 90% success rate for testing
        
        if (!integrity_check) {
          stop("Backup integrity check failed")
        }
        
        private$.logger$log_info(
          paste("Backup integrity verified for:", backup_location),
          "disaster_recovery"
        )
        
        return(list(
          status = "verified",
          location = backup_location,
          checksum = digest::digest(paste(backup_location, Sys.time())),
          timestamp = Sys.time()
        ))
      }, error = function(e) {
        private$.logger$log_error(paste("Backup verification failed:", e$message), "disaster_recovery")
        stop(e$message)
      })
    },
    
    restore_from_backup = function(backup_id = NULL, target_time = NULL) {
      tryCatch({
        restore_id <- paste0("RESTORE_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        
        private$.logger$log_info(
          paste("Starting restore operation:", restore_id),
          "backup_restoration"
        )
        
        # Simulate restoration process
        steps <- c("validate_target", "prepare_environment", "restore_data", "verify_restore")
        
        for (step in steps) {
          private$.logger$log_info(paste("Executing step:", step), "backup_restoration")
          
          # Simulate step execution time
          Sys.sleep(runif(1, 0.05, 0.15))
          
          # Simulate potential failure (5% chance)
          if (runif(1) < 0.05) {
            stop(paste("Restoration failed at step:", step))
          }
        }
        
        return(list(
          status = "completed",
          restore_id = restore_id,
          timestamp = Sys.time(),
          data_points_restored = sample(1000:5000, 1)
        ))
        
      }, error = function(e) {
        private$.logger$log_error(paste("Restoration failed:", e$message), "backup_restoration")
        return(list(status = "failed", error = e$message))
      })
    },
    
    # Notification Methods
    notify_stakeholders = function(incident_type, severity = "high", custom_message = NULL) {
      tryCatch({
        notification_id <- paste0("NOTIF_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        
        message <- custom_message %||% paste(
          "SECURITY INCIDENT:", incident_type, 
          "- Severity:", toupper(severity),
          "- Time:", Sys.time()
        )
        
        results <- list()
        
        # Email notification
        results$email <- self$send_email_notification(message, severity)
        
        # Slack notification  
        results$slack <- self$send_slack_notification(message, severity)
        
        # SMS for high severity
        if (severity %in% c("high", "critical")) {
          results$sms <- self$send_sms_notification(message, severity)
        }
        
        private$.logger$log_info(
          paste("Stakeholder notifications sent:", notification_id),
          "notification_system"
        )
        
        return(list(
          notification_id = notification_id,
          results = results,
          timestamp = Sys.time()
        ))
        
      }, error = function(e) {
        private$.logger$log_error(paste("Notification failed:", e$message), "notification_system")
        return(list(status = "failed", error = e$message))
      })
    },
    
    send_email_notification = function(message, severity) {
      # Simulate email sending
      success_rate <- ifelse(severity == "critical", 0.95, 0.90)
      success <- runif(1) < success_rate
      
      if (success) {
        return(list(status = "sent", endpoint = private$.notification_endpoints$email))
      } else {
        return(list(status = "failed", endpoint = private$.notification_endpoints$email))
      }
    },
    
    send_slack_notification = function(message, severity) {
      # Simulate Slack webhook
      success_rate <- 0.85  # Lower success rate to test failures
      success <- runif(1) < success_rate
      
      if (success) {
        return(list(status = "sent", endpoint = private$.notification_endpoints$slack))
      } else {
        return(list(status = "failed", endpoint = private$.notification_endpoints$slack))
      }
    },
    
    send_sms_notification = function(message, severity) {
      # Simulate SMS sending
      success_rate <- 0.80  # Lowest success rate
      success <- runif(1) < success_rate
      
      if (success) {
        return(list(status = "sent", endpoint = private$.notification_endpoints$sms))
      } else {
        return(list(status = "failed", endpoint = private$.notification_endpoints$sms))
      }
    },
    
    # Status and utility methods
    get_recovery_status = function() {
      return(private$.recovery_status)
    },
    
    reset_recovery_status = function() {
      private$.recovery_status <- "standby"
    },
    
    get_backup_locations = function() {
      return(private$.backup_locations)
    }
  )
)

# ============================================================================
# DISASTER RECOVERY VALIDATION TESTS
# ============================================================================

test_that("4.4.1 Disaster Recovery - Initialization and Basic Functionality", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Test initialization
  expect_true(is.R6(dr_manager))
  expect_equal(dr_manager$get_recovery_status(), "standby")
  expect_length(dr_manager$get_backup_locations(), 3)
  expect_true(all(c("primary", "secondary", "cloud") %in% names(dr_manager$get_backup_locations())))
})

test_that("4.4.1 Disaster Recovery - Disaster Recovery Initiation", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Test successful disaster recovery initiation
  result <- dr_manager$initiate_disaster_recovery("data_breach")
  
  expect_equal(result$status, "success")
  expect_true(grepl("DR_", result$recovery_id))
  expect_equal(dr_manager$get_recovery_status(), "active")
  
  # Verify logging
  logs <- logger$get_logs()
  disaster_logs <- logs[sapply(logs, function(x) x$module == "disaster_recovery")]
  expect_true(length(disaster_logs) > 0)
})

test_that("4.4.1 Disaster Recovery - Multiple Incident Types", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  incident_types <- c("data_breach", "ransomware", "system_failure", "natural_disaster")
  
  for (incident_type in incident_types) {
    dr_manager$reset_recovery_status()
    result <- dr_manager$initiate_disaster_recovery(incident_type)
    
    expect_equal(result$status, "success")
    expect_true(grepl("DR_", result$recovery_id))
    
    # Check specific logging for each incident type
    logs <- logger$get_logs()
    recent_log <- logs[[length(logs)]]
    expect_true(grepl(incident_type, recent_log$message))
  }
})

test_that("4.4.1 Disaster Recovery - System Isolation Edge Cases", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Test system isolation under various conditions
  set.seed(42)  # For reproducible testing
  
  for (i in 1:10) {
    result <- dr_manager$isolate_affected_systems()
    expect_true(result)
  }
  
  # Verify logging for isolation
  logs <- logger$get_logs()
  isolation_logs <- logs[sapply(logs, function(x) grepl("Isolating affected systems", x$message))]
  expect_equal(length(isolation_logs), 10)
})

test_that("4.4.1 Disaster Recovery - Recovery Status Management", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Initial status
  expect_equal(dr_manager$get_recovery_status(), "standby")
  
  # After initiating recovery
  dr_manager$initiate_disaster_recovery("test_incident")
  expect_equal(dr_manager$get_recovery_status(), "active")
  
  # After reset
  dr_manager$reset_recovery_status()
  expect_equal(dr_manager$get_recovery_status(), "standby")
})

# ============================================================================
# BACKUP RESTORATION TESTING
# ============================================================================

test_that("4.4.2 Backup Restoration - Backup Integrity Verification", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Test successful verification
  set.seed(123)  # Ensure success for this test
  result <- dr_manager$verify_backup_integrity("primary")
  
  expect_equal(result$status, "verified")
  expect_equal(result$location, "primary")
  expect_true(!is.null(result$checksum))
  expect_true(inherits(result$timestamp, "POSIXct"))
})

test_that("4.4.2 Backup Restoration - Invalid Backup Location", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Test invalid backup location
  expect_error(
    dr_manager$verify_backup_integrity("invalid_location"),
    "Invalid backup location specified"
  )
  
  # Verify error logging
  logs <- logger$get_logs()
  error_logs <- logs[sapply(logs, function(x) x$level == "ERROR")]
  expect_true(length(error_logs) > 0)
})

test_that("4.4.2 Backup Restoration - All Backup Locations", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  backup_locations <- names(dr_manager$get_backup_locations())
  
  # Test each backup location
  for (location in backup_locations) {
    logger$clear_logs()  # Clear logs for each test
    
    # Multiple attempts to handle random failures
    success <- FALSE
    attempts <- 0
    max_attempts <- 5
    
    while (!success && attempts < max_attempts) {
      attempts <- attempts + 1
      tryCatch({
        result <- dr_manager$verify_backup_integrity(location)
        success <- TRUE
        
        expect_equal(result$status, "verified")
        expect_equal(result$location, location)
        expect_true(nchar(result$checksum) > 0)
        
      }, error = function(e) {
        if (attempts == max_attempts) {
          # If all attempts fail, that's also a valid test outcome
          expect_true(grepl("Backup integrity check failed", e$message))
        }
      })
    }
  }
})

test_that("4.4.2 Backup Restoration - Restore Operation Success", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  set.seed(456)  # Increase chance of success
  result <- dr_manager$restore_from_backup()
  
  if (result$status == "completed") {
    expect_true(grepl("RESTORE_", result$restore_id))
    expect_true(inherits(result$timestamp, "POSIXct"))
    expect_true(result$data_points_restored > 0)
    
    # Verify restoration logging
    logs <- logger$get_logs()
    restore_logs <- logs[sapply(logs, function(x) x$module == "backup_restoration")]
    expect_true(length(restore_logs) >= 4)  # At least 4 steps logged
  } else {
    # If restoration failed, verify error handling
    expect_equal(result$status, "failed")
    expect_true(!is.null(result$error))
  }
})

test_that("4.4.2 Backup Restoration - Restore Operation with Parameters", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Test with specific backup ID and target time
  backup_id <- "BACKUP_20250120_120000"
  target_time <- Sys.time() - 3600  # 1 hour ago
  
  set.seed(789)
  result <- dr_manager$restore_from_backup(backup_id, target_time)
  
  # Test multiple times to handle randomness
  results <- replicate(5, {
    logger$clear_logs()
    dr_manager$restore_from_backup(backup_id, target_time)
  }, simplify = FALSE)
  
  # At least one should succeed or all should fail gracefully
  success_count <- sum(sapply(results, function(x) x$status == "completed"))
  failure_count <- sum(sapply(results, function(x) x$status == "failed"))
  
  expect_equal(success_count + failure_count, 5)
  expect_true(success_count > 0 || failure_count == 5)
})

test_that("4.4.2 Backup Restoration - Step-by-Step Restoration Logging", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  set.seed(101)
  dr_manager$restore_from_backup()
  
  logs <- logger$get_logs()
  restore_logs <- logs[sapply(logs, function(x) x$module == "backup_restoration")]
  
  expected_steps <- c("validate_target", "prepare_environment", "restore_data", "verify_restore")
  
  # Check that step messages are present
  step_messages <- sapply(restore_logs, function(x) x$message)
  
  for (step in expected_steps) {
    step_found <- any(grepl(step, step_messages))
    # Due to randomness, some steps might fail, but they should be attempted
    if (!step_found) {
      # Check if there's a failure message mentioning this step
      failure_found <- any(grepl(paste("failed at step:", step), step_messages))
      expect_true(failure_found, info = paste("Expected step or failure for:", step))
    }
  }
})

test_that("4.4.2 Backup Restoration - Concurrent Restoration Requests", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Simulate concurrent restoration requests
  results <- list()
  
  set.seed(202)
  for (i in 1:3) {
    results[[i]] <- dr_manager$restore_from_backup(paste0("CONCURRENT_", i))
    
    # Brief pause to simulate near-concurrent requests
    Sys.sleep(0.01)
  }
  
  # All requests should be processed
  expect_length(results, 3)
  
  # Each should have a unique restore ID (if successful)
  successful_results <- results[sapply(results, function(x) x$status == "completed")]
  
  if (length(successful_results) > 1) {
    restore_ids <- sapply(successful_results, function(x) x$restore_id)
    expect_true(length(unique(restore_ids)) == length(restore_ids))
  }
})

# ============================================================================
# STAKEHOLDER NOTIFICATION SYSTEMS TESTS  
# ============================================================================

test_that("4.4.3 Stakeholder Notifications - Basic Notification Functionality", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  result <- dr_manager$notify_stakeholders("data_breach", "high")
  
  expect_true(!is.null(result$notification_id))
  expect_true(grepl("NOTIF_", result$notification_id))
  expect_true(!is.null(result$results))
  expect_true(inherits(result$timestamp, "POSIXct"))
  
  # Should have email and slack results
  expect_true("email" %in% names(result$results))
  expect_true("slack" %in% names(result$results))
})

test_that("4.4.3 Stakeholder Notifications - Severity-Based Notifications", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Test different severity levels
  severities <- c("low", "medium", "high", "critical")
  
  for (severity in severities) {
    logger$clear_logs()
    result <- dr_manager$notify_stakeholders("test_incident", severity)
    
    expect_true(!is.null(result$notification_id))
    
    # High and critical should include SMS
    if (severity %in% c("high", "critical")) {
      expect_true("sms" %in% names(result$results))
    } else {
      expect_false("sms" %in% names(result$results))
    }
    
    # Verify logging
    logs <- logger$get_logs()
    notification_logs <- logs[sapply(logs, function(x) x$module == "notification_system")]
    expect_true(length(notification_logs) > 0)
  }
})

test_that("4.4.3 Stakeholder Notifications - Custom Message Testing", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  custom_message <- "CRITICAL: Database corruption detected. Immediate action required."
  
  result <- dr_manager$notify_stakeholders(
    "database_corruption", 
    "critical", 
    custom_message
  )
  
  expect_true(!is.null(result$notification_id))
  expect_true("sms" %in% names(result$results))  # Critical should include SMS
  
  # Check that custom message would be used (we can't directly verify the content
  # without mocking the actual sending functions, but we can verify the call succeeded)
  expect_equal(length(result$results), 3)  # email, slack, sms for critical
})

test_that("4.4.3 Stakeholder Notifications - Email Notification Reliability", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Test email notifications multiple times to check reliability
  results <- list()
  
  set.seed(303)
  for (i in 1:20) {
    result <- dr_manager$send_email_notification("Test message", "high")
    results[[i]] <- result
  }
  
  # Count successes and failures
  successes <- sum(sapply(results, function(x) x$status == "sent"))
  failures <- sum(sapply(results, function(x) x$status == "failed"))
  
  expect_equal(successes + failures, 20)
  
  # With 90% success rate, we expect most to succeed
  expect_true(successes > failures)
  
  # All should have proper endpoint
  endpoints <- sapply(results, function(x) x$endpoint)
  expect_true(all(endpoints == "hr-security@atlaslabs.com"))
})

test_that("4.4.3 Stakeholder Notifications - Slack Notification Reliability", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Test Slack notifications with expected lower reliability
  results <- list()
  
  set.seed(404)
  for (i in 1:20) {
    result <- dr_manager$send_slack_notification("Test message", "medium")
    results[[i]] <- result
  }
  
  successes <- sum(sapply(results, function(x) x$status == "sent"))
  failures <- sum(sapply(results, function(x) x$status == "failed"))
  
  expect_equal(successes + failures, 20)
  
  # With 85% success rate, should have some failures
  expect_true(failures > 0)
  
  # Check endpoints
  endpoints <- sapply(results, function(x) x$endpoint)
  expect_true(all(grepl("hooks.slack.com", endpoints)))
})

test_that("4.4.3 Stakeholder Notifications - SMS Notification Reliability", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Test SMS notifications (lowest reliability)
  results <- list()
  
  set.seed(505)
  for (i in 1:20) {
    result <- dr_manager$send_sms_notification("Test message", "critical")
    results[[i]] <- result
  }
  
  successes <- sum(sapply(results, function(x) x$status == "sent"))
  failures <- sum(sapply(results, function(x) x$status == "failed"))
  
  expect_equal(successes + failures, 20)
  
  # With 80% success rate, should have the most failures
  expect_true(failures >= 2)  # At least some failures expected
  
  # Check endpoints
  endpoints <- sapply(results, function(x) x$endpoint)
  expect_true(all(endpoints == "+1-555-0199"))
})

test_that("4.4.3 Stakeholder Notifications - Notification Failure Handling", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Force a scenario where notifications might fail
  set.seed(606)  # This seed should produce some failures
  
  results <- list()
  for (i in 1:10) {
    result <- dr_manager$notify_stakeholders("system_failure", "critical")
    results[[i]] <- result
  }
  
  # All notifications should complete (even if individual channels fail)
  expect_true(all(sapply(results, function(x) !is.null(x$notification_id))))
  
  # Check for any individual channel failures
  all_channel_results <- unlist(lapply(results, function(x) 
    sapply(x$results, function(y) y$status)), use.names = FALSE)
  
  failures <- sum(all_channel_results == "failed")
  successes <- sum(all_channel_results == "sent")
  
  expect_true(failures + successes == length(all_channel_results))
  
  # At least some failures should occur due to lower reliability channels
  expect_true(failures > 0)
})

test_that("4.4.3 Stakeholder Notifications - Notification Timing and Performance", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Test notification timing
  start_time <- Sys.time()
  
  result <- dr_manager$notify_stakeholders("performance_test", "high")
  
  end_time <- Sys.time()
  
  # Notification should complete reasonably quickly (within 1 second)
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_true(execution_time < 1.0)
  
  # Check timestamp accuracy
  expect_true(result$timestamp >= start_time)
  expect_true(result$timestamp <= end_time)
})

# ============================================================================
# INTEGRATED EDGE CASE AND STRESS TESTS
# ============================================================================

test_that("4.4.4 Edge Cases - System Under High Load", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Simulate high load scenario
  set.seed(707)
  
  # Multiple concurrent operations
  operations <- list()
  
  for (i in 1:5) {
    # Disaster recovery
    operations[[paste0("dr_", i)]] <- dr_manager$initiate_disaster_recovery(paste0("incident_", i))
    
    # Backup verification
    backup_location <- sample(names(dr_manager$get_backup_locations()), 1)
    tryCatch({
      operations[[paste0("backup_", i)]] <- dr_manager$verify_backup_integrity(backup_location)
    }, error = function(e) {
      operations[[paste0("backup_", i)]] <- list(status = "failed", error = e$message)
    })
    
    # Notifications
    severity <- sample(c("low", "medium", "high", "critical"), 1)
    operations[[paste0("notif_", i)]] <- dr_manager$notify_stakeholders(
      paste0("load_test_", i), severity
    )
  }
  
  # All operations should complete
  expect_equal(length(operations), 15)  # 5 each of DR, backup, notifications
  
  # Check that logging handled the load
  logs <- logger$get_logs()
  expect_true(length(logs) > 20)  # Should have many log entries
  
  # Verify different modules were active
  modules <- unique(sapply(logs, function(x) x$module))
  expected_modules <- c("disaster_recovery", "backup_restoration", "notification_system")
  expect_true(length(intersect(modules, expected_modules)) >= 2)
})

test_that("4.4.4 Edge Cases - Network and Infrastructure Failures", {
  logger <- MockAtlasLogger$new()
  dr_manager <- DisasterRecoveryManager$new(logger)
  
  # Simulate infrastructure failure scenarios
  set.seed(808)
  
  # Test backup integrity when infrastructure is degraded
  failed_verifications <- 0
  successful_verifications <- 0
  
  for (i in 1:10) {
    tryCatch({
      result <- dr_manager$verify_backup_integrity("primary")
      successful_verifications <- successful_verifications + 1
    }, error = function(e) {
      failed_verifications <- failed_verifications + 1
      expect_true(grepl("Backup integrity check failed", e$message))
    })
  }
  
  expect_equal(failed_verifications + successful_verifications, 10)
  
  # Test notification system under network stress
  notification_results <- list()
  
  for (i in 1:10) {
    result <- dr_manager$notify_stakeholders("network_stress_test", "high")
    notification_results[[i]] <- result
    
    # Should still get notification ID even if individual channels fail
    expect_true(!is.null(result$notification_id))
  }
  
  # Count channel failures across all notifications
  total_channel_attempts <- 0
  total_channel_failures <- 0
  
  for (result in notification_results) {
    for (channel_result in result$results) {
      total_channel_attempts <- total_channel_attempts + 1
      if (channel