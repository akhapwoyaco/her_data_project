# ==============================================================================
# ATLAS LABS HR ANALYTICS - CYBERSECURITY & INCIDENT RESPONSE UNIT TESTS
# ==============================================================================
# Comprehensive security testing framework covering:
# - Security incident simulation
# - Response plan validation  
# - Communication protocol testing
# - Recovery time objectives
# - Business continuity testing
# - Disaster recovery validation
# - Backup restoration testing
# - Stakeholder notification systems
# ==============================================================================

library(testthat)
library(shiny)
library(httr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(lubridate)
library(digest)
library(mockery)

# ==============================================================================
# SECURITY INCIDENT SIMULATION TESTS
# ==============================================================================

test_that("Security Incident Simulation - SQL Injection Attack", {
  
  # Setup mock database connection
  mock_db <- dbConnect(RSQLite::SQLite(), ":memory:")
  dbExecute(mock_db, "CREATE TABLE employees (id INTEGER, name TEXT, salary REAL)")
  dbExecute(mock_db, "INSERT INTO employees VALUES (1, 'John Doe', 50000)")
  
  # Test SQL injection attempts
  malicious_inputs <- c(
    "'; DROP TABLE employees; --",
    "1' OR '1'='1",
    "admin'/*",
    "1; DELETE FROM employees WHERE 1=1; --",
    "' UNION SELECT password FROM users --"
  )
  
  for (injection in malicious_inputs) {
    # Simulate parameterized query (secure)
    safe_query <- function(input) {
      stmt <- dbSendStatement(mock_db, "SELECT * FROM employees WHERE id = ?")
      dbBind(stmt, list(input))
      result <- dbFetch(stmt)
      dbClearResult(stmt)
      return(result)
    }
    
    # Test should not execute malicious code
    expect_error({
      result <- safe_query(injection)
    }, regexp = NA) # No error expected with proper parameterization
    
    # Verify table still exists
    expect_true(dbExistsTable(mock_db, "employees"))
    
    # Verify data integrity
    count <- dbGetQuery(mock_db, "SELECT COUNT(*) as count FROM employees")$count
    expect_equal(count, 1)
  }
  
  dbDisconnect(mock_db)
})

test_that("Security Incident Simulation - XSS Attack Prevention", {
  
  # Mock Shiny session for testing
  session <- MockShinySession$new()
  
  # XSS payload attempts
  xss_payloads <- c(
    "<script>alert('XSS')</script>",
    "javascript:alert('XSS')",
    "<img src='x' onerror='alert(\"XSS\")'>",
    "';alert(String.fromCharCode(88,83,83))//';alert(String.fromCharCode(88,83,83))//",
    "<svg onload=alert('XSS')>",
    "&#60;script&#62;alert('XSS')&#60;/script&#62;"
  )
  
  # Test input sanitization
  sanitize_input <- function(input) {
    # HTML entity encoding
    input <- gsub("<", "&lt;", input)
    input <- gsub(">", "&gt;", input)
    input <- gsub("\"", "&quot;", input)
    input <- gsub("'", "&#x27;", input)
    input <- gsub("/", "&#x2F;", input)
    return(input)
  }
  
  for (payload in xss_payloads) {
    sanitized <- sanitize_input(payload)
    
    # Verify dangerous characters are encoded
    expect_false(grepl("<script", sanitized, ignore.case = TRUE))
    expect_false(grepl("javascript:", sanitized, ignore.case = TRUE))
    expect_false(grepl("onerror", sanitized, ignore.case = TRUE))
    expect_false(grepl("onload", sanitized, ignore.case = TRUE))
    
    # Log security incident
    expect_message({
      message(sprintf("XSS attempt blocked: %s -> %s", payload, sanitized))
    })
  }
})

test_that("Security Incident Simulation - CSRF Attack Prevention", {
  
  # Generate CSRF token
  generate_csrf_token <- function(session_id) {
    paste0(session_id, "_", digest(paste0(session_id, Sys.time()), "md5"))
  }
  
  # Validate CSRF token
  validate_csrf_token <- function(token, session_id) {
    expected_prefix <- paste0(session_id, "_")
    return(startsWith(token, expected_prefix))
  }
  
  session_id <- "test_session_123"
  valid_token <- generate_csrf_token(session_id)
  
  # Test valid token
  expect_true(validate_csrf_token(valid_token, session_id))
  
  # Test invalid tokens
  invalid_tokens <- c(
    "malicious_token",
    "different_session_456_token",
    "",
    NULL
  )
  
  for (invalid_token in invalid_tokens) {
    if (!is.null(invalid_token)) {
      expect_false(validate_csrf_token(invalid_token, session_id))
    } else {
      expect_error(validate_csrf_token(invalid_token, session_id))
    }
  }
})

# ==============================================================================
# RESPONSE PLAN VALIDATION TESTS
# ==============================================================================

test_that("Incident Response Plan - Automated Detection", {
  
  # Mock security monitoring system
  SecurityMonitor <- R6::R6Class("SecurityMonitor",
    private = list(
      .alert_threshold = 5,
      .failed_attempts = 0,
      .last_reset = Sys.time()
    ),
    public = list(
      log_failed_attempt = function(ip_address, user_agent = "") {
        private$.failed_attempts <- private$.failed_attempts + 1
        
        # Log the attempt
        log_entry <- list(
          timestamp = Sys.time(),
          ip = ip_address,
          user_agent = user_agent,
          attempt_count = private$.failed_attempts
        )
        
        # Check if threshold exceeded
        if (private$.failed_attempts >= private$.alert_threshold) {
          self$trigger_security_alert(ip_address)
          return(TRUE) # Alert triggered
        }
        return(FALSE) # No alert
      },
      
      trigger_security_alert = function(ip_address) {
        # Simulate incident response activation
        alert <- list(
          alert_id = paste0("SEC_", format(Sys.time(), "%Y%m%d%H%M%S")),
          severity = "HIGH",
          ip_address = ip_address,
          threat_type = "Brute Force Attack",
          timestamp = Sys.time(),
          status = "ACTIVE"
        )
        
        message(sprintf("SECURITY ALERT: %s from IP %s", 
                       alert$threat_type, alert$ip_address))
        
        return(alert)
      },
      
      reset_counters = function() {
        private$.failed_attempts <- 0
        private$.last_reset <- Sys.time()
      }
    )
  )
  
  monitor <- SecurityMonitor$new()
  
  # Test normal activity (no alerts)
  for (i in 1:4) {
    alert_triggered <- monitor$log_failed_attempt("192.168.1.100")
    expect_false(alert_triggered)
  }
  
  # Test threshold breach
  expect_message({
    alert_triggered <- monitor$log_failed_attempt("192.168.1.100")
    expect_true(alert_triggered)
  }, "SECURITY ALERT")
})

test_that("Incident Response Plan - Escalation Procedures", {
  
  # Mock incident escalation system
  IncidentEscalation <- R6::R6Class("IncidentEscalation",
    private = list(
      .escalation_levels = c("L1_Support", "L2_Security", "L3_Management", "L4_Executive"),
      .response_times = c(15, 30, 60, 120) # minutes
    ),
    public = list(
      escalate_incident = function(severity, duration_minutes) {
        escalation_level <- 1
        
        # Determine escalation level based on severity and duration
        if (severity == "CRITICAL" || duration_minutes > 60) {
          escalation_level <- 4
        } else if (severity == "HIGH" || duration_minutes > 30) {
          escalation_level <- 3
        } else if (severity == "MEDIUM" || duration_minutes > 15) {
          escalation_level <- 2
        }
        
        return(list(
          level = private$.escalation_levels[escalation_level],
          expected_response_time = private$.response_times[escalation_level],
          escalated_at = Sys.time()
        ))
      }
    )
  )
  
  escalation <- IncidentEscalation$new()
  
  # Test escalation scenarios
  test_cases <- list(
    list(severity = "LOW", duration = 10, expected = "L1_Support"),
    list(severity = "MEDIUM", duration = 20, expected = "L2_Security"),
    list(severity = "HIGH", duration = 35, expected = "L3_Management"),
    list(severity = "CRITICAL", duration = 5, expected = "L4_Executive")
  )
  
  for (test_case in test_cases) {
    result <- escalation$escalate_incident(test_case$severity, test_case$duration)
    expect_equal(result$level, test_case$expected)
    expect_true(result$expected_response_time > 0)
  }
})

# ==============================================================================
# COMMUNICATION PROTOCOL TESTING
# ==============================================================================

test_that("Communication Protocol - Emergency Notification System", {
  
  # Mock notification system
  NotificationSystem <- R6::R6Class("NotificationSystem",
    private = list(
      .email_service = list(status = "active"),
      .sms_service = list(status = "active"),
      .slack_service = list(status = "active")
    ),
    public = list(
      send_emergency_notification = function(incident, stakeholders) {
        notifications_sent <- list()
        
        for (stakeholder in stakeholders) {
          # Try multiple channels
          channels_attempted <- c()
          
          # Email notification
          if (private$.email_service$status == "active") {
            email_result <- self$send_email(stakeholder$email, incident)
            channels_attempted <- c(channels_attempted, "email")
            if (email_result$success) {
              notifications_sent <- append(notifications_sent, list(list(
                stakeholder = stakeholder$name,
                channel = "email",
                status = "sent",
                timestamp = Sys.time()
              )))
            }
          }
          
          # SMS backup
          if (stakeholder$priority == "critical") {
            sms_result <- self$send_sms(stakeholder$phone, incident)
            channels_attempted <- c(channels_attempted, "sms")
            if (sms_result$success) {
              notifications_sent <- append(notifications_sent, list(list(
                stakeholder = stakeholder$name,
                channel = "sms", 
                status = "sent",
                timestamp = Sys.time()
              )))
            }
          }
        }
        
        return(notifications_sent)
      },
      
      send_email = function(email, incident) {
        # Simulate email sending
        if (grepl("@", email)) {
          return(list(success = TRUE, message_id = paste0("email_", runif(1))))
        }
        return(list(success = FALSE, error = "Invalid email"))
      },
      
      send_sms = function(phone, incident) {
        # Simulate SMS sending
        if (nchar(phone) >= 10) {
          return(list(success = TRUE, message_id = paste0("sms_", runif(1))))
        }
        return(list(success = FALSE, error = "Invalid phone"))
      }
    )
  )
  
  notification_system <- NotificationSystem$new()
  
  # Test stakeholder notification
  stakeholders <- list(
    list(name = "Security Team", email = "security@atlaslabs.com", 
         phone = "5551234567", priority = "critical"),
    list(name = "IT Manager", email = "it.manager@atlaslabs.com", 
         phone = "5559876543", priority = "high"),
    list(name = "HR Director", email = "invalid-email", 
         phone = "123", priority = "medium")
  )
  
  incident <- list(
    id = "INC_001",
    severity = "HIGH",
    description = "Potential data breach detected"
  )
  
  results <- notification_system$send_emergency_notification(incident, stakeholders)
  
  # Verify notifications were attempted
  expect_true(length(results) >= 2) # At least 2 successful notifications
  
  # Verify critical stakeholders got multiple channels
  critical_notifications <- Filter(function(x) x$stakeholder == "Security Team", results)
  expect_true(length(critical_notifications) >= 2) # Email + SMS
})

test_that("Communication Protocol - Status Page Updates", {
  
  # Mock status page system
  StatusPageManager <- R6::R6Class("StatusPageManager",
    private = list(
      .current_status = "operational",
      .incidents = list(),
      .maintenance_windows = list()
    ),
    public = list(
      update_status = function(new_status, message) {
        old_status <- private$.current_status
        private$.current_status <- new_status
        
        status_update <- list(
          timestamp = Sys.time(),
          old_status = old_status,
          new_status = new_status,
          message = message,
          update_id = paste0("status_", format(Sys.time(), "%Y%m%d%H%M%S"))
        )
        
        # Validate status transition
        valid_transitions <- list(
          "operational" = c("degraded", "maintenance", "outage"),
          "degraded" = c("operational", "outage", "maintenance"),
          "outage" = c("operational", "degraded"),
          "maintenance" = c("operational")
        )
        
        if (new_status %in% valid_transitions[[old_status]]) {
          return(list(success = TRUE, update = status_update))
        } else {
          return(list(success = FALSE, error = "Invalid status transition"))
        }
      },
      
      post_incident_update = function(incident_id, update_message) {
        update <- list(
          incident_id = incident_id,
          timestamp = Sys.time(),
          message = update_message,
          update_id = paste0("inc_update_", runif(1))
        )
        
        private$.incidents <- append(private$.incidents, list(update))
        
        return(update)
      },
      
      get_current_status = function() {
        return(private$.current_status)
      }
    )
  )
  
  status_manager <- StatusPageManager$new()
  
  # Test status transitions
  expect_equal(status_manager$get_current_status(), "operational")
  
  # Valid transition
  result <- status_manager$update_status("degraded", "Investigating performance issues")
  expect_true(result$success)
  expect_equal(result$update$new_status, "degraded")
  
  # Invalid transition
  result <- status_manager$update_status("maintenance", "Scheduled maintenance")
  expect_false(result$success)
  expect_true("error" %in% names(result))
  
  # Test incident updates
  incident_update <- status_manager$post_incident_update("INC_001", "Incident resolved")
  expect_true("update_id" %in% names(incident_update))
  expect_true("timestamp" %in% names(incident_update))
})

# ==============================================================================
# RECOVERY TIME OBJECTIVES (RTO) TESTING
# ==============================================================================

test_that("Recovery Time Objectives - Application Recovery", {
  
  # Mock application recovery system
  ApplicationRecovery <- R6::R6Class("ApplicationRecovery",
    private = list(
      .rto_targets = list(
        "critical" = 15,    # 15 minutes
        "high" = 60,        # 1 hour
        "medium" = 240,     # 4 hours
        "low" = 1440        # 24 hours
      ),
      .services = list(
        "database" = list(priority = "critical", status = "running"),
        "web_server" = list(priority = "critical", status = "running"),
        "auth_service" = list(priority = "high", status = "running"),
        "reporting" = list(priority = "medium", status = "running"),
        "analytics" = list(priority = "low", status = "running")
      )
    ),
    public = list(
      simulate_outage = function(service_name) {
        if (service_name %in% names(private$.services)) {
          private$.services[[service_name]]$status <- "down"
          private$.services[[service_name]]$outage_start <- Sys.time()
          
          return(list(
            success = TRUE,
            service = service_name,
            priority = private$.services[[service_name]]$priority,
            rto_target = private$.rto_targets[[private$.services[[service_name]]$priority]]
          ))
        }
        return(list(success = FALSE, error = "Service not found"))
      },
      
      attempt_recovery = function(service_name, recovery_duration_minutes) {
        if (service_name %in% names(private$.services) && 
            private$.services[[service_name]]$status == "down") {
          
          service <- private$.services[[service_name]]
          rto_target <- private$.rto_targets[[service$priority]]
          
          # Check if recovery meets RTO
          rto_met <- recovery_duration_minutes <= rto_target
          
          if (rto_met) {
            private$.services[[service_name]]$status <- "running"
            private$.services[[service_name]]$outage_end <- Sys.time()
          }
          
          return(list(
            success = TRUE,
            service = service_name,
            recovery_time = recovery_duration_minutes,
            rto_target = rto_target,
            rto_met = rto_met,
            status = ifelse(rto_met, "recovered", "recovery_failed")
          ))
        }
        return(list(success = FALSE, error = "Service not in outage state"))
      }
    )
  )
  
  recovery_system <- ApplicationRecovery$new()
  
  # Test RTO scenarios
  test_scenarios <- list(
    list(service = "database", recovery_time = 10, should_meet_rto = TRUE),
    list(service = "database", recovery_time = 30, should_meet_rto = FALSE),
    list(service = "auth_service", recovery_time = 45, should_meet_rto = TRUE),
    list(service = "reporting", recovery_time = 180, should_meet_rto = TRUE),
    list(service = "analytics", recovery_time = 1500, should_meet_rto = FALSE)
  )
  
  for (scenario in test_scenarios) {
    # Simulate outage
    outage_result <- recovery_system$simulate_outage(scenario$service)
    expect_true(outage_result$success)
    
    # Attempt recovery
    recovery_result <- recovery_system$attempt_recovery(
      scenario$service, 
      scenario$recovery_time
    )
    
    expect_true(recovery_result$success)
    expect_equal(recovery_result$rto_met, scenario$should_meet_rto)
    
    if (scenario$should_meet_rto) {
      expect_equal(recovery_result$status, "recovered")
    } else {
      expect_equal(recovery_result$status, "recovery_failed")
    }
  }
})

test_that("Recovery Time Objectives - Data Recovery", {
  
  # Mock data recovery system
  DataRecovery <- R6::R6Class("DataRecovery",
    private = list(
      .rpo_targets = list(  # Recovery Point Objectives
        "critical" = 5,     # 5 minutes data loss max
        "high" = 30,        # 30 minutes
        "medium" = 240,     # 4 hours
        "low" = 1440        # 24 hours
      ),
      .backup_locations = c("primary", "secondary", "offsite"),
      .last_backup = Sys.time() - minutes(10)
    ),
    public = list(
      check_data_currency = function(data_type, priority) {
        current_time <- Sys.time()
        data_age_minutes <- as.numeric(difftime(current_time, private$.last_backup, units = "mins"))
        rpo_target <- private$.rpo_targets[[priority]]
        
        return(list(
          data_type = data_type,
          priority = priority,
          data_age_minutes = data_age_minutes,
          rpo_target = rpo_target,
          rpo_met = data_age_minutes <= rpo_target,
          last_backup = private$.last_backup
        ))
      },
      
      simulate_data_recovery = function(backup_location, estimated_time_minutes) {
        recovery_start <- Sys.time()
        
        # Simulate different recovery scenarios
        success_rate <- switch(backup_location,
          "primary" = 0.95,
          "secondary" = 0.85,
          "offsite" = 0.75,
          0.5
        )
        
        recovery_successful <- runif(1) <= success_rate
        
        return(list(
          backup_location = backup_location,
          estimated_time = estimated_time_minutes,
          recovery_successful = recovery_successful,
          recovery_start = recovery_start,
          success_rate = success_rate
        ))
      }
    )
  )
  
  data_recovery <- DataRecovery$new()
  
  # Test RPO compliance
  rpo_tests <- list(
    list(data_type = "employee_data", priority = "critical"),
    list(data_type = "performance_data", priority = "high"),
    list(data_type = "reports", priority = "medium"),
    list(data_type = "logs", priority = "low")
  )
  
  for (test in rpo_tests) {
    rpo_check <- data_recovery$check_data_currency(test$data_type, test$priority)
    expect_true("rpo_met" %in% names(rpo_check))
    expect_true("data_age_minutes" %in% names(rpo_check))
    expect_true("rpo_target" %in% names(rpo_check))
  }
  
  # Test recovery from different locations
  backup_locations <- c("primary", "secondary", "offsite")
  for (location in backup_locations) {
    recovery_result <- data_recovery$simulate_data_recovery(location, 30)
    expect_true("recovery_successful" %in% names(recovery_result))
    expect_true(recovery_result$success_rate > 0)
  }
})

# ==============================================================================
# BUSINESS CONTINUITY TESTING
# ==============================================================================

test_that("Business Continuity - Critical Function Identification", {
  
  # Business continuity manager
  BusinessContinuity <- R6::R6Class("BusinessContinuity",
    private = list(
      .critical_functions = list(
        "employee_authentication" = list(
          priority = 1,
          dependencies = c("database", "auth_service"),
          max_downtime = 15 # minutes
        ),
        "payroll_processing" = list(
          priority = 1,
          dependencies = c("database", "calculation_engine"),
          max_downtime = 60
        ),
        "performance_reporting" = list(
          priority = 2,
          dependencies = c("database", "reporting_engine"),
          max_downtime = 240
        ),
        "analytics_dashboard" = list(
          priority = 3,
          dependencies = c("database", "web_server", "analytics_engine"),
          max_downtime = 480
        )
      )
    ),
    public = list(
      assess_business_impact = function(failed_services) {
        impacted_functions <- list()
        
        for (func_name in names(private$.critical_functions)) {
          func <- private$.critical_functions[[func_name]]
          
          # Check if any dependencies are in failed services
          dependencies_failed <- intersect(func$dependencies, failed_services)
          
          if (length(dependencies_failed) > 0) {
            impacted_functions[[func_name]] <- list(
              function_name = func_name,
              priority = func$priority,
              max_downtime = func$max_downtime,
              failed_dependencies = dependencies_failed,
              impact_level = self$calculate_impact_level(func$priority)
            )
          }
        }
        
        return(impacted_functions)
      },
      
      calculate_impact_level = function(priority) {
        switch(as.character(priority),
          "1" = "CRITICAL",
          "2" = "HIGH", 
          "3" = "MEDIUM",
          "LOW"
        )
      },
      
      generate_continuity_plan = function(impacted_functions) {
        plan <- list()
        
        # Sort by priority (1 = highest)
        sorted_functions <- impacted_functions[order(sapply(impacted_functions, function(x) x$priority))]
        
        for (func in sorted_functions) {
          plan[[func$function_name]] <- list(
            immediate_actions = self$get_immediate_actions(func$function_name),
            workaround_procedures = self$get_workaround_procedures(func$function_name),
            recovery_steps = self$get_recovery_steps(func$function_name),
            communication_plan = self$get_communication_plan(func$impact_level)
          )
        }
        
        return(plan)
      },
      
      get_immediate_actions = function(function_name) {
        actions <- switch(function_name,
          "employee_authentication" = c(
            "Enable emergency access procedures",
            "Activate backup authentication system",
            "Notify all employees of access procedures"
          ),
          "payroll_processing" = c(
            "Switch to manual payroll calculation",
            "Notify payroll team",
            "Prepare emergency payment procedures"
          ),
          c("Assess alternative solutions", "Notify stakeholders")
        )
        return(actions)
      },
      
      get_workaround_procedures = function(function_name) {
        return(c("Temporary manual process", "Alternative system usage"))
      },
      
      get_recovery_steps = function(function_name) {
        return(c("Restore primary systems", "Validate functionality", "Resume normal operations"))
      },
      
      get_communication_plan = function(impact_level) {
        return(switch(impact_level,
          "CRITICAL" = c("Immediate notification to executives", "All-hands communication"),
          "HIGH" = c("Notification to department heads", "Team communication"),
          "MEDIUM" = c("Standard incident notification"),
          c("Routine status update")
        ))
      }
    )
  )
  
  bc_manager <- BusinessContinuity$new()
  
  # Test business impact assessment
  failed_services <- c("database", "auth_service")
  impact_assessment <- bc_manager$assess_business_impact(failed_services)
  
  expect_true(length(impact_assessment) > 0)
  expect_true("employee_authentication" %in% names(impact_assessment))
  expect_equal(impact_assessment$employee_authentication$impact_level, "CRITICAL")
  
  # Test continuity plan generation
  continuity_plan <- bc_manager$generate_continuity_plan(impact_assessment)
  
  expect_true(length(continuity_plan) > 0)
  for (func_plan in continuity_plan) {
    expect_true("immediate_actions" %in% names(func_plan))
    expect_true("workaround_procedures" %in% names(func_plan))
    expect_true("recovery_steps" %in% names(func_plan))
    expect_true("communication_plan" %in% names(func_plan))
  }
})

# ==============================================================================
# DISASTER RECOVERY VALIDATION
# ==============================================================================

test_that("Disaster Recovery - Site Failover", {
  
  # Disaster recovery system
  DisasterRecovery <- R6::R6Class("DisasterRecovery",
    private = list(
      .primary_site = list(
        location = "Primary_DC",
        status = "active",
        services = c("web", "db", "auth", "analytics")
      ),
      .dr_site = list(
        location = "DR_DC", 
        status = "standby",
        services = c("web", "db", "auth")
      ),
      .failover_procedures = list(
        "database" = list(steps = 5, estimated_time = 20),
        "web_server" = list(steps = 3, estimated_time = 10),
        "auth_service" = list(steps = 4, estimated_time = 15)
      )
    ),
    public = list(
      initiate_failover = function(disaster_type) {
        failover_start <- Sys.time()
        
        # Simulate failover process
        failover_log <- list()
        total_estimated_time <- 0
        
        for (service in names(private$.failover_procedures)) {
          if (service %in% private$.dr_site$services) {
            procedure <- private$.failover_procedures[[service]]
            
            # Simulate each step
            for (step in 1:procedure$steps) {
              step_start <- Sys.time()
              
              # Simulate step execution (with potential delays)
              step_duration <- procedure$estimated_time / procedure$steps
              step_success <- runif(1) > 0.1 # 90% success rate per step
              
              failover_log <- append(failover_log, list(list(
                service = service,
                step = step,
                start_time = step_start,
                duration_minutes = step_duration,
                success = step_success
              )))
              
              if (!step_success) {
                # Retry logic
                retry_start <- Sys.time()
                retry_success <- runif(1) > 0.3 # 70% retry success rate
                
                failover_log <- append(failover_log, list(list(
                  service = service,
                  step = paste0(step, "_retry"),
                  start_time = retry_start,
                  duration_minutes = step_duration * 1.5,
                  success = retry_success
                )))
              }
            }
            
            total_estimated_time <- total_estimated_time + procedure$estimated_time
          }
        }
        
        # Update site status
        private$.primary_site$status <- "failed"
        private$.dr_site$status <- "active"
        
        failover_end <- Sys.time()
        actual_duration <- as.numeric(difftime(failover_end, failover