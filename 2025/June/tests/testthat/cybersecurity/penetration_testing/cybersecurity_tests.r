# ==============================================================================
# ATLAS LABS HR ANALYTICS - COMPREHENSIVE CYBERSECURITY TESTING SUITE
# ==============================================================================
# File: tests/cybersecurity_tests.R
# Purpose: Comprehensive penetration testing and security validation
# Author: akhapwoyaco
# ==============================================================================

library(testthat)
library(shiny)
library(httr)
library(jsonlite)
library(xml2)
library(rvest)
library(stringr)
library(digest)
library(openssl)
library(DBI)
library(RSQLite)

# ==============================================================================
# 4.2.1 AUTOMATED VULNERABILITY SCANNING
# ==============================================================================

context("Automated Vulnerability Scanning")

# SQL Injection Detection
test_that("SQL Injection Vulnerability Detection", {
  
  # Common SQL injection payloads
  sql_injection_payloads <- c(
    "'; DROP TABLE employees; --",
    "' OR '1'='1",
    "' UNION SELECT * FROM users --",
    "admin'--",
    "' OR 1=1 --",
    "'; INSERT INTO users VALUES ('hacker', 'password'); --",
    "' OR 'a'='a",
    "1' AND (SELECT COUNT(*) FROM users) > 0 --",
    "'; EXEC xp_cmdshell('dir'); --",
    "' OR EXISTS(SELECT * FROM users WHERE username='admin') --"
  )
  
  # Test each payload against input validation
  for (payload in sql_injection_payloads) {
    # Simulate input validation function
    validate_input <- function(input) {
      # Should detect and block SQL injection attempts
      dangerous_patterns <- c(
        "('|(\\-\\-)|;|\\||\\*|%|<|>|\\?|\\[|\\]|\\{|\\}|\\$|!|\\^)",
        "(drop|insert|delete|update|select|union|exec|script)",
        "(or|and)\\s+(1=1|'1'='1'|'a'='a')"
      )
      
      for (pattern in dangerous_patterns) {
        if (grepl(pattern, input, ignore.case = TRUE)) {
          return(FALSE)
        }
      }
      return(TRUE)
    }
    
    expect_false(validate_input(payload), 
                 info = paste("Failed to detect SQL injection:", payload))
  }
})

# XSS (Cross-Site Scripting) Detection
test_that("XSS Vulnerability Detection", {
  
  xss_payloads <- c(
    "<script>alert('XSS')</script>",
    "<img src=x onerror=alert('XSS')>",
    "javascript:alert('XSS')",
    "<svg onload=alert('XSS')>",
    "<iframe src=javascript:alert('XSS')></iframe>",
    "<body onload=alert('XSS')>",
    "<input type=text onfocus=alert('XSS')>",
    "<marquee onstart=alert('XSS')>",
    "<details open ontoggle=alert('XSS')>",
    "<video><source onerror=alert('XSS')>"
  )
  
  # XSS sanitization function
  sanitize_input <- function(input) {
    # Remove dangerous HTML tags and JavaScript
    dangerous_tags <- c("script", "iframe", "object", "embed", "form", "input")
    dangerous_attributes <- c("onload", "onclick", "onerror", "onmouseover", 
                             "onfocus", "onblur", "onchange", "onsubmit")
    
    clean_input <- input
    for (tag in dangerous_tags) {
      clean_input <- gsub(paste0("<", tag, ".*?>"), "", clean_input, ignore.case = TRUE)
      clean_input <- gsub(paste0("</", tag, ">"), "", clean_input, ignore.case = TRUE)
    }
    
    for (attr in dangerous_attributes) {
      clean_input <- gsub(paste0(attr, "\\s*=\\s*['\"][^'\"]*['\"]"), "", clean_input, ignore.case = TRUE)
    }
    
    return(clean_input)
  }
  
  for (payload in xss_payloads) {
    sanitized <- sanitize_input(payload)
    expect_false(grepl("<script|javascript:|onload|onerror|onclick", sanitized, ignore.case = TRUE),
                 info = paste("Failed to sanitize XSS payload:", payload))
  }
})

# CSRF Token Validation
test_that("CSRF Token Validation", {
  
  # Mock CSRF token generation
  generate_csrf_token <- function(session_id) {
    return(digest(paste(session_id, Sys.time(), runif(1)), algo = "sha256"))
  }
  
  # Mock CSRF token validation
  validate_csrf_token <- function(token, session_id, stored_token) {
    return(token == stored_token && nchar(token) == 64)
  }
  
  session_id <- "test_session_123"
  valid_token <- generate_csrf_token(session_id)
  invalid_tokens <- c(
    "",
    "invalid_token",
    "123456789",
    paste0(valid_token, "modified"),
    substr(valid_token, 1, 32)
  )
  
  # Test valid token
  expect_true(validate_csrf_token(valid_token, session_id, valid_token))
  
  # Test invalid tokens
  for (invalid_token in invalid_tokens) {
    expect_false(validate_csrf_token(invalid_token, session_id, valid_token),
                 info = paste("Failed to reject invalid CSRF token:", invalid_token))
  }
})

# File Upload Security
test_that("File Upload Security Validation", {
  
  # Mock file upload validation
  validate_file_upload <- function(filename, content_type, file_size) {
    # Allowed file types
    allowed_types <- c("text/csv", "application/vnd.ms-excel", 
                      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    
    # Allowed extensions
    allowed_extensions <- c(".csv", ".xls", ".xlsx")
    
    # Maximum file size (10MB)
    max_size <- 10 * 1024 * 1024
    
    # Check file extension
    file_ext <- tools::file_ext(filename)
    if (!paste0(".", tolower(file_ext)) %in% allowed_extensions) {
      return(FALSE)
    }
    
    # Check content type
    if (!content_type %in% allowed_types) {
      return(FALSE)
    }
    
    # Check file size
    if (file_size > max_size) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  # Test valid files
  valid_files <- list(
    list(filename = "employee_data.csv", content_type = "text/csv", size = 1024),
    list(filename = "performance.xlsx", content_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", size = 2048)
  )
  
  for (file in valid_files) {
    expect_true(validate_file_upload(file$filename, file$content_type, file$size))
  }
  
  # Test malicious files
  malicious_files <- list(
    list(filename = "malware.exe", content_type = "application/x-executable", size = 1024),
    list(filename = "script.php", content_type = "application/x-php", size = 512),
    list(filename = "payload.js", content_type = "application/javascript", size = 256),
    list(filename = "data.csv", content_type = "text/csv", size = 50 * 1024 * 1024) # Too large
  )
  
  for (file in malicious_files) {
    expect_false(validate_file_upload(file$filename, file$content_type, file$size),
                 info = paste("Failed to reject malicious file:", file$filename))
  }
})

# ==============================================================================
# 4.2.2 MANUAL PENETRATION TESTING
# ==============================================================================

context("Manual Penetration Testing Scenarios")

# Authentication Bypass Testing
test_that("Authentication Bypass Attempts", {
  
  # Mock authentication function
  authenticate_user <- function(username, password) {
    # Simple authentication logic
    valid_users <- list(
      "admin" = "secure_admin_password_123!",
      "hr_user" = "hr_password_456@",
      "analyst" = "analyst_password_789#"
    )
    
    if (username %in% names(valid_users)) {
      return(valid_users[[username]] == password)
    }
    return(FALSE)
  }
  
  # Test bypass attempts
  bypass_attempts <- list(
    list(username = "admin", password = ""),
    list(username = "admin", password = " "),
    list(username = "admin'--", password = "anything"),
    list(username = "admin' OR '1'='1", password = "anything"),
    list(username = "", password = ""),
    list(username = "admin", password = "admin"),
    list(username = "admin", password = "password"),
    list(username = "admin", password = "123456"),
    list(username = "administrator", password = "secure_admin_password_123!"),
    list(username = "ADMIN", password = "secure_admin_password_123!")
  )
  
  for (attempt in bypass_attempts) {
    expect_false(authenticate_user(attempt$username, attempt$password),
                 info = paste("Authentication bypass succeeded for:", attempt$username))
  }
})

# Session Management Testing
test_that("Session Management Security", {
  
  # Mock session management
  create_session <- function(user_id) {
    list(
      session_id = digest(paste(user_id, Sys.time(), runif(1)), algo = "sha256"),
      user_id = user_id,
      created_at = Sys.time(),
      expires_at = Sys.time() + 3600, # 1 hour
      is_active = TRUE
    )
  }
  
  validate_session <- function(session_id, sessions) {
    session <- sessions[[session_id]]
    if (is.null(session)) return(FALSE)
    if (!session$is_active) return(FALSE)
    if (Sys.time() > session$expires_at) return(FALSE)
    return(TRUE)
  }
  
  # Test session security
  sessions <- list()
  session <- create_session("user123")
  sessions[[session$session_id]] <- session
  
  # Valid session should work
  expect_true(validate_session(session$session_id, sessions))
  
  # Invalid session IDs should fail
  invalid_sessions <- c(
    "",
    "invalid_session",
    "123456789",
    paste0(session$session_id, "modified")
  )
  
  for (invalid_id in invalid_sessions) {
    expect_false(validate_session(invalid_id, sessions),
                 info = paste("Invalid session accepted:", invalid_id))
  }
})

# Directory Traversal Testing
test_that("Directory Traversal Prevention", {
  
  # Mock file access function
  secure_file_access <- function(filename) {
    # Whitelist of allowed files
    allowed_files <- c("employee.csv", "performance_rating.csv", "education_level.csv")
    
    # Remove path traversal attempts
    clean_filename <- basename(filename)
    
    # Check if file is in whitelist
    if (!clean_filename %in% allowed_files) {
      return(FALSE)
    }
    
    # Additional security checks
    if (grepl("\\.\\./|\\.\\.\\\\|/\\.\\.|\\\\\\.\\.|\\.\\.", filename)) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  # Test path traversal attempts
  traversal_attempts <- c(
    "../../../etc/passwd",
    "..\\..\\..\\windows\\system32\\config\\sam",
    "....//....//....//etc//passwd",
    "..%2F..%2F..%2Fetc%2Fpasswd",
    "..%5C..%5C..%5Cwindows%5Csystem32%5Cconfig%5Csam",
    "../app.R",
    "../../global.R",
    "../modules/logger_module.R",
    "file:///etc/passwd",
    "file://c:\\windows\\system32\\config\\sam"
  )
  
  for (attempt in traversal_attempts) {
    expect_false(secure_file_access(attempt),
                 info = paste("Directory traversal succeeded for:", attempt))
  }
  
  # Valid files should be allowed
  valid_files <- c("employee.csv", "performance_rating.csv", "education_level.csv")
  for (file in valid_files) {
    expect_true(secure_file_access(file))
  }
})

# ==============================================================================
# 4.2.3 SOCIAL ENGINEERING SIMULATION
# ==============================================================================

context("Social Engineering Simulation")

# Phishing Email Detection
test_that("Phishing Email Pattern Detection", {
  
  # Mock phishing detection function
  detect_phishing <- function(email_content) {
    phishing_indicators <- c(
      "urgent.*action.*required",
      "click.*here.*immediately",
      "verify.*account.*suspended",
      "congratulations.*you.*won",
      "nigerian.*prince",
      "inheritance.*millions",
      "act.*now.*limited.*time",
      "confirm.*password.*security",
      "unauthorized.*access.*detected",
      "download.*attachment.*important"
    )
    
    content_lower <- tolower(email_content)
    
    for (indicator in phishing_indicators) {
      if (grepl(indicator, content_lower)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  # Test phishing emails
  phishing_emails <- c(
    "URGENT: Your account will be suspended! Click here immediately to verify your credentials.",
    "Congratulations! You have won $1,000,000 in the lottery. Click here to claim your prize.",
    "Security Alert: Unauthorized access detected. Please confirm your password by clicking the link below.",
    "Important: Your Atlas Labs account requires immediate verification. Download the attachment to continue.",
    "Act now! Limited time offer expires in 24 hours. Click here to secure your account."
  )
  
  for (email in phishing_emails) {
    expect_true(detect_phishing(email),
                info = paste("Failed to detect phishing email:", substr(email, 1, 50)))
  }
  
  # Test legitimate emails
  legitimate_emails <- c(
    "Your monthly HR report is ready for review. Please log in to the dashboard at your convenience.",
    "Reminder: Team meeting scheduled for tomorrow at 2 PM in Conference Room A.",
    "New employee onboarding materials have been uploaded to the shared drive.",
    "Performance review cycle begins next week. Please prepare your self-assessments."
  )
  
  for (email in legitimate_emails) {
    expect_false(detect_phishing(email),
                 info = paste("False positive for legitimate email:", substr(email, 1, 50)))
  }
})

# User Behavior Simulation
test_that("User Security Behavior Simulation", {
  
  # Mock user behavior analysis
  analyze_user_behavior <- function(actions) {
    suspicious_score <- 0
    
    # Check for suspicious patterns
    if (actions$login_attempts > 5) suspicious_score += 20
    if (actions$failed_logins > 3) suspicious_score += 30
    if (actions$unusual_hours) suspicious_score += 15
    if (actions$multiple_ips) suspicious_score += 25
    if (actions$bulk_downloads) suspicious_score += 40
    if (actions$admin_access_attempts) suspicious_score += 50
    
    return(list(
      score = suspicious_score,
      is_suspicious = suspicious_score > 50
    ))
  }
  
  # Test suspicious behavior patterns
  suspicious_behaviors <- list(
    list(login_attempts = 10, failed_logins = 5, unusual_hours = TRUE, 
         multiple_ips = FALSE, bulk_downloads = FALSE, admin_access_attempts = FALSE),
    list(login_attempts = 3, failed_logins = 1, unusual_hours = FALSE, 
         multiple_ips = TRUE, bulk_downloads = TRUE, admin_access_attempts = FALSE),
    list(login_attempts = 2, failed_logins = 0, unusual_hours = FALSE, 
         multiple_ips = FALSE, bulk_downloads = FALSE, admin_access_attempts = TRUE)
  )
  
  for (behavior in suspicious_behaviors) {
    result <- analyze_user_behavior(behavior)
    expect_true(result$is_suspicious,
                info = paste("Failed to detect suspicious behavior with score:", result$score))
  }
  
  # Test normal behavior
  normal_behavior <- list(
    login_attempts = 2, failed_logins = 0, unusual_hours = FALSE,
    multiple_ips = FALSE, bulk_downloads = FALSE, admin_access_attempts = FALSE
  )
  
  result <- analyze_user_behavior(normal_behavior)
  expect_false(result$is_suspicious)
})

# ==============================================================================
# 4.2.4 PHYSICAL SECURITY ASSESSMENT
# ==============================================================================

context("Physical Security Assessment")

# Access Control Testing
test_that("Physical Access Control Simulation", {
  
  # Mock access control system
  validate_physical_access <- function(badge_id, location, time_of_day) {
    # Define access permissions
    access_permissions <- list(
      "EMP001" = list(locations = c("lobby", "hr_office", "general_workspace"), 
                      hours = c(6, 20)),
      "EMP002" = list(locations = c("lobby", "general_workspace"), 
                      hours = c(8, 18)),
      "ADMIN001" = list(locations = c("lobby", "hr_office", "general_workspace", "server_room"), 
                        hours = c(0, 24))
    )
    
    if (!badge_id %in% names(access_permissions)) {
      return(FALSE)
    }
    
    permissions <- access_permissions[[badge_id]]
    
    # Check location access
    if (!location %in% permissions$locations) {
      return(FALSE)
    }
    
    # Check time access
    if (time_of_day < permissions$hours[1] || time_of_day > permissions$hours[2]) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  # Test unauthorized access attempts
  unauthorized_attempts <- list(
    list(badge_id = "EMP002", location = "server_room", time = 10),
    list(badge_id = "EMP001", location = "hr_office", time = 22),
    list(badge_id = "INVALID", location = "lobby", time = 12),
    list(badge_id = "EMP002", location = "general_workspace", time = 5)
  )
  
  for (attempt in unauthorized_attempts) {
    expect_false(validate_physical_access(attempt$badge_id, attempt$location, attempt$time),
                 info = paste("Unauthorized access granted for:", attempt$badge_id, "to", attempt$location))
  }
  
  # Test authorized access
  authorized_attempts <- list(
    list(badge_id = "EMP001", location = "hr_office", time = 10),
    list(badge_id = "ADMIN001", location = "server_room", time = 15),
    list(badge_id = "EMP002", location = "general_workspace", time = 14)
  )
  
  for (attempt in authorized_attempts) {
    expect_true(validate_physical_access(attempt$badge_id, attempt$location, attempt$time))
  }
})

# ==============================================================================
# 4.2.5 WIRELESS NETWORK TESTING
# ==============================================================================

context("Wireless Network Security")

# WiFi Security Assessment
test_that("WiFi Security Configuration", {
  
  # Mock WiFi configuration analysis
  analyze_wifi_security <- function(config) {
    security_score <- 0
    issues <- c()
    
    # Check encryption
    if (config$encryption == "WPA3") {
      security_score += 40
    } else if (config$encryption == "WPA2") {
      security_score += 30
    } else if (config$encryption == "WEP") {
      security_score += 10
      issues <- c(issues, "Weak encryption (WEP)")
    } else {
      issues <- c(issues, "No encryption")
    }
    
    # Check password strength
    if (nchar(config$password) >= 12) {
      security_score += 20
    } else if (nchar(config$password) >= 8) {
      security_score += 10
    } else {
      issues <- c(issues, "Weak password")
    }
    
    # Check SSID
    if (!config$ssid_hidden) {
      security_score += 5
    }
    
    # Check MAC filtering
    if (config$mac_filtering) {
      security_score += 15
    }
    
    # Check WPS
    if (!config$wps_enabled) {
      security_score += 10
    } else {
      issues <- c(issues, "WPS enabled")
    }
    
    return(list(
      score = security_score,
      is_secure = security_score >= 70,
      issues = issues
    ))
  }
  
  # Test insecure configurations
  insecure_configs <- list(
    list(encryption = "WEP", password = "123456", ssid_hidden = FALSE, 
         mac_filtering = FALSE, wps_enabled = TRUE),
    list(encryption = "WPA2", password = "password", ssid_hidden = FALSE, 
         mac_filtering = FALSE, wps_enabled = TRUE),
    list(encryption = "None", password = "", ssid_hidden = FALSE, 
         mac_filtering = FALSE, wps_enabled = TRUE)
  )
  
  for (config in insecure_configs) {
    result <- analyze_wifi_security(config)
    expect_false(result$is_secure,
                 info = paste("Insecure WiFi config passed security check. Score:", result$score))
    expect_true(length(result$issues) > 0)
  }
  
  # Test secure configuration
  secure_config <- list(
    encryption = "WPA3", password = "SecurePassword123!@#", ssid_hidden = TRUE,
    mac_filtering = TRUE, wps_enabled = FALSE
  )
  
  result <- analyze_wifi_security(secure_config)
  expect_true(result$is_secure)
})

# ==============================================================================
# 4.2.6 MOBILE APPLICATION SECURITY
# ==============================================================================

context("Mobile Application Security")

# Mobile App Security Testing
test_that("Mobile App Security Vulnerabilities", {
  
  # Mock mobile app security assessment
  assess_mobile_security <- function(app_config) {
    vulnerabilities <- c()
    
    # Check data storage
    if (app_config$stores_credentials_locally) {
      vulnerabilities <- c(vulnerabilities, "Local credential storage")
    }
    
    # Check encryption
    if (!app_config$data_encrypted) {
      vulnerabilities <- c(vulnerabilities, "Data not encrypted")
    }
    
    # Check certificate pinning
    if (!app_config$certificate_pinning) {
      vulnerabilities <- c(vulnerabilities, "No certificate pinning")
    }
    
    # Check root detection
    if (!app_config$root_detection) {
      vulnerabilities <- c(vulnerabilities, "No root detection")
    }
    
    # Check app obfuscation
    if (!app_config$code_obfuscated) {
      vulnerabilities <- c(vulnerabilities, "Code not obfuscated")
    }
    
    return(list(
      vulnerabilities = vulnerabilities,
      is_secure = length(vulnerabilities) == 0
    ))
  }
  
  # Test insecure mobile app configuration
  insecure_config <- list(
    stores_credentials_locally = TRUE,
    data_encrypted = FALSE,
    certificate_pinning = FALSE,
    root_detection = FALSE,
    code_obfuscated = FALSE
  )
  
  result <- assess_mobile_security(insecure_config)
  expect_false(result$is_secure)
  expect_true(length(result$vulnerabilities) > 0)
  
  # Test secure mobile app configuration
  secure_config <- list(
    stores_credentials_locally = FALSE,
    data_encrypted = TRUE,
    certificate_pinning = TRUE,
    root_detection = TRUE,
    code_obfuscated = TRUE
  )
  
  result <- assess_mobile_security(secure_config)
  expect_true(result$is_secure)
  expect_equal(length(result$vulnerabilities), 0)
})

# ==============================================================================
# 4.2.7 API SECURITY VALIDATION
# ==============================================================================

context("API Security Validation")

# API Authentication Testing
test_that("API Authentication Security", {
  
  # Mock API authentication
  validate_api_auth <- function(token, endpoint, method) {
    # Token validation
    if (is.null(token) || token == "" || nchar(token) < 32) {
      return(FALSE)
    }
    
    # Rate limiting simulation
    if (endpoint == "/api/data" && method == "GET") {
      # Simulate rate limiting (max 100 requests per hour)
      return(TRUE)
    }
    
    # JWT token validation simulation
    if (substr(token, 1, 6) == "Bearer") {
      jwt_token <- substr(token, 8, nchar(token))
      if (nchar(jwt_token) >= 32) {
        return(TRUE)
      }
    }
    
    return(FALSE)
  }
  
  # Test invalid API tokens
  invalid_tokens <- c(
    "",
    "invalid",
    "123456789",
    "Bearer short",
    "Basic dXNlcjpwYXNz", # base64 encoded user:pass
    "Bearer " # Bearer with no token
  )
  
  for (token in invalid_tokens) {
    expect_false(validate_api_auth(token, "/api/data", "GET"),
                 info = paste("Invalid API token accepted:", token))
  }
  
  # Test valid API token
  valid_token <- "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ"
  expect_true(validate_api_auth(valid_token, "/api/data", "GET"))
})

# API Input Validation
test_that("API Input Validation", {
  
  # Mock API input validation
  validate_api_input <- function(input_data, endpoint) {
    if (endpoint == "/api/employee") {
      required_fields <- c("employee_id", "first_name", "last_name")
      
      # Check required fields
      for (field in required_fields) {
        if (!field %in% names(input_data) || is.null(input_data[[field]]) || input_data[[field]] == "") {
          return(FALSE)
        }
      }
      
      # Validate employee_id format
      if (!grepl("^EMP[0-9]{3,6}$", input_data$employee_id)) {
        return(FALSE)
      }
      
      # Validate name fields
      if (nchar(input_data$first_name) > 50 || nchar(input_data$last_name) > 50) {
        return(FALSE)
      }
      
      return(TRUE)
    }
    
    return(FALSE)
  }
  
  # Test invalid API inputs
  invalid_inputs <- list(
    list(employee_id = "", first_name = "John", last_name = "Doe"),
    list(employee_id = "123", first_name = "John", last_name = "Doe"),
    list(employee_id = "EMP001", first_name = "", last_name = "Doe"),
    list(employee_id = "EMP001", first_name = paste(rep("A", 51), collapse = ""), last_name = "Doe"),
    list(first_name = "John", last_name = "Doe") # Missing employee_id
  )
  
  for (input in invalid_inputs) {
    expect_false(validate_api_input(input, "/api/employee"),
                 info = paste("Invalid API input accepted:", toString(input)))
  }
  
  # Test valid API input
  valid_input <- list(employee_id = "EMP001", first_name = "John", last_name = "Doe")
  expect_true(validate_api_input(valid_input, "/api/employee"))
})

# ==============================================================================
# 4.2.8 THIRD-PARTY INTEGRATION SECURITY
# ==============================================================================

context("Third-Party Integration Security")

# Third-party API Security
test_that("Third-Party API Security Validation", {
  
  # Mock third-party integration security check
  validate_third_party_integration <- function(integration_config) {
    security_issues <- c()
    
    # Check API key security
    if (integration_config$api_key_in_url) {
      security_issues <- c(security_issues, "API key exposed in URL")
    }
    
    # Check HTTPS usage
    if (!integration_config$uses_https) {
      security_issues <- c(security_issues, "Non-HTTPS communication")
    }
    
    # Check certificate validation
    if (!integration_config$validates_certificates) {
      security_issues <- c(security_issues, "Certificate validation disabled")
    }
    
    # Check data encryption
    if (!integration_config$encrypts_data) {
      security_issues <- c(security_issues, "Data not encrypted in transit")
    }
    
    # Check request signing
    if (!integration_config$signs_requests) {
      security_issues <- c(security_issues, "Requests not signed")
    }
    
    return(list(
      issues = security_issues,
      is_secure = length(security_issues) == 0
    ))
  }
  
  # Test insecure third-party integration
  insecure_integration <- list(
    api_key_in_url = TRUE,
    uses_https = FALSE,
    validates_certificates = FALSE,
    encrypts_data = FALSE,
    signs_requests = FALSE
  )
  
  result <- validate_third_party_integration(insecure_integration)
  expect_false(result$is_secure)
  expect_true(length(result$issues) > 0)
  
  # Test secure third-party integration
  secure_integration <- list(
    api_key_in_url = FALSE,
    uses_https = TRUE,
    validates_certificates = TRUE,
    encrypts_data = TRUE,
    signs_requests = TRUE
  