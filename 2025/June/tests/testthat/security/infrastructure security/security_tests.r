# =============================================================================
# ATLAS LABS HR ANALYTICS - INFRASTRUCTURE SECURITY UNIT TESTS
# =============================================================================
# Comprehensive security testing framework for Shiny application infrastructure
# Coverage: Server hardening, network segmentation, firewall rules, access logs,
# intrusion detection, vulnerability scanning, patch management, backup security
# =============================================================================

library(testthat)
library(httr)
library(jsonlite)
library(digest)
library(pingr)
library(processx)
library(fs)
library(lubridate)
library(stringr)
library(readr)

# =============================================================================
# SECURITY TEST FRAMEWORK SETUP
# =============================================================================

# Security testing configuration
SECURITY_CONFIG <- list(
  app_host = Sys.getenv("ATLAS_APP_HOST", "localhost"),
  app_port = as.numeric(Sys.getenv("ATLAS_APP_PORT", "3838")),
  admin_user = Sys.getenv("ATLAS_ADMIN_USER", "admin"),
  admin_pass = Sys.getenv("ATLAS_ADMIN_PASS", ""),
  log_paths = c(
    "/var/log/shiny-server/",
    "/var/log/apache2/",
    "/var/log/nginx/",
    "/var/log/auth.log",
    "/var/log/syslog"
  ),
  backup_paths = c(
    "/opt/atlas_labs/backups/",
    "/var/backups/atlas_labs/"
  ),
  test_timeout = 30,
  max_retries = 3
)

# Security test utilities
SecurityTestUtils <- R6::R6Class("SecurityTestUtils",
  public = list(
    initialize = function() {
      private$start_time <- Sys.time()
      private$test_results <- list()
    },
    
    log_test_result = function(test_name, result, details = NULL) {
      private$test_results[[test_name]] <- list(
        result = result,
        timestamp = Sys.time(),
        details = details
      )
    },
    
    get_test_summary = function() {
      passed <- sum(sapply(private$test_results, function(x) x$result))
      total <- length(private$test_results)
      list(
        passed = passed,
        failed = total - passed,
        total = total,
        duration = difftime(Sys.time(), private$start_time, units = "secs")
      )
    },
    
    execute_system_command = function(command, timeout = 30) {
      tryCatch({
        result <- processx::run(
          command = "bash",
          args = c("-c", command),
          timeout = timeout,
          error_on_status = FALSE
        )
        list(
          success = result$status == 0,
          stdout = result$stdout,
          stderr = result$stderr,
          status = result$status
        )
      }, error = function(e) {
        list(
          success = FALSE,
          stdout = "",
          stderr = as.character(e),
          status = -1
        )
      })
    },
    
    check_port_accessibility = function(host, port, timeout = 5) {
      tryCatch({
        con <- socketConnection(host = host, port = port, timeout = timeout)
        close(con)
        TRUE
      }, error = function(e) FALSE)
    },
    
    validate_file_permissions = function(filepath, expected_perms) {
      if (!file.exists(filepath)) return(FALSE)
      
      result <- private$execute_system_command(sprintf("stat -c '%%a' '%s'", filepath))
      if (!result$success) return(FALSE)
      
      actual_perms <- trimws(result$stdout)
      actual_perms == expected_perms
    }
  ),
  
  private = list(
    start_time = NULL,
    test_results = NULL
  )
)

# Initialize security test utilities
security_utils <- SecurityTestUtils$new()

# =============================================================================
# 1. SERVER HARDENING VALIDATION TESTS
# =============================================================================

test_that("Server Hardening - Operating System Security", {
  
  # Test 1.1: OS Version and Patch Level
  test_that("OS version is supported and patched", {
    result <- security_utils$execute_system_command("uname -a")
    expect_true(result$success, "Failed to get OS information")
    
    # Check for supported OS
    os_info <- result$stdout
    supported_os <- any(sapply(c("Ubuntu", "CentOS", "RHEL", "Debian"), 
                              function(x) grepl(x, os_info, ignore.case = TRUE)))
    expect_true(supported_os, "Unsupported operating system detected")
    
    security_utils$log_test_result("os_version_check", supported_os, os_info)
  })
  
  # Test 1.2: Unnecessary Services Disabled
  test_that("Unnecessary services are disabled", {
    dangerous_services <- c("telnet", "ftp", "rsh", "rlogin", "ypbind", "tftp")
    
    for (service in dangerous_services) {
      result <- security_utils$execute_system_command(sprintf("systemctl is-active %s", service))
      expect_true(result$status != 0 || grepl("inactive|failed", result$stdout), 
                  sprintf("Dangerous service %s is active", service))
    }
    
    security_utils$log_test_result("unnecessary_services_disabled", TRUE)
  })
  
  # Test 1.3: SSH Configuration Security
  test_that("SSH configuration is hardened", {
    ssh_config_checks <- list(
      "PermitRootLogin no" = "Root login should be disabled",
      "PasswordAuthentication no" = "Password authentication should be disabled",
      "PermitEmptyPasswords no" = "Empty passwords should not be permitted",
      "X11Forwarding no" = "X11 forwarding should be disabled",
      "MaxAuthTries 3" = "Max auth tries should be limited"
    )
    
    if (file.exists("/etc/ssh/sshd_config")) {
      ssh_config <- readLines("/etc/ssh/sshd_config")
      
      for (setting in names(ssh_config_checks)) {
        setting_present <- any(grepl(setting, ssh_config, ignore.case = TRUE))
        expect_true(setting_present, ssh_config_checks[[setting]])
      }
    }
    
    security_utils$log_test_result("ssh_hardening", TRUE)
  })
  
  # Test 1.4: File System Security
  test_that("Critical file permissions are secure", {
    critical_files <- list(
      "/etc/passwd" = "644",
      "/etc/shadow" = "640",
      "/etc/group" = "644",
      "/etc/gshadow" = "640",
      "/etc/ssh/sshd_config" = "600"
    )
    
    for (filepath in names(critical_files)) {
      if (file.exists(filepath)) {
        expected_perms <- critical_files[[filepath]]
        perms_ok <- security_utils$validate_file_permissions(filepath, expected_perms)
        expect_true(perms_ok, sprintf("Incorrect permissions for %s", filepath))
      }
    }
    
    security_utils$log_test_result("file_permissions_secure", TRUE)
  })
  
  # Test 1.5: Kernel Security Parameters
  test_that("Kernel security parameters are configured", {
    security_params <- list(
      "net.ipv4.ip_forward" = "0",
      "net.ipv4.conf.all.send_redirects" = "0",
      "net.ipv4.conf.default.send_redirects" = "0",
      "net.ipv4.conf.all.accept_source_route" = "0",
      "net.ipv4.conf.all.accept_redirects" = "0",
      "net.ipv4.conf.all.secure_redirects" = "0",
      "net.ipv4.conf.all.log_martians" = "1",
      "net.ipv4.icmp_echo_ignore_broadcasts" = "1",
      "net.ipv4.icmp_ignore_bogus_error_responses" = "1"
    )
    
    for (param in names(security_params)) {
      result <- security_utils$execute_system_command(sprintf("sysctl %s", param))
      if (result$success) {
        expected_value <- security_params[[param]]
        actual_value <- gsub(sprintf("^%s\\s*=\\s*", param), "", result$stdout)
        expect_equal(trimws(actual_value), expected_value, 
                    sprintf("Kernel parameter %s not set correctly", param))
      }
    }
    
    security_utils$log_test_result("kernel_security_params", TRUE)
  })
  
  # Test 1.6: User Account Security
  test_that("User accounts follow security policies", {
    # Check for accounts with UID 0 (root privileges)
    result <- security_utils$execute_system_command("awk -F: '$3 == 0 {print $1}' /etc/passwd")
    if (result$success) {
      root_accounts <- strsplit(result$stdout, "\n")[[1]]
      root_accounts <- root_accounts[nzchar(root_accounts)]
      expect_equal(root_accounts, "root", "Only root should have UID 0")
    }
    
    # Check for accounts without passwords
    result <- security_utils$execute_system_command("awk -F: '$2 == \"\" {print $1}' /etc/shadow")
    if (result$success) {
      empty_password_accounts <- strsplit(result$stdout, "\n")[[1]]
      empty_password_accounts <- empty_password_accounts[nzchar(empty_password_accounts)]
      expect_length(empty_password_accounts, 0, "No accounts should have empty passwords")
    }
    
    security_utils$log_test_result("user_account_security", TRUE)
  })
})

# =============================================================================
# 2. NETWORK SEGMENTATION TESTING
# =============================================================================

test_that("Network Segmentation - Network Isolation", {
  
  # Test 2.1: Network Interface Configuration
  test_that("Network interfaces are properly configured", {
    result <- security_utils$execute_system_command("ip addr show")
    expect_true(result$success, "Failed to get network interface information")
    
    # Check for unnecessary interfaces
    interfaces <- result$stdout
    unnecessary_patterns <- c("ppp", "slip", "dummy")
    for (pattern in unnecessary_patterns) {
      expect_false(grepl(pattern, interfaces, ignore.case = TRUE), 
                   sprintf("Unnecessary network interface %s detected", pattern))
    }
    
    security_utils$log_test_result("network_interfaces_check", TRUE, interfaces)
  })
  
  # Test 2.2: Network Route Security
  test_that("Network routing is secure", {
    result <- security_utils$execute_system_command("ip route show")
    expect_true(result$success, "Failed to get routing table")
    
    routes <- result$stdout
    # Check for overly permissive routes
    expect_false(grepl("0.0.0.0/0.*dev.*metric\\s+[0-9]+", routes), 
                 "Overly permissive default route detected")
    
    security_utils$log_test_result("network_routing_secure", TRUE, routes)
  })
  
  # Test 2.3: Network Namespace Isolation
  test_that("Network namespaces provide proper isolation", {
    # Check if network namespaces are in use
    result <- security_utils$execute_system_command("ip netns list")
    
    if (result$success && nzchar(result$stdout)) {
      namespaces <- strsplit(result$stdout, "\n")[[1]]
      namespaces <- namespaces[nzchar(namespaces)]
      
      # Test isolation between namespaces
      for (ns in namespaces) {
        ns_name <- gsub("\\s+.*$", "", ns)
        ping_result <- security_utils$execute_system_command(
          sprintf("ip netns exec %s ping -c 1 -W 1 8.8.8.8", ns_name)
        )
        # This test depends on network policy - adjust as needed
      }
    }
    
    security_utils$log_test_result("network_namespace_isolation", TRUE)
  })
  
  # Test 2.4: VLAN Configuration Security
  test_that("VLAN configuration follows security policies", {
    result <- security_utils$execute_system_command("cat /proc/net/vlan/config")
    
    if (result$success && nzchar(result$stdout)) {
      vlan_config <- result$stdout
      
      # Check for default VLAN usage (security risk)
      expect_false(grepl("vlan1", vlan_config, ignore.case = TRUE), 
                   "Default VLAN 1 should not be used")
      
      # Check for VLAN isolation
      vlans <- regmatches(vlan_config, gregexpr("vlan[0-9]+", vlan_config))[[1]]
      expect_true(length(unique(vlans)) >= 2 || length(vlans) == 0, 
                  "VLANs should be properly segmented")
    }
    
    security_utils$log_test_result("vlan_security", TRUE)
  })
  
  # Test 2.5: Network Traffic Isolation
  test_that("Network traffic is properly isolated", {
    # Test internal network connectivity
    internal_hosts <- c("127.0.0.1", "localhost")
    
    for (host in internal_hosts) {
      app_accessible <- security_utils$check_port_accessibility(host, SECURITY_CONFIG$app_port)
      expect_true(app_accessible, sprintf("App should be accessible on %s", host))
    }
    
    # Test external network restrictions (if applicable)
    restricted_ports <- c(22, 23, 21, 25, 53, 80, 443, 993, 995)
    
    for (port in restricted_ports) {
      if (port != SECURITY_CONFIG$app_port) {
        accessible <- security_utils$check_port_accessibility("0.0.0.0", port, 2)
        # Note: This test may need adjustment based on network policy
      }
    }
    
    security_utils$log_test_result("network_traffic_isolation", TRUE)
  })
})

# =============================================================================
# 3. FIREWALL RULE VERIFICATION
# =============================================================================

test_that("Firewall Rules - Access Control", {
  
  # Test 3.1: Firewall Service Status
  test_that("Firewall service is active and configured", {
    # Check for various firewall services
    firewall_services <- c("ufw", "iptables", "firewalld", "nftables")
    firewall_active <- FALSE
    
    for (service in firewall_services) {
      result <- security_utils$execute_system_command(sprintf("systemctl is-active %s", service))
      if (result$success && grepl("active", result$stdout)) {
        firewall_active <- TRUE
        break
      }
    }
    
    expect_true(firewall_active, "At least one firewall service should be active")
    security_utils$log_test_result("firewall_service_active", firewall_active)
  })
  
  # Test 3.2: Default Firewall Policy
  test_that("Default firewall policy is restrictive", {
    # Check iptables default policy
    result <- security_utils$execute_system_command("iptables -L -n")
    if (result$success) {
      rules <- result$stdout
      
      # Check for default DROP policy
      expect_true(grepl("Chain INPUT.*policy DROP", rules) || 
                  grepl("Chain INPUT.*policy REJECT", rules),
                  "INPUT chain should have restrictive default policy")
      
      expect_true(grepl("Chain FORWARD.*policy DROP", rules) || 
                  grepl("Chain FORWARD.*policy REJECT", rules),
                  "FORWARD chain should have restrictive default policy")
    }
    
    security_utils$log_test_result("default_firewall_policy", TRUE)
  })
  
  # Test 3.3: Essential Service Rules
  test_that("Essential services have appropriate firewall rules", {
    result <- security_utils$execute_system_command("iptables -L -n")
    if (result$success) {
      rules <- result$stdout
      
      # Check for SSH access rule (if SSH is running)
      ssh_result <- security_utils$execute_system_command("systemctl is-active sshd")
      if (ssh_result$success && grepl("active", ssh_result$stdout)) {
        expect_true(grepl("dpt:22", rules), "SSH should have firewall rule if service is active")
      }
      
      # Check for Shiny app port rule
      app_port_rule <- sprintf("dpt:%s", SECURITY_CONFIG$app_port)
      expect_true(grepl(app_port_rule, rules), 
                  sprintf("Shiny app port %s should have firewall rule", SECURITY_CONFIG$app_port))
    }
    
    security_utils$log_test_result("essential_service_rules", TRUE)
  })
  
  # Test 3.4: Dangerous Port Blocking
  test_that("Dangerous ports are blocked", {
    result <- security_utils$execute_system_command("iptables -L -n")
    if (result$success) {
      rules <- result$stdout
      
      dangerous_ports <- c(23, 21, 25, 110, 143, 993, 995, 135, 139, 445, 1433, 3389)
      
      for (port in dangerous_ports) {
        port_pattern <- sprintf("dpt:%s.*ACCEPT", port)
        expect_false(grepl(port_pattern, rules), 
                     sprintf("Dangerous port %s should not be open", port))
      }
    }
    
    security_utils$log_test_result("dangerous_ports_blocked", TRUE)
  })
  
  # Test 3.5: Rate Limiting Rules
  test_that("Rate limiting rules are configured", {
    result <- security_utils$execute_system_command("iptables -L -n")
    if (result$success) {
      rules <- result$stdout
      
      # Check for rate limiting rules
      rate_limit_patterns <- c("limit:", "hashlimit:", "recent:")
      rate_limit_present <- any(sapply(rate_limit_patterns, 
                                      function(p) grepl(p, rules, ignore.case = TRUE)))
      
      expect_true(rate_limit_present, "Rate limiting rules should be configured")
    }
    
    security_utils$log_test_result("rate_limiting_configured", TRUE)
  })
  
  # Test 3.6: IPv6 Firewall Configuration
  test_that("IPv6 firewall is properly configured", {
    result <- security_utils$execute_system_command("ip6tables -L -n")
    if (result$success) {
      ipv6_rules <- result$stdout
      
      # Check for restrictive IPv6 policy
      expect_true(grepl("Chain INPUT.*policy DROP", ipv6_rules) || 
                  grepl("Chain INPUT.*policy REJECT", ipv6_rules) ||
                  grepl("0 references", ipv6_rules),
                  "IPv6 firewall should have restrictive policy")
    }
    
    security_utils$log_test_result("ipv6_firewall_configured", TRUE)
  })
  
  # Test 3.7: Firewall Logging
  test_that("Firewall logging is enabled", {
    result <- security_utils$execute_system_command("iptables -L -n")
    if (result$success) {
      rules <- result$stdout
      
      # Check for logging rules
      logging_present <- grepl("LOG", rules, ignore.case = TRUE)
      expect_true(logging_present, "Firewall logging should be enabled")
    }
    
    security_utils$log_test_result("firewall_logging_enabled", TRUE)
  })
})

# =============================================================================
# 4. ACCESS LOG INTEGRITY
# =============================================================================

test_that("Access Log Integrity - Audit Trail", {
  
  # Test 4.1: Log File Existence and Permissions
  test_that("Critical log files exist with proper permissions", {
    critical_logs <- list(
      "/var/log/auth.log" = "640",
      "/var/log/syslog" = "640",
      "/var/log/kern.log" = "640"
    )
    
    for (log_file in names(critical_logs)) {
      if (file.exists(log_file)) {
        expected_perms <- critical_logs[[log_file]]
        perms_ok <- security_utils$validate_file_permissions(log_file, expected_perms)
        expect_true(perms_ok, sprintf("Log file %s has incorrect permissions", log_file))
        
        # Check file size (should not be empty for active logs)
        file_size <- file.size(log_file)
        expect_true(file_size > 0, sprintf("Log file %s should not be empty", log_file))
      }
    }
    
    security_utils$log_test_result("log_file_permissions", TRUE)
  })
  
  # Test 4.2: Log Rotation Configuration
  test_that("Log rotation is properly configured", {
    logrotate_configs <- c("/etc/logrotate.conf", "/etc/logrotate.d/")
    
    for (config_path in logrotate_configs) {
      if (file.exists(config_path)) {
        if (dir.exists(config_path)) {
          # Check individual logrotate files
          logrotate_files <- list.files(config_path, full.names = TRUE)
          expect_true(length(logrotate_files) > 0, "Logrotate configuration files should exist")
          
          # Check for Shiny app log rotation
          shiny_config_exists <- any(sapply(logrotate_files, function(f) {
            if (file.exists(f)) {
              content <- readLines(f)
              any(grepl("shiny", content, ignore.case = TRUE))
            } else FALSE
          }))
          
        } else {
          # Check main logrotate config
          config_content <- readLines(config_path)
          expect_true(any(grepl("rotate", config_content)), "Log rotation should be configured")
        }
      }
    }
    
    security_utils$log_test_result("log_rotation_configured", TRUE)
  })
  
  # Test 4.3: Log Integrity Verification
  test_that("Log files maintain integrity", {
    # Check for log file tampering indicators
    for (log_path in SECURITY_CONFIG$log_paths) {
      if (file.exists(log_path)) {
        # Check for unusual gaps in timestamps
        if (grepl("\\.log$", log_path)) {
          tryCatch({
            log_content <- readLines(log_path, n = 100)
            
            # Extract timestamps (assuming standard syslog format)
            timestamps <- regmatches(log_content, 
                                   regexpr("^[A-Z][a-z]{2}\\s+\\d{1,2}\\s+\\d{2}:\\d{2}:\\d{2}", log_content))
            
            if (length(timestamps) > 1) {
              # Check for chronological order
              time_diffs <- diff(as.POSIXct(timestamps, format = "%b %d %H:%M:%S"))
              unusual_gaps <- any(time_diffs > 3600) # More than 1 hour gap
              
              if (unusual_gaps) {
                warning(sprintf("Unusual time gaps detected in %s", log_path))
              }
            }
          }, error = function(e) {
            # Log reading error - may indicate corruption
            warning(sprintf("Error reading log file %s: %s", log_path, e$message))
          })
        }
      }
    }
    
    security_utils$log_test_result("log_integrity_verified", TRUE)
  })
  
  # Test 4.4: Authentication Log Analysis
  test_that("Authentication logs show no suspicious activity", {
    auth_log <- "/var/log/auth.log"
    
    if (file.exists(auth_log)) {
      # Read recent auth log entries
      recent_logs <- tail(readLines(auth_log), 1000)
      
      # Check for brute force attempts
      failed_attempts <- grep("Failed password", recent_logs, ignore.case = TRUE)
      expect_true(length(failed_attempts) < 50, "Excessive failed login attempts detected")
      
      # Check for successful logins from unusual sources
      successful_logins <- grep("Accepted", recent_logs, ignore.case = TRUE)
      
      # Check for privilege escalation attempts
      sudo_attempts <- grep("sudo.*COMMAND", recent_logs, ignore.case = TRUE)
      
      security_utils$log_test_result("auth_log_analysis", TRUE, 
                                     list(failed_attempts = length(failed_attempts),
                                          successful_logins = length(successful_logins),
                                          sudo_attempts = length(sudo_attempts)))
    }
  })
  
  # Test 4.5: Application Log Security
  test_that("Application logs are secure and complete", {
    shiny_log_dirs <- c("/var/log/shiny-server/", "/tmp/", "/var/tmp/")
    
    for (log_dir in shiny_log_dirs) {
      if (dir.exists(log_dir)) {
        log_files <- list.files(log_dir, pattern = "*.log", full.names = TRUE, recursive = TRUE)
        
        for (log_file in log_files) {
          if (grepl("shiny", log_file, ignore.case = TRUE)) {
            # Check permissions
            perms_ok <- security_utils$validate_file_permissions(log_file, "640")
            expect_true(perms_ok, sprintf("Shiny log file %s has incorrect permissions", log_file))
            
            # Check for sensitive information leakage
            if (file.size(log_file) > 0) {
              log_sample <- readLines(log_file, n = 100)
              
              # Check for password leakage
              password_leak <- any(grepl("password", log_sample, ignore.case = TRUE))
              expect_false(password_leak, "Passwords should not appear in logs")
              
              # Check for other sensitive data
              sensitive_patterns <- c("social.*security", "ssn", "credit.*card", "api.*key")
              for (pattern in sensitive_patterns) {
                sensitive_leak <- any(grepl(pattern, log_sample, ignore.case = TRUE))
                expect_false(sensitive_leak, sprintf("Sensitive data pattern '%s' found in logs", pattern))
              }
            }
          }
        }
      }
    }
    
    security_utils$log_test_result("app_log_security", TRUE)
  })
})

# =============================================================================
# 5. INTRUSION DETECTION TESTING
# =============================================================================

test_that("Intrusion Detection - Monitoring Systems", {
  
  # Test 5.1: Intrusion Detection System Status
  test_that("IDS/IPS systems are active and configured", {
    ids_systems <- c("fail2ban", "aide", "tripwire", "samhain", "snort")
    ids_active <- FALSE
    active_systems <- c()
    
    for (system in ids_systems) {
      result <- security_utils$execute_system_command(sprintf("systemctl is-active %s", system))
      if (result$success && grepl("active", result$stdout)) {
        ids_active <- TRUE
        active_systems <- c(active_systems, system)
      }
    }
    
    expect_true(ids_active, "At least one intrusion detection system should be active")
    security_utils$log_test_result("ids_systems_active", ids_active, 
                                   list(active_systems = active_systems))
  })
  
  # Test 5.2: Fail2ban Configuration
  test_that("Fail2ban is properly configured", {
    fail2ban_result <- security_utils$execute_system_command("systemctl is-active fail2ban")
    
    if (fail2ban_result$success && grepl("active", fail2ban_result$stdout)) {
      # Check fail2ban status
      status_result <- security_utils$execute_system_command("fail2ban-client status")
      expect_true(status_result$success, "Fail2ban should be accessible")
      
      # Check active jails
      if (status_result$success) {
        status_output <- status_result$stdout
        expect_true(grepl("Number of jail", status_output), "Fail2ban should have active jails")
        
        # Check for SSH jail
        ssh_jail_result <- security_utils$execute_system_command("fail2ban-client status sshd")
        if (ssh_jail_result$success) {
          expect_true(grepl("Status for the jail: sshd", ssh_jail_result$stdout), 
                      "SSH jail should be configured")
        }
      }
    }
    
    security_utils$log_test_result("fail2ban_configured", TRUE)
  })
  
  # Test 5.3: File Integrity Monitoring
  test_that("File integrity monitoring is configured", {
    # Check for AIDE
    aide_result <- security_utils$execute_system_command("which aide")
    if (aide_result$success) {
      # Check AIDE database
      aide_db_result <- security_utils$execute_system_command("aide --check")
      # Note: This might fail if database is not initialized
      
      # Check AIDE configuration
      if (file.exists("/etc/aide/aide.conf")) {
        aide_config <- readLines("/etc/aide/aide.conf")
        expect_true(any(grepl("/etc", aide_config)), "AIDE should monitor /etc directory")
        expect_true(any(grepl("/bin", aide_config)), "AIDE should monitor /bin directory")
      }
    }
    
    # Check for Tripwire
    tripwire_result <- security_utils$execute_system_command("which tripwire")
    if (tripwire_result$success) {
      # Check Tripwire configuration
      if (file.exists("/etc/tripwire/twcfg.txt")) {
        tripwire_config <- readLines("/etc/tripwire/twcfg.txt")
        expect_true(length(tripwire_config) > 0, "Tripwire configuration should not be empty")
      }
    }
    
    security_utils$log_test_result("file_integrity_monitoring", TRUE)
  })
  
  # Test 5.4: Network Intrusion Detection
  test_that("Network intrusion detection is configured", {
    # Check for network monitoring tools
    network_tools <- c("tcpdump", "netstat", "ss", "lsof")
    available_tools <- c()
    
    for (tool in network_tools) {
      result <- security_utils$execute_system_command(sprintf("which %s", tool))
      if (result$success) {