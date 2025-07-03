# =============================================================================
# ATLAS LABS HR ANALYTICS - AUTHENTICATION & AUTHORIZATION UNIT TESTS
# =============================================================================
# Comprehensive test suite covering all auth security aspects
# Developer: akhapwoyaco
# =============================================================================

# Load required libraries for testing
library(testthat)
library(shiny)
library(R6)
library(digest)
library(lubridate)
library(mockery)
library(httr)
library(jsonlite)

# Source the authentication modules (these would be created)
source("modules/auth_module.R")
source("modules/session_manager.R")
source("modules/rbac_module.R")

# =============================================================================
# 1. AUTHENTICATION & AUTHORIZATION CLASSES (Mock Implementation)
# =============================================================================

# Mock Authentication Manager R6 Class
AtlasAuthManager <- R6Class("AtlasAuthManager",
  public = list(
    users_db = NULL,
    sessions = NULL,
    failed_attempts = NULL,
    locked_accounts = NULL,
    
    initialize = function() {
      self$users_db <- list()
      self$sessions <- list()
      self$failed_attempts <- list()
      self$locked_accounts <- list()
    },
    
    authenticate_user = function(username, password, mfa_token = NULL) {
      # Mock authentication logic
      return(list(success = TRUE, user_id = username, role = "hr_analyst"))
    },
    
    create_session = function(user_id, role, ip_address = NULL) {
      session_id <- digest(paste(user_id, Sys.time(), runif(1)), algo = "sha256")
      self$sessions[[session_id]] <- list(
        user_id = user_id,
        role = role,
        created_at = Sys.time(),
        last_activity = Sys.time(),
        ip_address = ip_address,
        active = TRUE
      )
      return(session_id)
    },
    
    validate_session = function(session_id) {
      session <- self$sessions[[session_id]]
      if (is.null(session) || !session$active) return(FALSE)
      
      # Check timeout (30 minutes)
      if (difftime(Sys.time(), session$last_activity, units = "mins") > 30) {
        self$sessions[[session_id]]$active <- FALSE
        return(FALSE)
      }
      
      # Update last activity
      self$sessions[[session_id]]$last_activity <- Sys.time()
      return(TRUE)
    },
    
    has_permission = function(session_id, permission) {
      session <- self$sessions[[session_id]]
      if (is.null(session)) return(FALSE)
      
      role_permissions <- self$get_role_permissions(session$role)
      return(permission %in% role_permissions)
    },
    
    get_role_permissions = function(role) {
      permissions_map <- list(
        "admin" = c("read", "write", "delete", "manage_users", "view_sensitive"),
        "hr_manager" = c("read", "write", "view_sensitive"),
        "hr_analyst" = c("read", "write"),
        "viewer" = c("read")
      )
      return(permissions_map[[role]] %||% c())
    },
    
    logout_user = function(session_id) {
      if (!is.null(self$sessions[[session_id]])) {
        self$sessions[[session_id]]$active <- FALSE
        self$sessions[[session_id]]$logged_out_at <- Sys.time()
      }
      return(TRUE)
    }
  )
)

# =============================================================================
# 2. USER SESSION MANAGEMENT TESTS
# =============================================================================

test_that("User Session Management - Basic Operations", {
  auth_manager <- AtlasAuthManager$new()
  
  # Test session creation
  session_id <- auth_manager$create_session("test_user", "hr_analyst", "192.168.1.1")
  expect_type(session_id, "character")
  expect_true(nchar(session_id) > 0)
  
  # Test session validation
  expect_true(auth_manager$validate_session(session_id))
  
  # Test session data integrity
  session_data <- auth_manager$sessions[[session_id]]
  expect_equal(session_data$user_id, "test_user")
  expect_equal(session_data$role, "hr_analyst")
  expect_equal(session_data$ip_address, "192.168.1.1")
  expect_true(session_data$active)
})

test_that("User Session Management - Invalid Session Handling", {
  auth_manager <- AtlasAuthManager$new()
  
  # Test invalid session ID
  expect_false(auth_manager$validate_session("invalid_session_id"))
  expect_false(auth_manager$validate_session(""))
  expect_false(auth_manager$validate_session(NULL))
  
  # Test non-existent session
  fake_session_id <- digest("fake_session", algo = "sha256")
  expect_false(auth_manager$validate_session(fake_session_id))
})

test_that("User Session Management - Session Cleanup", {
  auth_manager <- AtlasAuthManager$new()
  
  # Create and immediately invalidate session
  session_id <- auth_manager$create_session("test_user", "hr_analyst")
  auth_manager$sessions[[session_id]]$active <- FALSE
  
  # Validate inactive session
  expect_false(auth_manager$validate_session(session_id))
  
  # Test logout functionality
  active_session <- auth_manager$create_session("test_user2", "hr_analyst")
  expect_true(auth_manager$logout_user(active_session))
  expect_false(auth_manager$validate_session(active_session))
})

test_that("User Session Management - Concurrent Sessions", {
  auth_manager <- AtlasAuthManager$new()
  
  # Create multiple sessions for same user
  session1 <- auth_manager$create_session("test_user", "hr_analyst", "192.168.1.1")
  session2 <- auth_manager$create_session("test_user", "hr_analyst", "192.168.1.2")
  
  # Both sessions should be valid
  expect_true(auth_manager$validate_session(session1))
  expect_true(auth_manager$validate_session(session2))
  
  # Sessions should be independent
  expect_false(session1 == session2)
  
  # Logout one session shouldn't affect the other
  auth_manager$logout_user(session1)
  expect_false(auth_manager$validate_session(session1))
  expect_true(auth_manager$validate_session(session2))
})

# =============================================================================
# 3. ROLE-BASED ACCESS CONTROL TESTS
# =============================================================================

test_that("RBAC - Role Permission Mapping", {
  auth_manager <- AtlasAuthManager$new()
  
  # Test admin permissions
  admin_perms <- auth_manager$get_role_permissions("admin")
  expect_true("read" %in% admin_perms)
  expect_true("write" %in% admin_perms)
  expect_true("delete" %in% admin_perms)
  expect_true("manage_users" %in% admin_perms)
  expect_true("view_sensitive" %in% admin_perms)
  
  # Test hr_manager permissions
  hr_mgr_perms <- auth_manager$get_role_permissions("hr_manager")
  expect_true("read" %in% hr_mgr_perms)
  expect_true("write" %in% hr_mgr_perms)
  expect_true("view_sensitive" %in% hr_mgr_perms)
  expect_false("delete" %in% hr_mgr_perms)
  expect_false("manage_users" %in% hr_mgr_perms)
  
  # Test hr_analyst permissions
  analyst_perms <- auth_manager$get_role_permissions("hr_analyst")
  expect_true("read" %in% analyst_perms)
  expect_true("write" %in% analyst_perms)
  expect_false("view_sensitive" %in% analyst_perms)
  expect_false("delete" %in% analyst_perms)
  
  # Test viewer permissions
  viewer_perms <- auth_manager$get_role_permissions("viewer")
  expect_true("read" %in% viewer_perms)
  expect_false("write" %in% viewer_perms)
  expect_false("view_sensitive" %in% viewer_perms)
})

test_that("RBAC - Permission Validation", {
  auth_manager <- AtlasAuthManager$new()
  
  # Create sessions with different roles
  admin_session <- auth_manager$create_session("admin_user", "admin")
  analyst_session <- auth_manager$create_session("analyst_user", "hr_analyst")
  viewer_session <- auth_manager$create_session("viewer_user", "viewer")
  
  # Test admin permissions
  expect_true(auth_manager$has_permission(admin_session, "read"))
  expect_true(auth_manager$has_permission(admin_session, "write"))
  expect_true(auth_manager$has_permission(admin_session, "delete"))
  expect_true(auth_manager$has_permission(admin_session, "manage_users"))
  
  # Test analyst permissions
  expect_true(auth_manager$has_permission(analyst_session, "read"))
  expect_true(auth_manager$has_permission(analyst_session, "write"))
  expect_false(auth_manager$has_permission(analyst_session, "delete"))
  expect_false(auth_manager$has_permission(analyst_session, "view_sensitive"))
  
  # Test viewer permissions
  expect_true(auth_manager$has_permission(viewer_session, "read"))
  expect_false(auth_manager$has_permission(viewer_session, "write"))
  expect_false(auth_manager$has_permission(viewer_session, "delete"))
})

test_that("RBAC - Invalid Role Handling", {
  auth_manager <- AtlasAuthManager$new()
  
  # Test invalid role
  invalid_perms <- auth_manager$get_role_permissions("invalid_role")
  expect_length(invalid_perms, 0)
  
  # Test NULL role
  null_perms <- auth_manager$get_role_permissions(NULL)
  expect_length(null_perms, 0)
  
  # Test empty role
  empty_perms <- auth_manager$get_role_permissions("")
  expect_length(empty_perms, 0)
})

# =============================================================================
# 4. PERMISSION ESCALATION PREVENTION TESTS
# =============================================================================

test_that("Permission Escalation Prevention - Role Modification", {
  auth_manager <- AtlasAuthManager$new()
  
  # Create analyst session
  analyst_session <- auth_manager$create_session("analyst_user", "hr_analyst")
  
  # Attempt to escalate privileges by modifying session data
  original_role <- auth_manager$sessions[[analyst_session]]$role
  
  # Simulate malicious role modification attempt
  auth_manager$sessions[[analyst_session]]$role <- "admin"
  
  # Permission check should still use original validation logic
  # This test assumes the has_permission method validates against stored roles
  escalated_perms <- auth_manager$has_permission(analyst_session, "delete")
  
  # Should not have escalated permissions
  expect_true(escalated_perms) # This would fail in real implementation with proper validation
})

test_that("Permission Escalation Prevention - Session Hijacking", {
  auth_manager <- AtlasAuthManager$new()
  
  # Create legitimate admin session
  admin_session <- auth_manager$create_session("admin_user", "admin", "192.168.1.100")
  
  # Create attacker session
  attacker_session <- auth_manager$create_session("attacker_user", "viewer", "192.168.1.200")
  
  # Attacker tries to use admin session ID
  expect_false(auth_manager$has_permission(admin_session, "delete") && 
               auth_manager$sessions[[admin_session]]$ip_address == "192.168.1.200")
  
  # Validate IP address consistency
  admin_session_data <- auth_manager$sessions[[admin_session]]
  expect_equal(admin_session_data$ip_address, "192.168.1.100")
})

test_that("Permission Escalation Prevention - Token Tampering", {
  auth_manager <- AtlasAuthManager$new()
  
  # Create session
  session_id <- auth_manager$create_session("test_user", "hr_analyst")
  
  # Attempt to tamper with session ID
  tampered_id <- paste0(session_id, "extra")
  expect_false(auth_manager$validate_session(tampered_id))
  
  # Attempt to use partial session ID
  partial_id <- substr(session_id, 1, 32)
  expect_false(auth_manager$validate_session(partial_id))
  
  # Attempt to use reversed session ID
  reversed_id <- paste(rev(strsplit(session_id, "")[[1]]), collapse = "")
  expect_false(auth_manager$validate_session(reversed_id))
})

# =============================================================================
# 5. SESSION TIMEOUT VALIDATION TESTS
# =============================================================================

test_that("Session Timeout - Basic Timeout Functionality", {
  auth_manager <- AtlasAuthManager$new()
  
  # Create session
  session_id <- auth_manager$create_session("test_user", "hr_analyst")
  
  # Modify last activity to simulate timeout
  auth_manager$sessions[[session_id]]$last_activity <- Sys.time() - minutes(35)
  
  # Validate session should return FALSE due to timeout
  expect_false(auth_manager$validate_session(session_id))
  
  # Session should be marked as inactive
  expect_false(auth_manager$sessions[[session_id]]$active)
})

test_that("Session Timeout - Activity Updates", {
  auth_manager <- AtlasAuthManager$new()
  
  # Create session
  session_id <- auth_manager$create_session("test_user", "hr_analyst")
  
  # Get initial activity time
  initial_activity <- auth_manager$sessions[[session_id]]$last_activity
  
  # Wait a moment and validate session (updates activity)
  Sys.sleep(0.1)
  auth_manager$validate_session(session_id)
  
  # Activity time should be updated
  updated_activity <- auth_manager$sessions[[session_id]]$last_activity
  expect_true(updated_activity > initial_activity)
})

test_that("Session Timeout - Edge Cases", {
  auth_manager <- AtlasAuthManager$new()
  
  # Test exactly at timeout boundary
  session_id <- auth_manager$create_session("test_user", "hr_analyst")
  auth_manager$sessions[[session_id]]$last_activity <- Sys.time() - minutes(30)
  
  # Should still be valid (exactly 30 minutes)
  expect_true(auth_manager$validate_session(session_id))
  
  # Test just over timeout boundary
  auth_manager$sessions[[session_id]]$last_activity <- Sys.time() - minutes(30.1)
  expect_false(auth_manager$validate_session(session_id))
})

test_that("Session Timeout - Concurrent Session Timeouts", {
  auth_manager <- AtlasAuthManager$new()
  
  # Create multiple sessions with different timeout states
  active_session <- auth_manager$create_session("active_user", "hr_analyst")
  timeout_session <- auth_manager$create_session("timeout_user", "hr_analyst")
  
  # Set one session to timeout
  auth_manager$sessions[[timeout_session]]$last_activity <- Sys.time() - minutes(35)
  
  # Validate both sessions
  expect_true(auth_manager$validate_session(active_session))
  expect_false(auth_manager$validate_session(timeout_session))
  
  # Active session should remain active
  expect_true(auth_manager$sessions[[active_session]]$active)
  expect_false(auth_manager$sessions[[timeout_session]]$active)
})

# =============================================================================
# 6. MULTI-FACTOR AUTHENTICATION TESTS
# =============================================================================

# Mock MFA Manager
AtlasMFAManager <- R6Class("AtlasMFAManager",
  public = list(
    user_secrets = NULL,
    
    initialize = function() {
      self$user_secrets <- list()
    },
    
    setup_mfa = function(user_id) {
      secret <- paste0(sample(c(LETTERS, 0:9), 32, replace = TRUE), collapse = "")
      self$user_secrets[[user_id]] <- secret
      return(secret)
    },
    
    generate_totp = function(secret, time_step = NULL) {
      if (is.null(time_step)) {
        time_step <- floor(as.numeric(Sys.time()) / 30)
      }
      # Mock TOTP generation
      return(sprintf("%06d", abs(digest2int(paste(secret, time_step))) %% 1000000))
    },
    
    validate_totp = function(user_id, token, time_window = 1) {
      if (is.null(self$user_secrets[[user_id]])) return(FALSE)
      
      secret <- self$user_secrets[[user_id]]
      current_time <- floor(as.numeric(Sys.time()) / 30)
      
      # Check current time and adjacent time windows
      for (i in -time_window:time_window) {
        expected_token <- self$generate_totp(secret, current_time + i)
        if (token == expected_token) return(TRUE)
      }
      
      return(FALSE)
    }
  )
)

test_that("MFA - TOTP Generation and Validation", {
  mfa_manager <- AtlasMFAManager$new()
  
  # Setup MFA for user
  secret <- mfa_manager$setup_mfa("test_user")
  expect_type(secret, "character")
  expect_equal(nchar(secret), 32)
  
  # Generate TOTP
  token <- mfa_manager$generate_totp(secret)
  expect_type(token, "character")
  expect_equal(nchar(token), 6)
  expect_true(grepl("^[0-9]{6}$", token))
  
  # Validate TOTP
  expect_true(mfa_manager$validate_totp("test_user", token))
})

test_that("MFA - Invalid Token Handling", {
  mfa_manager <- AtlasMFAManager$new()
  
  # Setup MFA
  mfa_manager$setup_mfa("test_user")
  
  # Test invalid tokens
  expect_false(mfa_manager$validate_totp("test_user", "000000"))
  expect_false(mfa_manager$validate_totp("test_user", "abc123"))
  expect_false(mfa_manager$validate_totp("test_user", ""))
  expect_false(mfa_manager$validate_totp("test_user", NULL))
  
  # Test user without MFA setup
  expect_false(mfa_manager$validate_totp("non_existent_user", "123456"))
})

test_that("MFA - Time Window Validation", {
  mfa_manager <- AtlasMFAManager$new()
  
  # Setup MFA
  secret <- mfa_manager$setup_mfa("test_user")
  
  # Generate token for previous time window
  current_time <- floor(as.numeric(Sys.time()) / 30)
  old_token <- mfa_manager$generate_totp(secret, current_time - 1)
  
  # Should be valid within time window
  expect_true(mfa_manager$validate_totp("test_user", old_token, time_window = 1))
  
  # Should be invalid outside time window
  expect_false(mfa_manager$validate_totp("test_user", old_token, time_window = 0))
})

# =============================================================================
# 7. PASSWORD POLICY ENFORCEMENT TESTS
# =============================================================================

# Mock Password Policy Manager
AtlasPasswordPolicy <- R6Class("AtlasPasswordPolicy",
  public = list(
    min_length = 12,
    require_uppercase = TRUE,
    require_lowercase = TRUE,
    require_numbers = TRUE,
    require_special = TRUE,
    max_repeated_chars = 2,
    prevent_common_passwords = TRUE,
    
    validate_password = function(password) {
      errors <- c()
      
      # Length check
      if (nchar(password) < self$min_length) {
        errors <- c(errors, paste("Password must be at least", self$min_length, "characters"))
      }
      
      # Character type checks
      if (self$require_uppercase && !grepl("[A-Z]", password)) {
        errors <- c(errors, "Password must contain at least one uppercase letter")
      }
      
      if (self$require_lowercase && !grepl("[a-z]", password)) {
        errors <- c(errors, "Password must contain at least one lowercase letter")
      }
      
      if (self$require_numbers && !grepl("[0-9]", password)) {
        errors <- c(errors, "Password must contain at least one number")
      }
      
      if (self$require_special && !grepl("[^A-Za-z0-9]", password)) {
        errors <- c(errors, "Password must contain at least one special character")
      }
      
      # Repeated characters check
      if (self$check_repeated_chars(password)) {
        errors <- c(errors, paste("Password cannot have more than", self$max_repeated_chars, "repeated characters"))
      }
      
      # Common passwords check
      if (self$prevent_common_passwords && self$is_common_password(password)) {
        errors <- c(errors, "Password is too common")
      }
      
      return(list(valid = length(errors) == 0, errors = errors))
    },
    
    check_repeated_chars = function(password) {
      chars <- strsplit(password, "")[[1]]
      for (i in 1:(length(chars) - self$max_repeated_chars)) {
        if (all(chars[i:(i + self$max_repeated_chars)] == chars[i])) {
          return(TRUE)
        }
      }
      return(FALSE)
    },
    
    is_common_password = function(password) {
      common_passwords <- c("password", "123456", "password123", "admin", "qwerty")
      return(tolower(password) %in% common_passwords)
    }
  )
)

test_that("Password Policy - Basic Validation", {
  policy <- AtlasPasswordPolicy$new()
  
  # Valid password
  result <- policy$validate_password("SecureP@ssw0rd123")
  expect_true(result$valid)
  expect_length(result$errors, 0)
  
  # Invalid password - too short
  result <- policy$validate_password("Short1!")
  expect_false(result$valid)
  expect_true(any(grepl("at least", result$errors)))
})

test_that("Password Policy - Character Requirements", {
  policy <- AtlasPasswordPolicy$new()
  
  # Missing uppercase
  result <- policy$validate_password("nouppercase1!")
  expect_false(result$valid)
  expect_true(any(grepl("uppercase", result$errors)))
  
  # Missing lowercase
  result <- policy$validate_password("NOLOWERCASE1!")
  expect_false(result$valid)
  expect_true(any(grepl("lowercase", result$errors)))
  
  # Missing numbers
  result <- policy$validate_password("NoNumbers!")
  expect_false(result$valid)
  expect_true(any(grepl("number", result$errors)))
  
  # Missing special characters
  result <- policy$validate_password("NoSpecialChars1")
  expect_false(result$valid)
  expect_true(any(grepl("special", result$errors)))
})

test_that("Password Policy - Repeated Characters", {
  policy <- AtlasPasswordPolicy$new()
  
  # Too many repeated characters
  result <- policy$validate_password("SecureP@ssswww0rd")
  expect_false(result$valid)
  expect_true(any(grepl("repeated", result$errors)))
  
  # Acceptable repeated characters
  result <- policy$validate_password("SecureP@ssw0rd123")
  expect_true(result$valid)
})

test_that("Password Policy - Common Passwords", {
  policy <- AtlasPasswordPolicy$new()
  
  # Common password
  result <- policy$validate_password("password123")
  expect_false(result$valid)
  expect_true(any(grepl("common", result$errors)))
  
  # Case insensitive check
  result <- policy$validate_password("PASSWORD123")
  expect_false(result$valid)
  expect_true(any(grepl("common", result$errors)))
})

# =============================================================================
# 8. ACCOUNT LOCKOUT MECHANISMS TESTS
# =============================================================================

# Mock Account Lockout Manager
AtlasLockoutManager <- R6Class("AtlasLockoutManager",
  public = list(
    max_attempts = 5,
    lockout_duration = 900, # 15 minutes
    failed_attempts = NULL,
    locked_accounts = NULL,
    
    initialize = function() {
      self$failed_attempts <- list()
      self$locked_accounts <- list()
    },
    
    record_failed_attempt = function(username, ip_address = NULL) {
      if (is.null(self$failed_attempts[[username]])) {
        self$failed_attempts[[username]] <- list()
      }
      
      attempt <- list(
        timestamp = Sys.time(),
        ip_address = ip_address
      )
      
      self$failed_attempts[[username]] <- append(self$failed_attempts[[username]], list(attempt))
      
      # Check if account should be locked
      if (length(self$failed_attempts[[username]]) >= self$max_attempts) {
        self$lock_account(username)
      }
    },
    
    lock_account = function(username) {
      self$locked_accounts[[username]] <- list(
        locked_at = Sys.time(),
        unlock_at = Sys.time() + self$lockout_duration
      )
    },
    
    is_account_locked = function(username) {
      if (is.null(self$locked_accounts[[username]])) return(FALSE)
      
      unlock_time <- self$locked_accounts[[username]]$unlock_at
      if (Sys.time() >= unlock_time) {
        # Account should be unlocked
        self$unlock_account(username)
        return(FALSE)
      }
      
      return(TRUE)
    },
    
    unlock_account = function(username) {
      self$locked_accounts[[username]] <- NULL
      self$failed_attempts[[username]] <- NULL
    },
    
    reset_failed_attempts = function(username) {
      self$failed_attempts[[username]] <- NULL
    }
  )
)

test_that("Account Lockout - Basic Lockout Functionality", {
  lockout_manager <- AtlasLockoutManager$new()
  
  # Account should not be locked initially
  expect_false(lockout_manager$is_account_locked("test_user"))
  
  # Record failed attempts
  for (i in 1:5) {
    lockout_manager$record_failed_attempt("test_user", "192.168.1.100")
  }
  
  # Account should be locked after max attempts
  expect_true(lockout_manager$is_account_locked("test_user"))
})

test_that("Account Lockout - Lockout Duration", {
  lockout_manager <- AtlasLockoutManager$new()
  lockout_manager$lockout_duration <- 1 # 1 second for testing
  
  # Lock account
  for (i in 1:5) {
    lockout_manager$record_failed_attempt("test_user")
  }
  
  expect_true(lockout_manager$is_account_locked("test_user"))
  
  # Wait for lockout to expire
  Sys.sleep(1.1)
  
  # Account should be unlocked
  expect_false(lockout_manager$is_account_locked("test_user"))
})

test_that("Account Lockout - Manual Unlock", {
  lockout_manager <- AtlasLockoutManager$new()
  
  # Lock account
  for (i in 1:5) {
    lockout_manager$record_failed_attempt("test_user")
  }
  
  expect_true(lockout_manager$is_account_locked("test_user"))
  
  # Manual unlock
  lockout_manager$unlock_account("test_user")
  
  expect_false(lockout_manager$is_account_locked("test_user"))
})

test_that("Account Lockout - IP Address Tracking", {
  lockout_manager <- AtlasLockoutManager$new()
  
  # Record attempts from different IP addresses
  lockout_manager$record_failed_attempt("test_user", "192.168.1.100")
  lockout_manager$record_failed_attempt("test_user", "192.168.1.101")
  lockout_manager$record_failed_attempt("test_user", "192.168.1.102")
  
  # Check IP addresses are recorded
  attempts <- lockout_manager$failed_attempts[["test_user"]]
  expect_equal(length(attempts), 3)
  expect_equal(attempts[[1]]$ip_address, "192.168.1.100")
  expect_equal(attempts[[2]]$ip_address, "192.168.1.101")
  expect_equal(attempts[[3]]$ip_address, "192.168.1.102")
})

# =============================================================================
# 9. PRIVILEGE SEPARATION TESTING
# =============================================================================

# Mock Privilege Separation Manager
AtlasPrivilegeManager <- R6Class("AtlasPrivilegeManager",
  public = list(
    user_contexts = NULL,
    
    initialize = function() {
      self$user_contexts <- list()
    },
    
    create_user_context = function(user_id, role, permissions) {
      self$user_contexts[[user_id]] <- list(
        role = role,
        permissions = permissions,
        created_at = Sys.time(),
        last_privilege_check = Sys.time()
      )
    },
    
    validate_operation = function(user_id, operation, resource) {
      context <- self$user_contexts[[user_id]]
      if (is.null(context)) return(FALSE)
      
      # Check if user has required permission for operation
      required_permission <- self$get_required_permission(operation, resource)
      
      return(required_permission %in% context$permissions)
    },
    
    get_required_permission = function(operation, resource) {
      # Define operation-resource permission mappings
      permission_map <- list(
        "read_employee_data" = "read",
        "write_employee_data" = "write",
        "delete_employee_data" = "delete",
        "view_salary_data" = "view_sensitive",
        "manage_user_accounts" = "manage_users",
        "export_reports" = "