# =============================================================================
# ATLAS LABS HR ANALYTICS - DATABASE PERFORMANCE UNIT TESTS
# =============================================================================
# Comprehensive testing suite for database performance optimization
# Author: akhapwoyaco (GitHub)
# =============================================================================

library(testthat)
library(DBI)
library(RSQLite)
library(RPostgreSQL)
library(pool)
library(microbenchmark)
library(profvis)
library(memoise)
library(future)
library(future.apply)
library(lubridate)
library(dplyr)
library(purrr)

# =============================================================================
# TEST CONFIGURATION & SETUP
# =============================================================================

# Test database configurations
test_config <- list(
  sqlite = list(
    driver = RSQLite::SQLite(),
    dbname = ":memory:",
    host = NULL,
    port = NULL,
    user = NULL,
    password = NULL
  ),
  postgresql = list(
    driver = RPostgreSQL::PostgreSQL(),
    dbname = "atlas_test",
    host = "localhost",
    port = 5432,
    user = "test_user",
    password = "test_pass"
  )
)

# Test data generators
generate_test_employees <- function(n = 1000) {
  data.frame(
    employee_id = 1:n,
    first_name = sample(c("John", "Jane", "Mike", "Sarah", "David"), n, replace = TRUE),
    last_name = sample(c("Smith", "Johnson", "Williams", "Brown", "Jones"), n, replace = TRUE),
    department = sample(c("IT", "HR", "Finance", "Marketing", "Sales"), n, replace = TRUE),
    salary = round(runif(n, 30000, 120000), 2),
    hire_date = sample(seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = "day"), n),
    attrition = sample(c(0, 1), n, replace = TRUE, prob = c(0.85, 0.15)),
    created_at = Sys.time() - runif(n, 0, 365*24*3600),
    updated_at = Sys.time() - runif(n, 0, 30*24*3600)
  )
}

generate_test_performance <- function(n = 5000) {
  data.frame(
    performance_id = 1:n,
    employee_id = sample(1:1000, n, replace = TRUE),
    review_date = sample(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"), n),
    job_satisfaction = sample(1:5, n, replace = TRUE),
    environment_satisfaction = sample(1:5, n, replace = TRUE),
    manager_rating = sample(1:5, n, replace = TRUE),
    self_rating = sample(1:5, n, replace = TRUE),
    created_at = Sys.time() - runif(n, 0, 365*24*3600)
  )
}

# Database setup helper
setup_test_db <- function(config) {
  if (config$driver@identifier == "SQLite") {
    conn <- dbConnect(config$driver, dbname = config$dbname)
  } else {
    conn <- dbConnect(config$driver, 
                      dbname = config$dbname,
                      host = config$host,
                      port = config$port,
                      user = config$user,
                      password = config$password)
  }
  
  # Create test tables
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS employees (
      employee_id INTEGER PRIMARY KEY,
      first_name VARCHAR(50),
      last_name VARCHAR(50),
      department VARCHAR(50),
      salary DECIMAL(10,2),
      hire_date DATE,
      attrition INTEGER,
      created_at TIMESTAMP,
      updated_at TIMESTAMP
    )
  ")
  
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS performance_ratings (
      performance_id INTEGER PRIMARY KEY,
      employee_id INTEGER,
      review_date DATE,
      job_satisfaction INTEGER,
      environment_satisfaction INTEGER,
      manager_rating INTEGER,
      self_rating INTEGER,
      created_at TIMESTAMP,
      FOREIGN KEY (employee_id) REFERENCES employees(employee_id)
    )
  ")
  
  return(conn)
}

# =============================================================================
# 3.3.1 QUERY EXECUTION OPTIMIZATION TESTS
# =============================================================================

test_that("Query Execution Optimization", {
  
  # Test 1: Basic Query Performance
  test_that("basic query performance meets thresholds", {
    conn <- setup_test_db(test_config$sqlite)
    
    # Insert test data
    employees <- generate_test_employees(1000)
    dbWriteTable(conn, "employees", employees, overwrite = TRUE)
    
    # Test simple SELECT performance
    execution_time <- system.time({
      result <- dbGetQuery(conn, "SELECT * FROM employees WHERE department = 'IT'")
    })
    
    expect_lt(execution_time[["elapsed"]], 0.1, 
              "Simple SELECT query should execute in under 100ms")
    expect_gt(nrow(result), 0, "Query should return results")
    
    dbDisconnect(conn)
  })
  
  # Test 2: Complex Query Performance
  test_that("complex query optimization with joins", {
    conn <- setup_test_db(test_config$sqlite)
    
    employees <- generate_test_employees(1000)
    performance <- generate_test_performance(5000)
    
    dbWriteTable(conn, "employees", employees, overwrite = TRUE)
    dbWriteTable(conn, "performance_ratings", performance, overwrite = TRUE)
    
    # Complex join query
    complex_query <- "
      SELECT e.employee_id, e.first_name, e.last_name, 
             AVG(p.job_satisfaction) as avg_satisfaction,
             COUNT(p.performance_id) as review_count
      FROM employees e
      LEFT JOIN performance_ratings p ON e.employee_id = p.employee_id
      WHERE e.department IN ('IT', 'HR')
      GROUP BY e.employee_id, e.first_name, e.last_name
      HAVING COUNT(p.performance_id) > 2
      ORDER BY avg_satisfaction DESC
    "
    
    execution_time <- system.time({
      result <- dbGetQuery(conn, complex_query)
    })
    
    expect_lt(execution_time[["elapsed"]], 0.5, 
              "Complex JOIN query should execute in under 500ms")
    expect_gt(nrow(result), 0, "Complex query should return results")
    
    dbDisconnect(conn)
  })
  
  # Test 3: Query Plan Analysis
  test_that("query execution plan optimization", {
    conn <- setup_test_db(test_config$sqlite)
    
    employees <- generate_test_employees(10000)
    dbWriteTable(conn, "employees", employees, overwrite = TRUE)
    
    # Test query without index
    query <- "SELECT * FROM employees WHERE salary > 75000 AND department = 'IT'"
    
    # Get execution plan
    plan_before <- dbGetQuery(conn, paste("EXPLAIN QUERY PLAN", query))
    
    # Create index
    dbExecute(conn, "CREATE INDEX idx_salary_dept ON employees(salary, department)")
    
    # Get execution plan after index
    plan_after <- dbGetQuery(conn, paste("EXPLAIN QUERY PLAN", query))
    
    # Test execution time improvement
    time_before <- system.time({
      dbExecute(conn, "DROP INDEX IF EXISTS idx_salary_dept")
      dbGetQuery(conn, query)
    })
    
    time_after <- system.time({
      dbExecute(conn, "CREATE INDEX idx_salary_dept ON employees(salary, department)")
      dbGetQuery(conn, query)
    })
    
    expect_lt(time_after[["elapsed"]], time_before[["elapsed"]], 
              "Query with index should be faster")
    
    dbDisconnect(conn)
  })
  
  # Test 4: Parameter Binding Performance
  test_that("prepared statement performance", {
    conn <- setup_test_db(test_config$sqlite)
    
    employees <- generate_test_employees(1000)
    dbWriteTable(conn, "employees", employees, overwrite = TRUE)
    
    # Test parameterized query performance
    departments <- c("IT", "HR", "Finance", "Marketing", "Sales")
    
    # Using parameter binding
    param_time <- system.time({
      stmt <- dbSendStatement(conn, "SELECT * FROM employees WHERE department = ?")
      results <- map(departments, ~{
        dbBind(stmt, list(.x))
        dbFetch(stmt)
      })
      dbClearResult(stmt)
    })
    
    # Using string concatenation (less optimal)
    concat_time <- system.time({
      results <- map(departments, ~{
        dbGetQuery(conn, paste0("SELECT * FROM employees WHERE department = '", .x, "'"))
      })
    })
    
    expect_lt(param_time[["elapsed"]], concat_time[["elapsed"]] * 1.2, 
              "Parameterized queries should be more efficient")
    
    dbDisconnect(conn)
  })
  
  # Test 5: Edge Cases - Large Result Sets
  test_that("large result set handling", {
    conn <- setup_test_db(test_config$sqlite)
    
    # Generate large dataset
    large_employees <- generate_test_employees(50000)
    dbWriteTable(conn, "employees", large_employees, overwrite = TRUE)
    
    # Test chunked reading
    chunk_time <- system.time({
      stmt <- dbSendQuery(conn, "SELECT * FROM employees")
      chunks <- list()
      i <- 1
      while (!dbHasCompleted(stmt)) {
        chunks[[i]] <- dbFetch(stmt, n = 1000)
        i <- i + 1
      }
      dbClearResult(stmt)
      total_rows <- sum(map_int(chunks, nrow))
    })
    
    expect_equal(total_rows, 50000, "All rows should be fetched")
    expect_lt(chunk_time[["elapsed"]], 5, 
              "Chunked reading should complete in reasonable time")
    
    dbDisconnect(conn)
  })
  
  # Test 6: Query Timeout Handling
  test_that("query timeout and cancellation", {
    conn <- setup_test_db(test_config$sqlite)
    
    # Create a deliberately slow query
    employees <- generate_test_employees(10000)
    dbWriteTable(conn, "employees", employees, overwrite = TRUE)
    
    # Test timeout mechanism
    expect_error({
      # Simulate timeout by setting a very low timeout
      # This would need to be implemented in the actual database layer
      dbGetQuery(conn, "SELECT * FROM employees e1 CROSS JOIN employees e2 WHERE e1.employee_id < 100", 
                timeout = 0.001)
    }, regexp = "timeout|cancelled", ignore.case = TRUE)
    
    dbDisconnect(conn)
  })
})

# =============================================================================
# 3.3.2 INDEX UTILIZATION EFFICIENCY TESTS
# =============================================================================

test_that("Index Utilization Efficiency", {
  
  # Test 1: Index Creation and Usage
  test_that("index creation improves query performance", {
    conn <- setup_test_db(test_config$sqlite)
    
    employees <- generate_test_employees(10000)
    dbWriteTable(conn, "employees", employees, overwrite = TRUE)
    
    # Measure performance without index
    time_without_index <- system.time({
      result1 <- dbGetQuery(conn, "SELECT * FROM employees WHERE department = 'IT' AND salary > 60000")
    })
    
    # Create composite index
    dbExecute(conn, "CREATE INDEX idx_dept_salary ON employees(department, salary)")
    
    # Measure performance with index
    time_with_index <- system.time({
      result2 <- dbGetQuery(conn, "SELECT * FROM employees WHERE department = 'IT' AND salary > 60000")
    })
    
    expect_lt(time_with_index[["elapsed"]], time_without_index[["elapsed"]], 
              "Index should improve query performance")
    expect_equal(nrow(result1), nrow(result2), "Results should be identical")
    
    dbDisconnect(conn)
  })
  
  # Test 2: Index Selectivity Testing
  test_that("index selectivity affects performance", {
    conn <- setup_test_db(test_config$sqlite)
    
    # Create data with different selectivity patterns
    employees <- generate_test_employees(10000)
    employees$high_selectivity <- sample(1:9000, nrow(employees), replace = TRUE)
    employees$low_selectivity <- sample(1:3, nrow(employees), replace = TRUE)
    
    dbWriteTable(conn, "employees", employees, overwrite = TRUE)
    
    # Create indexes
    dbExecute(conn, "CREATE INDEX idx_high_sel ON employees(high_selectivity)")
    dbExecute(conn, "CREATE INDEX idx_low_sel ON employees(low_selectivity)")
    
    # Test high selectivity query
    time_high_sel <- system.time({
      result_high <- dbGetQuery(conn, "SELECT * FROM employees WHERE high_selectivity = 1000")
    })
    
    # Test low selectivity query
    time_low_sel <- system.time({
      result_low <- dbGetQuery(conn, "SELECT * FROM employees WHERE low_selectivity = 1")
    })
    
    expect_lt(time_high_sel[["elapsed"]], time_low_sel[["elapsed"]], 
              "High selectivity index should perform better")
    
    dbDisconnect(conn)
  })
  
  # Test 3: Index Maintenance Overhead
  test_that("index maintenance overhead during DML operations", {
    conn <- setup_test_db(test_config$sqlite)
    
    employees <- generate_test_employees(1000)
    dbWriteTable(conn, "employees", employees, overwrite = TRUE)
    
    # Test INSERT performance without indexes
    insert_time_no_index <- system.time({
      for (i in 1:100) {
        dbExecute(conn, "INSERT INTO employees (employee_id, first_name, department, salary) VALUES (?, ?, ?, ?)",
                  params = list(10000 + i, "Test", "IT", 50000))
      }
    })
    
    # Create multiple indexes
    dbExecute(conn, "CREATE INDEX idx_emp_dept ON employees(department)")
    dbExecute(conn, "CREATE INDEX idx_emp_salary ON employees(salary)")
    dbExecute(conn, "CREATE INDEX idx_emp_name ON employees(first_name)")
    
    # Test INSERT performance with indexes
    insert_time_with_index <- system.time({
      for (i in 1:100) {
        dbExecute(conn, "INSERT INTO employees (employee_id, first_name, department, salary) VALUES (?, ?, ?, ?)",
                  params = list(20000 + i, "Test", "IT", 50000))
      }
    })
    
    # Index maintenance should add some overhead
    expect_gt(insert_time_with_index[["elapsed"]], insert_time_no_index[["elapsed"]], 
              "Indexes should add some INSERT overhead")
    
    # But the overhead should be reasonable
    expect_lt(insert_time_with_index[["elapsed"]], insert_time_no_index[["elapsed"]] * 3, 
              "Index overhead should be reasonable")
    
    dbDisconnect(conn)
  })
  
  # Test 4: Partial Index Efficiency
  test_that("partial index optimization", {
    conn <- setup_test_db(test_config$sqlite)
    
    employees <- generate_test_employees(10000)
    dbWriteTable(conn, "employees", employees, overwrite = TRUE)
    
    # Create partial index (SQLite syntax)
    dbExecute(conn, "CREATE INDEX idx_active_emp ON employees(department) WHERE attrition = 0")
    
    # Test query that matches partial index condition
    time_partial <- system.time({
      result_partial <- dbGetQuery(conn, "SELECT * FROM employees WHERE department = 'IT' AND attrition = 0")
    })
    
    # Test query that doesn't match partial index condition
    time_full <- system.time({
      result_full <- dbGetQuery(conn, "SELECT * FROM employees WHERE department = 'IT' AND attrition = 1")
    })
    
    expect_gt(nrow(result_partial), 0, "Partial index query should return results")
    expect_gt(nrow(result_full), 0, "Full table query should return results")
    
    dbDisconnect(conn)
  })
  
  # Test 5: Index Fragmentation Detection
  test_that("index fragmentation monitoring", {
    conn <- setup_test_db(test_config$sqlite)
    
    employees <- generate_test_employees(5000)
    dbWriteTable(conn, "employees", employees, overwrite = TRUE)
    
    # Create index
    dbExecute(conn, "CREATE INDEX idx_salary ON employees(salary)")
    
    # Simulate fragmentation through random updates
    for (i in 1:1000) {
      random_id <- sample(1:5000, 1)
      new_salary <- runif(1, 30000, 120000)
      dbExecute(conn, "UPDATE employees SET salary = ? WHERE employee_id = ?",
                params = list(new_salary, random_id))
    }
    
    # Check index statistics (SQLite specific)
    index_stats <- dbGetQuery(conn, "PRAGMA index_info(idx_salary)")
    
    expect_gt(nrow(index_stats), 0, "Index statistics should be available")
    
    dbDisconnect(conn)
  })
  
  # Test 6: Edge Cases - Index Usage Patterns
  test_that("edge cases in index usage", {
    conn <- setup_test_db(test_config$sqlite)
    
    employees <- generate_test_employees(1000)
    dbWriteTable(conn, "employees", employees, overwrite = TRUE)
    
    # Create index
    dbExecute(conn, "CREATE INDEX idx_dept_salary ON employees(department, salary)")
    
    # Test cases where index might not be used
    
    # Case 1: Leading column not in WHERE clause
    result1 <- dbGetQuery(conn, "EXPLAIN QUERY PLAN SELECT * FROM employees WHERE salary > 50000")
    
    # Case 2: Function on indexed column
    result2 <- dbGetQuery(conn, "EXPLAIN QUERY PLAN SELECT * FROM employees WHERE UPPER(department) = 'IT'")
    
    # Case 3: OR condition with non-indexed column
    result3 <- dbGetQuery(conn, "EXPLAIN QUERY PLAN SELECT * FROM employees WHERE department = 'IT' OR first_name = 'John'")
    
    # Case 4: Proper index usage
    result4 <- dbGetQuery(conn, "EXPLAIN QUERY PLAN SELECT * FROM employees WHERE department = 'IT' AND salary > 50000")
    
    expect_true(is.data.frame(result1), "Query plan should be available")
    expect_true(is.data.frame(result2), "Query plan should be available")
    expect_true(is.data.frame(result3), "Query plan should be available")
    expect_true(is.data.frame(result4), "Query plan should be available")
    
    dbDisconnect(conn)
  })
})

# =============================================================================
# 3.3.3 CONNECTION POOLING EFFECTIVENESS TESTS
# =============================================================================

test_that("Connection Pooling Effectiveness", {
  
  # Test 1: Basic Connection Pool Creation
  test_that("connection pool creation and management", {
    skip_if_not_installed("pool")
    
    pool_obj <- pool::dbPool(
      drv = RSQLite::SQLite(),
      dbname = ":memory:",
      minSize = 2,
      maxSize = 10
    )
    
    expect_true(pool::dbIsValid(pool_obj), "Pool should be valid")
    expect_equal(pool_obj$minSize, 2, "Minimum pool size should be 2")
    expect_equal(pool_obj$maxSize, 10, "Maximum pool size should be 10")
    
    pool::poolClose(pool_obj)
  })
  
  # Test 2: Connection Pool Performance Under Load
  test_that("connection pool performance under concurrent load", {
    skip_if_not_installed("pool")
    skip_if_not_installed("future")
    
    pool_obj <- pool::dbPool(
      drv = RSQLite::SQLite(),
      dbname = ":memory:",
      minSize = 2,
      maxSize = 5
    )
    
    # Setup test table
    pool::poolWithTransaction(pool_obj, function(conn) {
      dbExecute(conn, "CREATE TABLE test_table (id INTEGER, value TEXT)")
      dbExecute(conn, "INSERT INTO test_table VALUES (1, 'test')")
    })
    
    # Test concurrent access
    plan(multisession, workers = 4)
    
    concurrent_time <- system.time({
      results <- future_map(1:20, function(i) {
        pool::poolWithTransaction(pool_obj, function(conn) {
          dbGetQuery(conn, "SELECT * FROM test_table")
        })
      })
    })
    
    expect_equal(length(results), 20, "All concurrent requests should complete")
    expect_lt(concurrent_time[["elapsed"]], 5, 
              "Concurrent requests should complete in reasonable time")
    
    plan(sequential)
    pool::poolClose(pool_obj)
  })
  
  # Test 3: Connection Pool Sizing Optimization
  test_that("optimal pool size determination", {
    skip_if_not_installed("pool")
    
    # Test different pool sizes
    pool_sizes <- c(2, 5, 10, 20)
    performance_results <- list()
    
    for (size in pool_sizes) {
      pool_obj <- pool::dbPool(
        drv = RSQLite::SQLite(),
        dbname = ":memory:",
        minSize = 1,
        maxSize = size
      )
      
      # Setup test data
      pool::poolWithTransaction(pool_obj, function(conn) {
        dbExecute(conn, "CREATE TABLE test_perf (id INTEGER, value TEXT)")
        for (i in 1:100) {
          dbExecute(conn, "INSERT INTO test_perf VALUES (?, ?)", 
                    params = list(i, paste("value", i)))
        }
      })
      
      # Test performance
      test_time <- system.time({
        results <- map(1:50, function(i) {
          pool::poolWithTransaction(pool_obj, function(conn) {
            dbGetQuery(conn, "SELECT COUNT(*) FROM test_perf WHERE id <= ?", 
                      params = list(i))
          })
        })
      })
      
      performance_results[[as.character(size)]] <- test_time[["elapsed"]]
      
      pool::poolClose(pool_obj)
    }
    
    expect_true(length(performance_results) > 0, 
                "Performance results should be collected")
    
    # Find optimal size (should not be the smallest or largest)
    optimal_size <- names(performance_results)[which.min(unlist(performance_results))]
    expect_true(optimal_size %in% c("5", "10"), 
                "Optimal pool size should be moderate")
  })
  
  # Test 4: Connection Pool Exhaustion Handling
  test_that("connection pool exhaustion scenarios", {
    skip_if_not_installed("pool")
    
    pool_obj <- pool::dbPool(
      drv = RSQLite::SQLite(),
      dbname = ":memory:",
      minSize = 1,
      maxSize = 2  # Deliberately small
    )
    
    # Setup test table
    pool::poolWithTransaction(pool_obj, function(conn) {
      dbExecute(conn, "CREATE TABLE test_exhaust (id INTEGER)")
    })
    
    # Test pool exhaustion
    connections <- list()
    
    # This should work fine
    conn1 <- pool::poolCheckout(pool_obj)
    conn2 <- pool::poolCheckout(pool_obj)
    
    expect_true(DBI::dbIsValid(conn1), "First connection should be valid")
    expect_true(DBI::dbIsValid(conn2), "Second connection should be valid")
    
    # This should either wait or fail gracefully
    expect_error({
      # Set a short timeout to avoid hanging tests
      conn3 <- pool::poolCheckout(pool_obj)
    }, regexp = "timeout|exhausted", ignore.case = TRUE)
    
    # Return connections to pool
    pool::poolReturn(conn1)
    pool::poolReturn(conn2)
    
    pool::poolClose(pool_obj)
  })
  
  # Test 5: Connection Pool Health Monitoring
  test_that("connection pool health monitoring", {
    skip_if_not_installed("pool")
    
    pool_obj <- pool::dbPool(
      drv = RSQLite::SQLite(),
      dbname = ":memory:",
      minSize = 2,
      maxSize = 5,
      validationQuery = "SELECT 1"
    )
    
    # Get pool statistics
    initial_stats <- pool::poolDbStatistics(pool_obj)
    
    expect_true(is.list(initial_stats), "Pool statistics should be available")
    expect_gte(initial_stats$totalConnections, 2, 
               "Should have minimum connections")
    
    # Use some connections
    pool::poolWithTransaction(pool_obj, function(conn) {
      dbExecute(conn, "CREATE TABLE health_test (id INTEGER)")
      dbExecute(conn, "INSERT INTO health_test VALUES (1)")
    })
    
    # Check statistics after usage
    after_stats <- pool::poolDbStatistics(pool_obj)
    
    expect_gte(after_stats$totalConnections, initial_stats$totalConnections, 
               "Connection count should not decrease")
    
    pool::poolClose(pool_obj)
  })
  
  # Test 6: Edge Cases - Connection Pool Recovery
  test_that("connection pool recovery from failures", {
    skip_if_not_installed("pool")
    
    pool_obj <- pool::dbPool(
      drv = RSQLite::SQLite(),
      dbname = ":memory:",
      minSize = 1,
      maxSize = 3
    )
    
    # Setup test table
    pool::poolWithTransaction(pool_obj, function(conn) {
      dbExecute(conn, "CREATE TABLE recovery_test (id INTEGER)")
    })
    
    # Simulate connection failure and recovery
    expect_error({
      pool::poolWithTransaction(pool_obj, function(conn) {
        # Simulate a bad query that might corrupt the connection
        dbExecute(conn, "INVALID SQL STATEMENT")
      })
    }, regexp = "SQL|syntax", ignore.case = TRUE)
    
    # Pool should recover and allow new connections
    expect_no_error({
      pool::poolWithTransaction(pool_obj, function(conn) {
        dbGetQuery(conn, "SELECT COUNT(*) FROM recovery_test")
      })
    })
    
    pool::poolClose(pool_obj)
  })
})

# =============================================================================
# 3.3.4 TRANSACTION ISOLATION TESTING
# =============================================================================

test_that("Transaction Isolation Testing", {
  
  # Test 1: Read Committed Isolation Level
  test_that("read committed isolation prevents dirty reads", {
    conn1 <- setup_test_db(test_config$sqlite)
    conn2 <- setup_test_db(test_config$sqlite)
    
    # Setup test data
    dbExecute(conn1, "CREATE TABLE isolation_test (id INTEGER, value TEXT)")
    dbExecute(conn1, "INSERT INTO isolation_test VALUES (1, 'original')")
    
    # Transaction 1: Begin transaction but don't commit
    dbBegin(conn1)
    dbExecute(conn1, "UPDATE isolation_test SET value = 'modified' WHERE id = 1")
    
    # Transaction 2: Should not see uncommitted changes
    result_before_commit <- dbGetQuery(conn2, "SELECT value FROM isolation_test WHERE id = 1")
    
    # Commit transaction 1
    dbCommit(conn1)
    
    # Transaction 2: Should now see committed changes
    result_after_commit <- dbGetQuery(conn2, "SELECT value FROM isolation_test WHERE id = 1")
    
    expect_equal(result_before_commit$value, "original", 
                 "Should not see uncommitted changes")
    expect_equal(result_after_commit$value, "modified", 
                 "Should see committed changes")
    
    dbDisconnect(conn1)
    dbDisconnect(conn2)
  })
  
  # Test 2: Repeatable Read Testing
  test_that("repeatable read isolation consistency", {
    conn1 <- setup_test_db(test_config$sqlite)
    conn2 <- setup_test_db(test_config$sqlite)
    
    # Setup test data
    dbExecute(conn1, "CREATE TABLE repeatable_test (id INTEGER, value INTEGER)")
    dbExecute(conn1, "INSERT INTO repeatable_test VALUES (1, 100), (2, 200)")
    
    # Transaction 1: Start long-running transaction
    dbBegin(conn1)
    first_read <- dbGetQuery(conn1, "SELECT SUM(value) as total FROM repeatable_test")
    
    # Transaction 2: Modify data
    dbExecute(conn2, "UPDATE repeatable_test SET value = 150 WHERE id = 1")
    
    # Transaction 1: Read again - should get same result (if repeatable read)
    second_read <- dbGetQuery(conn1, "SELECT SUM(value) as total FROM repeatable_test")
    
    # Note: SQLite doesn't fully support repeatable read, but we test the concept
    expect_equal(first_read$total, second_read$total, 
                 "Reads within transaction should be consistent")
    
    dbCommit(conn1)
    
    dbDisconnect(conn1)
    dbDisconnect(conn2)
  })
  
  # Test 3: Phantom Read Prevention
  test_that("phantom read prevention in transactions", {
    conn1 <- setup_test_db(test_config$sqlite)
    conn2 <- setup_test_db(test_config$sqlite)
    
    # Setup test data
    dbExecute(conn1, "CREATE TABLE phantom_test (id INTEGER, category TEXT, value INTEGER)")
    dbExecute(conn1, "INSERT INTO phantom_test VALUES (1, 'A', 100), (2, 'A', 200)")
    
    # Transaction 1: Start transaction and read
    dbBegin(conn1)
    first_count <- dbGetQuery(conn1, "SELECT COUNT(*) as count FROM phantom_test WHERE category = 'A'")
    
    # Transaction 2: Insert new row
    dbExecute(conn2, "INSERT INTO phantom_test VALUES (3, 'A', 300)")
    
    # Transaction 1: Read again - should not see phantom row
    second_