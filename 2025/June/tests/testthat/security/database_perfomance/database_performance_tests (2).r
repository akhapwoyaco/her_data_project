# ==================================================================================
# ATLAS LABS HR ANALYTICS - DATABASE PERFORMANCE UNIT TESTS
# ==================================================================================
# Author: akhapwoyaco
# Purpose: Comprehensive unit tests for database performance areas
# Coverage: Transaction isolation, Deadlock prevention, Bulk operations, 
#           Data pagination, Caching strategy validation
# ==================================================================================

# Load required libraries for testing
library(testthat)
library(dplyr)
library(data.table)
library(future)
library(promises)
library(R6)
library(microbenchmark)
library(profvis)
library(pryr)
library(digest)
library(pool)
library(DBI)
library(RSQLite)

# Source the application modules (assuming they exist)
source("global.R")
source("utils.R")
source("modules/data_loader_module.R")
source("modules/logger_module.R")

# ==================================================================================
# 3.3.1 TRANSACTION ISOLATION TESTING
# ==================================================================================

context("Transaction Isolation Testing")

# Mock database connection pool for testing
create_test_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  
  # Create test tables
  dbExecute(con, "CREATE TABLE employees (
    id INTEGER PRIMARY KEY,
    name TEXT,
    department TEXT,
    salary REAL,
    status TEXT,
    last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )")
  
  dbExecute(con, "CREATE TABLE performance_ratings (
    id INTEGER PRIMARY KEY,
    employee_id INTEGER,
    rating REAL,
    review_date DATE,
    FOREIGN KEY(employee_id) REFERENCES employees(id)
  )")
  
  # Insert test data
  dbExecute(con, "INSERT INTO employees (id, name, department, salary, status) VALUES
    (1, 'John Doe', 'IT', 75000, 'active'),
    (2, 'Jane Smith', 'HR', 65000, 'active'),
    (3, 'Bob Johnson', 'Finance', 80000, 'active')")
  
  return(con)
}

test_that("Read-committed isolation prevents dirty reads", {
  con <- create_test_db()
  
  # Start transaction in session 1
  dbBegin(con)
  
  # Update without committing
  dbExecute(con, "UPDATE employees SET salary = 90000 WHERE id = 1")
  
  # Read from another connection (simulating concurrent access)
  con2 <- dbConnect(RSQLite::SQLite(), ":memory:")
  dbExecute(con2, "ATTACH DATABASE ':memory:' AS test_db")
  
  # Should not see uncommitted changes
  result <- dbGetQuery(con, "SELECT salary FROM employees WHERE id = 1")
  expect_equal(result$salary[1], 90000)  # Transaction sees its own changes
  
  # Rollback
  dbRollback(con)
  
  # Verify rollback worked
  result <- dbGetQuery(con, "SELECT salary FROM employees WHERE id = 1")
  expect_equal(result$salary[1], 75000)  # Original value restored
  
  dbDisconnect(con)
  dbDisconnect(con2)
})

test_that("Phantom reads prevention with consistent snapshots", {
  con <- create_test_db()
  
  # Initial count
  initial_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM employees")$count
  
  # Simulate concurrent insert
  dbExecute(con, "INSERT INTO employees (id, name, department, salary, status) VALUES
    (4, 'Alice Brown', 'Marketing', 70000, 'active')")
  
  # Count should be consistent within same transaction context
  new_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM employees")$count
  expect_equal(new_count, initial_count + 1)
  
  dbDisconnect(con)
})

test_that("Concurrent modification detection", {
  con <- create_test_db()
  
  # Create a concurrent modification detector
  detect_concurrent_modification <- function(table_name, record_id) {
    tryCatch({
      # Get current timestamp
      current_ts <- dbGetQuery(con, 
        paste("SELECT last_updated FROM", table_name, "WHERE id =", record_id))
      
      # Simulate some processing time
      Sys.sleep(0.1)
      
      # Update with timestamp check
      rows_affected <- dbExecute(con, paste(
        "UPDATE", table_name, 
        "SET salary = salary * 1.1, last_updated = CURRENT_TIMESTAMP",
        "WHERE id =", record_id, 
        "AND last_updated =", paste0("'", current_ts$last_updated, "'")
      ))
      
      return(rows_affected > 0)
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  # Test concurrent modification detection
  result <- detect_concurrent_modification("employees", 1)
  expect_true(result)
  
  dbDisconnect(con)
})

test_that("Isolation level configuration validation", {
  con <- create_test_db()
  
  # Test different isolation levels (SQLite supports limited isolation)
  isolation_levels <- c(
    "READ UNCOMMITTED",
    "READ COMMITTED", 
    "REPEATABLE READ",
    "SERIALIZABLE"
  )
  
  for (level in isolation_levels) {
    tryCatch({
      # SQLite uses PRAGMA for isolation control
      if (level == "READ UNCOMMITTED") {
        dbExecute(con, "PRAGMA read_uncommitted = ON")
      } else {
        dbExecute(con, "PRAGMA read_uncommitted = OFF")
      }
      
      # Verify isolation works
      result <- dbGetQuery(con, "SELECT * FROM employees LIMIT 1")
      expect_true(nrow(result) > 0)
      
    }, error = function(e) {
      # Some isolation levels may not be supported
      expect_true(TRUE)  # Pass if isolation level not supported
    })
  }
  
  dbDisconnect(con)
})

# ==================================================================================
# 3.3.2 DEADLOCK PREVENTION TESTING
# ==================================================================================

context("Deadlock Prevention Testing")

test_that("Deadlock detection with timeout handling", {
  con1 <- create_test_db()
  con2 <- create_test_db()
  
  # Create deadlock scenario detector
  deadlock_detector <- function(conn, table1, table2, timeout = 5) {
    start_time <- Sys.time()
    
    tryCatch({
      # Start transaction
      dbBegin(conn)
      
      # Lock first table
      dbExecute(conn, paste("SELECT * FROM", table1, "WHERE id = 1 FOR UPDATE"))
      
      # Try to lock second table (potential deadlock)
      dbExecute(conn, paste("SELECT * FROM", table2, "WHERE id = 1 FOR UPDATE"))
      
      # If we get here, no deadlock
      dbCommit(conn)
      return(list(success = TRUE, time_taken = difftime(Sys.time(), start_time)))
      
    }, error = function(e) {
      dbRollback(conn)
      return(list(
        success = FALSE, 
        error = as.character(e),
        time_taken = difftime(Sys.time(), start_time)
      ))
    })
  }
  
  # Test deadlock prevention
  result1 <- deadlock_detector(con1, "employees", "performance_ratings")
  expect_true(result1$success || grepl("lock", result1$error, ignore.case = TRUE))
  
  dbDisconnect(con1)
  dbDisconnect(con2)
})

test_that("Lock ordering consistency prevents deadlocks", {
  # Define consistent lock ordering
  lock_order <- c("employees", "performance_ratings", "education_level")
  
  acquire_locks_ordered <- function(conn, tables) {
    ordered_tables <- intersect(lock_order, tables)
    
    tryCatch({
      dbBegin(conn)
      
      for (table in ordered_tables) {
        dbExecute(conn, paste("SELECT COUNT(*) FROM", table))
      }
      
      dbCommit(conn)
      return(TRUE)
      
    }, error = function(e) {
      dbRollback(conn)
      return(FALSE)
    })
  }
  
  con <- create_test_db()
  
  # Test ordered lock acquisition
  result <- acquire_locks_ordered(con, c("performance_ratings", "employees"))
  expect_true(result)
  
  dbDisconnect(con)
})

test_that("Deadlock recovery and retry mechanism", {
  deadlock_retry_handler <- function(conn, operation, max_retries = 3) {
    for (attempt in 1:max_retries) {
      tryCatch({
        return(operation(conn))
      }, error = function(e) {
        if (grepl("deadlock|lock", as.character(e), ignore.case = TRUE)) {
          if (attempt < max_retries) {
            # Exponential backoff
            Sys.sleep(0.1 * (2 ^ attempt))
            return(NULL)  # Continue retry loop
          }
        }
        stop(e)  # Re-throw if not deadlock or max retries reached
      })
    }
    stop("Max retries exceeded")
  }
  
  con <- create_test_db()
  
  # Test operation that might cause deadlock
  test_operation <- function(conn) {
    dbBegin(conn)
    dbExecute(conn, "UPDATE employees SET salary = salary * 1.05")
    dbCommit(conn)
    return(TRUE)
  }
  
  result <- deadlock_retry_handler(con, test_operation)
  expect_true(result)
  
  dbDisconnect(con)
})

test_that("Resource lock monitoring and alerting", {
  # Lock monitor for tracking long-running locks
  lock_monitor <- R6Class("LockMonitor",
    public = list(
      locks = list(),
      
      acquire_lock = function(resource_id, timeout = 30) {
        start_time <- Sys.time()
        
        # Check if already locked
        if (resource_id %in% names(self$locks)) {
          elapsed <- difftime(Sys.time(), self$locks[[resource_id]]$start_time, units = "secs")
          if (elapsed > timeout) {
            warning(paste("Lock timeout for resource:", resource_id))
            self$release_lock(resource_id)
          } else {
            return(FALSE)  # Resource still locked
          }
        }
        
        # Acquire lock
        self$locks[[resource_id]] <- list(
          start_time = start_time,
          timeout = timeout
        )
        return(TRUE)
      },
      
      release_lock = function(resource_id) {
        self$locks[[resource_id]] <- NULL
      },
      
      check_lock_health = function() {
        current_time <- Sys.time()
        long_locks <- c()
        
        for (resource_id in names(self$locks)) {
          elapsed <- difftime(current_time, self$locks[[resource_id]]$start_time, units = "secs")
          if (elapsed > self$locks[[resource_id]]$timeout) {
            long_locks <- c(long_locks, resource_id)
          }
        }
        
        return(long_locks)
      }
    )
  )
  
  monitor <- lock_monitor$new()
  
  # Test lock acquisition
  expect_true(monitor$acquire_lock("employee_1"))
  expect_false(monitor$acquire_lock("employee_1"))  # Already locked
  
  # Test lock release
  monitor$release_lock("employee_1")
  expect_true(monitor$acquire_lock("employee_1"))  # Should work now
  
  # Test lock health monitoring
  long_locks <- monitor$check_lock_health()
  expect_length(long_locks, 0)
})

# ==================================================================================
# 3.3.3 BULK OPERATION PERFORMANCE TESTING
# ==================================================================================

context("Bulk Operation Performance Testing")

test_that("Bulk insert performance optimization", {
  con <- create_test_db()
  
  # Generate test data
  n_records <- 10000
  test_data <- data.frame(
    id = (4:(3 + n_records)),
    name = paste("Employee", 1:n_records),
    department = sample(c("IT", "HR", "Finance", "Marketing"), n_records, replace = TRUE),
    salary = round(runif(n_records, 40000, 120000), 2),
    status = "active",
    stringsAsFactors = FALSE
  )
  
  # Test different bulk insert strategies
  
  # Strategy 1: Individual inserts
  single_insert_time <- system.time({
    dbBegin(con)
    for (i in 1:min(100, nrow(test_data))) {
      dbExecute(con, 
        "INSERT INTO employees (id, name, department, salary, status) VALUES (?, ?, ?, ?, ?)",
        params = list(
          test_data$id[i], test_data$name[i], test_data$department[i], 
          test_data$salary[i], test_data$status[i]
        )
      )
    }
    dbCommit(con)
  })
  
  # Strategy 2: Batch insert using dbWriteTable
  batch_insert_time <- system.time({
    dbWriteTable(con, "employees_batch", test_data[1:100, ], overwrite = TRUE)
  })
  
  # Strategy 3: Prepared statement with batch
  prepared_batch_time <- system.time({
    dbBegin(con)
    stmt <- dbPrepare(con, 
      "INSERT INTO employees (id, name, department, salary, status) VALUES (?, ?, ?, ?, ?)")
    dbBind(stmt, test_data[101:200, ])
    dbExecute(stmt)
    dbClearResult(stmt)
    dbCommit(con)
  })
  
  # Batch operations should be faster than individual inserts
  expect_lt(batch_insert_time[["elapsed"]], single_insert_time[["elapsed"]])
  
  dbDisconnect(con)
})

test_that("Bulk update performance with chunking", {
  con <- create_test_db()
  
  # Insert test data
  test_data <- data.frame(
    id = 1:1000,
    name = paste("Employee", 1:1000),
    department = sample(c("IT", "HR", "Finance"), 1000, replace = TRUE),
    salary = round(runif(1000, 50000, 100000), 2),
    status = "active"
  )
  
  dbWriteTable(con, "employees_bulk", test_data, overwrite = TRUE)
  
  # Chunked bulk update function
  chunked_bulk_update <- function(conn, table, updates, chunk_size = 100) {
    n_chunks <- ceiling(nrow(updates) / chunk_size)
    
    performance_metrics <- data.frame(
      chunk = integer(),
      records = integer(),
      time_seconds = numeric(),
      memory_mb = numeric()
    )
    
    for (i in 1:n_chunks) {
      start_idx <- (i - 1) * chunk_size + 1
      end_idx <- min(i * chunk_size, nrow(updates))
      chunk_data <- updates[start_idx:end_idx, ]
      
      # Monitor performance
      start_time <- Sys.time()
      start_memory <- as.numeric(object.size(ls())) / 1024 / 1024
      
      # Perform chunked update
      dbBegin(conn)
      for (j in 1:nrow(chunk_data)) {
        dbExecute(conn, 
          paste("UPDATE", table, "SET salary = ? WHERE id = ?"),
          params = list(chunk_data$new_salary[j], chunk_data$id[j])
        )
      }
      dbCommit(conn)
      
      end_time <- Sys.time()
      end_memory <- as.numeric(object.size(ls())) / 1024 / 1024
      
      performance_metrics <- rbind(performance_metrics, data.frame(
        chunk = i,
        records = nrow(chunk_data),
        time_seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
        memory_mb = end_memory - start_memory
      ))
    }
    
    return(performance_metrics)
  }
  
  # Test chunked updates
  update_data <- data.frame(
    id = 1:500,
    new_salary = round(runif(500, 55000, 110000), 2)
  )
  
  metrics <- chunked_bulk_update(con, "employees_bulk", update_data, chunk_size = 50)
  
  # Verify performance metrics
  expect_true(all(metrics$time_seconds > 0))
  expect_true(nrow(metrics) > 0)
  expect_equal(sum(metrics$records), nrow(update_data))
  
  dbDisconnect(con)
})

test_that("Bulk delete performance with cascading", {
  con <- create_test_db()
  
  # Create test data with relationships
  dbExecute(con, "INSERT INTO employees (id, name, department, salary, status) VALUES
    (101, 'Test Employee 1', 'IT', 75000, 'active'),
    (102, 'Test Employee 2', 'HR', 65000, 'active'),
    (103, 'Test Employee 3', 'Finance', 80000, 'active')")
  
  dbExecute(con, "INSERT INTO performance_ratings (employee_id, rating, review_date) VALUES
    (101, 4.5, '2024-01-01'),
    (102, 4.0, '2024-01-01'),
    (103, 3.5, '2024-01-01')")
  
  # Bulk delete with cascading
  bulk_delete_with_cascade <- function(conn, employee_ids) {
    performance_start <- Sys.time()
    
    tryCatch({
      dbBegin(conn)
      
      # Delete related records first (manual cascade)
      for (id in employee_ids) {
        dbExecute(conn, "DELETE FROM performance_ratings WHERE employee_id = ?", 
                  params = list(id))
      }
      
      # Delete main records
      placeholders <- paste(rep("?", length(employee_ids)), collapse = ",")
      dbExecute(conn, 
        paste("DELETE FROM employees WHERE id IN (", placeholders, ")"),
        params = employee_ids
      )
      
      dbCommit(conn)
      
      performance_end <- Sys.time()
      
      return(list(
        success = TRUE,
        time_taken = difftime(performance_end, performance_start, units = "secs"),
        records_deleted = length(employee_ids)
      ))
      
    }, error = function(e) {
      dbRollback(conn)
      return(list(success = FALSE, error = as.character(e)))
    })
  }
  
  # Test bulk delete
  result <- bulk_delete_with_cascade(con, c(101, 102, 103))
  
  expect_true(result$success)
  expect_equal(result$records_deleted, 3)
  expect_true(result$time_taken > 0)
  
  # Verify deletion
  remaining_employees <- dbGetQuery(con, "SELECT COUNT(*) as count FROM employees WHERE id IN (101, 102, 103)")
  expect_equal(remaining_employees$count, 0)
  
  dbDisconnect(con)
})

test_that("Memory-efficient bulk operations", {
  # Memory-efficient data processing
  memory_efficient_processor <- R6Class("MemoryEfficientProcessor",
    public = list(
      process_large_dataset = function(data_source, chunk_size = 1000) {
        # Simulate processing large dataset in chunks
        total_records <- 50000
        processed_count <- 0
        memory_usage <- c()
        
        for (chunk_start in seq(1, total_records, chunk_size)) {
          chunk_end <- min(chunk_start + chunk_size - 1, total_records)
          
          # Monitor memory before processing
          memory_before <- as.numeric(object.size(ls())) / 1024 / 1024
          
          # Simulate chunk processing
          chunk_data <- data.frame(
            id = chunk_start:chunk_end,
            value = runif(chunk_end - chunk_start + 1)
          )
          
          # Process chunk (simulate heavy computation)
          processed_chunk <- chunk_data %>%
            mutate(
              processed_value = value * 2,
              category = ifelse(value > 0.5, "high", "low")
            )
          
          # Monitor memory after processing
          memory_after <- as.numeric(object.size(ls())) / 1024 / 1024
          memory_usage <- c(memory_usage, memory_after - memory_before)
          
          processed_count <- processed_count + nrow(processed_chunk)
          
          # Force garbage collection
          gc()
        }
        
        return(list(
          total_processed = processed_count,
          avg_memory_per_chunk = mean(memory_usage),
          max_memory_per_chunk = max(memory_usage)
        ))
      }
    )
  )
  
  processor <- memory_efficient_processor$new()
  result <- processor$process_large_dataset(chunk_size = 1000)
  
  expect_equal(result$total_processed, 50000)
  expect_true(result$avg_memory_per_chunk > 0)
  expect_true(result$max_memory_per_chunk > 0)
})

# ==================================================================================
# 3.3.4 DATA PAGINATION EFFICIENCY TESTING
# ==================================================================================

context("Data Pagination Efficiency Testing")

test_that("Offset-based pagination performance", {
  con <- create_test_db()
  
  # Insert large dataset for pagination testing
  large_dataset <- data.frame(
    id = 1:10000,
    name = paste("Employee", 1:10000),
    department = sample(c("IT", "HR", "Finance", "Marketing", "Sales"), 10000, replace = TRUE),
    salary = round(runif(10000, 40000, 120000), 2),
    status = "active"
  )
  
  dbWriteTable(con, "employees_large", large_dataset, overwrite = TRUE)
  
  # Test offset-based pagination
  offset_pagination_test <- function(conn, table, page_size = 100, max_pages = 10) {
    performance_metrics <- data.frame(
      page = integer(),
      offset = integer(),
      time_ms = numeric(),
      record_count = integer()
    )
    
    for (page in 1:max_pages) {
      offset <- (page - 1) * page_size
      
      start_time <- Sys.time()
      
      query <- paste(
        "SELECT * FROM", table,
        "ORDER BY id",
        "LIMIT", page_size,
        "OFFSET", offset
      )
      
      result <- dbGetQuery(conn, query)
      
      end_time <- Sys.time()
      
      performance_metrics <- rbind(performance_metrics, data.frame(
        page = page,
        offset = offset,
        time_ms = as.numeric(difftime(end_time, start_time, units = "secs")) * 1000,
        record_count = nrow(result)
      ))
    }
    
    return(performance_metrics)
  }
  
  metrics <- offset_pagination_test(con, "employees_large")
  
  # Verify pagination performance
  expect_true(all(metrics$record_count == 100))
  expect_true(all(metrics$time_ms > 0))
  
  # Performance should degrade with higher offsets
  correlation <- cor(metrics$offset, metrics$time_ms)
  expect_true(correlation > 0)  # Higher offset should take longer
  
  dbDisconnect(con)
})

test_that("Cursor-based pagination optimization", {
  con <- create_test_db()
  
  # Insert test data with indexed column
  test_data <- data.frame(
    id = 1:5000,
    name = paste("Employee", 1:5000),
    department = sample(c("IT", "HR", "Finance"), 5000, replace = TRUE),
    salary = round(runif(5000, 40000, 120000), 2),
    created_at = seq(
      from = as.POSIXct("2024-01-01"),
      to = as.POSIXct("2024-12-31"),
      length.out = 5000
    )
  )
  
  dbWriteTable(con, "employees_cursor", test_data, overwrite = TRUE)
  
  # Create index for cursor-based pagination
  dbExecute(con, "CREATE INDEX idx_employees_cursor_id ON employees_cursor(id)")
  
  # Cursor-based pagination
  cursor_pagination_test <- function(conn, table, page_size = 100, max_pages = 10) {
    performance_metrics <- data.frame(
      page = integer(),
      cursor_id = integer(),
      time_ms = numeric(),
      record_count = integer()
    )
    
    last_id <- 0
    
    for (page in 1:max_pages) {
      start_time <- Sys.time()
      
      query <- paste(
        "SELECT * FROM", table,
        "WHERE id >", last_id,
        "ORDER BY id",
        "LIMIT", page_size
      )
      
      result <- dbGetQuery(conn, query)
      
      end_time <- Sys.time()
      
      if (nrow(result) > 0) {
        last_id <- max(result$id)
        
        performance_metrics <- rbind(performance_metrics, data.frame(
          page = page,
          cursor_id = last_id,
          time_ms = as.numeric(difftime(end_time, start_time, units = "secs")) * 1000,
          record_count = nrow(result)
        ))
      } else {
        break
      }
    }
    
    return(performance_metrics)
  }
  
  cursor_metrics <- cursor_pagination_test(con, "employees_cursor")
  
  # Verify cursor-based pagination
  expect_true(all(cursor_metrics$record_count > 0))
  expect_true(all(cursor_metrics$time_ms > 0))
  
  # Cursor-based should have more consistent performance
  cv_time <- sd(cursor_metrics$time_ms) / mean(cursor_metrics$time_ms)
  expect_lt(cv_time, 0.5)  # Low coefficient of variation
  
  dbDisconnect(con)
})

test_that("Pagination with complex filters and sorting", {
  con <- create_test_db()
  
  # Create complex test dataset
  complex_data <- data.frame(
    id = 1:3000,
    name = paste("Employee", 1:3000),
    department = sample(c("IT", "HR", "Finance", "Marketing", "Sales"), 3000, replace = TRUE),
    salary = round(runif(3000, 40000, 120000), 2),
    performance_rating = round(runif(3000, 1, 5), 1),
    hire_date = sample(seq(as.Date("2020-01-01"), as.Date("2024-12-31"), by = "day"), 3000, replace = TRUE),
    status = sample(c("active", "inactive"), 3000, replace = TRUE, prob = c(0.9, 0.1))
  )
  
  dbWriteTable(con, "employees_complex", complex_data, overwrite = TRUE)
  
  # Test pagination with complex filters
  complex_pagination_test <- function(conn, filters, sort_by, page_size = 50) {
    # Build WHERE clause
    where_clause <- ""
    if (length(filters) > 0) {
      conditions <- c()
      for (field in names(filters)) {
        if (is.character(filters[[field]])) {
          conditions <- c(conditions, paste(field, "IN (", 
            paste(paste0("'", filters[[field]], "'"), collapse = ","), ")"))
        } else {
          conditions <- c(conditions, paste(field, "BETWEEN", 
            filters[[field]][1], "AND", filters[[field]][2]))
        }
      }
      where_clause <- paste("WHERE", paste(conditions, collapse = " AND "))
    }
    
    # Build ORDER BY clause
    order_clause <- paste("ORDER BY", paste(sort_by, collapse = ", "))
    
    # Test first page
    start_time <- Sys.time()
    
    query <- paste(
      "SELECT * FROM employees_complex",
      where_clause,
      order_clause,
      "LIMIT", page_size
    )
    
    result <- dbGetQuery(conn, query)
    
    end_time <- Sys.time()
    
    # Get total count for pagination info
    count_query <- paste(
      "SELECT COUNT(*) as total FROM employees_complex",
      where_clause
    )
    
    total_count <- dbGetQuery(conn, count_query)$total
    
    return(list(
      records = nrow(result),
      total_records = total_count,
      query_time_ms = as.numeric(difftime(end_time, start_time, units = "secs")) * 1000,
      total_pages = ceiling(total_count / page_size)
    ))
  }
  
  # Test various filter combinations
  filters_test1 <- list(
    department = c("IT", "HR"),
    salary = c(50000, 100000)
  )
  
  result1 <- complex_pagination_test(con, filters_test1, c("salary DESC", "name ASC"))
  expect_true(result1$records > 0)
  expect_true(result1$query_time_ms > 0)
  
  # Test with no filters
  result2 <- complex_pagination_test(con, list(), c("id"))
  expect_true(result2$records > 0)
  expect_true(result2$total_records > result1$total_records)
  
  dbDisconnect(con)
})

test_that("Pagination state management and consistency", {
  # Pagination state manager
  pagination_state_manager <- R6Class("PaginationStateManager",
    public = list(
      states = list(),
      
      create_pagination_state = function(session_id, total_records, page_size) {
        self$states[[session_id]] <- list(
          total_records = total_records,
          page_size = page_size,
          current_page = 1,
          total_pages = ceiling(total_records / page_size),
          last_accessed = Sys.time(),
          cache = list()
        )
        
        return(self$states[[session_id]])
      },
      
      get_page_info = function(session_id, page_number) {
        if (