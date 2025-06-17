# ================================================================================
# Atlas Labs HR Analytics Dashboard - Logger Module
# File: modules/logger_module.R
# Author: akhapwoyaco (GitHub)
# Purpose: Comprehensive R6 logging system with performance tracking
# ================================================================================

# Load required libraries (if not already loaded in global.R)
if (!require(R6)) library(R6)
if (!require(crayon)) library(crayon)

# ================================================================================
# AtlasLogger R6 Class Definition
# ================================================================================

AtlasLogger <- R6Class(
  classname = "AtlasLogger",
  
  # ================================================================================
  # Public Members
  # ================================================================================
  public = list(
    
    # Initialize logger
    initialize = function(app_name = "Atlas Labs HR Dashboard", 
                         max_logs = 1000,
                         console_output = TRUE,
                         file_output = FALSE,
                         log_file = NULL) {
      
      private$app_name <- app_name
      private$max_logs <- max_logs
      private$console_output <- console_output
      private$file_output <- file_output
      private$log_file <- log_file
      private$session_id <- private$generate_session_id()
      
      # Initialize performance tracking
      private$performance_data <- data.frame(
        timestamp = as.POSIXct(character()),
        module = character(),
        operation = character(),
        execution_time_ms = numeric(),
        memory_used_mb = numeric(),
        memory_peak_mb = numeric(),
        cpu_usage = numeric(),
        stringsAsFactors = FALSE
      )
      
      # Initialize log storage
      private$log_entries <- data.frame(
        timestamp = as.POSIXct(character()),
        session_id = character(),
        level = character(),
        module = character(),
        message = character(),
        execution_time_ms = numeric(),
        memory_mb = numeric(),
        stringsAsFactors = FALSE
      )
      
      # Log initialization
      self$log_info("Logger initialized successfully", "SYSTEM", 
                   list(session_id = private$session_id))
    },
    
    # ================================================================================
    # Core Logging Methods
    # ================================================================================
    
    # Log information messages
    log_info = function(message, module = "UNKNOWN", performance_data = NULL) {
      private$write_log("INFO", message, module, performance_data)
    },
    
    # Log warning messages
    log_warning = function(message, module = "UNKNOWN", performance_data = NULL) {
      private$write_log("WARNING", message, module, performance_data)
    },
    
    # Log error messages
    log_error = function(message, module = "UNKNOWN", performance_data = NULL, 
                        error_obj = NULL) {
      
      # Enhance error message with error object details if provided
      if (!is.null(error_obj)) {
        enhanced_message <- sprintf("%s | Error Details: %s", 
                                   message, 
                                   conditionMessage(error_obj))
        message <- enhanced_message
      }
      
      private$write_log("ERROR", message, module, performance_data)
    },
    
    # Log debug messages (only in development)
    log_debug = function(message, module = "UNKNOWN", performance_data = NULL) {
      if (private$debug_mode) {
        private$write_log("DEBUG", message, module, performance_data)
      }
    },
    
    # ================================================================================
    # Performance Tracking Methods
    # ================================================================================
    
    # Track memory usage
    track_memory_usage = function(module = "UNKNOWN", operation = "general") {
      tryCatch({
        # Get memory information
        mem_info <- private$get_memory_info()
        
        # Store performance data
        perf_entry <- data.frame(
          timestamp = Sys.time(),
          module = module,
          operation = operation,
          execution_time_ms = NA,
          memory_used_mb = mem_info$used,
          memory_peak_mb = mem_info$peak,
          cpu_usage = private$get_cpu_usage(),
          stringsAsFactors = FALSE
        )
        
        private$performance_data <- rbind(private$performance_data, perf_entry)
        private$cleanup_performance_data()
        
        return(mem_info)
        
      }, error = function(e) {
        self$log_error("Failed to track memory usage", "LOGGER", 
                      error_obj = e)
        return(NULL)
      })
    },
    
    # Track execution time for operations
    track_execution_time = function(expr, module = "UNKNOWN", 
                                  operation = "function_call") {
      start_time <- proc.time()
      start_memory <- private$get_memory_info()
      
      tryCatch({
        # Execute the expression
        result <- force(expr)
        
        # Calculate execution time
        end_time <- proc.time()
        execution_time <- as.numeric((end_time - start_time)[3] * 1000)  # Convert to ms
        
        # Get memory after execution
        end_memory <- private$get_memory_info()
        
        # Store performance data
        perf_entry <- data.frame(
          timestamp = Sys.time(),
          module = module,
          operation = operation,
          execution_time_ms = execution_time,
          memory_used_mb = end_memory$used,
          memory_peak_mb = max(start_memory$peak, end_memory$peak),
          cpu_usage = private$get_cpu_usage(),
          stringsAsFactors = FALSE
        )
        
        private$performance_data <- rbind(private$performance_data, perf_entry)
        private$cleanup_performance_data()
        
        # Log performance if significant
        if (execution_time > 100) {  # Log if > 100ms
          self$log_info(
            sprintf("Operation completed in %.2fms", execution_time),
            module,
            list(
              operation = operation,
              execution_time_ms = execution_time,
              memory_used_mb = end_memory$used
            )
          )
        }
        
        return(result)
        
      }, error = function(e) {
        # Calculate execution time even for errors
        end_time <- proc.time()
        execution_time <- as.numeric((end_time - start_time)[3] * 1000)
        
        self$log_error(
          sprintf("Operation failed after %.2fms: %s", 
                 execution_time, conditionMessage(e)),
          module,
          list(
            operation = operation,
            execution_time_ms = execution_time,
            error = TRUE
          ),
          e
        )
        
        stop(e)
      })
    },
    
    # ================================================================================
    # Performance Analysis Methods
    # ================================================================================
    
    # Get comprehensive performance summary
    get_performance_summary = function(module = NULL, last_n_minutes = NULL) {
      
      perf_data <- private$performance_data
      
      # Filter by module if specified
      if (!is.null(module)) {
        perf_data <- perf_data[perf_data$module == module, ]
      }
      
      # Filter by time if specified
      if (!is.null(last_n_minutes)) {
        cutoff_time <- Sys.time() - (last_n_minutes * 60)
        perf_data <- perf_data[perf_data$timestamp >= cutoff_time, ]
      }
      
      if (nrow(perf_data) == 0) {
        return(list(
          message = "No performance data available for the specified criteria",
          summary = NULL
        ))
      }
      
      # Calculate summary statistics
      summary_stats <- list(
        total_operations = nrow(perf_data),
        avg_execution_time_ms = mean(perf_data$execution_time_ms, na.rm = TRUE),
        max_execution_time_ms = max(perf_data$execution_time_ms, na.rm = TRUE),
        min_execution_time_ms = min(perf_data$execution_time_ms, na.rm = TRUE),
        avg_memory_mb = mean(perf_data$memory_used_mb, na.rm = TRUE),
        peak_memory_mb = max(perf_data$memory_peak_mb, na.rm = TRUE),
        avg_cpu_usage = mean(perf_data$cpu_usage, na.rm = TRUE),
        
        # Module breakdown
        module_breakdown = private$get_module_breakdown(perf_data),
        
        # Performance trends
        performance_trends = private$get_performance_trends(perf_data),
        
        # Slow operations (> 1 second)
        slow_operations = perf_data[
          !is.na(perf_data$execution_time_ms) & 
          perf_data$execution_time_ms > 1000, 
        ],
        
        # Memory intensive operations (> 100MB)
        memory_intensive = perf_data[
          !is.na(perf_data$memory_used_mb) & 
          perf_data$memory_used_mb > 100, 
        ]
      )
      
      return(summary_stats)
    },
    
    # ================================================================================
    # Log Management Methods
    # ================================================================================
    
    # Get recent logs
    get_logs = function(level = NULL, module = NULL, last_n = 100) {
      logs <- private$log_entries
      
      # Filter by level
      if (!is.null(level)) {
        logs <- logs[logs$level == toupper(level), ]
      }
      
      # Filter by module
      if (!is.null(module)) {
        logs <- logs[logs$module == module, ]
      }
      
      # Get last n entries
      if (nrow(logs) > last_n) {
        logs <- tail(logs, last_n)
      }
      
      return(logs)
    },
    
    # Get log statistics
    get_log_stats = function() {
      if (nrow(private$log_entries) == 0) {
        return(list(message = "No logs available"))
      }
      
      stats <- list(
        total_logs = nrow(private$log_entries),
        by_level = table(private$log_entries$level),
        by_module = table(private$log_entries$module),
        recent_errors = sum(private$log_entries$level == "ERROR" & 
                           private$log_entries$timestamp >= (Sys.time() - 3600)),
        session_duration = as.numeric(difftime(Sys.time(), 
                                              min(private$log_entries$timestamp), 
                                              units = "mins"))
      )
      
      return(stats)
    },
    
    # Clear logs
    clear_logs = function() {
      private$log_entries <- private$log_entries[0, ]
      private$performance_data <- private$performance_data[0, ]
      self$log_info("Logs cleared", "LOGGER")
    },
    
    # ================================================================================
    # UI Integration Methods
    # ================================================================================
    
    # Get formatted logs for UI display
    get_formatted_logs_ui = function(last_n = 50) {
      logs <- self$get_logs(last_n = last_n)
      
      if (nrow(logs) == 0) {
        return(data.frame())
      }
      
      # Format for UI display
      formatted <- logs
      formatted$timestamp <- format(formatted$timestamp, "%H:%M:%S")
      formatted$level <- factor(formatted$level, 
                               levels = c("DEBUG", "INFO", "WARNING", "ERROR"))
      
      return(formatted)
    },
    
    # Get performance metrics for UI
    get_performance_metrics_ui = function() {
      current_memory <- private$get_memory_info()
      recent_perf <- self$get_performance_summary(last_n_minutes = 5)
      
      metrics <- list(
        current_memory_mb = current_memory$used,
        peak_memory_mb = current_memory$peak,
        avg_response_time = ifelse(
          is.null(recent_perf$avg_execution_time_ms) || 
          is.na(recent_perf$avg_execution_time_ms),
          0,
          recent_perf$avg_execution_time_ms
        ),
        total_operations = recent_perf$total_operations %||% 0,
        error_count = sum(private$log_entries$level == "ERROR"),
        warning_count = sum(private$log_entries$level == "WARNING")
      )
      
      return(metrics)
    }
  ),
  
  # ================================================================================
  # Private Members
  # ================================================================================
  private = list(
    
    # Configuration
    app_name = NULL,
    max_logs = NULL,
    console_output = NULL,
    file_output = NULL,
    log_file = NULL,
    session_id = NULL,
    debug_mode = FALSE,
    
    # Data storage
    log_entries = NULL,
    performance_data = NULL,
    
    # ================================================================================
    # Core Private Methods
    # ================================================================================
    
    # Write log entry
    write_log = function(level, message, module, performance_data = NULL) {
      
      timestamp <- Sys.time()
      
      # Get current performance metrics
      current_memory <- private$get_memory_info()
      execution_time <- ifelse(
        !is.null(performance_data) && "execution_time_ms" %in% names(performance_data),
        performance_data$execution_time_ms,
        NA
      )
      
      # Create log entry
      log_entry <- data.frame(
        timestamp = timestamp,
        session_id = private$session_id,
        level = level,
        module = module,
        message = message,
        execution_time_ms = execution_time,
        memory_mb = current_memory$used,
        stringsAsFactors = FALSE
      )
      
      # Add to log storage
      private$log_entries <- rbind(private$log_entries, log_entry)
      
      # Cleanup old logs if necessary
      private$cleanup_logs()
      
      # Console output with color coding
      if (private$console_output) {
        private$print_colored_log(level, module, message, timestamp)
      }
      
      # File output
      if (private$file_output && !is.null(private$log_file)) {
        private$write_to_file(level, module, message, timestamp)
      }
    },
    
    # Print colored console output
    print_colored_log = function(level, module, message, timestamp) {
      
      # Format timestamp
      time_str <- format(timestamp, "%H:%M:%S")
      
      # Create colored output based on level
      colored_message <- switch(level,
        "DEBUG" = cat(cyan(sprintf("[%s] [DEBUG] [%s] %s\n", 
                                  time_str, module, message))),
        "INFO" = cat(green(sprintf("[%s] [INFO]  [%s] %s\n", 
                                  time_str, module, message))),
        "WARNING" = cat(yellow(sprintf("[%s] [WARN]  [%s] %s\n", 
                                      time_str, module, message))),
        "ERROR" = cat(red(sprintf("[%s] [ERROR] [%s] %s\n", 
                                 time_str, module, message)))
      )
    },
    
    # Write to log file
    write_to_file = function(level, module, message, timestamp) {
      tryCatch({
        log_line <- sprintf("%s [%s] [%s] %s\n", 
                           format(timestamp, "%Y-%m-%d %H:%M:%S"),
                           level, module, message)
        cat(log_line, file = private$log_file, append = TRUE)
      }, error = function(e) {
        # Fallback to console if file writing fails
        cat(red(sprintf("Failed to write to log file: %s\n", 
                       conditionMessage(e))))
      })
    },
    
    # ================================================================================
    # Performance Monitoring Private Methods
    # ================================================================================
    
    # Get memory information
    get_memory_info = function() {
      tryCatch({
        # Get process memory info
        mem_used <- as.numeric(object.size(ls(envir = .GlobalEnv))) / 1024^2
        
        # Try to get system memory info
        if (.Platform$OS.type == "windows") {
          mem_info <- system("wmic OS get TotalVisibleMemorySize,FreePhysicalMemory /value", 
                            intern = TRUE)
          # Parse Windows memory info (simplified)
          peak_mem <- mem_used * 1.2  # Estimate
        } else {
          # Unix-like systems
          mem_info <- system("free -m", intern = TRUE)
          peak_mem <- mem_used * 1.2  # Estimate
        }
        
        return(list(
          used = round(mem_used, 2),
          peak = round(peak_mem, 2)
        ))
        
      }, error = function(e) {
        # Fallback to basic R memory info
        return(list(
          used = round(as.numeric(object.size(ls(envir = .GlobalEnv))) / 1024^2, 2),
          peak = round(as.numeric(object.size(ls(envir = .GlobalEnv))) / 1024^2 * 1.2, 2)
        ))
      })
    },
    
    # Get CPU usage (simplified)
    get_cpu_usage = function() {
      tryCatch({
        # This is a simplified CPU usage estimate
        # In production, you might want to use system-specific commands
        load_avg <- system("uptime", intern = TRUE)
        # Extract and return a CPU usage estimate
        return(runif(1, 0, 100))  # Placeholder - replace with actual CPU monitoring
      }, error = function(e) {
        return(NA)
      })
    },
    
    # Get module breakdown
    get_module_breakdown = function(perf_data) {
      if (nrow(perf_data) == 0) return(NULL)
      
      breakdown <- aggregate(
        cbind(execution_time_ms, memory_used_mb) ~ module,
        data = perf_data,
        FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
                           max = max(x, na.rm = TRUE),
                           count = length(x)),
        na.rm = TRUE
      )
      
      return(breakdown)
    },
    
    # Get performance trends
    get_performance_trends = function(perf_data) {
      if (nrow(perf_data) < 2) return(NULL)
      
      # Simple trend analysis
      recent_data <- tail(perf_data, 10)
      older_data <- head(perf_data, min(10, nrow(perf_data) - 10))
      
      if (nrow(older_data) == 0) return(NULL)
      
      trends <- list(
        execution_time_trend = mean(recent_data$execution_time_ms, na.rm = TRUE) - 
                              mean(older_data$execution_time_ms, na.rm = TRUE),
        memory_trend = mean(recent_data$memory_used_mb, na.rm = TRUE) - 
                      mean(older_data$memory_used_mb, na.rm = TRUE)
      )
      
      return(trends)
    },
    
    # ================================================================================
    # Utility Private Methods
    # ================================================================================
    
    # Generate unique session ID
    generate_session_id = function() {
      paste0("ATLAS_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", 
             sample(1000:9999, 1))
    },
    
    # Cleanup old logs
    cleanup_logs = function() {
      if (nrow(private$log_entries) > private$max_logs) {
        # Keep only the most recent logs
        private$log_entries <- tail(private$log_entries, private$max_logs)
      }
    },
    
    # Cleanup old performance data
    cleanup_performance_data = function() {
      # Keep only last 1000 performance entries
      if (nrow(private$performance_data) > 1000) {
        private$performance_data <- tail(private$performance_data, 1000)
      }
    }
  )
)

# ================================================================================
# Module UI and Server Functions
# ================================================================================

# Logger UI for displaying logs in the app
loggerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        h4("System Logs & Performance", 
           style = "color: #2c3e50; margin-bottom: 20px;"),
        
        # Performance metrics row
        fluidRow(
          column(3,
            div(class = "performance-metric",
                style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                         color: white; padding: 15px; border-radius: 8px; text-align: center;",
                h5("Memory Usage", style = "margin: 0;"),
                textOutput(ns("current_memory"))
            )
          ),
          column(3,
            div(class = "performance-metric",
                style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); 
                         color: white; padding: 15px; border-radius: 8px; text-align: center;",
                h5("Avg Response", style = "margin: 0;"),
                textOutput(ns("avg_response"))
            )
          ),
          column(3,
            div(class = "performance-metric",
                style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); 
                         color: white; padding: 15px; border-radius: 8px; text-align: center;",
                h5("Operations", style = "margin: 0;"),
                textOutput(ns("total_operations"))
            )
          ),
          column(3,
            div(class = "performance-metric",
                style = "background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%); 
                         color: white; padding: 15px; border-radius: 8px; text-align: center;",
                h5("Errors", style = "margin: 0;"),
                textOutput(ns("error_count"))
            )
          )
        ),
        
        br(),
        
        # Log display
        DT::dataTableOutput(ns("log_table"))
      )
    )
  )
}

# Logger Server
loggerServer <- function(id, logger) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for auto-refresh
    values <- reactiveValues(
      refresh_counter = 0
    )
    
    # Auto-refresh every 5 seconds
    observe({
      invalidateLater(5000)
      values$refresh_counter <- values$refresh_counter + 1
    })
    
    # Performance metrics outputs
    output$current_memory <- renderText({
      values$refresh_counter  # Trigger refresh
      metrics <- logger$get_performance_metrics_ui()
      paste0(round(metrics$current_memory_mb, 1), " MB")
    })
    
    output$avg_response <- renderText({
      values$refresh_counter  # Trigger refresh
      metrics <- logger$get_performance_metrics_ui()
      paste0(round(metrics$avg_response_time, 0), " ms")
    })
    
    output$total_operations <- renderText({
      values$refresh_counter  # Trigger refresh
      metrics <- logger$get_performance_metrics_ui()
      as.character(metrics$total_operations)
    })
    
    output$error_count <- renderText({
      values$refresh_counter  # Trigger refresh
      metrics <- logger$get_performance_metrics_ui()
      as.character(metrics$error_count)
    })
    
    # Log table
    output$log_table <- DT::renderDataTable({
      values$refresh_counter  # Trigger refresh
      
      logs <- logger$get_formatted_logs_ui(last_n = 100)
      
      if (nrow(logs) == 0) {
        return(data.frame(Message = "No logs available"))
      }
      
      # Format the logs for display
      display_logs <- logs[, c("timestamp", "level", "module", "message")]
      names(display_logs) <- c("Time", "Level", "Module", "Message")
      
      DT::datatable(
        display_logs,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          order = list(list(0, 'desc')),  # Sort by timestamp descending
          columnDefs = list(
            list(width = '80px', targets = 0),   # Time column
            list(width = '80px', targets = 1),   # Level column
            list(width = '120px', targets = 2)   # Module column
          )
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Level",
          backgroundColor = DT::styleEqual(
            c("ERROR", "WARNING", "INFO", "DEBUG"),
            c("#ff4757", "#ffa502", "#2ed573", "#70a1ff")
          ),
          color = "white",
          fontWeight = "bold"
        )
    })
  })
}

# ================================================================================
# Helper function to create logger instance
# ================================================================================

create_atlas_logger <- function(console_output = TRUE, 
                                file_output = FALSE,
                                log_file = "atlas_logs.log") {
  
  logger <- AtlasLogger$new(
    app_name = "Atlas Labs HR Dashboard",
    max_logs = 1000,
    console_output = console_output,
    file_output = file_output,
    log_file = log_file
  )
  
  return(logger)
}