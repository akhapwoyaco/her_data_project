# =============================================================================
# Atlas Labs HR Analytics Dashboard
# Logger Module - R6 Class Implementation
# 
# Author: akhapwoyaco (GitHub)
# Purpose: Comprehensive logging system with performance tracking
# =============================================================================

# Required libraries (loaded in global.R)
if (!require(R6)) library(R6)
if (!require(crayon)) library(crayon, quietly = TRUE) # For colorized output

# =============================================================================
# AtlasLogger R6 Class Definition
# =============================================================================

AtlasLogger <- R6::R6Class(
  classname = "AtlasLogger",
  
  # ---------------------------------------------------------------------------
  # Public Members
  # ---------------------------------------------------------------------------
  public = list(
    
    # Initialize logger
    initialize = function(app_name = "Atlas Labs HR Dashboard", 
                         log_level = "INFO",
                         enable_console = TRUE,
                         enable_file = FALSE,
                         max_log_entries = 1000) {
      
      private$app_name <- app_name
      private$log_level <- toupper(log_level)
      private$enable_console <- enable_console
      private$enable_file <- enable_file
      private$max_entries <- max_log_entries
      private$session_start <- Sys.time()
      
      # Initialize log storage
      private$logs <- data.frame(
        timestamp = as.POSIXct(character(0)),
        level = character(0),
        module = character(0),
        message = character(0),
        execution_time_ms = numeric(0),
        memory_mb = numeric(0),
        cpu_percent = numeric(0),
        session_id = character(0),
        stringsAsFactors = FALSE
      )
      
      # Initialize performance metrics
      private$performance_data <- list(
        module_times = list(),
        memory_usage = list(),
        function_calls = list(),
        errors = list(),
        warnings = list()
      )
      
      # Generate unique session ID
      private$session_id <- paste0("ATLAS_", format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                  "_", sample(1000:9999, 1))
      
      # Set log levels hierarchy
      private$log_levels <- c("DEBUG" = 1, "INFO" = 2, "WARN" = 3, "ERROR" = 4, "FATAL" = 5)
      
      # Start session
      self$log_info("Logger initialized successfully", "SYSTEM", 
                   list(session_id = private$session_id))
      
      invisible(self)
    },
    
    # -------------------------------------------------------------------------
    # Core Logging Methods
    # -------------------------------------------------------------------------
    
    # Log INFO level messages
    log_info = function(message, module = "MAIN", performance_data = NULL) {
      private$write_log("INFO", message, module, performance_data)
    },
    
    # Log DEBUG level messages
    log_debug = function(message, module = "MAIN", performance_data = NULL) {
      private$write_log("DEBUG", message, module, performance_data)
    },
    
    # Log WARNING level messages
    log_warning = function(message, module = "MAIN", performance_data = NULL) {
      private$write_log("WARN", message, module, performance_data)
    },
    
    # Log ERROR level messages
    log_error = function(message, module = "MAIN", performance_data = NULL) {
      private$write_log("ERROR", message, module, performance_data)
      # Store error for analysis
      private$performance_data$errors[[length(private$performance_data$errors) + 1]] <- 
        list(timestamp = Sys.time(), module = module, message = message)
    },
    
    # Log FATAL level messages
    log_fatal = function(message, module = "MAIN", performance_data = NULL) {
      private$write_log("FATAL", message, module, performance_data)
    },
    
    # -------------------------------------------------------------------------
    # Performance Tracking Methods
    # -------------------------------------------------------------------------
    
    # Start performance timer for a module/function
    start_timer = function(module, function_name = "main") {
      timer_key <- paste0(module, "::", function_name)
      private$timers[[timer_key]] <- list(
        start_time = Sys.time(),
        start_memory = private$get_memory_usage()
      )
      
      self$log_debug(paste("Started timer for", function_name), module)
      return(timer_key)
    },
    
    # End performance timer and log results
    end_timer = function(timer_key, additional_info = NULL) {
      if (!timer_key %in% names(private$timers)) {
        self$log_warning(paste("Timer key not found:", timer_key), "SYSTEM")
        return(NULL)
      }
      
      timer_data <- private$timers[[timer_key]]
      end_time <- Sys.time()
      end_memory <- private$get_memory_usage()
      
      execution_time <- as.numeric(difftime(end_time, timer_data$start_time, units = "secs")) * 1000
      memory_diff <- end_memory - timer_data$start_memory
      
      # Store performance data
      perf_data <- list(
        execution_time_ms = execution_time,
        memory_change_mb = memory_diff,
        start_memory_mb = timer_data$start_memory,
        end_memory_mb = end_memory
      )
      
      if (!is.null(additional_info)) {
        perf_data <- c(perf_data, additional_info)
      }
      
      # Extract module from timer key
      module <- strsplit(timer_key, "::")[[1]][1]
      function_name <- strsplit(timer_key, "::")[[1]][2]
      
      # Log performance
      self$log_info(
        paste0("Completed ", function_name, " - Time: ", round(execution_time, 2), 
               "ms, Memory: ", round(memory_diff, 2), "MB"),
        module, perf_data
      )
      
      # Store in performance tracking
      private$performance_data$module_times[[timer_key]] <- perf_data
      
      # Clean up timer
      private$timers[[timer_key]] <- NULL
      
      return(perf_data)
    },
    
    # Track memory usage
    track_memory_usage = function(module = "SYSTEM") {
      current_memory <- private$get_memory_usage()
      peak_memory <- private$get_peak_memory()
      
      memory_info <- list(
        current_mb = current_memory,
        peak_mb = peak_memory,
        available_mb = private$get_available_memory()
      )
      
      private$performance_data$memory_usage[[as.character(Sys.time())]] <- memory_info
      
      self$log_debug(
        paste0("Memory - Current: ", round(current_memory, 2), 
               "MB, Peak: ", round(peak_memory, 2), "MB"),
        module, memory_info
      )
      
      # Memory warning threshold (500MB)
      if (current_memory > 500) {
        self$log_warning(
          paste("High memory usage detected:", round(current_memory, 2), "MB"),
          module
        )
      }
      
      return(memory_info)
    },
    
    # Track execution time for a code block
    track_execution = function(expr, module = "MAIN", description = "operation") {
      timer_key <- self$start_timer(module, description)
      
      result <- tryCatch({
        force(expr)
      }, error = function(e) {
        self$log_error(paste("Execution failed:", e$message), module)
        stop(e)
      })
      
      self$end_timer(timer_key, list(operation = description))
      return(result)
    },
    
    # -------------------------------------------------------------------------
    # Analytics & Reporting Methods
    # -------------------------------------------------------------------------
    
    # Get performance summary
    get_performance_summary = function() {
      total_logs <- nrow(private$logs)
      session_duration <- as.numeric(difftime(Sys.time(), private$session_start, units = "mins"))
      
      # Calculate averages
      avg_execution_ms <- if (total_logs > 0) mean(private$logs$execution_time_ms, na.rm = TRUE) else 0
      avg_memory_mb <- if (total_logs > 0) mean(private$logs$memory_mb, na.rm = TRUE) else 0
      
      # Error and warning counts
      error_count <- sum(private$logs$level == "ERROR", na.rm = TRUE)
      warning_count <- sum(private$logs$level == "WARN", na.rm = TRUE)
      
      # Module statistics
      module_stats <- if (total_logs > 0) {
        private$logs %>%
          dplyr::group_by(module) %>%
          dplyr::summarise(
            log_count = n(),
            avg_execution_ms = mean(execution_time_ms, na.rm = TRUE),
            avg_memory_mb = mean(memory_mb, na.rm = TRUE),
            error_count = sum(level == "ERROR"),
            warning_count = sum(level == "WARN"),
            .groups = "drop"
          )
      } else {
        data.frame()
      }
      
      summary <- list(
        session_info = list(
          session_id = private$session_id,
          start_time = private$session_start,
          duration_minutes = round(session_duration, 2),
          total_logs = total_logs
        ),
        performance_metrics = list(
          avg_execution_time_ms = round(avg_execution_ms, 2),
          avg_memory_usage_mb = round(avg_memory_mb, 2),
          current_memory_mb = round(private$get_memory_usage(), 2),
          peak_memory_mb = round(private$get_peak_memory(), 2)
        ),
        quality_metrics = list(
          error_count = error_count,
          warning_count = warning_count,
          error_rate = if (total_logs > 0) round(error_count / total_logs * 100, 2) else 0,
          warning_rate = if (total_logs > 0) round(warning_count / total_logs * 100, 2) else 0
        ),
        module_statistics = module_stats,
        top_performing_modules = private$get_top_modules("performance"),
        slowest_modules = private$get_top_modules("slowest")
      )
      
      return(summary)
    },
    
    # Get logs for display in Shiny app
    get_logs_for_display = function(limit = 100, level_filter = NULL, module_filter = NULL) {
      logs_subset <- private$logs
      
      # Apply filters
      if (!is.null(level_filter)) {
        logs_subset <- logs_subset[logs_subset$level %in% level_filter, ]
      }
      
      if (!is.null(module_filter)) {
        logs_subset <- logs_subset[logs_subset$module %in% module_filter, ]
      }
      
      # Limit results and order by timestamp desc
      if (nrow(logs_subset) > 0) {
        logs_subset <- logs_subset[order(logs_subset$timestamp, decreasing = TRUE), ]
        logs_subset <- head(logs_subset, limit)
        
        # Format for display
        logs_subset$formatted_time <- format(logs_subset$timestamp, "%H:%M:%S")
        logs_subset$formatted_message <- substr(logs_subset$message, 1, 100)
      }
      
      return(logs_subset)
    },
    
    # Export logs to file
    export_logs = function(filename = NULL, format = "csv") {
      if (is.null(filename)) {
        filename <- paste0("atlas_logs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", format)
      }
      
      if (format == "csv") {
        write.csv(private$logs, filename, row.names = FALSE)
      } else if (format == "json") {
        jsonlite::write_json(private$logs, filename, pretty = TRUE)
      }
      
      self$log_info(paste("Logs exported to", filename), "SYSTEM")
      return(filename)
    },
    
    # Clear logs (keep summary)
    clear_logs = function(keep_summary = TRUE) {
      if (keep_summary) {
        summary <- self$get_performance_summary()
        private$session_summary <- summary
      }
      
      private$logs <- private$logs[0, ]
      private$performance_data <- list(
        module_times = list(),
        memory_usage = list(),
        function_calls = list(),
        errors = list(),
        warnings = list()
      )
      
      self$log_info("Logs cleared successfully", "SYSTEM")
    },
    
    # Get current status
    get_status = function() {
      list(
        active = TRUE,
        session_id = private$session_id,
        uptime_minutes = as.numeric(difftime(Sys.time(), private$session_start, units = "mins")),
        total_logs = nrow(private$logs),
        current_memory_mb = private$get_memory_usage(),
        log_level = private$log_level
      )
    }
  ),
  
  # ---------------------------------------------------------------------------
  # Private Members
  # ---------------------------------------------------------------------------
  private = list(
    
    # Private fields
    app_name = NULL,
    log_level = "INFO",
    enable_console = TRUE,
    enable_file = FALSE,
    max_entries = 1000,
    session_start = NULL,
    session_id = NULL,
    logs = NULL,
    performance_data = NULL,
    timers = list(),
    log_levels = NULL,
    session_summary = NULL,
    
    # Write log entry
    write_log = function(level, message, module, performance_data = NULL) {
      # Check if log level is enabled
      if (private$log_levels[[level]] < private$log_levels[[private$log_level]]) {
        return(invisible(NULL))
      }
      
      timestamp <- Sys.time()
      
      # Get performance metrics if not provided
      if (is.null(performance_data)) {
        performance_data <- list(
          execution_time_ms = 0,
          memory_mb = private$get_memory_usage()
        )
      }
      
      # Extract performance metrics
      exec_time <- performance_data$execution_time_ms %||% 0
      memory_mb <- performance_data$memory_mb %||% private$get_memory_usage()
      cpu_percent <- performance_data$cpu_percent %||% 0
      
      # Create log entry
      log_entry <- data.frame(
        timestamp = timestamp,
        level = level,
        module = module,
        message = message,
        execution_time_ms = exec_time,
        memory_mb = memory_mb,
        cpu_percent = cpu_percent,
        session_id = private$session_id,
        stringsAsFactors = FALSE
      )
      
      # Add to logs
      private$logs <- rbind(private$logs, log_entry)
      
      # Maintain max entries limit
      if (nrow(private$logs) > private$max_entries) {
        private$logs <- tail(private$logs, private$max_entries * 0.8)
      }
      
      # Console output with colors
      if (private$enable_console) {
        private$print_colored_log(level, module, message, timestamp, exec_time, memory_mb)
      }
      
      # File output (if enabled)
      if (private$enable_file) {
        private$write_to_file(log_entry)
      }
      
      invisible(log_entry)
    },
    
    # Print colored console output
    print_colored_log = function(level, module, message, timestamp, exec_time, memory_mb) {
      time_str <- format(timestamp, "%H:%M:%S")
      
      # Color scheme
      colors <- list(
        "DEBUG" = crayon::silver,
        "INFO" = crayon::cyan,
        "WARN" = crayon::yellow,
        "ERROR" = crayon::red,
        "FATAL" = crayon::white$bgRed
      )
      
      color_func <- colors[[level]] %||% crayon::white
      
      # Format performance info
      perf_info <- if (exec_time > 0 || memory_mb > 0) {
        sprintf(" [%.1fms, %.1fMB]", exec_time, memory_mb)
      } else {
        ""
      }
      
      # Create formatted message
      formatted_msg <- sprintf("[%s] %s %s: %s%s",
                              time_str,
                              color_func(sprintf("%-5s", level)),
                              crayon::blue(sprintf("%-12s", module)),
                              message,
                              crayon::silver(perf_info))
      
      cat(formatted_msg, "\n")
    },
    
    # Get current memory usage in MB
    get_memory_usage = function() {
      if (Sys.info()["sysname"] == "Windows") {
        # Windows memory check
        tryCatch({
          gc_info <- gc()
          sum(gc_info[, "used"]) * 8 / 1024 / 1024  # Convert to MB
        }, error = function(e) 0)
      } else {
        # Unix-like systems
        tryCatch({
          system("ps -o pid,vsz -p $(echo $$)", intern = TRUE) %>%
            tail(1) %>%
            strsplit("\\s+") %>%
            .[[1]] %>%
            .[2] %>%
            as.numeric() / 1024  # Convert KB to MB
        }, error = function(e) {
          # Fallback to gc
          gc_info <- gc()
          sum(gc_info[, "used"]) * 8 / 1024 / 1024
        })
      }
    },
    
    # Get peak memory usage
    get_peak_memory = function() {
      if (nrow(private$logs) > 0) {
        max(private$logs$memory_mb, na.rm = TRUE)
      } else {
        private$get_memory_usage()
      }
    },
    
    # Get available memory
    get_available_memory = function() {
      tryCatch({
        if (Sys.info()["sysname"] == "Windows") {
          # Windows - rough estimate
          2048  # 2GB default assumption
        } else {
          # Unix-like systems
          system("free -m | awk 'NR==2{print $7}'", intern = TRUE) %>%
            as.numeric()
        }
      }, error = function(e) 1024)  # 1GB fallback
    },
    
    # Get top performing modules
    get_top_modules = function(type = "performance", limit = 5) {
      if (nrow(private$logs) == 0) return(data.frame())
      
      module_perf <- private$logs %>%
        dplyr::filter(execution_time_ms > 0) %>%
        dplyr::group_by(module) %>%
        dplyr::summarise(
          avg_time_ms = mean(execution_time_ms, na.rm = TRUE),
          avg_memory_mb = mean(memory_mb, na.rm = TRUE),
          total_calls = n(),
          .groups = "drop"
        )
      
      if (type == "performance") {
        # Best performing (lowest avg time)
        module_perf %>%
          dplyr::arrange(avg_time_ms) %>%
          head(limit)
      } else if (type == "slowest") {
        # Slowest performing
        module_perf %>%
          dplyr::arrange(desc(avg_time_ms)) %>%
          head(limit)
      }
    },
    
    # Write to file (if file logging enabled)
    write_to_file = function(log_entry) {
      # Implementation for file logging would go here
      # For now, just a placeholder
      invisible(NULL)
    }
  )
)

# =============================================================================
# Logger Module UI and Server Functions
# =============================================================================

# Logger UI for displaying logs in Shiny app
loggerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(class = "logger-container",
      fluidRow(
        column(12,
          h4("System Logs & Performance", class = "text-primary"),
          
          # Log controls
          fluidRow(
            column(3,
              selectInput(ns("level_filter"), "Log Level:",
                         choices = c("ALL" = "", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"),
                         selected = "INFO")
            ),
            column(3,
              selectInput(ns("module_filter"), "Module:",
                         choices = c("ALL" = ""),
                         selected = "")
            ),
            column(3,
              numericInput(ns("log_limit"), "Show Logs:", 
                          value = 50, min = 10, max = 500, step = 10)
            ),
            column(3,
              br(),
              actionButton(ns("refresh_logs"), "Refresh", 
                          class = "btn-outline-primary btn-sm"),
              actionButton(ns("clear_logs"), "Clear", 
                          class = "btn-outline-warning btn-sm ml-2")
            )
          ),
          
          hr(),
          
          # Performance summary
          fluidRow(
            column(12,
              h5("Performance Summary"),
              verbatimTextOutput(ns("performance_summary"))
            )
          ),
          
          hr(),
          
          # Logs table
          DT::dataTableOutput(ns("logs_table"))
        )
      )
    )
  )
}

# Logger Server
loggerServer <- function(id, atlas_logger) {
  moduleServer(id, function(input, output, session) {
    
    # Update module filter choices
    observe({
      if (!is.null(atlas_logger)) {
        logs <- atlas_logger$get_logs_for_display(1000)
        modules <- unique(logs$module)
        choices <- c("ALL" = "", modules)
        updateSelectInput(session, "module_filter", choices = choices)
      }
    })
    
    # Reactive logs data
    logs_data <- reactive({
      input$refresh_logs  # Dependency for refresh
      
      if (!is.null(atlas_logger)) {
        level_filter <- if (input$level_filter == "") NULL else input$level_filter
        module_filter <- if (input$module_filter == "") NULL else input$module_filter
        
        atlas_logger$get_logs_for_display(
          limit = input$log_limit,
          level_filter = level_filter,
          module_filter = module_filter
        )
      } else {
        data.frame()
      }
    })
    
    # Performance summary
    output$performance_summary <- renderText({
      if (!is.null(atlas_logger)) {
        summary <- atlas_logger$get_performance_summary()
        
        paste0(
          "Session: ", summary$session_info$session_id, "\n",
          "Uptime: ", summary$session_info$duration_minutes, " minutes\n",
          "Total Logs: ", summary$session_info$total_logs, "\n",
          "Current Memory: ", summary$performance_metrics$current_memory_mb, " MB\n",
          "Peak Memory: ", summary$performance_metrics$peak_memory_mb, " MB\n",
          "Avg Execution Time: ", summary$performance_metrics$avg_execution_time_ms, " ms\n",
          "Errors: ", summary$quality_metrics$error_count, 
          " (", summary$quality_metrics$error_rate, "%)\n",
          "Warnings: ", summary$quality_metrics$warning_count,
          " (", summary$quality_metrics$warning_rate, "%)"
        )
      } else {
        "Logger not initialized"
      }
    })
    
    # Logs table
    output$logs_table <- DT::renderDataTable({
      logs <- logs_data()
      
      if (nrow(logs) > 0) {
        display_logs <- logs %>%
          select(formatted_time, level, module, formatted_message, 
                 execution_time_ms, memory_mb) %>%
          rename(
            Time = formatted_time,
            Level = level,
            Module = module,
            Message = formatted_message,
            `Exec (ms)` = execution_time_ms,
            `Memory (MB)` = memory_mb
          )
        
        DT::datatable(
          display_logs,
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            order = list(list(0, "desc")),
            columnDefs = list(
              list(width = "80px", targets = 0),
              list(width = "60px", targets = 1),
              list(width = "100px", targets = 2),
              list(width = "400px", targets = 3)
            )
          ),
          class = "compact hover"
        ) %>%
          DT::formatStyle("Level",
            backgroundColor = DT::styleEqual(
              c("DEBUG", "INFO", "WARN", "ERROR", "FATAL"),
              c("#f8f9fa", "#d1ecf1", "#fff3cd", "#f8d7da", "#f5c6cb")
            )
          )
      }
    })
    
    # Clear logs
    observeEvent(input$clear_logs, {
      if (!is.null(atlas_logger)) {
        atlas_logger$clear_logs(keep_summary = TRUE)
        showNotification("Logs cleared successfully", type = "message")
      }
    })
    
    # Return logger status
    return(reactive({
      if (!is.null(atlas_logger)) {
        atlas_logger$get_status()
      } else {
        list(active = FALSE)
      }
    }))
  })
}

# =============================================================================
# Utility Functions
# =============================================================================

# Create global logger instance (to be called in global.R)
create_atlas_logger <- function() {
  AtlasLogger$new(
    app_name = "Atlas Labs HR Dashboard",
    log_level = "INFO",
    enable_console = TRUE,
    enable_file = FALSE,
    max_log_entries = 1000
  )
}

# Safe logging wrapper
safe_log <- function(logger, level, message, module = "MAIN", performance_data = NULL) {
  tryCatch({
    switch(level,
      "DEBUG" = logger$log_debug(message, module, performance_data),
      "INFO" = logger$log_info(message, module, performance_data),
      "WARN" = logger$log_warning(message, module, performance_data),
      "ERROR" = logger$log_error(message, module, performance_data),
      "FATAL" = logger$log_fatal(message, module, performance_data)
    )
  }, error = function(e) {
    cat("Logger error:", e$message, "\n")
  })
}