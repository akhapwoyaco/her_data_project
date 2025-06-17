# File: modules/data_loader_module.R
# Atlas Labs HR Analytics Dashboard - Data Loader Module
# Author: akhapwoyaco
# Purpose: Robust data loading, validation, and integrity checking

# Data Loader UI Module
dataLoaderUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
        div(class = "data-loader-container",
          h4("📊 Data Loading Status", class = "module-header"),
          
          # Data loading status cards
          fluidRow(
            column(4,
              div(class = "status-card employee-card",
                h5("Employee Data", class = "card-title"),
                div(id = ns("employee_status"), class = "status-indicator loading",
                  icon("spinner", class = "fa-spin"),
                  span("Loading...", class = "status-text")
                ),
                div(class = "data-info",
                  span("Records: ", class = "info-label"),
                  span(id = ns("employee_count"), "0", class = "info-value")
                )
              )
            ),
            column(4,
              div(class = "status-card performance-card",
                h5("Performance Data", class = "card-title"),
                div(id = ns("performance_status"), class = "status-indicator loading",
                  icon("spinner", class = "fa-spin"),
                  span("Loading...", class = "status-text")
                ),
                div(class = "data-info",
                  span("Records: ", class = "info-label"),
                  span(id = ns("performance_count"), "0", class = "info-value")
                )
              )
            ),
            column(4,
              div(class = "status-card education-card",
                h5("Education Levels", class = "card-title"),
                div(id = ns("education_status"), class = "status-indicator loading",
                  icon("spinner", class = "fa-spin"),
                  span("Loading...", class = "status-text")
                ),
                div(class = "data-info",
                  span("Records: ", class = "info-label"),
                  span(id = ns("education_count"), "0", class = "info-value")
                )
              )
            )
          ),
          
          # Data quality summary
          br(),
          conditionalPanel(
            condition = paste0("output['", ns("data_loaded"), "']"),
            div(class = "data-quality-panel",
              h5("🔍 Data Quality Summary", class = "panel-title"),
              div(id = ns("quality_summary"), class = "quality-content")
            )
          ),
          
          # Error/Warning messages
          div(id = ns("messages"), class = "message-container"),
          
          # Manual reload button (hidden by default)
          conditionalPanel(
            condition = paste0("output['", ns("show_reload"), "']"),
            br(),
            actionButton(ns("reload_data"), "🔄 Reload Data", 
                        class = "btn btn-warning btn-sm")
          )
        )
      )
    )
  )
}

# Data Loader Server Module
dataLoaderServer <- function(id, logger = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for data storage
    data_store <- reactiveValues(
      employee_raw = NULL,
      performance_raw = NULL,
      education_raw = NULL,
      merged_data = NULL,
      quality_report = NULL,
      load_status = list(
        employee = FALSE,
        performance = FALSE,
        education = FALSE,
        merged = FALSE
      ),
      error_messages = character(0)
    )
    
    # Log function wrapper
    log_message <- function(message, level = "info", performance_data = NULL) {
      if (!is.null(logger)) {
        switch(level,
          "info" = logger$log_info(message, "data_loader", performance_data),
          "warning" = logger$log_warning(message, "data_loader"),
          "error" = logger$log_error(message, "data_loader")
        )
      }
    }
    
    # Initialize data loading on module start
    observe({
      log_message("Initializing data loader module")
      load_all_data()
    })
    
    # Main data loading function
    load_all_data <- function() {
      start_time <- Sys.time()
      
      tryCatch({
        # Load employee data
        log_message("Starting employee data load")
        data_store$employee_raw <- load_employee_data()
        data_store$load_status$employee <- !is.null(data_store$employee_raw)
        update_ui_status("employee", data_store$load_status$employee, nrow(data_store$employee_raw %||% data.frame()))
        
        # Load performance data
        log_message("Starting performance data load")
        data_store$performance_raw <- load_performance_data()
        data_store$load_status$performance <- !is.null(data_store$performance_raw)
        update_ui_status("performance", data_store$load_status$performance, nrow(data_store$performance_raw %||% data.frame()))
        
        # Load education data
        log_message("Starting education data load")
        data_store$education_raw <- load_education_data()
        data_store$load_status$education <- !is.null(data_store$education_raw)
        update_ui_status("education", data_store$load_status$education, nrow(data_store$education_raw %||% data.frame()))
        
        # Validate data integrity
        if (all(unlist(data_store$load_status[1:3]))) {
          log_message("Starting data validation")
          validation_result <- validate_data_integrity(
            data_store$employee_raw,
            data_store$performance_raw,
            data_store$education_raw
          )
          
          if (validation_result$valid) {
            # Merge datasets
            log_message("Starting data merge")
            data_store$merged_data <- merge_datasets(
              data_store$employee_raw,
              data_store$performance_raw,
              data_store$education_raw
            )
            data_store$load_status$merged <- !is.null(data_store$merged_data)
            
            # Generate quality report
            data_store$quality_report <- generate_quality_report(data_store$merged_data)
            
            end_time <- Sys.time()
            load_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
            
            log_message(
              sprintf("Data loading completed successfully in %.2f seconds", load_time),
              "info",
              list(execution_time = load_time, memory_used = object.size(data_store$merged_data))
            )
            
          } else {
            data_store$error_messages <- c(data_store$error_messages, validation_result$errors)
            log_message(paste("Data validation failed:", paste(validation_result$errors, collapse = "; ")), "error")
          }
        }
        
      }, error = function(e) {
        error_msg <- sprintf("Critical error in data loading: %s", e$message)
        data_store$error_messages <- c(data_store$error_messages, error_msg)
        log_message(error_msg, "error")
      })
      
      # Update UI with messages
      update_message_display()
    }
    
    # Load employee data with validation and type conversion
    load_employee_data <- function() {
      file_path <- "data/employee.csv"
      
      if (!file.exists(file_path)) {
        stop("Employee data file not found: ", file_path)
      }
      
      tryCatch({
        # Read with proper encoding and error handling
        raw_data <- read.csv(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA", "NULL"))
        
        # Validate required columns
        required_cols <- c("EmployeeID", "FirstName", "LastName", "Gender", "Age", 
                          "Department", "JobRole", "Salary", "Attrition", "HireDate")
        missing_cols <- setdiff(required_cols, names(raw_data))
        
        if (length(missing_cols) > 0) {
          stop("Missing required columns in employee data: ", paste(missing_cols, collapse = ", "))
        }
        
        colnames(raw_data)[8] = "DistanceFromHome"
        
        # Data type conversions
        processed_data <- raw_data %>%
          mutate(
            EmployeeID = as.character(EmployeeID),
            Age = as.numeric(Age),
            Salary = as.numeric(gsub("[^0-9.]", "", as.character(Salary))),
            HireDate = as.Date(HireDate, format = "%Y-%m-%d"),
            Attrition = case_when(
              tolower(Attrition) %in% c("yes", "true", "1") ~ TRUE,
              tolower(Attrition) %in% c("no", "false", "0") ~ FALSE,
              TRUE ~ NA
            ),
            YearsAtCompany = as.numeric(YearsAtCompany),
            DistanceFromHome = as.numeric(DistanceFromHome),
            # Clean and standardize categorical variables
            Gender = str_to_title(trimws(Gender)),
            Department = str_to_title(trimws(Department)),
            JobRole = str_to_title(trimws(JobRole)),
            MaritalStatus = str_to_title(trimws(MaritalStatus)),
            BusinessTravel = str_to_title(trimws(BusinessTravel))
          ) %>%
          # Remove completely empty rows
          filter(!if_all(everything(), is.na))
        
        log_message(sprintf("Employee data loaded: %d records", nrow(processed_data)))
        return(processed_data)
        
      }, error = function(e) {
        stop("Error loading employee data: ", e$message)
      })
    }
    
    # Load performance data
    load_performance_data <- function() {
      file_path <- "data/performance_rating.csv"
      
      if (!file.exists(file_path)) {
        stop("Performance data file not found: ", file_path)
      }
      
      tryCatch({
        raw_data <- read.csv(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA", "NULL"), sep = ";")
        
        # Validate required columns
        required_cols <- c("PerformanceID", "EmployeeID", "ReviewDate", "JobSatisfaction", 
                          "EnvironmentSatisfaction", "WorkLifeBalance")
        missing_cols <- setdiff(required_cols, names(raw_data))
        
        if (length(missing_cols) > 0) {
          stop("Missing required columns in performance data: ", paste(missing_cols, collapse = ", "))
        }
        
        # Data type conversions
        processed_data <- raw_data %>%
          mutate(
            PerformanceID = as.character(PerformanceID),
            EmployeeID = as.character(EmployeeID),
            ReviewDate = as.Date(ReviewDate, format = "%Y-%m-%d"),
            # Convert satisfaction ratings to numeric (1-5 scale)
            across(c(EnvironmentSatisfaction, JobSatisfaction, RelationshipSatisfaction, 
                    WorkLifeBalance, SelfRating, ManagerRating), 
                   ~ as.numeric(pmin(pmax(as.numeric(.x), 1), 5))),
            # Training opportunities
            TrainingOpportunitiesWithinYear = as.numeric(TrainingOpportunitiesWithinYear),
            TrainingOpportunitiesTaken = as.numeric(TrainingOpportunitiesTaken)
          ) %>%
          filter(!if_all(everything(), is.na))
        
        log_message(sprintf("Performance data loaded: %d records", nrow(processed_data)))
        return(processed_data)
        
      }, error = function(e) {
        stop("Error loading performance data: ", e$message)
      })
    }
    
    # Load education level data
    load_education_data <- function() {
      file_path <- "data/education_level.csv"
      
      if (!file.exists(file_path)) {
        stop("Education level data file not found: ", file_path)
      }
      
      tryCatch({
        raw_data <- read.csv(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA", "NULL"))
        
        # Validate required columns
        required_cols <- c("EducationLevelID", "EducationLevel")
        missing_cols <- setdiff(required_cols, names(raw_data))
        
        if (length(missing_cols) > 0) {
          stop("Missing required columns in education data: ", paste(missing_cols, collapse = ", "))
        }
        
        # Data type conversions
        processed_data <- raw_data %>%
          mutate(
            Education = as.numeric(EducationLevelID),
            EducationLevel = trimws(EducationLevel)
          ) %>%
          dplyr::select(-EducationLevelID) %>% 
          filter(!if_all(everything(), is.na)) %>%
          distinct()  # Remove duplicates
        
        log_message(sprintf("Education data loaded: %d records", nrow(processed_data)))
        return(processed_data)
        
      }, error = function(e) {
        stop("Error loading education data: ", e$message)
      })
    }
    
    # Validate data integrity across datasets
    validate_data_integrity <- function(employee_data, performance_data, education_data) {
      errors <- character(0)
      warnings <- character(0)
      
      tryCatch({
        # Check for NULL datasets
        if (is.null(employee_data)) errors <- c(errors, "Employee data is NULL")
        if (is.null(performance_data)) errors <- c(errors, "Performance data is NULL")
        if (is.null(education_data)) errors <- c(errors, "Education data is NULL")
        
        if (length(errors) > 0) {
          return(list(valid = FALSE, errors = errors, warnings = warnings))
        }
        
        # Check for empty datasets
        if (nrow(employee_data) == 0) errors <- c(errors, "Employee data is empty")
        if (nrow(performance_data) == 0) errors <- c(errors, "Performance data is empty")
        if (nrow(education_data) == 0) errors <- c(errors, "Education data is empty")
        
        # Validate key relationships
        emp_ids <- unique(employee_data$EmployeeID)
        perf_ids <- unique(performance_data$EmployeeID)
        
        # Check for orphaned performance records
        orphaned_perf <- setdiff(perf_ids, emp_ids)
        if (length(orphaned_perf) > 0) {
          warnings <- c(warnings, sprintf("Found %d performance records without matching employees", length(orphaned_perf)))
        }
        
        # Check education level references
        emp_education <- unique(employee_data$Education[!is.na(employee_data$Education)])
        edu_levels <- unique(education_data$Education)
        missing_edu <- setdiff(emp_education, edu_levels)
        
        if (length(missing_edu) > 0) {
          warnings <- c(warnings, sprintf("Found %d education level references without definitions", length(missing_edu)))
        }
        
        # Validate data ranges
        if (any(employee_data$Age < 16 | employee_data$Age > 80, na.rm = TRUE)) {
          warnings <- c(warnings, "Found employees with unusual ages (< 16 or > 80)")
        }
        
        if (any(employee_data$Salary < 20000 | employee_data$Salary > 500000, na.rm = TRUE)) {
          warnings <- c(warnings, "Found employees with unusual salaries (< $20,000 or > $500,000)")
        }
        
        # Check for missing critical data
        missing_attrition <- sum(is.na(employee_data$Attrition))
        if (missing_attrition > nrow(employee_data) * 0.1) {
          warnings <- c(warnings, sprintf("High percentage of missing attrition data: %.1f%%", 
                                        missing_attrition / nrow(employee_data) * 100))
        }
        
        log_message(sprintf("Data validation completed: %d errors, %d warnings", length(errors), length(warnings)))
        
        return(list(
          valid = length(errors) == 0,
          errors = errors,
          warnings = warnings
        ))
        
      }, error = function(e) {
        return(list(
          valid = FALSE,
          errors = c("Validation process failed: ", e$message),
          warnings = character(0)
        ))
      })
    }
    
    # Merge all datasets into a comprehensive dataset
    merge_datasets <- function(employee_data, performance_data, education_data) {
      tryCatch({
        start_time <- Sys.time()
        
        # Start with employee data as base
        merged_data <- employee_data
        
        # Add education level descriptions
        merged_data <- merged_data %>%
          left_join(education_data, by = "Education", suffix = c("", "_lookup"))
        
        # Add performance data (most recent performance record per employee)
        latest_performance <- performance_data %>%
          group_by(EmployeeID) %>%
          arrange(desc(ReviewDate)) %>%
          slice(1) %>%
          ungroup() %>%
          select(-PerformanceID)  # Remove performance ID to avoid confusion
        
        merged_data <- merged_data %>%
          left_join(latest_performance, by = "EmployeeID", suffix = c("", "_perf"))
        
        # Add calculated fields
        merged_data <- merged_data %>%
          mutate(
            # Calculate years since hire
            YearsAtCompanyCalculated = as.numeric(difftime(Sys.Date(), HireDate, units = "days")) / 365.25,
            
            # Create age groups
            AgeGroup = case_when(
              Age < 25 ~ "Under 25",
              Age >= 25 & Age < 35 ~ "25-34",
              Age >= 35 & Age < 45 ~ "35-44",
              Age >= 45 & Age < 55 ~ "45-54",
              Age >= 55 ~ "55+",
              TRUE ~ "Unknown"
            ),
            
            # Create salary bands
            SalaryBand = case_when(
              Salary < 40000 ~ "< $40K",
              Salary >= 40000 & Salary < 60000 ~ "$40K-$60K",
              Salary >= 60000 & Salary < 80000 ~ "$60K-$80K",
              Salary >= 80000 & Salary < 100000 ~ "$80K-$100K",
              Salary >= 100000 ~ "$100K+",
              TRUE ~ "Unknown"
            ),
            
            # Overall satisfaction score (average of all satisfaction metrics)
            OverallSatisfaction = rowMeans(
              select(., EnvironmentSatisfaction, JobSatisfaction, 
                    RelationshipSatisfaction, WorkLifeBalance), 
              na.rm = TRUE
            ),
            
            # Training utilization rate
            TrainingUtilization = ifelse(
              TrainingOpportunitiesWithinYear > 0,
              TrainingOpportunitiesTaken / TrainingOpportunitiesWithinYear,
              NA
            ),
            
            # Performance gap (difference between self and manager rating)
            PerformanceGap = SelfRating - ManagerRating
          )
        
        # Add data quality flags
        merged_data <- merged_data %>%
          mutate(
            DataQualityFlags = case_when(
              is.na(Attrition) ~ "Missing Attrition",
              is.na(JobSatisfaction) ~ "Missing Performance Data",
              is.na(Salary) ~ "Missing Salary",
              TRUE ~ "Complete"
            )
          )
        
        end_time <- Sys.time()
        merge_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
        
        log_message(
          sprintf("Data merge completed: %d records in %.2f seconds", nrow(merged_data), merge_time),
          "info",
          list(execution_time = merge_time, records_merged = nrow(merged_data))
        )
        
        return(merged_data)
        
      }, error = function(e) {
        log_message(sprintf("Error merging datasets: %s", e$message), "error")
        return(NULL)
      })
    }
    
    # Generate data quality report
    generate_quality_report <- function(data) {
      if (is.null(data)) return(NULL)
      
      list(
        total_records = nrow(data),
        complete_records = sum(data$DataQualityFlags == "Complete"),
        completion_rate = round(sum(data$DataQualityFlags == "Complete") / nrow(data) * 100, 1),
        missing_attrition = sum(is.na(data$Attrition)),
        missing_performance = sum(is.na(data$JobSatisfaction)),
        missing_salary = sum(is.na(data$Salary)),
        duplicate_employees = sum(duplicated(data$EmployeeID)),
        date_range = list(
          earliest_hire = min(data$HireDate, na.rm = TRUE),
          latest_hire = max(data$HireDate, na.rm = TRUE)
        )
      )
    }
    
    # Update UI status indicators
    update_ui_status <- function(dataset, success, count) {
      status_class <- if (success) "success" else "error"
      status_icon <- if (success) "check-circle" else "exclamation-triangle"
      status_text <- if (success) "Loaded" else "Error"
      
      runjs(sprintf("
        $('#%s').removeClass('loading success error').addClass('%s');
        $('#%s .fa-spinner').removeClass('fa-spinner fa-spin').addClass('fa-%s');
        $('#%s .status-text').text('%s');
        $('#%s').text('%d');
      ", 
        session$ns(paste0(dataset, "_status")), status_class,
        session$ns(paste0(dataset, "_status")), status_icon,
        session$ns(paste0(dataset, "_status")), status_text,
        session$ns(paste0(dataset, "_count")), count
      ))
    }
    
    # Update message display
    update_message_display <- function() {
      if (length(data_store$error_messages) > 0) {
        error_html <- paste(
          sprintf('<div class="alert alert-danger"><i class="fa fa-exclamation-triangle"></i> %s</div>', 
                  data_store$error_messages), 
          collapse = ""
        )
        insertUI(
          selector = paste0("#", session$ns("messages")),
          where = "afterBegin",
          ui = HTML(error_html)
        )
      }
    }
    
    # Manual reload functionality
    observeEvent(input$reload_data, {
      log_message("Manual data reload initiated")
      data_store$error_messages <- character(0)
      removeUI(selector = paste0("#", session$ns("messages"), " .alert"))
      load_all_data()
    })
    
    # Output reactive values for use by other modules
    output$data_loaded <- reactive({
      all(unlist(data_store$load_status))
    })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    output$show_reload <- reactive({
      length(data_store$error_messages) > 0
    })
    outputOptions(output, "show_reload", suspendWhenHidden = FALSE)
    
    # Render quality summary
    output$quality_summary <- renderUI({
      req(data_store$quality_report)
      
      qr <- data_store$quality_report
      
      div(class = "quality-grid",
        div(class = "quality-item",
          strong("Total Records: "), span(qr$total_records, class = "metric-value")
        ),
        div(class = "quality-item",
          strong("Completion Rate: "), span(paste0(qr$completion_rate, "%"), class = "metric-value")
        ),
        div(class = "quality-item",
          strong("Date Range: "), span(
            paste(format(qr$date_range$earliest_hire, "%Y-%m-%d"), "to", 
                  format(qr$date_range$latest_hire, "%Y-%m-%d")), 
            class = "metric-value"
          )
        )
      )
    })
    
    # Return reactive data for other modules
    return(
      list(
        merged_data = reactive({ data_store$merged_data }),
        employee_data = reactive({ data_store$employee_raw }),
        performance_data = reactive({ data_store$performance_raw }),
        education_data = reactive({ data_store$education_raw }),
        quality_report = reactive({ data_store$quality_report }),
        is_loaded = reactive({ all(unlist(data_store$load_status)) }),
        load_status = reactive({ data_store$load_status })
      )
    )
  })
}