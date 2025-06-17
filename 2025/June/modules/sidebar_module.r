# =============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD
# Sidebar Module - Navigation & Filtering System
# Developer: akhapwoyaco (GitHub)
# =============================================================================

# UI Function ----------------------------------------------------------------
sidebarUI <- function(id) {
  ns <- NS(id)
  
  div(
    class = "atlas-sidebar",
    
    # Atlas Labs Logo & Title
    div(
      class = "sidebar-header",
      img(src = "atlas_labs_logo.png", class = "logo", height = "40px"),
      h4("Atlas Labs", class = "brand-title"),
      p("HR Analytics Dashboard", class = "brand-subtitle")
    ),
    
    # Navigation Menu
    div(
      class = "nav-section",
      h5("Navigation", class = "section-title"),
      
      div(
        class = "nav-menu",
        actionButton(ns("nav_overview"), "Overview", 
                    class = "nav-btn active", icon = icon("chart-line")),
        actionButton(ns("nav_attrition"), "Attrition Analysis", 
                    class = "nav-btn", icon = icon("user-minus")),
        actionButton(ns("nav_demographics"), "Demographics", 
                    class = "nav-btn", icon = icon("users")),
        actionButton(ns("nav_performance"), "Performance", 
                    class = "nav-btn", icon = icon("star")),
        actionButton(ns("nav_compensation"), "Compensation", 
                    class = "nav-btn", icon = icon("dollar-sign")),
        actionButton(ns("nav_satisfaction"), "Satisfaction", 
                    class = "nav-btn", icon = icon("heart")),
        actionButton(ns("nav_reports"), "Reports", 
                    class = "nav-btn", icon = icon("file-alt"))
      )
    ),
    
    # Smart Filters Section
    div(
      class = "filters-section",
      h5("Smart Filters", class = "section-title"),
      
      # Quick Filter Toggle
      div(
        class = "filter-toggle",
        checkboxInput(ns("enable_filters"), "Enable Advanced Filters", 
                     value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("enable_filters"), "']"),
          
          # Department Filter
          selectizeInput(
            ns("filter_department"),
            "Department:",
            choices = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "All Departments",
              plugins = list("remove_button", "drag_drop")
            )
          ),
          
          # Job Role Filter
          selectizeInput(
            ns("filter_job_role"),
            "Job Role:",
            choices = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "All Roles",
              plugins = list("remove_button")
            )
          ),
          
          # Age Range Filter
          div(
            class = "filter-group",
            p("Age Range:", class = "filter-label"),
            div(
              class = "range-inputs",
              numericInput(ns("age_min"), "Min:", value = 18, min = 18, max = 70, width = "70px"),
              numericInput(ns("age_max"), "Max:", value = 70, min = 18, max = 70, width = "70px")
            )
          ),
          
          # Salary Range Filter
          div(
            class = "filter-group",
            p("Salary Range ($):", class = "filter-label"),
            div(
              class = "range-inputs",
              numericInput(ns("salary_min"), "Min:", value = 30000, min = 0, step = 5000, width = "90px"),
              numericInput(ns("salary_max"), "Max:", value = 200000, min = 0, step = 5000, width = "90px")
            )
          ),
          
          # Gender Filter
          checkboxGroupInput(
            ns("filter_gender"),
            "Gender:",
            choices = c("Male", "Female", "Other"),
            selected = c("Male", "Female", "Other"),
            inline = TRUE
          ),
          
          # Attrition Filter
          radioButtons(
            ns("filter_attrition"),
            "Employee Status:",
            choices = list(
              "All Employees" = "all",
              "Active Only" = "active",
              "Departed Only" = "departed"
            ),
            selected = "all",
            inline = FALSE
          ),
          
          # Business Travel Filter
          selectizeInput(
            ns("filter_travel"),
            "Business Travel:",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "All Travel Types")
          ),
          
          # Education Level Filter
          selectizeInput(
            ns("filter_education"),
            "Education Level:",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "All Education Levels")
          ),
          
          # Filter Actions
          div(
            class = "filter-actions",
            actionButton(ns("apply_filters"), "Apply Filters", 
                        class = "btn-primary btn-sm", icon = icon("filter")),
            actionButton(ns("reset_filters"), "Reset All", 
                        class = "btn-secondary btn-sm", icon = icon("refresh"))
          )
        )
      )
    ),
    
    # Data Summary Section
    div(
      class = "data-summary",
      h5("Data Summary", class = "section-title"),
      div(
        class = "summary-stats",
        div(class = "stat-item",
            span("Total Records:", class = "stat-label"),
            span(textOutput(ns("total_records"), inline = TRUE), class = "stat-value")
        ),
        div(class = "stat-item",
            span("Filtered Records:", class = "stat-label"),
            span(textOutput(ns("filtered_records"), inline = TRUE), class = "stat-value")
        ),
        div(class = "stat-item",
            span("Active Filters:", class = "stat-label"),
            span(textOutput(ns("active_filters_count"), inline = TRUE), class = "stat-value")
        )
      )
    ),
    
    # Performance Monitor (Hidden by default)
    conditionalPanel(
      condition = "window.showPerformanceMonitor === true",
      div(
        class = "performance-monitor",
        h5("Performance Monitor", class = "section-title"),
        div(
          class = "perf-stats",
          div("Memory Usage:", textOutput(ns("memory_usage"), inline = TRUE)),
          div("Last Update:", textOutput(ns("last_update"), inline = TRUE)),
          div("Filter Time:", textOutput(ns("filter_time"), inline = TRUE))
        )
      )
    ),
    
    # Easter Egg Trigger (Hidden)
    div(id = ns("easter_egg_trigger"), style = "display: none;")
  )
}

# Server Function ------------------------------------------------------------
sidebarServer <- function(id, data, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize reactive values
    values <- reactiveValues(
      current_page = "overview",
      filters_applied = FALSE,
      filter_start_time = NULL,
      filtered_data = NULL,
      original_data = NULL
    )
    
    # Initialize data when available
    observe({
      req(data())
      values$original_data <- data()
      values$filtered_data <- data()
      
      # Populate filter choices
      updateFilterChoices()
      
      # Log initialization
      logger$log_info("Sidebar initialized with data", "sidebar_module", 
                     list(records = nrow(data())))
    })
    
    # Update filter choices based on data
    updateFilterChoices <- function() {
      req(values$original_data)
      
      employee_data <- values$original_data$employee
      
      # Update department choices
      dept_choices <- sort(unique(employee_data$Department))
      updateSelectizeInput(session, "filter_department", choices = dept_choices)
      
      # Update job role choices
      role_choices <- sort(unique(employee_data$JobRole))
      updateSelectizeInput(session, "filter_job_role", choices = role_choices)
      
      # Update business travel choices
      travel_choices <- sort(unique(employee_data$BusinessTravel))
      updateSelectizeInput(session, "filter_travel", choices = travel_choices)
      
      # Update education choices (if education data available)
      if (!is.null(values$original_data$education)) {
        edu_choices <- sort(unique(values$original_data$education$`Education Level`))
        updateSelectizeInput(session, "filter_education", choices = edu_choices)
      }
      
      # Update age and salary ranges based on data
      updateNumericInput(session, "age_min", value = min(employee_data$Age, na.rm = TRUE))
      updateNumericInput(session, "age_max", value = max(employee_data$Age, na.rm = TRUE))
      updateNumericInput(session, "salary_min", value = min(employee_data$Salary, na.rm = TRUE))
      updateNumericInput(session, "salary_max", value = max(employee_data$Salary, na.rm = TRUE))
    }
    
    # Navigation Handlers -------------------------------------------------------
    
    # Overview Navigation
    observeEvent(input$nav_overview, {
      values$current_page <- "overview"
      updateNavButtons("overview")
      logger$log_info("Navigated to Overview", "sidebar_module")
    })
    
    # Attrition Navigation
    observeEvent(input$nav_attrition, {
      values$current_page <- "attrition"
      updateNavButtons("attrition")
      logger$log_info("Navigated to Attrition Analysis", "sidebar_module")
    })
    
    # Demographics Navigation
    observeEvent(input$nav_demographics, {
      values$current_page <- "demographics"
      updateNavButtons("demographics")
      logger$log_info("Navigated to Demographics", "sidebar_module")
    })
    
    # Performance Navigation
    observeEvent(input$nav_performance, {
      values$current_page <- "performance"
      updateNavButtons("performance")
      logger$log_info("Navigated to Performance", "sidebar_module")
    })
    
    # Compensation Navigation
    observeEvent(input$nav_compensation, {
      values$current_page <- "compensation"
      updateNavButtons("compensation")
      logger$log_info("Navigated to Compensation", "sidebar_module")
    })
    
    # Satisfaction Navigation
    observeEvent(input$nav_satisfaction, {
      values$current_page <- "satisfaction"
      updateNavButtons("satisfaction")
      logger$log_info("Navigated to Satisfaction", "sidebar_module")
    })
    
    # Reports Navigation
    observeEvent(input$nav_reports, {
      values$current_page <- "reports"
      updateNavButtons("reports")
      logger$log_info("Navigated to Reports", "sidebar_module")
    })
    
    # Update navigation button states
    updateNavButtons <- function(active_page) {
      buttons <- c("overview", "attrition", "demographics", "performance", 
                   "compensation", "satisfaction", "reports")
      
      for (btn in buttons) {
        if (btn == active_page) {
          addClass(id = paste0("nav_", btn), class = "active")
        } else {
          removeClass(id = paste0("nav_", btn), class = "active")
        }
      }
    }
    
    # Filter Handlers ------------------------------------------------------------
    
    # Apply Filters
    observeEvent(input$apply_filters, {
      req(values$original_data)
      
      values$filter_start_time <- Sys.time()
      
      # Apply filters to data
      filtered_data <- applyFilters()
      values$filtered_data <- filtered_data
      values$filters_applied <- TRUE
      
      # Calculate filter execution time
      filter_time <- as.numeric(difftime(Sys.time(), values$filter_start_time, units = "secs"))
      
      # Log filter application
      logger$log_info(
        paste("Filters applied:", getActiveFiltersCount(), "active filters"),
        "sidebar_module",
        list(
          execution_time = filter_time,
          filtered_records = nrow(filtered_data$employee),
          original_records = nrow(values$original_data$employee)
        )
      )
      
      # Show success notification
      showNotification("Filters applied successfully!", type = "success", duration = 2)
    })
    
    # Reset Filters
    observeEvent(input$reset_filters, {
      # Reset all filter inputs
      updateSelectizeInput(session, "filter_department", selected = character(0))
      updateSelectizeInput(session, "filter_job_role", selected = character(0))
      updateSelectizeInput(session, "filter_travel", selected = character(0))
      updateSelectizeInput(session, "filter_education", selected = character(0))
      
      # Reset numeric inputs to data ranges
      if (!is.null(values$original_data)) {
        employee_data <- values$original_data$employee
        updateNumericInput(session, "age_min", value = min(employee_data$Age, na.rm = TRUE))
        updateNumericInput(session, "age_max", value = max(employee_data$Age, na.rm = TRUE))
        updateNumericInput(session, "salary_min", value = min(employee_data$Salary, na.rm = TRUE))
        updateNumericInput(session, "salary_max", value = max(employee_data$Salary, na.rm = TRUE))
      }
      
      # Reset radio buttons and checkboxes
      updateRadioButtons(session, "filter_attrition", selected = "all")
      updateCheckboxGroupInput(session, "filter_gender", 
                              selected = c("Male", "Female", "Other"))
      
      # Reset filtered data
      values$filtered_data <- values$original_data
      values$filters_applied <- FALSE
      
      logger$log_info("All filters reset", "sidebar_module")
      showNotification("All filters reset!", type = "warning", duration = 2)
    })
    
    # Apply Filters Function
    applyFilters <- function() {
      req(values$original_data)
      
      employee_data <- values$original_data$employee
      performance_data <- values$original_data$performance
      education_data <- values$original_data$education
      
      # Apply department filter
      if (!is.null(input$filter_department) && length(input$filter_department) > 0) {
        employee_data <- employee_data %>% 
          filter(Department %in% input$filter_department)
      }
      
      # Apply job role filter
      if (!is.null(input$filter_job_role) && length(input$filter_job_role) > 0) {
        employee_data <- employee_data %>% 
          filter(JobRole %in% input$filter_job_role)
      }
      
      # Apply age range filter
      if (!is.null(input$age_min) && !is.null(input$age_max)) {
        employee_data <- employee_data %>% 
          filter(Age >= input$age_min & Age <= input$age_max)
      }
      
      # Apply salary range filter
      if (!is.null(input$salary_min) && !is.null(input$salary_max)) {
        employee_data <- employee_data %>% 
          filter(Salary >= input$salary_min & Salary <= input$salary_max)
      }
      
      # Apply gender filter
      if (!is.null(input$filter_gender) && length(input$filter_gender) > 0) {
        employee_data <- employee_data %>% 
          filter(Gender %in% input$filter_gender)
      }
      
      # Apply attrition filter
      if (!is.null(input$filter_attrition) && input$filter_attrition != "all") {
        if (input$filter_attrition == "active") {
          employee_data <- employee_data %>% filter(Attrition == "No")
        } else if (input$filter_attrition == "departed") {
          employee_data <- employee_data %>% filter(Attrition == "Yes")
        }
      }
      
      # Apply business travel filter
      if (!is.null(input$filter_travel) && length(input$filter_travel) > 0) {
        employee_data <- employee_data %>% 
          filter(BusinessTravel %in% input$filter_travel)
      }
      
      # Filter performance data to match filtered employees
      if (!is.null(performance_data)) {
        performance_data <- performance_data %>% 
          filter(EmployeeID %in% employee_data$`Employee ID`)
      }
      
      return(list(
        employee = employee_data,
        performance = performance_data,
        education = education_data
      ))
    }
    
    # Helper function to count active filters
    getActiveFiltersCount <- function() {
      count <- 0
      
      if (!is.null(input$filter_department) && length(input$filter_department) > 0) count <- count + 1
      if (!is.null(input$filter_job_role) && length(input$filter_job_role) > 0) count <- count + 1
      if (!is.null(input$filter_travel) && length(input$filter_travel) > 0) count <- count + 1
      if (!is.null(input$filter_education) && length(input$filter_education) > 0) count <- count + 1
      if (!is.null(input$filter_gender) && length(input$filter_gender) != 3) count <- count + 1
      if (!is.null(input$filter_attrition) && input$filter_attrition != "all") count <- count + 1
      
      # Check if age/salary ranges are modified from defaults
      if (!is.null(values$original_data)) {
        emp_data <- values$original_data$employee
        default_age_min <- min(emp_data$Age, na.rm = TRUE)
        default_age_max <- max(emp_data$Age, na.rm = TRUE)
        default_sal_min <- min(emp_data$Salary, na.rm = TRUE)
        default_sal_max <- max(emp_data$Salary, na.rm = TRUE)
        
        if (!is.null(input$age_min) && !is.null(input$age_max) &&
            (input$age_min != default_age_min || input$age_max != default_age_max)) {
          count <- count + 1
        }
        
        if (!is.null(input$salary_min) && !is.null(input$salary_max) &&
            (input$salary_min != default_sal_min || input$salary_max != default_sal_max)) {
          count <- count + 1
        }
      }
      
      return(count)
    }
    
    # Output Handlers ------------------------------------------------------------
    
    # Total records count
    output$total_records <- renderText({
      if (!is.null(values$original_data)) {
        scales::comma(nrow(values$original_data$employee))
      } else {
        "Loading..."
      }
    })
    
    # Filtered records count
    output$filtered_records <- renderText({
      if (!is.null(values$filtered_data)) {
        scales::comma(nrow(values$filtered_data$employee))
      } else {
        "0"
      }
    })
    
    # Active filters count
    output$active_filters_count <- renderText({
      if (input$enable_filters) {
        getActiveFiltersCount()
      } else {
        "0"
      }
    })
    
    # Performance monitoring outputs
    output$memory_usage <- renderText({
      paste0(round(as.numeric(object.size(.GlobalEnv)) / 1024^2, 1), " MB")
    })
    
    output$last_update <- renderText({
      if (!is.null(values$filter_start_time)) {
        format(values$filter_start_time, "%H:%M:%S")
      } else {
        "N/A"
      }
    })
    
    output$filter_time <- renderText({
      if (!is.null(values$filter_start_time)) {
        filter_time <- as.numeric(difftime(Sys.time(), values$filter_start_time, units = "secs"))
        paste0(round(filter_time, 3), "s")
      } else {
        "N/A"
      }
    })
    
    # Return reactive values for other modules ----------------------------------
    return(list(
      current_page = reactive(values$current_page),
      filtered_data = reactive(values$filtered_data),
      filters_applied = reactive(values$filters_applied),
      active_filters = reactive({
        list(
          department = input$filter_department,
          job_role = input$filter_job_role,
          age_range = c(input$age_min, input$age_max),
          salary_range = c(input$salary_min, input$salary_max),
          gender = input$filter_gender,
          attrition = input$filter_attrition,
          travel = input$filter_travel,
          education = input$filter_education
        )
      })
    ))
  })
}