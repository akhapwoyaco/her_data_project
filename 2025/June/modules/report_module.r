# =============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD
# Report Generation Module
# File: modules/report_module.R
# Author: akhapwoyaco (GitHub)
# =============================================================================

# Report UI Module ----
reportUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Report Header
    div(class = "report-header",
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                 color: white; padding: 20px; border-radius: 10px; margin-bottom: 20px;",
        h3("📊 Generate HR Analytics Report", style = "margin: 0; font-weight: 600;"),
        p("Create comprehensive reports with dynamic insights and recommendations", 
          style = "margin: 5px 0 0 0; opacity: 0.9;")
    ),
    
    # Report Configuration Panel
    fluidRow(
      column(6,
        wellPanel(
          h4("📋 Report Configuration", style = "color: #2c3e50; margin-top: 0;"),
          
          # Report Type Selection
          radioButtons(
            ns("report_format"),
            "Report Format:",
            choices = list(
              "📄 HTML Interactive" = "html",
              "📑 PDF Professional" = "pdf",
              "📊 Both Formats" = "both"
            ),
            selected = "html",
            inline = TRUE
          ),
          
          # Report Sections
          checkboxGroupInput(
            ns("report_sections"),
            "Include Sections:",
            choices = list(
              "Executive Summary" = "executive",
              "Demographics Analysis" = "demographics", 
              "Attrition Analysis" = "attrition",
              "Performance Metrics" = "performance",
              "Satisfaction Analysis" = "satisfaction",
              "Compensation Analysis" = "compensation",
              "Recommendations" = "recommendations"
            ),
            selected = c("executive", "demographics", "attrition", "performance"),
            inline = FALSE
          ),
          
          # Date Range
          dateRangeInput(
            ns("report_date_range"),
            "Analysis Period:",
            start = Sys.Date() - 365,
            end = Sys.Date(),
            format = "yyyy-mm-dd"
          )
        )
      ),
      
      column(6,
        wellPanel(
          h4("🎯 Report Filters", style = "color: #2c3e50; margin-top: 0;"),
          
          # Department Filter
          selectizeInput(
            ns("filter_departments"),
            "Departments:",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "All Departments")
          ),
          
          # Job Role Filter
          selectizeInput(
            ns("filter_roles"),
            "Job Roles:",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "All Roles")
          ),
          
          # Age Range
          sliderInput(
            ns("filter_age_range"),
            "Age Range:",
            min = 18, max = 65,
            value = c(18, 65),
            step = 1
          ),
          
          # Include Terminated Employees
          checkboxInput(
            ns("include_terminated"),
            "Include Terminated Employees",
            value = TRUE
          )
        )
      )
    ),
    
    # Report Preview & Actions
    fluidRow(
      column(12,
        wellPanel(
          h4("🔍 Report Preview", style = "color: #2c3e50; margin-top: 0;"),
          
          # Preview Summary
          div(id = ns("preview_summary"),
              style = "background: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
              verbatimTextOutput(ns("report_preview"), placeholder = TRUE)
          ),
          
          # Action Buttons
          div(class = "text-center",
              style = "margin-top: 20px;",
              actionButton(
                ns("generate_report"),
                "🚀 Generate Report",
                class = "btn btn-primary btn-lg",
                style = "margin-right: 10px; background: linear-gradient(45deg, #28a745, #20c997);"
              ),
              actionButton(
                ns("preview_report"),
                "👁️ Preview Report",
                class = "btn btn-info btn-lg",
                style = "margin-right: 10px;"
              ),
              downloadButton(
                ns("download_report"),
                "📥 Download Report",
                class = "btn btn-success btn-lg"
              )
          )
        )
      )
    ),
    
    # Report Status & Progress
    fluidRow(
      column(12,
        conditionalPanel(
          condition = paste0("input['", ns("report_status"), "'] == 'generating'"),
          wellPanel(
            h4("⏳ Generating Report...", style = "color: #e67e22;"),
            div(class = "progress",
                div(class = "progress-bar progress-bar-striped progress-bar-animated",
                    role = "progressbar",
                    style = "width: 100%; background: linear-gradient(45deg, #667eea, #764ba2);")
            ),
            textOutput(ns("generation_status"))
          )
        )
      )
    ),
    
    # Report History
    fluidRow(
      column(12,
        wellPanel(
          h4("📚 Report History", style = "color: #2c3e50; margin-top: 0;"),
          DT::dataTableOutput(ns("report_history"))
        )
      )
    )
  )
}

# Report Server Module ----
reportServer <- function(id, shared_data, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize logger
    logger$log_info("Report module initialized", "report_module")
    
    # Reactive Values ----
    report_data <- reactiveValues(
      filtered_data = NULL,
      report_params = NULL,
      generation_status = "ready",
      history = data.frame(
        date = character(),
        format = character(),
        sections = character(),
        filters = character(),
        status = character(),
        stringsAsFactors = FALSE
      )
    )
    
    # Update Filter Choices ----
    observeEvent(shared_data$employee_data, {
      req(shared_data$employee_data)
      
      logger$log_info("Updating filter choices", "report_module")
      
      # Update department choices
      dept_choices <- unique(shared_data$employee_data$Department)
      updateSelectizeInput(session, "filter_departments", choices = dept_choices)
      
      # Update role choices
      role_choices <- unique(shared_data$employee_data$JobRole)
      updateSelectizeInput(session, "filter_roles", choices = role_choices)
      
      # Update age range
      age_range <- range(shared_data$employee_data$Age, na.rm = TRUE)
      updateSliderInput(session, "filter_age_range", 
                       min = age_range[1], max = age_range[2], 
                       value = age_range)
    })
    
    # Generate Filtered Data ----
    filtered_data <- reactive({
      req(shared_data$employee_data)
      
      start_time <- Sys.time()
      logger$log_info("Filtering data for report", "report_module")
      
      data <- shared_data$employee_data
      
      # Apply filters
      if (!is.null(input$filter_departments) && length(input$filter_departments) > 0) {
        data <- data %>% filter(Department %in% input$filter_departments)
      }
      
      if (!is.null(input$filter_roles) && length(input$filter_roles) > 0) {
        data <- data %>% filter(JobRole %in% input$filter_roles)
      }
      
      if (!is.null(input$filter_age_range)) {
        data <- data %>% filter(Age >= input$filter_age_range[1], 
                               Age <= input$filter_age_range[2])
      }
      
      if (!input$include_terminated) {
        data <- data %>% filter(Attrition == "No")
      }
      
      # Apply date range if HireDate exists
      if ("HireDate" %in% names(data) && !is.null(input$report_date_range)) {
        data <- data %>% 
          filter(HireDate >= input$report_date_range[1],
                 HireDate <= input$report_date_range[2])
      }
      
      end_time <- Sys.time()
      logger$log_info(paste("Data filtering completed in", 
                           round(as.numeric(end_time - start_time), 2), "seconds"),
                     "report_module")
      
      data
    })
    
    # Report Preview ----
    output$report_preview <- renderText({
      req(filtered_data())
      
      data <- filtered_data()
      sections <- input$report_sections
      
      preview_text <- paste0(
        "📊 REPORT PREVIEW\n",
        "================\n",
        "• Format: ", toupper(input$report_format), "\n",
        "• Employee Records: ", nrow(data), "\n",
        "• Sections: ", length(sections), " selected\n",
        "• Date Range: ", input$report_date_range[1], " to ", input$report_date_range[2], "\n",
        "• Filters Applied: ",
        ifelse(is.null(input$filter_departments), "All Departments", 
               paste(length(input$filter_departments), "departments")), ", ",
        ifelse(is.null(input$filter_roles), "All Roles", 
               paste(length(input$filter_roles), "roles")), "\n",
        "• Status: Ready to generate"
      )
      
      if (nrow(data) == 0) {
        preview_text <- paste0(preview_text, "\n\n⚠️ WARNING: No data matches current filters!")
      }
      
      preview_text
    })
    
    # Generate Executive Summary ----
    generate_executive_summary <- function(data) {
      logger$log_info("Generating executive summary", "report_module")
      
      if (nrow(data) == 0) return(list())
      
      # Calculate key metrics
      total_employees <- nrow(data)
      attrition_rate <- mean(data$Attrition == "Yes", na.rm = TRUE)
      avg_age <- mean(data$Age, na.rm = TRUE)
      avg_salary <- mean(data$Salary, na.rm = TRUE)
      
      # Performance metrics if available
      performance_data <- shared_data$performance_data
      avg_job_satisfaction <- NULL
      avg_manager_rating <- NULL
      
      if (!is.null(performance_data)) {
        perf_subset <- performance_data %>% 
          filter(EmployeeID %in% data$EmployeeID)
        
        avg_job_satisfaction <- mean(perf_subset$JobSatisfaction, na.rm = TRUE)
        avg_manager_rating <- mean(perf_subset$ManagerRating, na.rm = TRUE)
      }
      
      # Departmental analysis
      dept_summary <- data %>%
        group_by(Department) %>%
        summarise(
          count = n(),
          attrition_rate = mean(Attrition == "Yes", na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(attrition_rate))
      
      # Generate recommendations
      recommendations <- list()
      
      if (attrition_rate > 0.15) {
        recommendations <- append(recommendations, list(list(
          title = "High Attrition Alert",
          description = paste("Current attrition rate of", 
                            scales::percent(attrition_rate, accuracy = 0.1),
                            "exceeds industry benchmark. Focus on retention strategies."),
          priority = "High"
        )))
      }
      
      if (nrow(dept_summary) > 0 && max(dept_summary$attrition_rate) > 0.25) {
        high_attrition_dept <- dept_summary$Department[which.max(dept_summary$attrition_rate)]
        recommendations <- append(recommendations, list(list(
          title = "Department-Specific Intervention",
          description = paste("The", high_attrition_dept, 
                            "department shows elevated attrition. Investigate department-specific factors."),
          priority = "Medium"
        )))
      }
      
      list(
        kpi_metrics = list(
          total_employees = total_employees,
          attrition_rate = attrition_rate,
          avg_age = avg_age,
          avg_salary = avg_salary,
          avg_satisfaction = avg_job_satisfaction,
          avg_manager_rating = avg_manager_rating
        ),
        departmental_summary = dept_summary,
        recommendations = recommendations
      )
    }
    
    # Create Downloadable Report ----
    create_downloadable_report <- function(format = "html") {
      logger$log_info(paste("Creating", format, "report"), "report_module")
      
      data <- filtered_data()
      executive_summary <- generate_executive_summary(data)
      
      # Prepare report parameters
      params <- list(
        data_summary = list(
          total_records = nrow(data),
          date_range = input$report_date_range,
          filters_applied = list(
            departments = input$filter_departments,
            roles = input$filter_roles,
            age_range = input$filter_age_range,
            include_terminated = input$include_terminated
          ),
          data_quality = list(
            "Complete Records" = sum(complete.cases(data)),
            "Missing Values" = sum(!complete.cases(data)),
            "Data Completeness" = scales::percent(mean(complete.cases(data)))
          )
        ),
        selected_filters = list(
          departments = input$filter_departments,
          roles = input$filter_roles,
          age_range = paste(input$filter_age_range, collapse = "-"),
          include_terminated = input$include_terminated
        ),
        analysis_date = Sys.Date(),
        kpi_metrics = executive_summary$kpi_metrics,
        attrition_analysis = list(
          by_department = executive_summary$departmental_summary
        ),
        performance_data = shared_data$performance_data,
        satisfaction_metrics = list(
          job_satisfaction = executive_summary$kpi_metrics$avg_satisfaction
        ),
        compensation_analysis = list(
          avg_salary = executive_summary$kpi_metrics$avg_salary
        ),
        recommendations = executive_summary$recommendations
      )
      
      # Generate report
      output_format <- switch(format,
        "html" = "html_document",
        "pdf" = "pdf_document",
        "both" = c("html_document", "pdf_document")
      )
      
      temp_dir <- tempdir()
      report_file <- file.path(temp_dir, paste0("atlas_labs_report_", 
                                               format(Sys.time(), "%Y%m%d_%H%M%S")))
      
      tryCatch({
        rmarkdown::render(
          input = "reports/hr_analytics_template.Rmd",
          output_format = output_format,
          output_file = paste0(basename(report_file), ".", 
                              ifelse(format == "pdf", "pdf", "html")),
          output_dir = temp_dir,
          params = params,
          quiet = TRUE
        )
        
        # Update report history
        new_record <- data.frame(
          date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          format = toupper(format),
          sections = paste(input$report_sections, collapse = ", "),
          filters = paste(length(input$filter_departments %||% c()), "depts,",
                         length(input$filter_roles %||% c()), "roles"),
          status = "✅ Success",
          stringsAsFactors = FALSE
        )
        
        report_data$history <- rbind(report_data$history, new_record)
        
        logger$log_info("Report generated successfully", "report_module")
        
        paste0(report_file, ".", ifelse(format == "pdf", "pdf", "html"))
        
      }, error = function(e) {
        logger$log_error(paste("Report generation failed:", e$message), "report_module")
        NULL
      })
    }
    
    # Event Handlers ----
    
    # Preview Report
    observeEvent(input$preview_report, {
      logger$log_info("Previewing report", "report_module")
      
      showModal(modalDialog(
        title = "📋 Report Preview",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        
        div(
          h4("Report Configuration"),
          tags$ul(
            tags$li("Format: ", tags$strong(toupper(input$report_format))),
            tags$li("Sections: ", tags$strong(length(input$report_sections))),
            tags$li("Records: ", tags$strong(nrow(filtered_data()))),
            tags$li("Date Range: ", tags$strong(paste(input$report_date_range, collapse = " to ")))
          ),
          
          h4("Included Sections"),
          renderUI({
            section_names <- c(
              "executive" = "Executive Summary",
              "demographics" = "Demographics Analysis",
              "attrition" = "Attrition Analysis", 
              "performance" = "Performance Metrics",
              "satisfaction" = "Satisfaction Analysis",
              "compensation" = "Compensation Analysis",
              "recommendations" = "Recommendations"
            )
            
            tags$ul(
              lapply(input$report_sections, function(section) {
                tags$li(section_names[section])
              })
            )
          })
        )
      ))
    })
    
    # Generate Report
    observeEvent(input$generate_report, {
      req(nrow(filtered_data()) > 0)
      
      logger$log_info("Starting report generation", "report_module")
      report_data$generation_status <- "generating"
      
      showNotification("🚀 Report generation started...", type = "message", duration = 3)
      
      # Simulate progress (in real implementation, this would track actual progress)
      future({
        Sys.sleep(2) # Simulate processing time
        create_downloadable_report(input$report_format)
      }) %...>% {
        report_data$generation_status <- "completed"
        showNotification("✅ Report generated successfully!", type = "success", duration = 5)
        logger$log_info("Report generation completed", "report_module")
      }
    })
    
    # Download Handler
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("atlas_labs_hr_report_", format(Sys.Date(), "%Y%m%d"), 
               ".", ifelse(input$report_format == "pdf", "pdf", "html"))
      },
      content = function(file) {
        logger$log_info("Downloading report", "report_module")
        
        # Generate report
        report_file <- create_downloadable_report(input$report_format)
        
        if (!is.null(report_file) && file.exists(report_file)) {
          file.copy(report_file, file)
          logger$log_info("Report downloaded successfully", "report_module")
        } else {
          logger$log_error("Report file not found for download", "report_module")
        }
      },
      contentType = ifelse(input$report_format == "pdf", "application/pdf", "text/html")
    )
    
    # Report History Table
    output$report_history <- DT::renderDataTable({
      req(nrow(report_data$history) > 0)
      
      DT::datatable(
        report_data$history,
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          order = list(list(0, 'desc'))
        ),
        rownames = FALSE,
        class = "table table-striped table-hover"
      )
    })
    
    # Generation Status
    output$generation_status <- renderText({
      switch(report_data$generation_status,
        "generating" = "Compiling report sections and generating visualizations...",
        "completed" = "Report generation completed successfully!",
        "error" = "An error occurred during report generation.",
        "ready" = "Ready to generate report."
      )
    })
    
    # Return reactive values for inter-module communication
    return(
      list(
        report_data = reactive(report_data),
        filtered_data = filtered_data,
        generate_summary = reactive(generate_executive_summary(filtered_data()))
      )
    )
  })
}