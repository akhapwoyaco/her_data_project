# ==============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD
# Overview Module - Executive KPI Dashboard
# File: modules/overview_module.R
# Author: akhapwoyaco (GitHub)
# ==============================================================================

# UI Function
overviewUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom CSS for KPI cards
    tags$style(HTML("
      .kpi-card {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border-radius: 15px;
        padding: 20px;
        color: white;
        text-align: center;
        box-shadow: 0 8px 32px rgba(0,0,0,0.1);
        transition: transform 0.3s ease, box-shadow 0.3s ease;
        margin-bottom: 20px;
        position: relative;
        overflow: hidden;
      }
      .kpi-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 12px 40px rgba(0,0,0,0.15);
      }
      .kpi-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(255,255,255,0.2), transparent);
        transition: left 0.5s;
      }
      .kpi-card:hover::before {
        left: 100%;
      }
      .kpi-value {
        font-size: 2.5rem;
        font-weight: bold;
        margin: 10px 0;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
      }
      .kpi-label {
        font-size: 1.1rem;
        opacity: 0.9;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      .kpi-icon {
        font-size: 2rem;
        margin-bottom: 10px;
        opacity: 0.8;
      }
      .overview-section {
        background: white;
        border-radius: 10px;
        padding: 25px;
        margin-bottom: 20px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        border: 1px solid #e8ecef;
      }
      .section-title {
        color: #2c3e50;
        font-size: 1.4rem;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #3498db;
      }
      .chart-container {
        background: #fafbfc;
        border-radius: 8px;
        padding: 15px;
        margin: 10px 0;
      }
    ")),
    
    # Page Header
    div(class = "overview-section",
        h2("📊 Executive Overview", class = "section-title"),
        p("Real-time insights into Atlas Labs workforce metrics and key performance indicators.", 
          style = "color: #6c757d; font-size: 1.1rem; margin: 0;")
    ),
    
    # KPI Cards Row
    fluidRow(
      column(3, div(class = "kpi-card", uiOutput(ns("total_employees_card")))),
      column(3, div(class = "kpi-card", uiOutput(ns("attrition_rate_card")))),
      column(3, div(class = "kpi-card", uiOutput(ns("avg_satisfaction_card")))),
      column(3, div(class = "kpi-card", uiOutput(ns("avg_salary_card"))))
    ),
    
    # Charts Section
    fluidRow(
      # Employee Distribution
      column(6,
        div(class = "overview-section",
            h3("👥 Employee Distribution", class = "section-title"),
            div(class = "chart-container",
                plotlyOutput(ns("employee_distribution_chart"), height = "350px")
            )
        )
      ),
      
      # Attrition Overview
      column(6,
        div(class = "overview-section",
            h3("📈 Attrition Analysis", class = "section-title"),
            div(class = "chart-container",
                plotlyOutput(ns("attrition_overview_chart"), height = "350px")
            )
        )
      )
    ),
    
    # Department Breakdown
    fluidRow(
      column(12,
        div(class = "overview-section",
            h3("🏢 Department Breakdown", class = "section-title"),
            div(class = "chart-container",
                plotlyOutput(ns("department_breakdown_chart"), height = "400px")
            )
        )
      )
    ),
    
    # Summary Statistics Table
    fluidRow(
      column(12,
        div(class = "overview-section",
            h3("📋 Summary Statistics", class = "section-title"),
            DT::dataTableOutput(ns("summary_stats_table"))
        )
      )
    )
  )
}

# Server Function
overviewServer <- function(id, data, logger) {
  moduleServer(id, function(input, output, session) {
    
    # Log module initialization
    logger$log_info("Overview module initialized", "overview_module")
    
    # Reactive values for performance tracking
    perf_start <- Sys.time()
    
    # Generate KPI metrics
    kpi_metrics <- reactive({
      req(data())
      logger$log_info("Generating KPI metrics", "overview_module")
      
      # print(data$employee_data)
      tryCatch({
        employee_data <- data$employee_data
        performance_data <- data$performance_data
        
        # Calculate key metrics
        total_employees <- nrow(employee_data)
        print(total_employees)
        attrition_count <- sum(employee_data$Attrition == "Yes", na.rm = TRUE)
        attrition_rate <- if(total_employees > 0) attrition_count / total_employees else 0
        
        # Average satisfaction from performance data
        avg_satisfaction <- if(!is.null(performance_data)) {
          mean(performance_data$JobSatisfaction, na.rm = TRUE)
        } else { 0 }
        
        # Average salary
        avg_salary <- mean(employee_data$Salary, na.rm = TRUE)
        
        list(
          total_employees = total_employees,
          attrition_rate = attrition_rate,
          avg_satisfaction = avg_satisfaction,
          avg_salary = avg_salary
        )
      }, error = function(e) {
        logger$log_error(paste("Error calculating KPI metrics:", e$message), "overview_module")
        list(total_employees = 0, attrition_rate = 0, avg_satisfaction = 0, avg_salary = 0)
      })
    })
    
    # Create KPI Cards
    output$total_employees_card <- renderUI({
      metrics <- kpi_metrics()
      create_kpi_card("👥", metrics$total_employees, "Total Employees", "people")
    })
    
    output$attrition_rate_card <- renderUI({
      metrics <- kpi_metrics()
      create_kpi_card("📊", scales::percent(metrics$attrition_rate, accuracy = 0.1), 
                      "Attrition Rate", "trend")
    })
    
    output$avg_satisfaction_card <- renderUI({
      metrics <- kpi_metrics()
      create_kpi_card("😊", round(metrics$avg_satisfaction, 1), 
                      "Avg Satisfaction", "rating")
    })
    
    output$avg_salary_card <- renderUI({
      metrics <- kpi_metrics()
      create_kpi_card("💰", scales::dollar(metrics$avg_salary, scale = 1e-3, suffix = "K"), 
                      "Avg Salary", "currency")
    })
    
    # Employee Distribution Chart
    output$employee_distribution_chart <- renderPlotly({
      req(data())
      logger$log_info("Rendering employee distribution chart", "overview_module")
      
      tryCatch({
        employee_data <- data$employee_data
        
        # Gender distribution
        gender_dist <- employee_data %>%
          count(Gender, name = "count") %>%
          mutate(percentage = count / sum(count) * 100)
        
        p <- ggplot(gender_dist, aes(x = Gender, y = count, fill = Gender)) +
          geom_col(alpha = 0.8, width = 0.6) +
          geom_text(aes(label = paste0(count, "\n(", round(percentage, 1), "%)")), 
                   vjust = -0.5, size = 4, fontface = "bold") +
          scale_fill_manual(values = c("Female" = "#e74c3c", "Male" = "#3498db")) +
          labs(title = "Employee Distribution by Gender",
               x = "Gender", y = "Number of Employees") +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "bold", color = "#2c3e50"),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 12, face = "bold"),
            legend.position = "none",
            panel.grid.minor = element_blank()
          ) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
        
        ggplotly(p, tooltip = c("x", "y")) %>%
          config(displayModeBar = FALSE) %>%
          layout(showlegend = FALSE)
        
      }, error = function(e) {
        logger$log_error(paste("Error rendering employee distribution chart:", e$message), "overview_module")
        plotly_empty() %>% layout(title = "Error loading chart")
      })
    })
    
    # Attrition Overview Chart
    output$attrition_overview_chart <- renderPlotly({
      req(data())
      logger$log_info("Rendering attrition overview chart", "overview_module")
      
      tryCatch({
        employee_data <- data$employee_data
        
        # Attrition by department
        attrition_dept <- employee_data %>%
          group_by(Department) %>%
          summarise(
            Total = n(),
            Attrition = sum(Attrition == "Yes", na.rm = TRUE),
            Rate = Attrition / Total * 100,
            .groups = "drop"
          ) %>%
          arrange(desc(Rate))
        
        p <- ggplot(attrition_dept, aes(x = reorder(Department, Rate), y = Rate)) +
          geom_col(fill = "#e74c3c", alpha = 0.8, width = 0.6) +
          geom_text(aes(label = paste0(round(Rate, 1), "%")), 
                   hjust = -0.1, size = 4, fontface = "bold") +
          coord_flip() +
          labs(title = "Attrition Rate by Department",
               x = "Department", y = "Attrition Rate (%)") +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "bold", color = "#2c3e50"),
            axis.text = element_text(size = 11),
            axis.title = element_text(size = 12, face = "bold"),
            panel.grid.minor = element_blank()
          ) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
        
        ggplotly(p, tooltip = c("x", "y")) %>%
          config(displayModeBar = FALSE)
        
      }, error = function(e) {
        logger$log_error(paste("Error rendering attrition overview chart:", e$message), "overview_module")
        plotly_empty() %>% layout(title = "Error loading chart")
      })
    })
    
    # Department Breakdown Chart
    output$department_breakdown_chart <- renderPlotly({
      req(data())
      logger$log_info("Rendering department breakdown chart", "overview_module")
      
      tryCatch({
        employee_data <- data$employee_data
        
        # Department breakdown with multiple metrics
        dept_breakdown <- employee_data %>%
          group_by(Department) %>%
          summarise(
            Employees = n(),
            `Avg Salary` = mean(Salary, na.rm = TRUE),
            `Attrition Rate` = sum(Attrition == "Yes", na.rm = TRUE) / n() * 100,
            .groups = "drop"
          ) %>%
          pivot_longer(cols = -Department, names_to = "Metric", values_to = "Value")
        
        # Create subplots for different metrics
        p1 <- dept_breakdown %>%
          filter(Metric == "Employees") %>%
          ggplot(aes(x = reorder(Department, Value), y = Value)) +
          geom_col(fill = "#3498db", alpha = 0.8) +
          geom_text(aes(label = Value), vjust = -0.5, size = 3.5, fontface = "bold") +
          labs(title = "Employee Count", x = "", y = "Count") +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.minor = element_blank()
          )
        
        p2 <- dept_breakdown %>%
          filter(Metric == "Avg Salary") %>%
          ggplot(aes(x = reorder(Department, Value), y = Value)) +
          geom_col(fill = "#2ecc71", alpha = 0.8) +
          geom_text(aes(label = scales::dollar(Value, scale = 1e-3, suffix = "K")), 
                   vjust = -0.5, size = 3.5, fontface = "bold") +
          labs(title = "Average Salary", x = "", y = "Salary ($)") +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.minor = element_blank()
          ) +
          scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K"))
        
        p3 <- dept_breakdown %>%
          filter(Metric == "Attrition Rate") %>%
          ggplot(aes(x = reorder(Department, Value), y = Value)) +
          geom_col(fill = "#e74c3c", alpha = 0.8) +
          geom_text(aes(label = paste0(round(Value, 1), "%")), 
                   vjust = -0.5, size = 3.5, fontface = "bold") +
          labs(title = "Attrition Rate", x = "Department", y = "Rate (%)") +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.minor = element_blank()
          )
        
        # Combine plots
        subplot(
          ggplotly(p1, tooltip = c("x", "y")) %>% config(displayModeBar = FALSE),
          ggplotly(p2, tooltip = c("x", "y")) %>% config(displayModeBar = FALSE),
          ggplotly(p3, tooltip = c("x", "y")) %>% config(displayModeBar = FALSE),
          nrows = 3, shareX = TRUE, titleY = TRUE
        ) %>%
          layout(title = list(text = "Department Metrics Overview", font = list(size = 16)))
        
      }, error = function(e) {
        logger$log_error(paste("Error rendering department breakdown chart:", e$message), "overview_module")
        plotly_empty() %>% layout(title = "Error loading chart")
      })
    })
    
    # Summary Statistics Table
    output$summary_stats_table <- DT::renderDataTable({
      req(data())
      logger$log_info("Generating summary statistics table", "overview_module")
      
      tryCatch({
        summary_stats <- generate_summary_stats(data())
        
        DT::datatable(
          summary_stats,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel'),
            columnDefs = list(
              list(className = 'dt-center', targets = "_all")
            )
          ),
          extensions = 'Buttons',
          rownames = FALSE,
          class = 'cell-border stripe hover'
        ) %>%
          DT::formatRound(columns = c("Avg_Salary", "Avg_Satisfaction"), digits = 2) %>%
          DT::formatPercentage(columns = "Attrition_Rate", digits = 1)
        
      }, error = function(e) {
        logger$log_error(paste("Error generating summary statistics table:", e$message), "overview_module")
        DT::datatable(data.frame(Error = "Unable to load summary statistics"))
      })
    })
    
    # Log performance metrics
    observe({
      perf_end <- Sys.time()
      exec_time <- as.numeric(difftime(perf_end, perf_start, units = "secs"))
      
      perf_data <- list(
        execution_time = exec_time,
        memory_usage = logger$track_memory_usage()
      )
      
      logger$log_info(
        paste("Overview module rendered in", round(exec_time, 2), "seconds"), 
        "overview_module", 
        perf_data
      )
    })
  })
}

# Helper Functions
create_kpi_card <- function(icon, value, label, type = "default") {
  tagList(
    div(class = "kpi-icon", HTML(icon)),
    div(class = "kpi-value", value),
    div(class = "kpi-label", label)
  )
}

generate_summary_stats <- function(data) {
  tryCatch({
    employee_data <- data$employee_data
    performance_data <- data$performance_data
    
    # Department-level summary
    dept_summary <- employee_data %>%
      group_by(Department) %>%
      summarise(
        Employee_Count = n(),
        Attrition_Rate = sum(Attrition == "Yes", na.rm = TRUE) / n(),
        Avg_Salary = mean(Salary, na.rm = TRUE),
        Avg_Age = mean(Age, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Add satisfaction data if available
    if (!is.null(performance_data)) {
      satisfaction_summary <- performance_data %>%
        left_join(employee_data, by = "EmployeeID") %>%
        group_by(Department) %>%
        summarise(
          Avg_Satisfaction = mean(JobSatisfaction, na.rm = TRUE),
          .groups = "drop"
        )
      
      dept_summary <- dept_summary %>%
        left_join(satisfaction_summary, by = "Department")
    } else {
      dept_summary$Avg_Satisfaction <- NA
    }
    
    # Clean column names for display
    names(dept_summary) <- c("Department", "Employee Count", "Attrition Rate", 
                           "Avg Salary", "Avg Age", "Avg Satisfaction")
    
    return(dept_summary)
    
  }, error = function(e) {
    # Return empty data frame on error
    data.frame(
      Department = character(0),
      `Employee Count` = numeric(0),
      `Attrition Rate` = numeric(0),
      `Avg Salary` = numeric(0),
      `Avg Age` = numeric(0),
      `Avg Satisfaction` = numeric(0),
      check.names = FALSE
    )
  })
}