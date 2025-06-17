# ==============================================================================
# ATLAS LABS HR ANALYTICS DASHBOARD
# Satisfaction Analysis Module
# File: modules/satisfaction_module.R
# ==============================================================================

# UI Module for Satisfaction Analysis
satisfactionUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Header Section
    column(12,
      div(class = "module-header",
        h2(icon("heart"), "Employee Satisfaction Analysis",
           style = "color: #2c3e50; margin-bottom: 20px;"),
        p("Comprehensive analysis of employee satisfaction across multiple dimensions",
          class = "module-description")
      )
    ),
    
    # Control Panel
    column(12,
      wellPanel(
        fluidRow(
          column(3,
            selectInput(ns("dept_filter"), "Department:",
                       choices = NULL, multiple = TRUE,
                       selected = NULL)
          ),
          column(3,
            selectInput(ns("role_filter"), "Job Role:",
                       choices = NULL, multiple = TRUE,
                       selected = NULL)
          ),
          column(3,
            sliderInput(ns("tenure_filter"), "Years at Company:",
                       min = 0, max = 40, value = c(0, 40))
          ),
          column(3,
            checkboxGroupInput(ns("satisfaction_metrics"), "Metrics to Display:",
                              choices = list(
                                "Job Satisfaction" = "JobSatisfaction",
                                "Environment" = "EnvironmentSatisfaction", 
                                "Relationships" = "RelationshipSatisfaction",
                                "Work-Life Balance" = "WorkLifeBalance"
                              ),
                              selected = c("JobSatisfaction", "EnvironmentSatisfaction", 
                                         "RelationshipSatisfaction", "WorkLifeBalance"))
          )
        ),
        fluidRow(
          column(6,
            actionButton(ns("refresh_analysis"), "Refresh Analysis",
                        class = "btn-primary", icon = icon("refresh"))
          ),
          column(6,
            downloadButton(ns("download_satisfaction"), "Download Report",
                          class = "btn-success", icon = icon("download"))
          )
        )
      )
    ),
    
    # KPI Cards Row
    column(12,
      fluidRow(
        column(3,
          div(class = "kpi-card satisfaction-kpi",
            h4("Overall Satisfaction", class = "kpi-title"),
            h2(textOutput(ns("overall_satisfaction")), class = "kpi-value"),
            p("Average across all metrics", class = "kpi-subtitle")
          )
        ),
        column(3,
          div(class = "kpi-card job-kpi",
            h4("Job Satisfaction", class = "kpi-title"),
            h2(textOutput(ns("job_satisfaction")), class = "kpi-value"),
            p("Role & responsibility rating", class = "kpi-subtitle")
          )
        ),
        column(3,
          div(class = "kpi-card environment-kpi",
            h4("Environment Score", class = "kpi-title"),
            h2(textOutput(ns("environment_satisfaction")), class = "kpi-value"),
            p("Workplace environment rating", class = "kpi-subtitle")
          )
        ),
        column(3,
          div(class = "kpi-card balance-kpi",
            h4("Work-Life Balance", class = "kpi-title"),
            h2(textOutput(ns("worklife_balance")), class = "kpi-value"),
            p("Balance satisfaction rating", class = "kpi-subtitle")
          )
        )
      )
    ),
    
    # Main Visualizations
    column(12,
      tabsetPanel(
        id = ns("satisfaction_tabs"),
        type = "pills",
        
        # Radar Chart Tab
        tabPanel("Satisfaction Overview",
          fluidRow(
            column(8,
              div(class = "chart-container",
                h4("Satisfaction Radar Chart", class = "chart-title"),
                plotlyOutput(ns("satisfaction_radar"), height = "500px")
              )
            ),
            column(4,
              div(class = "insight-panel",
                h4("Key Insights", class = "insight-title"),
                htmlOutput(ns("radar_insights"))
              )
            )
          )
        ),
        
        # Work-Life Balance Tab
        tabPanel("Work-Life Balance",
          fluidRow(
            column(6,
              div(class = "chart-container",
                h4("Balance by Demographics", class = "chart-title"),
                plotlyOutput(ns("balance_demographics"), height = "400px")
              )
            ),
            column(6,
              div(class = "chart-container",
                h4("Balance vs Attrition", class = "chart-title"),
                plotlyOutput(ns("balance_attrition"), height = "400px")
              )
            )
          ),
          fluidRow(
            column(12,
              div(class = "chart-container",
                h4("Work-Life Balance Trends", class = "chart-title"),
                plotlyOutput(ns("balance_trends"), height = "350px")
              )
            )
          )
        ),
        
        # Environment Tab
        tabPanel("Environment Analysis",
          fluidRow(
            column(8,
              div(class = "chart-container",
                h4("Environment Satisfaction Distribution", class = "chart-title"),
                plotlyOutput(ns("environment_distribution"), height = "400px")
              )
            ),
            column(4,
              div(class = "chart-container",
                h4("Environment Factors", class = "chart-title"),
                plotlyOutput(ns("environment_factors"), height = "400px")
              )
            )
          ),
          fluidRow(
            column(12,
              div(class = "chart-container",
                h4("Environment Satisfaction Heatmap", class = "chart-title"),
                plotlyOutput(ns("environment_heatmap"), height = "350px")
              )
            )
          )
        ),
        
        # Relationships Tab
        tabPanel("Relationship Satisfaction",
          fluidRow(
            column(6,
              div(class = "chart-container",
                h4("Relationship Satisfaction by Role", class = "chart-title"),
                plotlyOutput(ns("relationship_role"), height = "400px")
              )
            ),
            column(6,
              div(class = "chart-container",
                h4("Manager Relationship Impact", class = "chart-title"),
                plotlyOutput(ns("manager_relationship"), height = "400px")
              )
            )
          ),
          fluidRow(
            column(12,
              div(class = "chart-container",
                h4("Relationship Satisfaction Correlation Matrix", class = "chart-title"),
                plotlyOutput(ns("relationship_correlation"), height = "350px")
              )
            )
          )
        ),
        
        # Detailed Analysis Tab
        tabPanel("Detailed Analysis",
          fluidRow(
            column(12,
              div(class = "chart-container",
                h4("Satisfaction Drivers Analysis", class = "chart-title"),
                plotlyOutput(ns("satisfaction_drivers"), height = "450px")
              )
            )
          ),
          fluidRow(
            column(12,
              div(class = "table-container",
                h4("Detailed Satisfaction Metrics", class = "table-title"),
                DT::dataTableOutput(ns("satisfaction_table"))
              )
            )
          )
        )
      )
    )
  )
}

# Server Module for Satisfaction Analysis
satisfactionServer <- function(id, data, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize logger for this module
    logger$log_info("Initializing Satisfaction Module", "satisfaction_module")
    
    # Reactive values for module state
    satisfaction_data <- reactiveVal()
    satisfaction_metrics <- reactiveVal()
    
    # Update filter choices when data changes
    observe({
      req(data())
      
      df <- data()$merged_data
      
      # Update department choices
      dept_choices <- sort(unique(df$Department))
      updateSelectInput(session, "dept_filter", choices = dept_choices)
      
      # Update role choices
      role_choices <- sort(unique(df$JobRole))
      updateSelectInput(session, "role_filter", choices = role_choices)
      
      # Update tenure slider
      tenure_range <- range(df$YearsAtCompany, na.rm = TRUE)
      updateSliderInput(session, "tenure_filter", 
                       min = tenure_range[1], max = tenure_range[2],
                       value = tenure_range)
    })
    
    # Filtered data based on user inputs
    filtered_data <- reactive({
      req(data())
      
      start_time <- Sys.time()
      
      df <- data()$merged_data
      
      # Apply filters
      if (!is.null(input$dept_filter) && length(input$dept_filter) > 0) {
        df <- df %>% filter(Department %in% input$dept_filter)
      }
      
      if (!is.null(input$role_filter) && length(input$role_filter) > 0) {
        df <- df %>% filter(JobRole %in% input$role_filter)
      }
      
      df <- df %>% 
        filter(YearsAtCompany >= input$tenure_filter[1] & 
               YearsAtCompany <= input$tenure_filter[2])
      
      # Log performance
      end_time <- Sys.time()
      logger$log_info(paste("Data filtering completed in", 
                           round(as.numeric(end_time - start_time), 3), "seconds"),
                     "satisfaction_module")
      
      df
    })
    
    # Calculate satisfaction scores
    satisfaction_scores <- reactive({
      req(filtered_data())
      calculate_satisfaction_scores(filtered_data(), logger)
    })
    
    # Analyze satisfaction drivers
    satisfaction_analysis <- reactive({
      req(filtered_data())
      analyze_satisfaction_drivers(filtered_data(), logger)
    })
    
    # KPI Outputs
    output$overall_satisfaction <- renderText({
      scores <- satisfaction_scores()
      paste0(round(scores$overall_avg, 1), "/5.0")
    })
    
    output$job_satisfaction <- renderText({
      scores <- satisfaction_scores()
      paste0(round(scores$job_avg, 1), "/5.0")
    })
    
    output$environment_satisfaction <- renderText({
      scores <- satisfaction_scores()
      paste0(round(scores$environment_avg, 1), "/5.0")
    })
    
    output$worklife_balance <- renderText({
      scores <- satisfaction_scores()
      paste0(round(scores$worklife_avg, 1), "/5.0")
    })
    
    # Satisfaction Radar Chart
    output$satisfaction_radar <- renderPlotly({
      req(satisfaction_scores())
      
      scores <- satisfaction_scores()$by_department
      
      # Create radar chart data
      radar_data <- scores %>%
        select(Department, JobSatisfaction, EnvironmentSatisfaction, 
               RelationshipSatisfaction, WorkLifeBalance) %>%
        gather(key = "Metric", value = "Score", -Department)
      
      p <- ggplot(radar_data, aes(x = Metric, y = Score, group = Department, color = Department)) +
        geom_line(size = 1.2, alpha = 0.8) +
        geom_point(size = 3, alpha = 0.8) +
        coord_polar() +
        scale_y_continuous(limits = c(0, 5), breaks = 1:5) +
        labs(title = "Satisfaction Radar Chart by Department",
             subtitle = "Comparison across satisfaction dimensions") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom",
          axis.text.x = element_text(size = 10),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_line(color = "grey95")
        ) +
        scale_color_manual(values = ATLAS_COLORS)
      
      ggplotly(p, tooltip = c("Department", "Metric", "Score"))
    })
    
    # Work-Life Balance Demographics
    output$balance_demographics <- renderPlotly({
      req(filtered_data())
      
      df <- filtered_data()
      
      balance_demo <- df %>%
        group_by(Gender, Age_Group = cut(Age, breaks = c(0, 30, 40, 50, 60, 100),
                                        labels = c("Under 30", "30-39", "40-49", "50-59", "60+"))) %>%
        summarise(Avg_Balance = mean(WorkLifeBalance, na.rm = TRUE),
                 Count = n(), .groups = "drop")
      
      p <- ggplot(balance_demo, aes(x = Age_Group, y = Avg_Balance, fill = Gender)) +
        geom_col(position = "dodge", alpha = 0.8) +
        geom_text(aes(label = round(Avg_Balance, 1)), 
                 position = position_dodge(width = 0.9), vjust = -0.5) +
        labs(title = "Work-Life Balance by Demographics",
             x = "Age Group", y = "Average Balance Score") +
        theme_minimal() +
        scale_fill_manual(values = ATLAS_COLORS[1:2])
      
      ggplotly(p, tooltip = c("Age_Group", "Gender", "Avg_Balance", "Count"))
    })
    
    # Balance vs Attrition
    output$balance_attrition <- renderPlotly({
      req(filtered_data())
      
      df <- filtered_data()
      
      balance_attrition <- df %>%
        group_by(WorkLifeBalance, Attrition) %>%
        summarise(Count = n(), .groups = "drop") %>%
        group_by(WorkLifeBalance) %>%
        mutate(Percentage = Count / sum(Count) * 100)
      
      p <- ggplot(balance_attrition, aes(x = as.factor(WorkLifeBalance), y = Percentage, 
                                        fill = Attrition)) +
        geom_col(position = "stack", alpha = 0.8) +
        labs(title = "Work-Life Balance vs Attrition",
             x = "Work-Life Balance Score", y = "Percentage") +
        theme_minimal() +
        scale_fill_manual(values = c("No" = ATLAS_COLORS[3], "Yes" = ATLAS_COLORS[4]))
      
      ggplotly(p, tooltip = c("WorkLifeBalance", "Attrition", "Percentage"))
    })
    
    # Environment Distribution
    output$environment_distribution <- renderPlotly({
      req(filtered_data())
      
      df <- filtered_data()
      
      env_dist <- df %>%
        count(EnvironmentSatisfaction, Department) %>%
        group_by(Department) %>%
        mutate(Percentage = n / sum(n) * 100)
      
      p <- ggplot(env_dist, aes(x = as.factor(EnvironmentSatisfaction), y = Percentage, 
                               fill = Department)) +
        geom_col(position = "dodge", alpha = 0.8) +
        labs(title = "Environment Satisfaction Distribution",
             x = "Environment Satisfaction Score", y = "Percentage") +
        theme_minimal() +
        scale_fill_manual(values = ATLAS_COLORS)
      
      ggplotly(p, tooltip = c("EnvironmentSatisfaction", "Department", "Percentage"))
    })
    
    # Satisfaction Drivers
    output$satisfaction_drivers <- renderPlotly({
      req(satisfaction_analysis())
      
      drivers <- satisfaction_analysis()$drivers
      
      p <- ggplot(drivers, aes(x = reorder(Factor, Importance), y = Importance)) +
        geom_col(fill = ATLAS_COLORS[1], alpha = 0.8) +
        geom_text(aes(label = round(Importance, 2)), hjust = -0.1) +
        coord_flip() +
        labs(title = "Key Satisfaction Drivers",
             x = "Factors", y = "Importance Score") +
        theme_minimal()
      
      ggplotly(p, tooltip = c("Factor", "Importance"))
    })
    
    # Satisfaction Details Table
    output$satisfaction_table <- DT::renderDataTable({
      req(satisfaction_scores())
      
      table_data <- satisfaction_scores()$detailed_metrics
      
      DT::datatable(
        table_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      ) %>%
        DT::formatRound(columns = c("JobSatisfaction", "EnvironmentSatisfaction", 
                                   "RelationshipSatisfaction", "WorkLifeBalance"), 
                       digits = 1)
    })
    
    # Radar Insights
    output$radar_insights <- renderUI({
      req(satisfaction_scores())
      
      scores <- satisfaction_scores()
      insights <- satisfaction_analysis()$insights
      
      HTML(paste0(
        "<div class='insight-item'>",
        "<h5>Top Performing Area:</h5>",
        "<p>", insights$top_area, "</p>",
        "</div>",
        "<div class='insight-item'>",
        "<h5>Improvement Opportunity:</h5>",
        "<p>", insights$improvement_area, "</p>",
        "</div>",
        "<div class='insight-item'>",
        "<h5>Key Correlation:</h5>",
        "<p>", insights$correlation_insight, "</p>",
        "</div>"
      ))
    })
    
    # Download handler
    output$download_satisfaction <- downloadHandler(
      filename = function() {
        paste("satisfaction_analysis_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(satisfaction_scores()$detailed_metrics, file, row.names = FALSE)
      }
    )
    
    # Return reactive values for other modules
    return(
      list(
        satisfaction_scores = satisfaction_scores,
        satisfaction_analysis = satisfaction_analysis,
        filtered_data = filtered_data
      )
    )
  })
}

# Helper function to calculate satisfaction scores
calculate_satisfaction_scores <- function(data, logger) {
  start_time <- Sys.time()
  
  # Overall averages
  overall_avg <- mean(c(
    mean(data$JobSatisfaction, na.rm = TRUE),
    mean(data$EnvironmentSatisfaction, na.rm = TRUE),
    mean(data$RelationshipSatisfaction, na.rm = TRUE),
    mean(data$WorkLifeBalance, na.rm = TRUE)
  ), na.rm = TRUE)
  
  job_avg <- mean(data$JobSatisfaction, na.rm = TRUE)
  environment_avg <- mean(data$EnvironmentSatisfaction, na.rm = TRUE)
  relationship_avg <- mean(data$RelationshipSatisfaction, na.rm = TRUE)
  worklife_avg <- mean(data$WorkLifeBalance, na.rm = TRUE)
  
  # By department
  by_department <- data %>%
    group_by(Department) %>%
    summarise(
      JobSatisfaction = mean(JobSatisfaction, na.rm = TRUE),
      EnvironmentSatisfaction = mean(EnvironmentSatisfaction, na.rm = TRUE),
      RelationshipSatisfaction = mean(RelationshipSatisfaction, na.rm = TRUE),
      WorkLifeBalance = mean(WorkLifeBalance, na.rm = TRUE),
      Employee_Count = n(),
      .groups = "drop"
    )
  
  # Detailed metrics
  detailed_metrics <- data %>%
    select(EmployeeID, Department, JobRole, JobSatisfaction, 
           EnvironmentSatisfaction, RelationshipSatisfaction, 
           WorkLifeBalance, Attrition) %>%
    mutate(
      Overall_Satisfaction = (JobSatisfaction + EnvironmentSatisfaction + 
                             RelationshipSatisfaction + WorkLifeBalance) / 4
    )
  
  end_time <- Sys.time()
  logger$log_info(paste("Satisfaction scores calculated in", 
                       round(as.numeric(end_time - start_time), 3), "seconds"),
                 "satisfaction_module")
  
  list(
    overall_avg = overall_avg,
    job_avg = job_avg,
    environment_avg = environment_avg,
    relationship_avg = relationship_avg,
    worklife_avg = worklife_avg,
    by_department = by_department,
    detailed_metrics = detailed_metrics
  )
}

# Helper function to analyze satisfaction drivers
analyze_satisfaction_drivers <- function(data, logger) {
  start_time <- Sys.time()
  
  # Calculate correlations with attrition
  correlations <- data %>%
    select(JobSatisfaction, EnvironmentSatisfaction, RelationshipSatisfaction, 
           WorkLifeBalance, Attrition) %>%
    mutate(Attrition_Numeric = ifelse(Attrition == "Yes", 1, 0)) %>%
    select(-Attrition) %>%
    cor(use = "complete.obs")
  
  # Extract attrition correlations
  attrition_cors <- correlations[, "Attrition_Numeric"]
  attrition_cors <- attrition_cors[names(attrition_cors) != "Attrition_Numeric"]
  
  # Create drivers data frame
  drivers <- data.frame(
    Factor = names(attrition_cors),
    Importance = abs(attrition_cors)
  ) %>%
    arrange(desc(Importance))
  
  # Generate insights
  top_satisfaction <- data %>%
    summarise(
      JobSat = mean(JobSatisfaction, na.rm = TRUE),
      EnvSat = mean(EnvironmentSatisfaction, na.rm = TRUE),
      RelSat = mean(RelationshipSatisfaction, na.rm = TRUE),
      WLBSat = mean(WorkLifeBalance, na.rm = TRUE)
    ) %>%
    gather(key = "Metric", value = "Score") %>%
    arrange(desc(Score))
  
  insights <- list(
    top_area = paste("Highest satisfaction:", gsub("Sat", " Satisfaction", top_satisfaction$Metric[1])),
    improvement_area = paste("Focus area:", gsub("Sat", " Satisfaction", top_satisfaction$Metric[4])),
    correlation_insight = paste("Strongest attrition predictor:", 
                               gsub("Satisfaction", " Satisfaction", drivers$Factor[1]))
  )
  
  end_time <- Sys.time()
  logger$log_info(paste("Satisfaction drivers analyzed in", 
                       round(as.numeric(end_time - start_time), 3), "seconds"),
                 "satisfaction_module")
  
  list(
    drivers = drivers,
    correlations = correlations,
    insights = insights
  )
}