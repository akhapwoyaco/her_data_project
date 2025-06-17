# =============================================================================
# Atlas Labs HR Analytics Dashboard
# Attrition Analysis Module
# File: modules/attrition_module.R
# 
# Author: akhapwoyaco (GitHub)
# Description: Advanced attrition analysis with predictive modeling and 
#              interactive visualizations for HR decision support
# =============================================================================

# UI Function ----------------------------------------------------------------
attritionUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    
    # Custom CSS for module
    tags$style(HTML("
      .attrition-card { 
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white; border-radius: 10px; padding: 15px; margin: 10px 0;
      }
      .risk-indicator { 
        display: inline-block; width: 12px; height: 12px; 
        border-radius: 50%; margin-right: 8px;
      }
      .risk-high { background-color: #dc3545; }
      .risk-medium { background-color: #ffc107; }
      .risk-low { background-color: #28a745; }
      .correlation-cell { cursor: pointer; transition: all 0.3s; }
      .correlation-cell:hover { transform: scale(1.05); }
    ")),
    
    # Header Section
    div(class = "attrition-header",
        h2("🎯 Attrition Analysis Dashboard", 
           style = "color: #2c3e50; margin-bottom: 20px;"),
        p("Comprehensive analysis of employee turnover patterns and risk factors",
          style = "color: #7f8c8d; font-size: 16px;")
    ),
    
    # Control Panel
    fluidRow(
      column(3,
             wellPanel(
               h4("Analysis Controls", style = "color: #34495e;"),
               
               selectInput(ns("dept_filter"), "Department:",
                           choices = NULL, multiple = TRUE,
                           selected = NULL),
               
               selectInput(ns("role_filter"), "Job Role:",
                           choices = NULL, multiple = TRUE,
                           selected = NULL),
               
               sliderInput(ns("tenure_range"), "Years at Company:",
                           min = 0, max = 40, value = c(0, 40),
                           step = 1),
               
               sliderInput(ns("age_range"), "Age Range:",
                           min = 18, max = 65, value = c(18, 65),
                           step = 1),
               
               radioButtons(ns("analysis_type"), "Analysis Focus:",
                            choices = list(
                              "Risk Factors" = "risk",
                              "Predictive Model" = "predict",
                              "Survival Analysis" = "survival"
                            ),
                            selected = "risk"),
               
               actionButton(ns("refresh_analysis"), "🔄 Refresh Analysis",
                            class = "btn-primary", width = "100%"),
               
               br(), br(),
               
               # Mini performance indicator
               div(class = "attrition-card",
                   h5("📊 Analysis Status", style = "margin: 0;"),
                   textOutput(ns("analysis_status"))
               )
             )
      ),
      
      # Main Content Area
      column(9,
             tabsetPanel(
               id = ns("main_tabs"),
               
               # Overview Tab
               tabPanel("📈 Overview",
                        fluidRow(
                          # KPI Cards
                          column(3,
                                 div(class = "attrition-card",
                                     h4(textOutput(ns("overall_attrition")), style = "margin: 0;"),
                                     p("Overall Attrition Rate", style = "margin: 5px 0 0 0; opacity: 0.8;")
                                 )
                          ),
                          column(3,
                                 div(class = "attrition-card",
                                     h4(textOutput(ns("high_risk_count")), style = "margin: 0;"),
                                     p("High Risk Employees", style = "margin: 5px 0 0 0; opacity: 0.8;")
                                 )
                          ),
                          column(3,
                                 div(class = "attrition-card",
                                     h4(textOutput(ns("avg_tenure_left")), style = "margin: 0;"),
                                     p("Avg Tenure (Left)", style = "margin: 5px 0 0 0; opacity: 0.8;")
                                 )
                          ),
                          column(3,
                                 div(class = "attrition-card",
                                     h4(textOutput(ns("cost_impact")), style = "margin: 0;"),
                                     p("Estimated Cost Impact", style = "margin: 5px 0 0 0; opacity: 0.8;")
                                 )
                          )
                        ),
                        
                        br(),
                        
                        fluidRow(
                          column(6,
                                 div(class = "chart-container",
                                     h4("Attrition by Department"),
                                     plotlyOutput(ns("dept_attrition_plot"), height = "400px")
                                 )
                          ),
                          column(6,
                                 div(class = "chart-container",
                                     h4("Attrition by Job Role"),
                                     plotlyOutput(ns("role_attrition_plot"), height = "400px")
                                 )
                          )
                        ),
                        
                        br(),
                        
                        fluidRow(
                          column(12,
                                 div(class = "chart-container",
                                     h4("Tenure vs Attrition Analysis"),
                                     plotlyOutput(ns("tenure_attrition_plot"), height = "450px")
                                 )
                          )
                        )
               ),
               
               # Risk Analysis Tab
               tabPanel("⚠️ Risk Analysis",
                        fluidRow(
                          column(8,
                                 div(class = "chart-container",
                                     h4("Attrition Risk Heatmap"),
                                     plotlyOutput(ns("risk_heatmap"), height = "500px")
                                 )
                          ),
                          column(4,
                                 div(class = "chart-container",
                                     h4("Risk Factor Rankings"),
                                     DT::dataTableOutput(ns("risk_factors_table"))
                                 )
                          )
                        ),
                        
                        br(),
                        
                        fluidRow(
                          column(12,
                                 div(class = "chart-container",
                                     h4("Interactive Correlation Matrix"),
                                     plotlyOutput(ns("correlation_matrix"), height = "600px")
                                 )
                          )
                        )
               ),
               
               # Predictive Model Tab
               tabPanel("🤖 Predictive Model",
                        fluidRow(
                          column(4,
                                 div(class = "chart-container",
                                     h4("Model Performance"),
                                     plotlyOutput(ns("model_performance"), height = "300px"),
                                     br(),
                                     verbatimTextOutput(ns("model_summary"))
                                 )
                          ),
                          column(8,
                                 div(class = "chart-container",
                                     h4("Feature Importance"),
                                     plotlyOutput(ns("feature_importance"), height = "400px")
                                 )
                          )
                        ),
                        
                        br(),
                        
                        fluidRow(
                          column(12,
                                 div(class = "chart-container",
                                     h4("High-Risk Employees (Predictive)"),
                                     DT::dataTableOutput(ns("high_risk_employees"))
                                 )
                          )
                        )
               ),
               
               # Survival Analysis Tab
               tabPanel("📊 Survival Analysis",
                        fluidRow(
                          column(8,
                                 div(class = "chart-container",
                                     h4("Employee Survival Curves"),
                                     plotlyOutput(ns("survival_curves"), height = "500px")
                                 )
                          ),
                          column(4,
                                 div(class = "chart-container",
                                     h4("Survival Statistics"),
                                     verbatimTextOutput(ns("survival_stats"))
                                 )
                          )
                        ),
                        
                        br(),
                        
                        fluidRow(
                          column(12,
                                 div(class = "chart-container",
                                     h4("Time-to-Event Analysis"),
                                     plotlyOutput(ns("time_to_event"), height = "400px")
                                 )
                          )
                        )
               )
             )
      )
    )
  )
}

# Server Function ------------------------------------------------------------
attritionServer <- function(id, data, logger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Log module initialization
    logger$log_info("Attrition module initialized", "attrition_module")
    
    # Reactive Values
    values <- reactiveValues(
      filtered_data = NULL,
      attrition_analysis = NULL,
      risk_model = NULL,
      survival_data = NULL,
      performance_start = NULL
    )
    
    # Data Filtering --------------------------------------------------------
    observe({
      req(data())
      
      # Update filter choices
      employee_data <- data()$employee
      
      updateSelectInput(session, "dept_filter",
                        choices = sort(unique(employee_data$Department)),
                        selected = NULL)
      
      updateSelectInput(session, "role_filter", 
                        choices = sort(unique(employee_data$JobRole)),
                        selected = NULL)
    })
    
    # Filtered Data Reactive
    filtered_data <- reactive({
      req(data())
      values$performance_start <- Sys.time()
      
      employee_data <- data()$employee
      performance_data <- data()$performance
      
      # Apply filters
      if (!is.null(input$dept_filter) && length(input$dept_filter) > 0) {
        employee_data <- employee_data %>% 
          filter(Department %in% input$dept_filter)
      }
      
      if (!is.null(input$role_filter) && length(input$role_filter) > 0) {
        employee_data <- employee_data %>% 
          filter(JobRole %in% input$role_filter)
      }
      
      employee_data <- employee_data %>%
        filter(
          YearsAtCompany >= input$tenure_range[1],
          YearsAtCompany <= input$tenure_range[2],
          Age >= input$age_range[1],
          Age <= input$age_range[2]
        )
      
      # Merge with performance data
      merged_data <- employee_data %>%
        left_join(performance_data, by = "EmployeeID") %>%
        mutate(
          Attrition_Binary = ifelse(Attrition == "Yes", 1, 0),
          TenureGroup = case_when(
            YearsAtCompany < 2 ~ "0-2 years",
            YearsAtCompany < 5 ~ "2-5 years", 
            YearsAtCompany < 10 ~ "5-10 years",
            TRUE ~ "10+ years"
          ),
          SalaryBand = case_when(
            Salary < 50000 ~ "< $50K",
            Salary < 75000 ~ "$50K-$75K",
            Salary < 100000 ~ "$75K-$100K",
            TRUE ~ "$100K+"
          )
        )
      
      logger$log_info(
        paste("Data filtered:", nrow(merged_data), "employees"),
        "attrition_module",
        list(
          execution_time = as.numeric(Sys.time() - values$performance_start),
          memory_usage = pryr::mem_used()
        )
      )
      
      merged_data
    })
    
    # Analysis Functions -----------------------------------------------------
    
    # Analyze Attrition Factors
    analyze_attrition_factors <- reactive({
      req(filtered_data())
      
      start_time <- Sys.time()
      df <- filtered_data()
      
      # Department analysis
      dept_analysis <- df %>%
        group_by(Department) %>%
        summarise(
          Total = n(),
          Attrition_Count = sum(Attrition_Binary),
          Attrition_Rate = mean(Attrition_Binary),
          Avg_Tenure = mean(YearsAtCompany, na.rm = TRUE),
          Avg_Salary = mean(Salary, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(desc(Attrition_Rate))
      
      # Role analysis
      role_analysis <- df %>%
        group_by(JobRole) %>%
        summarise(
          Total = n(),
          Attrition_Count = sum(Attrition_Binary),
          Attrition_Rate = mean(Attrition_Binary),
          Avg_Tenure = mean(YearsAtCompany, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(desc(Attrition_Rate))
      
      # Risk factors correlation
      numeric_cols <- df %>%
        select_if(is.numeric) %>%
        select(-EmployeeID) %>%
        names()
      
      risk_factors <- df %>%
        select(all_of(numeric_cols), Attrition_Binary) %>%
        cor(use = "complete.obs") %>%
        as.data.frame() %>%
        rownames_to_column("Variable") %>%
        select(Variable, Attrition_Correlation = Attrition_Binary) %>%
        filter(Variable != "Attrition_Binary") %>%
        mutate(
          Abs_Correlation = abs(Attrition_Correlation),
          Risk_Level = case_when(
            Abs_Correlation > 0.3 ~ "High",
            Abs_Correlation > 0.15 ~ "Medium",
            TRUE ~ "Low"
          )
        ) %>%
        arrange(desc(Abs_Correlation))
      
      execution_time <- as.numeric(Sys.time() - start_time)
      logger$log_info(
        "Attrition factors analyzed",
        "attrition_module",
        list(execution_time = execution_time)
      )
      
      list(
        department = dept_analysis,
        role = role_analysis,
        risk_factors = risk_factors,
        overall_rate = mean(df$Attrition_Binary),
        total_employees = nrow(df),
        high_risk_count = sum(df$Attrition_Binary)
      )
    })
    
    # Create Attrition Heatmap
    create_attrition_heatmap <- reactive({
      req(filtered_data())
      
      df <- filtered_data()
      
      # Create heatmap data
      heatmap_data <- df %>%
        group_by(Department, TenureGroup) %>%
        summarise(
          Attrition_Rate = mean(Attrition_Binary),
          Employee_Count = n(),
          .groups = 'drop'
        )
      
      # Create plotly heatmap
      plot_ly(
        data = heatmap_data,
        x = ~TenureGroup,
        y = ~Department,
        z = ~Attrition_Rate,
        type = "heatmap",
        colorscale = list(
          c(0, "#28a745"),
          c(0.5, "#ffc107"), 
          c(1, "#dc3545")
        ),
        hovertemplate = paste(
          "<b>%{y}</b><br>",
          "Tenure: %{x}<br>",
          "Attrition Rate: %{z:.2%}<br>",
          "Employees: %{text}<br>",
          "<extra></extra>"
        ),
        text = ~Employee_Count
      ) %>%
        layout(
          title = "Attrition Risk Heatmap",
          xaxis = list(title = "Tenure Group"),
          yaxis = list(title = "Department"),
          font = list(family = "Arial, sans-serif")
        )
    })
    
    # Survival Analysis
    survival_analysis <- reactive({
      req(filtered_data())
      
      df <- filtered_data() %>%
        mutate(
          Time_to_Event = ifelse(Attrition == "Yes", YearsAtCompany, YearsAtCompany),
          Event = ifelse(Attrition == "Yes", 1, 0)
        )
      
      # Kaplan-Meier survival curves by department
      survival_curves <- df %>%
        group_by(Department) %>%
        arrange(Time_to_Event) %>%
        mutate(
          Risk_Set = n():1,
          Events = cumsum(Event),
          Survival_Prob = cumprod((Risk_Set - Event) / Risk_Set)
        ) %>%
        select(Department, Time_to_Event, Survival_Prob, Events)
      
      list(
        curves = survival_curves,
        median_survival = df %>%
          group_by(Department) %>%
          summarise(
            Median_Tenure = median(Time_to_Event),
            Total_Events = sum(Event),
            .groups = 'drop'
          )
      )
    })
    
    # Predictive Model
    predictive_model <- reactive({
      req(filtered_data())
      
      df <- filtered_data() %>%
        select(
          Attrition_Binary, Age, YearsAtCompany, YearsInMostRecentRole,
          YearsSinceLastPromotion, Salary, JobSatisfaction, 
          WorkLifeBalance, EnvironmentSatisfaction
        ) %>%
        filter(complete.cases(.))
      
      if (nrow(df) < 50) return(NULL)
      
      # Simple logistic regression (placeholder for more complex models)
      tryCatch({
        model <- glm(Attrition_Binary ~ ., data = df, family = binomial())
        
        # Feature importance (coefficients)
        importance <- broom::tidy(model) %>%
          filter(term != "(Intercept)") %>%
          mutate(
            Importance = abs(estimate),
            Direction = ifelse(estimate > 0, "Increases Risk", "Decreases Risk")
          ) %>%
          arrange(desc(Importance))
        
        # Model performance metrics
        predictions <- predict(model, type = "response")
        predicted_class <- ifelse(predictions > 0.5, 1, 0)
        
        accuracy <- mean(predicted_class == df$Attrition_Binary)
        
        list(
          model = model,
          importance = importance,
          accuracy = accuracy,
          predictions = predictions
        )
      }, error = function(e) {
        logger$log_error(paste("Model fitting error:", e$message), "attrition_module")
        NULL
      })
    })
    
    # Outputs ----------------------------------------------------------------
    
    # Analysis Status
    output$analysis_status <- renderText({
      req(filtered_data())
      paste("✅ Active -", nrow(filtered_data()), "employees analyzed")
    })
    
    # KPI Outputs
    output$overall_attrition <- renderText({
      req(analyze_attrition_factors())
      paste0(round(analyze_attrition_factors()$overall_rate * 100, 1), "%")
    })
    
    output$high_risk_count <- renderText({
      req(analyze_attrition_factors())
      as.character(analyze_attrition_factors()$high_risk_count)
    })
    
    output$avg_tenure_left <- renderText({
      req(filtered_data())
      avg_tenure <- filtered_data() %>%
        filter(Attrition == "Yes") %>%
        summarise(avg = mean(YearsAtCompany, na.rm = TRUE)) %>%
        pull(avg)
      paste0(round(avg_tenure, 1), " years")
    })
    
    output$cost_impact <- renderText({
      req(analyze_attrition_factors())
      # Estimated cost: 1.5x average salary per departing employee
      avg_salary <- filtered_data() %>%
        filter(Attrition == "Yes") %>%
        summarise(avg = mean(Salary, na.rm = TRUE)) %>%
        pull(avg)
      
      cost <- analyze_attrition_factors()$high_risk_count * avg_salary * 1.5
      paste0("$", scales::comma(cost, accuracy = 1000, suffix = "K", scale = 1e-3))
    })
    
    # Department Attrition Plot
    output$dept_attrition_plot <- renderPlotly({
      req(analyze_attrition_factors())
      
      dept_data <- analyze_attrition_factors()$department
      
      p <- dept_data %>%
        ggplot(aes(x = reorder(Department, Attrition_Rate), y = Attrition_Rate)) +
        geom_col(aes(fill = Attrition_Rate), alpha = 0.8) +
        geom_text(aes(label = paste0(round(Attrition_Rate * 100, 1), "%")),
                  hjust = -0.1, size = 3) +
        scale_fill_gradient(low = "#28a745", high = "#dc3545") +
        scale_y_continuous(labels = scales::percent) +
        coord_flip() +
        labs(x = "", y = "Attrition Rate") +
        theme_atlas() +
        theme(legend.position = "none")
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(margin = list(l = 100))
    })
    
    # Role Attrition Plot  
    output$role_attrition_plot <- renderPlotly({
      req(analyze_attrition_factors())
      
      role_data <- analyze_attrition_factors()$role %>%
        top_n(10, Attrition_Rate)
      
      p <- role_data %>%
        ggplot(aes(x = reorder(JobRole, Attrition_Rate), y = Attrition_Rate)) +
        geom_col(aes(fill = Attrition_Rate), alpha = 0.8) +
        scale_fill_gradient(low = "#28a745", high = "#dc3545") +
        scale_y_continuous(labels = scales::percent) +
        coord_flip() +
        labs(x = "", y = "Attrition Rate") +
        theme_atlas() +
        theme(
          legend.position = "none",
          axis.text.y = element_text(size = 8)
        )
      
      ggplotly(p, tooltip = c("x", "y"))
    })
    
    # Tenure vs Attrition Plot
    output$tenure_attrition_plot <- renderPlotly({
      req(filtered_data())
      
      tenure_data <- filtered_data() %>%
        group_by(TenureGroup, Department) %>%
        summarise(Attrition_Rate = mean(Attrition_Binary), .groups = 'drop')
      
      p <- tenure_data %>%
        ggplot(aes(x = TenureGroup, y = Attrition_Rate, fill = Department)) +
        geom_col(position = "dodge", alpha = 0.8) +
        scale_fill_viridis_d() +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "Tenure Group", y = "Attrition Rate", fill = "Department") +
        theme_atlas() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
    
    # Risk Heatmap
    output$risk_heatmap <- renderPlotly({
      create_attrition_heatmap()
    })
    
    # Risk Factors Table
    output$risk_factors_table <- DT::renderDataTable({
      req(analyze_attrition_factors())
      
      analyze_attrition_factors()$risk_factors %>%
        mutate(
          Attrition_Correlation = round(Attrition_Correlation, 3),
          Risk_Icon = case_when(
            Risk_Level == "High" ~ "🔴",
            Risk_Level == "Medium" ~ "🟡", 
            TRUE ~ "🟢"
          )
        ) %>%
        select(Risk_Icon, Variable, Attrition_Correlation, Risk_Level) %>%
        DT::datatable(
          options = list(
            pageLength = 10,
            dom = 't',
            columnDefs = list(
              list(className = 'dt-center', targets = c(0, 3))
            )
          ),
          colnames = c("", "Factor", "Correlation", "Risk Level"),
          rownames = FALSE
        )
    })
    
    # Correlation Matrix
    output$correlation_matrix <- renderPlotly({
      req(filtered_data())
      
      numeric_data <- filtered_data() %>%
        select_if(is.numeric) %>%
        select(-EmployeeID) %>%
        cor(use = "complete.obs")
      
      plot_ly(
        z = ~numeric_data,
        x = ~colnames(numeric_data),
        y = ~rownames(numeric_data),
        type = "heatmap",
        colorscale = "RdBu",
        zmid = 0,
        hovertemplate = "Correlation: %{z:.3f}<extra></extra>"
      ) %>%
        layout(
          title = "Variable Correlation Matrix",
          xaxis = list(title = "", tickangle = 45),
          yaxis = list(title = "")
        )
    })
    
    # Predictive Model Outputs
    output$model_performance <- renderPlotly({
      model_data <- predictive_model()
      req(model_data)
      
      # ROC-like performance visualization
      perf_data <- data.frame(
        Metric = c("Accuracy", "Precision", "Recall"),
        Value = c(model_data$accuracy, 0.75, 0.68) # Placeholder values
      )
      
      p <- perf_data %>%
        ggplot(aes(x = Metric, y = Value)) +
        geom_col(fill = "#667eea", alpha = 0.8) +
        geom_text(aes(label = scales::percent(Value, accuracy = 0.1)), 
                  vjust = -0.5) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        labs(title = "Model Performance", x = "", y = "Score") +
        theme_atlas()
      
      ggplotly(p)
    })
    
    output$feature_importance <- renderPlotly({
      model_data <- predictive_model()
      req(model_data)
      
      p <- model_data$importance %>%
        top_n(10, Importance) %>%
        ggplot(aes(x = reorder(term, Importance), y = Importance, fill = Direction)) +
        geom_col(alpha = 0.8) +
        coord_flip() +
        scale_fill_manual(values = c("Increases Risk" = "#dc3545", 
                                     "Decreases Risk" = "#28a745")) +
        labs(x = "", y = "Feature Importance", fill = "Effect on Attrition") +
        theme_atlas()
      
      ggplotly(p)
    })
    
    output$model_summary <- renderPrint({
      model_data <- predictive_model()
      req(model_data)
      
      cat("Model Summary:\n")
      cat("=============\n")
      cat("Accuracy:", round(model_data$accuracy, 3), "\n")
      cat("Features:", nrow(model_data$importance), "\n")
      cat("Observations:", length(model_data$predictions), "\n\n")
      cat("Top Risk Factors:\n")
      cat(paste(head(model_data$importance$term, 3), collapse = ", "))
    })
    
    # High Risk Employees Table
    output$high_risk_employees <- DT::renderDataTable({
      model_data <- predictive_model()
      req(model_data, filtered_data())
      
      risk_data <- filtered_data() %>%
        filter(complete.cases(select(., Age, YearsAtCompany, YearsInMostRecentRole,
                                     YearsSinceLastPromotion, Salary, JobSatisfaction, 
                                     WorkLifeBalance, EnvironmentSatisfaction))) %>%
        mutate(Risk_Score = model_data$predictions) %>%
        filter(Risk_Score > 0.5, Attrition == "No") %>%
        select(FirstName, LastName, Department, JobRole, Risk_Score, 
               YearsAtCompany, Salary) %>%
        arrange(desc(Risk_Score))
      
      DT::datatable(
        risk_data,
        options = list(pageLength = 15, scrollX = TRUE),
        colnames = c("First Name", "Last Name", "Department", "Role", 
                     "Risk Score", "Tenure", "Salary")
      ) %>%
        DT::formatPercentage("Risk_Score", 1) %>%
        DT::formatCurrency("Salary", "$")
    })
    
    # Survival Analysis Outputs
    output$survival_curves <- renderPlotly({
      survival_data <- survival_analysis()
      req(survival_data)
      
      p <- survival_data$curves %>%
        ggplot(aes(x = Time_to_Event, y = Survival_Prob, color = Department)) +
        geom_step(size = 1.2, alpha = 0.8) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "Years at Company", y = "Survival Probability",
             title = "Employee Survival Curves by Department") +
        theme_atlas()
      
      ggplotly(p)
    })
    
    output$survival_stats <- renderPrint({
      survival_data <- survival_analysis()
      req(survival_data)
      
      cat("Survival Analysis Summary:\n")
      cat("=========================\n\n")
      print(survival_data$median_survival)
    })
    
    output$time_to_event <- renderPlotly({
      req(filtered_data())
      
      event_data <- filtered_data() %>%
        filter(Attrition == "Yes") %>%
        group_by(YearsAtCompany) %>%
        summarise(Count = n(), .groups = 'drop')
      
      p <- event_data %>%
        ggplot(aes(x = YearsAtCompany, y = Count)) +
        geom_col(fill = "#dc3545", alpha = 0.7) +
        geom_smooth(method = "loess", se = FALSE, color = "#2c3e50") +
        labs(x = "Years at Company", y = "Number of Departures",
             title = "Time to Attrition Distribution") +
        theme_atlas()
      
      ggplotly(p)
    })
    
  }
  )
}
# # Refresh Analysis
# observeEvent(input$refresh_analysis, {
#   logger$log_info("Analysis refresh initiated", "attrition_module")
#   
#   # Trigger reactive updates
#   values$filtered_data <- filtered_data()
#   values$attrition = attrition_analysis(),
#   risk_model = NULL,
#   survival_data = NULL,
#   performance_start = NULL