# app.R - Uganda Child Health & Nutrition Analytics Dashboard
# Supporting World Vision's mission through data-driven insights

library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(DT)
library(randomForest)
library(glmnet)
library(MatchIt)
library(broom)
library(viridis)

# Generate synthetic Uganda health data (in production, load real CSVs)
generate_uganda_data <- function() {
  set.seed(42)
  years <- 2010:2023
  regions <- c("Central", "Eastern", "Northern", "Western")
  
  # Create comprehensive dataset
  data <- expand.grid(
    year = years,
    region = regions,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      # Child mortality indicators
      under5_mortality = case_when(
        region == "Northern" ~ 100 - (year - 2010) * 2.5 + rnorm(n(), 0, 3),
        region == "Eastern" ~ 95 - (year - 2010) * 2.3 + rnorm(n(), 0, 3),
        region == "Western" ~ 92 - (year - 2010) * 2.4 + rnorm(n(), 0, 3),
        TRUE ~ 88 - (year - 2010) * 2.6 + rnorm(n(), 0, 3)
      ),
      infant_mortality = under5_mortality * 0.7 + rnorm(n(), 0, 2),
      
      # Nutrition indicators
      stunting_prevalence = case_when(
        region == "Northern" ~ 42 - (year - 2010) * 0.8 + rnorm(n(), 0, 2),
        region == "Eastern" ~ 40 - (year - 2010) * 0.9 + rnorm(n(), 0, 2),
        TRUE ~ 38 - (year - 2010) * 1.0 + rnorm(n(), 0, 2)
      ),
      wasting_prevalence = 5 + rnorm(n(), 0, 1),
      underweight_prevalence = 15 - (year - 2010) * 0.5 + rnorm(n(), 0, 1.5),
      
      # Health system indicators
      vaccination_coverage = 50 + (year - 2010) * 3 + rnorm(n(), 0, 3),
      antenatal_care_coverage = 60 + (year - 2010) * 2.5 + rnorm(n(), 0, 2),
      skilled_birth_attendance = 55 + (year - 2010) * 2.8 + rnorm(n(), 0, 2.5),
      healthcare_facilities_per_100k = 10 + (year - 2010) * 0.5 + rnorm(n(), 0, 0.5),
      
      # Disease burden
      malaria_incidence_per_1000 = 300 - (year - 2010) * 8 + rnorm(n(), 0, 10),
      hiv_prevalence = 7.5 - (year - 2010) * 0.2 + rnorm(n(), 0, 0.3),
      tuberculosis_incidence_per_100k = 200 - (year - 2010) * 5 + rnorm(n(), 0, 5),
      
      # WASH indicators
      clean_water_access = 50 + (year - 2010) * 2 + rnorm(n(), 0, 2),
      sanitation_access = 45 + (year - 2010) * 1.8 + rnorm(n(), 0, 2),
      
      # Socioeconomic factors
      maternal_education_years = 6 + (year - 2010) * 0.3 + rnorm(n(), 0, 0.5),
      poverty_rate = 45 - (year - 2010) * 1.5 + rnorm(n(), 0, 2)
    ) %>%
    mutate_if(is.numeric, ~pmax(., 0)) %>%  # Ensure no negative values
    mutate(
      vaccination_coverage = pmin(vaccination_coverage, 95),
      antenatal_care_coverage = pmin(antenatal_care_coverage, 95),
      skilled_birth_attendance = pmin(skilled_birth_attendance, 95),
      clean_water_access = pmin(clean_water_access, 95),
      sanitation_access = pmin(sanitation_access, 95)
    )
  
  return(data)
}

# Load data
uganda_data <- generate_uganda_data()

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Uganda Child Health Analytics - World Vision",
    titleWidth = 450
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Statistical Modeling", tabName = "modeling", icon = icon("chart-line")),
      menuItem("Causal Inference", tabName = "causal", icon = icon("project-diagram")),
      menuItem("Intervention Simulator", tabName = "intervention", icon = icon("cogs")),
      menuItem("Machine Learning", tabName = "ml", icon = icon("brain"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .small-box {
          border-radius: 10px;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("mortality_box"),
          valueBoxOutput("stunting_box"),
          valueBoxOutput("vaccination_box")
        ),
        
        fluidRow(
          box(
            title = "Key Health Indicators Trend",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("trend_plot", height = "400px")
          ),
          
          box(
            title = "Controls",
            status = "info",
            width = 4,
            selectInput(
              "overview_indicator",
              "Select Indicator:",
              choices = c(
                "Under-5 Mortality" = "under5_mortality",
                "Stunting Prevalence" = "stunting_prevalence",
                "Vaccination Coverage" = "vaccination_coverage",
                "Malaria Incidence" = "malaria_incidence_per_1000",
                "Clean Water Access" = "clean_water_access"
              )
            ),
            
            sliderInput(
              "year_range",
              "Year Range:",
              min = 2010,
              max = 2023,
              value = c(2015, 2023),
              step = 1
            ),
            
            checkboxGroupInput(
              "regions",
              "Select Regions:",
              choices = c("Central", "Eastern", "Northern", "Western"),
              selected = c("Central", "Eastern", "Northern", "Western")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Regional Disparities (Latest Year)",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("regional_plot", height = "350px")
          )
        )
      ),
      
      # Statistical Modeling Tab
      tabItem(
        tabName = "modeling",
        fluidRow(
          box(
            title = "GLM Analysis Configuration",
            status = "primary",
            width = 4,
            selectInput(
              "glm_outcome",
              "Outcome Variable:",
              choices = c(
                "Under-5 Mortality" = "under5_mortality",
                "Stunting Prevalence" = "stunting_prevalence",
                "Malaria Incidence" = "malaria_incidence_per_1000"
              )
            ),
            
            checkboxGroupInput(
              "glm_predictors",
              "Select Predictors:",
              choices = c(
                "Vaccination Coverage" = "vaccination_coverage",
                "Antenatal Care" = "antenatal_care_coverage",
                "Skilled Birth Attendance" = "skilled_birth_attendance",
                "Clean Water Access" = "clean_water_access",
                "Maternal Education" = "maternal_education_years",
                "Healthcare Facilities" = "healthcare_facilities_per_100k"
              ),
              selected = c("vaccination_coverage", "antenatal_care_coverage", "clean_water_access")
            ),
            
            radioButtons(
              "model_type",
              "Model Type:",
              choices = c("Linear Model (GLM)" = "glm", "Ridge Regression" = "ridge", "LASSO" = "lasso")
            ),
            
            actionButton("run_model", "Run Analysis", class = "btn-success")
          ),
          
          box(
            title = "Model Results",
            status = "success",
            width = 8,
            verbatimTextOutput("model_summary"),
            plotlyOutput("coefficient_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Model Diagnostics",
            status = "info",
            width = 12,
            plotlyOutput("diagnostic_plots", height = "400px")
          )
        )
      ),
      
      # Causal Inference Tab
      tabItem(
        tabName = "causal",
        fluidRow(
          box(
            title = "Propensity Score Matching",
            status = "primary",
            width = 12,
            h4("Evaluate the causal effect of health interventions using PSM"),
            br(),
            
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "treatment_var",
                  "Treatment Variable:",
                  choices = c(
                    "High Vaccination Coverage (>80%)" = "high_vaccination",
                    "Access to Skilled Birth Attendance (>70%)" = "high_skilled_birth",
                    "Clean Water Access (>70%)" = "high_water_access"
                  )
                ),
                
                selectInput(
                  "psm_outcome",
                  "Outcome Variable:",
                  choices = c(
                    "Under-5 Mortality" = "under5_mortality",
                    "Stunting Prevalence" = "stunting_prevalence"
                  )
                ),
                
                actionButton("run_psm", "Run PSM Analysis", class = "btn-primary")
              ),
              
              column(
                width = 8,
                plotlyOutput("psm_balance", height = "300px")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Treatment Effect Estimation",
            status = "success",
            width = 6,
            tableOutput("treatment_effects")
          ),
          
          box(
            title = "Matched Sample Comparison",
            status = "info",
            width = 6,
            plotlyOutput("matched_comparison")
          )
        )
      ),
      
      # Intervention Simulator Tab
      tabItem(
        tabName = "intervention",
        fluidRow(
          box(
            title = "Design Your Intervention",
            status = "primary",
            width = 12,
            
            fluidRow(
              column(
                width = 3,
                h4("Vaccination Program"),
                sliderInput("vacc_increase", "Coverage Increase (%)", 0, 30, 10),
                numericInput("vacc_cost", "Cost per % point ($)", 50000, min = 0)
              ),
              
              column(
                width = 3,
                h4("Nutrition Program"),
                sliderInput("nutrition_coverage", "Program Coverage (%)", 0, 50, 20),
                numericInput("nutrition_cost", "Cost per % point ($)", 30000, min = 0)
              ),
              
              column(
                width = 3,
                h4("WASH Infrastructure"),
                sliderInput("wash_improvement", "Access Improvement (%)", 0, 40, 15),
                numericInput("wash_cost", "Cost per % point ($)", 75000, min = 0)
              ),
              
              column(
                width = 3,
                h4("Budget Constraint"),
                numericInput("total_budget", "Total Budget ($)", 5000000, min = 0),
                br(),
                actionButton("simulate_intervention", "Simulate Impact", 
                           class = "btn-success btn-lg")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Projected Impact",
            status = "success",
            width = 6,
            plotlyOutput("impact_visualization", height = "400px")
          ),
          
          box(
            title = "Cost-Effectiveness Analysis",
            status = "warning",
            width = 6,
            DTOutput("cost_effectiveness_table"),
            br(),
            valueBoxOutput("lives_saved"),
            valueBoxOutput("cost_per_life")
          )
        )
      ),
      
      # Machine Learning Tab
      tabItem(
        tabName = "ml",
        fluidRow(
          box(
            title = "Random Forest Prediction Model",
            status = "primary",
            width = 12,
            h4("Using ensemble methods to predict health outcomes"),
            
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "rf_target",
                  "Target Variable:",
                  choices = c(
                    "Under-5 Mortality" = "under5_mortality",
                    "Stunting Prevalence" = "stunting_prevalence"
                  )
                ),
                
                sliderInput(
                  "train_split",
                  "Training Data %:",
                  min = 60,
                  max = 90,
                  value = 80,
                  step = 5
                ),
                
                actionButton("train_rf", "Train Model", class = "btn-warning")
              ),
              
              column(
                width = 8,
                plotlyOutput("feature_importance", height = "400px")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Model Performance",
            status = "info",
            width = 6,
            verbatimTextOutput("rf_performance")
          ),
          
          box(
            title = "Predictions vs Actual",
            status = "success",
            width = 6,
            plotlyOutput("prediction_plot")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive data based on user selections
  filtered_data <- reactive({
    uganda_data %>%
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2],
        region %in% input$regions
      )
  })
  
  # Overview Tab Outputs
  output$mortality_box <- renderValueBox({
    latest_data <- uganda_data %>%
      filter(year == max(year)) %>%
      summarise(avg_mortality = mean(under5_mortality))
    
    valueBox(
      value = round(latest_data$avg_mortality, 1),
      subtitle = "Under-5 Mortality Rate",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  output$stunting_box <- renderValueBox({
    latest_data <- uganda_data %>%
      filter(year == max(year)) %>%
      summarise(avg_stunting = mean(stunting_prevalence))
    
    valueBox(
      value = paste0(round(latest_data$avg_stunting, 1), "%"),
      subtitle = "Stunting Prevalence",
      icon = icon("child"),
      color = "yellow"
    )
  })
  
  output$vaccination_box <- renderValueBox({
    latest_data <- uganda_data %>%
      filter(year == max(year)) %>%
      summarise(avg_vacc = mean(vaccination_coverage))
    
    valueBox(
      value = paste0(round(latest_data$avg_vacc, 1), "%"),
      subtitle = "Vaccination Coverage",
      icon = icon("syringe"),
      color = "green"
    )
  })
  
  output$trend_plot <- renderPlotly({
    trend_data <- filtered_data() %>%
      group_by(year) %>%
      summarise(
        avg_value = mean(!!sym(input$overview_indicator)),
        .groups = 'drop'
      )
    
    p <- plot_ly(
      data = trend_data,
      x = ~year,
      y = ~avg_value,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(width = 3, color = '#1f77b4'),
      marker = list(size = 8)
    ) %>%
      layout(
        title = paste("Trend:", names(which(c(
          "Under-5 Mortality" = "under5_mortality",
          "Stunting Prevalence" = "stunting_prevalence",
          "Vaccination Coverage" = "vaccination_coverage",
          "Malaria Incidence" = "malaria_incidence_per_1000",
          "Clean Water Access" = "clean_water_access"
        ) == input$overview_indicator))),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Value"),
        hovermode = 'x unified'
      )
    
    p
  })
  
  output$regional_plot <- renderPlotly({
    latest_year <- max(uganda_data$year)
    regional_data <- uganda_data %>%
      filter(year == latest_year) %>%
      select(region, under5_mortality, stunting_prevalence, vaccination_coverage, malaria_incidence_per_1000)
    
    plot_ly(
      data = regional_data,
      x = ~region,
      y = ~under5_mortality,
      type = 'bar',
      name = 'Under-5 Mortality',
      marker = list(color = '#ff7f0e')
    ) %>%
      add_trace(
        y = ~stunting_prevalence,
        name = 'Stunting %',
        marker = list(color = '#2ca02c')
      ) %>%
      add_trace(
        y = ~vaccination_coverage,
        name = 'Vaccination %',
        marker = list(color = '#d62728')
      ) %>%
      layout(
        title = paste("Regional Health Indicators -", latest_year),
        xaxis = list(title = "Region"),
        yaxis = list(title = "Value"),
        barmode = 'group'
      )
  })
  
  # Statistical Modeling Tab
  model_results <- eventReactive(input$run_model, {
    req(length(input$glm_predictors) > 0)
    
    # Prepare data
    model_data <- uganda_data %>%
      select(all_of(c(input$glm_outcome, input$glm_predictors)))
    
    if (input$model_type == "glm") {
      # Fit GLM
      formula_str <- paste(input$glm_outcome, "~", paste(input$glm_predictors, collapse = " + "))
      model <- lm(as.formula(formula_str), data = model_data)
      
      list(
        model = model,
        coefficients = tidy(model),
        summary = summary(model),
        type = "glm"
      )
    } else {
      # Prepare matrix for glmnet
      X <- as.matrix(model_data[, input$glm_predictors])
      y <- model_data[[input$glm_outcome]]
      
      alpha <- ifelse(input$model_type == "ridge", 0, 1)
      model <- cv.glmnet(X, y, alpha = alpha)
      
      # Extract coefficients at optimal lambda
      coef_values <- coef(model, s = "lambda.min")
      coef_df <- data.frame(
        term = rownames(coef_values),
        estimate = as.vector(coef_values),
        stringsAsFactors = FALSE
      ) %>%
        filter(term != "(Intercept)", estimate != 0)
      
      list(
        model = model,
        coefficients = coef_df,
        summary = model,
        type = input$model_type
      )
    }
  })
  
  output$model_summary <- renderPrint({
    req(model_results())
    
    if (model_results()$type == "glm") {
      summary(model_results()$model)
    } else {
      cat("Regularized Regression Model\n")
      cat("Optimal Lambda:", model_results()$model$lambda.min, "\n")
      cat("Number of non-zero coefficients:", 
          sum(coef(model_results()$model, s = "lambda.min") != 0) - 1, "\n")
    }
  })
  
  output$coefficient_plot <- renderPlotly({
    req(model_results())
    
    coef_data <- model_results()$coefficients
    
    if (model_results()$type == "glm") {
      coef_data <- coef_data %>%
        filter(term != "(Intercept)") %>%
        mutate(
          significant = p.value < 0.05,
          color = ifelse(significant, "Significant", "Not Significant")
        )
      
      p <- plot_ly(
        data = coef_data,
        x = ~estimate,
        y = ~term,
        type = 'bar',
        orientation = 'h',
        color = ~color,
        colors = c("Significant" = "#2ca02c", "Not Significant" = "#d62728"),
        error_x = list(
          type = "data",
          array = ~std.error * 1.96,
          visible = TRUE
        )
      )
    } else {
      p <- plot_ly(
        data = coef_data,
        x = ~estimate,
        y = ~term,
        type = 'bar',
        orientation = 'h',
        marker = list(color = '#1f77b4')
      )
    }
    
    p %>% layout(
      title = "Model Coefficients",
      xaxis = list(title = "Coefficient Value"),
      yaxis = list(title = ""),
      showlegend = TRUE
    )
  })
  
  # Causal Inference Tab
  psm_results <- eventReactive(input$run_psm, {
    # Create treatment variable
    psm_data <- uganda_data %>%
      mutate(
        high_vaccination = vaccination_coverage > 80,
        high_skilled_birth = skilled_birth_attendance > 70,
        high_water_access = clean_water_access > 70
      )
    
    # Select treatment based on input
    treatment_col <- input$treatment_var
    
    # Perform matching
    match_formula <- as.formula(paste(treatment_col, "~ vaccination_coverage + antenatal_care_coverage + 
                                     clean_water_access + maternal_education_years + healthcare_facilities_per_100k"))
    
    matched <- matchit(match_formula, data = psm_data, method = "nearest", ratio = 1)
    
    # Get matched data
    matched_data <- match.data(matched)
    
    # Calculate treatment effect
    treatment_effect <- matched_data %>%
      group_by(!!sym(treatment_col)) %>%
      summarise(
        mean_outcome = mean(!!sym(input$psm_outcome)),
        sd_outcome = sd(!!sym(input$psm_outcome)),
        n = n()
      )
    
    list(
      matched = matched,
      matched_data = matched_data,
      treatment_effect = treatment_effect
    )
  })
  
  output$psm_balance <- renderPlotly({
    req(psm_results())
    
    # Create balance plot
    matched_summary <- summary(psm_results()$matched)
    
    # Extract balance statistics
    balance_data <- data.frame(
      Variable = rownames(matched_summary$sum.all)[-1],
      Before = matched_summary$sum.all[-1, "Std. Mean Diff."],
      After = matched_summary$sum.matched[-1, "Std. Mean Diff."]
    )
    
    plot_ly(balance_data) %>%
      add_trace(
        y = ~Variable,
        x = ~Before,
        type = 'scatter',
        mode = 'markers',
        name = 'Before Matching',
        marker = list(size = 10, color = 'red')
      ) %>%
      add_trace(
        y = ~Variable,
        x = ~After,
        type = 'scatter',
        mode = 'markers',
        name = 'After Matching',
        marker = list(size = 10, color = 'green')
      ) %>%
      layout(
        title = "Covariate Balance: Before vs After Matching",
        xaxis = list(title = "Standardized Mean Difference"),
        yaxis = list(title = ""),
        shapes = list(
          list(type = "line", x0 = -0.1, x1 = -0.1, y0 = -0.5, y1 = length(balance_data$Variable),
               line = list(color = "gray", dash = "dash")),
          list(type = "line", x0 = 0.1, x1 = 0.1, y0 = -0.5, y1 = length(balance_data$Variable),
               line = list(color = "gray", dash = "dash"))
        )
      )
  })
  
  output$treatment_effects <- renderTable({
    req(psm_results())
    
    te <- psm_results()$treatment_effect
    ate <- diff(te$mean_outcome)
    
    data.frame(
      Metric = c("Control Group Mean", "Treatment Group Mean", "Average Treatment Effect", "Effect Size (%)"),
      Value = c(
        round(te$mean_outcome[1], 2),
        round(te$mean_outcome[2], 2),
        round(ate, 2),
        paste0(round(ate / te$mean_outcome[1] * 100, 1), "%")
      )
    )
  })
  
  # Intervention Simulator
  intervention_results <- eventReactive(input$simulate_intervention, {
    # Calculate costs
    total_cost <- (input$vacc_increase * input$vacc_cost + 
                   input$nutrition_coverage * input$nutrition_cost +
                   input$wash_improvement * input$wash_cost)
    
    # Check budget constraint
    if (total_cost > input$total_budget) {
      showNotification("Warning: Total cost exceeds budget!", type = "warning")
    }
    
    # Simulate impact based on historical correlations
    baseline <- uganda_data %>%
      filter(year == max(year)) %>%
      summarise(
        mortality = mean(under5_mortality),
        stunting = mean(stunting_prevalence)
      )
    
    # Simplified impact model (in reality, use more sophisticated modeling)
    mortality_reduction <- input$vacc_increase * 0.3 + 
                          input$nutrition_coverage * 0.15 +
                          input$wash_improvement * 0.2
    
    stunting_reduction <- input$nutrition_coverage * 0.25 +
                         input$wash_improvement * 0.15
    
    new_mortality <- max(20, baseline$mortality - mortality_reduction)
    new_stunting <- max(15, baseline$stunting - stunting_reduction)
    
    # Calculate lives saved (per 100,000 under-5 population)
    lives_saved <- (baseline$mortality - new_mortality) * 100
    
    list(
      baseline = baseline,
      new_mortality = new_mortality,
      new_stunting = new_stunting,
      lives_saved = lives_saved,
      total_cost = total_cost,
      cost_per_life = ifelse(lives_saved > 0, total_cost / lives_saved, NA)
    )
  })
  
  output$impact_visualization <- renderPlotly({
    req(intervention_results())
    
    results <- intervention_results()
    
    comparison_data <- data.frame(
      Scenario = rep(c("Baseline", "With Intervention"), 2),
      Indicator = c("Under-5 Mortality", "Under-5 Mortality", "Stunting %", "Stunting %"),
      Value = c(results$baseline$mortality, results$new_mortality,
                results$baseline$stunting, results$new_stunting)
    )
    
    plot_ly(
      data = comparison_data,
      x = ~Indicator,
      y = ~Value,
      color = ~Scenario,
      type = 'bar',
      colors = c("Baseline" = "#d62728", "With Intervention" = "#2ca02c")
    ) %>%
      layout(
        title = "Projected Health Outcomes",
        xaxis = list(title = ""),
        yaxis = list(title = "Value"),
        barmode = 'group'
      )
  })
  
  output$cost_effectiveness_table <- renderDT({
    req(intervention_results())
    
    results <- intervention_results()
    
    ce_data <- data.frame(
      Intervention = c("Vaccination Program", "Nutrition Program", "WASH Infrastructure"),
      Coverage = c(
        paste0(input$vacc_increase, "%"),
        paste0(input$nutrition_coverage, "%"),
        paste0(input$wash_improvement, "%")
      ),
      Cost = c(
        input$vacc_increase * input$vacc_cost,
        input$nutrition_coverage * input$nutrition_cost,
        input$wash_improvement * input$wash_cost
      )
    ) %>%
      mutate(
        Cost = paste0("$", format(Cost, big.mark = ","))
      )
    
    datatable(ce_data, options = list(dom = 't', pageLength = 3))
  })
  
  output$lives_saved <- renderValueBox({
    req(intervention_results())
    
    valueBox(
      value = round(intervention_results()$lives_saved),
      subtitle = "Lives Saved (per 100,000)",
      icon = icon("heartbeat"),
      color = "green"
    )
  })
  
  output$cost_per_life <- renderValueBox({
    req(intervention_results())
    
    cpl <- intervention_results()$cost_per_life
    
    valueBox(
      value = ifelse(is.na(cpl), "N/A", paste0("$", format(round(cpl), big.mark = ","))),
      subtitle = "Cost per Life Saved",
      icon = icon("dollar-sign"),
      color = "blue"
    )
  })
  
  # Machine Learning Tab
  rf_model <- eventReactive(input$train_rf, {
    # Prepare data for Random Forest
    ml_data <- uganda_data %>%
      select(-year, -region) %>%
      na.omit()
    
    # Split data
    set.seed(123)
    train_idx <- sample(1:nrow(ml_data), nrow(ml_data) * input$train_split / 100)
    train_data <- ml_data[train_idx, ]
    test_data <- ml_data[-train_idx, ]
    
    # Separate features and target
    target_col <- input$rf_target
    features <- setdiff(names(train_data), target_col)
    
    # Train Random Forest
    rf_formula <- as.formula(paste(target_col, "~ ."))
    model <- randomForest(rf_formula, data = train_data, ntree = 500, importance = TRUE)
    
    # Make predictions
    predictions <- predict(model, test_data)
    actual <- test_data[[target_col]]
    
    # Calculate performance metrics
    rmse <- sqrt(mean((predictions - actual)^2))
    r_squared <- 1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)
    mae <- mean(abs(predictions - actual))
    
    list(
      model = model,
      predictions = predictions,
      actual = actual,
      rmse = rmse,
      r_squared = r_squared,
      mae = mae,
      importance = importance(model)
    )
  })
  
  output$feature_importance <- renderPlotly({
    req(rf_model())
    
    # Get feature importance
    imp_data <- rf_model()$importance %>%
      as.data.frame() %>%
      mutate(Variable = rownames(.)) %>%
      arrange(desc(`%IncMSE`))
    
    # Top 10 features
    top_features <- head(imp_data, 10)
    
    plot_ly(
      data = top_features,
      x = ~`%IncMSE`,
      y = ~reorder(Variable, `%IncMSE`),
      type = 'bar',
      orientation = 'h',
      marker = list(color = viridis(10))
    ) %>%
      layout(
        title = "Feature Importance (Top 10)",
        xaxis = list(title = "% Increase in MSE"),
        yaxis = list(title = ""),
        margin = list(l = 150)
      )
  })
  
  output$rf_performance <- renderPrint({
    req(rf_model())
    
    cat("Random Forest Model Performance\n")
    cat("================================\n")
    cat("RMSE:", round(rf_model()$rmse, 3), "\n")
    cat("R-squared:", round(rf_model()$r_squared, 3), "\n")
    cat("MAE:", round(rf_model()$mae, 3), "\n")
    cat("\nModel Details:\n")
    print(rf_model()$model)
  })
  
  output$prediction_plot <- renderPlotly({
    req(rf_model())
    
    pred_data <- data.frame(
      Actual = rf_model()$actual,
      Predicted = rf_model()$predictions
    )
    
    # Add perfect prediction line
    min_val <- min(c(pred_data$Actual, pred_data$Predicted))
    max_val <- max(c(pred_data$Actual, pred_data$Predicted))
    
    plot_ly(data = pred_data) %>%
      add_trace(
        x = ~Actual,
        y = ~Predicted,
        type = 'scatter',
        mode = 'markers',
        marker = list(
          size = 8,
          color = ~abs(Actual - Predicted),
          colorscale = 'Viridis',
          showscale = TRUE,
          colorbar = list(title = "Prediction Error")
        ),
        name = 'Predictions'
      ) %>%
      add_trace(
        x = c(min_val, max_val),
        y = c(min_val, max_val),
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'red', dash = 'dash'),
        name = 'Perfect Prediction'
      ) %>%
      layout(
        title = "Predicted vs Actual Values",
        xaxis = list(title = "Actual"),
        yaxis = list(title = "Predicted"),
        showlegend = TRUE
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
