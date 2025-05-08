#' Data Explorer Module
#'
#' @description A Shiny module for exploring variable distributions and statistics
#' in the heart attack risk dataset.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density theme_minimal
#' @importFrom dplyr group_by summarise n
#' @importFrom tidyr pivot_longer
#' @importFrom bslib card card_header
#' @importFrom stats median sd quantile
#'
#' @return A Shiny module for data exploration

# UI Function
dataExplorerInput <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      min_height = "350px",
      card_header(
        "Data Explorer",
        tooltip(
          bs_icon("info-circle"),
          "Select variables to explore their distributions and statistics"
        )
      ),
      # Variable selection
      selectInput(
        ns("variable"),
        "Select Variable:",
        choices = c(
          "Age" = "age",
          "Gender" = "gender", 
          "Heart Rate" = "heart_rate",
          "Systolic Blood Pressure" = "systolic_blood_pressure",
          "Diastolic Blood Pressure" = "diastolic_blood_pressure",
          "Blood Sugar" = "blood_sugar",
          "CK-MB" = "ck_mb",
          "Troponin" = "troponin"
        ),
        selected = "age"
      ),
      # Plot type selection (for numeric variables)
      conditionalPanel(
        condition = "input.variable != 'gender'", 
        ns = ns,
        selectInput(
          ns("plot_type"),
          "Plot Type:",
          choices = c(
            "Histogram" = "histogram",
            "Density" = "density",
            "Boxplot" = "boxplot"
          ),
          selected = "histogram"
        )
      ),
      # Additional option to group by result
      checkboxInput(
        ns("group_by_result"),
        "Group by Heart Attack Result"
      )
    )
  )
}

# Output Function
dataExplorerOutput <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Variable Distribution"),
      plotOutput(ns("dist_plot"), height = "300px")
    ),
    card(
      card_header("Summary Statistics"),
      tableOutput(ns("summary_stats"))
    )
  )
}

# Server Function
dataExplorerServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression to get the selected variable data
    selected_data <- reactive({
      # Ensure the data is properly renamed
      if (input$variable == "gender") {
        # For categorical variables, just return the data as is
        return(data)
      } else {
        # For numeric variables, transform data if needed
        if (input$variable %in% c("ck_mb", "troponin")) {
          # These variables are log-transformed in the model
          return(data)
        } else {
          return(data)
        }
      }
    })
    
    # Distribution plot
    output$dist_plot <- renderPlot({
      # Get data
      plot_data <- selected_data()
      selected_var <- input$variable
      
      if (selected_var == "gender") {
        # For gender (categorical), create a bar chart
        p <- ggplot(plot_data, aes(x = gender, fill = factor(result))) 
        
        if (input$group_by_result) {
          # Grouped bar chart
          p <- p + 
            geom_bar(position = "dodge") +
            scale_fill_manual(
              values = c("negative" = "#2E9093", "positive" = "#DD4124"),
              name = "Heart Attack Result",
              labels = c("Negative", "Positive")
            )
        } else {
          # Simple bar chart
          p <- p + 
            geom_bar(fill = "#2E9093") +
            guides(fill = "none")
        }
        
        p <- p + 
          labs(
            title = "Distribution of Gender",
            x = "Gender",
            y = "Count"
          ) +
          theme_minimal() +
          theme(text = element_text(size = 14))
        
      } else {
        # For numeric variables
        if (input$group_by_result) {
          # Plot grouped by result
          if (input$plot_type == "histogram") {
            p <- ggplot(
              plot_data, 
              aes(x = .data[[selected_var]], fill = factor(result))
              ) +
              geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
              scale_fill_manual(
                values = c("negative" = "#2E9093", "positive" = "#DD4124"),
                name = "Heart Attack Result",
                labels = c("Negative", "Positive")
              )
          } else if (input$plot_type == "density") {
            p <- ggplot(plot_data, aes(x = .data[[selected_var]], fill = factor(result))) +
              geom_density(alpha = 0.6) +
              scale_fill_manual(
                values = c("negative" = "#2E9093", "positive" = "#DD4124"),
                name = "Heart Attack Result",
                labels = c("Negative", "Positive")
              )
          } else { # boxplot
            p <- ggplot(plot_data, aes(x = factor(result), y = .data[[selected_var]], fill = factor(result))) +
              geom_boxplot() +
              scale_fill_manual(
                values = c("negative" = "#2E9093", "positive" = "#DD4124"),
                name = "Heart Attack Result",
                labels = c("Negative", "Positive")
              )
          }
        } else {
          # Plot without grouping
          if (input$plot_type == "histogram") {
            p <- ggplot(plot_data, aes(x = .data[[selected_var]])) +
              geom_histogram(fill = "#2E9093", color = "white", bins = 30)
          } else if (input$plot_type == "density") {
            p <- ggplot(plot_data, aes(x = .data[[selected_var]])) +
              geom_density(fill = "#2E9093", alpha = 0.6)
          } else { # boxplot
            p <- ggplot(plot_data, aes(y = .data[[selected_var]])) +
              geom_boxplot(fill = "#2E9093")
          }
        }
        
        # Apply log transformation for visuals when appropriate
        if (selected_var %in% c("ck_mb", "troponin")) {
          # Nice labels for log-transformed values
          if (selected_var == "troponin") {
            p <- p + 
              scale_x_continuous(
                breaks = c(0.001, 0.01, 0.1, 1, 10),
                trans = "log"
              )
          } else if (selected_var == "ck_mb") {
            p <- p + 
              scale_x_continuous(
                breaks = c(0.3, 3, 30, 300),
                trans = "log"
              )
          }
        }
        
        # Get formatted title and x-axis label
        var_label <- switch(selected_var,
                            age = "Age",
                            heart_rate = "Heart Rate (bpm)",
                            systolic_blood_pressure = "Systolic Blood Pressure (mmHg)",
                            diastolic_blood_pressure = "Diastolic Blood Pressure (mmHg)",
                            blood_sugar = "Blood Sugar (mg/dL)",
                            ck_mb = "Creatine kinase-MB (ng/mL)",
                            troponin = "Troponin (ng/mL)",
                            selected_var)
        
        p <- p + 
          labs(
            title = paste("Distribution of", var_label),
            x = var_label,
            y = ifelse(input$plot_type == "boxplot", var_label, "Count/Density")
          ) +
          theme_minimal() +
          theme(text = element_text(size = 14))
      }
      
      return(p)
    })
    
    # Summary statistics
    output$summary_stats <- renderTable({
      # Get data
      plot_data <- selected_data()
      selected_var <- input$variable
      
      if (selected_var == "gender") {
        # For gender, show counts and percentages
        if (input$group_by_result) {
          # Group by result and gender
          stats <- plot_data %>%
            group_by(result, gender) %>%
            summarise(
              count = n(),
              percentage = sprintf("%.1f%%", 100 * n() / nrow(plot_data)),
              .groups = "drop"
            ) %>%
            arrange(result, gender)
        } else {
          # Just group by gender
          stats <- plot_data %>%
            group_by(gender) %>%
            summarise(
              count = n(),
              percentage = sprintf("%.1f%%", 100 * n() / nrow(plot_data))
            )
        }
      } else {
        # For numeric variables
        if (input$group_by_result) {
          # Group by result
          stats <- plot_data %>%
            group_by(result) %>%
            summarise(
              n = n(),
              min = min(.data[[selected_var]]),
              q1 = quantile(.data[[selected_var]], 0.25),
              median = median(.data[[selected_var]]),
              mean = mean(.data[[selected_var]]),
              q3 = quantile(.data[[selected_var]], 0.75),
              max = max(.data[[selected_var]]),
              sd = sd(.data[[selected_var]])
            ) %>%
            mutate(
              result = str_to_title(result),
              across(where(is.numeric), ~ round(., 1))
            )
        } else {
          # Overall statistics
          stats <- plot_data %>%
            summarise(
              n = n(),
              min = min(.data[[selected_var]]),
              q1 = quantile(.data[[selected_var]], 0.25),
              median = median(.data[[selected_var]]),
              mean = mean(.data[[selected_var]]),
              q3 = quantile(.data[[selected_var]], 0.75),
              max = max(.data[[selected_var]]),
              sd = sd(.data[[selected_var]])
            ) %>%
            mutate(
              across(where(is.numeric), ~ round(., 1))
            )
        }
      }
      
      return(stats)
    })
    
  })
}

# App Function for Testing
dataExplorerApp <- function() {
  # Load necessary data
  model_data <- readRDS("model/model_data.rds")
  
  ui <- page_fluid(
    title = "Heart Attack Risk - Data Explorer",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        dataExplorerInput("data_explorer")
      ),
      
      dataExplorerOutput("data_explorer")
    )
  )
  
  server <- function(input, output, session) {
    dataExplorerServer("data_explorer", model_data)
  }
  
  shinyApp(ui, server)
}