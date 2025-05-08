#' Model Diagnostics Module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
modelDiagnosticsInput <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        "Model Diagnostics",
        tooltip(
          bs_icon("info-circle"),
          "Examine the performance of the logistic regression model"
        )
      ),
      actionButton(
        ns("model_info_btn"), 
        "Model Information"
      )
    )
  )
}

modelDiagnosticsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Confusion Matrix"),
        plotOutput(ns("confusion_matrix"))
      ),
      card(
        card_header("ROC Curve"),
        plotOutput(ns("roc_curve"))
      )
    ),
    card(
      card_header("Model Coefficients"),
      tableOutput(ns("coef_table"))
    )
  )
}

modelDiagnosticsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$coef_table <- renderTable({
      mod_summary2 <- readRDS("model/mod_summary_table.rds")
      mod_summary2
    }, bordered = TRUE, striped = TRUE, hover = TRUE)
    
    output$confusion_matrix <- renderPlot({
      p_cm <- readRDS("model/confusion_matrix_plot.rds")
      p_cm
    })
    
    output$roc_curve <- renderPlot({
      p_roc <- readRDS("model/roc_curve_plot.rds")
      p_roc
    })
    
  })
}


# Test app for this module alone -----------------------------------------

modelDiagnosticsApp <- function() {
  ui <- page_fluid(
    title = "Model Diagnostics for Heart Attack Risk",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        modelDiagnosticsInput("mod_diag")
      ),
      
      modelDiagnosticsOutput("mod_diag")
      
    )
  )
  
  server <- function(input, output, session) {
    modelDiagnosticsServer("mod_diag")
  }
  
  shinyApp(ui, server)
}