#' Model Predictions Module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
modelPredictionInput <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        "Patient Information",
        tooltip(
          bs_icon("info-circle"),
          "Logistic regression model indicated that Toponin and Creatine kinase-MB levels are the strongest predictors of heart attacks."
        )
      ),
      numericInput(
        ns("troponin"),
        "Troponin (ng/mL)",
        value = 0.01,
        min = 0.001,
        max = 10,
        step = 0.001
      ),
      numericInput(
        ns("ck_mb"),
        "Creatine kinase-MB (ng/mL)",
        min = 0.3,
        max = 300,
        value = 1,
        step = 0.1
      ),
      materialSwitch(
        inputId = ns("show_data"),
        label = "Show Data", 
        value = TRUE,
        status = "success"
      ),
      actionButton(
        ns("about_btn"), 
        "About"
      )
    )
  )
}

modelPredictionOutput <- function(id) {
  ns <- NS(id)
  tagList(
    value_box(
      title = "Probability of Heart Attack",
      value = textOutput(ns("prob_ha")),
      showcase = bs_icon("heart-pulse"),
      max_height = "100px"
    ),
    card(
      # card_header("Key Biomarkers"),
      navset_card_tab(
        nav_panel(
          "Troponin & CK-MB Effect",
          plotOutput(ns("countour_plot"), height = "300px")
        ),
        nav_panel(
          "Troponin Effect",
          plotOutput(ns("troponin_plot"), height = "300px")
        ),
        nav_panel(
          "CK-MB Effect",
          plotOutput(ns("ckmb_plot"), height = "300px")
        )
      )
    )
  )
}


modelPredictionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Validate troponin input
    observeEvent( input$troponin,{
      shinyFeedback::feedbackDanger(
        inputId = "troponin",
        show = input$troponin < 0.001 || input$troponin > 10,
        text = "Troponin must be between 0.001 and 10 ng/mL"
      )
    })
    
    # Validate ck_mb input
    observeEvent(input$ck_mb, {
      shinyFeedback::feedbackDanger(
        inputId = "ck_mb",
        show = input$ck_mb < 0.3 || input$ck_mb > 300,
        text = "CK-MB must be between 0.3 and 300 ng/mL"
      )
    })
    
    output$prob_ha <- renderText({
      pred_data <- predict_result(
        ck_mb = log(input$ck_mb),
        troponin = log(input$troponin),
        model = logistic_model,
        model_data = model_data
      )
      prob <- round(pred_data$predicted_prob, 2)*100
      paste(prob, "%")
    })
    
    output$countour_plot <- renderPlot({
      plot_contour(
        prediction_grid = prediction_grid, 
        model_data = model_data, 
        user_data = tibble(
          log_troponin = log(input$troponin),
          log_ck_mb = log(input$ck_mb)
        ),
        show_data = input$show_data
      )
    })
    
    output$troponin_plot <- renderPlot({
      pred_data <- predict_result(
        ck_mb = log(input$ck_mb),
        troponin = log_troponin_seq,
        model = logistic_model,
        model_data = model_data
      )
      
      plot_pred_data(
        pred_data,
        model_data,
        "log_troponin",
        "Troponin (ng/mL)",
        x_breaks_original = c(0.001, 0.01, 0.1, 1, 10),
        x_int = input$troponin,
        title = "Effect of Troponin on Heart Attack Probability",
        show_data = input$show_data
      )
    })
    
    output$ckmb_plot <- renderPlot({
      pred_data <- predict_result(
        troponin = log(input$troponin),
        ck_mb = log_ckmb_seq,
        model = logistic_model,
        model_data = model_data
      )
      
      plot_pred_data(
        pred_data,
        model_data,
        "log_ck_mb",
        "Creatine kinase-MB (ng/mL)",
        x_breaks_original = c(0.3, 3, 30, 300),
        x_int = input$ck_mb,
        title = "Effect of CK-MB on Heart Attack Probability",
        show_data = input$show_data
      )
    })
    
    observeEvent(input$about_btn, {
      showModal(modalDialog(
        title = "About this App",
        tagList(
          "This app predicts the risk of a heart attack based on patient 
          information. It is intended as a proof-of-concept educational tool and 
          should not be used for medical diagnosis or treatment decisions.
          The model uses logistic regression and is trained on the ",
          a("Heart Attack Risk Assessment Dataset", 
            href = "https://www.kaggle.com/datasets/fajobgiua/heart-attack-risk-assessment-dataset/data", 
            target = "_blank"),
          " from Kaggle."
        ),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
  })
}


# mod app -----------------------------------------------------------------

modelPredictionApp <- function() {
  ui <- page_fluid(
    title = "Predicting the Risk of Heart Attack",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        modelPredictionInput("mod_pred")
      ),
      
      modelPredictionOutput("mod_pred")
      
    )
  )
  
  server <- function(input, output, session) {
    modelPredictionServer("mod_pred")
  }
  
  shinyApp(ui, server)
}
