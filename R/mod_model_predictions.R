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
      sliderInput(
        ns("ck_mb"),
        "Creatine kinase-MB (ng/mL)",
        min = 0.3,
        max = 300,
        value = 1
      ),
      sliderInput(
        ns("troponin"), 
        "Troponin (ng/mL)", 
        min = 0.001, 
        max = 0.3, 
        value = 0.01
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
    output$prob_ha <- renderText({
      pred_data <- predict_result(
        # age = input$age,
        # gender = input$gender,
        # heart_rate = input$heart_rate,
        # systolic_blood_pressure = input$blood_pressure[2],
        # diastolic_blood_pressure = input$blood_pressure[1],
        # blood_sugar = input$blood_sugar,
        # # ck_mb input is on original scale but model uses log transformed values
        ck_mb = log(input$ck_mb),
        troponin = log(input$troponin),
        model = reduced_model
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
        )
      )
    })
    
    output$troponin_plot <- renderPlot({
      pred_data <- predict_result(
        # age = input$age,
        # gender = input$gender,
        # heart_rate = input$heart_rate,
        # systolic_blood_pressure = input$blood_pressure[2],
        # diastolic_blood_pressure = input$blood_pressure[1],
        # blood_sugar = input$blood_sugar,
        # ck_mb input is on original scale but model uses log transformed values
        ck_mb = log(input$ck_mb),
        troponin = log_troponin_seq,
        model = reduced_model
      )
      
      plot_pred_data(
        pred_data,
        model_data,
        "log_troponin",
        "Troponin (ng/mL)",
        x_breaks_original = c(0.001, 0.01, 0.1, 1, 10),
        x_int = input$troponin
      )
    })
    
    output$ckmb_plot <- renderPlot({
      pred_data <- predict_result(
        # age = input$age,
        # gender = input$gender,
        # heart_rate = input$heart_rate,
        # systolic_blood_pressure = input$blood_pressure[2],
        # diastolic_blood_pressure = input$blood_pressure[1],
        # blood_sugar = input$blood_sugar,
        # troponin input is on original scale but model uses log transformed values
        troponin = log(input$troponin),
        ck_mb = log_ckmb_seq,
        model = reduced_model
      )
      
      plot_pred_data(
        pred_data,
        model_data,
        "log_ck_mb",
        "Creatine kinase-MB (ng/mL)",
        x_breaks_original = c(0.3, 3, 30, 300),
        x_int = input$ck_mb
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
  ui <- page_sidebar(
    title = "Predicting the Risk of Heart Attack",
    sidebar = modelPredictionInput("mod_pred"),
    modelPredictionOutput("mod_pred")
  )
  
  server <- function(input, output, session) {
    modelPredictionServer("mod_pred")
  }
  
  shinyApp(ui, server)
}