#' Model Predictions Module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
modelPredictionInput <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(
      ns("age"),
      "Age (Years)",
      value = 60,
      min = min(model_data$age),
      max = max(model_data$age),
      step = 1
    ),
    radioButtons(
      ns("gender"),
      "Gender",
      choices = c("Female", "Male"), 
      selected = "Female"
    ),
    numericInput(
      ns("heart_rate"), 
      "Resting Heart Rate (bpm)", 
      min = 20, 
      max = 135, 
      value = 75
    ),
    numericInput(
      ns("blood_sugar"), 
      "Blood Glucose (mg/dL)", 
      min = 35, 
      max = 500, 
      value = 100
    ),
    sliderInput(
      ns("blood_pressure"), 
      "Systolic/Diastolic Blood Pressure (mmHg)", 
      min = 40, 
      max = 220, 
      value = c(80, 120)
    ),
    actionButton(
      ns("about_btn"), 
      "About"
    )
  )
}

modelPredictionOutput <- function(id) {
  ns <- NS(id)
  tagList(
    value_box(
      title = "Probability of Heart Attack", 
      value = textOutput(ns("prob_ha")),
      showcase = bs_icon("heart-pulse")
    ),
    
    layout_column_wrap(
      width = "400px",
      card(
        card_header(
          "Probability of Positive Result vs. Troponin"
        ),
        plotOutput(ns("troponin_plot")),
        sliderInput(
          ns("ck_mb"), 
          "Creatine kinase-MB (ng/mL)", 
          min = 0.3, 
          max = 300, 
          value = 1
        )
      ),
      card(
        card_header(
          "Probability of Positive Result vs. Creatine kinase-MB"
        ),
        plotOutput(ns("ck_mb_plot")),
        sliderInput(
          ns("troponin"), 
          "Troponin (ng/mL)", 
          min = 0.001, 
          max = 0.3, 
          value = 0.01
        )
      )
    )
  )
}


modelPredictionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$prob_ha <- renderText({
      pred_data <- predict_result(
        age = input$age,
        gender = input$gender,
        heart_rate = input$heart_rate,
        systolic_blood_pressure = input$blood_pressure[2],
        diastolic_blood_pressure = input$blood_pressure[1],
        blood_sugar = input$blood_sugar,
        # ck_mb input is on original scale but model uses log transformed values
        ck_mb = log(input$ck_mb),
        troponin = log(input$troponin),
        logistic_model = logistic_model
      )
      prob <- round(pred_data$predicted_prob, 2)*100
      paste(prob, "%")
    })
    
    
    output$troponin_plot <- renderPlot({
      pred_data <- predict_result(
        age = input$age,
        gender = input$gender,
        heart_rate = input$heart_rate,
        systolic_blood_pressure = input$blood_pressure[2],
        diastolic_blood_pressure = input$blood_pressure[1],
        blood_sugar = input$blood_sugar,
        # ck_mb input is on original scale but model uses log transformed values
        ck_mb = log(input$ck_mb),
        troponin = seq(
          min(model_data$troponin), max(model_data$troponin), length.out = 100
        ),
        logistic_model = logistic_model
      )
      
      plot_pred_data(
        pred_data,
        model_data,
        "troponin",
        "Troponin (ng/mL)",
        x_breaks_original = c(0.001, 0.01, 0.1, 1, 10),
        x_int = input$troponin
      )
    })
    
    output$ck_mb_plot <- renderPlot({
      pred_data <- predict_result(
        age = input$age,
        gender = input$gender,
        heart_rate = input$heart_rate,
        systolic_blood_pressure = input$blood_pressure[2],
        diastolic_blood_pressure = input$blood_pressure[1],
        blood_sugar = input$blood_sugar,
        # troponin input is on original scale but model uses log transformed values
        troponin = log(input$troponin),
        ck_mb = seq(
          min(model_data$ck_mb), max(model_data$ck_mb), length.out = 100
        ),
        logistic_model = logistic_model
      )
      
      plot_pred_data(
        pred_data,
        model_data,
        "ck_mb",
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
    sidebar = sidebar(
      title = h4(
        "Patient Information",
      ),
      width = 350,
      modelPredictionInput("mod_pred")
    ),
    modelPredictionOutput("mod_pred")
  )
  
  server <- function(input, output, session) {
    modelPredictionServer("mod_pred")
  }
  
  shinyApp(ui, server)
}