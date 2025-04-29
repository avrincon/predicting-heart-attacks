server <- function(input, output, session) {
  output$prob_ha <- renderText({
    pred_data <- predict_result(
      age = input$age,
      gender = input$gender,
      heart_rate = mean(model_data$heart_rate),
      systolic_blood_pressure = mean(model_data$systolic_blood_pressure),
      diastolic_blood_pressure = mean(model_data$diastolic_blood_pressure),
      blood_sugar = mean(model_data$blood_sugar),
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
      heart_rate = mean(model_data$heart_rate),
      systolic_blood_pressure = mean(model_data$systolic_blood_pressure),
      diastolic_blood_pressure = mean(model_data$diastolic_blood_pressure),
      blood_sugar = mean(model_data$blood_sugar),
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
      heart_rate = mean(model_data$heart_rate),
      systolic_blood_pressure = mean(model_data$systolic_blood_pressure),
      diastolic_blood_pressure = mean(model_data$diastolic_blood_pressure),
      blood_sugar = mean(model_data$blood_sugar),
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
}