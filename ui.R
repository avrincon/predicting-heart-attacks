ui <- page_navbar(
  title = "Predicting the Risk of Heart Attack",
  nav_panel(
    title = "Model Predictions",
    value = "pred_tab",
    icon = bsicons::bs_icon(name = "card-list"),
    layout_sidebar(
      sidebar = sidebar(
        title = h4(
          "Patient Information",
          tooltip(
            bs_icon("info-circle"),
            "Patient features below have a minimal impact on the risk of a heart attack. The most influential predictors are Toponin and Creatine kinase-MB levels."
          )
        ),
        width = 350,
        
        numericInput(
          "age",
          "Age (Years)",
          value = 60,
          min = min(model_data$age),
          max = max(model_data$age),
          step = 1
        ),
        radioButtons(
          "gender",
          "Gender",
          choices = c("Female", "Male"), 
          selected = "Female"
        ),
        numericInput(
          "heart_rate", 
          "Resting Heart Rate (bpm)", 
          min = 20, 
          max = 135, 
          value = 75
        ),
        numericInput(
          "blood_sugar", 
          "Blood Glucose (mg/dL)", 
          min = 35, 
          max = 500, 
          value = 100
        ),
        sliderInput(
          "blood_pressure", 
          "Systolic/Diastolic Blood Pressure (mmHg)", 
          min = 40, 
          max = 220, 
          value = c(80, 120)
        ),
        actionButton(
          "about_btn", 
          "About"
        )
      ),
      
      # main panel ----------------------------------------------------------

      value_box(
        title = "Probability of Heart Attack", 
        value = textOutput("prob_ha"),
        showcase = bs_icon("heart-pulse")
      ),
      
      layout_column_wrap(
        width = "400px",
        card(
          card_header(
            "Probability of Positive Result vs. Troponin"
          ),
          plotOutput("troponin_plot"),
          sliderInput(
            "ck_mb", 
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
          plotOutput("ck_mb_plot"),
          sliderInput(
            "troponin", 
            "Troponin (ng/mL)", 
            min = 0.001, 
            max = 0.3, 
            value = 0.01
          )
        )
      )
    )
  )
)

