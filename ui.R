ui <- page_navbar(
  
  nav_panel(
    title = "Model Predictions",
    value = "pred_tab",
    icon = bsicons::bs_icon(name = "card-list"),
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        numericInput(
          "age",
          "Age",
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
        )
      ),
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
            "Probability of Positive Result vs. CK-MB"
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

