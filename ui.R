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
        modelPredictionInput("mod_pred")
      ),
      
      # main panel ----------------------------------------------------------

      modelPredictionOutput("mod_pred")
    )
  )
)

