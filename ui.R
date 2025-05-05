ui <- page_navbar(
  title = "Predicting the Risk of Heart Attack",
  theme = bs_theme(version = 5, bootswatch = "litera"),
  nav_panel(
    title = "Model Predictions",
    value = "pred_tab",
    icon = bsicons::bs_icon(name = "heart-pulse"),
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        modelPredictionInput("mod_pred")
      ),
      
      modelPredictionOutput("mod_pred")
      
    )
  )
)

