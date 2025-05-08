ui <- page_navbar(
  header = shinyFeedback::useShinyFeedback(),
  title = "Predicting the Risk of Heart Attack",
  theme = bs_theme(version = 5, bootswatch = "litera"),
  
  nav_panel(
    title = "About",
    value = "about_tab",
    icon = bsicons::bs_icon(name = "info-circle"),
    aboutUI("about")
  ),
  
  nav_panel(
    title = "Data Explorer",
    value = "data_tab",
    icon = bsicons::bs_icon(name = "bar-chart"),
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        dataExplorerInput("data_explorer")
      ),
      dataExplorerOutput("data_explorer")
    )
  ),
  
  nav_panel(
    title = "Model Predictions",
    value = "pred_tab",
    icon = bsicons::bs_icon(name = "graph-up"),
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        modelPredictionInput("mod_pred")
      ),
      modelPredictionOutput("mod_pred")
    )
  ),
  
  nav_panel(
    title = "Model Diagnostics",
    value = "diag_tab",
    icon = bsicons::bs_icon(name = "heart-pulse"),
    modelDiagnosticsOutput("mod_diag")
  )
  
)

