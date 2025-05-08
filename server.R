server <- function(input, output, session) {
  modelPredictionServer("mod_pred")
  modelDiagnosticsServer("mod_diag")
  dataExplorerServer("data_explorer", model_data)
  # bs_themer()
}