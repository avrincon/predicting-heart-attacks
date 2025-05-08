#' About Module UI
#'
#' @param id The module ID
aboutUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = paste0("max-width: 800px; margin: 0 auto;"),
      card(
        card_header("About This App"),
        includeMarkdown("inst/app/www/about.md")
      )
    )
  )
}


aboutApp <- function() {
  ui <- page_fluid(
    title = "About Heart Attack Risk Prediction Model",
    aboutUI("about")
  )
  
  server <- function(input, output, session) {
    aboutServer("about")
  }
  
  shinyApp(ui, server)
}