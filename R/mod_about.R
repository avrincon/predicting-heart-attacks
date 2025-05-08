#' About Module UI
#'
#' @param id The module ID
#'
#' @return A UI definition for the About module
#' 
#' @export
aboutUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("About This App"),
      includeMarkdown("inst/app/www/about.md")
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