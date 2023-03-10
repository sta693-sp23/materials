# filetype: shinyApp

library(shiny)
library(shinytableau)

manifest = tableau_manifest_from_yaml()

ui = function(req) {
  fillPage(
    theme = shinytableau_theme(), 
    padding = 12,
    h2("Hello, world!")
  )
}

server = function(input, output, session) {
}

tableau_extension(manifest, ui, server)

