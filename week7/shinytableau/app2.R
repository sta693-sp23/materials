# filetype: shinyApp

library(shiny)
library(shinytableau)
library(shinyvalidate)

manifest = tableau_manifest_from_yaml()

config_ui = function(req) {
  tagList(
    textInput("greetee", "Whom would you like to greet?", "Jane Doe"),
    textInput("msg2", "What else would you like to say?", "")
  )
}

config_server = function(input, output, session, iv) {
  # Ensure that the user provides a value for input$greetee
  iv$add_rule("greetee", sv_required())
  
  # config_server must have a save_settings function
  save_settings = function() {
    update_tableau_settings_async(
      greetee = input$greetee,
      msg2 = input$msg2
    )
  }
  
  # config_server must always return the save_settings function
  return(save_settings)
}

ui = function(req) {
  fillPage(
    theme = shinytableau_theme(), 
    padding = 12,
    textOutput("message", container = h2)
  )
}

server = function(input, output, session) {
  output$message = renderText({
    paste0("Hello, ", tableau_setting("greetee"), "!", "<br/><br/>", tableau_setting("msg2"))
  })
}

tableau_extension(manifest, ui, server, config_ui, config_server)
