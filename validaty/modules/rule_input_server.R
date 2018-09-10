
ruleFile <- function(input, output, session) {
  # The selected file, if any
  userFile <- 
    reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  ruleset <- reactive({
    validate::validator(.file=userFile()$datapath)
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(ruleset)
}

