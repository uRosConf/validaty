#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  ## reading data file ----
  DataFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$datafile, message = FALSE))
    input$datafile
  })
  
  # The user's data, parsed into a data frame
  DataSet <- reactive({
    fread(input=DataFile()$datapath)
  })
  
  output$datatable <- renderDataTable({
    DataSet()
  })
  
  observe({
    data <- DataSet()
    updateSelectInput(session, inputId = "key", choices = c("no key", names(DataSet())))
  })
  
  
  ## Reading rule file ----
  
  RuleFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$rulefile, message = FALSE))
    input$rulefile
  })
  
  # The user's data, parsed into a data frame
  RuleSet <- reactive({
    validate::validator(.file=RuleFile()$datapath)
  })
  
  output$RuleSet <- RuleSet
  output$rules <- renderDataTable({
     as.data.frame(RuleSet())[c("name","label","rule")]
  })
  
  
  observe({
    updateSelectInput(session, "selected_rule"
              , choices=c("Add new",names(RuleSet())) )
  })
  
  
  
  # Download rules as yaml file
  output$my_rules <- downloadHandler(
  
    filename = "my_rules.yaml",
    content = function(file) {
      validate::export_yaml(RuleSet(), file)
    }
  )

  
  
  
  
  
  
 
  
  output$ruleplot <- renderPlot({
    plot(RuleSet())
  })
  
  
  output$variablesCovered <- renderText({
    vrs <- paste(variables(RuleSet()),collapse=", ")
    HTML("<b>Variables covered: </b>", vrs)  
    })
  
  output$variablesNotCovered <- renderText({
    validate(need(DataSet(),"No data loaded"))
    vrs <- names(DataSet())
    vrs <- vrs[!vrs %in% variables(RuleSet())]
    HTML("<b>Variables not covered: </b> ",paste(vrs, collapse=", "))
  })

  ## Confrontation ----
  observe({
    updateNumericInput(session
            , inputId = "lin.eq.eps"
            , value = voptions(RuleSet(), "lin.eq.eps"))
  })
  
  ResultSet <- reactive({
    confront(DataSet()
       , RuleSet()
       , key = ifelse(input$key == "no key", NA_character_, input$key)
       , lin.eq.eps=input$lin.eq.eps)
  })
  
  output$resultset <- renderDataTable(summary(ResultSet()))
  output$confrontationplot <- renderPlot(plot(ResultSet(),main="Results by rule"))
})
