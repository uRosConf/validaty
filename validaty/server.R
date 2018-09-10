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
shinyServer(function(input, output) {
  
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
  
  output$rules <- renderDataTable({
     as.data.frame(RuleSet())[c("name","label","rule")]
  })
  
  ## Confrontation ----
  ResultSet <- reactive({
    confront(DataSet(), RuleSet())
  })
  
  output$resultset <- renderDataTable(summary(ResultSet()))
  output$confrontationplot <- renderPlot(plot(ResultSet()))
})
