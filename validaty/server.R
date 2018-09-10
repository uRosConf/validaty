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
   
  dataset <- callModule(csvFile, "datafile")
 
  output$table <- renderDataTable({
    dataset()
  })

  ruleset <- callModule(ruleFile, "rulefile") 
  output$rules <- renderDataTable({
     as.data.frame(ruleset())[c("name","label","rule")]
  })
})
