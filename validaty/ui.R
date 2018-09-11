#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(validate)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    title = "Validaty",
    
    tabPanel("Data Input",
             sidebarLayout(
               sidebarPanel(
                 shiny::fileInput("datafile","CSV file"),
                 shiny::selectInput("key", "Select key variable",
                                    "no key")),
               mainPanel(
                 dataTableOutput("datatable"))
    )),
    tabPanel("Rule Management",
             sidebarLayout(
               sidebarPanel(
                 fileInput("rulefile","Free text/YAML")),
               mainPanel(
                 tabsetPanel(
                   tabPanel("View Rules",
                            shiny::dataTableOutput("rules")),
                   tabPanel("Coverage",
                            shiny::plotOutput("ruleplot"),
                            shiny::htmlOutput("variablesCovered"),
                            shiny::htmlOutput("variablesNotCovered")
                            ),
                   tabPanel("Consistency",
                            "Rule")
               )))
                 ),
    tabPanel("Confrontation",
             sidebarLayout(
               sidebarPanel(
                 shiny::numericInput("lin.eq.eps"
                          , label="Tolerance for equality"
                          , value = 0, min=0, max=Inf),
                 submitButton("Go!")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("summary",shiny::dataTableOutput("resultset"))
                   , tabPanel("plot",shiny::plotOutput("confrontationplot"))
                 )
               )
             )),
    tabPanel("Error localization",
             "Input needed here")
  ))
