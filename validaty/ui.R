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


# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    title = "Validaty",
    
    tabPanel("Data Input",
             sidebarLayout(
               sidebarPanel(
                 shiny::fileInput("datafile","CSV file"),
                 shiny::selectInput("key", "Select key variable",
                                    c("a","b","c"))),
               mainPanel(
                 dataTableOutput("datatable"))
    )),
    tabPanel("Rule Input",
             sidebarLayout(
               sidebarPanel(
                 fileInput("rulefile","Free text/YAML")),
               mainPanel(
                 shiny::dataTableOutput("rules")))),
    tabPanel("Confrontation",
             sidebarLayout(
               sidebarPanel(
                 submitButton("Go!")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("summary",shiny::dataTableOutput("resultset"))
                   , tabPanel("plot",shiny::plotOutput("confrontationplot"))
                 )
               )
             )),
    tabPanel("Rule Investigation",
             "some rule investigation"),
    tabPanel("Aggregate Results",
             "some quick overview"),
    tabPanel("Detailed Results",
             "an in-depth result view")
    
  ))
