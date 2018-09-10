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
                 csvFileInput("datafile", "User data (.csv format)")),
               mainPanel(
                 dataTableOutput("table")))
    ),
    tabPanel("Rule Input",
             sidebarLayout(
               sidebarPanel(
                 ruleFileInput("rulefile", "Free text or YAML format")),
               mainPanel(
                 shiny::dataTableOutput("rules")))),
    tabPanel("Rule Investigation",
             "some rule investigation"),
    tabPanel("Aggregate Results",
             "some quick overview"),
    tabPanel("Detailed Results",
             "an in-depth result view")
    
  ))
