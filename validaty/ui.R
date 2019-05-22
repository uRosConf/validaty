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
                            shiny::dataTableOutput("rules"),
                            # Download Rules Button
                            downloadButton("my_rules", "Download Rules")
                            ),
                   
                   tabPanel("Modify rules",
                      h3("Modify "),
                      shiny::radioButtons(inputId = "modify_what",
                                          label = "What do you want to do?",
                                          choices = c("add", "remove", "edit"),
                                          inline = TRUE),
                      shiny::selectizeInput(inputId = "modifyrulename",
                                         label = "select a rule",
                                         selected = NULL,
                                         choices = c()),
                      shiny::textInput(inputId = "modify_editrule",
                                       label = "add/edit rule",
                                       value = "")
                      
                      
                   #     shiny::selectInput("selected_rule","Select rule"
                   #                        , choices=c("Add new","B"),selected = "B"),
                   #     shiny::textAreaInput("rule_exp","Expression"),
                   #     shiny::textInput("rule_name","Name"),
                   #     shiny::textInput("rule_label","Lalel"),
                   #     shiny::textInput("rule_description","Description")
                   #     # shiny::actionButton("rule_update","Update")
                        ),
                   tabPanel("Coverage",
                            shiny::plotOutput("ruleplot"),
                            shiny::htmlOutput("variablesCovered"),
                            shiny::htmlOutput("variablesNotCovered")
                            ),
                   tabPanel("Imposed limits",
                            tags$h3("Boundaries"),
                            tags$em("Limits imposed by the rule set"),
                            shiny::dataTableOutput("num_bdr"),
                            shiny::dataTableOutput("cat_bdr"),
                            tags$h3("Fixed values"),
                            tags$em("Variables which can only take one value under the current rule set"),
                            shiny::dataTableOutput("fixed_variables")
                            )
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
                   , tabPanel("plot",shiny::plotOutput("confrontationplot"),
                              # Download Report Button
                              downloadButton("report", "Download Report"))
                   , tabPanel("details", shiny::dataTableOutput("detailset"), 
                              # Download details Button
                              downloadButton("detailed_results", "Download Detailed Results"))
                              )
                 )
               )
             ),
    tabPanel("Error localization",
             # sidebarLayout(
               # sidebarPanel(
                 # selectInput("rules", "Select rules for inspection",
                 #             choices = "Confront first",
                 #             multiple = TRUE)
                 
               # ),
               # mainPanel(
                 tabsetPanel(
                   tabPanel("Summary", 
                            fluidRow(
                              column(width = 6,
                              tags$h2("Summary by variable"),
                              dataTableOutput("el_var")),
                              # hr(),
                              column(width = 6,
                                     tags$h2("Summary by record"),
                              dataTableOutput("el_rec")))),
                   tabPanel("Detailed",
                            
                              dataTableOutput("el_rows")#,
                              
                   )
                   # )
                 )
    ))
)
