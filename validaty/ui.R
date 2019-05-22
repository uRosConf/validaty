library(shiny)
library(data.table)
library(validate)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    useShinyjs(),
    title = "Validaty",
    tabPanel("Data Input",
      sidebarLayout(
        sidebarPanel(
          shiny::fileInput("datafile","CSV file"),
          shiny::selectInput("key", "Select key variable", "no key")
        ),
        mainPanel(
          shiny::dataTableOutput("datatable")
        )
      )
    ),
    tabPanel("Rule Management",
      sidebarLayout(
        sidebarPanel(
          fileInput("rulefile","Free text/YAML")
        ),
        mainPanel(
          tabsetPanel(id = "ts_rules",
            tabPanel("View Rules", value = "tp_view",
              shiny::dataTableOutput("rules"),
              # Download Rules Button
              downloadButton("my_rules", "Download Rules")
            ),
            tabPanel("Modify rules",
            h3("Update, Add or Delete rules"),
            
            div(id = "modify_placeholder", 
              h3("Please upload some rules first")
            ),
            div(id = "modify_content", 
              shiny::radioButtons(
                inputId = "modify_what",
                label = "What do you want to do?",
                choices = c(
                  "Add a rule" = "add", 
                  "Remove a rule" = "remove", 
                  "Edit a rule" = "edit"),
                inline = TRUE),
              hidden(shiny::selectInput(
                inputId = "modify_rulename",
                label = "select a rule",
                selected = NULL,
                choices = c())),
              shiny::textInput(
                inputId = "modify_editrule",
                label = "add/edit rule",
                value = ""),
              shiny::actionButton(inputId = "btn_rule_modify", label = "Add the rule")
            )),
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
          )
        )
      )
    ),
    tabPanel("Confrontation",
      sidebarLayout(
        sidebarPanel(
          shiny::numericInput(
            inputId = "lin.eq.eps",
            label = "Tolerance for equality",
            value = 0,
            min = 0,
            max = Inf
          ),
          #submitButton("Go!")
          actionButton("btn_confront", "Go!")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("summary", 
              shiny::dataTableOutput("resultset")
            ),
            tabPanel("plot",
              shiny::plotOutput("confrontationplot"),
              # Download Report Button
              downloadButton("report", "Download Report")
            ),
            tabPanel("details",
              shiny::dataTableOutput("detailset"),
              # Download details Button
              downloadButton("detailed_results", "Download Detailed Results")
            )
          )
        )
      )
    ),
    tabPanel("Error localization",
      tabsetPanel(
        tabPanel("Summary", 
          fluidRow(
            column(width = 6,
              tags$h2("Summary by variable"),
              shiny::dataTableOutput("el_var")
            ),
            column(width = 6,
              tags$h2("Summary by record"),
              shiny::dataTableOutput("el_rec")
            )
          )
        ),
        tabPanel("Detailed",
          shiny::dataTableOutput("el_rows")
        )
      )
    )
  )
)
