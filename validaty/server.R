#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(errorlocate)

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
  
  ## Error localization ----
  error_locs <- reactive({
    locate_errors(DataSet(), RuleSet())
  })
  el_summary <- reactive({
    summary(error_locs())
  })
  el_rules <- reactive({
    el_summary()[[1]][el_summary()[[1]]$errors > 0, "name"]
  })
  
  output$el_var  <- renderDataTable(el_summary()[[1]])
  output$el_rec  <- renderDataTable(as.data.table(el_summary()[[2]]))
  
  observe({
    erronous_rules <- el_rules()
    updateSelectInput(session, inputId = "rules",
                      choices = erronous_rules)
  })
  
  error_ind <- reactive({
    which(values(error_locs()), arr.ind = TRUE)
  })
  
  val_values <- reactive({
    values(ResultSet())
  })
  
  
  tab <- reactive({
    err_ind <- error_ind()
    DT <- DT::datatable(DataSet())#[error_ind()[, 1], ])#error_ind()[, 2], with = FALSE])
    DT <- DT::datatable(DataSet()[error_ind()[, 1], unique(error_ind()[, 2]), with = FALSE])
    # for (col in error_ind()[, 2]) {
    #   DT <- formatStyle(DT,
    #                     columns = col,
    #                     target = "cell",
    #                     backgroundColor = "red")
    # }
    return(DT)
  })
  
  
  output$el_rows <- renderDataTable(
    tab()
  )
}
) 
  
  
  
  
  # output$el_rows <- ifelse(input$rules == "",
  #                          renderDataTable(DataSet()[error_ind()[, 1], ]),
  #                          renderDataTable(DataSet()[val_values()[, "V2"] == FALSE]))
  
  

