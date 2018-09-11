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
library(validatetools)
library(data.table)
library(validate)


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
  
  output$rules <- renderDataTable({
     as.data.frame(RuleSet())[c("name","label","rule")]
  })
  
  
  # observe({
  #   updateSelectizeInput(session, "selected_rule"
  #             , choices=c("Add new",names(RuleSet())),
  #             server = TRUE)
  # })
  # observe({
  #   print(input$selected_rule)
  # })
   
  
  # Download rules as yaml file
  output$my_rules <- downloadHandler(
  
    filename = "my_rules.yaml",
    content = function(file) {
      validate::export_yaml(RuleSet(), file)
    }
  )

  ## Variable coverage ----
  
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

  ## Rule inspection ----
  observe({
    rls <- RuleSet()
    fixed <- validatetools::detect_fixed_variables(rls)
    fixed <- if (is.null(fixed)){
      data.frame(variable=character(0), value=numeric(0))
    } else {
      data.frame(variable=names(fixed), value=sapply(fixed, identity))
    }
    
    num_bdr <- validatetools::detect_boundary_num(rls)
    cat_bdr <- validatetools::detect_boundary_cat(rls)
    
    
    
    output$cat_bdr <- renderDataTable(cat_bdr)
    output$num_bdr <- renderDataTable(num_bdr)
    output$fixed_variables <- renderDataTable(fixed)
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
  
  output$detailset <- renderDataTable(as.data.frame(ResultSet()))
  
  # Download details as csv
  output$detailed_results <- downloadHandler(
    filename = "detailed_results.csv"
    ,
    content = function(file) {
      write.csv(as.data.frame(ResultSet()), file, row.names = FALSE)
    }
  )
  
  
  # Download plot in a pdf file 
  output$report = downloadHandler(
    filename = "report.pdf",
    
    content = function(file) {
      pdf(file) # open the pdf device
      plot(ResultSet(),main="Results by rule")
      dev.off()
      
    }
  )
  
  
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
  
  
  output$el_rows <- renderDataTable(
     subset(as.data.frame(error_locs()), apply(values(error_locs()),1,any))
  )
}) 
  
