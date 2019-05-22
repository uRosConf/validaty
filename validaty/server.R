library(shiny)
library(errorlocate)
library(validatetools)
library(data.table)
library(validate)
library(shinyjs)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  RulesAvailable <- reactiveVal(FALSE)
  DataAvailable <- reactiveVal(FALSE)
  RulesDF <- reactiveVal(NULL) # rules as a data.frame
  RuleSet <- reactiveVal(NULL) # set of rules
  
  # reading data file ----
  DataFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$datafile, message = FALSE))
    input$datafile
  })
  
  # The user's data, parsed into a data frame
  DataSet <- reactive({
    DataAvailable(TRUE)
    ff <- fread(input = DataFile()$datapath)
    ff
  })
  
  output$datatable <- shiny::renderDataTable({
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
  observe({
    rr <- validate::validator(.file = RuleFile()$datapath)
    RuleSet(rr)
    RulesDF(as.data.frame(rr))
  })
  
  ## show modify rules ui
  observe({
    if (RulesAvailable() & DataAvailable()) {
      shinyjs::show("modify_content")
      shinyjs::hide("modify_placeholder")
      
    } else {
      shinyjs::hide("modify_content")
      shinyjs::show("modify_placeholder")
    }
  })
  
  output$rules <- shiny::renderDataTable({
    RulesDF()[, c("name", "label", "rule")]
  })

  observe({
    df <- RulesDF()
    nn <- df$name
    if (input$modify_what != "add") {
      shiny::updateSelectInput(
        session = session, 
        inputId = "modify_rulename", 
        choices = nn)
    }
    RulesAvailable(TRUE)
  })  
  
  observeEvent(input$modify_what, {
    sel <- input$modify_what
    if (sel == "add") {
      shinyjs::hide(id = "modify_rulename")
      shiny::updateTextInput(
        session = session,
        inputId = "modify_editrule",
        value = "")
      btn_txt <- "Add the rule"
    } else {
      shinyjs::show(id = "modify_rulename")
      if (sel == "remove") {
        btn_txt <- "Remove the selected rule"
      } else {
        btn_txt <- "Edit the selected rule"
      }
    }
    
    if (sel == "remove") {
      shinyjs::hide(id = "modify_editrule")
    } else {
      shinyjs::show(id = "modify_editrule")
    }
    
    # updating btn label
    shiny::updateActionButton(
      session = session,
      inputId = "btn_rule_modify",
      label = btn_txt)
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$modify_rulename, {
    rule <- input$modify_rulename
    df <- RulesDF()
    expr <- df[df$name == input$modify_rulename, "rule"]
    updateTextInput(session, inputId = "modify_editrule", value = expr)
  }, ignoreInit = TRUE, priority = 10)  

  # delete a rule
  observeEvent(input$btn_rule_modify, {
    tmpf <- tempfile()
    what <- input$modify_what
    if (what == "add") {
      # write current rule to file
      cat(input$modify_editrule, "\n", file = tmpf)
      dat <- DataSet()
      res <- confront(dat = dat[1, ], validator(.file = tmpf))
      
      if (length(errors(res)) == 0) {
        # read all rules, add the new one, write to file and update everything
        rr <- as.character(RulesDF()$rule)
        rr <- c(rr, input$modify_editrule)
        cat(rr, "\n", sep = "\n", file = tmpf)
        val <- validator(.file = tmpf)
        df <- as.data.frame(val)
      } else {
        message("your rule was not valid --> Todo: show warning")
        shiny::updateTextInput(
          session = session, inputId = "modify_editrule", value = ""
        )
      }
    } else if (what == "edit") {
      message("editing a rule")
      
      # check current new rule
      cat(input$modify_editrule, "\n", file = tmpf)
      dat <- DataSet()
      res <- confront(dat = dat[1, ], validator(.file = tmpf))
      
      if (length(errors(res)) == 0) {
        rr <- RulesDF()
        ii <- rr$name == input$modify_rulename
        rr <- as.character(rr$rule)
        rr[ii] <- input$modify_editrule
        cat(rr, "\n", sep = "\n", file = tmpf)
        val <- validator(.file = tmpf)
        df <- as.data.frame(val)
      } else {
        df <- RulesDF()
        val <- RuleSet()
      }
    } else if (what == "remove") {
      rr <- RulesDF()
      rr <- rr[rr$name != input$modify_rulename, ]
      rr <- as.character(rr$rule)
      if (length(rr) > 0) {
        cat(rr, "\n", sep = "\n", file = tmpf)
        val <- validator(.file = tmpf)
        df <- as.data.frame(val)
      } else {
        # no rules left
        val <- df <- NULL
      }
    }
    # updating inputs    
    RuleSet(val)
    RulesDF(df)
    
    if (file.exists(tmpf)) {
      file.remove(tmpf)
    }
    # switching tabs
    updateTabsetPanel(
      session = session, 
      inputId = "ts_rules",
      selected = "tp_view"
    )   
  })
  
  
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
    vrs <- paste(variables(RuleSet()), collapse = ", ")
    HTML("<b>Variables covered: </b>", vrs)
  })

  output$variablesNotCovered <- renderText({
    validate(need(DataSet(), "No data loaded"))
    vrs <- names(DataSet())
    vrs <- vrs[!vrs %in% variables(RuleSet())]
    HTML("<b>Variables not covered: </b> ", paste(vrs, collapse = ", "))
  })

  ## Rule inspection ----
  observe({
    rls <- RuleSet()
    if (!is.null(rls)) {
      fixed <- tryCatch(
        validatetools::detect_fixed_variables(rls),
        error = function(e) e
      )
      if (!inherits(fixed, "error")) {
        fixed <- if (is.null(fixed)) {
          data.frame(
            variable = character(0), 
            value = numeric(0)
          )
        } else {
          data.frame(
            variable = names(fixed),
            value = sapply(fixed, identity))
        }
        
        num_bdr <- validatetools::detect_boundary_num(rls)
        cat_bdr <- validatetools::detect_boundary_cat(rls)
        
        output$cat_bdr <- shiny::renderDataTable(cat_bdr)
        output$num_bdr <- shiny::renderDataTable(num_bdr)
        output$fixed_variables <- shiny::renderDataTable(fixed)
        
      }
    }
  })
  # 
  # ## Confrontation ----
  observe({
    shiny::updateNumericInput(
      session = session,
      inputId = "lin.eq.eps",
      value = voptions(RuleSet(), "lin.eq.eps")
    )
  })
  
  ResultSet <- reactiveVal(NULL)
  
  observeEvent(input$btn_confront, {
    res <- validate::confront(
      dat = DataSet(),
      x = RuleSet(),
      key = ifelse(input$key == "no key", NA_character_, input$key),
      lin.eq.eps = input$lin.eq.eps
    )
    ResultSet(res)
  })
  # 
  # ResultSet <- reactive({
  #   confront(
  #     DataSet(),
  #     RuleSet(),
  #     key = ifelse(input$key == "no key", NA_character_, input$key),
  #     lin.eq.eps = input$lin.eq.eps
  #   )
  # })
  # 
  output$resultset <- shiny::renderDataTable(summary(ResultSet()))
  output$confrontationplot <-
    renderPlot(plot(ResultSet(), main = "Results by rule"))

  output$detailset <- shiny::renderDataTable(as.data.frame(ResultSet()))

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
      plot(ResultSet(), main = "Results by rule")
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
    updateSelectInput(session, inputId = "rules", choices = erronous_rules)
  })

  error_ind <- reactive({
    which(values(error_locs()), arr.ind = TRUE)
  })

  val_values <- reactive({
    values(ResultSet())
  })

  output$el_rows <- renderDataTable(
    subset(as.data.frame(error_locs()), apply(values(error_locs(
  )), 1, any)))
}) 
  
