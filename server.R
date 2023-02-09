####################################
##
##       Server for 
##        KS:Stature
##
####################################

# load libraries
library(shiny, quietly = TRUE)
library(shinydashboard, quietly = TRUE)
library(tidyverse, quietly = TRUE)
require(rmarkdown, quietly = TRUE)
library(DT, quietly = TRUE)

source("R/helpers.R")

# Matrix for measurement input
meas_mat <- matrix(NA,4,6)
rownames(meas_mat) <- c("Length","Proximal","Midshaft","Distal")
colnames(meas_mat) <- c("Femur","Tibia","Fibula","Humerus","Radius","Ulna")

shinyServer(function(input, output, session){
  mdf <- reactive({
    as.data.frame(input$mmat, rownames=TRUE)
    # print(input$mmat)  # FOR DEBUG
    # print(all(is.na(input$mmat)))  # FOR DEBUG
  })

  # output$mdf <- renderTable(mdf(), rownames=TRUE)  # FOR DEBUG

  # Make run button active if mdf is not NA and analyst and case_id are not NA
  observe({
    if(!input$caseid=="" & !input$analyst=="") {
      shinyjs::toggleState("run", (!all(is.na(mdf()))))
    }
  })

  #Reset Buttons
  observeEvent(input$reset_main, {
    # shinyjs::reset("mainbar")
    updateMatrixInput(session, "mmat", meas_mat)
  })

  observeEvent(input$reset_side, {
    shinyjs::reset("sidebar")
  })

  ############### RUN ANALYSES ##################
  case_data <- reactive({
    investigate_data(mdf())
  })
  
  stat_pred <- eventReactive(input$run, {
    updateCollapse(session, id="main", open="Output")  # open output
    show("wait")  # show generating report text
    predict_stature(case_data(), input$sex)
  })
  
  observeEvent(stat_pred(),{
    shinyjs::hide("wait")
    shinyjs::show("caseinfo")
    # print(stat_pred()$output$`Variable(s)`)  # FOR DEBUG
    opt_var <- recode_name(stat_pred()$output$`Variable(s)`, "forward")
    # print(opt_var)  # FOR DEBUG
    updateSelectInput(session,"pick_var",
                      choices=c(opt_var))
  })
  

  ############### PRINT ##################
  ## This section prepares the bits and pieces from the UI and the model for output ##

  output$report_case <- renderText({
    paste("Case ID:", input$caseid)
  })

  output$report_analyst <- renderText({
    paste("Analyst:", input$analyst)
  })

  output$report_date <- renderText({
    paste(Sys.Date())
  })

  output$report_version <- renderText({
    paste("KidStats: Stature Version:", gui_version())
  })

  output$ref_specs <- renderTable({
    data.frame("Sex"=str_to_title(input$sex),"Reference"=input$dem)
  })
  
  output$pred_tbl <- renderTable({
    pred_tbl <- stat_pred()$output
    pred_tbl$`Model Type` <- str_to_title(pred_tbl$`Model Type`)
    pred_tbl$`Variable(s)` <- recode_name(pred_tbl$`Variable(s)`, "forward")
    # print(pred_tbl)  # FOR DEBUG
    pred_tbl
  })
  
  observeEvent(input$pick_var, {
    out_df <- stat_pred()$output
    out_df$`Variable(s)` <- recode_name(out_df$`Variable(s)`, "forward")
    rows <- out_df[out_df$`Variable(s)`==input$pick_var,]
    opts <- stringr::str_to_title(rows$`Model Type`)
    # print(opts)  # FOR DEBUG
    
    updateSelectInput(session, "pick_type", 
                      choices=opts)
  })
  
  out_plot <- reactive({
    # print(stat_pred()$output)  # FOR DEBUG
    # print(case_data())  # FOR DEBUG
    # print(input$pick_var)  # FOR DEBUG
    # print(input$pick_type) # FOR DEBUG
    
    generate_pred_plot(case_data(), stat_pred()$output,
                       input$pick_var, input$pick_type,
                       input$sex)
  })
  
  
  output$pred_plot <- renderPlot({
    out_plot()
  })


  # Report Download Handler
  output$dwn_report <- downloadHandler(
    filename = function(case = input$caseid) {
      name <- paste0("KS-Stature-Report_", case, ".pdf")
      return(name)
    },

    content = function(file) {
      src <- 'www/md/ks-stature_report_template.Rmd'
      file.copy(src, 'ks-stature_report.Rmd', overwrite = TRUE)

      out <- rmarkdown::render('ks-stature_report.Rmd', rmarkdown::pdf_document()
      )
      file.rename(out, file)
    }
  )
  
})