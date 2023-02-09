####################################
##
##       User Interface for 
##            KS:Stature
##
####################################

# Libraries
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinyMatrix)
library(markdown)

# Imports
source("R/short_blurb.R")

# Matrix for measurement input
meas_mat <- matrix(NA,4,6)
rownames(meas_mat) <- c("Length","Proximal","Midshaft","Distal")
colnames(meas_mat) <- c("Femur","Tibia","Fibula","Humerus","Radius","Ulna")

# Start of UI
fluidPage(theme="make-me-pretty.css",
  useShinyjs(),
  h1(id="big-heading", "KidStats: Stature"),
  sidebarLayout(
    sidebarPanel(id="sidebar",
      h4(tags$b("Case Information:")),
      textInput("analyst",label=h6(tags$b("Analyst (Required)")),
                placeholder="XXXX"),
      textInput("caseid",label=h6(tags$b("Case ID (Required)")),
                placeholder="XXXX"),
      h4(tags$b("Reference Demographics:")),
      radioButtons("sex",label=h6(tags$b("Sex:")),
                   choices=list(
                     "Pooled"="pooled",
                     "Male"="M",
                     "Female"="F"
                   ), selected="pooled"),
      selectInput("dem",label=h6(tags$b("Geographic Region:")),
                  choices=list(
                    "United States"="US"
                  ), selected="US"),
      actionButton("reset_side","Reset Info",icon("trash")),
      hr(),
      tags$b("Citation: "),"Chu EY & Stull KE. (2023). KidStats: Stature - A graphical 
      user interface for subadult stature estimation. Version ",gui_version(),
      br(), br(),
      tags$b("Contact: "),"Elaine Y. Chu, Ph.D (",a(href="echu14@jh.edu","echu14@jh.edu"),")",
      br(), br(),
      tags$b("Last Updated: February 2023")
    ),  # close sidebarPanel
    mainPanel(
      blurb,
      hr(),
      bsCollapse(id="main",multiple=T,open="Measurements",
                 bsCollapsePanel(title="Measurements",
                                 matrixInput("mmat",
                                             value=meas_mat,
                                             cols=list(names=T),
                                             rows=list(names=T),
                                             class="numeric"),
                                 div(column(4,offset=1, 
                                            actionButton("reset_main","Reset Measurements",icon("trash"))),
                                     column(3),
                                     column(4, disabled(actionButton("run","Run Analysis",
                                                                     icon("arrow-right"))))),
                                 div(column(12, align="center",
                                            shinyjs::hidden(br(),h3("Generating results, please wait...", id="wait"))))
                                 ),
                 bsCollapsePanel(title="Output",
                                 # tableOutput("mdf")  # FOR DEBUG
                                 fluidRow(
                                   column(7,
                                            wellPanel(
                                              h4("Case Info"),
                                              textOutput("report_analyst"),
                                              textOutput("report_case"),
                                              textOutput("report_date"),
                                              textOutput("report_version"),
                                              tableOutput("ref_specs"),
                                              id="caseinfo"
                                            )
                                   ),
                                   column(2,
                                          downloadButton("dwn_report",
                                                         "Download Report",
                                                         style="background-color:#FFFFFFF")
                                   )
                                 ),
                                 hr(),
                                 tableOutput("pred_tbl"),
                                 hr(),
                                 # plot with prediction against reference data
                                 fluidRow(
                                   column(3,
                                          selectInput("pick_var",
                                                      "Select Variable to Plot:",
                                                      choices="None"),
                                          selectInput("pick_type",
                                                      "Select Model Type to Plot:",
                                                      choices=list(
                                                        "Nonlinear"="nonlinear",
                                                        "Linear"="linear"
                                                      ),selected="nonlinear")
                                          ),
                                   column(9,
                                          plotOutput("pred_plot")
                                          )
                                 )  # close fluidRow
                        )  # close bsCollapsePanel-output
                 )  # close bsCollapse
    )  # close mainPanel
  )  # close sidebarLayout
)  # close fluidPage