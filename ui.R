library(shiny)

setXYAxisOptionsUI <- function(id) {
  ns <- NS(id)
  tagList(
             uiOutput(ns("selectX")),
             uiOutput(ns("selectY"))
    )
}

selectBuilderUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             selectInput(ns("crType"), label = h3("choose Control Rule"), 
                         choices = list("CC", "CCC", "BB", "BB3yr", "BB5yr", "BB3yrPerc"), selected = "CC")
      )
    ),
    uiOutput(ns("crParams")),
    uiOutput(ns("low")),
    uiOutput(ns("hi"))
  )
}

showResultsOutput <- function(id) {
  ns <- NS(id)
  fluidPage(
    plotOutput(ns("distPlot")),
    tableOutput(ns("view"))
  )
}


shinyUI(fluidPage(

  titlePanel("Herring Interactive Visualization"),

  
  sidebarLayout(
    sidebarPanel(
      setXYAxisOptionsUI("specificCR"),
      selectBuilderUI("specificCR")
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Specific Rule", showResultsOutput("specificCR")), 
          tabPanel("A Category of Rules", verbatimTextOutput("crType"))
        )
      )
  )
))
