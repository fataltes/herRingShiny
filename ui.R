library(shiny)


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
    #conditionalPanel(condition = "input.crType == 'BB'",
    #                 selectInput(ns("alaki"), label = h3("choose Control Rule"), 
    #                             choices = list("CC", "CCC", "BB", "BB3yr", "BB5yr", "BB3yrPerc"), selected = "CC")
                     
                     #uiOutput(ns("low"))
    #                 )#,
    uiOutput(ns("hi"))
    #conditionalPanel(condition = "input.crType == 'BB'",
    #                 selectInput(ns("FracFtarg"), dispName('FracFtarg'),
    #                             choices = uniqueFracFtarg[uniqueFracFtarg$FracBmsyThreshHi == input$FracBmsyThreshHi &
    #                                                         uniqueFracFtarg$FracBmsyThreshLo == input$FracBmsyThreshLo  
    #                                                       ,'FracFtarg']))
                     
  )
}

drawPlotOutput <- function(id) {
  ns <- NS(id)
  plotOutput(ns("distPlot"))
}
# UI for yield-MedianSSB that draws a line
shinyUI(fluidPage(

  # Application title
  titlePanel("Herring Interactive Visualization"),

  
  sidebarLayout(
    sidebarPanel(
      selectBuilderUI("crOptions")
      ),

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Specific Rule", drawPlotOutput("crOptions")), 
          tabPanel("A Category of Rules", verbatimTextOutput("crType"))
        )
      )
  )
))
