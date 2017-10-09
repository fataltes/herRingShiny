library(shiny)

setXYAxisOptionsUI <- function(id) {
  ns <- NS(id)
  tagList(
             uiOutput(ns("selectX")),
             uiOutput(ns("selectY"))
    )
}

setCRUI <- function(id) {
  ns <- NS(id)
  tagList(
         selectInput(ns("crType"), label = h3("choose Control Rule"), 
                     choices = list("CC", "CCC", "BB", "BB3yr", "BB5yr", "BB3yrPerc"), selected = "CC")
  )
}

setFacetUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("split"), label = h3("choose Split Facet"), 
              choices = list("none", "bias", "steep", "bias_steep"), selected = "none")
  )
}

selectBuilderUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("crParams")),
    uiOutput(ns("low")),
    uiOutput(ns("hi"))
  )
}

showSpecificCRResultOutput <- function(id) {
  ns <- NS(id)
  fluidPage(
    plotOutput(ns("distPlot")),
    tableOutput(ns("view"))
  )
}

showCRCategoryResultOutput <- function(id) {
  ns <- NS(id)
  fluidPage(
    #plotlyOutput(ns("distPlot"))
    tags$head(tags$style('
     #my_tooltip {
                         position: absolute;
                         width: 300px;
                         z-index: 100;
                         padding: 0;
}
')),

  
    plotOutput(ns("distPlot"), hover = hoverOpts(ns("plot_hover"))),
    verbatimTextOutput(ns("my_tooltip"))
    #, placeholder = TRUE
  )
}

shinyUI(fluidPage(

  titlePanel("Herring Interactive Visualization"),

  
  tabsetPanel(
    tabPanel("Specific Rule", 
           sidebarLayout(
               sidebarPanel(
                   setXYAxisOptionsUI("specificCR"),
                   setCRUI("specificCR"),
                   selectBuilderUI("specificCR")
               ),
              mainPanel(
                 showSpecificCRResultOutput("specificCR")
              )
           )
    ),
    tabPanel("A Category of Rules", 
             sidebarLayout(
               sidebarPanel(
                 setXYAxisOptionsUI("crCategory"),
                 setCRUI("crCategory"),
                 setFacetUI("crCategory")
             ),
               mainPanel(
                 showCRCategoryResultOutput("crCategory")
               )
             )
    )
  )
))
