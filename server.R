library(shiny)
rm(list=ls())
#setwd("/home/fatemeh/summer2017/test-app")
allres<-readRDS("allres.rds")
dispNames<-read.csv('AllResNameDecoder.csv')
uniqueCR = unique(allres$CR)
uniquePropc = unique(allres$Propc)
uniqueFcapprop = unique(allres$Fcapprop)
uniqueFracBmsyThreshLo = unique(allres$FracBmsyThreshLo)
uniqueFracBmsyThreshHi = unique(allres[c('FracBmsyThreshHi', 'FracBmsyThreshLo')])
uniqueFracFtarg = unique(allres[c('FracBmsyThreshHi', 'FracBmsyThreshLo', 'FracFtarg')])

source('ui_helper.R', local=TRUE)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot

  selectedPanel <- callModule(selectBuilder, "crOptions")
  plotPanel <- callModule(drawPlot, "crOptions")
  
})
