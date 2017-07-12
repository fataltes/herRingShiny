library(shiny)
library(ggplot2)

rm(list=ls())
allres<-readRDS("allres.rds")
dispNames<-read.csv('AllResNameDecoder.csv')

source('global.R', local=TRUE)
source('specificCR.R', local=TRUE)
source('crCategory.R', local=TRUE)


shinyServer(function(input, output, session) {

  xyPanel <- callModule(setXYAxisOptions, "specificCR")
  selectedPanel <- callModule(selectBuilder, "specificCR")
  plotPanel <- callModule(showSpecificCRResult, "specificCR")
  xyPanel2 <- callModule(setXYAxisOptions, "crCategory")
  plotPanel2 <- callModule(showCRCategoryResult, "crCategory")
  
})
