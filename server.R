library(shiny)
rm(list=ls())
allres<-readRDS("allres.rds")
dispNames<-read.csv('AllResNameDecoder.csv')
uniqueCR = unique(allres$CR)
uniquePropc = unique(allres$Propc)
uniqueFcapprop = unique(allres$Fcapprop)
uniqueFracBmsyThreshLo = unique(allres$FracBmsyThreshLo)
uniqueFracBmsyThreshHi = unique(allres[c('FracBmsyThreshHi', 'FracBmsyThreshLo')])
uniqueFracFtarg = unique(allres[c('FracBmsyThreshHi', 'FracBmsyThreshLo', 'FracFtarg')])

selected <- dispNames[dispNames$Type == 'result', ];
axisOption <- selected$OutName;
axisDisplayName <- selected$DisplayName;
xyChoices = setNames(as.character(axisOption), axisDisplayName);




source('ui_helper.R', local=TRUE)


shinyServer(function(input, output, session) {

  xyPanel <- callModule(setXYAxisOptions, "specificCR")
  selectedPanel <- callModule(selectBuilder, "specificCR")
  plotPanel <- callModule(showResults, "specificCR")
  
})
