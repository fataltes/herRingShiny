rm(list=ls())
allres<-readRDS("allres.rds")
dispNames<-read.csv('AllResNameDecoder.csv')

uniqueCR = unique(allres$CR)
uniquePropc = unique(allres$Propc)
uniqueFcapprop = unique(allres$Fcapprop)
uniqueFracBmsyThreshLo = unique(allres$FracBmsyThreshLo)
uniqueFracBmsyThreshHi = unique(allres[c('FracBmsyThreshHi', 'FracBmsyThreshLo')])
uniqueFracFtarg = unique(allres[c('FracBmsyThreshHi', 'FracBmsyThreshLo', 'FracFtarg')])

selected <- dispNames[dispNames$Type == 'result' & (dispNames$SubType == 'result' | dispNames$SubType == 'median'), ];
axisOption <- selected$OutName;
axisDisplayName <- selected$DisplayName;
xyChoices = setNames(as.character(axisOption), axisDisplayName);

v25 <- dispNames[dispNames$Type == 'result' & dispNames$SubType == '25', 'OutName'];
v75 <- dispNames[dispNames$Type == 'result' & dispNames$SubType == '75', 'OutName'];

dispName <- function(outName) {
  return (dispNames[dispNames$OutName==outName, 'DisplayName'])
}

setXYAxisOptions <- function(input, output, session) {
  
  output$selectX <- renderUI({
    ns <- session$ns
    return (
      selectInput(ns("x"), 'Choose X Axis',
                  choices = xyChoices)
    )
  })
  
  output$selectY <- renderUI({
    ns <- session$ns
    return (
      selectInput(ns("y"), 'Choose Y Axis',
                  choices = xyChoices)
    )
  })
}

