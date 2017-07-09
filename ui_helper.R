library(ggplot2)

dispName <- function(outName) {
  return (dispNames[dispNames$OutName==outName, 'DisplayName'])
}

selectBuilder <- function(input, output, session) {
  #print (input$crType)
  crType <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$crType, message = FALSE))
    input$crType
  })
  
  getSelects <- reactive({
    #print ("in getSelects")
    #print (crType())
    ns <- session$ns
    if (crType() == 'CC') {
      renderedPanel <- selectInput(ns("Propc"), dispName('Propc'),
                                   choices = uniquePropc)
    } else if (crType() == 'CCC') {
      renderedPanel <- fluidRow(
        column(6, selectInput(ns("Propc"),dispName('Propc'),
                              choices = uniquePropc)),
        column(6, selectInput(ns("Fcapprop"),dispName('Fcapprop'),
                              choices = uniqueFcapprop))
      )
    } else {
      #print ("here again")
      #print (c(input$FracBmsyThreshLo, input$FracBmsyThreshHi, input$FracFtarg))
      renderedPanel <- 
               selectInput(ns("FracBmsyThreshLo"), dispName('FracBmsyThreshLo'),
                              choices = uniqueFracBmsyThreshLo)
    }
    return(renderedPanel)
  })
  output$crParams <- renderUI({
      ns <- session$ns
      getSelects()
  })
  

  output$low <- renderUI({
    ns <- session$ns
    if (input$crType == 'BB' | input$crType == 'BB3yr' | input$crType == 'BB5yr' | input$crType == 'BB3yrPerc' )
      return (
          selectInput(ns("FracBmsyThreshHi"), dispName('FracBmsyThreshHi'),
                          choices = uniqueFracBmsyThreshHi[
                            uniqueFracBmsyThreshHi$FracBmsyThreshLo == input$FracBmsyThreshLo, 'FracBmsyThreshHi'])
      )
    
  })
  output$hi <- renderUI({
    ns <- session$ns
    if (input$crType == 'BB' | input$crType == 'BB3yr' | input$crType == 'BB5yr' | input$crType == 'BB3yrPerc' )
      return (
          selectInput(ns("FracFtarg"), dispName('FracFtarg'),
                          choices = uniqueFracFtarg[uniqueFracFtarg$FracBmsyThreshHi == input$FracBmsyThreshHi &
                                                      uniqueFracFtarg$FracBmsyThreshLo == input$FracBmsyThreshLo  
                                                    ,'FracFtarg'])
      )

  })
}



drawPlot <- function(input, output, session) {
  ns <- session$ns
  
  output$distPlot <- renderPlot({
    #body()
    crRes = allres[allres$CR == input$crType,]
    if (input$crType == 'CC') {
      #print (input$Propc)
      if (!is.null(input$Propc))
        selectedRes <- crRes[crRes$Propc == input$Propc,]
    } else if (input$crType == 'CCC') {
      if (!is.null(input$Propc) & !is.null(input$Fcapprop))
        selectedRes <- crRes[crRes$Propc == input$Propc & 
                               crRes$Fcapprop == input$Fcapprop,]
    } else {
      if (!is.null(input$FracBmsyThreshHi) & !is.null(input$FracBmsyThreshLo) & !is.null(input$FracFtarg)) {
        #print (c(input$FracBmsyThreshLo, input$FracBmsyThreshHi, input$FracFtarg))
        selectedRes <- crRes[crRes$FracBmsyThreshHi == input$FracBmsyThreshHi &
                               crRes$FracBmsyThreshLo == input$FracBmsyThreshLo &
                               crRes$FracFtarg == input$FracFtarg,]
        #print (nrow(selectedRes))
      }
    }
    if (exists("selectedRes")) {
      xy <- selectedRes[order(selectedRes$MedianSSB), c('MedianSSB', 'Yield', 'bias', 'steep')]
      
      ggplot(xy, aes(x=MedianSSB, y=Yield, color=bias, shape=steep)) +
        geom_point(size=3) +
        theme_classic() +
        xlab(dispName('MedianSSB')) +
        ylab(dispName('Yield')) +
        theme(plot.title = element_text(hjust = 0.5))
      #plot(xy$MedianSSB, xy$Yield, type="p", col = xy$bias, 
      #     xlab = dispNames[dispNames$OutName=='MedianSSB', 'DisplayName'], ylab = dispNames[dispNames$OutName=='Yield', 'DisplayName'])
    }
    
  })
}