
selectBuilder <- function(input, output, session) {
  crType <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$crType, message = FALSE))
    input$crType
  })
  
  getSelects <- reactive({
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



showSpecificCRResult <- function(input, output, session) {
  ns <- session$ns
  
  x <- reactive({
    validate(need(input$x, message = FALSE))
    input$x
  })
  
  y <- reactive({
    validate(need(input$y, message = FALSE))
    input$y
  })
  
  xy <- reactive({
        crRes = allres[allres$CR == input$crType,]
        if (input$crType == 'CC') {
          if (!is.null(input$Propc))
            selectedRes <- crRes[crRes$Propc == input$Propc,]
        } else if (input$crType == 'CCC') {
          if (!is.null(input$Propc) & !is.null(input$Fcapprop))
            selectedRes <- crRes[crRes$Propc == input$Propc & 
                                   crRes$Fcapprop == input$Fcapprop,]
        } else {
          if (!is.null(input$FracBmsyThreshHi) & !is.null(input$FracBmsyThreshLo) & !is.null(input$FracFtarg)) {
            selectedRes <- crRes[crRes$FracBmsyThreshHi == input$FracBmsyThreshHi &
                                   crRes$FracBmsyThreshLo == input$FracBmsyThreshLo &
                                   crRes$FracFtarg == input$FracFtarg,]
          }
        }
      
        if (exists("selectedRes")) {
          return (selectedRes[order(selectedRes[,input$x]), c('bias', 'steep', input$x, input$y)])
        }  
  })
  
  
  output$distPlot <- renderPlot({
    xyVal <- xy()
    if (!is.null(xyVal)) {
      ggplot(xyVal, aes_string(x=input$x, y=input$y, color='bias', shape='steep')) +
        geom_point(size=3) +
        theme_classic() +
        xlab(dispName(x())) +
        ylab(dispName(y())) +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
  })
  
  output$view <- renderTable({
      res <- xy()
      colnames(res) <- c("Bias", "Steep", toString(dispName(x())), toString(dispName(y())))
      return (res)
  })
}