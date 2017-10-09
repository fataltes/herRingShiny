
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
          return (selectedRes)
        }  
  })
  
  output$distPlot <- renderPlot({
    xyVal <- xy()
    x25id = get25(x())
    x75id = get75(x())
    y25id = get25(y())
    y75id = get75(y())
    if (!is.null(xyVal) && !is.na(xyVal) && nrow(xyVal) > 0) {
      pll <- ggplot(xyVal, aes_string(x=input$x, y=input$y, color='bias', shape='steep')) 
      
        if (!is.null(x25id) && !is.na(x25id) && x25id != "" ) {
          x1 <- xyVal[x()]-xyVal[x25id]
          y <- xyVal[y()]
          x2 <- xyVal[x()]+xyVal[x75id]
          dd <- data.frame(x1, y, x2)
          pll <- pll + 
            geom_segment(aes(x=xyVal[x()]-xyVal[x25id], y=xyVal[y()], xend=xyVal[x()]+xyVal[x75id], yend=xyVal[y()]), alpha=1)
        }
        if (!is.null(y25id) && !is.na(y25id) && y25id != "" ) {
          pll <- pll + 
            geom_segment(aes(x=xyVal[x()], y=xyVal[y()]-xyVal[y25id], xend=xyVal[x()], yend=xyVal[y()]+xyVal[y75id]), alpha=1)
        }
      pll +
        geom_point(size=3, alpha=1) +
        theme_classic() +
        xlab(dispName(x())) +
        ylab(dispName(y())) 
      #+
      #  theme(plot.title = element_text(hjust = 0.5))
    }
  })

  get25 <- function(metric) {
    if (!is.null(metric) && !is.na(metric)) {
      if (length(grep(paste("Q25",substring(metric, 4), sep=""), v25)) > 0)
        return (paste("Q25",substring(metric, 4), sep=""))
      else if (length(grep(paste(metric, "_25", sep=""), v25)) > 0)
        return (paste(metric, "_25", sep=""))
    }
    return(NULL)
  }  
  get75 <- function(metric) {
    if (!is.null(metric) && !is.na(metric)) {
      if (length(grep(paste("Q75",substring(metric, 4), sep=""), v75)) > 0)
        return (paste("Q75",substring(metric, 4), sep=""))
      else if (length(grep(paste(metric, "_75", sep=""), v75)) > 0)
        return (paste(metric, "_75", sep=""))
    }
    return(NULL)
  }  
  
  output$view <- renderTable({
    res <- xy()
    if (!is.null(res) && !is.na(res) && nrow(res) > 0) {
      res <- res[c("Bias", "Steep", x(), y())]
      colnames(res) <- c("Bias", "Steep", toString(dispName(x())), toString(dispName(y())))
      return (res)
    }
  })
}