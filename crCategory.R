
showCRCategoryResult <- function(input, output, session) {
  ns <- session$ns
  #print(ns)
  crType <- reactive({
    validate(need(input$crType, message = FALSE))
    input$crType
  })
  
  x <- reactive({
    validate(need(input$x, message = FALSE))
    input$x
  })
  
  y <- reactive({
    validate(need(input$y, message = FALSE))
    input$y
  })
  
  xy <- reactive({
    selectedRes = allres[allres$CR == crType(),]
    return (selectedRes[order(selectedRes[, x()]), c('bias', 'steep', x(), y())])
  })
 
  output$distPlot <- renderPlot({
    xyVal <- xy()
    if (!is.null(xyVal)) {
      sp <- ggplot(xyVal, aes_string(x=x(), y=y())) +
        geom_point(size=3) +
        theme_classic() +
        xlab(dispName(x())) +
        ylab(dispName(y())) 
      #sp <- ggplotly(sp)
      if (!is.null(input$split)) {
        switch(input$split,
               none = sp,
               bias = sp + facet_grid(. ~ bias),
               steep = sp + facet_grid(. ~ steep),
               bias_steep = sp + facet_grid(bias ~ steep)
               )
      }
    }
  })
  
  output$my_tooltip <- renderPrint({
    #req(input$plot_hover)
    #print("got the hover signal")
    cat("X/Y values of the point:\n")
    xyVal <- xy()
    pnt <- nearPoints(xyVal, input$plot_hover, input$x, input$y)[1, c(input$x, input$y)]
    req(nrow(pnt) != 0)
    str(pnt)
  })
}