
library(shiny)
library(diagmeta)
library(lme4)
library(shinythemes)
library(gridExtra)
## Get the value of the dataset that is selected by user from the list of datasets
shinyServer(function(input, output) {
  Data1 <- reactive({
    inFile <- input$file
    read.csv2(inFile$datapath, header = TRUE)
  })
  
  diag1 <- eventReactive(input$submit, {
    diagmeta(Data1()$TP, Data1()$FP, Data1()$TN, Data1()$FN,  
             Data1()$cutoff, Data1()$studlab, Data1(), distr =input$distr, 
             model = input$model, equalvar = input$equalvar, lambda = input$lambda, log.cutoff = 
               input$log.cutoff, method.weights = input$method.weights, level=input$level,
             incr = input$incr, n.iter.max = input$n.iter)
    
  })
  
  ## to output the dataset
  output$result <- renderPrint({
    summary(diag1())  
  })
  output$result1 <- renderPrint({
    diag1()  
  })
  
  
  plotdiag <- eventReactive(input$submit1,{
    
    plot(diag1(), which = input$which, ci = input$ci,
         ciSens = input$ciSens, ciSpec = input$ciSpec, mark.optcut = input$mark.optcut, mark.cutpoints = input$mark.cutpoints,
         points = input$points, lines = input$lines, rlines = input$rlines,
         line.optcut = input$line.optcut,
         col.points =  input$col.points, shading = input$shading,  ellipse = input$ellipse)
    
  })
  
  output$plot<- renderPlot({
    plotdiag()
    
    
  })
  
  
  output$down <- downloadHandler(
    filename =  function() {
      paste("diagmeta", input$tplot, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$tplot == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      
      plot(diag1(), which = input$which, ci = input$ci,
           ciSens = input$ciSens, ciSpec = input$ciSpec, mark.optcut = input$mark.optcut, mark.cutpoints = input$mark.cutpoints,
           points = input$points, lines = input$lines, rlines = input$rlines,
           line.optcut = input$line.optcut,
           col.points =  input$col.points)
      dev.off()  # turn the device off
      
    } 
  )
  
  diagvalues <- eventReactive(input$submit2,{
    values(diag1(), pr = input$pr, cut = input$cut )
    
  })
  
  output$value <- renderPrint({
    diagvalues()
  })
  
  tabulate <- eventReactive(input$submit3, {
    y <- as.numeric(unlist(strsplit(input$cutoffs,",")))
    tabulate.SeSp(diag1(), cutoffs = y )
    
    
    
  })
  output$tab <- renderPrint({
    tabulate()
  })

  
 IPDdata <- eventReactive(input$submit4, {
    study <- as.numeric(unlist(strsplit(input$studlab,",")))
    value  <-  as.numeric(unlist(strsplit(input$value,",")))
    status <- as.numeric(unlist(strsplit(input$status,",")))
data.frame(study, value, status)


  })
  output$dframe <- renderTable({
    IPDdata()
    
  })
  
  IPDdata1 <- eventReactive(input$submit5, {
    study <- as.numeric(unlist(strsplit(input$studlab,",")))
    value  <-  as.numeric(unlist(strsplit(input$value,",")))
    status <- as.numeric(unlist(strsplit(input$status,",")))
   IPD2diag(study, value, status)
    
    
  })
  
  output$dframe1 <- renderTable({
    IPDdata1()
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
})
