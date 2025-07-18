library(shiny)
source("script.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
    sss(input,"dist")
      })
  output$densPlot <- renderPlot({
    sss(input,"dens")
  })
  output$mon <- renderText({ 
    paste("ver 1.0 Patrizio Frederic, 2016", "oggi è il", date())
  })
    
  
})

