library(DT)
library(shinythemes)
library(shinyWidgets)
library(shiny)
library(knitr)
library(rmarkdown)
source("script.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  output$xx <- renderUI({
    mm <- input$mm
    ss <- input$ss
    minVal <- round(mm - 4 * sqrt(ss),2)
    maxVal <- round(mm + 4 * sqrt(ss),2)
    
    sliderInput("xx", "Intervallo X:", 
                min = minVal, max = maxVal, 
                value = c(minVal, maxVal), step = .01)
  })
  output$distPlot <- renderPlot({
   sss(input,"dist")
      })
  output$densPlot <- renderPlot({
    sss(input,"dens")
  })
  output$densPlot2 <- renderPlot({
    sss(input,"dens")
  })
  output$mon <- renderPlot({ 
    sss(input,"tab")  
  },height = 1000)
  output$sol <- renderUI({
      x1 <- input$xx[1]
      x2 <- input$xx[2]
      mm <- input$mm
      ss <- input$ss
      vnm <- ifelse(mm == 0 & ss == 1,"Z","X")
      withMathJax(paste(
        paste("$$\\Large ",vnm,"\\sim N(\\mu=",mm,",\\sigma^2=",ss,")$$"),paste("$$",n_int(x1 = x1,x2 = x2,mm = mm,ss = ss),"$$")
        ))
  })
    
  
})

