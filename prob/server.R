#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# pat_book <- "pat.book" %in% installed.packages()[,1]
# if (pat_book) library(pat.book) else {
#   fun <- dir("R/")
#   lapply(fun, function(x) source(paste0("R/",x)))
#   }
fun <- dir("R/")
lapply(fun, function(x) source(paste0("R/",x)))

bprint <- function(expr) {
  out <- paste0(capture.output(expr),collapse = "\n")
  gsub("\\\\normalsize", "", out)
  }

library(DT)
library(shinythemes)
library(shinyWidgets)
library(shiny)
library(knitr)
library(rmarkdown)

# Define server logic required to draw a histogram
function(input, output) {
  
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
  output$mon <- renderPlot({ 
    sss(input,"tab")  
  },height = 1000)
  output$norm <- renderPlot({
    mu1 <- input$mu1
    s1  <- sqrt(input$sigma1)
    mu2 <- input$mu2
    s2  <- sqrt(input$sigma2)
    curve(dnorm(x,mu1,s1),-40,40,1001,col=4)
    curve(dnorm(x,mu2,s2),add=T,n = 1001,col=2)
  },height = 800)
  output$sol <- renderUI({
    x1 <- input$xx[1]
    x2 <- input$xx[2]
    mm <- input$mm
    ss <- input$ss
    if (x1<=mm-4*ss & x2 >=mm+4*ss) {verso <- NULL} else
      if (x1<=mm-4*ss & x2 <mm+4*ss) {
        x1 <- x2
        x2 <- NULL
        verso <- "<"
        } else
      if (x1> mm-4*ss & x2 >=mm+4*ss) {
        verso <- ">"
        x2 <- NULL
      } else { verso = NULL}
    vnm <- ifelse(mm == 0 & ss == 1,"Z","X")
    withMathJax(paste(
      paste("$$\\Large ",vnm,"\\sim N(\\mu=",mm,",\\sigma^2=",ss,")$$"),
      paste("$$\\Large",bprint(norm_int(x1 = x1,x2 = x2,mm = mm,ss = ss,verso = verso,vnam = vnm)),"$$")
    ))
  })
  output$plotsPanel <- renderUI({
    plotsToShow <- input$showPlots # Ottiene le scelte dell'utente
    
    # Crea una lista di elementi UI per i grafici selezionati
    plotsUI <- lapply(plotsToShow, function(plotId) {
      if(plotId == "distPlot") {
        plotOutput("distPlot")
      } else if(plotId == "densPlot") {
        plotOutput("densPlot")
      } else if(plotId == "mon") {
        plotOutput("mon")
      }
    })
    
    # Restituisce la lista degli elementi UI per il rendering
    do.call(tagList, plotsUI)
  })  
  
  output$bin <- renderUI({
    x1 <- input$x1
    n <- input$nn
    pp <- input$pp
    verso <- input$verso
    comp <- input$comp
    sing <- F
    if (verso == "=") sing <- T
    withMathJax(paste(
      paste("$$\\Large X\\sim \\text{Binom}(n=",n,";\\pi=",pp,")$$"),
      paste("$$\\Large ",bprint(bin_dis(x1 = x1,n = n, pp=pp,verso = verso,comp = comp,size = "\\Large",sing = sing)),"$$")
    ))
  })
  output$binTitle <- renderUI({
    n <- input$n
    p <- input$p
    withMathJax(paste("$$\\Large X\\sim \\text{Binom}(n=",n,";\\pi=",p,")$$"))
  })
  output$binPlot <- renderPlot({
    n <- input$n
    p <- input$p
    binom(n,p)
  },height = 800)
  output$poisPlot <- renderPlot({
    l <- input$l
    pois(l)
  },height = 800)
  output$poisTitle <- renderUI({
    l <- input$l
    withMathJax(paste("$$\\Large X\\sim \\text{Pois}(\\lambda=",l,")$$"))
  })
  output$pos <- renderUI({
    x1 <- input$xp
    ll <- input$ll
    verso <- input$verso1
    sing <- F
    if (verso == "=") sing <- T
    withMathJax(paste(
      paste("$$\\Large X\\sim \\text{Pois}(\\lambda=",ll,")$$"),
      paste("$$\\Large",bprint(pois_dis(x1 = x1,ll=ll,verso = verso,sing = sing)),"$$")
    ))
  })
  
  output$inputS1 <- renderUI({
    numInputs <- as.integer(input$m)
    lapply(1:numInputs, function(i) {
      fluidRow(
        column(6, numericInput(paste("S1_", i, sep = ""), sprintf("S_X %d", i), value = i)),
        column(6, numericInput(paste("num1_", i, sep = ""), sprintf("n_X %d", i), value = 1, min = 0, step = 1))
      )
    })
  })
  
  #output$inputNum1 <- NULL # Non necessario poiché già incluso sopra
  
  output$inputS2 <- renderUI({
    numInputs <- as.integer(input$k)
    lapply(1:numInputs, function(i) {
      fluidRow(
        column(6, numericInput(paste("S2_", i, sep = ""), sprintf("S_Y %d", i), value = i)),
        column(6, numericInput(paste("num2_", i, sep = ""), sprintf("n_Y %d", i), value = 1, min = 0, step = 1))
      )
    })
  })
  
  #output$inputNum2 <- NULL # Non necessario poiché già incluso sopra
  
  output$two_way <- renderUI({
    m <- input$m
    k <- input$k
    ope <- input$ope
    S_1 <- numeric(m)
    num1 <- integer(m)
    S_2 <- numeric(k)
    num2 <- integer(k)
    for(i in 1:m) {
      s1InputId <- paste("S1_", i, sep = "")
      num1InputId <- paste("num1_", i, sep = "")
      if(!is.null(input[[s1InputId]])) S_1[i] <- input[[s1InputId]]
      if(!is.null(input[[num1InputId]])) num1[i] <- input[[num1InputId]]
    }
    
    for(i in 1:k) {
      s2InputId <- paste("S2_", i, sep = "")
      num2InputId <- paste("num2_", i, sep = "")
      if(!is.null(input[[s2InputId]])) S_2[i] <- input[[s2InputId]]
      if(!is.null(input[[num2InputId]])) num2[i] <- input[[num2InputId]]
    }
    
    if (ope == "+") tw <- bprint(invisible(two_way(S_1 = S_1,S_2 = S_2,num1 = num1,num2 = num2,vnam = "W",op = `+`,size = "")))
    if (ope == "-") tw <- bprint(invisible(two_way(S_1 = S_1,S_2 = S_2,num1 = num1,num2 = num2,vnam = "W",op = `-`,size = "")))
    if (ope == "*") tw <- bprint(invisible(two_way(S_1 = S_1,S_2 = S_2,num1 = num1,num2 = num2,vnam = "W",op = `*`,size = "")))
    if (ope == "/") tw <- bprint(invisible(two_way(S_1 = S_1,S_2 = S_2,num1 = num1,num2 = num2,vnam = "W",op = `/`,size = "")))
    
    
    withMathJax(paste("$$ W = X ",ope," Y $$",tw,collapse = "\n"))
  })
  
}
