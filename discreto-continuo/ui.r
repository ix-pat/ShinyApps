library(shiny)
library(shinythemes)

# sudo cp -R ~/Dropbox/statistica-old/esempi/app3/ /srv/shiny-server/

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    theme = shinytheme("flatly"),
    # Application title
    titlePanel("Discreto/Continuo"),
    
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
      sidebarPanel(
        numericInput("mm", "numero partizioni", 1,max = 12),
        sliderInput("int", "intervallo",
                    min = -1, max = 9, value = c(-1,-1),step = .05),
        checkboxInput("sl","solve"),
        checkboxInput("md","model"),
        sliderInput("m1", "m1",
                    min = 0,max = 8, value = 3.5,step = .05),
        sliderInput("v1", "v1",
                    min = 0,max = 1, value = 0.55,step = .0001),
        sliderInput("m2", "m2",
                    min = 0,max = 8, value =2.2857143,step = .05),
        sliderInput("v2", "v2",
                    min = 0,max = 1, value = 0.3299144,step = .0001),
        sliderInput("alpha", "alpha",
                    min = 0, max = 1, value = 1 ,step = .05)
      ),
      mainPanel(
        plotOutput("distPlot"),
        plotOutput("densPlot"),
        textOutput("mon")
      )
      
    )
    # Show a plot of the generated distribution
  )
)

