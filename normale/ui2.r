library(shiny)

shinyUI(
  fluidPage(
    titlePanel("La Normale Standard"),
    
    sidebarLayout(
      sidebarPanel(
        sliderInput("it", "intervallo di itegrazione",
                    min = -4, max = 4, value = c(-4,0), step = .01),
        sliderInput("gc", "Grandezza Carattere",
                    min = 0.01, max = 2, value = .7, step = .01),
        "------------",
        numericInput("mu",label = "mu",value = 0),
        numericInput("ss",label = "sigma²",value = 0),
        numericInput("x1",label = "x1",value = 0),
        numericInput("x2",label = "x2",value = 0),
        "v 1.2, Patrizio Frederic, 2021-2024"
      ),
      
      mainPanel(
        # Utilizzo di tabsetPanel per creare schede
        tabsetPanel(
          tabPanel("Grafico", 
                   plotOutput("distPlot"),
                   plotOutput("densPlot")),
          tabPanel("Uso delle Tavole", 
                   plotOutput("densPlot2"),
                   plotOutput("mon")
                   )
          # Aggiungi ulteriori tabPanel qui se necessario
        )
      )
    )
  )
)
