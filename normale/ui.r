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
        "v 1.1, Patrizio Frederic, 2021"
      ),
      
      mainPanel(
        # Utilizzo di tabsetPanel per creare schede
        tabsetPanel(
          tabPanel("Primo Panel", 
                   plotOutput("distPlot"), 
                   plotOutput("densPlot")),
          tabPanel("Secondo Panel", 
                   plotOutput("densPlot"), 
                   plotOutput("mon"))
          # Aggiungi ulteriori tabPanel qui se necessario
        )
      )
    )
  )
)
