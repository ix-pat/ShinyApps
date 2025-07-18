library(DT)
library(shinythemes)
library(shinyWidgets)
library(shiny)
library(knitr)
library(rmarkdown)
# sudo cp -R ~/OneDrive/statistica-old/esempi/app3/ /srv/shiny-server/

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    theme = shinytheme("flatly"),
    withMathJax(),
    # Application title
    titlePanel("Funzione di Ripartizione"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        numericInput("k_value", "Inserisci k (k > 1):", min = 2, value = 4),
        fluidRow(
          column(4,actionButton("submit_k", "Conferma")),
          column(4,numericInput(inputId = "pp",label = "Percentile",value = 0.5,min = 0,max = 1,step = .05)),
          column(4,checkboxInput(inputId = "perc_",label = "Mostra Percentile",value = FALSE))
        ),
        uiOutput("range_slider"),   # Aggiungi questo per lo slider dinamico
        uiOutput("dynamicBrkInput"), # Placeholder per gli input dinamici
        paste("ver 1.1 Patrizio Frederic, 2024")
        
      ),
      mainPanel(
        width = 9,
        textOutput("error_message"),  # Visualizza un messaggio di errore se necessario
        fluidRow(                      # Definisce una riga fluida
          column(5, plotOutput("dens")),  # Assegna metà della larghezza (6/12) al primo grafico
          column(7, tableOutput("tab2"))    # Assegna metà della larghezza (6/12) al secondo grafico
        ),
        fluidRow(                      # Definisce una riga fluida
          column(5, plotOutput("rip")),  # Assegna metà della larghezza (6/12) al primo grafico
          column(7,    uiOutput("F_p") )    # Assegna metà della larghezza (6/12) al secondo grafico
        )
      )
      
    )
  )
)
