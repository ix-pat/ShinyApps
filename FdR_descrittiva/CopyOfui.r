library(shiny)
# sudo cp -R ~/OneDrive/statistica-old/esempi/app3/ /srv/shiny-server/

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    withMathJax(),
    # Application title
    titlePanel("Funzione di Ripartizione"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        numericInput("k_value", "Inserisci k (k > 1):", min = 2, value = 4),
        fluidRow(
          column(6,actionButton("submit_k", "Conferma")),
          column(6,numericInput(inputId = "pp",label = "Percentile",value = 0.5,min = 0,max = 1))
        ),
        uiOutput("range_slider"),   # Aggiungi questo per lo slider dinamico
        uiOutput("numeric_inputs"),  # UI dinamica per gli input numerici brk
        uiOutput("integer_inputs"),   # UI dinamica per gli input numerici nnn
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
        ),
        uiOutput("calcolo_perc")
      )
      
    )
  )
)
