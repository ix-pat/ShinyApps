library(shiny)
library(shinythemes)
library(shinyjs)
# Definizione dell'interfaccia utente
ui <- fluidPage(
  theme = shinytheme("flatly"),
  withMathJax(), # Abilita MathJax
  titlePanel("Test "),
  sidebarLayout(
    sidebarPanel(
      selectInput("scelta", 
                  label = "Scegli un'opzione",
                  choices = c('Test Z per una media', 'Test t per una media','Test Z per una proporzione','Test t per due campioni','Test Z per due proporzioni'),selected = "Test Z per una media"),
      uiOutput("input_dinamico") # Elemento UI per input dinamici
    ),
    mainPanel(
      uiOutput("risultato"),
      plotOutput("grf")
    )
  )
)
