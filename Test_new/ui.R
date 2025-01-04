library(shiny)
library(shinythemes)
library(shinyjs)
library(colorspace)
source("intro.R")
options(scipen=1000,digits = 4)
# Definizione dell'interfaccia utente
ui <- fluidPage(
  theme = shinytheme("readable"),
  withMathJax(), # Abilita MathJax
  titlePanel("Test "),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6, 
               checkboxInput("p_val_only", 
                             label = "Solo p-value", 
                             value = FALSE)),
        column(6, 
               checkboxInput("colori", 
                             label = "4 colori", 
                             value = TRUE))
      ),
      selectInput("alpha", 
                  label = "Scegli il livello di significatività",
                  choices = c('0.1', '0.05','0.01','0.001','Tutti'),selected = "Tutti"),
      
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
