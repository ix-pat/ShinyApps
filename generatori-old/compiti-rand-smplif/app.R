library(colorspace)
library(shiny)
library(reticulate)
library(knitr)
library(kableExtra)
library(pBrackets)
library(bookdown)
#library(bookdownplus)
#library(plotrix)

library(haven)
#library(rgl)
library(mvtnorm)
source("intro.R")
# source("recupera-pat-book.R")
# source("punti.R")
# source("estrae-punti.R")
# source_python("sposta-punt.py")
# source_python("modifica_punt.py")

ui <- fluidPage(
  shinyjs::useShinyjs(),  # Inizializza shinyjs
  titlePanel("Generatore di Documenti"),
  sidebarLayout(
    sidebarPanel(
      actionButton("generate", "Genera Documento")
    ),
    mainPanel(
      uiOutput("docLink")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$generate, {
    # Disabilita il pulsante per prevenire clic multipli
    shinyjs::disable("generate")
    
    # Mostra una finestra modale per indicare che l'elaborazione è in corso
    showModal(modalDialog(
      title = "Attendere prego",
      "Generazione del documento in corso...",
      easyClose = FALSE,
      footer = NULL
    ))
    source("intro.R")
    # Processo di generazione e compilazione dei documenti
#    reticulate::source_python("save_ex.py")
    reticulate::source_python('Random-ex2.py')
    reticulate::source_python('modifica_punt.py')
    
    # Assicurati di pulire l'ambiente in modo sicuro
#    rm(list=ls())
    rmarkdown::render("compito_com.Rmd", output_file = "www/compito_com.html", envir = globalenv())
#    rm(list=ls())
    rmarkdown::render("compito_sol.Rmd", output_file = "www/compito_sol.html", envir = globalenv())
#    rm(list=ls())
    system("cp compito_sol.Rmd www/compito_sol.Rmd")
    # Rimuovi la finestra modale una volta che il documento è pronto
    removeModal()
    
    # Rendi nuovamente cliccabile il pulsante
    shinyjs::enable("generate")
    
    # Aggiorna il link per il download
    output$docLink <- renderUI({
      tagList(
        tags$a(href = "compito_com.html", "Scarica il compito senza soluzioni", target = "_blank"),
        tags$br(), 
        tags$a(href = "compito_sol.html", "Scarica il compito con soluzioni", target = "_blank"),
        tags$br(), 
        tags$a(href = "compito_sol.Rmd", "Scarica Rmd", target = "_blank")
      )      
    })
  })
}

# Esegui l'app
shinyApp(ui = ui, server = server)
