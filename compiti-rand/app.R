library(shiny)
library(shinyjs)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Generatore di Compiti"),
  sidebarLayout(
    sidebarPanel(
      shiny::p("Questa app genera compiti casuali pescando gli esercizi a caso dalle prove passate."),
      shiny::p("È ancora presente qualche bug e l'app potrebbe smettere di funzionare, nel caso va riavviata."),
      
      checkboxGroupInput(
        inputId = "anni_sel",
        label = "Seleziona gli anni da cui estrarre:",
        choices = c("2021", "2022", "2023", "2024","2025"),
        selected = c("2022", "2023", "2024","2025")
      ),
      actionButton("generate", "Genera Documento"),
      shiny::p(""),
      shiny::p("v 1.1.0, Patrizio Frederic 2025")
    ),
    
    mainPanel(
      uiOutput("docLink")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$generate, {
    
    # --- BLOCCO: se nessun anno è selezionato, mostra modale e interrompe
    if (length(input$anni_sel) == 0) {
      showModal(modalDialog(
        title = "Errore",
        "Seleziona almeno un anno per procedere.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    }
    
    # --- Se tutto OK, prosegue
    
    # Disabilita il pulsante per prevenire clic multipli
    shinyjs::disable("generate")
    
    # Mostra una finestra modale per indicare che l'elaborazione è in corso
    showModal(modalDialog(
      title = "Attendere prego",
      "Generazione del documento in corso...",
      easyClose = FALSE,
      footer = NULL
    ))
    
    files <- paste0(input$anni_sel, ".Rmd")
    
    # Esegui codice di generazione
    source("random-test.R")  
    gen_test(files)
    
    # Copia file generato
    system("cp compito_sol.Rmd www/compito.Rmd")
    
    # Rimuove la finestra modale una volta che il documento è pronto
    removeModal()
    
    # Rende nuovamente cliccabile il pulsante
    shinyjs::enable("generate")
    
    # Aggiorna il link per il download
    output$docLink <- renderUI({
      tagList(
        tags$a(href = "compito.pdf", "Scarica il compito senza soluzioni", target = "_blank"),
        tags$br(), 
        tags$a(href = "compito.html", "Scarica il compito con soluzioni", target = "_blank"),
        tags$br(), 
        tags$a(href = "compito.Rmd", "Scarica Rmd", target = "_blank")
      )      
    })
  })
}

# Esegui l'app
shinyApp(ui = ui, server = server)
