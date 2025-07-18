library(shiny)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Generatore di Documenti"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "anni_sel",
        label = "Seleziona gli anni da cui estrarre:",
        choices = c("2021", "2022", "2023", "2024"),
        selected = c("2021", "2022", "2023", "2024")
      ),
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
    files <- paste0(input$anni_sel, ".Rmd")
    #cat(files)
    source("random-test.R")  
    gen_test(files)
    
    
    # Processo di generazione e compilazione dei documenti
    
    system("cp compito_sol.Rmd www/compito.Rmd")
    # Rimuovi la finestra modale una volta che il documento è pronto
    removeModal()
    
    # Rendi nuovamente cliccabile il pulsante
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
