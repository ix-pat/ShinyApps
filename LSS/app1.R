library(shiny)
library(ggplot2)
library(quantreg)

# Dati iniziali
original_data <- data.frame(
  x = c(0, 1, 2, 3),
  y = c(2.0, 3.5, 2.5, 4.0)
)

# Funzione per calcolare la somma dei minimi quadrati
calc_mse <- function(beta0, beta1, data) {
  y_pred <- beta0 + beta1 * data$x
  mse <- sum((data$y - y_pred)^2)
  mae <- sum(abs(data$y - y_pred))
  return(c(mse, mae))
}

ui <- fluidPage(
  titlePanel("Retta di regressione e residui"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Seleziona il dataset:",
                  choices = c("Dati originali", "Dataset simulato")),
      conditionalPanel(
        condition = "input.dataset == 'Dataset simulato'",
        numericInput("sim_beta0", "Vera Intercetta (beta0):", value = 2, step = 0.1),
        numericInput("sim_beta1", "Vera Pendenza (beta1):", value = 0.5, step = 0.1),
        numericInput("sim_sigma", "Deviazione standard (σ):", value = 1, step = 0.1),
        numericInput("sim_n", "Numero di punti (n):", value = 10, step = 1),
        actionButton("resimulate", "Simula")
      ),
      numericInput("beta0", "Intercetta stimata (beta0):", value = 2.2, step = 0.05),
      numericInput("beta1", "Pendenza stimata (beta1):", value = 0.3, step = 0.05),
      actionButton("fitMSE", "Fissa sui minimi quadrati"),
      actionButton("fitMAE", "Fissa sui minimi scostamenti assoluti")
    ),
    
    mainPanel(
      fluidRow(
        column(12, plotOutput("regressionPlot", width = "100%", height = "700px"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values per gestire beta0 e beta1
  beta <- reactiveValues(beta0 = 2.2, beta1 = 0.3)
  
  # Reactive value per il dataset
  simulated_data <- reactiveVal(NULL)
  
  # Generazione dinamica del dataset
  observe({
    if (input$dataset == "Dataset simulato") {
      req(input$resimulate)  # Aspetta un clic sul pulsante di risimulazione
      
      x <- runif(input$sim_n, min = 0, max = 3)
      epsilon <- rnorm(input$sim_n, mean = 0, sd = input$sim_sigma)
      y <- input$sim_beta0 + input$sim_beta1 * x + epsilon
      simulated_data(data.frame(x = x, y = y))
    }
  })
  
  # Dataset reattivo scelto
  reactive_data <- reactive({
    if (input$dataset == "Dati originali") {
      original_data
    } else {
      simulated_data()
    }
  })
  
  # Osservatori per sincronizzare i valori degli input con i reactiveValues
  observeEvent(input$beta0, {
    beta$beta0 <- input$beta0
  })
  
  observeEvent(input$beta1, {
    beta$beta1 <- input$beta1
  })
  
  # Osservatori per i bottoni
  observeEvent(input$fitMSE, {
    data <- reactive_data()
    lm_coeffs <- unname(lm(y ~ x, data = data)$coefficients)
    beta$beta0 <- lm_coeffs[1]
    beta$beta1 <- lm_coeffs[2]
    updateNumericInput(session, "beta0", value = beta$beta0)
    updateNumericInput(session, "beta1", value = beta$beta1)
  })
  
  observeEvent(input$fitMAE, {
    data <- reactive_data()
    rq_coeffs <- unname(rq(y ~ x, data = data)$coefficients)
    beta$beta0 <- rq_coeffs[1]
    beta$beta1 <- rq_coeffs[2]
    updateNumericInput(session, "beta0", value = beta$beta0)
    updateNumericInput(session, "beta1", value = beta$beta1)
  })
  
  # Plot
  output$regressionPlot <- renderPlot({
    data <- reactive_data()
    req(data)  # Assicurati che il dataset sia disponibile
    beta0 <- beta$beta0
    beta1 <- beta$beta1
    y_pred <- beta0 + beta1 * data$x
    mse <- calc_mse(beta0, beta1, data)
    
    # Colore della retta (rossa se coincide con la retta dei minimi quadrati)
    lm_coeffs <- unname(lm(y ~ x, data = data)$coefficients)
    line_color <- if (isTRUE(all.equal(c(beta0, beta1), lm_coeffs, tolerance = 1e-3))) {
      "red"
    } else {
      "blue"
    }
    
    ggplot(data, aes(x = x, y = y)) +
      geom_point(size = 3) +
      geom_segment(aes(x = x, xend = x, y = y, yend = y_pred), color = "green", linetype = "dashed") +
      geom_abline(intercept = beta0, slope = beta1, color = line_color, linetype = "solid", size = 1) +
      labs(x = "x", y = "y") +
      theme_minimal() +
      annotate("text", x = 1.5, y = max(data$y) + 0.5, label = paste0("Somma Quad = ", round(mse[1], 3)), size = 6, color = "blue") +
      annotate("text", x = 1.5, y = max(data$y) + 0.4, label = paste0("Somma VA   = ", round(mse[2], 3)), size = 6, color = "red")
  })
}

shinyApp(ui = ui, server = server)
