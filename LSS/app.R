library(shiny)
library(ggplot2)
library(quantreg)

# Dati iniziali
data <- data.frame(
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

# Coefficienti delle rette
lm_coeffs <- unname(lm(y ~ x, data = data)$coefficients)  # Rimuoviamo i nomi
rq_coeffs <- unname(rq(y ~ x, data = data)$coefficients)  # Rimuoviamo i nomi

ui <- fluidPage(
  titlePanel("Retta di regressione e residui"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("beta0", "Intercetta (beta0):", value = 2.2, step = .05),
      numericInput("beta1", "Pendenza (beta1):", value = 0.3, step = .05),
      actionButton("fitMSE", "Fissa sui minimi quadrati"),
      actionButton("fitMAE", "Fissa sui minimi scostamenti assoluti")
    ),
    
    mainPanel(
      fluidRow(
        column(12, plotOutput("regressionPlot", width = "100%", height = "700px"))
        #        column(12, h3(textOutput("mseText"), align = "center"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values per gestire beta0 e beta1
  beta <- reactiveValues(beta0 = 2.2, beta1 = 0.3)
  
  # Osservatori per sincronizzare i valori degli input con i reactiveValues
  observeEvent(input$beta0, {
    beta$beta0 <- input$beta0
  })
  
  observeEvent(input$beta1, {
    beta$beta1 <- input$beta1
  })
  
  # Osservatori per i bottoni
  observeEvent(input$fitMSE, {
    beta$beta0 <- lm_coeffs[1]
    beta$beta1 <- lm_coeffs[2]
    updateNumericInput(session, "beta0", value = beta$beta0)
    updateNumericInput(session, "beta1", value = beta$beta1)
  })
  
  observeEvent(input$fitMAE, {
    beta$beta0 <- rq_coeffs[1]
    beta$beta1 <- rq_coeffs[2]
    updateNumericInput(session, "beta0", value = beta$beta0)
    updateNumericInput(session, "beta1", value = beta$beta1)
  })
  
  # Plot
  output$regressionPlot <- renderPlot({
    beta0 <- beta$beta0
    beta1 <- beta$beta1
    y_pred <- beta0 + beta1 * data$x
    mse <- calc_mse(beta0, beta1, data)
    
    # Colore della retta (rossa se coincide con la retta dei minimi quadrati)
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
  
  # Testo dei residui
  output$mseText <- renderText({
    beta0 <- beta$beta0
    beta1 <- beta$beta1
    mse <- calc_mse(beta0, beta1, data)
    paste("Somma dei quadrati:", round(mse[1], 3), "\n\n\n", "Somma dei valori assoluti:", round(mse[2], 3), "\n")
  })
}

shinyApp(ui = ui, server = server)
