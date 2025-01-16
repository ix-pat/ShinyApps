library(shiny)
library(ggplot2)

# Dati iniziali
data <- data.frame(
  x = c(0, 1, 2, 3),
  y = c(2.0, 3.5, 2.5, 4.0)
)

# Funzione per calcolare la somma dei minimi quadrati
calc_mse <- function(beta0, beta1, data) {
  y_pred <- beta0 + beta1 * data$x
  mse <- sum((data$y - y_pred)^2)
  return(mse)
}

# Funzione per calcolare i coefficienti di minimi quadrati
lm_coeffs <- lm(y ~ x, data = data)$coefficients

ui <- fluidPage(
  titlePanel("Retta di regressione e residui"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("beta0", "Intercetta (beta0):", value = 2.2,step = .05),
      numericInput("beta1", "Pendenza (beta1):", value = 0.3,step = .05)
    ),
    
    mainPanel(
      fluidRow(
        column(12, plotOutput("regressionPlot", width = "100%", height = "700px")),
        column(12, h3(textOutput("mseText"), align = "center"))
      )
    )
  )
)

server <- function(input, output) {
  output$regressionPlot <- renderPlot({
    beta0 <- input$beta0
    beta1 <- input$beta1
    y_pred <- beta0 + beta1 * data$x
    mse <- calc_mse(beta0, beta1, data)
    
    # Colore della retta (rossa se coincide con la retta dei minimi quadrati)
    line_color <- if (isTRUE(all.equal(c(beta0, beta1), unname(lm_coeffs), tolerance = 1e-3))) {
      "red"
    } else {
      "blue"
    }
    
    ggplot(data, aes(x = x, y = y)) +
      geom_point(size = 3) +
      geom_segment(aes(x = x, xend = x, y = y, yend = y_pred), color = "green", linetype = "dashed") +
      geom_abline(intercept = beta0, slope = beta1, color = line_color, linetype = "solid", size = 1) +
      labs(title = "Retta di regressione e residui",
           x = "x", y = "y") +
      theme_minimal() +
      annotate("text", x = 1.5, y = max(data$y) + 0.5, label = paste0("MSE = ", round(mse, 3)), size = 6, color = "red")
  })
  
  output$mseText <- renderText({
    beta0 <- input$beta0
    beta1 <- input$beta1
    mse <- calc_mse(beta0, beta1, data)
    paste("Somma dei minimi quadrati (MSE):", round(mse, 3))
  })
}

shinyApp(ui = ui, server = server)
