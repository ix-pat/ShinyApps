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
  titlePanel("Regressione"),
  tabsetPanel(
    tabPanel("Minimi Quadrati",
             
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
),
tabPanel("Inferenza",
         sidebarLayout(
           sidebarPanel(
             numericInput("sge", "Deviazione standard degli errori (\u03c3):", value = 0.8, min = 0.01, step = 0.01),
             numericInput("nrep", "Numero di simulazioni totali:", value = 5000, min = 100, step = 100),
             actionButton("simula", "Genera nuovo campione"),
             actionButton("reset", "Cancella tutto"),
             checkboxInput("show","Mostra limite")
           ),
           mainPanel(
             splitLayout(
               plotOutput("plot_rette"),
               plotOutput("plot_beta")
             )
           )
         )
)
))

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

###### SECONDO TAB

  set.seed(1)
  x <- (1:10)/10
  X <- cbind(1, x)
  XX1 <- solve(t(X) %*% X)
  mu <- c(0, 1)
  
  dati <- reactiveValues(
    y = NULL,
    bhat = NULL,
    campioni = NULL,
    nuovi_punti = list()
  )
  
  observeEvent(input$reset, {
    dati$nuovi_punti <- list()
  })
  
  observeEvent(input$simula, {
    set.seed(NULL)
    sge <- input$sge
    if (is.null(dati$campioni)) {
      y_sim <- replicate(input$nrep, X %*% mu + rnorm(length(x), sd = sge))
      bhat <- t(apply(y_sim, 2, function(y) XX1 %*% t(X) %*% y))
      dati$campioni <- bhat
    }
    
    y_new <- X %*% mu + rnorm(length(x), sd = sge)
    dati$y <- y_new
    dati$bhat <- as.vector(XX1 %*% t(X) %*% y_new)
    dati$nuovi_punti <- append(dati$nuovi_punti, list(dati$bhat))
  })
  
  sey <- function(x, n, sge) sqrt(sge^2 * (1/n + (x - mean(x))^2 / (n * var(x))))
  
  output$plot_rette <- renderPlot({
    if (is.null(dati$y)) return(NULL)
    sge <- input$sge
    y <- dati$y
    
    plot(range(x),range(x), cex = 1, ylim = c(-1, 2), type = "n", 
         xlab = "x", ylab = "y", main = "Retta stimata + intervallo")
    points(x, y, pch = 16)
    abline(0, 1, col = "darkred", lwd = 2)
    abline(v = mean(x), lty = 2)
    abline(h = mean(x), lty = 2)
    
    if (input$show){
      curve(x + 1.96 * sey(x, n = 10, sge = sge), add = TRUE, lty = 2, col = "darkred")
      curve(x - 1.96 * sey(x, n = 10, sge = sge), add = TRUE, lty = 2, col = "darkred")
    }
    b <- dati$nuovi_punti[[length(dati$nuovi_punti)]]
    
    abline(b[1], b[2], col = 4,lwd=2)
    for (b in dati$nuovi_punti) {
      abline(b[1], b[2], col = adjustcolor("grey", 0.5))
    }
  })
  
  output$plot_beta <- renderPlot({
    sge <- input$sge
    CS <- XX1 * sge^2
    b0gr <- seq(-3 * sqrt(CS[1,1]), 3 * sqrt(CS[1,1]), length.out = 101)
    b1gr <- seq(1 - 3 * sqrt(CS[2,2]), 1 + 3 * sqrt(CS[2,2]), length.out = 101)
    grid <- expand.grid(b0 = b0gr, b1 = b1gr)
    
    f_norm <- function(x) {
      b <- c(x[1], x[2])
      exp(-0.5 * t(b - mu) %*% solve(CS) %*% (b - mu)) / (2 * pi * sqrt(det(CS)))
    }
    
    dens_vals <- apply(grid, 1, f_norm)
    dens_mat <- matrix(dens_vals, nrow = 101, byrow = TRUE)
    
    plot(b0gr,b1gr,axis=F,type="n",xlab = expression(hat(beta)[0]), ylab = expression(hat(beta)[1]),
         main = "Distribuzione stimata di (hat(beta)[0], hat(beta)[1])")
    if (input$show){
      image(b0gr, b1gr, dens_mat, col = rev(gray.colors(100)),add=T)
      contour(b0gr, b1gr, dens_mat, add = TRUE, drawlabels = FALSE)
    }
    points(sapply(dati$nuovi_punti, function(p) p[1]),
           sapply(dati$nuovi_punti, function(p) p[2]),
           pch = 16, col = "blue")
    points(0, 1, pch = 3, col = "red", cex = 1.5)
  })
  
}

shinyApp(ui = ui, server = server)
