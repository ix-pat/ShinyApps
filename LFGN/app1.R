library(shiny)

ui <- fluidPage(
  titlePanel("Convergenza di Sₙ oppure Sₙ/n"),
  sidebarLayout(
    sidebarPanel(
      numericInput("pi_val", "Valore di π", min = 0, max = 1, value = 0.5, step = 0.01),
      numericInput("n", "n massimo", min = 10, max = 10000, value = 10, step = 10),
      numericInput("n0", "n per approssimazione normale", min = 30, max = 10000, value = 30, step = 10),
      checkboxInput("scala_relativa", "Sₙ/n", value = FALSE),
      checkboxInput("immagine", "Mostra densità teorica", value = FALSE),
      checkboxInput("mostra_normale", "Mostra normale a destra", value = FALSE),
      actionButton("prossimo", "Prossimo campione"),
      actionButton("azzera", "Azzera tutto")
    ),
    mainPanel(
      plotOutput("plotCondiviso", height = "600px")
    )
  )
)

server <- function(input, output) {
  set.seed(2)
  colori <- colorRampPalette(c("white", "blue", "yellow"))(100)
  tracce <- reactiveValues(lista = list())
  
  observeEvent(input$prossimo, {
    x <- rbinom(input$n, 1, input$pi_val)
    y <- if (input$scala_relativa) cumsum(x) / (1:input$n) else cumsum(x)
    tracce$lista[[length(tracce$lista) + 1]] <- y
  })
  
  observeEvent(input$azzera, {
    tracce$lista <- list()
  })
  
  tick_seq <- function(n) {
    by <- if (n <= 10) 1 else if (n <= 20) 2 else if (n <= 50) 5 else if (n <= 100) 10 else if (n <= 200) 20 else if (n <= 500) 50 else 100
    c(1, seq(by, n, by = by))
  }
  
  NN <- 500
  
  Zmat <- reactive({
    n_seq <- seq(1, input$n, length.out = NN)
    pi <- input$pi_val
    
    if (input$scala_relativa) {
      x_seq <- seq(0, 1, length.out = NN)
      
      dens_rel <- Vectorize(function(x, n) {
        mu <- pi
        sigma <- sqrt(pi * (1 - pi) / n)
        d <- dnorm(x, mean = mu, sd = sigma)
        Znorm <- pnorm(1, mean = mu, sd = sigma) - pnorm(0, mean = mu, sd = sigma)
        d / Znorm * (x >= 0 & x <= 1)
      })
      
      outer(x_seq, n_seq, dens_rel)
      
    } else {
      x_seq <- seq(0, input$n, length.out = NN)
      
      dens_abs <- Vectorize(function(x, n) {
        mu <- n * pi
        sigma <- sqrt(n * pi * (1 - pi))
        d <- dnorm(x, mean = mu, sd = sigma)
        Znorm <- pnorm(n, mean = mu, sd = sigma) - pnorm(0, mean = mu, sd = sigma)
        d / Znorm * (x >= 0 & x <= input$n)
      })
      
      outer(x_seq, n_seq, dens_abs)
    }
  })
  
  output$plotCondiviso <- renderPlot({
    layout(matrix(c(1, if (input$mostra_normale && input$n >= 30) 2 else 0), nrow = 1), widths = c(3, 1))
    par(mar = c(5, 4, 4, 1))
    
    ylab <- if (input$scala_relativa) expression(S[n]/n) else expression(S[n])
    ylim <- if (input$scala_relativa) c(0, 1) else c(0, input$n)
    ref_y <- if (input$scala_relativa) input$pi_val else input$n * input$pi_val
    ic95 <- function(n, pi) {
      se <- sqrt(n * pi * (1 - pi))
      lo <- ref_y - 1.96 * if (input$scala_relativa) se / n else se
      hi <- ref_y + 1.96 * if (input$scala_relativa) se / n else se
      c(lo, hi)
    }
    
    # Plot principale
    plot(c(1, input$n), ylim, type = "n", axes = FALSE,
         xlab = "n", ylab = ylab,
         main = if (input$scala_relativa)
           expression(paste("Convergenza di ", frac(S[n], n), " → ", pi))
         else
           expression(paste("Crescita di ", S[n], " con ", E[S[n]] == n %.% pi)))
    
    if (input$immagine) {
      n_seq <- seq(1, input$n, length.out = NN)
      y_seq <- if (input$scala_relativa) seq(0, 1, length.out = NN) else seq(0, input$n, length.out = NN)
      cat(system.time(Z<- Zmat()))
      image(x = n_seq, y = y_seq, z = t(Z), col = colori, add = TRUE)
      
    }
    
    if (input$scala_relativa) {
      abline(h = ref_y, lty = 2)
      if (input$mostra_normale && input$n >= 30) {
        abline(v = input$n0, lty = 3)
        bounds <- ic95(input$n0, input$pi_val)
        segments(input$n0, bounds[1], input$n, bounds[1], lty = 2)
        segments(input$n0, bounds[2], input$n, bounds[2], lty = 2)
      }
    } else {
      # Scala assoluta: medie e intervalli sono rette crescenti
      abline(a = 0, b = input$pi_val, lty = 2)
      if (input$mostra_normale && input$n >= 30) {
        abline(v = input$n0, lty = 3)
        pi <- input$pi_val
        n0 <- input$n0
        se <- sqrt(n0 * pi * (1 - pi))
        
        segments(n0, n0 * pi - 1.96 * se, n, n0 * pi - 1.96 * se, lty = 2)
        segments(n0, n0 * pi + 1.96 * se, n, n0 * pi + 1.96 * se, lty = 2)
        segments(n0,n0*pi,n,n0*pi,lty=2)
      }
    }
    
    yat <- if (input$scala_relativa) (0:10)/10 else tick_seq(input$n)
    axis(1, at = tick_seq(input$n))
    axis(2, yat, las = 2)
    
    for (y in tracce$lista) {
      if (length(y) == input$n) {
        lines(1:input$n, y, col = "black", lwd = 1)
        points(1:input$n, y, col = "black", pch = 16, cex = 0.4)
        if (input$n <= 10) {
          txt <- paste0(round(y * if (input$scala_relativa) 100 else 1, 2), if (input$scala_relativa) "%" else "")
          text(1:input$n, y, txt, adj = 0)
        }
      }
    }
    
    # Grafico della normale verticale
    if (input$mostra_normale && input$n >= 30) {
      par(mar = c(5, 1, 4, 4))
      y_vals <- if (input$scala_relativa) seq(0, 1, length.out = 500) else seq(0, input$n, length.out = 500)
      mu <- if (input$scala_relativa) input$pi_val else input$n0 * input$pi_val
      sigma <- sqrt(input$n0 * input$pi_val * (1 - input$pi_val))
      if (input$scala_relativa) sigma <- sigma / input$n0
      d_vals <- dnorm(y_vals, mean = mu, sd = sigma)
      Znorm <- pnorm(max(y_vals), mean = mu, sd = sigma) - pnorm(min(y_vals), mean = mu, sd = sigma)
      d_vals <- d_vals / Znorm * (y_vals >= min(y_vals) & y_vals <= max(y_vals))
      
      plot(d_vals, y_vals, type = "l", lwd = 2, col = "darkblue",
           xlab = "densità", ylab = "", axes = FALSE,
           main = bquote("Normale approssimata per n = " ~ .(input$n0)),
           xlim = c(0, max(d_vals) * 1.1), ylim = ylim)
      axis(2, las = 2)
      axis(1)
      segments(0, mu, max(d_vals), mu, col = "grey", lty = 2)
    }
  })
}

shinyApp(ui = ui, server = server)
