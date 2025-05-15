library(shiny)
library(ggplot2)

ui <- navbarPage("App Statistiche",
                 tabPanel("Campionamento",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("mu2", "Media popolazione:", value = 175),
                              numericInput("sg2", "Deviazione standard:", value = 15, min = 0.01),
                              numericInput("n2", "Campione n:", value = 30, min = 1),
                              actionButton("go1", "CAMPIONA"),
                              actionButton("go100", "CAMPIONA 100 volte"),
                              actionButton("reset2", "AZZERA")
                            ),
                            mainPanel(plotOutput("plt"))
                          )
                 ),
                 tabPanel("Intervalli di Confidenza",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("mu", "Seleziona il valore di μ:", min = 0, max = 4, value = 2, step = 0.01),
                              sliderInput("s", "Seleziona il valore di σ:", min = 0, max = 5, value = 1.5, step = 0.01),
                              numericInput("n", "Inserisci il valore di n:", value = 5, min = 1, step = 1),
                              sliderInput("alpha", "Seleziona il livello di confidenza (1-α):", min = 0, max = 1, value = 0.95, step = 0.01),
                              actionButton("generate", "Genera Nuovo xm"),
                              actionButton("reset1", "Azzera")
                            ),
                            mainPanel(plotOutput("muPlot", width = "700px", height = "700px"))
                          )
                 )
)

server <- function(input, output, session) {
  # TAB 1
  v <- reactiveValues(mx = numeric(), md = numeric())
  observeEvent(input$reset2, {
    v$mx <- numeric(); v$md <- numeric()
  })
  observeEvent(input$go1, {
    x <- rnorm(input$n2, input$mu2, input$sg2)
    v$mx <- c(v$mx, mean(x)); v$md <- c(v$md, median(x))
  })
  observeEvent(input$go100, {
    v$mx <- c(v$mx, replicate(100, mean(rnorm(input$n2, input$mu2, input$sg2))))
    v$md <- c(v$md, replicate(100, median(rnorm(input$n2, input$mu2, input$sg2))))
  })
  output$plt <- renderPlot({
    if (length(v$mx) == 0) return()
    mu <- input$mu2; sg <- input$sg2; n <- input$n2; N <- length(v$mx)
    plot(c(mu-5*sg/sqrt(n), mu+5*sg/sqrt(n)), c(0, dnorm(mu, mu, sg/sqrt(n))),
         xlab = "Media-Mediana", ylab = "Densità", type="n", axes=F)
    axis(1, round(c(mu-5*sg/sqrt(n), mu, mu+5*sg/sqrt(n)), 2))
    legend(mu-4*sg/sqrt(n), dnorm(mu, mu, sg/sqrt(n)), c("media", "mediana"), pch=16:17, col = c("red", "blue"))
    title(paste("Campione", N))
    points(c(v$mx, v$md), rep(0, 2*N), col = "grey", pch = rep(c(16, 17), each = N))
    points(tail(v$mx, 1), 0, col = "red", pch = 16)
    points(tail(v$md, 1), 0, col = "blue", pch = 17)
    if (length(v$mx) > 100) {
      d_mx <- density(v$mx, adjust = 5)
      d_md <- density(v$md, adjust = 5)
      par(new = TRUE)
      plot(d_mx, col = rgb(1, 0, 0, 0.5), lwd = 2, axes = FALSE,
           xlab = "", ylab = "", main = "")
      lines(d_md, col = rgb(0, 0, 1, 0.5), lwd = 2)
      axis(4); mtext("Densità", side = 4, line = 3)
    }
  })
  
  # TAB 2
  xms <- reactiveValues(data = list(), count = 0)
  observeEvent(input$reset1, {
    xms$data <- list()
    xms$count <- 0
  })
  observeEvent(input$generate, {
    xms$count <- xms$count + 1
    xms$data[[xms$count]] <- rnorm(1, input$mu, input$s/sqrt(input$n))
  })
  output$muPlot <- renderPlot({
    n <- input$n; s <- input$s; mu <- input$mu
    se <- sqrt(s^2/n); alpha <- input$alpha
    za2 <- round(qnorm((1 - alpha) / 2), 4)
    mug <- seq(-1, 5, length.out = 4); xbar <- mug
    plot(mug, xbar, axes = FALSE, asp = 1, xlab = "", ylab = "", type='l')
    arrows(-1, 0, 5, 0, length = .1); arrows(0, -1, 0, 5, length = .1)
    text(4.5, -.2, expression(mu)); text(-.2, 4.5, expression(bar(x)))
    lines(mug, xbar + za2 * se, lty = 3); lines(mug, xbar - za2 * se, lty = 3)
    segments(mu, -5, mu, 5)
    segments(mu, mu - za2 * se, mu, mu + za2 * se, lwd = 2, col = "red")
    text(mu + .2, -.2, mu)
    segments(mu, mu - za2 * se, 0, mu - za2 * se, lty = 2)
    segments(mu, mu + za2 * se, 0, mu + za2 * se, lty = 2)
    segments(mu, mu, 0, mu, lty = 2)
    text(-.3, mu - za2 * se, round(mu - za2 * se, 3))
    text(-.3, mu + za2 * se, round(mu + za2 * se, 3))
    text(-.3, mu, mu)
    for (i in seq_along(xms$data)) intc(xms$data[[i]], i, za2, mu, se)
  })
}

intc <- function(xbar, i, za2, mu, se){
  arrows(-1,0,5,0,length = .1); arrows(0,-1,0,5,length = .1)
  points(0, xbar, pch=4, cex=.8, col=i)
  text(-.5, xbar, bquote(mu["obs"]^.(i) == .(round(xbar,2))))
  segments(0, mu-za2*se, mu, col='grey', lty=3)
  segments(0, mu+za2*se, mu, col='grey', lty=3)
  segments(0, xbar, xbar, xbar, col='grey', lty=3)
  segments(xbar, xbar, xbar, 0, col='grey', lty=3)
  segments(mu, mu-za2*se, mu, mu+za2*se, lwd=1, col="red")
  segments(xbar-za2*se, 0, xbar-za2*se, xbar, lty=2)
  segments(xbar+za2*se, 0, xbar+za2*se, xbar, lty=2)
  segments(xbar, 0, xbar, xbar, lty=2)
  segments(xbar-za2*se, xbar, xbar+za2*se, col=i, lwd=2)
  segments(xbar-za2*se, 0, xbar+za2*se, col=1, lwd=2)
  points(xbar, 0, pch=4, col=i, cex=.8)
  text(xbar-za2*se, -.3, round(xbar-za2*se,2))
  text(xbar+za2*se, -.3, round(xbar+za2*se,2))
}

shinyApp(ui, server)