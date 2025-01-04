library(shiny)
library(ggplot2)

# Definire l'interfaccia utente
ui <- fluidPage(
  titlePanel("Intervalli di Confidenza"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu", "Seleziona il valore di μ:",
                  min = 0, max = 4, value = 2, step = 0.01),
      sliderInput("s", "Seleziona il valore di σ:",
                  min = 0, max = 5, value = 1.5, step = 0.01),
      numericInput("n", "Inserisci il valore di n:",
                   value = 5, min = 1, step = 1),
      sliderInput("alpha", "Seleziona il livello di confidenza (1-α):",
                  min = 0, max = 1, value = 0.95, step = 0.01),
      actionButton("generate", "Genera Nuovo xm"),
      actionButton("reset", "Azzera") # Pulsante per azzerare
    ),
    mainPanel(
      plotOutput("muPlot", width = "700px", height = "700px")
    )
  )
)

# Definire la logica del server
# Definire la logica del server
server <- function(input, output) {
  # Lista reattiva per tenere traccia dei valori xm
  xms <- reactiveValues(data = list(), count = 0)
  set.seed(1)

  # Azzera tutto
  observeEvent(input$reset, {
    xms$data <- list()
    xms$count <- 0
  })
  
  # Osserva quando il bottone viene premuto
  observeEvent(input$generate, {
    xms$count <- xms$count + 1
    xms$data[[xms$count]] <- rnorm(1, input$mu, input$s/sqrt(input$n))
    # Non c'è bisogno di chiamare intc qui poiché il grafico verrà aggiornato automaticamente
  })

  # Renderizza il grafico
  output$muPlot <- renderPlot({
    n <- input$n
    s <- input$s
    mu <- input$mu
    s2 <- s^2
    se <- sqrt(s2/n)
    mumax <- 5
    alpha <- input$alpha
    za2 <- round(qnorm((1 - alpha) / 2), 4)
    mug <- seq(-1, mumax, length.out = 4)
    xbar <- mug

    # Grafico iniziale
    plot(mug, xbar, axes = FALSE, asp = 1, xlab = "", ylab = "", type='l')
    arrows(-1, 0, mumax, 0, length = .1)
    arrows(0, -1, 0, mumax, length = .1)
    text(mumax-.5, -.2, expression(mu))
    text(-.2, mumax-.5, expression(bar(x)))
    
    lines(mug, xbar + za2 * se, lty = 3)
    lines(mug, xbar - za2 * se, lty = 3)
    
    segments(mu, -mumax, mu, mumax)
    segments(mu, mu - za2 * se, mu, mu + za2 * se, lwd = 2, col = "red")
    text(mu + .2, -.2, mu)
    segments(mu, mu - za2 * se, 0, mu - za2 * se, lty = 2)
    segments(mu, mu + za2 * se, 0, mu + za2 * se, lty = 2)
    segments(mu, mu, 0, mu, lty = 2)
    text(-.3, mu - za2 * se, round(mu - za2 * se, 3))
    text(-.3, mu + za2 * se, round(mu + za2 * se, 3))
    text(-.3, mu, mu)
    
    
    # Aggiungi le nuove parti al grafico per ogni xm
    for (i in seq_along(xms$data)) {
      intc(xms$data[[i]], i, za2, mu, se)
    }
  })
}

intc <- function(xbar,i,za2,mu,se){
  mumax <- 5
  ex <- substitute(expression(mu[obs]^(i)==xbar),list(i=i,xbar=round(xbar,2)))
  mug <- seq(-1,mumax,length.out = 4)
  xg <- mug
  
  arrows(-1,0,mumax,0,length = .1)
  arrows(0,-1,0,mumax,length = .1)
#   text(mumax+.1,+.2,expression(mu))
#   text(-.2,mumax-.5,expression(hat(mu)))
# 
# 
  points(0,xbar,pch=4,cex=.8,col=i)
  text(-.5,xbar,eval(ex))
# 
  segments(0,mu-za2*se,mu,col='grey',lty=3)
  segments(0,mu+za2*se,mu,col='grey',lty=3)
  segments(0,xbar,xbar,xbar,col='grey',lty=3)
  segments(xbar,xbar,xbar,0,col='grey',lty=3)
# 
#   segments(mu,-mumax,mu,mumax,col='grey30')
 segments(mu,mu-za2*se,mu,mu+za2*se,lwd=1,col="red")
#   # segments(0,mu-za2*se,0,mu+za2*se,lwd=1,col="red")
  segments(xbar-za2*se,0,xbar-za2*se,xbar,lty=2)
  segments(xbar+za2*se,0,xbar+za2*se,xbar,lty=2)
  segments(xbar        ,0,xbar        ,xbar,lty=2)
   segments(xbar-za2*se,0,xbar+za2*se,col=i,lwd=2)
   segments(xbar-za2*se,xbar,xbar+za2*se,col=1,lwd=2)
#   text(xbar+.4,.2,eval(ex))
   points(xbar,0,pch=4,col=i,cex=.8)
   text(xbar-za2*se, -.3, round(xbar-za2*se,2))
   text(xbar+za2*se, -.3, round(xbar+za2*se,2))
}

# Eseguire l'applicazione
shinyApp(ui = ui, server = server)
