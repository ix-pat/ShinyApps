library(shiny)

ui <- fluidPage(
  titlePanel("Piano di Regressione"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("theta", "Angolo orizzontale (theta):", min = -180, max = 180, value = -35),
      sliderInput("phi", "Angolo verticale (phi):", min = 0, max = 90, value = 30),
      sliderInput("zoom", "Espansione:", min = 0.2, max = 1.5, value = 0.5, step = 0.05),
      sliderInput("zoom2", "zoom", min = 0, max = .3, value = 0, step = 0.01),
    ),
    mainPanel(
      plotOutput("perspPlot")
    )
  )
)

server <- function(input, output) {
  output$perspPlot <- renderPlot({
    set.seed(0)
    n <- 50
    x1 <- runif(n, 0, 10)
    x2 <- runif(n, 0, 10)
    beta0 <- 5; beta1 <- +.8; beta2 <- +1.2
    eps <- rnorm(n,0,2)
    y <- beta0 + beta1*x1 + beta2*x2 + eps
    yhat <- beta0 + beta1*x1 + beta2*x2
    # Griglia del piano
    xg <- seq(0, 10, length=30)
    yg <- seq(0, 10, length=30)
    zmat <- outer(xg, yg, function(x, y) beta0 + beta1*x + beta2*y)
    zoom <- input$zoom2
    xlim <- c(zoom*10,(1-zoom)*10)
    ylim <- c(zoom*10,(1-zoom)*10)
    zlim <- c(zoom*30,(1-zoom)*30)
    
    
    pmat <- persp(xg, yg, zmat,box=F,xlim=xlim,ylim=ylim,zlim = zlim,
          theta = input$theta,
          phi = input$phi,
          expand = input$zoom,
          shade = .05,
          col="white", border=1,
          xlab = "", ylab = "", zlab = "")
    text(trans3d(9, -1, 0, pmat), labels = expression(x[1]),cex=1,col=2)
    text(trans3d(-1, 9, 0, pmat), labels = expression(x[2]),cex=1,col=2)
    text(trans3d(0.3, 0, 32, pmat), labels = expression(y), cex=1,col=2)
    text(trans3d(10, 10, 30, pmat), labels = expression(mu(x[1], x[2])),cex=1,col=2)
    arrows(trans3d(0,0,0,pmat)$x,trans3d(0,0,0,pmat)$y,trans3d(0,10,0,pmat)$x,trans3d(0,10,0,pmat)$y,length = .1)
    arrows(trans3d(0,0,0,pmat)$x,trans3d(0,0,0,pmat)$y,trans3d(10,0,0,pmat)$x,trans3d(10,0,0,pmat)$y,length = .1)
    arrows(trans3d(0,0,0,pmat)$x,trans3d(0,0,0,pmat)$y,trans3d(0,0,35,pmat)$x,trans3d(0,0,35,pmat)$y,length = .1)
    
    pt1 <- trans3d(x1, x2, y, pmat)
    pt2 <- trans3d(x1, x2, yhat, pmat)
    
    points(pt1$x[y<yhat],pt1$y[y<yhat], pch=16,col="red")
    points(pt1$x[y>yhat],pt1$y[y>yhat], pch=16,col="blue")
    segments(pt1$x[y<yhat], pt1$y[y<yhat], pt2$x[y<yhat], pt2$y[y<yhat], col="red",lty=3)
    segments(pt1$x[y>yhat], pt1$y[y>yhat], pt2$x[y>yhat], pt2$y[y>yhat], col="blue",lty=3)
    
  })
}

shinyApp(ui, server)
