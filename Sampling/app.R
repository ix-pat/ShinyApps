library(shiny)
library(data.table)


ui <- fluidPage(
  titlePanel("Campionamento"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num", "Inserisci il numero di campioni:", min = 1, value = 1),
      "Eventuali parametri per generare i dati",
      fluidRow(
        column(3, numericInput("n", "n:", min = 1, value = 10)),
        column(3, numericInput("pi_", "pi:", min = 0, max = 1, value = 0,step=.01)),
        column(3, numericInput("mu", "mu:", value = 0,step=.1)),
        column(3, numericInput("sigma", "sigma:", min = 0, value = 0,step=.1))
      ),
      fluidRow(
      column(4,actionButton("updateButton", "Aggiorna Dati")),
      column(4,checkboxInput("seed", "Genera uguale", value = TRUE)),
      column(4,checkboxInput("rand", "Casuali?", value = FALSE))
      ),
      uiOutput("dynamicInput")
    ),
    mainPanel(
      tableOutput("data"),
      plotOutput("graf")
    )
  )
)

server <- function(input, output, session) {
  datiReattivi <- reactiveValues(dati = NULL, inputs = list())
  observe({
    M <- input$num
    k <- 4  
    n <- input$n
    pi_ <- input$pi_
    mu <- input$mu
    sigma <- input$sigma
    casuali <- input$rand
    headers <- c("n", "k", "Somma x", "Somma x²")  # Personalizza queste intestazioni
    
    output$dynamicInput <- renderUI({
      if (input$seed) set.seed(1)
      x <- matrix(rnorm(100*n,mu,sigma),ncol = n)
      if (M<=100){
        lapply(M:1, function(i) {
          if (casuali){
          fluidRow(
          column(1, HTML(as.character(i))),
          lapply(1:k, function(j) {
            column(2, numericInput(paste("value", i, j, sep = "_"), label = headers[j],
                                value = c(n,rbinom(1,n,pi_),round(sum(x[i,]),2),round(sum(x[i,]^2),2))[j]))  
          })
        )} else {
          fluidRow(
          column(1, HTML(as.character(i))),
          lapply(1:k, function(j) {
            column(2, numericInput(paste("value", i, j, sep = "_"), label = headers[j],
                                value = c(n,0,0,0)[j]))  
          })
        )}
      })}  else {return(NULL)}
    })  
  })
  
  observeEvent(input$updateButton, {
    # Codice per calcolare 'dati'
    M <- input$num
    k <- 4
    n <- input$n
    pi_ <- input$pi_
    mu <- input$mu
    sigma <- input$sigma
    if (M <= 100){
    input_ids <- outer(1:M, 1:k, function(i, j) paste("value", i, j, sep = "_"))
    data_matrix <- matrix(sapply(input_ids, function(x) input[[x]]), nrow = M, ncol = k)
    } else {
      x <- matrix(rnorm(M*n,mu,sigma),nrow = M)
      data_matrix <- matrix(nrow = M,ncol = 4)
      data_matrix[,1] <- n
      data_matrix[,2] <- rbinom(M,n,pi_)
      data_matrix[,3] <- rowSums(x)
      data_matrix[,4] <- rowSums(x^2)
    }
    dati <- data.frame(1:M, data_matrix)
    names(dati) <-  c("Campione", "n", "k", "sx", "sx2")  # Personalizza queste intestazioni
    dati$pi_stimato <- dati$k/dati$n
    dati$media <- dati$sx/dati$n
    dati$SD <- sqrt((dati$sx2/dati$n-(dati$sx/dati$n)^2)*dati$n/(dati$n-1))
    dati$Var <- (dati$sx2/dati$n-(dati$sx/dati$n)^2)*dati$n/(dati$n-1)
    dati$n <- as.integer(dati$n)
    dati$k <- as.integer(dati$k)
    new_row <- list(Campione="Stat",
                    n=sum(dati$n),
                    k=sum(dati$k),
                    sn=NA,
                    sx2=NA,
                    pi_stimato=sum(dati$k)/sum(dati$n),
                    media=mean(dati$media),
                    SD=sqrt(mean(dati$Var)),
                    vr=mean(dati$Var)
                  )
    dati[M+1,]  <- new_row

    datiReattivi$dati <- dati
  })
  
  
  output$data <- renderTable({
    M <- dim(datiReattivi$dati)[1]-1
    nrig <- max((M-9),1):(M+1)
    if (M <= 100) datiReattivi$dati[nrig,c(1:3,6:8)] else datiReattivi$dati[M+1,c(1,6:8)]
  })
  output$graf <- renderPlot({
    M <- dim(datiReattivi$dati)[1]-1
    dati <- datiReattivi$dati
    mus  <- dati$media[M+1]
    sigs <- dati$SD[M+1]
    dati <- dati[1:M,]
    
    n <-dati$n[1]
    par(mfrow=c(1,2))

    hist(dati$pi_stimato,breaks = (0:(n+1))/n-1/(2*n),axes=F,col="lightblue4",xlab="Percentuale del campione",main = "Istogramma",ylab="frequenza",xlim=c(-.05,1.05))
    axis(1,(0:10)/10)
    axis(2)
    if (M > 100) {
      mu <- input$mu
      sigma <- input$sigma
      hist(dati$media,breaks = seq(mu-5*sigma/sqrt(n),mu+5*sigma/sqrt(n),length=min(M,100)),axes=F,col="lightblue4",xlab="Media del campione",main = "Istogramma",ylab="frequenza")
    } else {
      hist(dati$media,breaks = seq(mus-5*sigs/sqrt(n),mus+5*sigs/sqrt(n),length=min(M,100)),axes=F,col="lightblue4",xlab="Media del campione",main = "Istogramma",ylab="frequenza")
      }
    axis(2)
    if (M <= 100) axis(1) else axis(1,round(seq(mu-4*sigma/sqrt(n),mu+4*sigma/sqrt(n),length=5),2))
  })
}

shinyApp(ui, server)
