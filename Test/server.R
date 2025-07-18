
source("tests.R")
# Logica del server
server <- function(input, output) {
  
  # Genera input dinamici in base alla scelta
  output$input_dinamico <- renderUI({
    if(input$scelta == 'Test Z per una media') {
      tagList(
        numericInput("xm", "x medio:", value = 11),
        numericInput("mu0", "mu0:", value = 10),
        numericInput("sig2", "sigma²:", value = 1.5),
        numericInput("n", "n", value = 10),
        selectInput("alpha", "Livello di significatività",choices = list(0.05,0.01,0.005,0.001)),        
        selectInput("verso",label = "Scegli il verso",choices = list("<" = "<", "diverso" = "\\neq", ">" = ">"),selected = "\\neq")
        # Aggiungi altri input specifici per l'Opzione 1 qui
      )
    } else if (input$scelta == 'Test Z per una proporzione') {
      tagList(
        numericInput("sn", "sn",value = 6),
        numericInput("n", "n",value = 10),
        numericInput("pi0", "pi0",min = 0,value = .5,max = 1,step = .1),
        selectInput("alpha", "Livello di significatività",choices = list(0.05,0.01,0.005,0.001)),        
        selectInput("verso",label = "Scegli il verso",choices = list("<" = "<", "diverso" = "\\neq", ">" = ">"),selected = "\\neq")
      )
    } else if (input$scelta == 'Test t per una media') {
      tagList(
        numericInput("muh", "x medio",value = 10),
        numericInput("sh", "sigma campionario",value = 1),
        numericInput("n", "n",value = 10),
        numericInput("mu0", "mu0",value = 9),
        selectInput("alpha", "Livello di significatività",choices = list(0.05,0.01,0.005,0.001)),        
        selectInput("verso",label = "Scegli il verso",choices = list("<" = "<", "diverso" = "\\neq", ">" = ">"),selected = "\\neq")
      )
    } else if (input$scelta == 'Test t per due campioni') {
      tagList(
        checkboxInput(inputId = "et",label = "eterogeneità",value = TRUE),
        numericInput("mu1", "x medio 1",value = 10),
        numericInput("mu2", "x medio 2",value = 11),
        numericInput("s1h", "sigma campionario",value = 1),
        numericInput("s2h", "sigma campionario",value = 2),
        numericInput("n1", "n1",value = 10),
        numericInput("n2", "n2",value = 10),
        selectInput("alpha", "Livello di significatività",choices = list(0.05,0.01,0.005,0.001)),        
        selectInput("verso",label = "Scegli il verso",choices = list("<" = "<", "diverso" = "\\neq", ">" = ">"),selected = "\\neq")
      )
    } else if (input$scelta == 'Test Z per due proporzioni') {
      tagList(
        checkboxInput(inputId = "et",label = "eterogeneità",value = TRUE),
        numericInput("s1", "S1",value = 10),
        numericInput("s2", "S2",value = 11),
        numericInput("n1", "n1",value = 50),
        numericInput("n2", "n2",value = 60),
        selectInput("alpha", "Livello di significatività",choices = list(0.05,0.01,0.005,0.001)),        
        selectInput("verso",label = "Scegli il verso",choices = list("<" = "<", "diverso" = "\\neq", ">" = ">"),selected = "\\neq")
      )
    }
  })

  # Esempio di output basato sugli input dinamici
  output$risultato <- renderUI({
    if(input$scelta == 'Test Z per una media') {
      n <- input$n
      muh <- input$xm
      mu0 <- input$mu0
      s  <- sqrt(input$sig2)
      h1 <- input$verso
      alpha <- as.numeric(input$alpha)
      withMathJax(paste(ztest_mu(muh,mu0,s,n,h1,alpha)))
    } else if (input$scelta == 'Test Z per una proporzione') {
      n <- input$n
      sn <- input$sn
      p0 <- input$pi0
      h1 <- input$verso
      alpha <- as.numeric(input$alpha)      
      withMathJax(paste(ztest_pi(sn = sn,n = n,p0 = p0,h1 = h1,alpha = alpha),sep="\n\n"))
    } else if (input$scelta == 'Test t per una media') {
      muh <- input$muh
      n <- input$n
      sh <- input$sh
      mu0 <- input$mu0
      h1 <- input$verso
      alpha <- as.numeric(input$alpha)
      withMathJax(paste(ttest(muh = muh,sh = sh,n = n,mu0 = mu0,h1 = h1,alpha = alpha)))
    } else if (input$scelta == 'Test Z per due proporzioni') {
      n1 <- input$n1
      n2 <- input$n2
      s1 <- input$s1
      s2 <- input$s2
      h1 <- input$verso
      alpha <- as.numeric(input$alpha)
      withMathJax(paste(ztest2pi(n1 = n1,n2 = n2,s1 = s1,s2 = s2,h1 = h1,alpha = alpha)))
    } else if (input$scelta == 'Test t per due campioni') {
      mu1 <- input$mu1
      mu2 <- input$mu2
      n1 <- input$n1
      n2 <- input$n2
      s1h <- input$s1h
      s2h <- input$s2h
      h1 <- input$verso
      et <- input$et
      alpha <- as.numeric(input$alpha)      
      if (et) withMathJax(paste(ttest_2e(mu1 = mu1,mu2 = mu2,s1h = s1h,s2h = s2h,n1 = n1,n2 = n2,h1 = h1,alpha = alpha))) else
        withMathJax(paste(ttest_2c(mu1 = mu1,mu2 = mu2,s1h = s1h,s2h = s2h,n1 = n1,n2 = n2,h1 = h1,alpha = alpha)))
    }  
  })
  output$grf <- renderPlot({
    if(input$scelta == 'Test Z per una media') {
      n <- input$n
      muh <- input$xm
      mu0 <- input$mu0
      s  <- sqrt(input$sig2)
      h1 <- input$verso
      alpha <- as.numeric(input$alpha)
      withMathJax(paste(ztest_mu(muh,mu0,s,n,h1,alpha)))
    } else if (input$scelta == 'Test Z per una proporzione') {
      n <- input$n
      sn <- input$sn
      p0 <- input$pi0
      h1 <- input$verso
      alpha <- as.numeric(input$alpha)      
      withMathJax(paste(ztest_pi(sn = sn,n = n,p0 = p0,h1 = h1,alpha = alpha),sep="\n\n"))
    } else if (input$scelta == 'Test t per una media') {
      muh <- input$muh
      n <- input$n
      sh <- input$sh
      mu0 <- input$mu0
      h1 <- input$verso
      alpha <- as.numeric(input$alpha)
      withMathJax(paste(ttest(muh = muh,sh = sh,n = n,mu0 = mu0,h1 = h1,alpha = alpha)))
    } else if (input$scelta == 'Test Z per due proporzioni') {
      n1 <- input$n1
      n2 <- input$n2
      s1 <- input$s1
      s2 <- input$s2
      h1 <- input$verso
      alpha <- as.numeric(input$alpha)
      withMathJax(paste(ztest2pi(n1 = n1,n2 = n2,s1 = s1,s2 = s2,h1 = h1,alpha = alpha)))
    } else if (input$scelta == 'Test t per due campioni') {
      mu1 <- input$mu1
      mu2 <- input$mu2
      n1 <- input$n1
      n2 <- input$n2
      s1h <- input$s1h
      s2h <- input$s2h
      h1 <- input$verso
      et <- input$et
      alpha <- as.numeric(input$alpha)      
      if (et) withMathJax(paste(ttest_2e(mu1 = mu1,mu2 = mu2,s1h = s1h,s2h = s2h,n1 = n1,n2 = n2,h1 = h1,alpha = alpha))) else
        withMathJax(paste(ttest_2c(mu1 = mu1,mu2 = mu2,s1h = s1h,s2h = s2h,n1 = n1,n2 = n2,h1 = h1,alpha = alpha)))
    }  
  })
}

