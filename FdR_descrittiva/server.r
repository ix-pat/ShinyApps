# Definisci il server
source("script.R")
library(knitr)
library(kableExtra)
options(knitr.kable.NA = '')

brk_def <- c(0,3,5,10,20)
nnn_def <- c(9,16,10,10)

server <- function(input, output, session) {
  
  observeEvent(input$submit_k, {
    k <- as.integer(input$k_value)
    
    output$dynamicBrkInput <- renderUI({
      brkInputs <- list()
      def_brk <- NA+numeric(100)
      def_nnn <- NA+numeric(100)
      def_brk[1:5] <- c(0,3,5,10,20)
      def_nnn[1:4] <- c(9,16,10,10)
      # Prima riga con brk[1] e brk[2]
      brkInputs[[1]] <- fluidRow(
        column(4, numericInput("brk_1", "x_1:", value = def_brk[1])),
        column(4, numericInput("brk_2", "x_2:", value = def_brk[2])),
        column(4, numericInput("nnn_1", "n_1:", value = def_nnn[1]))
      )
      
      # Genera dinamicamente le righe successive con brk[i-1] mostrato come testo e brk[i] come input
      for (i in 3:(k+1)) {
        local({
        output[[paste0("d_brk", i-1)]] <- renderText("__")
        })
        brkInputs[[i-1]] <- fluidRow(
          column(4, textOutput(outputId = paste0("d_brk", i-1))),
          column(4, numericInput(inputId = paste0("brk_", i), label = paste0("x_", i), value = def_brk[i])),
          column(4, numericInput(inputId = paste0("nnn_", i-1), label = paste0("n_", i-1), value = def_nnn[i-1]))
        )
      }
      
      do.call(tagList, brkInputs)
    })
    
    # Mostra il valore di brk[i-1] nelle colonne di testo
    for (i in 2:(k+1)) {
      output[[paste0("displayBrk", i-1)]] <- renderText({
        input[[paste0("brk_", i-1)]]
      })
    }
  })  
  output$range_slider <- renderUI({
    brk_values <- sapply(1:(as.integer(input$k_value)+1), function(j) input[[paste0("brk_", j)]])
    if (length(brk_values) > 0 && all(!is.na(brk_values))) {
      min_brk <- round(min(brk_values) - (max(brk_values)-min(brk_values))/10,2)
      max_brk <- round(max(brk_values) + (max(brk_values)-min(brk_values))/10,2)
      sliderInput("range", "Seleziona x1 e x2:", 
                  min = min_brk, max = max_brk, 
                  value = c(min_brk, max_brk),
                  step = 0.01,round = -2)
    }
  })


  
  checkOrder <- reactive({
    brk_values <- sapply(1:(as.integer(input$k_value)+1), function(j) input[[paste0("brk_", j)]])
    all(diff(brk_values) > 0)
  })
  
  samp_reactive <- reactive({
    req(checkOrder())  # Assicurati che i valori siano in ordine crescente
    brk_values <- sapply(1:(as.integer(input$k_value)+1), function(j) input[[paste0("brk_", j)]])
    nnn_values <- sapply(1:as.integer(input$k_value), function(j) input[[paste0("nnn_", j)]])
    n <- sum(nnn_values)
    
    # Chiama la funzione genera_dati
    genera_dati(brk = brk_values, nnn = nnn_values)
  })
  
  output$error_message <- renderText({
    req(input$submit_k)
    if (!checkOrder()) {
      "Errore: i valori brk devono essere in ordine crescente (brk[j] < brk[j+1])."
    } else {
      NULL
    }
  })
  
  output$values_output <- renderText({
    req(input$submit_k)
    req(checkOrder())
    brk_values <- sapply(1:(as.integer(input$k_value)+1), function(j) input[[paste0("brk_", j)]])
    nnn_values <- sapply(1:as.integer(input$k_value), function(j) input[[paste0("nnn_", j)]])
    paste("Valori brk in ordine crescente:", paste(brk_values, collapse = ", "),
          "\nValori nnn:", paste(nnn_values, collapse = ", "))
  })
  output$dens <- renderPlot({
    req(checkOrder())  # Assicurati che i valori siano in ordine crescente
    per <- input$perc_
    pp <- input$pp
    brk_values <- sapply(1:(as.integer(input$k_value)+1), function(j) input[[paste0("brk_", j)]])
    nnn_values <- sapply(1:as.integer(input$k_value), function(j) input[[paste0("nnn_", j)]])
#    req(input$range_slider)  # Assicurati che i valori dello slider siano stati selezionati
    x <- input$range
    dens(brk_values,nnn_values,x,per,pp)
  })
  output$rip <- renderPlot({
    req(checkOrder())  # Assicurati che i valori siano in ordine crescente
    per <- input$perc_
    pp <- input$pp
    brk_values <- sapply(1:(as.integer(input$k_value)+1), function(j) input[[paste0("brk_", j)]])
    nnn_values <- sapply(1:as.integer(input$k_value), function(j) input[[paste0("nnn_", j)]])
#    req(input$range_slider)  # Assicurati che i valori dello slider siano stati selezionati
    x <- input$range
    rip(brk_values,nnn_values,x,per,pp)
  })
  output$mon <- renderText({ 
    paste("ver 1.0 Patrizio Frederic, 2024", "oggi è il", date())
  })
  output$tab2 <- renderTable({
    # req(checkOrder())  # Assicurati che i valori siano in ordine crescente
    brk <- isolate(unlist(sapply(1:(as.integer(input$k_value)+1), function(j) input[[paste0("brk_", j)]])))
    nnn <- isolate(unlist(sapply(1:as.integer(input$k_value), function(j) input[[paste0("nnn_", j)]])))
    n <- sum(nnn)
    # req(samp_reactive())  # Assicurati che i dati siano disponibili
    samp <- samp_reactive()
    source("stat-base.R",local = T)
    names(dat3)[1] <- "$[\\text{x}_j,$"
    names(dat3)<- paste("$",names(dat3),"$",sep="")
    names(dat3) <- names(dat2)
    kable(dat3[,c(1:7)],booktabs=T,escape = F,linesep="",digits = 4) %>%
      kable_styling(full_width = F, latex_options = "HOLD_position")
    # dat3[,1:7]
  }, sanitize.text.function = function(x) x)
  
  output$F_p <- renderUI({
    brk <- isolate(unlist(sapply(1:(as.integer(input$k_value)+1), function(j) input[[paste0("brk_", j)]])))
    nnn <- isolate(unlist(sapply(1:as.integer(input$k_value), function(j) input[[paste0("nnn_", j)]])))
    n <- sum(nnn)
    samp <- samp_reactive()
    source("stat-base.R",local = T)
    pp <- input$pp
    x <- input$range[1]
    x2 <- input$range[2]
    if (x <  min(brk) & x2 <= max(brk)) withMathJax(paste("$$",F_print(x = x2,verso = "<"),"$$\n\n","$$",percentile(pp),"$$")) else
    if (x >= min(brk) & x2 >  max(brk)) withMathJax(paste("$$",F_print(x = x,verso = ">"),"$$\n\n","$$",percentile(pp),"$$")) else 
    {
      x <- ifelse(x<min(brk),min(brk),x)
      x2 <- ifelse(x2>max(brk),max(brk),x2)
      withMathJax(paste("$$",F_print(x = x,x2=x2,verso = ""),"$$\n\n","$$",percentile(pp),"$$"))
    }
  })  
  
  output$calcolo_perc <- renderUI({
    brk <- isolate(unlist(sapply(1:(as.integer(input$k_value)+1), function(j) input[[paste0("brk_", j)]])))
    nnn <- isolate(unlist(sapply(1:as.integer(input$k_value), function(j) input[[paste0("nnn_", j)]])))
    n <- sum(nnn)
    samp <- samp_reactive()
    pp <- input$pp
    source("stat-base.R",local = T)
    withMathJax(paste("$$",percentile(pp),"$$")) 
  })

  # output$tab1 <- renderTable({  
  #   brk <- sapply(1:(as.integer(input$k_value)+1), function(j) input[[paste0("brk_", j)]])
  #   nnn <- sapply(1:as.integer(input$k_value), function(j) input[[paste0("nnn_", j)]])
  #   #samp <- runif(10,brk[1],max(brk))
  #   df_ <- data.frame(A = c("$$\\alpha+\\beta$$", 33.1, 6),B = c(111111, 3333333, 3123.233))  
  #   names(df_) <- c("$$[\\text{x}_j,$$","$$\\text{x}_{j+1})$$")
  #   df_  
  # }, sanitize.text.function = function(x) x)
}