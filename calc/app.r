library(shiny)
library(shinyAce)
library(ggplot2)
library(gridExtra)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
        body {
            background-color: #343a40;
            color: #f8f9fa;
        }
        .shiny-input-container {
            width: 100% !important;
            color: #f8f9fa;
        }
        .form-group {
            margin-bottom: 10px;
        }
        .btn {
            width: 100%;
            background-color: #007bff;
            color: white;
        }
        .btn:hover {
            background-color: #0056b3;
        }
        .well {
            background-color: #495057;
            border: 1px solid #6c757d;
            box-shadow: 0 1px 1px rgba(0,0,0,.05);
        }
        .panel-heading {
            background-color: #007bff !important;
            color: white !important;
        }
        .panel-body {
            background-color: #343a40;
            color: #f8f9fa;
        }
        h3 {
            color: #f8f9fa;
        }
        label {
            color: #f8f9fa;
        }
        .shiny-text-output {
            color: #f8f9fa;
        }
    "))
  ),
  
  titlePanel("Calcolatrice Statistica"),
  tabsetPanel(
    tabPanel("Calcoli Statistici",
             sidebarLayout(
               sidebarPanel(
                 selectInput("dist", "Seleziona distribuzione:",
                             choices = list("Z" = "norm",
                                            "t" = "t",
                                            "Chi²" = "chisq")),
                 selectInput("func", "Seleziona funzione:",
                             choices = list("F" = "p", 
                                            "Q" = "q" )),
                 conditionalPanel(
                   condition = "input.dist == 't' || input.dist == 'chisq'",
                   numericInput("df", "Gradi di libertà:", value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.func == 'q'",
                   numericInput("prob", "Probabilità:", value = 0.5, min = 0, max = 1, step = 0.0001)
                 ),
                 conditionalPanel(
                   condition = "input.func == 'p'",
                   numericInput("value", "Valore:", value = 0,step = 0.01)
                 ),
                 actionButton("calc", "Calcola")
               ),
               mainPanel(
                 h3("Risultato"),
                 uiOutput("result"),
                 h3("Formula teorica"),
                 uiOutput("formula"),
                 h3("Formula con i numeri inseriti"),
                 uiOutput("formula_with_values"),
                 plotOutput("density_plot"),
                 plotOutput("distribution_plot")
               )
             )
    ),
    tabPanel("Editor di codice R",
             sidebarLayout(
               sidebarPanel(
                 aceEditor("code", mode = "r", theme = "solarized_dark", height = "300px",fontSize = 15),
                 actionButton("run_code", "Esegui Codice")
               ),
               mainPanel(
                 h3("Risultato dell'esecuzione"),
                 textOutput("code_output")
               )
             )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calc, {
    dist <- input$dist
    func <- input$func
    df <- ifelse(is.null(input$df), NA, input$df)
    prob <- ifelse(is.null(input$prob), NA, input$prob)
    value <- ifelse(is.null(input$value), NA, input$value)
    
    result <- switch(paste0(func, dist),
                     qnorm = qnorm(prob),
                     pnorm = pnorm(value),
                     qt = qt(prob, df),
                     pt = pt(value, df),
                     qchisq = qchisq(prob, df),
                     pchisq = pchisq(value, df),
                     "Errore nella selezione")
    
    res_print <- paste0("$$\\huge",format(result,digits=1,nsmall=4),"$$")
    output$result <- renderUI({
      withMathJax(HTML(res_print))
    })
    
    formula_text <- switch(paste0(func, dist),
                           qnorm = "$$z = \\Phi^{-1}(p),\\qquad\\text{dove $z$ è il risultato per $p$ dato}$$",
                           pnorm = "$$p = \\Phi(z) = P(Z < z),\\qquad\\text{dove  $p$ è il risultato per $z$ dato}$$",
                           qt = "$$t = T_{\\nu}^{-1}(p),\\qquad\\text{dove  $t$ è il risultato per $p$ dato, con gdl $\\nu$}$$",
                           pt = "$$p = P(T_\\nu < t),\\qquad\\text{dove  $p$ è il risultato per $t$ dato, con gdl $\\nu$}$$",
                           qchisq = "$$x = \\chi_{\\nu}^{2^{-1}}(p)$$",
                           pchisq = "$$p = P(\\chi^2_{\\nu} < t)$$",
                           "Errore nella selezione")
    
    value <- round(value,4)
    result <- round(result,4)
    
    formula_with_values_text <- switch(paste0(func, dist),
                                       qnorm = paste0("$$P(Z < ", result, ") = ", prob, "$$"),
                                       pnorm = paste0("$$P(Z < ", value, ") = ", result, "$$"),
                                       qt = paste0("$$P(T_{", df, "} < ", result, ") = ", prob, "$$"),
                                       pt = paste0("$$P(T_{", df, "} < ", value, ") = ", result, "$$"),
                                       qchisq = paste0("$$P(\\chi^2_{", df, "} < ", result, ") = ", prob, "$$"),
                                       pchisq = paste0("$$P(\\chi^2_{", df, "}< ", value, ") = ", result, "$$"),
                                       "Errore nella selezione")
    
    output$formula <- renderUI({
      withMathJax(HTML(formula_text))
    })
    
    output$formula_with_values <- renderUI({
      withMathJax(HTML(formula_with_values_text))
    })
    
    output$density_plot <- renderPlot({
      p_min <- pnorm(-4)
      if (func=="q") value <- result
      x <- switch(dist,
                  norm = seq(-4, 4, length.out = 1000),
                  t = seq(max(qt(p_min,df),-10),min(qt(1-p_min,df),+10), length.out = 1000),
                  chisq = seq(0,qchisq(1-p_min,df) , length.out = 1000))
      y <- switch(dist,
                  norm = dnorm(x),
                  t = dt(x, df),
                  chisq = dchisq(x, df))
      p <- switch(dist,
                  norm = pnorm(value),
                  t = pt(value, df),
                  chisq = pchisq(value, df))
      dens <- switch(dist,
                     norm = dnorm(value),
                     t = dt(value, df),
                     chisq = dchisq(value, df))
      simb <- switch(dist,
                     norm = "z",
                     t = "t",
                     chisq = "x")
      
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line(colour = "white") +
        geom_area(data = data.frame(x = x[x <= value], y = y[x <= value]), aes(x = x, y = y), fill = "blue", alpha = 0.5) +
        geom_segment(aes(x = value, y = 0, xend = value, yend = dens), linetype = "dashed", color = "red") +        
        annotate("text", x = value, y = 0, label = paste0(simb," = ", value), color = "red", vjust = 1.5, hjust = 1.1) +
        labs(title = "Grafico della Densità", x = simb, y = paste0("f(",simb,")")) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#343a40"),
          panel.background = element_rect(fill = "#343a40"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),  # Colore delle etichette numeriche
          axis.ticks = element_line(color = "white")  # Colore dei tick degli assi
        ) 
    })
    
    output$distribution_plot <- renderPlot({
      p_min <- pnorm(-4)
      if (func=="q") value <- result
      x <- switch(dist,
                  norm = seq(-4, 4, length.out = 1000),
                  t = seq(max(qt(p_min,df),-10),min(qt(1-p_min,df),+10), length.out = 1000),
                  chisq = seq(0,qchisq(1-p_min,df) , length.out = 1000))
      y <- switch(dist,
                  norm = pnorm(x),
                  t = pt(x, df),
                  chisq = pchisq(x, df))
      p <- switch(dist,
                  norm = pnorm(value),
                  t = pt(value, df),
                  chisq = pchisq(value, df))
      simb <- switch(dist,
                     norm = "z",
                     t = "t",
                     chisq = "x")
      
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line(color = "white") +
        geom_segment(aes(x = value, y = 0, xend = value, yend = p), linetype = "dashed", color = "red") +
        geom_segment(aes(x = min(x), y = p, xend = value, yend = p), linetype = "dashed", color = "red") +
        annotate("text", x = value, y = 0.05, label = paste0(simb," = ", value), color = "red", vjust = 1.5, hjust = 1.1) +
        annotate("text", x = min(x), y = p + ifelse(p < 0.9, 0.05, -0.05), label = paste0("F(",simb,") = ", round(p, 4)), color = "red", hjust = -0.1) +
        labs(title = "Grafico della Ripartizione", x = simb, y = paste0("F(",simb,")")) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#343a40"),
          panel.background = element_rect(fill = "#343a40"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),  # Colore delle etichette numeriche
          axis.ticks = element_line(color = "white")  # Colore dei tick degli assi
        ) +
        geom_hline(yintercept = seq(0.1, 0.9, by = 0.1), linetype = "dashed", color = "grey50") +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), expand = c(0, 0))
      
    })
  })
  
  observeEvent(input$run_code, {
    code <- input$code
    result <- tryCatch({
      eval(parse(text = code))
    }, error = function(e) {
      e$message
    }, warning = function(w) {
      w$message
    })
    
    output$code_output <- renderPrint({
      result
    })
  })
}

shinyApp(ui = ui, server = server)
