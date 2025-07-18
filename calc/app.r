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
             sidebarLayout(
               sidebarPanel(
                 selectInput("dist", "Seleziona distribuzione:",
                             choices = list("Z" = "norm",
                                            "t" = "t",
                                            "Chi²" = "chisq")),
                 selectInput("func", "Seleziona funzione:",
                             choices = list("F" = "p", 
                                            "Q" = "q" )),
                 checkboxInput("right_tail", "Calcola la coda destra (P(X > x))", value = FALSE),
                 conditionalPanel(
                   condition = "input.dist == 't' || input.dist == 'chisq'",
                   numericInput("df", "Gradi di libertà:", value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.func == 'q'",
                   numericInput("prob", "Probabilità:", value = 0.025, min = 0, max = 1, step = 0.005)
                 ),
                 conditionalPanel(
                   condition = "input.func == 'p'",
                   numericInput("value", "Valore:", value = 0,step = 0.01)
                 ),
                 actionButton("calc", "Calcola")
               ),
               mainPanel(
#                 h3("Risultato"),
                 uiOutput("result"),
#                 h3("Formula teorica"),
                 uiOutput("formula"),
#                 h3("Formula con i numeri inseriti"),
                 uiOutput("formula_with_values"),
                 plotOutput("density_plot"),
                 plotOutput("distribution_plot")
               ),
    # tabPanel("Editor di codice R",
    #          sidebarLayout(
    #            sidebarPanel(
    #              aceEditor("code", mode = "r", theme = "solarized_dark", height = "300px",fontSize = 15),
    #              actionButton("run_code", "Esegui Codice")
    #            ),
    #            mainPanel(
    #              h3("Risultato dell'esecuzione"),
    #              textOutput("code_output")
    #            )
    #          )
    # )
  )
)
server <- function(input, output, session) {
  auto_calc <- reactiveVal(TRUE)
  
  calcoli <- eventReactive({
    input$calc
    input$right_tail
    auto_calc()
  }, {
    auto_calc(FALSE)
    dist <- input$dist
    func <- input$func
    df <- input$df
    prob <- input$prob
    value <- input$value
    right_tail <- input$right_tail
    
    adjusted_prob <- if (right_tail && func == "q") 1-prob else prob
    
    result <- switch(paste0(func, dist),
                     qnorm = qnorm(adjusted_prob),
                     qt = qt(adjusted_prob, df),
                     qchisq = qchisq(adjusted_prob, df),
                     pnorm = if (right_tail) 1 - pnorm(value) else pnorm(value),
                     pt = if (right_tail) 1 - pt(value, df) else pt(value, df),
                     pchisq = if (right_tail) 1 - pchisq(value, df) else pchisq(value, df),
                     "Errore nella selezione")
    
    list(result = result,
         adjusted_prob = adjusted_prob,
         value = value,
         df = df,
         dist = dist,
         func = func,
         right_tail = right_tail)
  })
  
  output$result <- renderUI({
    req(calcoli())
    res <- format(calcoli()$result, digits = 1, nsmall = 4, scientific = F)
    withMathJax(HTML(paste0("$$\\huge", res, "$$")))
  })
  
  output$formula <- renderUI({
    req(calcoli())
    dist <- calcoli()$dist
    func <- calcoli()$func
    right_tail <- calcoli()$right_tail
    
    formula_text <- switch(paste0(func, dist),
                           qnorm = "$$z = \\Phi^{-1}(\\alpha)$$",
                           pnorm = if (right_tail) "$$p = P(Z > z)$$" else "$$p = P(Z < z) = \\Phi(z)$$",
                           qt = "$$t = T_{\\nu}^{-1}(\\alpha)$$",
                           pt = if (right_tail) "$$p = P(T_\\nu > t)$$" else "$$p = P(T_\\nu < t)$$",
                           qchisq = "$$x = \\chi_{\\nu}^{2^{-1}}(\\alpha)$$",
                           pchisq = if (right_tail) "$$p = P(\\chi^2_{\\nu} > x)$$" else "$$p = P(\\chi^2_{\\nu} < x)$$",
                           "Errore nella selezione")
    
    withMathJax(HTML(formula_text))
  })
  
  output$formula_with_values <- renderUI({
    req(calcoli())
    dist <- calcoli()$dist
    func <- calcoli()$func
    df <- calcoli()$df
    value <- round(calcoli()$value, 4)
    result <- calcoli()$result
    result <- format(result,digits = 4, scientific = F)
    prob <- round(calcoli()$adjusted_prob, 4)
    sgn <- ifelse(calcoli()$right_tail, ">", "<")
    right_tail <- input$right_tail
    adjusted_prob <- if (right_tail && func == "q") 1-prob else prob
    
    formula_with_values_text <- switch(paste0(func, dist),
                                       qnorm = paste0("$$P(Z ",sgn, result, ") = ", adjusted_prob, "$$"),
                                       pnorm = if (calcoli()$right_tail) paste0("$$P(Z > ", value, ") = ", result, "$$") else paste0("$$P(Z < ", value, ") = ", result, "$$"),
                                       qt = paste0("$$P(T_{", df, "} ",sgn, result, ") = ", adjusted_prob, "$$"),
                                       pt = if (calcoli()$right_tail) paste0("$$P(T_{", df, "} > ", value, ") = ", result, "$$") else paste0("$$P(T_{", df, "} < ", value, ") = ", result, "$$"),
                                       qchisq = paste0("$$P(\\chi^2_{", df, "} ",sgn, result, ") = ", adjusted_prob, "$$"),
                                       pchisq = if (calcoli()$right_tail) paste0("$$P(\\chi^2_{", df, "} > ", value, ") = ", result, "$$") else paste0("$$P(\\chi^2_{", df, "} < ", value, ") = ", result, "$$"),
                                       "Errore nella selezione")
    
    withMathJax(HTML(formula_with_values_text))
  })
  output$density_plot <- renderPlot({
    req(calcoli())
    dist <- calcoli()$dist
    func <- calcoli()$func
    df <- calcoli()$df
    right_tail <- calcoli()$right_tail
    value <- if (func == "q") calcoli()$result else calcoli()$value
    value <- round(value,4)
    picc <- pnorm(-4)
    tmin <- qt(picc,df)
    if (tmin< -10) tmin <- -10 else tmin <- round(tmin)
    x <- switch(dist,
                norm = seq(-4, 4, length.out = 1000),
                t = seq(-tmin, tmin, length.out = 1000),
                chisq = seq(0, qchisq(0.999, df), length.out = 1000))
    y <- switch(dist,
                norm = dnorm(x),
                t = dt(x, df),
                chisq = dchisq(x, df))
    dens <- switch(dist,
                   norm = dnorm(value),
                   t = dt(value, df),
                   chisq = dchisq(value, df))
    simb <- switch(dist, norm = "z", t = "t", chisq = "x")
    
    ggplot(data.frame(x, y), aes(x, y)) +
      geom_line(colour = "white") +
      geom_area(data = data.frame(x = x[if (right_tail) x > value else x <= value],
                                  y = y[if (right_tail) x > value else x <= value]),
                aes(x = x, y = y), fill = "blue", alpha = 0.5) +
      geom_segment(aes(x = value, y = 0, xend = value, yend = dens), linetype = "dashed", color = "red") +        
      annotate("text", x = value, y = 0, label = paste0(simb," = ", value), color = "lightgrey", vjust = 1.5, hjust = 1.1) +
      labs(title = "Grafico della Densità", x = simb, y = paste0("f(",simb,")")) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#343a40"),
        panel.background = element_rect(fill = "#343a40"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_line(color = "white")) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 20), labels = function(x) ifelse(x == floor(x), x, ""))
  })
  output$distribution_plot <- renderPlot({
    req(calcoli())
    dist <- calcoli()$dist
    func <- calcoli()$func
    df <- calcoli()$df
    right_tail <- calcoli()$right_tail
    value <- if (func == "q") calcoli()$result else calcoli()$value
    value <- round(value,4)
    picc <- pnorm(-4)
    tmin <- qt(picc,df)
    result <- round(calcoli()$result, 4)
    if (tmin< -10) tmin <- -10 else tmin <- round(tmin)
    x <- switch(dist,
                norm = seq(-4, 4, length.out = 1000),
                t = seq(-tmin, tmin, length.out = 1000),
                chisq = seq(0, qchisq(0.999, df), length.out = 1000))
    y <- switch(dist,
                norm = dnorm(x),
                t = dt(x, df),
                chisq = dchisq(x, df))
    dens <- switch(dist,
                   norm = dnorm(value),
                   t = dt(value, df),
                   chisq = dchisq(value, df))
    simb <- switch(dist, norm = "z", t = "t", chisq = "x")
    p_min <- pnorm(-4)
    if (func=="q") value <- result
    x <- switch(dist,
                norm = seq(-4, 4, length.out = 1000),
                t = seq(-tmin, tmin, length.out = 1000),
                chisq = seq(0, qchisq(0.999, df), length.out = 1000))
    y <- switch(dist,
                norm = pnorm(x),
                t = pt(x, df),
                chisq = pchisq(x, df))
    #if (input$right_tail) y <- 1 - y
    p <- switch(dist,
                norm = pnorm(value),
                t = pt(value, df),
                chisq = pchisq(value, df))
    simb <- switch(dist, norm = "z", t = "t", chisq = "x")
    
    ggplot(data.frame(x, y), aes(x, y)) +
      geom_line(color = "white") +
      geom_segment(aes(x = value, y = 0, xend = value, yend = p), linetype = "dashed", color = "red") +
      geom_segment(aes(x = min(x), y = p, xend = value, yend = p), linetype = "dashed", color = "red") +
      annotate("text", x = value, y = 0.05, label = paste0(simb," = ", value), color = "red", vjust = 1.5, hjust = 1.1) +
      annotate("text", x = min(x), y = p + ifelse(p < 0.9, 0.05, -0.05),
               label = paste0("P(",simb, " < ", value,") = ", round(p, 4)), color = "red", hjust = -0.1) +
      labs(title = "Grafico della Ripartizione", x = simb, y = paste0("F(",simb,")")) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#343a40"),
        panel.background = element_rect(fill = "#343a40"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_line(color = "white")) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 20), labels = function(x) ifelse(x == floor(x), x, ""))
    })
  
  
  observeEvent(input$run_code, {
    code <- input$code
    result <- tryCatch({
      eval(parse(text = code))
    }, error = function(e) e$message, warning = function(w) w$message)
    output$code_output <- renderPrint({ result })
  })
}

shinyApp(ui = ui, server = server)