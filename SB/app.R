require(shiny)  
ui <- shinyUI(
  fluidPage(
    withMathJax()
    , h2("$$\\mbox{My Math example }\\sqrt{2}$$")
    , tableOutput('mytable')
  )
)
server <- function(input, output, session){  
  output$mytable <- renderTable({  
    df <- data.frame(A = c("$$\\alpha+\\beta$$", 33.1, 6),B = c(111111, 3333333, 3123.233))  
    names(df) <- c("$$A$$","$$B$$")
    df  
  }, sanitize.text.function = function(x) x)
}

shinyApp(ui = ui, server = server)