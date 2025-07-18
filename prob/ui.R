
library(DT)
library(shinythemes)
library(shinyWidgets)
library(shiny)
library(knitr)
library(rmarkdown)


ui <- tagList(#shinythemes::themeSelector(),
navbarPage("Variabili Casuali", # Titolo dell'applicazione
           theme = shinytheme("flatly"),
                 tabPanel("Convoluzione", 
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("m", "Numero di valori per S_X e n_x", value = 2, min = 1, step = 1),
                              numericInput("k", "Numero di valori per S_Y e n_y", value = 2, min = 1, step = 1),
                              uiOutput("inputS1"),
                              uiOutput("inputS2"),
                              selectInput("ope", label = "Seleziona l'operatore",
                                          choices = list("+" = "+", "-" = "-", "*" = "*", "/" = "/"),
                                          selected = "+"),
                              "v 2.0, Patrizio Frederic, 2021-2024"
                            ),
                            mainPanel(
                              uiOutput("two_way")
                            )
                          )
                 ),
                 tabPanel("Binomiale", 
                          withMathJax(), # Abilita MathJax
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("p",label = "pigreco",value = 0.5,min = 0,max = 1,step = 0.1),
                              numericInput("n",label = "n",value = 5,min = 0.,step = 1),
                              "v 2.0, Patrizio Frederic, 2021-2024"
                            ),
                            mainPanel(
                              uiOutput("binTitle"),
                              plotOutput("binPlot")
                            )
                          )
                          # Aggiungi ulteriori tabPanel qui se necessario
                 ),
                 tabPanel("Calcolo Binomiale", 
                          withMathJax(), # Abilita MathJax
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("pp",label = "pigreco",value = 0.5,min = 0,max = 1,step = 0.1),
                              numericInput("nn",label = "n",value = 5,min = 0.,step = 1),
                              numericInput("x1",label = "x",value = 2,min = 0.,step = 1),
                              selectInput("verso", label = "Seleziona l'operatore",
                                          choices = list("=" = "=", "≤" = "\\leq", "≥" = "\\geq"),
                                          selected = "="),
                              checkboxInput("comp",label = "Complementare"),
                              "v 2.0, Patrizio Frederic, 2021-2024"
                            ),
                            mainPanel(
                              uiOutput("bin")
                            )
                          )
                          # Aggiungi ulteriori tabPanel qui se necessario
                 ),
                 tabPanel("Poisson", 
                          withMathJax(), # Abilita MathJax
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("l",label = "lambda",value = 1,min = 0,step = 0.1),
                              "v 2.0, Patrizio Frederic, 2021-2024"
                            ),
                            mainPanel(
                              uiOutput("poisTitle"),
                              plotOutput("poisPlot")
                            )
                          )
                          # Aggiungi ulteriori tabPanel qui se necessario
                 ),
                 tabPanel("Calcolo Poisson", 
                          withMathJax(), # Abilita MathJax
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("ll",label = "lambda",value = 1,min = 0,step = 0.1),
                              numericInput("xp",label = "x",value = 2,min = 0.,step = 1),
                              selectInput("verso1", label = "Seleziona l'operatore",
                                          choices = list("=" = "=", "≤" = "\\leq", "≥" = "\\geq"),
                                          selected = "="),
                              "v 2.0, Patrizio Frederic, 2021-2024"
                            ),
                            mainPanel(
                              uiOutput("pos")
                            )
                          ),
                          
                          # Aggiungi ulteriori tabPanel qui se necessario
                 ),
                 tabPanel("Normale", 
                          withMathJax(), # Abilita MathJax
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         sliderInput("mu1", "mu 1",
                                                     min = -40, max = 40, value = 0, step = .5),
                                         sliderInput("sigma1", "sigma² 1",
                                                     min = 0, max = 15, value = 1, step = .05),
                                         sliderInput("mu2", "mu 2",
                                                     min = -40, max = 40, value = 10, step = .5),
                                         sliderInput("sigma2", "sigma² 2",
                                                     min = 0, max = 40, value = 2, step = .05),
                                         "v 2.0, Patrizio Frederic, 2021-2024"
                            ),
                            mainPanel(
                              plotOutput("norm")
                            )
                          )
                          # Aggiungi ulteriori tabPanel qui se necessario
                 ),
                 tabPanel("Normale: Tavole", 
                          withMathJax(), # Abilita MathJax
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         # Numeric input per gli estremi dell'intervallo
                                         numericInput("it_min", "Estremo Inferiore",
                                                      min = -4, max = 4, value = -4, step = 0.01),
                                         numericInput("it_max", "Estremo Superiore",
                                                      min = -4, max = 4, value = 0, step = 0.01),
                                         sliderInput("it", "intervallo di itegrazione",
                                                     min = -4, max = 4, value = c(-4,0), step = .01),
                                         numericInput("gc", "Grandezza Carattere",
                                                      min = 0.01, max = 2, value = 1, step = .1),
                                         checkboxGroupInput("showPlots", "Seleziona i grafici da visualizzare:",
                                                            choices = list("Distribuzione" = "distPlot",
                                                                           "Densità" = "densPlot",
                                                                           "Tavole" = "mon"),
                                                            selected = c("distPlot", "densPlot", "mon")),
                                         "v 2.0, Patrizio Frederic, 2021-2024"
                            ),
                            mainPanel(
                              uiOutput("plotsPanel") # Modificato per renderizzare dinamicamente i grafici selezionati
                            )
                          )
                          # Aggiungi ulteriori tabPanel qui se necessario
                 ),
                 tabPanel("Normale mu, sigma²", 
                          withMathJax(), # Abilita MathJax
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("mm",label = "mu",value = 0,step = 0.1),
                              numericInput("ss",label = "sigma",value = 1,min = 0.01,step = .01),
                              uiOutput("xx"),
                              "v 2.0, Patrizio Frederic, 2021-2024"
                            ),
                            mainPanel(
                              uiOutput("sol")
                            )
                          )
                          # Aggiungi ulteriori tabPanel qui se necessario
                 ),
       
)
)