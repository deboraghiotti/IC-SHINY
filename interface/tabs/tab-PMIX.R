# TAB DA UI RELACIONADA AO MODELO PMIX
library(plotly)
library(shinyalert)
source('modules.R')

TabPMIX = tabPanel ("Modelo PMIX",
          sidebarLayout(
            sidebarPanel (
              titlePanel(h4(strong("Modelo PMIX"),align="center")),
              br(),
                                hr(),
                                div(id = "parametros_PMIX",
                                    selectizeInput("estacoes",label = "Escolha a Estacao",choices=""),
                                    hr (),
                                    radioButtons ("tipo", label = "Estimacao de parametros",
                                                  choices = list ("Metodo de Powell" = 1,
                                                                  "Algoritmo Genetico" = 2), 
                                                  selected = 1,inline=TRUE),
                                    tags$hr ( ),
                                    fluidRow (
                                      column (width = 3,
                                              numericInput ("p", label = "p", value = 1, min = 0, max = 12)),
                                      column (width = 3,
                                              numericInput ("q", label = "q", value = 0, min = 0, max = 12)),
                                      column (width = 3,
                                              numericInput ("P", label = "P", value = 0, min = 0, max = 12)),
                                      column (width = 3,
                                              numericInput ("Q", label = "Q", value = 0, min = 0, max = 12))
                                    ),
                                    hr(),
                                    sliderInput ("nsint", label = "Tamanho da serie sintetica", min = 0, max = 50000, value = 10000, width = "90%"),
                                    hr(),
                                    shinyjs::hidden(
                                      div(id = "parametros_ag",
                                          h5(strong("Parametros do Algoritmo Genetico"),align="center"),
                                          sliderInput ("nPop", label = "Tamanho da populacao", min = 10, max = 100, value = 50, width = "100%"),
                                          sliderInput ("cicloMax", label = "Ciclo Maximo", min = 0, max = 50000, value = 10000, width = "100%"),
                                          fluidRow (
                                            column (width = 6,numericInput ("pC", label = "Probabilidade de cruzamento", value = 80, min = 0, max = 100)),
                                            column(width = 6, numericInput ("pM", label = "Probabilidade de mutacao", value = 5, min = 0, max = 100))
                                          ), 
                                          fluidRow(
                                            column (width = 6,numericInput ("MAPEdiferencaMAX", label = "MAPEdiferencaMAX", value = 5, min = 0, max = 100)),
                                            column(width = 6,numericInput ("MAPEavaliacao", label = "MAPEavaliacao", value = 20, min = 0, max = 100))
                                          ),
                                          fluidRow(
                                            column (width = 6,numericInput ("lagAnual", label = "lag Anual", value = 1, min = 1, max = 12)),
                                            column(width = 6,numericInput ("lagMensal", label = "lag Mensal", value = 1, min = 1, max = 12))
                                          ),
                                          checkboxInput ("lagSignificativo", "Lag Significativo", TRUE)
                                      ))),
                                fluidRow( 
                                  column(6,actionButton ("iniciar", "Iniciar!")),
                                  column(6,actionButton("limparButton_PMIX", "Limpar", class = "btn-primary",style="background-color:#ff0000;border-color: #ff0000"))
                                )
              ,width = 4),
            
            mainPanel(
              h3 (strong ("Resultados"),align = "center"),
              hr(),
              shinyjs::hidden(
                div(id = "resultados_PMIX",
                    verbatimTextOutput ("resultadoGeral"),
                    shinyjs::hidden(
                      div(id="plotly_avaliacoes",
                          hr(),
                          h4 (strong ("Grafico: MAPEfacAnual x MAPEfacMensal x MAPE dp"),align = "center"),
                          plotlyOutput(outputId = "grafico_avaliacoes"))
                    ),
                    hr(),
                    selectInput ("nSerie", "Serie a ser analisada:", choices = 1:50, selected = 50),
                    shinyjs::hidden(
                      span(id = "armazenando_msg", "Armazenando..."),
                      div(id = "error_armazenar",
                          div(br(), tags$b("Error: "), span(id = "error_msg_armazenar"))
                      )
                    ),
                    tabsetPanel (
                      tabPanel("Tabela avaliacoes",
                               br ( ),
                               dataTableOutput ("tabelaAvaliacao")
                      ),
                      tabPanel("Graficos series",
                               br ( ),
                               # Module avaliacaoMensal
                               avaliacaoMensalOutput("PMIX")
                      ),
                      tabPanel("Graficos FAC anuais",
                               br ( ),
                               # Module facAnual
                               facAnualOutput("PMIX")
                      ),
                      tabPanel("Graficos FAC mensais",
                               br ( ),
                               # Module facMensal
                               facMensalOutput("PMIX")
                      ),
                      tabPanel("Medidas",
                               br ( ),
                               volumeOutput("PMIX"),
                               hr(),
                               h4(strong("Coeficiente de Hurst")),
                               fluidRow(
                                column(6,coeficienteHurstOutput("PMIX-Mensal")),
                                column(6,coeficienteHurstOutput("PMIX-Anual"))
                               )
                      )
                    ),
                    hr(),
                    fluidRow( 
                      column(width = 2,actionButton("armazenarBD","Armazenar",class = "btn-primary")),
                      column(width = 2,downloadButton ("downloadSerie", "Download Serie", icon ("save")))
                    )
                )
              )
              
              ,width = 8)
          ),
          tags$hr ( )
          
)