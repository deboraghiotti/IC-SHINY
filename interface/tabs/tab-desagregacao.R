# TAB DA UI RELACIONADA A DESAGREGACAO
source('modules.R')

TabDesagregacao = tabPanel("Desagregacao",
                           titlePanel(h2("Desagregacao Temporal",align="center")),
                           hr(),
                           selectInput ("analiseDesagregacao", label = "Local de analise", 
                                        choices = list ("Local" = 1, "Arquivados" = 2),
                                        selected = 1),
                           hr(),
                           radioButtons ("tipoDesagregacao", label = h4("Estimacao de parametros"),
                                         choices = list ("Parametrica" = 1,
                                                         "Nao-Parametrica" = 2), 
                                         selected = 1,inline=TRUE),
                           hr(),
                           conditionalPanel (condition ="input.analiseDesagregacao == 1",
                                            h4("Escolha a serie:"),
                                            DT::dataTableOutput("SeriesDesagregacao")
                           ),
                           conditionalPanel (condition ="input.analiseDesagregacao == 2",
                                             fluidRow(
                                             column(4,radioButtons ("tipoSerieDesagregacao", label = h4("Tipo da serie sintetica:"),
                                                           choices = list ("Anual" = 1,
                                                                           "Mensal" = 2),
                                                           selected = 1,inline=TRUE)),
                                             ),
                                             hr(),
                                             fluidRow( 
                                             column(4,fileInput ("serieHistDesagregacao", "Serie historica: ",
                                                        multiple = TRUE,
                                                        accept = c ("text/csv",
                                                                    "text/comma-separated-values,text/plain",
                                                                    ".csv")
                                                        )),
                                             column(4,fileInput ("serieSintDesagregacao", "Serie Sintetica: ",
                                                        multiple = TRUE,
                                                        accept = c ("text/csv",
                                                                    "text/comma-separated-values,text/plain",
                                                                    ".csv")
                                                        ))
                                             )
                          ),
                           fluidRow( 
                            column(2,actionButton("SeriesDesagregacao_button","Selecionar",class= "btn-primary")),
                            column(2,actionButton("limparDesagregacaoButton", "Limpar", class = "btn-primary",style="background-color:#ff0000;border-color: #ff0000"))
                           ),
                           br(),br(),
                           shinyjs::hidden(
                             div(id = "resultadosDesagregacao",
                                 tabsetPanel(
                                    tabPanel("Avaliacoes",
                                             br(),
                                             # Module avaliacaoMensal
                                             avaliacaoMensalOutput("Desagregacao")
                                             ),
                                    tabPanel("Grafico FAC Anuais",
                                             br ( ),
                                             # Module facAnual
                                             facAnualOutput("Desagregacao")
                                             
                                    ),
                                    tabPanel("Graficos FAC mensais",
                                             br ( ),
                                             #  Module facMensal
                                             facMensalOutput("Desagregacao")
                                    ),
                                    tabPanel("Medidas",
                                             br(),
                                             volumeOutput("Desagregacao"),
                                             hr(),
                                             h4(strong("Coeficiente Hurst")),
                                             fluidRow(
                                              column(6,coeficienteHurstOutput("Desagregacao-Mensal")),
                                              column(6,coeficienteHurstOutput("Desagregacao-Anual"))
                                             )
                                    )
                                 ),
                                 hr(),
                                 downloadButton ("downloadSerieDesagregacao", "Download Serie", icon ("save"))
                              )
                           ),
                          conditionalPanel(condition ="input.analiseDesagregacao == 1",
                                           br(),
                                           actionButton("armazenarBD_Desagregacao","Armazenar",class = "btn-primary"),
                                           shinyjs::hidden(
                                             span(id = "armazenando_msg_Desagregacao", "Armazenando..."),
                                             div(id = "error_armazenar_Desagregacao",
                                                 div(br(), tags$b("Error: "), span(id = "error_msg_armazenar_Desagregacao"))
                                             )
                                           )
                          )
)