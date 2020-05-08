
TabDesagregacao = tabPanel("Desagregacao",
                           titlePanel(h2("Desagregacao Nao-Parametrica",align="center")),
                           hr(),
                           selectInput ("analise_DNP", label = "Local de analise", 
                                        choices = list ("Estimar" = 1, "Arquivados" = 2),
                                        selected = "Estimados"),
                           hr(),
                           radioButtons ("tipoDesagregacao", label = h4("Estimacao de parametros"),
                                         choices = list ("Parametrica" = 1,
                                                         "Nao-Parametrica" = 2), 
                                         selected = 1,inline=TRUE),
                           hr(),
                           h4("Escolha a serie:"),
                           conditionalPanel (condition ="input.analise_DNP == 1",
                            DT::dataTableOutput("SeriesDesagregacao")
                           ),
                           conditionalPanel (condition ="input.analise_DNP == 2",
                                             fluidRow( 
                                             column(4,fileInput ("serieHArquivada", "Serie historica: ",
                                                        multiple = TRUE,
                                                        accept = c ("text/csv",
                                                                    "text/comma-separated-values,text/plain",
                                                                    ".csv")
                                                        )),
                                             column(4,fileInput ("serieDNPArquivada", "Serie Desagregada: ",
                                                        multiple = TRUE,
                                                        accept = c ("text/csv",
                                                                    "text/comma-separated-values,text/plain",
                                                                    ".csv")
                                                        ))
                                             )
                          ),
                           fluidRow( 
                            column(2,actionButton("SeriesDesagregacao_button","Selecionar",class= "btn-primary")),
                            column(2,actionButton("limparButton_DNP", "Limpar", class = "btn-primary",style="background-color:#ff0000;border-color: #ff0000"))
                           ),
                           br(),br(),
                           shinyjs::hidden(
                             div(id = "resultados_DNP",
                                 tabsetPanel(
                                    tabPanel("Avaliacoes",
                                             br(),
                                             # Module avaliacaoMensal
                                             avaliacaoMensalOutput("DNP")
                                             ),
                                    tabPanel("Grafico FAC Anuais",
                                             br ( ),
                                             # Module facAnual
                                             facAnualOutput("DNP")
                                             
                                    ),
                                    tabPanel("Graficos FAC mensais",
                                             br ( ),
                                             #  Module facMensal
                                             facMensalOutput("DNP")
                                    ),
                                    tabPanel("Medidas",
                                             br(),
                                             volumeOutput("DNP"),
                                             hr(),
                                             h4(strong("Coeficiente Hurst")),
                                             fluidRow(
                                              column(6,coeficienteHurstOutput("DNP-Mensal")),
                                              column(6,coeficienteHurstOutput("DNP-Anual"))
                                             )
                                    )
                                 ),
                                 hr(),
                                 downloadButton ("downloadSerie_DNP", "Download Serie", icon ("save"))
                              )
                           ),
                          conditionalPanel(condition ="input.analise_DNP == 1",
                                           br(),
                                           actionButton("armazenarBD_DNP","Armazenar",class = "btn-primary"),
                                           shinyjs::hidden(
                                             span(id = "armazenando_msg_DNP", "Armazenando..."),
                                             div(id = "error_armazenar_DNP",
                                                 div(br(), tags$b("Error: "), span(id = "error_msg_armazenar_DNP"))
                                             )
                                           )
                          )
)