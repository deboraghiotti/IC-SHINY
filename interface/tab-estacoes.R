source('auxiliar.R')
TabEstacoes = tabPanel ("Estacoes",
          sidebarPanel (
            
            ###################### ALTEREI PARA O BANCO DE DADOS
            
            shinyjs::useShinyjs(),
            useShinyalert(),
            shinyjs::inlineCSS(appCSS),
            titlePanel(h3("Cadastrar Estacao")),
            div(
              id="form",
              
              textInput("nomeEstacao",labelMandatory("Nome")),
              textInput("codigoEstacao",labelMandatory("Codigo")),
              textInput("rioEstacao","Rio"),
              textInput("anosEstacao","Anos"),
              textInput("areaEstacao","Area da Bacia"),
              textInput("latEstacao","Latitude"),
              textInput("lngEstacao","Longitude"),
              fileInput("fileSH",labelMandatory("Importar Serie Historica")),
              actionButton("cadastrar", "Cadastrar", class = "btn-primary"),
              
              shinyjs::hidden(
                span(id = "cadastrando_msg", "Cadastrando..."),
                div(id = "error",
                    div(br(), tags$b("Error: "), span(id = "error_msg"))
                )
              )
            ),
            shinyjs::hidden(
              div(
                id = "cadastro_realizado_msg",
                h3("Cadastro da Estacao realizado com sucesso!"),
                actionLink("cadastrar_outra", "Cadastrar outra Estacao")
              )
            )
            ####################################################
          ,width = 3),
          mainPanel (
            titlePanel(h3("Consultar Estacao")),
            selectizeInput("consultaEstacoes",label="",choices=""),
            actionButton("ConsultarButton", "Consultar", class = "btn-primary"),
            
            shinyjs::hidden( 
              div(id = "estacao_resultados",
                  hr(),
                  DT::dataTableOutput('dadosEstacaoTable'),
                  br(),
                  tabsetPanel(
                    tabPanel(
                      "Serie Historica",
                      br(),
                      plotOutput ("dados")
                      
                    ),
                    tabPanel(
                      "Volume Util",
                      br(),
                      fluidRow (
                        column (width = 6,
                                sliderInput ("porcentagemRegularizacaoHist", "Porcentagem de regularizacao", min = 0, max = 100, value = 50, width = "100%")
                        ),
                        column (width = 6,
                                verbatimTextOutput ("volumeUtilHist")
                        )
                      ),
                      
                    ),
                    tabPanel(
                      "Hurst",
                      br(),
                      verbatimTextOutput ("hurstHist")
                    ),
                    tabPanel(
                      "ACF ANUAL",
                      br(),
                      dataTableOutput("tabelaAnualHist")
                      
                    ),
                    tabPanel(
                      "ACF MENSAL",
                      br(),
                      dataTableOutput("tabelaMensalHist")
                    ),
                    tabPanel(
                      "Avaliacoes",
                      br(),
                      dataTableOutput("tabelaAvaliacaoHist")
                    )
                  ),
                  p("OBS: SE UMA ESTACAO FOR DELETADA, AS SERIES GERADAS A PARTIR DELA TAMBEM SERAO APAGADAS!'"),
                  fluidRow(column(12,
                                  actionButton("LimparButton", "   Limpar   ", class = "btn-primary"),
                                  actionButton("DeletarButton", "Delete", class = "btn-primary",style="background-color:#ff0000;border-color: #ff0000")))))
          ,width = 9)
)