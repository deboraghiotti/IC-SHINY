source('modules.R')
TabARMA = tabPanel("Modelo ARMA",
         sidebarLayout(
           sidebarPanel(
             titlePanel(h3("Modelo ARMA (p,q)",align="center")),
             br(),
             selectizeInput("estacoes_ARMA",label = "Escolha a Estacao",choices=""),
             hr(),
             titlePanel(h5(strong("Escolha os lags:"))),
             fluidRow(
               column(6,numericInput ("p_ARMA", label = "p", value = 1, min = 0, max = 12, width = "70px")),
               column(6,numericInput ("q_ARMA", label = "q", value = 0, min = 0, max = 12, width = "70px"))
             ),
             hr(),
             sliderInput ("nsint_ARMA", label = "Tamanho da serie sintetica", min = 0, max = 50000, value = 10000),
             hr(),
             fluidRow( 
               column(6,actionButton("goButton_ARMA", "Iniciar", class = "btn-primary")),
               column(6,actionButton("limparButton_ARMA", "Limpar", class = "btn-primary",style="background-color:#ff0000;border-color: #ff0000"))
             ),
             hr(),
             titlePanel(h5(strong("Armazenar Serie:"))),
             actionButton("armazenarButton_ARMA", "Armazenar", class = "btn-primary"),
             shinyjs::hidden(
               span(id = "armazenando_msg_ARMA", "Armazenando..."),
               div(id = "error_armazenar_ARMA",
                   div(br(), tags$b("Error: "), span(id = "error_msg_armazenar_ARMA"))
               )
             )
             
           ),
           mainPanel(
             shinyjs::hidden(
               div(id="resultados_ARMA",
                   tabsetPanel (
                     tabPanel("Graficos FAC anuais",
                              br(),
                              # Module facAnual
                              facAnualOutput("ARMA")
                     ),
                     
                     tabPanel("Avaliacao",
                              br ( ),
                              # Module avaliacaoAnual
                              avaliacaoAnualOutput("ARMA")
                     ),
                     tabPanel("Medidas",
                              br ( ),
                              volumeOutput("ARMA"),
                              hr ( ),
                              h4 (strong ("Coeficiente de Hurst")),
                              coeficienteHurstOutput("ARMA"),
                              hr(),
                              h4 (strong ("Soma Residual")),
                              verbatimTextOutput ("somaRes_ARMA")
                     )
                   ),
                   hr(),
                   downloadButton ("downloadSerie_ARMA", "Download Serie", icon ("save"))
               )
             )
           )
         )
)