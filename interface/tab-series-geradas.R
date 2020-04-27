source('auxiliar.R')
TabSerieGeradas = tabPanel("Series Geradas",
         shinyjs::useShinyjs(),
         shinyjs::inlineCSS(appCSS),
         fluidRow(
           column(12,titlePanel(h3("Modelagem Estocastica Resultados",align="center"))) ,
           br(),
           column(12, 
                  tabsetPanel(
                    tabPanel("Series Sinteticas",
                             br(),
                             DT::dataTableOutput("SeriesSinteticas"),
                             actionButton("selecionar_ss_button","Selecionar",class= "btn-primary"),
                             br(),hr(),
                             shinyjs::hidden(
                               div(id = "ss_resultados",
                                   tabsetPanel(
                                     tabPanel("Grafico da Serie",
                                              br(),
                                              shinyjs::hidden(
                                                div(
                                                  id="grafico_ss_panel",
                                                  plotOutput("GraficoSS"),
                                                  DT::dataTableOutput('Avaliacao_SS_Table'),
                                                  downloadButton ("downloadAvaliacoes_Gerada", "Download Avaliacoes", icon ("save"))
                                                ) 
                                              )
                                     ),
                                     tabPanel("ACF MENSAL",
                                              br(),
                                              shinyjs::hidden(
                                                div(
                                                  id="acf_mensal_ss_panel",
                                                  DT::dataTableOutput('AcfMensal_SS_Table'),
                                                  downloadButton ("downloadTabelaMensal_Gerada", "Download FAC Mensal", icon ("save"))
                                                ) 
                                              )
                                     ),
                                     tabPanel("ACF ANUAL",
                                              br(),
                                              shinyjs::hidden(
                                                div(
                                                  id="acf_anual_ss_panel",
                                                  DT::dataTableOutput('AcfAnual_SS_Table'),
                                                  downloadButton ("downloadTabelaAnual_Gerada", "Download FAC Anual", icon ("save"))
                                                ) 
                                              )
                                     ),
                                     tabPanel("Hurst",
                                              br(),
                                              shinyjs::hidden(
                                                div(
                                                  id="hurst_ss_panel",
                                                  DT::dataTableOutput('Hurst_SS_Table')
                                                ) 
                                              )
                                     ),
                                     tabPanel("Volume de Reservatorio",
                                              br(),
                                              shinyjs::hidden(
                                                div(
                                                  id="volume_ss_panel",
                                                  DT::dataTableOutput('Volume_SS_Table')
                                                ) 
                                              )
                                     ),
                                     tabPanel("Soma Residual",
                                              br(),
                                              shinyjs::hidden(
                                                div(
                                                  id="soma_ss_panel",
                                                  DT::dataTableOutput('Soma_SS_Table')
                                                ) 
                                              )
                                     )              
                                   ),
                                   hr(),
                                   downloadButton ("downloadSerie_Gerada", "Download Serie", icon ("save")),
                                   hr(),
                                   fluidRow(
                                     column(12,
                                            actionButton("limpar_ss_button", "   Limpar   ", class = "btn-primary"),
                                            actionButton("delete_ss_button", "   Deletar   ", class = "btn-primary",style="background-color:#ff0000;border-color: #ff0000")
                                     )
                                   )
                               )
                             )
                    ),
                    tabPanel("Series Desagregadas",
                             br(),
                             DT::dataTableOutput("SeriesDesagregadas"),
                             actionButton("selecionar_sd_button","Selecionar",class= "btn-primary"),
                             br(),hr(),
                             shinyjs::hidden(
                               div(id = "sd_resultados",
                                   tabsetPanel(
                                     tabPanel("Avaliacoes",
                                              br(),
                                              shinyjs::hidden(
                                                div(
                                                  id="avaliacao_sd_panel",
                                                  DT::dataTableOutput('Avaliacao_SD_Table'),
                                                  downloadButton ("downloadAvaliacoes_Gerada_SD", "Download Avaliacoes", icon ("save"))
                                                ) 
                                              )
                                     ),
                                     tabPanel("ACF MENSAL",
                                              br(),
                                              shinyjs::hidden(
                                                div(
                                                  id="acf_mensal_sd_panel",
                                                  DT::dataTableOutput('AcfMensal_SD_Table'),
                                                  downloadButton ("downloadTabelaMensal_Gerada_SD", "Download FAC Mensal", icon ("save"))
                                                ) 
                                              )
                                     ),
                                     tabPanel("ACF ANUAL",
                                              br(),
                                              shinyjs::hidden(
                                                div(
                                                  id="acf_anual_sd_panel",
                                                  DT::dataTableOutput('AcfAnual_SD_Table'),
                                                  downloadButton ("downloadTabelaAnual_Gerada_SD", "Download FAC Anual", icon ("save"))
                                                ) 
                                              )
                                     ),
                                     tabPanel("Hurst",
                                              br(),
                                              shinyjs::hidden(
                                                div(
                                                  id="hurst_sd_panel",
                                                  DT::dataTableOutput('Hurst_SD_Table')
                                                ) 
                                              )
                                     ),
                                     tabPanel("Volume de Reservatorio",
                                              br(),
                                              shinyjs::hidden(
                                                div(
                                                  id="volume_sd_panel",
                                                  DT::dataTableOutput('Volume_SD_Table')
                                                ) 
                                              )
                                     ),
                                     tabPanel("Soma Residual",
                                              br(),
                                              shinyjs::hidden(
                                                div(
                                                  id="soma_sd_panel",
                                                  DT::dataTableOutput('Soma_SD_Table')
                                                ) 
                                              )
                                     )              
                                   ),
                                   hr(),
                                   downloadButton ("downloadSerie_Gerada_SD", "Download Serie", icon ("save")),
                                   hr(),
                                   actionButton("limpar_sd_button", "Limpar", class = "btn-primary")
                               )
                             )
                    )
                  )
           )
         )
)