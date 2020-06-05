library('data.table')
library(e1071)
library(shinyalert)
library(plotly)
library(RMySQL)

# Shiny Server
# Fonte: https://shiny.rstudio.com/articles/scoping.html

## O Server foi dividido em 5 arquivos para facilitar a leitura do codigo.Esse arquivos se encontram na pasta 'server'.
## Os arquivos: 'server-pmix.R','server-estacoes.R','server-serie-geradas.R','server-arma.R','server-desagregacao.R'
## Arquivo 'server-pmix.R': Possui o server relacionado a tabpanel 'PMIX'. UI realionada a esse server: 'tab-PMIX.R'
## Arquivo 'server-estacoes.R': Possui o server relacionado a tabpanel 'Estacoes'. UI realionada a esse server: 'tab-estacoes.R'
## Arquivo 'server-series-geradas.R': Possui o server relacionado a tabpanel 'Series Geradas'. UI realionada a esse server: 'tab-series-geradas.R'
## Arquivo 'server-arma.R': Possui o server relacionado a tabpanel 'ARMA'. UI realionada a esse server: 'tab-ARMA.R'
## Arquivo 'serve-desagregacao.R': Possui o server relacionado a tabpanel 'Desagregacao'. UI realionada a esse server: 'tab-desagregacao.R'

server <- function (input, output, session) {
  
  source('server/server-pmix.R',local = TRUE)
  
  source('server/server-estacoes.R',local = TRUE)
  
  source('server/server-series-geradas.R',local = TRUE)
  
  source('server/server-arma.R',local = TRUE)
  
  source('server/server-desagregacao.R',local = TRUE)
  
}