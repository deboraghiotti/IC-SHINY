#Aplicacao

# Carregando os arquivos com as funcoes

source('auxiliar.R')
source('modules.R')
source ('global.R')
source ('ui.R')
source ('server.R')

library('data.table')
library(e1071)
library(shinyalert)
library(plotly)
library(RMySQL)
require(shinyjs)

shinyApp (ui = ui, server = server)