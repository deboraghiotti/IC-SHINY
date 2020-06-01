#Aplicacao

# Carregando os arquivos com as funcoes
pastas <- c("tabs","mysql","analise","modelo","otimizacao","algoritmos")
arquivos.sources = list.files(pastas,pattern="*.R$", full.names=TRUE, ignore.case=TRUE)
sapply(arquivos.sources,source,.GlobalEnv)

source('auxiliar.R')
source('modules.R')
source ('global.R')
source ('ui.R')
source ('server.R')


shinyApp (ui = ui, server = server)