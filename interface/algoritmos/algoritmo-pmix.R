library (shiny)
library (DT)

# Utiliza funcoes que estao nas pastas "analise","modelo" e "otimizacao"
# Algoritmo do modelo pmix
algoritmo = function (input,serieH) {
  lags = c (input$p, input$q, input$P, input$Q)

  if (input$tipo == 1) {
    inicio = Sys.time ( )
    arquivos = PMIX (serieH, lags)
    fim = Sys.time ( )
    parametrosIniciais = c (rep (1, 12*lags[1]), rep (0, 12*lags[2]), rep (1, 12*lags[3]), rep (0, 12*lags[4]))
    parametros = arquivos$parametros
    series = cenarioSintetico (serieH(), arquivos$parametros, lags, input$nsint)
    avaliacoes = arquivos$parametros
    algoritmo = list (ciclos = arquivos$ciclos, somRes = arquivos$somRes)
  }
  else if (input$tipo == 2) {
    inicio = Sys.time ( )
    arquivos = NSGA (serieH, lags,
                     input$nPop, ((input$pC) / 100), ((input$pM) / 100),
                     input$cicloMax, ((input$MAPEdiferencaMAX)/100), input$nsint)
    fim = Sys.time ( )
    
    parametrosIniciais = arquivos$arquivoParametrosIniciais
    parametros = arquivos$arquivoParametros
    series = lapply(arquivos$arquivosSeries, as.matrix)
    avaliacoes = arquivos$arquivoAvaliacoes
    algoritmo = list (ciclos = arquivos$ciclos)
  }
  
  final = list (arqParametrosIniciais = parametrosIniciais,
                arqParametros = parametros,
                arqSeries = series,
                arqAvaliacoes = avaliacoes,
                duracao = difftime (fim, inicio, units = c ("secs")),
                algoritmo = algoritmo)
  
  return (final)
}