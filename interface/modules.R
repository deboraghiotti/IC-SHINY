# Modularizacao
# Fonte: https://shiny.rstudio.com/articles/modules.html

# Module avaliacaoMensal: Esse "module" realiza a avaliacao da serie sintetica mensal.

# UI: Gráfico que compara a media e o desvio padrao da serie historica com a serie sintetica, Tabela com a media, desvio
# padrao, kurt, assimetria e coeficiente de variadrao, e um botao para baixar a avaliacao da serie sintetica.

# Server: retorna um data.frame com a avaliacao da serie sintetica (media, desvio padrao,kurt, assimetria e 
# coeficiente de variacao).

avaliacaoMensalOutput <- function(id){
  
  # Criado um namespace com o id
  ns <- NS(id)
  
  tagList(
    
    plotOutput(ns("graficoSerie")),
    dataTableOutput(ns("tabelaAvaliacao")),
    downloadButton (ns("downloadAvaliacao"), "Download Avaliacao", icon ("save"))
    
  )
  
}

avaliacaoMensal <- function(input,output,session,serieHist,serieSint){
  
  mediaSint = apply (serieSint, 2, mean)
  desvioSint = apply (serieSint, 2, sd)
  kurtSint = apply(serieSint,2,kurtosis)
  assimetriaSint = apply(serieSint,2,skewness)
  coefVarSint = desvioSint/mediaSint
  
  avaliacaoSint = data.frame (mediaSint,desvioSint,kurtSint,assimetriaSint,coefVarSint)
  colnames(avaliacaoSint) = c("mediaSint","desvioSint","kurtSint","assimetriaSint","coefVarSint")
  rownames (avaliacaoSint) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
  
  output$graficoSerie = renderPlot ({
      inicializaGraficoSERIE(serieHist)
      graficoSERIE (serieHist, 'cornflowerblue')
      graficoSERIE (serieSint, 'blue')
  })
  
  output$tabelaAvaliacao = renderDataTable ({
      mediaHist = apply (serieHist, 2, mean)
      desvioHist = apply (serieHist, 2, sd)
      
      medidas = round(data.frame (mediaHist, mediaSint, desvioHist, desvioSint,kurtSint,assimetriaSint,coefVarSint),digits = 5)
      rownames (medidas) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      colnames (medidas) = c ("Media Historica", "Media Sintetica", "Desvio-padrao Historico", "Desvio-padrao Sintetico","Indice Kurt","Assimetria","Coeficiente de Variacao")
      datatable (medidas)
  })
  
  output$downloadAvaliacao = downloadHandler (
    filename = function ( ) {
      paste("serieAvaliacao.csv",sep="")
    },
    content = function (file) {
      medidas = avaliacaoSint
      rownames (medidas) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      colnames (medidas) = c ("Media", "Desvio-padrao","Indice Kurt","Assimetria","Coeficiente de Variacao")
      write.table(medidas, file,
                  sep = ";",
                  dec = ",",
                  row.names = T,
                  col.names = NA)
    })
  
  return(avaliacaoSint)
    
}

# Module avaliacaoAnual: Esse "module" realiza a avaliacao da serie sintetica anual.

# UI: Gráfico que compara a media e o desvio padrao da serie historica com a serie sintetica, Tabela com a media, desvio
# padrao, kurt, assimetria e coeficiente de variadrao, e um botao para baixar a avaliacao da serie sintetica.

# Server: retorna um data.frame com a avaliacao da serie sintetica (media, desvio padrao,kurt, assimetria e 
# coeficiente de variacao).

avaliacaoAnualOutput <- function(id){
  
  # Criando o namespace
  ns <- NS(id)
  
  tagList( 
    dataTableOutput(ns("tabelaAvaliacaoAnual")),
    downloadButton (ns("downloadAvaliacaoAnual"), "Download Avaliacoes", icon ("save"))
  )
  
  
}

avaliacaoAnual <- function(input,output,session,serieHistAnual,serieSintAnual){
  
  mediaSint = mean(serieSintAnual)
  desvioSint = sd(serieSintAnual)
  kurtSint = kurtosis(serieSintAnual)
  assimetriaSint = skewness(serieSintAnual)
  coefVarSint = desvioSint/mediaSint
  
  
  mediaHist = mean (serieHistAnual)
  desvioHist = sd (serieHistAnual)
  kurtHist = kurtosis(serieHistAnual)
  assimetriaHist = skewness(serieHistAnual)
  coefVarHist = desvioHist/mediaHist
  
  media = c(mediaSint,mediaHist)
  desvio = c(desvioSint,desvioHist)
  kurt = c(kurtSint,kurtHist)
  assimetria = c(assimetriaSint,assimetriaHist)
  coefVar = c(coefVarSint,coefVarHist)
  
  avaliacaoSintAnual = data.frame(mediaSint,desvioSint,kurtSint,assimetriaSint,coefVarSint)
  colnames(avaliacaoSintAnual) = c("mediaSint","desvioSint","kurtSint","assimetriaSint","coefVarSint")
  
  output$tabelaAvaliacaoAnual = renderDataTable({
    medidas = data.frame (media,desvio,kurt,assimetria,coefVar)
    colnames (medidas) = c ("Media", "Desvio-padrao", "Indice Kurt","Assimetria","Coeficiente de Variacao")
    rownames(medidas) = c("Sintetico","Historico")
    datatable (medidas)
  })
  
  output$downloadAvaliacaoAnual = downloadHandler (
    filename = function ( ) {
      paste("serieAvaliacaoAnual.csv",sep="")
    },
    content = function (file) {
      
      medidas = avaliacaoSintAnual
      colnames (medidas) = c ("Media", "Desvio-padrao","Indice Kurt","Assimetria","Coeficiente de Variacao")
      write.table(medidas, file,
                  sep = ";",
                  dec = ",",
                  row.names = T,
                  col.names = NA)
    })
  
  return(avaliacaoSintAnual)
  
}