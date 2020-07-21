# Modularizacao
# Fonte: https://shiny.rstudio.com/articles/modules.html

# Module avaliacaoMensal: Esse "module" realiza a avaliacao da serie sintetica mensal.

# UI: Gráfico que compara a media e o desvio padrao da serie historica com a serie sintetica, Tabela com a media, desvio
# padrao, kurt, assimetria e coeficiente de variadrao, e um botao para baixar a avaliacao da serie sintetica.

# Server: retorna um data.frame com a avaliacao da serie sintetica (media, desvio padrao,kurt, assimetria e 
# coeficiente de variacao).

library(DT)

# a custom table container
sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Mes',class = 'dt-center'),
      th(colspan = 2, 'Media',class = 'dt-center'),
      th(colspan = 2, 'Desvio-Padrao',class = 'dt-center'),
      th(colspan = 2, 'Curtose',class = 'dt-center'),
      th(colspan = 2, 'Assimetria',class = 'dt-center'),
      th(colspan = 2, 'Coef. de Variacao',class = 'dt-center'),
    ),
    tr(
      lapply(rep(c('Hist', 'Sint'), 5), th)
    )
  )
))

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
  
  mediaSint = reactive(apply (serieSint(), 2, mean))
  desvioSint = reactive(apply (serieSint(), 2, sd))
  kurtSint = reactive(apply(serieSint(),2,kurtosis))
  assimetriaSint = reactive(apply(serieSint(),2,skewness))
  coefVarSint = reactive(desvioSint()/mediaSint())
  
  mediaHist = reactive(apply (serieHist(), 2, mean))
  desvioHist = reactive(apply (serieHist(), 2, sd))
  kurtHist = reactive(apply(serieHist(),2,kurtosis))
  assimetriaHist = reactive(apply(serieHist(),2,skewness))
  coefVarHist = reactive(desvioHist()/mediaHist())
  
  avaliacaoSint = reactive({
    avaliacao = data.frame (mediaSint(),desvioSint(),kurtSint(),assimetriaSint(),coefVarSint())
    colnames(avaliacao) = c("mediaSint","desvioSint","kurtSint","assimetriaSint","coefVarSint")
    rownames (avaliacao) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
  })
  
  output$graficoSerie = renderPlot ({
      inicializaGraficoSERIE(serieHist())
      graficoSERIE (serieHist(), 'cornflowerblue')
      graficoSERIE (serieSint(), 'blue')
  })
  
  output$tabelaAvaliacao = renderDataTable ({
      
      medidas = round(data.frame (mediaHist(), mediaSint(), desvioHist(), desvioSint(),kurtHist(),kurtSint(),assimetriaHist(),
                                  assimetriaSint(),coefVarHist(),coefVarSint()),digits = 3)
      rownames (medidas) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      datatable (medidas,container = sketch,options = list(
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ))
  })
  
  output$downloadAvaliacao = downloadHandler (
    filename = function ( ) {
      paste("serieAvaliacao.csv",sep="")
    },
    content = function (file) {
      medidas = data.frame (mediaSint(),desvioSint(),kurtSint(),assimetriaSint(),coefVarSint())
      rownames (medidas) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      colnames (medidas) = c ("Media", "Desvio-padrao","Indice Kurt","Assimetria","Coeficiente de Variacao")
      write.table(medidas, file,
                  sep = ";",
                  dec = ",",
                  row.names = T,
                  col.names = NA)
    })
  
  return(list(media = mediaSint,desvioPadrao = desvioSint,kurt = kurtSint,assimetria = assimetriaSint,coefVar = coefVarSint))
    
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
  
  mediaSint = reactive(mean(serieSintAnual()))
  desvioSint = reactive(sd(serieSintAnual()))
  kurtSint = reactive(kurtosis(serieSintAnual()))
  assimetriaSint = reactive(skewness(serieSintAnual()))
  coefVarSint = reactive(desvioSint()/mediaSint())
  
  mediaHist = reactive(mean (serieHistAnual()))
  desvioHist = reactive(sd (serieHistAnual()))
  kurtHist = reactive(kurtosis(serieHistAnual()))
  assimetriaHist = reactive(skewness(serieHistAnual()))
  coefVarHist = reactive(desvioHist()/mediaHist())
  
  media = reactive(c(mediaSint(),mediaHist()))
  desvio = reactive(c(desvioSint(),desvioHist()))
  kurt = reactive(c(kurtSint(),kurtHist()))
  assimetria = reactive(c(assimetriaSint(),assimetriaHist()))
  coefVar = reactive(c(coefVarSint(),coefVarHist()))
  
  avaliacaoSintAnual = reactive({
    avaliacao = data.frame(mediaSint(),desvioSint(),kurtSint(),assimetriaSint(),coefVarSint())
    colnames(avaliacao) = c("mediaSint","desvioSint","kurtSint","assimetriaSint","coefVarSint")
  })
  output$tabelaAvaliacaoAnual = renderDataTable({
    medidas = data.frame (media(),desvio(),kurt(),assimetria(),coefVar())
    colnames (medidas) = c ("Media", "Desvio-padrao", "Indice Kurt","Assimetria","Coeficiente de Variacao")
    rownames(medidas) = c("Sintetico","Historico")
    datatable (medidas)
  })
  
  output$downloadAvaliacaoAnual = downloadHandler (
    filename = function ( ) {
      paste("serieAvaliacaoAnual.csv",sep="")
    },
    content = function (file) {
      
      medidas = data.frame(mediaSint(),desvioSint(),kurtSint(),assimetriaSint(),coefVarSint())
      colnames (medidas) = c ("Media", "Desvio-padrao","Indice Kurt","Assimetria","Coeficiente de Variacao")
      write.table(medidas, file,
                  sep = ";",
                  dec = ",",
                  row.names = T,
                  col.names = NA)
    })
  
  return(list(media = mediaSint,desvioPadrao = desvioSint,kurt = kurtSint,assimetria = assimetriaSint,coefVar = coefVarSint))
  
}

# Module facAnual: Esse "module" calcula a FAC ANUAL da serie sintetica (anual).

# UI: Gráfico da FAC ANUAL (historico vs sintetico), a autocorrelacao anual e um botao para download da autocorrelacao 
# sintetica anual

facAnualOutput <- function(id){
  
  # Criando o namespace
  ns <- NS(id)
  
  tagList(
    
    plotOutput(ns("graficoFacAnual")),
    dataTableOutput(ns("tabelaFacAnual")),
    downloadButton (ns("downloadFacAnual"), "Download FAC Anual", icon ("save"))
    
  )
  
}

facAnual <- function(input,output,session,serieHistAnual,serieSintAnual){
  
  acfAnual = reactive({
    autocorrelacaoAnual (serieSintAnual(), 12)
  })
  
  
  
  output$graficoFacAnual = renderPlot ({
      inicializaGraficoFACANUAL (serieHistAnual(), 12)
      graficoFACANUAL (serieHistAnual(), 12, 'cornflowerblue')
      graficoFACANUAL (serieSintAnual(), 12, 'blue')
  })
  
  output$tabelaFacAnual = renderDataTable ({
      acf = data.frame (as.vector (acfAnual()[-1]))
      rownames (acf) = paste ("lag", 1:12)
      colnames (acf) = c (("FAC"))
      datatable (acf)
  })
  
  output$downloadFacAnual = downloadHandler (
    filename = function ( ) {
      paste0 ("serieFACAnual", ".csv")
    },
    content = function (file) {
      acf = data.frame (as.vector (acfAnual()[-1]))
      rownames (acf) = paste ("lag", 1:12)
      colnames (acf) = c (("FAC"))
      write.table (acf, file, col.names = NA, row.names = T,
                   sep = ";",
                   dec = ",")
    })
  
  return(acfAnual)
  
}


# Module facMensal: Esse "module" calcula a FAC Mensal da serie sintetica (mensal).

# UI: Gráfico da FAC Mensal (historico vs sintetico) para cada lag, select input para escolher o lag do grafico,
# a autocorrelacao mensal e um botao para download da autocorrelacao sintetica mensal

facMensalOutput <- function(id){
  
  # Criando o namespace
  
  ns <- NS(id)
  
  tagList( 
    selectInput (ns("lagFacMensal"), "lag mensal analisado:", choices = 1:12, selected = 1),
    plotOutput (ns("graficoFacMensal")),
    dataTableOutput (ns("tabelaFacMensal")),
    downloadButton (ns("downloadFacMensal"), "Download FAC Mensal", icon ("save"))
  )
  
}

facMensal <- function(input,output,session,serieHist,serieSint){
  
  acfMensal = reactive({
    
    autocorrelacaoMensal (serieSint(), 12)
  })
  
  output$graficoFacMensal = renderPlot ({
      inicializaGraficoMENSAL (serieHist(), as.numeric (input$lagFacMensal))
      graficoFACMENSAL (serieHist(), as.numeric (input$lagFacMensal), 'cornflowerblue')
      graficoFACMENSAL (serieSint(), as.numeric (input$lagFacMensal), 'blue')
  })
  
  output$tabelaFacMensal = renderDataTable ({
      acf = data.frame (acfMensal()[-1, ])
      colnames (acf) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      rownames (acf) = paste ("lag", 1:12)
      datatable (round(acf,digits = 5))
  })
  
  output$downloadFacMensal = downloadHandler (
    filename = function ( ) {
      paste0 ("serieFACMensal", ".csv")
    },
    content = function (file) {
      acf = data.frame (acfMensal()[-1, ])
      colnames (acf) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      rownames (acf) = paste ("lag", 1:12)
      write.table (acf, file, col.names = NA, row.names = T,
                   sep = ";",
                   dec = ",")
    })
  
  return(acfMensal)
  
}


# Module coeficienteHurst
# UI: Exibe os resultados do coeficiente hurst sintetico e historico


coeficienteHurstOutput <- function(id){
  
  # Criando o namespace
  
  ns <- NS(id)
  
  tagList(
    
    verbatimTextOutput (ns("hurst"))
  )
  
  
}

coeficienteHurst <- function(input,output,session,tipo,serieHist,serieSint){
  hurstHist = reactive(Hurst (as.vector (serieHist())))
  hurstSint = reactive(Hurst (as.vector (serieSint( ))))
  
  output$hurst = renderPrint ({
      print (paste("Hurst",tipo))
      print (paste("Historico: ",hurstHist()))
      print (paste("Sintetico:", hurstSint()))

  })
  
  return(hurstSint)
}

# Module volume
# UI: slider para selecionar a porcentagem de regularicao e o volume sintetico e historico
# server: tipo( FALSE para series anuais e TRUE para series sinteticas), a serie historica e a serie sintetica 
volumeOutput <- function(id){
  
  ns <- NS(id)
  
  tagList(
    h4(strong("Volume Util")),
    fluidRow (
      column (width = 6,
              sliderInput (ns("porcentagemRegularizacao"), "Porcentagem de regularizacao", min = 0, max = 100, value = 50, width = "100%")
      ),
      column (width = 6,
              verbatimTextOutput (ns("volumeUtil"))
      )
    ),
  )
  
}

# tipo: FALSE para series anuais e TRUE para series sinteticas
volume <- function(input,output,session,tipo,serieHist,serieSint){
  
  volumeHist = reactive({
      volumeUtil (serieHist(), (input$porcentagemRegularizacao/100), tipo)
  })
  
  volumeSint = reactive({
    volumeUtil (serieSint(), (input$porcentagemRegularizacao/100), tipo)  
  })
  
  output$volumeUtil = renderPrint ({
    print ("Serie historica")
    print (paste (volumeHist(), "m^3"))
    print ("Serie sintetica")
    print (paste (volumeSint(), "m^3"))
  })
  
  return(volumeSint)
  
}

