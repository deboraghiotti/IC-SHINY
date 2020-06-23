## SERVER: TABPANEL SERIES GERADAS 

SSTable <- SeriesSinteticas()
SDTable <- SeriesDesagregadas()
output$SeriesSinteticas<- DT::renderDataTable(SSTable,server = TRUE, selection = 'single')
output$SeriesDesagregadas <- DT::renderDataTable(SDTable,server = TRUE, selection = 'single')

observeEvent(input$selecionar_ss_button,{
  
  shinyjs::disable("selecionar_ss_button")
  SSTable <- SeriesSinteticas()
  selectedrowindex <<- input$SeriesSinteticas_rows_selected[length(input$SeriesSinteticas_rows_selected)]
  selectedrowindex <<- as.numeric(selectedrowindex)
  idSerieSintetica <- (SSTable[selectedrowindex,ID])
  nomeEstacao <- SSTable[selectedrowindex,Estacao]
  
  serieHistorica <- valorSH('',nomeEstacao)
  serieSintetica = selectSerie_Sintetica(idSerieSintetica)
  
  shinyjs::show("ss_resultados")
  shinyjs::show("grafico_ss_panel")
  output$GraficoSS = renderPlot ({
    inicializaGraficoSERIE (serieHistorica)
    graficoSERIE (serieSintetica, 'cornflowerblue')
    graficoSERIE (serieHistorica, 'blue')
    
  })
  
  shinyjs::show("acf_mensal_ss_panel")
  acfMensal = buscarACF_MENSAL_SS(idSerieSintetica)
  output$AcfMensal_SS_Table <- DT::renderDataTable(acfMensal)
  
  shinyjs::show("acf_anual_ss_panel")
  acfAnual <- buscarACF_ANUAL_SS(idSerieSintetica)
  output$AcfAnual_SS_Table <- DT::renderDataTable(acfAnual)
  
  shinyjs::show("hurst_ss_panel")
  output$Hurst_SS_Table <- DT::renderDataTable(buscarHURST_SS(idSerieSintetica))
  
  #shinyjs::show("avaliacao_ss_panel")
  avaliacoes <- buscarAVALIACAO_SS(idSerieSintetica)
  output$Avaliacao_SS_Table <- DT::renderDataTable(avaliacoes)
  
  shinyjs::show("volume_ss_panel")
  output$Volume_SS_Table <- DT::renderDataTable(buscarVOLUME_SS(idSerieSintetica))
  
  shinyjs::show("soma_ss_panel")
  output$Soma_SS_Table <- DT::renderDataTable(buscarSOMARESIDUAL_SS(idSerieSintetica))
  
  output$downloadSerie_Gerada = downloadHandler (
    filename = function ( ) {
      paste("serieSintetica.csv",sep="")
    },
    content = function (file) {
      colunas = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      write.table(data.frame (serieSintetica), file,
                  col.names = colunas,
                  row.names = F,
                  sep = ";",
                  dec = ",")
    })
  
  output$downloadAvaliacoes_Gerada = downloadHandler (
    filename = function ( ) {
      paste("serieAvaliacoes.csv",sep="")
    },
    content = function (file) {
      MediaSint = avaliacoes[,2]
      DesvioSint = avaliacoes[,3]
      KurtSint = avaliacoes[,5]
      AssimetriaSint = avaliacoes[,4]
      CoefVarSint = avaliacoes[,6]
      medidas = data.frame (MediaSint,DesvioSint,KurtSint,AssimetriaSint,CoefVarSint)
      rownames (medidas) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      colnames (medidas) = c ("Media", "Desvio-padrao","Indice Kurt","Assimetria","Coeficiente de Variacao")
      write.table(medidas, file,
                  sep = ";",
                  dec = ",",
                  row.names = T,
                  col.names = NA)
    })
  
  output$downloadTabelaAnual_Gerada = downloadHandler (
    filename = function ( ) {
      paste0 ("serieFACAnual", ".csv")
    },
    content = function (file) {
      tabela = data.frame (acfAnual$VALOR)
      colnames (tabela) = c (("FAC"))
      rownames (tabela) = c (paste ("lag", 1:12))
      write.table (tabela, file, col.names = NA, row.names = T,
                   sep = ";",
                   dec = ",")
    })
  
  output$downloadTabelaMensal_Gerada = downloadHandler (
    filename = function ( ) {
      paste0 ("serieFACMensal", ".csv")
    },
    content = function (file) {
      tabela = data.frame (acfMensal[,2:13])
      colnames (tabela) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      rownames (tabela) = c (paste ("lag", 1:12))
      write.table (tabela, file, col.names = NA, row.names = T,
                   sep = ";",
                   dec = ",")
    })
  
  
})


observeEvent(input$selecionar_sd_button,{
  
  shinyjs::disable("selecionar_sd_button")
  shinyjs::enable("delete_sd_button")
  selectedrowindex <<- input$SeriesDesagregadas_rows_selected[length(input$SeriesDesagregadas_rows_selected)]
  selectedrowindex <<- as.numeric(selectedrowindex)
  idSerieDesagregada <- (SDTable[selectedrowindex,ID])
  
  serieDesagregada = selectSerie_Desagregada(idSerieDesagregada)
  
  shinyjs::show("sd_resultados")
  shinyjs::show("acf_mensal_sd_panel")
  acfMensal_sd = buscarACF_MENSAL_SD(idSerieDesagregada)
  output$AcfMensal_SD_Table <- DT::renderDataTable(acfMensal_sd)
  
  shinyjs::show("acf_anual_sd_panel")
  acfAnual_sd = buscarACF_ANUAL_SD(idSerieDesagregada)
  output$AcfAnual_SD_Table <- DT::renderDataTable(acfAnual_sd)
  
  shinyjs::show("hurst_sd_panel")
  output$Hurst_SD_Table <- DT::renderDataTable(buscarHURST_SD(idSerieDesagregada))
  
  shinyjs::show("avaliacao_sd_panel")
  avaliacoes_sd = buscarAVALIACAO_SD(idSerieDesagregada)
  output$Avaliacao_SD_Table <- DT::renderDataTable(avaliacoes_sd)
  
  shinyjs::show("volume_sd_panel")
  output$Volume_SD_Table <- DT::renderDataTable(buscarVOLUME_SD(idSerieDesagregada))
  
  shinyjs::show("soma_sd_panel")
  output$Soma_SD_Table <- DT::renderDataTable(buscarSOMARESIDUAL_SD(idSerieDesagregada))
  
  output$downloadSerie_Gerada_SD = downloadHandler (
    filename = function ( ) {
      paste("serieDesagregada.csv",sep="")
    },
    content = function (file) {
      colunas = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      write.table(data.frame (serieDesagregada), file,
                  col.names = colunas,
                  row.names = F,
                  sep = ";",
                  dec = ",")
    })
  
  output$downloadAvaliacoes_Gerada_SD = downloadHandler (
    filename = function ( ) {
      paste("serieAvaliacoes.csv",sep="")
    },
    content = function (file) {
      MediaSint = avaliacoes_sd[,2]
      DesvioSint = avaliacoes_sd[,3]
      KurtSint = avaliacoes_sd[,5]
      AssimetriaSint = avaliacoes_sd[,4]
      CoefVarSint = avaliacoes_sd[,6]
      medidas = data.frame (MediaSint,DesvioSint,KurtSint,AssimetriaSint,CoefVarSint)
      rownames (medidas) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      colnames (medidas) = c ("Media", "Desvio-padrao","Indice Kurt","Assimetria","Coeficiente de Variacao")
      write.table(medidas, file,
                  sep = ";",
                  dec = ",",
                  row.names = T,
                  col.names = NA)
    })
  
  output$downloadTabelaMensal_Gerada_SD = downloadHandler (
    filename = function ( ) {
      paste0 ("serieFACMensal", ".csv")
    },
    content = function (file) {
      tabela = data.frame (acfMensal_sd[,2:13])
      colnames (tabela) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      rownames (tabela) = c (paste ("lag", 1:12))
      write.table (tabela, file, col.names = NA, row.names = T,
                   sep = ";",
                   dec = ",")
    })
  
  output$downloadTabelaAnual_Gerada_SD = downloadHandler (
    filename = function ( ) {
      paste0 ("serieFACAnual", ".csv")
    },
    content = function (file) {
      tabela = data.frame (acfAnual_sd$VALOR)
      colnames (tabela) = c (("FAC"))
      rownames (tabela) = c (paste ("lag", 1:12))
      write.table (tabela, file, col.names = NA, row.names = T,
                   sep = ";",
                   dec = ",")
    })
})

#DELETAR SERIES DO BANCO DE DADOS
observeEvent(input$delete_ss_button,{ 
  SSTable <- SeriesSinteticas()
  selectedrowindex <<- input$SeriesSinteticas_rows_selected[length(input$SeriesSinteticas_rows_selected)]
  selectedrowindex <<- as.numeric(selectedrowindex)
  idSerieSintetica <- (SSTable[selectedrowindex,ID])
  deleteSerieSS(idSerieSintetica)
  shinyalert("Deletado!","A Serie foi deletada com sucesso", type = "success")
  output$SeriesSinteticas<- DT::renderDataTable(SeriesSinteticas(),server = TRUE, selection = 'single')
  shinyjs::enable("selecionar_ss_button")
  shinyjs::hide("ss_resultados")
  shinyjs::hide("acf_mensal_ss_panel")
  shinyjs::hide("acf_anual_ss_panel")
  shinyjs::hide("hurst_ss_panel")
  shinyjs::hide("avaliacao_ss_panel")
  shinyjs::hide("volume_ss_panel")
  shinyjs::hide("soma_ss_panel")
  
})

#DELETAR SERIES DO BANCO DE DADOS
observeEvent(input$delete_sd_button,{ 
  SDTable <- SeriesDesagregadas()
  selectedrowindex <<- input$SeriesDesagregadas_rows_selected[length(input$SeriesDesagregadas_rows_selected)]
  selectedrowindex <<- as.numeric(selectedrowindex)
  idDesagregado <- (SDTable[selectedrowindex,ID])
  deleteSerieSD(idDesagregado)
  shinyalert("Deletado!","A Serie foi deletada com sucesso", type = "success")
  output$SeriesDesagregadas<- DT::renderDataTable(SeriesDesagregadas(),server = TRUE, selection = 'single')
  shinyjs::enable("selecionar_sd_button")
  shinyjs::hide("sd_resultados")
  shinyjs::hide("acf_mensal_sd_panel")
  shinyjs::hide("acf_anual_sd_panel")
  shinyjs::hide("hurst_sd_panel")
  shinyjs::hide("avaliacao_sd_panel")
  shinyjs::hide("volume_sd_panel")
  shinyjs::hide("soma_sd_panel")
  
})


#LIMPANDO UMA CONSULTA
observeEvent(input$limpar_ss_button,{
  shinyjs::enable("selecionar_ss_button")
  shinyjs::hide("ss_resultados")
  shinyjs::hide("acf_mensal_ss_panel")
  shinyjs::hide("acf_anual_ss_panel")
  shinyjs::hide("hurst_ss_panel")
  shinyjs::hide("avaliacao_ss_panel")
  shinyjs::hide("volume_ss_panel")
  shinyjs::hide("soma_ss_panel")
})

observeEvent(input$limpar_sd_button,{
  shinyjs::enable("selecionar_sd_button")
  shinyjs::disable("delete_sd_button")
  shinyjs::hide("sd_resultados")
  shinyjs::hide("acf_mensal_sd_panel")
  shinyjs::hide("acf_anual_sd_panel")
  shinyjs::hide("hurst_sd_panel")
  shinyjs::hide("avaliacao_sd_panel")
  shinyjs::hide("volume_sd_panel")
  shinyjs::hide("soma_sd_panel")
})