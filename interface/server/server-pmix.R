## SERVER: TABPANEL MODELO PMIX
# INPUT DO MODELO PMIX 
estacao = loadData("ESTACAO")
updateSelectInput(session, "estacoes",
                  choices = estacao$nome,
                  selected = NULL)

serieHist = reactive({
  output$estacaoSelecionada <- renderText(input$estacoes)
  serieH <- valorSH('',input$estacoes)
})

serieHistAnual = reactive ({
  apply (serieHist ( ), 1, sum)
})

# Funcao Algoritmo roda o modelo PMIX.

funcaoAlgoritmo = reactive({
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Calculando o Pmix", value = 0)
  
  if (input$iniciar)
    isolate (algoritmo (input,serieHist))
  
})

########## Serie Gerada pelo Modelo PMIX

serieEscolhida = reactive ({
  serieS = funcaoAlgoritmo ( )$arqSeries
  if (input$tipo == 2) {
    serieS = serieS[[as.numeric (input$nSerie)]]
  }
  
  return (serieS)
})

serieEscolhidaAnual = reactive ({
  apply (serieEscolhida ( ), 1, sum)
})

########## Avalicao da serie sintetica gerada pelo pmix

volumePMIX <- callModule(volume,"PMIX",TRUE,serieHist,serieEscolhida)
avaliacaoMensalPMIX <- callModule(avaliacaoMensal,"PMIX",serieHist,serieEscolhida)
acfAnualPMIX <- callModule(facAnual,"PMIX",serieHistAnual,serieEscolhidaAnual)
acfMensalPMIX <- callModule(facMensal,"PMIX",serieHist,serieEscolhida)
hurstMensalPMIX <- callModule(coeficienteHurst,"PMIX-Mensal","Mensal",serieHist,serieEscolhida)
hurstAnualPMIX <- callModule(coeficienteHurst,"PMIX-Anual","Anual",serieHistAnual,serieEscolhidaAnual)

observeEvent(input$iniciar,{
  
  shinyjs::enable("limparButton_PMIX")
  shinyjs::disable("parametros_PMIX")
  shinyjs::disable("iniciar")
  
  ########## Resultados da serie gerada pelo Modelo PMIX
  
  output$resultadoGeral = renderPrint ({
    if (input$iniciar == 0)
      return ("Aguardando inicio...")
    
    duracao = funcaoAlgoritmo ( )$duracao
    print (paste ("Duracao:", duracao, "seg"))
    
    if (input$tipo == 1) {
      ciclos = funcaoAlgoritmo ( )$algoritmo$ciclos
      somRes = funcaoAlgoritmo ( )$algoritmo$somRes
      
      print ("Metodo de Powell")
      print (paste ("ciclos: ", ciclos))
      print (paste ("Somatorio dos residuos:", somRes))
    }
    else {
      ciclos = funcaoAlgoritmo ( )$algoritmo$ciclos
      print("Algoritmo Genetico")
      print (paste ("ciclos: ", ciclos))
      
    }
  })
  
  output$tabelaAvaliacao = renderDataTable ({
    if (input$iniciar){
      if (input$tipo == 1) {
        parametros = funcaoAlgoritmo ( )$arqParametros
        
        phi = matrix (0, ncol = 12)
        tht = matrix (0, ncol = 12)
        PHI = matrix (0, ncol = 12)
        THT = matrix (0, ncol = 12)
        
        limInf = 0
        limSup = 0
        
        if (input$p > 0) {
          limInf = 1
          limSup = 12*input$p
          phi = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
        }
        if (input$q > 0) {
          limInf = limSup + 1
          limSup = limInf + 12*input$q - 1
          tht = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
        }
        if (input$P > 0) {
          limInf = limSup + 1
          limSup = limInf + 12*input$P - 1
          PHI = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
        }
        if (input$Q > 0) {
          limInf = limSup + 1
          limSup = limInf + 12*input$Q - 1
          THT = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
        }
        
        parametrosPowell = data.frame (t (phi), t (tht), t (PHI), t (THT))
        colnames (parametrosPowell) = c (rep ("phi", max (1, input$p)), rep ("tht", max (1, input$q)), rep ("PHI", max (1, input$P)), rep ("THT", max (1, input$Q)))
        rownames (parametrosPowell) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
        return (datatable (parametrosPowell))
      }
      else {
        avaliacoes = data.frame (funcaoAlgoritmo ( )$arqAvaliacoes)
        colnames (avaliacoes) = c ("MAPE media", "MAPE desvio", "MAPE FAC anual", "MAPE FAC mensal", "Soma Residual")
        rownames (avaliacoes) = paste ("Serie", 1:input$nPop)
        return (datatable (avaliacoes))
      }
    }
  })
  
  ########## Grafico que compara as 50 series geradas pelo Algoritmo Genetico
  
  output$grafico_avaliacoes = renderPlotly({
    dados = data.frame (funcaoAlgoritmo ( )$arqAvaliacoes)
    dados$X = 1:nrow(dados)
    dd = replicate(2, dados, simplify = F)
    dd[[2]]$MAPEdp = 0
    d = group2NA(dplyr::bind_rows(dd), "X")
    
    plot_ly(color = I("orange"), showlegend = F, text = ~X,
            hovertemplate = paste(
              "<b>Serie: %{text}</b><br>",
              "MAPEfacAnual: %{x}<br>",
              "MAPEfacMensal: %{y}<br>",
              "MAPEdp: %{z}",
              "<extra></extra>"
            )) %>%
      add_markers(data = dados, x = ~MAPEfacAnual, y = ~MAPEfacMensal, z = ~MAPEdp) %>%
      add_paths(data = d, x = ~MAPEfacAnual, y = ~MAPEfacMensal, z = ~MAPEdp)
  })
  
  observe({
    funcaoAlgoritmo()
    shinyjs::show("resultados_PMIX")
  })
  
  # Download da serie gerada pelo MODELO PMIX
  output$downloadSerie = downloadHandler (
    filename = function ( ) {
      paste("serie_", input$nSerie, ".csv",sep="")
    },
    content = function (file) {
      colunas = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      write.table(data.frame (serieEscolhida ( )), file,
                  col.names = colunas,
                  row.names = F,
                  sep = ";",
                  dec = ",")
    })
  
})

########## Armazenamento da serie gerada pelo MODELO PMIX no Banco de Dados
observeEvent(input$armazenarBD,{
  
  shinyjs::disable("armazenarBD")
  shinyjs::show("armazenando_msg")
  shinyjs::hide("error_armazenar")
  
  tryCatch({ 
    serieArmazenar = serieEscolhida()
    serieArmazenarAnual = serieEscolhidaAnual()
    
    #Tabela Avaliacao
    MediaArmazenar = avaliacaoMensalPMIX$media() 
    DesvioArmazenar = avaliacaoMensalPMIX$desvioPadrao() 
    KurtArmazenar = avaliacaoMensalPMIX$kurt() 
    AssimetriaArmazenar = avaliacaoMensalPMIX$assimetria() 
    CoefVarArmazenar = avaliacaoMensalPMIX$coefVar() 
    
    #Tabela Acf_anual
    acfAnual = data.frame (as.vector (acfAnualPMIX()[-1]))
    
    #Table Acf_Mensal
    acfMensal = data.frame (acfMensalPMIX()[-1, ])
    
    #Tabela Volume
    volumeArmazenar = volumePMIX()
    
    #Tabela Hurst
    HurstMensalArmazenar = hurstMensalPMIX()
    HurstAnualArmazenar = hurstAnualPMIX()
    
    #Tabela soma_residual
    somReSint = NULL
    if(input$tipo == 1){ 
      somReSint = funcaoAlgoritmo ( )$algoritmo$somRes
    }else{
      somReSint = funcaoAlgoritmo ( )$arqAvaliacoes$SomRes[1]
    }
    
    idEstacao <- findID(estacao,input$estacoes)
    idSERIE_SINTETICA <- registrarSSPMIX(input,idEstacao)
    inserirSS(idSERIE_SINTETICA,serieArmazenar)
    inserirAvaliacaoSS(idSERIE_SINTETICA,MediaArmazenar,DesvioArmazenar,AssimetriaArmazenar,KurtArmazenar,CoefVarArmazenar)
    inserirACF_MensalSS(idSERIE_SINTETICA,acfMensal)
    inserirACF_ANUALSS(idSERIE_SINTETICA,acfAnual)
    inserirSomHurstVol(idSERIE_SINTETICA,somReSint,HurstAnualArmazenar,HurstMensalArmazenar,volumeArmazenar)
    shinyalert("Armazenado!","A serie foi armazenada com sucesso", type = "success")
    
  },
  error = function(err) {
    shinyjs::hide("armazenando_msg")
    shinyjs::html("error_msg_armazenar", err$message)
    shinyjs::show(id = "error_armazenar", anim = TRUE, animType = "fade")
  },
  finally = {
    shinyjs::hide("armazenando_msg")
    SSTable <- SeriesDesagregacao <- SeriesSinteticas()
    output$SeriesSinteticas<- DT::renderDataTable(SSTable,server = TRUE, selection = 'single')
    output$SeriesDesagregacao <- DT::renderDataTable(SeriesDesagregacao,server = TRUE, selection = 'single')
    
  })
})


observeEvent(input$limparButton_PMIX,{
  shinyjs::enable("armazenarBD")
  shinyjs::enable("iniciar")
  shinyjs::enable("parametros_PMIX")
  shinyjs::disable("limparButton_PMIX")
  shinyjs::hide("resultados_PMIX")
  shinyjs::reset("resultados_PMIX")
  output$tabelaAvaliacao = renderDataTable ({})
  
})

observeEvent(input$tipo,{
  if(input$tipo == 2){
    shinyjs::show("parametros_ag")
  }else{
    shinyjs::hide("parametros_ag")
  }
})

observe({
  if (input$iniciar){
    if (input$tipo == 1){
      shinyjs::hide("plotly_avaliacoes")
    }else{
      shinyjs::show("plotly_avaliacoes")
    }
  }
})

observe ({
  if (input$tipo == 1) {
    updateSelectInput(session, "nSerie",
                      choices = 1,
                      selected = 1)
  }else {
    updateSelectInput (session, "nSerie",
                       choices = 1:input$nPop,
                       selected = input$nPop)
  }
})