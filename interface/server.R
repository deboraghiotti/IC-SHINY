source('mysql/mysql-functions.R')
source('mysql/mysql-pmix.R')
source('mysql/mysql-arma.R')
source('mysql/mysql-desagregacao.R')

source('auxiliar.R')
source('desagregacao.R')
source('modelo/cenarioAnual.R')
source('modules.R')

source('tabs/tab-desagregacao.R')
source('tabs/tab-series-geradas.R')
source('tabs/tab-ARMA.R')
source('tabs/tab-PMIX.R')
source('tabs/tab-estacoes.R')

library('data.table')
library(e1071)
library(shinyalert)
library(plotly)


function (input, output, session) {
  
  ############################## MODELO PMIX ##############################
  
  ########## INPUT DO MODELO PMIX 
  
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
 
  ########## Funcao Algoritmo roda o modelo PMIX.
  
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
      SSTable <- SeriesSinteticas()
      output$SeriesSinteticas<- DT::renderDataTable(SSTable,server = TRUE, selection = 'single')
      
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
    }
    else {
      updateSelectInput (session, "nSerie",
                         choices = 1:input$nPop,
                         selected = input$nPop
      )
    }
  })
  
  
  ############################### TABPANEL: DADOS HISTORICOS ##############################
  ############### CADASTRO DE UMA ESTACAO 
  
  observe({
    # Campos obrigatorios para cadastrar uma estacao: nome, codigo e o arquivo com a serie historica
    camposObrigatorios <- c("nomeEstacao", "codigoEstacao","fileSH")
    mandatoryFilled <- vapply(camposObrigatorios,function(x) {!is.null(input[[x]]) && input[[x]] != ""},logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # Se todos os campos forem completados, o botao de cadastrar é habiltado
     shinyjs::toggleState(id = "cadastrar", condition = mandatoryFilled)
  })
  
  observeEvent(input$cadastrar,{
    
    #Inserindo a estacao no banco de dados
    
    shinyjs::disable("cadastrar")
    shinyjs::show("cadastrando_msg")
    shinyjs::hide("error")
    
    tryCatch({
      cadastroEstacao(input$nomeEstacao,input$codigoEstacao,input$rioEstacao,input$anosEstacao,input$areaEstacao,input$latEstacao,input$lngEstacao)
      cadastrarSH(input$fileSH$datapath,input$codigoEstacao)
      shinyjs::reset("form")
      shinyalert("Cadastrado!","A Estacao foi cadastrada com sucesso", type = "success")
      
      estacao = loadData("ESTACAO")
      updateSelectInput(session, "estacoes",
                        choices = estacao$nome,
                        selected = NULL)
      updateSelectInput(session, "consultaEstacoes",
                        choices = estacao$nome,
                        selected = NULL)
      
    },
    error = function(err) {
      shinyjs::hide("cadastrando_msg")
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::hide("cadastrando_msg")
      shinyjs::enable("cadastrar")
      
    })
    
  })
  
  observeEvent(input$cadastrar_outra, {
    shinyjs::hide("cadastro_realizado_msg")
    shinyjs::hide("error")
    shinyjs::show("form")
    shinyjs::hide("cadastro_realizado")
  })  
  
  ############## CONSULTAR ESTACAO 
  updateSelectInput(session, "consultaEstacoes",
                    choices = estacao$nome,
                    selected = NULL)
  
  observeEvent(input$ConsultarButton,{
    
    serieHistConsulta = reactive({
      output$estacaoSelecionada <- renderText(input$consultaEstacoes)
      serieH <- valorSH('',input$consultaEstacoes)
    })
    
    serieHistAnualConsulta = reactive ({
      apply (serieHistConsulta ( ), 1, sum)
    })
    
    shinyjs::disable("ConsultarButton")
    shinyjs::disable("consultaEstacoes")
    shinyjs::show("estacao_resultados")
    
    output$dados = renderPlot({
      req(serieHistConsulta)
      plotSerie(serieHistConsulta())
    })
    
    infoEstacao <- infoEstacao(input$consultaEstacoes)
    
    output$dadosEstacaoTable = DT::renderDataTable(datatable(infoEstacao(input$consultaEstacoes), options = list(dom = 't')))
    output$volumeUtilHist = renderPrint ({
      print ("Volume util")
      print (paste (volumeUtil (serieHistConsulta ( ), (input$porcentagemRegularizacaoHist/100), TRUE), "m^3"))
    })
    
    output$hurstHist = renderPrint ({
      print ("Coeficiente de Hurst")
      print (isolate (Hurst (as.vector (serieHistConsulta ( )))))
    })
    
    output$tabelaAnualHist = renderDataTable ({
      facAnual = data.frame (as.vector (autocorrelacaoAnual (serieHistAnualConsulta ( ), 12)[-1]))
      rownames (facAnual) = paste ("lag", 1:12)
      datatable (facAnual, colnames = NULL) 
    })
    
    output$tabelaMensalHist = renderDataTable ({
      facMensal = data.frame (autocorrelacaoMensal (serieHistConsulta ( ), 12)[-1, ])
      colnames (facMensal) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      rownames (facMensal) = paste ("lag", 1:12)
      datatable (round(facMensal,digits = 5))
    })
    
    output$tabelaAvaliacaoHist = renderDataTable({
      MediaHist = apply (serieHistConsulta ( ), 2, mean)
      DesvioHist = apply (serieHistConsulta ( ), 2, sd)
      KurtHist = apply(serieHistConsulta(),2,kurtosis)
      AssimetriaHist = apply(serieHistConsulta(),2,skewness)
      CoefVarHist = DesvioHist/MediaHist
      medidas = data.frame (MediaHist, DesvioHist, KurtHist,AssimetriaHist,CoefVarHist)
      rownames (medidas) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      colnames (medidas) = c ("Media Historica", "Desvio-padrao Historico", "Indice Kurt","Assimetria","Coeficiente de Variacao")
      datatable (medidas)
    })
    
    #DELETAR SERIES DO BANCO DE DADOS
    observeEvent(input$DeletarButton,{ 
      deleteEstacao(input$consultaEstacoes)
      shinyalert("Deletado!","A Estacao foi deletada com sucesso", type = "success")
      estacao = loadData("ESTACAO")
      updateSelectInput(session, "consultaEstacoes",
                        choices = estacao$nome,
                        selected = NULL)
      
        shinyjs::hide("estacao_resultados")
        shinyjs::enable("ConsultarButton")
        shinyjs::enable("consultaEstacoes")

      
    })
    
  })
  
  observeEvent(input$LimparButton,{
    shinyjs::enable("ConsultarButton")
    shinyjs::hide("estacao_resultados")
    shinyjs::enable("consultaEstacoes")
  })
  
  ################################# TABPANEL SERIES GERADAS #################################
  
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
  
  ############################## TABPANEL: MODELO ARMA ##############################
  updateSelectInput(session, "estacoes_ARMA",
                    choices = estacao$nome,
                    selected = NULL)
  
  ########## INPUT DO MODELO PMIX
  
  serieHist_ARMA = reactive({
    serieHist_ARMA = valorSH('',input$estacoes_ARMA)
  })
  
  serieHistAnual_ARMA = reactive({
    apply (serieHist_ARMA(), 1, sum)  
  })
  
  ########## Funcao algoritmo do MODELO ARMA
  resultados_ARMA = reactive({

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculando o ARMA", value = 0)
    
    if (input$goButton_ARMA)
      isolate (cenarioSinteticoAnual(serieHist_ARMA(),c(input$p_ARMA,input$q_ARMA),input$nsint_ARMA))
  })
  
  ########## Serie gerada pelo MODELO ARMA
  serieSint_ARMA = reactive(resultados_ARMA()$serieSintetica)
  
  # Avaliacao da serie sintetica gerada pelo modelo ARMA
  avaliacaoAnualARMA <- callModule(avaliacaoAnual,"ARMA",serieHistAnual_ARMA,serieSint_ARMA)
  acfAnualARMA <- callModule(facAnual,"ARMA",serieHistAnual_ARMA,serieSint_ARMA)
  hurstAnualARMA <- callModule(coeficienteHurst,"ARMA","Anual",serieHistAnual_ARMA,serieSint_ARMA)
  volumeARMA <- callModule(volume,"ARMA",FALSE,serieHistAnual_ARMA,serieSint_ARMA)
  
  somaRes_ARMA = reactive({ 
    residuos = resultados_ARMA()$residuos
    somRes = sum(residuos^2)
  })
  
  observeEvent(input$goButton_ARMA,{
    shinyjs::enable("limparButton_ARMA")
    shinyjs::disable("goButton_ARMA")
    shinyjs::show("resultados_ARMA")
    
    output$somaRes_ARMA = renderPrint ({
      print (somaRes_ARMA())
    })
    
    output$downloadSerie_ARMA = downloadHandler (
      filename = function ( ) {
        paste("serieARMA.csv",sep="")
      },
      content = function (file) {
        write.table(data.frame (serieSint_ARMA()), file,
                    col.names = "Serie Anual",
                    row.names = F,
                    sep = ";",
                    dec = ",")
      })
  })
  
  observeEvent(input$limparButton_ARMA,{
    shinyjs::enable("goButton_ARMA")
    shinyjs::disable("limparButton_ARMA")
    shinyjs::hide("resultados_ARMA")
    shinyjs::enable("armazenarButton_ARMA")
  })
  
  observeEvent(input$armazenarButton_ARMA,{
    
    tryCatch({ 
      
      shinyjs::disable("armazenarButton_ARMA")
      shinyjs::show("armazenando_msg_ARMA")
      shinyjs::hide("error_armazenar_ARMA")
      
      p_ARMA = input$p_ARMA
      q_ARMA = input$q_ARMA
      lags_ARMA = c(p_ARMA,q_ARMA)
      nAnos_ARMA = input$nsint_ARMA
      estacao_ARMA = input$estacoes_ARMA
      
      MediaArmazenar = avaliacaoAnualARMA$media() 
      DesvioArmazenar = avaliacaoAnualARMA$desvioPadrao() 
      KurtArmazenar = avaliacaoAnualARMA$kurt() 
      AssimetriaArmazenar = avaliacaoAnualARMA$assimetria() 
      CoefVarArmazenar =  avaliacaoAnualARMA$coefVar() 
      HurstArmazenar = hurstAnualARMA()  
      VolumeArmazenar = volumeARMA()
      somResArmazenar = somaRes_ARMA()
      acfAnual = data.frame (as.vector (acfAnualARMA()[-1]))  
      
      idEstacao_ARMA <- findID(estacao,input$estacoes_ARMA)
      idSERIE_SINTETICA <- registrarSSARMA(p_ARMA,q_ARMA,nAnos_ARMA,idEstacao_ARMA)
      inserirSS_ARMA(idSERIE_SINTETICA, serieSint_ARMA())
      inserirAvaliacaoSS_ARMA(idSERIE_SINTETICA,MediaArmazenar,DesvioArmazenar,AssimetriaArmazenar,KurtArmazenar,CoefVarArmazenar)
      inserirACF_ANUALSS(idSERIE_SINTETICA,acfAnual)
      inserirSomHurst_ARMA(idSERIE_SINTETICA,somResArmazenar,HurstArmazenar)
      
      if(!is.infinite(VolumeArmazenar) && is.numeric(VolumeArmazenar)){
         inserirVol_ARMA(idSERIE_SINTETICA,VolumeArmazenar)
      }
      
      shinyalert("Armazenado!","A serie foi armazenada com sucesso", type = "success")
    },
    error = function(err) {
      shinyjs::hide("armazenando_msg_ARMA")
      shinyjs::html("error_msg_armazenar_ARMA", err$message)
      shinyjs::show(id = "error_armazenar_ARMA", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::hide("armazenando_msg_ARMA")
      SSTable <- SeriesSinteticas()
      output$SeriesSinteticas<- DT::renderDataTable(SSTable,server = TRUE, selection = 'single')
      
    })
    
  })
  
  
  
  ############################## TAB : DESAGREGACAO ##############################
  SeriesDesagregacao = SeriesSinteticas()
  output$SeriesDesagregacao <- DT::renderDataTable(SeriesDesagregacao,server = TRUE, selection = 'single')
  
  ########## INPUT DESAGREGACAO
  serieHistDNP <- reactive({
    input$SeriesDesagregacao_button
    if(input$analise_DNP == 1){ 
      selectedrowindex <<- input$SeriesDesagregacao_rows_selected[length(input$SeriesDesagregacao_rows_selected)]
      selectedrowindex <<- as.numeric(selectedrowindex)
      
      estacao = (SeriesDesagregacao[selectedrowindex,Estacao])
      codigo = (SeriesDesagregacao[selectedrowindex,Codigo])
      
      serieH = buscarSH(codigo,estacao)
      serieHist = div_mensais(serieH)
      
    }else if(input$analise_DNP == 2){
      serieH = data.frame(read.csv2(input$serieHArquivada$datapath))
      colnames(serieH)=c("periodo","valor")
      serieH = as.data.table(serieH)
      serieHist = div_mensais(serieH)
    }
  })
  
  serieHistAnualDNP <- reactive({
    serieHist_Anual = apply (serieHistDNP(), 1, sum)  
  })
  
  serieSintDNP <- reactive({
    input$SeriesDesagregacao_button
    if(input$analise_DNP == 1){ 
      selectedrowindex <<- input$SeriesDesagregacao_rows_selected[length(input$SeriesDesagregacao_rows_selected)]
      selectedrowindex <<- as.numeric(selectedrowindex)
      
      idSerie_Sintetica <- (SeriesDesagregacao[selectedrowindex,ID])
      modelo = (SeriesDesagregacao[selectedrowindex,modelo])
      serieSS = SeriesSinteica_Anuais(idSerie_Sintetica,modelo)

    }
    
  })
  
  ########## Algoritmo da DESAGREGACAO
  desagregadoNP = reactive({
    input$SeriesDesagregacao_button
    if(input$analise_DNP == 1){ 
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Calculando a desagregacao nao-parametrica", value = 0)
      
      desagregado <- desagrega_np(serieSintDNP(),serieHistDNP())
    }else if(input$analise_DNP == 2){
      colunas = c("x","JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")
      desagregado <- read.csv2(input$serieDNPArquivada$datapath,sep=";",dec=",",col.names = colunas,header=TRUE)
      desagregado = desagregado[,-c(1)]
    }
  })
  
  desagregadoNP_Anual = reactive({
    apply (desagregadoNP(), 1, sum)
  })
  
  # Avalicao da serie desagregada pela desagregacao nao-parametrica
  avaliacaoMensalDNP <- callModule(avaliacaoMensal,"DNP",serieHistDNP,desagregadoNP)
  acfAnualDNP <- callModule(facAnual,"DNP",serieHistAnualDNP,desagregadoNP_Anual)
  acfMensalDNP <- callModule(facMensal,"DNP",serieHistDNP,desagregadoNP)
  hurstMensalDNP <- callModule(coeficienteHurst,"DNP-Mensal","Mensal",reactive(as.matrix(serieHistDNP())),reactive(as.matrix(desagregadoNP())))
  hurstAnualDNP <- callModule(coeficienteHurst,"DNP-Anual","Anual",serieHistAnualDNP,desagregadoNP_Anual)
  volumeDNP <- callModule(volume,"DNP","TRUE",reactive(as.matrix(serieHistDNP())),reactive(as.matrix(desagregadoNP())))
  
  observeEvent(input$SeriesDesagregacao_button,{
  
    desagregadoNaoP = desagregadoNP()
    
    shinyjs::disable("SeriesDesagregacao_button")
    shinyjs::enable("limparButton_DNP")
    shinyjs::show("resultados_DNP")
    
    output$downloadSerie_DNP = downloadHandler (
      filename = function ( ) {
        paste("serieDNP.csv",sep="")
      },
      content = function (file) {
        colunas = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
        write.table(data.frame (desagregadoNaoP), file,
                    col.names = colunas,
                    row.names = F,
                    sep = ";",
                    dec = ",")
      })
    
  })
  
  ########## Armazenando a serie desagregada no banco de dados
  observeEvent(input$armazenarBD_DNP,{
   
    shinyjs::disable("armazenarBD_DNP")
    shinyjs::show("armazenando_msg_DNP")
    shinyjs::hide("error_armazenar_DNP")
    
    tryCatch({
      
      MediaArmazenar = avaliacaoMensalDNP$media()  
      DesvioArmazenar = avaliacaoMensalDNP$desvioPadrao() 
      KurtArmazenar = avaliacaoMensalDNP$kurt() 
      AssimetriaArmazenar = avaliacaoMensalDNP$assimetria() 
      CoefVarArmazenar = avaliacaoMensalDNP$coefVar()  
      acfMensal = data.frame (acfMensalDNP()[-1, ])
      acfAnual = data.frame (as.vector (acfAnualDNP()[-1]))
      
      selectedrowindex <<- input$SeriesDesagregacao_rows_selected[length(input$SeriesDesagregacao_rows_selected)]
      selectedrowindex <- as.numeric(selectedrowindex)
      idSerie_Sintetica <- (SeriesDesagregacao[selectedrowindex,ID])
    
      idDesagregado = registrarSSDESAGREGACAO(idSerie_Sintetica,"N")
      inserirSS_Desagregado(idDesagregado,desagregadoNP())
      inserirAvaliacaoDESAGREGACAO(idDesagregado,MediaArmazenar,DesvioArmazenar,AssimetriaArmazenar,KurtArmazenar,CoefVarArmazenar)
      inserirACF_MensalDESAGREGACAO(idDesagregado,acfMensal)
      inserirACF_ANUALDESAGREGACAO(idDesagregado,acfAnual)
      inserirHurstVolDESAGREGACAO(idDesagregado,hurstAnualDNP(),hurstMensalDNP(),volumeDNP())
    },
    error = function(err) {
      shinyjs::hide("armazenando_msg_DNP")
      shinyjs::html("error_msg_armazenar_DNP", err$message)
      shinyjs::show(id = "error_armazenar_DNP", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyalert("Armazenado!","A serie foi armazenada com sucesso", type = "success")
      shinyjs::hide("armazenando_msg_DNP")
      SSTable <- SeriesSinteticas()
      output$SeriesSinteticas<- DT::renderDataTable(SSTable,server = TRUE, selection = 'single')
      
    })
  
  })
  
  observeEvent(input$limparButton_DNP,{
    shinyjs::enable("SeriesDesagregacao_button")
    shinyjs::enable("armazenarBD_DNP")
    shinyjs::reset("resultados_DNP")
    shinyjs::hide("resultados_DNP")
    shinyjs::enable("armazenarBD_DNP")
  })
  
  
}