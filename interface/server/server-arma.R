## SERVER: TABPANEL MODELO ARMA
updateSelectInput(session, "estacoes_ARMA",
                  choices = estacao$nome,
                  selected = NULL)

# Input do modelo ARMA
serieHist_ARMA = reactive({
  serieHist_ARMA = valorSH('',input$estacoes_ARMA)
})

serieHistAnual_ARMA = reactive({
  apply (serieHist_ARMA(), 1, sum)  
})

# Funcao algoritmo do modelo ARMA
resultados_ARMA = reactive({
  
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Calculando o ARMA", value = 0)
  
  if (input$goButton_ARMA)
    isolate (cenarioSinteticoAnual(serieHist_ARMA(),c(input$p_ARMA,input$q_ARMA),input$nsint_ARMA))
})

# Serie gerada pelo MODELO ARMA
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
    output$SeriesDesagregacao <- DT::renderDataTable(SSTable,server = TRUE, selection = 'single')
    
  })
  
})