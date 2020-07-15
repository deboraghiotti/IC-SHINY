## SERVER: TABPANEL DESAGREGACAO

SeriesDesagregacao <- SeriesSinteticas()
output$SeriesDesagregacao <- DT::renderDataTable(SeriesDesagregacao,server = TRUE, selection = 'single')

# Input da desagregacao
serieHistDesagregacao <- reactive({
  input$SeriesDesagregacao_button
  if(input$analiseDesagregacao == 1){ 
    SeriesDesagregacao <- SeriesSinteticas()
    selectedrowindex <- input$SeriesDesagregacao_rows_selected[length(input$SeriesDesagregacao_rows_selected)]
    selectedrowindex <- as.numeric(selectedrowindex)
    
    estacao = (SeriesDesagregacao[selectedrowindex,Estacao])
    codigo = (SeriesDesagregacao[selectedrowindex,Codigo])
    
    serieH = buscarSH(codigo,estacao)
    serieHist = div_mensais(serieH)
    
  }else if(input$analiseDesagregacao == 2){
    serieH = data.frame(read.csv2(input$serieHistDesagregacao$datapath,header = TRUE))
    colnames(serieH)=c("periodo","valor")
    serieH = as.data.table(serieH)
    serieHist = div_mensais(serieH)
  }
})

serieHistAnualDesagregacao <- reactive({
  serieHist_Anual = apply (serieHistDesagregacao(), 1, sum)  
})

serieSintDesagregacao <- reactive({
  input$SeriesDesagregacao_button
  if(input$analiseDesagregacao == 1){
    SeriesDesagregacao <- SeriesSinteticas()
    selectedrowindex <- input$SeriesDesagregacao_rows_selected[length(input$SeriesDesagregacao_rows_selected)]
    selectedrowindex <- as.numeric(selectedrowindex)
    
    idSerie_Sintetica <- (SeriesDesagregacao[selectedrowindex,ID])
    modelo = (SeriesDesagregacao[selectedrowindex,Modelo])
    serieSS = SeriesSinteica_Anuais(idSerie_Sintetica,modelo)
    return(serieSS)
  }else if(input$analiseDesagregacao == 2){
    serieSS = data.frame(read.csv(input$serieSintDesagregacao$datapath,sep=";",dec=",",header=TRUE))
    if(input$tipoSerieDesagregacao == 2){
      serieSS = data.frame(serieSS = apply (serieSS, 1, sum))
      colnames(serieSS) = "anual"
      return(serieSS)
    }
    colnames(serieSS) = "anual"
    return(serieSS)
  }
  
})

# Algoritmo da desagregacao
serieDesagregada = reactive({
  input$SeriesDesagregacao_button
  
    if(input$tipoDesagregacao == 1){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Calculando a desagregacao parametrica", value = 0)
      desagregado <- desagregacao_parametrica(serieSintDesagregacao(),serieHistDesagregacao())
    }else if(input$tipoDesagregacao == 2){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Calculando a desagregacao nao-parametrica", value = 0)
      desagregado <- desagrega_np(serieSintDesagregacao(),serieHistDesagregacao())
    }

})

serieAnualDesagregada = reactive({
  apply (serieDesagregada(), 1, sum)
})

# Avalicao da serie desagregada pela desagregacao nao-parametrica
avaliacaoMensalDesagregacao <- callModule(avaliacaoMensal,"Desagregacao",serieHistDesagregacao,serieDesagregada)
acfAnualDesagregacao <- callModule(facAnual,"Desagregacao",serieHistAnualDesagregacao,serieAnualDesagregada)
acfMensalDesagregacao <- callModule(facMensal,"Desagregacao",serieHistDesagregacao,serieDesagregada)
hurstMensalDesagregacao <- callModule(coeficienteHurst,"Desagregacao-Mensal","Mensal",
                                      reactive(as.matrix(serieHistDesagregacao())),reactive(as.matrix(serieDesagregada())))
hurstAnualDesagregacao <- callModule(coeficienteHurst,"Desagregacao-Anual","Anual",serieHistAnualDesagregacao,serieAnualDesagregada)
volumeDesagregacao <- callModule(volume,"Desagregacao","TRUE",reactive(as.matrix(serieHistDesagregacao())),reactive(as.matrix(serieDesagregada())))

observeEvent(input$SeriesDesagregacao_button,{
  
  desagregadoNaoP = serieDesagregada()
  
  shinyjs::disable("SeriesDesagregacao_button")
  shinyjs::enable("limparDesagregacaoButton")
  shinyjs::show("resultadosDesagregacao")
  
  output$downloadSerieDesagregacao = downloadHandler (
    filename = function ( ) {
      paste("serieDesagregacao.csv",sep="")
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

# Armazenando a serie desagregada no banco de dados
observeEvent(input$armazenarBD_Desagregacao,{
  
  shinyjs::disable("armazenarBD_Desagregacao")
  shinyjs::show("armazenando_msg_Desagregacao")
  shinyjs::hide("error_armazenar_Desagregacao")
  
  tryCatch({
    
    MediaArmazenar = avaliacaoMensalDesagregacao$media()  
    DesvioArmazenar = avaliacaoMensalDesagregacao$desvioPadrao() 
    KurtArmazenar = avaliacaoMensalDesagregacao$kurt() 
    AssimetriaArmazenar = avaliacaoMensalDesagregacao$assimetria() 
    CoefVarArmazenar = avaliacaoMensalDesagregacao$coefVar()  
    acfMensal = data.frame (acfMensalDesagregacao()[-1, ])
    acfAnual = data.frame (as.vector (acfAnualDesagregacao()[-1]))
    
    selectedrowindex <<- input$SeriesDesagregacao_rows_selected[length(input$SeriesDesagregacao_rows_selected)]
    selectedrowindex <- as.numeric(selectedrowindex)
    idSerie_Sintetica <- (SeriesDesagregacao[selectedrowindex,ID])
    tipo_desagregacao = "N"
    if(input$tipoDesagregacao == 1){
      tipo_desagregacao = "S"
    }
    idDesagregado = registrarSSDESAGREGACAO(idSerie_Sintetica,tipo_desagregacao)
    inserirSS_Desagregado(idDesagregado,serieDesagregada())
    inserirAvaliacaoDESAGREGACAO(idDesagregado,MediaArmazenar,DesvioArmazenar,AssimetriaArmazenar,KurtArmazenar,CoefVarArmazenar)
    inserirACF_MensalDESAGREGACAO(idDesagregado,acfMensal)
    inserirACF_ANUALDESAGREGACAO(idDesagregado,acfAnual)
    inserirHurstVolDESAGREGACAO(idDesagregado,hurstAnualDesagregacao(),hurstMensalDesagregacao(),volumeDesagregacao())
  },
  error = function(err) {
    shinyjs::hide("armazenando_msg_Desagregacao")
    shinyjs::html("error_msg_armazenar_Desagregacao", err$message)
    shinyjs::show(id = "error_armazenar_Desagregacao", anim = TRUE, animType = "fade")
  },
  finally = {
    shinyalert("Armazenado!","A serie foi armazenada com sucesso", type = "success")
    shinyjs::hide("armazenando_msg_Desagregacao")
    SDTable <- SeriesDesagregadas()
    output$SeriesDesagregadas <- DT::renderDataTable(SDTable,server = TRUE, selection = 'single')
    
  })
  
})

observeEvent(input$limparDesagregacaoButton,{
  shinyjs::enable("SeriesDesagregacao_button")
  shinyjs::enable("armazenarBD_Desagregacao")
  shinyjs::reset("resultadosDesagregacao")
  shinyjs::hide("resultadosDesagregacao")
  shinyjs::enable("armazenarBD_Desagregacao")
})