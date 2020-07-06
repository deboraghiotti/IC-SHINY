## SERVER: TABPANEL DESAGREGACAO

SeriesDesagregacao = SeriesSinteticas()
output$SeriesDesagregacao <- DT::renderDataTable(SeriesDesagregacao,server = TRUE, selection = 'single')

# Input da desagregacao
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
    modelo = (SeriesDesagregacao[selectedrowindex,Modelo])
    serieSS = SeriesSinteica_Anuais(idSerie_Sintetica,modelo)
    
  }
  
})

# Algoritmo da desagregacao
desagregadoNP = reactive({
  input$SeriesDesagregacao_button
  if(input$analise_DNP == 1){ 
    
    if(input$tipoDesagregacao == 1){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Calculando a desagregacao parametrica", value = 0)
      desagregado <- desagregacao_parametrica(serieSintDNP(),serieHistDNP())
    }else if(input$tipoDesagregacao == 2){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Calculando a desagregacao nao-parametrica", value = 0)
      desagregado <- desagrega_np(serieSintDNP(),serieHistDNP())
    }
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

# Armazenando a serie desagregada no banco de dados
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
    tipo_desagregacao = "N"
    if(input$tipoDesagregacao == 1){
      tipo_desagregacao = "S"
    }
    idDesagregado = registrarSSDESAGREGACAO(idSerie_Sintetica,tipo_desagregacao)
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
    SDTable <- SeriesDesagregadas()
    output$SeriesDesagregadas <- DT::renderDataTable(SDTable,server = TRUE, selection = 'single')
    
  })
  
})

observeEvent(input$limparButton_DNP,{
  shinyjs::enable("SeriesDesagregacao_button")
  shinyjs::enable("armazenarBD_DNP")
  shinyjs::reset("resultados_DNP")
  shinyjs::hide("resultados_DNP")
  shinyjs::enable("armazenarBD_DNP")
})