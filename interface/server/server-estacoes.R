## SERVER: TABPANEL DADOS HISTORICOS 

# Cadastro de uma estacao 
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

# Consultar uma estacao 
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
  shinyjs::enable("DeletarButton")
  
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
    colnames (medidas) = c ("Media", "Desvio-padrao", "Curtose","Assimetria","Coeficiente de Variacao")
    datatable (medidas)
  })
  
})

# Deletar a estacao e as series da estacao
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
  shinyjs::disable("DeletarButton")
  
})

observeEvent(input$LimparButton,{
  shinyjs::enable("ConsultarButton")
  shinyjs::hide("estacao_resultados")
  shinyjs::enable("consultaEstacoes")
  shinyjs::disable("DeletarButton")
})