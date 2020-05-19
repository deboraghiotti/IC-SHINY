# Funções relacionadas ao Banco de Dados "modelagem_estocastica"
# Fonte: https://shiny.rstudio.com/articles/persistent-data-storage.html#mysql

library(RMySQL)
library('DT')
library('data.table')

options(mysql = list(
  "host" = "localhost",
  "user" = "root",
  "password" = "labhidro"
))

databaseName <- "modelagem_estocastica"

# Essa função realiza a conexao com o banco de dados "modelagem_estocastica"
# Output: objeto db
conectarDataBase <- function(){
  
  db <- dbConnect(MySQL(),
                  user = 'root',
                  password = 'labhidro',
                  host = 'localhost',
                  dbname = 'modelagem_estocastica')
  
  return(db)
    
}

# Função "saveData" insere dados dentro do banco de dados
saveData <- function(table,data){
  
  #Conectando com o banco de dados
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  user = options()$mysql$user, 
                  password = options()$mysql$password)
  
  #Construindo a query
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  
  # Submetendo a query e desconectando do banco de dados
  dbGetQuery(db, query)
  dbDisconnect(db)
}


# Função loadData seleciona dados do banco de Dados
loadData <- function(table) {
  # Conectando com o banco de dados
  db <- dbConnect(MySQL(),
                  user = 'root',
                  password = 'labhidro',
                  host = 'localhost',
                  dbname = 'modelagem_estocastica')
  
  # Construindo a query de seleção
  query <- sprintf("SELECT * FROM %s", table)
  
  # Submetendo a query e desconectando do bando de dados
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  return(data)
}

#Funcao alterDataBase realiza a query dada no banco de dadoa
alterDataBase <- function(query){
  # Conectando com o banco de dados
  db <- dbConnect(MySQL(),
                   user = 'root',
                   password = 'labhidro',
                   host = 'localhost',
                   dbname = 'modelagem_estocastica'
                   
  )
  
  # Submetendo a query e desconectando do bando de dados
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  return(data)
}

cadastroEstacao <- function(nomeEstacao,codigoEstacao,rioEstacao,anosEstacao,areaEstacao,latEstacao,lngEstacao){
  
  nome <- paste("'",nomeEstacao,"'",sep="")
  
  if(is.null(rioEstacao) || (rioEstacao == "")){
    rio <- "NULL"
  }else{
    rio <- paste("'",rioEstacao,"'",sep="")
  }
  
  if(is.null(anosEstacao) || (anosEstacao == "")){
    ano <- "NULL"
  }else{
    ano <- anosEstacao
  }
  
  if(is.null(areaEstacao) || (areaEstacao == "")){
    area <- "NULL"
  }else{
    area <- areaEstacao
  }
  
  if(is.null(latEstacao) || (latEstacao == "")){
    lat <- "NULL"
  }else{
    lat <- paste("'",latEstacao,"'",sep="")
  }
  
  if(is.null(lngEstacao) || (lngEstacao == "")){
    lng <- "NULL"
  }else{
    lng <- paste("'",lngEstacao,"'",sep="")
  }
  
  query <- paste("INSERT INTO ESTACAO VALUES (","NULL",",",codigoEstacao,",",nome,",",rio,",",ano,",",area,",",lat,",",lng,")",sep="")
  alterDataBase(query)
  
}

buscarEstacao <- function(nome,codigo){
  if(!is.null(codigo) && (codigo != "") && (codigo != ' ')){
    query <- paste("CALL ESTACAO_INFO(",codigo,")")
    dadosList<-alterDataBase(query)
    dadosDT <- setDT(dadosList)
    return(dadosDT)
  }else if(!is.null(nome) && (nome != "") && (nome != " ")){
    nome <- paste("'",nome,"'",sep="")
    query <- paste("CALL ESTACAO_INFO_NOME(",nome,")",sep="")
    dadosList <- alterDataBase(query)
    dadosDT <- setDT(dadosList)
    return(dadosDT)
  }

}

infoEstacao <- function(nome){
  nome <- paste("'",nome,"'",sep="")
  query <- paste("CALL ESTACAO_INFO_NOME(",nome,")",sep="")
  dadosList <- alterDataBase(query)
  dadosDT <- setDT(dadosList)
  return(dadosDT)
}

#A fun??o tem uma prerequisito que o valor esteja na sua forma correta,isto ?, como int ou double um varchar
buscarId <- function(id,table,atributo,valor){
  if(!is.null(valor) && (valor != "") 
     && !is.null(id) && (id != "") 
     && !is.null(table) && (table != "")
     && !is.null(atributo) && (atributo != "")){
    
    query <- paste("SELECT",id,"FROM",table,"WHERE",atributo,"=",valor)
    idEstacao<-alterDataBase(query)
    return(idEstacao)
    
  }
  
  return(NULL)
}

#CADASTRNADO A SERIE HISTORIA DE UMA ESTACAO
cadastrarSH <- function(fileSH,codigoEstacao){
  if(!is.null(fileSH) && (fileSH != "") 
     && !is.null(codigoEstacao) && (codigoEstacao != "")){
    
    #Lendo o arquivo com a Serie Historica
    serie_historica = read.table(fileSH,head=T,sep = ";" , dec = ",")
    db <- dbConnect(MySQL(),
                    user = 'root',
                    password = 'labhidro',
                    host = 'localhost',
                    dbname = 'modelagem_estocastica')
    
    #BUscando o id da estacao que se quer inserir a serie historica
    idEstacao <- buscarId("idESTACAO","ESTACAO","CODIGO",codigoEstacao)
    idEstacao <- idEstacao[1,1]
    
    query <- paste("SET AUTOCOMMIT = 0")
    dbGetQuery(db,query)
    query <- paste("START TRANSACTION")
    dbGetQuery(db,query)
    
    for (i in 1:nrow(serie_historica)) {
      valor <- serie_historica[i,2]
      periodo <- serie_historica[i,1]
      query <- paste("INSERT INTO VAZAO VALUES (","NULL",",",valor,",","'",periodo,"'",",",idEstacao,")",sep="")
      dbGetQuery(db,query)
    }
    
    query <- paste("COMMIT")
    dbGetQuery(db,query)
    
  }
}

buscarSH <- function(codigoEstacao, nomeEstacao){
  if(!is.null(codigoEstacao) && (codigoEstacao != "")){
    query <- paste("CALL ESTACAO_SERIE_HISTORICA(",codigoEstacao,")")
    dadosList<-alterDataBase(query)
    dadosDT <- setDT(dadosList)
    return(dadosDT)
  }else if(!is.null(nomeEstacao) && (nomeEstacao != "")){
    nomeEstacao <- paste("'",nomeEstacao,"'",sep="")
    query <- paste("CALL ESTACAO_SERIE_HISTORICA_NOME(",nomeEstacao,")",sep="")
    dadosList <- alterDataBase(query)
    dadosDT <- setDT(dadosList)
    return(dadosDT)
  }
}

  ##  FUNCOES RELACIONADAS A SERIE HISTORICA

  buscarACF_MENSAL <- function(codigoEstacao, nomeEstacao){
    if(!is.null(codigoEstacao) && (codigoEstacao != "") && (codigoEstacao != " ")){
      query <- paste("CALL ESTACAO_ACF_MENSAL(",codigoEstacao,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }else if(!is.null(nomeEstacao) && (nomeEstacao != "") && (nomeEstacao != " ")){
      nomeEstacao <- paste("'",nomeEstacao,"'",sep="")
      query <- paste("CALL ESTACAO_ACF_MENSAL_NOME(",nomeEstacao,")",sep="")
      dadosList <- alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarACF_ANUAL <- function(codigoEstacao, nomeEstacao){
    if(!is.null(codigoEstacao) && (codigoEstacao != "")){
      query <- paste("CALL ESTACAO_ACF_ANUAL(",codigoEstacao,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }else{
      nomeEstacao <- paste("'",nomeEstacao,"'",sep="")
      query <- paste("CALL ESTACAO_ACF_ANUAL_NOME(",nomeEstacao,")",sep="")
      dadosList <- alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarHURST <- function(codigoEstacao, nomeEstacao){
    if(!is.null(codigoEstacao) && (codigoEstacao != "")){
      query <- paste("CALL ESTACAO_HURST(",codigoEstacao,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }else{
      nomeEstacao <- paste("'",nomeEstacao,"'",sep="")
      query <- paste("CALL ESTACAO_HURST_NOME(",nomeEstacao,")",sep="")
      dadosList <- alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarAVALIACAO <- function(codigoEstacao, nomeEstacao){
    if(!is.null(codigoEstacao) && (codigoEstacao != "")){
      query <- paste("CALL ESTACAO_AVALIACAO(",codigoEstacao,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }else{
      nomeEstacao <- paste("'",nomeEstacao,"'",sep="")
      query <- paste("CALL ESTACAO_AVALIACAO_NOME(",nomeEstacao,")",sep="")
      dadosList <- alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarVOLUME <- function(codigoEstacao, nomeEstacao){
    if(!is.null(codigoEstacao) && (codigoEstacao != "")){
      query <- paste("CALL ESTACAO_VOLUME(",codigoEstacao,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }else{
      nomeEstacao <- paste("'",nomeEstacao,"'",sep="")
      query <- paste("CALL ESTACAO_VOLUME_NOME(",nomeEstacao,")",sep="")
      dadosList <- alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarSOMA_RESIDUAL <- function(codigoEstacao, nomeEstacao){
    if(!is.null(codigoEstacao) && (codigoEstacao != "")){
      query <- paste("CALL ESTACAO_SOMA_RESIDUAL(",codigoEstacao,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }else{
      nomeEstacao <- paste("'",nomeEstacao,"'",sep="")
      query <- paste("CALL ESTACAO_SOMA_RESIDUAL_NOME(",nomeEstacao,")",sep="")
      dadosList <- alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }

  buscarSS <- function(codigoEstacao, nomeEstacao){
    if(!is.null(codigoEstacao) && (codigoEstacao != "")){
      query <- paste("CALL ESTACAO_SERIES_SINTETICAS(",codigoEstacao,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }else{
      nomeEstacao <- paste("'",nomeEstacao,"'",sep="")
      query <- paste("CALL ESTACAO_SERIES_SINTETICAS_NOME(",nomeEstacao,")",sep="")
      dadosList <- alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  
  
  
  
  ##  FUNCOES RELACIONADAS A SERIE SINTETICA 
  
  buscarDESAGREGADO_SS <- function(idSerieSintetica){
    if(!is.null(idSerieSintetica) && (idSerieSintetica != "") && (idSerieSintetica != " ")){
      query <- paste("CALL SS_DESAGREGADO(",idSerieSintetica,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarACF_MENSAL_SS <- function(idSerieSintetica){
    if(!is.null(idSerieSintetica) && (idSerieSintetica != "") && (idSerieSintetica != " ")){
      query <- paste("CALL SS_ACF_MENSAL(",idSerieSintetica,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarACF_ANUAL_SS <- function(idSerieSintetica){
    if(!is.null(idSerieSintetica) && (idSerieSintetica != "") && (idSerieSintetica != " ")){
      query <- paste("CALL SS_ACF_ANUAL(",idSerieSintetica,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarHURST_SS <- function(idSerieSintetica){
    if(!is.null(idSerieSintetica) && (idSerieSintetica != "") && (idSerieSintetica != " ")){
      query <- paste("CALL SS_HURST(",idSerieSintetica,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarAVALIACAO_SS <- function(idSerieSintetica){
    if(!is.null(idSerieSintetica) && (idSerieSintetica != "") && (idSerieSintetica != " ")){
      query <- paste("CALL SS_AVALIACAO(",idSerieSintetica,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarVOLUME_SS <- function(idSerieSintetica){
    if(!is.null(idSerieSintetica) && (idSerieSintetica != "") && (idSerieSintetica != " ")){
      query <- paste("CALL SS_VOLUME(",idSerieSintetica,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarSOMARESIDUAL_SS <- function(idSerieSintetica){
    if(!is.null(idSerieSintetica) && (idSerieSintetica != "") && (idSerieSintetica != " ")){
      query <- paste("CALL SS_SOMA_RESIDUAL(",idSerieSintetica,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  ##  FUNCOES RELACIONADAS A SERIE DESAGREGADA
  buscarACF_MENSAL_SD <- function(idDESAGREGADO){
    if(!is.null(idDESAGREGADO) && (idDESAGREGADO != "") && (idDESAGREGADO != " ")){
      query <- paste("CALL SD_ACF_MENSAL(",idDESAGREGADO,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarACF_ANUAL_SD <- function(idDESAGREGADO){
    if(!is.null(idDESAGREGADO) && (idDESAGREGADO != "") && (idDESAGREGADO != " ")){
      query <- paste("CALL SD_ACF_ANUAL(",idDESAGREGADO,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarHURST_SD <- function(idDESAGREGADO){
    if(!is.null(idDESAGREGADO) && (idDESAGREGADO != "") && (idDESAGREGADO != " ")){
      query <- paste("CALL SD_HURST(",idDESAGREGADO,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarAVALIACAO_SD <- function(idDESAGREGADO){
    if(!is.null(idDESAGREGADO) && (idDESAGREGADO != "") && (idDESAGREGADO != " ")){
      query <- paste("CALL SD_AVALIACAO(",idDESAGREGADO,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarVOLUME_SD <- function(idDESAGREGADO){
    if(!is.null(idDESAGREGADO) && (idDESAGREGADO != "") && (idDESAGREGADO != " ")){
      query <- paste("CALL SD_VOLUME(",idDESAGREGADO,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  buscarSOMARESIDUAL_SD <- function(idDESAGREGADO){
    if(!is.null(idDESAGREGADO) && (idDESAGREGADO != "") && (idDESAGREGADO != " ")){
      query <- paste("CALL SD_SOMA_RESIDUAL(",idDESAGREGADO,")")
      dadosList<-alterDataBase(query)
      dadosDT <- setDT(dadosList)
      return(dadosDT)
    }
  }
  
  SeriesSinteticas <- function(){
    query <- paste("SELECT idSERIE_SINTETICA as 'ID',codigo as 'Codigo',nome as 'Estacao',modelo,lags,desagregado,metodo,SERIE_SINTETICA.anos as 'Anos',register_date as 'Data' FROM SERIE_SINTETICA, ESTACAO
WHERE ID_ESTACAO = IDESTACAO;")
    dadosList<-alterDataBase(query)
    dadosDT <- setDT(dadosList)
    return(dadosDT)
  }
  
  SeriesDesagregadas <- function(){
    query <- paste("SELECT idDESAGREGADO as 'ID',id_SERIE_SINTETICA as 'ID Serie Sintetica',parametrico,register_date as 'Data' FROM DESAGREGADO;")
    dadosList<-alterDataBase(query)
    dadosDT <- setDT(dadosList)
    return(dadosDT)
  }
  
  Estacoes <- function(){
    query <- paste("SELECT * FROM ESTACAO;")
    dadosEstacao <- alterDataBase(query)
    dadosDT <- setDT(dadosEstacao)
    return(dadosDT)
  }
  
  # A funcao registrarSSPMIX armazena uma nova serie sintetica no banco de dados e retorna
  # o idSERIE_SInNTETICA dessa nova serie na table serie_sintetica do banco de dados
  
  registrarSSPMIX <- function(input,idEstacao){
  
    db <- conectarDataBase()
    
    modelo <- "PMIX"
    anos <- (input$nsint)
    lags <- (paste("(",input$p,",",input$q,",",input$P,",",input$Q,")",sep=""))
    desagregado <- "N"
    
    if(input$tipo == 1){
      metodo <- "Powell"
    }else if(input$tipo == 2){
      metodo <- "Algoritmo Genetico"
    }
    
    query <- paste("INSERT INTO SERIE_SINTETICA VALUES(NULL,'",modelo,"',",anos,",'",lags,"','",metodo,"','",desagregado,"',",idEstacao,",CURRENT_TIMESTAMP())",sep="")
    data <- dbGetQuery(db, query)
    
    query <- "SELECT LAST_INSERT_ID();"
    idSERIE_SINTETICA <- dbGetQuery(db, query)
  
    dbDisconnect(db)
    
    return(idSERIE_SINTETICA)
    
  }
  
  #Funcao para inserir uma serie sintetica no banco de dados
  #Essa funcao recebe como parametro o id da serie sintetica
  inserirSS <- function(id_SERIE_SINTETICA,serie_sintetica){
    
    db <- conectarDataBase()
    inicio <- Sys.time()
    
    query <- paste("SET AUTOCOMMIT = 0")
    dbGetQuery(db,query)
    query <- paste("START TRANSACTION")
    dbGetQuery(db,query)
    
    for (i in 1:nrow(serie_sintetica)) {
      jan <- serie_sintetica[i,1]
      fev <- serie_sintetica[i,2]
      mar <- serie_sintetica[i,3]
      abr <- serie_sintetica[i,4]
      mai <- serie_sintetica[i,5]
      jun <- serie_sintetica[i,6]
      jul <- serie_sintetica[i,7]
      ago <- serie_sintetica[i,8]
      seb <- serie_sintetica[i,9]
      oub <- serie_sintetica[i,10]
      nov <- serie_sintetica[i,11]
      dez <- serie_sintetica[i,12]
      anual <- jan + fev + mar + abr+ mai + jun + jul + ago + seb + oub + nov + dez
      query <- paste("INSERT INTO SERIE VALUES (NULL",",",jan,",",fev,",",mar,",",abr,",",mai,
                     ",",jun,",",jul,",",ago,",",seb,",",oub,",",nov,",",dez,",",anual,",",id_SERIE_SINTETICA,",NULL)")
      dbGetQuery(db,query)
    }
    
    query <- paste("COMMIT")
    dbGetQuery(db,query)
    
    fim <- Sys.time()
    dbDisconnect(db)
    
  }
  
  inserirAvaliacaoSS <- function(id_SERIE_SINTETICA,mediaSint,dpSint,assimeSint,kurtSint,coefSint){

    db <- conectarDataBase()
    inicio <- Sys.time()
    
    query <- paste("SET AUTOCOMMIT = 0")
    dbGetQuery(db,query)
    query <- paste("START TRANSACTION")
    dbGetQuery(db,query)
    
    meses <- c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")
    for(i in 1:12){
      mes <- meses[i]
      media <- mediaSint[i]
      dp <- dpSint[i]
      assimetria <- assimeSint[i]
      kurt <- kurtSint[i]
      coef_var <- coefSint[i]
      
      query <- paste("INSERT INTO AVALIACAO(mes,media,dp,assimetria,ind_kurt,coef_var,id_SERIE_SINTETICA) 
                     VALUES (","'",mes,"'",",",media,",",dp,",",assimetria,",",kurt,",",coef_var,",",id_SERIE_SINTETICA,")")
      dbGetQuery(db,query)
    }
    
    query <- paste("COMMIT")
    dbGetQuery(db,query)
    
    fim <- Sys.time()
    dbDisconnect(db)
    
  }
  
  inserirACF_MensalSS <- function(id_SERIE_SINTETICA,acf_mensal){
    
    db <- conectarDataBase()
    
    inicio <- Sys.time()
    
    query <- paste("SET AUTOCOMMIT = 0")
    dbGetQuery(db,query)
    query <- paste("START TRANSACTION")
    dbGetQuery(db,query)
    
    for (i in 1:nrow(acf_mensal)) {
      lag <- i
      v1 <- acf_mensal[i,1]
      v2 <- acf_mensal[i,2]
      v3 <- acf_mensal[i,3]
      v4 <- acf_mensal[i,4]
      v5 <- acf_mensal[i,5]
      v6 <- acf_mensal[i,6]
      v7 <- acf_mensal[i,7]
      v8 <- acf_mensal[i,8]
      v9 <- acf_mensal[i,9]
      v10 <- acf_mensal[i,10]
      v11 <- acf_mensal[i,11]
      v12 <- acf_mensal[i,12]
      
      query <- paste("INSERT INTO ACF_MENSAL VALUES (","NULL",",",lag,",",v1,",",v2,",",v3,",",v4,",",v5,",",v6,",",v7,",",v8,",",v9,",",v10,",",v11,",",v12,",NULL,",id_SERIE_SINTETICA,",NULL)")
      dbGetQuery(db,query)
    }
    query <- paste("COMMIT")
    dbGetQuery(db,query)
    
    fim <- Sys.time()
    
    dbDisconnect(db)
    
  }
  
  inserirACF_ANUALSS <- function(id_SERIE_SINTETICA,acf_anual){

    db <- conectarDataBase()
    
    inicio <- Sys.time()
    query <- paste("SET AUTOCOMMIT = 0")
    dbGetQuery(db,query)
    query <- paste("START TRANSACTION")
    dbGetQuery(db,query)
    
    for (i in 1:nrow(acf_anual)) {
      lag <- i
      valor <- acf_anual[i,1]
      
      query <- paste("INSERT INTO ACF_ANUAL VALUES(NULL,",valor,",",lag,",NULL,",id_SERIE_SINTETICA,",NULL)")
      dbGetQuery(db,query)
    }
    
    query <- paste("COMMIT")
    dbGetQuery(db,query)
    
    
    fim <- Sys.time()
    dbDisconnect(db)
  }
  
  inserirSomHurstVol<- function(id_SERIE_SINTETICA,somRes,hurstAnual,hurstMensal,volume){
 
    db <- conectarDataBase()
    
    inicio <- Sys.time()
    
    query <- paste("INSERT INTO SOMA_RESIDUAL VALUES(NULL,",somRes,",NULL,",id_SERIE_SINTETICA,",NULL)")
    dbGetQuery(db,query)
    
    query <- paste("INSERT INTO HURST VALUES(NULL,",hurstAnual,",",hurstMensal,",NULL,",id_SERIE_SINTETICA,",NULL)")
    dbGetQuery(db,query)
    
    query <- paste("INSERT INTO VOLUME VALUES(NULL,",volume,",NULL,",id_SERIE_SINTETICA,",NULL)")
    dbGetQuery(db,query)
    
    fim <- Sys.time()
    dbDisconnect(db)
    
  }
  
  selectSerie_Sintetica = function(idSerie_Sintetica){
    if(!is.null(idSerie_Sintetica)){
      query <- paste("SELECT JAN, FEV,MAR,ABR,MAI,JUN,JUL,AGO,SEB,OUB,NOV,DEZ FROM SERIE WHERE id_SERIE_SINTETICA = ",idSerie_Sintetica)
      serie <- alterDataBase(query)
      return(serie)
    }
  }
  
  selectSerie_Desagregada = function(id_DESAGREGADO){
    if(!is.null(id_DESAGREGADO)){
      query <- paste("SELECT JAN, FEV,MAR,ABR,MAI,JUN,JUL,AGO,SEB,OUB,NOV,DEZ FROM SERIE WHERE id_DESAGREGADO = ",id_DESAGREGADO)
      serie <- alterDataBase(query)
      return(serie)
    }
  }
  
  deleteSerieSS =  function(idSerie_Sintetica){
    if(!is.null(idSerie_Sintetica)){
      query = paste("DELETE FROM SERIE_SINTETICA WHERE idSERIE_SINTETICA = ",idSerie_Sintetica)
      alterDataBase(query)
    }
  }
  
  deleteSerieSD =  function(idDesagregado){
    if(!is.null(idDesagregado)){
      query = paste("DELETE FROM DESAGREGADO WHERE idDESAGREGADO = ",idDesagregado)
      alterDataBase(query)
    }
  }
  
  deleteEstacao =  function(nome){
    if(!is.null(nome)){
      query = paste("DELETE FROM ESTACAO WHERE nome = '",nome,"'",sep="")
      alterDataBase(query)
    }
  }
  
  ############################# TABPANEL: MODELO ARMA #############################
  # A funcao registrarSSARMA armazena uma nova serie sintetica no banco de dados e retorna
  # o idSERIE_SInNTETICA dessa nova serie na table serie_sintetica do banco de dados
  
  registrarSSARMA <- function(p,q,nsint,idEstacao){
    
    db <- conectarDataBase()
    
    modelo <- "ARMA"
    anos <- nsint
    lags <- (paste("(",p,",",q,")",sep=""))
    desagregado <- "N"
    metodo <- "Metodo dos Momentos"
    
    query <- paste("INSERT INTO SERIE_SINTETICA VALUES(NULL,'",modelo,"',",anos,",'",lags,"','",metodo,"','",desagregado,"',",idEstacao,",CURRENT_TIMESTAMP())",sep="")
    data <- dbGetQuery(db, query)
    
    query <- "SELECT LAST_INSERT_ID();"
    idSERIE_SINTETICA <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    return(idSERIE_SINTETICA)
    
  }
  
  #Funcao para inserir uma serie sintetica gerada pelo modelo ARMA no banco de dados
  #Essa funcao recebe como parametro o id da serie sintetica
  inserirSS_ARMA <- function(id_SERIE_SINTETICA,serie_sintetica){
    
    db <- conectarDataBase()
    inicio <- Sys.time()
    
    query <- paste("SET AUTOCOMMIT = 0")
    dbGetQuery(db,query)
    query <- paste("START TRANSACTION")
    dbGetQuery(db,query)
    
    for (i in 1:length(serie_sintetica)) {
      anual <- serie_sintetica[i]
      query <- paste("INSERT INTO SERIE_ANUAL VALUES (NULL",",",anual,",",id_SERIE_SINTETICA,")")
      dbGetQuery(db,query)
    }
    
    query <- paste("COMMIT")
    dbGetQuery(db,query)
    
    fim <- Sys.time()
    dbDisconnect(db)
    
  }
  
  inserirAvaliacaoSS_ARMA <- function(id_SERIE_SINTETICA,mediaSint,dpSint,assimeSint,kurtSint,coefSint){
    
    db <- conectarDataBase() 
    inicio <- Sys.time()
    
    query <- paste("SET AUTOCOMMIT = 0")
    dbGetQuery(db,query)
    query <- paste("START TRANSACTION")
    dbGetQuery(db,query)
    mes <- 'ANUAL' 
    query <- paste("INSERT INTO AVALIACAO(mes,media,dp,assimetria,ind_kurt,coef_var,id_SERIE_SINTETICA) 
                     VALUES (","'",mes,"'",",",mediaSint,",",dpSint,",",assimeSint,",",kurtSint,",",coefSint,",",id_SERIE_SINTETICA,")")
    dbGetQuery(db,query)
    
    query <- paste("COMMIT")
    dbGetQuery(db,query)
    
    fim <- Sys.time()
    dbDisconnect(db)
    
  }
  
  
  inserirSomHurst_ARMA<- function(id_SERIE_SINTETICA,somRes,hurstAnual){
    
    db <- conectarDataBase()   
    inicio <- Sys.time()
    
    query <- paste("INSERT INTO SOMA_RESIDUAL VALUES(NULL,",somRes,",NULL,",id_SERIE_SINTETICA,",NULL)")
    dbGetQuery(db,query)
    
    query <- paste("INSERT INTO HURST VALUES(NULL,",hurstAnual,",NULL,NULL,",id_SERIE_SINTETICA,",NULL)")
    dbGetQuery(db,query)
    
    fim <- Sys.time()
    dbDisconnect(db)
    
  }
  
  inserirVol_ARMA<- function(id_SERIE_SINTETICA,volume){
    
    db <- conectarDataBase()   
    inicio <- Sys.time()
  
    query <- paste("INSERT INTO VOLUME VALUES(NULL,",volume,",NULL,",id_SERIE_SINTETICA,",NULL)")
    dbGetQuery(db,query)
    
    fim <- Sys.time()
    dbDisconnect(db)
    
  }
  
  
  SeriesSinteica_Anuais <- function(idSerieSintetica,modelo){
    # Modelos : PMIX ou ARMA
    if(modelo == "PMIX" && !is.null(idSerieSintetica)){
      
      query = paste("Select anual from serie where id_SERIE_SINTETICA=",idSerieSintetica)
      serie = alterDataBase(query)
       
    }else if(modelo == "ARMA" && !is.null(idSerieSintetica)){
      query = paste("Select anual from serie_anual where id_SERIE_SINTETICA=",idSerieSintetica)
      serie = alterDataBase(query)
    }
    return(serie)
  }
  
  
  ############################# TABPANEL: DESAGREGACAO #############################
  
  # A funcao registrarSSDESAGREGACAO armazena uma nova serie sintetica no banco de dados e retorna
  # o idDESAGREGADO dessa nova serie na table serie_sintetica do banco de dados
  # Parametros de entrada: 
  # > idSerie_Sintetica = o id da serie sintetica que foi desagregada 
  # > parametrico: 'S' ou 'N' indicando se a serie ? parametrica(S) ou nao-parametrica(N)
  
  registrarSSDESAGREGACAO <- function(idSerie_Sintetica,parametrico){
    
    db <- conectarDataBase()
    query <- paste("INSERT INTO DESAGREGADO VALUES(NULL,'",parametrico,"',",idSerie_Sintetica," ,CURRENT_TIMESTAMP())",sep="")
    data <- dbGetQuery(db, query)
    
    query <- "SELECT LAST_INSERT_ID();"
    idSERIE_SINTETICA <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    return(idSERIE_SINTETICA)
    
  } 
  
  
  inserirSS_Desagregado <- function(id_Desagregado,serie_sintetica){
    
    db <- conectarDataBase()
    inicio <- Sys.time()
    
    query <- paste("SET AUTOCOMMIT = 0")
    dbGetQuery(db,query)
    query <- paste("START TRANSACTION")
    dbGetQuery(db,query)
    
    for (i in 1:nrow(serie_sintetica)) {
      jan <- serie_sintetica[i,1]
      fev <- serie_sintetica[i,2]
      mar <- serie_sintetica[i,3]
      abr <- serie_sintetica[i,4]
      mai <- serie_sintetica[i,5]
      jun <- serie_sintetica[i,6]
      jul <- serie_sintetica[i,7]
      ago <- serie_sintetica[i,8]
      seb <- serie_sintetica[i,9]
      oub <- serie_sintetica[i,10]
      nov <- serie_sintetica[i,11]
      dez <- serie_sintetica[i,12]
      anual <- jan + fev + mar + abr+ mai + jun + jul + ago + seb + oub + nov + dez
      query <- paste("INSERT INTO SERIE VALUES (NULL",",",jan,",",fev,",",mar,",",abr,",",mai,
                     ",",jun,",",jul,",",ago,",",seb,",",oub,",",nov,",",dez,",",anual,",NULL,",id_Desagregado,")")
      dbGetQuery(db,query)
    }
    
    query <- paste("COMMIT")
    dbGetQuery(db,query)
    
    fim <- Sys.time()
    dbDisconnect(db)
    
  }
  
  inserirAvaliacaoDESAGREGACAO <- function(id_DESAGREGADO,mediaSint,dpSint,assimeSint,kurtSint,coefSint){
    
    db <- conectarDataBase()
    inicio <- Sys.time()
    
    query <- paste("SET AUTOCOMMIT = 0")
    dbGetQuery(db,query)
    query <- paste("START TRANSACTION")
    dbGetQuery(db,query)
    
    meses <- c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")
    for(i in 1:12){
      mes <- meses[i]
      media <- mediaSint[i]
      dp <- dpSint[i]
      assimetria <- assimeSint[i]
      kurt <- kurtSint[i]
      coef_var <- coefSint[i]
      
      query <- paste("INSERT INTO AVALIACAO(mes,media,dp,assimetria,ind_kurt,coef_var,id_DESAGREGADO) VALUES(","'",mes,"'",",",media,",",dp,",",assimetria,",",kurt,",",coef_var,",",id_DESAGREGADO,")")
      dbGetQuery(db,query)
    }
    
    query <- paste("COMMIT")
    dbGetQuery(db,query)
    
    fim <- Sys.time()
    dbDisconnect(db)
    
  }
  
  inserirACF_MensalDESAGREGACAO <- function(id_DESAGREGADO,acf_mensal){
    
    db <- conectarDataBase()
    inicio <- Sys.time()
    
    query <- paste("SET AUTOCOMMIT = 0")
    dbGetQuery(db,query)
    query <- paste("START TRANSACTION")
    dbGetQuery(db,query)
    
    for (i in 1:nrow(acf_mensal)) {
      lag <- i
      v1 <- acf_mensal[i,1]
      v2 <- acf_mensal[i,2]
      v3 <- acf_mensal[i,3]
      v4 <- acf_mensal[i,4]
      v5 <- acf_mensal[i,5]
      v6 <- acf_mensal[i,6]
      v7 <- acf_mensal[i,7]
      v8 <- acf_mensal[i,8]
      v9 <- acf_mensal[i,9]
      v10 <- acf_mensal[i,10]
      v11 <- acf_mensal[i,11]
      v12 <- acf_mensal[i,12]
      
      query <- paste("INSERT INTO ACF_MENSAL VALUES (","NULL",",",lag,",",v1,",",v2,",",v3,",",v4,",",v5,",",v6,",",v7,",",v8,",",v9,",",v10,",",v11,",",v12,",NULL,NULL,",id_DESAGREGADO,")")
      dbGetQuery(db,query)
    }
    query <- paste("COMMIT")
    dbGetQuery(db,query)
    
    fim <- Sys.time()
    dbDisconnect(db)
    
  }
  
  inserirACF_ANUALDESAGREGACAO <- function(id_DESAGREGADO,acf_anual){
    
    db <- conectarDataBase()
    inicio <- Sys.time()
    query <- paste("SET AUTOCOMMIT = 0")
    dbGetQuery(db,query)
    query <- paste("START TRANSACTION")
    dbGetQuery(db,query)
    
    for (i in 1:nrow(acf_anual)) {
      lag <- i
      valor <- acf_anual[i,1]
      
      query <- paste("INSERT INTO ACF_ANUAL VALUES(NULL,",valor,",",lag,",NULL,NULL,",id_DESAGREGADO,")")
      dbGetQuery(db,query)
    }
    
    query <- paste("COMMIT")
    dbGetQuery(db,query)
    
    
    fim <- Sys.time()
    dbDisconnect(db)
  }
  
  inserirHurstVolDESAGREGACAO<- function(id_Desagregado,hurstAnual,hurstMensal,volume){
    
    db <- conectarDataBase()
    inicio <- Sys.time()
    
    
    query <- paste("INSERT INTO HURST VALUES(NULL,",hurstAnual,",",hurstMensal,",NULL,NULL,",id_Desagregado,")")
    dbGetQuery(db,query)
    
    query <- paste("INSERT INTO VOLUME VALUES(NULL,",volume,",NULL,NULL,",id_Desagregado,")")
    dbGetQuery(db,query)
    
    fim <- Sys.time()
    dbDisconnect(db)
    
  }
 