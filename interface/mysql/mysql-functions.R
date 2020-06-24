# FUNCOES GERAIS RELACIONADAS AO BANCO DE DADOS "modelagem_estocastica"
# Fonte: https://shiny.rstudio.com/articles/persistent-data-storage.html#mysql

library(RMySQL)
library('DT')
library('data.table')

# Funcao conectarDataBase: Realiza a conexao com o banco de dados "modelagem_estocastica"
# Return: objeto db que abre a conexao com o banco de dados

conectarDataBase <- function(){
  
  db <- dbConnect(MySQL(),
                  user = 'root',
                  password = 'labhidro',
                  host = 'localhost',
                  dbname = 'modelagem_estocastica')
  
  return(db)
    
}

# Funcao loadData: Seleciona dados da uma table do banco de dados
# table: nome da table que se deseja extrair os dados
# Return: retorna os dados da table.

loadData <- function(table) {
  # Conectando com o banco de dados
  db <- conectarDataBase()
  
  # Construindo a query de seleção
  query <- sprintf("SELECT * FROM %s", table)
  
  # Submetendo a query e desconectando do bando de dados
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  return(data)
}

# Funcao alterDataBase: realiza uma query no banco de dados
# query: a query que se deseja executar no banco de dados
# Retunr: Retorna o dado, caso a query seja de consulta

alterDataBase <- function(query){
  # Conectando com o banco de dados
  db <- conectarDataBase()
  
  # Submetendo a query e desconectando do bando de dados
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  return(data)
}

# Funcao cadastroEstacao: Essa funcao realiza o cadastro de uma estacao no banco de dados (table estacao)

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

# Funcao infoEstacao: busca dos dados de uma estacao no banco de dados (table estacao)
# nome: nome da estacao
# Return: os dados da estacao 

infoEstacao <- function(nome){
  nome <- paste("'",nome,"'",sep="")
  query <- paste("CALL ESTACAO_INFO_NOME(",nome,")",sep="")
  dadosList <- alterDataBase(query)
  dadosDT <- setDT(dadosList)
  return(dadosDT)
}

# Funcao buscarID: faz uma consulta no banco de dados. Mais especificamente, busca um id de um registro no banco de dados.
# Return: o id desejado

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

# Funcao cadastrarSH: Insere a serie historica de uma estacao no banco de dados (table vazao)
# fileSH: arquivo com a serie historica da estacao
# codigoEstacao: codigo da estacao 

cadastrarSH <- function(fileSH,codigoEstacao){
  if(!is.null(fileSH) && (fileSH != "") 
     && !is.null(codigoEstacao) && (codigoEstacao != "")){
    
    #Lendo o arquivo com a Serie Historica
    serie_historica = read.table(fileSH,head=T,sep = ";" , dec = ",")
    db <- conectarDataBase()
    
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

# FUNCOES PARA CONSULTAR DADOS DE UMA ESTACAO (SERIE HISTORICA)
# Funcoes: buscarSH, buscarACF_MENSAL, buscarACF_ANUAL, buscarHURST,  buscarAVALIACAO, buscarVOLUME, buscarSOMA_RESIDUAL e buscarSS
# codigoEstacao: codigo da estacao
# nomeEstacao: nome da estacao
  
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
  
# FUNCOES PARA CONSULTAR DADOS DAS SERIES SINTETICAS
# Funcoes: buscarDESAGREGADO_SS, buscarACF_MENSAL_SS, buscarACF_ANUAL_SS, buscarHURST_SS, buscarAVALIACAO_SS, buscarVOLUME_SS e buscarSOMARESIDUAL_SS  
# idSerie_Sintetica: id da serie sintetica
  
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
  
# FUNCOES PARA CONSULTAR DADOS DAS SERIES DESAGREGADAS
# Funcoes: buscarACF_MENSAL_SD, buscarACF_ANUAL_SD, buscarHURST_SD, buscarAVALIACAO_SD, buscarVOLUME_SD e buscarSOMARESIDUAL_SD 
# idDesagregado: id da serie desagregada
  
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
  
  # Funcao SeriesSinteticas: Pega os dados da table serie_sintetica
  # Return: os dados da table serie_sintetica na forma de data table.
  
  SeriesSinteticas <- function(){
    query <- paste("SELECT idSERIE_SINTETICA,codigo,nome,modelo,lags,metodo,SERIE_SINTETICA.anos,register_date FROM SERIE_SINTETICA, ESTACAO WHERE ID_ESTACAO = IDESTACAO;")
    dadosList<-alterDataBase(query)
    dadosDT <- setDT(dadosList)
    colnames(dadosDT) = c("ID","Codigo","Estacao","Modelo","Lags","Metodo","Anos","Data")
    return(dadosDT)
  }
  
  # Funcao SeriesDesagregadas: Pega os dados da table desagregado
  # Return: os dados da table desagregado na forma de data table
  
  SeriesDesagregadas <- function(){
    query <- paste("SELECT idDESAGREGADO,id_SERIE_SINTETICA,parametrico,register_date FROM DESAGREGADO;")
    dadosList<-alterDataBase(query)
    dadosDT <- setDT(dadosList)
    colnames(dadosDT) = c("ID","ID Serie Sintetica","Parametrica","Data")
    return(dadosDT)
  }
  
  # Funcao SeriesSinteticas: Pega os dados da table estacao
  # Return: os dados da table estacao na forma de data table
  
  Estacoes <- function(){
    query <- paste("SELECT * FROM ESTACAO;")
    dadosEstacao <- alterDataBase(query)
    dadosDT <- setDT(dadosEstacao)
    return(dadosDT)
  }
  
  # Funcao selectSerie_Sintetica: Pega uma serie sintetica (serie temporal) na table serie 
  # idSerie_Sintetica: id da serie que se deseja pegar os dados
  
  selectSerie_Sintetica = function(idSerie_Sintetica){
    if(!is.null(idSerie_Sintetica)){
      query <- paste("SELECT JAN, FEV,MAR,ABR,MAI,JUN,JUL,AGO,SEB,OUB,NOV,DEZ FROM SERIE WHERE id_SERIE_SINTETICA = ",idSerie_Sintetica)
      serie <- alterDataBase(query)
      return(serie)
    }
  }
  
  # Funcao selectSerie_Desagregada: Pega uma serie desagregada (serie temporal) na table serie 
  # id_Desagregado: id da serie que se deseja pegar os dados
  
  selectSerie_Desagregada = function(id_DESAGREGADO){
    if(!is.null(id_DESAGREGADO)){
      query <- paste("SELECT JAN, FEV,MAR,ABR,MAI,JUN,JUL,AGO,SEB,OUB,NOV,DEZ FROM SERIE WHERE id_DESAGREGADO = ",id_DESAGREGADO)
      serie <- alterDataBase(query)
      return(serie)
    }
  }
  
  # Funcao SeriesSintetica_Anuais: Pega uma serie sintetica anual (que sera desagregada) na table serie/serie_anual 
  # idSerieSintetica: id da serie que se deseja pegar os dados
  # modelo: PMIX (serie esta na table "serie") OU ARMA (serie esta na table "serie_anual")
  
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
  
  # Funcoes para deletar as series do banco de dados
  
  # Funcao deleteSerieSS: deleta uma serie sintetica do banco de dados. Sempre que se deleta uma seria, suas avaliacoes
  # nas tables acf_mensal, acf_anual, avaliacao, hurste, volume e soma residual tambem sao apagadas.
  # idSerie_Sintetica: id da serie sintetica que deseja-se apagar.
  
  deleteSerieSS =  function(idSerie_Sintetica){
    if(!is.null(idSerie_Sintetica)){
      query = paste("DELETE FROM SERIE_SINTETICA WHERE idSERIE_SINTETICA = ",idSerie_Sintetica)
      alterDataBase(query)
    }
  }
  
  # Funcao deleteSerieSD: deleta uma serie desagregada do banco de dados. Sempre que se deleta uma seria, suas avaliacoes
  # nas tables acf_mensal, acf_anual, avaliacao, hurst e volume tambem sao apagadas.
  # idDesagregado: id da serie desagregada que deseja-se apagar.
  
  deleteSerieSD =  function(idDesagregado){
    if(!is.null(idDesagregado)){
      query = paste("DELETE FROM DESAGREGADO WHERE idDESAGREGADO = ",idDesagregado)
      alterDataBase(query)
    }
  }
  
  # Funcao deleteEstacao: deleta uma estacao do banco de dados. Sempre que se deleta uma estacao, suas avaliacoes
  # nas tables acf_mensal, acf_anual, avaliacao, hurste, volume e soma residual tambem sao apagadas. As series geradas por aquela estacao
  # tambem sao apagadas. CUIDADO AO USAR ESSA FUNCAO
  # nome: nome da estacao que deseja-se apagar.
  
  deleteEstacao =  function(nome){
    if(!is.null(nome)){
      query = paste("DELETE FROM ESTACAO WHERE nome = '",nome,"'",sep="")
      alterDataBase(query)
    }
  }
  