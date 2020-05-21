# FUNCOES RELACIONADAS AO ARMAZENAMENTO DA SERIE SINTETICA GERADA PELO MODELO ARMA

# Funcao registrarSSARMA: Armazena uma nova serie sintetica na table serie_sintetica do banco de dados
# p: lag p do Modelo ARMA que foi utilizado para gerar a serie sintetica 
# q: lag q do Modelo ARMA que foi utilizado para gerar a serie sintetica
# nsint: tamanho da serie sintetica 
# idEstacao: id da estacao que gerou a serie sintetica
# Return: retorna o id da serie cadastrada na table serie_sintetica

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

# Funcao inserirSS_ARMA: Funcao para inserir uma serie sintetica anual no banco de dados (table serie_anual)
# id_SERIE_SINTETICA: id da serie sintetica que foi gerado na funcao registrarSSARMA.
# serie_sintetica: serie sintetica gerada pelo modelo pmix

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

# Funcao inserirAvaliacaoSS_ARMA: Insere na table "avaliacao" do banco de dados as avaliacoes da serie sintetica gerada pelo modelo ARMA.
# id_SERIE_SINTETICA: id da serie sintetica que foi gerado na funcao registrarSSARMA.
# mediaSint, dpSint, assimeSint, kurtSint, coefSint: avaliacoes calculadas pelo module "avaliacaoAnual"

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

# Funcao inserirSomHurst_ARMA: Insere nas tables soma_residual e hurst os valores calculados da serie gerada pelo modelo ARMA.
# id_SERIE_SINTETICA: id da serie sintetica que foi gerado na funcao registrarSSARMA.
# somRes: soma residual (somaRes_ARMA)
# hurstAnual: hurst da serie sintetica agregada calculado pelo module "coeficienteHurst"

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

# Funcao inserirVol_ARMA: Insere nas table "volume" o volume calculado da serie gerada pelo modelo ARMA.
# id_SERIE_SINTETICA: id da serie sintetica que foi gerado na funcao registrarSSARMA.
# volume: volume da serie sintetica calculado pelo module "volume"

inserirVol_ARMA<- function(id_SERIE_SINTETICA,volume){
  
  db <- conectarDataBase()   
  inicio <- Sys.time()
  
  query <- paste("INSERT INTO VOLUME VALUES(NULL,",volume,",NULL,",id_SERIE_SINTETICA,",NULL)")
  dbGetQuery(db,query)
  
  fim <- Sys.time()
  dbDisconnect(db)
  
}