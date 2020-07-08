# FUNCOES RELACIONADAS AO ARMAZENAMENTO DAS SERIES DESAGREGADAS

# Funcao registrarSSDESAGREGACAO: Armazena uma nova serie desagregada na table desagregado do banco de dados
# idSerie_Sintetica: o id da serie sintetica que foi desagregada 
# parametrico: 'S' ou 'N' indicando se a serie foi desagregada pelo metodo parametrico(S) ou nao-parametrico(N)
# Return: retorna o id da serie cadastrada na table desagregado

registrarSSDESAGREGACAO <- function(idSerie_Sintetica,parametrico){
  
  db <- conectarDataBase()
  query <- paste("INSERT INTO DESAGREGADO VALUES(NULL,'",parametrico,"',",idSerie_Sintetica," ,CURRENT_TIMESTAMP())",sep="")
  data <- dbGetQuery(db, query)
  
  query <- "SELECT LAST_INSERT_ID();"
  idDESAGREGADO <- dbGetQuery(db, query)
  dbDisconnect(db)
  
  return(idDESAGREGADO)
  
} 

# Funcao inserirSS_Desagregado: Funcao para inserir uma serie desagregada no banco de dados (table serie)
# id_Desagregado: id da serie desagregada que foi gerado na funcao registrarSSDESAGREGACAO.
# serie_sintetica: serie desagregada gerada pelo modelo pmix

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

# Funcao inserirAvaliacaoDESAGREGACAO: Insere na table "avaliacao" do banco de dados as avaliacoes da serie desagregada.
# id_DESAGREGADO: id da serie desagregada que foi gerado na funcao registrarSSDESAGREGACAO.
# mediaSint, dpSint, assimeSint, kurtSint, coefSint: avaliacoes calculadas pelo module "avaliacaoMensal"

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

# Funcao inserirACF_MensalDESAGREGACAO: Insere na table "acf_mensal" do banco de dados a acf mensal da serie desagregada.
# id_DESAGREGADO id da serie desagregada que foi gerado na funcao registrarSSDESAGREGACAO.
# acf_mensal: acf mensal calculada pelo module "facMensal"

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

# Funcao inserirACF_ANUALDESAGREGACAO: Insere na table "acf_anual" do banco de dados a acf anual da serie desagregada.
# id_DESAGREGADO: id da serie desagregada que foi gerado na funcao registrarSSDESAGREGACAO.
# acf_anual: acf anual calculada pelo module "facAnual"

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

# Funcao inserirHurstVolDESAGREGACAO: Insere nas tables hurst e volume os valores calculados da serie desagregada.
# id_Desagregado: id da serie desagregada que foi gerado na funcao registrarSSDESAGREGACAO.
# hurstAnual: hurst anual da serie desagregada calculado pelo module "coeficienteHurst"
# hurstMensal: hurt mensal da serie desagregada calculado pelo module "coeficienteHurst"
# volume: volume da serie desagregada calculado pelo module "volume"

inserirHurstVolDESAGREGACAO<- function(id_Desagregado,hurstAnual,hurstMensal,volume){
  
  db <- conectarDataBase()
  
  query <- paste("INSERT INTO HURST VALUES(NULL,",hurstAnual,",",hurstMensal,",NULL,NULL,",id_Desagregado,")")
  dbGetQuery(db,query)
  
  if(!is.infinite(volume)){
    query <- paste("INSERT INTO VOLUME VALUES(NULL,",volume,",NULL,NULL,",id_Desagregado,")")
    dbGetQuery(db,query)
  }
  
  dbDisconnect(db)
  
}