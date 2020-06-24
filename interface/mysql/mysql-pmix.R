# FUNCOES RELACIONADAS AO ARMAZENAMENTO DA SERIE SINTETICA GERADA PELO MODELO PMIX

# Funcao registrarSSPMIX: Armazena uma nova serie sintetica na table serie_sintetica do banco de dados
# Input: input da UI
# idEstacao: id da estacao que gerou a serie sintetica
# Return: retorna o id da serie cadastrada na table serie_sintetica

registrarSSPMIX <- function(input,idEstacao){
  
  db <- conectarDataBase()
  
  modelo <- "PMIX"
  anos <- (input$nsint)
  lags <- (paste("(",input$p,",",input$q,",",input$P,",",input$Q,")",sep=""))
  
  if(input$tipo == 1){
    metodo <- "Powell"
  }else if(input$tipo == 2){
    metodo <- "Algoritmo Genetico"
  }
  
  query <- paste("INSERT INTO SERIE_SINTETICA VALUES(NULL,'",modelo,"',",anos,",'",lags,"','",metodo,"',",idEstacao,",CURRENT_TIMESTAMP())",sep="")
  data <- dbGetQuery(db, query)
  
  query <- "SELECT LAST_INSERT_ID();"
  idSERIE_SINTETICA <- dbGetQuery(db, query)
  
  dbDisconnect(db)
  
  return(idSERIE_SINTETICA)
  
}

# Funcao inserirSS: Funcao para inserir uma serie sintetica no banco de dados (table serie)
# id_SERIE_SINTETICA: id da serie sintetica que foi gerado na funcao registrarSSPMIX.
# serie_sintetica: serie sintetica gerada pelo modelo pmix

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

# Funcao inserirAvaliacaoSS: Insere na table "avaliacao" do banco de dados as avaliacoes da serie sintetica gerada pelo modelo PMIX.
# id_SERIE_SINTETICA: id da serie sintetica que foi gerado na funcao registrarSSPMIX.
# mediaSint, dpSint, assimeSint, kurtSint, coefSint: avaliacoes calculadas pelo module "avaliacaoMensal"

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

# Funcao inserirACF_MensalSS: Insere na table "acf_mensal" do banco de dados a acf mensal da serie sintetica gerada pelo modelo PMIX.
# id_SERIE_SINTETICA: id da serie sintetica que foi gerado na funcao registrarSSPMIX.
# acf_mensal: acf mensal calculada pelo module "facMensal"

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

# Funcao inserirACF_ANUALSS: Insere na table "acf_anual" do banco de dados a acf anual da serie sintetica gerada pelo modelo PMIX/ARMA.
# id_SERIE_SINTETICA: id da serie sintetica que foi gerado na funcao registrarSSPMIX/registrarSSARMA.
# acf_anual: acf anual calculada pelo module "facAnual"

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

# Funcao inserirSomHurstVol: Insere nas tables soma_residual, hurst e volume os valores calculados da serie gerada pelo modelo PMIX.
# id_SERIE_SINTETICA: id da serie sintetica que foi gerado na funcao registrarSSPMIX.
# somRes: soma residual calculada na funcaoAgoritmo
# hurstAnual: hurst da serie sintetica agregada calculado pelo module "coeficienteHurst"
# hurstMensal: hurt da serie sintetica calculado pelo module "coeficienteHurst"
# volume: volume da serie sintetica calculado pelo module "volume"

inserirSomHurstVol <- function(id_SERIE_SINTETICA,somRes,hurstAnual,hurstMensal,volume){
  
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