# Funcao lista_df: auxiliar que transforma uma lista num grande dataframe
lista_df<-function(lista){
  df = data.frame()
  for(i in 1:length(lista)){
    if (i==1){
      df = lista[[i]]
    }
    else{
      df = rbind(df, as.data.frame(lista[[i]]))
    }
  }
  return(df)
}


# Funcao padroniza_df: Padroniza os dados anuais de uma dataframe com informcoes mensais
padroniza_df<-function(df){
  df_mod = apply(df,1,sum)
  df_mod = as.data.frame(df_mod)
  df_padronizada= apply(log(df_mod), 2, function(x) (x-mean(x))/(sd(x)))
  return(df_padronizada)
}


#Funcao div_mensais: Esta funcao pega os dados brutos e retorna uma tabela ano x mes da serie historica
div_mensais<-function(sH)
{ 
  #Calcula, baseado no numero de linhas a quantidade de anos registrados no arquivo
  qtd_ano_hist = nrow(sH[,1])/12 
  
  #Quebra o dataframe em qtd_anos_hist partes(anos) e cada parte e convertida numa linha da nova tabela
  serie_hist = matrix(sH$valor, qtd_ano_hist,byrow = TRUE)
  
  #Pega a coluna de datas e interpreta quais os anos foram analisados baseados nos ultimos 4 caracteres da informacao da coluna "MES"
  anos = as.character(sH$periodo)
  anos = substr(anos, nchar(anos)-4+1, nchar(anos))
  anos = unique(anos)
  anos = sort(anos)                                       
  
  #Nomeia linhas e colunas e converte a matriz em um dataframe
  row.names(serie_hist)= anos
  colnames(serie_hist)=c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")
  serie_hist=as.data.frame(serie_hist)                    
 
  return(serie_hist)
}

# Funcao desagrega_np: Desagrega de forma nao parametrica os dados sinteticos utilizando estatisticas e dados historicos
# Metodo proposto por LEE, T., SALAS, J.D., PRAIRIE, J. (2010) DOI: 10.1029/2009WR007761

desagrega_np<-function(serieSint,SeriesDadosHist)
{ 
  qtd_ano_hist = nrow(SeriesDadosHist) 
  qtd_ano_des = length(serieSint[,1])
  
  #Cria um data frame vazio que sera nossa serie anual
  desagregado_final = data.frame() 
  ##################PRIMEIRA ITERACAO DE DESAGREGACAO##########################
  
  #Dados as vazoes mensais, calcula as vazoes anuais
  Anuais = data.frame(V1=apply(SeriesDadosHist,1,sum))
  Anuais_1 = rownames(Anuais)[1]
  primeiro_ano = rep(serieSint[1,1],length(Anuais))

  #Faz um vetor da diferenca(delta_i) do primeiro ano sintetico referente a vazao anual com todos os anos historicos( |X1-xi| )
  delta_i = abs(primeiro_ano-Anuais)
  x=delta_i$V1
  
  #Faz uma tabela que relaciona ano, vazao anual historica e diferenca(delta_i)
  Tabela = cbind(Anuais,delta_i$V1)
  
  #Ordena de forma crescente de delta_i
  Tabela = Tabela[order(Tabela$delta_i),]

  ############CALCULO DE K E DO CWM####################
  K = floor(sqrt(length(delta_i$V1)))
  
  div=sum(1/1:K)
  
  cwm=rep(0,K)
  for(i in 1:K)
  {
    if(i==1){
      cwm[i]=(1/div)
    }
    else
      cwm[i]=cwm[i-1]+(1/i)/div
  }

  #Escolhe numero aleatorio no vetor de pesos cumulativos(cwm) e armazena na variavel 'random'
  random = runif(1) 
  
  #Armazena na variavel 'posicao' a posicao do numero escolhido no vetor cwm
  posicao = which.min(abs(cwm - random))
  
  #Armazena o ano candidato a desagregacao na variavel 'candidato'
  candidato = rownames(Tabela)[posicao] 
  
  desagregado = SeriesDadosHist[candidato,]*(serieSint$anual[1]/(apply(SeriesDadosHist[candidato,],1,sum)))
  desagregado_final = rbind(desagregado_final,desagregado)
  
  
  ############FIM DA DESAGREGACAO DO ANO 1##############
  Tabela = Tabela[order(row.names(Tabela)),]
  ############DESAGREGACAO DOS OUTROS ANOS###########################
  fi_1 = 1/var(Anuais[2:qtd_ano_hist,1])
  fi_2 = 1/var(SeriesDadosHist$DEZ[2:qtd_ano_hist])
  
  delta_i = numeric (qtd_ano_hist-1)
  for (j in 2:(nrow(serieSint)))
  {
    ########### CALCULO DO DELTA_i###########
    for(i in 2:qtd_ano_hist)
    {
      delta_i[(i-1)] = sqrt(fi_1*(serieSint$anual[j]-Anuais[i,1])^2 + fi_2*(desagregado_final$DEZ[j-1]-SeriesDadosHist$DEZ[i-1])^2)
    }
    
    Tabela = Anuais
    Tabela = Tabela[-1, ]
    Tabela = data.frame(Tabela)
    Tabela[,2] = rep(0, (qtd_ano_hist-1))
    rownames(Tabela) = row.names(Anuais)[-1]
    colnames(Tabela) = c("V1", "delta_i")
    Tabela[,2] = delta_i
    Tabela = Tabela[!(row.names(Tabela) %in% Anuais_1),]
    Tabela = Tabela[order(Tabela$delta_i),]
    random = runif(1, 0, 1)
    posicao = which.min(abs(cwm - random))
    candidato = rownames(Tabela)[posicao]
    desagregado = SeriesDadosHist[candidato,]*(serieSint$anual[j]/(apply(SeriesDadosHist[candidato,],1,sum)))
    
    desagregado_final = rbind(desagregado_final,desagregado)
    
    
  }
  rownames(desagregado_final) = NULL
  return(desagregado_final)
}

######################### DESAGREGACAO PARAMETRICA #########################

# Funcao parametros_historicos: calcula dos parametros historicos necessarios para a desagregacao parametrica
# Parametro: a serie historica normalizada 
# Retorno: parametros historicos para a desagregacao
parametros_historicos<-function(serie_historica_normalizada){
  Info=list()
  for (i in 1:11){
    if (i==1)
      Info[[i]] = cbind(serie_historica_normalizada[,i],serie_historica_normalizada[,12],apply(serie_historica_normalizada[,-1],1,sum),apply(serie_historica_normalizada,1,sum))
    else if(i<11)
      Info[[i]] = cbind(serie_historica_normalizada[,i],serie_historica_normalizada[,i-1],apply(serie_historica_normalizada[,((i+1):12)],1,sum),apply(serie_historica_normalizada[,(i:12)],1,sum))
    else
      Info[[i]] = cbind(serie_historica_normalizada[,i],serie_historica_normalizada[,i-1],serie_historica_normalizada[,12],apply(serie_historica_normalizada[,(i:12)],1,sum))
  }
  return(Info)
}

# Funcao autocovariancia 
# Parametro: os parametros historicos calculados pela funcao parametros_historicos
# Retorno: a autocovariancia desses parametros, necessaria para realizar a desagregacao (de janeiro a novembro)
autocovariancia <- function(info){
  ACF_S = list()
  for(i in 1:11){
    ACF_S[[i]]= acf(info[[i]],lag.max = 1,type = "covariance", plot = FALSE)
    ACF_S[[i]]= ACF_S[[i]]$acf
  }
  return(ACF_S)
}

# Funcao parametro A : Essa funcao calculo o parametro A da formula da desagregacao parametrica
# Parametro: a autocovariancia calculada na funcao autocovariancia
# Retorno: parametro A da desagregacao
parametro_A <- function(ACF_S){
  A = list()
  for(i in 1:11){
    
    if(i == 1){
      
      Sxx = ACF_S[[i]][1,4,4] # Anual com Anual
      Sxx_ = ACF_S[[i]][2,4,4] # Anual com Anual lag1
      Syx = cbind(ACF_S[[i]][1,4,1],ACF_S[[i]][1,4,3]) # Sazonal com Anual
      Syz = cbind(ACF_S[[i]][1,2,1],ACF_S[[i]][1,2,3]) # Sazonal com Sazonalidade anterior  ##Alterei aqui
      Sxz = ACF_S[[i]][1,2,4] ##### Anual com Sazonalidade anterior //CONFERIR
      Szz = ACF_S[[i]][1,2,2] # Sazonalidade anterior com Sazonalidade anterior
      Syy = cbind(rbind(ACF_S[[i]][1,1,1],ACF_S[[i]][1,3,1]), rbind(ACF_S[[i]][1,1,3],ACF_S[[i]][1,3,3]))
      Sx_z = ACF_S[[i]][2,2,4] ##### Anual com //CONFERIR
      Sxz_cor = Sxx_%*%(solve(Sxx))%*%(Sx_z)
      Syz_cor = t(Syz)+(t(Syx)%*%solve(Sxx)%*%(Sxz_cor-Sxz))
      A[[i]] = (t(Syx)-Syz_cor%*%solve(Szz)%*%(Sxz_cor))%*%solve(Sxx-Sxz_cor%*%solve(Szz)%*%Sxz_cor)
      
    }else{
      
      Sxx = ACF_S[[i]][1,4,4] # Anual com Anual
      Sxx_ = ACF_S[[i]][2,4,4] # Anual com Anual lag1
      Syx = cbind(ACF_S[[i]][1,4,1],ACF_S[[i]][1,4,3]) # Sazonal com Anual
      Syz = cbind(ACF_S[[i]][1,2,1],ACF_S[[i]][1,2,3]) # Sazonal com Sazonalidade anterior  ##Alterei aqui
      Sxz = ACF_S[[i]][1,2,4] ##### Anual com Sazonalidade anterior //CONFERIR
      Szz = ACF_S[[i]][1,2,2] # Sazonalidade anterior com Sazonalidade anterior
      Syy = cbind(rbind(ACF_S[[i]][1,1,1],ACF_S[[i]][1,3,1]), rbind(ACF_S[[i]][1,1,3],ACF_S[[i]][1,3,3]))
      Sx_z = ACF_S[[i]][2,2,4] ##### Anual com //CONFERIR
      A[[i]] = (t(Syx)-t(Syz)%*%solve(Szz)%*%(Sxz))%*%solve(Sxx-Sxz%*%solve(Szz)%*%Sxz)
      
    }
    
  }
  
  return(A)
}

# Funcao parametro C : Essa funcao calculo o parametro C da formula da desagregacao parametrica
# Parametros: a autocovariancia calculada na funcao autocovariancia, o parametro A calculado pela funcao parametro_A
# Retorno: parametro C da desagregacao
parametro_C <- function(ACF_S,A){
  C = list()
  for(i in 1:11){
    
    if(i == 1){
      
      Sxx = ACF_S[[i]][1,4,4] # Anual com Anual
      Sxx_ = ACF_S[[i]][2,4,4] # Anual com Anual lag1
      Syx = cbind(ACF_S[[i]][1,4,1],ACF_S[[i]][1,4,3]) # Sazonal com Anual
      Syz = cbind(ACF_S[[i]][1,2,1],ACF_S[[i]][1,2,3]) # Sazonal com Sazonalidade anterior  ##Alterei aqui
      Sxz = ACF_S[[i]][1,2,4] ##### Anual com Sazonalidade anterior //CONFERIR
      Szz = ACF_S[[i]][1,2,2] # Sazonalidade anterior com Sazonalidade anterior
      Syy = cbind(rbind(ACF_S[[i]][1,1,1],ACF_S[[i]][1,3,1]), rbind(ACF_S[[i]][1,1,3],ACF_S[[i]][1,3,3]))
      Sx_z = ACF_S[[i]][2,2,4] ##### Anual com //CONFERIR
      Sxz_cor = Sxx_%*%(solve(Sxx))%*%(Sx_z)
      Syz_cor = t(Syz)+(t(Syx)%*%solve(Sxx)%*%(Sxz_cor-Sxz))
      C[[i]]= (Syz_cor-A[[i]]%*%Sxz_cor)%*%solve(Szz)
      
      
      
    }else{
      
      Sxx = ACF_S[[i]][1,4,4] # Anual com Anual
      Sxx_ = ACF_S[[i]][2,4,4] # Anual com Anual lag1
      Syx = cbind(ACF_S[[i]][1,4,1],ACF_S[[i]][1,4,3]) # Sazonal com Anual
      Syz = cbind(ACF_S[[i]][1,2,1],ACF_S[[i]][1,2,3]) # Sazonal com Sazonalidade anterior  ##Alterei aqui
      Sxz = ACF_S[[i]][1,2,4] ##### Anual com Sazonalidade anterior //CONFERIR
      Szz = ACF_S[[i]][1,2,2] # Sazonalidade anterior com Sazonalidade anterior
      Syy = cbind(rbind(ACF_S[[i]][1,1,1],ACF_S[[i]][1,3,1]), rbind(ACF_S[[i]][1,1,3],ACF_S[[i]][1,3,3]))
      Sx_z = ACF_S[[i]][2,2,4] ##### Anual com //CONFERIR
      C[[i]] = (t(Syz) - A[[i]]%*%Sxz)%*%solve(Szz)
      
      
    }
    
  }
  
  return(C)
}

# Funcao parametro_Bt : Essa funcao calculo o parametro B da formula da desagregacao parametrica
# Parametros: a autocovariancia calculada na funcao autocovariancia, o parametro A calculado pela funcao parametro_A e o parametro C calculado pela funcao parametro_C
# Retorno: parametro Bt da desagregacao
parametro_Bt <- function(ACF_S,A,C){
  Bt = list()
  for(i in 1:11){
    
    if(i == 1){
      
      Sxx = ACF_S[[i]][1,4,4] # Anual com Anual
      Sxx_ = ACF_S[[i]][2,4,4] # Anual com Anual lag1
      Syx = cbind(ACF_S[[i]][1,4,1],ACF_S[[i]][1,4,3]) # Sazonal com Anual
      Syz = cbind(ACF_S[[i]][1,2,1],ACF_S[[i]][1,2,3]) # Sazonal com Sazonalidade anterior  ##Alterei aqui
      Sxz = ACF_S[[i]][1,2,4] ##### Anual com Sazonalidade anterior //CONFERIR
      Szz = ACF_S[[i]][1,2,2] # Sazonalidade anterior com Sazonalidade anterior
      Syy = cbind(rbind(ACF_S[[i]][1,1,1],ACF_S[[i]][1,3,1]), rbind(ACF_S[[i]][1,1,3],ACF_S[[i]][1,3,3]))
      Sx_z = ACF_S[[i]][2,2,4] ##### Anual com //CONFERIR
      Sxz_cor = Sxx_%*%(solve(Sxx))%*%(Sx_z)
      Syz_cor = t(Syz)+(t(Syx)%*%solve(Sxx)%*%(Sxz_cor-Sxz))
      BBt = (Syy)-(A[[i]])%*%(Syx)-C[[i]]%*%t(Syz_cor)
      B = chol(BBt,pivot = TRUE)
      Bt[[i]] = t(B)
      
      
      
      
    }else{
      
      Sxx = ACF_S[[i]][1,4,4] # Anual com Anual
      Sxx_ = ACF_S[[i]][2,4,4] # Anual com Anual lag1
      Syx = cbind(ACF_S[[i]][1,4,1],ACF_S[[i]][1,4,3]) # Sazonal com Anual
      Syz = cbind(ACF_S[[i]][1,2,1],ACF_S[[i]][1,2,3]) # Sazonal com Sazonalidade anterior  ##Alterei aqui
      Sxz = ACF_S[[i]][1,2,4] ##### Anual com Sazonalidade anterior //CONFERIR
      Szz = ACF_S[[i]][1,2,2] # Sazonalidade anterior com Sazonalidade anterior
      Syy = cbind(rbind(ACF_S[[i]][1,1,1],ACF_S[[i]][1,3,1]), rbind(ACF_S[[i]][1,1,3],ACF_S[[i]][1,3,3]))
      Sx_z = ACF_S[[i]][2,2,4] ##### Anual com //CONFERIR
      BBt = (Syy)-(A[[i]])%*%(Syx)-C[[i]]%*%(Syz)
      B = chol(BBt,pivot=TRUE)
      Bt[[i]] = t(B)
      
      
    }
    
  }
  
  return(Bt)
}

# Funcao desagregacao_parametrica_ano: realiza a desagregacao de um ano, gerando 12 valores mensais
# Parametros: a vazao anual(ano), o zInicial, A(calculado com a funcao parametro_A),Bt(calculado com a funcao parametro_Bt)e C(calculado com a funcao parametro_C)
# Retorno: o ano desagregado
# OBS: Essa é a funcao desag_param modificada

desagregacao_parametrica_ano <- function(ano,Zinicial,A,Bt,C){
  Meses = 0
  Resto = 0
  
  # Gerando 22 valoreas aleatorios
  ERRO = rnorm (22, 0, 1)
  #normalizando o ERRO
  ERRO = (ERRO - mean (ERRO)) / sd (ERRO)
  #Criando a matriz com 11 linhas e duas colunas
  ERRO = matrix(ERRO,ncol=2)
  
  for(i in 1:11){
    if(i==1){
      #Pegando a linha correspondente a i
      erro = ERRO[i,]
      Y = A[[i]]%*%ano+Bt[[i]]%*%(erro)+C[[i]]%*%Zinicial 
      Meses[i] = Y[1,1]
      Resto[i] = Y[2,1]
      
    }
    else{
      erro = ERRO[i,]
      Y = A[[i]]%*%Resto[i-1]+Bt[[i]]%*%erro+C[[i]]%*%Meses[i-1]
      Meses[i] = Y[1,1]
      Resto[i] = Y[2,1]
    }
    
  }
  Meses[12] = Resto[11]
  return(Meses) 
}

# anual: vazao anual sintetica
# mensal: vetor mensal desagregado da vazao anual sintetica (vetor de 12 valores)
ajuste_proporcional <- function(anual,mensal){
  
  soma_mensal = sum(mensal)
  return(sapply(mensal,function(x)(x*anual/soma_mensal)))
  
}

# Funcao de desagregacao_parametrica : Realiaza a desagregacao da serie_sintetica
# Parametro: a serie sintetica e a serie historica.
# Retorno: a serie desagregada

desagregacao_parametrica <- function(serie_sintetica,serie_historica){
  
  # Normalizando a serie_historica
  serie_historica_normalizada <- apply(log(serie_historica),2, function(x) (x-mean(x)))
  
  # Normalizando a série_sintetica
  serie_sintetica_normalizada <- apply(log(serie_sintetica), 2, function(x) (x-mean(x)))
  
  # Calculando os parametros historicos
  info <- parametros_historicos(serie_historica_normalizada)
  
  #nAnos: numero de anos da serie_sintetica
  nAnos = nrow(serie_sintetica_normalizada)
  
  serie_desagregada_normalizada = data.frame(matrix(NA, nrow = nAnos, ncol = 12))
  inicio = 1
  fim = nAnos
  
  #Calculo dos Parametros A, Bt e C
  ACF_S = autocovariancia(info)
  
  A = parametro_A(ACF_S)
  #print('Calculo do parametro A')
  
  C = parametro_C(ACF_S,A)
  #print('Calculo do paramtro C')
  
  Bt = parametro_Bt(ACF_S,A,C)
  #print('Calculo do parametro Bt')
  
  #Zinicial = a vazão de dezembro do ano 1 da serie historica normalizada
  Zinicial = serie_historica_normalizada[1,12]
  
  # Desagregando cada ano da serie sintetica normalizada
  #print('Desagregando a serie')
  for(i in inicio:fim){
    
    # Desagregando o ano i da serie_sintetica_normalizada
    serie_desagregada_normalizada[i,] = desagregacao_parametrica_ano(serie_sintetica_normalizada[i,1], Zinicial,A,Bt,C)
    
    # Zinicial é o valor da vazao calculada pela desagregacao do mes de dezembro. Esse valor será utilizado na desagregacao do ano  i + 1
    Zinicial = serie_desagregada_normalizada[i,12]
    
  }
  
  #Desnormalizando a serie desagregada
  media_historica = apply(log(serie_historica),2,mean)
  serie_desagregada = apply(serie_desagregada_normalizada,1,function(x)(x + media_historica))
  serie_desagregada = t(exp(serie_desagregada))
  
  # Fazendo o ajuste proporcional para manter a aditividade
  for(i in seq_along(inicio:fim)){
    serie_desagregada[i,] <- ajuste_proporcional(serie_sintetica[i,],serie_desagregada[i,])
  }
  
  return(serie_desagregada)
}

