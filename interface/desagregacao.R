####Funcao auxiliar que transforma uma lista num grande dataframe####
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


#####Padronizacao de df############

##Padroniza os dados anuais de uma dataframe com informcoes mensais
padroniza_df<-function(df){
  df_mod = apply(df,1,sum)
  df_mod = as.data.frame(df_mod)
  df_padronizada= apply(log(df_mod), 2, function(x) (x-mean(x))/(sd(x)))
  return(df_padronizada)
}


#Esta funcao pega os dados brutos e retorna uma tabela ano x mes da serie historica
#alem disso plota 2 graficos necessarios na analise
div_mensais<-function(sH)
{ 
  qtd_ano_hist = nrow(sH[,1])/12  #Calcula, baseado no numero de linhas a quantidade de anos registrados no arquivo
  serie_hist = matrix(sH$valor, qtd_ano_hist,byrow = TRUE)#Quebra o dataframe em qtd_anos_hist partes(anos) e cada parte e convertida numa linha da nova tabela
  anos = as.character(sH$periodo)
  anos = substr(anos, nchar(anos)-4+1, nchar(anos))
  anos = unique(anos)
  anos = sort(anos)                                       #Pega a coluna de datas e interpreta quais os anos foram analisados baseados nos ultimos 4 caracteres da informaÃcao da coluna "MES"
  
  row.names(serie_hist)= anos
  colnames(serie_hist)=c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")
  serie_hist=as.data.frame(serie_hist)                    #Nomeia linhas e colunas e converte a matriz em um dataframe
  ### !!! ainda tem que melhorar os graficos, titulo, etc
 
  return(serie_hist)
}

#Pega os dados de entrada de vazao mensal e gera um relatorio de estatisticas com as medias mensais, desvio padrao, assimetria e coeficiente de kurtosis
relatorio_estatistico<-function(tabela_anual)
{
  Medias = apply(tabela_anual,2,mean)
  Desvio_Padrao = apply(tabela_anual,2,sd)
  Assimetria = apply(tabela_anual,2,skewness)
  Indice_Kurt = apply(tabela_anual,2,kurtosis)
  Coef_Var = (Desvio_Padrao/Medias)
  Relatorio = data.frame(Medias,Desvio_Padrao,Assimetria,Indice_Kurt,Coef_Var) 
  return(Relatorio)  
}


#########################Relatorio estatistico das series desagregadas##############################

Estatisticas_Serie_1<-function(serie){
  
  estatisticas = list()
  if(class(serie)=="list"){
    estatisticas = list()
    for(i in 1:length(serie)){
      estatisticas[[i]] = Estatisticas_Serie_1(as.data.frame(serie[[i]]))
    }
  }
  else
  {
    estatisticas$sd = apply(serie,2,sd)
    estatisticas$mean = apply(serie,2,mean)
    estatisticas$skewness = apply(serie,2,skewness)
    estatisticas$kurtosis = apply(serie,2,kurtosis)
    estatisticas$coef_Var = as.vector(apply(serie,2,sd))/as.vector(apply(serie,2,mean))
    estatisticas$hurst = rbind(Mensal = Indice_Hurst_Mensal(serie),Anual = Indice_Hurst(apply(serie,1,sum)))
    
  }
  return(estatisticas)
}

Estatisticas_Serie_2<-function(serie){
  df = lista_df(serie)
  Estatisticas_Serie_1(df)
}


###################
##ERRO MAPE
Erros_Desag_Hist_MAPE_1<-function(serie_sint){
  ERROS = list()
  historica = Estatisticas_Serie_1(SeriesDadosHist)
  sintetica = Estatisticas_Serie_1(serie_sint)
  
  if(class(serie_sint)=="list"){
    for(i in 1:length(serie_sint)){
      ERROS[[i]] = Erros_Desag_Hist_MAPE_1(as.data.frame(serie_sint[[i]]))
    }
  }
  else
  {
    ERROS$sd = (historica$sd - sintetica$sd)/historica$sd
    ERROS$sd = mean(ERROS$sd)
    
    ERROS$mean = (historica$mean - sintetica$mean)/historica$mean
    ERROS$mean = mean(ERROS$mean)*100
    
    ERROS$skewness = (historica$skewness - sintetica$skewness)/historica$skewness
    ERROS$skewness = mean(ERROS$skewness)*100
    
    ERROS$kurtosis =  (historica$kurtosis - sintetica$kurtosis)/historica$kurtosis
    ERROS$kurtosis = mean(ERROS$kurtosis)*100
    
    ERROS$coef_Var = (historica$coef_Var - sintetica$coef_Var)/historica$coef_Var
    ERROS$coef_Var = mean(ERROS$coef_Var)*100
    
    ERROS$hurst =  (historica$hurst - sintetica$hurst)/historica$hurst
  }
  return(ERROS)
}

Corrige_MAPE_1<-function(a){
  correto = list()
  tam = length(a)
  SD=0;MEAN=0;SKEWNESS=0;KURTOSIS=0;COEF_VAR=0;HURST=0;
  
  for(i in 1:tam)
  {
    SD = SD + getElement(a[[i]],"sd")
    MEAN = MEAN + getElement(a[[i]],"mean")
    SKEWNESS = SKEWNESS + getElement(a[[i]],"skewness")
    KURTOSIS = KURTOSIS + getElement(a[[i]],"kurtosis")
    COEF_VAR = COEF_VAR + getElement(a[[i]],"coef_Var")
    HURST = HURST + getElement(a[[i]],"hurst")
  }
  correto$sd = SD/tam
  correto$mean = MEAN/tam
  correto$skewness = SKEWNESS/tam
  correto$kurtosis = KURTOSIS/tam
  correto$coef_var = COEF_VAR/tam
  correto$hurst =  HURST/tam
  
  return(correto)
}
####dados corridos

Erros_Desag_Hist_MAPE_2<-function(serie_sint){
  df = lista_df(serie_sint)
  Erros_Desag_Hist_MAPE_1(df)
}

Indice_Hurst<-function(serie){
  N = length(serie)
  soma_ac<-vector();
  dif_media<-vector();
  media_serie<-mean(serie); 
  sd_serie<-sd(serie); 
  dif_media[1]<-(serie[1]-media_serie[1])^2;
  soma_ac[1]<-0;
  for(i in 2:(N)){
    soma_ac[i]<-soma_ac[i-1]+(serie[i]-media_serie);
    dif_media[i]<-(serie[i]-media_serie)^2;
  }
  #dif_media;
  soma_ac[N]<-0;
  #soma_ac;
  soma_dif_media<-sum(dif_media);
  Range<-max(soma_ac)-min(soma_ac);
  Desvio_D<-(N^(-0.5))*(soma_dif_media^0.5); 
  Range_escalonado<-Range/Desvio_D;
  Hurst<-log(Range_escalonado)/(log((N)/2)); 
  return(Hurst);
}#Para dados anuais, recebe uma coluna com as somas anuais

Indice_Hurst_Mensal<-function(serie){
  vetor = 0
  rownames(serie) = NULL
  colnames(serie) = NULL
  for(i in 1:length(serie[,1])){
    if(i==1){
      vetor = (serie[1,])
    }
    else{
      vetor = cbind(vetor,(serie[i,]))
    }
  }
  vetor = t(vetor)
  return(Indice_Hurst(vetor))
}#Para dados mensais, Entrada e um dataframe ano X mes


#########################Relatorio comparativos de ACF da serie historica x sintetica##############################
#transform_des<-function(Desagreg){
#  aux=matrix(t(as.matrix(Desagreg[,-13])), ncol=1, nrow=nrow(Desagreg)*(ncol(Desagreg)-1), byrow = FALSE)
#  return (aux)
#}

acf_desag<-function(elemento){
  tam = length(elemento)
  
  listaACF=list()
  
  for(i in 1:tam){
    #aux=transform_des(elemento[[i]])
    aux=elemento
    
    Timeserie = ts(aux,start=c(1,1) ,end=c(qtd_ano_hist,12),deltat = 1/12,class="ts")
    Correlacao_Sazonal = peacf(Timeserie,12, plot = F)
    listaACF[[i]]=Correlacao_Sazonal$acf
    
  }
  return(listaACF)
}

acf_desag_med<-function(elemento){
  t = acf_desag(elemento)
  a = data.frame() 
  for(i in 1:length(t))
  {
    if(i==1)
      a=t[[i]]
    else
      a = a+as.data.frame(t[[i]])
  }
  a=a/length(t)
  return(a)
}

##########################Maximos e Minimos############################

#Faz o maximo e o minimo da serie considerando separadamente series sinteticas diferentes
max_min_serie_1<-function(serie){
  maxmin=0
  if(class(serie)=="list")
  {
    maxmin = list()
    for(i in 1:length(serie)){
      maxmin[[i]] = max_min_serie_1(as.data.frame(serie[[i]]))
    }
  }
  else{
    max_ = apply(serie,2,max)
    min_ = apply(serie,2,min)
    maxmin = rbind(max_,min_)
  }
  return(maxmin)
}
#Concatena as series e define min e max global
max_min_serie_2<-function(serie){
  df = lista_df(serie)
  max_min_serie_1(df)
}

#############Desagrega de forma nao parametrica os dados sinteticos utilizando estatisticas e dados historicos
###Metodo proposto por LEE, T., SALAS, J.D., PRAIRIE, J. (2010) DOI: 10.1029/2009WR007761

desagrega_np<-function(serieSint,SeriesDadosHist)
{ 
  qtd_ano_hist = nrow(SeriesDadosHist) 
  qtd_ano_des = length(serieSint[,1])
  
  desagregado_final = data.frame()#Cria um data frame vazio que sera nossa serie anual 
  ##################PRIMEIRA ITERACAO DE DESAGREGACAO##########################
  
  Anuais = data.frame(V1=apply(SeriesDadosHist,1,sum))#Dados as vazoes mensais, calcula as vazoes anuais
  Anuais_1 = rownames(Anuais)[1]
  primeiro_ano = rep(serieSint[1,1],length(Anuais))

  
  delta_i = abs(primeiro_ano-Anuais)#Faz um vetor da diferenca(delta_i) do primeiro ano sintetico referente a vazao anual com todos os anos historicos( |X1-xi| )
  x=delta_i$V1
  Tabela = cbind(Anuais,delta_i$V1)#Faz uma tabela que relaciona ano, vazao anual historica e diferenca(delta_i)
  Tabela = Tabela[order(Tabela$delta_i),]#Ordena de forma crescente de delta_i

  
  ############CALLCULO DE K E DO CWM####################
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
  ################
  
  random = runif(1) #Escolhe numero aleatorio no vetor de pesos cumulativos(cwm) e armazena na variavel 'random'
  
  posicao = which.min(abs(cwm - random)) #Armazena na variavel 'posicao' a posicao do numero escolhido no vetor cwm
  candidato = rownames(Tabela)[posicao] #Armazena o ano candidato a desagregacao na variavel 'candidato'
  
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

#########
correlograma = function(serie, lagMax, grafico) {
  
  mPer = apply(serie, 2, mean)
  dpPer = apply(serie, 2, sd)
  serie = t((t(serie) - mPer))
  serieV = as.vector(t(serie))
  n = length(serie)/12
  intConfianca = 1.96 / sqrt(n)
  fac = matrix(1, nrow = lagMax+1, ncol = 12)
  
  if (lagMax > n) {
    print("Lag maior que a serie!")
    return();
  }
  
  for(lag in 1:lagMax) {
    serieLag = serieV[-(1:lag)] 
    serieAux = serieV[1:((n*12)-lag)]
    product = c(rep(0, lag), serieLag * serieAux)           
    serie = matrix(product, ncol = 12, byrow = T)
    
    for(mes in 1:12) {
      meslag = (mes - lag - 1) %% 12 + 1
      fac[(lag+1), mes] = sum(serie[, mes]) / ((n-1) * dpPer[mes] * dpPer[meslag])
    }
  }
  
  if(grafico) {
    plot(1, type = "n", xlab = "Periodo", xaxt="n", ylab = "Lag", xlim = c(1, 25), ylim = c(0, lagMax))
    axis(side = 1, at=c(2,4,6,8,10,12,14,16,18,20,22,24), 
         labels=c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
    for(mes in 1:12) {
      segments((mes*2), 0, (mes*2), lagMax)
      segments((mes*2) + intConfianca, 0, (mes*2) + intConfianca, lagMax, lty = 2)
      segments((mes*2) - intConfianca, 0, (mes*2) - intConfianca, lagMax, lty = 2)
      for(lag in 0:lagMax) {
        segments((mes*2), lag, ((mes*2) + fac[(lag+1), mes]), lag)
      }
    }
  }
  return(fac)
}

correlogramaAnual = function (serie, lagMax) {
  serieAnual = apply (serie, 1, sum)
  facAnual = acf (serieAnual, lag.max = lagMax, type = c("correlation"), plot = F, na.action = na.pass)
  facAnual = as.numeric (facAnual$acf)
  
  return (facAnual)
}

lagSignificativoAnual = function (serie) {
  intConfianca = 1.96 / sqrt (length (serie) / 12)
  lagAnual = 12
  while (lagAnual < (length (serie) / 12)) {
    facAnual = correlogramaAnual (serie, lagAnual)[-1]
    lagAnualMin = sum (facAnual >= intConfianca)
    if (lagAnualMin == lagAnual)
      lagAnual = lagAnual + 12
    else
      return (lagAnualMin)
  }
}

lagMensalSignificativo = function (serie) {
  intConfianca = 1.96 / sqrt (length (serie) / 12)
  lagMensal = 12
  while (lagMensal < (length (serie))) {
    facMensal = correlograma (serie, lagMensal, F)[-1, ]
    facMensal = facMensal >= intConfianca
    lagMensalMin = apply (facMensal, 2, sum)
    lagMensalMin = min (lagMensalMin)
    if (lagMensalMin == lagMensal)
      lagMensal = lagMensal + 12
    else
      return (lagMensalMin)
  }
}