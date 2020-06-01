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

