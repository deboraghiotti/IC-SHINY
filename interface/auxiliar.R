#Funcoes auxiliares

fieldsMandatory <- c("nomeEstacao", "codigoEstacao","fileSH")
camposConsultaEstacao <- c("codigo","nome")
camposConsultaSS <- c("nomeResultados","codigoResultados")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-  ".mandatory_star { color: red; }
            #error { color: red; }"

findID <-function(table,nome){
  for(i in 1:nrow(table)){
    if(table$nome[i] == nome){
      id = table$idESTACAO[i]
    }
  }
  id
}

valorSH <- function(codigoEstacao, nomeEstacao){
  serieHistorica = buscarSH(codigoEstacao,nomeEstacao)
  serieHistorica = serieHistorica[,2]
  serieHistorica$valor <- as.numeric(serieHistorica$valor)
  serieFinal = matrix(serieHistorica$valor,ncol=12,byrow=T)
  #print(serieFinal)
  return(serieFinal)
}

findCodigo <- function(table,nome){
  for(i in 1:nrow(table)){
    if(table$nome[i] == nome){
      return(table$codigo[i])
    }
  }
}