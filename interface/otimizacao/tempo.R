NSGA = function (serieH, lags, nP, Pc, Pm, cicloMAX, MAXDiferenca, nS) {
  entrada = entrada (serieH)
  saidasPMIX = PMIXs (entrada$serieHN, lags, nP)
  parametrosIniciais = sapply (saidasPMIX, function (x) x$parametros)
  parametrosIniciais = t (parametrosIniciais)

  populacao = geraPopulacao (entrada, lags, parametrosIniciais, nP, nS)
  populacao = CCO (populacao)
  ciclo = 0
  
  while ((ciclo < cicloMAX) && (MAPEdiferenca (populacao) > MAXDiferenca)) {
    ciclo = ciclo + 1
    novaPopulalacao = geraCruzamento (entrada, lags, populacao, Pc, Pm, nP, nS)
    populacaoTotal = c (populacao, novaPopulalacao)
    populacaoTotal = CCO (populacaoTotal)
    populacao = populacaoTotal[1:nP]
  }
  
  arquivoParametrosIniciais = arquivoParametrosIniciais (saidasPMIX, lags)
  arquivosSeries = arquivosSeries (populacao)
  arquivoParametros = arquivoParametros (populacao, lags)
  arquivoAvaliacoes = arquivoAvaliacoes (populacao)
  
  final = list (ciclos = ciclo,
                arquivoParametrosIniciais = arquivoParametrosIniciais,
                arquivosSeries = arquivosSeries,
                arquivoParametros = arquivoParametros,
                arquivoAvaliacoes = arquivoAvaliacoes)
  return (final)
}

MAPEdiferenca = function (populacao) {
  nP = length (populacao)
  a = round (runif (1, 1, nP))
  individuo = populacao[[a]]$individuo
  
  MAPE = lapply (populacao, function (x)
                                   abs ((individuo - x$individuo) / individuo))
  MAPEdif = sum (unlist (MAPE)) / (nP * (length (individuo)))
  
  if (is.finite (MAPEdif))
    return (MAPEdif)
  else
    return (1)
}

arquivoParametrosIniciais = function (saidasPMIX, lags) {
  p = 1:length (saidasPMIX)
  parametros = t (sapply (p, function (x) c (saidasPMIX[[x]]$ciclos, saidasPMIX[[x]]$somRes, saidasPMIX[[x]]$parametros)))
  parametros = data.frame (parametros)
  colnames (parametros) = c ("Ciclos", "SomRes", nomesLags (lags))
  return (parametros)
}

arquivosSeries = function (populacao) {
  p = 1:length (populacao)
  series = lapply (p, function (x) {
                      serie = data.frame (populacao[[x]]$serie)
                      colnames (serie) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
                      return (serie)
  })
  return (series)
}

arquivoParametros = function (populacao, lags) {
  parametros = t (sapply (populacao, function (x)
                                     x$individuo))
  parametros = data.frame (parametros)
  rownames (parametros) = c (1:length (populacao))
  colnames (parametros) = nomesLags (lags)
  return (parametros)
}

arquivoAvaliacoes = function (populacao) {
  avaliacoes = sapply (populacao, function (x)
                                  x$avaliacao)
  avaliacoes = t (matrix (as.numeric (avaliacoes), ncol = length (populacao)))
  avaliacoes = data.frame (avaliacoes)
  rownames (avaliacoes) = c (1:length (populacao))
  colnames (avaliacoes) = c("MAPEmedia", "MAPEdp", "MAPEfacAnual", "MAPEfacMensal", "SomRes")
  return (avaliacoes)
}

nomesLags = function (lags) {
  meses = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
  phis = NULL
  thts = NULL
  PHIs = NULL
  THTs = NULL
  if (lags[1] > 0)
    phis = paste0 (meses, "/", (paste0 ("phi", 1:lags[1])))
  if (lags[2] > 0)
    thts = paste0 (meses, "/", (paste0 ("tht", 1:lags[2])))
  if (lags[3] > 0)
    PHIs = paste0 (meses, "/", (paste0 ("PHI", 1:lags[3])))
  if (lags[4] > 0)
    THTs = paste0 (meses, "/", (paste0 ("THT", 1:lags[4])))
  
  nomes = c (phis, thts, PHIs, THTs)
  return (nomes)
}
