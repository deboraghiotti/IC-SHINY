autocorrelacaoMensal = function (serie, lagMax) {
  n = nrow (serie)
  
  if (lagMax > n) {
    lagMax = n-1
  }
  
  mPer = apply (serie, 2, mean)
  dpPer = apply (serie, 2, sd)
  serieS = t ((t (serie) - mPer))
  serieS = as.vector (t (serieS))

  fac = sapply (0 : lagMax, function (lag) {
                if (lag == 0)
                  product = serieS * serieS
                else
                  product = c (rep (0, lag), (serieS[-(1:lag)]) * (serieS[1:((length (serieS)) - lag)]))
                serieAux = matrix (product, ncol = ncol (serie), byrow = T)
                
                sapply (1 : (ncol (serie)), function (mes) {
                        meslag = (mes - lag - 1) %% 12 + 1
                        return (sum (serieAux[, mes]) / ((n - 1) * dpPer[mes] * dpPer[meslag]))
                })
      })
  
  fac = t (fac)
  return (fac)
}

autocorrelacaoAnual = function (serieAnual, lagMax) {
  facAnual = acf (serieAnual, lag.max = lagMax, type = c ("correlation"), plot = F, na.action = na.pass)
  facAnual = as.numeric (facAnual$acf)
  return (facAnual)
}

lagAnualSignificativo = function (serie) {
  intConfianca = 1.96 / (sqrt (length (serie)))
  lagAnual = 12
  while (lagAnual < (length (serie))) {
    facAnual = autocorrelacaoAnual (serie, lagAnual)[-1]
    lagAnualMin = sum (facAnual >= intConfianca)
    if (lagAnualMin == lagAnual)
      lagAnual = lagAnual + 12
    else
      return (max ((lagAnualMin - 1), 1))
  }
}

lagMensalSignificativo = function (serie) {
  intConfianca = 1.96 / (sqrt (nrow (serie)))
  lagMensal = 12
  while (lagMensal < (length (serie))) {
    facMensal = autocorrelacaoMensal (serie, lagMensal)[-1, ]
    facMensal = facMensal >= intConfianca
    lagMensalMin = apply (facMensal, 2, sum)
    lagMensalMin = min (lagMensalMin)
    if (lagMensalMin == lagMensal)
      lagMensal = lagMensal + 12
    else
      return (max ((lagMensalMin - 1), 1))
  }
}
