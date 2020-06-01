cenarioSinteticoAnual = function (serieH, lags, n) {
  
  serieAnualH = apply (serieH, 1, sum)
  serieHL = log (serieAnualH)
  mediaHL = mean (serieHL)
  dpHL = sd (serieHL)
  serieHN = (serieHL - mediaHL) / dpHL
  
  modelo = ARMA (serieHN, lags)
  parametros = modelo$parametros
  residuos = modelo$residuos
  dpRes =  modelo$dpRes
  c = modelo$constante
  
  serieS = serieSinteticaAnual (parametros, dpRes, c, lags, n)
  serieS = (serieS * dpHL) + mediaHL
  serieS = exp (serieS)
  
  final = list (serieSintetica = serieS, parametros = parametros, residuos = residuos)
  return (final)
}

ARMA = function (serieAnual, lags) {
  modelo = arima0 (ts (serieAnual), order = c (lags[1], 0, lags[2]), seasonal = list (order = c (0, 0, 0), period = NA),
                  xreg = NULL, include.mean = TRUE, delta = 0.01, transform.pars = TRUE, fixed = NULL, init = NULL, method = "CSS")
  parametros = as.vector (modelo$coef)
  constante = parametros[length(parametros)]
  parametros = parametros[-length(parametros)]
  dpRes = sqrt (modelo$sigma2)
  residual = modelo$residuals
  
  final = list (parametros = parametros, dpRes = dpRes, constante = constante, residuos = residual)
  return (final)
}

serieSinteticaAnual = function (parametros, dpRes, c, lags, n) {
  p = lags[1]
  q = lags[2]
  
  limInf = 0
  limSup = 0
  if (p > 0) {
    limInf = 1
    limSup = p
    phi = parametros[limInf : limSup]
  }
  if (q > 0) {
    limInf = limSup + 1
    limSup = limInf + q - 1
    tht = parametros[limInf : limSup]
  }
  
  residuoS = rnorm (n)
  residuoS = (residuoS - mean (residuoS)) / sd (residuoS)
  residuoS = residuoS * dpRes
  
  serieS = rep (0, n)
  for (v in ((max (p, q) + 1):n)) {
    auto = 0
    mm = 0
    if (p > 0) {
      for (i in (1:p))
        auto = auto + phi[i]*serieS[v-i]
    }
    if (q > 0) {
      for (j in (1:q))
        mm = mm + tht[j]*residuoS[v-j]
    }
    serieS[v] = c + auto + mm + residuoS[v]
  }
  
  return (serieS)
}