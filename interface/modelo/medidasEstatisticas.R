Hurst = function (serie) {
  n = length (serie)
  media = mean (serie)
  serieNormalizada = serie - media
  somaAcumulada = numeric (n)
  for (i in 2:(n+1)) {
    somaAcumulada[i] = somaAcumulada[i-1] + serieNormalizada[i-1]
  }

  R = max (somaAcumulada) - min (somaAcumulada)
  S =  sd (serie)
  Hurst = log (R/S) / (log((n)/2))
  
  return (Hurst)
}