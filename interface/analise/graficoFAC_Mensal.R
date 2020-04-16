inicializaGraficoMENSAL = function (serieH, lag) {
  par (lwd = 1, col= 'black')
  plot (NA, main = paste ("Autocorrelacoes mensais - lag", lag), xlim = c (1,12), ylim = c (-1, 1), xlab = "", ylab = "", axes = F, type = "n")
  axis (1, 1:12, c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
  axis (2, -1:1, c (-1, 0, 1))
  par (lty = 1)
  lines (1:12, rep (0, 12))
  intConfianca = 1.96 / sqrt (length (serieH) / 12)
  par (lty = 2)
  lines (1:12, rep (intConfianca, 12))
  lines (1:12, rep (-intConfianca, 12))
  box ( )
}

graficoFACMENSAL = function (serie, lag, cor) {
  fac = autocorrelacaoMensal (serie, lag)[(lag + 1), ]
  par (col = cor, lty = 1)
  points (fac, type = "o", pch = 20)
}