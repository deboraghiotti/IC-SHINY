inicializaGraficoFACANUAL = function (serieH, lagMax) {
  par (lwd = 1, col= 'black')
  plot (NA, main = "Autocorrelacoes anuais", xlim = c (1, lagMax), ylim = c (-1, 1), xlab = "", ylab = "", axes = F, type = "n")
  axis (1, 1:lagMax, 1:lagMax)
  axis (2, -1:1, c (-1, 0, 1))
  par (lty = 1)
  lines (1:12, rep (0, 12))
  intConfianca = 1.96 / sqrt (length (serieH))
  par (lty = 2)
  lines (1:12, rep (intConfianca, 12))
  lines (1:12, rep (-intConfianca, 12))
  box ( )
}

graficoFACANUAL = function (serie, lagMax, cor) {
  fac = autocorrelacaoAnual (serie, lagMax)[-1]
  par (col = cor, lty = 1)
  points (fac, type = "o", pch = 20)
}