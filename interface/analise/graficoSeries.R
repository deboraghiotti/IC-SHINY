inicializaGraficoSERIE = function (serieH) {
  media = apply (serieH, 2, mean)
  desvio = apply (serieH, 2, sd)
  par (lwd = 1, col= 'black')
  plot (NA, main = "Medias e Desvios", xlim = c (1,12), ylim = c (0, 1.2 * (max (media + desvio))),
        xlab = "", ylab = "Vazoes medias mensais (m^3/s)", axes = F, type = "n")
  axis (1, 1:12, c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
  box ( )
}

graficoSERIE = function (serie, cor) {
  media = apply (serie, 2, mean)
  desvio = apply (serie, 2, sd)
  par (lty = 1)
  par (lwd = 2, lty = 1, col= cor)
  lines (1:12, media)
  par (lty = 2)
  lines (1:12, media + desvio)
  lines (1:12, media - desvio)
}
