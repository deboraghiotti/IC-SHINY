plotSerie = function (serie) {
  par (lwd = 1, col = 'black')
  plot (NA, main = "Serie Historica", xlim = c (1, 12), ylim = c (0, max (serie)), xlab = "", ylab = "Vazoes mensais (m^3/s)", axes = F, type = "n")
  axis (1, 1:12, c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
  box ( )
  par (col = 'gray70', pch = 16)
  for (ano in (1: (length (serie) / 12))) {
    for (mes in (1:12))
      points (mes, serie[ano, mes])
  }
  media = apply (serie, 2, mean)
  desvio = apply (serie, 2, sd)
  par (lty = 1)
  par (lwd = 2, lty = 1, col= 'cornflowerblue')
  lines (1:12, media)
  par (lty = 2)
  lines (1:12, media + desvio)
  lines (1:12, media - desvio)
}