PMIX = function (serieH, lags) {
  #serie = entrada (dados)$serieHN
  serie = entrada(serieH)$serieHN
  P0 = c (rep (1, 12 * lags[1]), rep (0, 12*lags[2]), rep (1, 12*lags[3]), rep (0, 12*lags[4]))
  parametros = powell (serie, lags, P0)
  
  return (parametros)
}

powell = function (serie, lags, P0) {
  iteracoesMAX = 500
  ordem = sum (lags)
  passo = matrix (0, ncol = (ordem*12), nrow = (ordem*12))
  for (i in 1 : (ordem*12)) passo[i, i] = 1
  
  E0 = residuos (serie, P0, lags)$somRes
  Pi = P0
  Ei = E0
  
  for (ciclos in (1 : iteracoesMAX)) {
    Pdec = 1
    delta = 0
    for (i in (1 : (ordem*12))) {
      Eiant = Ei
      busca = tamanhoPasso (serie, lags, Pi, passo[, i])
      Pi = busca$ponto
      Ei = busca$somRes
      
      if (Eiant - Ei > delta) {
        delta = Eiant - Ei
        Pdec = i
      }
    }
    
    if (E0 - Ei <  10^-5) {
      final = list (parametros = Pi, somRes = Ei, ciclos = ciclos)
      return (final)
    }
    
    Pe = 2*Pi - P0
    Ee = residuos (serie, Pe, lags)$somRes
    if (Ee < E0) {
      if (2*(E0 - 2*Ei + Ee)*((E0 - Ei - delta)^2) < ((E0 - Ee)^2)*delta) {
        S = Pi - P0
        busca = tamanhoPasso (serie, lags, S, Pi)
        Ps = busca$ponto
        Es = busca$somRes
        if (Pdec < ordem*12)
          passo[, (Pdec:((ordem*12)-1))] = passo[, ((Pdec+1):(ordem*12))]
        
        passo[, (ordem*12)] = S
        Pi = Ps
        Ei = Es
      }
    }
    P0 = Pi
    E0 = Ei
  }

  final = list (parametros = Pi, somRes = Ei, ciclos = ciclos)
  return (final)
}

tamanhoPasso = function (serie, lags, ponto, passo) {
  passoMin = -1
  passoMax = 1
  otimizacao = optimize (function (x) residuos (serie, (ponto + x*passo), lags)$somRes,
                         c (passoMin, passoMax), maximum = FALSE, tol = (.Machine$double.eps^0.5))
  
  final = list (ponto = ponto + otimizacao$minimum*passo, somRes = otimizacao$objective)
  return (final)
}

PMIXs = function (serie, lags, n) {
  p = 1:n
  Pinicial = lapply (p, function (x)
                        geraPinicial (lags))
  saidas = lapply (Pinicial, function (x)
                             powell (serie, lags, x))
  
  return (saidas)
}

geraPinicial = function (lags) {
  phi = numeric (0)
  tht = numeric (0)
  PHI = numeric (0)
  THT = numeric (0)
  if (lags[1] > 0)
    phi = (runif (12*lags[1], 0.5, 1.5)) * (sapply ((round (runif (12*lags[1], -1, 0))), function (x) if (x == 0) x = 1 else x = -1))
  if (lags[2] > 0)
    tht = runif (12*lags[2], -0.5, 0.5)
  if (lags[3] > 0)
    PHI = (runif (12*lags[3], 0.5, 1.5)) * (sapply ((round (runif (12*lags[3], -1, 0))), function (x) if (x == 0) x = 1 else x = -1))
  if (lags[4] > 0)
    THT = runif (12*lags[4], -0.5, 0.5)
  
  Pinicial = c (phi, tht, PHI, THT)
  
  return (Pinicial)
}