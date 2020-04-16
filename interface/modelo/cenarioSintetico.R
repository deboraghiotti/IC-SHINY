serieSintetica = function (parametros, dpRes, lags, n) {
  p = lags[1]
  q = lags[2]
  P = lags[3]
  Q = lags[4]
  
  limInf = 0
  limSup = 0
  if (p > 0) {
    limInf = 1
    limSup = 12*p
    phi = matrix (parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  if (q > 0) {
    limInf = limSup + 1
    limSup = limInf + 12*q - 1
    tht = matrix (parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  if (P > 0) {
    limInf = limSup + 1
    limSup = limInf + 12*P - 1
    PHI = matrix (parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  if (Q > 0) {
    limInf = limSup + 1
    limSup = limInf + 12*Q - 1
    THT = matrix (parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  
  n = n+50
  residuoS = matrix (rnorm (12*n), ncol = 12, nrow = n)
  residuoS = t ((t (residuoS) - (apply (residuoS, 2, mean))) / (apply (residuoS, 2, sd)))
  residuoS = t (t (residuoS) * dpRes)
  residuoS = as.vector (t (residuoS))
  
  serieS = rep (0, 12*n)
  for(t in ((12*(max(P, Q) + 1)) : (12*n))) {
    mes = t %% 12
    if(mes == 0) mes = 12
    
    autoMensal = 0
    autoAnual = 0
    autoMensalAnual = 0
    mmMensal = 0
    mmAnual = 0
    mmMensalAnual = 0
    
    if (p > 0) {
      for(i in 1 : p) {
        tlag = t - i
        autoMensal = autoMensal + phi[i, mes]*serieS[tlag]
        
        if (P > 0) {
          for(k in 1 : P) {
            vtlag = (t - k*12) - i
            autoMensalAnual = autoMensalAnual + phi[i, mes]*PHI[k, mes]*serieS[vtlag]
          }
        }
      }
    }
    if (P > 0) {
      for(j in 1 : P) {
        vlag = t - j*12
        autoAnual = autoAnual + PHI[j, mes]*serieS[vlag]
      }
    }
    if (q > 0) {
      for(l in 1 : q) {
        tlag = t - l
        mmMensal = mmMensal + tht[l, mes]*residuoS[tlag]
        if (Q > 0) {
          for(n in 1 : Q) {
            vtlag = (t - n*12) - l
            mmMensalAnual = mmMensalAnual + tht[l, mes]*THT[n, mes]*residuoS[vtlag]
          }
        }
      }
    }
    
    if (Q > 0) {
      for(m in 1 : Q) {
        vlag = t - m*12
        mmAnual = mmAnual + THT[m, mes]*residuoS[vlag]
      }
    }
    serieS[t] = autoMensal + autoAnual - autoMensalAnual - mmMensal - mmAnual + mmMensalAnual + residuoS[t]
  }
  
  serieS = matrix (serieS, ncol = 12, byrow = T)
  serieS = serieS[-(1:50), ]
  return (serieS)
}

cenarioSintetico = function (serieH, parametros, lags, n) {
  serieHL = log (serieH)
  mediaHL = apply (serieHL, 2, mean)
  dpHL = apply (serieHL, 2, sd)
  serieHN = t ((t (serieHL) - mediaHL) / dpHL)
  
  dpRes = residuos (serieHN, parametros, lags)$dpRes
  serieS = serieSintetica (parametros, dpRes, lags, n)
  serieS = t ((t (serieS) * dpHL) + mediaHL)
  serieS = exp (serieS)
  
  return (serieS)
}