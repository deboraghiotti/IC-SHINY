source('auxiliar.R')
source('tab-desagregacao.R')
source('tab-series-geradas.R')
source('tab-ARMA.R')
source('tab-PMIX.R')
source('tab-estacoes.R')
library(shinyalert)
library(plotly)
library(shinyjs)

navbarPage ("PMIX (p,q,P,Q)",
            TabEstacoes,      
            TabPMIX,
            TabARMA,
            TabDesagregacao,
            TabSerieGeradas
                      
)