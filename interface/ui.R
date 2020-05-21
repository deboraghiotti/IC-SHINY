source('auxiliar.R')
source('tabs/tab-desagregacao.R')
source('tabs/tab-series-geradas.R')
source('tabs/tab-ARMA.R')
source('tabs/tab-PMIX.R')
source('tabs/tab-estacoes.R')
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