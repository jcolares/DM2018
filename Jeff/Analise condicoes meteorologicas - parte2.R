# Identificação das condições meteorológicas que causam atrasos em voos em cada aeroporto
# utilizando dados curado fornecidos pelo prof.

# Quais são as combinações de condições meteorológicas que ocorrem com maior frequencia durante os atrasos?
# Baixa pressão + temperatura?

# PARTE 2 - Discretização e preparação de dados para o ARULES


library(dplyr)
library(lubridate)

setwd("~/github/DM2018")
load("datasets/dadosMet.Rdata")

source("eogasawara/mylibrary/myGraphics.R")
source("eogasawara/mylibrary/myPreprocessing.R")
loadlibrary("gridExtra")

miColores=c("#c7e9c0","#a1d99b","#74c476","#41ab5d","#238b45",
            "#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5",
            "#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d",
            "#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801")





dadosMet %>% select(origin,depart_expect, depart_temperature, dif_temp_dew, depart_humidit, depart_pressure, depart_visibility, depart_conditions, depart_events) 


# CHECKPOINT
#save(dadosMet, top20_icao, top20_icao_atrasos, top20_icao_atrasos_prop, top5, file = "datasets/dadosMet.Rdata")
#load("datasets/dadosMet.Rdata")


dadosMet5d = as.data.frame(dadosMet5$origin)[,1]

#Temperatura
n = seq(0,40,2)
dadosMet5d$temperatura = as.data.frame( paste("T",.bincode(dadosMet5$depart_temperature, breaks = n)))[,1]
#colnames(dadosMet5d)[1] = "Temperatura"
#Dif.Temp-Dew
n = seq(0,40,20)


#Temperatura
series <- data.frame(variable=dadosMet5d$origin, value=dadosMet5d$depart_temperature)
grfA <- plot.hist(series, colors=miColores[c(1:5)], bin=1, title="Todas as observações")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_temperature)
grfB <- plot.hist(series, colors=miColores[c(6:10)], bin=1, title="Durante atrasos/cancelamentos")
options(repr.plot.width=7, repr.plot.height=1)
grid.arrange(grfA, grfB, ncol=2, top = "Temperatura")

