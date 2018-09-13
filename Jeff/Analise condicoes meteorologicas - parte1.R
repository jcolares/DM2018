# Identificação das condições meteorológicas que causam atrasos em voos em cada aeroporto
# utilizando dados curado fornecidos pelo prof.

# Quais são as combinações de condições meteorológicas que ocorrem com maior frequencia durante os atrasos?
# Baixa pressão + temperatura?

# PARTE 1 - Exploratory Data Analysis

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
library(dplyr)
library(lubridate)

setwd("~/github/DM2018")
load("dados/vra-wu.RData")

source("eogasawara/mylibrary/myGraphics.R")
loadlibrary("gridExtra")

miColores=c("#c7e9c0","#a1d99b","#74c476","#41ab5d","#238b45",
          "#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5",
          "#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d",
          "#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801")

## DATA PREPARATION ###

# Ajustes de tipos de variáveis
vra_wu$depart_expect = as.POSIXct(vra_wu$depart_expect)
vra_wu$depart = as.POSIXct(vra_wu$depart)
vra_wu$arrival_expect = as.POSIXct(vra_wu$arrival_expect)
vra_wu$arrival = as.POSIXct(vra_wu$arrival)
vra_wu$origin = as.character(vra_wu$origin)
vra_wu$flight = as.character(vra_wu$flight)
vra_wu$status = as.character(vra_wu$status)
vra_wu$observation = as.character(vra_wu$observation)

# Apenas os voos entre aeroportos brasileiros
# (O prefixo icao "SB" identifica os aeroportos do Brasil)
vra_wu = vra_wu %>% filter(substr(vra_wu$origin, 1,2)=="SB" & substr(vra_wu$destiny, 1,2)=="SB" )

# Seleção de:
# a) Aeroportos com mais movimento
top20_icao = vra_wu %>% count(origin) %>% top_n(20) %>% arrange(desc(n))
# b) Aeroportos com mais atrasos
top20_icao_atrasos = vra_wu %>% filter(observation %in% c("WO", "XO")) %>% count(origin) %>% top_n(20) %>% arrange(desc(n))
t1 =  vra_wu %>% count(origin) 
t2 =  vra_wu %>% filter(observation %in% c("WO", "XO")) %>% count(origin)
t3 = merge(x=t1, y=t2, by.x = origin, by.y = origin, all.x = TRUE)
# c) Aeroportos com mais atrasos (proporcionalmente à quantidade total de voos)
top20_icao_atrasos_prop = t3 %>% mutate(prop = n.y/n.x) %>% select(origin.x, prop) %>% top_n(20) %>% arrange(desc(prop))
rm(t1, t2, t3)

# Geração do dataframe dadosMet, contendo apenas os as variáveis relevantes para
# as análises, a partir de 2014
dadosMet = vra_wu %>% 
  mutate(dif_temp_dew = depart_temperature - depart_dew_point,
         atrasometorig = ifelse(observation %in% c("WO", "XO"), 1, as.numeric("")) ) %>% 
  select(origin, flight, depart_expect, departure_delay, status, observation, 
         depart_temperature, depart_dew_point, dif_temp_dew, depart_humidity, 
         depart_pressure, depart_visibility, depart_events, depart_conditions,
         atrasometorig) %>% 
  filter(year(depart_expect) >= 2014) 
# Obs: a variavel atrasometorig é um flag que indica se o atraso ou cancelamento foi 
# causado por razões meteorológicas. 
# normativa IAC-1504  
# http://www.anac.gov.br/assuntos/legislacao/legislacao-1/iac-e-is/iac/iac-1504/@@display-file/arquivo_norma/IAC1504.pdf

rm(vra_wu)

#Ajustes nos dados
# Limites para pressão : de 1000 a 1030
dadosMet$depart_pressure = ifelse(dadosMet$depart_pressure >=1030, 1030, dadosMet$depart_pressure)
dadosMet$depart_pressure = ifelse(dadosMet$depart_pressure <=1000, 1000, dadosMet$depart_pressure)

# Limites para visibilidade : Última faixa é "10 ou mais milhas"
dadosMet$depart_visibility = ifelse(dadosMet$depart_visibility >=10, 10, dadosMet$depart_visibility)

#CHECKPOINT
#save(dadosMet, top20_icao, top20_icao_atrasos, top20_icao_atrasos_prop, file = "datasets/dadosMet.Rdata")
#load("datasets/dadosMet.Rdata")


# Análise dos dados meteorológicos
# A) nos aeroportos com maiior movimentação
# B) nos aeroportos com maior quantidade de atrasos
# C) nos aeroportos com maior quantidade proporcional de atrasos

# a) em condições normais
# b) durante os eventos de atraso/acancelamento


### A)

# As análises seguintes contemplam apenas os
# 5 aeroportos mais movimentados

top5 = top20_icao[1:5,1]  

#Gerar os conjunto de dados.  
dadosMet5 = dadosMet %>% filter(is.na(atrasometorig)) %>% filter(origin %in% top5)
dadosMet5Eventos = dadosMet %>% filter(!is.na(atrasometorig)) %>% filter(origin %in% top5)
# a) O dadosMet5 contempla as condições meteorologicas durante voos normais (incl. outros tipos de atraso)
# b) O dataframe dadosMet5Eventos contempla apenas as condições meterorologicas nos horarios
#    em que ocorreram atrasos/cancelamentos. 

#HISTOGRAMAS
#Temperatura
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_temperature)
grfA <- plot.hist(series, colors=miColores[c(1:5)], bin=1, title="Temperatura-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_temperature)
grfB <- plot.hist(series, colors=miColores[c(6:10)], bin=1, title="Temperatura-Atrasos/canc")
#DewPoint
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_dew_point)
grfC <- plot.hist(series, colors=miColores[c(1:5)], bin=1, title="DewPoint-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_dew_point)
grfD <- plot.hist(series, colors=miColores[c(6:10)], bin=1, title="DewPoint-Atrasos/canc")
#Temp-Dew
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$dif_temp_dew)
grfE <- plot.hist(series, colors=miColores[c(1:5)], bin=2, title="Temp-Dew-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$dif_temp_dew)
grfF <- plot.hist(series, colors=miColores[c(6:10)], bin=2, title="Temp-Dew-Atrasos/canc")
#depart_humidity
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_humidity)
grfG <- plot.hist(series, colors=miColores[c(1:5)], bin=4, title="Umidade-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_humidity)
grfH <- plot.hist(series, colors=miColores[c(6:10)], bin=4, title="Umidade-Atrasos/canc")
#depart_pressure
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_pressure)
grfI <- plot.hist(series, colors=miColores[c(1:5)], bin=2, title="Pressão-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_pressure)
grfJ <- plot.hist(series, colors=miColores[c(6:10)], bin=2, title="Pressão-Atrasos/canc")
#depart_visibility
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_visibility)
grfK <- plot.hist(series, colors=miColores[c(1:5)], bin=2, title="Visibilidade-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_visibility)
grfL <- plot.hist(series, colors=miColores[c(6:10)], bin=2, title="Visibilidade-Atrasos/canc")
options(repr.plot.width=7, repr.plot.height=3)
grid.arrange(grfA, grfB, grfC, grfD, grfE, grfF, grfG, grfH, grfI, grfJ, grfK, grfL, ncol=4, top = "5 aeroportos com maior movimento")




### B)

# As análises seguintes contemplam apenas os
# 5 aeroportos com mais atrasos
top5 = top20_icao_atrasos[1:5,1]  

#Gerar os conjunto de dados.  
dadosMet5 = dadosMet %>% filter(is.na(atrasometorig)) %>% filter(origin %in% top5)
dadosMet5Eventos = dadosMet %>% filter(!is.na(atrasometorig)) %>% filter(origin %in% top5)
# a) O dadosMet5 contempla as condições meteorologicas durante voos normais (incl. outros tipos de atraso)
# b) O dataframe dadosMet5Eventos contempla apenas as condições meterorologicas nos horarios
#    em que ocorreram atrasos/cancelamentos. 

#HISTOGRAMAS
#Temperatura
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_temperature)
grfA <- plot.hist(series, colors=miColores[c(1:5)], bin=1, title="Temperatura-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_temperature)
grfB <- plot.hist(series, colors=miColores[c(6:10)], bin=1, title="Temperatura-Atrasos/canc")
#DewPoint
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_dew_point)
grfC <- plot.hist(series, colors=miColores[c(1:5)], bin=1, title="DewPoint-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_dew_point)
grfD <- plot.hist(series, colors=miColores[c(6:10)], bin=1, title="DewPoint-Atrasos/canc")
#Temp-Dew
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$dif_temp_dew)
grfE <- plot.hist(series, colors=miColores[c(1:5)], bin=2, title="Temp-Dew-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$dif_temp_dew)
grfF <- plot.hist(series, colors=miColores[c(6:10)], bin=2, title="Temp-Dew-Atrasos/canc")
#depart_humidity
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_humidity)
grfG <- plot.hist(series, colors=miColores[c(1:5)], bin=4, title="Umidade-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_humidity)
grfH <- plot.hist(series, colors=miColores[c(6:10)], bin=4, title="Umidade-Atrasos/canc")
#depart_pressure
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_pressure)
grfI <- plot.hist(series, colors=miColores[c(1:5)], bin=2, title="Pressão-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_pressure)
grfJ <- plot.hist(series, colors=miColores[c(6:10)], bin=2, title="Pressão-Atrasos/canc")
#depart_visibility
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_visibility)
grfK <- plot.hist(series, colors=miColores[c(1:5)], bin=2, title="Visibilidade-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_visibility)
grfL <- plot.hist(series, colors=miColores[c(6:10)], bin=2, title="Visibilidade-Atrasos/canc")
options(repr.plot.width=7, repr.plot.height=3)
grid.arrange(grfA, grfB, grfC, grfD, grfE, grfF, grfG, grfH, grfI, grfJ, grfK, grfL, ncol=4, top = "5 aeroportos com mais atrasos")



### C)

# As análises seguintes contemplam apenas os
#5 areroportos com mais atrasos, proporcionalmente á qtd total de voos
top5 = top20_icao_atrasos_prop[1:5,1]   

#Gerar os conjunto de dados.  
dadosMet5 = dadosMet %>% filter(is.na(atrasometorig)) %>% filter(origin %in% top5)
dadosMet5Eventos = dadosMet %>% filter(!is.na(atrasometorig)) %>% filter(origin %in% top5)
# a) O dadosMet5 contempla as condições meteorologicas durante voos normais (incl. outros tipos de atraso)
# b) O dataframe dadosMet5Eventos contempla apenas as condições meterorologicas nos horarios
#    em que ocorreram atrasos/cancelamentos. 

#HISTOGRAMAS
#Temperatura
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_temperature)
grfA <- plot.hist(series, colors=miColores[c(1:5)], bin=1, title="Temperatura-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_temperature)
grfB <- plot.hist(series, colors=miColores[c(6:10)], bin=1, title="Temperatura-Atrasos/canc")
#DewPoint
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_dew_point)
grfC <- plot.hist(series, colors=miColores[c(1:5)], bin=1, title="DewPoint-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_dew_point)
grfD <- plot.hist(series, colors=miColores[c(6:10)], bin=1, title="DewPoint-Atrasos/canc")
#Temp-Dew
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$dif_temp_dew)
grfE <- plot.hist(series, colors=miColores[c(1:5)], bin=2, title="Temp-Dew-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$dif_temp_dew)
grfF <- plot.hist(series, colors=miColores[c(6:10)], bin=2, title="Temp-Dew-Atrasos/canc")
#depart_humidity
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_humidity)
grfG <- plot.hist(series, colors=miColores[c(1:5)], bin=4, title="Umidade-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_humidity)
grfH <- plot.hist(series, colors=miColores[c(6:10)], bin=4, title="Umidade-Atrasos/canc")
#depart_pressure
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_pressure)
grfI <- plot.hist(series, colors=miColores[c(1:5)], bin=2, title="Pressão-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_pressure)
grfJ <- plot.hist(series, colors=miColores[c(6:10)], bin=2, title="Pressão-Atrasos/canc")
#depart_visibility
series <- data.frame(variable=dadosMet5$origin, value=dadosMet5$depart_visibility)
grfK <- plot.hist(series, colors=miColores[c(1:5)], bin=2, title="Visibilidade-Todos")
series <- data.frame(variable=dadosMet5Eventos$origin, value=dadosMet5Eventos$depart_visibility)
grfL <- plot.hist(series, colors=miColores[c(6:10)], bin=2, title="Visibilidade-Atrasos/canc")
options(repr.plot.width=7, repr.plot.height=3)
grid.arrange(grfA, grfB, grfC, grfD, grfE, grfF, grfG, grfH, grfI, grfJ, grfK, grfL, ncol=4, top = "5 aeroportos com mais atrasos (proporcionais)")

