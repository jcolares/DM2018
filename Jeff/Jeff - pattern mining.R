# ANÁLISE DE ZALGORIT

library(dplyr)
library(arules)
library(data.table)

setwd("~/github/DM2018")

# O dataframe dados_cron contém todos os eventos dos 20 aeroportos com maior
# quantidade de atrasos
load("datasets/dados_cron.RData")

# Carrega minhas funções 
source("funkyFunctions.R")


# Prepraração dos dados para análise

# Aeroportos analisados
lista_icao = unique(dados_cron$icao)
#lista_icao = lista_icao[1]

# Mantém apenas as variáveis relevantes
dados_cron = select(dados_cron, icao, horario, temperatura, dew_point, umidade, vento_direcao, vento_nos, rajada_nos, teto_local, eventos )
dados_cron$flag = ifelse(is.na(dados_cron$eventos), as.numeric(""), 1)
dados_cron$eventos = NULL

# Carrega apenas as n observações anteriores a cada evento, 
# em cada aeroporto, e junta tudo no dataframe DADOS:
dados = dados_cron[0,]
for (este.icao in lista_icao) {
  dados_icao = dados_cron %>% filter(icao == este.icao) 
  dados_icao = get.seq(dados_icao, flag_var = "flag")
  dados = rbind(dados, dados_icao)
  }
rm(dados_icao)

# CHECKPOINT
#save(dados, file="datasets/dados.RData")
#load("datasets/dados.RData")


# Analise dos dados para melhor discretização
# usando biblioteca do Eogasawara

source("eogasawara/mylibrary/myGraphics.R")
loadlibrary("gridExtra")

colores=c("#c7e9c0","#a1d99b","#74c476","#41ab5d","#238b45",
          "#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5",
          "#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d",
          "#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801")


# histogramas

# temperatura - converter em inteiros
seriesA <- data.frame(variable=dados_cron$icao, value=dados_cron$temperatura)
#grfA <- plot.hist(seriesA, colors=colores, bin=1, label_series = "Todos os voos")
grfA <- plot.hist(seriesA, colors=colores, bin=2)
seriesB <- data.frame(variable=dados.d$icao, value=dados$temperatura)
#grfB <- plot.hist(seriesB, colors=colores, bin=1, label_series ="voos atrasados")
grfB <- plot.hist(seriesB, colors=colores, bin=2)
seriesC <- data.frame(variable=dados$icao, value=dados$temperatura) %>% filter(!is.na(dados$flag))
grfC <- plot.hist(seriesC, colors=colores, bin=2)
options(repr.plot.width=6, repr.plot.height=4)
grid.arrange(grfA, grfB, grfC, ncol=3)

# dew_point - 50 bins
seriesA <- data.frame(variable=dados_cron$icao, value=dados_cron$dew_point)
#grfA <- plot.hist(seriesA, colors=colores, bin=, label_series = "Todos os voos")
grfA <- plot.hist(seriesA, colors=colores, bin=2)
seriesB <- data.frame(variable=dados$icao, value=dados$dew_point)
#grfB <- plot.hist(seriesB, colors=colores, bin=1, label_series ="voos atrasados")
grfB <- plot.hist(seriesB, colors=colores, bin=2)
seriesC <- data.frame(variable=dados$icao, value=dados$dew_point) %>% filter(!is.na(dados$flag))
grfC <- plot.hist(seriesC, colors=colores, bin=2)
options(repr.plot.width=6, repr.plot.height=4)
grid.arrange(grfA, grfB, grfC, ncol=3)

# umidade - 20 bins 
seriesA <- data.frame(variable=dados_cron$icao, value=dados_cron$umidade)
#grfA <- plot.hist(seriesA, colors=colores, bin=1, label_series = "Todos os voos")
grfA <- plot.hist(seriesA, colors=colores, bin=5)
seriesB <- data.frame(variable=dados$icao, value=dados$umidade)
#grfB <- plot.hist(seriesB, colors=colores, bin=1, label_series ="voos atrasados")
grfB <- plot.hist(seriesB, colors=colores, bin=5)
seriesC <- data.frame(variable=dados$icao, value=dados$umidade) %>% filter(!is.na(dados$flag))
grfC <- plot.hist(seriesC, colors=colores, bin=5)
options(repr.plot.width=6, repr.plot.height=4)
grid.arrange(grfA, grfB, grfC, ncol=3)

# vento direção - 8 bins
seriesA <- data.frame(variable=dados_cron$icao, value=dados_cron$vento_direcao)
#grfA <- plot.hist(seriesA, colors=colores, bin=1, label_series = "Todos os voos")
grfA <- plot.hist(seriesA, colors=colores, bin=45)
seriesB <- data.frame(variable=dados$icao, value=dados$vento_direcao)
#grfB <- plot.hist(seriesB, colors=colores, bin=1, label_series ="voos atrasados")
grfB <- plot.hist(seriesB, colors=colores, bin=45)
seriesC <- data.frame(variable=dados$icao, value=dados$vento_direcao) %>% filter(!is.na(dados$flag))
grfC <- plot.hist(seriesC, colors=colores, bin=45)
options(repr.plot.width=6, repr.plot.height=4)
grid.arrange(grfA, grfB, grfC, ncol=3)

# vento nos - remover > 60, 30 bins 
seriesA <- data.frame(variable=dados_cron$icao, value=dados_cron$vento_nos)
#grfA <- plot.hist(seriesA, colors=colores, bin=1, label_series = "Todos os voos")
grfA <- plot.hist(seriesA, colors=colores, bin=2)
seriesB <- data.frame(variable=dados$icao, value=dados$vento_nos)
#grfB <- plot.hist(seriesB, colors=colores, bin=1, label_series ="voos atrasados")
grfB <- plot.hist(seriesB, colors=colores, bin=2)
seriesC <- data.frame(variable=dados$icao, value=dados$vento_nos) %>% filter(!is.na(dados$flag))
grfC <- plot.hist(seriesC, colors=colores, bin=2)
options(repr.plot.width=6, repr.plot.height=4)
grid.arrange(grfA, grfB, grfC, ncol=3)

# rajada nos - 36 bins de 5
seriesA <- data.frame(variable=dados_cron$icao, value=dados_cron$rajada_nos)
#grfA <- plot.hist(seriesA, colors=colores, bin=1, label_series = "Todos os voos")
grfA <- plot.hist(seriesA, colors=colores, bin=5)
seriesB <- data.frame(variable=dados$icao, value=dados$rajada_nos)
#grfB <- plot.hist(seriesB, colors=colores, bin=1, label_series ="voos atrasados")
grfB <- plot.hist(seriesB, colors=colores, bin=5)
seriesC <- data.frame(variable=dados$icao, value=dados$rajada_nos) %>% filter(!is.na(dados$flag))
grfC <- plot.hist(seriesC, colors=colores, bin=5)
options(repr.plot.width=6, repr.plot.height=4)
grid.arrange(grfA, grfB, grfC, ncol=3)

# teto local - 20 bins de 500
seriesA <- data.frame(variable=dados_cron$icao, value=dados_cron$teto_local)
#grfA <- plot.hist(seriesA, colors=colores, bin=1, label_series = "Todos os voos")
grfA <- plot.hist(seriesA, colors=colores, bin=500)
seriesB <- data.frame(variable=dados$icao, value=dados$teto_local)
#grfB <- plot.hist(seriesB, colors=colores, bin=1, label_series ="voos atrasados")
grfB <- plot.hist(seriesB, colors=colores, bin=500)
seriesC <- data.frame(variable=dados$icao, value=dados$teto_local) %>% filter(!is.na(dados$flag))
grfC <- plot.hist(seriesC, colors=colores, bin=500)
options(repr.plot.width=6, repr.plot.height=4)
grid.arrange(grfA, grfB, grfC, ncol=3)


# preparar dados para processamento

# DISCCRETIZAÇÂO
dados.d = as.data.frame(NULL)
dados.d = as.data.frame(dados$icao)
colnames(dados.d) = "icao"
dados.d$horario = as.numeric(rownames(dados.d))
dados.d$TID = as.factor(dados$TID) 
dados.d$temperatura = discretize(x = dados$temperatura, method = "fixed", breaks = seq(0,48,2), labels = FALSE)
dados.d$dew_point = discretize(x = dados$dew_point, method = "fixed", breaks = seq(0,48,2), labels = FALSE)
dados.d$umidade = discretize(x = dados$umidade, method = "fixed", breaks = seq(0,100,5), labels = FALSE)
dados.d$vento_direcao = discretize(x = dados$vento_direcao, method = "fixed", breaks = seq(0,359,45), labels = FALSE)
dados.d$vento_nos = discretize(x = dados$vento_nos, method = "fixed", breaks = seq(0,60,2), labels = FALSE)
dados.d$rajada_nos = discretize(x = dados$rajada_nos, method = "fixed", breaks = seq(0,80,2), labels = FALSE)
dados.d$teto_local = discretize(x = dados$teto_local, method = "fixed", breaks = seq(0,10000,500), labels = FALSE)
dados.d$flag = dados$flag

dados_cron.d = as.data.frame(NULL)
dados_cron.d = as.data.frame(dados_cron$icao)
colnames(dados_cron.d) = "icao"
dados_cron.d$horario = as.numeric(rownames(dados_cron.d))
dados_cron.d$TID = as.factor(dados_cron$TID) 
dados_cron.d$temperatura = discretize(x = dados_cron$temperatura, method = "fixed", breaks = seq(0,48,2), labels = FALSE)
dados_cron.d$dew_point = discretize(x = dados_cron$dew_point, method = "fixed", breaks = seq(0,48,2), labels = FALSE)
dados_cron.d$umidade = discretize(x = dados_cron$umidade, method = "fixed", breaks = seq(0,100,5), labels = FALSE)
dados_cron.d$vento_direcao = discretize(x = dados_cron$vento_direcao, method = "fixed", breaks = seq(0,359,45), labels = FALSE)
dados_cron.d$vento_nos = discretize(x = dados_cron$vento_nos, method = "fixed", breaks = seq(0,60,2), labels = FALSE)
dados_cron.d$rajada_nos = discretize(x = dados_cron$rajada_nos, method = "fixed", breaks = seq(0,80,2), labels = FALSE)
dados_cron.d$teto_local = discretize(x = dados_cron$teto_local, method = "fixed", breaks = seq(0,10000,500), labels = FALSE)
dados_cron.d$flag = dados_cron$flag

# CHECKPOINT
#save(dados.d, file="datasets/dados.d.RData")
#save(dados_cron.d, file="datasets/dados_cron.d.RData")
load("datasets/dados.d.RData")
load("datasets/dados_cron.d.RData")

dados.d = dados.d %>% filter(icao=="SBRJ")
dados_cron.d = dados_cron.d %>% filter(icao=="SBRJ")

# histogramas APÓS DISCRETIZACAO

# temperatura - converter em inteiros
seriesA <- data.frame(variable=dados_cron.d$icao, value=dados_cron.d$temperatura)
#grfA <- plot.hist(seriesA, colors=colores, bin=1, label_series = "Todos os voos")
grfA <- plot.hist(seriesA, colors=colores, bin=1)
seriesB <- data.frame(variable=dados.d$icao, value=dados.d$temperatura) # %>% filter(is.na(dados.d$flag))
#grfB <- plot.hist(seriesB, colors=colores, bin=1, label_series ="voos atrasados")
grfB <- plot.hist(seriesB, colors=colores, bin=1)
seriesC <- data.frame(variable=dados.d$icao, value=dados.d$temperatura) %>% filter(!is.na(dados.d$flag))
grfC <- plot.hist(seriesC, colors=colores, bin=1)
options(repr.plot.width=6, repr.plot.height=4)
grid.arrange(grfA, grfB, grfC, ncol=3)

# dew_point - 50 bins
seriesA <- data.frame(variable=dados_cron.d$icao, value=dados_cron.d$dew_point)
#grfA <- plot.hist(seriesA, colors=colores, bin=, label_series = "Todos os voos")
grfA <- plot.hist(seriesA, colors=colores, bin=1)
seriesB <- data.frame(variable=dados.d$icao, value=as.numeric(dados.d$dew_point)) #%>% filter(is.na(dados.d$flag))
#grfB <- plot.hist(seriesB, colors=colores, bin=1, label_series ="voos atrasados")
grfB <- plot.hist(seriesB, colors=colores, bin=1)
seriesC <- data.frame(variable=dados.d$icao, value=as.numeric(dados.d$dew_point)) %>% filter(!is.na(dados.d$flag))
grfC <- plot.hist(seriesC, colors=colores, bin=1)
options(repr.plot.width=6, repr.plot.height=4)
grid.arrange(grfA, grfB, grfC, ncol=3)

# umidade - 20 bins 
seriesA <- data.frame(variable=dados_cron.d$icao, value=dados_cron.d$umidade)
#grfA <- plot.hist(seriesA, colors=colores, bin=1, label_series = "Todos os voos")
grfA <- plot.hist(seriesA, colors=colores, bin=1)
seriesB <- data.frame(variable=dados.d$icao, value=dados.d$umidade)
#grfB <- plot.hist(seriesB, colors=colores, bin=1, label_series ="voos atrasados")
grfB <- plot.hist(seriesB, colors=colores, bin=1)
seriesC <- data.frame(variable=dados.d$icao, value=dados.d$umidade) %>% filter(!is.na(dados.d$flag))
grfC <- plot.hist(seriesC, colors=colores, bin=1)
options(repr.plot.width=6, repr.plot.height=4)
grid.arrange(grfA, grfB, grfC, ncol=3)

# vento direção - 8 bins
seriesA <- data.frame(variable=dados_cron.d$icao, value=dados_cron.d$vento_direcao)
#grfA <- plot.hist(seriesA, colors=colores, bin=1, label_series = "Todos os voos")
grfA <- plot.hist(seriesA, colors=colores, bin=1)
seriesB <- data.frame(variable=dados.d$icao, value=dados.d$vento_direcao)
#grfB <- plot.hist(seriesB, colors=colores, bin=1, label_series ="voos atrasados")
grfB <- plot.hist(seriesB, colors=colores, bin=1)
seriesC <- data.frame(variable=dados.d$icao, value=dados.d$vento_direcao) %>% filter(!is.na(dados.d$flag))
grfC <- plot.hist(seriesC, colors=colores, bin=1)
options(repr.plot.width=6, repr.plot.height=4)
grid.arrange(grfA, grfB, grfC, ncol=3)

# vento nos - remover > 60, 30 bins 
seriesA <- data.frame(variable=dados_cron.d$icao, value=dados_cron.d$vento_nos)
#grfA <- plot.hist(seriesA, colors=colores, bin=1, label_series = "Todos os voos")
grfA <- plot.hist(seriesA, colors=colores, bin=1)
seriesB <- data.frame(variable=dados.d$icao, value=dados.d$vento_nos)# %>% filter(is.na(dados.d$flag))
#grfB <- plot.hist(seriesB, colors=colores, bin=1, label_series ="voos atrasados")
grfB <- plot.hist(seriesB, colors=colores, bin=1)
seriesC <- data.frame(variable=dados.d$icao, value=dados.d$vento_nos) %>% filter(!is.na(dados.d$flag))
grfC <- plot.hist(seriesC, colors=colores, bin=1)
options(repr.plot.width=6, repr.plot.height=4)
grid.arrange(grfA, grfB, grfC, ncol=3)

# rajada nos - 36 bins de 5
seriesA <- data.frame(variable=dados_cron.d$icao, value=dados_cron.d$rajada_nos)
#grfA <- plot.hist(seriesA, colors=colores, bin=1, label_series = "Todos os voos")
grfA <- plot.hist(seriesA, colors=colores, bin=1)
seriesB <- data.frame(variable=dados.d$icao, value=dados.d$rajada_nos)
#grfB <- plot.hist(seriesB, colors=colores, bin=1, label_series ="voos atrasados")
grfB <- plot.hist(seriesB, colors=colores, bin=1)
seriesC <- data.frame(variable=dados.d$icao, value=dados.d$rajada_nos) %>% filter(!is.na(dados.d$flag))
grfC <- plot.hist(seriesC, colors=colores, bin=1)
options(repr.plot.width=6, repr.plot.height=4)
grid.arrange(grfA, grfB, grfC, ncol=3)

# teto local - 20 bins de 500
seriesA <- data.frame(variable=dados_cron.d$icao, value=dados_cron.d$teto_local)
#grfA <- plot.hist(seriesA, colors=colores, bin=1, label_series = "Todos os voos")
grfA <- plot.hist(seriesA, colors=colores, bin=1)
seriesB <- data.frame(variable=dados.d$icao, value=dados.d$teto_local)
#grfB <- plot.hist(seriesB, colors=colores, bin=1, label_series ="voos atrasados")
grfB <- plot.hist(seriesB, colors=colores, bin=1)
seriesC <- data.frame(variable=dados.d$icao, value=dados.d$teto_local) %>% filter(!is.na(dados.d$flag))
grfC <- plot.hist(seriesC, colors=colores, bin=1)
options(repr.plot.width=6, repr.plot.height=4)
grid.arrange(grfA, grfB, grfC, ncol=3)



series <- data.frame(variable="Temperatura", value=dados_cron$temperatura)
grfC <- plot.density(series, colors=colores[1])
series <- data.frame(variable=dados_cron$icao, value=dados_cron$temperatura)
grfD <- plot.density(series, colors=colores[c(1:20)])
series <- data.frame(variable="Temperatura Atrasos", value=dados$temperatura)
grfA <- plot.density(series, colors=colores[1])
series <- data.frame(variable=dados$icao, value=dados$temperatura)
grfB <- plot.density(series, colors=colores[c(1:20)])
options(repr.plot.width=7, repr.plot.height=3)
grid.arrange(grfC, grfD, grfA, grfB, ncol=2)



series <- data.frame(variable="Umidade", value=dados_cron$umidade)
series <- arrange(series, series$value)
grfC <- plot.density(series, colors=colores[1])
series <- data.frame(variable=dados_cron$icao, value=dados_cron$umidade)
grfD <- plot.density(series, colors=colores[c(1:20)])
series <- data.frame(variable="Umidade Atrasos", value=dados$umidade)
grfA <- plot.density(series, colors=colores[1])
series <- data.frame(variable=dados$icao, value=dados$umidade)
grfB <- plot.density(series, colors=colores[c(1:20)])
options(repr.plot.width=7, repr.plot.height=3)
grid.arrange(grfC, grfD, grfA, grfB, ncol=2)


options(repr.plot.width=4, repr.plot.height=3)
qqplot(dados$temperatura, dados$dew_point)
fm <- lm(dados$temperatura ~ dados$dew_point)
abline(coef(fm), lty=4)

options(repr.plot.width=4, repr.plot.height=3)
qqplot(dados$temperatura, dados$umidade)
fm <- lm(dados$temperatura ~ dados$umidade)
abline(coef(fm), lty=4)

options(repr.plot.width=4, repr.plot.height=3)
qqplot(dados$dew_point, dados$umidade)
fm <- lm(dados$dew_point ~ dados$umidade)
abline(coef(fm), lty=4)

options(repr.plot.width=4, repr.plot.height=3)
qqplot(dados$vento_direcao, dados$vento_nos)
fm <- lm(dados$vento_direcao ~ dados$vento_nos)
abline(coef(fm), lty=4)

# Correlações entre as variáveis em condições normais (very slow)
dad = dados_cron %>% select(temperatura, dew_point, umidade, vento_direcao, vento_nos, rajada_nos, teto_local)
options(repr.plot.width=6, repr.plot.height=6)
pairs(~.,data=dad, main="Scatterplot Matrix")

# Correlações entre as variáveis em horarios anteriores aos eventos
dad = dados %>% select(temperatura, dew_point, umidade, vento_direcao, vento_nos, rajada_nos, teto_local)
options(repr.plot.width=6, repr.plot.height=6)
pairs(~.,data=dad, main="Scatterplot Matrix")

# Correlações entre as variáveis na hora dos eventos
dad = dados %>% select(temperatura, dew_point, umidade, vento_direcao, vento_nos, rajada_nos, teto_local) %>% filter(dados$flag==1)
options(repr.plot.width=6, repr.plot.height=6)
pairs(~.,data=dad, main="Scatterplot Matrix")



# Correlações entre as variáveis na hora dos eventos
dad = dados.d %>% select(temperatura, dew_point, umidade, vento_direcao, vento_nos, rajada_nos, teto_local) %>% filter(dados$flag==1)
options(repr.plot.width=6, repr.plot.height=6)
pairs(~.,data=dad, main="Scatterplot Matrix")

dadrj = dados.d  %>% filter(icao=="SBRJ")  %>% filter(flag==1) %>% select(temperatura, dew_point, umidade, vento_direcao, vento_nos, rajada_nos, teto_local) 
options(repr.plot.width=6, repr.plot.height=6)
pairs(~.,data=dadrj, main="Scatterplot Matrix")


#Analise temperatura por aerooporto



