# ANÁLISE EXPLORATÓRIA DO NOVO DATASET WU 

setwd("~/github/DM2018")

#bibliotecas
library(dplyr)
library(DataExplorer)
library(stats)
library(ggplot2)

# Coleta dados dos voos/aeroportos
load("datasets/vra.RData")
ranking_mov = count(vra, icao_origem) %>% arrange(desc(n))
ranking_atrasos_met = vra %>% filter(atrasometorig==1) %>% count(icao_origem) %>% arrange(desc(n))
atrasos = merge(x = ranking_mov, y = ranking_atrasos_met , by.x = "icao_origem", by.y = "icao_origem", all.x = TRUE) 
atrasos$proporcional = atrasos$n.y / atrasos$n.x
colnames(atrasos) = c("icao", "voos", "atrasos", "prop")
top20 = top_n(atrasos, 20, atrasos)
eventos = vra %>% filter(atrasometorig==1) %>% filter(icao_origem %in% top20$icao) %>% select(icao_origem, partida_prevista, situacao_voo, atraso_partida) 
rm(ranking_atrasos_met, ranking_mov, vra)

load("datasets/wu.RData")

# Análise superficial
introduce(wu)
head(wu)

# Dados incompletos (missing data)
plot_missing(wu)

# Abrangência 
substr(wu$data.date, 1, 4) %>% unique()
unique(wu$data.airport)

# Distribuição
plot_histogram(wu)

# Aplica filtro apenas aeroportos brasileiros
wu = filter(wu, substr(data.airport,1,2)=="SB")
unique(wu$data.airport)
plot_histogram(wu)

# Temperatura
# A maior temperatura registrada no brasil é 46 graus. Os valores acima disso provavelmente representam erros de medição.

# Dew point
# Está na mesma escala da temperatura e tem o mesmo problema.

# A umidade máxima é 100%. Valores maiores provavelmente representam erros.
# Umidade

# Pressão atmosférica
# em torno de 1000 mbar ao nivel do mar e com variação de 870 a 1084. Valores fora dessa faixa posivelmente são erros.
# https://www.thoughtco.com/low-and-high-pressure-1434434

# Visibilidade
# Medida em milhas, na faixa de 0-20. Fora dessa faixa, podem ser erros ou registros em outras escalas.

rm(wu)

# ANALISE EXPLORATORIA DO DATASET WD
#####################################

setwd("~/github/DM2018")
load("datasets/wd.RData")

# Análise superficial
introduce(wd)
head(wd)
# source("datasets/Jeff - Script de tratamento do dataset wd.R")

# Aeroprtos
unique(wd$icao)
# Há muitos aeroportos que não estão no vra. Remover.

# Remove colunas com mesmo significado ou não relevantes
vars = c("icao", "horario", "temperatura", "dew_point", "umidade", "vento_direcao", 
         "vento_nos", "alti", "pressao", "precipitacao_mm", "visibilidade", "rajada_nos", 
         "nebulosidade_local", "teto_local")
wd = select(wd, vars)

#plot_histogram(wd)

# calcular limites distribuição normal e remover os outliers

# 35 observacoes com dew_point > 60. Remover
# 947 observações com umidade > 100. Remover.
# 47 obs com vento_nos > 216. Remover (https://weather.com/storms/severe/news/2018-04-10-most-extreme-winds-earth-surface)
# 5 obs de teto_local acima de 30000 pes. Remover.
# teto acima de 10000 transforma para 10000. Não faz diferenca para a analise

wd$dew_point = ifelse(wd$dew_point > 60, as.numeric(""), wd$dew_point)
wd$umidade = ifelse(wd$umidade > 100, as.numeric(""), wd$umidade)
wd$vento_nos = ifelse(wd$vento_nos > 216, as.numeric(""), wd$vento_nos)
wd$teto_local = ifelse(wd$teto_local > 30000, as.numeric(""), wd$teto_local)
wd$teto_local = ifelse(wd$teto_local > 10000, 10000, wd$teto_local)

tempdata = select (wd, "temperatura", "dew_point", "umidade", "vento_direcao", 
                   "vento_nos", "visibilidade", "rajada_nos", 
                    "teto_local" )

plot_histogram(tempdata)

# analises sobre os top20 aeroportos
#####################################
wd20 = wd %>% filter(icao %in% top20$icao) 
rm(wd)

# ou: read("datasets/wd20.Rdata")

tempdata = select (wd20, "temperatura", "dew_point", "umidade", "vento_direcao", 
                   "vento_nos", "visibilidade", "rajada_nos", 
                   "teto_local" )

plot_histogram(tempdata)

plot_bar(wd20)

View(filter(wd20a, Group.1 =="SBGL"))

# Alguns aeroportos apresentam mais de uma medição meteorológica na mesma hora. 
#wd20$horario_ajustado = round.POSIXt(wd20$horario, units = "hours")
#wd20$horario_ajustado = as.character(wd20$horario_ajustado)

# Aplicar as médias quando houver mais de uma observação na mesma hora
#wd20a = select(wd20, "icao", "horario_ajustado", "temperatura", "dew_point", "umidade", "vento_direcao", 
#               "vento_nos", "alti", "pressao", "precipitacao_mm", "visibilidade", "rajada_nos", 
#               "teto_local")
#wd20a = aggregate(wd20a[, 3:13], list(wd20$icao, wd20$horario_ajustado), mean, na.rm=TRUE)
#colnames(wd20a)[1] = "icao"
#colnames(wd20a)[2] = "horario"
#wd20a$horario = as.POSIXct(wd20a$horario)

# Tentativa 2:
# Alguns aeroportos apresentam mais de uma medição meteorológica na mesma hora. 
wd20$horario_ajustado = round.POSIXt(wd20$horario, units = "hours")
wd20$horario_ajustado = as.character(wd20$horario_ajustado)
wd20$horario_char= as.character(wd20$horario_ajustado)
wd20$horario_ajustado = NULL


# agrega os valoes de cada aeroporto, uma observação para cada hora.
wd20a = wd20 %>% 
  group_by(icao, horario_char) %>%
  summarise(temperatura = mean(temperatura, na.rm = TRUE), 
            dew_point = mean(dew_point, na.rm = TRUE),
            umidade = mean(umidade, na.rm = TRUE), 
            vento_direcao = mean(vento_direcao, na.rm = TRUE),
            vento_nos = mean(vento_nos, na.rm = TRUE), 
            alti = mean(alti, na.rm = TRUE), 
            pressao = mean(pressao, na.rm = TRUE), 
            precipitacao_mm = mean(precipitacao_mm, na.rm = TRUE), 
            visibilidade = mean(visibilidade, na.rm = TRUE), 
            rajada_nos = mean(rajada_nos, na.rm = TRUE),
            nebulosidade_local = paste(nebulosidade_local, collapse = " " ), 
            teto_local = mean(teto_local, na.rm = TRUE))

rm(wd20)

eventos$horario_ajustado = round.POSIXt(eventos$partida_prevista, units = "hours")
eventos$horario_char = as.character(eventos$horario_ajustado)
eventos$horario_ajustado = NULL
  
eventosa = eventos %>%
  group_by(icao_origem, horario_char) %>%
  summarise(atraso_medio = mean(atraso_partida, na.rm = TRUE),
            eventos = n(),
            cancelamentos = sum(is.na(atraso_partida)))

dados_cron = merge(x= wd20a, y=eventosa, by.x = c("icao", "horario_char"), by.y = c("icao_origem", "horario_char"), all = TRUE)

dados_cron$horario = as.POSIXct(dados_cron$horario_char, tz="GMT")

# TESTE
tempdata = dados_cron %>% filter(icao=="SBRJ")
n_occur <- data.frame(table(tempdata$horario))
n_occur[n_occur$Freq >1, ]

dados_cron$horario_char = NULL
dados_cron$horario_ajustado = NULL

save(dados_cron, file="datasets/dados_cron.RData")  

rm(eventosa, n_occur, tempdata, wd20a, atrasos)

#load("datasets/dados_cron.RData")

