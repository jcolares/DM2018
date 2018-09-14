# IdentificaÃ§Ã£o das condiÃ§Ãµes meteorolÃ³gicas que causam atrasos em voos em cada aeroporto
# utilizando dados curado fornecidos pelo prof.

# Quais sÃ£o as combinaÃ§Ãµes de condiÃ§Ãµes meteorolÃ³gicas que ocorrem com maior frequencia durante os atrasos?
# Baixa pressÃ£o + temperatura?

# PARTE 2 - PreparaÃ§Ã£o de dados e Data Mining


library(dplyr)
library(lubridate)
#library(zoo)
library(arulesSequences)
library(arulesViz)

# Linux:
# setwd("~/github/DM2018")
# Windows:
setwd("C:/Users/Jefferson/Documents/GitHub/DM2018")


# Carrega minhas funÃ§Ãµes 
source("Jeff/funkyFunctions.R")

load("datasets/dadosMet.Rdata")

icaoSelect = c("SBJV")

# Gera o dataset para o ICAO informado

dadosMetSBRJ = dadosMet %>% 
  filter(origin==icaoSelect) %>%
  mutate(timekey = as.numeric(paste(substr(depart_expect,1,4), 
                                    substr(depart_expect,6,7), 
                                    substr(depart_expect,9,10), 
                                    substr(depart_expect,12,13), 
                                    "00", sep = "")),
         cancels = ifelse(observation =="XO", 1, 0),
         delays = ifelse(observation == "WO", 1, 0)) %>%
  mutate(events = ifelse(cancels + delays > 0 , 1, 0)) %>%
  mutate(delay_minutes = ifelse(delays == 1, departure_delay, 0)) %>%
  group_by(timekey) %>%
  summarise(temperature = mean(depart_temperature, na.rm = TRUE),
            dif_temp_dew = mean(dif_temp_dew, na.rm = TRUE),
            humidity = mean(depart_humidity, na.rm = TRUE),
            pressure = mean(depart_pressure, na.rm = TRUE),
            visibility = mean(depart_visibility, na.rm = TRUE),
            avg_delay = mean(delay_minutes, na.rm = TRUE),
            delays = sum(delays),
            cancels = sum(cancels))

# Flag Eventos (atrasos ou cancelamentos)
dadosMetSBRJ$flag = ifelse(dadosMetSBRJ$delays + dadosMetSBRJ$cancels > 0, 1, as.numeric(""))

# Identifica sequencias de eventos
dadosMetSBRJ = dadosMetSBRJ %>%
  group_by(flag) %>%
  mutate(eventID=cumsum(flag)) 

# DiscretizaÃ§Ã£o
dadosMetSBRJ$temperatureD = ifelse(is.na(dadosMetSBRJ$temperature), "", paste("T",.bincode(dadosMetSBRJ$temperature, breaks = seq(0,40,2)), sep = ""))
dadosMetSBRJ$dif_temp_dewD =  ifelse(is.na(dadosMetSBRJ$dif_temp_dew), 
                                     "",  
                                     ifelse(dadosMetSBRJ$dif_temp_dew==0, 
                                            "D0",  
                                            paste("D",.bincode(dadosMetSBRJ$dif_temp_dew, breaks = seq(0,40,2)), sep = "")))
dadosMetSBRJ$humidityD = ifelse(is.na(dadosMetSBRJ$humidity), "", paste("H",.bincode(dadosMetSBRJ$humidity, breaks = seq(0,100,4)), sep = ""))
dadosMetSBRJ$pressureD = ifelse(is.na(dadosMetSBRJ$pressure), "", paste("P",.bincode(dadosMetSBRJ$pressure, breaks = seq(1000,1030,2)), sep = ""))
dadosMetSBRJ$visibilityD = ifelse(is.na(dadosMetSBRJ$visibility), "", paste("V",.bincode(dadosMetSBRJ$visibility, breaks = seq(0,10,2)), sep = ""))

# Gerar sequencias contendo as informaÃ§Ãµes de 3 horas anteriores a cada evento
dadosMetSBRJ = get.seq(dadosMetSBRJ, flag_var = "flag", buffer_size = 4)

# Gerar coluna com a combinaÃ§Ã£o de eventos meteorologicos
dadosMetSBRJ$metConditions = paste(dadosMetSBRJ$temperatureD, 
                                   dadosMetSBRJ$dif_temp_dewD,
                                   dadosMetSBRJ$humidityD,
                                   dadosMetSBRJ$pressureD,
                                   dadosMetSBRJ$visibilityD)

# Gerar coluna com a quantidade de itens em cada linha transaÃ§Ã£o 
dadosMetSBRJ$SIZE = ifelse(gsub("\\s", "", dadosMetSBRJ$metConditions)=="",0, sapply(strsplit(dadosMetSBRJ$metConditions, " "), length))

#Selecionar colunas relevantes para o CSPADE
dadosMetSBRJ = select(dadosMetSBRJ, SID, timekey, SIZE, metConditions)
colnames(dadosMetSBRJ) = c("sequenceID", "eventID", "SIZE", "metConditions" )


# Gerar Transactios
write.table(dadosMetSBRJ, "tmp/dados.temp.txt", sep=" ", row.names = FALSE, col.names = FALSE, quote = FALSE)
metTransactions = read_baskets("tmp/dados.temp.txt", info = c("sequenceID","eventID","SIZE"))

# Verificar as transaÃ§Ãµes importadas
as(metTransactions, "data.frame")

# Exectuar o Cspade
#teste = cspade(metTransactions, parameter = list(support = 0.5), control = list(verbose = TRUE))
teste = cspade(metTransactions, control = list(verbose = TRUE))

# Verificar o resultado
resultSBRJ = as(teste, "data.frame")
save(resultSBRJ, file = "results_cspade.RData")

