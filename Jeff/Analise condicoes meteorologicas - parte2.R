# Identificacao das condicoes meteorologicas que causam atrasos em voos em cada aeroporto
# utilizando dados curado fornecidos pelo prof.

# Quais sao as combinacoes de condicoes meteorologicas que ocorrem com maior frequencia durante os atrasos?
# Ex: Baixa pressao + temperatura?

# PARTE 2 - Preparacao de dados e execucao do Cspade

library(dplyr)
library(lubridate)
library(arulesSequences)

# Linux:
setwd("~/github/DM2018")
# Windows:
# setwd("C:/Users/Jefferson/Documents/GitHub/DM2018")


# Carrega minhas funcoes 
source("Jeff/funkyFunctions.R")

load("datasets/dadosMet.Rdata")

#Informar os codigos icao dos aeroportos para analise:
icaoList = c("SBJV", "SBGR", "SBRJ")

# Gera o dataset e roda o CSPADE para os ICAO informados
for (icaoSelect in icaoList) { 
  dadosMetICAO = dadosMet %>% 
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
  dadosMetICAO$flag = ifelse(dadosMetICAO$delays + dadosMetICAO$cancels > 0, 1, as.numeric(""))
  
  # Identifica sequencias de eventos
  dadosMetICAO = dadosMetICAO %>%
    group_by(flag) %>%
    mutate(eventID=cumsum(flag)) 
  
  # Discretizacao
  dadosMetICAO$temperatureD = ifelse(is.na(dadosMetICAO$temperature), "", paste("T",.bincode(dadosMetICAO$temperature, breaks = seq(0,40,2)), sep = ""))
  dadosMetICAO$dif_temp_dewD =  ifelse(is.na(dadosMetICAO$dif_temp_dew), 
                                       "",  
                                       ifelse(dadosMetICAO$dif_temp_dew==0, 
                                              "D0",  
                                              paste("D",.bincode(dadosMetICAO$dif_temp_dew, breaks = seq(0,40,2)), sep = "")))
  dadosMetICAO$humidityD = ifelse(is.na(dadosMetICAO$humidity), "", paste("H",.bincode(dadosMetICAO$humidity, breaks = seq(0,100,4)), sep = ""))
  dadosMetICAO$pressureD = ifelse(is.na(dadosMetICAO$pressure), "", paste("P",.bincode(dadosMetICAO$pressure, breaks = seq(1000,1030,2)), sep = ""))
  dadosMetICAO$visibilityD = ifelse(is.na(dadosMetICAO$visibility), "", paste("V",.bincode(dadosMetICAO$visibility, breaks = seq(0,10,2)), sep = ""))
  
  # Gerar sequencias contendo as informacoes de 3 horas anteriores a cada evento
  dadosMetICAO = get.seq(dadosMetICAO, flag_var = "flag", buffer_size = 4)
  
  # Gerar coluna com a combinacao de eventos meteorologicos
  dadosMetICAO$metConditions = paste(dadosMetICAO$temperatureD, 
                                     dadosMetICAO$dif_temp_dewD,
                                     dadosMetICAO$humidityD,
                                     dadosMetICAO$pressureD,
                                     dadosMetICAO$visibilityD)
  
  # Gerar coluna com a quantidade de itens em cada linha transacao 
  dadosMetICAO$SIZE = ifelse(gsub("\\s", "", dadosMetICAO$metConditions)=="",0, sapply(strsplit(dadosMetICAO$metConditions, " "), length))
  
  #Selecionar colunas relevantes para o CSPADE
  dadosMetICAO = select(dadosMetICAO, SID, timekey, SIZE, metConditions)
  colnames(dadosMetICAO) = c("sequenceID", "eventID", "SIZE", "metConditions" )

  # Gerar Transactios
  write.table(dadosMetICAO, "tmp/dados.temp.txt", sep=" ", row.names = FALSE, col.names = FALSE, quote = FALSE)
  metTransactions = read_baskets("tmp/dados.temp.txt", info = c("sequenceID","eventID","SIZE"))
  
  # Exectuar o Cspade e armazenar as regras encontradas
  rules = cspade(metTransactions, control = list(verbose = TRUE))
  assign(paste("rules", icaoSelect, sep="."), rules)
  
  # Armazenar o resultado como dataframe
  #results = as(rules, "data.frame")
  #assign(paste("results", icaoSelect, sep="."), results)
  
  #registrar nomes de objetos para salvar em arquivo 
  new_rules = c(paste("rules", icaoSelect, sep="."))
  if(!exists("rules_list")){rules_list = vector()}
  rules_list = c(rules_list,  new_rules)
}  

save(list = rules_list, file = "rules_cspade.RData")
