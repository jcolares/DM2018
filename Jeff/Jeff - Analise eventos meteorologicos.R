# Análise de eventos que levam a atrasos por razões meteorológicas.

setwd("~/github/DM2018")
load("datasets/vra.RData")
load("datasets/wd.RData")

#bibliotecas
library(dplyr)
library(DataExplorer)
library(ggplot2)
#library(readr)
#library(stats)
#library(readxl)

# 23 aeroportos brasileiros concentram 85% dos atrasos/cancelamentos por razões meteorológicas
vra %>% 
  filter(atrasometorig == 1) %>%
  filter(substr(icao_origem, 1, 2) == "SB") %>%
  group_category(feature = "icao_origem", threshold = 0.15) %>%
  select(icao_origem, pct) %>%
  ggplot(aes(x=reorder(icao_origem, -pct, sum), y=pct)) + 
  geom_bar(show.legend = TRUE, stat = "identity") #+ coord_flip()

aerop = vra %>% 
  filter(atrasometorig == 1) %>%
  filter(substr(icao_origem, 1, 2) == "SB") %>%
  group_category(feature = "icao_origem", threshold = 0.15) %>%
  select(icao_origem)

# Lista de atrasos/cancelamentos ocorridos nos aeroportos citados
eventos = 
  vra %>%
  filter(atrasometorig == 1) %>%
  filter(icao_origem %in% aerop$icao_origem) %>%
  filter(atraso_partida >= 15) %>%
  select(icao_origem, partida_prevista, atraso_partida, situacao_voo)
    
# Filtra apenas os dados meteorologicos dos 23 aeroportos
wd_eventos = filter(wd, icao %in% aerop$icao_origem)

# Libera espaço na memória
rm(aerop, vra, wd)

 

