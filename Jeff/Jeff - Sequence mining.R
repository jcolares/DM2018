# ANÁLISE DE ZALGORIT

library(dplyr)
library(zoo)

setwd("~/github/DM2018")
load("datasets/dados_cron.RData")

cron_SBRJ = dados_cron %>% filter(icao == "SBRJ") %>% arrange(horario)

cron_SBRJ$flag = ifelse(is.na(cron_SBRJ$eventos), 0, 1)

#teste = cron_SBRJ %>%
#  group_by(flag) %>%
#  mutate(Sequencia=seq(n())) %> 
#  mutate(Sequencia=replace(Sequencia, flag!=1 , ""))
# errado errado errado

#teste = cron_SBRJ %>%
# cron_SBRJ$flag = ifelse(is.na(cron_SBRJ$eventos), 0, 1)
#  group_by(icao) %>%
#  mutate(Sequencia=cumsum(flag)) 
# quase certo

#cron_SBRJ$flag = ifelse(!is.na(cron_SBRJ$eventos), 1, as.numeric(""))
#teste = cron_SBRJ %>%
#  group_by(icao) %>%
#  mutate(Sequencia=zoo::na.locf(flag, fromLast=TRUE))
# NA TRAAAAVE

cron_SBRJ$flag = ifelse(!is.na(cron_SBRJ$eventos), 1, as.numeric(""))

teste = cron_SBRJ %>%
  group_by(flag) %>%
  mutate(runsum=cumsum(flag)) 

teste = zoo(teste, cron_SBRJ$horario)
teste$sequencia = na.locf(teste$runsum,  fromLast=TRUE)

####  NEXT STEPS:

# Fazer o código acima funcionar em um data-frame com multiplos aeroportos
# Rodar o algoriitmo

dados_cron$flag = ifelse(!is.na(dados_cron$eventos), 1, as.numeric(""))

fulltest = dados_cron %>%
  group_by(icao, flag) %>% 
  mutate(runsum=cumsum(flag))

fulltest = zoo(fulltest, order.by=dados_cron$icao, dados_cron$horario) # não funciona.. rever.
fulltest$sequencia = na.locf(fulltest$runsum, fromLast=TRUE)

