## Tratamento de dados meteorologicos

library(dplyr)

setwd("~/github/DM2018/")

load("dados/wd.RData")

colnames(wd) = c("icao", "horario", "temperatura_f", "temperatura", "dew_point_f", "dew_point", "umidade",
"vento_direcao", "vento_nos", "vento_mph", "alti", "pressao", "precipitacao_mm", "preceipicatao_pol",
"visibilidade", "rajada_nos", "rajada_mph", "nebulosidade_local", "nebulosidade_regiao", "nebulosidade_nacional", "teto_local",
"teto_regional", "teto_nacional")

wd$temperatura_f = ifelse(wd$temperatura_f=="M",as.numeric(""), as.numeric(wd$temperatura_f))
wd$temperatura = ifelse(wd$temperatura=="M",as.numeric(""), as.numeric(wd$temperatura))
wd$dew_point_f = ifelse(wd$dew_point_f=="M",as.numeric(""), as.numeric(wd$dew_point_f))
wd$dew_point = ifelse(wd$dew_point=="M",as.numeric(""), as.numeric(wd$dew_point))
wd$umidade = ifelse(wd$umidade=="M",as.numeric(""), as.numeric(wd$umidade))
wd$vento_direcao = ifelse(wd$vento_direcao=="M",as.numeric(""), as.numeric(wd$vento_direcao))
wd$vento_nos = ifelse(wd$vento_nos=="M",as.numeric(""), as.numeric(wd$vento_nos))
wd$vento_mph = ifelse(wd$vento_mph=="M",as.numeric(""), as.numeric(wd$vento_mph))
wd$alti = ifelse(wd$alti=="M",as.numeric(""), as.numeric(wd$alti))
wd$pressao = ifelse(wd$pressao=="M",as.numeric(""), as.numeric(wd$pressao))
wd$preceipicatao_mm = ifelse(is.na(wd$preceipicatao_mm),as.numeric(""), as.numeric(wd$preceipicatao_mm))
wd$preceipicatao_pol = ifelse(is.na(wd$preceipicatao_pol),as.numeric(""), as.numeric(wd$preceipicatao_pol))
wd$visibilidade = ifelse(wd$visibilidade=="M",as.numeric(""), as.numeric(wd$visibilidade))
wd$rajada_nos = ifelse(wd$rajada_nos=="M",as.numeric(""), as.numeric(wd$rajada_nos))
wd$rajada_mph = ifelse(wd$rajada_mph=="M",as.numeric(""), as.numeric(wd$rajada_mph))
wd$nebulosidade_local = ifelse(wd$nebulosidade_local=="M","", wd$nebulosidade_local)
wd$nebulosidade_regiao = ifelse(wd$nebulosidade_regiao=="M","", wd$nebulosidade_regiao)
wd$nebulosidade_nacional = ifelse(wd$nebulosidade_nacional=="M","", wd$nebulosidade_nacional)
wd$teto_local = ifelse(wd$teto_local=="M",as.numeric(""), as.numeric(wd$teto_local))
wd$teto_regional = ifelse(wd$teto_regional=="M",as.numeric(""), as.numeric(wd$teto_regional))
wd$teto_nacional = ifelse(wd$teto_nacional=="M",as.numeric(""), as.numeric(wd$teto_nacional))

save(wd, file = "datasets/wd.RData")


