# Carga dos dados do vra 2014-2017

#bibliotecas
library(readr)
library(dplyr)
library(DataExplorer)
library(stats)
library(ggplot2)
library(readxl)

# Carga de todos os arquivos nos diretórios

# 2014
setwd("~/github/DM2018/dados/vra2014/")
files = list.files(pattern="*.csv")
nomes = c("icao_empresa", "voo", "codigo_di", "codigo_tipo_linha", "icao_origem", "icao_destino", "partida_prevista", "partida_real","chegada_prevista", "chegada_real", "situacao_voo", "cod_justificativa")
myfiles14 = do.call(rbind, lapply(files, function(x) read_delim(x, ";",locale = locale(encoding = "ISO-8859-1"), col_names = nomes, col_types = cols("codigo_di"= col_character(), "voo"= col_character()), skip = 1)))
myfiles14$partida_prevista = as.POSIXct(myfiles14$partida_prevista, format = "%d/%m/%Y %H:%M", tz=" ")
myfiles14$partida_real = as.POSIXct(myfiles14$partida_real, format = "%d/%m/%Y %H:%M", tz=" ")
myfiles14$chegada_prevista = as.POSIXct(myfiles14$chegada_prevista, format = "%d/%m/%Y %H:%M", tz=" ")
myfiles14$chegada_real = as.POSIXct(myfiles14$chegada_real, format = "%d/%m/%Y %H:%M", tz=" ")
atraso_partida <- difftime(as.POSIXct(myfiles14$partida_real), as.POSIXct(myfiles14$partida_prevista), units = "mins")
myfiles14 <- cbind(myfiles14, atraso_partida)
atraso_chegada <- difftime(as.POSIXct(myfiles14$chegada_real), as.POSIXct(myfiles14$chegada_prevista), units = "mins")
myfiles14 <- cbind(myfiles14, atraso_chegada)
myfiles14$atraso15ptd = ifelse(myfiles14$atraso_partida >= 15, 1, 0)  
myfiles14$atrasometorig = ifelse(myfiles14$cod_justificativa %in% c("WO", "XO"), 1, 0)  

# 2015
setwd("~/github/DM2018/dados/vra2015/")
files = list.files(pattern="*.csv")
nomes = c("icao_empresa", "voo", "codigo_di", "codigo_tipo_linha", "icao_origem", "icao_destino", "partida_prevista", "partida_real","chegada_prevista", "chegada_real", "situacao_voo", "cod_justificativa")
myfiles15 = do.call(rbind, lapply(files, function(x) read_delim(x, ";",locale = locale(encoding = "ISO-8859-1"), col_names = nomes, col_types = cols("codigo_di"= col_character(), "voo"= col_character()), skip = 1)))
myfiles15$partida_prevista = as.POSIXct(myfiles15$partida_prevista, format = "%d/%m/%Y %H:%M", tz=" ")
myfiles15$partida_real = as.POSIXct(myfiles15$partida_real, format = "%d/%m/%Y %H:%M", tz=" ")
myfiles15$chegada_prevista = as.POSIXct(myfiles15$chegada_prevista, format = "%d/%m/%Y %H:%M", tz=" ")
myfiles15$chegada_real = as.POSIXct(myfiles15$chegada_real, format = "%d/%m/%Y %H:%M", tz=" ")
atraso_partida <- difftime(as.POSIXct(myfiles15$partida_real), as.POSIXct(myfiles15$partida_prevista), units = "mins")
myfiles15 <- cbind(myfiles15, atraso_partida)
atraso_chegada <- difftime(as.POSIXct(myfiles15$chegada_real), as.POSIXct(myfiles15$chegada_prevista), units = "mins")
myfiles15 <- cbind(myfiles15, atraso_chegada)
myfiles15$atraso15ptd = ifelse(myfiles15$atraso_partida >= 15, 1, 0)  
myfiles15$atrasometorig = ifelse(myfiles15$cod_justificativa %in% c("WO", "XO"), 1, 0)  

# 2016
setwd("~/github/DM2018/dados/vra2016/")
files = list.files(pattern="*.csv")
nomes = c("icao_empresa", "voo", "codigo_di", "codigo_tipo_linha", "icao_origem", "icao_destino", "partida_prevista", "partida_real","chegada_prevista", "chegada_real", "situacao_voo", "cod_justificativa")
myfiles16 = do.call(rbind, lapply(files, function(x) read_delim(x, ";",locale = locale(encoding = "ISO-8859-1"), col_names = nomes, col_types = cols("codigo_di"= col_character(), "voo"= col_character()), skip = 1)))
myfiles16$partida_prevista = as.POSIXct(myfiles16$partida_prevista, format = "%d/%m/%Y %H:%M", tz=" ")
myfiles16$partida_real = as.POSIXct(myfiles16$partida_real, format = "%d/%m/%Y %H:%M", tz=" ")
myfiles16$chegada_prevista = as.POSIXct(myfiles16$chegada_prevista, format = "%d/%m/%Y %H:%M", tz=" ")
myfiles16$chegada_real = as.POSIXct(myfiles16$chegada_real, format = "%d/%m/%Y %H:%M", tz=" ")
atraso_partida <- difftime(as.POSIXct(myfiles16$partida_real), as.POSIXct(myfiles16$partida_prevista), units = "mins")
myfiles16 <- cbind(myfiles16, atraso_partida)
atraso_chegada <- difftime(as.POSIXct(myfiles16$chegada_real), as.POSIXct(myfiles16$chegada_prevista), units = "mins")
myfiles16 <- cbind(myfiles16, atraso_chegada)
myfiles16$atraso15ptd = ifelse(myfiles16$atraso_partida >= 15, 1, 0)  
myfiles16$atrasometorig = ifelse(myfiles16$cod_justificativa %in% c("WO", "XO"), 1, 0)  

# 2017
setwd("~/github/DM2018/dados/vra2017/")
files = list.files(pattern="*.csv")
nomes = c("icao_empresa", "voo", "codigo_di", "codigo_tipo_linha", "icao_origem", "icao_destino", "partida_prevista", "partida_real","chegada_prevista", "chegada_real", "situacao_voo", "cod_justificativa")
#myfiles17 = do.call(rbind, lapply(files[1:11], function(x) read_delim(x, ";",locale = locale(encoding = "ISO-8859-1"), col_types = cols("codigo_di"= col_character(), "partida_prevista" = col_datetime(format = "%d/%m/%Y %H:%M"), "partida_real" = col_datetime(format = "%d/%m/%Y %H:%M"), "chegada_prevista" = col_datetime(format = "%d/%m/%Y %H:%M"), "chegada_real" = col_datetime(format = "%d/%m/%Y %H:%M") ), col_names = nomes, skip = 1)))
myfiles17 = do.call(rbind, lapply(files, function(x) read_delim(x, ";",locale = locale(encoding = "ISO-8859-1"), col_names = nomes, col_types = cols("codigo_di"= col_character(), "voo"= col_character()), skip = 1)))
myfiles17$partida_prevista = as.POSIXct(myfiles17$partida_prevista, format = "%d/%m/%Y %H:%M", tz=" ")
myfiles17$partida_real = as.POSIXct(myfiles17$partida_real, format = "%d/%m/%Y %H:%M", tz=" ")
myfiles17$chegada_prevista = as.POSIXct(myfiles17$chegada_prevista, format = "%d/%m/%Y %H:%M", tz=" ")
myfiles17$chegada_real = as.POSIXct(myfiles17$chegada_real, format = "%d/%m/%Y %H:%M", tz=" ")
# acrescenta colunas para análises
atraso_partida <- difftime(as.POSIXct(myfiles17$partida_real), as.POSIXct(myfiles17$partida_prevista), units = "mins")
myfiles17 <- cbind(myfiles17, atraso_partida)
atraso_chegada <- difftime(as.POSIXct(myfiles17$chegada_real), as.POSIXct(myfiles17$chegada_prevista), units = "mins")
myfiles17 <- cbind(myfiles17, atraso_chegada)
myfiles17$atraso15ptd = ifelse(myfiles17$atraso_partida >= 15, 1, 0)  
myfiles17$atrasometorig = ifelse(myfiles17$cod_justificativa %in% c("WO", "XO"), 1, 0)  

vra = rbind(myfiles14, myfiles15, myfiles16, myfiles17)
rm(myfiles14, myfiles15, myfiles16, myfiles17)

save(vra, file="../vra.RData")
