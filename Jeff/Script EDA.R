#bibliotecas
library(readr)
library(dplyr)
library(DataExplorer)
library(stats)
library(ggplot2)
library(readxl)


#Carga de todos os arquivos no diretório
setwd("~/github/DM2018/dados/2017/")
files = list.files(pattern="*.csv")

#Carrega os meses 1 a 11:
# OLD nomes = c("ICAO Empresa Aérea", "Número Voo", "Código Autorização (DI)", "Código Tipo Linha", "ICAO Aeródromo Origem", "ICAO Aeródromo Destino", "Partida Prevista", "Partida Real","Chegada Prevista", "Chegada Real", "Situação Voo", "Código Justificativa")
nomes = c("icao_empresa", "voo", "codigo_di", "codigo_tipo_linha", "icao_origem", "icao_destino", "partida_prevista", "partida_real","chegada_prevista", "chegada_real", "situacao_voo", "cod_justificativa")
# OPS: nomes = c("icao_origem", "timekey_partida", "icao_empresa", "voo", "codigo_di", "codigo_tipo_linha", "icao_destino", "partida_prevista", "partida_real", "chegada_prevista", "chegada_real", "situacao_voo", "cod_justificativa")
# OLD myfiles = do.call(rbind, lapply(files[1:11], function(x) read_delim(x, ";",locale = locale(encoding = "ISO-8859-1"), col_types = cols("Código Autorização (DI)"= col_character(), "Partida Prevista" = col_datetime(format = "%d/%m/%Y %H:%M"), "Partida Real" = col_datetime(format = "%d/%m/%Y %H:%M"), "Chegada Prevista" = col_datetime(format = "%d/%m/%Y %H:%M"), "Chegada Real" = col_datetime(format = "%d/%m/%Y %H:%M") ), col_names = nomes, skip = 1)))
myfiles = do.call(rbind, lapply(files[1:11], function(x) read_delim(x, ";",locale = locale(encoding = "ISO-8859-1"), col_types = cols("codigo_di"= col_character(), "partida_prevista" = col_datetime(format = "%d/%m/%Y %H:%M"), "partida_real" = col_datetime(format = "%d/%m/%Y %H:%M"), "chegada_prevista" = col_datetime(format = "%d/%m/%Y %H:%M"), "chegada_real" = col_datetime(format = "%d/%m/%Y %H:%M") ), col_names = nomes, skip = 1)))

#Carrega o mês 12
# OLD nomes2 = c("ICAO Empresa Aérea", "Número Voo", "Código Autorização (DI)", "Código Tipo Linha", "ICAO Aeródromo Origem", "ICAO Aeródromo Destino", "Partida Prevista", "Data Prevista", "Partida Real","Chegada Prevista", "Chegada Real", "Situação Voo", "Código Justificativa")
nomes2 = c("icao_empresa", "voo", "codigo_di", "codigo_tipo_linha", "icao_origem", "icao_destino", "partida_prevista", "data_prevista", "partida_real","chegada_prevista", "chegada_real", "situacao_voo", "cod_justificativa")

# OLD myfiles2 = read_delim(files[12], ";",locale = locale(encoding = "ISO-8859-1"), col_types = cols("Código Autorização (DI)"= col_character(), "Partida Prevista" = col_datetime(format = "%d/%m/%Y %H:%M"), "Partida Real" = col_datetime(format = "%d/%m/%Y %H:%M"), "Chegada Prevista" = col_datetime(format = "%d/%m/%Y %H:%M"), "Chegada Real" = col_datetime(format = "%d/%m/%Y %H:%M") ), col_names = nomes2, skip = 1)
myfiles2 = read_delim(files[12], ";",locale = locale(encoding = "ISO-8859-1"), col_types = cols("codigo_di"= col_character(), "partida_prevista" = col_datetime(format = "%d/%m/%Y %H:%M"), "partida_real" = col_datetime(format = "%d/%m/%Y %H:%M"), "chegada_prevista" = col_datetime(format = "%d/%m/%Y %H:%M"), "chegada_real" = col_datetime(format = "%d/%m/%Y %H:%M") ), col_names = nomes2, skip = 1)

#Elimina a coluna adicional do mes 12
# OLD myfiles2 = select(myfiles2, "ICAO Empresa Aérea", "Número Voo", "Código Autorização (DI)", "Código Tipo Linha", "ICAO Aeródromo Origem", "ICAO Aeródromo Destino", "Partida Prevista", "Partida Real","Chegada Prevista", "Chegada Real", "Situação Voo", "Código Justificativa")
myfiles2$data_prevista <- NULL

#Adiciona o mês 12 ao final dos demais meses
vra17 = rbind(myfiles, myfiles2)

#Adiciona colunas de id de tempo
timekey_partida <- paste(substr(vra17$partida_prevista,1,4), substr(vra17$partida_prevista,6,7), substr(vra17$partida_prevista,9,10), substr(vra17$partida_prevista,12,13), "00", sep = "")
vra17 <- cbind(vra17,timekey_partida)
timekey_chegada <- paste(substr(vra17$chegada_prevista,1,4), substr(vra17$chegada_prevista,6,7), substr(vra17$chegada_prevista,9,10), substr(vra17$chegada_prevista,12,13), "00", sep = "")
vra17 <- cbind(vra17,timekey_chegada)

# acrescenta colunas para análises
atraso_partida <- difftime(as.POSIXct(vra17$partida_real), as.POSIXct(vra17$partida_prevista), units = "mins")
vra17 <- cbind(vra17, atraso_partida)
atraso_chegada <- difftime(as.POSIXct(vra17$chegada_real), as.POSIXct(vra17$chegada_prevista), units = "mins")
vra17 <- cbind(vra17, atraso_chegada)

# salva o ano 2017 em um arquivo RData
save(vra17, file = "vra17.RData")

#remove os objetos da memoria
# OLD rm(myfiles, myfiles1, myfiles2, myfiles3)
# OLD rm(files, files2, nomes, nomes_extra, nomes2, timekey_partida)
rm(myfiles, myfiles2)
rm(files, nomes, nomes2, timekey_partida, timekey_chegada, atraso_chegada, atraso_partida)


#Carrega os dados meteorológicos
load("~/github/DM2018/dados/wu.RData")

#filtra apenas os dados referentes a 2017
#wu17 <- wu %>% filter(data.date >= "2017-01-01") %>% filter(data.date < "2018-01-01")
#rm(wu)

#Cria coluna tempo
timekey <- as.numeric(paste(substr(wu$data.date, 1,4), substr(wu$data.date, 6, 7), substr(wu$data.date, 9, 10), substr(wu$data.hour, 1,2), substr(wu$data.hour, 4,5), sep  = "" ))
wu <- cbind(timekey, wu)
rm(timekey)

# Acrescenta condições meteorologicas partida
colnames(wu) <- c("timekey", "origem_icao", "origem_data", "origem_hora", "origem_temperatura", "origem_dew_point", "origem_umidade", "origem_pressao", "origem_visibilidade", "origem_eventos", "origem_condicoes")
vrawu17 <- merge(x = vra17, y = wu, by.x = c("icao_origem", "timekey_partida"), by.y = c("origem_icao", "timekey"), all.x = TRUE)

# Acrescenta condições meteorologicas chegada
wu_destino <- wu
colnames(wu_destino) <- c("timekey","destino_icao", "destino_data", "destino_hora", "destino_temperatura", "destino_dew_point", "destino_umidade", "destino_pressao", "destino_visibilidade", "destino_eventos", "destino_condicoes")
vrawu17aux <- merge(x = vra17, y = wu_destino, by.x = c("icao_destino", "timekey_chegada"), by.y = c("destino_icao", "timekey"), all.x = TRUE)
vrawu17aux <- select(vrawu17aux, "destino_data", "destino_hora", "destino_temperatura", "destino_dew_point", "destino_umidade", "destino_pressao", "destino_visibilidade", "destino_eventos", "destino_condicoes")
vrawu17 <- cbind(vrawu17, vrawu17aux)
rm(vra17, vrawu17aux, wu, wu_destino)
save(vrawu17, file="vrawu17.RData")

# Carrega dimensões - aerodromos
aerodromos <- read_excel("~/github/DM2018/dados/glossario_de_aerodromo.xls", skip = 3, col_names = TRUE )
origens <- aerodromos
colnames(origens) <- c("icao_origem", "origem_descricao", "origem_cidade","origem_uf", "origem_pais", "origem_continente")
vrawu17 <- merge(x=vrawu17, y=origens, all.x = TRUE)
destinos <- aerodromos
colnames(destinos) <- c("icao_destino", "destino_descricao", "destino_cidade","destino_uf", "destino_pais", "destino_continente")
vratemp <- merge(x=vrawu17, y=destinos, all.x = TRUE)
vrawu17 <- vratemp
rm(vratemp, origens, destinos, aerodromos)

#Carrega dimensoes - DI
di <- read_excel("~/github/DM2018/dados/glossario_de_digito_identificador.xls", skip = 3, col_names = TRUE )
colnames(di) <- c("codigo_di", "descricao_codigo_di")
vrawu17 <- merge(x=vrawu17, y=di, all.x = TRUE)
rm(di)

# Carrega dimensões - empresa
empresas <- read_excel("~/github/DM2018/dados/glossario_de_empresas_aereas.xls", skip = 3, col_names = TRUE )
colnames(empresas) <- c("icao_empresa", "empresa", "x1", "nacionalidade")
empresas$x1 <- NULL
vrawu17 <- merge(x=vrawu17, y=empresas, all.x = TRUE)
rm(empresas)

# Carrega dimensões - justificativas
#Obs: o arquivo original de justificativas continha caracteres estranhos que impediam sua leitura
#     foi necessário editar o xls antes de carregar
justificativas <- read_excel("~/github/DM2018/dados/glossario_de_justificativas.xls", skip = 3, col_names = TRUE )
colnames(justificativas) <- c("cod_justificativa", "descricao_justificativa")
vrawu17 <- merge(x=vrawu17, y=justificativas, all.x = TRUE)

# Carrega dimensões - tipo de linha
tipo_linha <- read_excel("~/github/DM2018/dados/glossario_de_tipo_de_linha.xls", skip = 3, col_names = TRUE )
View(tipo_linha)
colnames(tipo_linha) <- c("codigo_tipo_linha", "tipo_linha")
vrawu17 <- merge(x=vrawu17, y=tipo_linha, all.x = TRUE)

save(vrawu17, file = "vrawu17.RData")

# Empresas aéreas e voos
# Total de Empresas: 61
# Principais Empresas (95% dos voos): 20
# Principais Empresas (99% dos voos): 42
testdf <- group_category(data=vra17, feature = "Empresa", threshold = 0.099)
testdf <- group_category(data=vrawu17, feature = "Empresa", threshold = 0.099)

# Gráfico de barras - quantidade de voos por compania aérea
ggplot(testdf, aes(x=reorder(`ICAO Empresa Aérea`,cnt,sum ) , y=cnt)) + geom_bar(show.legend = TRUE, stat = "identity") + coord_flip()

# Distribuição de voos por Tipo de Voo
# group_category(data=vra17, feature = "Tipo de Voo", threshold = 0.0000)
load("github/DM2018/dados/wu.RData")


