# Carga do vrawu17 a partir do vra-wu fornecido pelo professor

library(dplyr)
library(lubridate)
library(readxl)

#Carregar os dados necessários
setwd("~/github/DM2018")
load("dados/vra-wu.RData")

#Mudar tipos de dados POSIX
vra_wu$depart_expect = as.POSIXct(vra_wu$depart_expect)
vra_wu$depart = as.POSIXct(vra_wu$depart)
vra_wu$arrival_expect = as.POSIXct(vra_wu$arrival_expect)
vra_wu$arrival = as.POSIXct(vra_wu$arrival)

# Filtrar apenas o ano desejado
vrawu17a = vra_wu %>% filter(year(depart_expect) == 2017 | year(arrival_expect)==2017)

#Remover o dataframe original da memória
rm(vra_wu)

# Alterar os nomes das colunas já existentes
colnames(vrawu17a)[colnames(vrawu17a)=="destiny"] = "icao_destino"
colnames(vrawu17a)[colnames(vrawu17a)=="origin"] = "icao_origem"
colnames(vrawu17a)[colnames(vrawu17a)=="airline"] = "icao_empresa"
colnames(vrawu17a)[colnames(vrawu17a)=="flight"] = "voo"
#colnames(vrawu17a)[colnames(vrawu17a)=="autho_code"] = "new_name"     #n
colnames(vrawu17a)[colnames(vrawu17a)=="line_type"] = "codigo_tipo_linha"        

colnames(vrawu17a)[colnames(vrawu17a)=="depart_expect"] = "partida_prevista"
colnames(vrawu17a)[colnames(vrawu17a)=="depart"] = "partida_real"
colnames(vrawu17a)[colnames(vrawu17a)=="arrival_expect"] = "chegada_prevista"
colnames(vrawu17a)[colnames(vrawu17a)=="arrival"] = "chegada_real"
colnames(vrawu17a)[colnames(vrawu17a)=="status"] = "situacao_voo"
colnames(vrawu17a)[colnames(vrawu17a)=="observation"] = "cod_justificativa"

colnames(vrawu17a)[colnames(vrawu17a)=="departure_delay"] = "atraso_partida"
colnames(vrawu17a)[colnames(vrawu17a)=="arrival_delay"] = "atraso_chegada"
#colnames(vrawu17a)[colnames(vrawu17a)=="duration_expect"] = "duracao_prevista"   #não existia no nosso vrawu17
#colnames(vrawu17a)[colnames(vrawu17a)=="duration"] = "duracao_real"    #não existia no nosso vrawu17
#colnames(vrawu17a)[colnames(vrawu17a)=="duration_delta"] = "duracao_delta"   #não existia no nosso vrawu17
colnames(vrawu17a)[colnames(vrawu17a)=="origin.state"] = "origem_uf"

colnames(vrawu17a)[colnames(vrawu17a)=="destiny.state"] = "destino_uf"
colnames(vrawu17a)[colnames(vrawu17a)=="depart_temperature"] = "origem_temperatura"
colnames(vrawu17a)[colnames(vrawu17a)=="depart_dew_point"] = "origem_dew_point"
colnames(vrawu17a)[colnames(vrawu17a)=="depart_humidity"] = "origem_umidade"
colnames(vrawu17a)[colnames(vrawu17a)=="depart_pressure"] = "origem_pressao"
colnames(vrawu17a)[colnames(vrawu17a)=="depart_visibility"] = "origem_visibilidade"

colnames(vrawu17a)[colnames(vrawu17a)=="depart_events"] = "origem_eventos"
colnames(vrawu17a)[colnames(vrawu17a)=="depart_conditions"] = "origem_condicoes"
colnames(vrawu17a)[colnames(vrawu17a)=="arrival_temperature"] = "destino_temperatura"
colnames(vrawu17a)[colnames(vrawu17a)=="arrival_dew_point"] = "destino_dew_point"
colnames(vrawu17a)[colnames(vrawu17a)=="arrival_humidity"] = "destino_umidade"
colnames(vrawu17a)[colnames(vrawu17a)=="arrival_pressure"] = "destino_pressao"

colnames(vrawu17a)[colnames(vrawu17a)=="arrival_visibility"] = "destino_visibilidade"
colnames(vrawu17a)[colnames(vrawu17a)=="arrival_events"] = "destino_eventos"
colnames(vrawu17a)[colnames(vrawu17a)=="arrival_conditions"] = "destino_condicoes"


# Acrescentar as colunas calculadas

# id de tempo
vrawu17a$timekey_partida <- paste(substr(vrawu17a$partida_prevista,1,4), substr(vrawu17a$partida_prevista,6,7), substr(vrawu17a$partida_prevista,9,10), substr(vrawu17a$partida_prevista,12,13), "00", sep = "")
vrawu17a$timekey_chegada <- paste(substr(vrawu17a$chegada_prevista,1,4), substr(vrawu17a$chegada_prevista,6,7), substr(vrawu17a$chegada_prevista,9,10), substr(vrawu17a$chegada_prevista,12,13), "00", sep = "")
# datas
vrawu17a$origem_data = as.Date(vrawu17a$partida_prevista)
vrawu17a$origem_hora = as.numeric(substr(vrawu17a$partida_prevista, 12,13))
vrawu17a$destno_data = as.Date(vrawu17a$chegada_prevista)
vrawu17a$destino_hora = as.numeric(substr(vrawu17a$chegada_prevista, 12,13))


# Acrescentar as colunas que faltam

# Carregar dimensões - aerodromos
aerodromos <- read_excel("dados/glossario_de_aerodromo.xls", skip = 3, col_names = TRUE )
origens <- aerodromos
colnames(origens) <- c("icao_origem", "origem_descricao", "origem_cidade","origem_uf", "origem_pais", "origem_continente")
vrawu17a$icao_origem = as.character(vrawu17a$icao_origem)
vrawu17a <- merge(x=vrawu17a, y=origens, all.x = TRUE, by.x='icao_origem', by.y='icao_origem')
colnames(vrawu17a)[colnames(vrawu17a)=="origem_uf.y"] = "origem_uf"
vrawu17a$origem_uf.x = NULL
destinos <- aerodromos
colnames(destinos) <- c("icao_destino", "destino_descricao", "destino_cidade","destino_uf", "destino_pais", "destino_continente")
vrawu17a <- merge(x=vrawu17a, y=destinos, all.x = TRUE, by.x='icao_destino', by.y='icao_destino')
colnames(vrawu17a)[colnames(vrawu17a)=="destino_uf.y"] = "destino_uf"
vrawu17a$destino_uf.x = NULL
rm(origens, destinos, aerodromos)

#Carrega dimensoes - DI
vrawu17a$codigo_di = as.character(vrawu17a$autho_code)
di <- read_excel("dados/glossario_de_digito_identificador.xls", skip = 3, col_names = TRUE )
colnames(di) <- c("codigo_di", "descricao_codigo_di")
vrawu17a <- merge(x=vrawu17a, y=di, all.x = TRUE)
rm(di)
vrawu17a$autho_code = NULL

# Carrega dimensões - empresa
empresas <- read_excel("dados/glossario_de_empresas_aereas.xls", skip = 3, col_names = TRUE )
colnames(empresas) <- c("icao_empresa", "empresa", "x1", "nacionalidade")
empresas$x1 <- NULL
vrawu17a <- merge(x=vrawu17a, y=empresas, all.x = TRUE)
rm(empresas)

# Carrega dimensões - justificativas
#Obs: o arquivo original de justificativas continha caracteres estranhos que impediam sua leitura
#     foi necessário editar o xls antes de carregar
justificativas <- read_excel("dados/glossario_de_justificativas.xls", skip = 3, col_names = TRUE )
colnames(justificativas) <- c("cod_justificativa", "descricao_justificativa")
vrawu17a <- merge(x=vrawu17a, y=justificativas, all.x = TRUE)
rm(justificativas)

# Carrega dimensões - tipo de linha
tipo_linha <- read_excel("dados/glossario_de_tipo_de_linha.xls", skip = 3, col_names = TRUE )
colnames(tipo_linha) <- c("codigo_tipo_linha", "tipo_linha")
vrawu17a <- merge(x=vrawu17a, y=tipo_linha, all.x = TRUE)
rm(tipo_linha)


save(vrawu17a, file = "datasets/vrawu17a.RData")

