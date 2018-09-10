
#bibliotecas
library(readr)
library(dplyr)
library(DataExplorer)
library(stats)
library(ggplot2)
library(readxl)
library(tidyr)

# Carga dos dados
setwd("/home/jeff/github/DM2018/")
load("datasets/vrawu17.RData")

# Mantém apenas os vôos nacionais
vrawu17 <- vrawu17 %>% filter(origem_pais=="BRASIL"& destino_pais=="BRASIL")

# Tipos de linhas
tipo_linha <- vrawu17 %>% count(tipo_linha)
plot_bar(tipo_linha, with = "n", title = "Voos nacionais por tipo de linha")

# Aeródromos
aerop = count(vrawu17, icao_origem)

# Tabela temporaria com detalhes de aerportos
aerop = filter(aerop, n > 1460)
# Prepara rótulos mais apropriados para o gráfico
temp = select(vrawu17, icao_origem, origem_cidade, origem_descricao)
temp = unique(temp)
temp$aerop = substr(paste(temp$origem_cidade, " - ", temp$origem_descricao), 1, 40)
# Troca o código pelo nome 
aerop = merge(x = aerop, y = temp, all.x = TRUE)
aerop = select(aerop, icao_origem, aerop, n)
colnames(aerop) = c("Código", "Aeroporto", "Voos")
# Exibe o gráfico
ggplot(aerop, aes(x=reorder(Aeroporto,Voos,sum ) , y=Voos)) + geom_bar(show.legend = TRUE, stat = "identity") + coord_flip()
# (não foi possivel usar o Data Explorer pois há mais de 50 categorias na variável)
rm(temp)

# Filtrar o dataset
vrawu17 = vrawu17 %>% 
  filter(icao_origem %in% aerop$`Código`) %>%
  filter(icao_destino %in% aerop$`Código`)

# apenas os registros de cancelamento/atraso relacionados ao clima
# normativa IAC-1504
# http://www.anac.gov.br/assuntos/legislacao/legislacao-1/iac-e-is/iac/iac-1504/@@display-file/arquivo_norma/IAC1504.pdf
justificativas = select(vrawu17, cod_justificativa, descricao_justificativa)
justificativas = unique(justificativas)
justificativas = na.exclude(justificativas)
justmeteorologicas = filter(justificativas, cod_justificativa %in% c("WO", "WI", "WR", "WS", "WT", "WA", "XO", "XS", "XT", "AM", "RM"))


justMeteorOrigem = filter(justificativas, cod_justificativa %in% c("WO", "XO"))

 
# Cria coluna target, que identifica os atrasos ou cancelamentos provocados por eventos meteorológicos
target = ifelse(vrawu17$cod_justificativa %in% justmeteorologicas$cod_justificativa, 1, 0)
vrawu17 = cbind(vrawu17, target)

# Cria coluna target_ori, que identifica os atrasos ou cancelamentos provocados por eventos meteorológicos no aeroporto de origem
target_ori = ifelse(vrawu17$cod_justificativa %in% justMeteorOrigem$cod_justificativa, 1, 0)
vrawu17 = cbind(vrawu17, target_ori)

# Ordena os aeroportos por percentual de atrasos
ranking_aerop_atrasos  = 
  vrawu17 %>% 
    group_by(icao_origem) %>% 
    count(target_ori) %>% 
    spread(target_ori, n) %>% 
    mutate(perc = `1`/`0`) %>% 
    arrange(desc(perc))
# Recupera os nomes de aeroportos
ranking_aerop_atrasos = merge(x=ranking_aerop_atrasos, y = aerop, by.x = "icao_origem", by.y = "Código", all.x = TRUE)

# Exibe o gráfico
g = ggplot(ranking_aerop_atrasos, aes(x=reorder(Aeroporto,perc,sum ) , y=perc)) + geom_bar(show.legend = TRUE, stat = "identity") + coord_flip()
g = g  +ggtitle("Percentual de cancelamentos/atrasos")
plot(g)


## Analise do clima nos top 50 em eventos de atraso/cancelamento

vrawu50 = 
  vrawu17 %>% 
    filter(icao_origem %in% ranking_aerop_atrasos[1:10,1]) %>%
    select(icao_origem, origem_temperatura, origem_dew_point, origem_umidade, origem_pressao, origem_visibilidade)
    