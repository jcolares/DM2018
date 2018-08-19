# Exemplo de análise

Esse script de exemplo mostra como plotar um gráfico de empresas que mais
atrasaram voos, considerando apenas atrasos >= 15 minutos.

Instruções:
1. baixar o dataset [nesse endereço](https://drive.google.com/drive/folders/1nW_G9pCRhvJFY5oOM2cDjZyRAi0UGO_S?usp=sharing) 
2. alterar o path na primeira linha do script
3. executar o script no RStudio

```
load("~/github/DM2018/dados/2017/vrawu17.RData") 
library(dplyr)
library(DataExplorer)
library(ggplot2)
atraso_partida_15 <- vrawu17$atraso_partida >= 15
vrawu17 <- cbind(vrawu17, atraso_partida_15)
rm(atraso_partida_15)
View(vrawu17)
atrasos <- filter(vrawu17, vrawu17$atraso_partida_15 == TRUE)
atrasos_empresas <- select(atrasos, empresa, atraso_partida_15
View(atrasos_empresas)
atrasos_empresas <- group_category(atrasos_empresas, feature = "empresa", threshold = 0.099)
ggplot(atrasos_empresas, aes(x=reorder(empresa, cnt, sum), y=cnt)) + geom_bar(show.legend = TRUE, stat = "identity") + coord_flip()
```
