# Identificação das condições meteorológicas que causam atrasos em voos em cada aeroporto
# utilizando dados curado fornecidos pelo prof.

# Quais são as combinações de condições meteorológicas que ocorrem com maior frequencia durante os atrasos?
# Baixa pressão + temperatura?

# PARTE 3 - Visualiza??o

#library(dplyr)
#library(lubridate)
#library(zoo)
library(arules)
library(arulesSequences)
library(arulesViz)

# Linux:
setwd("~/github/DM2018")
# Windows:
# setwd("C:/Users/Jefferson/Documents/GitHub/DM2018")

load("rules_cspade.RData")

#closedRulesSBRJ = ruleInduction(rulesSBRJ)
#inspect(head(closedRulesSBRJ, n = 5, by ="lift"))

resultSBJV = as(rulesSBJV, "data.frame")
resultSBGR = as(rulesSBGR, "data.frame")
resultSBRJ = as(rulesSBRJ, "data.frame")

topSBJV = top_n(resultSBJV, 50)
topSBGR = top_n(resultSBGR, 50)
topSBRJ = top_n(resultSBRJ, 50)

vis = cbind(topSBJV, topSBGR, topSBRJ)
