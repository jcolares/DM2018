library("arules")
library("arulesSequences")
library(dplyr)
library(arules)

# Carrega minhas funções 
source("funkyFunctions.R")

setwd("~/github/DM2018")

# Carrega os dados para analise
load("datasets/dados.RData")










# Generate test data
set.seed(101)
orders <- data.frame(
  transactionID = sample(1:500, 1000, replace=T),
  item = paste("item", sample(1:50, 1000, replace=T),sep = "")
  )

# Method 1 > Using a CSV ####
# Create a temporary directory
dir.create(path = "tmp", showWarnings = FALSE)

# Write our data.frame to a csv
write.csv(orders, "./tmp/tall_transactions.csv")

# Read that csv back in
order_trans <- read.transactions(
  file = "./tmp/tall_transactions.csv",
  format = "single",
  sep = ",",
  cols=c("transactionID","item"),
  rm.duplicates = T
)
summary(order_trans)


#Teste Cspade
order_cspde = cspade(order_trans, 
                     parameter = list(support = 0, maxsize = 1, maxlen = 1),
                     control   = list(verbose = TRUE))

                     
# Minha vez:

#prepara arquvivo.
# (O arquivo de dados deve ter 3 colunas: SID, EID e a sequencia de eventos)
dados.temp = filter(dados.d, dados.d$icao == "SBBE")
dados.temp$icao = NULL

#salvar os dados em um CSV
write.csv(dados.temp, "./tmp/dados.temp.csv")

# Ler o CSV de volta             ---- Isso não funciona com meu arquivo, pois os dados estão em +de1 coluna.
dados.trans <- read.transactions(
  file = "./tmp/dados.temp.csv",
  format = "basket",
  sep = ",",
  #cols=c("transactionID","item"),
  #cols=c("icao","TID","temperatura","dew_point","umidade","vento_direcao","vento_nos","rajada_nos","teto_local"),
  cols=3,
  rm.duplicates = F
)
summary(dados.trans)

teste = cspade(dados.trans, tmpdir="tmp")


# ===== OUTRO MÉTODO

# Converting to a Matrix ####
orders$const = TRUE

# Remove duplicates
dim(orders) #1,000 x 3
orders <- unique(orders)
dim(orders) #979 x 3

# Need to reshape the matrix
orders_mat_prep <- reshape(data = orders,
                           idvar = "transactionID",
                           timevar = "item",
                           direction = "wide")

# Drop the transaction ID
order_matrix <- as.matrix(orders_mat_prep[,-1])

# Clean up the missing values to be FALSE
order_matrix[is.na(order_matrix)] <- FALSE

# Clean up names
colnames(order_matrix) <- gsub(x=colnames(order_matrix),
                               pattern="const\\.", replacement="")
# to the point
order_trans2 <- as(order_matrix,"transactions")


####### Minha vez: ######## --    tambem naão funciona para dadoe em multiplas colunas 

dados.temp = filter(dados.d, dados.d$icao == "SBBE")
dados.temp$horario = 1:nrow(dados.temp)
  
# criar a constante const=TRUE
dados.temp$const = TRUE

# Remove duplicates, se ainda houver. 
dim(dados.temp) #3025 x 10
dados.temp <- unique(dados.temp)
dim(dados.temp) #2879 x 10

# remover coluna icao
dados.temp$icao = NULL

# Precisa reformatar a matriz
dados_mat_prep <- reshape(data = dados.temp,
                          varying = c("horario","TID","temperatura","dew_point","umidade","vento_direcao","vento_nos","rajada_nos","teto_local","const"),
                          idvar = "TID",
                          timevar = "horario",
                          direction = "long")

# Drop the transaction ID
dados_matrix <- as.matrix(dados_mat_prep[,-1])

# Clean up the missing values to be FALSE
dados_matrix[is.na(dados_matrix)] <- FALSE

# Clean up names
#colnames(dados_matrix) <- gsub(x=colnames(order_matrix), pattern="const\\.", replacement="")

# to the point
dados_trans <- as(dados_matrix,"transactions")

