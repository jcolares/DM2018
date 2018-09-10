# funkyFunctions

# Get.seq() - Obtém a sequência de n observaçoes anteriores a cada ocorrência.
# Parâmetros:

get.seq = function(dataset, buffer_size=3, flag_var) {
  # Error treatment
  if(nrow(dataset) == 0) {stop("No data to process")}
  if(flag_var == "") {stop("No flag column informed")}  

  # Local variables
  flag_var = which( colnames(dataset)==flag_var)
  i = 1
  j = 0
  TID = 1
  n = buffer_size
  buffer = dataset[0,]
  buffer_row = buffer[0,]
  result = buffer[0]

  for (i in 1:nrow(dataset))
  #for (i in 1:300)
  {
    if (i > n) { buffer = buffer[-1, ]  }
    buffer_row = cbind(dataset[i,], i)
    buffer = rbind(buffer, buffer_row)
    
    if (!is.na(dataset[i, flag_var]) ) 
    {
      newDataframe = cbind(buffer, TID)
      TID = TID + 1
      result = rbind(result, newDataframe)
      newDataframe = newDataframe[0,]
    }
    i = i + 1
  }
  return(result)
}

get.colors = function(selection) {
  # ver depois como fazer isso direito.
  
  if(selection == "greens") {result = c("#f7fcf5","#e5f5e0","#c7e9c0","#a1d99b","#74c476","#41ab5d","#238b45","#006d2c","#00441b")}
  if(selection == "blues") {result = c("#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b")}
  if(selection == "reds") {result = c("#fff5f0","#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d")}
  if(selection == "oranges") {result = c("#fff5eb","#fee6ce","#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801","#a63603","#7f2704")}
  if(selection == "purples") {result = c("#fcfbfd","#efedf5","#dadaeb","#bcbddc","#9e9ac8","#807dba","#6a51a3","#54278f","#3f007d")}
  
  return(result)  
  }
  
