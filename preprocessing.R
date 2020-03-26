#######################################
#    Pré-processamento dos dados     #
#######################################

run_preprocessing <- function(data, num_cols=c("temp", "vento", "umid", "sensa")){

  # Eliminando valores NA (devido à falha na leitura):
  data <- dropna(data)
  
  # Checando se todas as colunas são numéricas:
  for (col in num_cols){
    if (class(data[, col]) != "numeric"){
      data[, col] <- as.character(data[, col])
      data[, col] <- as.numeric(data[, col])
    }
  }

  # Eliminando outliers:
  data[data$sensa == 99.9, 5] <- NA
  # Eliminando valores NA (devido à remoção de outliers):
  data <- dropna(data)
  
  # Adicionando colunas auxiliares:
  data$horario <- as.POSIXct(as.character(data$horario), format='%d/%m/%Y-%H:%M')
  data$horario <- as.POSIXlt(data$horario)
  data$ano <- unclass(data$horario)$year + 1900
  data$mes <- unclass(data$horario)$mon + 1
  month_num <- as.character(unclass(data$horario)$mon + 1)
  for (num in c("1", "2", "3", "4", "5", "6", "7", "8", "9")){
    month_num[month_num == num] = paste("0", num, sep="")
  }
  data$ano_mes <- paste(as.character(unclass(data$horario)$year + 1900), month_num, sep="-")
  
  # Filtrando os anos de interesse:
  data <- data[data$ano >= 2015,]
  data <- data[data$ano <= 2019,]
  
  # Remove as medições incorretas de umidade que ocorreram
  # em 2019 entre as 7 e 9 da manha com umidade relativa == 0
  data[data$umi == 0.0 &
       data$ano == 2019 & 
       data$horario$hour >= 7 &
       data$horario$hour <= 9, 4] <- NA
  data <- dropna(data)
  
  return(data)
}

dropna <- function(data){
  # Removendo linhas incompletas:
  n1 <- nrow(data)
  for (col in colnames(data)){
    data <- data[!is.na(data[col]), ]
  }
  n2 <- nrow(data)
  # cat(sprintf("\"%s\" \"%i\"\n", "Linhas incompletas encontradas:", n1 - n2))
  return(data)
}

consecutive <- function(vector, k = 1) {
  n <- length (vector)
  result <- logical (n)
  for (i in (1+k):n)
    if (all( vector [(i-k):(i-1)] == vector[i]))
      result [i] <- TRUE
  for (i in 1:(n-k))
    if (all( vector [(i+1):(i+k)] == vector[i]))
      result [i] <- TRUE
  return (result)
}
