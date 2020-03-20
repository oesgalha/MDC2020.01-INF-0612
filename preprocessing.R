#######################################
#    Pré-processamento dos dados     #
#######################################

run_preprocessing <- function(data, num_cols=c("temp", "vento", "umid", "sensa"),
                              range_list=list(c(-10, 1000), c(0, 200), c(0, 100), c(-10, 99))){
  print("Preprocessing the data!")
  
  data <- dropna(data)
  
  # Checando se todas as colunas são numéricas:
  for (col in num_cols){
    if (class(data[, col]) != "numeric"){
      data[, col] <- as.character(data[, col])
      data[, col] <- as.numeric(data[, col])
    }
  }

  # Eliminando outliers:
  # for (i in 1:length(num_cols)){
  #   range_vals <- range_list[[i]]
  #   col <- num_cols[i]
  #   data[data$col < range_vals[0], col] <- NA
  #   data[data$col > range_vals[1], col] <- NA
  # }
  data[data$sensa == 99.9, 5] <- NA
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
