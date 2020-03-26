# Adicionando as bibliotecas necessárias:
library(ggplot2)

run_thermal_prediction <- function(data, num_cols = c("temp", "vento", "umid", "sensa"),
                                   split_prop=0.7, EPOCHS=100, init_params=c(1, 0, 0)){
  # Separando os dados em treinamento e teste:
  data_train <- data[1:as.integer(split_prop*nrow(data)),]
  data_test <- data[as.integer(split_prop*nrow(data)): nrow(data),]
  
  # Executando o treinamento:
  print("Performing the training:")
  opt_params <- init_params
  error_min <- Inf
  error_array <- NULL
  epoch_array <- 1:EPOCHS
  for (epoch in epoch_array){
    # Ajustando a taxa de aprendizado:
    lr <- adjust_lr(error_min)
    # Gerar mutações:
    params_set <- mutate_params(opt_params, lr)
    # Obtendo a melhor mutação:
    for (i in 1:length(params_set)){
      params <- params_set[[i]]
      sensa_pred <- predict_sensa(data_train, params)
      error <- calculate_error(data_train$sensa, sensa_pred)
      if (error < error_min){
        error_min <- error
        opt_params <- params
      }
    }
    error_array <- c(error_array, error_min)
    print(paste("Epoch:", as.character(epoch), "Error:", as.character(error_min)))
  }
  print("Optimal parameters:")
  print(paste("alpha:", as.character(opt_params[1])))
  print(paste("beta:", as.character(opt_params[2])))
  print(paste("gamma:", as.character(opt_params[3])))

  # Executando o teste:
  sensa_test <- predict_sensa(data_test, opt_params)
  error_test <- calculate_error(data_test$sensa, sensa_test)
  print(paste("Test error:", as.character(error_test)))
  
  # Plotting a boxplot graph:
  sensa_pred_all <- predict_sensa(data, opt_params)
  plot_boxplot(data, sensa_pred_all)

}


mutate_params <- function(init_condition, lr){
  alpha <- init_condition[1]
  beta <- init_condition[2]
  gamma <- init_condition[3]
  
  params_set <- list()
  params_set[[1]] = c(alpha, beta, gamma)
  params_set[[2]] = c(alpha+lr*runif(1), beta, gamma)
  params_set[[3]] = c(alpha-lr*runif(1), beta, gamma)
  params_set[[4]] = c(alpha, beta+lr*runif(1), gamma)
  params_set[[5]] = c(alpha, beta-lr*runif(1), gamma)
  params_set[[6]] = c(alpha, beta, gamma+lr*runif(1))
  params_set[[7]] = c(alpha, beta, gamma-lr*runif(1))
  
  return(params_set)
}


predict_sensa <- function(data, params){
  alpha <- params[1]
  beta <- params[2]
  gamma <- params[3]
  predictions <- (data$temp * alpha * 0.9) + (data$vento * beta * 0.2) + (data$umid * gamma * 0.5)

  return(predictions)
}


calculate_error <- function(y_true, y_pred){
  error <- sum(abs(y_true-y_pred))/length(y_true)
  return(error)
}


adjust_lr <- function(error){
  # Ajustando a taxa de aprendizado:
  if (error > 10){
    lr <- 0.1
  }
  else if(error > 1){
    lr <- 0.05
  }
  else{
    lr <- 0.01
  }
  return(lr)
}

plot_boxplot <- function(data, sensa_pred_all){
  data_copy <- data
  data$status <- "1 - medido"
  data_copy$sensa <- sensa_pred_all
  data_copy$status <- "2 - estimado"
  data_all <- rbind(data, data_copy)
  # grouped boxplot
  ggplot(data_all, aes(x=ano_mes, y=sensa, fill=status)) + 
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(NULL) + ylab("sensação térmica [ºC]") + 
    ggtitle("Medições x valores estimados")
}
