#------------------------------------------------#
# INF-0612 Análise de Dados                      #
#                                                #
# Trabalho FINAL                                 #
#------------------------------------------------#
# Nome COMPLETO Aluno (a) 1:                     #
#  Karla Fátima Calvoso Simões                   #
# Nome COMPLETO Aluno (a) 2:                     #
#  Oscar Esgalha                                 #
# Nome COMPLETO Aluno (a) 3:                     #
#  Renan Afonso Rossi                            #
# Nome COMPLETO Aluno (a) 4:                     #
#  Weld Lucas Cunha                              #
#------------------------------------------------#

#------------------------------------------------#
# Configuracao dos arquivos auxiliares           #
#------------------------------------------------#
rm(list = ls());
setwd("~/Projetos/UNICAMP/data/inf-0612/TesteFinal"); # configure o caminho antes de descomentar essa linha
library(plyr);
library(ggplot2);
library(GGally);

#------------------------------------------------#
#     Pré-processamento                          #
#------------------------------------------------#
# Carrega os dados
names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri_bkp <- read.csv("cepagri.csv", header = FALSE, sep = ";", col.names = names)

cepagri <- cepagri_bkp;

#------------------------------------------------#
#     Funções auxiliares                         #
#------------------------------------------------#
addAuxColumn <- function(data) {
  data$horario <- as.POSIXct(as.character(data$horario), format='%d/%m/%Y-%H:%M')
  data$horario <- as.POSIXlt(data$horario)
  data$ano <- unclass(data$horario)$year + 1900
  data$mes <- unclass(data$horario)$mon + 1
  data$dia <- unclass(cepagri$horario)$mday;
  return(data);
}

filterYears <- function(data) {
  data <- data[data$ano >= 2015,]
  data <- data[data$ano <= 2019,]
  return(data);
}

# Função de eliminação de dados indesejados
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

# Função de eliminação de dados repetidos
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

markUmidError <- function(data) {
  data[data$umi == 0.0 &
         data$ano == 2019 & 
         data$horario$hour >= 7 &
         data$horario$hour <= 9, 4] <- NA;
  return(data);
}
##################################################
#------------------------------------------------#
#     Análise Sensação Térmica                   #
#------------------------------------------------#

# Função de pré processamento
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
  data <- addAuxColumn(data);
  
  month_num <- as.character(unclass(data$horario)$mon + 1)
  for (num in c("1", "2", "3", "4", "5", "6", "7", "8", "9")){
    month_num[month_num == num] = paste("0", num, sep="")
  }
  data$ano_mes <- paste(as.character(unclass(data$horario)$year + 1900), month_num, sep="-")
  
  # Filtrando os anos de interesse:
  data <- filterYears(data);
  
  # Remove as medições incorretas de umidade que ocorreram
  # em 2019 entre as 7 e 9 da manha com umidade relativa == 0
  markUmidError(data);
  data <- dropna(data);
  
  return(data)
}

#------------------------------------------------#
#     Análise Sensação Térmica                   #
#------------------------------------------------#

run_thermal_sensation <- function(data, num_cols = c("temp", "vento", "umid", "sensa"),
                                  year = 2015, print_cor=FALSE, split_year=TRUE){
  # Analisando a correlação entre as variáveis de interesse:
  if (print_cor == TRUE){
    print(cor(data[num_cols]))  
  }
  # Plotting some correlograms:
  if (split_year == TRUE){
    data <- data[data$ano == year,]
    title <- paste("Correlogram", as.character(year))
  }
  else{
    title <- "Correlogram 2015-2019"
  }
  plot_correlogram(data[num_cols], title=title)
}

plot_correlogram <- function(data, title){
  ggpairs(data, title=title)
}

plot_scatters <- function(data){
  # A basic scatterplot with color depending on Species
  ggplot(data, aes(x = min(sensa):max(sensa))) +
    geom_point(aes(x=sensa, y=temp, alpha=0.001), color="red", size=1)
  geom_point(aes(x=sensa, y=vento, alpha=0.001), color="green", size=1)
  geom_point(aes(x=sensa, y=umid, alpha=0.001), color="blue", size=1) 
}

# Pré-processamento dos dados para esta análise:
cepagri <- run_preprocessing(cepagri, num_cols=names[2:length(names)])
summary(cepagri)

# # Análise da Influência de Temperatura, umidade do ar e velocidade do vento na sensação térmica:
# run_thermal_sensation(cepagri, year = 2015, print_cor=TRUE)
# # run_thermal_sensation(cepagri, year = 2016)
# # run_thermal_sensation(cepagri, year = 2017)
# # run_thermal_sensation(cepagri, year = 2018)
# run_thermal_sensation(cepagri, year = 2019)
run_thermal_sensation(cepagri, split_year=FALSE)

#------------------------------------------------#
#     Análise de Predição Sensação Térmica       #
#------------------------------------------------#

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

# Implementação de um modelo preditivo simples de sensação térmica.
run_thermal_prediction(cepagri)

##################################################
#------------------------------------------------#
#     Análise Saude                              #
#------------------------------------------------#

# Reiniando base de dados, para garantir que não tenha dados necessários para esta análise
cepagri <- cepagri_bkp;

cepagri <- addAuxColumn(cepagri);

# Filtra para analisar apenas os dados entre 2015 e 2019
cepagri <- filterYears(cepagri);

# Limpa as entradas com erro de medição
cepagri <- cepagri[!is.na(cepagri$umid), ]

# Verifica se as umidades estão numa faixa
# de valores aceitáveis (devem estar entre 0 e 100)
summary(cepagri[,c("umid")])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00   57.20   73.30   70.12   85.10  100.00

# Os valores estão dentro da faixa de valores esperados
# Porém, uma umidade relativa de 0 representa um clima
# muito seco e perigoso. Talvez seja um outlier.

head(cepagri[cepagri$umi == 0.0, c("horario", "umid")])
# horario umid
# 250975 2019-01-05 07:00:00    0
# 250976 2019-01-05 07:10:00    0
# 250977 2019-01-05 07:20:00    0
# 250978 2019-01-05 07:30:00    0
# 250979 2019-01-05 07:40:00    0
# 250980 2019-01-05 07:50:00    0

tail(cepagri[cepagri$umi == 0.0, c("horario", "umid")])
# horario umid
# 302266 2019-12-29 07:00:00    0
# 302267 2019-12-29 07:10:00    0
# 302410 2019-12-30 07:00:00    0
# 302411 2019-12-30 07:10:00    0
# 302554 2019-12-31 07:00:00    0
# 302555 2019-12-31 07:10:00    0

# É possível notar que essas medições de umidade == 0
# começaram em 2019 e ocorreram próximo das 7 da manhã

# Para observar os valores em torno de um
# dos horários suspeitos
cepagri[
  cepagri$horario >= "2019-01-05 06:30:00" &
    cepagri$horario <= "2019-01-05 08:30:00",
  c("horario", "umid")
  ]
# horario umid
# 250972 2019-01-05 06:30:00 96.3
# 250973 2019-01-05 06:40:00 96.3
# 250974 2019-01-05 06:50:00 96.3
# 250975 2019-01-05 07:00:00  0.0
# 250976 2019-01-05 07:10:00  0.0
# 250977 2019-01-05 07:20:00  0.0
# 250978 2019-01-05 07:30:00  0.0
# 250979 2019-01-05 07:40:00  0.0
# 250980 2019-01-05 07:50:00  0.0
# 250981 2019-01-05 08:00:00  0.0
# 250982 2019-01-05 08:10:00  0.0
# 250983 2019-01-05 08:20:00 94.9
# 250984 2019-01-05 08:30:00 94.9

# Pela variação, dá para notar que esse dia na
# verdade era um dia úmido! Algum problema ocorreu
# na medição de umidade relativa em 2019 durante a manhã

# Remove as medições incorretas de umidade que ocorreram
# em 2019 entre as 7 e 9 da manha com umidade relativa == 0
cepagri <- markUmidError(cepagri)
cepagri <- cepagri[!is.na(cepagri$umid), ]

summary(cepagri[,c("umid")])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 5.70   57.20   73.40   70.31   85.10  100.00

# Segundo o artigo da Cepagri os meses de Agosto
# e Setembro são os que apresentam mais risco à
# saúde (considerando a umidade relativa)
# A seguir vamos plotar a mediana das umidades
# relativas por mês para validar essa afirmação
monthmean <- aggregate(cepagri$umid, list(cepagri$horario$mon), mean)
colnames(monthmean) <- c("mes", "umidade")
monthmean$nomemes <- factor(month.name[monthmean$mes + 1], levels = month.name)
# Ordena os meses
levels(monthmean$nomemes) <- month.name

# Compara a mediana da umidade relativa de cada mês
# fica bem visível que Agosto e Setembro são os meses
# mais secos
ggplot(monthmean, aes(x=nomemes, y=umidade)) +
  geom_bar(aes(fill = umidade), stat = "identity") +
  scale_fill_gradient(low = "red", high = "blue", na.value = NA) +
  labs(title = "Mediana da Umidade Relativa por Mês") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Umidade") +
  scale_x_discrete(name = "Mês")

# Agora temos valores mais coerentes e podemos
# começar a classificação por estado
cepagri$state <- "OK"
cepagri[cepagri$umid <= 12.0, ]$state <- "Emergencia"
cepagri[cepagri$umid >= 13.0 & cepagri$umid <= 20.0, ]$state <- "Alerta"
cepagri[cepagri$umid >= 21.0 & cepagri$umid <= 30.0, ]$state <- "Atencao"

cepagri$data<-as.Date(cepagri$horario)

# Classifica um dia de forma bem rígida:
# usa o pior estado alcançado em um dia,
# mesmo que ele tenha se manifestado por pouco tempo
classification_method_1 <- function(states) {
  max(states)
}

# Classifica de acordo com o terceiro quartil
# das observações de um determinado estado
classification_method_2 <- function(states) {
  quartiles <- quantile(as.numeric(states))
  levels(states)[quartiles["75%"]]
}

# Gera a tabela com as ocorrências e porcentagens
# de cada estado agrupado por mês.
#
# Parâmetros:
#   classification_method
#     -> função que classifica um dia. Recebe uma
#        lista de estados, deve definir qual estado
#        representa o dia
#   df
#     -> dataframe com os dados de umidade relativa
#        e os dias em que a umidade foi medida
generate_table <- function(classification_method, df) {
  # Cria um agrupamento com todos os estados medidos
  # em um determinado dia
  daystate <- as.data.frame(
    tapply(
      df$state,
      df$data,
      function(state){
        factor(
          state,
          levels = c("OK", "Atencao", "Alerta", "Emergencia"),
          ordered=TRUE
        )
      }
    )
  )
  colnames(daystate) <- c("states")
  # Determina o estado que identifica aquele dia
  daystate$state <- unlist(
    lapply(
      daystate[,c("states")],
      classification_method
    )
  )
  daystate$day <- rownames(daystate)
  daystate$day <- as.POSIXct(as.character(daystate$day), format='%Y-%m-%d')
  daystate$day <- as.POSIXlt(daystate$day)
  daystate$mes <- as.factor(month.name[daystate$day$mon + 1])
  
  # Agrupa as ocorrências por mês
  bymonth <- as.data.frame(aggregate(daystate$mes, list(daystate$state), summary))
  colnames(bymonth) <- c("Estado", "Mes")
  bymonth <- cbind(bymonth, bymonth$Mes)
  bymonth$Mes <- NULL
  # Calcula as porcentagens de ocorrências para cada mês
  months <- colnames(bymonth)[2:length(colnames(bymonth))]
  for (month in months) {
    bymonth[paste("%", month)] <- round(
      bymonth[month] /  sum(bymonth[month]) * 100,
      digits = 1
    )
  }
  # Tabela com quantidade e porcentagem de ocorrências de cada
  # estado perigoso nos meses do dataframe recebido
  bymonth
}

# Separa o período de risco que será analisado: Agosto e Setembro,
# entre às 10 e 17 horas
periodorisco <- cepagri[
  cepagri$horario$mon >= 7 &
    cepagri$horario$mon <= 8 &
    cepagri$horario$hour >= 10 &
    cepagri$horario$hour <= 17,
  ]

# Classificação pelo pior estado
table1 <- generate_table(classification_method_1, periodorisco); table1
# Estado August September % August % September
# 1         OK     99        87     63.9        58.8
# 2    Atencao     38        28     24.5        18.9
# 3     Alerta     17        28     11.0        18.9
# 4 Emergencia      1         5      0.6         3.4

# Classificação pelo terceiro quartil
table2 <- generate_table(classification_method_2, periodorisco); table2
# Estado August September % August % September
# 1     Alerta      8        24      5.2        16.2
# 2    Atencao     34        31     21.9        20.9
# 3 Emergencia      0         1      0.0         0.7
# 4         OK    113        92     72.9        62.2

# Tabela com os dados de 1997-2008
original_table <- data.frame(
  Estado = c("Atencao","Alerta","Emergencia"),
  August = c(10.1, 1.7, 0),
  September = c(4.9, 2.3, 0.3)
)
# Tabela com os dados de 2015-2019
current_table <- table2
# Iguala a tabela com os dados atuais:
# - Remove os OKs
# - Deixa apenas as colunas com porcentagem
current_table[4,] <- NA
current_table <- current_table[!is.na(current_table$Estado),]
current_table$August <- NULL
current_table$September <- NULL
colnames(current_table) <- c("Estado", "August", "September")

# Recebe um dataframe com valores de estados
# com Agosto e Setembro como colunas e converte
# eles para valores da linha, preenchendo uma
# coluna "mes" com o nome do mes de origem dos
# valores
colunize_month_value <- function(df) {
  colunized <- df
  colunized1 <- df[c("Estado", "August")]
  colunized2 <- df[c("Estado", "September")]
  colunized1$mes <- "August"
  colunized2$mes <- "September"
  colnames(colunized1) <- c("estado", "porcentagem", "mes")
  colnames(colunized2) <- c("estado", "porcentagem", "mes")
  rbind(colunized1, colunized2)
}

# Move os meses para um valor em uma coluna "mes"
current_table <- colunize_month_value(current_table)
original_table <- colunize_month_value(original_table)

# Junta os dados separados por ano
current_table$anos <- "2015-2019"
original_table$anos <- "1997-2008"
comparativo <- rbind(original_table, current_table)

# Grouped
ggplot(comparativo, aes(fill=estado, y=porcentagem, x=mes)) +
  geom_bar(aes(fill = estado), position="dodge", stat="identity") +
  facet_wrap(~ anos) +
  scale_fill_manual(values = c("yellow", "orange", "red")) +
  labs(title = "Porcentagem de Ocorrência dos Estados") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "% no estado") +
  scale_x_discrete(name = "Mês")

##################################################
#------------------------------------------------#
#     Análise Vento                              #
#------------------------------------------------#

# Reiniando base de dados, para garantir que não tenha dados necessários para esta análise
cepagri <- cepagri_bkp;

# Le tabela da escala de beaufort
beaufort <- read.csv("escala-de-beaufort.csv" , header = TRUE);
beaufort;

# Remove vento igual NA
nrow(cepagri)
cepagri <- cepagri[!is.na(cepagri$vento), ]
nrow(cepagri)

# Verifica outliers
summary(cepagri$vento);
head(cepagri[cepagri$vento == 143.60, ], 10);
head(cepagri[cepagri$vento == 0, ], 50);

#Convertendo horario para POSIXlt e criando colunas ano/mes/dia
cepagri <- addAuxColumn(cepagri);
head(cepagri);

cepagri2015 <- cepagri[cepagri$ano == 2015, ];
cepagri201512 <- cepagri2015[cepagri2015$mes == 12,];
cepagri201512 <- cepagri201512[cepagri201512$dia == 12, ];
cepagri201512;

## http://g1.globo.com/sp/campinas-regiao/noticia/2015/09/temporal-tem-ventos-ate-1425-kmh-na-regiao-de-campinas-diz-cepagri.html

filtro <- consecutive(cepagri$vento, 144)
cepagri <- cepagri[!filtro, ];

# Media por mês/ano
tapply(cepagri$vento, list(cepagri$ano, cepagri$mes), mean)

# removendo anos com dados incompletos
class(cepagri$ano);
cepagri <- filterYears(cepagri);

## Análise exploratória ##

# Media por mês/ano
table_mean_mounth <- tapply(cepagri$vento, list(cepagri$ano, cepagri$mes), mean)
table_mean_mounth <- round(table_mean_mounth, digits = 2);

mounths <- c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro');
colnames(table_mean_mounth) <- mounths; table_mean_mounth;

# exporta tabela
write.table(table_mean_mounth, file = "table_mean_mounth.csv", sep = ",", quote = FALSE, row.names = T)

# Mediana por mês/ano
tapply(cepagri$vento, list(cepagri$ano, cepagri$mes), median)

# Desvio padrão por mês/ano
tapply(cepagri$vento, list(cepagri$ano, cepagri$mes), sd)

# Média por ano
tapply(cepagri$vento, cepagri$ano, mean)

# Mediana por ano
tapply(cepagri$vento, cepagri$ano, median)

# Desvio padrão por ano
tapply(cepagri$vento, cepagri$ano, sd)

# Média por mês
tapply(cepagri$vento, cepagri$mes, mean)

# Mediana por mês
tapply(cepagri$vento, cepagri$mes, median)

## Em média, Em campinas, entre 2015 e 2019
## O mês que mais ventou = OUTUBRO;
## O Ano que mais ventou = 2019
## O mês que menos ventou = FEVEREIRO;
## O Ano que mais ventou = 2019

## funcao que recebe uma velocidade de vento
## e retorna sua categoria na escala de beaufort
calc_beaufort <- function(vento) {
  i <- 1;
  while(vento > beaufort[i, ]$max) {
    i = i + 1; 
  }
  
  return(beaufort[i, ]$Grau)
}

# Soma ocorrencias de cada grau de beaufort
cepagri["beaufort"] <- sapply(cepagri$vento, calc_beaufort);
cepagri[cepagri$beaufort == 12, ];
beaufort_sum <- tapply(cepagri$beaufort, cepagri$ano, plyr::count);

# deixa os itens com mesmo numero de linhas
for (i in 1:length(beaufort_sum)) {
  while(nrow(beaufort_sum[[i]]) < 13) {
    beaufort_sum[[i]] <- rbind(beaufort_sum[[i]], c(nrow(beaufort_sum[[i]]), 0));
  }
}

# agrupa em um data frame
beaufort_sum;
beaufort_matrix <- matrix(NA, nrow = 1, ncol = 0);
for (i in 1:length(beaufort_sum)) {
  beaufort_sum[[i]]$year <- names(beaufort_sum[i])
  beaufort_matrix <- rbind(beaufort_matrix, beaufort_sum[[i]]);
}

df_beaufort <- as.data.frame(beaufort_matrix); df_beaufort;

png("escala-beaufort.png", width = 638, height = 399);
p<-ggplot(df_beaufort , aes(x = year, y = freq, fill = as.factor(x)));
p<-p+geom_bar(stat="identity", position=position_dodge(), color="Black");
p<-p+labs(y="Quantidade de Medições", x="Ano", fill="Escala de Beaufort");
p<-p+scale_fill_discrete(labels=beaufort$Designação);
p<-p+theme_minimal();
print(p)
dev.off()

##################################################
#------------------------------------------------#
#     Análise Estações                             #
#------------------------------------------------#
# Reiniando base de dados, para garantir que não tenha dados necessários para esta análise
cepagri <- cepagri_bkp;

#Eliminando linhas com valores não disponíveis (NA)
#Tamanho inicial: 314.325
cepagri <- cepagri [!is.na(cepagri$sensa), ] 
#Após retirada dos NA´s da coluna sensa: 312.001
nrow(cepagri)

#Convertendo horario para POSIXlt
class(cepagri$horario)
cepagri$horario <- strptime(cepagri$horario, "%d/%m/%Y-%H:%M")
class(cepagri$horario)

#Retirando linhas que não serão utilizadas na análise
#Retirando 2014, deixando somente a partir do dia 21/12/2014 - inicio do verao de 2015
cepagri <- cepagri [as.Date(cepagri$horario) >= "2014-12-21", ]
#Retirando a partir do começo do verão de 2020: 21/12/2019
cepagri <- cepagri [as.Date(cepagri$horario) < "2019-12-21", ]
head(cepagri)
tail(cepagri)

#Definindo os anos e as respectivas estações.
#Foram considerados os períodos abaixo para definição de cada estação, sem considerar os horários
#Primavera: 21 setembro até 20 dezembro.
#Verão:     21 dezembro até 20 março.
#Outono:    21 março até 20 junho.
#Inverno:   21 junho até 20 setembro.
#Criando função define_estacao_ano
define_estacao_ano <- function(p_horario) {
  estacao <- NULL
  ano <- NULL
  if ((p_horario >= "2015-09-21" && p_horario < "2015-12-21") ||
      (p_horario >= "2016-09-21" && p_horario < "2016-12-21") ||
      (p_horario >= "2017-09-21" && p_horario < "2017-12-21") || 
      (p_horario >= "2018-09-21" && p_horario < "2018-12-21") ||
      (p_horario >= "2019-09-21" && p_horario < "2019-12-21")) 
  {estacao <- "Primavera"; ano <- year(p_horario); 
  }
  
  else if (p_horario >= "2014-12-21" && p_horario < "2015-03-21") 
  {estacao <- "Verao"; ano <- 2015; }
  
  else if (p_horario >= "2015-12-21" && p_horario < "2016-03-21")
  {estacao <- "Verao"; ano <- 2016; }
  else if (p_horario >= "2016-12-21" && p_horario < "2017-03-21") 
  {estacao <- "Verao"; ano <- 2017; } 
  else if (p_horario >= "2017-12-21" && p_horario < "2018-03-21") 
  {estacao <- "Verao"; ano <- 2018; }
  else if (p_horario >= "2018-12-21" && p_horario < "2019-03-21") 
  {estacao <- "Verao"; ano <- 2019; }
  else if ((p_horario >= "2015-03-21" && p_horario < "2015-06-21") || 
           (p_horario >= "2016-03-21" && p_horario < "2016-06-21") ||
           (p_horario >= "2017-03-21" && p_horario < "2017-06-21") || 
           (p_horario >= "2018-03-21" && p_horario < "2018-06-21") ||
           (p_horario >= "2019-03-21" && p_horario < "2019-06-21") ) 
  {estacao <- "Outono"; ano <- year(p_horario); }
  else if ((p_horario >= "2015-06-21" && p_horario < "2015-09-21") || 
           (p_horario >= "2016-06-21" && p_horario < "2016-09-21") ||
           (p_horario >= "2017-06-21" && p_horario < "2017-09-21") || 
           (p_horario >= "2018-06-21" && p_horario < "2018-09-21") ||
           (p_horario >= "2019-06-21" && p_horario < "2019-09-21") ) 
  {estacao <- "Inverno"; ano <- year(p_horario); }
  return(cbind(estacao,ano))
}
#Chamando a função define_estacao_ano
estacoes <- matrix(ncol = 2)
estacoes <- NULL
for(i in 1:nrow(cepagri)) {
  horario <- cepagri[[1]][i];
  estacoes <- rbind(estacoes, define_estacao_ano(as.Date(horario)));
}
#Incluindo as colunas estacao e ano no dataframe cepagri
cepagri <- cbind(cepagri,estacoes)
head(cepagri)
tail(cepagri)

# Converte valores para numéricos
cepagri$temp <- as.character(cepagri$temp)
cepagri$temp <- as.numeric(cepagri$temp)
cepagri$vento <- as.character(cepagri$vento)
cepagri$vento <- as.numeric(cepagri$vento)
cepagri$umid <- as.character(cepagri$umid)
cepagri$umid <- as.numeric(cepagri$umid)
cepagri$sensa <- as.character(cepagri$sensa)
cepagri$sensa <- as.numeric(cepagri$sensa)

#Separando dataframe em anos
cepagri2015 <- cepagri[cepagri$ano == 2015, ]
cepagri2016 <- cepagri[cepagri$ano == 2016, ]
cepagri2017 <- cepagri[cepagri$ano == 2017, ]
cepagri2018 <- cepagri[cepagri$ano == 2018, ]
cepagri2019 <- cepagri[cepagri$ano == 2019, ]

# Criando dataframe por metrica de cada estação/ano
Media <- data.frame(Media_2015 = round(tapply(cepagri2015$temp , cepagri2015$estacao , mean),2),
                    Media_2016 = round(tapply(cepagri2016$temp , cepagri2016$estacao , mean),2),
                    Media_2017 = round(tapply(cepagri2017$temp , cepagri2017$estacao , mean),2),
                    Media_2018 = round(tapply(cepagri2018$temp , cepagri2018$estacao , mean),2),
                    Media_2019 = round(tapply(cepagri2019$temp , cepagri2019$estacao , mean),2))

Minima <- data.frame(Min_2015 = tapply(cepagri2015$temp , cepagri2015$estacao , min),
                     Min_2016 = tapply(cepagri2016$temp , cepagri2016$estacao , min),
                     Min_2017 = tapply(cepagri2017$temp , cepagri2017$estacao , min),
                     Min_2018 = tapply(cepagri2018$temp , cepagri2018$estacao , min),
                     Min_2019 = tapply(cepagri2019$temp , cepagri2019$estacao , min))

Maxima <- data.frame(Max_2015 = tapply(cepagri2015$temp , cepagri2015$estacao , max),
                     Max_2016 = tapply(cepagri2016$temp , cepagri2016$estacao , max),
                     Max_2017 = tapply(cepagri2017$temp , cepagri2017$estacao , max),
                     Max_2018 = tapply(cepagri2018$temp , cepagri2018$estacao , max),
                     Max_2019 = tapply(cepagri2019$temp , cepagri2019$estacao , max))

Desvio <- data.frame(Dv_2015 = round(tapply(cepagri2015$temp , cepagri2015$estacao , sd),2),
                     Dv_2016 = round(tapply(cepagri2016$temp , cepagri2016$estacao , sd),2),
                     Dv_2017 = round(tapply(cepagri2017$temp , cepagri2017$estacao , sd),2),
                     Dv_2018 = round(tapply(cepagri2018$temp , cepagri2018$estacao , sd),2),
                     Dv_2019 = round(tapply(cepagri2019$temp , cepagri2019$estacao , sd),2))    

Variancia <- data.frame(Var_2015 = round(tapply(cepagri2015$temp , cepagri2015$estacao , var),2),
                        Var_2016 = round(tapply(cepagri2016$temp , cepagri2016$estacao , var),2),
                        Var_2017 = round(tapply(cepagri2017$temp , cepagri2017$estacao , var),2),
                        Var_2018 = round(tapply(cepagri2018$temp , cepagri2018$estacao , var),2),
                        Var_2019 = round(tapply(cepagri2019$temp , cepagri2019$estacao , var),2))  

Metricas <- list(Minima,Media, Maxima, Desvio,Variancia)

#Geração dos gráficos
cepagri2015$estacao <- factor(cepagri2015$estacao)
head(cepagri2015)
class(cepagri2015$estacao_f)

library(ggplot2)
ggplot(cepagri2015, aes(x = estacao , y = temp, group = estacao, fill = estacao)) + labs(title = "2015") + geom_violin () + scale_fill_brewer(palette = "Set1")
ggplot(cepagri2016, aes(x = estacao , y = temp, group = estacao, fill = estacao)) + labs(title = "2016") + geom_violin () + scale_fill_brewer(palette = "Set1")
ggplot(cepagri2017, aes(x = estacao , y = temp, group = estacao, fill = estacao)) + labs(title = "2017") + geom_violin () + scale_fill_brewer(palette = "Set1")
ggplot(cepagri2018, aes(x = estacao , y = temp, group = estacao, fill = estacao)) + labs(title = "2018") + geom_violin () + scale_fill_brewer(palette = "Set1")
ggplot(cepagri2019, aes(x = estacao , y = temp, group = estacao, fill = estacao)) + labs(title = "2019") + geom_violin () + scale_fill_brewer(palette = "Set1")
