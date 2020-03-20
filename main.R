########################################
# Trabalho Final
# Nome(s): Karla Fátima Calvoso Simões
#          Oscar Esgalha
#          Renan Afonso Rossi
#          Weld Lucas Cunha
########################################

# Limpando todas as varíáveis e configurando o diretório de trabalho:
rm(list = ls())
setwd("/home/weld/Desktop/MDC/INF-0612 - Análise de Dados/Trabalho Final")

# Adicionando os outros arquivos:
source("./preprocessing.R")
source("./analysis_thermal.R")
source("./analysis_prediction.R")

## Main script:
# Fazendo a leitura do aquivo:
file <- './data/cepagri.csv'
names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri <- read.csv(file, header = FALSE, sep = ";", col.names = names,  fill=TRUE)

# Pré-processamento dos dados:
cepagri <- run_preprocessing(cepagri, num_cols=names[2:length(names)])
summary(cepagri)

# # Análise da Influência de Temperatura, umidade do ar e velocidade do vento na sensação térmica:
# run_thermal_sensation()
# # Implementação de um modelo preditivo simples de sensação térmica.
# run_thermal_prediction()


