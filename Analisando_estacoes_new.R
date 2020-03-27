# Limpando todas as varíáveis e configurando o diretório de trabalho:
rm(list = ls())
setwd("C:\\MDC 2020\\INF-0612 - Análise de DadosTrabalho Final")

# Fazendo a leitura do aquivo:
names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri <- read.csv('cepagri.csv', header = FALSE, sep = ";", col.names = names,  fill=TRUE, stringsAsFactors = FALSE)

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


ggplot(cepagri, aes(x = estacao , y = temp, 
                    group = estacao, fill = estacao)) +
 labs(title = "Variação Temperatura") + geom_violin () +
 scale_fill_brewer(palette = "Set1") + facet_wrap( ~ ano, ncol=2) +
 labs(x = "Estações", y = "Temperatura")

#Dados levantados para análise
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

