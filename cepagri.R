library(ggplot2)

setwd("~/learning/mdc2020/INF0612/trabalho/")

# Carrega os dados
names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri <- read.csv("cepagri.csv", header = FALSE, sep = ";", col.names = names)
cepagri$horario <- as.POSIXct(as.character(cepagri$horario), format='%d/%m/%Y-%H:%M')
cepagri$horario <- as.POSIXlt(cepagri$horario)
cepagri$ano <- unclass(cepagri$horario)$year + 1900

# Filtra para analisar apenas os dados entre 2015 e 2019
cepagri <- cepagri[cepagri$ano >= 2015,]
cepagri <- cepagri[cepagri$ano <= 2019,]

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
cepagri[
  cepagri$umi == 0.0 &
  cepagri$ano == 2019 & 
  cepagri$horario$hour >= 7 &
  cepagri$horario$hour <= 9,
  4
] <- NA
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
  scale_x_discrete(name = "Mês") +
  guides(gradient=guide_legend("Mediana da Umidade Relativa"))

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
          levels = c("OK", "Atencao","Alerta","Emergencia"),
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

