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

# Agora temos valores mais coerentes e podemos
# começar a classificação por estado
cepagri$state <- "OK"
cepagri[cepagri$umid <= 12.0, ]$state <- "Emergencia"
cepagri[cepagri$umid >= 13.0 & cepagri$umid <= 20.0, ]$state <- "Alerta"
cepagri[cepagri$umid >= 21.0 & cepagri$umid <= 30.0, ]$state <- "Atencao"

cepagri$data<-as.Date(cepagri$horario)

# Separa o período de risco que será analisado: Agosto e Setembro,
# entre às 10 e 17 horas
periodorisco <- cepagri[
  cepagri$horario$mon >= 7 &
  cepagri$horario$mon <= 8,
]
periodorisco <- periodorisco[
  periodorisco$horario$hour >= 10 &
  periodorisco$horario$hour <= 17,
]

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
# de cada estado da Cepagri para os meses de Agosto e Setembro
# usa como parâmetro uma função que classifica um dia:
# para uma lista de estados, ela deve definir qual estado
# define aquele dia
generate_table <- function(classification_method) {
  # Cria um agrupamento com todos os estados medidos
  # em um determinado dia
  daystate <- as.data.frame(
    tapply(
      periodorisco$state,
      periodorisco$data,
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
  daystate$mes <- as.factor(sprintf("%02d", daystate$day$mon + 1))

  # Agrupa as ocorrências por mês
  bymonth <- as.data.frame(aggregate(daystate$mes, list(daystate$state), summary))
  colnames(bymonth) <- c("Estado", "Mes")
  bymonth <- cbind(bymonth, bymonth$Mes)
  bymonth$Mes <- NULL
  colnames(bymonth) <- c("Estado", "Agosto", "Setembro")
  # Calcula as porcentagens de ocorrências por mês
  bymonth$PercAgosto <- bymonth$Agosto / sum(bymonth$Agosto) * 100
  bymonth$PercSetembro <- bymonth$Setembro / sum(bymonth$Setembro) * 100
  bymonth$PercAgosto <- round(bymonth$PercAgosto, digits = 1)
  bymonth$PercSetembro <- round(bymonth$PercSetembro, digits = 1)
  # Melhora nome das colunas
  colnames(bymonth) <- c("Estado", "# Agosto", "# Setembro", "% Agosto", "% Setembro")
  # Tabela com quantidade e porcentagem de ocorrências de cada
  # estado perigoso nos meses de Agosto e Setembro
  bymonth
}

# Classificação pelo pior estado
table1 <- generate_table(classification_method_1); table1
# Classificação pelo terceiro quartil
table2 <- generate_table(classification_method_2); table2

