setwd("~/learning/mdc2020/INF0612/trabalho/")

names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri <- read.csv("cepagri.csv", header = FALSE, sep = ";", col.names = names)
cepagri$horario <- as.POSIXct(as.character(cepagri$horario), format='%d/%m/%Y-%H:%M')
cepagri$horario <- as.POSIXlt(cepagri$horario)
cepagri$ano <- unclass(cepagri$horario)$year + 1900

cepagri <- cepagri[cepagri$ano >= 2015,]
cepagri <- cepagri[cepagri$ano <= 2019,]

cepagricor <- cepagri
cepagricor <- cepagricor[!is.na(cepagricor$sensa), ]
# Remove os erros de medicao em que a sensacao
# termica foi calculada como 99.9
cepagricor[cepagricor$sensa == 99.9, 5] <- NA
cepagricor <- cepagricor[!is.na(cepagricor$sensa), ]
cepagricor$temp <- as.character(cepagricor$temp)
cepagricor$temp <- as.numeric(cepagricor$temp)
cepagricor$vento <- as.character(cepagricor$vento)
cepagricor$vento <- as.numeric(cepagricor$vento)
cepagricor$umid <- as.character(cepagricor$umid)
cepagricor$umid <- as.numeric(cepagricor$umid)
cepagricor$sensa <- as.character(cepagricor$sensa)
cepagricor$sensa <- as.numeric(cepagricor$sensa)

# Buscar dias em que a umidade relativa era menor que 12
# Estado de Emergencia
cepagricor[
  cepagricor$horario >= "2019-01-05 05:00:00" & 
    cepagricor$horario <= "2019-01-06 00:00:00",
]


# Para a analise de umidade relativa, remove as medicoes
# incorretas de umidade que ocorreram em 2019 entre
# as 7 e 9 da manha com umidade relativa == 0
cepagricor[
  cepagricor$umi == 0.0 & 
  cepagricor$ano == 2019 & 
  cepagricor$horario$hour >= 7 &
  cepagricor$horario$hour <= 9,
  4
] <- NA
cepagricor <- cepagricor[!is.na(cepagricor$umid), ]

states <- factor(levels = c("Atencao", "Alerta", "Emergencia"), ordered=TRUE)

# Classifica
cepagricor$state <- "OK"
cepagricor[cepagricor$umid <= 12.0, ]$state <- "Emergencia"
cepagricor[cepagricor$umid >= 13.0 & cepagricor$umid <= 20.0, ]$state <- "Alerta"
cepagricor[cepagricor$umid >= 21.0 & cepagricor$umid <= 30.0, ]$state <- "Atencao"

cepagricor$mesano <- as.factor(paste(cepagricor$horario$mon + 1, cepagricor$ano, sep= "-"))

agostosetembro <- cepagricor[cepagricor$horario$mon >= 7 & cepagricor$horario$mon <= 8,]
horarioderisco <- agostosetembro[agostosetembro$horario$hour >= 10 & agostosetembro$horario$hour <= 17,]

horarioderisco$dia <- as.factor(paste(horarioderisco$horario$mday , horarioderisco$mesano, sep= "-"))

df <- aggregate(horarioderisco$state, list(horarioderisco$dia), summary)
