# Adicionando as bibliotecas necessárias:
library(ggplot2)
library(GGally)

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
