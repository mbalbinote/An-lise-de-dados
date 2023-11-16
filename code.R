library(data.table)
base <- fread(input = paste0("MDC - Worksheet.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=".") 

base$year <- as.character(base$year)

#VER QUEM TEM MAIS FILMES NO DATASET USADO
library(dplyr)
#tabela
freq.tabela <- table(base$entity)
porc.tabela <- round(prop.table(freq.tabela)*100,1)
freq.tabelaL <- data.frame(porc.tabela)
#gráfico
library(RColorBrewer)
par(bg="white") 
COR<-brewer.pal(2,"Set1")
library(plotrix)
pielabels<-
  c("DC","Marvel")
pielabels2=paste(pielabels,"\n",freq.tabelaL$Freq,sep="")
pie3D(freq.tabelaL$Freq,radius=0.95,labels=pielabels2,explode=0.1,main="Marvel x DC - Frequência",
      col=COR)

#MPA RATING
#tabela
freq.tabela <- table(base$mpa_rating, useNA = "ifany") #se a reposta em branco é eliminada caso não usar NA
porc.tabela <- round(prop.table(freq.tabela)*100,1)
freq.tabelaL <- data.frame(porc.tabela)
freq.tabelaL <- freq.tabelaL[c(-1,-2),]
freq.tabelaL
#gráfico
barplot(height=freq.tabelaL$Freq, names=freq.tabelaL$Var1,
        col=rgb(1.000, 0.000, 0.000),
        xlab="Mpa Rating",
        ylab="Porcentagem",
        main="Classificação de Indicação por faixa etária")

calcularPorCategoria <- function(dataframe, coluna_categoria, coluna_valor) {
  resultado <- dataframe %>%
    group_by(.data[[coluna_categoria]]) %>%
    summarise(
      Media = mean(.data[[coluna_valor]]),
      Mediana = median(.data[[coluna_valor]]),
      Q25 = quantile(.data[[coluna_valor]], 0.25),
      Q75 = quantile(.data[[coluna_valor]], 0.75),
      Max = max(.data[[coluna_valor]]),
      Min = min(.data[[coluna_valor]])
    )
  
  return(resultado)
}

resultadoIMDB <- calcularPorCategoria(base, "entity", "imdb_rating")
resultadoIMDB
resultadoTOMATO <- calcularPorCategoria(base, "entity", "tomato_meter")
resultadoTOMATO

library(hrbrthemes)
#GRÁFICO DE CAIXA PARA COMPARAÇÃO DE NOTAS ENTRE DC E MARVEL, usando notas do imdb e tomato meter
library(ggplot2)
gerarBoxplotsPorCategoriaComCoresPersonalizadas <- function(dataframe, coluna_categoria, coluna_valor, cores, titulo) {
  plot <- ggplot(dataframe, aes(x = .data[[coluna_categoria]], y = .data[[coluna_valor]], fill = .data[[coluna_categoria]])) +
    geom_boxplot() +
    scale_fill_manual(values = cores) + 
    labs(
      x = coluna_categoria,
      y = coluna_valor,
      title = titulo
    )+
  theme_ipsum()
  print(plot)
}

cores_personalizadas <- c("red", "blue")
gerarBoxplotsPorCategoriaComCoresPersonalizadas(base, "entity", "imdb_rating",cores_personalizadas,"Avaliação IMDb")
cores_personalizadas <- c("red", "blue")
gerarBoxplotsPorCategoriaComCoresPersonalizadas(base, "entity", "tomato_meter",cores_personalizadas, "Avaliação Rotten Tomatoes")

#BASE DE DADOS PARA CADA ENTIDADE
baseDC <- base[base$entity == "DC",]
baseMarvel <- base[base$entity == "MARVEL",]

#COMPARAÇÃO DAS 5 MAIORES NOTAS DO IMDB E TOMATO METER PARA VER SE SÃO OS MESMOS FILMES
#TOP5imdb_rating
dados_ordenados <- base[order(-base$imdb_rating), ]
top_5_maiores_valores <- head(dados_ordenados, 5)
top5IMDB <- data.frame(top_5_maiores_valores)
tabelaTOP5IMDB <- top5IMDB[ ,c("title", "imdb_rating" ,"tomato_meter")]
tabelaTOP5IMDB$tomato_meter <- tabelaTOP5IMDB$tomato_meter/10
library(tidyr)
dados_longos <- pivot_longer(tabelaTOP5IMDB, cols = c(imdb_rating, tomato_meter), names_to = "nota")
ggplot(dados_longos, aes(x = title, y = value, fill = nota)) +
  geom_bar(stat = "identity", position = "Dodge") +
  labs(
    x = "Filme",
    y = "Nota",
    title = "Top 5 Melhores Avaliações - IMDb"
  ) +
  scale_fill_manual(values = c("imdb_rating" = "yellow", "tomato_meter" = "darkred"))+
  theme_ipsum()


#top5Tomato_Meter
dados_ordenados <- base[order(-base$tomato_meter), ]
top_5_maiores_valoresT <- head(dados_ordenados, 5)
top5T <- data.frame(top_5_maiores_valoresT)
tabelaTOP5T <- top5T[ ,c("title", "imdb_rating" ,"tomato_meter")]
tabelaTOP5T$tomato_meter <- tabelaTOP5T$tomato_meter/10

dados_longos <- pivot_longer(tabelaTOP5T, cols = c(imdb_rating, tomato_meter), names_to = "nota")
ggplot(dados_longos, aes(x = title, y = value, fill = nota)) +
  geom_bar(stat = "identity", position = "Dodge") +
  labs(
    x = "Filme",
    y = "Nota",
    title = "Top 5 Melhores Avaliações - Rotten Tomatoes"
  ) +
  scale_fill_manual(values = c("imdb_rating" = "yellow", "tomato_meter" = "darkred"))+
  theme_ipsum()


#calcular a média, dp e cv de quanto foi gasto para fazer os filmes e quanto foi arrecadado
calcular <- function(dataframe, coluna_categoria, coluna_valor) {
  resultado <- dataframe %>%
    filter(!is.na(.data[[coluna_valor]])) %>%
    group_by(.data[[coluna_categoria]]) %>%
    summarise(
      Media = mean(.data[[coluna_valor]]),
      Desvio_Padrao = sd(.data[[coluna_valor]]),
      Coeficiente_de_Variacao = (sd(.data[[coluna_valor]]) / mean(.data[[coluna_valor]])) * 100
    )
  
  return(resultado)
}

base$production_budget <- base$production_budget/1000000
resultado_estratificado <- calcular(base, "entity", "production_budget")
resultado_estratificado
resultado_estratificado2 <- calcular(base, "entity", "worldwide_box_office")
resultado_estratificado2

#TRANSFORMANDO OS DADOS DE MINUTOS DE FILMES PARA HORA:MINUTOS
base$hora_minuto <- sprintf("%02d:%02d", base$runtime %/% 60, base$runtime %% 60)

dados_ordenados_preço <- base[order(-base$production_budget), ]
top_10_maiores_preços <- head(dados_ordenados_preço, 10)
top10P <- data.frame(top_10_maiores_preços)
top10P$worldwide_box_office <- top10P$worldwide_box_office/1000000


ggplot(top10P, aes(x=top10P$production_budget, y=top10P$worldwide_box_office, color=top10P$title)) + 
  geom_point(size=4)+
  theme_ipsum()


