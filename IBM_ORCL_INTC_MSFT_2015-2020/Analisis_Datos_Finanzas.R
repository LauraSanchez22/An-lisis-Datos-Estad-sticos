library(dygraphs)
library(xts)
library(quantmod)
library(dplyr)
library(htmltools)


start <- format(as.Date("2015-01-01"), "%Y-%m-%d")
end <- format(as.Date("2020-08-01"), "%Y-%m-%d")

priceVolume <- function(simbolo){
  #Se obtienen precios Stock de Yahoo Finance
  datos <- getSymbols(simbolo, from=start, to=end, auto.assign = FALSE)
  #Elimina datos faltantes
  datos <- na.omit(datos)
  #Se mantienen columnas con precios cierre y volumenes (4 y 5) 
  datos <- datos[,4:5]
  #Los datos se hacen accesibles en el ambiente global
  assign(simbolo, datos, envir = .GlobalEnv)
}

#Datos a analizar segun Yahooo Finance: IBM, ORACLE, INTEL, MICROSOFT
priceVolume("IBM")
priceVolume("ORCL")
priceVolume("INTC")
priceVolume("MSFT")

#Juntamos los datos y renombramos las columnas
mergeData <- merge.xts(IBM, ORCL, INTC, MSFT)
colnames(mergeData) <- c("IBM P. Cierre", "IBM Vol.","ORCL P. Cierre", "ORCL Vol.", 
                         "INTC P. Cierre", "INTC Vol.", "MSFT P. Cierre", "MSFT Vol.")

#Se genera la grafica de precios: Series de Tiempo
Prices <- dygraph(mergeData[,c(1,3,5,7)], main = "Prices for IBM, ORCL, INTC, and MSFT" )%>%
  dyAxis("y", label = "Prices")%>%
  dyRangeSelector(dateWindow = c("2015-01-01", "2020-12-31"))%>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"))
Prices

#Se puede visualizar los ultimos 5 datos, hasta 3 decimales
round(tail(mergeData, n=5), 3)

dy_graphics <- list(
  dygraphs::dygraph(mergeData[,c(1,3,5,7)], main = "Prices for IBM, ORCL, INTC, and MSFT"),
  dygraphs::dygraph(mergeData[,c(2,4,6,8)], main =  "Volumes for IBM, ORCL, INTC, and MSFT")
  
)

htmltools::browsable(htmltools::tagList(dy_graphics))

#Se seleccionaron los datos de MSFT del 2015 al 2020
MSFT_2015 <- subset(mergeData[,1], index(mergeData) >= "2015-01-01" & index(mergeData) <= "2015-12-31")
MSFT_2015[c(1:5, nrow(MSFT_2015))]

MSFT_2020 <- subset(mergeData[,1], index(mergeData) >= "2020-01-01" & index(mergeData) <= "2020-12-31")
MSFT_2020[c(1:5, nrow(MSFT_2020))]

par(mfrow=c(2,1))

hist(MSFT_2015, freq = FALSE, col = "yellow", border="blue", main = "Densities of MSFT Prices in 2015", xlab = "Closing Prices")
lines(density(MSFT_2015), lwd = 2, col = "red")
hist(MSFT_2020, freq = FALSE, col = "blue", border = "blue", main = "Densities of MSFT Prices in 2020", xlab = "Closing Prices")
lines(density(AMNZ_2020), lwd = 2, col = "red")

