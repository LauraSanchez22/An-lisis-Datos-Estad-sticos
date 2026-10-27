#Titulo: Modelo ARIMA, Suavizante Exponencial
#Archivo: EXP_HW_ARIMA.R
#Author: Laura Lizbeth Sanchez Ramirez

install.packages("fpp2")
library(fpp3)
install.packages("PerfomanceAnalytics")
library(xts)
install.packages(c("quantmod", "xts", "zoo", "TTR"))
library(quantmod)
library(fUnitRoots)
library(forecast)
library(ggplot2)
library(tseries)
library(lmtest)
library(TSA)
install.packages("Metrics")
library(Metrics)
install.packages("forecast", dependencies = TRUE)
options(digits = 3)

#Se obtienen datos
AMZN <- getSymbols("AMZN", from="2020-08-01", to="2021-03-31", src="yahoo", auto.assign = FALSE)
#Se eliminan valores faltantes
AMZN <- na.omit(AMZN)
#Se mantiene la columna 4: Precios Cierre
AMZN <- AMZN[,4]
plot(AMZN, ylab="Precios")

length(AMZN)

#Se utiliza el 7% para los datos de prueba
h <- round(length(AMZN)*0.07, digits = 0)
h

train <- AMZN[1:(nrow(AMZN) - h),]
test <- AMZN[(nrow(AMZN) - h + 1):nrow(AMZN),]

##### MODELO ARIMA #####
dftrain <- diff(train)[-1,]
adfTest(dftrain) #Se obtiene un valor de P VALUE: 0.01
adf.test(dftrain) #Datos obtenidos: Dickey-Fuller = -6.2722, Lag order = 5, p-value = 0.01 alternative hypothesis: stationary

#Graficación de ambos resultados
par(mfrow=c(2,1))
plot(train, col="red")
plot(dftrain, col="blue")

#Se definen candidatos del modelo ARIMA
m <- eacf(dftrain,15,10)

m2<- auto.arima(train, seasonal = TRUE)

summary(m2)

#Modelación de datos
modl<-Arima(train, order=c(7,1,3), method= "ML")

summary(modl)

coeftest(modl)
tsdiag(modl)

mod2 <- Arima(train, order=c(1,0,0), method = "ML")
mod2

coeftest(mod2)

#Modelos pronóstico para mod1 y mod2
Pron_m1 <- forecast(modl,h)
Pron_m2 <- forecast(mod2,h)

summary(Pron_m1)
summary(Pron_m2)

#Gráficos Forecast del Modelo Arima
par(mfrow=c(2,1))
plot(Pron_m1, include = 50)
plot(Pron_m2, include = 50)

#Gráfico de Datos de entrenamiento
traints<-ts(train,start = c(2020,08,01), frequency = 154)
fitted1<-ts(modl$fitted, start = c(2020,08,01), frequency = 154)
fitted2<-ts(mod2$fitted, start = c(2020,08,01), frequency = 154)
pron1<-ts(Pron_m1$mean, start = c(2021,08), frequency = 154)
pron2<-ts(Pron_m2$mean, start = c(2021,08), frequency = 154)

autoplot(traints)+
  autolayer(fitted1, series="ARIMA (7,1,3)")+
  autolayer(fitted2, series="ARIMA (1,0,0)")+
  autolayer(pron1, series = "PRON ARIMA (7,1,3)")+
  autolayer(pron2, series = "PRON ARIMA (1,0,0)")

#Medición del error pronóstico, RMSE y MAPE
RMSE_arima<-rmse(test, Pron_m1$mean)
RMSEar1<-rmse(test, Pron_m2$mean)

MAPE_arima<-mape(test, Pron_m1$mean)
MAPE_ar1<-mape(test, Pron_m2$mean)

#Se imprimen los resultados en una tabla
Modelo<-c("ARIMA(7,1,3)", "AR(3)")

RMSE<-c(RMSE_arima, RMSEar1)
MAPE<-c(MAPE_arima, MAPE_ar1)
res<-data.frame(Modelo,RMSE,MAPE)

print((res))
