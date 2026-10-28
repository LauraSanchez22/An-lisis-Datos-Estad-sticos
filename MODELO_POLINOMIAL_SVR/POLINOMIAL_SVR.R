#Laura Lizbeth Sanchez Ramirez

source("Lib_Preprocess.R")

set.seed(1991)
options(scipen = 999)

#Preprocesamiento de datos
df.Startups <- read.csv("50_Startups.csv",
                      header = T,
                      stringsAsFactors = T)
df.Startups$Startups <- NULL
plot(df.Startups)

#Modelo de datos lineal
mdl.lineal.Startups <- lm(formula = Profit ~ R.D.Spend,
                          data = df.Startups)
summary(mdl.lineal.Startups)

plt.Startups.M <- ggplot() +
  geom_line(aes(x = df.Startups$Level,
                y = predict(mdl.lineal.Startups,
                            newdata = df.Startups)),
            colour = "green")


#Clasificación de los datos del modelo en entrenamiento y prueba
Split <- sample.split(Y =df.Startups$Profit,
                      SplitRatio = 0.8)
df.Startups.Train <- subset(df.Startups, Split == T)
df.Startups.Test <- subset(df.Startups, Split == F)

mdl.modelStartups <- lm(formula = Profit ~ .,
              data = df.Startups.Train)

#Resumen de los coeficientes estimados
mdl.modelStartups.sum <- summary(mdl.ALL)
mdl.modelStartups.sum

#Se realiza un ajuste al modelo de regresión polinómica en segundo grado
mdl.poly2.Startups <- lm(formula = Profit ~ poly(Level, 2),
                         data = df.Startups.Train)

mdl.poly2.Startups.sum <- summary(mdl.poly2.Startups)

plt.Startups.M2 <- geom_line(aes(x = df.Startups.Train$Level,
                                 y = predict(mdl.poly2.Startups,
                                             newdata = df.Startups)),
                             colour = "orange")

#mdl.ec.Startups <- lm(formula = Profit + 
                         # I(Level^2) +
                        #data = df.Startups)
#Gráficos
plt.Startups.PolyTrainig <- ggplot(data = df.Startups.Train, 
                                  aes(x = Level, y = Profit)) +
                                  geom_point() +
                                  geom_line(aes(y = predict(mdl.poly2.Startups, 
                                  newdata = df.Startups.Train)),
                                  color = "aquamarine") +
                                  ggtitle("Modelo para datos de Entrenamiento") +
                                  labs(x = "Level", y = "Profit")

plt.Startups.PolyTest <- ggplot(data = df.Startups.Test, 
                                aes(x = Level, y = Profit)) +
                                eom_point() +
                                geom_line(aes(y = predict(mdl.poly2.Startups, 
                                newdata = df.Startups.Test)), color = "orange") +
                                ggtitle("Modelo para datos de Prueba") +
                                labs(x = "Level", y = "Profit")

#¿Cómo se obtuvo el modelo final?
#Se realizó el cargado y preprocesamiento de los datos para preparalos para su análisis,
#después, se implementa el modelado de los datos de manera lineal en donde se emplean
#las variables Profit y R.D.Spend, enseguida de esto, se realiza la calsificación de los
#datos en Test y Train para continuar con el ajuste del modelo de regresión polinómica en 
#segundo grado.

#¿Por qué resultó un modelo de ese grado?
#El grado del modelo polinómico resultante implica aspectos relacionados con la cantidad de datos
#utilizados con base a la relación cuadrática de la variable predictora y las respuestas generadas.

#Ajuste del SVR utilizando un kernel lineal
svr.lineal <- swm(formula = Profit ~ .,
                  data = df.Startups.Train,
                  kernel = "linear",
                  type = "eps-regression")
svr.lineal.sum <- summary(svr.lineal)

#Predicción de los datos usando el modelo svr.lineal
df.Startups$predictions <- predict(svr.lineal, newdata = df.Startups)

#Se hace uso de un rango para identificar los hiperplanos
hiperplano <- seq(min(df.Startups$R.D.Spend), max(df.Startups$R.D.Spend), length.out = 100)

#Se identifican las lineas de los hiperplanos
hiperplano.nivel <- data.frame(
  R.D.Spend = rep(hiperplano, each = 3),
  lineas = c(
    svr.lineal$rho - cbind(1, -1, 0), t(predict(svr.lineal, newdata = data.frame(R.D.Spend = hiperplano))),
    svr.lineal$rho - 1 - cbind(1, -1, 0), t(predict(svr.lineal, newdata = data.frame(R.D.Spend = hiperplano))),
    svr.lineal$rho + 1 - cbind(1, -1, 0), t(predict(svr.lineal, newdata = data.frame(R.D.Spend = hiperplano)))
  )
)

#Gráfico SVR
plt.Startups.SVR1 <- ggplot(df.Startups, geom_line(aes(x = R.D.Spend, y = lineas),
                                                   colour = "red")) +
  ggtitle("SVR: Startups") +
  labs(x = "R.D.Spend", y = "Lineas") +
  theme_minimal()
#Se realiza el cálculo de R^2 para determinar el mejor modelo
Y.pred <- predict(svr.lineal, newdata = df.Startups)
SSr <- sum((df.Startups$Profit - Y.pred)^2)
SSy <- sum((df.Startups$Profit - mean(df.Startups$Profit))^2)
r.squared <- 1 - (SSr/SSy)

summary(r.squared)

Y.pred2 <- predict(mdl.poly2.Startups, newdata = df.Startups)
SSr2 <- sum((df.Startups$Profit - Y.pred)^2)
SSy2 <- sum((df.Startups$Profit - mean(df.Startups$Profit))^2)
r.squared2 <- 1 - (SSr2/SSy2)

summary(r.squared2)

#Conclusión:
#El modelo que en mi opinión se ajusta al procesamiento de los datos es el SVR lineal, ya que este mismo
#es menos propenso al sobreajuste de los datos en comparación con el modelo polinomial, sin embargo,
#los resultados que se puedan obtener dependerán de la cantidad de datos utilizados.

