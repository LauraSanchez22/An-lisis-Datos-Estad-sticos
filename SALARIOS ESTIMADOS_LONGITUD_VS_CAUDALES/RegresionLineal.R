#Titulo: Salarios Estimados; Longitud vs Caudales
#Archivo: RegresionLineal.R
#Author: Laura Lizbeth Sanchez Ramirez

#Revisión de los datasets disponibles en la biblioteca de R
library(datasets)

#Librería para dividir datos
library(caTools)

#Uso de notación cientifica para numeros grandes
options(scipen = 999)
#Usa de semilla para generación de numeros aleatorios
set.seed(2002)


#Se importan los datos del archivo "Social_Network_Ads.csv"
df.Riv <- read.csv(
  file = "Datasets/Social_Network_Ads.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

#Preprocesamiento
summary(df.Riv)

#Visualización general
plot(df.Riv)

#Se muestra un resumen general del dataset llamado "rivers"
summary(rivers)

#Creación de un dataset
Rivers <- data.frame(
  ID = 1:100,
  Nombre = paste("Río", 1:100, sep = "_"),
  Longitud_km = seq(from = 0, by = 20, length.out = 100),
  Caudal_m3s = rnorm(100, mean = 500, sd = 100),
  Paises = sample(
    c("País A", "País B", "País C"),
    100,
    replace = TRUE
  )
)

#Verificacipon del dataset creado
summary(Rivers)
head(Rivers)

#División de los datos (80% entrenamiento, 20% prueba)
Div <- sample.split(
  Y = df.Riv$Purchased,
  SplitRatio = 0.8
)

training_set <- subset(df.Riv, Div == TRUE)
test_set <- subset(df.Riv, Div == FALSE)

#Se verifican tamaños del dataset
#el 80% de los datos son de entrenamiento 
nrow(training_set) 

#El 20% de los datos de prueba
nrow(test_set)


# Gráfico de los conjuntos de entrenamiento y prueba: Salarios estimados, Edad

plot(
  training_set$Age,
  training_set$EstimatedSalary,
  col = "blue",
  pch = 19,
  xlab = "Edad",
  ylab = "Salario estimado",
  main = "Datos de Entrenamiento y Prueba"
)

points(
  test_set$Age,
  test_set$EstimatedSalary,
  col = "red",
  pch = 17
)

legend(
  "topleft",
  legend = c("Entrenamiento", "Prueba"),
  col = c("blue", "red"),
  pch = c(19, 17)
)


#Grafico Rivers, comparacion entre longitudes de caudales
colores <- c("red", "blue", "green")

plot(
  Rivers$Longitud_km,
  Rivers$Caudal_m3s,
  col = colores[as.factor(Rivers$Paises)],
  pch = 19,
  main = "Longitud vs Caudal por País",
  xlab = "Longitud (km)",
  ylab = "Caudal (m³/s)"
)

legend(
  "topright",
  legend = levels(as.factor(Rivers$Paises)),
  col = colores,
  pch = 19
)

grid()
