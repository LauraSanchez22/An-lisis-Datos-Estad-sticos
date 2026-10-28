#Titulo: Modelos Polinomiales y SVR
#Archivo: POLINOMIAL_SVR.R
#Author: Laura Lizbeth Sanchez Ramirez

source("Lib_Preprocess.R")

set.seed(1991)
options(scipen = 999)


### PREPROCESAMIENTO ###


df.Position <- read.csv(
  "Position_Salaries.csv",
  header = TRUE,
  stringsAsFactors = TRUE
)

df.Position$Position <- NULL

summary(df.Position)
plot(df.Position)

### MODELO LINEAL ###


mdl.lineal.Position <- lm(
  formula = Salary ~ Level,
  data = df.Position
)

summary(mdl.lineal.Position)

# GRADO 1

plt.Position.g1 <- geom_line(
  aes(
    x = df.Position$Level,
    y = predict(
      mdl.lineal.Position,
      newdata = df.Position
    )
  ),
  colour = "tomato",
  linewidth = 1
)


### GRADO 2 ###

mdl.poly2.Position <- lm(
  formula = Salary ~ poly(Level, 2),
  data = df.Position
)

summary(mdl.poly2.Position)

plt.Position.g2 <- geom_line(
  aes(
    x = df.Position$Level,
    y = predict(
      mdl.poly2.Position,
      newdata = df.Position
    )
  ),
  colour = "blue",
  linewidth = 1
)

### GRADO 3 ##


mdl.poly3.Position <- lm(
  formula = Salary ~ poly(Level, 3),
  data = df.Position
)

summary(mdl.poly3.Position)

plt.Position.g3 <- geom_line(
  aes(
    x = df.Position$Level,
    y = predict(
      mdl.poly3.Position,
      newdata = df.Position
    )
  ),
  colour = "green",
  linewidth = 1
)

### GRADO 4 ###

mdl.poly4.Position <- lm(
  formula = Salary ~ poly(Level, 4),
  data = df.Position
)

summary(mdl.poly4.Position)

plt.Position.g4 <- geom_line(
  aes(
    x = df.Position$Level,
    y = predict(
      mdl.poly4.Position,
      newdata = df.Position
    )
  ),
  colour = "yellow",
  linewidth = 1
)

### GRADO 5 ###

mdl.poly5.Position <- lm(
  formula = Salary ~ poly(Level, 5),
  data = df.Position
)

summary(mdl.poly5.Position)

plt.Position.g5 <- geom_line(
  aes(
    x = df.Position$Level,
    y = predict(
      mdl.poly5.Position,
      newdata = df.Position
    )
  ),
  colour = "orange",
  linewidth = 1
)


### GRÁFICO BASE ###

plt.Position <- ggplot() +
  theme_light() +
  geom_point(
    aes(
      x = df.Position$Level,
      y = df.Position$Salary
    )
  ) +
  xlab("Nivel de puesto") +
  ylab("Salario")

### GRÁFICOS COMPARATIVOS ###

plt.G1 <- plt.Position + plt.Position.g1 + ggtitle("Grado 1")
plt.G2 <- plt.Position + plt.Position.g2 + ggtitle("Grado 2")
plt.G3 <- plt.Position + plt.Position.g3 + ggtitle("Grado 3")
plt.G4 <- plt.Position + plt.Position.g4 + ggtitle("Grado 4")
plt.G5 <- plt.Position + plt.Position.g5 + ggtitle("Grado 5")

plt.grid <- plot_grid(
  plt.G1,
  plt.G2,
  plt.G3,
  plt.G4,
  plt.G5,
  ncol = 2
)

ggsave(
  filename = "Polinomial_lin.jpg",
  plot = plt.grid,
  units = "in",
  height = 7,
  width = 14
)

### MODELO POLINOMIAL PERSONALIZADO ###

mdl.raro.Position <- lm(
  formula = Salary ~ Level +
    I(Level^2) +
    I(Level^4),
  data = df.Position
)

summary(mdl.raro.Position)

plt.Position.raro <-
  plt.Position +
  geom_line(
    aes(
      x = df.Position$Level,
      y = predict(
        mdl.raro.Position,
        newdata = df.Position
      )
    ),
    colour = "orange",
    linewidth = 1.5
  )

plt.Position.raro

# REGRESION CON VECTORES DE SOPORTE

svr.lin <- svm(
  formula = Salary ~ .,
  data = df.Position,
  kernel = "linear",
  type = "eps-regression"
)

summary(svr.lin)

svr.poly <- svm(
  formula = Salary ~ .,
  data = df.Position,
  kernel = "polynomial",
  type = "eps-regression"
)

summary(svr.poly)

svr.rad <- svm(
  formula = Salary ~ .,
  data = df.Position,
  kernel = "radial",
  type = "eps-regression"
)

summary(svr.rad)

svr.sig <- svm(
  formula = Salary ~ .,
  data = df.Position,
  kernel = "sigmoid",
  type = "eps-regression"
)

summary(svr.sig)

### COMPARACIÓN SVR ###

plt.svr <- ggplot() +
  theme_light() +
  geom_point(
    aes(
      x = df.Position$Level,
      y = df.Position$Salary
    )
  ) +
  xlab("Nivel de puesto") +
  ylab("Salario") +
  
  geom_line(
    aes(
      x = df.Position$Level,
      y = predict(svr.lin, df.Position)
    ),
    colour = "dodgerblue",
    linewidth = 1.5
  ) +
  
  geom_line(
    aes(
      x = df.Position$Level,
      y = predict(svr.poly, df.Position)
    ),
    colour = "green",
    linewidth = 1.5
  ) +
  
  geom_line(
    aes(
      x = df.Position$Level,
      y = predict(svr.rad, df.Position)
    ),
    colour = "orange",
    linewidth = 1.5
  ) +
  
  geom_line(
    aes(
      x = df.Position$Level,
      y = predict(svr.sig, df.Position)
    ),
    colour = "deeppink3",
    linewidth = 1.5
  )

plt.svr

### R² Y R² AJUSTADO ###

Y.pred <- predict(
  svr.rad,
  newdata = df.Position
)

Y.mean <- mean(df.Position$Salary)

SSt <- sum(
  (df.Position$Salary - Y.mean)^2
)

SSr <- sum(
  (df.Position$Salary - Y.pred)^2
)

R.squared <- 1 - (SSr / SSt)

n <- nrow(df.Position)
p <- ncol(df.Position) - 1

Adj.R.squared <- 1 -
  (1 - R.squared) *
  ((n - 1) / (n - p - 1))

R.squared
Adj.R.squared

