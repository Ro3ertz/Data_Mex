#1 Carga del dataset
library(tidyverse)
data_mundial <- read.csv("https://bitsandbricks.github.io/data/gapminder.csv")
data_mundial
summary(data_mundial)

#2 Transformacion

#Mexico
data_mex <- data_mundial %>% 
  filter(pais  == "Mexico")
data_mex
summary(data_mex)

#3 EDA

ggplot(data = data_mex) + 
  geom_point(aes(x = anio, y = expVida)) +
  labs(title = "Correlación entre tiempo y expectativa de vida",
       subtitle = "México",
       y = "expectativa de vida")


#4 Modelado

#Sin usar geom_smooth
cor(data_mex$anio,data_mex$expVida)

modelo_exp <- lm(expVida ~ anio, data = data_mex)
modelo_exp

ggplot(data = data_mex) + 
  geom_point(aes(x = anio, y = expVida)) +
  labs(title = "Correlación entre tiempo y expectativa de vida",
       subtitle = "México",
       y = "expectativa de vida") + 
  geom_abline(aes(intercept = -827.415, slope = 0.451), color = "blue") +
  xlim(c(1950, 2050))+
  ylim(c(50, 100))

#Usando geom_smooth
ggplot(data = data_mex) + 
  geom_point(aes(x = anio, y = expVida)) +
  labs(title = "Correlación entre tiempo y expectativa de vida",
       subtitle = "Mexico",
       y = "expectativa de vida",
       caption = "con línea de regresión vía geom_smooth()") +
  geom_smooth(aes(x = anio, y = expVida), method = "lm")

# Existe correlacion entre la exp de vida y PBI
ggplot(data = data_mex) + 
  geom_point(aes(x = anio, y = PBI_PC)) +
  labs(title = "Correlacion entre el PBI y expectativa de vida",
       subtitle = "Mexico", 
       y = "PBI per capital") +
  geom_smooth(aes(x = anio, y = PBI_PC), method = "lm")

cor(data_mex$expVida,data_mex$PBI_PC)

modelo_PBI <- lm(PBI_PC ~ anio, data = data_mex)
modelo_PBI
residuos <- residuals(modelo_PBI)
residuos
data_mex <- data_mex %>% mutate(residuo_ml = residuos)
ggplot(data_mex) +
  geom_point(aes(x = anio, y = residuo_ml)) +
  geom_hline(yintercept = 0, col = "blue") +
  labs(x = "año", y = "residuo del modelo lineal")

ggplot(data_mex) +
  geom_line(aes(x = anio, y = PBI_PC)) +
  labs(title = "Evolución del PBI en México",
       y = "PBI per cápita")
