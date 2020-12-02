

setwd("E:/Documentos/ING_BIOMEDICA/Cuarto Año/Segundo Cuatrimestre/Introducción a la Ciencia de Datos/Student-ICD")

portugues <- read.table("student-por.csv", sep = ",", header = TRUE)

lm.portugues <- lm(G3~., data = portugues)
summary(lm)

library(ggplot2)

ggplot(lm.portugues, aes(x = G2, y = G3)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

pca <- prcomp(t(portugues), scale = TRUE)

    