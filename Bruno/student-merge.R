

##importe de los datos

library(ggplot2)
library(GGally)
library(stats)
library(FactoMineR)
library(factoextra)

setwd("E:/Bruno/Favaloro/4 Año/2do Cuatrimestre/ICD/ProyectoFinal/Student-ICD")

d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)

d3=merge(d1,d2,by = c("school","sex","age","address","famsize","Pstatus","Medu",
                      "Fedu","Mjob","Fjob","reason","nursery","internet"))

print(nrow(d3)) # 382 students

######################### Primera Regresion Logistica ##########################

#Simple Regresion Linear para identificar si hay algun tipo de variables
#significativas
lm.linear <- lm(G3~.,data=d1)
summary(lm.linear)

#
forw<-regsubsets(G3~.,data = d1, method = "forward")
summary(forw)
par(mfrow=c(2,2))
plot(summary(forw)$rss,pch=20,xlab="Modelo", ylab= "RSS")
plot(summary(forw)$rsq,pch=20,xlab="Modelo", ylab= "R^2")
plot(summary(forw)$adjr2,pch=20,xlab="Modelo", ylab= "R^2 aj")
plot(1:8,summary(forw)$cp,pch=20,xlab="Modelo", ylab= "CP")
abline(0,1)

#Famrel+Absences+G1+G2


lm.linear <- lm((G3.x+G3.y)~.,data=d3)
summary(lm.linear)
