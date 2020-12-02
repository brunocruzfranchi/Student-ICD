##importe de los datos

library(ggplot2)
library(GGally)
library(stats)
library(FactoMineR)
library(factoextra)

setwd('C:/Users/bruno/OneDrive/Escritorio/Bruno/Student-ICD')

d2=read.table("student-por.csv",sep=",",header=TRUE)
d2$G1 <- d2$G1+1
d2$G2 <- d2$G2+1
d2$G3 <- d2$G3+1
d3=merge(d1,d2,by = c("school","sex","age","address","famsize","Pstatus","Medu",
                      "Fedu","Mjob","Fjob","reason","nursery","internet"))

print(nrow(d3)) # 382 students

########################## Arreglo de los datasets #############################

Por = read.csv("student-por.csv",sep=",",header=TRUE)

Por$school <- factor(Por$school)
Por$sex <- factor(Por$sex)
Por$address <- factor(Por$address)+
Por$famsize <- factor(Por$famsize)
Por$Pstatus <- factor(Por$Pstatus)
Por$Mjob <- factor(Por$Mjob)
Por$Fjob <- factor(Por$Fjob)
Por$reason <- factor(Por$reason)
Por$guardian <- factor(Por$guardian)
Por$schoolsup <- factor(Por$schoolsup)
Por$famsup <- factor(Por$famsup)
Por$paid <- factor(Por$Pstatus)
Por$activities <- factor(Por$activities)
Por$nursery <- factor(Por$nursery)
Por$higher <- factor(Por$higher)
Por$internet <- factor(Por$internet)
Por$romantic <- factor(Por$romantic)
Por$higher <- factor(Por$higher)

######################### Primera Regresion ##########################

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

######################### Primera Regresion  ##########################

## Separacion de datos en training y testing
library(caTools)
set.seed(123)   
sample = sample.split(d1,SplitRatio = 0.70) 
train =subset(d1,sample == TRUE)
test=subset(d1, sample == FALSE)

lm.linear <- lm(G3~., data = train)

summary(lm.linear)

y_hat <- predict(lm.linear, newdata = test)

ecm <- mean((test$G3-round(y_hat)))

