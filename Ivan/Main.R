
############### Abro los datos ###############
setwd("E:/Documentos/ING_BIOMEDICA/Cuarto Año/Segundo Cuatrimestre/Introducción a la Ciencia de Datos/Student-ICD")

Por = read.csv("student-por.csv",sep=",",header=TRUE)

Por$school <- factor(Por$school)
Por$sex <- factor(Por$sex)
Por$address <- factor(Por$address)
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

############### Regresion Lineal ###############

lm.linear <- lm(G3~.,data = Por)
summary(lm.linear)
Por <- data.frame(Por)

#
forw<-regsubsets(G3~.,data = Por, method = "forward")
summary(forw)
par(mfrow=c(2,2))
plot(summary(forw)$rss,pch=20,xlab="Modelo", ylab= "RSS")
plot(summary(forw)$rsq,pch=20,xlab="Modelo", ylab= "R^2")
plot(summary(forw)$adjr2,pch=20,xlab="Modelo", ylab= "R^2 aj")
plot(summary(forw)$cp,pch=20,xlab="Modelo", ylab= "CP")
abline(0,1)

#Famrel+Absences+G1+G2

lm.linear <- lm((G3.x+G3.y)~.,data=d3)
summary(lm.linear)

############### Regresion Stepwise ###############
library(MASS)
library(caret)
set.seed(123)
res.lm <- lm(G3~., data = Por)
step <- stepAIC(res.lm, direction = "both", trace = FALSE)
step.model <- train(G3~., data = Por,
                    method = "lmStepAIC", 
                    trControl = train.control,
                    trace = FALSE)
# Model accuracy
step.model$results
# Final model coefficients
step.model$finalModel
# Summary of the model
summary(step.model$finalModel)



