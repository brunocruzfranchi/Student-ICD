#_________________________ Inicializacion del archivo ##########################

library(ggplot2)
library(GGally)
library(stats)
library(FactoMineR)
library(factoextra)
library(leaps)
library(caTools)
library(RColorBrewer)
library(glmnet)
library(ISLR)
library(coefplot)
library(tidyverse)
library(skimr)
library(DataExplorer)
library(dplyr)
library(broom)

setwd("E:/Bruno/Favaloro/4 A�o/2do Cuatrimestre/ICD/ProyectoFinal/Student-ICD")

#_________________________ Graficos #####################################

## Esto setea todos los parametros de fuentes a utilizar en las graficas

mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                      legend.title = element_text(colour = "gray28",  face = "bold.italic", family = "Helvetica"), 
                      legend.text = element_text(face = "italic", colour="gray28",family = "Helvetica"), 
                      axis.title = element_text(family = "Helvetica", size = (11), colour = "steelblue4"),
                      axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (11)),
                      plot.tag = element_text(family = "Helvetica", face = "bold", size = (11)),
                      axis.text.x=element_blank(),
                      #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
                      #legend.position = c(.95, .95),
                      #legend.justification = c("right", "top"),
                      #legend.box.just = "right",
                      #legend.margin = margin(6, 6, 6, 6)
)

mynamesthemev2 <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                      legend.title = element_text(colour = "gray28",  face = "bold.italic", family = "Helvetica"), 
                      legend.text = element_text(face = "italic", colour="gray28",family = "Helvetica"), 
                      axis.title = element_text(family = "Helvetica", size = (11), colour = "steelblue4"),
                      axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (11)),
                      plot.tag = element_text(family = "Helvetica", face = "bold", size = (11)),
                      #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
                      #legend.position = c(.95, .95),
                      #legend.justification = c("right", "top"),
                      #legend.box.just = "right",
                      #legend.margin = margin(6, 6, 6, 6)
)

#_________________________ Arreglo de los datasets ############################

Math = read.table("student-mat.csv",sep=",",header=TRUE)

Math$school <- factor(Math$school)
Math$sex <- factor(Math$sex)
Math$address <- factor(Math$address)
Math$famsize <- factor(Math$famsize)
Math$Pstatus <- factor(Math$Pstatus)
Math$Mjob <- factor(Math$Mjob)
Math$Fjob <- factor(Math$Fjob)
Math$reason <- factor(Math$reason)
Math$guardian <- factor(Math$guardian)
Math$schoolsup <- factor(Math$schoolsup)
Math$famsup <- factor(Math$famsup)
Math$paid <- factor(Math$paid)
Math$activities <- factor(Math$activities)
Math$nursery <- factor(Math$nursery)
Math$higher <- factor(Math$higher)
Math$internet <- factor(Math$internet)
Math$romantic <- factor(Math$romantic)

Por = read.csv("student-por.csv",sep=",",header=TRUE)
Por = Por[-18]

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
#Por$paid <- factor(Por$Pstatus) Error 
Por$activities <- factor(Por$activities)
Por$nursery <- factor(Por$nursery)
Por$higher <- factor(Por$higher)
Por$internet <- factor(Por$internet)
Por$romantic <- factor(Por$romantic)
Por$higher <- factor(Por$higher)

#_________________________ Regresion lineal normal #############################

set.seed(123)
sample = sample.split(Math, SplitRatio = 0.80) 
Math.train = subset(Math, sample == TRUE)
Math.test = subset(Math, sample == FALSE)

sample = sample.split(Por, SplitRatio = 0.80) 
Por.train = subset(Math, sample == TRUE)
Por.test = subset(Math, sample == FALSE)

Math.lineal <- lm(G3~., data = Math.train)
Por.lineal <- lm(G3~., data = Por.train)

Math.pred.lineal.train <- predict(Math.lineal, newdata = Math.train)
Math.pred.lineal <- predict(Math.lineal, newdata = Math.test)

aux <- sqrt((Math.test$G3 - Math.pred.lineal)^2)*5
em.lineal <- mean(aux) # ya esta en porcentaje (ej: 3 es 3%)

mse <- mean((Math.test$G3-Math.pred.lineal)^2)
mse <- mean((Math.train$G3-Math.pred.lineal.train)^2)

#_________________________ Hybrid, Forward, Backward #########################

df.frecuencia <- function(nombre_dataset, n_umbral, iteraciones, metodo){
  if (nombre_dataset == 'Math'){
    n <- 42
    matrix_var <- rep(0L, n)
    variables_nombres <- c("(Intercept)","schoolMS","sexM","age","addressU","famsizeLE3", "PstatusT","Medu",
                           "Fedu","Mjobhealth","Mjobother","Mjobservices","Mjobteacher","Fjobhealth", "Fjobother","Fjobservices",
                           "Fjobteacher", "reasonhome", "reasonother", "reasonreputation", "guardianmother","guardianother",
                           "traveltime", "studytime", "failures","schoolsupyes",  "famsupyes",  "paidyes"
                           ,"activitiesyes","nurseryyes", "higheryes", "internetyes", "romanticyes", "famrel",
                           "freetime","goout", "Dalc", "Walc", "health","absences","G1","G2")
    dataset <- Math
  }
  else{
    n <- 41
    matrix_var <- rep(0L, n)
    variables_nombres <- c("(Intercept)","schoolMS","sexM","age","addressU","famsizeLE3",
                           "PstatusT","Medu","Fedu",
                           "Mjobhealth","Mjobother","Mjobservices","Mjobteacher","Fjobhealth",
                           "Fjobother","Fjobservices",
                           "Fjobteacher","reasonhome","reasonother","reasonreputation"
                           ,"guardianmother","guardianother",
                           "traveltime","studytime","failures","schoolsupyes","famsupyes","activitiesyes","nurseryyes",
                           "higheryes","internetyes",
                           "romanticyes","famrel","freetime","goout","Dalc","Walc","health","absences","G1","G2")
    dataset <- Por
  }
  
  for (k in 1:iteraciones) {
    set.seed(k)  
    sample = sample.split(dataset, SplitRatio = 0.80) 
    train = subset(dataset, sample == TRUE)
    
    reg.var<-regsubsets(G3~., nbest=1, nvmax=n,
                        force.in=NULL, force.out=NULL, intercept=TRUE,
                        method=c(metodo), data = train)
    
    reg.summary = summary(reg.var)
    
    cp_min = which.min(reg.summary$cp)
    
    n_aux <- c()
    n_aux <- names(which(reg.summary$which[cp_min,]))
    
    for (i in 1:length(n_aux)) {
      aux <- n_aux[i]
      for (j in 1:n) {
        if(aux == variables_nombres[j])
          matrix_var[j] = matrix_var[j] + 1
      }
    }
    
  }
  
  #Obtengo aquello que tengan mayor relevancia de acuerdo con un umbral
  
  umbral <- max(matrix_var)*n_umbral
  
  df <- data.frame(Variables = variables_nombres[matrix_var>umbral],
                   Frecuencia = matrix_var[matrix_var>umbral])
  
  return(df)
}

#____________ Matem�tica

df.Math.forward <- df.frecuencia('Math', 0.50, 400, "forward")
df.Math.backward <- df.frecuencia('Math', 0.50, 400, "backward")
df.Math.hybrid <- df.frecuencia('Math', 0.50, 400, "seqrep")

getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Plot
g1 <- ggplot(data = df.Math.forward, mapping = aes(x = Variables, y = Frecuencia, fill = Variables)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Frecuencia), position = position_stack(vjust= 0.5),
            colour = "black", size = 3)+
  scale_fill_manual(values = getPalette(nrow(df.Math.forward)))+
  theme_light()+
  theme_minimal()

g2 <- ggplot(data = df.Math.backward, mapping = aes(x = Variables, y = Frecuencia, fill = Variables)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Frecuencia), position = position_stack(vjust= 0.5),
            colour = "black", size = 3)+
  scale_fill_manual(values = getPalette(nrow(df.Math.backward)))+
  theme_light()+
  theme_minimal()

g3 <- ggplot(data = df.Math.hybrid, mapping = aes(x = Variables, y = Frecuencia, fill = Variables)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Frecuencia), position = position_stack(vjust= 0.5),
            colour = "black", size = 3)+
  scale_fill_manual(values = getPalette(nrow(df.Math.hybrid)))+
  theme_light()+
  theme_minimal()

print(g1 + mynamestheme + labs( title= "Matem�tica - Forward", y="Frecuencia", 
                                x = "Variables"))
print(g2 + mynamestheme + labs( title= "Matem�tica - Backward", y="Frecuencia", 
                                x = "Variables"))
print(g3 + mynamestheme + labs( title= "Matem�tica - Hybrid", y="Frecuencia", 
                                x = "Variables"))
#________________ Portugu�s


df.Por.forward <- df.frecuencia('Por', 0.50, 400, "forward")
df.Por.backward <- df.frecuencia('Por', 0.50, 400, "backward")
df.Por.hybrid <- df.frecuencia('Por', 0.50, 400, "seqrep")

# Plot
g4 <- ggplot(data = df.Por.forward, mapping = aes(x = Variables, y = Frecuencia, fill = Variables)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Frecuencia), position = position_stack(vjust= 0.5),
            colour = "black", size = 3)+
  scale_fill_manual(values = getPalette(nrow(df.Por.forward)))+
  theme_light()+
  theme_minimal()

g5 <- ggplot(data = df.Por.backward, mapping = aes(x = Variables, y = Frecuencia, fill = Variables)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Frecuencia), position = position_stack(vjust= 0.5),
            colour = "black", size = 3)+
  scale_fill_manual(values = getPalette(nrow(df.Por.backward)))+
  theme_light()+
  theme_minimal()

g6 <- ggplot(data = df.Por.hybrid, mapping = aes(x = Variables, y = Frecuencia, fill = Variables)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Frecuencia), position = position_stack(vjust= 0.5),
            colour = "black", size = 3)+
  scale_fill_manual(values = getPalette(nrow(df.Por.hybrid)))+
  theme_light()+
  theme_minimal()

print(g4 + mynamestheme + labs( title= "Portugu�s - Forward", y="Frecuencia", 
                                x = "Variables"))
print(g5 + mynamestheme + labs( title= "Portugu�s - Backward", y="Frecuencia", 
                                x = "Variables"))
print(g6 + mynamestheme + labs( title= "Portugu�s - Hybrid", y="Frecuencia", 
                                x = "Variables"))

#_________________________ Calculo del Error ###################################

set.seed(123)

#___ Armo los datasets para testeo y training
sample = sample.split(Math,SplitRatio = 0.80) 
Math.train = subset(Math,sample == TRUE)
Math.test = subset(Math, sample == FALSE)

#___ Armo los datasets para testeo y training
sample = sample.split(Por,SplitRatio = 0.80) 
Por.train = subset(Por,sample == TRUE)
Por.test = subset(Por, sample == FALSE)

error_prediccion <- function(predicciones, dataset){
  for (i in 1:length(predicciones)) {
    if ((predicciones[i]-trunc(predicciones[i])) <= 0.5) {
      predicciones[i] <- floor(predicciones[i])
    }
    else
      predicciones[i] <- ceiling(predicciones[i])
  }
  
  mse <- mean((dataset$G3 - predicciones)^2)

  return(mse) 
}

#______________ Matem�tica

Math.lineal.forward <- lm(G3~absences+activities+age+famrel+Fjob+
                            G1+G2,data=Math.train)
Math.pred.forward <- predict(Math.lineal.forward, newdata = Math.test)

Math.lineal.backward <- lm(G3~absences+activities+age+famrel+G1+G2,data=Math.train)
Math.pred.backward <- predict(Math.lineal.backward, newdata = Math.test)

Math.lineal.hybrid <- lm(G3~absences+activities+age+famrel+Fjob+
                             G1+G2+Walc,data=Math.train)
Math.pred.hybrid <- predict(Math.lineal.hybrid, newdata = Math.test)

error.Math <- c(error_prediccion(Math.pred.forward,Math.test),
                error_prediccion(Math.pred.backward,Math.test),
                error_prediccion(Math.pred.hybrid,Math.test))
error.Math

#______________ Portugu�s

Por.lineal.forward <- lm(G3~absences+failures+G1+G2+reason+Mjob+
                           sex+traveltime,data=Por.train)
Por.pred.forward <- predict(Por.lineal.forward, newdata = Por.test)

Por.lineal.backward <- lm(G3~failures+G1+G2+reason+school+
                            sex+traveltime,data=Por.train)
Por.pred.backward <- predict(Por.lineal.backward, newdata = Por.test)

Por.lineal.hybrid <- lm(G3~failures+G1+G2+Mjob+reason+
                            sex+traveltime,data=Por.train)
Por.pred.hybrid <- predict(Por.lineal.hybrid, newdata = Por.test)

error.Por <- c(error_prediccion(Por.pred.forward,Por.test),
                error_prediccion(Por.pred.backward,Por.test),
                error_prediccion(Por.pred.hybrid,Por.test))
error.Por

#______________ Graficos

df.errores <- data.frame(Error=c(error.Math,error.Por),
                         Dataset=c('Matem�tica','Matem�tica','Matem�tica',
                                   'Portugu�s','Portugu�s','Portugu�s'),
                         Metodo=c('Forward','Backward','Hybrid',
                                  'Forward','Backward','Hybrid'))

# Plot
g7 <- ggplot(data = df.errores, mapping = aes(x = Dataset, y = Error, fill = Metodo)) +
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_brewer(palette = "Set1")+
  theme_light()+
  theme_minimal()

print(g7 + mynamesthemev2 + labs( title= "Errores - Selecci�n Stepwise", 
                                  y="Error", 
                                  x = "Datasets"))

#_________________________ Calculo del Error CV ################################

#_____________ Matem�tica

error_prediccion <- function(predicciones, dataset){
  for (i in 1:length(predicciones)) {
    if ((predicciones[i]-trunc(predicciones[i])) <= 0.5) {
      predicciones[i] <- floor(predicciones[i])
    }
    else
      predicciones[i] <- ceiling(predicciones[i])
  }
  
  mse <- mean((dataset$G3 - predicciones)^2)
  
  return(mse) 
}

errores.cv.Math <- matrix(0, nrow=nrow(Math), ncol=3)
colnames(errores.cv.Math) <- c('forward','backward','hybrid')

for(i in 1:nrow(Math)){
  Math.t <- Math[-i,]  
  Math.lineal.forward <- lm(G3~absences+activities+age+famrel+Fjob+G1+G2,data=Math.t)
  Math.lineal.backward <- lm(G3~absences+activities+age+famrel+G1+G2,data=Math.t)
  Math.lineal.hybrid <- lm(G3~absences+activities+age+famrel+Fjob+G1+G2+Walc,data=Math.t)
  
  Math.pred.forward <- predict(Math.lineal.forward, newdata = Math.t)
  Math.pred.backward <- predict(Math.lineal.backward, newdata = Math.t)
  Math.pred.hybrid <- predict(Math.lineal.hybrid, newdata = Math.t)
  
  errores.cv.Math[i,] <- c(error_prediccion(Math.pred.forward,Math.t),
                  error_prediccion(Math.pred.backward,Math.t),
                  error_prediccion(Math.pred.hybrid,Math.t))
}

errores.cv.Math <- apply(errores.cv.Math, 2, mean)
errores.cv.Math

#_____________ Portugu�s

error_prediccion <- function(predicciones, dataset){
  for (i in 1:length(predicciones)) {
    if ((predicciones[i]-trunc(predicciones[i])) <= 0.5) {
      predicciones[i] <- floor(predicciones[i])
    }
    else
      predicciones[i] <- ceiling(predicciones[i])
  }
  
  mse <- mean((dataset$G3 - predicciones)^2)
  
  return(mse) 
}

errores.cv.Por <- matrix(0, nrow=nrow(Por), ncol=3)
colnames(errores.cv.Por) <- c('forward','backward','hybrid')

for(i in 1:nrow(Por)){
  Por.t <- Por[-i,]  
  Por.lineal.forward <- lm(G3~absences+failures+G1+G2+reason+Mjob+sex+traveltime,data=Por.t)
  Por.lineal.backward <- lm(G3~failures+G1+G2+reason+school+sex+traveltime,data=Por.t)
  Por.lineal.hybrid <- lm(G3~failures+G1+G2+Mjob+reason+sex+traveltime,data=Por.t)
  
  Por.pred.forward <- predict(Por.lineal.forward, newdata = Por.t)
  Por.pred.backward <- predict(Por.lineal.backward, newdata = Por.t)
  Por.pred.hybrid <- predict(Por.lineal.hybrid, newdata = Por.t)
  
  errores.cv.Por[i,] <- c(error_prediccion(Por.pred.forward,Por.t),
                           error_prediccion(Por.pred.backward,Por.t),
                           error_prediccion(Por.pred.hybrid,Por.t))
}

errores.cv.Por <- apply(errores.cv.Por, 2, mean)
errores.cv.Por

#______________ Graficos

df.errores.cv <- data.frame(Error=c(errores.cv.Math,errores.cv.Por),
                         Dataset=c('Matem�tica','Matem�tica','Matem�tica',
                                   'Portugu�s','Portugu�s','Portugu�s'),
                         Metodo=c('Forward','Backward','Hybrid',
                                  'Forward','Backward','Hybrid'))
# Plot
g11 <- ggplot(data = df.errores.cv, mapping = aes(x = Dataset, y = Error, fill = Metodo)) +
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_brewer(palette = "Set1")+
  theme_light()+
  theme_minimal()

print(g11 + mynamesthemev2 + labs( title= "Errores CV - Selecci�n Stepwise", 
                                  y="ECM", 
                                  x = "Datasets"))

#_________________________ Analisis Lasso solo de Variables #############################

df.frecuencia.lasso <- function(nombre_dataset, n_umbral, iteraciones){
  if (nombre_dataset == 'Math'){
    n <- 42
    matrix_var <- rep(0L, n)
    variables_nombres <- c("(Intercept)","schoolMS","sexM","age","addressU","famsizeLE3", "PstatusT","Medu",
                           "Fedu","Mjobhealth","Mjobother","Mjobservices","Mjobteacher","Fjobhealth", "Fjobother","Fjobservices",
                           "Fjobteacher", "reasonhome", "reasonother", "reasonreputation", "guardianmother","guardianother",
                           "traveltime", "studytime", "failures","schoolsupyes",  "famsupyes",  "paidyes"
                           ,"activitiesyes","nurseryyes", "higheryes", "internetyes", "romanticyes", "famrel",
                           "freetime","goout", "Dalc", "Walc", "health","absences","G1","G2")
    dataset <- Math
  }
  else{
    n <- 41
    matrix_var <- rep(0L, n)
    variables_nombres <- c("(Intercept)","schoolMS","sexM","age","addressU","famsizeLE3",
                           "PstatusT","Medu","Fedu",
                           "Mjobhealth","Mjobother","Mjobservices","Mjobteacher","Fjobhealth",
                           "Fjobother","Fjobservices",
                           "Fjobteacher","reasonhome","reasonother","reasonreputation"
                           ,"guardianmother","guardianother",
                           "traveltime","studytime","failures","schoolsupyes","famsupyes","activitiesyes","nurseryyes",
                           "higheryes","internetyes",
                           "romanticyes","famrel","freetime","goout","Dalc","Walc","health","absences","G1","G2")
    dataset <- Por
  }
  
  for (k in 1:iteraciones) {
    set.seed(k)  
    sample = sample.split(dataset, SplitRatio = 0.80) 
    train = subset(dataset, sample == TRUE)
    test = subset(dataset, sample == FALSE)
    
    x_train <- model.matrix(G3~., data = train)[, -1]
    y_train <- train$G3
    
    x_test <- model.matrix(G3~., data = test)[, -1]
    y_test <- test$G3
    
    
    cv_output <- cv.glmnet(x_train, y_train, 
                           alpha = 1, nfolds = 10, 
                           type.measure = "mse",
                           standardize = TRUE)
    
    best_lam <- cv_output$lambda.min
    
    lasso_modelo <- glmnet(x_train, y_train,
                         alpha = 1, lambda = best_lam,
                         standardize = TRUE)
    
    n_aux <- c()
    n_aux <- variables_nombres[coef(lasso_modelo)@i+1]
    
    for (i in 1:length(n_aux)) {
      aux <- n_aux[i]
      for (j in 1:n) {
        if(aux == variables_nombres[j])
          matrix_var[j] = matrix_var[j] + 1
      }
    }
    
  }
  
  #Obtengo aquello que tengan mayor relevancia de acuerdo con un umbral
  
  umbral <- max(matrix_var)*n_umbral
  
  df <- data.frame(Variables = variables_nombres[matrix_var>umbral],
                   Frecuencia = matrix_var[matrix_var>umbral])
  
  return(df)
}

#______________ Matem�tica

df.Math.Lasso <- df.frecuencia.lasso('Math', 0.25 ,400)

colourCount = length(nrow(df.Math.Lasso))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Plot
g8 <- ggplot(data = df.Math.Lasso, mapping = aes(x = Variables, y = Frecuencia, fill = Variables)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Frecuencia), position = position_stack(vjust= 0.5),
            colour = "black", size = 3)+
  scale_fill_manual(values = getPalette(nrow(df.Math.Lasso)))+
  theme_light()+
  theme_minimal()

print(g8 + mynamestheme + labs( title= "Matem�tica - Lasso", y="Frecuencia", 
                                x = "Variables"))
      
#_______________ Portugu�s

df.Por.Lasso <- df.frecuencia.lasso('Por', 0.25 ,400)

colourCount = length(nrow(df.Por.Lasso))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Plot
g9 <- ggplot(data = df.Por.Lasso, mapping = aes(x = Variables, y = Frecuencia, fill = Variables)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Frecuencia), position = position_stack(vjust= 0.5),
            colour = "black", size = 3)+
  scale_fill_manual(values = getPalette(nrow(df.Por.Lasso)))+
  theme_light()+
  theme_minimal()

print(g9 + mynamestheme + labs( title= "Portugu�s - Lasso", y="Frecuencia", 
                                x = "Variables"))


#_________________________ Regresion Lasso Error ###############################

#________ Matem�tica

set.seed(123)

sample = sample.split(Math, SplitRatio = 0.80) 
Math.train = subset(Math, sample == TRUE)
Math.test = subset(Math, sample == FALSE)

x_train <- model.matrix(G3~., data = Math.train)[, -1]
y_train <- Math.train$G3

x_test <- model.matrix(G3~., data = Math.test)[, -1]
y_test <- Math.test$G3


cv_output <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10, 
                       type.measure = "mse")

#cv_output[["glmnet.fit"]][["beta"]][,cv_output[["glmnet.fit"]][["lambda"]]==cv_output$lambda.min]

#Valor Minimo de MSE
min(cv_output$cvm)
#Valor Minimo de lambda
cv_output$lambda.min

# 1 st.error of min MSE
cv_output$cvm[cv_output$lambda == cv_output$lambda.1se]
# lambda for this MSE
cv_output$lambda.1se

best_lam <- cv_output$lambda.min

lasso_modelo <- glmnet(x_train, y_train,
                       alpha = 1, lambda = best_lam,
                       standardize = TRUE)

predicciones_train <- predict(lasso_modelo, newx = x_train)
predicciones_train <- predict(cv_output, s = cv_output$lambda.min , newx = x_train)
Math_training_mse <- mean((predicciones_train - y_train)^2)
paste("Error (mse) de entrenamiento:", Math_training_mse)


predicciones_test <- predict(lasso_modelo, newx = x_test)
predicciones_test <- predict(cv_output, s = cv_output$lambda.min , newx = x_test)
Math_test_mse <- mean((predicciones_test - y_test)^2)
paste("Error (mse) de test:", Math_test_mse)


rss <- sum((predicciones_test - y_test) ^ 2)
tss <- sum((predicciones_test - mean(predicciones_test)) ^ 2)
rsq <- 1 - rss/tss
rsq

#________ Portugu�s

cv_output <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10, 
                       type.measure = "mse")

#Valor Minimo de MSE
min(cv_output$cvm)
#Valor Minimo de lambda
cv_output$lambda.min

# 1 st.error of min MSE
cv_output$cvm[cv_output$lambda == cv_output$lambda.1se]
# lambda for this MSE
cv_output$lambda.1se

best_lam <- cv_output$lambda.min

lasso_modelo <- glmnet(x_train, y_train,
                       alpha = 1, lambda = best_lam)

predicciones_train <- predict(lasso_modelo, newx = x_train)
predicciones_train <- predict(cv_output, s = cv_output$lambda.min , newx = x_train)
Por_training_mse <- mean((predicciones_train - y_train)^2)
paste("Error (mse) de entrenamiento:", Por_training_mse)


predicciones_test <- predict(lasso_modelo, newx = x_test)
predicciones_test <- predict(cv_output, s = cv_output$lambda.min , newx = x_test)
Por_test_mse <- mean((predicciones_test - y_test)^2)
paste("Error (mse) de test:", Por_test_mse)

#_________________________ Regresion Lasso Error CV ###############################

error_prediccion <- function(predicciones, dataset){
  for (i in 1:length(predicciones)) {
    if ((predicciones[i]-trunc(predicciones[i])) <= 0.5) {
      predicciones[i] <- floor(predicciones[i])
    }
    else
      predicciones[i] <- ceiling(predicciones[i])
  }
  
  mse <- mean((dataset$G3 - predicciones)^2)
  
  return(mse) 
}

errores.cv.Math <- matrix(0, nrow=nrow(Math), ncol=3)
colnames(errores.cv.Math) <- c('forward','backward','hybrid')

for(i in 1:nrow(Math)){
  Math.t <- Math[-i,]  
  Math.lineal.forward <- lm(G3~absences+activities+age+famrel+Fjob+G1+G2,data=Math.t)
  Math.lineal.backward <- lm(G3~absences+activities+age+famrel+G1+G2,data=Math.t)
  Math.lineal.hybrid <- lm(G3~absences+activities+age+famrel+Fjob+G1+G2+Walc,data=Math.t)
  
  Math.pred.forward <- predict(Math.lineal.forward, newdata = Math.t)
  Math.pred.backward <- predict(Math.lineal.backward, newdata = Math.t)
  Math.pred.hybrid <- predict(Math.lineal.hybrid, newdata = Math.t)
  
  errores.cv.Math[i,] <- c(error_prediccion(Math.pred.forward,Math.t),
                           error_prediccion(Math.pred.backward,Math.t),
                           error_prediccion(Math.pred.hybrid,Math.t))
}

errores.cv.Math <- apply(errores.cv.Math, 2, mean)
errores.cv.Math

#_____________ Portugu�s

error_prediccion <- function(predicciones, dataset){
  for (i in 1:length(predicciones)) {
    if ((predicciones[i]-trunc(predicciones[i])) <= 0.5) {
      predicciones[i] <- floor(predicciones[i])
    }
    else
      predicciones[i] <- ceiling(predicciones[i])
  }
  
  mse <- mean((dataset$G3 - predicciones)^2)
  
  return(mse) 
}

errores.cv.Por <- matrix(0, nrow=nrow(Por), ncol=3)
colnames(errores.cv.Por) <- c('forward','backward','hybrid')

for(i in 1:nrow(Por)){
  Por.t <- Por[-i,]  
  Por.lineal.forward <- lm(G3~absences+failures+G1+G2+reason+Mjob+sex+traveltime,data=Por.t)
  Por.lineal.backward <- lm(G3~failures+G1+G2+reason+school+sex+traveltime,data=Por.t)
  Por.lineal.hybrid <- lm(G3~failures+G1+G2+Mjob+reason+sex+traveltime,data=Por.t)
  
  Por.pred.forward <- predict(Por.lineal.forward, newdata = Por.t)
  Por.pred.backward <- predict(Por.lineal.backward, newdata = Por.t)
  Por.pred.hybrid <- predict(Por.lineal.hybrid, newdata = Por.t)
  
  errores.cv.Por[i,] <- c(error_prediccion(Por.pred.forward,Por.t),
                          error_prediccion(Por.pred.backward,Por.t),
                          error_prediccion(Por.pred.hybrid,Por.t))
}

errores.cv.Por <- apply(errores.cv.Por, 2, mean)
errores.cv.Por

#_________________________ Graficos Lasso ######################
# Inspecting beta coefficients
coef(lasso_modelo)

lasso.modelo <- glmnet( x= x_train, y= y_train, alpha=1,
                  nlambda= 100,standardize = TRUE)

coefpath(lasso_mdl)

regularizacion <- lasso.modelo$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = lasso.modelo$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

g10 <- regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line(size=1) +
  geom_vline(xintercept = cv_output$lambda.min)+
  scale_x_log10(
    #breaks = trans_breaks("log10", function(x) 10^x),
    #labels = trans_format("log10", math_format(10^.x))
  ) +
  theme_light()+
  theme_minimal() 

print(g10 + mynamesthemev2 + labs( title= "Coeficientes del modelo en funci�n de la regularizaci�n", 
                                    y="Coeficientes", 
                                    x = "Lambda"))

coefplot(lasso.modelo, lambda=cv_output$lambda.min, sort="magnitude")




#_________________________ Regresion Ridge Error  #############################

Edf.frecuencia.ridge <- function(nombre_dataset, n_umbral, iteraciones){
  if (nombre_dataset == 'Math'){
    n <- 42
    matrix_var <- rep(0L, n)
    variables_nombres <- c("(Intercept)","schoolMS","sexM","age","addressU","famsizeLE3", "PstatusT","Medu",
                           "Fedu","Mjobhealth","Mjobother","Mjobservices","Mjobteacher","Fjobhealth", "Fjobother","Fjobservices",
                           "Fjobteacher", "reasonhome", "reasonother", "reasonreputation", "guardianmother","guardianother",
                           "traveltime", "studytime", "failures","schoolsupyes",  "famsupyes",  "paidyes"
                           ,"activitiesyes","nurseryyes", "higheryes", "internetyes", "romanticyes", "famrel",
                           "freetime","goout", "Dalc", "Walc", "health","absences","G1","G2")
    dataset <- Math
  }
  else{
    n <- 41
    matrix_var <- rep(0L, n)
    variables_nombres <- c("(Intercept)","schoolMS","sexM","age","addressU","famsizeLE3",
                           "PstatusT","Medu","Fedu",
                           "Mjobhealth","Mjobother","Mjobservices","Mjobteacher","Fjobhealth",
                           "Fjobother","Fjobservices",
                           "Fjobteacher","reasonhome","reasonother","reasonreputation"
                           ,"guardianmother","guardianother",
                           "traveltime","studytime","failures","schoolsupyes","famsupyes","activitiesyes","nurseryyes",
                           "higheryes","internetyes",
                           "romanticyes","famrel","freetime","goout","Dalc","Walc","health","absences","G1","G2")
    dataset <- Por
  }
  
  for (k in 1:iteraciones) {
    set.seed(k)  
    sample = sample.split(dataset, SplitRatio = 0.80) 
    train = subset(dataset, sample == TRUE)
    test = subset(dataset, sample == FALSE)
    
    x_train <- model.matrix(G3~., data = train)[, -1]
    y_train <- train$G3
    
    x_test <- model.matrix(G3~., data = test)[, -1]
    y_test <- test$G3
    
    
    cv_output <- cv.glmnet(x_train, y_train, 
                           alpha = 0, nfolds = 10, 
                           type.measure = "mse",
                           standardize = TRUE)
    
    best_lam <- cv_output$lambda.min
    
    lasso_modelo <- glmnet(x_train, y_train,
                           alpha = 1, lambda = best_lam,
                           standardize = TRUE)
    
    n_aux <- c()
    n_aux <- variables_nombres[coef(lasso_modelo)@i+1]
    
    for (i in 1:length(n_aux)) {
      aux <- n_aux[i]
      for (j in 1:n) {
        if(aux == variables_nombres[j])
          matrix_var[j] = matrix_var[j] + 1
      }
    }
    
  }
  
  #Obtengo aquello que tengan mayor relevancia de acuerdo con un umbral
  
  umbral <- max(matrix_var)*n_umbral
  
  df <- data.frame(Variables = variables_nombres[matrix_var>umbral],
                   Frecuencia = matrix_var[matrix_var>umbral])
  
  return(df)
}

#______________ Matem�tica

df.Math.Lasso <- df.frecuencia.lasso('Math', 0.25 ,400)

colourCount = length(nrow(df.Math.Lasso))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Plot
g8 <- ggplot(data = df.Math.Lasso, mapping = aes(x = Variables, y = Frecuencia, fill = Variables)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Frecuencia), position = position_stack(vjust= 0.5),
            colour = "black", size = 3)+
  scale_fill_manual(values = getPalette(nrow(df.Math.Lasso)))+
  theme_light()+
  theme_minimal()

print(g8 + mynamestheme + labs( title= "Matem�tica - Lasso", y="Frecuencia", 
                                x = "Variables"))

#_______________ Portugu�s

df.Por.Lasso <- df.frecuencia.lasso('Por', 0.25 ,400)

colourCount = length(nrow(df.Por.Lasso))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Plot
g9 <- ggplot(data = df.Por.Lasso, mapping = aes(x = Variables, y = Frecuencia, fill = Variables)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Frecuencia), position = position_stack(vjust= 0.5),
            colour = "black", size = 3)+
  scale_fill_manual(values = getPalette(nrow(df.Por.Lasso)))+
  theme_light()+
  theme_minimal()

print(g9 + mynamestheme + labs( title= "Portugu�s - Lasso", y="Frecuencia", 
                                x = "Variables"))

#________ Matem�tica

set.seed(123)

sample = sample.split(Math, SplitRatio = 0.80) 
Math.train = subset(Math, sample == TRUE)
Math.test = subset(Math, sample == FALSE)

x_train <- model.matrix(G3~., data = Math.train)[, -1]
y_train <- Math.train$G3

x_test <- model.matrix(G3~., data = Math.test)[, -1]
y_test <- Math.test$G3


cv_output <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 10, 
                       type.measure = "mse")

predicciones_train <- predict(cv_output, s = cv_output$lambda.min , newx = x_train)
Math_training_mse_ridge <- mean((predicciones_train - y_train)^2)
paste("Error (mse) de entrenamiento:", Math_training_mse_ridge)


predicciones_test <- predict(cv_output, s = cv_output$lambda.min , newx = x_test)
Math_test_mse_ridge <- mean((predicciones_test - y_test)^2)
paste("Error (mse) de test:", Math_test_mse_ridge)

#________ Portugu�s

set.seed(123)

sample = sample.split(Por, SplitRatio = 0.80) 
Por.train = subset(Por, sample == TRUE)
Por.test = subset(Por, sample == FALSE)

x_train <- model.matrix(G3~., data = Por.train)[, -1]
y_train <- Por.train$G3

x_test <- model.matrix(G3~., data = Por.test)[, -1]
y_test <- Por.test$G3

cv_output <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 10, 
                       type.measure = "mse")

predicciones_train <- predict(cv_output, s = cv_output$lambda.min , newx = x_train)
Por_training_mse_ridge <- mean((predicciones_train - y_train)^2)
paste("Error (mse) de entrenamiento:", Por_training_mse_ridge)


predicciones_test <- predict(cv_output, s = cv_output$lambda.min , newx = x_test)
Por_test_mse_ridge <- mean((predicciones_test - y_test)^2)
paste("Error (mse) de test:", Por_test_mse_ridge)

#_________________________ Analisis Regresion Elastic Net ######################

#______ Matematica

set.seed(123)

sample = sample.split(Math, SplitRatio = 0.80) 
Math.train = subset(Math, sample == TRUE)
Math.test = subset(Math, sample == FALSE)

x_train <- model.matrix(G3~., data = Math.train)[, -1]
y_train <- Math.train$G3

x_test <- model.matrix(G3~., data = Math.test)[, -1]
y_test <- Math.test$G3

list.of.fits <- list()
for (i in 1:10) {
  fit.name <- paste0('alpha', i/10)
  
  list.of.fits[[fit.name]] <- 
    cv.glmnet(x_train, y_train, type.measure = 'mse',
              alpha = i/10, family = 'gaussian') 
}

for (i in 1:10) {
  fit.name <- paste0('alpha', i/10)
  
  EN.pred.mult <- predict(list.of.fits[[fit.name]],
                          s = list.of.fits[[fit.name]]$lambda.1se, 
                          newx = x_test) 
  
  em.EN.mult <- mean((y_test - EN.pred.mult)^2)
  
  temp <- data.frame(alpha = i/10, mse = em.EN.mult, fit.name = fit.name)
  
  results.1se <- rbind(results, temp)
}

for (i in 1:10) {
  fit.name <- paste0('alpha', i/10)
  
  EN.pred.mult <- predict(list.of.fits[[fit.name]],
                          s = list.of.fits[[fit.name]]$lambda.min, 
                          newx = x_test) 
  em.EN.mult <- mean((y_test - EN.pred.mult)^2)
  temp <- data.frame(alpha = i/10, mse = em.EN.mult, fit.name = fit.name)
  results.min <- rbind(results, temp)
}

Math.test.net <- results.min$mse


#______ Portugues

set.seed(123)

sample = sample.split(Por, SplitRatio = 0.80) 
Por.train = subset(Por, sample == TRUE)
Por.test = subset(Por, sample == FALSE)

x_train <- model.matrix(G3~., data = Por.train)[, -1]
y_train <- Por.train$G3

x_test <- model.matrix(G3~., data = Por.test)[, -1]
y_test <- Por.test$G3

list.of.fits <- list()
for (i in 1:10) {
  fit.name <- paste0('alpha', i/10)
  
  list.of.fits[[fit.name]] <- 
    cv.glmnet(x_train, y_train, type.measure = 'mse',
              alpha = i/10, family = 'gaussian') 
}

for (i in 1:10) {
  fit.name <- paste0('alpha', i/10)
  
  EN.pred.mult <- predict(list.of.fits[[fit.name]],
                          s = list.of.fits[[fit.name]]$lambda.1se, 
                          newx = x_test) 
  
  em.EN.mult <- mean((y_test - EN.pred.mult)^2)
  
  temp <- data.frame(alpha = i/10, mse = em.EN.mult, fit.name = fit.name)
  
  results.1se <- rbind(results, temp)
}

for (i in 1:10) {
  fit.name <- paste0('alpha', i/10)
  
  EN.pred.mult <- predict(list.of.fits[[fit.name]],
                          s = list.of.fits[[fit.name]]$lambda.min, 
                          newx = x_test) 
  em.EN.mult <- mean((y_test - EN.pred.mult)^2)
  temp <- data.frame(alpha = i/10, mse = em.EN.mult, fit.name = fit.name)
  results.min <- rbind(results, temp)
}

Por.test.net <- results.min$mse

#_________________________ Valores de Error para todas los metodos ##############
df.errores <- data.frame(Error=c(error.Math,Math_test_mse,Math_test_mse_ridge,Math.test.net,
                                 error.Por,Por_test_mse,Por_test_mse_ridge,Por.test.net),
                         Dataset=c('Matem�tica','Matem�tica','Matem�tica','Matem�tica','Matem�tica','Matem�tica',
                                   'Portugu�s','Portugu�s','Portugu�s','Portugu�s','Portugu�s','Portugu�s'),
                         Metodo=c('Forward','Backward','Hybrid','Lasso','Ridge','ElasticNet',
                                  'Forward','Backward','Hybrid','Lasso','Ridge','ElasticNet'))

getPalette = colorRampPalette(brewer.pal(5, "Set1"))
# Plot
g12 <- ggplot(data = df.errores, mapping = aes(x = Dataset, y = Error, fill = Metodo)) +
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_manual(values = getPalette(nrow(df.errores)))+
  theme_light()+
  theme_minimal()

print(g12 + mynamesthemev2 + labs( title= "Resultados", 
                                   y="ECM", 
                                   x = "Datasets"))

