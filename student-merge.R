

#_________________________ Inicializacion del archivo ##########################

library(ggplot2)
library(GGally)
library(stats)
library(FactoMineR)
library(factoextra)
library(leaps)

setwd("E:/Bruno/Favaloro/4 Año/2do Cuatrimestre/ICD/ProyectoFinal/Student-ICD")

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

#_________________________ Dividimos las observaciones en Train y Test #############################

library(caTools)
set.seed(800)   
sample = sample.split(Math,SplitRatio = 0.85) 
Math.train = subset(Math,sample == TRUE)
Math.test = subset(Math, sample == FALSE)

sample = sample.split(Por,SplitRatio = 0.85) 
Por.train = subset(Por,sample == TRUE)
Por.test = subset(Por, sample == FALSE)

#_________________________ Regresion lineal normal #############################

Math.lineal <- lm(G3~., data = Math.train)
Por.lineal <- lm(G3~., data = Por.train)

Math.pred.lineal <- predict(Math.lineal, newdata = Math.test)

aux <- sqrt((Math.test$G3 - Math.pred.lineal)^2)*5
em.lineal <- mean(aux) # ya esta en porcentaje (ej: 3 es 3%)

#_________________________ Seleccion de Variable Forward #######################

#____________ Matematica
Math.forw<-regsubsets(G3~., nbest=1, nvmax=41,
           force.in=NULL, force.out=NULL, intercept=TRUE,
           method=c("forward"), data = Math.train)

reg.summary = summary(Math.forw)

library(ggvis)
rsq <- as.data.frame(summary(Math.forw)$rsq)
names(rsq) <- "R2"
rsq %>% 
  ggvis(x=~ c(1:nrow(rsq)), y=~R2 ) %>%
  layer_points(fill = ~ R2 ) %>%
  add_axis("y", title = "R2") %>% 
  add_axis("x", title = "Number of variables")

# Set up a 2x2 grid so we can look at 4 plots at once
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# We will now plot a red dot to indicate the model with the largest adjusted R^2 statistic.
# The which.max() function can be used to identify the location of the maximum point of a vector
adj_r2_max = which.max(reg.summary$adjr2) # 13

# The points() command works like the plot() command, except that it puts points 
# on a plot that has already been created instead of creating a new plot
points(adj_r2_max, reg.summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)

# We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min = which.min(reg.summary$cp) # 1
points(cp_min, reg.summary$cp[cp_min], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min = which.min(reg.summary$bic) # 5
points(bic_min, reg.summary$bic[bic_min], col = "red", cex = 2, pch = 20)

adj_r2_max
cp_min
bic_min

#____________ Portugues
Por.forw<-regsubsets(G3~., nbest=1, nvmax=40,
                      force.in=NULL, force.out=NULL, intercept=TRUE,
                      method=c("forward"), data = Por.train)

reg.summary = summary(Por.forw)

library(ggvis)
rsq <- as.data.frame(summary(Por.forw)$rsq)
names(rsq) <- "R2"
rsq %>% 
  ggvis(x=~ c(1:nrow(rsq)), y=~R2 ) %>%
  layer_points(fill = ~ R2 ) %>%
  add_axis("y", title = "R2") %>% 
  add_axis("x", title = "Number of variables")

# Set up a 2x2 grid so we can look at 4 plots at once
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# We will now plot a red dot to indicate the model with the largest adjusted R^2 statistic.
# The which.max() function can be used to identify the location of the maximum point of a vector
adj_r2_max = which.max(reg.summary$adjr2) # 13

# The points() command works like the plot() command, except that it puts points 
# on a plot that has already been created instead of creating a new plot
points(adj_r2_max, reg.summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)

# We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min = which.min(reg.summary$cp) # 1
points(cp_min, reg.summary$cp[cp_min], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min = which.min(reg.summary$bic) # 5
points(bic_min, reg.summary$bic[bic_min], col = "red", cex = 2, pch = 20)

adj_r2_max
cp_min
bic_min

#_________________________ Seleccion de Variable Backward ######################

Math.back<-regsubsets(G3~., nbest=1, nvmax=41,
                      force.in=NULL, force.out=NULL, intercept=TRUE,
                      method=c("backward"), data = Math)

# reg.summary = summary(Math.back)

library(ggvis)
rsq <- as.data.frame(summary(Math.back)$rsq)
names(rsq) <- "R2"
rsq %>% 
  ggvis(x=~ c(1:nrow(rsq)), y=~R2 ) %>%
  layer_points(fill = ~ R2 ) %>%
  add_axis("y", title = "R2") %>% 
  add_axis("x", title = "Number of variables")

# Set up a 2x2 grid so we can look at 4 plots at once
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# We will now plot a red dot to indicate the model with the largest adjusted R^2
# statistic. The which.max() function can be used to identify the location of
# the maximum point of a vector
adj_r2_max = which.max(reg.summary$adjr2) # 13

# The points() command works like the plot() command, except that it puts points 
# on a plot that has already been created instead of creating a new plot
points(adj_r2_max, reg.summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)

# We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min = which.min(reg.summary$cp) # 11
points(cp_min, reg.summary$cp[cp_min], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min = which.min(reg.summary$bic) # 5
points(bic_min, reg.summary$bic[bic_min], col = "red", cex = 2, pch = 20)

adj_r2_max
cp_min
bic_min

#____________ Portugues
Por.back<-regsubsets(G3~., nbest=1, nvmax=40,
                     force.in=NULL, force.out=NULL, intercept=TRUE,
                     method=c("backward"), data = Por)

reg.summary = summary(Por.back)

library(ggvis)
rsq <- as.data.frame(summary(Por.back)$rsq)
names(rsq) <- "R2"
rsq %>% 
  ggvis(x=~ c(1:nrow(rsq)), y=~R2 ) %>%
  layer_points(fill = ~ R2 ) %>%
  add_axis("y", title = "R2") %>% 
  add_axis("x", title = "Number of variables")

# Set up a 2x2 grid so we can look at 4 plots at once
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# We will now plot a red dot to indicate the model with the largest adjusted R^2 statistic.
# The which.max() function can be used to identify the location of the maximum point of a vector
adj_r2_max = which.max(reg.summary$adjr2) # 13

# The points() command works like the plot() command, except that it puts points 
# on a plot that has already been created instead of creating a new plot
points(adj_r2_max, reg.summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)

# We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min = which.min(reg.summary$cp) # 1
points(cp_min, reg.summary$cp[cp_min], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min = which.min(reg.summary$bic) # 5
points(bic_min, reg.summary$bic[bic_min], col = "red", cex = 2, pch = 20)

adj_r2_max
cp_min
bic_min

#____________

rlm.vacio <- lm(formula = G3~1, data = Math.train)
rlm.completo <- lm(formula = G3~., data = Math.train)
rlm.backward <- step(rlm.completo,
                     scope = list(lower = rlm.vacio, upper = rlm.completo),
                     direction = 'backward')

#_________________________ Seleccion de Variable Stepwise  ######################

#____________ Matematica
Math.step<-regsubsets(G3~., nbest=1, nvmax=41,
                     force.in=NULL, force.out=NULL, intercept=TRUE,
                     method=c("seqrep"), data = Math)

reg.summary = summary(Math.step)

library(ggvis)
rsq <- as.data.frame(summary(Math.step)$rsq)
names(rsq) <- "R2"
rsq %>% 
  ggvis(x=~ c(1:nrow(rsq)), y=~R2 ) %>%
  layer_points(fill = ~ R2 ) %>%
  add_axis("y", title = "R2") %>% 
  add_axis("x", title = "Number of variables")

# Set up a 2x2 grid so we can look at 4 plots at once
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# We will now plot a red dot to indicate the model with the largest adjusted R^2 statistic.
# The which.max() function can be used to identify the location of the maximum point of a vector
adj_r2_max = which.max(reg.summary$adjr2) # 13

# The points() command works like the plot() command, except that it puts points 
# on a plot that has already been created instead of creating a new plot
points(adj_r2_max, reg.summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)

# We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min = which.min(reg.summary$cp) # 1
points(cp_min, reg.summary$cp[cp_min], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min = which.min(reg.summary$bic) # 5
points(bic_min, reg.summary$bic[bic_min], col = "red", cex = 2, pch = 20)

adj_r2_max
cp_min
bic_min

#____________ Portugues
Por.step<-regsubsets(G3~., nbest=1, nvmax=40,
                     force.in=NULL, force.out=NULL, intercept=TRUE,
                     method=c("seqrep"), data = Por)

reg.summary = summary(Por.step)

library(ggvis)
rsq <- as.data.frame(summary(Por.step)$rsq)
names(rsq) <- "R2"
rsq %>% 
  ggvis(x=~ c(1:nrow(rsq)), y=~R2 ) %>%
  layer_points(fill = ~ R2 ) %>%
  add_axis("y", title = "R2") %>% 
  add_axis("x", title = "Number of variables")

# Set up a 2x2 grid so we can look at 4 plots at once
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# We will now plot a red dot to indicate the model with the largest adjusted R^2 statistic.
# The which.max() function can be used to identify the location of the maximum point of a vector
adj_r2_max = which.max(reg.summary$adjr2) # 13

# The points() command works like the plot() command, except that it puts points 
# on a plot that has already been created instead of creating a new plot
points(adj_r2_max, reg.summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)

# We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min = which.min(reg.summary$cp) # 1
points(cp_min, reg.summary$cp[cp_min], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min = which.min(reg.summary$bic) # 5
points(bic_min, reg.summary$bic[bic_min], col = "red", cex = 2, pch = 20)

adj_r2_max
cp_min
bic_min

#______________
rlm.stepwise <- step(rlm.vacio,
                     scope = list(lower = rlm.vacio, upper = rlm.completo),
                     direction = 'both')
#_________________________ Seleccion de Variable Finales  ######################

# A partir de la seleccion de variables por backward y forward se obtuvo que
# para el dataset Math con un modelo de 5 variables se tiene un modelo pobre
# mientras que con un modelo de 13 variables se tiene un modelo con overfit


#El modelo que elegimos es el modelo 11 que tiene a las variables:
# Math:
# Por: schoolMS + sexM + Fjobother + Fjobservices + Fjobteacher + reasonother +
# traveltime + failures + health + G1 + G2

Math.lineal <- lm(G3~school+age+Fjob+activities+famrel+reason+
                    G1+G2+absences+romantic+Walc,data=Math.train)

Por.lineal <- lm(G3~school+sex+Fjob+reason+traveltime+failures
                 +health+G1+G2,data=Por.train)

Math.lineal <- lm(G3 ~ sex + age + activities + famrel + 
                    absences + G1 + G2, data = Math.train)


#_________________________ ECM del predictor Backward  ######################

Math.pred.back <- predict(rlm.backward, newdata = Math.test)

for (i in 1:length(Math.pred.back)) {
  if ((Math.pred.back[i]-trunc(Math.pred.back[i])) <= 0.5) {
    Math.pred.back[i] <- floor(Math.pred.back[i])
  }
  else  
    Math.pred.back[i] <- ceiling(Math.pred.back[i])
}
aux <- sqrt((Math.test$G3 - Math.pred.back)^2)*5
Math.em.back <- mean(aux) # ya esta en porcentaje (ej: 3 es 3%)


#_________________________ ECM del predictor Stepwise  ######################
Math.pred.step <- predict(rlm.stepwise, newdata = Math.test)

for (i in 1:length(Math.pred.step)) {
  if ((Math.pred.step[i]-trunc(Math.pred.step[i])) <= 0.5) {
    Math.pred.step[i] <- floor(Math.pred.step[i])
  }
  else  
    Math.pred.step[i] <- ceiling(Math.pred.step[i])
}
aux <- sqrt((Math.test$G3 - Math.pred.step)^2)*5
Math.em.stepwise <- mean(aux) # ya esta en porcentaje (ej: 3 es 3%)












