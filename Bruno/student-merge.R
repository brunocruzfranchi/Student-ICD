

#_________________________ Inicializacion del archivo ##########################

library(ggplot2)
library(GGally)
library(stats)
library(FactoMineR)
library(factoextra)
library(leaps)

setwd("E:/Bruno/Favaloro/4 Año/2do Cuatrimestre/ICD/ProyectoFinal/Student-ICD")

#_________________________ Arreglo de los datasets ############################

Math=read.table("student-mat.csv",sep=",",header=TRUE)

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

#_________________________ Regresion lineal normal #############################

lm.lineal <- lm(G3~.,data=Math.train)
summary(lm.lineal)
summary(lm.lineal)$r.squared

#_________________________ Seleccion de Variable Forward #######################

Math.forw<-regsubsets(G3~., nbest=1, nvmax=41,
           force.in=NULL, force.out=NULL, intercept=TRUE,
           method=c("forward"), data = Math)

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

#_________________________ Seleccion de Variable Backward ######################

Math.back<-regsubsets(G3~., nbest=1, nvmax=41,
                      force.in=NULL, force.out=NULL, intercept=TRUE,
                      method=c("backward"), data = Math)

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

