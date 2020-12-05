# PCA numerico

library(dplyr)
Por.num <- Por %>% dplyr::select(where(is.numeric))

pca <- prcomp(Por.num, scale = TRUE)

# X: Contiene los PCA para hacer el grafico

plot(pca$x[, 1], pca$x[, 2])

# Calculamos cuanta variacion de la data original corresponde cada PC
pca.var <- pca$sdev^2 
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main = 'Scree Plot de las variables numericas',
        xlab = 'Componentes Principales', 
        ylab = 'Porcentaje de Variacion')

library(ggplot2)
pca.data <- data.frame(X = pca$x[,1],
                       Y = pca$x[,2])
pca.data
g <- ggplot(data = pca.data, aes(x = X, y = Y)) +
            geom_point() + 
            xlab(paste('PC1 - ', pca.var.per[1], '%', sep = '')) +
            ylab(paste('PC2 - ', pca.var.per[2], '%', sep = '')) + 
            theme_bw() + 
            ggtitle('PCA 1v2') # por ejemplo. Se pueden hacer cualquier PCi vs PCi
g # Como los componentes principales no representan un gran porcentaje de la explicacion de las 
  # variables, se ve que no se arman clusters de observaciones
ls <- pca$rotation[, 1]
obss <- abs(ls)
obss.ranked <- sort(obss, decreasing = TRUE)
n <- 16 # Para rankear los 10 maximos
top.var <- names(obss.ranked[1 : n])

top.var
pca$rotation[top.var, 1]
# Para que se armen clusters, los autovalores de las variables tienen que ser mas diferentes.
# En este caso, son bastante similares


# PCA categorico se puede hacer, pero si ya da mal con los numeros, que son los mas explicativos
# no le veo como puede servir



# Ridge, Lasso y Elastic Net

# Es la combinacion de la regresion Ridge y Lasso
# dou
# vamo boquita

install.packages('glmnet')
install.packages('caTools')
library(glmnet)
library(caTools)

# EN combina Ridge y Lasso. Pero, en vez de usar dos lambda como en la teoria, le pone un
# termino alfa que va de 0 a 1 y queda: lambda*[alfa*Lasso + (1-alfa)*Ridge]
# Cuando alfa es 0, es el Ridge
# Cuando alfa es 1, es el Lasso
# Cuando alfa esta entre 0 y 1, queda Elastic Net 
# Para testear, hacemos un grafico alfa en funcion le lambda


set.seed(123)

sample = sample.split(Por, SplitRatio = 0.80)  
train = subset(Por, sample == TRUE) 
test = subset(Por, sample == FALSE) 

x.train <- model.matrix(G3~., data = train)[, -1] 
y.train <- train$G3 

x.test <- model.matrix(G3~., data = test)[, -1] 
y.test <- test$G3 


# Ridge
Ridge.fit <- cv.glmnet(x.train, y.train, type.measure = 'mse',
                        alpha = 0, family = 'gaussian') 
# Auntomaticamente es un k-fold de k = 10
# mse = mean square error. Si fueran categoricas podria 'devians'
# Gaussial = regresion lineal, si fuera categorica pongo binomial
Ridge.pred <- predict(Ridge.fit, s = Ridge.fit$lambda.1se, newx = x.test)
# lambda.1se es el valor de lambda con el modelo mas simple. Lambda.min es la suma minima
# El resultado es igual pero te da menos variables con 1se
Ridge.result <- cbind(test$G3, round(Ridge.pred))
em.ridge <- mean(sqrt((y.test - round(Ridge.pred))^2)*5)
em.ridge

# Lasso
Lasso.fit <- cv.glmnet(x.train, y.train, type.measure = 'mse',
                       alpha = 1, family = 'gaussian')

lasso_modelo_1se <- glmnet(x.train, y.train,
                       alpha = 1, lambda = Lasso.fit$lambda.1se)

lasso_modelo_min <- glmnet(x.train, y.train,
                           alpha = 1, lambda = Lasso.fit$lambda.min)

Lasso.pred.1se <- predict(lasso_modelo_1se, newx = x.test)
Lasso.pred.min <- predict(lasso_modelo_min, newx = x.test)

mse.lasso.min <- mean((y.test - Lasso.pred.min)^2)
mse.lasso.1se <- mean((y.test - Lasso.pred.1se)^2)

Lasso.fit$lambda.min
mse.lasso.min
Lasso.fit$lambda.1se
mse.lasso.1se

# Elastic Net
EN.fit <- cv.glmnet(x.train, y.train, type.measure = 'mse',
                       alpha = 0.5, family = 'gaussian') 
EN.pred <- predict(EN.fit, s = EN.fit$lambda.1se, newx = x.test)
em.EN <- mean(sqrt((y.test - round(EN.pred))^2)*5)
em.EN

# Elastic Net pero con mas alfa a ver cual es mejorcete
list.of.fits <- list()
for (i in 1:10) {
  fit.name <- paste0('alpha', i/10)
  
  list.of.fits[[fit.name]] <- 
    cv.glmnet(x.train, y.train, type.measure = 'mse',
              alpha = i/10, family = 'gaussian') 
}
results <- data.frame()
for (i in 1:10) {
  fit.name <- paste0('alpha', i/10)
  
  EN.pred.mult <- predict(list.of.fits[[fit.name]],
                       s = list.of.fits[[fit.name]]$lambda.1se, 
                       newx = x.test) 
  em.EN.mult <- mean(sqrt((y.test - round(EN.pred.mult))^2)*5) 
  temp <- data.frame(alpha = i/10, mse = em.EN.mult, fit.name = fit.name)
  results <- rbind(results, temp)
}
results





