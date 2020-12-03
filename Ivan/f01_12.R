# Prueba de normalidad

install.packages("dplyr")
# install.packages("ggpubr")

library("dplyr")
library("ggplot2")

# Portugues esta por read.csv
# Evaluo normalidad de las variables numericas

nums <- unlist(lapply(Por, is.numeric))  
Por.num <- Por[ , nums]

# Esto hace lo mismo en una linea de codigo porque es crack
Por.num <- Por %>% dplyr::select(where(is.numeric))

# Sampling
set.seed(1234)
sample_n(Por.num, 10)

p <- ggplot(Por.num, aes(x=G3)) + 
  geom_density()
p

library("rstatix")
shapiro_test(Por.num[[i]])

# Regresion sin variables explicativas

rlm.vacio <- lm(formula = G3~1, data = Por)
summary(rlm.vacio)

# Regresion con todas las variables explicativas

rlm.completo <- lm(formula = G3~., data = Por)
summary(rlm.completo) # Rs = .85 (el modelo explica el 84% de la nota G3)
                      # P-Value < 0.005 (el modelo es significativo)

# Regresion Forward
rlm.forward <- step(rlm.vacio,
                    scope = list(lower = rlm.vacio, upper = rlm.completo),
                    direction = 'forward', k = log(n))
summary(rlm.forward) # Creeeeeo que el AIC significa Akaike Information Criterion
# Rs = .85 (el modelo explica el 84% de la nota G3)
# P-Value < 0.005 (el modelo es significativo)
# Vemos que, aunque el modelo es bueno, algunas variables todavia tienen un P-Value grande,
# por ejemplo reasonhome tiene un P value enorme y no explica casi nada el resultado

# Regresion Backward
rlm.backward <- step(rlm.completo,
                    scope = list(lower = rlm.vacio, upper = rlm.completo),
                    direction = 'backward')
summary(rlm.backward) # Creeeeeo que el AIC significa Akaike Information Criterion
# Rs = .85 (el modelo explica el 84% de la nota G3)
# P-Value < 0.005 (el modelo es significativo)
# Vemos que el resultado es el mismo, en cuanto a las variables que conserva pero no son significativas

# Regresion Stepwise
rlm.stepwise <- step(rlm.vacio,
                     scope = list(lower = rlm.vacio, upper = rlm.completo),
                     direction = 'both')
summary(rlm.stepwise) # Creeeeeo que el AIC significa Akaike Information Criterion

# Regresion con el modelo de stepwise
library(leaps)
model.stepwise <- regsubsets(G3~., data = Por,
                     method = "seqrep")
summary(model.stepwise)

# Train
install.packages("caret")
library(caret)
set.seed(123)

# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "LOOCV")

# Train the model
step.model <- train(G3~., data = Por,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)

step.model$results
step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, 3)

# Pruebo ese modelo
lm.stepwise <- lm(G3 ~ reason + G1 + G2, data = Por)
summary(lm.stepwise)

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
