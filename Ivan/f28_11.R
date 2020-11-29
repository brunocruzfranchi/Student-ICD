library("pls")

setwd('C:/Users/bruno/OneDrive/Escritorio/Bruno/Student-ICD')

set.seed (123)
pcr.model <- pcr(G3~., data = d2, scale = TRUE, validation = "CV")

summary(pcr.model)

validationplot(pcr.model)
validationplot(pcr.model, val.type = "R2")
validationplot(pcr.model, val.type="MSEP")

predplot(pcr.model)

coefplot(pcr.model)

# train test
set.seed(123)   
sample = sample.split(d2, SplitRatio = 0.70) 
train = subset(d2,sample == TRUE)
aux = subset(d2,sample == FALSE)
sample = sample.split(aux, SplitRatio = 0.50) 
validation = subset(aux, sample == TRUE)
test = subset(aux, sample == FALSE)


pcr.pred <- predict(pcr.model, validation, ncomp = 10)
rownames(pcr.pred)<-1:nrow(pcr.pred)

for (i in 1:nrow(pcr.pred)) {
  if ((pcr.pred[i]-trunc(pcr.pred[i])) <= 0.5) {
    pcr.pred[i] <- floor(pcr.pred[i])
  }
  else  
    pcr.pred[i] <- ceiling(pcr.pred[i])
}



aux <- sqrt((pcr.pred - validation$G3)^2)*5
ecm <- mean(aux) # ya esta en porcentaje (ej: 3 es 3%)
ecm
