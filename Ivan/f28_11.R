library("pls")

setwd('C:/Users/bruno/OneDrive/Escritorio/Bruno/Student-ICD')

set.seed (123)
pcr.model <- pcr(G3~., data = d2, scale = TRUE, test = "CV")

summary(pcr.model)

testplot(pcr.model)
testplot(pcr.model, val.type = "R2")
testplot(pcr.model, val.type="MSEP")

predplot(pcr.model)

coefplot(pcr.model)

# train test
set.seed(123)   
sample = sample.split(d2, SplitRatio = 0.70) 
train = subset(d2,sample == TRUE)
test = subset(d2,sample == FALSE)

pcr.pred <- predict(pcr.model, test, ncomp = 10)
rownames(pcr.pred)<-1:nrow(pcr.pred)

for (i in 1:nrow(pcr.pred)) {
  if ((pcr.pred[i]-trunc(pcr.pred[i])) <= 0.5) {
    pcr.pred[i] <- floor(pcr.pred[i])
  }
  else  
    pcr.pred[i] <- ceiling(pcr.pred[i])
}
pcr.pred <- pcr.pred-1
test$G3 <- test$G3-1

aux <- sqrt((pcr.pred - test$G3)^2)*5
em <- mean(aux) # ya esta en porcentaje (ej: 3 es 3%)


em <- c()

for (i in 1:40) {
  pcr.pred <- predict(pcr.model, test, ncomp = i)
  for (j in 1:nrow(pcr.pred)) {
    if ((pcr.pred[j]-trunc(pcr.pred[j])) <= 0.5) {
      pcr.pred[j] <- floor(pcr.pred[j])
    }
    else  
      pcr.pred[j] <- ceiling(pcr.pred[j])
  }
  aux <- sqrt((pcr.pred - test$G3)^2)*5
  em[i] <- mean(aux) # ya esta en porcentaje (ej: 3 es 3%)
}

plot(em)
