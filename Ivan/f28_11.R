library("pls")

setwd('C:/Users/bruno/OneDrive/Escritorio/Bruno/Student-ICD')

set.seed (1000)
pcr.model <- pcr(Sepal.Length~., data = iris, scale = TRUE, validation = "CV")

