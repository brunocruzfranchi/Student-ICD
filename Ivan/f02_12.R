library(caTools)
set.seed(123)   
sample = sample.split(productos,SplitRatio = 0.70) 
train1 = subset(productos,sample == TRUE)
test1 = subset(productos, sample == FALSE)

# Segundo metodo
sample_size = floor(0.70*nrow(productos))
train_ind = sample(seq_len(nrow(productos)),size = sample_size)
train =productos[train_ind,]
test=productos[-train_ind,]