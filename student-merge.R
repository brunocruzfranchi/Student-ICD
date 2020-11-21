

##importe de los datos

setwd("E:/Documentos/ING_BIOMEDICA/Cuarto Año/Segundo Cuatrimestre/Introducción a la Ciencia de Datos/Student-ICD")

matematica = read.table("student-mat.csv",sep=",",header=TRUE)
portugues = read.table("student-por.csv",sep=",",header=TRUE)

dset = merge(matematica, portugues, by = c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
# print(nrow(d4)) # 382 students

names(dset)[names(dset) == 'G1.x'] <- 'G1.Matematica'
names(dset)[names(dset) == 'G2.x'] <- 'G2.Matematica'
names(dset)[names(dset) == 'G3.x'] <- 'G3.Matematica'
names(dset)[names(dset) == 'G1.y'] <- 'G1.Portugues'
names(dset)[names(dset) == 'G2.y'] <- 'G2.Portugues'
names(dset)[names(dset) == 'G3.y'] <- 'G3.Portugues'

