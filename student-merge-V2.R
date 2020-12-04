
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

#_________________________ Inicializacion del archivo ##########################

library(ggplot2)
library(GGally)
library(stats)
library(FactoMineR)
library(factoextra)
library(leaps)
library(caTools)
library(RColorBrewer)

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

#_________________________ Regresion lineal normal #############################

Math.lineal <- lm(G3~., data = Math.train)
Por.lineal <- lm(G3~., data = Por.train)

Math.pred.lineal <- predict(Math.lineal, newdata = Math.test)

aux <- sqrt((Math.test$G3 - Math.pred.lineal)^2)*5
em.lineal <- mean(aux) # ya esta en porcentaje (ej: 3 es 3%)

#_________________________ Dividimos las observaciones en Train y Test #############################

library(caTools)
 
sample = sample.split(Math,SplitRatio = 0.80) 
Math.train = subset(Math,sample == TRUE)
Math.test = subset(Math, sample == FALSE)

sample = sample.split(Por,SplitRatio = 0.80) 
Por.train = subset(Por,sample == TRUE)
Por.test = subset(Por, sample == FALSE)

#_________________________ Armo el grafico de frecuencias de las variables ##################
#____________ Matematica

#matrix_var <- matrix(0, nrow = 1, ncol = 42)
matrix_var <- rep(0L, 42)
variables_nombres <- c("(Intercept)","schoolMS","sexM","age","addressU","famsizeLE3", "PstatusT","Medu",
                       "Fedu","Mjobhealth","Mjobother","Mjobservices","Mjobteacher","Fjobhealth", "Fjobother","Fjobservices",
                       "Fjobteacher", "reasonhome", "reasonother", "reasonreputation", "guardianmother","guardianother",
                       "traveltime", "studytime", "failures","schoolsupyes",  "famsupyes",  "paidyes"
                       ,"activitiesyes","nurseryyes", "higheryes", "internetyes", "romanticyes", "famrel",
                       "freetime","goout", "Dalc", "Walc", "health","absences","G1","G2")

colnames(matrix_var) <- variables_nombres

for (k in 1:400) {
  set.seed(k)  
  
  sample = sample.split(Math, SplitRatio = 0.80) 
  Math.train = subset(Math, sample == TRUE)
  
  Math.forw<-regsubsets(G3~., nbest=1, nvmax=41,
                        force.in=NULL, force.out=NULL, intercept=TRUE,
                        method=c("forward"), data = Math.train)
  
  reg.summary = summary(Math.forw)
  
  cp_min = which.min(reg.summary$cp)
  
  n_aux <- c()
  n_aux <- names(which(reg.summary$which[cp_min,]))
  
  for (i in 1:length(n_aux)) {
    aux <- n_aux[i]
    for (j in 1:42) {
      #if(aux == colnames(matrix_var)[j])
      if(aux == variables_nombres[j])
        matrix_var[j] = matrix_var[j] + 1
    }
  }
  
}

#Obtengo aquello que tengan mayor relevancia de acuerdo con un umbral

umbral <- max(matrix_var)*0.40

df <- data.frame(Variables = variables_nombres[matrix_var>umbral],
                 Frecuencia = matrix_var[matrix_var>umbral])

colourCount = length(matrix_var[matrix_var>umbral])
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Plot
g1 <- ggplot(data = df, mapping = aes(x = Variables, y = Frecuencia, fill = Variables)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Frecuencia), position = position_stack(vjust= 0.5),
            colour = "black", size = 3)+
  scale_fill_manual(values = getPalette(colourCount))+
  theme_light()+
  theme_minimal()

print(g1 + mynamestheme + labs( title= "Matematica - Forward", y="Frecuencia", 
                                x = "Variables"))

#________________ Portugues

#matrix_var <- matrix(0, nrow = 1, ncol = 41)
#colnames(matrix_var) <- names(reg.summary$which[40,])
matrix_var <- rep(0L, 41)
variables_nombres <- c("(Intercept)","schoolMS","sexM","age","addressU","famsizeLE3",
"PstatusT","Medu","Fedu",
"Mjobhealth","Mjobother","Mjobservices","Mjobteacher","Fjobhealth",
"Fjobother","Fjobservices",
"Fjobteacher","reasonhome","reasonother","reasonreputation"
,"guardianmother","guardianother",
"traveltime","studytime","failures","schoolsupyes","famsupyes","activitiesyes","nurseryyes",
"higheryes","internetyes",
"romanticyes","famrel","freetime","goout","Dalc","Walc","health","absences","G1","G2")


for (k in 1:650) {
  set.seed(k)  
  
  sample = sample.split(Por,SplitRatio = 0.80) 
  Por.train = subset(Por,sample == TRUE)
  
  Por.forw<-regsubsets(G3~., nbest=1, nvmax=41,
                        force.in=NULL, force.out=NULL, intercept=TRUE,
                        method=c("forward"), data = Por.train)
  
  reg.summary = summary(Por.forw)
  
  cp_min = which.min(reg.summary$cp)
  
  n_aux <- c()
  n_aux <- names(which(reg.summary$which[cp_min,]))
  
  for (i in 1:length(n_aux)) {
    aux <- n_aux[i]
    for (j in 1:41) {
      #if(aux == colnames(matrix_var)[j])
      if(aux == variables_nombres[j])  
        matrix_var[j] = matrix_var[j] + 1
    }
  }
  
}

#Obtengo aquello que tengan mayor relevancia de acuerdo con un umbral

umbral <- max(matrix_var)*0.40

df <- data.frame(Variables = variables_nombres[matrix_var>umbral],
                 Frecuencia = matrix_var[matrix_var>umbral])

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
    
    reg.var<-regsubsets(G3~., nbest=1, nvmax=41,
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

df <- df.frecuencia('Math', 0.50, 400, "forward")


colourCount = length(matrix_var[matrix_var>umbral])
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Plot
g2 <- ggplot(data = df, mapping = aes(x = Variables, y = Frecuencia, fill = Variables)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Frecuencia), position = position_stack(vjust= 0.5),
            colour = "black", size = 3)+
  scale_fill_manual(values = getPalette(colourCount))+
  theme(axis.text.x=element_blank())+
  theme_light()+
  theme_minimal()

print(g2 + mynamestheme + labs( title= "Portugues - Forward", y="Frecuencia", 
                                x = "Variables"))

#_________________________ Calculo del Error ###################################

#______________ Matematica
Math.lineal <- lm(G3~school+age+Fjob+activities+famrel+reason+ schoolsup +
                    G1+G2+absences+romantic+Walc,data=Math.train)

Math.pred.back <- predict(Math.lineal, newdata = Math.test)

for (i in 1:length(Math.pred.back)) {
  if ((Math.pred.back[i]-trunc(Math.pred.back[i])) <= 0.5) {
    Math.pred.back[i] <- floor(Math.pred.back[i])
  }
  else  
    Math.pred.back[i] <- ceiling(Math.pred.back[i])
}
aux <- sqrt((Math.test$G3 - Math.pred.back)^2)*5
Math.em.back <- mean(aux) # ya esta en porcentaje (ej: 3 es 3%)

notas_final <- cbind(Math.test$G3, Math.pred.back)

#______________ Portugues

Por.lineal <- lm(G3~school+sex+Fjob+health+reason+failures+traveltime+
                   G1+G2+absences+guardian,data=Por.train)

Por.pred.forw <- predict(Por.lineal, newdata = Por.test)

for (i in 1:length(Por.pred.forw)) {
  if ((Por.pred.forw[i]-trunc(Por.pred.forw[i])) <= 0.5) {
    Por.pred.forw[i] <- floor(Por.pred.forw[i])
  }
  else  
    Por.pred.forw[i] <- ceiling(Por.pred.forw[i])
}
aux <- sqrt((Por.test$G3 - Por.pred.forw)^2)*5
Math.em.back <- mean(aux) # ya esta en porcentaje (ej: 3 es 3%)

notas_final <- cbind(Por.test$G3, Por.pred.forw)

#_________________________ Analisis por PCR #############################



