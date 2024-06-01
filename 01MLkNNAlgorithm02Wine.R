# Paso 01 - Instalar librerias y llamarlas 
############################################

# Instalar las librerías
#install.packages("tidyverse")
#install.packages("class")
#install.packages("gmodels")
#install.packages("caret")

# Llamar a las librerías 
library(tidyverse)
library(class)
library(gmodels)
library(caret)
library(ggplot2)


# Paso 02 - Recolectar los datos y graficar
############################################
# Import and inspect data
data <- read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/data-for-knn.csv')
str(data)
summary(data)
#dibujar los datos
with(data, #simple plot{plot(X1,X2,col=Y)})

# Lo mismo con gplot
# library(ggplot2)
# ggplot(data) + geom_point(aes(x=X1,y=X2,col=Y))
# ggplot(data,aes(x=X1,y=X2)) +   geom_point(aes(col=Y)) # Alternativo


# Paso 03 - Preparar los datos
############################################
p <- 0.8 											# Use 80% for entrenamiento and 20% para validación
train.index <- sample.int(nrow(data),nrow(data)*p) 	# Redondea a enteros
str(train.index) 									# Calcula los datos 0.8*1000=800 observations
summary(train.index) 								# Chequea que el rango sea correcto (1-1000)
data.train <- data[train.index,] 					# Crea un dataframe con 800 valores aleatorios
data.val <- data[-train.index,]                     # y valida los datos con el resto
str(data.train)
str(data.val)


# Paso 03 - Entrenar los datos
############################################
library(class) 										# Libreria k-NN 
set.seed (1) 										# Para reproducir
Ypred_knn=knn(data.train[,c("X1","X2")],  			# Predictores asociado a los datos de prueba
              data.val[,c("X1","X2")],    			# Predictores associa con los datos para hacer prodicciones
              data.train$Y,                  		# Etiquetado para las observaciones de entrenamientos
              k=3)                            		# Número de los vecinos cercanos a ser usados

# Paso 04 - Evaluar los datos
############################################
table(data.val$Y,Ypred_knn) 						# Matriz de Confusión

my.statistics <- function(Actual,Predicted) {
  confusion.table <- table(Actual=Actual,Predicted=Predicted)
  output <- list(confusion.table=confusion.table)
  TN <- confusion.table[1]
  FN <- confusion.table[2]
  FP <- confusion.table[3]
  TP <- confusion.table[4]
  output$accuracy <- (TP+TN)/sum(confusion.table)
  output$precission <- (TP)/(TP+FP)
  output$sensitivity <- (TP)/(TP+FN)
  output$specificity <- (TN)/(TN+FP)
  output$FPR <- (FP)/(TN+FP)
  
  return(output)
}


# Paso 05 - Mejorar el Modelo
############################################
my.statistics(Actual = data.val$Y,Predicted = Ypred_knn)

Ypred_knn=knn(data.train[,c("X1","X2")],  			#Predictors associated with the training data
              data.val[,c("X1","X2")],    			#Predictors associated with the data to make predictions
              data.train$Y,                  		#class labels for the training observations,
              k=51)                            		#number of nearest neighbors to be used

table(data.val$Y,Ypred_knn) 						# Confusion matrix

my.statistics(Actual = data.val$Y,Predicted = Ypred_knn)


Ypred_knn=knn(data.train[,c("X1","X2")],  			#Predictors associated with the training data
              data.val[,c("X1","X2")],    			#Predictors associated with the data to make predictions
              data.train$Y,                  		#class labels for the training observations,
              k=199)                            	#number of nearest neighbors to be used
my.statistics(Actual = data.val$Y,Predicted = Ypred_knn)


plot(data.train$X1,data.train$X2,col=as.numeric(data.train$Y),pch=19,cex=.8)
points(data.val$X1,data.val$X2,col=as.numeric(Ypred_knn)+2,lwd=2)




Ypred_knn=knn(data.train[,c("X1","X2")],  			#Predictors associated with the training data
              data.val[,c("X1","X2")],    			#Predictors associated with the data to make predictions
              data.train$Y,                  		#class( labels for the training observations,
              k=101,
              prob=TRUE)   

probs <- attributes(Ypred_knn)
data.val$Ypred <- probs$prob
ggplot(data.val,aes(X1,X2,size=Ypred,col=Y))+geom_point()

my.roc <- function(k) {
    Ypred_knn=knn(data.train[,c("X1","X2")],  		#Predictors associated with the training data
              data.val[,c("X1","X2")],    			#Predictors associated with the data to make predictions
              data.train$Y,                  		#class labels for the training observations,
              k=k)                            		#number of nearest neighbors to be used
    ms <- my.statistics(Actual = data.val$Y,Predicted = Ypred_knn)
    return(c(FPR=ms$FPR,sensitivity=ms$sensitivity))
}

roc.df <- c(1,1)
roc.df <- rbind(roc.df,t(sapply(seq(1,199,by=5),my.roc)))
roc.df <- rbind(roc.df,c(0,0))

str(roc.df)
head(roc.df)
plot(roc.df,xlim=c(0,1),ylim=c(0,1),type='l')
