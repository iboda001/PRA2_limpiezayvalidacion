#Carga de librerías
install.packages("ggplot2", repos = 'http://cran.rstudio.org')
library(ggplot2)
install.packages("corrplot", repos = 'http://cran.rstudio.org')
install.packages("randomForest", repos = 'http://cran.rstudio.org')
install.packages("pROC", repos = 'http://cran.rstudio.org')

library(corrplot)
library(reshape2)
library(caret)
library(randomForest)
library(pROC)
library(car)



#Directorio
path="C:\\Users\\Irati\\Documents\\Unibertsitatea\\UOC\\SEMESTRE II\\TIPOLOGÍA Y CICLO DE VIDA DE LOS DATOS\\PRA2\\data\\raw"

#Carga de datos
setwd(path)
data=read.csv(file="data.csv")
head(data[,1:6])

#Tipo de dato
tipo.variable <- sapply(data,class)
knitr::kable(data.frame(variables=names(tipo.variable),clase=as.vector(tipo.variable)))


#Ceros y elementos vacíos
na.count <-sapply(data, function(x) sum((is.na(x))))
na.count

#Eliminamos la columna vacía "X"
data=data[,!colnames(data)=="X"]
attach(data)


#Valores extremos (outliers)
numeric.data <- data[,c(-1,-2)]

for(i in 1:length(numeric.data) ) {
  cat('Outliers de la variable "',names(numeric.data)[i],'" :\n')
  print(boxplot.stats(numeric.data[,i])$out)
}


#Cálculo de mediana de cada variable
median.n <- as.vector(sapply(numeric.data,median,na.rm=TRUE)) 
print(median.n)

#Agrupación de valores según rango de valores
var.e01.1<-numeric.data[,c(13,2)]
var.e01.2<-numeric.data[,c(1,21,22)]
var.e02<-numeric.data[,c(4,24)]
var.min1.1<-numeric.data[,c(9,11,25,20)]
var.min1.2<-numeric.data[,c(26,27,29)]
var.min2.1<-numeric.data[,c(5,6,7,8)]
var.min2.2<-numeric.data[,c(10,15,16,17)]
var.min2.3<-numeric.data[,c(18,28,30)]
var.00<-numeric.data[,12]
var.min3<-numeric.data[,c(14,23,3)]

#Visualización de valores extremos (outliers)
ggplot(stack(var.e01.1), aes(x = ind, y = values))+geom_boxplot()+xlab("")+ylab("")
ggplot(stack(var.e01.2), aes(x = ind, y = values))+geom_boxplot()+xlab("")+ylab("")
ggplot(stack(var.e02), aes(x = ind, y = values))+geom_boxplot()+xlab("")+ylab("")
ggplot(stack(var.min1.1), aes(x = ind, y = values))+geom_boxplot()+xlab("")+ylab("")
ggplot(stack(var.min1.2), aes(x = ind, y = values))+geom_boxplot()+xlab("")+ylab("")
ggplot(stack(var.min2.1), aes(x = ind, y = values))+geom_boxplot()+xlab("")+ylab("")
ggplot(numeric.data, aes(x= "", y = numeric.data[,12])) +geom_boxplot()+xlab("")+ylab("")
ggplot(stack(var.min3), aes(x = ind, y = values))+geom_boxplot()+xlab("")+ylab("")

#Guardamos los datos preprocesados
write.csv(data,"./data/processed/data.csv")


#Número de casos benignos vs malignos
data=data[,!colnames(data)=="id"]
attach(data)
diagnostic <- plyr::count(data$diagnosis)
print(sprintf("Maligno: %d | Benigno: %d",diagnostic$freq[2],diagnostic$freq[1]))

#Porcentaje de casos malignos
print(sprintf(
  "Porcentaje de tumores malignos: %.2f%%",round(diagnostic$freq[2]/nrow(data)*100,2)
))




for(i in 1:length(numeric.data) ) {
  if (shapiro.test(numeric.data[,i])$p.value<0.05){
    cat('\nRechazamos la hipótesis nula para la variable numérica ',names(numeric.data)[i])
  }
  else{
    cat('\nNo rechazamos la hipótesis nula para la variable numérica', names(numeric.data)[i])
  }
}




for(i in 3:ncol(data)-1 ) {
  if (fligner.test(data[,i]~diagnosis,data=data)$p.value<0.05){
    cat('\nRechazamos la hipótesis de que las varianzas de ambas muestras sean homogéneas ',names(data)[i])
  }
  else{
    cat('\nNo rechazamos la hipótesis de que las varianzas de ambas muestras sean homogéneas', names(data)[i])
  }
}







numeric.data$diagnosis <- as.integer(factor(data$diagnosis))-1

correlations <- cor(numeric.data,method="pearson")

corrplot(correlations, number.cex = .9, method = "square", 
         hclust.method = "ward", order = "FPC",
         type = "lower", tl.cex=0.8,tl.col = "black")


#Visualización de valores extremos (outliers) por grupos
mm<-melt(data, id=c('id','diagnosis'))
ggplot(mm[c(1:4552),])+geom_boxplot(aes(x=diagnosis, y=value))+facet_grid(.~variable)
ggplot(mm[c(4553:8535),])+geom_boxplot(aes(x=diagnosis, y=value))+facet_grid(.~variable)
ggplot(mm[c(8536:13656),])+geom_boxplot(aes(x=diagnosis, y=value))+facet_grid(.~variable)
ggplot(mm[c(13657:17070),])+geom_boxplot(aes(x=diagnosis, y=value))+facet_grid(.~variable)


#FeaturePlot
featurePlot(x=data[,c(3:32)], y=data[,2], plot="density",
            scales = list(x = list(relation="free"), y = list(relation="free"),cex=0.8),
            layout = c(3,10), auto.key = list(columns = 2), pch = "|")


numeric.data <- data[,2:32]
numeric.data $diagnosis = as.integer(factor(numeric.data $diagnosis))-1

set.seed(314)
#Separamos el conjunto de datos en un conjunto de entrenamiento y test
training.index <- sample(1:nrow(numeric.data), 0.8 * nrow(numeric.data))
training.data = numeric.data[training.index,]
test.data = numeric.data[-training.index,]
random.forest<-randomForest(diagnosis ~ ., data=training.data, ntree=100,keep.forest=TRUE, importance=TRUE)
variable.importance <- data.frame(random.forest$importance)

ggplot(variable.importance, aes(x=reorder(rownames(variable.importance),X.IncMSE), y=X.IncMSE)) +geom_bar(stat="identity", fill="darkolivegreen", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +labs(title="Prediction using RandomForest with 100 trees")+ylab("")+xlab("")

library(pROC)
breast.pred <- predict(random.forest,test.data)

print(sprintf("Area under curve (AUC) : %.3f",auc(test.data$diagnosis, breast.pred)))
