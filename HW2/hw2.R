library(ggplot2)
# install.packages("e1071")
library("e1071")
library(reshape2)
# install.packages("mlbench")
library(mlbench)
library(ggfortify)
library(corrplot)
install.packages("caret")
library(caret)
library(moments)
# para usar a biblioteca do livro:
# antes tem que executar:  install.packages(AppliedPredictiveModeling)    
library(AppliedPredictiveModeling)
data(solubility)

## os dados que começam com "sol":
ls(pattern = "^solT")


set.seed(2)
sample(names(solTrainX), 10)


#ANALISE EXPLORATORIA:
#parte 0
#Faz a separação das variáveis continuas
cont_pred <- names(solTrainX)[!grepl("FP", names(solTrainX))]
cont_predTrain <- solTrainX[,cont_pred]
cont_predTest <- solTestX[,cont_pred]


# calculando a Skewness inicial
Skew <- apply(cont_predTrain,2,skewness)
Skew

help("apply")
#Redução da skewness com o yeo-jonhson
preprocessed <- preProcess(cont_predTrain, method = "YeoJohnson")
preprocessed
cont_predTrain <- predict(preprocessed, cont_predTrain)
cont_predTrain


#Plotagem do scatterplot
png("ScatterPlotPreprocess.png", width = 1024, height = 768)
par(mfrow =c(4,5))
for(i in 1:20){
  plot(cont_predTrain[,i], solTrainY, main = names(cont_predTrain[i]))
  lines(lowess(cont_predTrain[,i], solTrainY), col="green")
}
dev.off()

#Matriz de correlalção
cont_predTrain$Solubility <- solTrainX$x
correl <- cor(cont_predTrain)
png("CorrelationMatrix.png",width = 1024, height = 768)
corrplot(correl, order = "hclust")
dev.off()

