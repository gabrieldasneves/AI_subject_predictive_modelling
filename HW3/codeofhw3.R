library(class)
library(dplyr)
library(lubridate)
library(readr)
library(caret)
library(pROC)
library(ROSE)
set.seed(100)

reduzido <- read_csv("R/Homeworks/Homework3/grantData_hw3/reducedSet.csv")
training <- read_csv("R/Homeworks/Homework3/grantData_hw3/training.csv")
testing <- read_csv("R/Homeworks/Homework3/grantData_hw3/testing.csv")

# REGRESS�O LOG�STICA
# Conjunto de dados contendo 1 n = 8708 observações relacionadas a pedidos de subs�?dios
# Para cada observação de pedido de subs�?dio, há D = 1882 preditores.
# os dados que devem ser considerados sao os dados do reducedSet (252 preditores)
# bem sucedido e mal sucedido são as classes
# As observações e a classe são divididas entre conjuntos de treinamento e teste 
# e dados nos conjuntos de dados de treinamento (N tr = 8190) e teste (N ts = 518).
help("head")
attach(training)
# Conhecendo os dados:
# Função names() nomeia ou pega os nomes 
predictors = names(training)
# Função dim() retorna ou atribui dimensão para um objeto
dim ( training )
# Função summary() usada para produzir resumos de resultados 
summary ( training )
# pairs() : gera a matriz de gráficos de dispersão 
pairs ( training )
# Para a matriz de correlação:
cor ( training )

# codigo da regressão logistica seguido pelo livro ISLR
# Criar modelo Logit no conjunto de dados de treinamento
# glm() gera modelos lineares generalizados e o argumento family="binomial" faz o R executar uma regressão log�?stica.
glm.fits <- glm(Class ~ . , family="binomial", data = training[,reducedSet])
coef (glm.fits)
summary(glm.fits)$coef[,4]

#  Preveja a saida no conjunto de dados de teste
glm.probs=predict(glm.fits,testing,type="response")
glm.probs
# contrasts() fornece os contrastes associados a um fator.
contrasts(Class)

#  converter as probabilidades previstas em alguma das duas classes
pred_lim <-0.5
glm.pred=rep("successful",nrow(testing))
glm.pred[glm.probs> pred_lim]="unsuccessful"

# checando
head(glm.pred)

ldaRoc = roc.curve(testing$Class,glm.probs)


# KNN - Vari�veis de controle:
pre2008 <- 1:6633                                   # Criado para selecionar as amostras de antes de 2008
reducedSet <- as.list(reduzido)                     # Primeiro se converte o conjunto reducedSet para lista
reducedSet <- unlist(reducedSet, use.names = FALSE) # Depois se converte para um vetor de nomes

# Este c�digo � respons�vel para ajustar os par�metros na base da curva ROC
# A fun��o twoClassSummary calcula a �rea abaixo da curva ROC, a sensibilidade e a especificidade.
ctrl <- trainControl(method = "LGOCV", summaryFunction = twoClassSummary,
                     classProbs = TRUE, index = list(TrainSet = pre2008),
                     savePredictions = TRUE)

# Este c�digo gera o modelo KNN a partir dos par�metros da fun��o train
# O c�digo training[,reducedSet] permite que apenas as amostras de antes de 2008 sejam selecionadas.
knnFit <- train(training[,reducedSet], training$Class,
                method = "knn", metric = "ROC",
                preProc = c("center", "scale"),
                tuneGrid = data.frame(k = c(4*(0:5)+1,20*(1:5)+1,50*(2:9)+1)),
                trControl = ctrl)
knnFit

plot(knnFit) # Para ver a quantidade de vizinhos VS desempenho

# Este c�digo prev� as sa�das do conjunto de teste e a curva ROC correspondente.
knnFit$pred <- merge(knnFit$pred,  knnFit$bestTune)
knnCM <- confusionMatrix(knnFit, norm = "none")
knnCM
knnRoc <- roc(response = knnFit$pred$obs, predictor = knnFit$pred$successful,
              levels = rev(levels(knnFit$pred$obs)))
plot(knnRoc, legacy.axes = TRUE, main = "Curva ROC - KNN")

# Predi��o do conjunto de teste e matriz de confus�o
valid <- predict(knnFit, testing[,reducedSet])
validCM <- table(valid, testing$Class)
# Digite typeof(OBJETO) para saber o tipo do objeto