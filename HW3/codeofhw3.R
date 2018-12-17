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

# REGRESSÃO LOGÍSTICA
# Conjunto de dados contendo 1 n = 8708 observaÃ§Ãµes relacionadas a pedidos de subsÃ?dios
# Para cada observaÃ§Ã£o de pedido de subsÃ?dio, hÃ¡ D = 1882 preditores.
# os dados que devem ser considerados sao os dados do reducedSet (252 preditores)
# bem sucedido e mal sucedido sÃ£o as classes
# As observaÃ§Ãµes e a classe sÃ£o divididas entre conjuntos de treinamento e teste 
# e dados nos conjuntos de dados de treinamento (N tr = 8190) e teste (N ts = 518).
help("head")
attach(training)
# Conhecendo os dados:
# FunÃ§Ã£o names() nomeia ou pega os nomes 
predictors = names(training)
# FunÃ§Ã£o dim() retorna ou atribui dimensÃ£o para um objeto
dim ( training )
# FunÃ§Ã£o summary() usada para produzir resumos de resultados 
summary ( training )
# pairs() : gera a matriz de grÃ¡ficos de dispersÃ£o 
pairs ( training )
# Para a matriz de correlaÃ§Ã£o:
cor ( training )

# codigo da regressÃ£o logistica seguido pelo livro ISLR
# Criar modelo Logit no conjunto de dados de treinamento
# glm() gera modelos lineares generalizados e o argumento family="binomial" faz o R executar uma regressÃ£o logÃ?stica.
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


# KNN - Variáveis de controle:
pre2008 <- 1:6633                                   # Criado para selecionar as amostras de antes de 2008
reducedSet <- as.list(reduzido)                     # Primeiro se converte o conjunto reducedSet para lista
reducedSet <- unlist(reducedSet, use.names = FALSE) # Depois se converte para um vetor de nomes

# Este código é responsável para ajustar os parâmetros na base da curva ROC
# A função twoClassSummary calcula a área abaixo da curva ROC, a sensibilidade e a especificidade.
ctrl <- trainControl(method = "LGOCV", summaryFunction = twoClassSummary,
                     classProbs = TRUE, index = list(TrainSet = pre2008),
                     savePredictions = TRUE)

# Este código gera o modelo KNN a partir dos parâmetros da função train
# O código training[,reducedSet] permite que apenas as amostras de antes de 2008 sejam selecionadas.
knnFit <- train(training[,reducedSet], training$Class,
                method = "knn", metric = "ROC",
                preProc = c("center", "scale"),
                tuneGrid = data.frame(k = c(4*(0:5)+1,20*(1:5)+1,50*(2:9)+1)),
                trControl = ctrl)
knnFit

plot(knnFit) # Para ver a quantidade de vizinhos VS desempenho

# Este código prevê as saídas do conjunto de teste e a curva ROC correspondente.
knnFit$pred <- merge(knnFit$pred,  knnFit$bestTune)
knnCM <- confusionMatrix(knnFit, norm = "none")
knnCM
knnRoc <- roc(response = knnFit$pred$obs, predictor = knnFit$pred$successful,
              levels = rev(levels(knnFit$pred$obs)))
plot(knnRoc, legacy.axes = TRUE, main = "Curva ROC - KNN")

# Predição do conjunto de teste e matriz de confusão
valid <- predict(knnFit, testing[,reducedSet])
validCM <- table(valid, testing$Class)
# Digite typeof(OBJETO) para saber o tipo do objeto