
getwd()

# importando arqiuvo com os dados a serem pre-processados ************************************************
mydata <- read.csv("dados_homework.txt", header=FALSE, col.names=c("Id number","RI","Na",
                                                                   "Mg", "Al", "Si", "K","Ca", "Ba", "Fe",
                                                                   "Type of glass" ))


# plot do histograma de cada preditor*************************************************************************


hist(mydata$RI, main="Histogram of RI")
hist(mydata$Na, main="Histogram of Na")
hist(mydata$Mg, main="Histogram of Mg")
hist(mydata$Al, main="Histogram of Al")
hist(mydata$Si, main="Histogram of Si")
hist(mydata$K, main="Histogram of K")
hist(mydata$Ca, main="Histogram of Ca")
hist(mydata$Ba, main="Histogram of Ba")
hist(mydata$Fe, main="Histogram of Fe")
# calculo da media do valor de cada preditor independente da classe*******************************************


mean(mydata$RI)
mean(mydata$Na)
mean(mydata$Mg)
mean(mydata$Al)
mean(mydata$Si)
mean(mydata$K)
mean(mydata$Ca)
mean(mydata$Ba)
mean(mydata$Fe)
# calculo do desvio padrao de cada prditor independente das classes********************************************************************************
sd(mydata$RI)
sd(mydata$Na)
sd(mydata$Mg)
sd(mydata$Al)
sd(mydata$Si)
sd(mydata$K)
sd(mydata$Ca)
sd(mydata$Ba)
sd(mydata$Fe)
# package para utilizar a funcao skewness ****************************************************************************

install.packages("moments")
library(moments)
# calculo da obliquidade dos preditores ****************************************************************************


skewness(mydata$RI)
skewness(mydata$Na)
skewness(mydata$Mg)
skewness(mydata$Al)
skewness(mydata$Si)
skewness(mydata$K)
skewness(mydata$Ca)
skewness(mydata$Ba)
skewness(mydata$Fe)

# package para calcular histogram condicionado a classe  ****************************************************************************
library(lattice)
attach(mydata)


histogram( ~ RI | factor(Type.of.glass) , mydata)
histogram( ~ Na | factor(Type.of.glass) , mydata)
histogram( ~ Mg | factor(Type.of.glass) , mydata)
histogram( ~ Al | factor(Type.of.glass) , mydata)
histogram( ~ Si | factor(Type.of.glass) , mydata)
histogram( ~ K | factor(Type.of.glass) , mydata)
histogram( ~ Ca | factor(Type.of.glass) , mydata)
histogram( ~ Ba | factor(Type.of.glass) , mydata)
histogram( ~ Fe | factor(Type.of.glass) , mydata)
# calculo da class-conditional mean,SD e skewness ****************************************************************************

aggregate(RI ~ Type.of.glass , data=mydata, mean)
aggregate(Na ~ Type.of.glass , data=mydata, mean)
aggregate(Mg ~ Type.of.glass , data=mydata, mean)
aggregate(Al ~ Type.of.glass , data=mydata, mean)
aggregate(Si ~ Type.of.glass , data=mydata, mean)
aggregate(K ~ Type.of.glass , data=mydata, mean)
aggregate(Ca ~ Type.of.glass , data=mydata, mean)
aggregate(Ba ~ Type.of.glass , data=mydata, mean)
aggregate(Fe ~ Type.of.glass , data=mydata, mean)


aggregate(RI ~ Type.of.glass , data=mydata, sd)
aggregate(Na ~ Type.of.glass , data=mydata, sd)
aggregate(Mg ~ Type.of.glass , data=mydata, sd)
aggregate(Al ~ Type.of.glass , data=mydata, sd)
aggregate(Si ~ Type.of.glass , data=mydata, sd)
aggregate(K ~ Type.of.glass , data=mydata, sd)
aggregate(Ca ~ Type.of.glass , data=mydata, sd)
aggregate(Ba ~ Type.of.glass , data=mydata, sd)



aggregate(RI ~ Type.of.glass , data=mydata, skewness)
aggregate(Na ~ Type.of.glass , data=mydata, skewness)
aggregate(Mg ~ Type.of.glass , data=mydata,skewness)
aggregate(Al ~ Type.of.glass , data=mydata, skewness)
aggregate(Si ~ Type.of.glass , data=mydata, skewness)
aggregate(K ~ Type.of.glass , data=mydata, skewness)
aggregate(Ca ~ Type.of.glass , data=mydata, skewness)
aggregate(Ba ~ Type.of.glass , data=mydata, skewness)
aggregate(Fe ~ Type.of.glass , data=mydata, skewness)


#scatter plot*********************************************************************************
install.packages("corrplot")

library(corrplot)

attach(mtcars)

mydata_preditors <- subset( mydata, select = -c(Type.of.glass, Id.number ) )

pairs(mydata_preditors,panels = points, col = mydata$Type.of.glass,pch=16)
#matriz de correlação*****************************************************

correlation <- cor(mydata_preditors)

corrplot(correlation, order = "hclust")

#PCA*********************************************************************************
install.packages("factoextra")
library(factoextra)
pcaObject <- prcomp(mydata[,-c(1,11)],center = TRUE, scale. = TRUE)
plot(pcaObject)
#nesse plot vemos os que tem as duas maiores variancias 


pcaObject$rotation #para visualizar os valores
pcaObject$sdev



#######################################################################################
install.packages("ggplot2")
install.packages("ggfortify")
library(ggfortify)
library(ggplot2)
attach(mtcars)

install.packages("mlbench")
library(mlbench)
data(Glass) 
attach(Glass)

PCA <- prcomp(Glass[,1:9], center = TRUE,scale. = TRUE)
autoplot(PCA, data= Glass, colour = 'Type', loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3)