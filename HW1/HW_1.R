
getwd()

# importando arqiuvo com os dados a serem pre-processados ****************************************************
mydata <- read.csv("data_set_sample_glass.txt", header=FALSE, col.names=c("Id number","RI","Na",
                                                                   "Mg", "Al", "Si", "K","Ca", "Ba", "Fe",
                                                                   "Type of glass" ))
# plot do histograma de cada preditor*************************************************************************

hist(mydata$Id.number)
hist(mydata$RI)
hist(mydata$Na)
hist(mydata$Mg)
hist(mydata$Al)
hist(mydata$Si)
hist(mydata$K)
hist(mydata$Ca)
hist(mydata$Ba)
hist(mydata$Fe)
hist(mydata$Type.of.glass)

# calculo da media do valor de cada preditor independente da classe*******************************************

mean(mydata$Id.number)
mean(mydata$RI)
mean(mydata$Na)
mean(mydata$Mg)
mean(mydata$Al)
mean(mydata$Si)
mean(mydata$K)
mean(mydata$Ca)
mean(mydata$Ba)
mean(mydata$Fe)
mean(mydata$Type.of.glass)

# calculo do desvio padrao de cada prditor independente das classes********************************************************************************

sd(mydata$Id.number)
sd(mydata$RI)
sd(mydata$Na)
sd(mydata$Mg)
sd(mydata$Al)
sd(mydata$Si)
sd(mydata$Ca)
sd(mydata$Ba)
sd(mydata$Fe)
sd(mydata$Type.of.glass)

# package para utilizar a funcao skewness ********************************************************************************

#install.packages("moments")

library(moments)

# calculo da obliquidade dos preditores ********************************************************************************

skewness(mydata$Id.number)
skewness(mydata$RI)
skewness(mydata$Na)
skewness(mydata$Mg)
skewness(mydata$Al)
skewness(mydata$Si)
skewness(mydata$Ca)
skewness(mydata$Ba)
skewness(mydata$Fe)
skewness(mydata$Type.of.glass)

# package para culcular histogram( ~ RI | factor(Type.of.glass) , mydata) ********************************************************************************

library(lattice)

attach(mydata)

histogram( ~ RI | factor(Type.of.glass) , mydata)
histogram( ~ Na | factor(Type.of.glass) , mydata)
histogram( ~ Mg | factor(Type.of.glass) , mydata)
histogram( ~ Al | factor(Type.of.glass) , mydata)
histogram( ~ Si | factor(Type.of.glass) , mydata)
histogram( ~ Ca | factor(Type.of.glass) , mydata)
histogram( ~ Ba | factor(Type.of.glass) , mydata)
histogram( ~ Fe | factor(Type.of.glass) , mydata)
histogram( ~ Type.of.glass| factor(Type.of.glass) , mydata)

# calculo da class-conditional mean,SD e skewness ********************************************************************************

aggregate(Id.number ~ Type.of.glass , data=mydata, mean)
aggregate(RI ~ Type.of.glass , data=mydata, mean)
aggregate(Na ~ Type.of.glass , data=mydata, mean)
aggregate(Mg ~ Type.of.glass , data=mydata, mean)
aggregate(Al ~ Type.of.glass , data=mydata, mean)
aggregate(Si ~ Type.of.glass , data=mydata, mean)
aggregate(K ~ Type.of.glass , data=mydata, mean)
aggregate(Ca ~ Type.of.glass , data=mydata, mean)
aggregate(Ba ~ Type.of.glass , data=mydata, mean)
aggregate(Fe ~ Type.of.glass , data=mydata, mean)

aggregate(Id.number ~ Type.of.glass , data=mydata, sd)
aggregate(RI ~ Type.of.glass , data=mydata, sd)
aggregate(Na ~ Type.of.glass , data=mydata, sd)
aggregate(Mg ~ Type.of.glass , data=mydata, sd)
aggregate(Al ~ Type.of.glass , data=mydata, sd)
aggregate(Si ~ Type.of.glass , data=mydata, sd)
aggregate(K ~ Type.of.glass , data=mydata, sd)
aggregate(Ca ~ Type.of.glass , data=mydata, sd)
aggregate(Ba ~ Type.of.glass , data=mydata, sd)
aggregate(Fe ~ Type.of.glass , data=mydata, sd)

aggregate(Id.number ~ Type.of.glass , data=mydata, skewness)
aggregate(RI ~ Type.of.glass , data=mydata, skewness)
aggregate(Na ~ Type.of.glass , data=mydata, skewness)
aggregate(Mg ~ Type.of.glass , data=mydata, skewness)
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

pairs(mydata,panels = points, col = mydata$Type.of.glass,pch=16)

correlation <- cor(mydata)

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
autoplot(pcaObject,mydata[,-c(1,11)],col = mydata$Type.of.glass, loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3)


