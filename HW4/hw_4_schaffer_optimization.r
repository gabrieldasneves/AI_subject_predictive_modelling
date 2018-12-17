# Homework 4: 
install.packages("plot3D")
install.packages("fGarch")
install.packages("GA")

library("GA")
library("fGarch")
library("plot3D")

#definindo a funcao schaffer em codigo: lembrando que 
#ela faz parte de um conjunto de funções utilizadas para testar a eficácia do
#método de optimização que vc pretende utilizar
schaffer <-function(x1,x2){
  return(    0.5 + (   ((sin(x1^2 - x2^2))^2 - 0.5)/(1 + 0.001*(x1^2 + x2^2))^2    )     )
}

# definindo o dominio da funcao 
x1 <- x2 <- seq(-100,100)

#  instanciando a primeira funcao schaffer
# outer(x,y,func) -> faz o produto dos arrays x e y e a func usa esse produto como dominio, ou seja 
# faz a função 3D
schaffer1 <- outer(x1, x2, schaffer)

# plot da funcao colorida: theta e phi são inclinações. ou seja, angulos de visão do gráfico... 
# so o jeito que vc vizualiza: de lado, de cima, inclinado tantos graus en tal direção etc...
#corpalette é so p deixar colorido bem roxeda
persp3D(x1, x2, schaffer1, theta = -45, phi = 15, color.palette = jet.colors)

# mostra a vista de cima do grafico 3D com a profundidade de acordo com as cores
filled.contour(x1,x2,schaffer1, color.palette = jet.colors)

#pequena modificacao na logica da declaracao da funcao: so p ela usar o "x" como variável 
#p usar na função GA
# é a mesma coisa que implementar a função matematicamente (que nem na linha 13)
x <- c(x1,x2)
schaffer <-function(x){
  return(    0.5 + (   ((sin(x[1]^2 - x[2]^2))^2 - 0.5)/(1 + 0.001*(x[1]^2 + x[2]^2))^2    )     )
}

#funcao de otimizacao que vai ser usada p comparar com o AG:
#essa função por padrão usa o método do Gradiente que vai fazendo as derivadas até achar um minimo ou máximo
# como vimos na cadeira isso não é muito indicado... 
opt <- optim(par = c(-100, 100), schaffer )
#printa os valores da função:
opt

# Algoritmo Genético
# a biblioteca GA permite-nos utilizar essa implementação genérica de um algoritimo genetico
GA <- ga( type = "real-valued",
          fitness = function(x) - schaffer(x),
          lower = c(-100, -100),
          upper = c( 100,  100),
          population = gaControl("real-valued")$population,
          selection = gaControl("real-valued")$selection,
          crossover = gaControl("real-valued")$crossover,
          mutation = gaControl("real-valued")$mutation,
          popSize = 100,
          pcrossover = 0.7,
          pmutation = 0.1,
          elitism = 5,
          maxiter = 100,
          run = 50)
# só p resumir os resultados do nosso GA;
summary(GA)
#plota o gráfico do GA
plot(GA)

# Quando voce rodar o comando summary(GA) ele vai entregar os pontos x1 e x2. Aplique na fun??o schaffer e veja o valor y.
# Entao compara com o valor da funcao optim().
valorga <- ( 0.5 + (((5.65052e-05)^2 - (7.925928e-05)^2))^2 - 0.5)/(1 + 0.001*((5.65052e-05)^2 + (7.925928e-05)^2))^2 