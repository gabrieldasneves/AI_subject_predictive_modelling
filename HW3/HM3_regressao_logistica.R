# Conjunto de dados contendo 1 n = 8708 observações relacionadas a pedidos de subsídios
# Para cada observação de pedido de subsídio, há D = 1882 preditores.
# os dados que devem ser considerados sao os dados do reducedSet (252 preditores)
# bem sucedido e mal sucedido são as classes
# As observações e a classe são divididas entre conjuntos de treinamento e teste 
# e dados nos conjuntos de dados de treinamento (N tr = 8190) e teste (N ts = 518).
#________________________________________________________________________________________________

#esse help é so p auxiliar caso tenha duvida em alguma função
help("head")


attach(training)

# Conhecendo os dados: esses codigos seguintes são para conhecer os dados... 
#não são necessários no HW
# Função names() nomeia ou pega os nomes 
predictors = names(training)
# Função dim() retorna ou atribui dimensão para um objeto
dim ( training )
# Função summary() usada para produzir resumos de resultados 
summary ( training )
# pairs() : gera a matriz de gráficos de dispersão 
pairs ( training )
# Para a matriz de correlação: não vai funcionar pois tem dado não numerico
cor ( training )
#_____________________________________________________________________________________________


# codigo da regressão logistica seguido pelo livro ISLR
# Criar modelo Logit no conjunto de dados de treinamento
# glm() gera modelos lineares generalizados 
#o argumento family="binomial" faz o R executar uma regressão logística.
# usou-se o training[,reducedSet] pq nesse reducedset estao os preditores menos correlatos
#isso foi dito no hw
glm.fits <- glm(Class ~ . , family="binomial", data = training[,reducedSet])
#pegando os coeficientes "betas" da regressão logistica
coef (glm.fits)
summary(glm.fits)$coef[,4]


#  Preveja a saida no conjunto de dados de teste
glm.probs=predict(glm.fits,testing,type="response")
glm.probs


# contrasts() fornece os contrastes associados a um fator.
# aqui são gerados probabilidade 1 para insucesso e probabilidade 0 p sucesso... 
#so p classificar
contrasts(Class)

#  converter as probabilidades previstas em alguma das duas classes
# aqui é dito o limite p decisão das classes pelas probab.
pred_lim <-0.5
glm.pred=rep("successful",nrow(testing))
glm.pred[glm.probs> pred_lim]="unsuccessful"

# checando
head(glm.pred)

# Os elementos diagonais da matriz de confusão indicam previsões corretas, 
# enquanto as diagonais de fora representam previsões incorretas.
table(glm.pred,testing$Class)

# porcentagem de predições corretas 
# essa é p erro
mean(glm.pred!=testing$Class)
#essa é p acerto
mean(glm.pred==testing$Class)


