
setwd("D:/Dropbox/Data Science/Curso de Extensão/IME - Introdução a Aprendizagem Estatistica e a Ciencia de Dados em R/Programas Tarefas")

library(leaps) # tem as funcoes de selecao de variaveis
library(glmnet) # tem as funcoes para lasso, ridge e elasticnet
library(ggplot2) # funcoes para criacao de graficos
library(plotmo) # funcoes para criar graficos com objetos do glmnet

#------------------------------------------------------------------
#------------------------- DEFINICOES
#------------------------------------------------------------------

set.seed(12345)

# carregar dados que estao no arquivo csv
dados <- read.csv("sao-paulo-properties.csv", fileEncoding = "UTF-8")
head(dados)


variaveis <- c("Price", "Condo","Size","Rooms","Toilets", "Suites","Parking",
               "Elevator","Furnished","Swimming.Pool","New",
               "Latitude","Longitude")


# somente propriedades aluguel menores que 200 m2
aluguel = dados[dados['Negotiation.Type']=='rent' & dados['Size']<=200,variaveis]
head(aluguel)

# treinamento e validacao
ids <- sample(nrow(aluguel), size = .75*nrow(aluguel), replace = FALSE)
ids


# variavel target
y <- aluguel$Price


#-------------------------------- QUESTAO 1

# modelo best subset: 7

bs <- regsubsets(Price ~ ., # forumula
                   data = aluguel[ids,], # dados
                   nvmax = ncol(aluguel)-1, # numero max de variaveis
                   method = "exhaustive") # metodo
  
  
bs_resumo = summary(bs)  
bs_resumo

bs_num_coef = which.min(bs_resumo$bic)
bs_num_coef # 7

bs_best_coef = coef(bs,bs_num_coef)
bs_best_coef  


# erro dentro da amostra de treinamento: 1545430
dados_matriz = model.matrix(Price ~ ., data=aluguel)
dados_matriz

dentro = dados_matriz[ids, names(bs_best_coef)]
erro_dentro <- mean(( dentro %*% 
                        bs_best_coef - aluguel[ids,'Price'])^2) 
erro_dentro 

# erro fora da amostra de treinamento: 2007153

fora = dados_matriz[-ids, names(bs_best_coef)]
erro_fora <- mean(( fora %*% 
                      bs_best_coef - aluguel[-ids,'Price'])^2) 
erro_fora  
  
  
  
  
#-------------------------------- QUESTAO 2

# modelo selecao progressiva: 7

fw <- regsubsets(Price ~ ., # forumula
                       data = aluguel[ids,], # dados
                       nvmax = ncol(aluguel)-1, # numero max de variaveis
                       method = "forward") # metodo


fw_resumo = summary(fw)  
fw_resumo

fw_num_coef = which.min(fw_resumo$bic)
fw_num_coef 
fw_best_coef = coef(fw,fw_num_coef)
fw_best_coef  


# erro dentro da amostra de treinamento: 1545430
dentro = dados_matriz[ids, names(fw_best_coef)]
erro_dentro <- mean(( dentro %*% 
                        fw_best_coef - aluguel[ids,'Price'])^2) 
erro_dentro 

# erro fora da amostra de treinamento: 2007153

fora = dados_matriz[-ids, names(fw_best_coef)]
erro_fora <- mean(( fora %*% 
                      fw_best_coef - aluguel[-ids,'Price'])^2) 
erro_fora  



#-------------------------------- QUESTAO 3
# Regressao Linear com penalizacao LASSO


# preparacao de variaveis para os modelos usando glmnet
X = model.matrix(Price ~ .,
                 data = aluguel)[,-1] #tira a coluna (Intercept)

y = aluguel$Price

# Lasso com validacao cruzada: lambda minimo eh 2, lambda.1se eh 100
cv_lasso = cv.glmnet(X[ids,],y[ids],alpha = 1,lambda = c(0.01, 0.1, 1, 2, 10, 100))
plot(cv_lasso, cex.lab = 1.3)

lambda_min = cv_lasso$lambda.min




# retreina o modelo considerando o lambda min
lasso <- glmnet(X[ids,], y[ids], alpha = 1, 
                lambda = lambda_min)

y_lasso_dentro <- predict(lasso, newx = X[ids,],
                          s = lambda_min) # valor predito dentro da amostra

y_lasso_fora <- predict(lasso, newx = X[-ids,],
                        s = lambda_min) # valor predito fora da amostra
                        
lasso_erro_dentro <- mean((y_lasso_dentro - y[ids])^2)
lasso_erro_dentro

lasso_erro_fora <- mean((y_lasso_fora - y[-ids])^2)
lasso_erro_fora




#-------------------------------- QUESTAO 4
# le arquivo para teste
teste_final <- read.csv("sao-paulo-properties-test.csv", fileEncoding = "UTF-8")
head(teste_final)

# Apenas locacao e menor que 200 m2
aluguel_teste = teste_final[teste_final['Negotiation.Type']=='rent' & teste_final['Size']<=200,variaveis]
head(aluguel_teste)

# define o target
y_teste = aluguel_teste['Price']
y_teste

# preparacao de variaveis para os modelos usando glmnet
X_teste = model.matrix(Price ~ .,
                 data = aluguel_teste)[,-1] #tira a coluna (Intercept)

y_lasso_pred <- predict(lasso, newx = X_teste,
                        s = lambda_min) # valor predito fora da amostra

ao_quadrado = (y_lasso_pred - y_teste)^2
lasso_erro_final <- mean(ao_quadrado[1:nrow(ao_quadrado),]) # R maluco nao faz a media dessa porcaria

lasso_erro_final

