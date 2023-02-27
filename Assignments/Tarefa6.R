
setwd("D:/Dropbox/Data Science/Curso de Extensão/IME - Introdução a Aprendizagem Estatistica e a Ciencia de Dados em R/Programas Tarefas")

library(tree)
library(randomForest)

#------------------------------------------------------------------
#------------------------- DEFINICOES
#------------------------------------------------------------------
#formatC(numero, format = "f", digits = 4)

dados <- read.csv("sao-paulo-properties.csv", fileEncoding = "UTF-8")

variaveis <- c("Price", "Condo","Size","Rooms","Toilets", "Suites","Parking",
               "Elevator","Furnished","Swimming.Pool","New",
               "Latitude","Longitude")

aluguel <- dados[dados$Negotiation.Type == "rent" & dados$Size <=200, variaveis]

set.seed(12345)
ids <- sample(nrow(aluguel), size = .75*nrow(aluguel), replace = FALSE)

y = aluguel$Price

#-------------------------------- QUESTAO 1: 1673451, 2114715

arvore <- tree(Price ~ . , data = aluguel[ids,])

# DENTRO AMOSTRA
arvore_pred_dentro <- predict(arvore,
                    newdata= aluguel[ids,],
                     type = "vector")

arvore_erro_dentro <- mean(( arvore_pred_dentro - y[ids])^2)
arvore_erro_dentro

# FORA  AMOSTRA
arvore_pred_fora <- predict(arvore,
                              newdata= aluguel[-ids,],
                              type = "vector")

arvore_erro_fora <- mean(( arvore_pred_fora - y[-ids])^2)
arvore_erro_fora


#-------------------------------- QUESTAO 2: 221725.1, 1207172
# mtry: considerando como 4 o número de variáveis aleatoriamente selecionadas como candidatas em cada split.
set.seed(1)
floresta <- randomForest(Price ~ ., data = aluguel[ids,], mtry = 4)

# DENTRO AMOSTRA
floresta_pred_dentro <- predict(floresta,
                              newdata= aluguel[ids,],
                              type = "response")

floresta_erro_dentro <- mean(( floresta_pred_dentro - y[ids])^2)
floresta_erro_dentro

# FORA  AMOSTRA
floresta_pred_fora <- predict(floresta,
                            newdata= aluguel[-ids,],
                            type = "response")

floresta_erro_fora <- mean(( floresta_pred_fora - y[-ids])^2)
floresta_erro_fora

#-------------------------------- QUESTAO 3: 167723.9, 1157607

set.seed(1)
bagging <- randomForest(Price ~ ., data = aluguel[ids,], mtry = ncol(aluguel)-1)

# DENTRO AMOSTRA
bagging_pred_dentro <- predict(bagging,
                                newdata= aluguel[ids,],
                                type = "response")

bagging_erro_dentro <- mean(( bagging_pred_dentro - y[ids])^2)
bagging_erro_dentro

# FORA  AMOSTRA
bagging_pred_fora <- predict(bagging,
                              newdata= aluguel[-ids,],
                              type = "response")

bagging_erro_fora <- mean(( bagging_pred_fora - y[-ids])^2)
bagging_erro_fora

#-------------------------------- QUESTAO 4: 913216.2

set.seed(1)

# Modelo escolhido, BAGGING

# fit na base de dados de treinamento inteira
model = randomForest(Price ~ ., data = aluguel, mtry = ncol(aluguel)-1)

# le arquivo para teste
teste_final <- read.csv("sao-paulo-properties-test.csv", fileEncoding = "UTF-8")

# Apenas locacao
aluguel_teste = teste_final[teste_final$Negotiation.Type == "rent" & teste_final$Size <=200, variaveis]

# define o target
y_teste = aluguel_teste['Price']
y_teste

# predicao
y_pred = predict(model,
                 newdata= aluguel_teste,
                 type = "response")

ao_quadrado = (y_pred - y_teste)^2
erro = mean(ao_quadrado[1:nrow(ao_quadrado),]) 
erro




