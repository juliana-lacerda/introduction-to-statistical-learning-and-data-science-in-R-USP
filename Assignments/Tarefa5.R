
setwd("D:/Dropbox/Data Science/Curso de Extensão/IME - Introdução a Aprendizagem Estatistica e a Ciencia de Dados em R/Programas Tarefas")

library(ggplot2) # criacao de graficos
library(e1071)  # funcoes de modelos de aprendizagem, inclusive de naive bayes
library(class) # funcao para o knn

#------------------------------------------------------------------
#------------------------- DEFINICOES
#------------------------------------------------------------------
#formatC(numero, format = "f", digits = 4)

#carrega dados e divide em conjunto de treino e validacao

library(ISLR)
data(Default)
set.seed(4567)
ids <- sample(1:nrow(Default), size = 0.6 * nrow(Default))
dados_tr <- Default[ids,]
dados_val <- Default[-ids,]


head(dados_tr)







#-------------------------------- QUESTAO 1: 0,9737
#PRECISAO DENTRO DA AMOSTRA

# modelo de regressão logística
modelo_rl = glm(default ~ student+balance+income, data = dados_tr, family = "binomial")

#probabilidades
pred_probs_tr = predict(modelo_rl,dados_tr,type='response')

#labels
pred_labels_tr = ifelse(pred_probs_tr>0.5,1,0)

#observado vrs predito
tabela_tr = table(predito = pred_labels_tr, observado = dados_tr$default)

tabela_tr

#precisao dentro da amostra
prec_dentro = (tabela_tr[1,1]+tabela_tr[2,2])/nrow(dados_tr)
formatC(prec_dentro, format = "f", digits = 4)


#-------------------------------- QUESTAO 2: 0,9730
#PRECISAO FORA DA AMOSTRA

#probabilidades
pred_probs_val = predict(modelo_rl,dados_val,type='response')

#labels
pred_labels_val = ifelse(pred_probs_val>0.5,1,0)

#observado vrs predito
tabela_val = table(predito = pred_labels_val, observado = dados_val$default)
tabela_val

prec_fora = (tabela_val[1,1]+tabela_val[2,2])/nrow(dados_val)

formatC(prec_fora, format = "f", digits = 4)


#-------------------------------- QUESTAO 3: 0,9715
#PRECISAO DENTRO DA AMOSTRA

# modelo de Naive Bayes
modelo_nb = naiveBayes(default ~ student+income+balance, data=dados_tr)

#predicao
class_nb_tr = predict(modelo_nb,dados_tr)

#observado vrs predito
tabela_tr = table(predito = class_nb_tr, observado = dados_tr$default)
tabela_tr

#precisao dentro da amostra
prec_dentro = (tabela_tr[1,1]+tabela_tr[2,2])/nrow(dados_tr)
formatC(prec_dentro, format = "f", digits = 4)


#-------------------------------- QUESTAO 4: 0,9688
#PRECISAO FORA DA AMOSTRA

#predicao
class_nb_val = predict(modelo_nb,dados_val)

#observado vrs predito
tabela_val = table(predito = class_nb_val, observado = dados_val$default)
tabela_val

#precisao dentro da amostra
prec_fora = (tabela_val[1,1]+tabela_val[2,2])/nrow(dados_val)
formatC(prec_fora, format = "f", digits = 4)






