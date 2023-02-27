
setwd("D:/Dropbox/Data Science/Curso de Extensão/IME - Introdução a Aprendizagem Estatistica e a Ciencia de Dados em R/Programas Tarefas")


#------------------------------------------------------------------
#------------------------- QUESTAO 1
#------------------------------------------------------------------

erro_quadratico = function(y_real,y_pred){
  L = (y_real-y_pred)^2
  return(L)
}

#formatC(numero, format = "f", digits = 3)

set.seed(1)

# carregar dados que estao no arquivo csv
dados <- read.csv("sao-paulo-properties.csv", fileEncoding = "UTF-8")
head(dados)

# somente propriedades aluguel
aluguel = dados[dados['Negotiation.Type']=='rent',]
head(aluguel)

# 5 lotes para validacao cruzada
lote = sample(1:5,size=nrow(aluguel), replace=TRUE)
lote


# variavel target
y <- aluguel$Price


#-------------------------------- MODELO 1

erro_estimado_validacao_cruzada = numeric(5)

for (i in 1:5){
  # train test split da validacao cruzada
  x_train = aluguel[lote!=i,]
  y_train = y[lote!=i]
  x_test = aluguel[lote==i,]
  y_test = y[lote==i]
  
  
  # criacao e fit do modelo
  linear_model = lm(y_train ~ Condo + Size + Rooms + Toilets + Suites + Parking + Furnished, x_train)
  
  # previsoes
  y_pred = predict(linear_model, x_test)
  erro_estimado = erro_quadratico(y_test,y_pred)
  erro_estimado_validacao_cruzada[i] = mean(erro_estimado)

}

erro_estimado_medio_1 = mean(erro_estimado_validacao_cruzada)
erro_estimado_medio_1


#-------------------------------- MODELO 2

# cria as novas variaveis no dataframe
aluguel['Rooms_2'] = aluguel['Rooms']^2
aluguel['Suites_2'] = aluguel['Suites']^2
aluguel['Suites_3'] = aluguel['Suites']^3

erro_estimado_validacao_cruzada = numeric(5)

for (i in 1:5){
  # train test split da validacao cruzada
  x_train = aluguel[lote!=i,]
  y_train = y[lote!=i]
  x_test = aluguel[lote==i,]
  y_test = y[lote==i]
  
  
  # criacao e fit do modelo
  linear_model = lm(y_train ~ Condo + Size + Rooms + + Rooms_2 + Toilets + Suites + Suites_2 + Suites_3 + Parking + Furnished, x_train)
  
  # previsoes
  y_pred = predict(linear_model, x_test)
  erro_estimado = erro_quadratico(y_test,y_pred)
  erro_estimado_validacao_cruzada[i] = mean(erro_estimado)
  
}

erro_estimado_medio_2 = mean(erro_estimado_validacao_cruzada)
erro_estimado_medio_2




#-------------------------------- MODELO 3

# cria as novas variaveis no dataframe
aluguel['log_Size'] = log(aluguel['Size'])

erro_estimado_validacao_cruzada = numeric(5)

for (i in 1:5){
  # train test split da validacao cruzada
  x_train = aluguel[lote!=i,]
  y_train = y[lote!=i]
  x_test = aluguel[lote==i,]
  y_test = y[lote==i]
  
  
  # criacao e fit do modelo
  linear_model = lm(y_train ~ Condo + Size + log_Size + Rooms + Toilets + Suites + Parking + Furnished, x_train)
  
  # previsoes
  y_pred = predict(linear_model, x_test)
  erro_estimado = erro_quadratico(y_test,y_pred)
  erro_estimado_validacao_cruzada[i] = mean(erro_estimado)
  
}

erro_estimado_medio_3 = mean(erro_estimado_validacao_cruzada)
erro_estimado_medio_3





#---------------------------------------------- Questao 5

# Modelo escolhido, MODELO 1

# fit na base de dados de treinamento inteira
linear_model = lm(y ~ Condo + Size + Rooms + Toilets + Suites + Parking + Furnished, aluguel)

# le arquivo para teste
teste_final <- read.csv("sao-paulo-properties-test.csv", fileEncoding = "UTF-8")
head(teste_final)

# Apenas locacao
aluguel_teste = teste_final[teste_final['Negotiation.Type']=='rent',]
head(aluguel_teste)

# define o target
y_teste = aluguel_teste['Price']
y_teste

# predicao
y_pred = predict(linear_model, aluguel_teste)
y_pred

erro_estimado = erro_quadratico(y_teste,y_pred)
erro_estimado_medio = mean(erro_estimado[1:nrow(erro_estimado),]) # R maluco nao queria fazer a media dessa porcaria




