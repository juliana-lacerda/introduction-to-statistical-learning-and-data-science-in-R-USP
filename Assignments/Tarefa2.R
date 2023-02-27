
setwd("D:/Dropbox/Data Science/Curso de Extensão/IME - Introdução a Aprendizagem Estatistica e a Ciencia de Dados em R/Programas Tarefas")

#------------------------------------------------------------------
#------------------------- QUESTAO 1
#------------------------------------------------------------------
formatC(numero, format = "f", digits = 4)

modelo1 = function(x1,x2){
    b0 = 2.38
    b1 = 8.31
    b2 = -4.03
    
    y = b0 + b1*x1 + b2*x2
    return(y)
}

erro_quadratico1 = function(y_real,y_pred){
  L = (y_real-y_pred)^2
  return(L)
}



#----- a
y_real_q1 = 50.84
x1 = 7.47
x2 = 3.44
y_pred_q1 = modelo1(x1,x2)
y_pred_q1 # 50.5925



#------ b
L_q1 = erro_quadratico1(y_real_q1,y_pred_q1)
L_q1 # 0,06125625



#------------------------------------------------------------------
#------------------------- QUESTAO 2
#------------------------------------------------------------------

modelo2 = function(x1,x2){
  b0 = 1.98
  b1 = 2.87
  b2 = -2.76
  b3 = -2.88
  b4 = -0.75
  
  y = b0 + b1*x1 + b2*x2 + b3*x1^2 + b4* x2^2
  return(y)
}

#----- a
y_real_q2 = -249.88
x1 = 9.57
x2 = 2.85
y_pred_q2 = modelo2(x1,x2)
y_pred_q2 # -248,2765



#------ b
L_q2 = erro_quadratico1(y_real_q2,y_pred_q2)
L_q2 # 2.571254




#------------------------------------------------------------------
#------------------------- QUESTAO 3
#------------------------------------------------------------------

formatC(numero, format = "f", digits = 3)

# carregar dados que estao no arquivo csv
dados <- read.csv("sao-paulo-properties.csv",
                  fileEncoding = "UTF-8")


head(dados)

# somente propriedades aluguel
df = dados[dados['Negotiation.Type']=='rent',]

head(df)

# variavel target
y <- df$Price

# modelo de predicao
linear_model = lm(y ~ Condo + Size + Rooms + Toilets + Suites + Parking + Furnished, df)

summary(linear_model)

# coefifientes do modelo
linear_model$coefficients


#------------ a

#predicao
novos_dados = data.frame(1000,55,2,1,0,1,0)
colnames(novos_dados) = c('Condo', 'Size', 'Rooms', 'Toilets', 'Suites', 'Parking', 'Furnished')
novos_dados

y_pred_q3a = predict(linear_model,novos_dados)
y_pred_q3a #1678.432



#------------ b 
# Erro Estimado Dentro da Amostra : E_D_hat = 1/n * sum(L(y_i,y_pred_i))


y_pred_q3b = predict(linear_model,df[c('Condo', 'Size', 'Rooms', 'Toilets', 'Suites', 'Parking', 'Furnished')])


E_D_hat_q3b = 0

for (i in 1:nrow(df)){
  E_D_hat_q3b = E_D_hat_q3b + erro_quadratico1(y[i],y_pred_q3b[i])
}

E_D_hat_q3b = E_D_hat_q3b/nrow(df)
E_D_hat_q3b #4815078



#------------ c

# transformacao de variaveis
df['Rooms_2'] = df['Rooms']^2
df['Suites_2'] = df['Suites']^2
df['Suites_3'] = df['Suites']^3

linear_model_q3c = lm(y ~ Condo + Size + Rooms + Toilets + Suites + Parking + Furnished + Rooms_2 + Suites_2 + Suites_3, df)


y_pred_q3c = predict(linear_model_q3c,df[c('Condo', 'Size', 'Rooms', 'Toilets', 'Suites', 'Parking', 'Furnished', 'Rooms_2', 'Suites_2', 'Suites_3')])


E_D_hat_q3c = 0

for (i in 1:nrow(df)){
  E_D_hat_q3c = E_D_hat_q3c + erro_quadratico1(y[i],y_pred_q3c[i])
}

E_D_hat_q3c = E_D_hat_q3c/nrow(df)
E_D_hat_q3c #4788290

























