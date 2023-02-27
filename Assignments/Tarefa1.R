# O codigo abaixo gera uma matriz com 100 linhas e 50 colunas.
#
# set.seed(338)
# ma <- matrix(rnorm(100*50), ncol=50)
#
# Considere que cada linha corresponde a uma amostra independente com 50 observaÃ§Ãµes de uma variavel aleatoria X com distribuicao 
# normal de parametros mu=0 e sigma^2=1. Responda a cada um dos pontos abaixo:
#  
# 1. A media estimada para a amostra correspondente a primeira linha da matriz eh:
#
# 2. A variancia estimada para a amostra correspondente a primeira linha da matriz eh: 
#
# 3. Considere o parametro theta=E(|X-E(X)|), o desvio absoluto medio em relacao a media, que no caso da distribuicao normal de parametros
# mu=0 e sigma^2=1 eh igual a sqrt(2/pi). Considere o estimador theta_hat = 1/n*sum(|x_i - x_bar|) para theta, 
# baseado na amostra x1,...,xn. Para cada uma das amostras (linhas da matriz ma), calcule theta_hat e logo apresente uma estimativa de:
#  
#  a. O vies de theta_hat em relacao a theta
#
# b. A variancia de theta_hat
#
# c. O erro quadratico medio de theta_hat em relacao a theta

setwd("D:/Dropbox/Data Science/Curso de Extensão/IME - Introdução a Aprendizagem Estatistica e a Ciencia de Dados em R/Programas Tarefas")

set.seed(134)

ma <- matrix(rnorm(100*50), ncol=50)

tam = length(ma[1,])

# -------------------------- 1
media = sum(ma[1,])/tam
media

# -------------------------- 2

somatorio = 0
for (i in 1:tam){
  somatorio = somatorio + (ma[1,i]-media)^2
}
variancia = somatorio/(tam-1)
variancia

# -------------------------- ESTIMADOR: theta_hat = E[x] = 1/n somatorio abs(x_i-media(x))
#3

func_esperanca = function(x){
  somatorio = 0
  media = mean(x)
  
  for (i in 1:length(x)){
    somatorio = somatorio + abs(x[i] - media)
  }  
  return(somatorio/length(x))
}
  
  
theta = sqrt(2/pi)

theta_hat = numeric(nrow(ma))

for (i in 1:nrow(ma)){
  x = ma[i,]
  theta_hat[i] = func_esperanca(x)
}

# -------------------------- 3(a)
# vies = E(theta_hat) - theta

#esperanca_theta_hat = func_esperanca(theta_hat)
#vies = esperanca_theta_hat - theta
#vies

vies = mean(theta_hat) - theta
vies


# -------------------------- 3(b)

# b variancia = E(theta_hat^2) - E[theta_hat]^2

#esp_theta_hat_quadrado = func_esperanca(theta_hat^2)
#esp_quadrado_theta_hat = func_esperanca(theta_hat)^2
#variancia = esp_theta_hat_quadrado - esp_quadrado_theta_hat
#variancia

# b variancia = 1/(n-1) * somatorio (x-media)^2

somatorio = 0
media = mean(theta_hat)
for (i in 1:length(theta_hat)){
  somatorio = somatorio + (theta_hat[i]-media)^2
}
variancia = somatorio/(tam-1)
variancia

# -------------------------- 3(c)
# eqm = variancia + vies^2 = E[(theta_hat-theta)^2]

#eqm = variancia + vies^2
#eqm

x = (theta_hat - theta)^2
eqm = func_esperanca(x)
eqm


# a - Calculo do Vies com a equacao do eqm
vies = sqrt(abs(eqm-variancia))
vies
