
setwd("D:/Dropbox/Data Science/Curso de Extensão/IME - Introdução a Aprendizagem Estatistica e a Ciencia de Dados em R/Programas Tarefas")



#------------------------------------------------------------------
#------------------------- DEFINICOES
#------------------------------------------------------------------


set.seed(2)
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:29, 1] <- x[1:29, 1] + 3
x[1:29, 2] <- x[1:29, 2] - 4

#-------------------------------- Q1
# Utilize o código abaixo para estimar o modelo k-médias para k=2

set.seed(2)
km2 <- kmeans(x, 2, nstart = 20)

km2


# ---> O código acima atribui cada observação à um único cluster. Quantos elementos foram atribuídos ao 
# cluster com o maior número de observações? 29   
table(km2$cluster)

# ---> Quantos elementos foram atribuídos ao cluster com o menor número de observações? 21

# ---> Qual o valor da soma de quadrados total intra-cluster (dica: veja ?kmeans)? 129,1688
km2$tot.withinss


#-------------------------------- Q1
# Utilize o código abaixo para estimar o modelo k-médias para k=2

set.seed(5)
km3 <- kmeans(x, 3, nstart = 20)

# ---> O código acima atribui cada observação à um único cluster. Quantos elementos foram atribuídos ao 
# cluster com o maior número de observações? 19
table(km3$cluster)

# ---> Quantos elementos foram atribuídos ao cluster com o menor número de observações? 14

# ---> Quantos elementos foram atribuídos ao outro cluster? 17

# ---> Qual o valor da soma de quadrados total intra-cluster? 97,748
km3$tot.withinss




