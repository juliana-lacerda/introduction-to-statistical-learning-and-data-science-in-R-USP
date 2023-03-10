
setwd("D:/Dropbox/Data Science/Curso de Extens?o/IME - Introdu??o a Aprendizagem Estatistica e a Ciencia de Dados em R/Programas Tarefas")



#------------------------------------------------------------------
#------------------------- DEFINICOES
#------------------------------------------------------------------


set.seed(2)
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:29, 1] <- x[1:29, 1] + 3
x[1:29, 2] <- x[1:29, 2] - 4

#-------------------------------- Q1
# Utilize o c?digo abaixo para estimar o modelo k-m?dias para k=2

set.seed(2)
km2 <- kmeans(x, 2, nstart = 20)

km2


# ---> O c?digo acima atribui cada observa??o ? um ?nico cluster. Quantos elementos foram atribu?dos ao 
# cluster com o maior n?mero de observa??es? 29   
table(km2$cluster)

# ---> Quantos elementos foram atribu?dos ao cluster com o menor n?mero de observa??es? 21

# ---> Qual o valor da soma de quadrados total intra-cluster (dica: veja ?kmeans)? 129,1688
km2$tot.withinss


#-------------------------------- Q1
# Utilize o c?digo abaixo para estimar o modelo k-m?dias para k=2

set.seed(5)
km3 <- kmeans(x, 3, nstart = 20)

# ---> O c?digo acima atribui cada observa??o ? um ?nico cluster. Quantos elementos foram atribu?dos ao 
# cluster com o maior n?mero de observa??es? 19
table(km3$cluster)

# ---> Quantos elementos foram atribu?dos ao cluster com o menor n?mero de observa??es? 14

# ---> Quantos elementos foram atribu?dos ao outro cluster? 17

# ---> Qual o valor da soma de quadrados total intra-cluster? 97,748
km3$tot.withinss




