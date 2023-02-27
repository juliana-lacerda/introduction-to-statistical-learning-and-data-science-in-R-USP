# - tipos basicos de dados (numerico, logico, caractere, fator)
# - estruturas de dados (vetor, matriz, dataframe)

# calculadora -------------------------------------------------------------

1 + 2 # soma

1:5 # vetor

1:5 + 6:10 # soma de vetores

c(1, 2, 3, 4, 5) + c(6, 7, 8, 9, 10) # equivalente ao comando acima

c(2, 3, 5, 7, 11, 13) - 2 # subtração

-2:2 * -2:2 # multiplicação

1:10 / 3 # divisão

(1:10) ^ 2 # exponenciação


# operadores logicos ------------------------------------------------------

c(3, 4 - 1, 1 + 1 + 1) == c(3, 3, 3) # verifica igualdade

1:3 != 3:1 # verifica diferenca

exp(1:5) < 100 # verifica desigualdade

c("A", "B", "C") == c("A", "b", "d") # comparando caracteres


# variaveis ---------------------------------------------------------------
a <- 1 # armazena o numero 1 na variavel a

x <- 1:5 # armazera o vetor 1:5 na variavel x

y <- 6:10 # armazena o vetor 6:10 na variavel y

x + 2 * y - 3 # faz uma operacao com as variaveis

z <- 1:10 >= 5 #armazena o resultado logico na variavel z

!z # operacao logica de "negacao"

w <- 1:10 <= 3 #armazena o resultado na variavel w

z & w # operacao logica "e"

z | w # operacao logica "ou"


# classes de variaveis ----------------------------------------------------

class(x) # classe de x

class(z) # classe de z

?class # como pedir ajuda no R

palavras <- c("Eu", "estou", "aprendendo", "a",
              "programar", "em", "r") # vetor de caracteres

class(palavras) # classe de "palavras"

# representacao de variaveis categoricas/qualitativas

estudo <- factor(c("Fundamental", "Médio", "Graduação",
                   "Médio", NA, "Fundamental")) # define um fator

estudo

class(estudo) # classe de "estudo"

levels(estudo) # mostra as categorias do fator


# funcoes -----------------------------------------------------------------

f <- function(x) { # declaracao da funcao f
  x^2              # define o que a funcao faz
}

f(2)  # avalia a funcao para x=2
f(10) # avalia a funcao para x=10

g <- function(x, y) { # declaracao da funcao g
  x^y                 # define o que a funcao faz
}

g(2, 3)         # avalia a funcao para x=2 e y=3
g(y = 2, x = 3) # avalia a funcao trocando a ordem dos argumentos

h <- function(x, y = 2) { # declaracao da funcao h
  x^y                     # define o que a funcao faz
}

h(2)    # avalia a funcao para x=2
h(2, 3) # avalia a funcao para x=2 e y=3

# vetores -----------------------------------------------------------------

x <- (1:5) ^ 2 # definicao dos valores do vetor x

length(x) # retorna o numero de elementos do vetor

x[c(1, 3, 5)] # seleciona um subconjunto

x[c(-2, -4)] # equivalente ao comando acima

x[c(TRUE, FALSE, TRUE, FALSE, TRUE)] # equivalente ao comando acima

x[6] # atencao!!!


# matrizes ----------------------------------------------------------------
?matrix

uma_matriz <- matrix(
  1:12,
  nrow = 4, #ncol = 3 gera o mesmo resultado. Verifique!
)

uma_matriz # mostra a matriz

class(uma_matriz) # classe de "uma_matriz"

dim(uma_matriz) # dimensao de "uma_matriz"

nrow(uma_matriz) # retorna o número de linhas da matriz

ncol(uma_matriz) # retorna o número de colunas da matriz

uma_matriz[1, c(2,3)] # elementos na primeira linha, segunda e terceira colunas

uma_matriz[1, ] # todos elementos da primeira linha

mean(uma_matriz[1, ])

uma_matriz[, c(2,3)] # todos elementos da segunda e terceira colunas

uma_matriz[, -1] # todos elementos da segunda e terceira colunas


# dataframe ---------------------------------------------------------------

um_data_frame <- data.frame(
  x = letters[1:10],       # coluna de caracteres
  y = rnorm(10),           # coluna de numeros
  z = runif(10) > 0.5,     # coluna de logicos
  w = rpois(10, 3),        # coluna de numeros
  n = 1:10                 # coluna de numeros
)

um_data_frame # imprime o conteudo do dataframe

class(um_data_frame) # mostra a classe do dataframe

um_data_frame[2:3, -3] # seleciona um subconjundo do dataframe

um_data_frame[c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
              c("x", "y", "w", "n")] # equivalente ao comando acima

um_data_frame$x # seleciona a coluna x

um_data_frame$x[2:3] # seleciona os elementos 2 e 3 da coluna x

novo_data_frame <- data.frame( # mesmas colunas que o dataframe anterior, ordem diferente
  z = rlnorm(10), # números distribuídos seguindo a distribuição lognormal
  y = sample(10), # número 1 a 10 distribuidos em uma ordem aleatória
  x = letters[3:12],
  w = 1:10,
  n = sample(1:10)
)

rbind(um_data_frame, novo_data_frame) # empilha os dataframe

cbind(um_data_frame, novo_data_frame) # cola os dataframes lado a lado


# visualizacao de dados ---------------------------------------------------

library(ggplot2)

?mtcars

ggplot(mtcars, aes(x = mpg)) + # aes define a estetica do grafico (variavel mpg no eixo x)
  geom_histogram(binwidth = 5) + # geometria do grafico: histograma
  labs(title = "Histograma de mpg", # titulo do grafico
       x = "Milhas por galão (mpg)", # titulo do eixo x
       y = "Número de carros") # titulo do eixo y

ggplot(mtcars, aes(x = hp, y = mpg)) + # aes define a estetica do grafico (variavel hp no eixo x e mpg no y)
  geom_point() + # geometria do grafico: pontos
  labs(title = "Grafico de dispersão", x = "Potência", y="Milhas por galão (mpg)") # labels

# Extra: ver documento com controle de fluxo e repeticao e criacao de funcoes personalizadas
