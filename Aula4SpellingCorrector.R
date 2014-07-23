###################################
#### Manoel Galdino 19/07.2014 ####
### Spelling Corrector  ###########
### Naive Bayes  ##################
###################################

#######################################################
## Baseado em: http://norvig.com/spell-correct.html  ##
#######################################################

install.packages("arm")
library(arm)

################
# Naive Bayes ##
################

# No problema de Spelling corrector, queremos sugerir uma palavra dado o que o usuário escreveu.
# Modelando esse problema, nós queremos sugerir uma palavra w_i, dado que a pessoa escreveu w_k
# Como nós nunca temos certeza qual a palavras que o usuário quis dizer 
# (ex.: se digita post, quis dizer posto ou poste?) a abordagem probabilística é adequada
# 
# Basicamente, queremos saber qual a probabilidade de a palavra desejada ser w_i, dado que
# o usuário escreveu w_k. Matematicamente, Prob(w_i|w_k). E queremos computar essa probabilidade
# para cada palavra k que pode ser digitada e cada palavra i que pode ser a desejada.

# Pelo Teorema de Bayes, sabemos que Prob(w_i|w_k) = Prob(w_i) * Prob(w_k|w_i)/ Prob(w_k)
# Prob(w_i) é chamada de probabilidade a priori de w_i, ou seja, qual a probabilidade
# de, independente do usuário ter digitado algo, essa ser a palavra desejada.
# Podemos interpretá-la como a frequência relativa do uso dessa palavra pelos usuários
# E vamos aproximá-la pela frequencia do uso das palavras em português

# PAra tanto iremos procurar dados de uso de palavras em pt na internet
# Eu escolhi as palavras utilizadas pelo opensubtitles
# e os livros em português disponiveis no projeto Gutemberg
# depois iremos computar a frequência de cada palavra nesses corpus (conjunto de documentos)
# E assim teremos Prob(w_i) para cada palavra do nosso corpus


## pegando wordlist do opensubtitle
#http://invokeit.wordpress.com/frequency-word-lists/



setwd("D:\\2014\\aulas\\IESP\\scripts\\textMining")

open2 <- read.delim2("pt_br.txt", header=T)

getwd()
dir()
?save
#save(tabela, file="tabela.RData")
load("opensubtitles.RData")
head(opensub)
#opensub <- open2
names(opensub) <-c("words", "freq")

# Falta ainda calcularmos Prob(w_k|w_i) e Prob(w_k)
# Com relação à Prob(w_k), iremos usar um truque para evitar a necessidade de computá-lo
# e conhecido na literatura como MAP (Maximo a posteriori)
# Para cada palavra digitada, w_k, nós iremos sugerir a w_i que tiver a maior probabilidade
# Isso significa que se nós computarmos a razão entre Prob(w_1|w_k) e Prob(w_2|w_k), p. ex.:
# Prob(w_1|w_k)/Prob(w_2|w_k) = Prob(w_1) * Prob(w_k|w_1) / Prob(w_1) * Prob(w_k|w_1)
# se a razão for maior que 1, então devemos sugerir w_1, caso contrário, w_2
# e podemos fazer isso para todas as palavras, de modo que não precisamos computar Prob(w_k)

# e para calcular Prob(w_k|w_i) teremos que fazer algumas suposições e modelagem
# essa será nossa verossimilhança

# Existe uma função que calcula a distancia entre duas palavras
# basicamente, quanto mais semelhantes duas palavras, menor a distancia
# e semelhança é medido por inserções, remoções e trocas de posiçao
# necessárias para transformar uma palavra igual a outra
# precisamos colocar essa distancia numa outra função
# iremos usar a invlogit
# ela tem um comportamento adequado


library(arm)
opensub$words <- as.character(opensub$words)
?adist

distancia <- adist( opensub$words,opensub$words[1])
head(distancia)

## Agora vou fazer minha verossimilhança
# vamos usar invlogit
invlogit

# é a função da logística!

likelihood <- invlogit(distancia)
summary(likelihood)

## tá muito concentrado perto do 1
## quero suavizar mais
## vou centrar meus dados
likelihood <- invlogit(distancia - mean(distancia))
summary(likelihood)

df <- data.frame(likelihood=likelihood, opensub)
head(df)
df$distancia <- distancia
df$words <- as.character(df$words)

# vamos ver o gráfico das minha verossimilhança?
## como tenho muitos data.points vou usar uma amostra
## comando sample
library(ggplot2)
set.seed(2)
df1 <- df[sample(1:nrow(df), 2000),]

p <- ggplot(df1, aes(y=likelihood, x = factor(1)))
p + geom_boxplot()


p <- ggplot(df1, aes(x=distancia, y=likelihood))
(p <- p + geom_line())
p + geom_point()


## fiz errado: qto menor distância, maior deveria ser likelihood

likelihood <- invlogit( mean(distancia) - distancia)
df$likelihood <- likelihood
summary(likelihood)

set.seed(2)
df1 <- df[sample(1:nrow(df), 2000),]

p <- ggplot(df1, aes(y=likelihood, x = factor(1)))
p + geom_boxplot()


p <- ggplot(df1, aes(x=distancia, y=likelihood))
(p <- p + geom_line())
p + geom_point()



## os dados tão demorando pra cair, e depois cai rápido demais
## preciso ajustar
unique(likelihood)
#imprimiu em notação científica

options(scipen=20, digits=4) # imprime melhor

unique(likelihood)

## quero que de 0.9993670098510825106 para 0.9874378161925373432 caia mais rápido
## alguma sugestão?

distNormalizada <- (mean(distancia) - distancia)/sd(distancia) # padronizar
likelihood <- invlogit(distNormalizada)
df$likelihood <- likelihood

set.seed(2)
df1 <- df[sample(1:nrow(df), 2000),]

p <- ggplot(df1, aes(y=likelihood, x = factor(1)))
p + geom_boxplot()


p <- ggplot(df1, aes(x=distancia, y=likelihood))
(p <- p + geom_line())
p + geom_point()



# e fazer transformação não-linear

distNormalizada1 <- distNormalizada*40 -113
sort(unique(distNormalizada1))
df$likelihood1 <- invlogit( distNormalizada1)
sort(unique(df$likelihood1))

set.seed(2)
df1 <- data.frame(likelihood1=unique(df$likelihood1),
                  distancia=unique(df$distancia))

p <- ggplot(df1, aes(x=distancia, y=likelihood1))
(p <- p + geom_line())
p + geom_point()


## melhorou bastante

## Pronto, temos nossa verossimilhança
# Agora, só aplicar teorema de Bayes
df$priori <- df$freq/sum(df$freq)

sugestao <- function(palavra) {
  #browser()
  distancia <- adist( opensub$words,palavra)
  distNormalizada <- (mean(distancia) - distancia)/sd(distancia)
  distNormalizada1 <- (distNormalizada*40 -113)
  likelihood <- invlogit( distNormalizada1)
  posicao <- which.max(df$priori*likelihood)
  df$words[posicao]
}

sugestao("porta")
sugestao("port")
sugestao("nao")
sugestao("paralizado")
sugestao("paralisado")

## Priori ruim, tenho que dar mais peso pra likelihood
## se dist for zero, likelihood = 1
## se dist for 1, likelihood = .05
## se dist for 2, likelihood = .00001
## se dist for 3 ou maior, likelihood = 0

## vai demorar muito tempo pra calcular a distancia para cada palavra
sugestao1 <- function(palavra) {
  #browser()
  distancia <- adist( opensub$words,palavra)
  likelihood  <- ifelse( distancia < 1, 1,
                         ifelse(distancia < 2, .05, 
                                ifelse(distancia < 3, .0001, 0)))
  posicao <- which.max(df$priori*likelihood)
  df$words[posicao]
}

sugestao1("porta")
sugestao1("port")
sugestao1("nao")
sugestao1("paralizado")
sugestao1("paralisado")

## melhorou um pouco

## idealmente, deveria ter um training set e ajustar o melhor modelo

### speeding up R code
# http://lookingatdata.blogspot.se/2011/04/speeding-up-r-computations.html
# http://www.r-statistics.com/2012/04/speed-up-your-r-code-using-a-just-in-time-jit-compiler/

## Compilando

library(compiler)
library(microbenchmark)
## Compiler fornece um compilador para suas funções, ou seja, suas funções vão ser compiladas
##

myFunction <- function() { for(i in 1:10000) { 1*(1+1) } }

myCompiledFunction <- cmpfun(myFunction) # Compiled function

microbenchmark(
  myFunction(),
  myCompiledFunction()
)

## Pra não precisar compilar cada função em particular
## podemos usar enableJIT(3) no começo do nosso codigo
## isso irá compilar todas as funções criadas a partir daquele momento, naquela sessão

#require(compiler)
#enableJIT(3)

#Pra desabilitar a compilação, basta rodar enableJIT(0)
# as funções já compiladas continuarão compiladas
# mas as novas funções não serão compiladas.
# não é possivel descompilar, exceto criando de novo a função.

## too good to be true?
## Dúvida: como ficam as variáveis livres? E a questão do escopo
## e with, que supostamente não usa escopo léxico?
## Boas questões... Fiquem atentos
## Tem um custo pra compilar. Se for usar a função poucas vezes e ela for rápida, eja se compensa
## experimente...

