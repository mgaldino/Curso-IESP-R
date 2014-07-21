###################################
#### Manoel Galdino 19/07.2014 ####
### Spelling Corrector  ###########
### Naive Bayes  ##################
###################################

#######################################################
## Baseado em: http://norvig.com/spell-correct.html  ##
#######################################################

install.packages("arm")

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

opensub <- read.delim2("pt_br.txt", sep=" ", header=F, encoding = "UTF-8") 
head(opensub)
names(opensub) <- c("words", "freq")
#opensub$V1 <- iconv(enc2utf8(opensub$V1), sub = "byte")


wget -w 2 -m -H http://www.gutenberg.org/robot/harvest?filetypes[]=txt&langs[]=pt
link <- url('http://www.gutenberg.org/robot/harvest?filetypes[]=html&langs[]=pt')

## esperar 2 segundos


url <- "http://www.gutenberg.org/robot/harvest?filetypes[]=html&langs[]=pt"
doc <- htmlParse(url)
links <- xpathSApply(doc, "//a/@href")
free(doc)

aux <- grep("offset", links)
links[aux]
# new url etc.

## generalizando pra uma função
## vamos usar append
?append

getMyLinks <- function ( primeiraPagina, StepOut = 3 ) {
  url <- character()
  url[1] <- primeiraPagina
  condition <- T
  i <- 1
  links <- list()
  while ( condition ) {
    doc <- htmlParse(url[length(url)])
    links[i] <- xpathSApply(doc, "//a/@href")
    free(doc) 
    aux <- grep("offset", links[i])
    url <- append (url, aux)
    i <- i + 1
    if (!grepl("offset", links) ) {
      condition <- F
    }
    if (i > StepOut) {
      condition <- F
    }
    Sys.sleep(2)
  }
  return(links)
}

url <- "http://www.gutenberg.org/robot/harvest?filetypes[]=html&langs[]=pt"
linksTeste <- getMyLinks(url )

  
  



class(links)
length(links)
links[1]
gsub("href", "", links[1])
nchar("http://www.gutenberg.lib.md.us/1/3/0/9/13092/13092-h.zip")
nchar(links[1])

vec1 <- "a"
microbenchmark(
  vec1 <- append(vec1, "b"),
  vec1[2] <- "b"
) 


Sys.sleep(2)

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

?adist
opensub

distancia <- adist(opensub$words[1], opensub$words)


### speeding up R code
# http://lookingatdata.blogspot.se/2011/04/speeding-up-r-computations.html
# http://www.r-statistics.com/2012/04/speed-up-your-r-code-using-a-just-in-time-jit-compiler/

## Compilando

library(compiler)
library(microbenchmark)
## Compiler fornece um compilador para suas funções, ou seja, suas funções vão ser compiladas
##

myFunction <-function() { for(i in 1:10000) { 1*(1+1) } }

myCompiledFunction <- cmpfun(myFunction) # Compiled function

microbenchmark(
  myFunction(),
  myCompiledFunction()
)

## Pra não precisar compilar cada função em particular
## podemos usar enableJIT(3) no começo do nosso codigo
## isso irá compilar todas as funções criadas a partir daquele momento, naquela sessão

require(compiler)
enableJIT(3)

#Pra desabilitar a compilação, basta rodar enableJIT(0)
# as funções já compiladas continuarão compiladas
# mas as novas funções não serão compiladas.
# não é possivel descompilar, exceto criando de novo a função.

