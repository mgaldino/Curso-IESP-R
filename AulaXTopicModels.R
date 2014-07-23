## 
## LDA

http://java.dzone.com/articles/topic-modeling-python-and-r
https://github.com/OhLookCake/xkcd-Topics/commit/11ccb14160746c29c06a5a14b97dadc759e8c292
https://github.com/benmarwick/dayofarchaeology/blob/master/004_generate_topic_model.r
http://www.rtexttools.com/1/post/2011/08/getting-started-with-latent-dirichlet-allocation-using-rtexttools-topicmodels.html


# Cada documento d é gerado do seguinte modo:

# A proporção de tópicos do documento d
# é sorteada de uma distribuição apriori
# ou seja, geramos uma mistrua de tópicos

# Sorteamos um tópico z com as prob. acima
# E palavras são sorteadas de cada tópico
# de acordo com a distribuição de z.
# em Geral, Z é multinomial e a prob de cada
# categoria (palavra do vocabulário) é específica
# para cada tópico z

# exemplo com dois tópicos, t1 e t2
# pra gerar um documento d1

# Sorteio de uma priori theta a prop. de tópicos de d1 e
# gero o número de palavras, n, de d1
# (na verdade, geramos n1 palavras pra t1 e n2 palavras
# pra t2). E assim temos n = n1 + n2 e a proporção é
# n1/n e n2/n

# gero as n1 palavras de t1 de acordo com uma multinomial
# m1
# gero as n2 palavras de t2 de acordo com uma multinomial
# m2
# theta é uma dirichelet(alpha)
# no exemplo com dois topicos, alpha é um vetor de tamanho 2
# e cada elemento de alpha influencia n1 e n2

## bag-of-words assumption

######
## Análise de Dados de Revistas na Scielo
###########################################

# Quais os tópicos da revista BPSR?
library(XML)
pegaLinks1 <- function ( url.inicial, padrao.inicial, arg.xpath="//a/@href") {
  #browser()
  doc <- htmlParse( url.inicial)   # parseia url
  linksAux <- xpathSApply(doc, arg.xpath)   # coleta os links
  linksMandato <- unique(linksAux[grep(padrao.inicial, linksAux)]) # me traz apenas os links certos
  free(doc)
  return(linksMandato)
}

url <- "http://www.scielo.br/scielo.php?script=sci_issues&pid=1981-3821&lng=en&nrm=iso"
bpsr2014 <- pegaLinks1(url, "2014")
bpsr2013 <- pegaLinks1(url, "2013")
bpsr2012 <- pegaLinks1(url, "2012")

listaBpsr <- c(bpsr2014, bpsr2013, bpsr2012)
n <- length(listaBpsr)
abstract <- list()
for ( i in 1:n) abstract[[i]] <- pegaLinks1(listaBpsr[i], "abstract")
length(abstract)
length(abstract[[1]])
abstract[[1]]
# duas opções
# readLines ou usarpegaLinks1

#pegaLinks1
aux1 <- pegaLinks1(abstract[[1]][1], ".",arg.xpath="//@xmlns")

arg.xpath="//p[contains(@xmlns, 'the')]" 

string(/*/p[@name='description']/@content)


doc <- htmlParse( abstract[[1]][1])   # parseia url
linksAux <- xpathSApply(doc, arg.xpath)   # coleta os links
linksMandato <- unique(linksAux[grep(padrao.inicial, linksAux)]) # me traz apenas os links certos
free(doc)
return(linksMandato)


# readLines
teste1 <- readLines(abstract[[1]][1])
# d'a um warning. MAs é so warning, não erro
View(teste1) ## após inspecionar, achei que o abstract ficou na linha 19
## vamos ver se grep "Abstract"funciona
grep("Abstract", teste1)
# dois resultados. Vamos tentar sem os ":"
# com regex
grep("Abstract[^\\:]", teste1)

tmp <- readLines(doc)


teste2 <- teste1[grep("Abstract[^\\:]", teste1)] 

teste3 <- gsub("\\b(.*?)\\bISSN", "" , teste2)

teste4 <- gsub("xmlns=\\\"\\\"><strong>Keywords$", "", teste3)






gsub("\\b(xmlns=.?xmlns=)\\b", "" , teste2)
"

linksFinais <- unlist(abstract)
# 25 links, logo, 25 abstracts
listaAbstract <- list()
for ( i in 1:length(linksFinais)) listaAbstract[[i]] <- readLines(linksFinais[i])

index <- 0
listaAbstractFinal <- list()
for ( i in 1:length(linksFinais)) {
  index[i] <- grep("Abstract[^\\:]", listaAbstract[[i]])
  listaAbstractFinal[i] <- listaAbstract[[i]][index[i]]
}


# 25 warnings, mas sem problema


                                                   
# funcionou
# ver se generaliza



## Dirichlet
#install.packages("MCMCpack")
library(MCMCpack)

teste <- rmultinom(10000, 130 , c(.1, .4, .8))


sim <- rdirichlet(10000, alpha = c(10,40,80))
head(sim)
hist(sim[,1])
hist(sim[,2])
hist(sim[,3])
hist(sim)

nTopicos <- 3 
alpha <- rmultinom(10, 1:nTopicos)
## Topic Modeling
install.packages("topicmodels")
library(topicmodels)

data(AssociatedPress)

train <- AssociatedPress[1:100]
test <- AssociatedPress[101:150]

train.lda <- LDA(train,5)
(train.topics <- topics(train.lda))

test.topics <- posterior(train.lda,test)

(test.topics <- apply(test.topics$topics, 1, which.max))

