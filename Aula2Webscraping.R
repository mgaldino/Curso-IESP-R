###################################
#### Manoel Galdino 19/07.2014 ####
#### Aula 2 - Webscraping      ####
###################################


# O que é webscraping? Ou raspagem (tradução em português), crawler, spider 

# Da [Wikipedia](http://en.wikipedia.org/wiki/Web_scraping)
# Web scraping (web harvesting or web data extraction) is a computer software technique of
# extracting information from websites.

## "More Data Trumps Smarter Algorithms"
## Extraindo dados da web, podemos usar modelos mais simples (nem sempre, mas ok)


## Começando extraindo uma tabela simples

## Escolhi uma tabela meio ao acaso
## peguei uma tabela do censo do ibge 2010
## voces podem copiar e colar o endereço abaixo no navegador de vcs

## http://www.censo2010.ibge.gov.br/sinopse/index.php?dados=8
## ou simplesmente rodarem

url <- "http://www.censo2010.ibge.gov.br/sinopse/index.php?dados=8"
browseURL(url)

## temos uma mapa e uma tabela
## vamos extrair a tabela pro R

## Carregamos a biblioteca xml
library(XML)
## install.packages("XML")

# E usamos htmlParse() para ler o html e transformar num objeto do R,
# e readHTMLTable() para ler a tabela no html. The length() function indicates 
#there is a single table in the document, simplifying our work.

url <- "http://www.censo2010.ibge.gov.br/sinopse/index.php?dados=8"

srts <- htmlParse(url)
class(srts)

srts.table <- readHTMLTable(srts, stringsAsFactors = FALSE)

srts.table
View(srts.table)


## that's it!!
# Brincadeira. Parcialmente
class(srts.table)
str(srts.table)
length(srts.table)

df1 <- as.data.frame(srts.table)
df <- as.data.frame(srts.table[[1]])

head(df)
head(df1)
all.equal(df, df1) ## checa e o conteúdo é igual
## nomes das variáveis ficam diferentes
names(df)
names(df1)
## vamos arrumar nosso banco?

## melhorando os nomes das variáveis
names(df)[1] <- "UF"
names(df)

colnames(df)
# podemos usar colnames tbm...
## o que é melhor, colnames ou names?
library(microbenchmark)

## names é melhor! 1,5x mais rápido.
## normalmente eu uso o pacote data.table
## e não preciso usar names ou colnames
## se der tempo falamos dele depois

# voltando ao webscraping

head(df)
## mudamos o nome da coluna errada
## mas o que tem na primeira coluna?

df[,1]

## nada, podemos dropar

df <- df[, -1]

## agora assim, mudando pra uf
names(df)[1] <- "uf"

head(df)

 
## vamos retirar o número após os anos, que originalmente era um subescrito?
## alguma ideia de como fazer isso?

# vou fazer uma função

retiraIndice <- function (df) {
  substr(names(df), 5,5) <- ""
}

## pra não zuar meu banco original, vou faer uma cópia
df1 <- df

aux <- names(df[,-1])
aux
substr(aux, 5,5) <- ""
aux

aux2 <- paste(substr(aux, 1,4), substr(aux, 6,nchar(aux)), sep="")

names(df)[2:13] <- aux2
names(df)

## that's it!!


###
### Exemplo desenvolvido pelo Thiago Silva
### http://people.tamu.edu/~nsthiago/websitetables.r

## Pegando várias tabelas da wiki

wiki2012 <- 'http://en.wikipedia.org/wiki/United_States_presidential_election,_2012'

auxTable <- htmlParse(wiki2012)

tables2 <- readHTMLTable(auxTable, stringsAsFactors = FALSE)

tables2
class(tables2)
length(tables2)

# muitas tabelas
# como achar qual queremos?

## tentativa e erro
## método do Thiago

## que tal algo mais automático?? Vamos buscar por um termo
## grep procura um padrão num vetor/lista e retorna o índice do match

grep("abc", c("abcd", "abc", "bcde"))

grep("abc", c("abcd", "abc", "bcde"), value=T)


grep('Demographic subgroup', tables2)

table16 <- readHTMLTable(auxTable, which=16, stringsAsFactors = FALSE)
## ou diretamente, table16 <- tables2[[16]]

View(table16)

## funcionou
## Vamos fazer uma função

scrapingWiki <- function (url, padrao1, ...) {
  stopifnot(is.character(padrao1)) ## padrao1 tem que ser string/character
  auxTable <- htmlParse(url)
  tabela <- readHTMLTable(auxTable, stringsAsFactors = FALSE)
  aux <- grep(padrao1, tabela)
  tabeltaFinal <- tabela[aux][[1]]
}

## checando se função funciona
## é boa prática remover os objetos ou testar em outra sessão do R tbm

tableTeste <- scrapingWiki(wiki2012, 'Demographic subgroup')

head(tableTeste)
head(table16)


## Mas nós queríamos duas tabelas, não?
#2) 2012 Presidential Votes by states

# só modificar nossa função pra ficar mais genérica

scrapingWiki1 <- function (url, padraoVec, ...) {
  ## a Função retorna as tabelas da url, que contém o texto especificado em padraoVec
  auxTable <- htmlParse(url)
  tabela <- readHTMLTable(auxTable, stringsAsFactors = FALSE)
  ## criando listas que vão receber os padroes e as tabelas
  aux <- list()
  listaTabelas <- list()
  for ( i in 1:length(padraoVec)) {
    aux[i] <- grep(padraoVec[i], tabela)
    listaTabelas[i] <- tabela[aux[[i]]]
  }
return(listaTabelas)
}

## Qual a outra palavra? hummm difícil
## vamso tentar Margin

tabelasTeste <- scrapingWiki1(wiki2012, c("Demographic subgroup", "Margin") )

## podemos transformar as duas tabeas em dataframe
## exercicios para vocês
## chamem de df1 e df2
## lembrem de corrigir os nomes das colunas
## e transformar os tipso das variáveis (numérticas)
##onde for porcentagem, checar se precisa diidir por 100

## na origem, daria pra corrigir o nome das colunas, usando o argumento header=T em readHTMLTable

library(XML)
scrapingWiki2 <- function (url, padraoVec, header=T, ...) {
  
  ## a Função retorna as tabelas da url, que contém o texto especificado em padraoVec
  ## alterando nossa função
  
  auxTable <- htmlParse(url)
  tabela <- readHTMLTable(auxTable, stringsAsFactors = FALSE, header=header)
  
  ## criando listas que vão receber os padroes e as tabelas
  aux <- list()
  listaTabelas <- list()
  
  for ( i in 1:length(padraoVec)) {
    aux[i] <- grep(padraoVec[i], tabela)
    listaTabelas[i] <- tabela[aux[[i]]]
  }
  return(listaTabelas)
}


wiki2012 <- 'http://en.wikipedia.org/wiki/United_States_presidential_election,_2012'

tabelasTeste2 <- scrapingWiki2(wiki2012, c("Demographic subgroup", "Margin") )

length(tabelasTeste2)
View(tabelasTeste2[[1]])
View(tabelasTeste2[[2]])

## processando resultado
class(tabelasTeste2[[1]])
mydf1 <- tabelasTeste2[[1]]
names(tabelasTeste2[[1]]) <- mydf1[1,]

View(tabelasTeste2[[1]])
tabelasTeste2[[1]] <- tabelasTeste2[[1]][-1,]

View(tabelasTeste2[[1]])
tabelasTeste2[[1]][, 2:5] <- as.numeric(tabelasTeste2[[1]][, 2:5])

tabelasTeste2[[1]][,2] <- as.numeric(tabelasTeste2[[1]][,2])
tabelasTeste2[[1]][,3] <- as.numeric(tabelasTeste2[[1]][,3])
tabelasTeste2[[1]][,4] <- as.numeric(tabelasTeste2[[1]][,4])
tabelasTeste2[[1]][,5] <- as.numeric(tabelasTeste2[[1]][,5])

summary(tabelasTeste2[[1]])

## Dívidas
## 2. Entender porque numeric pra 2:5 não funcionou
## corrigiu parcialmente apenas. Ok.







http://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais



http://www.r-bloggers.com/web-scraping-working-with-apis/



