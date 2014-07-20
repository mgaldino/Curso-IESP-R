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
## vamso extrair a tabela pro R

## Carregamos a biblioteca xml
library(XML)

# E usamos htmlParse() para ler o html e transformar num objeto do R,
# e readHTMLTable() para ler a tabela no html. The length() function indicates 
#there is a single table in the document, simplifying our work.


srts <- htmlParse(url)
class(srts)

srts.table <- readHTMLTable(srts, stringsAsFactors = FALSE)

srts.table

## that's it!!
# Brincadeira. Parcialmente
class(srts.table)
str(srts.table)
length(srts.table)
df <- as.data.frame(srts.table[[1]])

## vamos arrumar nosso banco?

## melhorando os nomes das variáveis
names(df)[1] <- "UF"
names(df)

# podemos usar colnames tbm...
## o que é melhor, colnames ou names?


microbenchmark(
  colnames(df)[1] <- "UF",
  names(df)[1] <- "UF"
)


## names é melhor! 1,5x mais rápido.
## normalmente eu uso o pacote data.table
## e não preciso usar names ou colnames
## se der tempo falamos dele depois

# voltando ao webscraping

head(df)
## mudamos o nome da coluna errada
## mas o que tem na primeira coluna?

df[,1]

## nada, podemso dropar

df <- df[, -1]
## agora assim, mudando pra uf
names(df)[1] <- "uf"

head(df)
## vamos retirar o númeor após os anos, que originalmente era um subescrito?
## alguma ideia de como fazer isso?

# vou fazer uma função

retiraIndice <- function (df) {
  substr(names(df), 5,5) <- ""
}

## pra não zuar meu banco original, vou faer uma cópia
df1 <- df

sub('[:alnum:]{4}', '', aux)

aux <- names(df[,-1])
substr(aux, 5,5) <- ""

paste(substr(myvec, 1,4), substr(myvec, 6,nchar(myvec)), sep="")

teste <- replace(myvec, 5, "")

class(srts.table)
str(srts.table)
length(srts.table)
df <- as.data.frame(srts.table[[1]])

## that's it!!


###
### Exemplo desenvolvido pelo Thiago Silva
### http://people.tamu.edu/~nsthiago/websitetables.r

## Pegando várias tabelas da wiki

wiki2012 <- 'http://en.wikipedia.org/wiki/United_States_presidential_election,_2012'

auxTable <- htmlParse(wiki2012)
tables2 <- readHTMLTable(auxTable, stringsAsFactors = FALSE)

tables2

# muitas tabelas
# como achar qual queremos?

## tentativa e erro
## método do Thiago

## que tal algo mais automático?? Vamos buscar por um termo
## grep procura um padrão num vetor/lista e retorna o índice do match

grep('Demographic subgroup', tables2)

table16 <- readHTMLTable(auxTable, which=16, stringsAsFactors = FALSE)
## ou diretamnte, table16 <- tables2[16]


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



tabelasTeste2 <- scrapingWiki2(wiki2012, c("Demographic subgroup", "Margin") )


## corrigiu parcialmente apenas. Ok.







http://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais



http://www.r-bloggers.com/web-scraping-working-with-apis/



