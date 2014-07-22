###################################
#### Manoel Galdino 19/07.2014 ####
#### Aula 3 - Webscraping      ####
#### Continuação               ####
###################################

# definindo diretorio
setwd('D:\\2014\\aulas\\IESP\\scripts\\Curso-IESP-R-aula')
library(XML)

## Agora que nós sabemos fazer raspagem, vamos sair por ai coletando milhões de dados?

## Antes, algumas boas práticas
## Sigo de perto, aqui, o endereço abaixo
## http://lethain.com/an-introduction-to-compassionate-screenscraping/


# This brings us to my single rule for socially responsible screen scraping:
# screen scraper traffic should be indistinguishable from human traffic. 
# By following this rule we avoid placing undue stress on the websites we are scraping, 
# and as an added bonus we'll avoid any attempts to throttle or prevent screen scraping. 
# We'll use a couple of guidelines to help our scripts feign humanity:

# We'll use a couple of guidelines to help our scripts feign humanity:


# Cache feverently. Especially when you're scraping a relatively small amount of data 
# (and your computer has a lot of memory to store in), you should save and reuse results
# as much as possible.

# Stagger http requests. Although it'll speed things up for you to make all the http requests
# in parallel, it won't be appreciated. However, even if you make the requests sequentially 
# it can end up being a fairly large hit. Instead, space your requests out by a small period 
# of time. Several seconds between each request assures that you won't be causing problems.

# Take only what you need. When screen scraping it's tempting to just grab everything,
# downloading more and more pages just because they are there. But its a waste of their
# bandwidth and your time, so make a list of the data you want to extract and stick to it.

## E se você quer usar alguma ferramenta já pronta, acesse
## http://www.notprovided.eu/six-tools-web-scraping-use-data-journalism-creating-insightful-content/
## http://scrapy.org/

## Eu não vou cobrir aqui essas ferramentas, pois não usam o R.

# Função importante para o acesso à internet: 
# http://www.r-bloggers.com/friday-function-setinternet2/
# http://stackoverflow.com/questions/12647207/reading-url-in-r-and-rstudio?rq=1
setInternet2(TRUE)
options(timeout=500)

### Coletando os discursos (em PDF) do Lula
### Adapatado de: http://people.tamu.edu/~nsthiago/RCodeDownloadSpeeches.R
### Os discursos estão em: http://www.biblioteca.presidencia.gov.br/ex-presidentes/luiz-inacio-lula-da-silva/discursos

url <- "http://www.biblioteca.presidencia.gov.br/ex-presidentes/luiz-inacio-lula-da-silva/discursos"

## Vamos acessaro  site e dar uma olhada?
browseURL(url)

# Temos um link para cada mandato
# Abrindo os dois links, vemos um link para cada ano de cada mandato
# clique em 2008, p. ex.
# Depois tem uma divisão entre primeiro e segundo semestre
# e por fim um link para um arquivo em pdf com cada discurso.

## vamos automatizar essa coleta dos discursos?

## no site do Thiago, tem um jeito mais manual
## Deem uma olhada e comparem com o que iremos fazer aqui
## será mais automático



## E agora vamos usar xpathSapply
# vejam o help
?xpathSApply

#é uma func. que permite achar nós do XML (o parse transforma numa árvore)
# que satisfaça a algum ctirério
# numa árvore, nós é o ponto que separa ramificações ou galhos
# é preciso usar a sintaxe XPath para especificar o critério

## Ele converte o resultado para vetor ou matriz, se este puder ser simplificado
## é como sapply em relação a lapply

# xpath ajuda a gente a navegar XML

## Entendendo um pouco de XML

## Exemplo smples de um documento XML
# 
#   <?xml version="1.0" encoding="UTF-8"?>
#   
#   <bookstore>
#     <book>
#       <title lang="en">Harry Potter</title>
#       <author>J K. Rowling</author>
#       <year>2005</year>
#       <price>29.99</price>
#     </book>
#   </bookstore>
#   
#   Exemplos de nós no documento XML acima:
#   <bookstore> (é o elemento raiz)
# 
# <author>J K. Rowling</author> (nó elemento)
# 
# lang="en" (nó atributo) 

## Entendo expressões de path
## é o que usaremos...

## Vamos usar outro exemplo
# 
# <?xml version="1.0" encoding="UTF-8"?>
#   
#   <bookstore>
#   
#   <book>
#     <title lang="en">Harry Potter</title>
#     <price>29.99</price>
#   </book>
#   
#   <book>
#     <title lang="en">Learning XML</title>
#     <price>39.95</price>
#   </book>
#   
#   </bookstore> 


## Sintaxe de XPath
# Expression    Description
# nodename 	    Seleciona todos os nós com nome "nodename"
# / 	          Seleciona o nós raiz
# // 	          Seleciona nós do documento que tenha match, não importa onde esteja o nó
# . 	          Seleciona o nó atual
# @ 	          Seleciona atributos
# 
# Path Expression   Result
# bookstore 	      Seleciona todos os nós com o nome "bookstore"
# /bookstore 	      Seleciona o elemento raiz bookstore
#   bookstore/book 	Seleciona todos os elementos book que são filhos de bookstore
# //book 	          Seleciona todos os elementos book não importa onde estão no documento
# //@lang 	        Seleciona todos os atributos que são nomeados lang

# Se a gente olhar o conteúdo de doc
# veremos que ele tem muitas coisas, e os hyperlinks que queremos (com "mandato")
# Isso pode ser visto também no código fonte ex. na linha 488
# Todo hyperlink é no formato <a href=
# o "<a"é uma tag de html e o href diz pro html que, 
# naquela tag, haverá um hyperlink
# Depois de parsearmos o HTML em XML
# Teremos uma árvore com nós
# a tag "<a"vira um nós no nosso XML parseado
# E, portanto, podemos usar o Xpath
# ideia é procurar, nos nós "<a", o atributo href
# ou seja //a/@href"
## Entendendo essa sintaxe
## //a seleciona esse nó, não importa onde esteja no documento
## e o /@href vai procurar os href que forem filhos de "<a", ou seja
## só vai procurar href que seja precedido da tag <a
## isso evita que, se por um acaso, alguém escreveu
## um nome href, mas sem ser html, ele traga isso.

## Tutorial simples
## http://www.w3schools.com/xpath/

url <- "http://www.biblioteca.presidencia.gov.br/ex-presidentes/luiz-inacio-lula-da-silva/discursos"

doc <- htmlParse(url)

links1 <- xpathSApply(doc, "//a/@href")
## doc tem que ter sido parseado. p ex. por htmlParse
length(links1)
View(links1)
# vieram vários linkls
# quero apenas com mandato
# podemos usar grep
linksMandato <- links1[grep("mandato", links1)]
View(linksMandato)
## tivemos algumas repetições
## vamos eliminar duplicados
linksMandato <- unique(linksMandato)
linksMandato

## Se eu manjar mais de xpath, posso usar expressões mais avançadas, ao invés de grep

linksMandato2 <- xpathSApply(doc, "//a[contains(@href, 'mandato')]")
linksMandato2

## veio num formato mais feio. Fica como exercício para vocês limpar esse resultado
## o ponto é que aprendendo xpath, podemos fazer buscas mais específicas...

free(doc) # libera da memória o objeto

# Com isso conseguimos apenas o primeiro link. Dentro desse link há mais links.

linksMandatoVec <- linksMandato
linksVec <- list()
linksAno <- list()

linksVec[[1]] <- htmlParse(linksMandatoVec[1])
linksAno[[1]] <- xpathSApply(linksVec[[1]], "//a/@href")

linksVec[[2]] <- htmlParse(linksMandatoVec[2])
linksAno[[2]] <- xpathSApply(linksVec[[2]], "//a/@href")

listaLinksMandato1 <- vector("list",8) ## cria uma lista, pré-alocando tamanho 8
class(listaLinksMandato1)
  
listaLinksMandato1[[1]] <-  unique(linksAno[[1]][grep(2003, linksAno[[1]])])
listaLinksMandato1[[2]] <-  unique(linksAno[[1]][grep(2004, linksAno[[1]])])
listaLinksMandato1[[3]] <-  unique(linksAno[[1]][grep(2005, linksAno[[1]])])
listaLinksMandato1[[4]] <-  unique(linksAno[[1]][grep(2006, linksAno[[1]])])
listaLinksMandato1[[5]] <-  unique(linksAno[[2]][grep(2007, linksAno[[2]])])
listaLinksMandato1[[6]] <-  unique(linksAno[[2]][grep(2008, linksAno[[2]])])
listaLinksMandato1[[7]] <-  unique(linksAno[[2]][grep(2009, linksAno[[2]])])
listaLinksMandato1[[8]] <-  unique(linksAno[[2]][grep(2010, linksAno[[2]])])

## E assim por diante
## Mas já estou com preguiça
## podemos automatizar?

## Vamos fazer uma primeira função
url <- "http://www.biblioteca.presidencia.gov.br/ex-presidentes/luiz-inacio-lula-da-silva/discursos"

pegaLinks1 <- function ( url.inicial, padrao.inicial, arg.xpath="//a/@href") {
  #browser()
  doc <- htmlParse( url.inicial)   # parseia url
  linksAux <- xpathSApply(doc, arg.xpath)   # coleta os links
  linksMandato <- unique(linksAux[grep(padrao.inicial, linksAux)]) # me traz apenas os links certos
  free(doc)
  return(linksMandato)
}

crawler <- function (url.inicial, padrao.inicial, arg.xpath="//a/@href",
                     primeiraExtracao=T) {
  #browser()
  linksMandatoVec <- pegaLinks1(url.inicial, padrao.inicial, arg.xpath) # inicia o crawler
  if( primeiraExtracao) linksMandatoVec <- linksMandatoVec[3:4] ## joga fora o q não presta da primeira ver
  padrao.inicial <- "mandato" ## altera para buscar apenas com palavra mandato
  ## restringindo o escopo de busca
  i <- 1 # inicia contador do while
  while ( i  <= length(linksMandatoVec)) {
    aux <- pegaLinks1(linksMandatoVec[i], padrao.inicial) # guarda resultado em auxiliar
    linksMandatoVec <- append(linksMandatoVec, aux[!(aux %in% linksMandatoVec)]) # acrescenta só o que for novo
    i <- i + 1 # incrementa contador
    print(i) # só pra ver evolução
    print(length(linksMandatoVec)) # evolução do laço
    Sys.sleep(.5) # pra não afetar o servidor
    if (i > 50) break ## pra não rodar pra sempre
  }
  return(linksMandatoVec)
}

primeiraExtracao <-  crawler(url, "discursos" )



# visualizando resultados
View(primeiraExtracao)

# apenas que tem pdf
View(primeiraExtracao[grep("pdf",primeiraExtracao)])

#apenas que tem view
# view é o link antes de download
View(primeiraExtracao[grep("view",primeiraExtracao)])

# os que já tem download
View(primeiraExtracao[grep("download",primeiraExtracao)])

# # loop para fazer segunda extracao de tudo
# nao rodar em hipótese alguma!!!!
# vai travar o site
# for ( i in 1:length(index)
# segundaExtracao <-  crawler(primeiraExtracao[index[i]], "download", primeiraExtracao=F )


# alternativa
grep("view",primeiraExtracao)[1]
primeiraExtracao[grep("view",primeiraExtracao)[1]] # n é o que queremos
# queremos dos mandatos, nao posse

grep("view",primeiraExtracao)[3]
primeiraExtracao[grep("view",primeiraExtracao)[3]]

# Poderia rodar o crawler até ele pegar tudo
# tornar mais eficiente para não buscar links que já foram procurados
# então, olharia nesse banco links já procurados e pularia eles
# mesmo assim, inferno...

## Vamos adaptar Crawler pra algo mais manual
## vai ser mais eficiente


crawler3 <- function (url.inicial, padrao.inicial, arg.xpath="//a/@href",
                     primeiraExtracao=T) {
  browser()
  linksMandatoVec <- pegaLinks1(url.inicial, padrao.inicial, arg.xpath) # inicia o crawler
  if( primeiraExtracao) linksMandatoVec <- linksMandatoVec[3:4] ## joga fora o q não presta da primeira ver
  padrao.inicial <- "mandato" ## altera para buscar apenas com palavra mandato
  aux <- pegaLinks1(linksMandatoVec[1], padrao.inicial) # guarda resultado em auxiliar
  linksMandatoVec <- append(linksMandatoVec, aux[!(aux %in% linksMandatoVec)])
  aux <- pegaLinks1(linksMandatoVec[2], padrao.inicial) # guarda resultado em auxiliar
  linksMandatoVec <- append(linksMandatoVec, aux[!(aux %in% linksMandatoVec)])
  vec <- as.character(2003:2010)
  for ( i in 1:length(vec)) {
    padrao.inicial <- vec[i]
    aux <- pegaLinks1(linksMandatoVec[i], padrao.inicial) # guarda resultado em auxiliar
    linksMandatoVec <- append(linksMandatoVec, aux[!(aux %in% linksMandatoVec)])
    Sys.sleep(.5)
  }

# faz uma limpeza
linksMandatoVec <- linksMandatoVec[grep( "[20[:alnum:]+]", linksMandatoVec )]

padrao.inicial <- "semestre"
for ( i in 1:length(linksMandatoVec)) {
  aux <- pegaLinks1(linksMandatoVec[i], padrao.inicial) # guarda resultado em auxiliar
  linksMandatoVec <- append(linksMandatoVec, aux[!(aux %in% linksMandatoVec)])
  print("semestre")
  print(i)
  Sys.sleep(.5)
  if ( i > 20) break
}
  
  padrao.inicial <- "view"
for ( i in 1:length(linksMandatoVec)) {
  aux <- pegaLinks1(linksMandatoVec[i], padrao.inicial) # guarda resultado em auxiliar
  linksMandatoVec <- append(linksMandatoVec, aux[!(aux %in% linksMandatoVec)])
  print("view")
  print(i)
  Sys.sleep(.5)
  if ( i > 20) break
}

# faz uma limpeza
linksMandatoVec <- linksMandatoVec[grep( "view", linksMandatoVec )]

  padrao.inicial <- "file"
for ( i in 1:length(linksMandatoVec)) {
  aux <- pegaLinks1(linksMandatoVec[i], padrao.inicial) # guarda resultado em auxiliar
  linksMandatoVec <- append(linksMandatoVec, aux[!(aux %in% linksMandatoVec)])
  print("file")
  print(i)
  Sys.sleep(.5)
  if ( i > 20) break
}
  return(linksMandatoVec)
}

extracaoFinal <- crawler3(url, "discursos" )

extracaoFinal1 <- extracaoFinal[grep("file", extracaoFinal)]
extracaoFinal1[1]

## Agora fazendo o download

folder<-paste("D:\\2014\\aulas\\IESP\\scripts\\textMining\\discursos\\Lula", #remember to change the directory according to where your files are
        1:length(extracaoFinal1), ".pdf",sep="")

for(x in 1:length(extracaoFinal1))
{
  download.file(extracaoFinal1[x],folder[x],mode="wb")
}

#"http://www.biblioteca.presidencia.gov.br/ex-presidentes/luiz-inacio-lula-da-silva/discursos/1o-mandato/2004/2o-semestre/08-07-2004-discurso-do-presidente-da-republica-luiz-inacio-lula-da-silva-reuniao-de-cupula-do-mercosul/download" 
#"http://www.biblioteca.presidencia.gov.br/ex-presidentes/luiz-inacio-lula-da-silva/discursos/1o-mandato/2004/2o-semestre/02-07-2004-discurso-do-presidente-da-republica-luiz-inacio-lula-da-silva-na-cerimonia-de-entrega-das-novas-instalacoes-da-radio-nacional-do-rio-de-janeiro/view"




  
## Essa função funciona pro primeiro passo, quando tenho um link
## MAs no segundo passo, não vai funcionar, pois posso ter mais de um link
## Além disso, posso ter mais de um padrão pra fazer match


## Como preencher formulários no R
http://stackoverflow.com/questions/16601520/what-if-i-want-to-web-scrape-with-r-for-a-page-with-parameters/16860430#16860430




