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
# / 	          Seleciona do nós raix
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

doc <- htmlParse(url)

links1 <- xpathSApply(doc, "//a/@href")
## doc tem que ter sido parseado. p ex. por htmlParse
length(links1)
# vieram vários linkls
# quero apenas com mandato
# podemos usar grep
linksMandato <- links1[grep("mandato", links1)]

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
mandato1 <- htmlParse(linksMandato[1])
linksAno1 <- xpathSApply(mandato1, "//a/@href")

mandato2 <- htmlParse(linksMandato[2])
linksAno2 <- xpathSApply(mandato2, "//a/@href")

listaLinksMandato1 <- vector("list",4) ## cria uma lista, pr'e-alocando tamanho 4
class(listaLinksMandato1)
  
listaLinksMandato1[[1]] <-  unique(linksAno1[grep(2003, linksAno1)])
listaLinksMandato1[[2]] <-  unique(linksAno1[grep(2004, linksAno1)])

## E assim por diante
## Mas já estou com preguiça
## podemos automatizar?

## Vamos fazer uma função

# Eu vou quebrar meu problema em duas partes
## 1. Primeiro preciso de uma função que pegue apenas links específicos
## pra isos uso xpath e depois grep

##2. PReciso usar os links pegos no começo, pra pegar novos links
## Em suma, preciso fazer um web-crowler

## Vamos fazer a primeira função

pegaLinks1 <- function ( url, padrao) {
  ## carrega a biblioteca
  if( require(XML)==F) {
  install.packages("XML")
  library(XML)}
  
  doc <- htmlParse(url)   # parseia url
  linksAux <- xpathSApply(doc, "//a/@href")   # coleta os links
  linksMandato <- unqiue(linksAux[grep(padrao, linksAux)]) # me traz apenas os links certos
}
    
## Essa função funciona pro primeiro passo, quando tenho um link
## MAs no segundo passo, não vai funcionar, pois posso ter mais de um link
## Além disso, posso ter mais de um padrão pra fazer match

## Vamos modificá-la
## 


pegaLinks2 <- function ( url, padrao) {
  browser()
  ## carrega a biblioteca
  if( require(XML)==F) {
    install.packages("XML")
    library(XML)}
  
  doc <- vector("list", length(url)) ## estou pré-alocando lista
  linksAux <- vector("list", length(url))
  linksMandato <- vector("list", length(url))
  
  if (length(url) < 2 ) {
    for ( i in 1:length(url)) {
      doc[[i]] <- htmlParse(url[1])
      linksAux[[i]] <- xpathSApply(doc[[i]], "//a/@href")   # coleta os links
      
      if (length(padrao[[i]]) < 2 ) {
        linksMandato[[i]] <- unique(linksAux[grep(padrao, linksAux)]) # me traz apenas os links certos
      } else {
        for ( j in 1:length(padrao)) {
          linksMandato[[i]] <- unique(linksAux[grep(padrao[j], linksAux)]) # me traz apenas os links certos
        }
      }
    }
  } else {
    for ( i in 1:length(url)) {
      doc[[i]] <- htmlParse(url[i])
      linksAux[[i]] <- xpathSApply(doc[[i]], "//a/@href")   # coleta os links
      if (length(padrao) < 2 ) {
        linksMandato[[i]] <- unique(linksAux[grep(padrao, linksAux)]) # me traz apenas os links certos
        
      } else {
        for ( j in 1:length(padrao)) {
          linksMandato[[i]] <- unique(linksAux[grep(padrao[j], linksAux)]) # me traz apenas os links certos
        }
      }   
      
    }     
    
    }
  
  return(linksMandato)
}

## E Agora, nosso crowler
mycrawler <- function (urlInicial, listaPadrao ) {
  browser()
  listaLinks <- list()
  
  listaLinks[[1]] <- urlInicial
  
  for ( i in 1:length(listaPadrao)) {
    listaLinks <- append(listaLinks, pegaLinks2(listaLinks, listaPadrao[[i]]))  
  }
  return(listaLinks)
}

urlDiscLula  <- "http://www.biblioteca.presidencia.gov.br/ex-presidentes/luiz-inacio-lula-da-silva/discursos"
listaTermos <- list("mandato", as.character(2003:2010), "semestre", "pdf")
links <- mycrawler(urlDiscLula, listaTermos )

pegaLinks2

#And now we will concatenate our links for the first term to a single vector
term01<-c(term01a,term01b,term01c) 
term01[4]<-"http://www.biblioteca.presidencia.gov.br/ex-presidentes/luiz-inacio-lula-da-silva/discursos/1o-mandato/2005/1o-semestre-1"

#We will do the same thing for the second term.
#We will divide our code to get the speeches from 
#2007-2010 (first semester) and 2007-2010 (second semester)
term02a<-paste("http://www.biblioteca.presidencia.gov.br/ex-presidentes/luiz-inacio-lula-da-silva/discursos/2o-mandato/",c(2007:2010),
               "/1o-semestre",sep="")
term02b<-paste("http://www.biblioteca.presidencia.gov.br/ex-presidentes/luiz-inacio-lula-da-silva/discursos/2o-mandato/",c(2007:2010),
               "/2o-semestre",sep="")

#Now we will concatenate our links for the second term to a single vector
term02<-c(term02a,term02b) ##links of lula's speeches term2

#At last, we will concatenate all our links (for the two terms) to a single vector
speeches<-c(term01,term02)



## Pra evitar sobrecarregar os servidores, vamos usar
# Sys.sleep



?Sys.sleep

http://www2.planalto.gov.br/acompanhe-o-planalto/discursos#b_start=0


BuscaInfoPronunciamentos <- function(vetorNomes, vetorUF, dataInicio, dataFinal, k){  
  # Fun??o que busca informa??es dos pronunciamentos dos parlamentares 
  # utilizando um vetor de nomes, um vetor de ufs, as datas de inicio e 
  # final do periodo desejado e o numero de pronunciamentos obtidos por vez (maximo de 1000).
  # retorna uma lista com tres elementos: contabiliza o numero de pronunciamentos,
  # contabiliza o numero de parlamentares e, por fim, retorna os dados dos pronunciamentos
  if(require(seqinr) == F) install.packages("seqinr")
  dados <- list()
  vetorNomes <- trimSpace(vetorNomes)  # removendo espa?os do inicio e do final
  vetorNomes <- gsub(" ", '%20', vetorNomes)  
  dataInicio <- gsub("/", "%2F", dataInicio)
  dataFinal <- gsub("/", "%2F", dataFinal)
  nParlamentares <- 0
  totalPronunciamentos <- 0
  for  (i in 1:length(vetorNomes)){
    currentPage <- 1  # define pagina inicial da busca
    # Link que ser? procurado
    link <- url(paste("http://www.camara.gov.br/internet/sitaqweb/resultadoPesquisaDiscursos.asp?txIndexacao=&CurrentPage=",
                      currentPage,
                      "&BasePesq=plenario&txOrador=", 
                      vetorNomes[i],
                      "&txPartido=",
                      "&dtInicio=",
                      dataInicio,
                      "&dtFim=",
                      dataFinal, 
                      "&txUF=",
                      vetorUF[i], 
                      #listaFaseSessao=1 - pequeno expediente
                      # Legislatura=52
                      "&txSessao=&listaTipoSessao=&listaTipoInterv=&inFalaPres=&listaTipoFala=&listaFaseSessao=&txAparteante=&listaEtapa=&CampoOrdenacao=dtSessao", 
                      "&TipoOrdenacao=DESC&PageSize=", 
                      k,
                      "&txTexto=&txSumario=", 
                      sep=''))  
    # lendo c?digo da p?gina
    #    setInternet2(use = TRUE)
    dados[[i]] <- readLines(link, encoding = "UTF-8")  
    close(link)  # fechando conex?o
    # verificando de h? ou n?o um discurso na p?gina
    semPronunciamento <- grepl('Nenhum discurso encontrado.', dados[[i]])
    # caso n?o haja discurso, seguir para o pr?ximo parlamentar
    if(sum(semPronunciamento) > 0){
      dados[[i]] <- NA
      next  # proximo i
    } 
    # conta o numero de parlamentares com discurso
    nParlamentares <- nParlamentares + 1 
    
    # regex para obter total de discursos do parlamentar                    
    padraoNum <- '<span class=\"visualStrong\">[[:digit:]]+\\.*[[:digit:]]*</span>'  
    listaNum <- grep(padraoNum, dados[[i]], value=T)  # refinando dados  
    
    num <- 0 # vari?vel que receber? o total de discursos do parlamentar
    num <- substr(listaNum, regexpr("([[:alpha:]]+|[[:blank:]]+|[[:punct:]]+)>", listaNum)+2, regexpr("</span>", listaNum)-1)  # nomes dos deputados
    num <- gsub("\\.", '', num)
    num <- as.numeric(num)
    totalPronunciamentos <- totalPronunciamentos + num  # somando o total de discursos obtidos
    # caso exista mais de uma p?gina por nome de deputado: num > 1000
    pageSize <- k  # numero max de registros ja obtidos
    
    while (num > 0 & num  > pageSize){
      print(paste("Esse deputado", i, "tem mais de", k, "discursos. Total de", num, sep=' '))
      currentPage <- currentPage + 1  # incrementando pag
      # atualizando link com nova current page                  
      link <- url(paste("http://www.camara.gov.br/internet/sitaqweb/resultadoPesquisaDiscursos.asp?txIndexacao=&CurrentPage=",
                        currentPage,
                        "&BasePesq=plenario&txOrador=", 
                        vetorNomes[i],
                        "&txPartido=",
                        "&dtInicio=",
                        dataInicio,
                        "&dtFim=",
                        dataFinal, 
                        "&txUF=",
                        vetorUF[i], 
                        #listaFaseSessao=1 - pequeno expediente
                        # Legislatura=52
                        "&txSessao=&listaTipoSessao=&listaTipoInterv=&inFalaPres=&listaTipoFala=&listaFaseSessao=&txAparteante=&listaEtapa=&CampoOrdenacao=dtSessao",
                        "&TipoOrdenacao=DESC&PageSize=",
                        k,
                        "&txTexto=&txSumario=", 
                        sep=''))  
      # concatenando vetor com os novos dados do parlamentar i
      #     setInternet2(use = TRUE)
      dados[[i]] <- c(dados[[i]], readLines(link, encoding = "UTF-8"))  # adicionando novos dados
      close(link) # fechando conex?o
      pageSize <- pageSize + k  # incrementando numero max de registros obtidos
    }
    
    print(paste("Voc? j? pesquisou", i , "deputado(s) de", length(vetorNomes), sep=' '))
    #Sys.sleep(5)  # faz o R esperar n segundos para continuar
    gc()
  }
  listaDados <- list("dados" = dados, "nDiscursos" = totalPronunciamentos, "nParlamentares" = nParlamentares)
  return(listaDados)
}



http://stackoverflow.com/questions/16601520/what-if-i-want-to-web-scrape-with-r-for-a-page-with-parameters/16860430#16860430




