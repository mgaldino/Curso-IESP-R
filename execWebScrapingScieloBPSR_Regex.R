########################################################
#### Manoel Galdino 19/07.2014                      ####
#### Aula 3 - Webscraping                           ####
#### Continuação                                    ####
## Tentativa de usar readLines pra pegar abstracts  ####
## Não funcionou                                    ####
########################################################


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

#3 parei aqui.
## ver respost no SO
## http://stackoverflow.com/questions/24908740/webscraping-potentially-ill-formated-html-in-r-with-xpath-or-regex/24916748#24916748
## Exerc[icio implementar resposta do SO