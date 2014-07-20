###################################
#### Manoel Galdino 19/07.2014 ####
### Spelling Corrector  ###########
### Naive Bayes  ##################
###################################


## pegando wordlist do opensubtitle
http://invokeit.wordpress.com/frequency-word-lists/
  
  
## Baseado em: http://norvig.com/spell-correct.html

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

#####
# Naive Bayes
##

# No problrma de Spelling corrector, queremos sugerir uma palavra dado o que o usuário escreveu.
# Modelando esse problema, nós queremos sugerir uma palavra w_i, dado que a pessoa escreveu w_k
# Como nós nunca temos certeza qual a palavras que o usuário quis dizer 
# (ex.: se digita post, quis dizer posto ou poste?) a abordagem probabilística é adequada
# 
# Basicamente, queremos saber qual a probabilidade de a palavra desejada ser w_i, dado que
# o usuário escreveu w_k. Matematicamente, Prob(w_i|w_k). E queremos computar essa probabilidade
# para cada palavra k que pode ser digitada e cada palavra i que pode ser a desejada.



