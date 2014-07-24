###################################
#### Manoel Galdino 19/07.2014 ####
#### Aula 6 - LDA/Topic Model  ####
###################################
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

# pacotes
if(require("XML") == F){install.packages("XML")
                        library("XML")}
if(require("MCMCpack") == F){install.packages("MCMCpack"); library("MCMCpack")}
if(require("tm") == F){install.packages('tm'); library("tm")}
if(require("SnowballC") == F){install.packages('SnowballC'); library("SnowballC")}
if(require("data.table") == F){install.packages('data.table'); library("data.table")}
if(require("ggplot2") == F){install.packages('ggplot2'); library("ggplot2")}
if(require("seqinr") == F) {install.packages("seqinr"); library("seqinr")}
if(require("RTextTools") == F) {install.packages("RTextTools")
                                library("RTextTools")}
if(require(topicmodels) == F) {install.packages("topicmodels")
                               library("topicmodels")}


######
## Análise de Dados de Revistas na Scielo
###########################################


# Funções de scraping
pegaLinks1 <- function ( url.inicial, padrao.inicial, arg.xpath="//a/@href") {
  #browser()
  doc <- htmlParse( url.inicial)   # parseia url
  linksAux <- xpathSApply(doc, arg.xpath)   # coleta os links
  linksMandato <- unique(linksAux[grep(padrao.inicial, linksAux)]) # me traz apenas os links certos
  free(doc)
  return(linksMandato)
}

pegaLinks2 <- function ( url.inicial, busca.padrao=F, 
                         padrao.inicial, arg.xpath="//p[@xmlns]") {
  #browser()
  doc <- htmlParse( url.inicial)   # parseia url
  links <- xpathSApply(doc, arg.xpath, xmlValue)   # coleta os links
  free(doc)
  if (busca.padrao) links <- unique(linksAux[grep(padrao.inicial, links)]) # me traz apenas os links certos
  return(links)
}

# Funções de Processamento e limpeza

remove_acento <- function(vec, Toupper=F) {
  vec <- tolower(vec)
  vec <- gsub('á', 'a', vec) 
  vec <- gsub('ã', 'a', vec)
  vec <- gsub('à', 'a', vec)
  vec <- gsub('â', 'a', vec)
  vec <- gsub('é', 'e', vec) 
  vec <- gsub('ê', 'e', vec)
  vec <- gsub('í', 'i', vec)
  vec <- gsub('ó', 'o', vec) 
  vec <- gsub('ô', 'o', vec)
  vec <- gsub('õ', 'o', vec)
  vec <- gsub('ú', 'u', vec)
  vec <- gsub('ç', 'c', vec)
  vec <- gsub('\'', '', vec)
  if ( Toupper==T) vec <- toupper(vec)
  return(vec)
}

RemovePalavrasMuitoUsadasAutor <- function(corpus, autorMatrix, perc, verbose = T){
  # retorna o corpus de entrada retirando as palvras que aparecem em mais de 90%
  # dos documentos de um mesmo autor  
  
  if(verbose == T){
    temp <- strsplit(as.character(corpus), " ")   
    nStems <- length(unique(unlist(temp))) 
  }
  vecW <- list()
  for  (i in 1:length(autorMatrix[,1])){  
    
    m <- corpus[autorMatrix[i,1]:autorMatrix[i,2]]
    corpus[1]
    dtm <- as.matrix(DocumentTermMatrix(m))
    mBin  <- as.matrix((dtm > 0) + 0)
    
    docs <- autorMatrix[i,2]-autorMatrix[i,1]
    nomes <- names(which(colSums(mBin)/(docs+1) > perc))
    
    vecW[i] <- ifelse(length(nomes) > 0, list(nomes), NA)     
    aux <- unlist(vecW[i])
    
    
    if(!is.na(vecW[i])){
      corpus[autorMatrix[i,1]:autorMatrix[i,2]] <- tm_map(m, removeWords, 
                                                          aux) # removendo palavras escolhidas    
    }
    
    #if(i%%20 == 0) 
    print(paste(i, "de", length(autorMatrix[,1]), sep = " "))
    
    if(i== nrow(autorMatrix)) print(paste(i, "de", length(autorMatrix[,1]), sep = " "))  
  }
  
  if(verbose == T){
    #retiradas <- length(unique(unlist(vecW)))
    
    temp1 <- strsplit(as.character(corpus), " ")   
    sobra <- length(unique(unlist(temp1))) 
    
    
    #sum(do.call(rbind, lapply(vecW, function(x) {  ifelse(is.na(x) , 0, length(x))})))
    retiradas <- nStems - sobra    
    print(paste("Tinhamos", nStems, "raizes e retiramos", retiradas, "palavras. Sobraram",  sobra,  sep = " "))
    #  if(retiradas < 500) print(paste("Palavras retiradas:", unique(unlist(vecW))))
  }
  gc()
  return(corpus)  
  
}  

RemovePalavrasPoucoUsadas <- function(corpus, perc.doc){
  # retorna o corpus de entrada retirando as palvras que aparecem em menos de perDoc%
  # do total de documentos
  #
  # obtendo vetor com stems menos utilizados
  # corpus <- pequenoExpediente
  vector <- as.character(corpus)
  #vector[1]
  # lista com  todas as palavras por documento
  temp <- strsplit(vector, " " , fixed = FALSE, perl = FALSE, useBytes = FALSE)   
  nStems <- sum(do.call(rbind, lapply(temp, function(x) length(x))))  
  nDocs <- length(temp)
  print(paste("Total de stems em todos os", nDocs, "documentos:", nStems, sep = " "))
  # lista de palavras unicas por documento      
  temp <- lapply(temp, function(x) unique(x))  # lista com palavras unicas por documento 
  nStems <- sum(do.call(rbind, lapply(temp, function(x) length(x))))
  nDocs <- length(temp)
  print(paste("Total de stems unicos somando-se cada um dos", nDocs, "documentos:", nStems, sep = " "))
  # vetor com palavras unicas em cada documento  
  temp <- unlist(temp)
  print(paste("Total de stems unicos em todo o Corpus:", length(unique(temp)), sep = " ")) 
  # data.frame com o numero de documentos em que cada palavra aparece
  temp <- as.data.frame(table(temp))
  # length(temp2[,1])
  temp <- temp[order(temp$Freq, decreasing = T),]
  print(paste("Dez stems que parecem em mais documentos:", paste(temp[1:10, 1], collapse = " "), sep = " "))
  docs <- length(corpus) # numero de documentos
  # vetor com as palavras que parecem em menos de perc.doc% dos documentos
  vecW <- temp[(which(temp$Freq/docs < perc.doc)), 1]
  print(paste("Os stems que aparecem em menos de", perc.doc, "dos documentos representam", 
              round(length(vecW)/length(temp[,1]), 2), "do total.", sep = " "))
  resto <- length(temp[,1]) - length(vecW)
  print(paste("V?o restar", resto, "stems", sep = " "))
  # removendo stems selecionados
  # inventei o fas gsub!!!
  vector <- strsplit(vector, " ", fixed = T, perl = F, useBytes = FALSE)  
  vector <- lapply(vector, function(x) ifelse(x %in% vecW, "", x))  # fast gsub
  vector <- lapply(vector, paste, collapse = " ")
  vector <- unlist(vector)  
  #sapply(gregexpr("\\W+", j2), length) + 1 fun??o que conta palavras de uma string - ver stackoverflow
  corpus <- Corpus(VectorSource(vector), 
                   readerControl = list(language = "portuguese"))        
  gc()
  return(corpus)  
}  

StemsTopico <- function(matrix){
  # fun??o que recebe uma matriz stem \times topics e retorna um vetor cujo tamanho 
  # e igual ao numero de topicos e cada elemento representa at? 10 Stems mais 
  # importantes daquele topico.
  wordTopic <- data.frame()
  for( i in 1:dim(matrix)[1]){
    wordTopic[i, 1] <- which.max(matrix[i,])
    wordTopic[i, 2] <- matrix[i, wordTopic[i, 1]]
  }
  wordTopic$topic <- rownames(matrix)  
  wordTopic <- wordTopic[order(wordTopic[,1], -wordTopic[,2]), ]
  vec <- NULL
  for (i in 1:length(unique(wordTopic$V1))){
    vec[i] <- paste(subset(wordTopic, V1 == i)[1:10, 3], collapse = " ")  
  }
  # retirando NAs
  vec <- gsub("NA", "", vec)
  vec <- trimSpace(vec)
  vec <- gsub(" ", "; ", vec)
  return(vec)
}


# Quais os tópicos da revista BPSR?



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
vecAbstract <- unlist(abstract)
aux <- 0

for ( i in 1:length(vecAbstract)) aux[i] <- pegaLinks2(vecAbstract[i])[2]

View(aux)




# criando corpus ---------------------------------------------------------------------------------------------------------------

extendedstopwords <- c("a","about","above","across","after",
                     "again","against","all","almost",
                     "alone","along","already","also",
                     "although","always","am","among",
                     "an","and","another","any","anybody","anyone","anything","anywhere","are","area","areas","aren't","around","as","ask","asked","asking","asks","at","away","b","back","backed","backing","backs","be","became","because","become","becomes","been","before","began","behind","being","beings","below","best","better","between","big","both","but","by","c","came","can","cannot","can't","case","cases","certain","certainly","clear","clearly","come","could","couldn't","d","did","didn't","differ","different","differently","do","does","doesn't","doing","done","don't","down","downed","downing","downs","during","e","each","early","either","end","ended","ending","ends","enough","even","evenly","ever","every","everybody","everyone","everything","everywhere","f","face","faces","fact","facts","far","felt","few","find","finds","first","for","four","from","full","fully","further","furthered","furthering","furthers","g","gave","general","generally","get","gets","give","given","gives","go","going","good","goods","got","great","greater","greatest","group","grouped","grouping","groups","h","had","hadn't","has","hasn't","have","haven't","having","he","he'd","he'll","her","here","here's","hers","herself","he's","high","higher","highest","him","himself","his","how","however","how's","i","i'd","if","i'll","i'm","important","in","interest","interested","interesting","interests","into","is","isn't","it","its","it's","itself","i've","j","just","k","keep","keeps","kind","knew","know","known","knows","l","large","largely","last","later","latest","least","less","let","lets","let's","like","likely","long","longer","longest","m","made","make","making","man","many","may","me","member","members","men","might","more","most","mostly","mr","mrs","much","must","mustn't","my","myself","n","necessary","need","needed","needing","needs","never","new","newer","newest","next","no","nobody","non","noone","nor","not","nothing","now","nowhere","number","numbers","o","of","off","often","old","older","oldest","on","once","one","only","open","opened","opening","opens","or","order","ordered","ordering","orders","other","others","ought","our","ours","ourselves","out","over","own","p","part","parted","parting","parts","per","perhaps","place","places","point","pointed","pointing","points","possible","present","presented","presenting","presents","problem","problems","put","puts","q","quite","r","rather","really","right","room","rooms","s","said","same","saw","say","says","second","seconds","see","seem","seemed","seeming","seems","sees","several","shall","shan't","she","she'd","she'll","she's","should","shouldn't","show","showed","showing","shows","side","sides","since","small","smaller","smallest","so","some","somebody","someone","something","somewhere","state","states","still","such","sure","t","take","taken","than","that","that's","the","their","theirs","them","themselves","then","there","therefore","there's","these","they","they'd","they'll","they're","they've","thing","things","think","thinks","this","those","though","thought","thoughts","three","through","thus","to","today","together","too","took","toward","turn","turned","turning","turns","two","u","under","until","up","upon","us","use","used","uses","v","very","w","want","wanted","wanting","wants","was","wasn't","way","ways","we","we'd","well","we'll","wells","went","were","we're","weren't","we've","what","what's","when","when's","where","where's","whether","which","while","who","whole","whom","who's","whose","why","why's","will","with","within","without","won't","work","worked","working","works","would","wouldn't","x","y","year","years","yes","yet","you","you'd","you'll","young","younger","youngest","your","you're","yours","yourself","yourselves","you've","z")
regexextendedstopwords <- paste("\\b", extendedstopwords, "\\b", sep="")

aux1 <- removeStopoWords(aux, extendedstopwords)  

dtm.control <- list(
  tolower = T,
  removePunctuation = T,
  removeNumbers = T,
  stopwords = c(stopwords("english"),extendedstopwords),
  # snowball tem stopwords em pt
  stemming = T,
  wordLengths = c(3,Inf),
  weighting = weightTf
)



# transformando vetor de documentos em corpus
abstractBPSR <- Corpus(VectorSource(aux)) 

dtm <- DocumentTermMatrix(abstractBPSR, control=dtm.control)
dim(dtm)
## cada doc é uma linha
# cada palavra uma coluna
# número de colunas e igual número de palavras únicas em todos os docs
View(dtm) ## erro. Tem que usar inspect
inspect(dtm[1:10,850:872])


# dtm1 <- removeSparseTerms(dtm,0.95)
# dim(dtm1)
# 

# Drop documents with little or no text (left)
#dtm <- dtm[rowSums(as.matrix(dtm))>0,]

#### Topic Modeling - LDA ####

### Qtos tópicos? 3?
## Número pequeno, pois tenho poucos docs
set.seed(2)
lda <- LDA(dtm,3)

## imprime os tópicos (numerados de 1 a 3)
## para cada documento
# t termos mais prováveis por tópico
t <- 15
View(terms(lda, t))

## vamos olhar o Topic 17
## Aprov Eleitoral? Bush, Clinton etc.



# t termos com prob acima de minimo
minimo <- .05
terms(lda, t, threshold=minimo)

p <- posterior(lda)$terms
p[, 1:10]


# Ref

http://java.dzone.com/articles/topic-modeling-python-and-r
https://github.com/OhLookCake/xkcd-Topics/commit/11ccb14160746c29c06a5a14b97dadc759e8c292
https://github.com/benmarwick/dayofarchaeology/blob/master/004_generate_topic_model.r
http://www.rtexttools.com/1/post/2011/08/getting-started-with-latent-dirichlet-allocation-using-rtexttools-topicmodels.html
