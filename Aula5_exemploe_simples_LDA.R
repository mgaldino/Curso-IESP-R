###################################
#### Manoel Galdino 19/07.2014 ####
#### Aula 5 - LDA/Parte 1      ####
###################################

##########################################
## Começando com um exemplo bem simples ##
##########################################

# Latent Dirichlet Allocation
## exemplo do próprio R

# pacotes
library(XML)
if(require(MCMCpack) == F){install.packages("MCMCpack"); library(MCMCpack)}
if(require(tm) == F){install.packages('tm'); library(tm)}
if(require(SnowballC) == F){install.packages('SnowballC'); library(SnowballC)}
if(require(data.table) == F){install.packages('data.table'); library(data.table)}
if(require(ggplot2) == F){install.packages('ggplot2'); library(ggplot2)}
if(require(seqinr) == F) {install.packages("seqinr"); library(seqinr)}
if(require(RTextTools) == F) {install.packages("RTextTools"); library(RTextTools)}
if(require(topicmodels) == F) {install.packages("topicmodels"); library(topicmodels)}


data(AssociatedPress)
AssociatedPress
dim(AssociatedPress)
# divide o banco entre dados de treinamento
# e dados de teste ou validação

train <- AssociatedPress[1:100]
test <- AssociatedPress[101:150]

save(train, file= "trainAP.RData")
setwd("D:/2014/aulas/IESP/scripts/Curso-IESP-R-aula")
load("trainAP.RData")
dim(train)
# estima o modelo
train.lda <- LDA(train,5)

# extrai os tópicos
(train.topics <- topics(train.lda))



# calcula prob para novos documentos, com base no modelo
# cross-validação...
test.topics <- posterior(train.lda,test)

# escolhe o tópico com maior prob. como tópico dos novos docs.
(test.topics1 <- apply(test.topics$topics, 1, which.max))

# resultado com docs nas linhas
# topicos nas colunas
# prob nas células
test.topics[[2]]

#################################
## Ex. 2, mais completo        ##
## Carregando manchetes do NYT ##
#################################

data(NYTimes)
dim(NYTimes)
nyt <- NYTimes

extendedstopwords <- c("a","about","above","across","after",
                       "again","against","all","almost",
                       "alone","along","already","also",
                       "although","always","am","among",
                       "an","and","another","any","anybody","anyone","anything","anywhere","are","area","areas","aren't","around","as","ask","asked","asking","asks","at","away","b","back","backed","backing","backs","be","became","because","become","becomes","been","before","began","behind","being","beings","below","best","better","between","big","both","but","by","c","came","can","cannot","can't","case","cases","certain","certainly","clear","clearly","come","could","couldn't","d","did","didn't","differ","different","differently","do","does","doesn't","doing","done","don't","down","downed","downing","downs","during","e","each","early","either","end","ended","ending","ends","enough","even","evenly","ever","every","everybody","everyone","everything","everywhere","f","face","faces","fact","facts","far","felt","few","find","finds","first","for","four","from","full","fully","further","furthered","furthering","furthers","g","gave","general","generally","get","gets","give","given","gives","go","going","good","goods","got","great","greater","greatest","group","grouped","grouping","groups","h","had","hadn't","has","hasn't","have","haven't","having","he","he'd","he'll","her","here","here's","hers","herself","he's","high","higher","highest","him","himself","his","how","however","how's","i","i'd","if","i'll","i'm","important","in","interest","interested","interesting","interests","into","is","isn't","it","its","it's","itself","i've","j","just","k","keep","keeps","kind","knew","know","known","knows","l","large","largely","last","later","latest","least","less","let","lets","let's","like","likely","long","longer","longest","m","made","make","making","man","many","may","me","member","members","men","might","more","most","mostly","mr","mrs","much","must","mustn't","my","myself","n","necessary","need","needed","needing","needs","never","new","newer","newest","next","no","nobody","non","noone","nor","not","nothing","now","nowhere","number","numbers","o","of","off","often","old","older","oldest","on","once","one","only","open","opened","opening","opens","or","order","ordered","ordering","orders","other","others","ought","our","ours","ourselves","out","over","own","p","part","parted","parting","parts","per","perhaps","place","places","point","pointed","pointing","points","possible","present","presented","presenting","presents","problem","problems","put","puts","q","quite","r","rather","really","right","room","rooms","s","said","same","saw","say","says","second","seconds","see","seem","seemed","seeming","seems","sees","several","shall","shan't","she","she'd","she'll","she's","should","shouldn't","show","showed","showing","shows","side","sides","since","small","smaller","smallest","so","some","somebody","someone","something","somewhere","state","states","still","such","sure","t","take","taken","than","that","that's","the","their","theirs","them","themselves","then","there","therefore","there's","these","they","they'd","they'll","they're","they've","thing","things","think","thinks","this","those","though","thought","thoughts","three","through","thus","to","today","together","too","took","toward","turn","turned","turning","turns","two","u","under","until","up","upon","us","use","used","uses","v","very","w","want","wanted","wanting","wants","was","wasn't","way","ways","we","we'd","well","we'll","wells","went","were","we're","weren't","we've","what","what's","when","when's","where","where's","whether","which","while","who","whole","whom","who's","whose","why","why's","will","with","within","without","won't","work","worked","working","works","would","wouldn't","x","y","year","years","yes","yet","you","you'd","you'll","young","younger","youngest","your","you're","yours","yourself","yourselves","you've","z")

nyt1 <- nyt[,3]

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

# do pacote tm
nyt1 <- Corpus(VectorSource(nyt1), readerControl = dtm.control) 
class(nyt1)
# Limpando no R 3.0
nyt1 <- tm_map(nyt1, stemDocument, language="english")
nyt1 <- tm_map(nyt1, tolower) # convertendo para caixa baixa
nyt1 <- tm_map(nyt1, removeWords, extendedstopwords) # removendo palavras escolhidas
nyt1 <- tm_map(nyt1, removeNumbers) # removendo numeros
nyt1 <- tm_map(nyt1, removePunctuation) # removendo pontuacao
nyt1 <- tm_map(nyt1, removeWords, stopwords("english")) # removendo stop words (snowball)

# Limpando no R 3.1.1
#nyt1 <- tm_map(nyt1, content_transformer(stemDocument, language="english"))
nyt1 <- tm_map(nyt1, content_transformer(tolower)) # convertendo para caixa baixa
#nyt1 <- tm_map(nyt1, content_transformer(removeWords, extendedstopwords)) # removendo palavras escolhidas
nyt1 <- tm_map(nyt1, content_transformer(removeNumbers)) # removendo numeros
nyt1 <- tm_map(nyt1, content_transformer(removePunctuation)) # removendo pontuacao
#nyt1 <- tm_map(nyt1, content_transformer(removeWords(stopwords("english")))) # removendo stop words (snowball)

# criando uma DocumentTermMatrix 
# é usada como argumento de LDA
dtm <- DocumentTermMatrix(nyt1)
dim(dtm)
## cada doc é uma linha
# cada palavra uma coluna
# número de colunas e igual número de palavras únicas em todos os docs
View(dtm) ## erro. Tem que usar inspect
inspect(dtm[1:10,1:20])

x <- rowSums(as.matrix(dtm))
index <- which(x==0)
dtm1 <- dtm[-index, ]
dim(dtm1)

## Precisamos determinar o númerode tópicos nos dados
## como número de cluster em análise de cluster
## nesse exemplo, já temos
## de uma outra análise


#### Topic Modeling - LDA ####
#set.seed(51)
#trainpoints <- sample(1:nrow(dtm1),1*nrow(dtm1),replace=F) # to train on a subsample, change 1.0 to a lower value, say 0.8

k <- length(unique(nyt$Topic.Code))

## função pra extrair termos
SpecificTerms <- function(lda.model,n=1) {
  p <- posterior(lda.model)$terms
  n <- min(n,ncol(p))
  cumulativep <- colSums(p)
  specificp <- t(apply(p,1,function(x) ((x) + (x/cumulativep) )))
  
  topterms <- t(apply(specificp, 1, function(x)
    (colnames(specificp)[order(x,decreasing=T)[1:n]]) ))
  
  topterms
}

set.seed(4)
lda <- LDA(dtm1, k)

# t termos mais prováveis por tópico
t <- 15
View(terms(lda, t))

## vamos olhar o Topic 17
## Aprov Eleitoral? Bush, Clinton etc.



# t termos com prob acima de minimo
minimo <- .05
terms(lda, t, threshold=minimo)



# tópicos mais prováveis por documentos
head(topics(lda))


## Agora vamos fazer algumas análises dos resultados

# Here I construct a dataframe that scores each document according to
# how closely its content
# matches up with each topic.  
# The closer the score is to 0, the more likely its content matches
# up with a particular topic.


nytTopics <- posterior(lda)$topics
dfnytTopics <- as.data.frame(nytTopics)
dim(dfnytTopics)
head(dfnytTopics)
names(dfnytTopics) <- paste("topic", names(dfnytTopics), sep="")
dfnytTopics$docs <- row.names(dfnytTopics)
head(dfnytTopics)

summary(dfnytTopics$topic17)
# qtos docs topico 17 é o maior??

library(compiler)

which.rowMax <- cmpfun (
  function(df,n) {
    tmp <- numeric(length=n)
    for ( i in 1:n) tmp[i] <- which.max(df[i,-ncol(df)])
    return(tmp)
    }
  )
n <- nrow(dfnytTopics)
tmp <- which.rowMax(dfnytTopics[, -n], n )
topic1 <- dfnytTopics[which(tmp==1),]

dfTemp <- as.character(nyt1)
docEscolhido <-  dfTemp[which(tmp==1)]
set.seed(2)
docEscolhido[sample(1:length(docEscolhido), 15) ]
terms(lda, t)[,1]


