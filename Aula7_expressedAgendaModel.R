###################################
#### Manoel Galdino 24/07.2014 ####
#### Aula 7 - Projeto Retórica ####
###################################


# pacotes
library("XML")
library("MCMCpack")
library("tm")
library("SnowballC")
library("data.table")
library("ggplot2")
library("seqinr")
library("RTextTools")
library("topicmodels")
library("compiler")


##

## Para rodar o modelo
## Muda diretório onde estão os dados
setwd("D:\\2014\\aulas\\IESP\\scripts\\Curso-IESP-R-aula\\Data")

load("DTM.RData") # Document Term Matrix
load("Autor_Matrix.RData") # Author Matrix
load("Info.RData")

## script em outro diretório
setwd("D:\\2014\\aulas\\IESP\\scripts\\Curso-IESP-R-aula")
source('ExpAgendVMVA.R')

exp.agenda.vonmon.comp <- cmpfun( exp.agenda.vonmon)



# Originalmente...
# topics <- exp.agenda.vonmon(term.doc = as.matrix(dtm), authors = autorMatrix,
#                             n.cats = 70,
#                             verbose = T, kappa = 400)

# Vamos tentar com função compilada

system.time(topics  <- exp.agenda.vonmon.comp(term.doc = as.matrix(dtm),
                                              authors = autorMatrix,
                             n.cats = 70,
                             verbose = T, kappa = 400))

## topics tem o output da func
## precisamos extrair os tópicos

autorTopicOne <- NULL
for( i in 1:dim(topics[[1]])[1]){
  autorTopicOne[i] <- which.max(topics[[1]][i,])
}
autorTopicPerc <- prop.table(topics[[1]], 1) # compute the proportion of documents from each author to each topic

autorTopicOne <- as.data.frame(autorTopicOne)

for( i in 1:nrow(autorTopicOne)){
  autorTopicOne$enfase[i] <- autorTopicPerc[i,which.max(autorTopicPerc[i,])]
}

json_file <- "https://github.com/Demoulidor/Dados/tree/master/deputadosFederais/deputados.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

df <- data.frame(nome=json_data[[1]][[1]]$nome,
                 url=json_data[[1]][[1]]$url,
                 foto=gsub("full\\/", "", json_data[[1]][[1]]$images[[1]]$path),
                 email=gsub("mailto:", "", json_data[[1]][[1]]$email),
                 id=json_data[[1]][[1]]$id_dep)

for ( i in 2:length(json_data[[1]])) {
  df <- rbind(df, data.frame(nome=json_data[[1]][[i]]$nome,
                             url=json_data[[1]][[i]]$url,
                             foto=gsub("full\\/", "", json_data[[1]][[i]]$images[[1]]$path),
                             email=gsub("mailto:", "", json_data[[1]][[i]]$email),
                             id=json_data[[1]][[i]]$id_dep))
}


autorTopicOne$uf <- infoPeqExpArrumado$uf[!duplicated(infoPeqExpArrumado$autor)]
autorTopicOne$partido <- infoPeqExpArrumado$partido[!duplicated(infoPeqExpArrumado$autor)]

autorTopicOne$autor <- unique(infoPeqExpArrumado$autor)

head(autorTopicOne)

autorTopicVis <- merge(autorTopicOne, df, by.x="autor", by.y="nome", all.x=T)

# arrumando sites dos deputados
autorTopicVis$url <- gsub("http:\\/\\/", "http:\\/\\/www.", autorTopicVis$url)

# rotulando topicos
rotulos <- read.table('20131031_rotulos1.txt', sep = "\t", header = T)

autorTopicVis <- merge(autorTopicVis, rotulos[,c('topico','rotulo')], by.x='autorTopicOne', by.y='topico', all.x=T)

write.table(autorTopicVis , file="20131031_autorTopicVis_70.csv", sep=",", row.names=T)

## Ref
# Paper do Gimmer

# http://web.stanford.edu/~jgrimmer/ExpAgendaFinal.pdf


