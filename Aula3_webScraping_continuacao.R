###################################
#### Manoel Galdino 19/07.2014 ####
#### Aula 3 - Webscraping      ####
#### Continuação               ####
###################################

# definindo diretorio
setwd('D:\\2014\\aulas\\IESP\\scripts\\Curso-IESP-R-aula')


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

## Pra evitar sobrecarregar os servidores, vamos usar
# Sys.sleep

?Sys.sleep


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




