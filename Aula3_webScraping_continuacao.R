###################################
#### Manoel Galdino 19/07.2014 ####
#### Aula 3 - Webscraping      ####
#### Continuação               ####
###################################

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

# definindo diretorio
setwd('D:\\2014\\aulas\\IESP\\scripts\\Curso-IESP-R-aula')


