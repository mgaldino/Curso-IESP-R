###################################
#### Manoel Galdino 19/07.2014 ####
#### Aula 1  - revisão         ####
###################################


## Revisão de alguns conceitos básicos

### Funções, Escopo e Lazy-evaluation

# No site do Hadley, http://adv-r.had.co.nz/Functions.html#function-components
# Temos algumas notas de aulas do R
# Vou utilizar aqui um pouco do que está lá
# 
# Todas as funcões no R têm três partes
# 
# o corpo: que e o código no interior da função
# os argumentos (formals): que controlam como voce chama a função
# o ambiente: que mapeia onde as variáveis das funções estão localizadas
# 
# 


f <- function(x) x^2
f

## sempre que voce imprime uma função no R, ele mostra esses três componentes da função
## Quando o ambiente não é mostrado, é porque é o global.

formals(f)
body(f)
environment(f)

## exceções: funções primitivas, que chamam código diretamente em C

sum
formals(sum)
body(sum)
environment(sum)

## apenas as funções no pacote base são primitivas.

#####################################
#### Escopo e Lazy-evaluation no R ##
#####################################

# Segundo a wikipedia, o escopo é “um contexto delimitante aos quais valores e expressões 
# estão associados”. Em outras palavras, quando fazemos um programa, o escopo determina 
# o que faz parte daquele contexto e o que não faz parte. Por exemplo, quando pedimos para
# o computados avaliar x + 5, precisamos de regras que definam onde acharmos o valor de x.
# Essas regras definem como é formado o escopo da linguagem. Na prática, isso é importante
# para evitar, por exemplo, que variáveis com o mesmo nome em contextos diferentes gerem 
# conflitos na execução do programa da parte do computador. E por isso é importante que 
# o programador conheça como funciona o escopo da linguagem em que ele está trabalhando.
# Um paper dos criadores do R sobre o escopo da linguagem.
# https://www.stat.auckland.ac.nz/~ihaka/downloads/lexical.pdf

# "In R the scoping rules state that the free variables in a function are resolved 
# in the environment that was active at the time the function was created. In S the scope
# rules state that the free variables are resolved at top-level" 
# (Robert Gentleman and Ross Ihaka, p. 4).

## Outras ref.: http://www.johndcook.com/blog/2008/10/16/default-arguments-and-lazy-evaluation-in-r/


f <- function(x) {
  y <- 2 * x 
  print(x)
  print(y)
  print(z)
}

# Na função acima, x é um argumento da função ou parâmetro formal da função,
# y é uma variável local (pois é criada e definida no interior da função)
# e z é uma variável livre (não é criada no interior da função).

f(10)



# Quando formos rodar (avaliar) essa função, precisamos passar o argumento da função,
# por exemplo, f(10). A função então sabe que y é 20 e pode imprimir o valor de x e de y, 
# mas não de z. O que as regras de escopo devem determinar é onde a função deve olhar
# para determinar quanto é x, y e z.  Se z não tiver sido definido fora da função no 
# ambiente global, o R não conseguirá determinar o seu valor e, portanto, retornará o erro.
# Se vocês rodarem f(10), verão que o R imprime o valor de x, de y e retorna um erro para z.
# Se, por outro lado, criarmos uma outra função, g, definida abaixo, e rodarmos g(10), 
# o R retornará apenas um erro.

g <- function(x) {
  y <- 2 * x 
  print(z)
  print(y)
  print(x)
}

# Em computação, nós dizemos que o R tem lazy-evaluation, e não strict evaluation.
# Ou seja, o R só tenta executar um comando no momento em que ele é chamado. 
# Ele vai executando a função sequencialmente e checando se encontra os valores
# necessários enquanto executa o código.  Assim, uma função como a do código abaixo 
# pode funcionar perfeitamente em R:


h <- function (x) {
  y <- 3*abs(x) + 1
  if (y < x) {
    z <- minha_funcao_nao_definida_em_lugar_algum(10)  
  }
  return(y)
}

set.seed(2)
x <- rnorm(20, 0, 50)
for ( i in 1:20) h(x[i])


### Escopo léxico

# Vocês podem ler um texto explicando as diferenças conceituais entre escopo léxico e
# espoco dinâmico aqui:
# http://darrenjw.wordpress.com/2011/11/23/lexical-scope-and-function-closures-in-r/
# Eu vou explicar como o escopo funciona no R por meio de exemplos

fun1 <- function (x) {
  x + y
}

# quiz: qual variável é livre em fun1?


## Agora olhemos o caso mais interessante, fun2

fun2 <- function () {
  y <- 20
  function (x) x + y
}

# fun1 foi criada no ambiente global. É um caso fácil
# fun2 tbm foi criada no ambiente global. Mas tem uma função anônima, igual a fun1
# que foi criada no ambiente local
# fun2 retorna uma função como objeto!
# igual a fun1, mas criada localmente
# a função anônima usa o valor de y no ambiente onde foi criada, ou seja, dentro de fun2

# o valor de fun1 depende do valor de y no ambiente global

fun1(3)
#> Error in fun1(3) : object 'y' not found

y <- 10
fun1(3)
#> [1] 13

y <- 13
fun1(3)
#> [1] 16

fun3 <- fun2()

y
fun1(3)

fun3(3)

y
fun3

## Não executem o código abaixo.
## Apenas leiam o c[odigo e respondam
## o que g(2) deve retornar?

a=1
b=2
f <- function(x) {
  a*x + b
}

g <- function(x) {
  a=2
  b=1
  f(x)
}

g(2)

## Pensem antes de olhar as resposta

### pensem...
### pensem...


### ....

### Vocês já devem saber que a resposta é 4 ou 5.
# No escopo dinâmico, g(2) retornaria 5, num escopo léxico, retornaria 4 (que é o do R).
# A razão para tal é que no escopo léxico, o escopo de uma função é definido pelo ambiente 
# em que ela foi criada. A função f foi criada no ambiente global e, portanto, 
# as variáveis livres ‘a’ e ‘b’ terão seus valores determinados globalmente 
# (que é o ambiente onde f foi criada). Numa linguagem de escopo dinâmico, 
# o que importa é onde a função está sendo chamada. Nesse caso, as variáveis livres 
# teriam seus valores determinados primeiramente localmente, dentro de g e, apenas caso
# não existissem esses valores em g, procuraríamos no ambiente global.
# Vale notar, ainda, que se a f tivesse sido criada localmente em g, então g(2) retornaria 5.

g(2)

## vamos aproveitar e introduzir, rapidamente, a ideia de debugar um código

gdebug <- function(x) {
  browser() ## função para debugar o cóodigo
  a=2
  b=1
  f(x)
}

gdebug(2)

## Viram como browser() funciona?
## ele permite executar linha a linha o código
## "n"+ "enter" vai pra próxima linha do código ou clicando no botão
## e posso inspecionar o valor de das variáveis a cada linha
## mas não foi muito útil

## vamos modificar f
fdebug <- function(x) {
  browser()
  y <- 0
  y <- a
  y <- y*x
  y <- y + b
  return(y)
}

gdebug1 <- function(x) {
  browser() ## função para debugar o cóodigo
  a=2
  b=1
  fdebug(x)
}

gdebug1(2)


## Entenderam como funciona o escopo no R?
## Exercícios


## O que o código abaixo retorna?

# y <- 10
# f1 <- function(x) {
#   function() {
#     x + 10
#   }
# }
# f1(1)()

## A Função abaixo retorna um erro quando é chamada? Pq?

f2 <- function(a, b) {
  a * 10
}
f2(10, stop("This is an error!"))

####
#### respostas

###
###
# 11
## Não, retorna 100. Como o R é lazy-evaluated, o segundo argumento não faz parte
## do corpo da função e, portanto, não é executado.




###################################
## comparando tempo de execução  ##
###################################


## instalando pacote microbenchmark


if ( require(microbenchmark)==F) {
  install.packages('microbenchmark')
  library(microbenchmark)}

## qual a diferença entre "require" e "library"? Vejam o help do R


mean1 <- function(x) mean(x)
mean2 <- function(x) sum(x) / length(x)

x <- runif(100)
microbenchmark(
  mean1(x),
  mean2(x)
)



