###################################
#### Manoel Galdino 19/07.2014 ####
#### Aula 1                    ####
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

#### Escopo

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


f <- function(x) {
  y <- 2 * x 
  print(x)
  print(y)
  print(z)
}



