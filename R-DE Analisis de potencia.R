library(tidyverse)
library(pwr)


# calculo potencia de una prueba 
set.seed(123456)
g1<- rnorm(15,2.3,0.39)
g2<- rnorm(15,1.9,0.45)

t.test(g1,g2,mu=0,paired = FALSE,alternative = "two.sided")

pwr.t.test(
  n = 75,
  d = 0.4, #diferencia entre los grupos
  sig.level = 0.05,
  type = "two.sample",
  alternative = "two.sided"
)

