library(tidyverse)
library(doBy) #Estadisticas agrupadas
library(dplyr) #manejo de dataframes
library(EnvStats) # informacion estadistica mas completa
library(nortest) #pruebas de normalidad
library(car) #contiene prueba de levene 
library(agricolae) #algunas pruebas posthoc
library(DescTools)


# datos es el DATAFRAME


#prueba de normalidad por grupo
by(data = datos,INDICE= datos$vi, FUN=function(datos){shapiro.test(datos$vd)})

# la prueba de normalidad se puede modificar por cualquiera de los otros test 

#prueba de honogeniedad de la varianza u HOMOCEDASTICIDAD
bartlett.test(datos$vd~datos$vi)
leveneTest(datos$vd,datos$vi)
fligner.test(datos$vd,datos$vi)


#Prueba de igualdad de la Media T.student, varianza igual o distinta depende de levene o barttlet.
t.test(datos$vd[datos$vi==1],datos$vd[datos$vi==3],alternative = "greater",var.equal = TRUE,paired=FALSE)

#alternativa no parametrica 
#Umann whitney
wilcox.test(x = datos$vd[datos$vi==1], y = datos$vd[datos$vi==2], alternative = "two.sided", mu = 0, paired = FALSE, conf.int = 0.95)


#prueba Binomial para la proporcion de 2 grupos
#x1=numero de exitos en el grupo1
#x2=numero de exitos en el grupo2

#n1=tama?o de la muestra grupo1
#n2=tama?o de la muestra grupo2

#p0= proporcion esperada, teorica, historica, norma, si p0=0 entonces se compara g1 v2 g2
prop.test(x=c(x1, x2), n=c(n1, n2), p=p0, conf.level =0.95, alternative = "two.sided", correct = FALSE)